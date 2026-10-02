// What each kernel does with the 32-bit address length a caller passes to the
// socket calls that take one: bind(2), connect(2), accept(2), accept4(2) (Linux
// only), getsockname(2) and getpeername(2). Linux declares the length `int` (or
// `int *`), so a word at or above 2^31 is negative there; Darwin declares it
// `socklen_t`, so the same word is a length of more than two gigabytes.
//
// Every socket is IPv4 TCP on 127.0.0.1. Lengths are 32-bit words, printed as
// hex with their value read as `int32_t` alongside.
//
// Sections:
//   S  the sweep. For every word in [-300, 300], every +/-2^k and +/-2^k +/- 1
//      for k in [0, 31], and INT32_MIN, INT32_MAX: each call on a descriptor
//      that should succeed, with the address buffer 4096 mapped bytes (a
//      sockaddr_in at its start, the rest zero, for the calls that read it; a
//      0xAA sentinel for those that write it). Consecutive words (in signed
//      order) with the same answer are printed as one range. Columns:
//        bind     errno, and whether the socket is then bound
//        connect  errno, to a listener on loopback
//        getsockname / getpeername
//                 errno, the length cell afterwards ("kept" if untouched), and
//                 how many sentinel bytes were overwritten
//        accept / accept4
//                 on a non-blocking listener with one connection queued:
//                 errno, cell, bytes written, then whether the connection is
//                 still queued (a second accept), whether the call consumed a
//                 descriptor number, and what the client then sees: poll's
//                 revents within 200 ms, and recv's answer (0 is a FIN).
//   O  the check order, at the lengths -1, INT32_MIN, 0, 16, 129 and 256:
//        O1 a closed descriptor;   O2 a pipe;
//        O3 an unmapped address buffer (PROT_NONE page);
//        O4 a NULL address buffer;
//        O5 an unmapped length cell, on a good descriptor and on a closed one;
//        O6 accept on a non-listening socket, a datagram socket, and a
//           non-blocking listener with nothing queued.
//        O7 bind and connect on descriptors that are not sockets (a pipe, a
//           regular file, an epoll or kqueue descriptor), at the lengths -1,
//           INT32_MIN, 0, 15, 16, 128, 129 and 256, through a mapped and an
//           unmapped address buffer: whether the length and the copy are
//           judged before the descriptor's kind. Then the same for an AF_UNIX
//           and an AF_INET6 stream socket, whose address is not a
//           sockaddr_in.
//   B  a *blocking* accept with nothing queued, at -1 and INT32_MIN: a child
//      connects 200 ms in; the answer, the elapsed time, whether the
//      connection is still queued, and what the client sees.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/sal socket-address-length.c && /tmp/sal
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/sal /probe/socket-address-length.c && /tmp/sal'
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64 (uid 501) and on Linux 6.18.5
// aarch64 (Apple `container`, debian:trixie, root); the outputs are checked in
// beside this file as socket-address-length.darwin-27.0-uid501.txt and
// socket-address-length.linux-6.18.5-aarch64-root.txt. In short:
//   Linux reads every length as a signed int. bind and connect: EINVAL outside
//     [0, 128] (so for every negative word), before the copy. getsockname,
//     getpeername, accept and accept4: EINVAL for a negative length, the
//     length cell and the buffer untouched. accept and accept4 have already
//     taken the connection off the queue by then: it is gone (a second accept
//     is EAGAIN), no descriptor number is consumed, and the client sees an
//     orderly close (POLLIN|POLLRDHUP, recv 0). A blocking accept sleeps
//     first and then does the same. A NULL accept buffer never reads the
//     length at all, so every length succeeds there.
//   Darwin reads every length as the socklen_t it is. bind and connect:
//     ENAMETOOLONG above 255, so for every word at or above 2^31.
//     getsockname, getpeername and accept: no length is an error; the copy is
//     the smaller of the length and 16, and 16 is reported.
//   Check order: EBADF and ENOTSOCK precede the length on both, except that
//     Linux's connect copies the sockaddr in before it asks whether the
//     descriptor is a socket: on a pipe, a file or an epoll descriptor it
//     answers EINVAL for a length outside [0, 128] and EFAULT for an unmapped
//     buffer at 1..128, and ENOTSOCK only after that. Linux's bind, and
//     Darwin's bind and connect, answer ENOTSOCK at every length.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

#ifndef POLLRDHUP
#define POLLRDHUP 0
#endif

static const char *en(int e)
{
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case ENOTSOCK: return "ENOTSOCK";
    case EFAULT: return "EFAULT";
    case EINVAL: return "EINVAL";
    case EAGAIN: return "EAGAIN";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EAFNOSUPPORT: return "EAFNOSUPPORT";
    case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ECONNABORTED: return "ECONNABORTED";
    case ECONNRESET: return "ECONNRESET";
    case ECONNREFUSED: return "ECONNREFUSED";
    case EISCONN: return "EISCONN";
    case ENOTCONN: return "ENOTCONN";
    case ENOMEM: return "ENOMEM";
    case EDESTADDRREQ: return "EDESTADDRREQ";
    default: {
        static char buf[32];
        snprintf(buf, sizeof buf, "errno%d", e);
        return buf;
    }
    }
}

static unsigned char *buffer;   // 4096 mapped bytes
static void *unmapped;          // a PROT_NONE page

static int64_t now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static struct sockaddr_in loopback(uint16_t port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    return a;
}

static void fill_sockaddr(uint16_t port)
{
    memset(buffer, 0, 4096);
    struct sockaddr_in a = loopback(port);
    memcpy(buffer, &a, sizeof a);
}

static int tcp(void) { return socket(AF_INET, SOCK_STREAM, 0); }

// A socket bound to 127.0.0.1 on an ephemeral port neither of whose bytes is
// the 0xAA sentinel, so that a written port can never be mistaken for an
// untouched byte.
static int bound_socket(void)
{
    for (;;) {
        int s = tcp();
        struct sockaddr_in a = loopback(0);
        socklen_t len = sizeof a;
        if (bind(s, (struct sockaddr *)&a, sizeof a) != 0 || getsockname(s, (struct sockaddr *)&a, &len) != 0) {
            perror("bound_socket");
            exit(1);
        }
        unsigned char *port = (unsigned char *)&a.sin_port;
        if (port[0] != 0xAA && port[1] != 0xAA)
            return s;
        close(s);
    }
}

static int listener(int nonblocking)
{
    int l = bound_socket();
    if (listen(l, 128) != 0) {
        perror("listener");
        exit(1);
    }
    if (nonblocking)
        fcntl(l, F_SETFL, fcntl(l, F_GETFL) | O_NONBLOCK);
    return l;
}

static uint16_t port_of(int fd)
{
    struct sockaddr_in a;
    socklen_t len = sizeof a;
    if (getsockname(fd, (struct sockaddr *)&a, &len) != 0)
        return 0;
    return ntohs(a.sin_port);
}

static int connect_to(uint16_t port)
{
    int c = bound_socket();
    struct sockaddr_in a = loopback(port);
    if (connect(c, (struct sockaddr *)&a, sizeof a) != 0) {
        perror("connect_to");
        exit(1);
    }
    return c;
}

// Connect to the listener `l`, and wait until the connection is on its accept
// queue: Darwin's connect(2) can return before the server half is acceptable.
static int queue_connection(int l)
{
    int c = connect_to(port_of(l));
    struct pollfd p = { .fd = l, .events = POLLIN };
    if (poll(&p, 1, 1000) != 1) {
        fprintf(stderr, "queue_connection: the connection never became acceptable\n");
        exit(1);
    }
    return c;
}

static int lowest_free_fd(void)
{
    int fd = dup(0);
    close(fd);
    return fd;
}

static void drain(int l)
{
    int flags = fcntl(l, F_GETFL);
    fcntl(l, F_SETFL, flags | O_NONBLOCK);
    for (;;) {
        int a = accept(l, NULL, NULL);
        if (a < 0)
            break;
        close(a);
    }
    fcntl(l, F_SETFL, flags);
}

static int sentinel_overwritten(void)
{
    int n = 0;
    for (int i = 0; i < 4096; i++)
        if (buffer[i] != 0xAA)
            n++;
    return n;
}

static void cell_text(char *out, size_t size, uint32_t before, uint32_t after)
{
    if (after == before)
        snprintf(out, size, "kept");
    else
        snprintf(out, size, "%u", after);
}

// What the client of a connection sees once the server end has had its chance
// to go: poll's revents within 200 ms, and recv's answer.
static void client_view(char *out, size_t size, int client)
{
    struct pollfd p = { .fd = client, .events = POLLIN | POLLRDHUP };
    int n = poll(&p, 1, 200);
    char c;
    errno = 0;
    ssize_t r = n > 0 ? recv(client, &c, 1, MSG_DONTWAIT) : -2;
    int e = errno;
    if (r == -2)
        snprintf(out, size, "revents=0 (nothing in 200ms)");
    else if (r < 0)
        snprintf(out, size, "revents=0x%x recv=%s", p.revents, en(e));
    else
        snprintf(out, size, "revents=0x%x recv=%zd", p.revents, r);
}

// ---------------------------------------------------------------------------
// Section S: one answer per word, then grouped.

static char answer[512];

static void sweep_bind(uint32_t word)
{
    int s = tcp();
    fill_sockaddr(0);
    errno = 0;
    int r = bind(s, (struct sockaddr *)buffer, (socklen_t)word);
    int e = r == 0 ? 0 : errno;
    snprintf(answer, sizeof answer, "%s bound=%s", en(e), port_of(s) != 0 ? "yes" : "no");
    close(s);
}

static int connect_listener;

static void sweep_connect(uint32_t word)
{
    int s = tcp();
    fill_sockaddr(port_of(connect_listener));
    errno = 0;
    int r = connect(s, (struct sockaddr *)buffer, (socklen_t)word);
    int e = r == 0 ? 0 : errno;
    snprintf(answer, sizeof answer, "%s", en(e));
    close(s);
    drain(connect_listener);
}

static int name_socket;
static int peer_socket;

static void sweep_name(uint32_t word, int peer)
{
    memset(buffer, 0xAA, 4096);
    uint32_t cell = word;
    errno = 0;
    int r = peer ? getpeername(peer_socket, (struct sockaddr *)buffer, (socklen_t *)&cell)
                 : getsockname(name_socket, (struct sockaddr *)buffer, (socklen_t *)&cell);
    int e = r == 0 ? 0 : errno;
    char c[32];
    cell_text(c, sizeof c, word, cell);
    snprintf(answer, sizeof answer, "%s cell=%s written=%d", en(e), c, sentinel_overwritten());
}

static int accept_listener;

static void sweep_accept(uint32_t word, int use_accept4)
{
    int client = queue_connection(accept_listener);
    int freeBefore = lowest_free_fd();
    memset(buffer, 0xAA, 4096);
    uint32_t cell = word;
    errno = 0;
    int r;
#ifdef __linux__
    if (use_accept4)
        r = accept4(accept_listener, (struct sockaddr *)buffer, (socklen_t *)&cell, SOCK_CLOEXEC);
    else
#endif
        r = accept(accept_listener, (struct sockaddr *)buffer, (socklen_t *)&cell);
    int e = r >= 0 ? 0 : errno;
    char c[32];
    cell_text(c, sizeof c, word, cell);
    int written = sentinel_overwritten();
    if (r >= 0) {
        snprintf(answer, sizeof answer, "%s cell=%s written=%d fd=%s", en(e), c, written,
                 r == freeBefore ? "lowest" : "other");
        close(r);
        close(client);
        return;
    }
    int freeAfter = lowest_free_fd();
    int again = accept(accept_listener, NULL, NULL);
    int againErrno = again >= 0 ? 0 : errno;
    char view[128];
    client_view(view, sizeof view, client);
    snprintf(answer, sizeof answer, "%s cell=%s written=%d; then accept=%s; fd consumed=%s; client %s",
             en(e), c, written, en(againErrno), freeAfter == freeBefore ? "no" : "yes", view);
    if (again >= 0)
        close(again);
    close(client);
}

static int compare_words(const void *a, const void *b)
{
    int32_t x = *(const int32_t *)a, y = *(const int32_t *)b;
    return x < y ? -1 : x > y;
}

static uint32_t words[1024];
static int word_count;

static void add_word(int64_t value)
{
    uint32_t w = (uint32_t)(int32_t)value;
    for (int i = 0; i < word_count; i++)
        if (words[i] == w)
            return;
    words[word_count++] = w;
}

static void build_words(void)
{
    for (int v = -300; v <= 300; v++)
        add_word(v);
    for (int k = 0; k <= 31; k++) {
        int64_t p = (int64_t)1 << k;
        add_word(p); add_word(p - 1); add_word(p + 1);
        add_word(-p); add_word(-p - 1); add_word(-p + 1);
    }
    add_word(INT32_MIN);
    add_word(INT32_MAX);
    qsort(words, word_count, sizeof words[0], compare_words);
}

static void sweep(const char *call, void (*one)(uint32_t, int), int arg)
{
    char previous[512] = "";
    uint32_t from = 0, to = 0;
    int samples = 0;
    for (int i = 0; i <= word_count; i++) {
        if (i < word_count)
            one(words[i], arg);
        if (i == word_count || strcmp(answer, previous) != 0) {
            if (samples > 0)
                printf("S %-11s 0x%08x..0x%08x (%d..%d, %d samples): %s\n", call, from, to, (int32_t)from,
                       (int32_t)to, samples, previous);
            if (i == word_count)
                break;
            strcpy(previous, answer);
            from = words[i];
            samples = 0;
        }
        to = words[i];
        samples++;
    }
}

static void sweep_bind_(uint32_t w, int unused) { (void)unused; sweep_bind(w); }
static void sweep_connect_(uint32_t w, int unused) { (void)unused; sweep_connect(w); }

// ---------------------------------------------------------------------------
// Section O: check order.

static const int64_t order_lengths[] = { -1, INT32_MIN, 0, 16, 129, 256 };
#define ORDER_COUNT (sizeof order_lengths / sizeof order_lengths[0])

static void order_row(const char *label, int64_t len, int r, int e, const char *extra)
{
    printf("O %-44s len=%-11d %s%s\n", label, (int32_t)len, r >= 0 ? "OK" : en(e), extra);
}

static void order_section(void)
{
    // A descriptor number nothing in this process has open, rather than one
    // closed here, which the pipe below would reuse.
    int closed = 1000;
    if (fcntl(closed, F_GETFD) != -1 || errno != EBADF) {
        fprintf(stderr, "descriptor %d is open\n", closed);
        exit(1);
    }
    int pipefd[2];
    pipe(pipefd);
    int bound = bound_socket();
    int l = listener(1);
    int connected = queue_connection(l);
    drain(l);

    for (size_t i = 0; i < ORDER_COUNT; i++) {
        int64_t len = order_lengths[i];
        socklen_t w = (socklen_t)(uint32_t)(int32_t)len;
        uint32_t cell;
        int r;
        int fds[2] = { closed, pipefd[0] };
        const char *names[2] = { "closed", "pipe" };
        for (int k = 0; k < 2; k++) {
            char label[64];
            fill_sockaddr(port_of(l));
            snprintf(label, sizeof label, "O%d bind %s", k + 1, names[k]);
            errno = 0; r = bind(fds[k], (struct sockaddr *)buffer, w); order_row(label, len, r, errno, "");
            snprintf(label, sizeof label, "O%d connect %s", k + 1, names[k]);
            errno = 0; r = connect(fds[k], (struct sockaddr *)buffer, w); order_row(label, len, r, errno, "");
            cell = w;
            snprintf(label, sizeof label, "O%d getsockname %s", k + 1, names[k]);
            errno = 0; r = getsockname(fds[k], (struct sockaddr *)buffer, (socklen_t *)&cell);
            order_row(label, len, r, errno, cell == w ? " cell=kept" : " cell=written");
            cell = w;
            snprintf(label, sizeof label, "O%d getpeername %s", k + 1, names[k]);
            errno = 0; r = getpeername(fds[k], (struct sockaddr *)buffer, (socklen_t *)&cell);
            order_row(label, len, r, errno, cell == w ? " cell=kept" : " cell=written");
            cell = w;
            snprintf(label, sizeof label, "O%d accept %s", k + 1, names[k]);
            errno = 0; r = accept(fds[k], (struct sockaddr *)buffer, (socklen_t *)&cell);
            order_row(label, len, r, errno, cell == w ? " cell=kept" : " cell=written");
            if (r >= 0) close(r);
        }

        // O3: an unmapped address buffer.
        {
            int s = tcp();
            errno = 0; r = bind(s, (struct sockaddr *)unmapped, w);
            order_row("O3 bind unmapped", len, r, errno, "");
            close(s);
            s = tcp();
            errno = 0; r = connect(s, (struct sockaddr *)unmapped, w);
            order_row("O3 connect unmapped", len, r, errno, "");
            close(s);
            cell = w;
            errno = 0; r = getsockname(bound, (struct sockaddr *)unmapped, (socklen_t *)&cell);
            order_row("O3 getsockname unmapped", len, r, errno, cell == w ? " cell=kept" : " cell=written");
            cell = w;
            errno = 0; r = getpeername(connected, (struct sockaddr *)unmapped, (socklen_t *)&cell);
            order_row("O3 getpeername unmapped", len, r, errno, cell == w ? " cell=kept" : " cell=written");
            int client = queue_connection(l);
            cell = w;
            errno = 0; r = accept(l, (struct sockaddr *)unmapped, (socklen_t *)&cell);
            int e = errno;
            char extra[160];
            int again = accept(l, NULL, NULL);
            int againErrno = again >= 0 ? 0 : errno;
            snprintf(extra, sizeof extra, "%s; then accept=%s", cell == w ? " cell=kept" : " cell=written",
                     en(againErrno));
            order_row("O3 accept unmapped, one queued", len, r, e, extra);
            if (r >= 0) close(r);
            if (again >= 0) close(again);
            close(client);
        }

        // O4: a NULL address buffer.
        {
            int s = tcp();
            errno = 0; r = bind(s, NULL, w);
            order_row("O4 bind NULL", len, r, errno, "");
            close(s);
            s = tcp();
            errno = 0; r = connect(s, NULL, w);
            order_row("O4 connect NULL", len, r, errno, "");
            close(s);
            cell = w;
            errno = 0; r = getsockname(bound, NULL, (socklen_t *)&cell);
            order_row("O4 getsockname NULL", len, r, errno, cell == w ? " cell=kept" : " cell=written");
            cell = w;
            errno = 0; r = getpeername(connected, NULL, (socklen_t *)&cell);
            order_row("O4 getpeername NULL", len, r, errno, cell == w ? " cell=kept" : " cell=written");
            int client = queue_connection(l);
            cell = w;
            errno = 0; r = accept(l, NULL, (socklen_t *)&cell);
            int e = errno;
            char extra[160];
            int again = accept(l, NULL, NULL);
            int againErrno = again >= 0 ? 0 : errno;
            snprintf(extra, sizeof extra, "%s; then accept=%s", cell == w ? " cell=kept" : " cell=written",
                     en(againErrno));
            order_row("O4 accept NULL, one queued", len, r, e, extra);
            if (r >= 0) close(r);
            if (again >= 0) close(again);
            close(client);
        }

        // O6: accept's own failures, with this length.
        {
            int idle = tcp();
            cell = w;
            errno = 0; r = accept(idle, (struct sockaddr *)buffer, (socklen_t *)&cell);
            order_row("O6 accept idle stream socket", len, r, errno, cell == w ? " cell=kept" : " cell=written");
            close(idle);
            int dgram = socket(AF_INET, SOCK_DGRAM, 0);
            cell = w;
            errno = 0; r = accept(dgram, (struct sockaddr *)buffer, (socklen_t *)&cell);
            order_row("O6 accept datagram socket", len, r, errno, cell == w ? " cell=kept" : " cell=written");
            close(dgram);
            cell = w;
            errno = 0; r = accept(l, (struct sockaddr *)buffer, (socklen_t *)&cell);
            order_row("O6 accept non-blocking, nothing queued", len, r, errno,
                      cell == w ? " cell=kept" : " cell=written");
        }
    }

    // O5: an unmapped length cell, which has no length in it to vary.
    {
        int r;
        errno = 0; r = getsockname(bound, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 getsockname, cell unmapped", 0, r, errno, "");
        errno = 0; r = getsockname(closed, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 getsockname closed, cell unmapped", 0, r, errno, "");
        errno = 0; r = getpeername(connected, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 getpeername, cell unmapped", 0, r, errno, "");
        errno = 0; r = getpeername(closed, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 getpeername closed, cell unmapped", 0, r, errno, "");
        errno = 0; r = accept(closed, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 accept closed, cell unmapped", 0, r, errno, "");
        errno = 0; r = accept(pipefd[0], (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 accept pipe, cell unmapped", 0, r, errno, "");
        errno = 0; r = accept(l, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        order_row("O5 accept nothing queued, cell unmapped", 0, r, errno, "");
        int client = queue_connection(l);
        errno = 0; r = accept(l, (struct sockaddr *)buffer, (socklen_t *)unmapped);
        int e = errno;
        int again = accept(l, NULL, NULL);
        int againErrno = again >= 0 ? 0 : errno;
        char extra[64];
        snprintf(extra, sizeof extra, "; then accept=%s", en(againErrno));
        order_row("O5 accept one queued, cell unmapped", 0, r, e, extra);
        if (r >= 0) close(r);
        if (again >= 0) close(again);
        close(client);
    }

    close(bound);
    close(connected);
    close(l);
    close(pipefd[0]);
    close(pipefd[1]);
}

static void non_socket_section(void)
{
    int pipefd[2];
    pipe(pipefd);
    char path[] = "/tmp/socket-address-length-XXXXXX";
    int file = mkstemp(path);
    unlink(path);
#ifdef __linux__
    int port = epoll_create1(0);
    const char *port_name = "epoll";
#else
    int port = kqueue();
    const char *port_name = "kqueue";
#endif
    int unix_socket = socket(AF_UNIX, SOCK_STREAM, 0);
    int inet6_socket = socket(AF_INET6, SOCK_STREAM, 0);
    const int fds[5] = { pipefd[0], file, port, unix_socket, inet6_socket };
    const char *names[5] = { "pipe", "file", port_name, "AF_UNIX socket", "AF_INET6 socket" };
    const int64_t lengths[] = { -1, INT32_MIN, 0, 15, 16, 128, 129, 256 };
    for (int k = 0; k < 5; k++) {
        for (size_t i = 0; i < sizeof lengths / sizeof lengths[0]; i++) {
            socklen_t w = (socklen_t)(uint32_t)(int32_t)lengths[i];
            for (int mapped = 1; mapped >= 0; mapped--) {
                void *address = mapped ? (void *)buffer : unmapped;
                char label[64];
                int r;
                fill_sockaddr(9);
                snprintf(label, sizeof label, "O7 bind %s, %s", names[k], mapped ? "mapped" : "unmapped");
                errno = 0; r = bind(fds[k], (struct sockaddr *)address, w); order_row(label, lengths[i], r, errno, "");
                snprintf(label, sizeof label, "O7 connect %s, %s", names[k], mapped ? "mapped" : "unmapped");
                errno = 0; r = connect(fds[k], (struct sockaddr *)address, w); order_row(label, lengths[i], r, errno, "");
            }
        }
    }
    close(pipefd[0]);
    close(pipefd[1]);
    close(file);
    close(port);
    close(unix_socket);
    close(inet6_socket);
}

// ---------------------------------------------------------------------------
// Section B: a blocking accept that sleeps, then is handed a connection.

static void blocking_section(void)
{
    const int64_t lengths[] = { -1, INT32_MIN, 16 };
    for (size_t i = 0; i < sizeof lengths / sizeof lengths[0]; i++) {
        int64_t len = lengths[i];
        int l = listener(0);
        uint16_t port = port_of(l);
        int sync[2];
        pipe(sync);
        pid_t child = fork();
        if (child == 0) {
            usleep(200 * 1000);
            int c = connect_to(port);
            char view[128];
            client_view(view, sizeof view, c);
            // Hold the connection open until the parent has looked.
            write(sync[1], view, strlen(view) + 1);
            usleep(300 * 1000);
            _exit(0);
        }
        uint32_t cell = (uint32_t)(int32_t)len;
        memset(buffer, 0xAA, 4096);
        int64_t start = now_ms();
        errno = 0;
        int r = accept(l, (struct sockaddr *)buffer, (socklen_t *)&cell);
        int e = r >= 0 ? 0 : errno;
        int64_t elapsed = now_ms() - start;
        char view[128] = "";
        if (r >= 0)
            close(r);
        read(sync[0], view, sizeof view);
        fcntl(l, F_SETFL, O_NONBLOCK);
        int again = accept(l, NULL, NULL);
        int againErrno = again >= 0 ? 0 : errno;
        printf("B blocking accept len=%-11d %s after ~%lld ms; written=%d; then accept=%s; client %s\n", (int32_t)len,
               r >= 0 ? "OK" : en(e), (long long)(elapsed / 100 * 100), sentinel_overwritten(), en(againErrno), view);
        if (again >= 0)
            close(again);
        waitpid(child, NULL, 0);
        close(l);
        close(sync[0]);
        close(sync[1]);
    }
}

int main(void)
{
    alarm(600);
    signal(SIGPIPE, SIG_IGN);
    setvbuf(stdout, NULL, _IOLBF, 0);

    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);

    buffer = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    unmapped = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (buffer == MAP_FAILED || unmapped == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    build_words();
    printf("# %d words swept\n", word_count);

    sweep("bind", sweep_bind_, 0);

    connect_listener = listener(0);
    sweep("connect", sweep_connect_, 0);
    close(connect_listener);

    name_socket = bound_socket();
    sweep("getsockname", sweep_name, 0);
    {
        int l = listener(0);
        peer_socket = queue_connection(l);
        sweep("getpeername", sweep_name, 1);
        close(peer_socket);
        close(l);
    }
    close(name_socket);

    accept_listener = listener(1);
    sweep("accept", sweep_accept, 0);
#ifdef __linux__
    sweep("accept4", sweep_accept, 1);
#endif
    close(accept_listener);

    order_section();
    non_socket_section();
    blocking_section();
    return 0;
}
