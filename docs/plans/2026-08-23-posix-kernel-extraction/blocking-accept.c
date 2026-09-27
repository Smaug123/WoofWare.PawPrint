// A blocking accept(2) on an IPv4 stream listener with an empty queue: what
// wakes it, which of several waiters a connection wakes, and what closing,
// shutting down, or reconfiguring the listener does to a waiter.
//
// Every listener is bound to 127.0.0.1 on an ephemeral port with a backlog of
// 16. A "connect" is a blocking connect(2) from a fresh socket, which completes
// at once on loopback. A waiter that nothing else ends is interrupted by
// SIGUSR1, whose handler is installed without SA_RESTART, so "EINTR" in a
// result means "still blocked when interrupted". Elapsed times are
// CLOCK_MONOTONIC, in milliseconds.
//
// Sections:
//   A  one waiter, a connect 50 ms in: rv, elapsed, the peer address and
//      length reported, and the accepted descriptor's O_NONBLOCK.
//   B  several waiters on one listener:
//      B1 three threads park 30 ms apart in the order given, then three
//         connects 100 ms apart: which thread returns after each; 10 trials
//         of each of the six orders.
//      B2 two threads that accept again as soon as they return, six connects
//         100 ms apart: the order of returns, 20 trials.
//      B3 two threads parked, two connects back to back: how many threads
//         return within 100 ms, 50 trials.
//      B4 one blocked accepter and one blocked poll(POLLIN) on the listener,
//         one connect: which return, 20 trials.
//   C  the listener closed by another thread 50 ms into a wait: whether the
//      waiter returns within 300 ms; then a connect to its port, and whether
//      the waiter returns within 300 ms of that. C1 with no other descriptor,
//      C2 with a dup kept open, C3 closing the dup and keeping the descriptor
//      the waiter entered through.
//   D  shutdown(2) of the listener 50 ms into a wait, for SHUT_RD, SHUT_WR and
//      SHUT_RDWR: shutdown's rv, whether the waiter returns within 300 ms and
//      with what, then a connect to the port, and a further non-blocking
//      accept. D0 is the same shutdowns with no waiter.
//   E  SO_RCVTIMEO: E1 100 ms set before a blocking accept on an empty queue;
//      E2 100 ms set 50 ms into a wait begun without it; E3 100 ms set on a
//      listener with a connection already queued. rv and elapsed, each wait
//      interrupted at 1000 ms.
//   F  O_NONBLOCK set on the listener's description 50 ms into a wait: whether
//      the waiter returns within 300 ms; then a connect, and the accepted
//      descriptor's O_NONBLOCK.
//   G  the length cell rewritten 50 ms into a wait, from 16 to 4 and to 0,
//      then a connect: the length reported and how many of 16 buffer bytes
//      were written.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -pthread -o /tmp/ba blocking-accept.c && /tmp/ba
//   Linux:  container run --rm -v "$PWD:/probe" gcc:14 sh -c
//             "gcc -Wall -O1 -pthread -o /tmp/ba /probe/blocking-accept.c && /tmp/ba"
//
// Measured 2026-09-27 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14
// image, glibc 2.41), twice in full and once as far as section D, and on
// Darwin 27.0.0 arm64 three times (C3 in the last two), with the same answers
// every time bar B4's split; E's constants are from the last run on each:
//   A  both: rv a descriptor ~50 ms in, the client's address as the peer,
//      length 16, the accepted descriptor blocking.
//   B  both: B1 each connection woke exactly one thread, the one that parked
//      *first*, in all 60 trials (10 of each of the six park orders): FIFO,
//      where epoll_wait is LIFO. B2 all 20 trials returned 010101: a thread
//      that accepts again goes to the back. B3 two connects back to back woke
//      both threads in all 50 trials. B4 the accepter returned every time, and
//      the poller with it in 15-20 of 20 (it rescans, and loses the race when
//      the accepter has dequeued first).
//   C  Linux: C1 the close does not wake the waiter; the port still listens,
//      the connect succeeds, and the accept returns it. C2 and C3 likewise.
//      Darwin: C1 and C2 end the wait at once with ECONNABORTED (C2 even
//      though the dup keeps the listener, whose port a connect still reaches);
//      C3, closing the dup, changes nothing, and a connect completes the wait.
//   D  Linux: shutdown(SHUT_RD) and (SHUT_RDWR) of a listener succeed, end a
//      waiter with EINVAL, stop the port listening (a connect is refused), and
//      every later accept is EINVAL; SHUT_WR succeeds and changes nothing.
//      Darwin: every shutdown of a listener is ENOTCONN, waiter or not, and
//      changes nothing.
//   E  SOL_SOCKET/SO_RCVTIMEO are 1/20 on Linux and 0xffff/0x1006 on Darwin.
//      Linux: E1 EAGAIN at ~100 ms; E2, set while the wait is under way, has
//      no effect on it (still blocked at 1000 ms). Darwin: neither has any
//      effect on accept. E3 both: a queued connection is returned at once.
//   F  both: O_NONBLOCK set while the accept sleeps does not wake it. The
//      accepted descriptor is then blocking on Linux and non-blocking on
//      Darwin: Darwin copies the listener's flag as it stands when the call
//      returns.
//   G  Linux reads the length cell when it copies the address out, after the
//      wait: rewritten to 4 it wrote 4 bytes, to 0 none. Darwin reads it when
//      the call is entered: 16 bytes both times. Both report 16.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

static int64_t now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static void on_usr1(int sig) { (void)sig; }

static int listener(struct sockaddr_in *addr)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0) { perror("socket"); exit(1); }
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) { perror("bind"); exit(1); }
    if (listen(s, 16) != 0) { perror("listen"); exit(1); }
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    if (addr) *addr = a;
    return s;
}

// A blocking connect from a fresh socket: the socket, or -1 with errno.
static int connect_to(const struct sockaddr_in *addr, int *err)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(c, (const struct sockaddr *)addr, sizeof *addr) != 0) {
        *err = errno;
        close(c);
        return -1;
    }
    *err = 0;
    return c;
}

static const char *errname(int e)
{
    switch (e) {
    case 0: return "0";
    case EAGAIN: return "EAGAIN";
    case EINTR: return "EINTR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ECONNABORTED: return "ECONNABORTED";
    case ECONNREFUSED: return "ECONNREFUSED";
    case ENOTCONN: return "ENOTCONN";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ENOTSOCK: return "ENOTSOCK";
    case ETIMEDOUT: return "ETIMEDOUT";
    default: return strerror(e);
    }
}

// One blocked accept, run on its own thread.
struct waiter {
    int fd;
    int id;
    socklen_t len;               // the length cell, which the main thread may rewrite
    unsigned char buf[16];
    pthread_t thread;
    atomic_int done;
    int rv;
    int err;
    int64_t started;
    int64_t finished;
    int nonblock;                // of the accepted descriptor
    int again;                   // accept again after returning (B2)
};

#define RETURN_LOG 64
static atomic_int return_count;
static int return_log[RETURN_LOG];

static void *accept_thread(void *p)
{
    struct waiter *w = p;
    for (;;) {
        w->started = now_ms();
        int rv = accept(w->fd, (struct sockaddr *)w->buf, &w->len);
        w->err = rv < 0 ? errno : 0;
        w->rv = rv;
        w->finished = now_ms();
        if (rv >= 0) w->nonblock = (fcntl(rv, F_GETFL) & O_NONBLOCK) != 0;
        int n = atomic_fetch_add(&return_count, 1);
        if (n < RETURN_LOG) return_log[n] = w->id;
        if (!w->again || rv < 0) break;
        w->len = sizeof w->buf;
    }
    atomic_store(&w->done, 1);
    return NULL;
}

static void start(struct waiter *w, int fd, int id)
{
    memset(w, 0, sizeof *w);
    w->fd = fd;
    w->id = id;
    w->len = sizeof w->buf;
    memset(w->buf, 0xAA, sizeof w->buf);
    pthread_create(&w->thread, NULL, accept_thread, w);
}

// Interrupt `w` if it is still blocked, and join it.
static void finish(struct waiter *w)
{
    if (!atomic_load(&w->done)) pthread_kill(w->thread, SIGUSR1);
    pthread_join(w->thread, NULL);
}

static void describe(const char *label, struct waiter *w)
{
    if (!atomic_load(&w->done)) {
        printf("%s still_blocked\n", label);
        return;
    }
    printf("%s rv=%s errno=%s after=%lldms", label, w->rv >= 0 ? "fd" : "-1", errname(w->err),
           (long long)(w->finished - w->started));
    if (w->rv >= 0) printf(" nonblock=%d", w->nonblock);
    printf("\n");
}

static void section_a(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    struct waiter w;
    start(&w, l, 0);
    sleep_ms(50);
    int err;
    int c = connect_to(&addr, &err);
    sleep_ms(100);
    finish(&w);
    describe("A", &w);
    struct sockaddr_in own;
    socklen_t ownlen = sizeof own;
    getsockname(c, (struct sockaddr *)&own, &ownlen);
    struct sockaddr_in *peer = (struct sockaddr_in *)w.buf;
    printf("A reported_len=%d peer_is_client=%d\n", (int)w.len,
           peer->sin_port == own.sin_port && peer->sin_addr.s_addr == own.sin_addr.s_addr);
    if (w.rv >= 0) close(w.rv);
    close(c);
    close(l);
}

static void section_b1(void)
{
    static const int orders[6][3] = { {0,1,2}, {0,2,1}, {1,0,2}, {1,2,0}, {2,0,1}, {2,1,0} };
    for (int o = 0; o < 6; o++) {
        int fifo = 0, lifo = 0, other = 0, not_one = 0;
        char first_other[64] = "";
        for (int trial = 0; trial < 10; trial++) {
            struct sockaddr_in addr;
            int l = listener(&addr);
            struct waiter w[3];
            atomic_store(&return_count, 0);
            for (int i = 0; i < 3; i++) {
                start(&w[orders[o][i]], l, orders[o][i]);
                sleep_ms(30);
            }
            int clients[3];
            int seen = 0;
            char seq[8] = "";
            for (int k = 0; k < 3; k++) {
                int err;
                clients[k] = connect_to(&addr, &err);
                sleep_ms(100);
                int n = atomic_load(&return_count);
                if (n != seen + 1) not_one++;
                for (; seen < n && seen < RETURN_LOG; seen++) {
                    char d[2] = { (char)('0' + return_log[seen]), 0 };
                    strcat(seq, d);
                }
            }
            char parked[4] = { (char)('0'+orders[o][0]), (char)('0'+orders[o][1]), (char)('0'+orders[o][2]), 0 };
            char reversed[4] = { parked[2], parked[1], parked[0], 0 };
            if (strcmp(seq, parked) == 0) fifo++;
            else if (strcmp(seq, reversed) == 0) lifo++;
            else { other++; if (!first_other[0]) snprintf(first_other, sizeof first_other, "%s", seq); }
            for (int i = 0; i < 3; i++) { finish(&w[i]); if (w[i].rv >= 0) close(w[i].rv); }
            for (int k = 0; k < 3; k++) if (clients[k] >= 0) close(clients[k]);
            close(l);
        }
        printf("B1 park_order=%d,%d,%d earliest_first=%d latest_first=%d other=%d not_one_per_connect=%d first_other=[%s]\n",
               orders[o][0], orders[o][1], orders[o][2], fifo, lifo, other, not_one, first_other);
    }
}

static void section_b2(void)
{
    int tally[RETURN_LOG] = {0};
    (void)tally;
    for (int trial = 0; trial < 20; trial++) {
        struct sockaddr_in addr;
        int l = listener(&addr);
        struct waiter w[2];
        atomic_store(&return_count, 0);
        start(&w[0], l, 0); w[0].again = 1;
        sleep_ms(30);
        start(&w[1], l, 1); w[1].again = 1;
        sleep_ms(30);
        int clients[6];
        for (int k = 0; k < 6; k++) {
            int err;
            clients[k] = connect_to(&addr, &err);
            sleep_ms(100);
        }
        int n = atomic_load(&return_count);
        char seq[RETURN_LOG + 1] = "";
        for (int i = 0; i < n && i < RETURN_LOG; i++) seq[i] = (char)('0' + return_log[i]);
        seq[n < RETURN_LOG ? n : RETURN_LOG] = 0;
        printf("B2 trial=%d returns=%s\n", trial, seq);
        // Each thread is now blocked again; interrupting it ends its loop.
        for (int i = 0; i < 2; i++) finish(&w[i]);
        for (int k = 0; k < 6; k++) if (clients[k] >= 0) close(clients[k]);
        close(l);
    }
}

static void section_b3(void)
{
    int both = 0, one = 0, none = 0;
    for (int trial = 0; trial < 50; trial++) {
        struct sockaddr_in addr;
        int l = listener(&addr);
        struct waiter w[2];
        atomic_store(&return_count, 0);
        start(&w[0], l, 0);
        sleep_ms(30);
        start(&w[1], l, 1);
        sleep_ms(30);
        int err;
        int c1 = connect_to(&addr, &err);
        int c2 = connect_to(&addr, &err);
        sleep_ms(100);
        int n = atomic_load(&return_count);
        if (n == 2) both++; else if (n == 1) one++; else none++;
        for (int i = 0; i < 2; i++) { finish(&w[i]); if (w[i].rv >= 0) close(w[i].rv); }
        close(c1); close(c2); close(l);
    }
    printf("B3 two_connects_two_waiters both_returned=%d one=%d none=%d\n", both, one, none);
}

struct poller {
    int fd;
    pthread_t thread;
    atomic_int done;
    int rv;
    short revents;
};

static void *poll_thread(void *p)
{
    struct poller *q = p;
    struct pollfd pfd = { q->fd, POLLIN, 0 };
    q->rv = poll(&pfd, 1, -1);
    q->revents = pfd.revents;
    atomic_store(&q->done, 1);
    return NULL;
}

static void section_b4(void)
{
    int accepter_and_poller = 0, accepter_only = 0, poller_only = 0, neither = 0;
    for (int trial = 0; trial < 20; trial++) {
        struct sockaddr_in addr;
        int l = listener(&addr);
        struct waiter w;
        struct poller q;
        memset(&q, 0, sizeof q);
        q.fd = l;
        start(&w, l, 0);
        sleep_ms(30);
        pthread_create(&q.thread, NULL, poll_thread, &q);
        sleep_ms(30);
        int err;
        int c = connect_to(&addr, &err);
        sleep_ms(100);
        int a = atomic_load(&w.done), p = atomic_load(&q.done);
        if (a && p) accepter_and_poller++;
        else if (a) accepter_only++;
        else if (p) poller_only++;
        else neither++;
        finish(&w);
        if (!atomic_load(&q.done)) pthread_kill(q.thread, SIGUSR1);
        pthread_join(q.thread, NULL);
        if (w.rv >= 0) close(w.rv);
        close(c); close(l);
    }
    printf("B4 accepter_then_poller both=%d accepter_only=%d poller_only=%d neither=%d\n",
           accepter_and_poller, accepter_only, poller_only, neither);
}

// mode 0: close the only descriptor; 1: close the one the waiter entered
// through, keeping a dup; 2: close the dup, keeping the one it entered through.
static void section_c(int mode)
{
    const char *label = mode == 0 ? "C1" : mode == 1 ? "C2" : "C3";
    struct sockaddr_in addr;
    int l = listener(&addr);
    int dup_fd = mode != 0 ? dup(l) : -1;
    struct waiter w;
    start(&w, l, 0);
    sleep_ms(50);
    int crv;
    if (mode == 2) { crv = close(dup_fd); dup_fd = l; } else crv = close(l);
    printf("%s close rv=%d\n", label, crv);
    sleep_ms(300);
    char buf[64];
    snprintf(buf, sizeof buf, "%s after_close", label);
    describe(buf, &w);
    int err;
    int c = connect_to(&addr, &err);
    printf("%s connect rv=%s errno=%s\n", label, c >= 0 ? "0" : "-1", errname(err));
    sleep_ms(300);
    snprintf(buf, sizeof buf, "%s after_connect", label);
    describe(buf, &w);
    finish(&w);
    snprintf(buf, sizeof buf, "%s final", label);
    describe(buf, &w);
    if (w.rv >= 0) close(w.rv);
    if (c >= 0) close(c);
    if (dup_fd >= 0) close(dup_fd);
}

static void section_d(void)
{
    static const int hows[3] = { SHUT_RD, SHUT_WR, SHUT_RDWR };
    static const char *names[3] = { "SHUT_RD", "SHUT_WR", "SHUT_RDWR" };
    for (int h = 0; h < 3; h++) {
        struct sockaddr_in addr;
        int l = listener(&addr);
        int rv = shutdown(l, hows[h]);
        printf("D0 %s no_waiter shutdown rv=%d errno=%s\n", names[h], rv, errname(rv < 0 ? errno : 0));
        close(l);
    }
    for (int h = 0; h < 3; h++) {
        struct sockaddr_in addr;
        int l = listener(&addr);
        struct waiter w;
        start(&w, l, 0);
        sleep_ms(50);
        int rv = shutdown(l, hows[h]);
        printf("D %s shutdown rv=%d errno=%s\n", names[h], rv, errname(rv < 0 ? errno : 0));
        sleep_ms(300);
        char buf[64];
        snprintf(buf, sizeof buf, "D %s after_shutdown", names[h]);
        describe(buf, &w);
        int err;
        int c = connect_to(&addr, &err);
        printf("D %s connect rv=%s errno=%s\n", names[h], c >= 0 ? "0" : "-1", errname(err));
        sleep_ms(300);
        snprintf(buf, sizeof buf, "D %s after_connect", names[h]);
        describe(buf, &w);
        finish(&w);
        if (w.rv >= 0) close(w.rv);
        fcntl(l, F_SETFL, fcntl(l, F_GETFL) | O_NONBLOCK);
        int a = accept(l, NULL, NULL);
        printf("D %s later_nonblocking_accept rv=%s errno=%s\n", names[h], a >= 0 ? "fd" : "-1",
               errname(a < 0 ? errno : 0));
        if (a >= 0) close(a);
        if (c >= 0) close(c);
        close(l);
    }
}

static void set_rcvtimeo(int fd, int ms)
{
    struct timeval tv = { ms / 1000, (ms % 1000) * 1000 };
    if (setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv) != 0) perror("setsockopt SO_RCVTIMEO");
}

// Interrupt `w` at `ms` after it started, unless it has returned.
static void wait_then_interrupt(struct waiter *w, int ms)
{
    int64_t deadline = w->started + ms;
    while (!atomic_load(&w->done) && now_ms() < deadline) sleep_ms(5);
    finish(w);
}

static void section_e(void)
{
    printf("E SOL_SOCKET=%#x SO_RCVTIMEO=%#x\n", SOL_SOCKET, SO_RCVTIMEO);
    {
        struct sockaddr_in addr;
        int l = listener(&addr);
        set_rcvtimeo(l, 100);
        struct waiter w;
        start(&w, l, 0);
        sleep_ms(10);
        wait_then_interrupt(&w, 1000);
        describe("E1 rcvtimeo_before", &w);
        close(l);
    }
    {
        struct sockaddr_in addr;
        int l = listener(&addr);
        struct waiter w;
        start(&w, l, 0);
        sleep_ms(50);
        set_rcvtimeo(l, 100);
        wait_then_interrupt(&w, 1000);
        describe("E2 rcvtimeo_mid_wait", &w);
        close(l);
    }
    {
        struct sockaddr_in addr;
        int l = listener(&addr);
        set_rcvtimeo(l, 100);
        int err;
        int c = connect_to(&addr, &err);
        struct waiter w;
        start(&w, l, 0);
        sleep_ms(10);
        wait_then_interrupt(&w, 1000);
        describe("E3 rcvtimeo_queued", &w);
        if (w.rv >= 0) close(w.rv);
        close(c);
        close(l);
    }
}

static void section_f(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    struct waiter w;
    start(&w, l, 0);
    sleep_ms(50);
    fcntl(l, F_SETFL, fcntl(l, F_GETFL) | O_NONBLOCK);
    sleep_ms(300);
    describe("F after_setfl", &w);
    int err;
    int c = connect_to(&addr, &err);
    sleep_ms(300);
    describe("F after_connect", &w);
    finish(&w);
    if (w.rv >= 0) close(w.rv);
    if (c >= 0) close(c);
    close(l);
}

static void section_g(socklen_t rewritten)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    struct waiter w;
    start(&w, l, 0);
    sleep_ms(50);
    w.len = rewritten;
    int err;
    int c = connect_to(&addr, &err);
    sleep_ms(200);
    finish(&w);
    int written = 0;
    for (int i = 0; i < 16; i++) if (w.buf[i] != 0xAA) written = i + 1;
    printf("G 16_to_%d rv=%s errno=%s reported_len=%d bytes_written_upto=%d\n", (int)rewritten,
           w.rv >= 0 ? "fd" : "-1", errname(w.err), (int)w.len, written);
    if (w.rv >= 0) close(w.rv);
    if (c >= 0) close(c);
    close(l);
}

int main(void)
{
    alarm(300);
    setvbuf(stdout, NULL, _IONBF, 0);
    signal(SIGPIPE, SIG_IGN);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sigaction(SIGUSR1, &sa, NULL);

    section_a();
    section_b1();
    section_b2();
    section_b3();
    section_b4();
    section_c(0);
    section_c(1);
    section_c(2);
    section_d();
    section_e();
    section_f();
    section_g(4);
    section_g(0);
    return 0;
}
