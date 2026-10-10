// recv(2) and send(2) on a connected loopback TCP socket, on Linux and Darwin,
// beyond what tcp-transfer.c's section S measured: the flag word's numbering,
// where recv and send look at the buffer and the descriptor, and what
// MSG_PEEK, MSG_DONTWAIT and MSG_NOSIGNAL do on a blocking socket and to a
// call that sleeps.
//
// c is the connecting socket and p the accepted one, both blocking unless
// stated. Signals are SIGUSR1, sent by pthread_kill to the sleeping thread,
// whose handler counts them; "restart" means it was installed with
// SA_RESTART. SIGPIPE is caught and counted. Each line is tab-separated, its
// first field the section.
//
// Sections:
//   H   the flavour's <sys/socket.h> numbers for every MSG_* it defines.
//   O   the order of the checks: recv and send through a descriptor nothing
//       holds, a regular file, each end of a pipe, and a connected socket
//       with nothing queued (O_NONBLOCK set), each through a mapped buffer,
//       NULL and (void*)-1, at lengths 4 and 0.
//   P   MSG_PEEK:
//       P-sleep     a blocking recv(c, 100, MSG_PEEK) with nothing queued;
//                   100 ms in, p writes 10 bytes. The peek's answer, then
//                   FIONREAD(c), then a recv(c, 100, 0).
//       P-zero      as P-sleep, but recv(c, 0, MSG_PEEK) and recv(c, 0, 0),
//                   and non-blocking recv(c, 0, MSG_PEEK) beforehand.
//       P-eof       p writes 1000 bytes and closes: a peek, a recv, then a
//                   peek, blocking; and a blocking peek asleep when p closes
//                   with nothing sent.
//       P-reset     a blocking peek asleep when p closes with 100 bytes from
//                   c unread; then recv(c) and SO_ERROR.
//       P-eintr     a blocking peek asleep, signalled, without SA_RESTART
//                   and with it (100 ms later p writes 10 bytes).
//   Z   recv(c, 0, 0) on a blocking socket with nothing queued: whether it
//       sleeps, and what it returns once p writes 10 bytes 100 ms in.
//   D   MSG_DONTWAIT on a blocking socket: recv with nothing queued, and a
//       65536-byte send, idle; then a send(c, 65536, MSG_DONTWAIT) once c
//       has been filled to EAGAIN (on another thread: whether it sleeps, and
//       its answer once p drains). On Linux, sends with MSG_DONTWAIT in
//       65536-byte pieces until EAGAIN (the total, and the last short
//       count), with an edge-triggered EPOLLOUT registration on c made
//       before the fill, and the edges it reports as p drains.
//   N   MSG_NOSIGNAL:
//       N-recv      recv(c, 100, MSG_NOSIGNAL | MSG_DONTWAIT) with nothing
//                   queued, then with 10 bytes queued.
//       N-partial   a blocking send(c, 8 MiB, MSG_NOSIGNAL) asleep having
//                   taken some; p closes with bytes unread. The answer and
//                   SIGPIPE.
//       N-empty     c is filled to EAGAIN; a blocking send(c, 1000,
//                   MSG_NOSIGNAL) sleeps; p closes with bytes unread.
//       N-plain     N-partial and N-empty again without MSG_NOSIGNAL.
//   W   F_GETFL of c before and after a write(2) of 10 bytes, a send(2) of
//       10, and a send of 0, each on a fresh pair: whether a send marks the
//       description written, as Darwin's write does (FWASWRITTEN, 0x10000).
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -pthread -o /tmp/tcp-recv-send tcp-recv-send.c && /tmp/tcp-recv-send
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -pthread -o /tmp/p /probe/tcp-recv-send.c && /tmp/p'
// An argument restricts the run to the sections it names, e.g. `HO`; the
// default is all of them.
//
// Measured 2026-10-08 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// glibc 2.41, default sysctls) and Darwin 27.0.0 arm64 (uid 501), twice each;
// one run of each is tcp-recv-send.linux-6.18.5-aarch64.txt and
// tcp-recv-send.darwin-27.0.txt. The two runs differed only in how much a
// fill took. What they say, on both unless stated:
//   H   the shared flags are MSG_OOB 0x1, MSG_PEEK 0x2 and MSG_DONTROUTE 0x4;
//       the rest are numbered per flavour (MSG_DONTWAIT is 0x40 on Linux and
//       0x80 on Darwin, MSG_NOSIGNAL 0x4000 and 0x80000).
//   O   Linux screens the buffer first: (void*)-1 is EFAULT ahead of EBADF
//       and ENOTSOCK, at length 0 too, and NULL passes. Darwin screens
//       nothing: EBADF, then ENOTSOCK, then the socket's own answer. A
//       non-socket is ENOTSOCK on both. On an idle connected socket a
//       recv(0) is EAGAIN on Linux and 0 on Darwin, and a send whose buffer
//       faults is EFAULT, taking nothing.
//   P   a blocking MSG_PEEK sleeps until bytes arrive and answers them
//       without taking them (FIONREAD still 10); asleep at a FIN it answers
//       0; asleep at a reset ECONNRESET, which it takes on Linux and leaves
//       pending on Darwin. A signal gives EINTR, or under SA_RESTART the peek
//       sleeps on. recv(0, MSG_PEEK) is recv(0)'s answer.
//   Z   a blocking recv(0) with nothing queued sleeps on Linux until bytes
//       arrive, then returns 0; on Darwin it returns 0 at once.
//   D   MSG_DONTWAIT on a blocking socket: recv answers EAGAIN on both. Linux's
//       send takes what fits and answers EAGAIN when nothing does, its last
//       short count 4736, and arms the edge-triggered EPOLLOUT edge as
//       O_NONBLOCK does (one edge as the peer drains). Darwin's send ignores
//       it: with no room it sleeps, and returns 65536 once the peer drains.
//       Neither sets O_NONBLOCK.
//   N   MSG_NOSIGNAL changes nothing about a receive. On a send asleep when
//       its connection is reset it suppresses Darwin's SIGPIPE (EPIPE either
//       way); Linux raises none either way (the count taken, or ECONNRESET).
//   W   Darwin's write marks its description FWASWRITTEN (F_GETFL 0x10000);
//       its send does not. Linux shows neither.

#ifdef __linux__
#define _GNU_SOURCE
#endif

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#ifdef __linux__
#include <sys/epoll.h>
#endif

static atomic_int sigpipes;
static atomic_int sigusr1s;

static void on_sigpipe(int signo)
{
    (void)signo;
    atomic_fetch_add(&sigpipes, 1);
}

static void on_sigusr1(int signo)
{
    (void)signo;
    atomic_fetch_add(&sigusr1s, 1);
}

static void die(const char *what)
{
    perror(what);
    exit(2);
}

static void install(int restart)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_sigusr1;
    sa.sa_flags = restart ? SA_RESTART : 0;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGUSR1, &sa, NULL) < 0) die("sigaction SIGUSR1");
    sa.sa_handler = on_sigpipe;
    sa.sa_flags = 0;
    if (sigaction(SIGPIPE, &sa, NULL) < 0) die("sigaction SIGPIPE");
}

static const char *errname(int e)
{
    static char other[4][32];
    static int next;
    switch (e)
    {
        case 0: return "0";
        case EAGAIN: return "EAGAIN";
        case EPIPE: return "EPIPE";
        case ECONNRESET: return "ECONNRESET";
        case ENOTCONN: return "ENOTCONN";
        case ENOTSOCK: return "ENOTSOCK";
        case EINTR: return "EINTR";
        case EBADF: return "EBADF";
        case EINVAL: return "EINVAL";
        case EFAULT: return "EFAULT";
        case EOPNOTSUPP: return "EOPNOTSUPP";
        default:
        {
            char *out = other[next++ % 4];
            snprintf(out, 32, "errno%d", e);
            return out;
        }
    }
}

static int64_t now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000 };
    while (nanosleep(&ts, &ts) < 0 && errno == EINTR) { }
}

static void set_nonblocking(int fd, int on)
{
    int flags = fcntl(fd, F_GETFL);
    if (flags < 0) die("F_GETFL");
    flags = on ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK);
    if (fcntl(fd, F_SETFL, flags) < 0) die("F_SETFL");
}

static int is_nonblocking(int fd)
{
    return (fcntl(fd, F_GETFL) & O_NONBLOCK) != 0;
}

static int so_error(int fd)
{
    int v = -1;
    socklen_t len = sizeof v;
    if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &v, &len) < 0) return -1000 - errno;
    return v;
}

static int nread(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) < 0) return -1000 - errno;
    return n;
}

// A connected blocking pair over 127.0.0.1: *c connecting, *p accepted.
static void make_pair(int *c, int *p)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    if (l < 0) die("socket");
    int one = 1;
    setsockopt(l, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, 8) < 0) die("listen");
    socklen_t al = sizeof a;
    if (getsockname(l, (struct sockaddr *)&a, &al) < 0) die("getsockname");
    *c = socket(AF_INET, SOCK_STREAM, 0);
    if (*c < 0) die("socket");
    if (connect(*c, (struct sockaddr *)&a, sizeof a) < 0) die("connect");
    *p = accept(l, NULL, NULL);
    if (*p < 0) die("accept");
    close(l);
}

static char payload[8 << 20];
static char sink[1 << 22];

// One call's answer, as text.
static const char *answer(ssize_t rv, int error, char *out, size_t len)
{
    if (rv < 0)
        snprintf(out, len, "-1 %s", errname(error));
    else
        snprintf(out, len, "%zd", rv);
    return out;
}

typedef struct
{
    int send;      // 0: recv, 1: send
    int fd;
    size_t len;
    int flags;
    atomic_int started;
    atomic_int done;
    ssize_t rv;
    int error;
    int64_t elapsed;
} call;

static void *run_call(void *arg)
{
    call *k = arg;
    int64_t begun = now_ms();
    atomic_store(&k->started, 1);
    errno = 0;
    if (k->send)
        k->rv = send(k->fd, payload, k->len, k->flags);
    else
        k->rv = recv(k->fd, sink, k->len, k->flags);
    k->error = k->rv < 0 ? errno : 0;
    k->elapsed = now_ms() - begun;
    atomic_store(&k->done, 1);
    return NULL;
}

static void start(call *k, pthread_t *t, int send, int fd, size_t len, int flags)
{
    memset(k, 0, sizeof *k);
    k->send = send;
    k->fd = fd;
    k->len = len;
    k->flags = flags;
    atomic_init(&k->started, 0);
    atomic_init(&k->done, 0);
    if (pthread_create(t, NULL, run_call, k) != 0) die("pthread_create");
    while (!atomic_load(&k->started)) { }
}

static const char *state(call *k, char *out, size_t len)
{
    if (!atomic_load(&k->done))
        snprintf(out, len, "asleep");
    else
        answer(k->rv, k->error, out, len);
    return out;
}

// Fill fd's buffers non-blocking until three EAGAINs in a row, 50 ms apart,
// take nothing. Returns the bytes taken.
static long long fill(int fd)
{
    set_nonblocking(fd, 1);
    long long total = 0;
    int quiet = 0;
    while (quiet < 3)
    {
        ssize_t n = write(fd, payload, 65536);
        if (n > 0) { total += n; quiet = 0; continue; }
        if (errno != EAGAIN) die("fill");
        quiet++;
        sleep_ms(50);
        n = write(fd, payload, 65536);
        if (n > 0) { total += n; quiet = 0; }
    }
    set_nonblocking(fd, 0);
    return total;
}

static void section_h(void)
{
#define FLAG(name) printf("H\t%s\t0x%x\n", #name, (unsigned)(name))
#ifdef MSG_OOB
    FLAG(MSG_OOB);
#endif
#ifdef MSG_PEEK
    FLAG(MSG_PEEK);
#endif
#ifdef MSG_DONTROUTE
    FLAG(MSG_DONTROUTE);
#endif
#ifdef MSG_EOR
    FLAG(MSG_EOR);
#endif
#ifdef MSG_TRUNC
    FLAG(MSG_TRUNC);
#endif
#ifdef MSG_CTRUNC
    FLAG(MSG_CTRUNC);
#endif
#ifdef MSG_WAITALL
    FLAG(MSG_WAITALL);
#endif
#ifdef MSG_DONTWAIT
    FLAG(MSG_DONTWAIT);
#endif
#ifdef MSG_NOSIGNAL
    FLAG(MSG_NOSIGNAL);
#endif
#ifdef MSG_PROXY
    FLAG(MSG_PROXY);
#endif
#ifdef MSG_FIN
    FLAG(MSG_FIN);
#endif
#ifdef MSG_SYN
    FLAG(MSG_SYN);
#endif
#ifdef MSG_CONFIRM
    FLAG(MSG_CONFIRM);
#endif
#ifdef MSG_RST
    FLAG(MSG_RST);
#endif
#ifdef MSG_ERRQUEUE
    FLAG(MSG_ERRQUEUE);
#endif
#ifdef MSG_MORE
    FLAG(MSG_MORE);
#endif
#ifdef MSG_WAITFORONE
    FLAG(MSG_WAITFORONE);
#endif
#ifdef MSG_BATCH
    FLAG(MSG_BATCH);
#endif
#ifdef MSG_SOCK_DEVMEM
    FLAG(MSG_SOCK_DEVMEM);
#endif
#ifdef MSG_ZEROCOPY
    FLAG(MSG_ZEROCOPY);
#endif
#ifdef MSG_FASTOPEN
    FLAG(MSG_FASTOPEN);
#endif
#ifdef MSG_CMSG_CLOEXEC
    FLAG(MSG_CMSG_CLOEXEC);
#endif
#ifdef MSG_EOF
    FLAG(MSG_EOF);
#endif
#ifdef MSG_WAITSTREAM
    FLAG(MSG_WAITSTREAM);
#endif
#ifdef MSG_FLUSH
    FLAG(MSG_FLUSH);
#endif
#ifdef MSG_HOLD
    FLAG(MSG_HOLD);
#endif
#ifdef MSG_SEND
    FLAG(MSG_SEND);
#endif
#ifdef MSG_HAVEMORE
    FLAG(MSG_HAVEMORE);
#endif
#ifdef MSG_RCVMORE
    FLAG(MSG_RCVMORE);
#endif
#ifdef MSG_NEEDSA
    FLAG(MSG_NEEDSA);
#endif
#undef FLAG
}

static void section_o(void)
{
    install(0);
    char path[] = "/tmp/tcp-recv-send-XXXXXX";
    int file = mkstemp(path);
    if (file < 0) die("mkstemp");
    unlink(path);
    if (write(file, "hello", 5) != 5) die("file write");
    lseek(file, 0, SEEK_SET);
    int pipefd[2];
    if (pipe(pipefd) < 0) die("pipe");
    int c, p;
    make_pair(&c, &p);
    set_nonblocking(c, 1);
    int bad = 900;
    close(bad);

    struct { const char *name; int fd; } targets[] = {
        { "badfd", bad },
        { "file", file },
        { "pipe-read", pipefd[0] },
        { "pipe-write", pipefd[1] },
        { "socket-idle", c },
    };
    struct { const char *name; void *ptr; } buffers[] = {
        { "mapped", sink },
        { "null", NULL },
        { "minus1", (void *)-1 },
    };
    size_t lengths[] = { 4, 0 };
    for (size_t t = 0; t < sizeof targets / sizeof targets[0]; t++)
        for (size_t b = 0; b < sizeof buffers / sizeof buffers[0]; b++)
            for (size_t l = 0; l < 2; l++)
            {
                char r[64], s[64];
                atomic_store(&sigpipes, 0);
                errno = 0;
                ssize_t rv = recv(targets[t].fd, buffers[b].ptr, lengths[l], 0);
                answer(rv, errno, r, sizeof r);
                errno = 0;
                // A mapped send buffer is the payload, which is readable.
                void *src = b == 0 ? (void *)payload : buffers[b].ptr;
                rv = send(targets[t].fd, src, lengths[l], 0);
                answer(rv, errno, s, sizeof s);
                printf("O\t%s\t%s\tlen=%zu\trecv=%s\tsend=%s\tsigpipe=%d\n", targets[t].name, buffers[b].name, lengths[l], r,
                       s, atomic_load(&sigpipes));
            }
    // What the socket-idle sends left for p.
    sleep_ms(20);
    printf("O\tp-fionread-after\t%d\n", nread(p));
    close(c);
    close(p);
    close(pipefd[0]);
    close(pipefd[1]);
    close(file);
}

static void section_p(void)
{
    char s1[64], s2[64], s3[64], s4[64];

    // P-sleep
    {
        install(0);
        int c, p;
        make_pair(&c, &p);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 100, MSG_PEEK);
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        if (write(p, payload, 10) != 10) die("P-sleep write");
        pthread_join(t, NULL);
        int queued = nread(c);
        ssize_t rv = recv(c, sink, 100, 0);
        printf("P\tP-sleep\tbefore=%s\tpeek=%s\telapsed_ge100=%d\tfionread=%d\trecv=%s\n", s1, state(&k, s2, sizeof s2),
               k.elapsed >= 100, queued, answer(rv, errno, s3, sizeof s3));
        close(c);
        close(p);
    }

    // P-zero
    for (int flags = 0; flags < 2; flags++)
    {
        int f = flags ? MSG_PEEK : 0;
        install(0);
        int c, p;
        make_pair(&c, &p);
        set_nonblocking(c, 1);
        errno = 0;
        ssize_t nb = recv(c, sink, 0, f);
        answer(nb, errno, s4, sizeof s4);
        set_nonblocking(c, 0);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 0, f);
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        if (write(p, payload, 10) != 10) die("P-zero write");
        sleep_ms(100);
        state(&k, s2, sizeof s2);
        if (!atomic_load(&k.done)) { close(p); p = -1; sleep_ms(100); }
        pthread_join(t, NULL);
        printf("P\tP-zero\tflags=%s\tnonblocking=%s\tbefore=%s\tafter_write=%s\tfinally=%s\tfionread=%d\n",
               flags ? "MSG_PEEK" : "0", s4, s1, s2, state(&k, s3, sizeof s3), nread(c));
        close(c);
        if (p >= 0) close(p);
    }

    // P-eof
    {
        install(0);
        int c, p;
        make_pair(&c, &p);
        if (write(p, payload, 1000) != 1000) die("P-eof write");
        close(p);
        sleep_ms(20);
        errno = 0;
        ssize_t a = recv(c, sink, 4096, MSG_PEEK);
        answer(a, errno, s1, sizeof s1);
        errno = 0;
        ssize_t b = recv(c, sink, 4096, 0);
        answer(b, errno, s2, sizeof s2);
        errno = 0;
        ssize_t d = recv(c, sink, 4096, MSG_PEEK);
        answer(d, errno, s3, sizeof s3);
        printf("P\tP-eof\tpeek=%s\trecv=%s\tpeek_again=%s\tso_error=%s\n", s1, s2, s3, errname(so_error(c)));
        close(c);

        make_pair(&c, &p);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 100, MSG_PEEK);
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        close(p);
        pthread_join(t, NULL);
        errno = 0;
        ssize_t again = recv(c, sink, 100, MSG_PEEK);
        printf("P\tP-eof-asleep\tbefore=%s\tpeek=%s\tpeek_again=%s\n", s1, state(&k, s2, sizeof s2),
               answer(again, errno, s3, sizeof s3));
        close(c);
    }

    // P-reset
    {
        install(0);
        int c, p;
        make_pair(&c, &p);
        if (write(c, payload, 100) != 100) die("P-reset write");
        sleep_ms(20);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 100, MSG_PEEK);
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        close(p);
        pthread_join(t, NULL);
        int pending = so_error(c);
        set_nonblocking(c, 1);
        errno = 0;
        ssize_t again = recv(c, sink, 100, 0);
        printf("P\tP-reset\tbefore=%s\tpeek=%s\tso_error_after_peek=%s\trecv_after=%s\n", s1, state(&k, s2, sizeof s2),
               errname(pending), answer(again, errno, s3, sizeof s3));
        close(c);
    }

    // P-eintr
    for (int restart = 0; restart < 2; restart++)
    {
        install(restart);
        atomic_store(&sigusr1s, 0);
        int c, p;
        make_pair(&c, &p);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 100, MSG_PEEK);
        sleep_ms(100);
        pthread_kill(t, SIGUSR1);
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        if (write(p, payload, 10) != 10) die("P-eintr write");
        sleep_ms(100);
        state(&k, s2, sizeof s2);
        if (!atomic_load(&k.done)) { close(p); p = -1; sleep_ms(100); }
        pthread_join(t, NULL);
        printf("P\tP-eintr\trestart=%d\tafter_signal=%s\tafter_write=%s\tsigusr1=%d\tfionread=%d\n", restart, s1, s2,
               atomic_load(&sigusr1s), nread(c));
        close(c);
        if (p >= 0) close(p);
    }
}

static void section_z(void)
{
    char s1[64], s2[64], s3[64];
    install(0);
    int c, p;
    make_pair(&c, &p);
    call k;
    pthread_t t;
    start(&k, &t, 0, c, 0, 0);
    sleep_ms(100);
    state(&k, s1, sizeof s1);
    if (write(p, payload, 10) != 10) die("Z write");
    sleep_ms(100);
    state(&k, s2, sizeof s2);
    if (!atomic_load(&k.done)) { close(p); p = -1; sleep_ms(100); }
    pthread_join(t, NULL);
    printf("Z\trecv0-blocking\tbefore=%s\tafter_write=%s\tfinally=%s\tfionread=%d\n", s1, s2, state(&k, s3, sizeof s3),
           nread(c));
    close(c);
    if (p >= 0) close(p);
}

static void section_d(void)
{
    char s1[64], s2[64];
    install(0);
    int c, p;
    make_pair(&c, &p);
    errno = 0;
    ssize_t rv = recv(c, sink, 100, MSG_DONTWAIT);
    printf("D\trecv-idle\t%s\tnonblocking_after=%d\n", answer(rv, errno, s1, sizeof s1), is_nonblocking(c));
    errno = 0;
    rv = send(c, payload, 65536, MSG_DONTWAIT);
    printf("D\tsend-idle\t%s\tnonblocking_after=%d\n", answer(rv, errno, s1, sizeof s1), is_nonblocking(c));
    close(c);
    close(p);

    // D-full: c filled to EAGAIN through O_NONBLOCK, which is then cleared;
    // a send(c, 65536, MSG_DONTWAIT) is made on another thread, so that a
    // send that sleeps does not hang the probe. 200 ms in, its state; then p
    // drains everything, and its answer.
    {
        make_pair(&c, &p);
        long long filled = fill(c);
        call k;
        pthread_t t;
        start(&k, &t, 1, c, 65536, MSG_DONTWAIT);
        sleep_ms(200);
        state(&k, s1, sizeof s1);
        set_nonblocking(p, 1);
        long long drained = 0;
        int idle = 0;
        while (idle < 20)
        {
            ssize_t n = read(p, sink, sizeof sink);
            if (n > 0) { drained += n; idle = 0; }
            else { idle++; sleep_ms(5); }
        }
        pthread_join(t, NULL);
        printf("D\tsend-full\tfilled=%lld\tafter_200ms=%s\tafter_drain=%s\tdrained=%lld\tnonblocking_after=%d\n", filled,
               s1, state(&k, s2, sizeof s2), drained, is_nonblocking(c));
        close(c);
        close(p);
    }

#ifdef __linux__
    // D-edge: an edge-triggered EPOLLOUT registration on blocking c, then
    // sends with MSG_DONTWAIT until EAGAIN, then the edges as p drains.
    {
        make_pair(&c, &p);
        int ep = epoll_create1(0);
        struct epoll_event ev = { .events = EPOLLOUT | EPOLLET, .data.fd = c };
        if (epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev) < 0) die("epoll_ctl");
        struct epoll_event got[4];
        // The registration's first report, which ADD makes since c is
        // writable.
        int first = epoll_wait(ep, got, 4, 0);
        long long total = 0;
        ssize_t last_short = -2;
        int quiet = 0;
        for (;;)
        {
            errno = 0;
            rv = send(c, payload, 65536, MSG_DONTWAIT);
            if (rv > 0)
            {
                total += rv;
                if (rv < 65536) last_short = rv;
                quiet = 0;
                continue;
            }
            if (errno != EAGAIN) { printf("D\tsend-fill\tunexpected\t%s\n", errname(errno)); break; }
            if (++quiet == 3) break;
            sleep_ms(50);
        }
        printf("D\tsend-fill\ttotal=%lld\tlast_short=%zd\tends=EAGAIN\tnonblocking_after=%d\n", total, last_short,
               is_nonblocking(c));
        int before = epoll_wait(ep, got, 4, 0);
        set_nonblocking(p, 1);
        long long drained = 0;
        int edges = 0;
        int idle = 0;
        while (idle < 10)
        {
            ssize_t n = read(p, sink, 16384);
            if (n > 0) { drained += n; idle = 0; }
            else { idle++; sleep_ms(5); }
            int e = epoll_wait(ep, got, 4, 0);
            if (e > 0) edges += e;
        }
        printf("D\tepollet-out\tfirst=%d\tbefore_drain=%d\tedges_while_draining=%d\tdrained=%lld\n", first, before, edges,
               drained);
        close(ep);
        close(c);
        close(p);
    }
#endif
}

static void section_n(void)
{
    char s1[64], s2[64];

    // N-recv
    {
        install(0);
        int c, p;
        make_pair(&c, &p);
        errno = 0;
        ssize_t a = recv(c, sink, 100, MSG_NOSIGNAL | MSG_DONTWAIT);
        answer(a, errno, s1, sizeof s1);
        if (write(p, payload, 10) != 10) die("N-recv write");
        sleep_ms(20);
        errno = 0;
        ssize_t b = recv(c, sink, 100, MSG_NOSIGNAL | MSG_DONTWAIT);
        printf("N\tN-recv\tidle=%s\tqueued=%s\n", s1, answer(b, errno, s2, sizeof s2));
        close(c);
        close(p);
    }

    for (int nosignal = 1; nosignal >= 0; nosignal--)
    {
        int f = nosignal ? MSG_NOSIGNAL : 0;
        const char *label = nosignal ? "MSG_NOSIGNAL" : "0";

        // N-partial
        {
            install(0);
            atomic_store(&sigpipes, 0);
            int c, p;
            make_pair(&c, &p);
            call k;
            pthread_t t;
            start(&k, &t, 1, c, 8 << 20, f);
            sleep_ms(200);
            state(&k, s1, sizeof s1);
            // p closes with c's bytes unread: a reset.
            close(p);
            pthread_join(t, NULL);
            printf("N\tN-partial\tflags=%s\tbefore=%s\tsend=%s\tsigpipe=%d\tso_error=%s\n", label, s1,
                   state(&k, s2, sizeof s2), atomic_load(&sigpipes), errname(so_error(c)));
            close(c);
        }

        // N-empty
        {
            install(0);
            atomic_store(&sigpipes, 0);
            int c, p;
            make_pair(&c, &p);
            long long filled = fill(c);
            call k;
            pthread_t t;
            start(&k, &t, 1, c, 1000, f);
            sleep_ms(100);
            state(&k, s1, sizeof s1);
            close(p);
            pthread_join(t, NULL);
            printf("N\tN-empty\tflags=%s\tfilled=%lld\tbefore=%s\tsend=%s\tsigpipe=%d\tso_error=%s\n", label, filled, s1,
                   state(&k, s2, sizeof s2), atomic_load(&sigpipes), errname(so_error(c)));
            close(c);
        }
    }
}

static void section_w(void)
{
    // W: F_GETFL of c before and after a call that moves bytes through it:
    // write(2), then send(2), each on a fresh pair, and a send of 0 bytes.
    const char *names[] = { "write", "send", "send0" };
    for (int i = 0; i < 3; i++)
    {
        int c, p;
        make_pair(&c, &p);
        int before = fcntl(c, F_GETFL);
        ssize_t rv = i == 0 ? write(c, payload, 10) : send(c, payload, i == 1 ? 10 : 0, 0);
        int after = fcntl(c, F_GETFL);
        printf("W\t%s\trv=%zd\tgetfl_before=0x%x\tgetfl_after=0x%x\n", names[i], rv, before, after);
        close(c);
        close(p);
    }
}

int main(int argc, char **argv)
{
    const char *sections = argc > 1 ? argv[1] : "HOPZDNW";
    setvbuf(stdout, NULL, _IOLBF, 0);
    memset(payload, 'x', sizeof payload);
    if (strchr(sections, 'H')) section_h();
    if (strchr(sections, 'O')) section_o();
    if (strchr(sections, 'P')) section_p();
    if (strchr(sections, 'Z')) section_z();
    if (strchr(sections, 'D')) section_d();
    if (strchr(sections, 'N')) section_n();
    if (strchr(sections, 'W')) section_w();
    return 0;
}
