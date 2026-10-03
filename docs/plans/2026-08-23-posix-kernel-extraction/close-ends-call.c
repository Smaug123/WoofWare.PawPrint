// What a close of the descriptor a sleeping accept(2), pipe read(2) or pipe
// write(2) was entered through does on Darwin, beyond ending that call (which
// blocking-accept.c section C, pipe-blocking.c section K and
// open-file-references.c sections A-C measured): what it does to other calls
// asleep on the same object, through the same descriptor or another; whether
// it leaves the object changed for later calls; whether an ended write raises
// SIGPIPE or reports bytes it had put in; and what it does to the pipe's
// timestamps.
//
// A "sleeper" is a thread blocked in one call, started and given 50 ms to fall
// asleep. A listener is an IPv4 stream socket on 127.0.0.1, backlog 16; a
// "connect" is a blocking connect from a fresh socket. "Full" is a pipe filled
// as pipe-blocking.c fills it (Linux: 4096-byte non-blocking writes until
// EAGAIN; Darwin: one 65536-byte write). SIGPIPE has a handler that records
// which thread it ran on (the sleeper, the main thread, or another). A sleeper
// that nothing ends is interrupted by SIGUSR1 (no SA_RESTART) and reported
// "asleep". Every close below is made from the main thread, with A the
// descriptor the sleeper was entered through and B a dup of it.
//
// Sections:
//   A  accept.
//      A1 one accepter on A, no dup: the close's rv and how long it took, and
//         the accepter's answer 100 ms later.
//      A2 one accepter on A, B kept: as A1; then a connect, and a
//         non-blocking accept through B; then a blocking accept through B, a
//         connect 50 ms later, and its answer.
//      A3 two accepters on A, B kept: the close, then each one's answer.
//      A4 an accepter on A and one on B: the close, each one's answer 100 ms
//         later, then a connect and each one's answer.
//      A5 an accepter on A and a poll(POLLIN, -1) of B: the close, whether the
//         poll returned within 100 ms and with what.
//      A6 (Darwin) an accepter on A and a kevent wait on a kqueue holding
//         EVFILT_READ registered through B: the close, whether the wait
//         returned within 100 ms and with what.
//      A7 a listener whose accepter on A a close of A has ended, B kept, and
//         what later calls through B answer: a a non-blocking accept, the
//         queue empty; b a blocking accept, asleep or not 100 ms in, then a
//         connect, its answer, and a non-blocking accept after; c a blocking
//         accept with a connection already queued; d a blocking accept
//         signalled 100 ms in, without and with SA_RESTART; e a blocking
//         accept through a fresh dup of B, then a connect; g two blocking
//         accepts 30 ms apart, then one connect: which return (5 trials); h
//         listen(B, 16) again, then a blocking accept and a connect; f
//         poll(POLLIN|POLLOUT, 0) of B, empty and with a connection queued.
//   P  pipes.
//      P1 a reader of an empty pipe on A (the read end), B kept: the close,
//         its answer; then 3 bytes written and a blocking read through B.
//      P2 a 1-byte writer into a full pipe on A (the write end), B kept: the
//         close, its answer and SIGPIPE; then 4096 bytes read, and a 1-byte
//         write through B.
//      P3 as P2 with no dup: the answer and SIGPIPE.
//      P4 a 200000-byte writer into an empty pipe on A (65536 bytes in before
//         it sleeps), B kept and not: the answer, SIGPIPE, and the bytes the
//         read end holds afterwards.
//      P5 two readers on A, B kept: each one's answer to the close.
//      P6 a reader on A and one on B: each one's answer to the close; then 3
//         bytes written, and B's reader's answer.
//      P7 a 1-byte writer on A and one on B, both into a full pipe: each
//         one's answer to the close and SIGPIPE; then 4096 bytes read, and
//         B's writer's answer.
//      P8 (timestamps) a reader on A, B kept, closed ~1000 ms in: the read
//         end's atime, mtime and ctime, through B, ~1100 ms in, in ms from the
//         start; likewise a 1-byte writer into a full pipe and the write end's.
//      P9 a reader on A and a poll(POLLIN, -1) of B: the close, whether the
//         poll returned within 100 ms and with what.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -pthread -o /tmp/cec close-ends-call.c && /tmp/cec
//   Linux:  container run --rm -v "$PWD:/probe" gcc:14 sh -c
//             "gcc -Wall -O1 -pthread -o /tmp/cec /probe/close-ends-call.c && /tmp/cec"
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 three times, with the same
// answers each time bar the milliseconds (close-ends-call.darwin-27.0.txt),
// and on Linux 6.18.5 aarch64 (Apple `container`, gcc:14 image, glibc 2.41)
// once (close-ends-call.linux-6.18.5-aarch64.txt):
//   Linux: no close ended any call. Every sleeper slept on and finished as
//      if nothing had been closed, and every later call through B answered as
//      on a listener or pipe nothing had happened to.
//   Darwin, accept: every close returned 0 at once. Closing the descriptor
//      an accept was entered through ended, with ECONNABORTED, every accept
//      asleep on the listener, through that descriptor or another (A3, A4).
//      It woke neither a poll nor a kevent of the listener (A5, A6). It left
//      the listener changed for good, B kept: a later accept that finds a
//      connection queued takes it (A2, A7c), a non-blocking one finding none
//      is EAGAIN (A7a), but a blocking one finding none sleeps and, woken,
//      answers ECONNABORTED whatever woke it: a connection, which it leaves
//      queued for a later accept to take (A7b, A7e), or a signal, under
//      SA_RESTART or not (A7d). One connection woke one such sleeper, the
//      first parked, and the other slept on (A7g, 5 of 5). A second listen
//      changes none of this (A7h), and a poll still reports the queue as
//      before (A7f).
//   Darwin, pipes: every close returned 0 at once. Closing the descriptor a
//      read or write was entered through ended every call asleep through that
//      descriptor (P5), a read with 0 and a write with EPIPE, the write
//      whether or not it had put bytes in (P4: 65536 of 200000 in, the bytes
//      left in the pipe), and raised SIGPIPE, which ran on the main thread
//      (P2, P3, P4, P7), as a write into a pipe with no reader raises it. A
//      call asleep through a dup of it slept on and finished normally (P6,
//      P7), a later call through the dup was answered normally (P1, P2), and
//      a poll of the dup was not woken (P9). The ending moved the read end's
//      atime, and the write end's mtime and ctime, at the close (P8).
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
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#ifdef __APPLE__
#include <sys/event.h>
#endif

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

static pthread_t main_thread;
static atomic_int sigpipe_count;
static pthread_t sigpipe_thread;

static void on_sigpipe(int sig)
{
    (void)sig;
    sigpipe_thread = pthread_self();
    atomic_fetch_add(&sigpipe_count, 1);
}

static const char *errname(int e)
{
    switch (e) {
    case 0: return "0";
    case EAGAIN: return "EAGAIN";
    case EINTR: return "EINTR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EPIPE: return "EPIPE";
    case ECONNABORTED: return "ECONNABORTED";
    case ECONNREFUSED: return "ECONNREFUSED";
    default: return strerror(e);
    }
}

enum kind { ACCEPT, READ, WRITE };

struct sleeper {
    enum kind kind;
    int fd;
    size_t count;
    const unsigned char *buf;
    pthread_t thread;
    atomic_int done;
    ssize_t rv;
    int err;
};

static unsigned char big[1 << 20];

static void *sleeper_main(void *p)
{
    struct sleeper *s = p;
    errno = 0;
    switch (s->kind) {
    case ACCEPT: s->rv = accept(s->fd, NULL, NULL); break;
    case READ: { unsigned char b[64]; s->rv = read(s->fd, b, s->count); break; }
    case WRITE: s->rv = write(s->fd, s->buf, s->count); break;
    }
    s->err = s->rv < 0 ? errno : 0;
    atomic_store(&s->done, 1);
    return NULL;
}

static void start(struct sleeper *s, enum kind kind, int fd, size_t count)
{
    memset(s, 0, sizeof *s);
    s->kind = kind;
    s->fd = fd;
    s->count = count;
    s->buf = big;
    pthread_create(&s->thread, NULL, sleeper_main, s);
}

// "asleep", or what the call answered; interrupts and joins a sleeper only
// when `finish` is set.
static void report(const char *label, struct sleeper *s, int finish)
{
    int done = atomic_load(&s->done);
    if (!done && finish) {
        pthread_kill(s->thread, SIGUSR1);
        pthread_join(s->thread, NULL);
    } else if (done && finish) {
        pthread_join(s->thread, NULL);
    }
    if (!done) { printf("%s asleep\n", label); return; }
    if (s->kind == ACCEPT)
        printf("%s rv=%s errno=%s\n", label, s->rv >= 0 ? "fd" : "-1", errname(s->err));
    else
        printf("%s rv=%zd errno=%s\n", label, s->rv, errname(s->err));
    if (s->kind == ACCEPT && s->rv >= 0) close((int)s->rv);
}

static const char *sigpipe_where(struct sleeper *s, int before)
{
    if (atomic_load(&sigpipe_count) == before) return "none";
    if (pthread_equal(sigpipe_thread, s->thread)) return "sleeper";
    if (pthread_equal(sigpipe_thread, main_thread)) return "main";
    return "other";
}

static int listener(struct sockaddr_in *addr)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) { perror("bind"); exit(1); }
    if (listen(s, 16) != 0) { perror("listen"); exit(1); }
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    *addr = a;
    return s;
}

static int connect_to(const struct sockaddr_in *addr)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(c, (const struct sockaddr *)addr, sizeof *addr) != 0) {
        printf("  connect errno=%s\n", errname(errno));
        close(c);
        return -1;
    }
    return c;
}

static void timed_close(const char *label, int fd)
{
    int64_t t0 = now_ms();
    int rv = close(fd);
    printf("%s close rv=%d took=%lldms\n", label, rv, (long long)(now_ms() - t0));
}

static void set_nonblock(int fd, int on)
{
    int flags = fcntl(fd, F_GETFL);
    fcntl(fd, F_SETFL, on ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK));
}

static int held(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) != 0) return -1;
    return n;
}

static void fill(int w)
{
    set_nonblock(w, 1);
#ifdef __linux__
    while (write(w, big, 4096) == 4096) { }
#else
    if (write(w, big, 65536) != 65536) { printf("fill: short\n"); exit(1); }
#endif
    set_nonblock(w, 0);
}

static void drain_bytes(int r, int n)
{
    unsigned char buf[4096];
    int got = 0;
    while (got < n) {
        ssize_t k = read(r, buf, (size_t)(n - got) < sizeof buf ? (size_t)(n - got) : sizeof buf);
        if (k <= 0) { printf("drain: %zd errno %s\n", k, errname(errno)); return; }
        got += (int)k;
    }
}

// ---- A ---------------------------------------------------------------------

static void section_a1(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    struct sleeper s;
    start(&s, ACCEPT, l, 0);
    sleep_ms(50);
    timed_close("A1", l);
    sleep_ms(100);
    report("A1 accepter", &s, 1);
}

static void section_a2(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    int b = dup(l);
    struct sleeper s;
    start(&s, ACCEPT, l, 0);
    sleep_ms(50);
    timed_close("A2", l);
    sleep_ms(100);
    report("A2 accepter", &s, 1);
    int c = connect_to(&addr);
    sleep_ms(20);
    set_nonblock(b, 1);
    int a = accept(b, NULL, NULL);
    printf("A2 later non-blocking accept through B, after a connect: rv=%s errno=%s\n", a >= 0 ? "fd" : "-1",
           errname(a < 0 ? errno : 0));
    if (a >= 0) close(a);
    if (c >= 0) close(c);
    set_nonblock(b, 0);
    struct sleeper t;
    start(&t, ACCEPT, b, 0);
    sleep_ms(50);
    c = connect_to(&addr);
    sleep_ms(100);
    report("A2 later blocking accept through B, a connect 50 ms in", &t, 1);
    if (c >= 0) close(c);
    close(b);
}

static void section_a3(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    int b = dup(l);
    struct sleeper s1, s2;
    start(&s1, ACCEPT, l, 0);
    start(&s2, ACCEPT, l, 0);
    sleep_ms(50);
    timed_close("A3", l);
    sleep_ms(100);
    report("A3 first accepter on A", &s1, 1);
    report("A3 second accepter on A", &s2, 1);
    close(b);
}

static void section_a4(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    int b = dup(l);
    struct sleeper sa, sb;
    start(&sa, ACCEPT, l, 0);
    start(&sb, ACCEPT, b, 0);
    sleep_ms(50);
    timed_close("A4", l);
    sleep_ms(100);
    report("A4 accepter on A, after the close", &sa, 0);
    report("A4 accepter on B, after the close", &sb, 0);
    int c = connect_to(&addr);
    sleep_ms(100);
    report("A4 accepter on A, after a connect", &sa, 1);
    report("A4 accepter on B, after a connect", &sb, 1);
    if (c >= 0) close(c);
    close(b);
}

struct poller {
    int fd;
    short events;
    pthread_t thread;
    atomic_int done;
    int rv;
    int err;
    short revents;
};

static void *poll_main(void *p)
{
    struct poller *q = p;
    struct pollfd pfd = { q->fd, q->events, 0 };
    q->rv = poll(&pfd, 1, -1);
    q->err = q->rv < 0 ? errno : 0;
    q->revents = pfd.revents;
    atomic_store(&q->done, 1);
    return NULL;
}

static void report_poll(const char *label, struct poller *q)
{
    int done = atomic_load(&q->done);
    if (!done) pthread_kill(q->thread, SIGUSR1);
    pthread_join(q->thread, NULL);
    if (!done) printf("%s asleep\n", label);
    else printf("%s rv=%d errno=%s revents=%#x\n", label, q->rv, errname(q->err), (unsigned)(unsigned short)q->revents);
}

static void section_a5(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    int b = dup(l);
    struct sleeper s;
    struct poller q;
    memset(&q, 0, sizeof q);
    q.fd = b;
    q.events = POLLIN;
    start(&s, ACCEPT, l, 0);
    pthread_create(&q.thread, NULL, poll_main, &q);
    sleep_ms(50);
    timed_close("A5", l);
    sleep_ms(100);
    report("A5 accepter on A", &s, 1);
    report_poll("A5 poll of B", &q);
    close(b);
}

#ifdef __APPLE__
struct kwaiter {
    int kq;
    pthread_t thread;
    atomic_int done;
    int rv;
    int err;
    struct kevent ev;
};

static void *kevent_main(void *p)
{
    struct kwaiter *k = p;
    k->rv = kevent(k->kq, NULL, 0, &k->ev, 1, NULL);
    k->err = k->rv < 0 ? errno : 0;
    atomic_store(&k->done, 1);
    return NULL;
}

static void section_a6(void)
{
    struct sockaddr_in addr;
    int l = listener(&addr);
    int b = dup(l);
    int kq = kqueue();
    struct kevent change;
    EV_SET(&change, b, EVFILT_READ, EV_ADD, 0, 0, NULL);
    if (kevent(kq, &change, 1, NULL, 0, NULL) != 0) perror("kevent register");
    struct sleeper s;
    struct kwaiter k;
    memset(&k, 0, sizeof k);
    k.kq = kq;
    start(&s, ACCEPT, l, 0);
    pthread_create(&k.thread, NULL, kevent_main, &k);
    sleep_ms(50);
    timed_close("A6", l);
    sleep_ms(100);
    report("A6 accepter on A", &s, 1);
    int done = atomic_load(&k.done);
    if (!done) pthread_kill(k.thread, SIGUSR1);
    pthread_join(k.thread, NULL);
    if (!done) printf("A6 kevent of B asleep\n");
    else
        printf("A6 kevent of B rv=%d errno=%s ident=%d filter=%d flags=%#x data=%ld\n", k.rv, errname(k.err),
               (int)k.ev.ident, (int)k.ev.filter, (unsigned)k.ev.flags, (long)k.ev.data);
    close(kq);
    close(b);
}
#endif

// A listener whose accepter on A a close of A has ended, B kept: what later
// calls through B answer.
static int ended_listener(const char *label, struct sockaddr_in *addr)
{
    int l = listener(addr);
    int b = dup(l);
    struct sleeper s;
    start(&s, ACCEPT, l, 0);
    sleep_ms(50);
    close(l);
    sleep_ms(50);
    char buf[96];
    snprintf(buf, sizeof buf, "%s (the ended accepter)", label);
    report(buf, &s, 1);
    return b;
}

static void section_a7(void)
{
    struct sockaddr_in addr;
    {
        int b = ended_listener("A7a", &addr);
        set_nonblock(b, 1);
        int a = accept(b, NULL, NULL);
        printf("A7a non-blocking accept through B, empty queue: rv=%s errno=%s\n", a >= 0 ? "fd" : "-1",
               errname(a < 0 ? errno : 0));
        if (a >= 0) close(a);
        close(b);
    }
    {
        int b = ended_listener("A7b", &addr);
        struct sleeper t;
        start(&t, ACCEPT, b, 0);
        sleep_ms(100);
        report("A7b blocking accept through B, empty queue, 100 ms in", &t, 0);
        int c = connect_to(&addr);
        sleep_ms(100);
        report("A7b the same, 100 ms after a connect", &t, 1);
        set_nonblock(b, 1);
        int a = accept(b, NULL, NULL);
        printf("A7b then a non-blocking accept through B: rv=%s errno=%s\n", a >= 0 ? "fd" : "-1",
               errname(a < 0 ? errno : 0));
        if (a >= 0) close(a);
        if (c >= 0) close(c);
        close(b);
    }
    {
        int b = ended_listener("A7c", &addr);
        int c = connect_to(&addr);
        sleep_ms(20);
        struct sleeper t;
        start(&t, ACCEPT, b, 0);
        sleep_ms(100);
        report("A7c blocking accept through B, a connection queued", &t, 1);
        if (c >= 0) close(c);
        close(b);
    }
    for (int restart = 0; restart <= 1; restart++) {
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_usr1;
        sa.sa_flags = restart ? SA_RESTART : 0;
        sigaction(SIGUSR1, &sa, NULL);
        int b = ended_listener(restart ? "A7d SA_RESTART" : "A7d", &addr);
        struct sleeper t;
        start(&t, ACCEPT, b, 0);
        sleep_ms(100);
        report(restart ? "A7d SA_RESTART blocking accept through B, 100 ms in" : "A7d blocking accept through B, 100 ms in", &t, 0);
        pthread_kill(t.thread, SIGUSR1);
        sleep_ms(100);
        report(restart ? "A7d SA_RESTART the same, 100 ms after SIGUSR1" : "A7d the same, 100 ms after SIGUSR1", &t, 0);
        sa.sa_flags = 0;
        sigaction(SIGUSR1, &sa, NULL);
        report(restart ? "A7d SA_RESTART final" : "A7d final", &t, 1);
        close(b);
    }
    {
        int b = ended_listener("A7e", &addr);
        int c2 = dup(b);
        struct sleeper t;
        start(&t, ACCEPT, c2, 0);
        sleep_ms(100);
        report("A7e blocking accept through a fresh dup C of B, 100 ms in", &t, 0);
        int c = connect_to(&addr);
        sleep_ms(100);
        report("A7e the same, after a connect", &t, 1);
        if (c >= 0) close(c);
        close(c2);
        close(b);
    }
    for (int trial = 0; trial < 5; trial++) {
        int b = ended_listener("A7g", &addr);
        struct sleeper t1, t2;
        start(&t1, ACCEPT, b, 0);
        sleep_ms(30);
        start(&t2, ACCEPT, b, 0);
        sleep_ms(70);
        int c = connect_to(&addr);
        sleep_ms(100);
        printf("A7g trial %d: two blocking accepts through B, one connect: first %s, second %s\n", trial,
               atomic_load(&t1.done) ? "returned" : "asleep", atomic_load(&t2.done) ? "returned" : "asleep");
        report("A7g first", &t1, 1);
        report("A7g second", &t2, 1);
        if (c >= 0) close(c);
        close(b);
    }
    {
        int b = ended_listener("A7h", &addr);
        int rv = listen(b, 16);
        printf("A7h listen(B, 16) again: rv=%d errno=%s\n", rv, errname(rv < 0 ? errno : 0));
        struct sleeper t;
        start(&t, ACCEPT, b, 0);
        sleep_ms(100);
        report("A7h blocking accept through B after the second listen, 100 ms in", &t, 0);
        int c = connect_to(&addr);
        sleep_ms(100);
        report("A7h the same, after a connect", &t, 1);
        if (c >= 0) close(c);
        close(b);
    }
    {
        // A poll of a listener the close has ended an accept on.
        int b = ended_listener("A7f", &addr);
        struct pollfd pfd = { b, POLLIN | POLLOUT, 0 };
        int rv = poll(&pfd, 1, 0);
        printf("A7f poll(POLLIN|POLLOUT, 0) of B, empty queue: rv=%d revents=%#x\n", rv, (unsigned)(unsigned short)pfd.revents);
        int c = connect_to(&addr);
        sleep_ms(20);
        pfd.revents = 0;
        rv = poll(&pfd, 1, 0);
        printf("A7f the same, a connection queued: rv=%d revents=%#x\n", rv, (unsigned)(unsigned short)pfd.revents);
        if (c >= 0) close(c);
        close(b);
    }
}

// ---- P ---------------------------------------------------------------------

static void section_p1(void)
{
    int fds[2];
    pipe(fds);
    int b = dup(fds[0]);
    struct sleeper s;
    start(&s, READ, fds[0], 8);
    sleep_ms(50);
    timed_close("P1", fds[0]);
    sleep_ms(100);
    report("P1 reader on A", &s, 1);
    write(fds[1], "abc", 3);
    struct sleeper t;
    start(&t, READ, b, 8);
    sleep_ms(100);
    report("P1 later read through B, 3 bytes held", &t, 1);
    close(b);
    close(fds[1]);
}

static void writer_case(const char *label, int dup_kept, size_t count, int prefill)
{
    int fds[2];
    pipe(fds);
    if (prefill) fill(fds[1]);
    int b = dup_kept ? dup(fds[1]) : -1;
    struct sleeper s;
    int before = atomic_load(&sigpipe_count);
    start(&s, WRITE, fds[1], count);
    sleep_ms(50);
    int heldBefore = held(fds[0]);
    timed_close(label, fds[1]);
    sleep_ms(100);
    char buf[128];
    snprintf(buf, sizeof buf, "%s writer on A", label);
    printf("%s sigpipe=%s held_before_close=%d held_after=%d\n", label, sigpipe_where(&s, before), heldBefore,
           held(fds[0]));
    report(buf, &s, 1);
    if (dup_kept && prefill) {
        drain_bytes(fds[0], 4096);
        ssize_t rv = write(b, "x", 1);
        printf("%s later 1-byte write through B, room made: rv=%zd errno=%s\n", label, rv, errname(rv < 0 ? errno : 0));
    }
    if (b >= 0) close(b);
    close(fds[0]);
}

static void section_p5(void)
{
    int fds[2];
    pipe(fds);
    int b = dup(fds[0]);
    struct sleeper s1, s2;
    start(&s1, READ, fds[0], 8);
    start(&s2, READ, fds[0], 8);
    sleep_ms(50);
    timed_close("P5", fds[0]);
    sleep_ms(100);
    report("P5 first reader on A", &s1, 1);
    report("P5 second reader on A", &s2, 1);
    close(b);
    close(fds[1]);
}

static void section_p6(void)
{
    int fds[2];
    pipe(fds);
    int b = dup(fds[0]);
    struct sleeper sa, sb;
    start(&sa, READ, fds[0], 8);
    start(&sb, READ, b, 8);
    sleep_ms(50);
    timed_close("P6", fds[0]);
    sleep_ms(100);
    report("P6 reader on A, after the close", &sa, 0);
    report("P6 reader on B, after the close", &sb, 0);
    write(fds[1], "abc", 3);
    sleep_ms(100);
    report("P6 reader on A, after 3 bytes", &sa, 1);
    report("P6 reader on B, after 3 bytes", &sb, 1);
    close(b);
    close(fds[1]);
}

static void section_p7(void)
{
    int fds[2];
    pipe(fds);
    fill(fds[1]);
    int b = dup(fds[1]);
    struct sleeper sa, sb;
    int before = atomic_load(&sigpipe_count);
    start(&sa, WRITE, fds[1], 1);
    start(&sb, WRITE, b, 1);
    sleep_ms(50);
    timed_close("P7", fds[1]);
    sleep_ms(100);
    report("P7 writer on A, after the close", &sa, 0);
    report("P7 writer on B, after the close", &sb, 0);
    printf("P7 sigpipe=%s (count %d)\n", sigpipe_where(&sa, before), atomic_load(&sigpipe_count) - before);
    drain_bytes(fds[0], 4096);
    sleep_ms(100);
    report("P7 writer on A, after room", &sa, 1);
    report("P7 writer on B, after room", &sb, 1);
    close(b);
    close(fds[0]);
}

static double ms_since(struct timespec t, struct timespec base)
{
    return (double)(t.tv_sec - base.tv_sec) * 1000.0 + (double)(t.tv_nsec - base.tv_nsec) / 1e6;
}

static void section_p8(int writing)
{
    int fds[2];
    pipe(fds);
    if (writing) fill(fds[1]);
    int a = writing ? fds[1] : fds[0];
    int b = dup(a);
    struct timespec base;
    clock_gettime(CLOCK_REALTIME, &base);
    struct stat before;
    fstat(b, &before);
    sleep_ms(200);
    struct sleeper s;
    start(&s, writing ? WRITE : READ, a, writing ? 1 : 8);
    sleep_ms(800);
    timed_close(writing ? "P8 writer" : "P8 reader", a);
    sleep_ms(100);
    struct stat after;
    fstat(b, &after);
#ifdef __APPLE__
    printf("P8 %s: before atime=%.0f mtime=%.0f ctime=%.0f; after atime=%.0f mtime=%.0f ctime=%.0f (ms from start, close ~1000)\n",
           writing ? "writer" : "reader", ms_since(before.st_atimespec, base), ms_since(before.st_mtimespec, base),
           ms_since(before.st_ctimespec, base), ms_since(after.st_atimespec, base), ms_since(after.st_mtimespec, base),
           ms_since(after.st_ctimespec, base));
#else
    printf("P8 %s: before atime=%.0f mtime=%.0f ctime=%.0f; after atime=%.0f mtime=%.0f ctime=%.0f (ms from start, close ~1000)\n",
           writing ? "writer" : "reader", ms_since(before.st_atim, base), ms_since(before.st_mtim, base),
           ms_since(before.st_ctim, base), ms_since(after.st_atim, base), ms_since(after.st_mtim, base),
           ms_since(after.st_ctim, base));
#endif
    report(writing ? "P8 writer on A" : "P8 reader on A", &s, 1);
    close(b);
    close(writing ? fds[0] : fds[1]);
}

static void section_p9(void)
{
    int fds[2];
    pipe(fds);
    int b = dup(fds[0]);
    struct sleeper s;
    struct poller q;
    memset(&q, 0, sizeof q);
    q.fd = b;
    q.events = POLLIN;
    start(&s, READ, fds[0], 8);
    pthread_create(&q.thread, NULL, poll_main, &q);
    sleep_ms(50);
    timed_close("P9", fds[0]);
    sleep_ms(100);
    report("P9 reader on A", &s, 1);
    report_poll("P9 poll of B", &q);
    close(b);
    close(fds[1]);
}

int main(int argc, char **argv)
{
    alarm(120);
    main_thread = pthread_self();
    setvbuf(stdout, NULL, _IONBF, 0);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sigaction(SIGUSR1, &sa, NULL);
    sa.sa_handler = on_sigpipe;
    sigaction(SIGPIPE, &sa, NULL);
    const char *only = argc > 1 ? argv[1] : "AP";
    if (strchr(only, 'A')) {
        section_a1();
        section_a2();
        section_a3();
        section_a4();
        section_a5();
#ifdef __APPLE__
        section_a6();
#endif
        section_a7();
    }
    if (strchr(only, 'P')) {
        section_p1();
        writer_case("P2", 1, 1, 1);
        writer_case("P3", 0, 1, 1);
        writer_case("P4 B kept", 1, 200000, 0);
        writer_case("P4 no dup", 0, 200000, 0);
        section_p5();
        section_p6();
        section_p7();
        section_p8(0);
        section_p8(1);
        section_p9();
    }
    return 0;
}
