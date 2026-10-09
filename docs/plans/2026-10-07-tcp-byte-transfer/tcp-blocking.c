// Blocking read and write on a connected loopback TCP socket, on Linux and
// Darwin: what wakes a sleeping reader or writer, how much a woken writer
// takes, when a blocking write returns, and what a signal, a reset and a close
// of the descriptor do to each.
//
// c is the connecting socket and p the accepted one, both blocking unless
// stated. Signals are SIGUSR1, sent by pthread_kill to the sleeping thread,
// whose handler counts them; "restart" means it was installed with
// SA_RESTART. SIGPIPE is caught and counted. Each line is tab-separated, its
// first field the section.
//
// Sections:
//   R   a reader asleep in read(c, buf, 4096) with nothing queued, 100 ms
//       in:
//       R-data      p writes 100 bytes.
//       R-fin       p closes, having nothing unread.
//       R-reset     c had written 100 bytes p never read; p closes.
//       R-eintr     the reader is signalled (no SA_RESTART).
//       R-restart   the reader is signalled (SA_RESTART); 100 ms later p
//                   writes 100 bytes.
//       R-close     another thread closes c; 100 ms later p writes 100 bytes;
//                   100 ms later p closes. Whether the reader has returned is
//                   reported after each step.
//       R-order     three readers park 30 ms apart in the order 0, 1, 2; then
//                   p writes 1 byte three times, 100 ms apart: the order they
//                   return in, over 10 trials.
//   W   a writer asleep in write(c, buf, N):
//       W-resume    N = 8 MiB (or 1 MiB with SO_SNDBUF 32768 set on c before
//                   connect); 200 ms in, p reads 16384 bytes at a time, 5 ms
//                   apart. After each read, "taken" = bytes p has read +
//                   FIONREAD(p) + c's unsent bytes (SIOCOUTQ on Linux,
//                   SO_NWRITE on Darwin): how much of the write the kernel
//                   has taken so far. A line is printed whenever taken
//                   changes, with c's queue just before; then the write's
//                   answer.
//       W-partial   N = 8 MiB; 200 ms in (some taken, the buffers full), the
//                   writer is signalled, without and with SA_RESTART.
//       W-empty     c is filled non-blocking to EAGAIN (repeated after
//                   50 ms sleeps until three in a row take nothing), then a
//                   blocking write of 1000 bytes is made; 100 ms in it is
//                   signalled, without and with SA_RESTART. Under SA_RESTART,
//                   100 ms later p drains everything.
//       W-reset     as W-partial and W-empty, but p closes (with bytes unread)
//                   instead of the signal: the answer, SIGPIPE, and
//                   SO_ERROR afterwards.
//       W-close     as W-partial, but another thread closes c; 100 ms later
//                   p drains everything. Whether the writer has returned is
//                   reported after each step.
//   D   (Linux only; needs CAP_SYS_NICE) the sleeper and a hog share CPU 0
//       under SCHED_FIFO, the hog higher, so that both the call's own wake
//       and the signal hold before the sleeper next runs. The wake is made 5
//       ms into the hog and the signal sent 10 ms in (ready-first), or the
//       other way round (signal-first); the hog spins to 40 ms. 20 trials
//       each, no SA_RESTART:
//       D-read      a reader as R; the wake is p writing 100 bytes.
//       D-write     a writer as W-partial; the wake is p reading 4 MiB, far
//                   past any threshold. Reported: how many answered with more
//                   than the writer had taken before the wake (it wrote into
//                   the freed space) and how many with exactly that (the
//                   signal ended it first).
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -pthread -o /tmp/tcp-blocking tcp-blocking.c && /tmp/tcp-blocking
//   Linux:  container run --rm --cap-add CAP_SYS_NICE -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -pthread -o /tmp/p /probe/tcp-blocking.c && /tmp/p'
// An argument restricts the run to the sections it names, e.g. `RW`; the
// default is `RWD`.
//
// Measured 2026-10-08 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), twice each; one run of
// each is tcp-blocking.linux-6.18.5-aarch64.txt and tcp-blocking.darwin-27.0.txt.
// The answers were the same in both runs; the byte counts, and the order in
// R-order, were not. What they say, on both unless stated:
//   R   a sleeping read returns 100 for the bytes, 0 for the FIN, and
//       ECONNRESET for the reset (taking it; the next read is 0). A signal
//       gives EINTR, or under SA_RESTART the read sleeps on and returns the
//       bytes. Closing the descriptor ends the read with EBADF on Darwin;
//       on Linux the read sleeps on and returns the peer's bytes. Three
//       readers return in no fixed order: Linux 201 or 210, Darwin 012 or
//       021.
//   W   a blocking write returns only once all N are taken. Linux's sleeping
//       writer takes nothing while its queue (SIOCOUTQ) drains from 4194304
//       to about two thirds, then refills it; Darwin's takes room as reads
//       make it. A signal returns the count taken, with SA_RESTART or not, or
//       with nothing taken EINTR, or under SA_RESTART a write that goes on
//       and returns 1000. A reset: Linux returns the count taken, leaving
//       ECONNRESET in SO_ERROR, or with nothing taken ECONNRESET (taking it,
//       no SIGPIPE); Darwin answers EPIPE and raises SIGPIPE either way,
//       leaving ECONNRESET in SO_ERROR. Closing the descriptor: Darwin EBADF
//       whatever was taken, and no SIGPIPE; Linux's write sleeps on, and
//       completes once the peer drains.
//   D   Linux: the reader answered the bytes in 40 of 40, whichever came
//       first; the writer took none of the room and returned its count in 40
//       of 40, whichever came first.

#ifdef __linux__
#define _GNU_SOURCE
#endif

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <sched.h>
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
#include <linux/sockios.h>
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
    static char other[32];
    switch (e)
    {
        case 0: return "0";
        case EAGAIN: return "EAGAIN";
        case EPIPE: return "EPIPE";
        case ECONNRESET: return "ECONNRESET";
        case ENOTCONN: return "ENOTCONN";
        case EINTR: return "EINTR";
        case EBADF: return "EBADF";
        case EINVAL: return "EINVAL";
        default:
            snprintf(other, sizeof other, "errno%d", e);
            return other;
    }
}

#ifdef __linux__
static int64_t now_us(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}
#endif

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

static int getint(int fd, int level, int name)
{
    int v = -1;
    socklen_t len = sizeof v;
    if (getsockopt(fd, level, name, &v, &len) < 0) return -1000 - errno;
    return v;
}

static int nread(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) < 0) return -1000 - errno;
    return n;
}

static int unsent(int fd)
{
#ifdef __linux__
    int n = -1;
    if (ioctl(fd, SIOCOUTQ, &n) < 0) return -1000 - errno;
    return n;
#else
    return getint(fd, SOL_SOCKET, SO_NWRITE);
#endif
}

// A connected blocking pair over 127.0.0.1: *c connecting, *p accepted.
static void make_pair(int sndbuf, int *c, int *p)
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
    if (sndbuf && setsockopt(*c, SOL_SOCKET, SO_SNDBUF, &sndbuf, sizeof sndbuf) < 0) die("SO_SNDBUF");
    if (connect(*c, (struct sockaddr *)&a, sizeof a) < 0) die("connect");
    *p = accept(l, NULL, NULL);
    if (*p < 0) die("accept");
    close(l);
}

static char payload[8 << 20];
static char sink[1 << 22];

typedef struct
{
    int write;     // 0: read, 1: write
    int fd;
    size_t len;
    atomic_int started;
    atomic_int done;
    ssize_t rv;
    int error;
    int index;
    int fifo;      // pin to CPU 0 under SCHED_FIFO 10 first (section D)
} call;

#ifdef __linux__
static int pin_fifo(int prio)
{
    cpu_set_t set;
    CPU_ZERO(&set);
    CPU_SET(0, &set);
    if (pthread_setaffinity_np(pthread_self(), sizeof set, &set) != 0) return -1;
    struct sched_param sp = { .sched_priority = prio };
    return pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp);
}
#endif

static atomic_int order_log[8];
static atomic_int order_len;

static void *run_call(void *arg)
{
    call *k = arg;
#ifdef __linux__
    if (k->fifo)
    {
        int pinned = pin_fifo(10);
        if (pinned != 0)
        {
            k->rv = -100 - pinned;
            atomic_store(&k->started, 1);
            atomic_store(&k->done, 1);
            return NULL;
        }
    }
#endif
    atomic_store(&k->started, 1);
    errno = 0;
    if (k->write)
        k->rv = write(k->fd, payload, k->len);
    else
        k->rv = read(k->fd, sink, k->len);
    k->error = k->rv < 0 ? errno : 0;
    int at = atomic_fetch_add(&order_len, 1);
    if (at < 8) atomic_store(&order_log[at], k->index);
    atomic_store(&k->done, 1);
    return NULL;
}

static void start(call *k, pthread_t *t, int write, int fd, size_t len)
{
    memset(k, 0, sizeof *k);
    k->write = write;
    k->fd = fd;
    k->len = len;
    atomic_init(&k->started, 0);
    atomic_init(&k->done, 0);
    if (pthread_create(t, NULL, run_call, k) != 0) die("pthread_create");
    while (!atomic_load(&k->started)) { }
}

static const char *state(call *k, char *out, size_t len)
{
    if (!atomic_load(&k->done))
        snprintf(out, len, "asleep");
    else if (k->rv < 0)
        snprintf(out, len, "-1 %s", errname(k->error));
    else
        snprintf(out, len, "%zd", k->rv);
    return out;
}

// Read whatever is waiting on fd, non-blocking, until 50 ms pass with nothing.
static long long drain(int fd)
{
    set_nonblocking(fd, 1);
    long long total = 0;
    int idle = 0;
    while (idle < 10)
    {
        ssize_t n = read(fd, sink, sizeof sink);
        if (n > 0) { total += n; idle = 0; continue; }
        idle++;
        sleep_ms(5);
    }
    set_nonblocking(fd, 0);
    return total;
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
        // One more attempt straight away is what decides "took nothing".
        n = write(fd, payload, 65536);
        if (n > 0) { total += n; quiet = 0; }
    }
    set_nonblocking(fd, 0);
    return total;
}

static void section_r(void)
{
    char s1[64], s2[64], s3[64];
    for (int variant = 0; variant < 5; variant++)
    {
        const char *names[] = { "R-data", "R-fin", "R-reset", "R-eintr", "R-restart" };
        install(variant == 4);
        atomic_store(&sigpipes, 0);
        atomic_store(&sigusr1s, 0);
        int c, p;
        make_pair(0, &c, &p);
        if (variant == 2 && write(c, payload, 100) != 100) die("R-reset write");
        sleep_ms(20);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 4096);
        sleep_ms(100);
        switch (variant)
        {
            case 0: if (write(p, payload, 100) != 100) die("R-data write"); break;
            case 1:
            case 2: close(p); p = -1; break;
            case 3:
            case 4: pthread_kill(t, SIGUSR1); break;
        }
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        if (variant == 4)
        {
            if (write(p, payload, 100) != 100) die("R-restart write");
            sleep_ms(100);
        }
        state(&k, s2, sizeof s2);
        if (!atomic_load(&k.done)) { close(p); p = -1; sleep_ms(100); }
        pthread_join(t, NULL);
        // A second read, non-blocking, after the first returned.
        set_nonblocking(c, 1);
        ssize_t again = read(c, sink, 4096);
        snprintf(s3, sizeof s3, "%zd %s", again, again < 0 ? errname(errno) : "");
        printf("R\t%s\tafter=%s\tthen=%s\tsecond_read=%s\tsigusr1=%d\tsigpipe=%d\tso_error=%s\n", names[variant], s1, s2,
               s3, atomic_load(&sigusr1s), atomic_load(&sigpipes), errname(getint(c, SOL_SOCKET, SO_ERROR)));
        close(c);
        if (p >= 0) close(p);
    }

    // R-close
    {
        install(0);
        int c, p;
        make_pair(0, &c, &p);
        call k;
        pthread_t t;
        start(&k, &t, 0, c, 4096);
        sleep_ms(100);
        int closed = close(c);
        int close_errno = closed < 0 ? errno : 0;
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        ssize_t w = write(p, payload, 100);
        sleep_ms(100);
        state(&k, s2, sizeof s2);
        close(p);
        sleep_ms(100);
        state(&k, s3, sizeof s3);
        printf("R\tR-close\tclose=%d %s\tafter_close=%s\tafter_peer_write(%zd)=%s\tafter_peer_close=%s\n", closed,
               errname(close_errno), s1, w, s2, s3);
        if (!atomic_load(&k.done)) { printf("R\tR-close\treader still asleep; leaving it\n"); pthread_detach(t); }
        else pthread_join(t, NULL);
    }

    // R-order
    {
        install(0);
        char tally[10][8];
        for (int trial = 0; trial < 10; trial++)
        {
            int c, p;
            make_pair(0, &c, &p);
            atomic_store(&order_len, 0);
            call k[3];
            pthread_t t[3];
            for (int i = 0; i < 3; i++)
            {
                start(&k[i], &t[i], 0, c, 1);
                k[i].index = i;
                sleep_ms(30);
            }
            for (int i = 0; i < 3; i++)
            {
                if (write(p, payload, 1) != 1) die("R-order write");
                sleep_ms(100);
            }
            for (int i = 0; i < 3; i++) pthread_join(t[i], NULL);
            for (int i = 0; i < 3; i++) tally[trial][i] = (char)('0' + atomic_load(&order_log[i]));
            tally[trial][3] = 0;
            close(c);
            close(p);
        }
        printf("R\tR-order\t");
        for (int trial = 0; trial < 10; trial++) printf("%s ", tally[trial]);
        printf("\n");
    }
}

static void section_w(void)
{
    char s1[64], s2[64];

    // W-resume
    for (int small = 0; small <= 1; small++)
    {
        install(0);
        int c, p;
        make_pair(small ? 32768 : 0, &c, &p);
        size_t n = small ? (1 << 20) : (8 << 20);
        call k;
        pthread_t t;
        start(&k, &t, 1, c, n);
        sleep_ms(200);
        long long got = 0;
        long long last_taken = -1;
        int last_unsent = unsent(c);
        printf("W\tW-resume\t%s\tsndbuf=%d\trcvbuf(p)=%d\n", small ? "sndbuf32768" : "defaults",
               getint(c, SOL_SOCKET, SO_SNDBUF), getint(p, SOL_SOCKET, SO_RCVBUF));
        int lines = 0;
        while (!atomic_load(&k.done) || nread(p) > 0)
        {
            int queued = nread(p);
            int out = unsent(c);
            long long taken = got + queued + out;
            if (taken != last_taken && lines < 200)
            {
                printf("W\tW-resume\tread=%lld\tfionread=%d\tunsent_before=%d\tunsent=%d\ttaken=%lld\tsndbuf=%d\n", got,
                       queued, last_unsent, out, taken, getint(c, SOL_SOCKET, SO_SNDBUF));
                lines++;
                last_taken = taken;
            }
            last_unsent = out;
            ssize_t r = recv(p, sink, 16384, MSG_DONTWAIT);
            if (r > 0) got += r;
            sleep_ms(5);
        }
        pthread_join(t, NULL);
        got += drain(p);
        printf("W\tW-resume\tanswer=%s\tread_in_all=%lld\n", state(&k, s1, sizeof s1), got);
        close(c);
        close(p);
    }

    // W-partial, W-empty, W-reset (partial and empty), W-close
    for (int variant = 0; variant < 7; variant++)
    {
        const char *names[] = { "W-partial", "W-partial-restart", "W-empty", "W-empty-restart",
                                "W-reset-partial", "W-reset-empty", "W-close" };
        int restart = variant == 1 || variant == 3;
        int empty = variant == 2 || variant == 3 || variant == 5;
        install(restart);
        atomic_store(&sigpipes, 0);
        atomic_store(&sigusr1s, 0);
        int c, p;
        make_pair(0, &c, &p);
        // The peer has something unread of its own, so that its close resets.
        if (write(c, payload, 1) != 1) die("seed");
        long long before = empty ? fill(c) : 0;
        call k;
        pthread_t t;
        start(&k, &t, 1, c, empty ? 1000 : (8 << 20));
        sleep_ms(200);
        int queued = nread(p);
        int out = unsent(c);
        switch (variant)
        {
            case 0: case 1: case 2: case 3: pthread_kill(t, SIGUSR1); break;
            case 4: case 5: close(p); p = -1; break;
            case 6: close(c); break;
        }
        sleep_ms(100);
        state(&k, s1, sizeof s1);
        long long drained = -1;
        if (!atomic_load(&k.done) && p >= 0)
        {
            drained = drain(p);
            sleep_ms(100);
        }
        state(&k, s2, sizeof s2);
        if (!atomic_load(&k.done)) { printf("W\t%s\twriter still asleep; leaving it\n", names[variant]); pthread_detach(t); }
        else pthread_join(t, NULL);
        printf("W\t%s\tfilled_first=%lld\tpeer_fionread=%d\tunsent=%d\ttaken_by_call_before=%lld\tafter=%s\tdrained=%lld\tthen=%s\tsigusr1=%d\tsigpipe=%d\tso_error=%s\n",
               names[variant], before, queued, out, (long long)queued + out - 1 - before, s1, drained, s2,
               atomic_load(&sigusr1s), atomic_load(&sigpipes),
               variant == 6 ? "-" : errname(getint(c, SOL_SOCKET, SO_ERROR)));
        if (variant != 6) close(c);
        if (p >= 0) close(p);
    }
}

#ifdef __linux__
static void section_d(void)
{
    install(0);
    for (int write_side = 0; write_side <= 1; write_side++)
    {
        for (int signal_first = 0; signal_first <= 1; signal_first++)
        {
            int ready = 0, eintr = 0, partial_only = 0, other = 0;
            for (int trial = 0; trial < 20; trial++)
            {
                int c, p;
                make_pair(0, &c, &p);
                call k;
                pthread_t t;
                memset(&k, 0, sizeof k);
                k.write = write_side;
                k.fd = c;
                k.len = write_side ? (8 << 20) : 4096;
                k.fifo = 1;
                atomic_init(&k.started, 0);
                atomic_init(&k.done, 0);
                if (pthread_create(&t, NULL, run_call, &k) != 0) die("pthread_create");
                while (!atomic_load(&k.started)) { }
                if (k.rv <= -100)
                {
                    printf("D\tsleeper could not take SCHED_FIFO on CPU 0: error %zd\n", -100 - k.rv);
                    pthread_join(t, NULL);
                    return;
                }
                sleep_ms(write_side ? 200 : 20);
                long long taken_before = write_side ? (long long)nread(p) + unsent(c) : 0;
                int hog = pin_fifo(20);
                if (hog != 0)
                {
                    printf("D\thog could not take SCHED_FIFO on CPU 0: error %d\n", hog);
                    pthread_kill(t, SIGUSR1);
                    pthread_join(t, NULL);
                    return;
                }
                int64_t t0 = now_us();
                int woke = 0, signalled = 0;
                long long freed = 0;
                int wake_at = signal_first ? 10000 : 5000;
                int signal_at = signal_first ? 5000 : 10000;
                while (now_us() - t0 < 40000)
                {
                    int64_t dt = now_us() - t0;
                    if (!woke && dt >= wake_at)
                    {
                        if (write_side)
                        {
                            set_nonblocking(p, 1);
                            while (freed < (4 << 20))
                            {
                                ssize_t r = read(p, sink, sizeof sink);
                                if (r <= 0) break;
                                freed += r;
                            }
                        }
                        else if (write(p, payload, 100) != 100) die("D write");
                        woke = 1;
                    }
                    if (!signalled && dt >= signal_at)
                    {
                        pthread_kill(t, SIGUSR1);
                        signalled = 1;
                    }
                }
                struct sched_param sp = { .sched_priority = 0 };
                pthread_setschedparam(pthread_self(), SCHED_OTHER, &sp);
                {
                    cpu_set_t all;
                    CPU_ZERO(&all);
                    for (int i = 0; i < 64; i++) CPU_SET(i, &all);
                    pthread_setaffinity_np(pthread_self(), sizeof all, &all);
                }
                if (write_side)
                {
                    // A writer that went back to sleep is ended by draining.
                    sleep_ms(20);
                    if (!atomic_load(&k.done)) drain(p);
                }
                pthread_join(t, NULL);
                if (!write_side)
                {
                    if (k.rv > 0) ready++;
                    else if (k.rv < 0 && k.error == EINTR) eintr++;
                    else other++;
                }
                else
                {
                    if (k.rv < 0 && k.error == EINTR) eintr++;
                    else if (k.rv == taken_before) partial_only++;
                    else if (k.rv > taken_before) ready++;
                    else other++;
                }
                close(c);
                close(p);
            }
            printf("D\t%s\t%s\twrote_more_or_read=%d\tpartial_only=%d\teintr=%d\tother=%d\n",
                   write_side ? "D-write" : "D-read", signal_first ? "signal-first" : "ready-first", ready,
                   partial_only, eintr, other);
        }
    }
}
#endif

int main(int argc, char **argv)
{
    const char *sections = argc > 1 ? argv[1] : "RWD";
    setvbuf(stdout, NULL, _IOLBF, 0);
    memset(payload, 'x', sizeof payload);
    if (strchr(sections, 'R')) section_r();
    if (strchr(sections, 'W')) section_w();
#ifdef __linux__
    if (strchr(sections, 'D')) section_d();
#endif
    return 0;
}
