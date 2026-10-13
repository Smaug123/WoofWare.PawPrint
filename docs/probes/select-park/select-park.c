// What a reserved `select(2)` park must be able to hold, measured on both
// flavours. Design note: pawprint-kernel-design/select-park-options.md.
//
// Build and run:
//   Darwin: cc -Wall -o /tmp/sp select-park.c -lpthread && /tmp/sp
//   Linux:  gcc -Wall -o /tmp/sp select-park.c -lpthread && /tmp/sp
//
// Sections:
//   A  select's timeout argument after each way the call ends
//   B  the screens on the timeout's fields
//   C  pselect and ppoll: does the timeout come back, through libc and raw
//   D  the temporary mask of pselect and ppoll
//   E  a close of a selected descriptor while the call sleeps
//   F  counting, and descriptors past nfds or closed before the call
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif

static volatile sig_atomic_t handled = 0;
static void on_usr1(int sig) { (void)sig; handled++; }

static double now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1e3 + ts.tv_nsec / 1e6;
}

static void install(int restart)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sa.sa_flags = restart ? SA_RESTART : 0;
    sigaction(SIGUSR1, &sa, NULL);
}

struct later { pthread_t target; int ms; int signal_it; int fd_to_close; int fd_to_write; };

static void *do_later(void *p)
{
    struct later *l = p;
    usleep(l->ms * 1000);
    if (l->signal_it) pthread_kill(l->target, SIGUSR1);
    if (l->fd_to_close >= 0) close(l->fd_to_close);
    if (l->fd_to_write >= 0) { char c = 'x'; (void)!write(l->fd_to_write, &c, 1); }
    return NULL;
}

static pthread_t start_later(int ms, int signal_it, int fd_to_close, int fd_to_write, struct later *l)
{
    pthread_t t;
    l->target = pthread_self();
    l->ms = ms; l->signal_it = signal_it; l->fd_to_close = fd_to_close; l->fd_to_write = fd_to_write;
    pthread_create(&t, NULL, do_later, l);
    return t;
}

static void show_tv(const char *row, int r, int e, struct timeval *tv, double took)
{
    printf("%s: ret=%d errno=%s tv_after=%ld.%06ld took_ms=%.0f\n", row, r, r < 0 ? strerror(e) : "-",
           (long)tv->tv_sec, (long)tv->tv_usec, took);
}

static void section_a(void)
{
    int p[2];
    struct timeval tv;
    fd_set rd;
    double t0;
    int r, e;
    struct later l;
    pthread_t t;

    // A1: runs its course.
    pipe(p);
    FD_ZERO(&rd); FD_SET(p[0], &rd);
    tv.tv_sec = 0; tv.tv_usec = 300000;
    t0 = now_ms();
    r = select(p[0] + 1, &rd, NULL, NULL, &tv); e = errno;
    show_tv("A1 timeout 300ms, nothing happens", r, e, &tv, now_ms() - t0);

    // A2: a byte arrives 100 ms into 2 s.
    FD_ZERO(&rd); FD_SET(p[0], &rd);
    tv.tv_sec = 2; tv.tv_usec = 0;
    t = start_later(100, 0, -1, p[1], &l);
    t0 = now_ms();
    r = select(p[0] + 1, &rd, NULL, NULL, &tv); e = errno;
    show_tv("A2 timeout 2s, byte at 100ms", r, e, &tv, now_ms() - t0);
    pthread_join(t, NULL);
    { char c; (void)!read(p[0], &c, 1); }

    // A3: a caught signal 100 ms into 2 s, without and with SA_RESTART.
    for (int restart = 0; restart <= 1; restart++) {
        install(restart);
        handled = 0;
        FD_ZERO(&rd); FD_SET(p[0], &rd);
        tv.tv_sec = 2; tv.tv_usec = 0;
        t = start_later(100, 1, -1, -1, &l);
        t0 = now_ms();
        r = select(p[0] + 1, &rd, NULL, NULL, &tv); e = errno;
        char row[128];
        snprintf(row, sizeof row, "A3 timeout 2s, caught signal at 100ms, SA_RESTART=%d, handled=%d", restart, (int)handled);
        show_tv(row, r, e, &tv, now_ms() - t0);
        pthread_join(t, NULL);
    }
    install(0);

    // A4: a ready descriptor and a zero timeout: is the timeout touched?
    pipe(p);
    { char c = 'x'; (void)!write(p[1], &c, 1); }
    FD_ZERO(&rd); FD_SET(p[0], &rd);
    tv.tv_sec = 0; tv.tv_usec = 0;
    r = select(p[0] + 1, &rd, NULL, NULL, &tv); e = errno;
    show_tv("A4 zero timeout, ready", r, e, &tv, 0);

    // A5: the timeout in read-only memory, with a ready descriptor after 100 ms
    // and, separately, a caught signal. Each row runs in a child, since a C
    // library that writes the timeout back itself faults there. Under Linux,
    // the same through the raw pselect6, whose write-back is the kernel's.
    long page = sysconf(_SC_PAGESIZE);
    for (int raw = 0; raw <= 1; raw++) {
#ifndef __linux__
        if (raw) break;
#endif
        for (int which = 0; which < 2; which++) {
            pid_t child = fork();
            if (child == 0) {
                void *ro = mmap(NULL, page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
                struct timeval *rotv = ro;
                struct timespec *rots = ro;
                if (raw) { rots->tv_sec = 2; rots->tv_nsec = 0; } else { rotv->tv_sec = 2; rotv->tv_usec = 0; }
                mprotect(ro, page, PROT_READ);
                int q[2];
                pipe(q);
                FD_ZERO(&rd); FD_SET(q[0], &rd);
                handled = 0;
                t = start_later(100, which == 1, -1, which == 0 ? q[1] : -1, &l);
                t0 = now_ms();
#ifdef __linux__
                if (raw) r = syscall(SYS_pselect6, q[0] + 1, &rd, NULL, NULL, rots, NULL); else
#endif
                r = select(q[0] + 1, &rd, NULL, NULL, rotv);
                e = errno;
                printf("A5 %s, read-only timeout 2s, %s at 100ms: ret=%d errno=%s handled=%d took_ms=%.0f\n",
                       raw ? "raw pselect6" : "libc select", which == 0 ? "byte" : "caught signal", r,
                       r < 0 ? strerror(e) : "-", (int)handled, now_ms() - t0);
                pthread_join(t, NULL);
                _exit(0);
            }
            int status;
            waitpid(child, &status, 0);
            if (WIFSIGNALED(status))
                printf("A5 %s, read-only timeout 2s, %s at 100ms: the process died of signal %d\n",
                       raw ? "raw pselect6" : "libc select", which == 0 ? "byte" : "caught signal", WTERMSIG(status));
        }
    }

    // A6: an invalid descriptor in the set, with a 2 s timeout: the screen
    // fails, and is the timeout still written?
    FD_ZERO(&rd); FD_SET(p[0], &rd); FD_SET(200, &rd);
    tv.tv_sec = 2; tv.tv_usec = 0;
    r = select(201, &rd, NULL, NULL, &tv); e = errno;
    show_tv("A6 descriptor 200 not open, timeout 2s", r, e, &tv, 0);
    close(p[0]); close(p[1]);
}

static void section_b(void)
{
    struct timeval cases[] = { { 0, 1000000 }, { 0, 1500000 }, { 0, -1 }, { -1, 0 }, { 0, 999999 } };
    for (unsigned i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        struct timeval tv = cases[i];
        double t0 = now_ms();
        int r = select(0, NULL, NULL, NULL, &tv); int e = errno;
        char row[128];
        snprintf(row, sizeof row, "B timeout {%ld, %ld}", (long)cases[i].tv_sec, (long)cases[i].tv_usec);
        show_tv(row, r, e, &tv, now_ms() - t0);
    }
}

static void section_c(void)
{
    struct timespec ts;
    int r, e;
    double t0;
    int p[2];
    pipe(p);
    fd_set rd;
    struct later l;
    pthread_t t;

    // C1: libc pselect, a byte at 100 ms into 2 s.
    FD_ZERO(&rd); FD_SET(p[0], &rd);
    ts.tv_sec = 2; ts.tv_nsec = 0;
    t = start_later(100, 0, -1, p[1], &l);
    t0 = now_ms();
    r = pselect(p[0] + 1, &rd, NULL, NULL, &ts, NULL); e = errno;
    printf("C1 libc pselect 2s, byte at 100ms: ret=%d errno=%s ts_after=%ld.%09ld took_ms=%.0f\n", r,
           r < 0 ? strerror(e) : "-", (long)ts.tv_sec, ts.tv_nsec, now_ms() - t0);
    pthread_join(t, NULL);
    { char c; (void)!read(p[0], &c, 1); }

#ifdef __linux__
    // C2: raw pselect6, the same.
    FD_ZERO(&rd); FD_SET(p[0], &rd);
    ts.tv_sec = 2; ts.tv_nsec = 0;
    t = start_later(100, 0, -1, p[1], &l);
    t0 = now_ms();
    r = syscall(SYS_pselect6, p[0] + 1, &rd, NULL, NULL, &ts, NULL); e = errno;
    printf("C2 raw pselect6 2s, byte at 100ms: ret=%d errno=%s ts_after=%ld.%09ld took_ms=%.0f\n", r,
           r < 0 ? strerror(e) : "-", (long)ts.tv_sec, ts.tv_nsec, now_ms() - t0);
    pthread_join(t, NULL);
    { char c; (void)!read(p[0], &c, 1); }

    // C3: libc ppoll and raw ppoll, the same.
    for (int raw = 0; raw <= 1; raw++) {
        struct pollfd pfd = { p[0], POLLIN, 0 };
        (void)pfd;
        ts.tv_sec = 2; ts.tv_nsec = 0;
        t = start_later(100, 0, -1, p[1], &l);
        t0 = now_ms();
        if (raw)
            r = syscall(SYS_ppoll, &pfd, 1, &ts, NULL, (size_t)8);
        else
            r = ppoll(&pfd, 1, &ts, NULL);
        e = errno;
        printf("C3 %s ppoll 2s, byte at 100ms: ret=%d errno=%s ts_after=%ld.%09ld took_ms=%.0f\n",
               raw ? "raw" : "libc", r, r < 0 ? strerror(e) : "-", (long)ts.tv_sec, ts.tv_nsec, now_ms() - t0);
        pthread_join(t, NULL);
        { char c; (void)!read(p[0], &c, 1); }
    }
#else
    printf("C2 raw pselect6: no such call\n");
    printf("C3 ppoll: no such call\n");
#endif
    close(p[0]); close(p[1]);
}

static int usr1_blocked(void)
{
    sigset_t cur;
    pthread_sigmask(SIG_BLOCK, NULL, &cur);
    return sigismember(&cur, SIGUSR1);
}

static int usr1_pending(void)
{
    sigset_t pend;
    sigpending(&pend);
    return sigismember(&pend, SIGUSR1);
}

static volatile sig_atomic_t blocked_in_handler = -1;
static void on_usr1_mask(int sig)
{
    (void)sig;
    handled++;
    sigset_t cur;
    pthread_sigmask(SIG_BLOCK, NULL, &cur);
    blocked_in_handler = sigismember(&cur, SIGUSR1) * 10 + sigismember(&cur, SIGUSR2);
}

static void section_d(void)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1_mask;
    sigaction(SIGUSR1, &sa, NULL);

    sigset_t block, empty;
    sigemptyset(&block); sigaddset(&block, SIGUSR1);
    sigemptyset(&empty);
    // The temporary mask blocks USR2 so the handler's mask can show it.
    sigset_t temp;
    sigemptyset(&temp); sigaddset(&temp, SIGUSR2);

    for (int ppoll_too = 0; ppoll_too <= 1; ppoll_too++) {
#ifndef __linux__
        if (ppoll_too) { printf("D ppoll: no such call\n"); break; }
#endif
        const char *call = ppoll_too ? "ppoll" : "pselect";
        int p[2];
        pipe(p);

        // D1: USR1 blocked and pending; the temporary mask lets it through;
        // nothing is ready and the timeout is 1 s.
        pthread_sigmask(SIG_SETMASK, &block, NULL);
        raise(SIGUSR1);
        handled = 0; blocked_in_handler = -1;
        struct timespec ts = { 1, 0 };
        fd_set rd; FD_ZERO(&rd); FD_SET(p[0], &rd);
        struct pollfd pfd = { p[0], POLLIN, 0 };
        (void)pfd;
        double t0 = now_ms();
        int r;
#ifdef __linux__
        if (ppoll_too) r = ppoll(&pfd, 1, &ts, &temp); else
#endif
        r = pselect(p[0] + 1, &rd, NULL, NULL, &ts, &temp);
        int e = errno;
        printf("D1 %s, pending USR1 the temporary mask unblocks, nothing ready: ret=%d errno=%s took_ms=%.0f "
               "handled=%d handler_mask(USR1*10+USR2)=%d after: usr1_blocked=%d usr1_pending=%d\n",
               call, r, r < 0 ? strerror(e) : "-", now_ms() - t0, (int)handled, (int)blocked_in_handler,
               usr1_blocked(), usr1_pending());

        // D2: the same, and the pipe is ready too.
        { char c = 'x'; (void)!write(p[1], &c, 1); }
        raise(SIGUSR1);
        handled = 0; blocked_in_handler = -1;
        FD_ZERO(&rd); FD_SET(p[0], &rd);
        ts.tv_sec = 1; ts.tv_nsec = 0;
#ifdef __linux__
        if (ppoll_too) r = ppoll(&pfd, 1, &ts, &temp); else
#endif
        r = pselect(p[0] + 1, &rd, NULL, NULL, &ts, &temp);
        e = errno;
        printf("D2 %s, pending USR1 the temporary mask unblocks, pipe ready: ret=%d errno=%s handled=%d "
               "after: usr1_blocked=%d usr1_pending=%d\n",
               call, r, r < 0 ? strerror(e) : "-", (int)handled, usr1_blocked(), usr1_pending());
        // Clear the pending USR1.
        pthread_sigmask(SIG_SETMASK, &empty, NULL);
        printf("D2 %s, unblocking afterwards: handled=%d\n", call, (int)handled);

        // D3: a signal the temporary mask blocks arrives while asleep, then
        // the deadline passes.
        pthread_sigmask(SIG_SETMASK, &empty, NULL);
        { char c; (void)!read(p[0], &c, 1); }
        handled = 0;
        sigset_t temp1; sigemptyset(&temp1); sigaddset(&temp1, SIGUSR1);
        struct later l;
        pthread_t t = start_later(100, 1, -1, -1, &l);
        ts.tv_sec = 0; ts.tv_nsec = 400000000;
        FD_ZERO(&rd); FD_SET(p[0], &rd);
        t0 = now_ms();
#ifdef __linux__
        if (ppoll_too) r = ppoll(&pfd, 1, &ts, &temp1); else
#endif
        r = pselect(p[0] + 1, &rd, NULL, NULL, &ts, &temp1);
        e = errno;
        int handled_at_return = handled;
        pthread_join(t, NULL);
        printf("D3 %s, USR1 blocked by the temporary mask arrives at 100ms, timeout 400ms: ret=%d errno=%s "
               "took_ms=%.0f handled_at_return=%d handled_after=%d\n",
               call, r, r < 0 ? strerror(e) : "-", now_ms() - t0, handled_at_return, (int)handled);
        close(p[0]); close(p[1]);
    }
    pthread_sigmask(SIG_SETMASK, &empty, NULL);
    install(0);
}

static void section_e(void)
{
    // E1: thread B closes the only selected descriptor (a pipe's read end)
    // 100 ms into a 1 s select.
    for (int with_dup = 0; with_dup <= 1; with_dup++) {
        int p[2];
        pipe(p);
        int extra = with_dup ? dup(p[0]) : -1;
        fd_set rd; FD_ZERO(&rd); FD_SET(p[0], &rd);
        struct timeval tv = { 1, 0 };
        struct later l;
        pthread_t t = start_later(100, 0, p[0], -1, &l);
        double t0 = now_ms();
        int r = select(p[0] + 1, &rd, NULL, NULL, &tv); int e = errno;
        double took = now_ms() - t0;
        pthread_join(t, NULL);
        printf("E1 close of the selected read end at 100ms of 1s%s: ret=%d errno=%s took_ms=%.0f in_set_after=%d\n",
               with_dup ? " (a dup keeps the pipe open)" : "", r, r < 0 ? strerror(e) : "-", took,
               r >= 0 ? FD_ISSET(p[0], &rd) : -1);
        if (extra >= 0) close(extra);
        close(p[1]);
    }

    // E2: the same, and a byte arrives on the pipe at 200 ms through the
    // write end (the read end's description lives on through the dup).
    {
        int p[2];
        pipe(p);
        int extra = dup(p[0]);
        fd_set rd; FD_ZERO(&rd); FD_SET(p[0], &rd);
        struct timeval tv = { 1, 0 };
        struct later l1, l2;
        pthread_t t1 = start_later(100, 0, p[0], -1, &l1);
        pthread_t t2 = start_later(200, 0, -1, p[1], &l2);
        double t0 = now_ms();
        int r = select(p[0] + 1, &rd, NULL, NULL, &tv); int e = errno;
        double took = now_ms() - t0;
        pthread_join(t1, NULL); pthread_join(t2, NULL);
        printf("E2 close at 100ms, byte at 200ms, a dup keeps the pipe open: ret=%d errno=%s took_ms=%.0f\n", r,
               r < 0 ? strerror(e) : "-", took);
        close(extra); close(p[1]);
    }
}

static void section_f(void)
{
    int s[2];
    socketpair(AF_UNIX, SOCK_STREAM, 0, s);
    (void)!write(s[1], "x", 1);
    fd_set rd, wr;
    FD_ZERO(&rd); FD_ZERO(&wr); FD_SET(s[0], &rd); FD_SET(s[0], &wr);
    struct timeval tv = { 0, 0 };
    int r = select(s[0] + 1, &rd, &wr, NULL, &tv);
    printf("F1 one socket readable and writable, in both sets: ret=%d\n", r);

    // F2: a descriptor at nfds is not looked at.
    FD_ZERO(&rd); FD_SET(s[0], &rd);
    tv.tv_sec = 0; tv.tv_usec = 0;
    r = select(s[0], &rd, NULL, NULL, &tv);
    printf("F2 the readable descriptor at nfds: ret=%d in_set_after=%d\n", r, FD_ISSET(s[0], &rd));

    // F3: nfds far past the table, nothing set.
    FD_ZERO(&rd);
    tv.tv_sec = 0; tv.tv_usec = 0;
    r = select(FD_SETSIZE, &rd, NULL, NULL, &tv); int e = errno;
    printf("F3 nfds=FD_SETSIZE, empty sets: ret=%d errno=%s\n", r, r < 0 ? strerror(e) : "-");
    r = select(-1, NULL, NULL, NULL, &tv); e = errno;
    printf("F3 nfds=-1: ret=%d errno=%s\n", r, r < 0 ? strerror(e) : "-");
    close(s[0]); close(s[1]);
}

int main(void)
{
    setvbuf(stdout, NULL, _IONBF, 0);
    install(0);
    section_a();
    section_b();
    section_c();
    section_d();
    section_e();
    section_f();
    return 0;
}
