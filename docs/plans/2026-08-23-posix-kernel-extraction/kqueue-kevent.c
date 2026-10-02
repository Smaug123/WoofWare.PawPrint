// kqueue(2) and the wait half of kevent(2) on Darwin: the descriptor kqueue()
// hands back, kevent's argument errors and the order it decides them in, its
// timeout, and what a close does to a thread asleep in it.
//
// Every kqueue below has nothing registered, so a wait on one can end only by
// its timeout, a signal, or a close. Elapsed times are CLOCK_MONOTONIC, in
// microseconds. A wait that would otherwise sleep for ever is cut short by a
// SIGALRM interval timer whose handler does not restart: EINTR at about the
// timer's interval means "it was still asleep".
//
// Sections:
//   A  kqueue(): with 3 and 4 open and 3 closed again, which descriptor comes
//      back; FD_CLOEXEC; the access mode and O_NONBLOCK from F_GETFL.
//   B  the argument sweep, nchanges/changelist x fd x nevents x eventlist x
//      timeout, every combination (5 x 7 x 4 x 3 x 12 = 5040 calls): rv, errno,
//      and whether it slept (cut short by a 40 ms timer). One line per call,
//      prefixed "B\t", for the analysis in kqueue-kevent-analyse.py.
//        changes:  0/NULL, -1/NULL, INT_MIN/NULL, 1/unreadable (0x8),
//                  -1/unreadable.
//        fd:       not open (500), -1, a UDP socket, a regular file, a pipe's
//                  read end, a kqueue, the kqueue's dup.
//        nevents:  1, 0, -1, INT_MIN.
//        events:   a real 4-entry array, NULL, unreadable (0x8).
//        timeout:  NULL, {0,0}, {0,20ms}, {0,-1}, {0,1e9}, {-1,0}, {-1,1},
//                  {INT64_MAX,0}, an unreadable pointer (0x8), {INT32_MAX,0},
//                  {INT32_MAX+1,0}, {0,1e9+1}.
//   C  expiry: timeout {0,0} (20 waits), and 1, 2, 3, 5, 10, 20 and 50 ms (20
//      waits each): rv, how many returned before the deadline, least and
//      greatest elapsed. Then a 10 s timeout, a {1e10, 0} one and a
//      {0, 999999999} one, each cut short at 100 ms; and nevents 0 and -1
//      under a 200 ms timeout, which may answer at once or sleep.
//   D  a signal with a handler while asleep, under SA_RESTART and without,
//      under a NULL timeout and under a 2 s one: rv, errno, elapsed.
//   E  closing the kqueue while another thread is asleep in kevent on it (NULL
//      timeout, a 1000 ms safety timer on the sleeper's side). The close is
//      made 50 ms into the wait. Each row reports the sleeper's rv/errno/
//      elapsed and the close's rv/errno/duration, and whether the number the
//      close freed is reusable at once:
//        E1  the sleeper entered through fd K, and K is closed (no dup);
//        E2  the sleeper entered through K, and a dup D of it is closed;
//        E3  the sleeper entered through D, and K is closed;
//        E4  the sleeper entered through K, and K is closed while D lives;
//        E5  two sleepers, one through K and one through D; K is closed;
//        E6  as E1 with a 300 ms timeout on the wait;
//        E7  as E1, but the closing thread then makes a new kqueue at once.
//        E8  as E4, and then, once the sleeper has returned, two more calls
//            through D: a {0,0} wait and a 100 ms wait.
//      10 trials each.
//   F  a drained kqueue: one sleeper through K, a dup D, K closed (as E4); then
//      through D, every combination of nchanges/changelist (0/NULL,
//      1/unreadable, -1/NULL), nevents (1, 0, -1) and timeout (NULL, {0,0},
//      {0,20ms}, {0,-1}, unreadable), with a real eventlist (45 calls, each cut
//      short by a 40 ms timer): rv, errno, whether it slept. Then close(D).
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -O1 -o /tmp/kk kqueue-kevent.c && /tmp/kk
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64 (xnu-13432.1.9), twice with the
// same answers; kqueue-kevent.darwin-27.0.txt is the full output, and
// kqueue-kevent-analyse.py reduces section B to the rule below and checks
// every line against it:
//   A  kqueue() takes the lowest free descriptor (3, with 3 free and 4 open),
//      O_RDWR, not O_NONBLOCK, and with FD_CLOEXEC set.
//   B  all 5040 calls agree with one ladder: an unreadable timeout is EFAULT
//      and a timeout with tv_sec < 0, tv_sec > INT32_MAX, tv_nsec < 0 or
//      tv_nsec > 1000000000 is EINVAL (tv_nsec of exactly 1000000000 is
//      accepted), ahead of everything else; then a descriptor that is not open
//      or not a kqueue (-1, a socket, a file, a pipe) is EBADF; then a positive
//      nchanges whose first change cannot be read is EFAULT, and a zero or
//      negative nchanges reads nothing; then nevents <= 0 returns 0 at once;
//      then an empty kqueue returns 0 at once for {0,0}, 0 at a positive
//      timeout, and sleeps for a NULL one. The eventlist is never read: NULL
//      and unreadable eventlists answer as a real one does.
//   C  no wait returned before its timeout (20 of each of 1..50 ms) and every
//      one returned 0; {0,0} returned within 20 us. {10,0} and {0,999999999}
//      slept until cut short; {1e10,0} is EINVAL even with nevents 0; nevents
//      0 and -1 return 0 at once under a 200 ms timeout.
//   D  a caught signal ends the wait with EINTR, with SA_RESTART and without,
//      under a NULL timeout and a 2 s one.
//   E  closing the descriptor a sleeper entered through ends its wait at once
//      with EBADF (E1, E4, E6, E7: 40 of 40), and ends with EBADF every other
//      sleeper on that kqueue too, whichever descriptor it entered through
//      (E5: 10 of 10). Closing a descriptor no sleeper entered through wakes
//      nobody (E2, E3: 20 of 20, each sleeper still asleep a second later).
//      The close itself returns 0 at once and frees the number for the next
//      open.
//   F  a kqueue a close has ended a wait on stays that way: every later wait
//      through a surviving dup that gets past the ladder above to the kqueue
//      itself (nevents > 0) is EBADF at once, whatever its timeout ({0,0}
//      included); the timeout, changelist and nevents rows ahead of it answer
//      as on any kqueue. Closing the dup then succeeds.
//   E8 agrees with F through a {0,0} and a 100 ms wait, 10 of 10.
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

static int64_t now_us(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static volatile sig_atomic_t alarms;
static void on_alarm(int sig) { (void)sig; alarms++; }

static void install(int sig, int restart)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_alarm;
    sa.sa_flags = restart ? SA_RESTART : 0;
    sigemptyset(&sa.sa_mask);
    sigaction(sig, &sa, NULL);
}

// One SIGALRM `ms` from now (0 disarms).
static void arm(int ms)
{
    struct itimerval it;
    memset(&it, 0, sizeof it);
    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_usec = (ms % 1000) * 1000;
    setitimer(ITIMER_REAL, &it, NULL);
}

static const char *ename(int e)
{
    switch (e) {
    case 0: return "-";
    case EBADF: return "EBADF";
    case EINVAL: return "EINVAL";
    case EFAULT: return "EFAULT";
    case EINTR: return "EINTR";
    case ENOENT: return "ENOENT";
    default: return strerror(e);
    }
}

// ---- A ------------------------------------------------------------------

static void section_a(void)
{
    int a = open("/dev/null", O_RDONLY);
    int b = open("/dev/null", O_RDONLY);
    close(a);
    int kq = kqueue();
    int fdflags = fcntl(kq, F_GETFD);
    int flflags = fcntl(kq, F_GETFL);
    printf("A\tfirst-free=%d kqueue=%d FD_CLOEXEC=%d accmode=%d(O_RDONLY=%d O_WRONLY=%d O_RDWR=%d) O_NONBLOCK=%d fl=0x%x\n",
           a, kq, (fdflags & FD_CLOEXEC) != 0, flflags & O_ACCMODE, O_RDONLY, O_WRONLY, O_RDWR,
           (flflags & O_NONBLOCK) != 0, flflags);
    close(kq);
    close(b);
}

// ---- B ------------------------------------------------------------------

static void section_b(void)
{
    install(SIGALRM, 0);
    char path[] = "/tmp/kqueue-kevent-XXXXXX";
    int file = mkstemp(path);
    unlink(path);
    int pipefd[2];
    pipe(pipefd);
    int sock = socket(AF_INET, SOCK_DGRAM, 0);
    int kq = kqueue();
    int kqdup = dup(kq);

    struct { const char *name; int fd; } fds[] = {
        { "closed", 500 }, { "minus1", -1 }, { "socket", sock }, { "file", file },
        { "pipe", pipefd[0] }, { "kqueue", kq }, { "kqueue-dup", kqdup },
    };
    struct { const char *name; int n; const struct kevent *list; } changes[] = {
        { "0/NULL", 0, NULL }, { "-1/NULL", -1, NULL }, { "INT_MIN/NULL", INT_MIN, NULL },
        { "1/unreadable", 1, (const struct kevent *)8 }, { "-1/unreadable", -1, (const struct kevent *)8 },
    };
    int nevents[] = { 1, 0, -1, INT_MIN };
    struct kevent real[4];
    struct { const char *name; struct kevent *list; } events[] = {
        { "real", real }, { "NULL", NULL }, { "unreadable", (struct kevent *)8 },
    };
    struct timespec ts_zero = { 0, 0 }, ts_20ms = { 0, 20000000 }, ts_nsneg = { 0, -1 },
                    ts_nsbig = { 0, 1000000000 }, ts_secneg = { -1, 0 }, ts_secneg1 = { -1, 1 },
                    ts_huge = { INT64_MAX, 0 }, ts_secmax = { INT32_MAX, 0 },
                    ts_secover = { (int64_t)INT32_MAX + 1, 0 }, ts_nsover = { 0, 1000000001 };
    struct { const char *name; const struct timespec *ts; } timeouts[] = {
        { "NULL", NULL }, { "{0,0}", &ts_zero }, { "{0,20ms}", &ts_20ms }, { "{0,-1}", &ts_nsneg },
        { "{0,1e9}", &ts_nsbig }, { "{-1,0}", &ts_secneg }, { "{-1,1}", &ts_secneg1 },
        { "{INT64_MAX,0}", &ts_huge }, { "unreadable", (const struct timespec *)8 },
        { "{INT32_MAX,0}", &ts_secmax }, { "{INT32_MAX+1,0}", &ts_secover }, { "{0,1e9+1}", &ts_nsover },
    };

    for (size_t c = 0; c < sizeof changes / sizeof changes[0]; c++)
    for (size_t f = 0; f < sizeof fds / sizeof fds[0]; f++)
    for (size_t n = 0; n < sizeof nevents / sizeof nevents[0]; n++)
    for (size_t e = 0; e < sizeof events / sizeof events[0]; e++)
    for (size_t t = 0; t < sizeof timeouts / sizeof timeouts[0]; t++) {
        alarms = 0;
        arm(40);
        int64_t start = now_us();
        errno = 0;
        int rv = kevent(fds[f].fd, changes[c].list, changes[c].n, events[e].list, nevents[n], timeouts[t].ts);
        int err = rv < 0 ? errno : 0;
        int64_t elapsed = now_us() - start;
        arm(0);
        // "slept": cut short by the timer, or took at least 15 ms.
        int slept = (err == EINTR && alarms > 0) || elapsed >= 15000;
        printf("B\t%s\t%s\t%d\t%s\t%s\trv=%d\t%s\tslept=%d\t%lld\n",
               changes[c].name, fds[f].name, nevents[n], events[e].name, timeouts[t].name,
               rv, ename(err), slept, (long long)elapsed);
    }

    close(kqdup); close(kq); close(sock); close(pipefd[0]); close(pipefd[1]); close(file);
}

// ---- C ------------------------------------------------------------------

static void section_c(void)
{
    install(SIGALRM, 0);
    int kq = kqueue();
    struct kevent out[1];
    int ms[] = { 0, 1, 2, 3, 5, 10, 20, 50 };
    for (size_t i = 0; i < sizeof ms / sizeof ms[0]; i++) {
        int early = 0, nonzero = 0;
        int64_t least = INT64_MAX, most = 0;
        for (int k = 0; k < 20; k++) {
            struct timespec ts = { 0, (long)ms[i] * 1000000L };
            int64_t start = now_us();
            int rv = kevent(kq, NULL, 0, out, 1, &ts);
            int64_t elapsed = now_us() - start;
            if (rv != 0) nonzero++;
            if (elapsed < (int64_t)ms[i] * 1000) early++;
            if (elapsed < least) least = elapsed;
            if (elapsed > most) most = elapsed;
        }
        printf("C\ttimeout=%dms\tnonzero=%d\tearly=%d\tleast=%lld\tmost=%lld\n", ms[i], nonzero, early,
               (long long)least, (long long)most);
    }

    struct { const char *name; struct timespec ts; int nevents; } longs[] = {
        { "{10,0}", { 10, 0 }, 1 }, { "{1e10,0}", { 10000000000LL, 0 }, 1 }, { "{0,999999999}", { 0, 999999999 }, 1 },
        { "{0,200ms} nevents=0", { 0, 200000000 }, 0 }, { "{0,200ms} nevents=-1", { 0, 200000000 }, -1 },
        { "{1e10,0} nevents=0", { 10000000000LL, 0 }, 0 },
    };
    for (size_t i = 0; i < sizeof longs / sizeof longs[0]; i++) {
        alarms = 0;
        arm(100);
        int64_t start = now_us();
        errno = 0;
        int rv = kevent(kq, NULL, 0, out, longs[i].nevents, &longs[i].ts);
        int err = rv < 0 ? errno : 0;
        int64_t elapsed = now_us() - start;
        arm(0);
        printf("C\t%s\trv=%d\t%s\telapsed=%lld\n", longs[i].name, rv, ename(err), (long long)elapsed);
    }
    close(kq);
}

// ---- D ------------------------------------------------------------------

static void section_d(void)
{
    int kq = kqueue();
    struct kevent out[1];
    for (int restart = 0; restart <= 1; restart++)
    for (int timed = 0; timed <= 1; timed++) {
        install(SIGALRM, restart);
        struct timespec ts = { 2, 0 };
        alarms = 0;
        arm(50);
        int64_t start = now_us();
        errno = 0;
        int rv = kevent(kq, NULL, 0, out, 1, timed ? &ts : NULL);
        int err = rv < 0 ? errno : 0;
        int64_t elapsed = now_us() - start;
        arm(0);
        printf("D\tSA_RESTART=%d timeout=%s\trv=%d\t%s\telapsed=%lld\talarms=%d\n", restart, timed ? "2s" : "NULL", rv,
               ename(err), (long long)elapsed, (int)alarms);
    }
    close(kq);
}

// ---- E ------------------------------------------------------------------

// The result fields are written by the sleeper before it sets `done`, and read
// by another thread only once it has seen `done` set or joined the sleeper.
struct sleeper {
    int fd;
    const struct timespec *ts;
    atomic_int started;
    atomic_int done;
    int rv, err;
    int64_t elapsed;
};

static void *sleep_in_kevent(void *p)
{
    struct sleeper *s = p;
    struct kevent out[1];
    atomic_store(&s->started, 1);
    int64_t start = now_us();
    errno = 0;
    s->rv = kevent(s->fd, NULL, 0, out, 1, s->ts);
    s->err = s->rv < 0 ? errno : 0;
    s->elapsed = now_us() - start;
    atomic_store(&s->done, 1);
    return NULL;
}

struct closer {
    int victim;
    int row;
    int rv, err, reused;
    int64_t duration;
};

static void *close_it(void *p)
{
    struct closer *c = p;
    int64_t start = now_us();
    errno = 0;
    c->rv = close(c->victim);
    c->err = c->rv < 0 ? errno : 0;
    c->duration = now_us() - start;
    c->reused = (c->row == 7) ? kqueue() : open("/dev/null", O_RDONLY);
    return NULL;
}

// The close is made from a thread of its own, in case it waits for the
// sleeper; any sleeper still asleep a second later is pulled out with SIGUSR1,
// whose handler does not restart.
static void section_e(void)
{
    install(SIGUSR1, 0);
    for (int row = 1; row <= 8; row++)
    for (int trial = 0; trial < 10; trial++) {
        int k = kqueue();
        int d = (row >= 2 && row <= 5) || row == 8 ? dup(k) : -1;
        struct timespec ts300 = { 0, 300000000 };
        struct sleeper s1 = { .fd = (row == 3) ? d : k, .ts = (row == 6) ? &ts300 : NULL };
        struct sleeper s2 = { .fd = d, .ts = NULL };
        pthread_t t1, t2, tc;
        pthread_create(&t1, NULL, sleep_in_kevent, &s1);
        if (row == 5) pthread_create(&t2, NULL, sleep_in_kevent, &s2);
        while (!atomic_load(&s1.started)) { }
        if (row == 5) while (!atomic_load(&s2.started)) { }
        sleep_ms(50);

        struct closer c = { .victim = (row == 2) ? d : k, .row = row, .reused = -1 };
        pthread_create(&tc, NULL, close_it, &c);

        int64_t deadline = now_us() + 1000000;
        while (now_us() < deadline && (!atomic_load(&s1.done) || (row == 5 && !atomic_load(&s2.done)))) sleep_ms(5);
        int rescued1 = !atomic_load(&s1.done), rescued2 = row == 5 && !atomic_load(&s2.done);
        if (rescued1) pthread_kill(t1, SIGUSR1);
        if (rescued2) pthread_kill(t2, SIGUSR1);
        pthread_join(t1, NULL);
        if (row == 5) pthread_join(t2, NULL);
        pthread_join(tc, NULL);

        printf("E%d\ttrial=%d\tsleeper1(fd %d) rv=%d %s %lldus%s", row, trial, s1.fd, s1.rv, ename(s1.err),
               (long long)s1.elapsed, rescued1 ? " (rescued)" : "");
        if (row == 5)
            printf("\tsleeper2(fd %d) rv=%d %s %lldus%s", s2.fd, s2.rv, ename(s2.err), (long long)s2.elapsed,
                   rescued2 ? " (rescued)" : "");
        printf("\tclose(%d) rv=%d %s %lldus\tnext-open=%d", c.victim, c.rv, ename(c.err), (long long)c.duration,
               c.reused);
        if (row == 8) {
            struct kevent out[1];
            struct timespec zero = { 0, 0 }, hundred = { 0, 100000000 };
            errno = 0;
            int64_t start = now_us();
            int rv0 = kevent(d, NULL, 0, out, 1, &zero);
            int err0 = rv0 < 0 ? errno : 0;
            int64_t el0 = now_us() - start;
            errno = 0;
            start = now_us();
            int rv1 = kevent(d, NULL, 0, out, 1, &hundred);
            int err1 = rv1 < 0 ? errno : 0;
            int64_t el1 = now_us() - start;
            printf("\tthen kevent(%d,{0,0}) rv=%d %s %lldus, kevent(%d,100ms) rv=%d %s %lldus", d, rv0, ename(err0),
                   (long long)el0, d, rv1, ename(err1), (long long)el1);
        }
        printf("\n");

        if (c.reused >= 0) close(c.reused);
        if (row == 2) close(k);
        else if (d >= 0) close(d);
    }
}

static void section_f(void)
{
    install(SIGUSR1, 0);
    install(SIGALRM, 0);
    int k = kqueue();
    int d = dup(k);
    struct sleeper s1 = { .fd = k, .ts = NULL };
    pthread_t t1;
    pthread_create(&t1, NULL, sleep_in_kevent, &s1);
    while (!atomic_load(&s1.started)) { }
    sleep_ms(50);
    close(k);
    pthread_join(t1, NULL);
    printf("F\tsleeper(fd %d) rv=%d %s\n", s1.fd, s1.rv, ename(s1.err));

    struct { const char *name; int n; const struct kevent *list; } changes[] = {
        { "0/NULL", 0, NULL }, { "1/unreadable", 1, (const struct kevent *)8 }, { "-1/NULL", -1, NULL },
    };
    int nevents[] = { 1, 0, -1 };
    struct timespec ts_zero = { 0, 0 }, ts_20ms = { 0, 20000000 }, ts_nsneg = { 0, -1 };
    struct { const char *name; const struct timespec *ts; } timeouts[] = {
        { "NULL", NULL }, { "{0,0}", &ts_zero }, { "{0,20ms}", &ts_20ms }, { "{0,-1}", &ts_nsneg },
        { "unreadable", (const struct timespec *)8 },
    };
    struct kevent real[4];
    for (size_t c = 0; c < 3; c++)
    for (size_t n = 0; n < 3; n++)
    for (size_t t = 0; t < 5; t++) {
        alarms = 0;
        arm(40);
        int64_t start = now_us();
        errno = 0;
        int rv = kevent(d, changes[c].list, changes[c].n, real, nevents[n], timeouts[t].ts);
        int err = rv < 0 ? errno : 0;
        int64_t elapsed = now_us() - start;
        arm(0);
        int slept = (err == EINTR && alarms > 0) || elapsed >= 15000;
        printf("F\t%s\t%d\t%s\trv=%d\t%s\tslept=%d\n", changes[c].name, nevents[n], timeouts[t].name, rv, ename(err),
               slept);
    }
    errno = 0;
    int crv = close(d);
    printf("F\tclose(%d) rv=%d %s\n", d, crv, ename(crv < 0 ? errno : 0));
}

static void *watchdog(void *p)
{
    (void)p;
    sleep(300);
    fprintf(stderr, "watchdog: 300 s elapsed\n");
    _exit(2);
}

int main(void)
{
    // The process-wide bound is a watchdog thread rather than alarm(), which
    // would share ITIMER_REAL with the interval timers below. It starts with
    // SIGALRM blocked, so that a process-directed SIGALRM reaches the thread
    // asleep in kevent.
    sigset_t alrm, old;
    sigemptyset(&alrm);
    sigaddset(&alrm, SIGALRM);
    sigaddset(&alrm, SIGUSR1);
    pthread_sigmask(SIG_BLOCK, &alrm, &old);
    pthread_t w;
    pthread_create(&w, NULL, watchdog, NULL);
    pthread_sigmask(SIG_SETMASK, &old, NULL);
    setvbuf(stdout, NULL, _IOLBF, 0);
    section_a();
    section_b();
    section_c();
    section_d();
    section_e();
    section_f();
    return 0;
}
