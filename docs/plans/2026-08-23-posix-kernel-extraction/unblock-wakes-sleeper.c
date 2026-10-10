// Whether a thread asleep in a syscall, with a signal pending that it blocks,
// is woken when *another* thread's mask call unblocks that signal for it.
//
// Darwin's sigprocmask changes the mask of every thread of the process
// (`sigprocmask-ops.c`, the "spread" rows), so a second thread can make a
// signal pending on a sleeper deliverable to it without sending anything. This
// asks what the sleeper does then: return at once (EINTR, or the handler then
// a restart), or sleep on until its own condition ends the call.
//
// Every row runs in a fresh forked child. The child blocks S in every thread
// before it makes any, then has three roles:
//   sleeper   - parks in the row's call. `sleeper=main` is the main thread, to
//               which Darwin gives a signal sent to the process while every
//               thread blocks it; `sleeper=second` is a thread made for it.
//   unblocker - a second thread. With `order=during` it generates S at
//               T_GEN; with `order=before` the sleeper generates it itself
//               just before it parks. At T_UNBLOCK it does the row's `route`:
//                 sigprocmask      - sigprocmask(SIG_UNBLOCK, {S})
//                 pthread_sigmask  - pthread_sigmask(SIG_UNBLOCK, {S}) (its own
//                                    mask alone, on both flavours: a control)
//                 none             - nothing (a control)
//                 sleeper-unblocked - the sleeper unblocked S itself before it
//                                    parked, and S is generated during the
//                                    sleep (a control that a wake is seen)
//   ender     - at T_END makes the call's own condition hold: writes a byte,
//               drains the buffer, connects, unlocks, or sends SIGUSR2 (which
//               has a handler and is blocked by nobody but the unblocker and
//               the ender) to the sleeper. `nanosleep` and `poll-none` time out
//               at T_END instead. `spin` is a sleeper that is not asleep: it
//               runs user code until T_END, calling getppid about every
//               100 us, so that it returns to user mode often.
// `target=process` sends S with kill, `target=thread` with pthread_kill to the
// sleeper.
//
// S is SIGUSR1 with a handler (`disp=caught`, `flags` 0 or SA_RESTART),
// SIGTERM at its default (`disp=term`), SIGCONT at its default (`disp=cont`)
// or SIGWINCH at its default, which discards it (`disp=winch`).
//
// The child reports, for the sleeper's call: its return value and errno
// (`ret`, `errno`), when it returned (`returned`, ms from the row's start),
// the sleeper's mask after it returned (`after-mask`, the word whose bit n-1 is
// signo n), when each S handler ran and on which role (`handled`), and
// whether the USR2 handler ran on the sleeper (`usr2`). The parent adds how
// the child ended and when (`end`, `died`).
//
// Times: T_GEN 100 ms, T_UNBLOCK 200 ms, T_END 700 ms. A call woken by the
// unblock returns near 200; one that sleeps on returns near 700.
//
// Bounded: alarm(900) in the parent, GUARD_CHILD (alarm) in each child, and
// every loop over a fixed list.
//
// Darwin: clang -Wall -Wno-unused-function -Wno-deprecated-declarations -pthread -o /tmp/unblock-wakes-sleeper unblock-wakes-sleeper.c && /tmp/unblock-wakes-sleeper
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/unblock-wakes-sleeper.c && /tmp/p'
//
// Run twice each on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64,
// Apple `container`, gcc:14 image, glibc 2.41), 2026-10-10, with the same
// answer in every row both times; the second run's output is beside this file
// as unblock-wakes-sleeper.darwin-27.0.txt and
// unblock-wakes-sleeper.linux-6.18.5-aarch64.txt. Transcribed as
// WoofWare.PosixKernel.Test/TestUnblockForAnotherTask.fs.
//
//   Darwin: no sleeper was woken by the unblock, in any call, for any
//     disposition: every one returned near T_END. Then read and write of a
//     pipe or a TCP socket, accept, flock, sigsuspend and pause answered EINTR
//     (under SA_RESTART, all but sigsuspend and pause restarted and completed
//     instead) and the handler ran, or a TERM killed the process, as the call
//     returned. poll, kevent and
//     nanosleep answered as if no signal were pending, and the signal was
//     delivered as they returned only with `order=during` (another thread had
//     sent it); with `order=before` (the sleeper had sent it itself) it was
//     never delivered, neither then nor at the nanosleep after, and a TERM
//     never killed the process. A `spin` thread never took it either way,
//     TERM included. CONT and WINCH did nothing observable. The controls
//     behaved: with `pthread_sigmask` or no unblock nothing ran, and with
//     `sleeper-unblocked` every call returned EINTR near T_GEN, or, `spin`,
//     ran the handler then.
//   Linux: the unblocker's mask call changed its own mask alone, so a signal
//     sent to the process was taken by the unblocker near T_UNBLOCK (a TERM
//     killed the process then), and one sent to the sleeper stayed pending and
//     blocked; every sleeper returned near T_END with its own answer.
#include "signal-probe-common.h"
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

#define T_GEN 100
#define T_UNBLOCK 200
#define T_END 700

enum park {
    P_POLL_PIPE, P_POLL_NONE, P_PORT, P_READ_PIPE, P_WRITE_PIPE, P_ACCEPT,
    P_READ_TCP, P_WRITE_TCP, P_FLOCK, P_SIGSUSPEND, P_PAUSE, P_NANOSLEEP, P_SPIN, P_COUNT
};
static const char *park_name[P_COUNT] = {
    "poll-pipe", "poll-none",
#ifdef __linux__
    "epoll_wait",
#else
    "kevent",
#endif
    "read-pipe", "write-pipe", "accept", "read-tcp", "write-tcp", "flock",
    "sigsuspend", "pause", "nanosleep", "spin"
};

enum route { R_SIGPROCMASK, R_PTHREAD, R_NONE, R_SLEEPER_UNBLOCKED, R_COUNT };
static const char *route_name[R_COUNT] = { "sigprocmask", "pthread_sigmask", "none", "sleeper-unblocked" };

enum disp { D_CAUGHT, D_TERM, D_CONT, D_WINCH, D_COUNT };
static const char *disp_name[D_COUNT] = { "caught", "term", "cont", "winch" };
static const int disp_signal[D_COUNT] = { SIGUSR1, SIGTERM, SIGCONT, SIGWINCH };

struct row {
    enum park park;
    int sleeper_main;
    int target_process;
    int order_before;
    enum route route;
    enum disp disp;
    int restart;
};

// Child state. One row per child, so globals are fine.
static struct row R;
static int S;
static long t0;
static pthread_t sleeper_thread, unblocker_thread, ender_thread;
static int pipe_fds[2] = { -1, -1 };
static int listen_fd = -1;
static struct sockaddr_in listen_addr;
static int tcp_server = -1, tcp_client = -1;
static int lock_holder = -1, lock_waiter = -1;
static int port_fd = -1;
static int result_fd = -1;

#define MAX_HANDLED 8
static _Atomic int handled_count;
static _Atomic long handled_at[MAX_HANDLED];
static _Atomic int handled_role[MAX_HANDLED];
static _Atomic int usr2_on_sleeper;

static int role_of_self(void)
{
    pthread_t me = pthread_self();
    if (pthread_equal(me, sleeper_thread)) return 0;
    if (pthread_equal(me, unblocker_thread)) return 1;
    if (pthread_equal(me, ender_thread)) return 2;
    return 3;
}
static const char *role_name[4] = { "sleeper", "unblocker", "ender", "main" };

static void on_s(int sig)
{
    (void)sig;
    int i = atomic_fetch_add(&handled_count, 1);
    if (i < MAX_HANDLED) {
        atomic_store(&handled_at[i], now_ms() - t0);
        atomic_store(&handled_role[i], role_of_self());
    }
}

static void on_usr2(int sig)
{
    (void)sig;
    if (role_of_self() == 0) atomic_store(&usr2_on_sleeper, 1);
}

static uint64_t mask_word(void)
{
    sigset_t s;
    sigemptyset(&s);
    pthread_sigmask(SIG_BLOCK, NULL, &s);
    uint64_t w = 0;
    for (int sig = 1; sig <= 31; sig++)
        if (sigismember(&s, sig)) w |= (uint64_t)1 << (sig - 1);
    return w;
}

static void set_nonblock(int fd, int on)
{
    int fl = fcntl(fd, F_GETFL);
    if (on) fl |= O_NONBLOCK; else fl &= ~O_NONBLOCK;
    if (fcntl(fd, F_SETFL, fl) != 0) die("fcntl");
}

static void fill(int fd)
{
    char buf[4096];
    memset(buf, 'x', sizeof buf);
    set_nonblock(fd, 1);
    for (int i = 0; i < 100000; i++) {
        ssize_t n = write(fd, buf, sizeof buf);
        if (n < 0) {
            if (errno == EAGAIN) break;
            die("fill write");
        }
    }
    // Top up byte by byte, so nothing at all fits.
    for (int i = 0; i < 100000; i++) {
        ssize_t n = write(fd, buf, 1);
        if (n < 0) {
            if (errno == EAGAIN) break;
            die("fill write 1");
        }
    }
    set_nonblock(fd, 0);
}

static void drain(int fd)
{
    char buf[65536];
    set_nonblock(fd, 1);
    for (int i = 0; i < 1000; i++) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n <= 0) break;
    }
}

static void make_listener(void)
{
    listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_fd < 0) die("socket");
    memset(&listen_addr, 0, sizeof listen_addr);
    listen_addr.sin_family = AF_INET;
    listen_addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(listen_fd, (struct sockaddr *)&listen_addr, sizeof listen_addr) != 0) die("bind");
    socklen_t len = sizeof listen_addr;
    if (getsockname(listen_fd, (struct sockaddr *)&listen_addr, &len) != 0) die("getsockname");
    if (listen(listen_fd, 8) != 0) die("listen");
}

static int connect_listener(void)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (c < 0) die("socket");
    if (connect(c, (struct sockaddr *)&listen_addr, sizeof listen_addr) != 0) die("connect");
    return c;
}

static void setup(void)
{
    switch (R.park) {
    case P_POLL_PIPE: case P_READ_PIPE: case P_PORT:
        if (pipe(pipe_fds) != 0) die("pipe");
        if (R.park == P_PORT) {
#ifdef __linux__
            port_fd = epoll_create1(0);
            if (port_fd < 0) die("epoll_create1");
            struct epoll_event ev = { .events = EPOLLIN, .data.fd = pipe_fds[0] };
            if (epoll_ctl(port_fd, EPOLL_CTL_ADD, pipe_fds[0], &ev) != 0) die("epoll_ctl");
#else
            port_fd = kqueue();
            if (port_fd < 0) die("kqueue");
            struct kevent ch;
            EV_SET(&ch, pipe_fds[0], EVFILT_READ, EV_ADD, 0, 0, NULL);
            if (kevent(port_fd, &ch, 1, NULL, 0, NULL) != 0) die("kevent add");
#endif
        }
        break;
    case P_WRITE_PIPE:
        if (pipe(pipe_fds) != 0) die("pipe");
        fill(pipe_fds[1]);
        break;
    case P_ACCEPT:
        make_listener();
        break;
    case P_READ_TCP: case P_WRITE_TCP:
        make_listener();
        tcp_client = connect_listener();
        tcp_server = accept(listen_fd, NULL, NULL);
        if (tcp_server < 0) die("accept");
        // Loopback TCP moves bytes on to the peer's receive buffer, and
        // acknowledges them, after the writes return: fill until a pause
        // frees nothing, or a blocking write finds room at once.
        if (R.park == P_WRITE_TCP)
            for (int i = 0; i < 4; i++) { fill(tcp_server); sleep_ms(150); }
        break;
    case P_FLOCK: {
        char path[] = "/tmp/unblock-wakes-sleeper.XXXXXX";
        int fd = mkstemp(path);
        if (fd < 0) die("mkstemp");
        close(fd);
        lock_holder = open(path, O_RDWR);
        lock_waiter = open(path, O_RDWR);
        unlink(path);
        if (lock_holder < 0 || lock_waiter < 0) die("open lock");
        if (flock(lock_holder, LOCK_EX) != 0) die("flock holder");
        break;
    }
    case P_POLL_NONE: case P_SIGSUSPEND: case P_PAUSE: case P_NANOSLEEP: case P_SPIN: case P_COUNT:
        break;
    }
}

static void generate(void)
{
    if (R.target_process) {
        if (kill(getpid(), S) != 0) die("kill");
    } else {
        if (pthread_kill(sleeper_thread, S) != 0) die("pthread_kill");
    }
}

static void at(long ms)
{
    long now = now_ms() - t0;
    if (now < ms) sleep_ms((int)(ms - now));
}

static void *unblocker(void *arg)
{
    (void)arg;
    if (!R.order_before) { at(T_GEN); generate(); }
    at(T_UNBLOCK);
    sigset_t s;
    sigemptyset(&s);
    sigaddset(&s, S);
    switch (R.route) {
    case R_SIGPROCMASK: if (sigprocmask(SIG_UNBLOCK, &s, NULL) != 0) die("sigprocmask"); break;
    case R_PTHREAD: if (pthread_sigmask(SIG_UNBLOCK, &s, NULL) != 0) die("pthread_sigmask"); break;
    case R_NONE: case R_SLEEPER_UNBLOCKED: case R_COUNT: break;
    }
    // Stay alive past the end, so that a signal it could take finds it.
    at(T_END + 150);
    return NULL;
}

static void *ender(void *arg)
{
    (void)arg;
    at(T_END);
    char c = 'y';
    switch (R.park) {
    case P_POLL_PIPE: case P_READ_PIPE: case P_PORT:
        if (write(pipe_fds[1], &c, 1) != 1) die("ender write");
        break;
    case P_WRITE_PIPE: drain(pipe_fds[0]); break;
    case P_ACCEPT: (void)connect_listener(); break;
    case P_READ_TCP: if (write(tcp_client, &c, 1) != 1) die("ender write tcp"); break;
    case P_WRITE_TCP:
        for (int i = 0; i < 20; i++) { drain(tcp_client); sleep_ms(5); }
        break;
    case P_FLOCK: if (flock(lock_holder, LOCK_UN) != 0) die("ender unlock"); break;
    case P_SIGSUSPEND: case P_PAUSE:
        // In the "sleeper-unblocked" control a second sleeper has returned
        // and exited by now, which pthread_kill answers ESRCH.
        (void)pthread_kill(sleeper_thread, SIGUSR2);
        break;
    case P_POLL_NONE: case P_NANOSLEEP: case P_SPIN: case P_COUNT: break;
    }
    at(T_END + 150);
    return NULL;
}

static void out(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
static void out(const char *fmt, ...)
{
    char line[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(line, sizeof line, fmt, ap);
    va_end(ap);
    write(result_fd, line, strlen(line));
}

static void park_now(void)
{
    if (R.route == R_SLEEPER_UNBLOCKED) {
        sigset_t s;
        sigemptyset(&s);
        sigaddset(&s, S);
        pthread_sigmask(SIG_UNBLOCK, &s, NULL);
    }
    if (R.order_before) generate();

    long ret = 0;
    char c;
    switch (R.park) {
    case P_POLL_PIPE: {
        struct pollfd p = { .fd = pipe_fds[0], .events = POLLIN };
        ret = poll(&p, 1, -1);
        break;
    }
    case P_POLL_NONE: ret = poll(NULL, 0, T_END); break;
    case P_PORT: {
#ifdef __linux__
        struct epoll_event ev;
        ret = epoll_wait(port_fd, &ev, 1, -1);
#else
        struct kevent ev;
        ret = kevent(port_fd, NULL, 0, &ev, 1, NULL);
#endif
        break;
    }
    case P_READ_PIPE: ret = read(pipe_fds[0], &c, 1); break;
    case P_WRITE_PIPE: c = 'z'; ret = write(pipe_fds[1], &c, 1); break;
    case P_ACCEPT: ret = accept(listen_fd, NULL, NULL); break;
    case P_READ_TCP: ret = read(tcp_server, &c, 1); break;
    case P_WRITE_TCP: c = 'z'; ret = write(tcp_server, &c, 1); break;
    case P_FLOCK: ret = flock(lock_waiter, LOCK_EX); break;
    case P_SIGSUSPEND: {
        sigset_t cur;
        pthread_sigmask(SIG_BLOCK, NULL, &cur);
        ret = sigsuspend(&cur);
        break;
    }
    case P_PAUSE: ret = pause(); break;
    case P_NANOSLEEP: {
        struct timespec ts = { 0, (long)T_END * 1000000L };
        ret = nanosleep(&ts, NULL);
        break;
    }
    case P_SPIN:
        // Running rather than asleep: user code, with a cheap system call
        // (and so a return to user mode) about every 100 us, until T_END.
        while (now_ms() - t0 < T_END) {
            for (volatile int i = 0; i < 10000; i++) {}
            (void)getppid();
        }
        break;
    case P_COUNT: break;
    }
    int err = errno;
    long returned = now_ms() - t0;
    uint64_t after = mask_word();
    // Let anything delivered late land before reporting.
    sleep_ms(80);

    out("ret=%ld errno=%d returned=%ld after-mask=%llx handled=", ret, ret < 0 ? err : 0, returned,
        (unsigned long long)after);
    int n = atomic_load(&handled_count);
    if (n == 0) out("none");
    for (int i = 0; i < n && i < MAX_HANDLED; i++)
        out("%s%ld@%s", i ? "," : "", atomic_load(&handled_at[i]), role_name[atomic_load(&handled_role[i])]);
    out(" usr2=%d", atomic_load(&usr2_on_sleeper));
}

static void *sleeper_main(void *arg)
{
    (void)arg;
    park_now();
    return NULL;
}

static void run_child(void)
{
    GUARD_CHILD();
    S = disp_signal[R.disp];
    if (R.disp == D_CAUGHT) {
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_s;
        sa.sa_flags = R.restart ? SA_RESTART : 0;
        sigemptyset(&sa.sa_mask);
        if (sigaction(S, &sa, NULL) != 0) die("sigaction S");
    }
    {
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_usr2;
        sigemptyset(&sa.sa_mask);
        if (sigaction(SIGUSR2, &sa, NULL) != 0) die("sigaction USR2");
    }
    setup();

    // Every thread made from here on blocks S.
    sigset_t s;
    sigemptyset(&s);
    sigaddset(&s, S);
    if (pthread_sigmask(SIG_BLOCK, &s, NULL) != 0) die("block S");

    t0 = now_ms();
    if (R.sleeper_main) sleeper_thread = pthread_self();

    // The unblocker and the ender block USR2, so that only the sleeper takes it.
    sigset_t with_usr2 = s;
    sigaddset(&with_usr2, SIGUSR2);
    sigset_t old;
    pthread_sigmask(SIG_BLOCK, &with_usr2, &old);
    if (pthread_create(&unblocker_thread, NULL, unblocker, NULL) != 0) die("create unblocker");
    if (pthread_create(&ender_thread, NULL, ender, NULL) != 0) die("create ender");
    pthread_sigmask(SIG_SETMASK, &old, NULL);

    if (R.sleeper_main) {
        park_now();
    } else {
        if (pthread_create(&sleeper_thread, NULL, sleeper_main, NULL) != 0) die("create sleeper");
        pthread_join(sleeper_thread, NULL);
    }
    pthread_join(unblocker_thread, NULL);
    pthread_join(ender_thread, NULL);
    _exit(0);
}

static void run_row(struct row r)
{
    printf("row park=%s sleeper=%s target=%s order=%s route=%s disp=%s flags=%s ", park_name[r.park],
           r.sleeper_main ? "main" : "second", r.target_process ? "process" : "thread",
           r.order_before ? "before" : "during", route_name[r.route], disp_name[r.disp],
           r.restart ? "RESTART" : "0");
    fflush(stdout);
    int p[2];
    if (pipe(p) != 0) die("pipe");
    long start = now_ms();
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        close(p[0]);
        result_fd = p[1];
        R = r;
        run_child();
    }
    close(p[1]);
    char buf[2048];
    size_t used = 0;
    for (int i = 0; i < 1000; i++) {
        ssize_t n = read(p[0], buf + used, sizeof buf - 1 - used);
        if (n <= 0) break;
        used += (size_t)n;
    }
    buf[used] = 0;
    close(p[0]);
    int status;
    if (waitpid(pid, &status, 0) != pid) die("waitpid");
    long died = now_ms() - start;
    printf("%s", used ? buf : "no-report");
    if (WIFEXITED(status)) printf(" end=exit%d", WEXITSTATUS(status));
    else if (WIFSIGNALED(status)) printf(" end=signal%d died=%ld", WTERMSIG(status), died);
    else printf(" end=?%x", status);
    printf("\n");
    fflush(stdout);
}

int main(void)
{
    // About 400 rows of about a second each: longer than GUARD_MAIN allows.
    alarm(900);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);
    printf("# flavour %s T_GEN %d T_UNBLOCK %d T_END %d\n", FLAVOUR, T_GEN, T_UNBLOCK, T_END);
    fflush(stdout);

    // Caught: every call, both targets (thread only for a second sleeper), both
    // orders, every route; SA_RESTART for the sigprocmask route.
    for (int park = 0; park < P_COUNT; park++)
        for (int sm = 1; sm >= 0; sm--)
            for (int tp = 1; tp >= 0; tp--) {
                if (!sm && tp) continue;
                for (int ob = 1; ob >= 0; ob--)
                    for (int route = 0; route < R_COUNT; route++) {
                        if (route == R_SLEEPER_UNBLOCKED && ob) continue;
                        for (int restart = 0; restart <= (route == R_SIGPROCMASK); restart++) {
                            struct row r = { park, sm, tp, ob, route, D_CAUGHT, restart };
                            run_row(r);
                        }
                    }
            }

    // At their defaults: the main thread asleep in poll, read or sigsuspend,
    // or running; the sigprocmask route and none.
    enum park few[4] = { P_POLL_PIPE, P_READ_PIPE, P_SIGSUSPEND, P_SPIN };
    for (int d = D_TERM; d < D_COUNT; d++)
        for (int i = 0; i < 4; i++)
            for (int tp = 1; tp >= 0; tp--)
                for (int ob = 1; ob >= 0; ob--)
                    for (int route = R_SIGPROCMASK; route <= R_NONE; route += 2) {
                        struct row r = { few[i], 1, tp, ob, route, d, 0 };
                        run_row(r);
                    }
    return 0;
}
