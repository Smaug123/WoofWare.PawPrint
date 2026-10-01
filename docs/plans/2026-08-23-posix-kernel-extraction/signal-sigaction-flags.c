/* From the signals research of 2026-09-23/24, run on Linux 6.18.5 aarch64 and
 * Darwin 25.6.0; its rows are summarised in the flavour-divergence table and
 * cited by SignalState's handler-frame rules. Build with
 * signal-probe-common.h beside it. */
/* What sigaction's sa_mask and sa_flags do, as a model would have to state.
 *
 * mask:     SIGUSR1's handler, installed with every combination of
 *           flags in {0, SA_NODEFER, SA_RESETHAND, SA_NODEFER|SA_RESETHAND}
 *           and sa_mask in {{}, {USR2}}, with the thread's mask {HUP} at
 *           delivery. Inside, record which of USR1/USR2/HUP/TERM are
 *           blocked, then unblock HUP and block TERM. After return: the mask,
 *           and the disposition (handler or SIG_DFL). 8 rows.
 * resethand: every standard catchable signal with SA_RESETHAND: is the
 *           disposition SIG_DFL after one delivery, and was the signal
 *           blocked inside the handler? 29 rows.
 * reraise:  the handler for USR1 raises USR1 once more; flags 0 vs
 *           SA_NODEFER; is the second delivery nested (depth 2 observed)?
 * restart:  a blocking call in the main thread, SIGUSR1 sent to it by a
 *           helper thread at 50 ms, the call's completion event at 200 ms
 *           (or its own 300 ms timeout), with and without SA_RESTART. The
 *           row reports the call's result, errno, and when it returned. 17
 *           calls x 2 = 34 rows.
 * sicode:   si_code and si_pid seen by a SA_SIGINFO handler for kill(),
 *           raise(), pthread_kill() and (Linux) sigqueue().
 */
#include "signal-probe-common.h"
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <sys/file.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/un.h>
#ifdef __APPLE__
#include <sys/event.h>
#else
#include <sys/epoll.h>
#endif

static int in_mask(int s) { sigset_t m; pthread_sigmask(SIG_BLOCK, NULL, &m); return sigismember(&m, s); }

/* --- mask --- */
static volatile sig_atomic_t seen_usr1, seen_usr2, seen_hup, seen_term;
static void mask_handler(int s)
{
    (void)s;
    seen_usr1 = in_mask(SIGUSR1); seen_usr2 = in_mask(SIGUSR2); seen_hup = in_mask(SIGHUP); seen_term = in_mask(SIGTERM);
    sigset_t u; sigemptyset(&u); sigaddset(&u, SIGHUP); pthread_sigmask(SIG_UNBLOCK, &u, NULL);
    sigset_t b; sigemptyset(&b); sigaddset(&b, SIGTERM); pthread_sigmask(SIG_BLOCK, &b, NULL);
}

static void mask_trial(int flags, int with_usr2)
{
    pid_t pid = fork();
    if (pid == 0) {
        GUARD_CHILD();
        struct sigaction sa; memset(&sa, 0, sizeof sa);
        sa.sa_handler = mask_handler; sa.sa_flags = flags;
        sigemptyset(&sa.sa_mask); if (with_usr2) sigaddset(&sa.sa_mask, SIGUSR2);
        sigaction(SIGUSR1, &sa, NULL);
        sigset_t m; sigemptyset(&m); sigaddset(&m, SIGHUP); sigprocmask(SIG_SETMASK, &m, NULL);
        kill(getpid(), SIGUSR1);
        struct sigaction now; sigaction(SIGUSR1, NULL, &now);
        printf("mask flags=%-19s sa_mask=%-6s | inside: USR1=%d USR2=%d HUP=%d TERM=%d | after return: HUP=%d TERM=%d disposition=%s\n",
               flags == 0 ? "0" : flags == SA_NODEFER ? "NODEFER" : flags == SA_RESETHAND ? "RESETHAND" : "NODEFER|RESETHAND",
               with_usr2 ? "{USR2}" : "{}", (int)seen_usr1, (int)seen_usr2, (int)seen_hup, (int)seen_term,
               in_mask(SIGHUP), in_mask(SIGTERM), now.sa_handler == SIG_DFL ? "SIG_DFL" : now.sa_handler == mask_handler ? "handler" : "other");
        fflush(stdout); _exit(0);
    }
    waitpid(pid, NULL, 0);
}

/* --- resethand sweep --- */
static volatile sig_atomic_t rh_blocked_inside;
static void rh_handler(int s) { rh_blocked_inside = in_mask(s); }
static void resethand_trial(int s)
{
    pid_t pid = fork();
    if (pid == 0) {
        GUARD_CHILD();
        struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = rh_handler; sa.sa_flags = SA_RESETHAND;
        sigaction(s, &sa, NULL);
        kill(getpid(), s);
        struct sigaction now; sigaction(s, NULL, &now);
        printf("resethand %2d %-7s blocked_inside=%d disposition_after=%s\n", s, signame(s), (int)rh_blocked_inside,
               now.sa_handler == SIG_DFL ? "SIG_DFL" : now.sa_handler == rh_handler ? "handler" : "other");
        fflush(stdout); _exit(0);
    }
    int st; waitpid(pid, &st, 0);
    if (!WIFEXITED(st)) printf("resethand %2d child died 0x%x\n", s, st);
}

/* --- reraise --- */
static volatile sig_atomic_t depth, maxdepth, calls;
static void rr_handler(int s)
{
    depth++; calls++; if (depth > maxdepth) maxdepth = depth;
    if (calls == 1) { raise(s); /* nested now, or pending until return */ }
    depth--;
}
static void reraise_trial(int flags)
{
    pid_t pid = fork();
    if (pid == 0) {
        GUARD_CHILD();
        struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = rr_handler; sa.sa_flags = flags;
        sigaction(SIGUSR1, &sa, NULL);
        kill(getpid(), SIGUSR1);
        printf("reraise flags=%s calls=%d maxdepth=%d\n", flags ? "NODEFER" : "0", (int)calls, (int)maxdepth);
        fflush(stdout); _exit(0);
    }
    waitpid(pid, NULL, 0);
}

/* --- restart --- */
static volatile sig_atomic_t handled;
static void count_handler(int s) { (void)s; handled++; }

enum op { OP_READ_PIPE, OP_WRITE_PIPE, OP_ACCEPT, OP_RECV_UNIX, OP_RECVFROM_UDP, OP_RECV_TIMEO, OP_NANOSLEEP,
          OP_POLL_INF, OP_POLL_TIMEOUT, OP_SELECT, OP_EPOLL_OR_KEVENT, OP_FLOCK, OP_FCNTL_SETLKW, OP_WAITPID, OP_PAUSE,
          OP_SIGSUSPEND, OP_CONNECT_UNIX, OP_COUNT };
static const char *opname[] = { "read(empty pipe)", "write(full pipe)", "accept(tcp)", "recv(unix stream)", "recvfrom(udp)",
                                "recv(SO_RCVTIMEO=1s)", "nanosleep(300ms)", "poll(pipe,-1)", "poll(pipe,300ms)",
                                "select(pipe,NULL)",
#ifdef __APPLE__
                                "kevent(pipe,NULL)",
#else
                                "epoll_wait(pipe,-1)",
#endif
                                "flock(LOCK_EX)", "fcntl(F_SETLKW)", "waitpid(child)", "pause()", "sigsuspend({})",
                                "connect(unix, full backlog)" };

struct ctx { enum op op; pthread_t victim; int fds[4]; pid_t child; int child_pipe; };

static void *helper(void *arg)
{
    struct ctx *c = arg;
    sleep_ms(50);
    pthread_kill(c->victim, SIGUSR1);
    sleep_ms(150);
    char byte = 'x'; char big[65536];
    switch (c->op) {
    case OP_READ_PIPE: case OP_POLL_INF: case OP_SELECT: case OP_EPOLL_OR_KEVENT: case OP_POLL_TIMEOUT:
        write(c->fds[1], &byte, 1); break;
    case OP_WRITE_PIPE: { int fl = fcntl(c->fds[0], F_GETFL); fcntl(c->fds[0], F_SETFL, fl | O_NONBLOCK); while (read(c->fds[0], big, sizeof big) > 0) {} break; }
    case OP_ACCEPT: { int s = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in a; socklen_t l = sizeof a; getsockname(c->fds[0], (struct sockaddr *)&a, &l); connect(s, (struct sockaddr *)&a, l); break; }
    case OP_RECV_UNIX: case OP_RECV_TIMEO: send(c->fds[1], &byte, 1, 0); break;
    case OP_RECVFROM_UDP: { int s = socket(AF_INET, SOCK_DGRAM, 0); struct sockaddr_in a; socklen_t l = sizeof a; getsockname(c->fds[0], (struct sockaddr *)&a, &l); sendto(s, &byte, 1, 0, (struct sockaddr *)&a, l); break; }
    case OP_FLOCK: flock(c->fds[1], LOCK_UN); break;
    case OP_FCNTL_SETLKW: case OP_WAITPID: write(c->child_pipe, &byte, 1); break;
    case OP_CONNECT_UNIX: { int a = accept(c->fds[0], NULL, NULL); (void)a; break; }
    case OP_PAUSE: case OP_SIGSUSPEND: pthread_kill(c->victim, SIGUSR1); break; /* nothing else can end them */
    default: break;
    }
    return NULL;
}

static void restart_trial(enum op op, int restart)
{
    int p[2]; pipe(p);
    pid_t pid = fork();
    if (pid == 0) {
        GUARD_CHILD();
        close(p[0]); dup2(p[1], 1);
        struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = count_handler; sa.sa_flags = restart ? SA_RESTART : 0;
        sigaction(SIGUSR1, &sa, NULL);
        signal(SIGPIPE, SIG_IGN);
        struct ctx c; memset(&c, 0, sizeof c); c.op = op; c.victim = pthread_self();
        char buf[16]; char tmpl[] = "/tmp/sigflock.XXXXXX";
        switch (op) {
        case OP_READ_PIPE: case OP_POLL_INF: case OP_POLL_TIMEOUT: case OP_SELECT: case OP_EPOLL_OR_KEVENT: pipe(c.fds); break;
        case OP_WRITE_PIPE: { pipe(c.fds); int fl = fcntl(c.fds[1], F_GETFL); fcntl(c.fds[1], F_SETFL, fl | O_NONBLOCK);
            char z[4096] = {0}; while (write(c.fds[1], z, sizeof z) > 0) {} while (write(c.fds[1], z, 1) > 0) {}
            fcntl(c.fds[1], F_SETFL, fl); break; }
        case OP_ACCEPT: case OP_RECVFROM_UDP: { c.fds[0] = socket(AF_INET, op == OP_ACCEPT ? SOCK_STREAM : SOCK_DGRAM, 0);
            struct sockaddr_in a; memset(&a, 0, sizeof a); a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
            bind(c.fds[0], (struct sockaddr *)&a, sizeof a); if (op == OP_ACCEPT) listen(c.fds[0], 4); break; }
        case OP_RECV_UNIX: case OP_RECV_TIMEO: socketpair(AF_UNIX, SOCK_STREAM, 0, c.fds);
            if (op == OP_RECV_TIMEO) { struct timeval tv = { 1, 0 }; setsockopt(c.fds[0], SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv); } break;
        case OP_FLOCK: { int fd = mkstemp(tmpl); unlink(tmpl); (void)fd;
            /* Two open file descriptions of one file: reopen via /dev/fd is
             * not portable, so use a named file for the second open. */
            char path[] = "/tmp/sigflock2.XXXXXX"; int a = mkstemp(path); int b = open(path, O_RDWR); unlink(path);
            c.fds[0] = b; c.fds[1] = a; flock(a, LOCK_EX); break; }
        case OP_FCNTL_SETLKW: { char path[] = "/tmp/sigfcntl.XXXXXX"; int fd = mkstemp(path); c.fds[0] = fd;
            int go[2], ready[2]; pipe(go); pipe(ready);
            pid_t k = fork();
            if (k == 0) { GUARD_CHILD(); int f = open(path, O_RDWR); struct flock fl = { 0 }; fl.l_type = F_WRLCK; fl.l_whence = SEEK_SET;
                fcntl(f, F_SETLK, &fl); write(ready[1], "r", 1); char x; read(go[0], &x, 1); _exit(0); }
            char x; read(ready[0], &x, 1); unlink(path); c.child = k; c.child_pipe = go[1]; break; }
        case OP_WAITPID: { int go[2]; pipe(go); pid_t k = fork();
            if (k == 0) { GUARD_CHILD(); char x; read(go[0], &x, 1); _exit(5); }
            c.child = k; c.child_pipe = go[1]; break; }
        case OP_CONNECT_UNIX: { struct sockaddr_un a; memset(&a, 0, sizeof a); a.sun_family = AF_UNIX;
            snprintf(a.sun_path, sizeof a.sun_path, "/tmp/sigconn.%d", (int)getpid()); unlink(a.sun_path);
            c.fds[0] = socket(AF_UNIX, SOCK_STREAM, 0); bind(c.fds[0], (struct sockaddr *)&a, sizeof a); listen(c.fds[0], 0);
            /* fill the backlog with nonblocking connects */
            for (int k = 0; k < 8; k++) { int s = socket(AF_UNIX, SOCK_STREAM, 0); fcntl(s, F_SETFL, O_NONBLOCK); if (connect(s, (struct sockaddr *)&a, sizeof a) != 0) { close(s); break; } }
            c.fds[1] = socket(AF_UNIX, SOCK_STREAM, 0); c.fds[2] = 0; break; }
        default: break;
        }
        pthread_t h; pthread_create(&h, NULL, helper, &c);
        long t0 = now_ms(); long r = -2; errno = 0;
        switch (op) {
        case OP_READ_PIPE: r = read(c.fds[0], buf, 1); break;
        case OP_WRITE_PIPE: r = write(c.fds[1], buf, 1); break;
        case OP_ACCEPT: r = accept(c.fds[0], NULL, NULL); break;
        case OP_RECV_UNIX: case OP_RECV_TIMEO: r = recv(c.fds[0], buf, 1, 0); break;
        case OP_RECVFROM_UDP: r = recvfrom(c.fds[0], buf, 1, 0, NULL, NULL); break;
        case OP_NANOSLEEP: { struct timespec ts = { 0, 300000000L }; r = nanosleep(&ts, NULL); break; }
        case OP_POLL_INF: { struct pollfd pf = { c.fds[0], POLLIN, 0 }; r = poll(&pf, 1, -1); break; }
        case OP_POLL_TIMEOUT: { struct pollfd pf = { c.fds[0], POLLIN, 0 }; r = poll(&pf, 1, 300); break; }
        case OP_SELECT: { fd_set rs; FD_ZERO(&rs); FD_SET(c.fds[0], &rs); r = select(c.fds[0] + 1, &rs, NULL, NULL, NULL); break; }
        case OP_EPOLL_OR_KEVENT: {
#ifdef __APPLE__
            int kq = kqueue(); struct kevent ev; EV_SET(&ev, c.fds[0], EVFILT_READ, EV_ADD, 0, 0, NULL); kevent(kq, &ev, 1, NULL, 0, NULL);
            struct kevent out; r = kevent(kq, NULL, 0, &out, 1, NULL);
#else
            int ep = epoll_create1(0); struct epoll_event ev = { .events = EPOLLIN }; epoll_ctl(ep, EPOLL_CTL_ADD, c.fds[0], &ev);
            struct epoll_event out; r = epoll_wait(ep, &out, 1, -1);
#endif
            break; }
        case OP_FLOCK: r = flock(c.fds[0], LOCK_EX); break;
        case OP_FCNTL_SETLKW: { struct flock fl = { 0 }; fl.l_type = F_WRLCK; fl.l_whence = SEEK_SET; r = fcntl(c.fds[0], F_SETLKW, &fl); break; }
        case OP_WAITPID: { int st; r = waitpid(c.child, &st, 0); break; }
        case OP_PAUSE: r = pause(); break;
        case OP_SIGSUSPEND: { sigset_t e; sigemptyset(&e); r = sigsuspend(&e); break; }
        case OP_CONNECT_UNIX: { struct sockaddr_un a; memset(&a, 0, sizeof a); a.sun_family = AF_UNIX;
            snprintf(a.sun_path, sizeof a.sun_path, "/tmp/sigconn.%d", (int)getpid()); r = connect(c.fds[1], (struct sockaddr *)&a, sizeof a); unlink(a.sun_path); break; }
        default: break;
        }
        int e = errno; long t = now_ms() - t0;
        pthread_join(h, NULL);
        if (c.child) { char x = 'x'; write(c.child_pipe, &x, 1); waitpid(c.child, NULL, 0); }
        const char *verdict = r < 0 && e == EINTR ? "EINTR" : (t >= 190 ? "completed after the signal (restarted or never interrupted)" : "completed early?");
        printf("restart %-28s SA_RESTART=%d: ret=%ld errno=%-10s at %4ld ms handler_ran=%d => %s\n", opname[op], restart, r,
               r < 0 ? strerror(e) : "-", t, (int)handled, verdict);
        fflush(stdout); _exit(0);
    }
    close(p[1]);
    char buf[512]; int n;
    while ((n = read(p[0], buf, sizeof buf)) > 0) fwrite(buf, 1, n, stdout);
    close(p[0]);
    int st; waitpid(pid, &st, 0);
    if (!WIFEXITED(st)) printf("restart %-28s SA_RESTART=%d: CHILD DIED status=0x%x (0x0e = the 10 s guard)\n", opname[op], restart, st);
}

/* --- si_code --- */
static volatile sig_atomic_t got_code, got_pid;
static void info_handler(int s, siginfo_t *si, void *ctx) { (void)s; (void)ctx; got_code = si->si_code; got_pid = si->si_pid; }
static void sicode_trial(int how)
{
    const char *names[] = { "kill(getpid())", "raise()", "pthread_kill(self)", "sigqueue(getpid())" };
    pid_t pid = fork();
    if (pid == 0) {
        GUARD_CHILD();
        struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_sigaction = info_handler; sa.sa_flags = SA_SIGINFO;
        sigaction(SIGUSR1, &sa, NULL);
        if (how == 0) kill(getpid(), SIGUSR1);
        else if (how == 1) raise(SIGUSR1);
        else if (how == 2) pthread_kill(pthread_self(), SIGUSR1);
#ifndef __APPLE__
        else { union sigval v; v.sival_int = 7; sigqueue(getpid(), SIGUSR1, v); }
#endif
        printf("sicode %-20s si_code=%d si_pid=%s  (SI_USER=%d SI_QUEUE=%d"
#ifndef __APPLE__
               " SI_TKILL=%d"
#endif
               ")\n", names[how], (int)got_code, got_pid == getpid() ? "self" : got_pid == 0 ? "0" : "other", SI_USER, SI_QUEUE
#ifndef __APPLE__
               , SI_TKILL
#endif
               );
        fflush(stdout); _exit(0);
    }
    waitpid(pid, NULL, 0);
}

int main(void)
{
    GUARD_MAIN();
    setvbuf(stdout, NULL, _IOLBF, 0);
    printf("# flavour %s\n", FLAVOUR);
    int flagsets[] = { 0, SA_NODEFER, SA_RESETHAND, SA_NODEFER | SA_RESETHAND };
    for (int f = 0; f < 4; f++) for (int m = 0; m < 2; m++) mask_trial(flagsets[f], m);
    int std[64]; int n = standard_catchable(std);
    for (int i = 0; i < n; i++) resethand_trial(std[i]);
    reraise_trial(0); reraise_trial(SA_NODEFER);
    for (int op = 0; op < OP_COUNT; op++) for (int r = 0; r < 2; r++) restart_trial(op, r);
#ifdef __APPLE__
    for (int how = 0; how < 3; how++) sicode_trial(how);
#else
    for (int how = 0; how < 4; how++) sicode_trial(how);
#endif
    return 0;
}
