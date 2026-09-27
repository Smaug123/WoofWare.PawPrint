// In what order are several pending, blocked, distinct signals delivered
// once they are unblocked?
//
// Sweep: the set G of signals generated is every standard catchable signal
// (1..31 minus KILL/STOP; 29 signals), in 2 fixed orders (ascending,
// descending) and 40 seeded random permutations. Three consumers:
//   sigwait   - block G, generate, drain with sigwait(G) (no handler runs)
//   fullmask  - handler with sa_mask = G, so handlers never nest; the entry
//               order is the kernel's pick order
//   nomask    - handler with an empty sa_mask (the default), recording both
//               entry and exit, to show how nesting reorders what a handler
//               observes
// and two generation shapes:
//   proc      - every signal via kill(getpid(), s)
//   thread    - every signal via pthread_kill(pthread_self(), s)
// Then, separately, the "pair" sweep: every ordered pair (a, b) of distinct
// standard catchable signals (29*28 = 812 pairs), a generated thread-directed
// and b process-directed, consumed by fullmask: which comes first?
// And on Linux, the "rt" sweep: G plus every SIGRTMIN..SIGRTMAX, 20 seeded
// permutations, fullmask and sigwait; and FIFO within one real-time signo
// (sigqueue values).
//
// Every trial runs in a fresh forked child, so no disposition or pending
// state crosses trials.
//
// Recorded in WoofWare.PosixKernel.Test/signalOrder/, one file per flavour; TestSignalPickOrder
// replays every row against the model.
//
// Build: Darwin `nix develop -c clang -pthread -O0 -o /tmp/p signal-pick-order.c`; Linux the same
// with gcc in a Debian trixie container (Apple's `container` CLI). Every trial runs in a forked
// child with its own alarm(10), and the whole run has alarm(300).
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifdef __APPLE__
#define FLAVOUR "darwin"
#define HIGHEST_SIGNO 31
#else
#define FLAVOUR "linux"
#define HIGHEST_SIGNO 64
#endif

/* SIGALRM's default action terminates, so a hung child dies visibly (the
 * parent reports status 0x0e). A probe that installs a SIGALRM handler must
 * not rely on this in that child. */
#define GUARD_MAIN() alarm(300)
#define GUARD_CHILD() alarm(10)

static const char *signame(int s)
{
    static char buf[16];
    switch (s) {
    case SIGHUP: return "HUP"; case SIGINT: return "INT"; case SIGQUIT: return "QUIT";
    case SIGILL: return "ILL"; case SIGTRAP: return "TRAP"; case SIGABRT: return "ABRT";
    case SIGBUS: return "BUS"; case SIGFPE: return "FPE"; case SIGKILL: return "KILL";
    case SIGUSR1: return "USR1"; case SIGSEGV: return "SEGV"; case SIGUSR2: return "USR2";
    case SIGPIPE: return "PIPE"; case SIGALRM: return "ALRM"; case SIGTERM: return "TERM";
    case SIGCHLD: return "CHLD"; case SIGCONT: return "CONT"; case SIGSTOP: return "STOP";
    case SIGTSTP: return "TSTP"; case SIGTTIN: return "TTIN"; case SIGTTOU: return "TTOU";
    case SIGURG: return "URG"; case SIGXCPU: return "XCPU"; case SIGXFSZ: return "XFSZ";
    case SIGVTALRM: return "VTALRM"; case SIGPROF: return "PROF"; case SIGWINCH: return "WINCH";
    case SIGIO: return "IO"; case SIGSYS: return "SYS";
#ifdef __APPLE__
    case SIGEMT: return "EMT"; case SIGINFO: return "INFO";
#else
    case SIGSTKFLT: return "STKFLT"; case SIGPWR: return "PWR";
#endif
    }
#ifndef __APPLE__
    if (s >= SIGRTMIN && s <= SIGRTMAX) { snprintf(buf, sizeof buf, "RTMIN+%d", s - SIGRTMIN); return buf; }
#endif
    snprintf(buf, sizeof buf, "sig%d", s);
    return buf;
}

/* Standard (non-real-time) signals a handler can be installed for:
 * 1..31 minus KILL and STOP. */
static int standard_catchable(int *out)
{
    int n = 0;
    for (int s = 1; s <= 31; s++)
        if (s != SIGKILL && s != SIGSTOP) out[n++] = s;
    return n;
}

/* Every signal sigaction accepts on this flavour: the standard ones, plus
 * glibc's SIGRTMIN..SIGRTMAX on Linux (32 and 33 are glibc's own). */
static int all_catchable(int *out)
{
    int n = standard_catchable(out);
#ifndef __APPLE__
    for (int s = SIGRTMIN; s <= SIGRTMAX; s++) out[n++] = s;
#endif
    return n;
}

static uint64_t xs_state = 0x9E3779B97F4A7C15ull;
static uint64_t xs_next(void)
{
    xs_state ^= xs_state << 13; xs_state ^= xs_state >> 7; xs_state ^= xs_state << 17;
    return xs_state;
}
static void shuffle(int *a, int n)
{
    for (int i = n - 1; i > 0; i--) { int j = (int)(xs_next() % (uint64_t)(i + 1)); int t = a[i]; a[i] = a[j]; a[j] = t; }
}

static void die(const char *what) { perror(what); _exit(99); }

static long now_ms(void)
{
    struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000L + ts.tv_nsec / 1000000L;
}

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {}
}

#define MAXS 128
static volatile sig_atomic_t entry_log[MAXS * 4], exit_log[MAXS * 4];
static volatile sig_atomic_t n_entry, n_exit;
static volatile sig_atomic_t rt_vals[64]; static volatile sig_atomic_t n_rt;

static void rec(int s) { entry_log[n_entry++] = s; exit_log[n_exit++] = s; }
static void rec_nomask(int s)
{
    entry_log[n_entry++] = s;
    exit_log[n_exit++] = s;
}
static void rec_info(int s, siginfo_t *si, void *ctx)
{
    (void)ctx; entry_log[n_entry++] = s; exit_log[n_exit++] = s;
    if (si->si_code == SI_QUEUE) rt_vals[n_rt++] = si->si_value.sival_int;
}

enum consumer { C_SIGWAIT, C_FULLMASK, C_NOMASK };
enum shape { S_PROC, S_THREAD };

static void gen(int s, enum shape sh)
{
    if (sh == S_PROC) { if (kill(getpid(), s) != 0) die("kill"); }
    else { int r = pthread_kill(pthread_self(), s); if (r) { errno = r; die("pthread_kill"); } }
}

static void trial(int *g, int n, enum consumer c, enum shape sh, const char *label)
{
    int p[2]; if (pipe(p)) die("pipe");
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        GUARD_CHILD();
        close(p[0]);
        sigset_t set; sigemptyset(&set);
        for (int i = 0; i < n; i++) sigaddset(&set, g[i]);
        if (c != C_SIGWAIT) {
            for (int i = 0; i < n; i++) {
                struct sigaction sa; memset(&sa, 0, sizeof sa);
                sa.sa_handler = c == C_NOMASK ? rec_nomask : rec;
                if (c == C_FULLMASK) sa.sa_mask = set; else sigemptyset(&sa.sa_mask);
                if (sigaction(g[i], &sa, NULL)) die("sigaction");
            }
        }
        if (sigprocmask(SIG_BLOCK, &set, NULL)) die("sigprocmask");
        for (int i = 0; i < n; i++) gen(g[i], sh);
        char out[4096]; int o = 0;
        if (c == C_SIGWAIT) {
            for (int guard = 0; guard < 4 * MAXS; guard++) {
                sigset_t pend; sigpending(&pend);
                int any = 0; for (int i = 0; i < n; i++) if (sigismember(&pend, g[i])) any = 1;
                if (!any) break;
                int s; if (sigwait(&set, &s)) die("sigwait");
                entry_log[n_entry++] = s; exit_log[n_exit++] = s;
            }
        } else {
            sigprocmask(SIG_UNBLOCK, &set, NULL);
        }
        o += snprintf(out + o, sizeof out - o, "delivered:");
        for (int i = 0; i < n_entry; i++) o += snprintf(out + o, sizeof out - o, " %d", (int)entry_log[i]);
        if (c == C_NOMASK) {
            o += snprintf(out + o, sizeof out - o, " | exit:");
            for (int i = 0; i < n_exit; i++) o += snprintf(out + o, sizeof out - o, " %d", (int)exit_log[i]);
        }
        o += snprintf(out + o, sizeof out - o, "\n");
        write(p[1], out, o);
        _exit(0);
    }
    close(p[1]);
    char buf[4096]; int got = 0, r;
    while ((r = read(p[0], buf + got, sizeof buf - 1 - got)) > 0) got += r;
    buf[got] = 0; close(p[0]);
    int st; waitpid(pid, &st, 0);
    printf("%s generated:", label);
    for (int i = 0; i < n; i++) printf(" %d", g[i]);
    printf(" %s", buf);
    if (!WIFEXITED(st) || WEXITSTATUS(st) != 0) printf("%s CHILD-DIED status=0x%x\n", label, st);
}

int main(void)
{
    GUARD_MAIN();
    setvbuf(stdout, NULL, _IOLBF, 0);
    int std[64]; int nstd = standard_catchable(std);
    const char *cname[] = { "sigwait", "fullmask", "nomask" };
    const char *sname[] = { "proc", "thread" };
    printf("# flavour %s; standard catchable signals: %d\n", FLAVOUR, nstd);

    for (int c = 0; c < 3; c++)
        for (int sh = 0; sh < 2; sh++)
            for (int perm = 0; perm < 42; perm++) {
                int g[64]; memcpy(g, std, sizeof(int) * nstd);
                if (perm == 1) for (int i = 0; i < nstd; i++) g[i] = std[nstd - 1 - i];
                if (perm >= 2) { xs_state = 0x1234567ull + perm; shuffle(g, nstd); }
                char label[64]; snprintf(label, sizeof label, "std %s %s perm%02d", cname[c], sname[sh], perm);
                trial(g, nstd, c, sh, label);
            }

    for (int i = 0; i < nstd; i++)
        for (int j = 0; j < nstd; j++) {
            if (i == j) continue;
            int a = std[i], b = std[j];
            int p[2]; pipe(p);
            pid_t pid = fork();
            if (pid == 0) {
                GUARD_CHILD();
                sigset_t set; sigemptyset(&set); sigaddset(&set, a); sigaddset(&set, b);
                struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = rec; sa.sa_mask = set;
                sigaction(a, &sa, NULL); sigaction(b, &sa, NULL);
                sigprocmask(SIG_BLOCK, &set, NULL);
                pthread_kill(pthread_self(), a);
                kill(getpid(), b);
                sigprocmask(SIG_UNBLOCK, &set, NULL);
                char out[64]; int o = snprintf(out, sizeof out, "%d", n_entry > 0 ? (int)entry_log[0] : 0);
                for (int k = 1; k < n_entry; k++) o += snprintf(out + o, sizeof out - o, ",%d", (int)entry_log[k]);
                write(p[1], out, o); _exit(0);
            }
            close(p[1]); char buf[64] = {0}; read(p[0], buf, 63); close(p[0]); waitpid(pid, NULL, 0);
            printf("pair thread=%d proc=%d delivered=%s\n", a, b, buf);
        }

#ifndef __APPLE__
    {
        int all[128]; int nall = all_catchable(all);
        for (int c = 0; c < 2; c++)
            for (int perm = 0; perm < 20; perm++) {
                int g[128]; memcpy(g, all, sizeof(int) * nall);
                xs_state = 0xABCDEFull + perm; shuffle(g, nall);
                char label[64]; snprintf(label, sizeof label, "rt %s proc perm%02d", cname[c], perm);
                trial(g, nall, c, S_PROC, label);
            }
        pid_t pid = fork();
        if (pid == 0) {
            GUARD_CHILD();
            int a = SIGRTMIN + 3, b = SIGRTMIN + 1;
            sigset_t set; sigemptyset(&set); sigaddset(&set, a); sigaddset(&set, b);
            struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_sigaction = rec_info; sa.sa_flags = SA_SIGINFO; sa.sa_mask = set;
            sigaction(a, &sa, NULL); sigaction(b, &sa, NULL);
            sigprocmask(SIG_BLOCK, &set, NULL);
            int seq[] = { a, b, a, b, a, b };
            for (int k = 0; k < 6; k++) { union sigval v; v.sival_int = 100 * (k + 1) + (seq[k] == a ? 3 : 1); sigqueue(getpid(), seq[k], v); }
            sigprocmask(SIG_UNBLOCK, &set, NULL);
            printf("rtfifo queued (signo:value) %d:103 %d:201 %d:303 %d:401 %d:503 %d:601 -> delivered values:", a, b, a, b, a, b);
            for (int k = 0; k < n_rt; k++) printf(" %d", (int)rt_vals[k]);
            printf("\n"); fflush(stdout); _exit(0);
        }
        waitpid(pid, NULL, 0);
    }
#endif
    return 0;
}
