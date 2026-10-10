// Whether a SIGCONT that nothing blocks is ever pending: what generating it
// does, against what delivering it does.
//
// Every row runs in a fresh forked child of two threads, the main thread (the
// process's leader, "m") and a second ("s"), and is repeated REPEATS times;
// each distinct line a row printed is printed once, after "xN" giving how many
// of its repetitions printed it. A pending set is the 64-bit word whose bit
// n-1 is signo n, as sigpending(2) returns it to the thread that asks: raw,
// without the probe masking it.
//
// Part "gen" (generation, both threads awake). SIGCONT under a disposition
// (DFL, IGN, or a handler H, installed first), with each thread blocking it or
// not (blk=none, m, s, both), is sent by one thread (from=m or s) with
// kill(getpid()) (to=proc) or pthread_kill of a thread (to=m or to=s). Every
// thread that is waiting sleeps in pthread_cond_wait. In order:
//   A - the sender's sigpending, straight after the send;
//   B - the other thread's sigpending, once the sender has finished;
//   then the main thread installs a counting handler for SIGCONT (for DFL and
//   IGN; H already has one), the main thread unblocks everything, and then the
//   second thread does.
//   runs - every run of the handler, as thread@phase: phase 0 before the late
//   handler was installed, 1 after it, 2 once the main thread has unblocked,
//   3 once the second thread has. "-" if none ran.
// So a SIGCONT that was pending anywhere once the sender had finished, and
// that nothing took before the late handler arrived, shows as a run at
// phase >= 1. The same rows for two signals other than SIGCONT that are
// ignored at generation, USR1 under SIG_IGN and URG at its default, sent with
// kill, are the controls ("gen USR1 IGN", "gen URG DFL").
//
// Part "unblock" (one thread). SIGCONT under DFL or IGN, blocked, sent with
// kill (to=proc) or pthread_kill of the thread itself (to=self):
//   A - sigpending; then the thread unblocks SIGCONT and blocks it again;
//   B - sigpending; then a counting handler is installed and SIGCONT unblocked.
//   runs - as for "gen", phase 1 after the first unblock, 2 after the second.
//
// Part "sleep". One thread (sleeper=m or s) sleeps, with SIGCONT not blocked:
// in poll(NULL, 0, 300) (in=poll), or in sigsuspend with an empty set while
// its own mask blocks SIGCONT (in=sigsuspend). The other thread blocks SIGCONT
// and, 50 ms after the sleeper announced itself, sends it (to=proc or to the
// sleeper), under DFL, IGN or H. Then:
//   A - the sender's sigpending straight after the send (phase 1);
//   B - 50 ms later;
//   the sender installs the late handler (DFL and IGN), phase 2;
//   C - 50 ms later; then phase 3, and for a sigsuspend that has not returned
//   the sender sends SIGUSR1 (caught, by a handler that logs it) to the
//   sleeper with pthread_kill.
//   ret - the sleeper's call's return and errno, and the phase it returned in.
//   runs - as for "gen", the signal named: CONT or USR1.
//
// Part "stopinfo". Whether a SIGSTOP discards a pending SIGCONT. The child
// catches SIGCONT with SA_SIGINFO, blocks it, and sends it to itself; then the
// parent sends SIGSTOP (stop=1) or nothing (stop=0), waits for the stop, and
// sends SIGCONT; the child, told to go on, reports its sigpending and unblocks
// SIGCONT. The handler records si_pid: "self" if the instance delivered is the
// child's own, "parent" if it is the parent's, "0" or "other" otherwise. The stop itself cannot be seen
// by a process, which runs again only once a SIGCONT is generated, which
// coalesces into any pending one; the siginfo kept says which survived.
//
// What generating SIGCONT does to pending stop signals, and generating a stop
// signal to a pending SIGCONT, are measured by signal-disposition-table.c
// (parts "fl" and "fu").
//
// Bounded: GUARD_MAIN and GUARD_CHILD (alarm), every loop over a fixed list,
// and every sleep finite: the sigsuspend rows are always ended by the SIGUSR1.
// No signal leaves the child except the parent's SIGSTOP and SIGCONT in
// "stopinfo".
//
// Darwin: clang -Wall -Wno-unused-function -o /tmp/sigcont-generation sigcont-generation.c && /tmp/sigcont-generation
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/sigcont-generation.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container, 5 CPUs), 2026-10-10; the output is beside this
// file as sigcont-generation.darwin-27.0-uid501.txt and
// sigcont-generation.linux-6.18.5-aarch64.txt. An earlier run of the SIGCONT
// "gen" rows and the "sleep" rows on each printed the same lines. WoofWare.PosixKernel.Test/TestSigcontGeneration.fs
// replays the "gen", "sleep" and "unblock" rows.
#include "signal-probe-common.h"
#include <poll.h>
#include <stdarg.h>
#include <sys/utsname.h>

#define REPEATS 10

static uint64_t bits_of(const sigset_t *set)
{
#ifdef __APPLE__
    uint32_t w;
#else
    uint64_t w;
#endif
    memcpy(&w, set, sizeof w);
    return (uint64_t)w;
}

static uint64_t pending_now(void)
{
    sigset_t s;
    memset(&s, 0, sizeof s);
    if (sigpending(&s) != 0) die("sigpending");
    return bits_of(&s);
}

static void set_blocked(const int *sigs, int n)
{
    sigset_t s;
    sigemptyset(&s);
    for (int i = 0; i < n; i++) sigaddset(&s, sigs[i]);
    if (pthread_sigmask(SIG_SETMASK, &s, NULL) != 0) die("pthread_sigmask");
}

static void block_one(int sig, int yes)
{
    set_blocked(&sig, yes ? 1 : 0);
}

static void block_cont(int yes) { block_one(SIGCONT, yes); }

static uint64_t bit(int signo) { return (uint64_t)1 << (signo - 1); }

// ---------------------------------------------------------------- shared --

static pthread_mutex_t g_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_cond = PTHREAD_COND_INITIALIZER;
static int g_stage;

static void wait_stage(int stage)
{
    pthread_mutex_lock(&g_lock);
    while (g_stage < stage) pthread_cond_wait(&g_cond, &g_lock);
    pthread_mutex_unlock(&g_lock);
}

static void advance(int stage)
{
    pthread_mutex_lock(&g_lock);
    g_stage = stage;
    pthread_cond_broadcast(&g_cond);
    pthread_mutex_unlock(&g_lock);
}

static pthread_t g_main, g_second;
static volatile sig_atomic_t g_phase;

#define MAX_RUNS 8
static volatile sig_atomic_t g_run_count;
static volatile int g_run_sig[MAX_RUNS];
static volatile int g_run_main[MAX_RUNS];
static volatile int g_run_phase[MAX_RUNS];

static void on_signal(int s)
{
    int i = __atomic_fetch_add((int *)&g_run_count, 1, __ATOMIC_SEQ_CST);
    if (i < MAX_RUNS) {
        g_run_sig[i] = s;
        g_run_main[i] = pthread_equal(pthread_self(), g_main);
        g_run_phase[i] = g_phase;
    }
}

static void catch(int sig)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_signal;
    sigemptyset(&sa.sa_mask);
    if (sigaction(sig, &sa, NULL) != 0) die("sigaction");
}

enum disp { DFL, IGN, H };
static const char *disp_name[] = { "DFL", "IGN", "H" };

static void set_disp(int sig, enum disp d)
{
    switch (d) {
    case DFL: signal(sig, SIG_DFL); break;
    case IGN: signal(sig, SIG_IGN); break;
    case H: catch(sig); break;
    }
}

// Where a send goes: the process, or one of the two threads.
enum route { TO_PROC, TO_MAIN, TO_SECOND };
static const char *route_name[] = { "proc", "m", "s" };

static void send_sig(int sig, enum route r)
{
    switch (r) {
    case TO_PROC: if (kill(getpid(), sig) != 0) die("kill"); break;
    case TO_MAIN: if (pthread_kill(g_main, sig) != 0) die("pthread_kill"); break;
    case TO_SECOND: if (pthread_kill(g_second, sig) != 0) die("pthread_kill"); break;
    }
}

static void out(int fd, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
static void out(int fd, const char *fmt, ...)
{
    char line[512];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(line, sizeof line, fmt, ap);
    va_end(ap);
    write(fd, line, strlen(line));
}

static void out_runs(int fd)
{
    int n = g_run_count;
    out(fd, " runs=");
    if (n == 0) { out(fd, "-"); return; }
    for (int i = 0; i < n && i < MAX_RUNS; i++)
        out(fd, "%s%s:%s@%d", i ? "," : "", signame(g_run_sig[i]), g_run_main[i] ? "m" : "s", g_run_phase[i]);
    if (n > MAX_RUNS) out(fd, ",...(%d)", n);
}

// Run `body` REPEATS times, each in a fresh child, and print each distinct
// line it produced once, with its count.
static void in_children(const char *tag, void (*body)(int fd, void *arg), void *arg)
{
    char lines[REPEATS][1024];
    int counts[REPEATS];
    int distinct = 0;
    for (int rep = 0; rep < REPEATS; rep++) {
        int fds[2];
        if (pipe(fds) != 0) die("pipe");
        fflush(stdout);
        pid_t child = fork();
        if (child < 0) die("fork");
        if (child == 0) {
            GUARD_CHILD();
            close(fds[0]);
            body(fds[1], arg);
            _exit(0);
        }
        close(fds[1]);
        char buf[900];
        ssize_t total = 0, n;
        while (total < (ssize_t)sizeof buf - 1 && (n = read(fds[0], buf + total, sizeof buf - 1 - total)) > 0)
            total += n;
        buf[total] = 0;
        close(fds[0]);
        int status;
        waitpid(child, &status, 0);
        char line[1024];
        snprintf(line, sizeof line, "%s end=%s%d", buf, WIFEXITED(status) ? "exit" : "signal",
                 WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
        int found = -1;
        for (int i = 0; i < distinct; i++)
            if (strcmp(lines[i], line) == 0) found = i;
        if (found < 0) {
            found = distinct++;
            strcpy(lines[found], line);
            counts[found] = 0;
        }
        counts[found]++;
    }
    for (int i = 0; i < distinct; i++) printf("%s x%d%s\n", tag, counts[i], lines[i]);
    fflush(stdout);
}

// ------------------------------------------------------------------- gen --

struct gen_row {
    int sig;
    enum disp disp;
    enum route route;
    int from_second;
    int block_main, block_second;
};

static struct gen_row *g_gen;
static uint64_t g_a, g_b;

// The steps both threads share, each run by the thread whose turn it is.
static void gen_steps(int is_main)
{
    int is_sender = is_main != g_gen->from_second;
    // Stage 1: the second thread has set its mask.
    wait_stage(1);
    if (is_sender) {
        send_sig(g_gen->sig, g_gen->route);
        g_a = pending_now();
        advance(2);
    }
    wait_stage(2);
    if (!is_sender) {
        g_b = pending_now();
        advance(3);
    }
    wait_stage(3);
    if (is_main) {
        g_phase = 1;
        if (g_gen->disp != H) catch(g_gen->sig);
        g_phase = 2;
        block_one(g_gen->sig, 0);
        advance(4);
    }
    wait_stage(4);
    if (!is_main) {
        g_phase = 3;
        block_one(g_gen->sig, 0);
        advance(5);
    }
    wait_stage(5);
}

static void *gen_second(void *arg)
{
    (void)arg;
    g_second = pthread_self();
    block_one(g_gen->sig, g_gen->block_second);
    advance(1);
    gen_steps(0);
    return NULL;
}

static void gen_body(int fd, void *arg)
{
    g_gen = arg;
    g_main = pthread_self();
    set_disp(g_gen->sig, g_gen->disp);
    block_one(g_gen->sig, g_gen->block_main);
    if (pthread_create(&g_second, NULL, gen_second, NULL) != 0) die("pthread_create");
    gen_steps(1);
    pthread_join(g_second, NULL);
    sleep_ms(10);
    out(fd, " A=%llx B=%llx", (unsigned long long)g_a, (unsigned long long)g_b);
    out_runs(fd);
}

// ----------------------------------------------------------------- sleep --

struct sleep_row {
    enum disp disp;
    int sleeper_second;
    int to_proc;
    int in_sigsuspend;
};

static struct sleep_row *g_sl;
static volatile int g_ret, g_errno, g_ret_phase, g_returned;

static void sleeper_steps(void)
{
    if (g_sl->in_sigsuspend) {
        block_cont(1);
        advance(1);
        sigset_t empty;
        sigemptyset(&empty);
        g_ret = sigsuspend(&empty);
    } else {
        block_cont(0);
        advance(1);
        g_ret = poll(NULL, 0, 300);
    }
    g_errno = errno;
    g_ret_phase = g_phase;
    g_returned = 1;
    advance(2);
}

static uint64_t g_c;

static void sender_steps(void)
{
    block_cont(1);
    wait_stage(1);
    sleep_ms(50);
    g_phase = 1;
    pthread_t sleeper = g_sl->sleeper_second ? g_second : g_main;
    if (g_sl->to_proc) {
        if (kill(getpid(), SIGCONT) != 0) die("kill");
    } else {
        if (pthread_kill(sleeper, SIGCONT) != 0) die("pthread_kill");
    }
    g_a = pending_now();
    sleep_ms(50);
    g_b = pending_now();
    if (g_sl->disp != H) catch(SIGCONT);
    g_phase = 2;
    sleep_ms(50);
    g_c = pending_now();
    g_phase = 3;
    if (g_sl->in_sigsuspend && !g_returned) pthread_kill(sleeper, SIGUSR1);
    wait_stage(2);
}

static void *sleep_second(void *arg)
{
    (void)arg;
    g_second = pthread_self();
    if (g_sl->sleeper_second) sleeper_steps();
    else sender_steps();
    return NULL;
}

static void sleep_body(int fd, void *arg)
{
    g_sl = arg;
    g_main = pthread_self();
    set_disp(SIGCONT, g_sl->disp);
    catch(SIGUSR1);
    if (pthread_create(&g_second, NULL, sleep_second, NULL) != 0) die("pthread_create");
    if (g_sl->sleeper_second) sender_steps();
    else sleeper_steps();
    pthread_join(g_second, NULL);
    sleep_ms(10);
    out(fd, " A=%llx B=%llx C=%llx ret=%d/%s@%d", (unsigned long long)g_a, (unsigned long long)g_b,
        (unsigned long long)g_c, g_ret, g_ret == 0 ? "0" : g_errno == EINTR ? "EINTR" : "other", g_ret_phase);
    out_runs(fd);
}

// --------------------------------------------------------------- unblock --

struct unblock_row { enum disp disp; int to_proc; };

static void unblock_body(int fd, void *arg)
{
    struct unblock_row *row = arg;
    g_main = pthread_self();
    set_disp(SIGCONT, row->disp);
    block_cont(1);
    if (row->to_proc) kill(getpid(), SIGCONT);
    else pthread_kill(g_main, SIGCONT);
    uint64_t a = pending_now();
    g_phase = 1;
    block_cont(0);
    block_cont(1);
    uint64_t b = pending_now();
    catch(SIGCONT);
    g_phase = 2;
    block_cont(0);
    out(fd, " A=%llx B=%llx", (unsigned long long)a, (unsigned long long)b);
    out_runs(fd);
}

// -------------------------------------------------------------- stopinfo --

static volatile pid_t g_si_pid = -1;
static volatile int g_si_count;

static void on_cont_info(int s, siginfo_t *info, void *ctx)
{
    (void)s;
    (void)ctx;
    g_si_pid = info->si_pid;
    g_si_count++;
}

static void stopinfo(int stop)
{
    int to_child[2], from_child[2];
    if (pipe(to_child) != 0 || pipe(from_child) != 0) die("pipe");
    fflush(stdout);
    pid_t child = fork();
    if (child < 0) die("fork");
    if (child == 0) {
        GUARD_CHILD();
        close(to_child[1]);
        close(from_child[0]);
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_sigaction = on_cont_info;
        sa.sa_flags = SA_SIGINFO;
        sigemptyset(&sa.sa_mask);
        if (sigaction(SIGCONT, &sa, NULL) != 0) die("sigaction");
        block_cont(1);
        kill(getpid(), SIGCONT);
        uint64_t before = pending_now();
        char c = 'r';
        write(from_child[1], &c, 1);
        // The parent stops us (or not), then sends SIGCONT, then says go on.
        while (read(to_child[0], &c, 1) < 0 && errno == EINTR) {}
        uint64_t after = pending_now();
        block_cont(0);
        char line[256];
        pid_t self = getpid(), parent = getppid();
        snprintf(line, sizeof line, " before=%llx after=%llx runs=%d si_pid=%s", (unsigned long long)before,
                 (unsigned long long)after, g_si_count,
                 g_si_pid == self ? "self" : g_si_pid == parent ? "parent" : g_si_pid == 0 ? "0" : "other");
        write(from_child[1], line, strlen(line));
        _exit(0);
    }
    close(to_child[0]);
    close(from_child[1]);
    char c;
    while (read(from_child[0], &c, 1) < 0 && errno == EINTR) {}
    int status;
    int stopped = 0;
    if (stop) {
        kill(child, SIGSTOP);
        pid_t w = waitpid(child, &status, WUNTRACED);
        stopped = w == child && WIFSTOPPED(status);
    }
    kill(child, SIGCONT);
    if (stop) {
        pid_t w = waitpid(child, &status, WCONTINUED);
        (void)w;
    }
    c = 'g';
    write(to_child[1], &c, 1);
    char buf[512];
    ssize_t total = 0, n;
    while (total < (ssize_t)sizeof buf - 1 && (n = read(from_child[0], buf + total, sizeof buf - 1 - total)) > 0)
        total += n;
    buf[total] = 0;
    close(from_child[0]);
    close(to_child[1]);
    waitpid(child, &status, 0);
    printf("stopinfo stop=%d stopped=%d%s end=%s%d\n", stop, stopped, buf, WIFEXITED(status) ? "exit" : "signal",
           WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
    fflush(stdout);
}

int main(void)
{
    GUARD_MAIN();
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, uid %d\n", u.sysname, u.release, u.machine, (int)getuid());
    printf("# flavour %s CONT %llx USR1 %llx URG %llx, %d repetitions a row\n", FLAVOUR,
           (unsigned long long)bit(SIGCONT), (unsigned long long)bit(SIGUSR1), (unsigned long long)bit(SIGURG), REPEATS);

    char tag[160];
    const char *blk_name[] = { "none", "m", "s", "both" };
    for (int d = DFL; d <= H; d++)
        for (int r = TO_PROC; r <= TO_SECOND; r++)
            for (int from_second = 0; from_second <= 1; from_second++)
                for (int blk = 0; blk < 4; blk++) {
                    struct gen_row row = { SIGCONT, d, r, from_second, blk & 1, (blk >> 1) & 1 };
                    snprintf(tag, sizeof tag, "gen CONT %s to=%s from=%s blk=%s", disp_name[d], route_name[r],
                             from_second ? "s" : "m", blk_name[blk]);
                    in_children(tag, gen_body, &row);
                }
    // Controls: two signals other than SIGCONT that are ignored at generation.
    struct { int sig; enum disp disp; } controls[] = { { SIGUSR1, IGN }, { SIGURG, DFL } };
    for (size_t c = 0; c < 2; c++)
        for (int from_second = 0; from_second <= 1; from_second++)
            for (int blk = 0; blk < 4; blk++) {
                struct gen_row row = { controls[c].sig, controls[c].disp, TO_PROC, from_second, blk & 1, (blk >> 1) & 1 };
                snprintf(tag, sizeof tag, "gen %s %s to=proc from=%s blk=%s", signame(controls[c].sig),
                         disp_name[controls[c].disp], from_second ? "s" : "m", blk_name[blk]);
                in_children(tag, gen_body, &row);
            }

    for (int in_sigsuspend = 0; in_sigsuspend <= 1; in_sigsuspend++)
        for (int d = DFL; d <= H; d++)
            for (int sleeper_second = 0; sleeper_second <= 1; sleeper_second++)
                for (int to_proc = 0; to_proc <= 1; to_proc++) {
                    struct sleep_row row = { d, sleeper_second, to_proc, in_sigsuspend };
                    snprintf(tag, sizeof tag, "sleep in=%s %s sleeper=%s to=%s",
                             in_sigsuspend ? "sigsuspend" : "poll", disp_name[d], sleeper_second ? "s" : "m",
                             to_proc ? "proc" : "sleeper");
                    in_children(tag, sleep_body, &row);
                }

    for (int d = DFL; d <= IGN; d++)
        for (int to_proc = 0; to_proc <= 1; to_proc++) {
            struct unblock_row row = { d, to_proc };
            snprintf(tag, sizeof tag, "unblock %s to=%s", disp_name[d], to_proc ? "proc" : "self");
            in_children(tag, unblock_body, &row);
        }

    for (int rep = 0; rep < 3; rep++) {
        stopinfo(0);
        stopinfo(1);
    }
    return 0;
}
