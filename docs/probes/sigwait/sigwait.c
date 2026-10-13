// Probe M6 of the signal-masks-and-sleep scoping note: what sigwait(3),
// sigwaitinfo(2) and sigtimedwait(2) take, and when.
//
// Build and run: cc -O0 -Wall -pthread -o sigwait sigwait.c && ./sigwait
// (Linux: gcc ... ; run as root for the `rank fifo` rows, which need
// SCHED_FIFO.)
//
// Every row runs in a fresh forked child, so no row sees another's mask or
// dispositions, and a row whose child a signal kills reports `end=signalN`.
// A child that hangs is killed by its own alarm (`end=signal14`).
//
// Darwin's C library has sigwait(3) alone; its system call is __sigwait, which
// the `raw` rows call directly. Linux's C library implements all three over the
// one system call rt_sigtimedwait, which the `raw` rows call directly.
//
// Fields: `ret` is what the call returned (sigwait returns an error number;
// the others return a signal number or -1 with `errno`); `sig` is what sigwait
// stored (preset to -77, so -77 means "not written"); `handled` lists the
// handlers that had run when the call returned, in order; `later` lists those
// that ran after it, within 200 ms, each as SIG@ms since the call began;
// `pending` is sigpending's word afterwards (bit n-1 is signo n); `took` is the
// call's duration in milliseconds.
//
// In the `unblocked` rows whose call returns at about 100 ms, `pending` is read
// at about 300 ms, which races with the USR2 sent then: it may or may not show
// USR2.
//
// Sections:
//   entry      - USR1 and USR2 blocked and pending before the call; two calls
//                with the set {USR1, USR2}: which comes first, and is each
//                consumed?
//   during     - USR1 blocked; sent 100 ms into the wait by another thread,
//                to the waiter (`to=thread`) or to the process (`to=process`,
//                the sender blocking it too).
//   receiver   - the main thread blocks USR1 and sleeps; a second thread
//                blocks USR1 and waits. kill() to the process: who takes it?
//                Then the main thread does not block USR1 but has a handler,
//                and is running when the signal is sent.
//   unblocked  - the awaited signal is NOT blocked: with a handler, at its
//                default action (USR1 terminates; URG ignores), and under
//                SIG_IGN; sent 100 ms in. USR2, blocked and in the set, is sent
//                at 300 ms to end a wait that the first signal did not.
//   ignored    - the awaited signal is blocked and ignored (SIG_IGN, or URG at
//                its default): sent before the call, and during it. USR2 ends
//                the wait as above.
//   outside    - the set is {USR1}, blocked. USR2 has a handler and is not
//                blocked; it is sent at 100 ms, USR1 at 300 ms. With flags 0
//                and SA_RESTART, through the C library and the raw call.
//   rank       - both an awaited signal (USR1, blocked) and a deliverable
//                handler's signal (USR2) arrive while the waiter cannot run.
//                `stop`: the parent stops the child with STOP, sends both in
//                either order, and continues it. `fifo` (Linux): waiter and
//                sender on one CPU under SCHED_FIFO, the sender at the higher
//                priority, so it sends both before the waiter runs.
//   mask       - (Linux) the waiter's SigBlk in /proc while it waits, with
//                {USR1, USR2} blocked and the set {USR1}.
//   timed      - (Linux) sigtimedwait's timeouts and screens.
//   info       - where the signal is reported: a null or unmapped `sig`
//                (Darwin __sigwait) or `info` (Linux rt_sigtimedwait), with
//                the signal pending at entry and arriving during the wait;
//                what si_code says.
//   set        - an empty set, and a set of KILL and STOP alone.
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifdef __APPLE__
#define FLAVOUR "darwin"
extern int __sigwait(const sigset_t *, int *);
#else
#define FLAVOUR "linux"
#endif

static pthread_t waiter;
static volatile sig_atomic_t log_buf[16];
static volatile int64_t log_at[16];
static volatile sig_atomic_t log_len;
static int64_t now_ms(void);
// When the row's call began; a handler's entry in the log says how many
// milliseconds after it the handler ran.
static volatile int64_t call_start;

static void record(int s)
{
    if (log_len < 16) { log_at[log_len] = now_ms(); log_buf[log_len++] = s; }
}

static void on_signal(int s) { record(s); }

static const char *signame(int s)
{
    static char buf[16];
    switch (s) {
    case SIGUSR1: return "USR1"; case SIGUSR2: return "USR2"; case SIGURG: return "URG";
    case SIGKILL: return "KILL"; case SIGSTOP: return "STOP"; case SIGTERM: return "TERM";
    case SIGALRM: return "ALRM"; case SIGCONT: return "CONT";
    }
    snprintf(buf, sizeof buf, "sig%d", s);
    return buf;
}

static int64_t now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) == -1 && errno == EINTR) {}
}

static void print_log(const char *label, int from, int to)
{
    printf(" %s=", label);
    if (from >= to) { printf("-"); return; }
    for (int i = from; i < to; i++)
        printf("%s%s@%lld", i > from ? "," : "", signame(log_buf[i]), (long long)(log_at[i] - call_start));
}

static unsigned long pending_word(void)
{
    sigset_t p;
    sigpending(&p);
    unsigned long w = 0;
    for (int s = 1; s <= 31; s++)
        if (sigismember(&p, s)) w |= 1UL << (s - 1);
    return w;
}

static void block(int s)
{
    sigset_t m;
    sigemptyset(&m);
    sigaddset(&m, s);
    pthread_sigmask(SIG_BLOCK, &m, NULL);
}

static void block_all_in_thread(void)
{
    sigset_t m;
    sigfillset(&m);
    pthread_sigmask(SIG_BLOCK, &m, NULL);
}

static void handle(int s, int flags)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_signal;
    sa.sa_flags = flags;
    sigemptyset(&sa.sa_mask);
    sigaction(s, &sa, NULL);
}

static void dispose(int s, void (*h)(int))
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = h;
    sigemptyset(&sa.sa_mask);
    sigaction(s, &sa, NULL);
}

// A scripted sender: up to four steps, each "after `at` ms since the start,
// send `sig` to the waiter thread or the process".
struct step { int at; int sig; int to_process; };
struct script { struct step steps[4]; int n; int64_t start; };

static void *sender(void *arg)
{
    struct script *sc = arg;
    block_all_in_thread();
    for (int i = 0; i < sc->n; i++) {
        int64_t wait = sc->start + sc->steps[i].at - now_ms();
        if (wait > 0) sleep_ms((int)wait);
        if (sc->steps[i].to_process) kill(getpid(), sc->steps[i].sig);
        else pthread_kill(waiter, sc->steps[i].sig);
    }
    return NULL;
}

static pthread_t start_script(struct script *sc)
{
    pthread_t t;
    sc->start = now_ms();
    pthread_create(&t, NULL, sender, sc);
    return t;
}

// The C library's sigwait, the way the rows call it, printing what it did.
static void libc_wait(const sigset_t *set)
{
    int sig = -77;
    int64_t t0 = now_ms();
    call_start = t0;
    int before = log_len;
    int ret = sigwait(set, &sig);
    int64_t took = now_ms() - t0;
    int at_return = log_len;
    printf(" ret=%d sig=%s took=%lld", ret, sig == -77 ? "-77" : signame(sig), (long long)took);
    print_log("handled", before, at_return);
    sleep_ms(200);
    print_log("later", at_return, log_len);
    printf(" pending=%lx", pending_word());
}

// The raw system call, the way the rows call it.
static void raw_wait(const sigset_t *set)
{
    int sig = -77;
    int64_t t0 = now_ms();
    call_start = t0;
    int before = log_len;
#ifdef __APPLE__
    int ret = __sigwait(set, &sig);
#else
    siginfo_t info;
    memset(&info, 0, sizeof info);
    int ret = (int)syscall(SYS_rt_sigtimedwait, set, &info, NULL, 8);
    if (ret > 0) sig = info.si_signo;
#endif
    int e = errno;
    int64_t took = now_ms() - t0;
    int at_return = log_len;
    printf(" ret=%d errno=%d sig=%s took=%lld", ret, ret == -1 ? e : 0, sig == -77 ? "-77" : signame(sig), (long long)took);
    print_log("handled", before, at_return);
    sleep_ms(200);
    print_log("later", at_return, log_len);
    printf(" pending=%lx", pending_word());
}

static void run_child(const char *label, void (*row)(void))
{
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        setvbuf(stdout, NULL, _IONBF, 0);
        alarm(5);
        printf("%s", label);
        row();
        printf("\n");
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    if (WIFSIGNALED(status)) {
        printf(" end=signal%d\n", WTERMSIG(status));
    }
    fflush(stdout);
}

static sigset_t set_of(int a, int b)
{
    sigset_t s;
    sigemptyset(&s);
    if (a) sigaddset(&s, a);
    if (b) sigaddset(&s, b);
    return s;
}

// ---- entry ----
static void row_entry(void)
{
    block(SIGUSR1); block(SIGUSR2);
    raise(SIGUSR2); raise(SIGUSR1);
    sigset_t set = set_of(SIGUSR1, SIGUSR2);
    printf(" pending-before=%lx first:", pending_word());
    libc_wait(&set);
    printf(" second:");
    libc_wait(&set);
}

// ---- during ----
static int during_to_process;
static void row_during(void)
{
    block(SIGUSR1);
    struct script sc = { { { 100, SIGUSR1, during_to_process } }, 1, 0 };
    pthread_t t = start_script(&sc);
    sigset_t set = set_of(SIGUSR1, 0);
    libc_wait(&set);
    pthread_join(t, NULL);
}
static void row_during_thread(void) { during_to_process = 0; row_during(); }
static void row_during_process(void) { during_to_process = 1; row_during(); }

// ---- receiver ----
static volatile int worker_sig = -77;
static volatile int64_t worker_took = -1;
static void *worker_waits(void *arg)
{
    (void)arg;
    block(SIGUSR1);
    sigset_t set = set_of(SIGUSR1, 0);
    int sig = -77;
    int64_t t0 = now_ms();
    sigwait(&set, &sig);
    worker_took = now_ms() - t0;
    worker_sig = sig;
    return NULL;
}
static int receiver_main_blocks;
static void row_receiver(void)
{
    if (receiver_main_blocks) block(SIGUSR1);
    else handle(SIGUSR1, 0);
    pthread_t t;
    pthread_create(&t, NULL, worker_waits, NULL);
    sleep_ms(100);
    call_start = now_ms();
    kill(getpid(), SIGUSR1);
    sleep_ms(200);
    printf(" worker-sig=%s worker-took=%lld", worker_sig == -77 ? "-77" : signame(worker_sig), (long long)worker_took);
    print_log("main-handled", 0, log_len);
    printf(" main-pending=%lx", pending_word());
    if (worker_sig == -77) {
        // Release the worker so that the child can exit.
        pthread_kill(t, SIGUSR1);
    }
    pthread_join(t, NULL);
}
static void row_receiver_blocks(void) { receiver_main_blocks = 1; row_receiver(); }
static void row_receiver_handles(void) { receiver_main_blocks = 0; row_receiver(); }

// ---- unblocked ----
static int unblocked_sig;
static int unblocked_kind; // 0 handler, 1 default, 2 SIG_IGN
static int unblocked_raw;
static void row_unblocked(void)
{
    if (unblocked_kind == 0) handle(unblocked_sig, 0);
    else if (unblocked_kind == 1) dispose(unblocked_sig, SIG_DFL);
    else dispose(unblocked_sig, SIG_IGN);
    block(SIGUSR2);
    struct script sc = { { { 100, unblocked_sig, 0 }, { 300, SIGUSR2, 0 } }, 2, 0 };
    pthread_t t = start_script(&sc);
    sigset_t set = set_of(unblocked_sig, SIGUSR2);
    if (unblocked_raw) raw_wait(&set); else libc_wait(&set);
    pthread_join(t, NULL);
}

// ---- ignored ----
static int ignored_sig;
static int ignored_by_sig_ign;
static int ignored_before;
static void row_ignored(void)
{
    if (ignored_by_sig_ign) dispose(ignored_sig, SIG_IGN);
    else dispose(ignored_sig, SIG_DFL);
    block(ignored_sig);
    block(SIGUSR2);
    if (ignored_before) {
        raise(ignored_sig);
        printf(" pending-before=%lx", pending_word());
    }
    struct script sc = { { { 100, ignored_sig, 0 }, { 300, SIGUSR2, 0 } }, 2, 0 };
    if (ignored_before) { sc.steps[0] = sc.steps[1]; sc.n = 1; }
    pthread_t t = start_script(&sc);
    sigset_t set = set_of(ignored_sig, SIGUSR2);
    libc_wait(&set);
    pthread_join(t, NULL);
}

// ---- outside ----
static int outside_flags;
static int outside_raw;
static void row_outside(void)
{
    block(SIGUSR1);
    handle(SIGUSR2, outside_flags);
    struct script sc = { { { 100, SIGUSR2, 0 }, { 300, SIGUSR1, 0 } }, 2, 0 };
    pthread_t t = start_script(&sc);
    sigset_t set = set_of(SIGUSR1, 0);
    if (outside_raw) raw_wait(&set); else libc_wait(&set);
    pthread_join(t, NULL);
}

// ---- rank (stop) ----
// The parent drives this one, so it is not run through run_child.
static void rank_stop(int raw, int usr2_first)
{
    fflush(stdout);
    int pipefd[2];
    pipe(pipefd);
    pid_t pid = fork();
    if (pid == 0) {
        setvbuf(stdout, NULL, _IONBF, 0);
        alarm(5);
        close(pipefd[0]);
        block(SIGUSR1);
        handle(SIGUSR2, 0);
        sigset_t set = set_of(SIGUSR1, 0);
        printf("rank stop %s %s-first", raw ? "raw" : "libc", usr2_first ? "USR2" : "USR1");
        write(pipefd[1], "x", 1);
        if (raw) raw_wait(&set); else libc_wait(&set);
        printf("\n");
        fflush(stdout);
        _exit(0);
    }
    close(pipefd[1]);
    char c;
    read(pipefd[0], &c, 1);
    sleep_ms(100);
    kill(pid, SIGSTOP);
    int status;
    waitpid(pid, &status, WUNTRACED);
    if (usr2_first) { kill(pid, SIGUSR2); kill(pid, SIGUSR1); }
    else { kill(pid, SIGUSR1); kill(pid, SIGUSR2); }
    sleep_ms(50);
    kill(pid, SIGCONT);
    waitpid(pid, &status, 0);
    if (WIFSIGNALED(status)) printf(" end=signal%d\n", WTERMSIG(status));
    fflush(stdout);
    close(pipefd[0]);
}

#ifndef __APPLE__
// ---- rank (fifo) ----
static int fifo_raw;
static int fifo_usr2_first;
static void *fifo_sender(void *arg)
{
    (void)arg;
    block_all_in_thread();
    sleep_ms(100);
    if (fifo_usr2_first) { pthread_kill(waiter, SIGUSR2); pthread_kill(waiter, SIGUSR1); }
    else { pthread_kill(waiter, SIGUSR1); pthread_kill(waiter, SIGUSR2); }
    // Spin a little at the higher priority before giving the CPU up, so the
    // waiter certainly cannot run between the two sends.
    int64_t until = now_ms() + 5;
    while (now_ms() < until) {}
    return NULL;
}
static void row_rank_fifo(void)
{
    cpu_set_t cpus;
    CPU_ZERO(&cpus);
    CPU_SET(0, &cpus);
    if (sched_setaffinity(0, sizeof cpus, &cpus) != 0) { printf(" affinity-failed errno=%d", errno); return; }
    struct sched_param low = { 10 };
    if (pthread_setschedparam(pthread_self(), SCHED_FIFO, &low) != 0) { printf(" fifo-refused"); return; }
    block(SIGUSR1);
    handle(SIGUSR2, 0);
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED);
    pthread_attr_setschedpolicy(&attr, SCHED_FIFO);
    struct sched_param high = { 20 };
    pthread_attr_setschedparam(&attr, &high);
    pthread_t t;
    if (pthread_create(&t, &attr, fifo_sender, NULL) != 0) { printf(" create-failed"); return; }
    sigset_t set = set_of(SIGUSR1, 0);
    if (fifo_raw) raw_wait(&set); else libc_wait(&set);
    pthread_join(t, NULL);
}

// ---- mask ----
static volatile pid_t waiter_tid;
static void *mask_reader(void *arg)
{
    (void)arg;
    block_all_in_thread();
    sleep_ms(100);
    char path[64];
    snprintf(path, sizeof path, "/proc/self/task/%d/status", (int)waiter_tid);
    FILE *f = fopen(path, "r");
    char line[256];
    while (f && fgets(line, sizeof line, f)) {
        if (strncmp(line, "SigBlk:", 7) == 0) {
            line[strcspn(line, "\n")] = 0;
            printf(" during-%s", line);
        }
    }
    if (f) fclose(f);
    pthread_kill(waiter, SIGUSR1);
    return NULL;
}
static void row_mask(void)
{
    block(SIGUSR1); block(SIGUSR2);
    waiter_tid = (pid_t)syscall(SYS_gettid);
    pthread_t t;
    pthread_create(&t, NULL, mask_reader, NULL);
    sigset_t set = set_of(SIGUSR1, 0);
    libc_wait(&set);
    sigset_t now;
    pthread_sigmask(SIG_BLOCK, NULL, &now);
    printf(" after-USR1-blocked=%d after-USR2-blocked=%d", sigismember(&now, SIGUSR1), sigismember(&now, SIGUSR2));
    pthread_join(t, NULL);
}

// ---- timed ----
static void timed_call(const char *what, const sigset_t *set, siginfo_t *info, const struct timespec *ts, size_t size)
{
    struct rusage r0, r1;
    getrusage(RUSAGE_THREAD, &r0);
    int64_t t0 = now_ms();
    call_start = t0;
    long ret = syscall(SYS_rt_sigtimedwait, set, info, ts, size);
    int e = errno;
    int64_t took = now_ms() - t0;
    getrusage(RUSAGE_THREAD, &r1);
    printf(" %s: ret=%ld errno=%d took=%lld nvcsw=%ld", what, ret, ret == -1 ? e : 0, (long long)took, r1.ru_nvcsw - r0.ru_nvcsw);
}
static void row_timed_nothing(void)
{
    block(SIGUSR1);
    sigset_t set = set_of(SIGUSR1, 0);
    struct timespec t100 = { 0, 100000000 };
    struct timespec zero = { 0, 0 };
    siginfo_t info;
    timed_call("100ms", &set, &info, &t100, 8);
    timed_call("zero", &set, &info, &zero, 8);
    struct timespec bad_nsec = { 0, 1000000000 };
    timed_call("nsec=1e9", &set, &info, &bad_nsec, 8);
    struct timespec neg = { -1, 0 };
    timed_call("sec=-1", &set, &info, &neg, 8);
    timed_call("size=4", &set, &info, &t100, 4);
    timed_call("set-unmapped", (const sigset_t *)8, &info, &t100, 8);
    timed_call("ts-unmapped", &set, &info, (const struct timespec *)8, 8);
    // The C library's sigtimedwait, for its convention.
    errno = 0;
    int r = sigtimedwait(&set, &info, &zero);
    printf(" libc-zero: ret=%d errno=%d", r, r == -1 ? errno : 0);
}
static void row_timed_pending(void)
{
    block(SIGUSR1);
    sigset_t set = set_of(SIGUSR1, 0);
    siginfo_t info;
    struct timespec bad_nsec = { 0, 1000000000 };
    raise(SIGUSR1);
    timed_call("pending,nsec=1e9", &set, &info, &bad_nsec, 8);
    printf(" pending=%lx", pending_word());
    timed_call("pending,size=4", &set, &info, NULL, 4);
    printf(" pending=%lx", pending_word());
    struct timespec zero = { 0, 0 };
    memset(&info, 0, sizeof info);
    timed_call("pending,zero", &set, &info, &zero, 8);
    printf(" si_signo=%d si_code=%d si_pid-is-self=%d pending=%lx", info.si_signo, info.si_code, info.si_pid == getpid(), pending_word());
}
static void row_timed_interrupt(void)
{
    block(SIGUSR1);
    handle(SIGUSR2, SA_RESTART);
    struct script sc = { { { 100, SIGUSR2, 0 } }, 1, 0 };
    pthread_t t = start_script(&sc);
    sigset_t set = set_of(SIGUSR1, 0);
    siginfo_t info;
    struct timespec t500 = { 0, 500000000 };
    struct timespec copy = t500;
    timed_call("SA_RESTART-handler-at-100ms,500ms", &set, &info, &t500, 8);
    print_log("handled", 0, log_len);
    printf(" ts-unchanged=%d", memcmp(&copy, &t500, sizeof copy) == 0);
    pthread_join(t, NULL);
}
#endif

// ---- info ----
static int info_pending;
static int info_kind; // 0 null, 1 unmapped
static void row_info(void)
{
    block(SIGUSR1);
    sigset_t set = set_of(SIGUSR1, 0);
    pthread_t t = 0;
    struct script sc = { { { 100, SIGUSR1, 0 } }, 1, 0 };
    if (info_pending) raise(SIGUSR1);
    else t = start_script(&sc);
    int64_t t0 = now_ms();
#ifdef __APPLE__
    int *dest = info_kind == 0 ? NULL : (int *)8;
    int ret = __sigwait(&set, dest);
#else
    siginfo_t *dest = info_kind == 0 ? NULL : (siginfo_t *)8;
    int ret = (int)syscall(SYS_rt_sigtimedwait, &set, dest, NULL, 8);
#endif
    int e = errno;
    printf(" ret=%d errno=%d took=%lld pending-after=%lx", ret, ret == -1 ? e : 0, (long long)(now_ms() - t0), pending_word());
    if (t) pthread_join(t, NULL);
}

// ---- set ----
static int set_kind; // 0 empty, 1 KILL and STOP
static void row_set(void)
{
    alarm(1);
    sigset_t set = set_kind == 0 ? set_of(0, 0) : set_of(SIGKILL, SIGSTOP);
#ifdef __APPLE__
    int sig = -77;
    int ret = __sigwait(&set, &sig);
    printf(" raw: ret=%d errno=%d", ret, ret == -1 ? errno : 0);
#else
    siginfo_t info;
    struct timespec t100 = { 0, 100000000 };
    long ret = syscall(SYS_rt_sigtimedwait, &set, &info, &t100, 8);
    printf(" raw-100ms: ret=%ld errno=%d", ret, ret == -1 ? errno : 0);
#endif
    fflush(stdout);
    int sig2 = -77;
    int r2 = sigwait(&set, &sig2);
    printf(" libc: ret=%d", r2);
}

int main(void)
{
    alarm(300);
    setvbuf(stdout, NULL, _IOLBF, 0);
    waiter = pthread_self();
    printf("# flavour %s, uid %d\n", FLAVOUR, (int)getuid());

    run_child("entry", row_entry);
    run_child("during to=thread", row_during_thread);
    run_child("during to=process", row_during_process);
    run_child("receiver main-blocks", row_receiver_blocks);
    run_child("receiver main-handles", row_receiver_handles);

    struct { int sig; int kind; const char *name; } unb[] = {
        { SIGUSR1, 0, "USR1 handler" },
        { SIGUSR1, 1, "USR1 SIG_DFL(terminate)" },
        { SIGURG, 1, "URG SIG_DFL(ignore)" },
        { SIGUSR1, 2, "USR1 SIG_IGN" },
    };
    for (int raw = 0; raw <= 1; raw++) {
        for (unsigned i = 0; i < sizeof unb / sizeof unb[0]; i++) {
            char label[96];
            snprintf(label, sizeof label, "unblocked %s %s", raw ? "raw" : "libc", unb[i].name);
            unblocked_sig = unb[i].sig;
            unblocked_kind = unb[i].kind;
            unblocked_raw = raw;
            run_child(label, row_unblocked);
        }
    }

    struct { int sig; int sig_ign; const char *name; } ign[] = {
        { SIGUSR1, 1, "USR1 SIG_IGN" },
        { SIGURG, 0, "URG SIG_DFL(ignore)" },
    };
    for (int before = 0; before <= 1; before++) {
        for (unsigned i = 0; i < sizeof ign / sizeof ign[0]; i++) {
            char label[96];
            snprintf(label, sizeof label, "ignored %s %s", before ? "before" : "during", ign[i].name);
            ignored_sig = ign[i].sig;
            ignored_by_sig_ign = ign[i].sig_ign;
            ignored_before = before;
            run_child(label, row_ignored);
        }
    }

    for (int raw = 0; raw <= 1; raw++) {
        for (int restart = 0; restart <= 1; restart++) {
            char label[96];
            snprintf(label, sizeof label, "outside %s flags=%s", raw ? "raw" : "libc", restart ? "SA_RESTART" : "0");
            outside_flags = restart ? SA_RESTART : 0;
            outside_raw = raw;
            run_child(label, row_outside);
        }
    }

    for (int raw = 0; raw <= 1; raw++)
        for (int first = 0; first <= 1; first++)
            rank_stop(raw, first);

#ifndef __APPLE__
    for (int raw = 0; raw <= 1; raw++) {
        for (int first = 0; first <= 1; first++) {
            char label[96];
            snprintf(label, sizeof label, "rank fifo %s %s-first", raw ? "raw" : "libc", first ? "USR2" : "USR1");
            fifo_raw = raw;
            fifo_usr2_first = first;
            run_child(label, row_rank_fifo);
        }
    }
    run_child("mask", row_mask);
    run_child("timed nothing", row_timed_nothing);
    run_child("timed pending", row_timed_pending);
    run_child("timed interrupt", row_timed_interrupt);
#endif

    for (int pending = 0; pending <= 1; pending++) {
        for (int kind = 0; kind <= 1; kind++) {
            char label[96];
            snprintf(label, sizeof label, "info raw %s %s", kind ? "unmapped" : "null", pending ? "pending-at-entry" : "arrives-during");
            info_pending = pending;
            info_kind = kind;
            run_child(label, row_info);
        }
    }

    set_kind = 0;
    run_child("set empty", row_set);
    set_kind = 1;
    run_child("set KILL,STOP", row_set);
    return 0;
}
