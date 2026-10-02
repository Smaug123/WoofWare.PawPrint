// What raise(3) does: its answer for every signal number, which thread takes
// the signal, and whether the handler has run by the time it returns.
//
// raise(sig) is pthread_kill(pthread_self(), sig) in POSIX. Four parts, each
// row its own forked child:
//
//   args    - every number from -1 to HIGHEST_SIGNO + 2, then 65, 66, 128,
//             1000, INT_MIN and INT_MAX, through raise and through
//             pthread_kill(pthread_self()), with every signal blocked: the
//             answer, errno, whether the signal is then pending, and how the
//             child ended. (glibc's sigprocmask will not block its own 32 and
//             33, and nothing blocks SIGKILL or SIGSTOP, so those rows show
//             the signal acting.)
//   handler - every signal sigaction accepts, caught, raised by the main
//             thread and by a second thread while the main thread waits in
//             pthread_join: the answer, how many times the handler had run
//             when raise returned, on which thread it ran, and its si_code.
//   blocked - the same signals, raised by a thread that blocks them while the
//             other thread does not: whether the other thread took it, whether
//             it is pending, and where it went once the raiser unblocked it.
//   pair    - the same signals, sent once to the process (kill) and once to
//             one thread (raise, or pthread_kill of the other thread), by
//             either thread, while every thread blocks it; then each thread
//             unblocks in turn, main first. Run with one thread and with two,
//             to see whether the two instances are kept apart or merged.
//
// Bounded: GUARD_MAIN and GUARD_CHILD, every loop over a fixed list, and every
// wait for a thread a bounded sleep. The child's guard is not armed in a row
// that catches SIGALRM.
//
// Darwin: nix develop -c clang -Wall -Wno-unused-function -o raise-sweep raise-sweep.c && ./raise-sweep
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq && apt-get install -y -qq gcc libc6-dev && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/raise-sweep.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container); the output is beside this file as
// raise-sweep.darwin-27.0-uid501.txt and raise-sweep.linux-6.18.5-aarch64.txt.
// The rows are transcribed beside `UnixSignal.pthreadKill` and
// `SignalState.generate`, and in WoofWare.PosixKernel.Test/TestPthreadKill.fs.
#include "signal-probe-common.h"
#include <limits.h>

static pthread_t main_thread;
static volatile int count_main, count_worker, last_code;

static void on_signal(int s, siginfo_t *info, void *ctx)
{
    (void)s; (void)ctx;
    if (pthread_equal(pthread_self(), main_thread)) __atomic_add_fetch(&count_main, 1, __ATOMIC_SEQ_CST);
    else __atomic_add_fetch(&count_worker, 1, __ATOMIC_SEQ_CST);
    last_code = info->si_code;
}

static void catch_signal(int s)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_sigaction = on_signal;
    sa.sa_flags = SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    if (sigaction(s, &sa, NULL) != 0) die("sigaction");
}

static int total(void) { return __atomic_load_n(&count_main, __ATOMIC_SEQ_CST) + __atomic_load_n(&count_worker, __ATOMIC_SEQ_CST); }

static void block(int s, int how)
{
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, s);
    if (pthread_sigmask(how, &set, NULL) != 0) die("pthread_sigmask");
}

static int is_pending(int s)
{
    sigset_t set;
    sigemptyset(&set);
    if (sigpending(&set) != 0) die("sigpending");
    return sigismember(&set, s);
}

static const char *status_text(int status)
{
    static char buf[48];
    if (WIFEXITED(status)) snprintf(buf, sizeof buf, "exit%d", WEXITSTATUS(status));
    else if (WIFSIGNALED(status)) snprintf(buf, sizeof buf, "killed%d", WTERMSIG(status));
    else if (WIFSTOPPED(status)) snprintf(buf, sizeof buf, "stopped%d", WSTOPSIG(status));
    else snprintf(buf, sizeof buf, "status%x", status);
    return buf;
}

// Run `body` in a fresh child, which writes its row into `out`; print the row
// and how the child ended.
static void in_child(const char *tag, void (*body)(int fd, int a, int b), int a, int b, int guard)
{
    int fds[2];
    if (pipe(fds) != 0) die("pipe");
    fflush(stdout);
    pid_t child = fork();
    if (child < 0) die("fork");
    if (child == 0) {
        if (guard) GUARD_CHILD();
        close(fds[0]);
        main_thread = pthread_self();
        body(fds[1], a, b);
        _exit(0);
    }
    close(fds[1]);
    // Wait first: a stopped child keeps the pipe open, and its row, if any,
    // is already in the pipe.
    int status;
    if (waitpid(child, &status, WUNTRACED) != child) die("waitpid");
    if (WIFSTOPPED(status)) {
        kill(child, SIGKILL);
        int ignored;
        waitpid(child, &ignored, 0);
    }
    char row[256];
    ssize_t n = 0, got;
    while (n < (ssize_t)sizeof row - 1 && (got = read(fds[0], row + n, sizeof row - 1 - n)) > 0) n += got;
    row[n] = 0;
    close(fds[0]);
    printf("%s a=%d b=%d %s end=%s\n", tag, a, b, n > 0 ? row : "(no row)", status_text(status));
}

static void put(int fd, const char *text)
{
    size_t len = strlen(text);
    if (write(fd, text, len) != (ssize_t)len) _exit(98);
}

/* ---- args ---- */

static void args_body(int fd, int sig, int viaPthreadKill)
{
    sigset_t all;
    sigfillset(&all);
    if (sigprocmask(SIG_SETMASK, &all, NULL) != 0) die("sigprocmask");
    errno = 0;
    int r, e;
    if (viaPthreadKill) { r = pthread_kill(pthread_self(), sig); e = r; }
    else { r = raise(sig); e = r == 0 ? 0 : errno; }
    int pending = sig > 0 && sig <= HIGHEST_SIGNO ? is_pending(sig) : -1;
    char row[128];
    snprintf(row, sizeof row, "call=%s sig=%d ret=%d errno=%d pending=%d", viaPthreadKill ? "pthread_kill" : "raise", sig, r, e, pending);
    put(fd, row);
}

/* ---- handler and blocked ---- */

static volatile int raiser_done;
static volatile int row_ret, row_before, row_while_blocked, row_pending;
static int row_sig;

static void raise_and_count(void)
{
    int before = total();
    errno = 0;
    int r = raise(row_sig);
    row_ret = r == 0 ? 0 : errno;
    row_before = total() - before;
}

static void *handler_worker(void *arg)
{
    (void)arg;
    raise_and_count();
    return NULL;
}

static void handler_body(int fd, int sig, int byWorker)
{
    row_sig = sig;
    catch_signal(sig);
    if (byWorker) {
        pthread_t t;
        if (pthread_create(&t, NULL, handler_worker, NULL) != 0) die("pthread_create");
        pthread_join(t, NULL);
    } else {
        raise_and_count();
    }
    char row[160];
    snprintf(row, sizeof row, "raiser=%s sig=%d ret=%d ran_before_return=%d main=%d worker=%d si_code=%d",
             byWorker ? "worker" : "main", sig, row_ret, row_before, count_main, count_worker, last_code);
    put(fd, row);
}

// The raiser blocks the signal and raises it; the bystander leaves it
// unblocked, and naps until the raiser is done.
static void raise_blocked(void)
{
    block(row_sig, SIG_BLOCK);
    errno = 0;
    int r = raise(row_sig);
    row_ret = r == 0 ? 0 : errno;
    sleep_ms(50);
    row_while_blocked = total();
    row_pending = is_pending(row_sig);
    block(row_sig, SIG_UNBLOCK);
    sleep_ms(20);
    raiser_done = 1;
}

static void bystand(void)
{
    for (int i = 0; i < 400 && !raiser_done; i++) sleep_ms(5);
}

static void *blocked_worker(void *byWorker)
{
    if (byWorker) raise_blocked();
    else bystand();
    return NULL;
}

static void blocked_body(int fd, int sig, int byWorker)
{
    row_sig = sig;
    catch_signal(sig);
    pthread_t t;
    if (pthread_create(&t, NULL, blocked_worker, byWorker ? (void *)1 : NULL) != 0) die("pthread_create");
    sleep_ms(20);
    if (byWorker) bystand();
    else raise_blocked();
    pthread_join(t, NULL);
    char row[192];
    snprintf(row, sizeof row, "raiser=%s sig=%d ret=%d taken_while_blocked=%d pending=%d main=%d worker=%d",
             byWorker ? "worker" : "main", sig, row_ret, row_while_blocked, row_pending, count_main, count_worker);
    put(fd, row);
}

/* ---- pair ---- */

// Shapes, every thread blocking the signal throughout the sends:
//   0 one thread: kill, then raise
//   1 one thread: raise, then kill
//   2 two threads: main kills, then raises
//   3 two threads: main kills, then pthread_kills the worker
//   4 two threads: the worker kills, then main raises
//   5 two threads: the worker kills, then raises
// Then main unblocks, and then the worker.
static volatile int worker_blocked, worker_command, worker_obeyed, worker_unblock;
static volatile int worker_ret1, worker_ret2;

static void *pair_worker(void *arg)
{
    (void)arg;
    block(row_sig, SIG_BLOCK);
    worker_blocked = 1;
    for (int i = 0; i < 400 && !worker_command && !worker_unblock; i++) sleep_ms(5);
    if (worker_command) {
        worker_ret1 = kill(getpid(), row_sig);
        if (worker_command == 2) worker_ret2 = raise(row_sig);
        worker_obeyed = 1;
    }
    for (int i = 0; i < 400 && !worker_unblock; i++) sleep_ms(5);
    block(row_sig, SIG_UNBLOCK);
    sleep_ms(20);
    return NULL;
}

// Have the worker kill (command 1), or kill then raise (command 2).
static void worker_does(int command)
{
    worker_command = command;
    for (int i = 0; i < 400 && !worker_obeyed; i++) sleep_ms(5);
}

static void pair_body(int fd, int sig, int shape)
{
    row_sig = sig;
    catch_signal(sig);
    block(sig, SIG_BLOCK);
    pthread_t t;
    int threads = shape >= 2 ? 2 : 1;
    if (threads == 2) {
        if (pthread_create(&t, NULL, pair_worker, NULL) != 0) die("pthread_create");
        for (int i = 0; i < 400 && !worker_blocked; i++) sleep_ms(5);
    }
    int r1 = 0, r2 = 0;
    switch (shape) {
    case 0: r1 = kill(getpid(), sig); r2 = raise(sig); break;
    case 1: r2 = raise(sig); r1 = kill(getpid(), sig); break;
    case 2: r1 = kill(getpid(), sig); r2 = raise(sig); break;
    case 3: r1 = kill(getpid(), sig); r2 = pthread_kill(t, sig); break;
    case 4: worker_does(1); r1 = worker_ret1; r2 = raise(sig); break;
    default: worker_does(2); r1 = worker_ret1; r2 = worker_ret2; break;
    }
    block(sig, SIG_UNBLOCK);
    sleep_ms(20);
    int main_first = count_main, worker_first = count_worker;
    if (threads == 2) {
        worker_unblock = 1;
        pthread_join(t, NULL);
    }
    static const char *shapes[] = {
        "1thread.kill,raise", "1thread.raise,kill", "2thread.kill,raise", "2thread.kill,pthread_kill(worker)",
        "2thread.worker_kill,raise", "2thread.worker_kill,worker_raise",
    };
    char row[192];
    snprintf(row, sizeof row, "shape=%s sig=%d ret=%d,%d after_main_unblock=%d/%d after_worker_unblock=%d/%d",
             shapes[shape], sig, r1, r2, main_first, worker_first, count_main, count_worker);
    put(fd, row);
}

int main(void)
{
    GUARD_MAIN();
    setvbuf(stdout, NULL, _IOLBF, 0);
    printf("# flavour %s NSIG %d\n", FLAVOUR, NSIG);

    int numbers[HIGHEST_SIGNO + 16];
    int n = 0;
    for (int s = -1; s <= HIGHEST_SIGNO + 2; s++) numbers[n++] = s;
    int extras[] = { 65, 66, 128, 1000, INT_MIN, INT_MAX };
    for (unsigned i = 0; i < sizeof extras / sizeof extras[0]; i++) {
        int dup = 0;
        for (int j = 0; j < n; j++) dup |= numbers[j] == extras[i];
        if (!dup) numbers[n++] = extras[i];
    }
    for (int i = 0; i < n; i++)
        for (int via = 0; via < 2; via++)
            in_child("args", args_body, numbers[i], via, 1);

    int catchable[128];
    int c = all_catchable(catchable);
    for (int i = 0; i < c; i++)
        for (int w = 0; w < 2; w++)
            in_child("handler", handler_body, catchable[i], w, catchable[i] != SIGALRM);
    for (int i = 0; i < c; i++)
        for (int w = 0; w < 2; w++)
            in_child("blocked", blocked_body, catchable[i], w, catchable[i] != SIGALRM);
    for (int i = 0; i < c; i++)
        for (int shape = 0; shape < 6; shape++)
            in_child("pair", pair_body, catchable[i], shape, catchable[i] != SIGALRM);
    return 0;
}
