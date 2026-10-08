// What sigpending(2) reports, and to which thread.
//
// Every row runs in a fresh forked child. A pending set is printed as the
// 64-bit word whose bit n-1 is signo n.
//
// Rows:
//   proc    - the main thread and a second thread both block USR1; one of
//             them (`sender`) sends it to the process with kill; then each
//             reports its sigpending.
//   thread  - both block USR2; one sends it to the other with pthread_kill;
//             then each reports its sigpending.
//   ignored - one thread blocks a signal that is ignored (SIG_IGN on USR1;
//             WINCH and URG at their default, which discards them; CONT
//             under SIG_IGN) and sends it to itself with kill. sigpending;
//             then the signal is unblocked and blocked again, and sigpending
//             once more.
//   cont    - CONT at its default, blocked, sent with kill: sigpending.
//   handler - a handler for USR1 with sa_mask {TERM}; inside it the thread
//             raises USR1 and kills itself with TERM, and reports
//             sigpending and its mask; then which handlers ran after it.
//
// Bounded: GUARD_MAIN and GUARD_CHILD (alarm), and every loop over a fixed
// list. Nothing waits on a signal.
//
// Darwin: clang -Wall -Wno-unused-function -Wno-deprecated-declarations -o /tmp/sigpending-scope sigpending-scope.c && /tmp/sigpending-scope
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/sigpending-scope.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container), 2026-10-08; the output is beside this file
// as sigpending-scope.darwin-27.0-uid501.txt and
// sigpending-scope.linux-6.18.5-aarch64.txt. The rows are transcribed in
// WoofWare.PosixKernel.Test/TestSigprocmask.fs.
#include "signal-probe-common.h"
#include <stdarg.h>
#include <sys/utsname.h>

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

static uint64_t mask_now(void)
{
    sigset_t s;
    memset(&s, 0, sizeof s);
    pthread_sigmask(SIG_BLOCK, NULL, &s);
    return bits_of(&s);
}

static void block_only(int sig)
{
    sigset_t s;
    sigemptyset(&s);
    if (sig) sigaddset(&s, sig);
    pthread_sigmask(SIG_SETMASK, &s, NULL);
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

static void in_child(const char *tag, void (*body)(int fd, void *arg), void *arg)
{
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
    char buf[4096];
    ssize_t total = 0, n;
    while (total < (ssize_t)sizeof buf - 1 && (n = read(fds[0], buf + total, sizeof buf - 1 - total)) > 0)
        total += n;
    buf[total] = 0;
    close(fds[0]);
    int status;
    waitpid(child, &status, 0);
    printf("%s%s end=%s%d\n", tag, buf, WIFEXITED(status) ? "exit" : "signal",
           WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
    fflush(stdout);
}

// Two threads stepping in turn: the second waits for each stage the main
// thread announces.
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

// What a two-thread row asks of the second thread.
struct pair_row {
    int sig;
    int second_sends; // 1: the second thread sends; 0: the main thread does
    int thread_directed; // 1: pthread_kill of the other thread; 0: kill
};

static pthread_t g_main;
static struct pair_row *g_row;
static volatile uint64_t g_second_pending;

static void send_it(pthread_t other)
{
    if (g_row->thread_directed) pthread_kill(other, g_row->sig);
    else kill(getpid(), g_row->sig);
}

static void *pair_second(void *arg)
{
    pthread_t self = pthread_self();
    (void)arg;
    block_only(g_row->sig);
    advance(1);
    wait_stage(2);
    if (g_row->second_sends) send_it(g_main);
    advance(3);
    wait_stage(4);
    g_second_pending = pending_now();
    (void)self;
    return NULL;
}

static void pair_body(int fd, void *arg)
{
    g_row = arg;
    g_main = pthread_self();
    block_only(g_row->sig);
    pthread_t t;
    if (pthread_create(&t, NULL, pair_second, NULL) != 0) die("pthread_create");
    wait_stage(1);
    if (!g_row->second_sends) send_it(t);
    advance(2);
    wait_stage(3);
    uint64_t main_pending = pending_now();
    advance(4);
    pthread_join(t, NULL);
    out(fd, " main=%llx second=%llx", (unsigned long long)main_pending, (unsigned long long)g_second_pending);
}

struct ignored_row { int sig; int sig_ign; };

static void ignored_body(int fd, void *arg)
{
    struct ignored_row *row = arg;
    if (row->sig_ign) signal(row->sig, SIG_IGN);
    block_only(row->sig);
    kill(getpid(), row->sig);
    out(fd, " pending=%llx", (unsigned long long)pending_now());
    block_only(0);
    block_only(row->sig);
    out(fd, " after-unblock-reblock=%llx", (unsigned long long)pending_now());
}

static void cont_body(int fd, void *arg)
{
    (void)arg;
    block_only(SIGCONT);
    kill(getpid(), SIGCONT);
    out(fd, " pending=%llx", (unsigned long long)pending_now());
}

static int g_fd;
static volatile sig_atomic_t g_depth;
static volatile sig_atomic_t g_ran[8];
static volatile sig_atomic_t g_count;

static void on_term(int s)
{
    if (g_count < 8) g_ran[g_count] = s;
    g_count++;
}

static void on_usr1(int s)
{
    if (g_count < 8) g_ran[g_count] = s;
    g_count++;
    if (g_depth++ == 0) {
        raise(SIGUSR1);
        kill(getpid(), SIGTERM);
        out(g_fd, " in-handler pending=%llx mask=%llx", (unsigned long long)pending_now(),
            (unsigned long long)mask_now());
    }
}

static void handler_body(int fd, void *arg)
{
    (void)arg;
    g_fd = fd;
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sigemptyset(&sa.sa_mask);
    sigaddset(&sa.sa_mask, SIGTERM);
    sigaction(SIGUSR1, &sa, NULL);
    sa.sa_handler = on_term;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGTERM, &sa, NULL);
    block_only(0);
    raise(SIGUSR1);
    out(fd, " after pending=%llx ran=", (unsigned long long)pending_now());
    for (int i = 0; i < g_count && i < 8; i++) out(fd, "%s%s", i ? "," : "", signame(g_ran[i]));
}

static uint64_t bit(int signo) { return (uint64_t)1 << (signo - 1); }

int main(void)
{
    GUARD_MAIN();
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, uid %d\n", u.sysname, u.release, u.machine, (int)getuid());
    printf("# flavour %s USR1 %llx USR2 %llx TERM %llx CONT %llx WINCH %llx URG %llx\n", FLAVOUR,
           (unsigned long long)bit(SIGUSR1), (unsigned long long)bit(SIGUSR2), (unsigned long long)bit(SIGTERM),
           (unsigned long long)bit(SIGCONT), (unsigned long long)bit(SIGWINCH), (unsigned long long)bit(SIGURG));

    char tag[128];
    for (int second_sends = 0; second_sends <= 1; second_sends++) {
        struct pair_row row = { SIGUSR1, second_sends, 0 };
        snprintf(tag, sizeof tag, "proc sender=%s", second_sends ? "second" : "main");
        in_child(tag, pair_body, &row);
    }
    for (int second_sends = 0; second_sends <= 1; second_sends++) {
        struct pair_row row = { SIGUSR2, second_sends, 1 };
        snprintf(tag, sizeof tag, "thread sender=%s target=%s", second_sends ? "second" : "main",
                 second_sends ? "main" : "second");
        in_child(tag, pair_body, &row);
    }

    struct ignored_row ignored[] = {
        { SIGUSR1, 1 }, { SIGWINCH, 0 }, { SIGURG, 0 }, { SIGCONT, 1 },
    };
    for (size_t i = 0; i < sizeof ignored / sizeof ignored[0]; i++) {
        snprintf(tag, sizeof tag, "ignored %s %s", signame(ignored[i].sig), ignored[i].sig_ign ? "SIG_IGN" : "SIG_DFL");
        in_child(tag, ignored_body, &ignored[i]);
    }

    in_child("cont SIG_DFL", cont_body, NULL);
    in_child("handler", handler_body, NULL);
    return 0;
}
