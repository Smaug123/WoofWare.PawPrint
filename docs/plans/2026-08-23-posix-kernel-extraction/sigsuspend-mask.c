// What sigsuspend(2) and pause(2) do to the calling thread's mask: during the
// wait, inside the handler that ends it, and after it returns; and which
// signals end the wait at all.
//
// Every row runs in a fresh forked child, so no row sees another's mask or
// dispositions. A mask is printed as the word whose bit n-1 is signo n, read
// with the raw per-thread query (Linux rt_sigprocmask, Darwin
// __pthread_sigmask), which screens nothing. Each handler records three
// things: its signal; the mask the kernel saved in its frame, which is what
// sigreturn restores (`uc_sigmask` of the SA_SIGINFO context); and the mask it
// runs under. A row prints them as `SIG(saved=..,in=..)`, in the order the
// handlers ran.
//
// The main thread suspends; a helper thread, which blocks every signal so that
// a signal sent to the process can only go to the main thread, drives the
// row: it sends a signal, waits 100 ms, and records whether the suspension had
// returned yet (`woke-after-X=0` means it was still asleep), and so on. The
// last thing it sends ends the wait. `sigs` are sent with pthread_kill to the
// main thread (`to=thread`) or with kill to the process (`to=process`).
//
// Sections:
//   during   - original {USR1, USR2, HUP}, temporary {USR2, TERM}. TERM, then
//              USR2, then USR1 sent. Which ones wake it (the mask during the
//              wait), the helper's own mask meanwhile (does the temporary
//              mask reach other threads?), and the handlers.
//   handler  - original {USR2, HUP}, temporary {INT}, USR1's sa_mask {QUIT},
//              with flags 0, SA_NODEFER and SA_RESTART: the mask inside the
//              handler (temp | sa_mask | sig?), the saved mask, the mask after
//              return, and the return value.
//   pending  - USR1 blocked and already pending (sent before the call), the
//              temporary mask {INT} unblocks it: does the call return at
//              once, having run the handler?
//   two      - ILL, USR1 and TERM blocked and pending, the temporary mask {INT}
//              unblocks all three: how many handlers run before the call
//              returns, in what order, and what each one's frame saved.
//   ignored  - original {HUP}, temporary {}. USR2 under SIG_IGN, then URG at
//              its default (ignore), then USR1 sent: do the ignored ones wake
//              it?
//   stale    - a signal that is ignored and blocked, sent before the call so
//              that it is pending (on Linux; Darwin discards it), and the
//              temporary mask {} unblocks it. The helper waits (still asleep?),
//              then installs a handler for it and waits again (was it still
//              pending to be delivered?), then sends USR1. sigpending is
//              printed before the call and after it. For URG and CONT at their
//              defaults, and USR2 and CONT under SIG_IGN.
//   term     - TERM at its default sent during the wait; and TERM blocked,
//              pending, then unblocked by the temporary mask: the child's end.
//   kill     - the temporary mask is every bit; KILL sent during the wait.
//   killstop - the temporary mask is every bit except USR1's; USR1 sent. The
//              mask inside the handler shows what of the temporary mask the
//              kernel kept: KILL and STOP, and on Linux 32 and 33 (does the C
//              library screen them, as it does for sigprocmask?), and on
//              Darwin bit 31.
//   stopcont - (the parent drives this one) the child suspends with a USR1
//              handler; the parent stops it with STOP, waits for the stop,
//              continues it with CONT, waits 200 ms, and checks whether the
//              child has returned from the call (and so exited); if not, it
//              sends USR1. With and without a handler for CONT. Not TSTP: run
//              from a harness, the child's process group is orphaned, and the
//              kernel discards TSTP sent to one (POSIX), so it never stops.
//   spread   - original {HUP}, temporary {INT}. While the main thread is asleep
//              the helper calls the C library's sigprocmask (on Darwin it
//              changes every thread's mask) with SIG_BLOCK {TERM} or
//              SIG_UNBLOCK {HUP}, then sends USR1: does the change reach the
//              temporary mask, the saved mask, or both?
//   rtsize   - (Linux) raw rt_sigsuspend with every sigsetsize but 8, which
//              would sleep: EINVAL, and screened before anything else?
//   pause-*  - the same, through pause(), where they apply: during (original
//              {USR2, HUP}; USR2, then USR1), handler, ignored, stopcont and
//              spread.
//
// Bounded: GUARD_MAIN and GUARD_CHILD (a child killed by its alarm ends
// "signal14"), and every loop over a fixed list.
//
// Darwin: clang -Wall -Wno-unused-function -Wno-deprecated-declarations -o /tmp/sigsuspend-mask sigsuspend-mask.c && /tmp/sigsuspend-mask
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/sigsuspend-mask.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container), 2026-10-08; the output is beside this file
// as sigsuspend-mask.darwin-27.0-uid501.txt and
// sigsuspend-mask.linux-6.18.5-aarch64.txt. The rows are transcribed in
// WoofWare.PosixKernel.Test/TestSigsuspend.fs.
#include "signal-probe-common.h"
#include <stdarg.h>
#include <sys/syscall.h>
#include <sys/utsname.h>
#ifndef __APPLE__
#include <ucontext.h>
#endif

#ifdef __APPLE__
typedef uint32_t raw_word;
#else
typedef uint64_t raw_word;
#endif

static uint64_t bits_of(const void *set)
{
    raw_word w;
    memcpy(&w, set, sizeof w);
    return (uint64_t)w;
}

static void set_bits(sigset_t *set, uint64_t bits)
{
    memset(set, 0, sizeof *set);
    raw_word w = (raw_word)bits;
    memcpy(set, &w, sizeof w);
}

static uint64_t bit(int signo) { return (uint64_t)1 << (signo - 1); }

static uint64_t current(void)
{
    sigset_t old;
    memset(&old, 0, sizeof old);
#ifdef __APPLE__
    syscall(SYS___pthread_sigmask, SIG_BLOCK, NULL, &old);
#else
    syscall(SYS_rt_sigprocmask, SIG_BLOCK, NULL, &old, (size_t)8);
#endif
    return bits_of(&old);
}

static void set_current(uint64_t bits)
{
    sigset_t s;
    set_bits(&s, bits);
#ifdef __APPLE__
    if (syscall(SYS___pthread_sigmask, SIG_SETMASK, &s, NULL) != 0) die("setmask");
#else
    if (syscall(SYS_rt_sigprocmask, SIG_SETMASK, &s, NULL, (size_t)8) != 0) die("setmask");
#endif
}

static uint64_t pending_now(void)
{
    sigset_t s;
    memset(&s, 0, sizeof s);
    if (sigpending(&s) != 0) die("sigpending");
    return bits_of(&s);
}

static int g_out = -1;

static void out(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
static void out(const char *fmt, ...)
{
    char line[512];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(line, sizeof line, fmt, ap);
    va_end(ap);
    write(g_out, line, strlen(line));
}

// What each handler saw, in the order they ran.
struct seen { int sig; uint64_t saved; uint64_t in; };
static struct seen g_seen[8];
static volatile sig_atomic_t g_count;

static void record(int s, siginfo_t *info, void *context)
{
    (void)info;
    ucontext_t *uc = context;
    if (g_count < 8) {
        g_seen[g_count].sig = s;
        g_seen[g_count].saved = bits_of(&uc->uc_sigmask);
        g_seen[g_count].in = current();
    }
    g_count++;
}

static void catch_with(int s, uint64_t sa_mask, int flags)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_sigaction = record;
    sa.sa_flags = SA_SIGINFO | flags;
    set_bits(&sa.sa_mask, sa_mask);
    if (sigaction(s, &sa, NULL) != 0) die("sigaction");
}

static void catch(int s) { catch_with(s, 0, 0); }

static void disposition(int s, void (*h)(int))
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = h;
    if (sigaction(s, &sa, NULL) != 0) die("sigaction");
}

static void print_seen(int upto)
{
    out(" ran=");
    if (upto == 0) out("none");
    for (int i = 0; i < upto && i < 8; i++)
        out("%s%s(saved=%llx,in=%llx)", i ? "," : "", signame(g_seen[i].sig), (unsigned long long)g_seen[i].saved,
            (unsigned long long)g_seen[i].in);
}

// The suspension, and what the main thread reports once it returns.
enum kind { SUSPEND, PAUSE };
static volatile int g_returned;
static pthread_t g_main;
static int g_to_process;

static void suspend_and_report(enum kind k, uint64_t temp)
{
    long start = now_ms();
    int ret;
    errno = 0;
    if (k == SUSPEND) {
        sigset_t s;
        set_bits(&s, temp);
        ret = sigsuspend(&s);
    } else {
        ret = pause();
    }
    int err = errno;
    int at_return = g_count;
    __atomic_store_n(&g_returned, 1, __ATOMIC_SEQ_CST);
    long took = now_ms() - start;
    out(" ret=%d errno=%d after=%llx", ret, ret == 0 ? 0 : err, (unsigned long long)current());
    out(" at-return=%d", at_return);
    print_seen(at_return);
    out(" took=%s", took < 50 ? "<50ms" : ">=50ms");
}

static void send(int s)
{
    if (g_to_process) kill(getpid(), s);
    else pthread_kill(g_main, s);
}

static int returned(void) { return __atomic_load_n(&g_returned, __ATOMIC_SEQ_CST); }

// The helper: blocks every signal, then runs the row's script.
static void (*g_script)(void);

static void *helper(void *arg)
{
    (void)arg;
    set_current(~(uint64_t)0);
    sleep_ms(100);
    g_script();
    return NULL;
}

static void with_helper(void (*script)(void), enum kind k, uint64_t temp)
{
    g_main = pthread_self();
    g_script = script;
    pthread_t t;
    if (pthread_create(&t, NULL, helper, NULL) != 0) die("pthread_create");
    suspend_and_report(k, temp);
    pthread_join(t, NULL);
}

// Send `s`, wait, and record whether the call had returned.
static void probe_send(const char *name, int s)
{
    send(s);
    sleep_ms(100);
    out(" woke-after-%s=%d", name, returned());
}

// ---- rows ----

struct row { enum kind k; int to_process; int flags; int sig; int alt; uint64_t a; uint64_t b; };

static void during_script(void)
{
    probe_send("TERM", SIGTERM);
    probe_send("USR2", SIGUSR2);
    out(" helper-mask=%llx", (unsigned long long)current());
    send(SIGUSR1);
}

static void pause_during_script(void)
{
    probe_send("USR2", SIGUSR2);
    out(" helper-mask=%llx", (unsigned long long)current());
    send(SIGUSR1);
}

static void during_body(struct row *row)
{
    int sigs[] = { SIGUSR1, SIGUSR2, SIGHUP, SIGTERM };
    for (int i = 0; i < 4; i++) catch(sigs[i]);
    if (row->k == SUSPEND) {
        set_current(bit(SIGUSR1) | bit(SIGUSR2) | bit(SIGHUP));
        with_helper(during_script, SUSPEND, bit(SIGUSR2) | bit(SIGTERM));
    } else {
        set_current(bit(SIGUSR2) | bit(SIGHUP));
        with_helper(pause_during_script, PAUSE, 0);
    }
}

static void wake_script(void) { send(SIGUSR1); }

static void handler_body(struct row *row)
{
    catch_with(SIGUSR1, bit(SIGQUIT), row->flags);
    set_current(bit(SIGUSR2) | bit(SIGHUP));
    with_helper(wake_script, row->k, bit(SIGINT));
}

static void pending_body(struct row *row)
{
    catch(SIGUSR1);
    set_current(bit(SIGUSR1) | bit(SIGHUP));
    if (row->to_process) kill(getpid(), SIGUSR1);
    else pthread_kill(pthread_self(), SIGUSR1);
    out(" pending-before=%llx", (unsigned long long)pending_now());
    suspend_and_report(SUSPEND, bit(SIGINT));
}

static void two_body(struct row *row)
{
    int sigs[] = { SIGTERM, SIGUSR1, SIGILL };
    uint64_t all = bit(SIGHUP);
    for (int i = 0; i < 3; i++) {
        catch(sigs[i]);
        all |= bit(sigs[i]);
    }
    set_current(all);
    for (int i = 0; i < 3; i++) {
        if (row->to_process) kill(getpid(), sigs[i]);
        else pthread_kill(pthread_self(), sigs[i]);
    }
    out(" pending-before=%llx", (unsigned long long)pending_now());
    suspend_and_report(SUSPEND, bit(SIGINT));
}

static void ignored_script(void)
{
    probe_send("USR2", SIGUSR2);
    probe_send("URG", SIGURG);
    send(SIGUSR1);
}

static void ignored_body(struct row *row)
{
    catch(SIGUSR1);
    disposition(SIGUSR2, SIG_IGN);
    disposition(SIGURG, SIG_DFL);
    set_current(bit(SIGHUP));
    with_helper(ignored_script, row->k, 0);
}

static int g_stale_sig;

static void stale_script(void)
{
    sleep_ms(100);
    out(" woke-before-handler=%d", returned());
    catch(g_stale_sig);
    sleep_ms(100);
    out(" woke-after-handler=%d", returned());
    send(SIGUSR1);
}

// row->sig is the stale signal; row->alt is 1 for SIG_IGN, 0 for SIG_DFL.
static void stale_body(struct row *row)
{
    catch(SIGUSR1);
    g_stale_sig = row->sig;
    disposition(row->sig, row->alt ? SIG_IGN : SIG_DFL);
    set_current(bit(row->sig) | bit(SIGHUP));
    kill(getpid(), row->sig);
    out(" pending-before=%llx", (unsigned long long)pending_now());
    with_helper(stale_script, SUSPEND, 0);
    out(" pending-after=%llx", (unsigned long long)pending_now());
}

static void term_script(void)
{
    send(SIGTERM);
    sleep_ms(200);
    out(" survived-TERM");
    send(SIGUSR1);
}

static void term_body(struct row *row)
{
    catch(SIGUSR1);
    if (row->alt) {
        // Blocked and pending before the call, which unblocks it.
        set_current(bit(SIGTERM));
        kill(getpid(), SIGTERM);
        out(" pending-before=%llx", (unsigned long long)pending_now());
        suspend_and_report(SUSPEND, 0);
    } else {
        set_current(bit(SIGHUP));
        with_helper(term_script, row->k, 0);
    }
}

static void kill_script(void)
{
    send(SIGKILL);
    sleep_ms(200);
    out(" survived-KILL");
    send(SIGUSR1);
}

static void kill_body(struct row *row)
{
    (void)row;
    catch(SIGUSR1);
    set_current(0);
    with_helper(kill_script, SUSPEND, ~(uint64_t)0);
}

static void killstop_body(struct row *row)
{
    (void)row;
    catch(SIGUSR1);
    set_current(0);
    with_helper(wake_script, SUSPEND, ~(uint64_t)0 & ~bit(SIGUSR1));
}

static void spread_script_block(void)
{
    sigset_t s;
    set_bits(&s, bit(SIGTERM));
    int ret = sigprocmask(SIG_BLOCK, &s, NULL);
    out(" helper-block-ret=%d helper-mask=%llx", ret, (unsigned long long)current());
    sleep_ms(50);
    send(SIGUSR1);
}

static void spread_script_unblock(void)
{
    sigset_t s;
    set_bits(&s, bit(SIGHUP));
    int ret = sigprocmask(SIG_UNBLOCK, &s, NULL);
    out(" helper-unblock-ret=%d helper-mask=%llx", ret, (unsigned long long)current());
    sleep_ms(50);
    send(SIGUSR1);
}

static void spread_body(struct row *row)
{
    catch(SIGUSR1);
    set_current(bit(SIGHUP));
    with_helper(row->alt ? spread_script_unblock : spread_script_block, row->k, bit(SIGINT));
}

#ifndef __APPLE__
static void rtsize_body(struct row *row)
{
    catch(SIGUSR1);
    set_current(bit(SIGHUP));
    sigset_t s;
    set_bits(&s, 0);
    errno = 0;
    int ret = (int)syscall(SYS_rt_sigsuspend, &s, (size_t)row->a);
    out(" ret=%d errno=%d after=%llx", ret, ret == 0 ? 0 : errno, (unsigned long long)current());
}
#endif

// Run `body` in a fresh child and print what it wrote, prefixed by `tag`.
static void in_child(const char *tag, void (*body)(struct row *), struct row *row)
{
    int fds[2];
    if (pipe(fds) != 0) die("pipe");
    fflush(stdout);
    pid_t child = fork();
    if (child < 0) die("fork");
    if (child == 0) {
        GUARD_CHILD();
        close(fds[0]);
        g_out = fds[1];
        g_to_process = row->to_process;
        body(row);
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

// stopcont: the parent stops and continues the child.
static void stopcont(const char *tag, enum kind k, int stop_sig, int cont_handler)
{
    int fds[2];
    if (pipe(fds) != 0) die("pipe");
    fflush(stdout);
    pid_t child = fork();
    if (child < 0) die("fork");
    if (child == 0) {
        GUARD_CHILD();
        close(fds[0]);
        g_out = fds[1];
        catch(SIGUSR1);
        if (cont_handler) catch(SIGCONT);
        set_current(bit(SIGHUP));
        suspend_and_report(k, bit(SIGINT));
        _exit(0);
    }
    close(fds[1]);
    sleep_ms(100);
    kill(child, stop_sig);
    int status;
    pid_t w = waitpid(child, &status, WUNTRACED);
    int stopped = w == child && WIFSTOPPED(status);
    kill(child, SIGCONT);
    sleep_ms(200);
    w = waitpid(child, &status, WNOHANG);
    int exited_after_cont = w == child;
    if (!exited_after_cont) {
        kill(child, SIGUSR1);
        waitpid(child, &status, 0);
    }
    char buf[4096];
    ssize_t total = 0, n;
    while (total < (ssize_t)sizeof buf - 1 && (n = read(fds[0], buf + total, sizeof buf - 1 - total)) > 0)
        total += n;
    buf[total] = 0;
    close(fds[0]);
    printf("%s stopped=%d returned-after-cont=%d%s end=%s%d\n", tag, stopped, exited_after_cont, buf,
           WIFEXITED(status) ? "exit" : "signal", WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
    fflush(stdout);
}

int main(void)
{
    GUARD_MAIN();
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, uid %d\n", u.sysname, u.release, u.machine, (int)getuid());
    printf("# flavour %s; HUP=%llx INT=%llx QUIT=%llx ILL=%llx USR1=%llx USR2=%llx TERM=%llx URG=%llx CONT=%llx\n",
           FLAVOUR, (unsigned long long)bit(SIGHUP), (unsigned long long)bit(SIGINT), (unsigned long long)bit(SIGQUIT),
           (unsigned long long)bit(SIGILL), (unsigned long long)bit(SIGUSR1), (unsigned long long)bit(SIGUSR2),
           (unsigned long long)bit(SIGTERM), (unsigned long long)bit(SIGURG), (unsigned long long)bit(SIGCONT));

    char tag[128];
    const char *to_name[] = { "thread", "process" };
    for (int p = 0; p <= 1; p++) {
        struct row row = { SUSPEND, p, 0, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "during to=%s", to_name[p]);
        in_child(tag, during_body, &row);
    }

    struct { int flags; const char *name; } flags[] = { { 0, "0" }, { SA_NODEFER, "NODEFER" }, { SA_RESTART, "RESTART" } };
    for (size_t f = 0; f < 3; f++) {
        struct row row = { SUSPEND, 0, flags[f].flags, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "handler flags=%s", flags[f].name);
        in_child(tag, handler_body, &row);
    }

    for (int p = 0; p <= 1; p++) {
        struct row row = { SUSPEND, p, 0, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "pending to=%s", to_name[p]);
        in_child(tag, pending_body, &row);
    }

    for (int p = 0; p <= 1; p++) {
        struct row row = { SUSPEND, p, 0, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "two to=%s", to_name[p]);
        in_child(tag, two_body, &row);
    }

    for (int p = 0; p <= 1; p++) {
        struct row row = { SUSPEND, p, 0, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "ignored to=%s", to_name[p]);
        in_child(tag, ignored_body, &row);
    }

    struct { int sig; int ign; } stales[] = { { SIGURG, 0 }, { SIGUSR2, 1 }, { SIGCONT, 0 }, { SIGCONT, 1 } };
    for (size_t i = 0; i < 4; i++) {
        struct row row = { SUSPEND, 0, 0, stales[i].sig, stales[i].ign, 0, 0 };
        snprintf(tag, sizeof tag, "stale %s %s", signame(stales[i].sig), stales[i].ign ? "SIG_IGN" : "SIG_DFL");
        in_child(tag, stale_body, &row);
    }

    for (int p = 0; p <= 1; p++) {
        struct row row = { SUSPEND, p, 0, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "term during to=%s", to_name[p]);
        in_child(tag, term_body, &row);
    }
    {
        struct row row = { SUSPEND, 1, 0, 0, 1, 0, 0 };
        in_child("term pending-unblocked", term_body, &row);
    }
    {
        struct row row = { SUSPEND, 0, 0, 0, 0, 0, 0 };
        in_child("kill temp=all", kill_body, &row);
        in_child("killstop temp=all-but-USR1", killstop_body, &row);
    }

    for (int cont_handler = 0; cont_handler <= 1; cont_handler++) {
        snprintf(tag, sizeof tag, "stopcont STOP cont-handler=%d", cont_handler);
        stopcont(tag, SUSPEND, SIGSTOP, cont_handler);
    }

    for (int alt = 0; alt <= 1; alt++) {
        struct row row = { SUSPEND, 0, 0, 0, alt, 0, 0 };
        snprintf(tag, sizeof tag, "spread %s", alt ? "SIG_UNBLOCK HUP" : "SIG_BLOCK TERM");
        in_child(tag, spread_body, &row);
    }

#ifndef __APPLE__
    size_t sizes[] = { 0, 1, 4, 7, 9, 16, 128 };
    for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        struct row row = { SUSPEND, 0, 0, 0, 0, sizes[i], 0 };
        snprintf(tag, sizeof tag, "rtsize %zu", sizes[i]);
        in_child(tag, rtsize_body, &row);
    }
#endif

    for (int p = 0; p <= 1; p++) {
        struct row row = { PAUSE, p, 0, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "pause-during to=%s", to_name[p]);
        in_child(tag, during_body, &row);
    }
    for (size_t f = 0; f < 3; f++) {
        struct row row = { PAUSE, 0, flags[f].flags, 0, 0, 0, 0 };
        snprintf(tag, sizeof tag, "pause-handler flags=%s", flags[f].name);
        in_child(tag, handler_body, &row);
    }
    {
        struct row row = { PAUSE, 0, 0, 0, 0, 0, 0 };
        in_child("pause-ignored to=thread", ignored_body, &row);
    }
    for (int cont_handler = 0; cont_handler <= 1; cont_handler++) {
        snprintf(tag, sizeof tag, "pause-stopcont STOP cont-handler=%d", cont_handler);
        stopcont(tag, PAUSE, SIGSTOP, cont_handler);
    }
    for (int alt = 0; alt <= 1; alt++) {
        struct row row = { PAUSE, 0, 0, 0, alt, 0, 0 };
        snprintf(tag, sizeof tag, "pause-spread %s", alt ? "SIG_UNBLOCK HUP" : "SIG_BLOCK TERM");
        in_child(tag, spread_body, &row);
    }
    return 0;
}
