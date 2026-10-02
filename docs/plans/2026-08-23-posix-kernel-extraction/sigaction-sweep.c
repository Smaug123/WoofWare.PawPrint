// What sigaction(2) answers for every signal number: a query (a NULL new
// action), SIG_DFL, SIG_IGN and a handler, and the old action it hands back.
//
// Each row is one number, in a fresh forked child, taken through the same
// sequence of calls:
//
//   q0  - query: the disposition the child started with
//   h   - install a handler H, whose sa_mask is every number from 1 to the
//         highest signal, set bit by bit (glibc's sigaddset and sigfillset
//         screen 32 and 33, so on Linux the bits are written directly)
//   q1  - query: is H installed, and does its mask still hold SIGKILL and
//         SIGSTOP (and, on Linux, 32 and 33)?
//   i   - install SIG_IGN; the old action should be H
//   d   - install SIG_DFL; the old action should be SIG_IGN
//   q2  - query: SIG_DFL?
//
// Each call prints its return value, errno, and the old action it reported
// (DFL, IGN, H, or another pointer). Every number from -1 to HIGHEST_SIGNO + 2
// is swept, then 65, 66, 128, 1000, INT_MIN and INT_MAX, through the C
// library's sigaction. Linux sweeps the same numbers again through the raw
// rt_sigaction(2) syscall, which glibc does not screen; Darwin sweeps them
// again through the raw sigaction syscall (SYS_sigaction), querying only,
// since installing an action that way needs the C library's signal
// trampoline.
//
// What setting a disposition does to a pending instance of the signal is
// signal-disposition-table.c's part "tr", which this does not repeat.
//
// Bounded: GUARD_MAIN and GUARD_CHILD, and every loop over a fixed list.
// No signal is generated, so nothing leaves the child.
//
// Darwin: nix develop -c clang -Wall -Wno-unused-function -Wno-deprecated-declarations -o sigaction-sweep sigaction-sweep.c && ./sigaction-sweep
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq && apt-get install -y -qq gcc libc6-dev && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/sigaction-sweep.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container); the output is beside this file as
// sigaction-sweep.darwin-27.0-uid501.txt and
// sigaction-sweep.linux-6.18.5-aarch64.txt. The rows are transcribed beside
// `UnixSignal.sigaction` and in WoofWare.PosixKernel.Test/TestSigaction.fs.
#include "signal-probe-common.h"
#include <limits.h>
#include <sys/syscall.h>
#include <sys/utsname.h>

static void h(int s) { (void)s; }

static const char *describe(void (*handler)(int))
{
    if (handler == SIG_DFL) return "DFL";
    if (handler == SIG_IGN) return "IGN";
    if (handler == h) return "H";
    return "other";
}

#ifndef __APPLE__
// The kernel's own struct sigaction for rt_sigaction on aarch64 and x86-64:
// handler, flags, restorer, then a 64-bit mask.
struct kernel_sigaction {
    void (*handler)(int);
    unsigned long flags;
    void (*restorer)(void);
    uint64_t mask;
};

static uint64_t full_mask(void)
{
    return ~(uint64_t)0;
}
#else
// xnu's struct __sigaction, which the raw syscall takes as the new action.
struct darwin_raw_sigaction {
    void (*handler)(int);
    void (*tramp)(void);
    sigset_t mask;
    int flags;
};
#endif

// Which call path a row takes.
enum path { LIBC, RAW };

// One call: install `next` (NULL to query) and report the old action.
// `next_kind` names what is being installed, for the output.
static void step(int fd, enum path p, int sig, const char *label, void (*next)(int), int query)
{
    char line[256];
    int ret, err;
    const char *old = "-";
    int masked_kill = -1, masked_stop = -1, masked_32 = -1, masked_33 = -1;

    if (p == LIBC) {
        struct sigaction sa, oldsa;
        memset(&sa, 0, sizeof sa);
        memset(&oldsa, 0, sizeof oldsa);
        sa.sa_handler = next;
        // Every number to the highest signal, written straight into the set
        // where the C library would screen it.
#ifdef __APPLE__
        sa.sa_mask = ~(sigset_t)0;
#else
        memset(&sa.sa_mask, 0xff, sizeof sa.sa_mask);
#endif
        errno = 0;
        ret = sigaction(sig, query ? NULL : &sa, &oldsa);
        err = errno;
        if (ret == 0) {
            old = describe(oldsa.sa_handler);
            if (oldsa.sa_handler == h) {
                masked_kill = sigismember(&oldsa.sa_mask, SIGKILL);
                masked_stop = sigismember(&oldsa.sa_mask, SIGSTOP);
#ifndef __APPLE__
                // glibc's sigismember screens nothing, so read the bits.
                const unsigned long *bits = (const unsigned long *)&oldsa.sa_mask;
                masked_32 = (int)((bits[0] >> 31) & 1);
                masked_33 = (int)((bits[0] >> 32) & 1);
#endif
            }
        }
    } else {
#ifndef __APPLE__
        struct kernel_sigaction ka, oldka;
        memset(&ka, 0, sizeof ka);
        memset(&oldka, 0, sizeof oldka);
        ka.handler = next;
        ka.mask = full_mask();
        errno = 0;
        ret = (int)syscall(SYS_rt_sigaction, sig, query ? NULL : &ka, &oldka, (size_t)8);
        err = errno;
        if (ret == 0) {
            old = describe(oldka.handler);
            if (oldka.handler == h) {
                masked_kill = (int)((oldka.mask >> (SIGKILL - 1)) & 1);
                masked_stop = (int)((oldka.mask >> (SIGSTOP - 1)) & 1);
                masked_32 = (int)((oldka.mask >> 31) & 1);
                masked_33 = (int)((oldka.mask >> 32) & 1);
            }
        }
#else
        if (!query) {
            // Installing through the raw call needs the C library's
            // trampoline; only queries are swept this way.
            return;
        }
        struct sigaction oldsa;
        memset(&oldsa, 0, sizeof oldsa);
        errno = 0;
        ret = syscall(SYS_sigaction, sig, NULL, &oldsa);
        err = errno;
        if (ret == 0) old = describe(oldsa.sa_handler);
#endif
    }

    snprintf(line, sizeof line, " %s=%d/%d/%s", label, ret, ret == 0 ? 0 : err, old);
    write(fd, line, strlen(line));
    if (masked_kill >= 0) {
        snprintf(line, sizeof line, " mask(KILL,STOP,32,33)=%d%d%d%d", masked_kill, masked_stop, masked_32, masked_33);
        write(fd, line, strlen(line));
    }
}

static void row(enum path p, int sig)
{
    int fds[2];
    if (pipe(fds) != 0) { perror("pipe"); exit(1); }
    pid_t child = fork();
    if (child < 0) { perror("fork"); exit(1); }
    if (child == 0) {
        GUARD_CHILD();
        close(fds[0]);
        step(fds[1], p, sig, "q0", NULL, 1);
        step(fds[1], p, sig, "h", h, 0);
        step(fds[1], p, sig, "q1", NULL, 1);
        step(fds[1], p, sig, "i", SIG_IGN, 0);
        step(fds[1], p, sig, "d", SIG_DFL, 0);
        step(fds[1], p, sig, "q2", NULL, 1);
        _exit(0);
    }
    close(fds[1]);
    char buf[2048];
    ssize_t total = 0, n;
    while (total < (ssize_t)sizeof buf - 1 && (n = read(fds[0], buf + total, sizeof buf - 1 - total)) > 0)
        total += n;
    buf[total] = 0;
    close(fds[0]);
    int status;
    waitpid(child, &status, 0);
    printf("%s sig=%d%s end=%s%d\n", p == LIBC ? "libc" : "raw", sig, buf,
           WIFEXITED(status) ? "exit" : "signal", WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
    fflush(stdout);
}

int main(void)
{
    GUARD_MAIN();
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, uid %d\n", u.sysname, u.release, u.machine, (int)getuid());
    printf("# flavour %s NSIG %d\n", FLAVOUR, NSIG);

    int extra[] = { 65, 66, 128, 1000, INT_MIN, INT_MAX };
    for (int p = LIBC; p <= RAW; p++) {
        for (int sig = -1; sig <= HIGHEST_SIGNO + 2; sig++) row((enum path)p, sig);
        for (size_t i = 0; i < sizeof extra / sizeof extra[0]; i++) row((enum path)p, extra[i]);
    }
    return 0;
}
