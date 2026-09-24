// Measures what kill(2) answers for each combination of a target pid and a
// signal number, and so which of the two a kernel checks first.
//
// Only a null signal or an invalid number is ever sent to anything other
// than this process or its own child, so the probe signals nobody else even
// when it runs as root.
//
// Darwin: nix develop -c clang -Wall -o kill-arguments kill-arguments.c && ./kill-arguments
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq && apt-get install -y -qq gcc libc6-dev && gcc -Wall -o /tmp/p /probe/kill-arguments.c && /tmp/p'
//
// Measured on Darwin 25.6.0 (arm64, uid 501) and Linux 6.18.5 (arm64, root in
// the container); the rows are transcribed in
// WoofWare.PosixKernel.Test/TestUnixSignal.fs, and the ones the flavours agree
// on are asserted against the real runtime by
// WoofWare.PawPrint.Test/sourcesPure/LibcKillArguments.cs.
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EINVAL: return "EINVAL";
    case ESRCH: return "ESRCH";
    case EPERM: return "EPERM";
    default: return "OTHER";
    }
}

static void row(const char *pidLabel, pid_t pid, const char *sigLabel, int sig) {
    errno = 0;
    int r = kill(pid, sig);
    int e = r == 0 ? 0 : errno;
    printf("%-26s %-22s ret=%-3d %s\n", pidLabel, sigLabel, r, en(e));
}

int main(void) {
    alarm(30);

    // A pid that certainly names no process: a child that has been reaped.
    pid_t gone = fork();
    if (gone == 0) _exit(0);
    waitpid(gone, NULL, 0);

    // A live process other than this one, which we may signal.
    pid_t other = fork();
    if (other == 0) { alarm(20); pause(); _exit(0); }

    pid_t self = getpid();
    pid_t group = getpgrp();

    struct { const char *label; pid_t pid; } pids[] = {
        { "self", self },
        { "live child", other },
        { "reaped child", gone },
        { "1", 1 },
        { "0 (own group)", 0 },
        { "-own group", -group },
        { "-reaped child (no group)", -gone },
        { "-1 (every process)", -1 },
        { "INT_MIN", INT_MIN },
    };
    struct { const char *label; int sig; } sigs[] = {
        { "0", 0 },
        { "-1", -1 },
        { "65", 65 },
        { "1000", 1000 },
        { "INT_MIN", INT_MIN },
        // Last, because on Linux they are signals, and kill the child.
        { "32", 32 },
        { "64", 64 },
    };

    printf("# NSIG %d\n", NSIG);
    for (unsigned p = 0; p < sizeof pids / sizeof pids[0]; p++) {
        for (unsigned s = 0; s < sizeof sigs / sizeof sigs[0]; s++) {
            // 32 and 64 are signals on Linux, and would be delivered: only
            // ever send a real signal to our own child.
            int sig = sigs[s].sig;
            int valid = sig > 0 && sig < NSIG;
            if (valid && pids[p].pid != other) continue;
            row(pids[p].label, pids[p].pid, sigs[s].label, sig);
        }
    }

    kill(other, SIGKILL);
    waitpid(other, NULL, 0);
    return 0;
}
