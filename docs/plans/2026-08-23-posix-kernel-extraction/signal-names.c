// Measures which signal names each flavour's <signal.h> defines, and their
// numbers.
//
// Two directions, so that neither a missing macro nor an alias goes unseen:
//   * every candidate name, as a macro, with the number it expands to, or
//     "undefined" (the candidate list is every signal name glibc, Darwin and
//     the BSDs define, aliases included);
//   * every number from 1 to NSIG - 1, with the C library's own abbreviation
//     for it: `sigabbrev_np(3)` on glibc, `sys_signame[]` on Darwin.
// Also SIGRTMIN and SIGRTMAX where they exist, which glibc computes at run
// time.
//
// Build it twice on Darwin, because <signal.h> there spells 7 as SIGPOLL
// under strict POSIX and as SIGEMT otherwise:
//
// Darwin: nix develop -c clang -Wall -o /tmp/sn signal-names.c && /tmp/sn
//         nix develop -c clang -Wall -D_POSIX_C_SOURCE=200809L -o /tmp/sn-posix signal-names.c && /tmp/sn-posix
// Linux:  container run --rm [--arch amd64] -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null && apt-get install -y -qq gcc libc6-dev >/dev/null && gcc -Wall -o /tmp/p /probe/signal-names.c && /tmp/p'
//
// Signal numbers are the C library's header constants, so the amd64 run (an
// x86-64 glibc and its headers, under Rosetta) is a measurement of x86-64's
// numbering even though the kernel underneath is aarch64's.
//
// Measured 2026-10-02 on Darwin 27.0.0 (arm64), and on Linux 6.18.5 with
// glibc 2.41 for aarch64 and for x86-64. The rows are in
// signal-names.{darwin-27.0-arm64,linux-glibc2.41-aarch64,linux-glibc2.41-x86_64}.txt
// beside this file, and transcribed in WoofWare.PosixKernel.Test/TestSignal.fs.
// signal-names.linux-kill-l.txt is what Debian's two shells print for
// `kill -l N`, which TestSignalAgainstHost reads on a Linux host: dash has no
// name for 16 (SIGSTKFLT) or for glibc's reserved 32 and 33.
#if !defined(__APPLE__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define NAME(s) printf("name %-10s %d\n", #s, (int)(s));
#define MISSING(s) printf("name %-10s undefined\n", #s);

int main(void)
{
    alarm(10);
    // Darwin's strict-POSIX <signal.h> defines neither NSIG nor sys_signame,
    // so that build prints the names alone.
#if defined(__APPLE__) && defined(NSIG)
    printf("# flavour darwin NSIG %d\n", NSIG);
#elif defined(__APPLE__)
    printf("# flavour darwin, strict POSIX\n");
#else
    printf("# flavour linux NSIG %d SIGRTMIN %d SIGRTMAX %d\n", NSIG, SIGRTMIN, SIGRTMAX);
#endif

#ifdef SIGHUP
    NAME(SIGHUP)
#else
    MISSING(SIGHUP)
#endif
#ifdef SIGINT
    NAME(SIGINT)
#else
    MISSING(SIGINT)
#endif
#ifdef SIGQUIT
    NAME(SIGQUIT)
#else
    MISSING(SIGQUIT)
#endif
#ifdef SIGILL
    NAME(SIGILL)
#else
    MISSING(SIGILL)
#endif
#ifdef SIGTRAP
    NAME(SIGTRAP)
#else
    MISSING(SIGTRAP)
#endif
#ifdef SIGABRT
    NAME(SIGABRT)
#else
    MISSING(SIGABRT)
#endif
#ifdef SIGIOT
    NAME(SIGIOT)
#else
    MISSING(SIGIOT)
#endif
#ifdef SIGBUS
    NAME(SIGBUS)
#else
    MISSING(SIGBUS)
#endif
#ifdef SIGEMT
    NAME(SIGEMT)
#else
    MISSING(SIGEMT)
#endif
#ifdef SIGFPE
    NAME(SIGFPE)
#else
    MISSING(SIGFPE)
#endif
#ifdef SIGKILL
    NAME(SIGKILL)
#else
    MISSING(SIGKILL)
#endif
#ifdef SIGUSR1
    NAME(SIGUSR1)
#else
    MISSING(SIGUSR1)
#endif
#ifdef SIGSEGV
    NAME(SIGSEGV)
#else
    MISSING(SIGSEGV)
#endif
#ifdef SIGUSR2
    NAME(SIGUSR2)
#else
    MISSING(SIGUSR2)
#endif
#ifdef SIGPIPE
    NAME(SIGPIPE)
#else
    MISSING(SIGPIPE)
#endif
#ifdef SIGALRM
    NAME(SIGALRM)
#else
    MISSING(SIGALRM)
#endif
#ifdef SIGTERM
    NAME(SIGTERM)
#else
    MISSING(SIGTERM)
#endif
#ifdef SIGSTKFLT
    NAME(SIGSTKFLT)
#else
    MISSING(SIGSTKFLT)
#endif
#ifdef SIGCHLD
    NAME(SIGCHLD)
#else
    MISSING(SIGCHLD)
#endif
#ifdef SIGCLD
    NAME(SIGCLD)
#else
    MISSING(SIGCLD)
#endif
#ifdef SIGCONT
    NAME(SIGCONT)
#else
    MISSING(SIGCONT)
#endif
#ifdef SIGSTOP
    NAME(SIGSTOP)
#else
    MISSING(SIGSTOP)
#endif
#ifdef SIGTSTP
    NAME(SIGTSTP)
#else
    MISSING(SIGTSTP)
#endif
#ifdef SIGTTIN
    NAME(SIGTTIN)
#else
    MISSING(SIGTTIN)
#endif
#ifdef SIGTTOU
    NAME(SIGTTOU)
#else
    MISSING(SIGTTOU)
#endif
#ifdef SIGURG
    NAME(SIGURG)
#else
    MISSING(SIGURG)
#endif
#ifdef SIGXCPU
    NAME(SIGXCPU)
#else
    MISSING(SIGXCPU)
#endif
#ifdef SIGXFSZ
    NAME(SIGXFSZ)
#else
    MISSING(SIGXFSZ)
#endif
#ifdef SIGVTALRM
    NAME(SIGVTALRM)
#else
    MISSING(SIGVTALRM)
#endif
#ifdef SIGPROF
    NAME(SIGPROF)
#else
    MISSING(SIGPROF)
#endif
#ifdef SIGWINCH
    NAME(SIGWINCH)
#else
    MISSING(SIGWINCH)
#endif
#ifdef SIGIO
    NAME(SIGIO)
#else
    MISSING(SIGIO)
#endif
#ifdef SIGPOLL
    NAME(SIGPOLL)
#else
    MISSING(SIGPOLL)
#endif
#ifdef SIGPWR
    NAME(SIGPWR)
#else
    MISSING(SIGPWR)
#endif
#ifdef SIGINFO
    NAME(SIGINFO)
#else
    MISSING(SIGINFO)
#endif
#ifdef SIGLOST
    NAME(SIGLOST)
#else
    MISSING(SIGLOST)
#endif
#ifdef SIGSYS
    NAME(SIGSYS)
#else
    MISSING(SIGSYS)
#endif
#ifdef SIGUNUSED
    NAME(SIGUNUSED)
#else
    MISSING(SIGUNUSED)
#endif
#ifdef SIGTHR
    NAME(SIGTHR)
#else
    MISSING(SIGTHR)
#endif
#ifdef SIGLIBRT
    NAME(SIGLIBRT)
#else
    MISSING(SIGLIBRT)
#endif
#ifdef SIGRTMIN
    NAME(SIGRTMIN)
#else
    MISSING(SIGRTMIN)
#endif
#ifdef SIGRTMAX
    NAME(SIGRTMAX)
#else
    MISSING(SIGRTMAX)
#endif

#ifdef NSIG
    for (int signo = 1; signo < NSIG; signo++) {
#ifdef __APPLE__
        const char *abbrev = sys_signame[signo];
#else
        const char *abbrev = sigabbrev_np(signo);
#endif
        printf("number %2d %s\n", signo, abbrev ? abbrev : "(null)");
    }
#endif
    return 0;
}
