/* Shared helpers for the signal probes. Each probe is self-contained apart
 * from this header, and prints plain-text rows prefixed by a tag so the
 * output of a Linux run and a Darwin run can be diffed line-for-line.
 *
 * Safety: every probe calls GUARD_MAIN() at the top of main (the whole run
 * is SIGALRM-killed after 300 s) and GUARD_CHILD() first thing in every
 * forked child (10 s; alarms are not inherited across fork). Every fork is
 * inside a loop with a fixed bound. */
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
