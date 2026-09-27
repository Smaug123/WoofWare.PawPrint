// Linux and Darwin: which signal mask a new thread starts with, and whether a
// signal pending on its creator alone is pending on it.
//
// For each mask the creator can have (empty, SIGUSR1, SIGUSR1+SIGTERM, and with
// a SIGUSR2 pending on the creator alone), the creator makes a thread, which
// reports its own mask and its own pending set, then exits.
//
// TSV: case, signal, blocked_in_child, pending_in_child
//
// Measured on Darwin 27.0.0 arm64 and on Linux 6.18.5 aarch64 and x86-64 (Apple
// `container`, gcc:14, the latter under Rosetta), 2026-09-27; all three agree.
// The new thread's mask is exactly its creator's in every case, and a signal
// pending on the creator alone is not pending on the new thread.
#define _GNU_SOURCE
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

static const int probe_signals[] = { SIGHUP, SIGUSR1, SIGUSR2, SIGTERM };
static const char *const probe_names[] = { "SIGHUP", "SIGUSR1", "SIGUSR2", "SIGTERM" };
enum { N = sizeof probe_signals / sizeof probe_signals[0] };

static const char *g_case;

static void *report(void *arg) {
    (void)arg;
    sigset_t blocked, pending;
    pthread_sigmask(SIG_BLOCK, NULL, &blocked);
    sigpending(&pending);
    for (int i = 0; i < N; i++)
        printf("%s\t%s\t%d\t%d\n", g_case, probe_names[i], sigismember(&blocked, probe_signals[i]),
               sigismember(&pending, probe_signals[i]));
    return NULL;
}

static void run(const char *name, const int *block, int nblock, int pend_on_self) {
    sigset_t set, old;
    sigemptyset(&set);
    for (int i = 0; i < nblock; i++) sigaddset(&set, block[i]);
    pthread_sigmask(SIG_SETMASK, &set, &old);
    if (pend_on_self) pthread_kill(pthread_self(), SIGUSR2);
    g_case = name;
    pthread_t t;
    pthread_create(&t, NULL, report, NULL);
    pthread_join(t, NULL);
    // Drain anything left pending on this thread before the next case, with
    // every probe signal blocked so the drain cannot deliver it.
    sigset_t all;
    sigemptyset(&all);
    for (int i = 0; i < N; i++) sigaddset(&all, probe_signals[i]);
    pthread_sigmask(SIG_SETMASK, &all, NULL);
    sigset_t pending;
    sigpending(&pending);
    for (int i = 0; i < N; i++) {
        if (sigismember(&pending, probe_signals[i])) {
            sigset_t one;
            sigemptyset(&one);
            sigaddset(&one, probe_signals[i]);
            int got;
            sigwait(&one, &got);
        }
    }
    pthread_sigmask(SIG_SETMASK, &old, NULL);
}

int main(void) {
    alarm(20);
    setvbuf(stdout, NULL, _IONBF, 0);
    static const int usr1[] = { SIGUSR1 };
    static const int usr1_term[] = { SIGUSR1, SIGTERM };
    static const int usr2[] = { SIGUSR2 };
    run("empty", NULL, 0, 0);
    run("usr1", usr1, 1, 0);
    run("usr1_term", usr1_term, 2, 0);
    run("usr2_pending_on_creator", usr2, 1, 1);
    return 0;
}
