// What becomes of a signal pending on one thread when that thread exits.
//
// A worker blocks `sig`, directs it at itself with pthread_kill (so it is
// pending on the worker's own set, not the process's), checks sigpending sees
// it, and then leaves. The main thread then looks for it: does its handler
// ever run, and does sigpending on the main thread show it?
//
// Each scenario runs in its own forked child, so no state leaks between rounds,
// and every child has an alarm().
//
// Scenarios, for each signal in the sweep:
//   main_unblocked   main has `sig` unblocked throughout. If the kernel moved
//                    the worker's pending instance to the process, the handler
//                    runs on main once the worker is gone.
//   main_blocked     main blocks `sig` before the worker starts, joins, reads
//                    sigpending, then unblocks. If the instance moved to the
//                    process set, sigpending shows it and the unblock delivers.
//   control_process  as main_blocked, but the worker sends a *process*-directed
//                    kill(getpid(), sig) instead: this is what "moved to the
//                    process" looks like, so the two exits are distinguishable.
//
// How the worker leaves: `return` from its start routine (what every
// pthread-based runtime's thread does), or pthread_exit. Both end in the
// thread-exit syscall (SYS_exit on Linux, __bsdthread_terminate on Darwin).
//
// Each (scenario, signal, how) is repeated ROUNDS times; the output is one
// line per combination with the counts observed.
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define ROUNDS 20

static volatile sig_atomic_t g_delivered;
static pthread_t g_main;
static volatile sig_atomic_t g_delivered_on_main;

static void on_sig(int s) {
    (void)s;
    g_delivered++;
    if (pthread_equal(pthread_self(), g_main)) g_delivered_on_main++;
}

static void msleep(int ms) {
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {}
}

struct worker_args {
    int sig;
    int process_directed;
    int use_pthread_exit;
    int saw_pending; // out: sigpending on the worker showed `sig` before exit
};

static void *worker(void *p) {
    struct worker_args *a = p;
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, a->sig);
    pthread_sigmask(SIG_BLOCK, &set, NULL);
    if (a->process_directed) {
        kill(getpid(), a->sig);
    } else {
        pthread_kill(pthread_self(), a->sig);
    }
    sigset_t pend;
    sigemptyset(&pend);
    sigpending(&pend);
    a->saw_pending = sigismember(&pend, a->sig);
    if (a->use_pthread_exit) pthread_exit(NULL);
    return NULL;
}

// Returns a bitmask: 1 = worker saw it pending, 2 = main's sigpending showed it
// after the join, 4 = delivered at all, 8 = delivered on main.
static int one_round(int sig, int main_blocks, int process_directed, int use_pthread_exit) {
    int fds[2];
    if (pipe(fds) != 0) { perror("pipe"); exit(2); }
    pid_t child = fork();
    if (child < 0) { perror("fork"); exit(2); }
    if (child == 0) {
        close(fds[0]);
        alarm(5);
        g_main = pthread_self();
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_sig;
        sigemptyset(&sa.sa_mask);
        sigaction(sig, &sa, NULL);
        sigset_t set;
        sigemptyset(&set);
        sigaddset(&set, sig);
        if (main_blocks) pthread_sigmask(SIG_BLOCK, &set, NULL);
        else pthread_sigmask(SIG_UNBLOCK, &set, NULL);

        struct worker_args a = { sig, process_directed, use_pthread_exit, 0 };
        pthread_t t;
        pthread_create(&t, NULL, worker, &a);
        pthread_join(t, NULL);
        msleep(20);

        sigset_t pend;
        sigemptyset(&pend);
        sigpending(&pend);
        int main_saw = sigismember(&pend, sig);
        if (main_blocks) {
            pthread_sigmask(SIG_UNBLOCK, &set, NULL);
            msleep(20);
        }
        int r = (a.saw_pending ? 1 : 0) | (main_saw ? 2 : 0) | (g_delivered ? 4 : 0) | (g_delivered_on_main ? 8 : 0);
        write(fds[1], &r, sizeof r);
        _exit(0);
    }
    close(fds[1]);
    int r = -1;
    ssize_t n = read(fds[0], &r, sizeof r);
    close(fds[0]);
    int status = 0;
    waitpid(child, &status, 0);
    if (n != sizeof r || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        return -1;
    }
    return r;
}

int main(void) {
    alarm(120);
    struct { int sig; const char *name; } sigs[] = {
        { SIGUSR1, "SIGUSR1" },
        { SIGUSR2, "SIGUSR2" },
        { SIGTERM, "SIGTERM" },
        { SIGHUP, "SIGHUP" },
#ifdef SIGRTMIN
        { 0, "SIGRTMIN+1" },
#endif
    };
    int nsigs = (int)(sizeof sigs / sizeof sigs[0]);
#ifdef SIGRTMIN
    sigs[nsigs - 1].sig = SIGRTMIN + 1;
#endif
    struct { const char *name; int main_blocks; int process_directed; } scenarios[] = {
        { "main_unblocked", 0, 0 },
        { "main_blocked", 1, 0 },
        { "control_process", 1, 1 },
    };
    printf("# scenario\tsignal\texit\trounds\tworker_saw_pending\tmain_sigpending_after_join\tdelivered\tdelivered_on_main\tfailed_rounds\n");
    for (int s = 0; s < 3; s++) {
        for (int g = 0; g < nsigs; g++) {
            for (int how = 0; how < 2; how++) {
                int counts[4] = { 0, 0, 0, 0 };
                int failed = 0;
                for (int i = 0; i < ROUNDS; i++) {
                    int r = one_round(sigs[g].sig, scenarios[s].main_blocks, scenarios[s].process_directed, how);
                    if (r < 0) { failed++; continue; }
                    for (int b = 0; b < 4; b++) if (r & (1 << b)) counts[b]++;
                }
                printf("%s\t%s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\n", scenarios[s].name, sigs[g].name,
                       how ? "pthread_exit" : "return", ROUNDS, counts[0], counts[1], counts[2], counts[3], failed);
            }
        }
    }
    return 0;
}
