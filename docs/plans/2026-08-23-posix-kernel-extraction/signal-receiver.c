// Which thread receives a process-directed signal when several could?
//
// Four threads: T0 (main, the controller) and workers T1..T3. Every thread
// has SIGUSR1's handler, which records the receiving thread's index.
// Sweep: every set B of threads blocking SIGUSR1 other than all four
// (15 sets) x every sender T0..T3 (the sender calls kill(getpid(), SIGUSR1)
// itself; it may be in B) x 8 consecutive sends, recording the receiver of
// each. The whole sweep runs 3 times in one process (rounds), so that
// state carried between sends (Linux's curr_target) shows up.
// Two thread states:
//   sleep - idle threads are blocked in read() on a command pipe (SA_RESTART,
//           so the handler does not disturb them)
//   spin  - idle threads busy-wait on a volatile command word
// The process is its own fresh fork for each state. Bounded: 2 states x 3
// rounds x 60 configs x 8 sends; each wait for a receiver is bounded by the
// child guard.
//
// Build and bounds as for signal-pick-order.c. Its rows are summarised beside
// `SignalState.receiverFor`.
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

#define NT 4
static int spin_mode;
static int cmd_pipe[NT][2], ack_pipe[2], res_pipe[2];
static volatile int cmd_word[NT];
static volatile sig_atomic_t last_receiver = -1, n_received;
static __thread int my_index;

static void handler(int s)
{
    (void)s;
    last_receiver = my_index;
    n_received++;
    if (!spin_mode) { char b = (char)my_index; write(res_pipe[1], &b, 1); }
}

static void set_block(int block)
{
    sigset_t set; sigemptyset(&set); sigaddset(&set, SIGUSR1);
    pthread_sigmask(block ? SIG_BLOCK : SIG_UNBLOCK, &set, NULL);
}

/* Commands: 'b' block, 'u' unblock, 'k' send, each acknowledged. */
static void obey(int c)
{
    if (c == 'b') set_block(1);
    else if (c == 'u') set_block(0);
    else if (c == 'k') kill(getpid(), SIGUSR1);
}

static void *worker(void *arg)
{
    my_index = (int)(intptr_t)arg;
    for (;;) {
        int c;
        if (spin_mode) { while ((c = cmd_word[my_index]) == 0) {} cmd_word[my_index] = 0; }
        else { char b; if (read(cmd_pipe[my_index][0], &b, 1) != 1) continue; c = b; }
        if (c == 'q') return NULL;
        obey(c);
        char a = 'a'; write(ack_pipe[1], &a, 1);
    }
}

static void command(int t, int c)
{
    if (t == 0) { obey(c); return; }
    if (spin_mode) cmd_word[t] = c; else { char b = (char)c; write(cmd_pipe[t][1], &b, 1); }
    char a; while (read(ack_pipe[0], &a, 1) != 1) {}
}

static int wait_receiver(int before)
{
    if (spin_mode) { while (n_received == before) {} return last_receiver; }
    char b; while (read(res_pipe[0], &b, 1) != 1) {}
    return b;
}

static void run(int spin)
{
    spin_mode = spin;
    for (int t = 0; t < NT; t++) pipe(cmd_pipe[t]);
    pipe(ack_pipe); pipe(res_pipe);
    struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = handler; sa.sa_flags = SA_RESTART;
    sigaction(SIGUSR1, &sa, NULL);
    my_index = 0;
    pthread_t th[NT];
    for (int t = 1; t < NT; t++) pthread_create(&th[t], NULL, worker, (void *)(intptr_t)t);
    sleep_ms(50);
    for (int round = 0; round < 3; round++)
        for (int B = 0; B < (1 << NT) - 1; B++)
            for (int sender = 0; sender < NT; sender++) {
                for (int t = 0; t < NT; t++) command(t, (B >> t) & 1 ? 'b' : 'u');
                printf("%s round%d blocked={", spin ? "spin " : "sleep", round);
                for (int t = 0; t < NT; t++) if ((B >> t) & 1) printf("T%d", t);
                printf("} sender=T%d receivers:", sender);
                for (int k = 0; k < 8; k++) {
                    int before = n_received;
                    command(sender, 'k');
                    printf(" T%d", wait_receiver(before));
                }
                printf("\n");
            }
    for (int t = 1; t < NT; t++) {
        if (spin_mode) cmd_word[t] = 'q'; else { char b = 'q'; write(cmd_pipe[t][1], &b, 1); }
        pthread_join(th[t], NULL);
    }
}

int main(void)
{
    GUARD_MAIN();
    setvbuf(stdout, NULL, _IOLBF, 0);
    printf("# flavour %s; online cpus %ld\n", FLAVOUR, sysconf(_SC_NPROCESSORS_ONLN));
    for (int spin = 0; spin < 2; spin++) {
        fflush(stdout);
        pid_t pid = fork();
        if (pid == 0) { alarm(60); run(spin); fflush(stdout); _exit(0); }
        int st; waitpid(pid, &st, 0);
        if (!WIFEXITED(st)) printf("# %s run died, status 0x%x\n", spin ? "spin" : "sleep", st);
    }
    return 0;
}
