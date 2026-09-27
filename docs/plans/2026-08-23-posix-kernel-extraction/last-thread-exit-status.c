// Linux only: what a parent's waitpid and waitid see when a process ends because
// its last thread made the raw thread-exit syscall (SYS_exit, not exit_group),
// over the same sweep of statuses as waitstatus.c's exit(n).
//
// Scenarios, each in its own forked child with an alarm():
//   single          the only thread calls SYS_exit(n)
//   leader_last     a worker calls SYS_exit(3) first; once it has gone, the
//                   leader calls SYS_exit(n)
//   worker_last     the leader calls SYS_exit(3) first; the worker waits until
//                   the leader is a zombie task, then calls SYS_exit(n)
//
// TSV: scenario, n, raw_status_hex, decoded, waitid_code, waitid_status
#define _GNU_SOURCE
#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static const char *code_name(int c) {
    switch (c) {
    case CLD_EXITED: return "CLD_EXITED";
    case CLD_KILLED: return "CLD_KILLED";
    case CLD_DUMPED: return "CLD_DUMPED";
    default: return "?";
    }
}

static void decode(int st, char *buf, size_t n) {
    if (WIFEXITED(st)) snprintf(buf, n, "exited(%d)", WEXITSTATUS(st));
    else if (WIFSIGNALED(st)) snprintf(buf, n, "signaled(%d)", WTERMSIG(st));
    else snprintf(buf, n, "other");
}

static void msleep(int ms) {
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {}
}

static _Noreturn void raw_thread_exit(long n) {
    for (;;) syscall(SYS_exit, (int)n);
}

static long g_n;
static pid_t g_pid;

static void *worker_exits_first(void *p) {
    (void)p;
    raw_thread_exit(3);
}

// Waits (bounded) until the leader's /proc/self/task/<pid>/stat says Z.
static void *worker_exits_last(void *p) {
    (void)p;
    char path[64];
    snprintf(path, sizeof path, "/proc/self/task/%d/stat", (int)g_pid);
    for (int i = 0; i < 500; i++) {
        FILE *f = fopen(path, "r");
        char state = '?';
        if (f) {
            int ignored;
            char comm[64];
            if (fscanf(f, "%d %63s %c", &ignored, comm, &state) != 3) state = '?';
            fclose(f);
        }
        if (state == 'Z') break;
        msleep(2);
    }
    raw_thread_exit(g_n);
}

static void child(const char *scenario, long n) {
    alarm(5);
    g_n = n;
    g_pid = getpid();
    if (strcmp(scenario, "single") == 0) {
        raw_thread_exit(n);
    } else if (strcmp(scenario, "leader_last") == 0) {
        pthread_t t;
        pthread_create(&t, NULL, worker_exits_first, NULL);
        // The worker's SYS_exit does not wake a joiner (it is not pthread_exit),
        // so wait for it to leave the task list instead.
        for (int i = 0; i < 500; i++) {
            int threads = 0;
            FILE *f = fopen("/proc/self/status", "r");
            char line[256];
            while (f && fgets(line, sizeof line, f)) {
                if (sscanf(line, "Threads: %d", &threads) == 1) break;
            }
            if (f) fclose(f);
            if (threads == 1) break;
            msleep(2);
        }
        raw_thread_exit(n);
    } else {
        pthread_t t;
        pthread_create(&t, NULL, worker_exits_last, NULL);
        raw_thread_exit(3);
    }
}

static void observe(const char *scenario, long n) {
    pid_t pid = fork();
    if (pid < 0) { perror("fork"); exit(2); }
    if (pid == 0) { child(scenario, n); _exit(200); }
    siginfo_t si;
    memset(&si, 0, sizeof si);
    int r = waitid(P_PID, (id_t)pid, &si, WEXITED | WNOWAIT);
    int wcode = r == 0 ? si.si_code : -errno;
    int wstatus = r == 0 ? si.si_status : 0;
    int st = 0;
    waitpid(pid, &st, 0);
    char d[64];
    decode(st, d, sizeof d);
    printf("%s\t%ld\t0x%x\t%s\t%s\t%d\n", scenario, n, (unsigned)st, d, code_name(wcode), wstatus);
    fflush(stdout);
}

int main(void) {
    alarm(120);
    printf("scenario\tn\traw_status\tdecoded\twaitid_code\twaitid_status\n");
    long ns[] = { 0, 1, 5, 7, 127, 128, 255, 256, 257, 263, 511, 65535, 65536 + 7, -1, -256, INT_MAX, INT_MIN };
    const char *scenarios[] = { "single", "leader_last", "worker_last" };
    for (size_t s = 0; s < 3; s++)
        for (size_t i = 0; i < sizeof ns / sizeof ns[0]; i++) observe(scenarios[s], ns[i]);
    return 0;
}
