// How a multi-threaded process ends, seen from inside (a surviving thread) and
// from outside (the parent's waitpid). Each scenario runs in its own forked
// child; the child reports through a pipe, the parent records the status.
//
// Scenarios:
//   main_pthread_exit_then_worker_returns   main: pthread_exit; worker: probes, then returns
//   main_pthread_exit_then_worker_exit5     main: pthread_exit; worker: probes, then exit(5)
//   main_pthread_exit_then_worker_pexit     main: pthread_exit; worker: probes, then pthread_exit
//   worker_exit9_main_blocked               worker: exit(9) while main blocks in read()
//   worker__exit9_main_blocked              worker: _exit(9) (atexit handlers must not run)
//   worker_exit_atexit_race                 worker: exit(4); main's atexit handler sleeps
//                                           while a third thread keeps writing
//   linux_main_sys_exit3_worker_sys_exit5   raw SYS_exit (one thread) on each, leader first
//   linux_main_sys_exit3_worker_returns     raw SYS_exit on the leader, worker returns
//   linux_worker_sys_exit_group6            raw SYS_exit_group from a worker
//   single_pthread_exit                     single-threaded main: pthread_exit
//
// Measured on Linux 6.18.5 aarch64 (Apple `container`, gcc:14) and Darwin 27.0.0
// arm64 on 2026-09-26. When the main thread exits first, by pthread_exit or (Linux)
// the raw SYS_exit, the process carries on: getpid is unchanged, kill(getpid(), 0)
// is 0, and a process-directed signal reaches the surviving worker. Linux keeps the
// leader as a zombie task (/proc/self/task/<pid> and /proc/self/status read Z,
// Threads: 2); on Darwin it is gone (one thread). The process's status is then the
// last thread's.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <stdarg.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#else
#include <libproc.h>
#include <sys/proc_info.h>
#endif

static int g_out; // write end of the report pipe
static char g_scenario[64];

static void say(const char *fmt, ...) {
    char buf[512];
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(buf, sizeof buf, fmt, ap);
    va_end(ap);
    write(g_out, buf, (size_t)n);
}

static uint64_t my_tid(void) {
#ifdef __linux__
    return (uint64_t)syscall(SYS_gettid);
#else
    uint64_t t = 0;
    pthread_threadid_np(NULL, &t);
    return t;
#endif
}

static void msleep(int ms) {
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {}
}

static volatile uint64_t g_usr1_receiver;
static void on_usr1(int s) { (void)s; g_usr1_receiver = my_tid(); }

static uint64_t g_leader_tid;

// What a surviving thread sees once the leader has left.
static void probe_after_leader_gone(void) {
    say("worker\ttid=%llu leader_tid=%llu getpid=%d\n", (unsigned long long)my_tid(), (unsigned long long)g_leader_tid, (int)getpid());
    say("worker\tkill(getpid(),0)=%d\n", kill(getpid(), 0));
#ifdef __linux__
    DIR *d = opendir("/proc/self/task");
    struct dirent *e;
    while (d && (e = readdir(d))) {
        if (e->d_name[0] == '.') continue;
        char p[128], line[256];
        snprintf(p, sizeof p, "/proc/self/task/%s/status", e->d_name);
        FILE *f = fopen(p, "r");
        while (f && fgets(line, sizeof line, f))
            if (!strncmp(line, "State:", 6)) { line[strcspn(line, "\n")] = 0; say("worker\ttask %s %s\n", e->d_name, line); }
        if (f) fclose(f);
    }
    if (d) closedir(d);
    FILE *f = fopen("/proc/self/status", "r");
    char line[256];
    while (f && fgets(line, sizeof line, f))
        if (!strncmp(line, "State:", 6) || !strncmp(line, "Threads:", 8)) { line[strcspn(line, "\n")] = 0; say("worker\t/proc/self/status %s\n", line); }
    if (f) fclose(f);
#else
    struct proc_taskinfo ti;
    int n = proc_pidinfo(getpid(), PROC_PIDTASKINFO, 0, &ti, sizeof ti);
    say("worker\tproc_pidinfo(TASKINFO) ret=%d pti_threadnum=%d\n", n, n > 0 ? ti.pti_threadnum : -1);
    uint64_t tids[16];
    int m = proc_pidinfo(getpid(), PROC_PIDLISTTHREADS, 0, tids, sizeof tids);
    say("worker\tproc_pidinfo(LISTTHREADS) count=%d\n", m > 0 ? m / (int)sizeof(uint64_t) : m);
#endif
    // A process-directed signal: which thread takes it now?
    signal(SIGUSR1, on_usr1);
    g_usr1_receiver = 0;
    int r = kill(getpid(), SIGUSR1);
    for (int i = 0; i < 100 && !g_usr1_receiver; i++) msleep(5);
    say("worker\tkill(getpid(),SIGUSR1)=%d receiver=%llu (self=%llu)\n", r, (unsigned long long)g_usr1_receiver, (unsigned long long)my_tid());
}

static void *worker_after_leader(void *arg) {
    long mode = (long)arg;
    msleep(200); // let the leader leave first
    probe_after_leader_gone();
    if (mode == 1) exit(5);
    if (mode == 2) pthread_exit(NULL);
#ifdef __linux__
    if (mode == 3) syscall(SYS_exit, 5);
#endif
    return NULL;
}

static void atexit_announce(void) { say("atexit\trun in tid=%llu\n", (unsigned long long)my_tid()); }

static volatile long g_ticks;
static void atexit_slow(void) {
    long before = g_ticks;
    msleep(300);
    say("atexit_slow\tticks advanced during handler by %ld\n", g_ticks - before);
}
static void *ticker(void *arg) {
    (void)arg;
    for (;;) { g_ticks++; msleep(1); }
    return NULL;
}

static void *worker_exits(void *arg) {
    long how = (long)arg;
    msleep(100);
    say("worker\tabout to end the process, how=%ld\n", how);
    if (how == 0) exit(9);
    if (how == 1) _exit(9);
    if (how == 2) exit(4);
#ifdef __linux__
    if (how == 3) syscall(SYS_exit_group, 6);
#endif
    return NULL;
}

static void run_scenario(const char *name) {
    pthread_t t;
    g_leader_tid = my_tid();
    atexit(atexit_announce);
    if (!strcmp(name, "main_pthread_exit_then_worker_returns")) {
        pthread_create(&t, NULL, worker_after_leader, (void *)0);
        pthread_exit(NULL);
    } else if (!strcmp(name, "main_pthread_exit_then_worker_exit5")) {
        pthread_create(&t, NULL, worker_after_leader, (void *)1);
        pthread_exit(NULL);
    } else if (!strcmp(name, "main_pthread_exit_then_worker_pexit")) {
        pthread_create(&t, NULL, worker_after_leader, (void *)2);
        pthread_exit(NULL);
    } else if (!strcmp(name, "worker_exit9_main_blocked") || !strcmp(name, "worker__exit9_main_blocked")
               || !strcmp(name, "linux_worker_sys_exit_group6")) {
        long how = !strcmp(name, "worker_exit9_main_blocked") ? 0 : !strcmp(name, "worker__exit9_main_blocked") ? 1 : 3;
        int p[2]; pipe(p);
        pthread_create(&t, NULL, worker_exits, (void *)how);
        char c;
        read(p[0], &c, 1); // blocks forever
        say("main\tread returned (unexpected)\n");
    } else if (!strcmp(name, "worker_exit_atexit_race")) {
        atexit(atexit_slow);
        pthread_t tk; pthread_create(&tk, NULL, ticker, NULL);
        pthread_create(&t, NULL, worker_exits, (void *)2);
        for (;;) pause();
#ifdef __linux__
    } else if (!strcmp(name, "linux_main_sys_exit3_worker_sys_exit5")) {
        pthread_create(&t, NULL, worker_after_leader, (void *)3);
        syscall(SYS_exit, 3);
    } else if (!strcmp(name, "linux_main_sys_exit3_worker_returns")) {
        pthread_create(&t, NULL, worker_after_leader, (void *)0);
        syscall(SYS_exit, 3);
#endif
    } else if (!strcmp(name, "single_pthread_exit")) {
        pthread_exit(NULL);
    }
    say("main\tfell through\n");
    exit(99);
}

int main(int argc, char **argv) {
    alarm(60);
    const char *all[] = {
        "main_pthread_exit_then_worker_returns",
        "main_pthread_exit_then_worker_exit5",
        "main_pthread_exit_then_worker_pexit",
        "worker_exit9_main_blocked",
        "worker__exit9_main_blocked",
        "worker_exit_atexit_race",
#ifdef __linux__
        "linux_main_sys_exit3_worker_sys_exit5",
        "linux_main_sys_exit3_worker_returns",
        "linux_worker_sys_exit_group6",
#endif
        "single_pthread_exit",
    };
    (void)argc; (void)argv;
    for (size_t i = 0; i < sizeof all / sizeof all[0]; i++) {
        int p[2];
        pipe(p);
        fflush(stdout);
        pid_t pid = fork();
        if (pid == 0) {
            close(p[0]);
            g_out = p[1];
            strncpy(g_scenario, all[i], sizeof g_scenario - 1);
            alarm(10);
            run_scenario(all[i]);
        }
        close(p[1]);
        // While the leader is gone and the worker still sleeps, is the child reapable?
        msleep(100);
        int st = 0;
        pid_t early = waitpid(pid, &st, WNOHANG);
        char buf[8192];
        size_t len = 0;
        ssize_t n;
        while ((n = read(p[0], buf + len, sizeof buf - 1 - len)) > 0) len += (size_t)n;
        buf[len] = 0;
        close(p[0]);
        if (early == 0) waitpid(pid, &st, 0);
        printf("=== %s\n", all[i]);
        printf("parent\twaitpid(WNOHANG) at 100ms returned %s\n", early == 0 ? "0 (still running)" : "the pid (already dead)");
        fputs(buf, stdout);
        if (WIFEXITED(st)) printf("parent\tstatus=0x%x exited(%d)\n", st, WEXITSTATUS(st));
        else if (WIFSIGNALED(st)) printf("parent\tstatus=0x%x signaled(%d)\n", st, WTERMSIG(st));
        else printf("parent\tstatus=0x%x other\n", st);
    }
    return 0;
}
