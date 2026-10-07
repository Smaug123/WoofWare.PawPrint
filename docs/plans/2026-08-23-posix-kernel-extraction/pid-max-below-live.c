// Linux only, as root with /proc/sys writable: what writing
// `kernel.pid_max` does when a live process or thread already has an id at or
// above the new value.
//
//   container run --rm --cap-add ALL -v "$PWD":/probe gcc:14 sh -c 'mount -o remount,rw /proc/sys && gcc -Wall -O0 -pthread -o /tmp/p /probe/pid-max-below-live.c && /tmp/p'
//
// The probe moves the pid counter to 4999 by starting and joining threads
// (this kernel has no `kernel.ns_last_pid`) and forks,
// so the child is process 5000; the child starts a thread that stays alive,
// which is thread 5001, and the counter's next search starts at 5002. The child
// then writes pid_max for each of 1000, 5000 (the process's own id), 5001 (the
// live thread's id), 5002 and 400, and after each write records whether it
// took, what pid_max reads back, what the process and both threads report as
// their ids, whether `kill(pid, 0)` and `tgkill(pid, tid, 0)` still find them,
// and the ids of two threads started and joined at that pid_max. Last it
// writes the default back, starts two more threads, and exits 7; the parent
// reaps it with `waitpid(5000)`.
//
// Each line is TSV: section, key, value...
//
// Measured on Linux 6.18.5 aarch64 (Apple `container`, gcc:14, 2026-10-07),
// twice, identically (pid-max-below-live.linux-6.18.5-aarch64.txt beside this
// file):
//   every write takes and reads back as written, the live process's and
//   thread's ids among them; the process stays 5000 and the thread 5001, and
//   `kill(5000, 0)` and `tgkill(5000, 5001, 0)` both succeed after each write;
//   the threads started after the writes get 300, 301, ..., 311 in turn (the
//   first search after the write of 1000 starts past it, at 5004, so it starts
//   again at 300), including after pid_max is set back to 4194304; and the
//   parent reaps the child as 5000, with its exit status 7.
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static int g_hold[2];
static int g_ask[2];
static int g_answer[2];

static long my_tid(void) { return (long)syscall(SYS_gettid); }

static int write_long(const char *path, long value) {
    FILE *f = fopen(path, "w");
    if (!f) return errno;
    int err = 0;
    if (fprintf(f, "%ld\n", value) < 0) err = errno;
    if (fclose(f) != 0 && err == 0) err = errno;
    return err;
}

static long read_long(const char *path) {
    FILE *f = fopen(path, "r");
    long v = -1;
    if (f) {
        if (fscanf(f, "%ld", &v) != 1) v = -1;
        fclose(f);
    }
    return v;
}

static const char *pid_max_path = "/proc/sys/kernel/pid_max";

static void *report_tid(void *arg) {
    *(long *)arg = my_tid();
    return NULL;
}

static long spawn_and_join(int *err_out) {
    pthread_t t;
    long tid = -1;
    int err = pthread_create(&t, NULL, report_tid, &tid);
    *err_out = err;
    if (err != 0) return -1;
    pthread_join(t, NULL);
    return tid;
}

// The held thread answers each byte on g_ask with its current tid on g_answer,
// until g_hold closes.
static void *held(void *arg) {
    (void)arg;
    for (;;) {
        char b;
        ssize_t n = read(g_ask[0], &b, 1);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return NULL;
        long tid = my_tid();
        if (write(g_answer[1], &tid, sizeof tid) != sizeof tid) return NULL;
    }
}

static long ask_held(void) {
    char b = 0;
    long tid = -1;
    if (write(g_ask[1], &b, 1) != 1) return -1;
    if (read(g_answer[0], &tid, sizeof tid) != sizeof tid) return -1;
    return tid;
}

static void observe(const char *section, long held_tid) {
    pid_t pid = getpid();
    printf("%s\tpid_max\t%ld\n", section, read_long(pid_max_path));
    printf("%s\tgetpid\t%d\n", section, (int)pid);
    printf("%s\tmain_gettid\t%ld\n", section, my_tid());
    printf("%s\theld_gettid\t%ld\n", section, ask_held());
    int r = kill(pid, 0);
    printf("%s\tkill_self_0\t%s\n", section, r == 0 ? "ok" : strerror(errno));
    r = (int)syscall(SYS_tgkill, (long)pid, held_tid, 0L);
    printf("%s\ttgkill_held_0\t%s\n", section, r == 0 ? "ok" : strerror(errno));
    for (int i = 0; i < 2; i++) {
        int err;
        long tid = spawn_and_join(&err);
        if (err != 0)
            printf("%s\tnew_thread\t%d\t%s\n", section, i, strerror(err));
        else
            printf("%s\tnew_thread\t%d\t%ld\n", section, i, tid);
    }
}

static int child(void) {
    if (pipe(g_ask) != 0 || pipe(g_answer) != 0) return 2;
    pthread_t t;
    if (pthread_create(&t, NULL, held, NULL) != 0) return 2;
    long held_tid = ask_held();
    printf("child\tpid\t%d\n", (int)getpid());
    printf("child\theld_tid\t%ld\n", held_tid);
    observe("before", held_tid);

    static const long values[] = {1000, 5000, 5001, 5002, 400};
    for (size_t i = 0; i < sizeof values / sizeof values[0]; i++) {
        int err = write_long(pid_max_path, values[i]);
        char section[32];
        snprintf(section, sizeof section, "write_%ld", values[i]);
        printf("%s\twrite\t%s\n", section, err ? strerror(err) : "ok");
        observe(section, held_tid);
    }

    int err = write_long(pid_max_path, 4194304);
    printf("restore\twrite\t%s\n", err ? strerror(err) : "ok");
    observe("restore", held_tid);

    close(g_ask[1]);
    pthread_join(t, NULL);
    return 7;
}

int main(void) {
    alarm(120);
    setvbuf(stdout, NULL, _IONBF, 0);
    printf("start\tpid_max\t%ld\n", read_long(pid_max_path));
    printf("start\tpid\t%d\n", (int)getpid());
    if (pipe(g_hold) != 0) { perror("pipe"); return 2; }

    // This kernel has no `kernel.ns_last_pid` (no CONFIG_CHECKPOINT_RESTORE),
    // so the counter is moved by starting and joining threads.
    long last = -1;
    while (last < 4999) {
        int err;
        last = spawn_and_join(&err);
        if (err != 0) { printf("start\tadvance_failed\t%s\n", strerror(err)); return 2; }
    }
    printf("start\tadvanced_to\t%ld\n", last);

    pid_t pid = fork();
    if (pid < 0) { perror("fork"); return 2; }
    if (pid == 0) _exit(child());

    int status = 0;
    pid_t reaped = waitpid(pid, &status, 0);
    printf("parent\tforked\t%d\n", (int)pid);
    printf("parent\twaitpid\t%d\t%s\n", (int)reaped, reaped < 0 ? strerror(errno) : "ok");
    if (reaped == pid && WIFEXITED(status)) printf("parent\texit_status\t%d\n", WEXITSTATUS(status));
    printf("parent\tpid_max\t%ld\n", read_long(pid_max_path));
    return 0;
}
