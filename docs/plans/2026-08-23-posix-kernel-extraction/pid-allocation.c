// Linux only, as root with /proc/sys writable: how the kernel hands out thread
// ids once `kernel.pid_max` is small enough to reach.
//
//   container run --rm --cap-add ALL -v "$PWD":/probe gcc:14 sh -c 'mount -o remount,rw /proc/sys && gcc -Wall -O0 -pthread -o /tmp/p /probe/pid-allocation.c && /tmp/p'
//
// Sections, each TSV (section, key, value...):
//   bounds    every value written to pid_max in a sweep around both ends of
//             its range, and whether the write took (errno otherwise).
//   skip      pid_max 1000: create-and-join until the ids wrap; then hold the
//             next id alive, join one, hold the one after that, and record the
//             ids the next wrap hands out.
//   exhaust   pid_max 400, with those two still held: create threads that stay
//             alive until pthread_create fails; record how many, the errno, and
//             the ids.
//
// Every thread that stays alive blocks in read() on a pipe the main thread
// closes at the end, and the whole probe runs under alarm(120).
//
// Measured on Linux 6.18.5 aarch64 (Apple `container`, gcc:14, 2026-09-27),
// twice, identically:
//   bounds    pid_max accepts exactly 301..4194304; every other value swept is
//             EINVAL and leaves it as it was.
//   skip      the ids wrap from 999 to 300. With 301 and 303 held alive, the next
//             wrap hands out 300, 302, 304, 305, 306: every live id is skipped.
//   exhaust   with the cursor at 307 and pid_max lowered to 400, 98 threads start
//             (307..399, then 300, 302, 304, 305, 306) and the next
//             pthread_create fails with EAGAIN.
// Under `--arch amd64` the bounds sweep agrees, and Rosetta then aborts at the
// first wrap ("duplicate thread entry found"): it keys its own thread table by
// tid, so the wrap is not measurable on x86-64 there.
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

static int g_hold[2];

static long my_tid(void) { return (long)syscall(SYS_gettid); }

static int write_pid_max(long value) {
    FILE *f = fopen("/proc/sys/kernel/pid_max", "w");
    if (!f) return errno;
    int err = 0;
    if (fprintf(f, "%ld\n", value) < 0) err = errno;
    if (fclose(f) != 0 && err == 0) err = errno;
    return err;
}

static long read_pid_max(void) {
    FILE *f = fopen("/proc/sys/kernel/pid_max", "r");
    long v = -1;
    if (f) {
        if (fscanf(f, "%ld", &v) != 1) v = -1;
        fclose(f);
    }
    return v;
}

typedef struct { long tid; pthread_mutex_t m; pthread_cond_t c; int ready; } slot_t;

static void *report(void *arg) {
    slot_t *s = arg;
    pthread_mutex_lock(&s->m);
    s->tid = my_tid();
    s->ready = 1;
    pthread_cond_signal(&s->c);
    pthread_mutex_unlock(&s->m);
    return NULL;
}

static void *report_and_hold(void *arg) {
    report(arg);
    char b;
    while (read(g_hold[0], &b, 1) < 0 && errno == EINTR) {}
    return NULL;
}

// Start a thread and wait until it has reported its id. Returns 0, or the
// pthread_create error.
static int start(void *(*fn)(void *), slot_t *s, pthread_t *t) {
    pthread_mutex_init(&s->m, NULL);
    pthread_cond_init(&s->c, NULL);
    s->ready = 0;
    s->tid = -1;
    int err = pthread_create(t, NULL, fn, s);
    if (err != 0) return err;
    pthread_mutex_lock(&s->m);
    while (!s->ready) pthread_cond_wait(&s->c, &s->m);
    pthread_mutex_unlock(&s->m);
    return 0;
}

static long spawn_and_join(void) {
    slot_t s;
    pthread_t t;
    int err = start(report, &s, &t);
    if (err != 0) { fprintf(stderr, "pthread_create: %s\n", strerror(err)); exit(2); }
    pthread_join(t, NULL);
    return s.tid;
}

static void bounds(void) {
    static const long probes[] = {
        -1, 0, 1, 2, 299, 300, 301, 302, 303, 1000, 32768,
        4194302, 4194303, 4194304, 4194305, 4194306, 8388608, 2147483647,
    };
    for (size_t i = 0; i < sizeof probes / sizeof probes[0]; i++) {
        if (write_pid_max(4194304) != 0) { printf("bounds\treset_failed\n"); exit(2); }
        int err = write_pid_max(probes[i]);
        printf("bounds\t%ld\t%s\tread_back=%ld\n", probes[i], err ? strerror(err) : "ok", read_pid_max());
    }
}

static void skip(void) {
    if (write_pid_max(1000) != 0) { printf("skip\tset_failed\n"); exit(2); }
    printf("skip\tpid\t%d\n", (int)getpid());

    // Run up to the first wrap.
    long last = spawn_and_join(), id;
    int rounds = 0;
    while ((id = spawn_and_join()) > last && rounds++ < 5000) last = id;
    printf("skip\tfirst_wrap\tfrom=%ld\tto=%ld\n", last, id);

    // `id` was joined. Hold the next id and the one after the one after, so the
    // ids after the next wrap have two live ids to step round.
    slot_t a, c;
    pthread_t ta, tc;
    if (start(report_and_hold, &a, &ta) != 0) exit(2);
    long b = spawn_and_join();
    if (start(report_and_hold, &c, &tc) != 0) exit(2);
    printf("skip\theld\t%ld\t%ld\tjoined_between=%ld\n", a.tid, c.tid, b);

    last = c.tid;
    rounds = 0;
    while ((id = spawn_and_join()) > last && rounds++ < 5000) last = id;
    printf("skip\tsecond_wrap\tfrom=%ld\tto=%ld\n", last, id);
    for (int i = 0; i < 4; i++) printf("skip\tafter_second_wrap\t%d\t%ld\n", i, spawn_and_join());
}

static void exhaust(void) {
    if (write_pid_max(400) != 0) { printf("exhaust\tset_failed\n"); exit(2); }
    printf("exhaust\tpid\t%d\n", (int)getpid());
    enum { MAX = 2000 };
    static slot_t slots[MAX];
    static pthread_t ts[MAX];
    int n = 0, err = 0;
    while (n < MAX && (err = start(report_and_hold, &slots[n], &ts[n])) == 0) n++;
    printf("exhaust\tlive_threads\t%d\n", n);
    printf("exhaust\terror\t%s\n", err ? strerror(err) : "none");
    if (n > 0) {
        long min = slots[0].tid, max = slots[0].tid;
        for (int i = 0; i < n; i++) {
            if (slots[i].tid < min) min = slots[i].tid;
            if (slots[i].tid > max) max = slots[i].tid;
        }
        printf("exhaust\tfirst\t%ld\n", slots[0].tid);
        printf("exhaust\tmin\t%ld\n", min);
        printf("exhaust\tmax\t%ld\n", max);
        for (int i = n > 3 ? n - 3 : 0; i < n; i++) printf("exhaust\tlast\t%d\t%ld\n", i, slots[i].tid);
    }
}

int main(void) {
    alarm(120);
    setvbuf(stdout, NULL, _IONBF, 0);
    if (pipe(g_hold) != 0) { perror("pipe"); return 2; }
    printf("start\tpid_max\t%ld\n", read_pid_max());
    bounds();
    skip();
    exhaust();
    close(g_hold[1]);
    return 0;
}
