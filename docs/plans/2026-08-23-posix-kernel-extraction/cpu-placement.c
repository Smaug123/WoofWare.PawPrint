// Linux and Darwin: which facts about a thread's processor a process can read,
// whether they agree with each other, and how they move when the thread blocks,
// wakes, or has its affinity changed.
//
// Output is TSV: section, key, value. Sections:
//   count      how many processors each interface reports
//   agree      (Linux) sched_getcpu, the getcpu syscall, glibc's rseq area and
//              /proc/self/task/<tid>/stat field 39, read back to back while
//              running: how often they disagree
//   setaff     (Linux) sched_setaffinity to one CPU: where the caller is on return
//   affinerr   (Linux) the errors sched_{set,get}affinity give for bad arguments
//   sleeper    (Linux) field 39 of a thread blocked in read(2): before and after
//              its affinity is changed to exclude that CPU, and where it runs on wake
//   migrate    how often the CPU changes across a block (pipe ping-pong, a
//              100 us nanosleep) and across a busy spin with no block, first
//              on an otherwise idle machine and then beside one busy thread
//              per online CPU
//   darwin     (Darwin) sched_getcpu's presence, pthread_cpu_number_np, and
//              THREAD_AFFINITY_POLICY
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/rseq.h>
#include <sys/syscall.h>
#else
#include <dlfcn.h>
#include <mach/mach.h>
#include <mach/thread_policy.h>
#include <sys/sysctl.h>
#endif

static void row(const char *section, const char *key, long long value) {
    printf("%s\t%s\t%lld\n", section, key, value);
}

// The processor the calling thread is on, by the platform's own route.
static int current_cpu(void) {
#ifdef __linux__
    return sched_getcpu();
#else
    size_t n;
    if (pthread_cpu_number_np(&n) != 0) return -1;
    return (int)n;
#endif
}

#ifdef __linux__
static int getcpu_syscall(unsigned *node) {
    unsigned cpu;
    if (syscall(SYS_getcpu, &cpu, node, NULL) != 0) return -1;
    return (int)cpu;
}

static int rseq_cpu(void) {
    if (__rseq_size == 0) return -2;
    struct rseq *r = (struct rseq *)((char *)__builtin_thread_pointer() + __rseq_offset);
    return (int)*(volatile uint32_t *)&r->cpu_id;
}

// Field 39 (processor) and field 3 (state) of /proc/self/task/<tid>/stat.
static int stat_cpu(pid_t tid, char *state) {
    char path[64], buf[1024];
    snprintf(path, sizeof path, "/proc/self/task/%d/stat", (int)tid);
    FILE *f = fopen(path, "r");
    if (!f) return -1;
    size_t n = fread(buf, 1, sizeof buf - 1, f);
    fclose(f);
    buf[n] = 0;
    char *p = strrchr(buf, ')');
    if (!p) return -1;
    p += 2; // field 3
    if (state) *state = *p;
    for (int field = 3; field < 39; field++) {
        p = strchr(p, ' ');
        if (!p) return -1;
        p++;
    }
    return atoi(p);
}

static pid_t gettid_(void) { return (pid_t)syscall(SYS_gettid); }
#endif

static void spin_us(long us) {
    struct timespec a, b;
    clock_gettime(CLOCK_MONOTONIC, &a);
    do clock_gettime(CLOCK_MONOTONIC, &b);
    while ((b.tv_sec - a.tv_sec) * 1000000L + (b.tv_nsec - a.tv_nsec) / 1000 < us);
}

// ---- migrate: ping-pong over two pipes, the measured thread on one end ----
enum { ROUNDS = 2000 };
static int ping[2], pong[2];
static int pingpong_changes, pingpong_seen[1024];

static void *pingpong_thread(void *arg) {
    (void)arg;
    char c;
    for (int i = 0; i < ROUNDS; i++) {
        int before = current_cpu();
        if (read(ping[0], &c, 1) != 1) break; // blocks until main writes
        int after = current_cpu();
        if (after != before) pingpong_changes++;
        if (after >= 0 && after < 1024) pingpong_seen[after] = 1;
        if (write(pong[1], &c, 1) != 1) break;
    }
    return NULL;
}

static volatile int spinners_stop;
static void *spinner(void *arg) {
    (void)arg;
    while (!spinners_stop) { }
    return NULL;
}

static void migrate_section(const char *prefix) {
    char key[96];
#define MROW(name, value) (snprintf(key, sizeof key, "%s%s", prefix, name), row("migrate", key, value))
#ifdef __linux__
    cpu_set_t allowed;
    sched_getaffinity(0, sizeof allowed, &allowed);
    MROW("allowed_cpus", CPU_COUNT(&allowed));
#endif
    memset(pingpong_seen, 0, sizeof pingpong_seen);
    pingpong_changes = 0;
    pipe(ping);
    pipe(pong);
    pthread_t t;
    pthread_create(&t, NULL, pingpong_thread, NULL);
    char c = 'x';
    for (int i = 0; i < ROUNDS; i++) {
        spin_us(20); // so the reader is asleep in read(2) when the byte arrives
        write(ping[1], &c, 1);
        read(pong[0], &c, 1);
    }
    pthread_join(t, NULL);
    int distinct = 0;
    for (int i = 0; i < 1024; i++) distinct += pingpong_seen[i];
    MROW("pipe_wakes", ROUNDS);
    MROW("pipe_wakes_on_another_cpu", pingpong_changes);
    MROW("pipe_distinct_cpus", distinct);

    int changes = 0, seen[1024] = { 0 };
    for (int i = 0; i < ROUNDS; i++) {
        int before = current_cpu();
        struct timespec ts = { 0, 100000 };
        nanosleep(&ts, NULL);
        int after = current_cpu();
        if (after != before) changes++;
        if (after >= 0 && after < 1024) seen[after] = 1;
    }
    distinct = 0;
    for (int i = 0; i < 1024; i++) distinct += seen[i];
    MROW("sleep_wakes", ROUNDS);
    MROW("sleep_wakes_on_another_cpu", changes);
    MROW("sleep_distinct_cpus", distinct);

    changes = 0;
    memset(seen, 0, sizeof seen);
    int last = current_cpu();
    for (int i = 0; i < ROUNDS; i++) {
        spin_us(100);
        int now = current_cpu();
        if (now != last) changes++;
        if (now >= 0 && now < 1024) seen[now] = 1;
        last = now;
    }
    distinct = 0;
    for (int i = 0; i < 1024; i++) distinct += seen[i];
    MROW("spin_samples", ROUNDS);
    MROW("spin_samples_on_another_cpu", changes);
    MROW("spin_distinct_cpus", distinct);
}

#ifdef __linux__
// ---- sleeper: a thread blocked in read(2) ----
static int sleeper_pipe[2];
static volatile pid_t sleeper_tid;
static volatile int sleeper_before = -1, sleeper_after = -1;

static void *sleeper(void *arg) {
    (void)arg;
    sleeper_tid = gettid_();
    sleeper_before = sched_getcpu();
    char c;
    read(sleeper_pipe[0], &c, 1);
    sleeper_after = sched_getcpu();
    return NULL;
}

static void sleeper_section(int ncpus) {
    pipe(sleeper_pipe);
    pthread_t t;
    pthread_create(&t, NULL, sleeper, NULL);
    usleep(100000);
    char state = '?';
    int asleep_cpu = stat_cpu(sleeper_tid, &state);
    row("sleeper", "cpu_before_block", sleeper_before);
    row("sleeper", "state_is_S", state == 'S');
    row("sleeper", "field39_while_blocked", asleep_cpu);
    int target = -1;
    if (ncpus >= 2) {
        // Exclude the CPU it last ran on: allow exactly one other.
        target = (sleeper_before + 1) % ncpus;
        cpu_set_t set;
        CPU_ZERO(&set);
        CPU_SET(target, &set);
        row("sleeper", "setaffinity_rc", pthread_setaffinity_np(t, sizeof set, &set));
        usleep(50000);
        row("sleeper", "setaffinity_target", target);
        row("sleeper", "field39_after_setaffinity_while_blocked", stat_cpu(sleeper_tid, &state));
        row("sleeper", "still_S", state == 'S');
    }
    write(sleeper_pipe[1], "x", 1);
    pthread_join(t, NULL);
    row("sleeper", "cpu_after_wake", sleeper_after);
}

static void agree_section(void) {
    int n = 20000, syscall_mismatch = 0, rseq_mismatch = 0, stat_mismatch = 0, stat_reads = 0;
    unsigned node = 99;
    int node_seen_nonzero = 0;
    pid_t me = gettid_();
    for (int i = 0; i < n; i++) {
        int a = sched_getcpu();
        int b = getcpu_syscall(&node);
        int c = rseq_cpu();
        if (node != 0) node_seen_nonzero = 1;
        if (a != b) syscall_mismatch++;
        if (c != -2 && a != c) rseq_mismatch++;
        if (i % 20 == 0) {
            int d = stat_cpu(me, NULL);
            int a2 = sched_getcpu();
            stat_reads++;
            if (d != a2) stat_mismatch++;
        }
    }
    row("agree", "rseq_registered_size", (long long)__rseq_size);
    row("agree", "samples", n);
    row("agree", "getcpu_syscall_differs", syscall_mismatch);
    row("agree", "rseq_cpu_id_differs", rseq_mismatch);
    row("agree", "field39_reads", stat_reads);
    row("agree", "field39_differs", stat_mismatch);
    row("agree", "numa_node_nonzero", node_seen_nonzero);
}

static void setaff_section(int ncpus) {
    cpu_set_t original;
    sched_getaffinity(0, sizeof original, &original);
    char key[64];
    for (int k = 0; k < ncpus && k < 16; k++) {
        cpu_set_t one;
        CPU_ZERO(&one);
        CPU_SET(k, &one);
        int rc = sched_setaffinity(0, sizeof one, &one);
        int a = sched_getcpu();
        int d = stat_cpu(gettid_(), NULL);
        snprintf(key, sizeof key, "pin_%d_rc", k);
        row("setaff", key, rc);
        snprintf(key, sizeof key, "pin_%d_sched_getcpu_on_return", k);
        row("setaff", key, a);
        snprintf(key, sizeof key, "pin_%d_field39_on_return", k);
        row("setaff", key, d);
    }
    sched_setaffinity(0, sizeof original, &original);
}

static void affinerr_section(int nconf) {
    cpu_set_t empty;
    CPU_ZERO(&empty);
    errno = 0;
    int rc = sched_setaffinity(0, sizeof empty, &empty);
    row("affinerr", "set_empty_rc", rc);
    row("affinerr", "set_empty_errno_is_EINVAL", errno == EINVAL);

    cpu_set_t absent;
    CPU_ZERO(&absent);
    CPU_SET(CPU_SETSIZE - 1, &absent); // CPU 1023: not a CPU this machine has
    errno = 0;
    rc = sched_setaffinity(0, sizeof absent, &absent);
    row("affinerr", "set_only_cpu_1023_rc", rc);
    row("affinerr", "set_only_cpu_1023_errno_is_EINVAL", errno == EINVAL);

    if (nconf < CPU_SETSIZE) {
        cpu_set_t mixed;
        CPU_ZERO(&mixed);
        CPU_SET(0, &mixed);
        CPU_SET(CPU_SETSIZE - 1, &mixed);
        errno = 0;
        rc = sched_setaffinity(0, sizeof mixed, &mixed);
        row("affinerr", "set_cpu0_and_1023_rc", rc);
        cpu_set_t back;
        sched_getaffinity(0, sizeof back, &back);
        row("affinerr", "set_cpu0_and_1023_reads_back_count", CPU_COUNT(&back));
        row("affinerr", "set_cpu0_and_1023_reads_back_1023", CPU_ISSET(CPU_SETSIZE - 1, &back) != 0);
        cpu_set_t all;
        CPU_ZERO(&all);
        for (int i = 0; i < nconf; i++) CPU_SET(i, &all);
        sched_setaffinity(0, sizeof all, &all);
    }

    unsigned char tiny[1];
    errno = 0;
    rc = (int)syscall(SYS_sched_getaffinity, 0, sizeof tiny, tiny);
    row("affinerr", "raw_get_1_byte_rc", rc);
    row("affinerr", "raw_get_1_byte_errno_is_EINVAL", errno == EINVAL);
    unsigned long word[1];
    errno = 0;
    rc = (int)syscall(SYS_sched_getaffinity, 0, sizeof word, word);
    row("affinerr", "raw_get_8_bytes_rc", rc);

    errno = 0;
    cpu_set_t s;
    rc = sched_getaffinity(999999, sizeof s, &s);
    row("affinerr", "get_no_such_pid_rc", rc);
    row("affinerr", "get_no_such_pid_errno_is_ESRCH", errno == ESRCH);
}
#endif

int main(void) {
    alarm(120);
    setvbuf(stdout, NULL, _IONBF, 0);
    long onln = sysconf(_SC_NPROCESSORS_ONLN), conf = sysconf(_SC_NPROCESSORS_CONF);
    row("count", "sysconf_nprocessors_onln", onln);
    row("count", "sysconf_nprocessors_conf", conf);
#ifdef __linux__
    cpu_set_t set;
    sched_getaffinity(0, sizeof set, &set);
    row("count", "sched_getaffinity_count", CPU_COUNT(&set));
    agree_section();
    setaff_section((int)onln);
    affinerr_section((int)conf);
    sleeper_section((int)onln);
#else
    int v;
    size_t len = sizeof v;
    if (sysctlbyname("hw.ncpu", &v, &len, NULL, 0) == 0) row("count", "hw.ncpu", v);
    len = sizeof v;
    if (sysctlbyname("hw.activecpu", &v, &len, NULL, 0) == 0) row("count", "hw.activecpu", v);
    len = sizeof v;
    if (sysctlbyname("hw.perflevel0.logicalcpu", &v, &len, NULL, 0) == 0) row("count", "hw.perflevel0.logicalcpu", v);
    len = sizeof v;
    if (sysctlbyname("hw.perflevel1.logicalcpu", &v, &len, NULL, 0) == 0) row("count", "hw.perflevel1.logicalcpu", v);

    row("darwin", "dlsym_sched_getcpu_present", dlsym(RTLD_DEFAULT, "sched_getcpu") != NULL);
    row("darwin", "dlsym_getcpu_present", dlsym(RTLD_DEFAULT, "getcpu") != NULL);
    size_t n = 12345;
    int rc = pthread_cpu_number_np(&n);
    row("darwin", "pthread_cpu_number_np_rc", rc);
    int lo = 1 << 30, hi = -1;
    for (int i = 0; i < 100000; i++) {
        int c = current_cpu();
        if (c < lo) lo = c;
        if (c > hi) hi = c;
        if ((i & 1023) == 0) sched_yield();
    }
    row("darwin", "pthread_cpu_number_np_min", lo);
    row("darwin", "pthread_cpu_number_np_max", hi);
    thread_affinity_policy_data_t policy = { 1 };
    kern_return_t kr = thread_policy_set(mach_thread_self(), THREAD_AFFINITY_POLICY, (thread_policy_t)&policy,
                                         THREAD_AFFINITY_POLICY_COUNT);
    row("darwin", "thread_affinity_policy_kern_return", kr);
    row("darwin", "thread_affinity_policy_is_KERN_NOT_SUPPORTED", kr == KERN_NOT_SUPPORTED);
#endif
    migrate_section("idle_");
    // The same with one busy thread per online CPU beside it, so that every
    // CPU has something else to run.
    pthread_t spinners[256];
    int nspin = onln < 256 ? (int)onln : 256;
    for (int i = 0; i < nspin; i++) pthread_create(&spinners[i], NULL, spinner, NULL);
    migrate_section("loaded_");
    spinners_stop = 1;
    for (int i = 0; i < nspin; i++) pthread_join(spinners[i], NULL);
    return 0;
}
