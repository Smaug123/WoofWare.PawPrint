// What sigprocmask(2) and pthread_sigmask(3) answer, and what they do to the
// calling thread's mask.
//
// Every row runs in a fresh forked child, so no row sees another's mask. A
// mask is printed as the 64-bit word whose bit n-1 is signo n, read straight
// out of the sigset_t (glibc's sigismember screens nothing, but its
// sigaddset refuses 32 and 33, so sets are written bit by bit here).
//
// Sections:
//   how     - each `how` (the three real ones and several invalid numbers),
//             with a set and with a NULL set, through each route: the C
//             library's sigprocmask, pthread_sigmask, and the raw system call
//             (Linux rt_sigprocmask with sigsetsize 8; Darwin's
//             SYS_sigprocmask and SYS___pthread_sigmask). The thread starts
//             with {HUP, INT} blocked and the set is {INT, TERM}. `old` is
//             filled with 0xA5 first, so a call that does not write it prints
//             "untouched".
//   size    - (Linux) raw rt_sigprocmask with every sigsetsize from 0 to 16,
//             and 128 (glibc's sizeof(sigset_t)), with a set, with a NULL set,
//             and with a NULL set and an invalid `how`: which is screened
//             first.
//   rt      - (Linux) the raw call blocking 32 and 33, then the mask read
//             back through glibc's wrapper and through the raw call.
//   full    - every bit of the word set and blocked, through each route, and
//             the mask read back: what is dropped.
//   bit     - (Darwin) SIG_SETMASK and SIG_BLOCK with bit 31 alone (signo 32,
//             which Darwin does not have), and with bit 31 and HUP.
//   threads - a second thread blocks USR1 through each route; then both
//             threads report their masks. Is the call per-thread?
//   spread  - the main thread blocks HUP and a second thread blocks INT; the
//             second thread waits while the main thread makes one call
//             through each route (SIG_BLOCK {TERM}, SIG_UNBLOCK {HUP, INT},
//             SIG_SETMASK {TERM}), then both report their masks. Where the
//             call reaches another thread, exactly what it does there.
//   unblock - handlers for ILL, USR1 and TERM (empty sa_mask, no flags), all
//             three blocked, all three sent (process-directed with kill, or
//             thread-directed with pthread_kill), then one SIG_UNBLOCK of all
//             three. Which handlers ran, in which order, and how many had run
//             when the unblocking call returned.
//
// Bounded: GUARD_MAIN and GUARD_CHILD, and every loop over a fixed list.
//
// Darwin: clang -Wall -Wno-unused-function -Wno-deprecated-declarations -o /tmp/sigprocmask-ops sigprocmask-ops.c && /tmp/sigprocmask-ops
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/sigprocmask-ops.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container), 2026-10-08; the output is beside this file
// as sigprocmask-ops.darwin-27.0-uid501.txt and
// sigprocmask-ops.linux-6.18.5-aarch64.txt. The rows are transcribed in
// WoofWare.PosixKernel.Test/TestSigprocmask.fs.
#include "signal-probe-common.h"
#include <limits.h>
#include <sys/syscall.h>
#include <sys/utsname.h>

#ifdef __APPLE__
typedef uint32_t raw_word;
#define WORD_BITS 32
#else
typedef uint64_t raw_word;
#define WORD_BITS 64
#endif

// Which call a row makes.
enum route { LIBC, PTHREAD, RAW, RAW_PTHREAD };
static const char *route_name[] = { "libc", "pthread", "raw", "rawpthread" };

static int route_exists(enum route r)
{
#ifdef __APPLE__
    (void)r;
    return 1;
#else
    return r != RAW_PTHREAD;
#endif
}

static uint64_t bits_of(const sigset_t *set)
{
    raw_word w;
    memcpy(&w, set, sizeof w);
    return (uint64_t)w;
}

static void set_bits(sigset_t *set, uint64_t bits)
{
    memset(set, 0, sizeof *set);
    raw_word w = (raw_word)bits;
    memcpy(set, &w, sizeof w);
}

static uint64_t bit(int signo) { return (uint64_t)1 << (signo - 1); }

// One call of `how` through route `r`, with `set` (NULL for a query) and
// `old`. Answers -1 with errno for every route, pthread_sigmask's returned
// number moved into errno, so that rows compare.
static int call(enum route r, int how, const sigset_t *set, sigset_t *old)
{
    int ret;
    errno = 0;
    switch (r) {
    case LIBC:
        return sigprocmask(how, set, old);
    case PTHREAD:
        ret = pthread_sigmask(how, set, old);
        if (ret != 0) { errno = ret; return -1; }
        return 0;
    case RAW:
#ifdef __APPLE__
        return (int)syscall(SYS_sigprocmask, how, set, old);
#else
        return (int)syscall(SYS_rt_sigprocmask, how, set, old, (size_t)8);
#endif
    case RAW_PTHREAD:
#ifdef __APPLE__
        return (int)syscall(SYS___pthread_sigmask, how, set, old);
#else
        return -2;
#endif
    }
    return -2;
}

// The calling thread's mask, read through the raw call, which screens
// nothing on the way out.
static uint64_t current(void)
{
    sigset_t old;
    memset(&old, 0, sizeof old);
#ifdef __APPLE__
    if (syscall(SYS___pthread_sigmask, SIG_BLOCK, NULL, &old) != 0) die("query");
#else
    if (syscall(SYS_rt_sigprocmask, SIG_BLOCK, NULL, &old, (size_t)8) != 0) die("query");
#endif
    return bits_of(&old);
}

static void set_current(uint64_t bits)
{
    sigset_t s;
    set_bits(&s, bits);
#ifdef __APPLE__
    if (syscall(SYS___pthread_sigmask, SIG_SETMASK, &s, NULL) != 0) die("setmask");
#else
    if (syscall(SYS_rt_sigprocmask, SIG_SETMASK, &s, NULL, (size_t)8) != 0) die("setmask");
#endif
}

// Run `body` in a fresh child and print what it wrote, prefixed by `tag`.
static void in_child(const char *tag, void (*body)(int fd, void *arg), void *arg)
{
    int fds[2];
    if (pipe(fds) != 0) die("pipe");
    fflush(stdout);
    pid_t child = fork();
    if (child < 0) die("fork");
    if (child == 0) {
        GUARD_CHILD();
        close(fds[0]);
        body(fds[1], arg);
        _exit(0);
    }
    close(fds[1]);
    char buf[4096];
    ssize_t total = 0, n;
    while (total < (ssize_t)sizeof buf - 1 && (n = read(fds[0], buf + total, sizeof buf - 1 - total)) > 0)
        total += n;
    buf[total] = 0;
    close(fds[0]);
    int status;
    waitpid(child, &status, 0);
    printf("%s%s end=%s%d\n", tag, buf, WIFEXITED(status) ? "exit" : "signal",
           WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
    fflush(stdout);
}

static void out(int fd, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
#include <stdarg.h>
static void out(int fd, const char *fmt, ...)
{
    char line[512];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(line, sizeof line, fmt, ap);
    va_end(ap);
    write(fd, line, strlen(line));
}

// Print the outcome of one call: return, errno, old (or "untouched"), and
// the mask afterwards.
static void report(int fd, int ret, int err, const sigset_t *old)
{
    unsigned char sentinel[sizeof(sigset_t)];
    memset(sentinel, 0xA5, sizeof sentinel);
    out(fd, " ret=%d errno=%d", ret, ret == 0 ? 0 : err);
    if (memcmp(old, sentinel, sizeof sentinel) == 0)
        out(fd, " old=untouched");
    else
        out(fd, " old=%llx", (unsigned long long)bits_of(old));
    out(fd, " now=%llx", (unsigned long long)current());
}

struct how_row { enum route r; int how; int with_set; };

static void how_body(int fd, void *arg)
{
    struct how_row *row = arg;
    set_current(bit(SIGHUP) | bit(SIGINT));
    sigset_t set, old;
    set_bits(&set, bit(SIGINT) | bit(SIGTERM));
    memset(&old, 0xA5, sizeof old);
    int ret = call(row->r, row->how, row->with_set ? &set : NULL, &old);
    report(fd, ret, errno, &old);
}

#ifndef __APPLE__
struct size_row { size_t size; int with_set; int how; };

static void size_body(int fd, void *arg)
{
    struct size_row *row = arg;
    set_current(bit(SIGHUP) | bit(SIGINT));
    sigset_t set, old;
    set_bits(&set, bit(SIGINT) | bit(SIGTERM));
    memset(&old, 0xA5, sizeof old);
    errno = 0;
    int ret = (int)syscall(SYS_rt_sigprocmask, row->how, row->with_set ? &set : NULL, &old, row->size);
    report(fd, ret, errno, &old);
}

static void rt_body(int fd, void *arg)
{
    (void)arg;
    sigset_t set, old;
    set_bits(&set, bit(32) | bit(33) | bit(SIGHUP));
    errno = 0;
    int ret = (int)syscall(SYS_rt_sigprocmask, SIG_BLOCK, &set, NULL, (size_t)8);
    out(fd, " raw-block ret=%d errno=%d", ret, ret == 0 ? 0 : errno);
    memset(&old, 0, sizeof old);
    ret = sigprocmask(SIG_BLOCK, NULL, &old);
    out(fd, " libc-old ret=%d old=%llx", ret, (unsigned long long)bits_of(&old));
    memset(&old, 0, sizeof old);
    ret = pthread_sigmask(SIG_BLOCK, NULL, &old);
    out(fd, " pthread-old ret=%d old=%llx", ret, (unsigned long long)bits_of(&old));
    out(fd, " raw-now=%llx", (unsigned long long)current());
    // A SIG_SETMASK through glibc with neither 32 nor 33: does glibc keep
    // the bits the raw call set, or clear them?
    sigset_t only_hup;
    set_bits(&only_hup, bit(SIGHUP));
    ret = sigprocmask(SIG_SETMASK, &only_hup, NULL);
    out(fd, " libc-setmask-hup ret=%d raw-now=%llx", ret, (unsigned long long)current());
    // And SIG_UNBLOCK of a set holding 32 and 33 through glibc, from a mask
    // holding them.
    set_current(bit(32) | bit(33) | bit(SIGHUP));
    sigset_t rt_set;
    set_bits(&rt_set, bit(32) | bit(33));
    ret = sigprocmask(SIG_UNBLOCK, &rt_set, NULL);
    out(fd, " libc-unblock-rt ret=%d raw-now=%llx", ret, (unsigned long long)current());
}
#endif

struct full_row { enum route r; int how; };

static void full_body(int fd, void *arg)
{
    struct full_row *row = arg;
    set_current(0);
    sigset_t set, old;
    set_bits(&set, ~(uint64_t)0);
    memset(&old, 0xA5, sizeof old);
    int ret = call(row->r, row->how, &set, &old);
    report(fd, ret, errno, &old);
}

#ifdef __APPLE__
struct bit_row { enum route r; int how; uint64_t bits; };

static void bit_body(int fd, void *arg)
{
    struct bit_row *row = arg;
    set_current(0);
    sigset_t set, old;
    set_bits(&set, row->bits);
    memset(&old, 0xA5, sizeof old);
    int ret = call(row->r, row->how, &set, &old);
    report(fd, ret, errno, &old);
}
#endif

static enum route g_thread_route;
static volatile uint64_t g_thread_mask;

static void *thread_blocks(void *arg)
{
    (void)arg;
    sigset_t set;
    set_bits(&set, bit(SIGUSR1));
    if (call(g_thread_route, SIG_BLOCK, &set, NULL) != 0) die("thread block");
    g_thread_mask = current();
    return NULL;
}

static void threads_body(int fd, void *arg)
{
    g_thread_route = *(enum route *)arg;
    set_current(0);
    pthread_t t;
    if (pthread_create(&t, NULL, thread_blocks, NULL) != 0) die("pthread_create");
    pthread_join(t, NULL);
    out(fd, " second=%llx main=%llx", (unsigned long long)g_thread_mask, (unsigned long long)current());
}

struct spread_row { enum route r; int how; uint64_t set; };
static pthread_mutex_t g_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_cond = PTHREAD_COND_INITIALIZER;
static int g_stage;

static void wait_stage(int stage)
{
    pthread_mutex_lock(&g_lock);
    while (g_stage < stage) pthread_cond_wait(&g_cond, &g_lock);
    pthread_mutex_unlock(&g_lock);
}

static void advance(int stage)
{
    pthread_mutex_lock(&g_lock);
    g_stage = stage;
    pthread_cond_broadcast(&g_cond);
    pthread_mutex_unlock(&g_lock);
}

static void *spread_second(void *arg)
{
    (void)arg;
    set_current(bit(SIGINT));
    advance(1);
    wait_stage(2);
    g_thread_mask = current();
    return NULL;
}

static void spread_body(int fd, void *arg)
{
    struct spread_row *row = arg;
    set_current(bit(SIGHUP));
    pthread_t t;
    if (pthread_create(&t, NULL, spread_second, NULL) != 0) die("pthread_create");
    wait_stage(1);
    sigset_t set, old;
    set_bits(&set, row->set);
    memset(&old, 0xA5, sizeof old);
    int ret = call(row->r, row->how, &set, &old);
    int err = errno;
    advance(2);
    pthread_join(t, NULL);
    report(fd, ret, err, &old);
    out(fd, " second=%llx", (unsigned long long)g_thread_mask);
}

static volatile sig_atomic_t g_order[8];
static volatile sig_atomic_t g_count;

static void record(int s)
{
    if (g_count < 8) g_order[g_count] = s;
    g_count++;
}

static void unblock_body(int fd, void *arg)
{
    int thread_directed = *(int *)arg;
    int sigs[] = { SIGTERM, SIGUSR1, SIGILL };
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = record;
    sigemptyset(&sa.sa_mask);
    uint64_t all = 0;
    for (int i = 0; i < 3; i++) {
        if (sigaction(sigs[i], &sa, NULL) != 0) die("sigaction");
        all |= bit(sigs[i]);
    }
    set_current(all);
    for (int i = 0; i < 3; i++) {
        if (thread_directed) pthread_kill(pthread_self(), sigs[i]);
        else kill(getpid(), sigs[i]);
    }
    int before = g_count;
    sigset_t set;
    set_bits(&set, all);
    int ret = sigprocmask(SIG_UNBLOCK, &set, NULL);
    int at_return = g_count;
    out(fd, " before=%d ret=%d at-return=%d order=", before, ret, at_return);
    for (int i = 0; i < g_count && i < 8; i++) out(fd, "%s%s", i ? "," : "", signame(g_order[i]));
}

int main(void)
{
    GUARD_MAIN();
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, uid %d\n", u.sysname, u.release, u.machine, (int)getuid());
    printf("# flavour %s SIG_BLOCK %d SIG_UNBLOCK %d SIG_SETMASK %d sizeof(sigset_t) %zu\n", FLAVOUR, SIG_BLOCK,
           SIG_UNBLOCK, SIG_SETMASK, sizeof(sigset_t));
    printf("# start mask HUP|INT = %llx, set INT|TERM = %llx\n", (unsigned long long)(bit(SIGHUP) | bit(SIGINT)),
           (unsigned long long)(bit(SIGINT) | bit(SIGTERM)));

    int hows[] = { SIG_BLOCK, SIG_UNBLOCK, SIG_SETMASK, -1, 0, 1, 2, 3, 4, 5, 100, INT_MAX, INT_MIN };
    char tag[128];
    for (int r = LIBC; r <= RAW_PTHREAD; r++) {
        if (!route_exists((enum route)r)) continue;
        for (size_t h = 0; h < sizeof hows / sizeof hows[0]; h++) {
            // The first three are the named ones; the rest are every small
            // number and some large ones, deduplicated against the named.
            if (h >= 3 && (hows[h] == SIG_BLOCK || hows[h] == SIG_UNBLOCK || hows[h] == SIG_SETMASK)) continue;
            for (int with_set = 1; with_set >= 0; with_set--) {
                struct how_row row = { (enum route)r, hows[h], with_set };
                snprintf(tag, sizeof tag, "how %s how=%d set=%s", route_name[r], hows[h], with_set ? "yes" : "null");
                in_child(tag, how_body, &row);
            }
        }
    }

#ifndef __APPLE__
    size_t sizes[] = { 0, 1, 4, 7, 8, 9, 16, 128 };
    for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        struct size_row rows[] = {
            { sizes[i], 1, SIG_BLOCK },
            { sizes[i], 0, SIG_BLOCK },
            { sizes[i], 0, 100 },
            { sizes[i], 1, 100 },
        };
        for (size_t j = 0; j < sizeof rows / sizeof rows[0]; j++) {
            snprintf(tag, sizeof tag, "size %zu set=%s how=%d", sizes[i], rows[j].with_set ? "yes" : "null",
                     rows[j].how);
            in_child(tag, size_body, &rows[j]);
        }
    }
    in_child("rt", rt_body, NULL);
#endif

    int real_hows[] = { SIG_BLOCK, SIG_SETMASK };
    for (int r = LIBC; r <= RAW_PTHREAD; r++) {
        if (!route_exists((enum route)r)) continue;
        for (size_t h = 0; h < 2; h++) {
            struct full_row row = { (enum route)r, real_hows[h] };
            snprintf(tag, sizeof tag, "full %s how=%d", route_name[r], real_hows[h]);
            in_child(tag, full_body, &row);
        }
    }

#ifdef __APPLE__
    uint64_t bit_sets[] = { (uint64_t)1 << 31, ((uint64_t)1 << 31) | bit(SIGHUP) };
    for (int r = LIBC; r <= RAW_PTHREAD; r++) {
        for (size_t h = 0; h < 2; h++) {
            for (size_t b = 0; b < 2; b++) {
                struct bit_row row = { (enum route)r, real_hows[h], bit_sets[b] };
                snprintf(tag, sizeof tag, "bit %s how=%d set=%llx", route_name[r], real_hows[h],
                         (unsigned long long)bit_sets[b]);
                in_child(tag, bit_body, &row);
            }
        }
    }
#endif

    for (int r = LIBC; r <= RAW_PTHREAD; r++) {
        if (!route_exists((enum route)r)) continue;
        enum route route = (enum route)r;
        snprintf(tag, sizeof tag, "threads %s", route_name[r]);
        in_child(tag, threads_body, &route);
    }

    struct { int how; uint64_t set; } spreads[] = {
        { SIG_BLOCK, bit(SIGTERM) },
        { SIG_UNBLOCK, bit(SIGHUP) | bit(SIGINT) },
        { SIG_SETMASK, bit(SIGTERM) },
    };
    for (int r = LIBC; r <= RAW_PTHREAD; r++) {
        if (!route_exists((enum route)r)) continue;
        for (size_t i = 0; i < sizeof spreads / sizeof spreads[0]; i++) {
            struct spread_row row = { (enum route)r, spreads[i].how, spreads[i].set };
            snprintf(tag, sizeof tag, "spread %s how=%d set=%llx (main %llx, second %llx)", route_name[r],
                     spreads[i].how, (unsigned long long)spreads[i].set, (unsigned long long)bit(SIGHUP),
                     (unsigned long long)bit(SIGINT));
            in_child(tag, spread_body, &row);
        }
    }

    for (int thread_directed = 0; thread_directed <= 1; thread_directed++) {
        snprintf(tag, sizeof tag, "unblock %s", thread_directed ? "pthread_kill" : "kill");
        in_child(tag, unblock_body, &thread_directed);
    }
    return 0;
}
