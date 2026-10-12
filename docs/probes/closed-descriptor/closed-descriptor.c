// Whether INT_MAX can stand in for "a closed descriptor" in a host test: that
// the calls TestDirectoryReadAgainstHost and TestSockOptAgainstHost make answer
// it exactly as they answer a slot this probe opened and closed again, and that
// no descriptor the process can open ever has that number.
//
// Each row is one call, made once on the freed slot and once on INT_MAX, with
// every other argument the same; it prints both answers, and `differ` counts
// the rows on which they disagree. The rows cover the arguments those tests
// pass beside a closed descriptor, and other bad ones, so that the answers
// also say which error comes first:
//   D: the directory read (Linux's getdents64, Darwin's __getdirentries64),
//      with real, null and faulting buffers (and, on Darwin, `basep`), and
//      counts from 0 to UINT_MAX.
//   S: setsockopt of every option the model holds, with real, null and
//      faulting value buffers and every length TestSockOptAgainstHost picks.
//   G: getsockopt of the same, with every combination of real, null and
//      faulting value and length buffers, and those lengths.
// Then dup2 and F_DUPFD ask for INT_MAX itself, under the soft RLIMIT_NOFILE
// the probe starts with, under soft=hard (the .NET runtime raises the soft
// limit to the hard one at startup), and under the highest limit the kernel
// will set; the soft limit each is tried under is printed beside it.
//
// Build and run, from this directory:
//   Darwin: cc -Wall -O1 -o /tmp/closed-descriptor closed-descriptor.c && /tmp/closed-descriptor
//   Linux:  container run --rm --cap-add ALL -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/closed-descriptor.c && mount -o remount,rw /proc/sys && (echo 2147483647 > /proc/sys/fs/nr_open) 2>/dev/null; echo 2147483584 > /proc/sys/fs/nr_open && /tmp/p'
// On Linux the wrapper first raises fs.nr_open, which caps the hard limit even
// for root, to the highest value the kernel accepts: 2147483647 is refused, and
// 2147483584 (INT_MAX rounded down to a multiple of 64) taken, so the probe, as
// root, can then raise its own limit that far.
//
// Measured 2026-10-12 on Linux 6.18.5 aarch64 (Apple's container VM, root) and
// Darwin 27.0.0 arm64 (uid 501); one run of each is beside this file. On both,
// every row answers INT_MAX exactly as the freed slot, under the starting limit
// and under the highest, and INT_MAX is never allocated. Linux answers EBADF on
// every row. Darwin answers EBADF on every row but one kind: setsockopt with a
// null value and a nonzero length answers EFAULT, for both numbers, because it
// checks that before it looks the descriptor up.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>

#ifdef __APPLE__
extern ssize_t __getdirentries64(int fd, void *buf, size_t count, off_t *basep);
#endif

static const char *errno_name(int e) {
    static char other[32];
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case EINVAL: return "EINVAL";
    case EFAULT: return "EFAULT";
    case ENOTSOCK: return "ENOTSOCK";
    case ENOTDIR: return "ENOTDIR";
    case ENOPROTOOPT: return "ENOPROTOOPT";
    case EMFILE: return "EMFILE";
    case EPERM: return "EPERM";
    default:
        snprintf(other, sizeof other, "errno=%d", e);
        return other;
    }
}

static int differ = 0;
static int rows = 0;
// Whether a row whose answers agree is printed.
static int print_agreeing = 1;

// One row: the answer for the freed slot, then the one for INT_MAX.
static void row(const char *call, const char *args, long slot, int slot_errno, long max, int max_errno) {
    rows++;
    int same = slot == max && slot_errno == max_errno;
    if (!same)
        differ++;
    if (same && !print_agreeing)
        return;
    printf("%s\t%s\t%ld %s\t%ld %s%s\n", call, args, slot, errno_name(slot_errno), max, errno_name(max_errno),
           same ? "" : "\tDIFFER");
}

static void *faulting;
static char storage[4096];

static void *buffer_named(int which) {
    switch (which) {
    case 0: return storage;
    case 1: return NULL;
    default: return faulting;
    }
}

static const char *buffer_names[] = { "real", "null", "faulting" };

static long directory_read(int fd, int buffer, int basep_buffer, size_t count) {
#ifdef __APPLE__
    off_t basep_storage = 0;
    off_t *basep = basep_buffer == 0 ? &basep_storage : (off_t *)buffer_named(basep_buffer);
    return (long)__getdirentries64(fd, buffer_named(buffer), count, basep);
#else
    (void)basep_buffer;
    return syscall(SYS_getdents64, fd, buffer_named(buffer), count);
#endif
}

static void directory_reads(int slot) {
    static const size_t counts[] = { 0, 1, 8, 24, 4096, 0x7fffffffu, 0x80000000u, 0xffffffffu };
#ifdef __APPLE__
    const int baseps = 3;
#else
    const int baseps = 1;
#endif
    for (int b = 0; b < 3; b++)
        for (int p = 0; p < baseps; p++)
            for (size_t c = 0; c < sizeof counts / sizeof counts[0]; c++) {
                char args[96];
                snprintf(args, sizeof args, "buf=%s basep=%s count=%zu", buffer_names[b], buffer_names[p], counts[c]);
                errno = 0;
                long a = directory_read(slot, b, p, counts[c]);
                int ae = errno;
                errno = 0;
                long m = directory_read(INT_MAX, b, p, counts[c]);
                int me = errno;
                row("D", args, a, ae, m, me);
            }
}

struct option { const char *name; int level; int optname; };

static const struct option options[] = {
    { "SO_REUSEADDR", SOL_SOCKET, SO_REUSEADDR },
    { "TCP_NODELAY", IPPROTO_TCP, TCP_NODELAY },
    { "IPV6_V6ONLY", IPPROTO_IPV6, IPV6_V6ONLY },
    { "SO_LINGER", SOL_SOCKET, SO_LINGER },
#ifdef __APPLE__
    { "SO_LINGER_SEC", SOL_SOCKET, 0x1080 },
#endif
    // Not an option either flavour has, so the call would otherwise fail on it.
    { "level=12345", 12345, 1 },
};

static const uint32_t lengths[] = {
    0, 1, 2, 3, 4, 5, 7, 8, 9, 16, 0x7fffffffu, 0x80000000u, 0xfffffffeu, 0xffffffffu,
};

static void sockopts(int slot) {
    for (size_t o = 0; o < sizeof options / sizeof options[0]; o++)
        for (int b = 0; b < 3; b++)
            for (size_t l = 0; l < sizeof lengths / sizeof lengths[0]; l++) {
                char args[96];
                snprintf(args, sizeof args, "%s value=%s length=%u", options[o].name, buffer_names[b], lengths[l]);
                memset(storage, 0, sizeof storage);
                errno = 0;
                long a = setsockopt(slot, options[o].level, options[o].optname, buffer_named(b), (socklen_t)lengths[l]);
                int ae = errno;
                errno = 0;
                long m = setsockopt(INT_MAX, options[o].level, options[o].optname, buffer_named(b), (socklen_t)lengths[l]);
                int me = errno;
                row("S", args, a, ae, m, me);
            }

    // The length buffer is its own storage, so that a real value buffer and a
    // real length buffer do not overlap.
    static socklen_t length_storage;
    for (size_t o = 0; o < sizeof options / sizeof options[0]; o++)
        for (int vb = 0; vb < 3; vb++)
            for (int lb = 0; lb < 3; lb++)
                for (size_t l = 0; l < sizeof lengths / sizeof lengths[0]; l++) {
                    char args[128];
                    snprintf(args, sizeof args, "%s value=%s lengthbuf=%s length=%u", options[o].name,
                             buffer_names[vb], buffer_names[lb], lengths[l]);
                    socklen_t *length = lb == 0 ? &length_storage : (socklen_t *)buffer_named(lb);
                    length_storage = (socklen_t)lengths[l];
                    errno = 0;
                    long a = getsockopt(slot, options[o].level, options[o].optname, buffer_named(vb), length);
                    int ae = errno;
                    length_storage = (socklen_t)lengths[l];
                    errno = 0;
                    long m = getsockopt(INT_MAX, options[o].level, options[o].optname, buffer_named(vb), length);
                    int me = errno;
                    row("G", args, a, ae, m, me);
                }
}

static void answer(const char *row, const char *what, int r) {
    if (r < 0)
        printf("%s\t%s\t-1 %s\n", row, what, errno_name(errno));
    else
        printf("%s\t%s\t%d\n", row, what, r);
}

static void allocations(const char *limit) {
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    char what[128];
    snprintf(what, sizeof what, "%s(soft=%llu)\tdup2", limit, (unsigned long long)rl.rlim_cur);
    errno = 0;
    answer("A", what, dup2(0, INT_MAX));
    snprintf(what, sizeof what, "%s(soft=%llu)\tF_DUPFD", limit, (unsigned long long)rl.rlim_cur);
    errno = 0;
    answer("A", what, fcntl(0, F_DUPFD, INT_MAX));
}

int main(void) {
    struct utsname u;
    uname(&u);
    printf("#\t%s %s %s\n", u.sysname, u.release, u.machine);

    faulting = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (faulting == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    // A slot this probe opened and closed. Nothing else in the process opens a
    // descriptor, so it stays free.
    int slot = open("/dev/null", O_RDONLY);
    close(slot);

    directory_reads(slot);
    sockopts(slot);
    printf("#\t%d rows, %d differ\n", rows, differ);

    allocations("start");
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    rl.rlim_cur = rl.rlim_max;
    errno = 0;
    answer("L", "soft=hard", setrlimit(RLIMIT_NOFILE, &rl));
    allocations("hard");
    // The highest limit the kernel accepts: RLIM_INFINITY if it takes that,
    // and otherwise the highest value up to INT_MAX, found by trying only
    // raises, since a lowered hard limit cannot be raised again without
    // privilege.
    struct rlimit infinite = { RLIM_INFINITY, RLIM_INFINITY };
    errno = 0;
    int r = setrlimit(RLIMIT_NOFILE, &infinite);
    answer("L", "infinity", r);
    if (r < 0) {
        getrlimit(RLIMIT_NOFILE, &rl);
        rlim_t lo = rl.rlim_max, hi = (rlim_t)INT_MAX + 1;
        while (hi - lo > 1) {
            rlim_t mid = lo + (hi - lo) / 2;
            struct rlimit candidate = { mid, mid };
            if (setrlimit(RLIMIT_NOFILE, &candidate) == 0)
                lo = mid;
            else
                hi = mid;
        }
        printf("L\thighest\t%llu\n", (unsigned long long)lo);
    }
    allocations("highest");

    // The same rows again under the highest limit, under which a freed slot
    // could in principle be any number up to it; only disagreements print.
    print_agreeing = 0;
    rows = 0;
    differ = 0;
    directory_reads(slot);
    sockopts(slot);
    printf("#\t%d rows under the highest limit, %d differ\n", rows, differ);
    return differ != 0;
}
