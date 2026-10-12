// Whether shutdown(2) of a descriptor number that can never be allocated
// answers as one of a freed slot does, and whether INT_MAX is such a number.
//
// `closed-slot` is a descriptor opened and closed again by this probe, as in
// tcp-shutdown.c's `U closed-fd` rows; `int-max` is INT_MAX. Each is asked
// for every `how` those rows use, and for the out-of-range 3 and -1. Then
// dup2 and F_DUPFD ask for INT_MAX itself, once under the soft
// RLIMIT_NOFILE the probe starts with, and again after the soft limit is
// raised to the hard one (as the .NET runtime does at startup), and again
// under the highest limit the kernel will set; the soft limit each is tried
// under is printed beside it.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/unallocatable-fd unallocatable-fd.c && /tmp/unallocatable-fd
//   Linux:  container run --rm --cap-add ALL -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/unallocatable-fd.c && mount -o remount,rw /proc/sys && (echo 2147483647 > /proc/sys/fs/nr_open) 2>/dev/null; echo 2147483584 > /proc/sys/fs/nr_open && /tmp/p'
// On Linux the wrapper first raises fs.nr_open, which caps the hard limit
// even for root, to the highest value the kernel accepts: 2147483647 is
// refused, and 2147483584 (INT_MAX rounded down to a multiple of 64) taken,
// so the probe, as root, can then raise its own limit that far.
//
// Measured 2026-10-12 on Linux 6.18.5 aarch64 (Apple's container VM, root)
// and Darwin 27.0.0 arm64 (uid 501); one run of each is
// unallocatable-fd.linux-6.18.5-aarch64.txt and unallocatable-fd.darwin-27.0.txt.
// On both, INT_MAX answers every shutdown exactly as the freed slot does
// (EBADF, ahead of the EINVAL of a bad `how`), and is never allocated, under
// any limit the process can reach.
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>

static const char *errno_name(int e) {
    switch (e) {
    case EBADF: return "EBADF";
    case EINVAL: return "EINVAL";
    case ENOTSOCK: return "ENOTSOCK";
    case EMFILE: return "EMFILE";
    case EPERM: return "EPERM";
    default: return "other";
    }
}

static void answer(const char *row, const char *what, int r) {
    if (r < 0)
        printf("%s\t%s\t-1 %s\n", row, what, errno_name(errno));
    else
        printf("%s\t%s\t%d\n", row, what, r);
}

static void shutdowns(const char *row, int fd) {
    static const int hows[] = { SHUT_RD, SHUT_WR, SHUT_RDWR, 3, -1 };
    static const char *names[] = { "RD", "WR", "RDWR", "how=3", "how=-1" };
    for (int h = 0; h < 5; h++) {
        errno = 0;
        answer(row, names[h], shutdown(fd, hows[h]));
    }
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

    int f = open("/dev/null", O_RDONLY);
    close(f);
    shutdowns("S\tclosed-slot", f);
    shutdowns("S\tint-max", INT_MAX);

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
    shutdowns("S\tint-max-raised", INT_MAX);
    return 0;
}
