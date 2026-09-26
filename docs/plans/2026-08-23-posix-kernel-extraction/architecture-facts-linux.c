// The Linux facts that follow from the architecture a process runs as rather
// than from the kernel's source: `struct epoll_event`'s size, the `maxevents`
// bound derived from it, the page size, and `getrandom`'s per-call cap.
//
// Every fact is asked of the kernel as well as of the compiler:
//
// * `sizeof(struct epoll_event)` is printed, then measured three ways the
//   kernel decides: the largest `maxevents` that is not EINVAL (bisected over
//   [1, INT_MAX] after a strided sweep checks the answer is monotone), the
//   byte extent `access_ok` screens (the highest buffer address that is not
//   EFAULT for one event and for two; their difference is the stride, and the
//   first plus one stride is the machine's TASK_SIZE_MAX), and the bytes the
//   kernel writes when it delivers one event into a buffer prefilled 0xAA.
// * The page size, from `sysconf` and from the auxiliary vector.
// * `getrandom`'s cap: a SIZE_MAX request into a mapping larger than 2 GiB,
//   compared against INT_MAX rounded down to a page. The mapping is touched
//   in full, so the container wants `-m 6G`.
//
// Results, 2026-09-26:
//
//                          aarch64 (6.18.5)   x86-64 (6.12.107, QEMU)
//   sizeof(epoll_event)    16                 12 (data at offset 4)
//   largest maxevents      134217727          178956970
//   screened stride        16                 12
//   TASK_SIZE_MAX          2^48               2^47 - 4096 (-cpu qemu64),
//                                             2^56 - 4096 (-cpu max: LA57)
//   page size              4096               4096
//   getrandom cap          0x7FFFF000         0x7FFFF000 (INT_MAX, 2^31 and
//                                             SIZE_MAX requests alike)
//
// The sweep found one flip in each column, so the bisection's answer is the
// only boundary, and every count within 4096 of it agreed.
//
// Where each column came from:
//
// * aarch64: natively in Apple's `container` (gcc:14 image, `-m 6G`).
// * x86-64: a real x86-64 kernel, Debian trixie's linux-image-amd64, booted
//   under full-system emulation with this probe linked statically as the
//   initramfs's /init:
//     qemu-system-x86_64 -m 4G -cpu max -kernel vmlinuz -initrd init.cpio.gz \
//       -append "console=ttyS0 panic=-1 quiet" -nographic -no-reboot
//   and again with `-cpu qemu64`, which has no five-level paging (that run
//   was stopped after the epoll rows).
// * NOT `container --arch amd64`. That runs the x86-64 binary under Rosetta
//   on the same aarch64 kernel, and Rosetta renders the ABI only partly: it
//   delivers 12-byte packed events, but passes `maxevents` through to the
//   aarch64 kernel (largest 134217727, not 178956970) and screens no buffer
//   at all (UINTPTR_MAX is not EFAULT). Its page size and getrandom cap agree
//   with the table above.
//
// Build: gcc -O1 -Wall -o /tmp/arch architecture-facts-linux.c
#define _GNU_SOURCE
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/auxv.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/utsname.h>
#include <unistd.h>

static int ep;
static struct epoll_event scratch[4];

// -1 and errno for a failure, otherwise the count.
static int wait_at(uintptr_t address, int maxevents) {
    errno = 0;
    int r = epoll_wait(ep, (struct epoll_event *)address, maxevents, 0);
    return r;
}

static int is_einval(int maxevents) {
    int r = wait_at((uintptr_t)scratch, maxevents);
    if (r < 0 && errno != EINVAL) {
        printf("UNEXPECTED maxevents=%d errno=%d (%s)\n", maxevents, errno, strerror(errno));
        exit(2);
    }
    return r < 0;
}

static int is_efault(uintptr_t address, int maxevents) {
    int r = wait_at(address, maxevents);
    if (r < 0 && errno != EFAULT) {
        printf("UNEXPECTED address=0x%lx maxevents=%d errno=%d (%s)\n", (unsigned long)address, maxevents, errno,
               strerror(errno));
        exit(2);
    }
    return r < 0;
}

// Largest address whose `maxevents`-event range is not EFAULT, or 0 when not
// even UINTPTR_MAX is refused (Rosetta screens nothing). Monotone:
// a low address passes (nothing is ready, so nothing is copied), and every
// address above TASK_SIZE_MAX - extent fails the screen.
static uintptr_t highest_unscreened(int maxevents) {
    uintptr_t lo = (uintptr_t)scratch, hi = UINTPTR_MAX;
    if (is_efault(lo, maxevents)) {
        printf("UNEXPECTED: a real buffer is EFAULT\n");
        exit(2);
    }
    if (!is_efault(hi, maxevents))
        return 0;
    while (hi - lo > 1) {
        uintptr_t mid = lo + (hi - lo) / 2;
        if (is_efault(mid, maxevents))
            hi = mid;
        else
            lo = mid;
    }
    return lo;
}

int main(void) {
    alarm(3600);
    struct utsname u;
    uname(&u);
    printf("uname: %s %s %s\n", u.sysname, u.release, u.machine);
#if defined(__x86_64__)
    printf("compiled for: __x86_64__\n");
#elif defined(__aarch64__)
    printf("compiled for: __aarch64__\n");
#else
    printf("compiled for: other\n");
#endif

    // --- page size ---
    printf("sysconf(_SC_PAGESIZE) = %ld\n", sysconf(_SC_PAGESIZE));
    printf("getauxval(AT_PAGESZ) = %lu\n", getauxval(AT_PAGESZ));

    // --- epoll_event ---
    printf("sizeof(struct epoll_event) = %zu, offsetof(data) = %zu\n", sizeof(struct epoll_event),
           offsetof(struct epoll_event, data));

    ep = epoll_create1(0);
    if (ep < 0) {
        perror("epoll_create1");
        return 1;
    }

    // maxevents: strided sweep for monotonicity, then bisect.
    long flips = 0;
    int prev = is_einval(1);
    long samples = 0;
    for (long m = 1; m <= INT_MAX; m += 65521) {
        int now = is_einval((int)m);
        samples++;
        if (now != prev)
            flips++;
        prev = now;
    }
    int top = is_einval(INT_MAX);
    printf("maxevents sweep: %ld samples on stride 65521 over [1, INT_MAX], %ld flips; 0 -> %s, -1 -> %s, INT_MAX -> %s\n",
           samples, flips, is_einval(0) ? "EINVAL" : "ok", is_einval(-1) ? "EINVAL" : "ok", top ? "EINVAL" : "ok");
    int lo = 1, hi = INT_MAX;
    while (hi - lo > 1) {
        int mid = lo + (hi - lo) / 2;
        if (is_einval(mid))
            hi = mid;
        else
            lo = mid;
    }
    int bad = 0;
    for (int m = lo - 4096; m <= lo + 4096 && m > 0; m++)
        if (is_einval(m) != (m > lo))
            bad++;
    printf("largest maxevents not EINVAL = %d; INT_MAX / sizeof = %zu; %d disagreements in [%d, %d]\n", lo,
           (size_t)INT_MAX / sizeof(struct epoll_event), bad, lo - 4096, lo + 4096);

    // access_ok extent.
    uintptr_t a1 = highest_unscreened(1), a2 = highest_unscreened(2), a3 = highest_unscreened(3);
    if (a1 == 0)
        printf("no buffer screen observed: UINTPTR_MAX is not EFAULT for one event\n");
    else
        printf("highest buffer not EFAULT: 1 event 0x%lx, 2 events 0x%lx, 3 events 0x%lx; stride %lu and %lu; TASK_SIZE_MAX "
           "= 0x%lx\n",
           (unsigned long)a1, (unsigned long)a2, (unsigned long)a3, (unsigned long)(a1 - a2), (unsigned long)(a2 - a3),
           (unsigned long)(a1 + (a1 - a2)));

    // Delivered bytes.
    int efd = eventfd(1, 0);
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN;
    ev.data.u64 = 0x1122334455667788ULL;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, efd, &ev) != 0) {
        perror("epoll_ctl");
        return 1;
    }
    unsigned char buf[64];
    memset(buf, 0xAA, sizeof buf);
    int n = epoll_wait(ep, (struct epoll_event *)buf, 4, 0);
    int last = -1;
    for (int i = 0; i < (int)sizeof buf; i++)
        if (buf[i] != 0xAA)
            last = i;
    printf("delivered %d event(s); last byte written at offset %d; bytes:", n, last);
    for (int i = 0; i < 24; i++)
        printf(" %02x", buf[i]);
    printf("\n");
    epoll_ctl(ep, EPOLL_CTL_DEL, efd, NULL);
    close(efd);

    // --- getrandom cap ---
    long page = sysconf(_SC_PAGESIZE);
    size_t mapLen = (size_t)1 << 31;
    mapLen += 16 * (size_t)page;
    unsigned char *big = mmap(NULL, mapLen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
    if (big == MAP_FAILED) {
        perror("mmap");
        return 1;
    }
    size_t requests[] = {INT_MAX, (size_t)INT_MAX + 1, SIZE_MAX};
    for (int i = 0; i < 3; i++) {
        errno = 0;
        long r = syscall(SYS_getrandom, big, requests[i], 0);
        printf("getrandom(%zu) -> %ld errno=%d; INT_MAX & ~(page-1) = %ld\n", requests[i], r, r < 0 ? errno : 0,
               (long)INT_MAX & ~(page - 1));
    }
    return 0;
}
