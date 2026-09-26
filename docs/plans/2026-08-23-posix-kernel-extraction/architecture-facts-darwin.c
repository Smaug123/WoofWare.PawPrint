// The Darwin facts this library models as independent of the architecture,
// asked of an arm64 process and of an x86-64 one, so that a fact that does
// depend on it shows up as a difference between the two runs:
//
// * the page size, from `getpagesize`, `sysconf`, `vm_page_size` and the
//   `hw.pagesize` sysctl;
// * `sizeof(struct kevent)`, and whether `kevent`'s event count has any upper
//   bound (a strided sweep over [1, INT_MAX] on an empty kqueue with a zero
//   timeout, and INT_MAX itself);
// * `getentropy`'s cap (255, 256 and 257 bytes);
// * the socket address sizes (`sockaddr_storage`, `sockaddr_in`,
//   `sockaddr_in6`, `sockaddr_un`);
// * that `read` screens no buffer up front: a read at end of file into the
//   last address there is.
//
// Measured on this machine (Darwin 27.0.0, Apple silicon) natively and under
// Rosetta. There is no Intel Mac to hand, and macOS 27 runs on none, so the
// x86-64 rows are Rosetta's.
//
// Build: clang -Wall -arch arm64 -o arch-arm64 architecture-facts-darwin.c
//        clang -Wall -arch x86_64 -o arch-x86_64 architecture-facts-darwin.c
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <mach/mach.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/event.h>
#include <sys/random.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <unistd.h>

static int sysctl_int(const char *name) {
    int v = -1;
    size_t len = sizeof v;
    if (sysctlbyname(name, &v, &len, NULL, 0) != 0)
        return -1;
    return v;
}

int main(void) {
    alarm(300);
    struct utsname u;
    uname(&u);
    printf("uname: %s %s %s; sysctl.proc_translated = %d\n", u.sysname, u.release, u.machine,
           sysctl_int("sysctl.proc_translated"));

    printf("getpagesize() = %d, sysconf(_SC_PAGESIZE) = %ld, vm_page_size = %lu, hw.pagesize = %d\n", getpagesize(),
           sysconf(_SC_PAGESIZE), (unsigned long)vm_page_size, sysctl_int("hw.pagesize"));

    printf("sizeof(struct kevent) = %zu, sizeof(struct kevent64_s) = %zu\n", sizeof(struct kevent),
           sizeof(struct kevent64_s));

    int kq = kqueue();
    struct kevent out[4];
    struct timespec zero = {0, 0};
    long samples = 0, failures = 0;
    for (long n = 1; n <= INT_MAX; n += 65521) {
        errno = 0;
        int r = kevent(kq, NULL, 0, out, (int)n, &zero);
        samples++;
        if (r != 0) {
            failures++;
            if (failures < 5)
                printf("kevent nevents=%ld -> %d errno=%d\n", n, r, errno);
        }
    }
    errno = 0;
    int rmax = kevent(kq, NULL, 0, out, INT_MAX, &zero);
    printf("kevent sweep: %ld samples on stride 65521 over [1, INT_MAX], %ld not answering 0; INT_MAX -> %d errno=%d\n",
           samples, failures, rmax, rmax < 0 ? errno : 0);

    unsigned char buf[512];
    size_t lens[] = {255, 256, 257};
    for (int i = 0; i < 3; i++) {
        errno = 0;
        int r = getentropy(buf, lens[i]);
        printf("getentropy(%zu) -> %d errno=%d\n", lens[i], r, r < 0 ? errno : 0);
    }

    printf("sizeof sockaddr_storage = %zu, sockaddr_in = %zu, sockaddr_in6 = %zu, sockaddr_un = %zu\n",
           sizeof(struct sockaddr_storage), sizeof(struct sockaddr_in), sizeof(struct sockaddr_in6),
           sizeof(struct sockaddr_un));

    int fd = open("/dev/null", O_RDONLY);
    errno = 0;
    ssize_t r = read(fd, (void *)UINTPTR_MAX, 5);
    printf("read(/dev/null, UINTPTR_MAX, 5) -> %zd errno=%d\n", r, r < 0 ? errno : 0);
    return 0;
}
