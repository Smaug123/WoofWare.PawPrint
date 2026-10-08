// What getsockname(2) and getpeername(2) leave in the caller's length cell
// when the address copy faults, through a null destination and through an
// unmapped page, on a connected loopback TCP socket.
//
// Linux's move_addr_to_user stored the length after the copy until 6.17, and
// before it from 6.18 (commit 1fb0e471611d, "net: remove one stac/clac pair
// from move_addr_to_user()"); Darwin stores it only once the copy succeeds.
// Outputs beside this file, measured 2026-10-08:
//
// * darwin-27.0.0-arm64.txt: natively.
// * linux-6.18.5-aarch64.txt: Apple's `container`, gcc:14 image,
//     cc -Wall -O1 -o /tmp/p sockname-fault-length.c && /tmp/p
// * linux-6.12.111-x86_64.txt, linux-6.17.13-x86_64.txt,
//   linux-6.18.5-x86_64.txt: Debian trixie's and trixie-backports'
//   linux-image-*-amd64, booted under full-system emulation with this probe
//   linked statically (`gcc -static`) as the initramfs's /init, which brings
//   up the loopback interface itself and powers off when done:
//     qemu-system-x86_64 -m 1G -cpu max -kernel vmlinuz -initrd init.cpio.gz \
//       -append "console=ttyS0 panic=-1 quiet" -nographic -no-reboot
//   Not `container --arch amd64`, which is an aarch64 kernel under Rosetta.
#define _GNU_SOURCE
#include <errno.h>
#include <net/if.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#ifdef __linux__
#include <sys/reboot.h>
#endif
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>

static void lo_up(void) {
    int s = socket(AF_INET, SOCK_DGRAM, 0);
    struct ifreq ifr;
    memset(&ifr, 0, sizeof ifr);
    strcpy(ifr.ifr_name, "lo");
    if (ioctl(s, SIOCGIFFLAGS, &ifr) == 0) {
        ifr.ifr_flags |= IFF_UP;
        ioctl(s, SIOCSIFFLAGS, &ifr);
    }
    close(s);
}

typedef int (*namefn)(int, struct sockaddr *, socklen_t *);

static void row(const char *call, namefn f, const char *dest, void *addr, int fd, int declared) {
    int cell = declared;
    errno = 0;
    int r = f(fd, (struct sockaddr *)addr, (socklen_t *)&cell);
    int e = r == 0 ? 0 : errno;
    printf("%-11s %-8s declared=%-10d -> r=%d errno=%d cell=%d\n", call, dest, declared, r, e, cell);
}

int main(void) {
    struct utsname u;
    uname(&u);
    printf("uname: %s %s %s\n", u.sysname, u.release, u.machine);
    lo_up();

    int l = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(l, (struct sockaddr *)&a, sizeof a) != 0) perror("bind");
    if (listen(l, 4) != 0) perror("listen");
    socklen_t al = sizeof a;
    getsockname(l, (struct sockaddr *)&a, &al);
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(c, (struct sockaddr *)&a, sizeof a) != 0) perror("connect");

    // A page that is mapped and then unmapped, so its address faults.
    void *page = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    munmap(page, 4096);

    int lengths[] = { 1, 7, 13, 16, 100, 4096 };
    for (unsigned i = 0; i < sizeof lengths / sizeof lengths[0]; i++) {
        row("getsockname", getsockname, "null", NULL, c, lengths[i]);
        row("getsockname", getsockname, "unmapped", page, c, lengths[i]);
        row("getpeername", getpeername, "null", NULL, c, lengths[i]);
        row("getpeername", getpeername, "unmapped", page, c, lengths[i]);
    }
    fflush(stdout);
#ifdef __linux__
    if (getpid() == 1) reboot(RB_POWER_OFF);
#endif
    return 0;
}
