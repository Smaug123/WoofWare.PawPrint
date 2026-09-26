// Directory-read probe: what getdents answers on every descriptor kind that is
// not a directory, what it answers for a removed directory at every kind of
// position, and the lseek arithmetic on a directory descriptor.
//
// Usage: directory-descriptors <base-dir>
// Build as directory-layout.c.
//
// Measured 2026-09-26 on Darwin 27.0.0 (arm64, APFS) and Linux 6.18.5 (arm64,
// tmpfs on /dev/shm); transcribed in WoofWare.PosixKernel.Test/TestDirectoryDescription.fs
// and checked against the host by TestDirectoryReadAgainstHost.fs.
#include "directory-common.h"
#include <netinet/in.h>
#include <stdint.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static unsigned char big[1 << 16];
#define BUF 4096

static void report(const char *what, int fd) {
    errno = 0;
    long n = rawgetdents(fd, big, BUF, NULL);
    printf("KIND %s ret=%ld errno=%d\n", what, n, n < 0 ? errno : 0);
}

int main(int argc, char **argv) {
    alarm(60);
    char root[4096];
    snprintf(root, sizeof root, "%s/dirprobe-seek", argv[1]);
    rmrf(root);
    mkdir(root, 0755);
    printf("FLAVOUR %s fs=%s\n", FLAVOUR, fsname(root));
    char f[4096]; snprintf(f, sizeof f, "%s/file", root); touch(f);

    int rd = open(f, O_RDONLY); report("regular_rdonly", rd); close(rd);
    int wr = open(f, O_WRONLY); report("regular_wronly", wr); close(wr);
    int p[2]; pipe(p); report("pipe_read", p[0]); report("pipe_write", p[1]); close(p[0]); close(p[1]);
    int us = socket(AF_UNIX, SOCK_STREAM, 0); report("unix_stream_socket", us); close(us);
    int ud = socket(AF_UNIX, SOCK_DGRAM, 0); report("unix_dgram_socket", ud); close(ud);
    int is = socket(AF_INET, SOCK_STREAM, 0); report("inet_stream_socket", is); close(is);
    int id = socket(AF_INET, SOCK_DGRAM, 0); report("inet_dgram_socket", id); close(id);
#ifdef __linux__
    int ep = epoll_create1(0); report("epoll", ep); close(ep);
#else
    int kq = kqueue(); report("kqueue", kq); close(kq);
#endif
    report("closed_fd", 999);
    report("negative_fd", -1);
    // a non-directory with a zero-size buffer, and a bad fd with a NULL buffer
    rd = open(f, O_RDONLY);
    errno = 0; long n0 = rawgetdents(rd, big, 0, NULL);
    printf("KIND regular_size0 ret=%ld errno=%d\n", n0, n0 < 0 ? errno : 0);
    errno = 0; n0 = rawgetdents(rd, NULL, BUF, NULL);
    printf("KIND regular_nullbuf ret=%ld errno=%d\n", n0, n0 < 0 ? errno : 0);
    close(rd);
    errno = 0; n0 = rawgetdents(999, NULL, 0, NULL);
    printf("KIND closed_nullbuf_size0 ret=%ld errno=%d\n", n0, n0 < 0 ? errno : 0);

    // Removed directory, at each kind of position.
    const char *how[] = {"fresh", "partial", "set5", "set2^62", "setEOD", "drained", "set0_after_drain"};
    for (int h = 0; h < 7; h++) {
        char d[4096]; snprintf(d, sizeof d, "%s/gone%d", root, h); mkdir(d, 0755);
        for (int i = 0; i < 4; i++) { char q[4096]; snprintf(q, sizeof q, "%s/n%d", d, i); touch(q); }
        int fd = open(d, O_RDONLY | O_DIRECTORY);
        if (h == 1) rawgetdents(fd, big, 64, NULL);
        if (h == 2) lseek(fd, 5, SEEK_SET);
        if (h == 3) lseek(fd, 1LL << 62, SEEK_SET);
        if (h == 4) lseek(fd, 0x7fffffff, SEEK_SET);
        if (h >= 5) while (rawgetdents(fd, big, BUF, NULL) > 0) {}
        long long before = lseek(fd, 0, SEEK_CUR);
        for (int i = 0; i < 4; i++) { char q[4096]; snprintf(q, sizeof q, "%s/n%d", d, i); unlink(q); }
        rmdir(d);
        if (h == 6) lseek(fd, 0, SEEK_SET);
        errno = 0;
        long n = rawgetdents(fd, big, BUF, NULL);
        int e = errno;
        long long after = lseek(fd, 0, SEEK_CUR);
        errno = 0;
        long long s = lseek(fd, 0, SEEK_SET);
        printf("REMOVED %s pos_before=%lld ret=%ld errno=%d pos_after=%lld lseek0=%lld errno=%d\n", how[h], before, n, n < 0 ? e : 0, after, s, s < 0 ? errno : 0);
        close(fd);
    }

    // lseek arithmetic on a directory descriptor.
    {
        char d[4096]; snprintf(d, sizeof d, "%s/arith", root); mkdir(d, 0755);
        int fd = open(d, O_RDONLY | O_DIRECTORY);
        long long sets[] = {0, 1, 5, 0x7fffffffLL, 1LL << 62, 0x7fffffffffffffffLL - 1, 0x7fffffffffffffffLL};
        long long curs[] = {0, 1, -1, 5, -5, -6, 1LL << 62, -(1LL << 62), 0x7fffffffffffffffLL, (long long)0x8000000000000000ULL};
        for (size_t i = 0; i < sizeof sets / sizeof sets[0]; i++)
            for (size_t j = 0; j < sizeof curs / sizeof curs[0]; j++) {
                lseek(fd, 0, SEEK_SET);
                errno = 0;
                long long a = lseek(fd, sets[i], SEEK_SET);
                int ea = errno;
                errno = 0;
                long long b = lseek(fd, curs[j], SEEK_CUR);
                int eb = errno;
                long long c = lseek(fd, 0, SEEK_CUR);
                printf("ARITH set=%lld -> %lld errno=%d | cur%+lld -> %lld errno=%d | now=%lld\n", sets[i], a, a < 0 ? ea : 0, curs[j], b, b < 0 ? eb : 0, c);
            }
        long long negs[] = {-1, -2, (long long)0x8000000000000000ULL};
        for (size_t i = 0; i < 3; i++) {
            lseek(fd, 7, SEEK_SET);
            errno = 0;
            long long a = lseek(fd, negs[i], SEEK_SET);
            printf("ARITH set=%lld -> %lld errno=%d now=%lld\n", negs[i], a, a < 0 ? errno : 0, (long long)lseek(fd, 0, SEEK_CUR));
        }
        close(fd);
    }
    rmrf(root);
    return 0;
}
