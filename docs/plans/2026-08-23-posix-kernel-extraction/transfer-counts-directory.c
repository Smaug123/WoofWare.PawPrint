// Where a directory description is positioned partway through a scan, and
// whether `read(2)` on it then answers the position + count check or EISDIR.
//
// Linux answers EINVAL for a read whose position + count passes INT64_MAX,
// ahead of a directory's EISDIR (transfer-counts-position.c). A description a
// scan has moved is at whatever position the filesystem's readdir left it, so
// this reads a directory of N entries a few entries per getdents64 call, and after
// each call (and after the end) prints `lseek(fd, 0, SEEK_CUR)` and what
// `read(fd, buf, count)` answers for counts 1, 2^31 and 2^47.
//
// The buffer holds a dirent64 with a 256-byte name, so each call returns
// several short entries: the positions below are where each call left off.
//
// Results, 2026-09-26, Linux 6.18.5 aarch64 (Apple's `container`), for
// directories of 0, 1, 2, 5 and 40 entries:
//
// * tmpfs (/dev/shm): 0 at the start; partway through a scan, the offset of
//   the next entry, small and counting down in creation order (35, 26, 17, 8
//   for 40 entries); and 2147483647 (INT_MAX) once the scan has finished,
//   whatever the size. No scanned tmpfs directory was positioned above
//   INT_MAX in any row.
// * ext4 (the container's root, for comparison only; not modelled): byte
//   offsets into the directory block (136, 280, ...), and 4096 at the end of
//   a one-block directory.
//
// Every read answered EISDIR at counts 1 and 2^31, at every position: none
// is within 2^31 of INT64_MAX. (2^47 was EFAULT, the static buffer sitting
// high enough that its range passes 2^48.)
//
// Build: `gcc -O1 -Wall -o /tmp/tcd transfer-counts-directory.c`; argv[1] is
// a scratch directory on the filesystem under test.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

static void report(int fd, const char *when) {
    off_t pos = lseek(fd, 0, SEEK_CUR);
    int seekErr = pos < 0 ? errno : 0;
    static char buf[64];
    uint64_t counts[] = {1, 0x80000000ull, 0x800000000000ull};
    printf("%-14s pos=%lld(err %d)", when, (long long)pos, seekErr);
    for (int i = 0; i < 3; i++) {
        // lseek again each time: a failed read must not move it, but check.
        errno = 0;
        long r = read(fd, buf, (size_t)counts[i]);
        printf("  read(%llx)=%ld/%d", (unsigned long long)counts[i], r, r < 0 ? errno : 0);
    }
    printf("  pos-after=%lld\n", (long long)lseek(fd, 0, SEEK_CUR));
}

int main(int argc, char **argv) {
    alarm(60);
    const char *root = argc > 1 ? argv[1] : "/tmp";
    int sizes[] = {0, 1, 2, 5, 40};
    for (size_t s = 0; s < sizeof sizes / sizeof sizes[0]; s++) {
        char dir[512];
        snprintf(dir, sizeof dir, "%s/d%d", root, sizes[s]);
        mkdir(dir, 0755);
        for (int i = 0; i < sizes[s]; i++) {
            char p[600];
            snprintf(p, sizeof p, "%s/entry%03d", dir, i);
            close(open(p, O_CREAT | O_WRONLY, 0644));
        }
        int fd = open(dir, O_RDONLY | O_DIRECTORY);
        printf("== %d entries\n", sizes[s]);
        report(fd, "start");
        // One entry per call: a buffer just large enough for one dirent64.
        char dbuf[sizeof(struct dirent64) + 16];
        for (int step = 0; step < sizes[s] + 4; step++) {
            long n = syscall(SYS_getdents64, fd, dbuf, sizeof dbuf);
            char when[32];
            if (n > 0) {
                struct dirent64 *d = (struct dirent64 *)dbuf;
                snprintf(when, sizeof when, "after %.8s", d->d_name);
            } else {
                snprintf(when, sizeof when, "getdents=%ld", n);
            }
            report(fd, when);
            if (n <= 0) break;
        }
        close(fd);
    }
    return 0;
}
