// Where Darwin leaves a directory description's position after each
// getdirentries call, and what `read(2)` then answers.
//
// Darwin answers a directory read at position INT64_MAX with 0 rather than
// EISDIR (transfer-counts-position.c). A description a scan has moved is at
// whatever cookie the filesystem's readdir left it, so this reads directories
// of 0, 1, 2, 5, 40 and 300 entries with `__getdirentries64`, and after each
// call prints `lseek(fd, 0, SEEK_CUR)` and what `read(fd, buf, 1)` and
// `read(fd, buf, 0)` answer. Last, it asks what getdirentries itself answers
// from position INT64_MAX.
//
// Results, 2026-09-26, Darwin 27.0.0 arm64, buffers of 1100, 2200 and 8192
// bytes:
//
// * APFS: 0 at the start. Partway through a scan, the number of calls made so
//   far in the high 32 bits and the number of entries returned so far in the
//   low 32 (300 entries, 2200-byte buffer: 0x1_00000037, 0x2_0000006d, ...;
//   1100-byte buffer: 0x1_0000001b, 0x2_00000036, ...). 0x7FFFFFFF (INT_MAX)
//   once the scan is done, whatever the size. A getdirentries call from
//   INT64_MAX is EAGAIN (35).
// * HFS+ (a disk image made with `hdiutil create -fs HFS+`, for comparison):
//   0x04000000 plus the entries returned so far, and a getdirentries call from
//   INT64_MAX returns 0.
//
// Every read answered EISDIR, at both counts and every position: a scan never
// leaves the description at INT64_MAX.
//
// Build: `nix develop -c clang -O1 -Wall -o /tmp/tcdd
// transfer-counts-darwin-directory.c`. argv[1] is a scratch directory on the
// filesystem under test; argv[2], optional, the getdirentries buffer size.
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
ssize_t __getdirentries64(int, void *, size_t, off_t *);
static char buf[65536];
static size_t bufsize = 2200;
static void probe_read(int fd, const char *when) {
    off_t cur = lseek(fd, 0, SEEK_CUR);
    char b[8];
    errno = 0; long r = read(fd, b, 1); int e = errno;
    errno = 0; long r0 = read(fd, b, 0); int e0 = errno;
    printf("  %-8s pos=0x%016llx read1=%ld/%d read0=%ld/%d\n", when, (unsigned long long)cur, r, r < 0 ? e : 0, r0, r0 < 0 ? e0 : 0);
}
int main(int argc, char **argv) {
    const char *root = argv[1];
    if (argc > 2) bufsize = (size_t)atol(argv[2]);
    int sizes[] = {0, 1, 2, 5, 40, 300};
    for (int s = 0; s < 6; s++) {
        char d[1024]; snprintf(d, sizeof d, "%s/d%d", root, sizes[s]);
        mkdir(d, 0755);
        for (int i = 0; i < sizes[s]; i++) { char f[1100]; snprintf(f, sizeof f, "%s/entry%03d", d, i); close(open(f, O_CREAT | O_WRONLY, 0644)); }
        int fd = open(d, O_RDONLY | O_DIRECTORY);
        printf("dir of %d entries\n", sizes[s]);
        probe_read(fd, "start");
        off_t base = 0; int calls = 0;
        for (;;) {
            ssize_t n = __getdirentries64(fd, buf, bufsize, &base);
            if (n < 0) { printf("  getdirentries errno=%d\n", errno); break; }
            char when[32]; snprintf(when, sizeof when, "call%d:%zd", ++calls, n);
            probe_read(fd, when);
            if (n == 0) break;
        }
        // And from INT64_MAX: what a scan does there, and lseek beyond.
        off_t got = lseek(fd, INT64_MAX, SEEK_SET);
        ssize_t n = __getdirentries64(fd, buf, sizeof buf, &base);
        printf("  lseek(INT64_MAX)=0x%llx getdirentries=%zd/%d\n", (unsigned long long)got, n, n < 0 ? errno : 0);
        close(fd);
    }
    return 0;
}
