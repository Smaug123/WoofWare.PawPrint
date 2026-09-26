// Companion to transfer-counts.c: the questions its grid raised.
//
// * POSITION: `read`/`write` after `lseek(SEEK_SET)` to a position near
//   INT64_MAX, and `pread`/`pwrite` at such an offset, on a regular file and
//   a directory, with a low mapped buffer, NULL and UINTPTR_MAX: where does
//   position + count leaving the signed range sit against EFAULT, EISDIR and
//   end-of-file?
// * RAW (Darwin): the count screen through `syscall(SYS_read, ...)` as well as
//   libc, to say whose check it is.
// * GETCWD: `getcwd` with capacities across the size_t range, into a mapped
//   buffer and an unmapped one.
//
// Results, 2026-09-26, on Linux 6.18.5 aarch64 (4 KiB pages, 48-bit VA, in
// Apple's `container`, files on /dev/shm's tmpfs) and Darwin 27.0.0 arm64
// (APFS). Positions 0, 5, 1000, 2^62, INT64_MAX - 10, - 4, - 1 and INT64_MAX,
// counts 0, 1, 4, 5, 10, 11, 12, INT_MAX, 2^31 and 2^32 - 1.
//
// Linux: position + count > INT64_MAX is EINVAL, for read and write at the
// description's position (after lseek) exactly as for pread and pwrite at the
// argument, and for a directory as for a file. It follows the buffer screen
// (UINTPTR_MAX is EFAULT at every position and count) and precedes everything
// the object does: a directory's EISDIR, end-of-file's 0, and the fault a NULL
// buffer takes at the copy. A count of zero never overflows. The count checked
// is the one asked for, not the clamped one (CLAMPPOS): at position
// INT64_MAX - 0x7FFFF000, 0x7FFFF000 bytes read 0 and 0x7FFFF001 are EINVAL,
// though both clamp to 0x7FFFF000.
//
// Darwin has no such check: a read or pread past end-of-file is 0 at every
// position, and a write or pwrite at 2^62 or beyond is EFBIG. Two answers there
// that no count explains: a zero-length write at INT64_MAX is EFBIG, and a
// directory read or pread at position INT64_MAX is 0 rather than EISDIR.
//
// GETCWD: nothing new at large capacities. Every capacity from the path's
// length + 1 to SIZE_MAX reports the path on both, and an unmapped destination
// is EFAULT at each on Linux; below that, 0 is EINVAL and the rest ERANGE, as
// UnixPathResolution.getcwd already says.
//
// Build: `gcc -O1 -Wall -o /tmp/tcp transfer-counts-position.c` on Linux,
// `nix develop -c clang -O1 -Wall -Wno-deprecated-declarations -o /tmp/tcp
// transfer-counts-position.c` on Darwin. argv[1] is a scratch directory (a
// tmpfs on Linux: /dev/shm).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/utsname.h>
#include <unistd.h>

static char dir[512];
static char *path(const char *name) {
    static char p[1024];
    snprintf(p, sizeof p, "%s/%s", dir, name);
    return p;
}

static void refresh(void) {
    int fd = open(path("data"), O_WRONLY | O_CREAT | O_TRUNC, 0644);
    write(fd, "hello", 5);
    close(fd);
    mkdir(path("d"), 0755);
}

static const char *ops[] = {"read", "write", "pread", "pwrite"};

int main(int argc, char **argv) {
    alarm(120);
    signal(SIGPIPE, SIG_IGN);
    snprintf(dir, sizeof dir, "%s", argc > 1 ? argv[1] : "/tmp");
    struct utsname u;
    uname(&u);
    printf("%s %s %s\n", u.sysname, u.release, u.machine);

    // A low mapping, so that `map + count` stays below the address limit for
    // every count up to 2^32.
    long page = sysconf(_SC_PAGESIZE);
    char *m = MAP_FAILED;
    for (uintptr_t hint = 0x10000000; m == MAP_FAILED && hint < 0x100000000ull; hint += 0x10000000) {
        m = mmap((void *)hint, (size_t)page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if (m != MAP_FAILED && (uintptr_t)m != hint) { munmap(m, (size_t)page); m = MAP_FAILED; }
    }
    // Darwin arm64 reserves the low 4 GiB (__PAGEZERO), and screens no buffer
    // up front anyway, so an ordinary mapping does there.
    if (m == MAP_FAILED) m = mmap(NULL, (size_t)page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    printf("map=%p\n", (void *)m);
    struct { const char *name; void *p; } bufs[] = {{"map", m}, {"null", NULL}, {"wild", (void *)UINTPTR_MAX}};

    off_t poss[] = {0, 5, 1000, (off_t)1 << 62, INT64_MAX - 10, INT64_MAX - 4, INT64_MAX - 1, INT64_MAX};
    uint64_t ns[] = {0, 1, 4, 5, 10, 11, 12, 0x7FFFFFFFull, 0x80000000ull, 0xFFFFFFFFull};
    for (int op = 0; op < 4; op++)
        for (int target = 0; target < 2; target++) {
            if (target == 1 && (op == 1 || op == 3)) continue; // a directory is never open for writing
            for (size_t b = 0; b < 3; b++)
                for (size_t i = 0; i < sizeof poss / sizeof poss[0]; i++)
                    for (size_t j = 0; j < sizeof ns / sizeof ns[0]; j++) {
                        refresh();
                        int writing = op == 1 || op == 3;
                        int fd = target == 0 ? open(path("data"), writing ? O_WRONLY : O_RDONLY) : open(path("d"), O_RDONLY);
                        long r;
                        int e;
                        errno = 0;
                        if (op < 2) {
                            off_t got = lseek(fd, poss[i], SEEK_SET);
                            if (got != poss[i]) {
                                printf("POS %-6s %-4s %-4s pos=0x%016llx lseek failed errno=%d\n", ops[op],
                                       target ? "dir" : "file", bufs[b].name, (unsigned long long)poss[i], errno);
                                close(fd);
                                break;
                            }
                            errno = 0;
                            r = op == 0 ? read(fd, bufs[b].p, (size_t)ns[j]) : write(fd, bufs[b].p, (size_t)ns[j]);
                        } else {
                            r = op == 2 ? pread(fd, bufs[b].p, (size_t)ns[j], poss[i])
                                        : pwrite(fd, bufs[b].p, (size_t)ns[j], poss[i]);
                        }
                        e = errno;
                        off_t after = lseek(fd, 0, SEEK_CUR);
                        close(fd);
                        printf("POS %-6s %-4s %-4s pos=0x%016llx count=0x%llx -> ret=%ld errno=%d after=0x%llx\n", ops[op],
                               target ? "dir" : "file", bufs[b].name, (unsigned long long)poss[i],
                               (unsigned long long)ns[j], r, r < 0 ? e : 0, (unsigned long long)after);
                    }
        }

    // Whether position + count is checked against the count asked for or the
    // count after the per-call clamp: a count one past the clamp, at the
    // position where the clamped count would just fit.
    {
        off_t base = INT64_MAX - 0x7FFFF000ll;
        struct { off_t pos; uint64_t n; } cases[] = {
            {base, 0x7FFFF000ull}, {base, 0x7FFFF001ull}, {base, 0x7FFFFFFFull}, {base, 0x80000000ull},
            {base + 1, 0x7FFFF000ull}, {base + 1, 0x7FFFEFFFull},
        };
        for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++)
            for (int op = 0; op < 4; op++) {
                refresh();
                int writing = op == 1 || op == 3;
                int fd = open(path("data"), writing ? O_WRONLY : O_RDONLY);
                long r;
                errno = 0;
                if (op < 2) {
                    lseek(fd, cases[i].pos, SEEK_SET);
                    errno = 0;
                    r = op == 0 ? read(fd, m, (size_t)cases[i].n) : write(fd, NULL, (size_t)cases[i].n);
                } else {
                    r = op == 2 ? pread(fd, m, (size_t)cases[i].n, cases[i].pos) : pwrite(fd, NULL, (size_t)cases[i].n, cases[i].pos);
                }
                int e = errno;
                close(fd);
                printf("CLAMPPOS %-6s pos=0x%016llx count=0x%llx -> ret=%ld errno=%d\n", ops[op], (unsigned long long)cases[i].pos,
                       (unsigned long long)cases[i].n, r, r < 0 ? e : 0);
            }
    }

#ifndef __linux__
    {
        uint64_t cs[] = {0x7FFFFFFFull, 0x80000000ull, 0xFFFFFFFFull, 0x100000000ull, UINT64_MAX};
        for (size_t i = 0; i < 5; i++) {
            refresh();
            int fd = open(path("data"), O_RDONLY);
            lseek(fd, 0, SEEK_END);
            errno = 0;
            long r = syscall(SYS_read, fd, m, (size_t)cs[i]);
            int e = errno;
            errno = 0;
            long r2 = syscall(SYS_read, 987, m, (size_t)cs[i]);
            int e2 = errno;
            errno = 0;
            long r3 = syscall(SYS_write, 987, m, (size_t)cs[i]);
            int e3 = errno;
            close(fd);
            printf("RAW count=0x%llx read(eof)=%ld/%d read(closed)=%ld/%d write(closed)=%ld/%d\n", (unsigned long long)cs[i], r,
                   r < 0 ? e : 0, r2, r2 < 0 ? e2 : 0, r3, r3 < 0 ? e3 : 0);
        }
    }
#endif

    {
        uint64_t caps[] = {0, 1, 2, 3, 100, 0x7FFFFFFFull, 0x80000000ull, 0xFFFFFFFFull, 0x100000000ull,
                           0x0001000000000000ull, 0x7FFFFFFFFFFFFFFFull, 0x8000000000000000ull, UINT64_MAX};
        char *hole = mmap(NULL, (size_t)page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        munmap(hole, (size_t)page);
        chdir(dir);
        for (size_t i = 0; i < sizeof caps / sizeof caps[0]; i++) {
            memset(m, 0xAA, 64);
            errno = 0;
            char *r = getcwd(m, (size_t)caps[i]);
            int e = errno;
            printf("GETCWD map  cap=0x%016llx -> %s errno=%d first=0x%02x\n", (unsigned long long)caps[i], r ? "buf" : "NULL",
                   r ? 0 : e, (unsigned char)m[0]);
#ifdef __linux__
            // Darwin's getcwd stores from user space and dies on an unmapped
            // destination (see GetCwdDestinationFault), so only Linux is asked.
            errno = 0;
            r = getcwd(hole, (size_t)caps[i]);
            e = errno;
            printf("GETCWD hole cap=0x%016llx -> %s errno=%d\n", (unsigned long long)caps[i], r ? "buf" : "NULL", r ? 0 : e);
            errno = 0;
            long rr = syscall(SYS_getcwd, m, (size_t)caps[i]);
            e = errno;
            printf("GETCWD raw  cap=0x%016llx -> %ld errno=%d\n", (unsigned long long)caps[i], rr, rr < 0 ? e : 0);
#endif
        }
    }
    unlink(path("data"));
    rmdir(path("d"));
    return 0;
}
