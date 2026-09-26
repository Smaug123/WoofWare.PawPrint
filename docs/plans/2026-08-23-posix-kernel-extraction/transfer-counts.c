// What `read`, `pread`, `write` and `pwrite` do with a transfer count, which
// the kernel takes as a `size_t`: where a count too large to transfer is
// answered, relative to the descriptor, the buffer and end-of-file; whether a
// count past the per-call limit is clamped or refused; and whether the buffer
// screen covers the whole count or the clamped one.
//
// Sections:
//
// * GRID: one row per (op, object, buffer, count), printing the return value
//   and errno. Objects are those the model knows (a regular file at EOF and
//   with data, the wrong access mode, a closed descriptor, a directory, a
//   socket event port, an unconnected TCP socket, a pipe's read end at EOF and
//   write end) plus /dev/null, whose write touches no buffer. Buffers are one
//   mapped page followed by a PROT_NONE page ("map"), NULL, an unmapped page
//   inside user space ("hole"), the last page below the aarch64 48-bit limit
//   ("top"), and UINTPTR_MAX ("wild"). Counts are listed in `counts[]`.
// * EDGE: for each op, a bisection over the whole size_t range of the largest
//   count whose answer is not an error, against a zero-byte transfer (a file
//   at EOF, /dev/null for writes) through the "map" buffer, after a strided
//   sweep that checks the answer is monotone. On a kernel whose screen covers
//   the full count, map + edge is its address limit.
// * CLAMP: the most one call moves. `write(/dev/null, map, n)` for n around
//   INT_MAX, and, with BIG=1, a read of /dev/zero into a mapping of 2 GiB +
//   a page (touched in full, so a Linux container wants `-m 6G`).
// * OVERFLOW: `pread`/`pwrite` at offsets near INT64_MAX, where offset +
//   count leaves the signed range.
//
// Results, 2026-09-26, on Linux 6.18.5 aarch64 (4 KiB pages, 48-bit VA, in
// Apple's `container`, `-m 6G`, files on /dev/shm's tmpfs) and Darwin 27.0.0
// arm64 (16 KiB pages, APFS). No x86-64 kernel was measured: `container
// --arch amd64` is Rosetta on the same aarch64 kernel.
//
// Darwin. Every count above INT_MAX (0x7FFFFFFF) is EINVAL, and it is the
// first answer of all: ahead of a closed descriptor's EBADF, the access mode's
// EBADF, a kqueue's ENXIO, a pipe's or socket's ESPIPE for pread/pwrite, a
// directory's EISDIR, end-of-file's 0 and every buffer. The raw syscall agrees
// with libc (transfer-counts-position.c, RAW), so the check is the kernel's.
// At or below INT_MAX nothing is clamped: /dev/null takes 0x7FFFFFFF in one
// write, /dev/zero and a 3 GiB sparse file give 0x7FFFFFFF in one read.
//
// Linux. No count is refused as a count. The buffer screen is
// `buffer + count <= 2^48` over the count as asked: NULL passes every count up
// to 2^48 and faults at 2^48 + 1, and the EDGE bisection through a page at
// 0xffff840f8000 found the last accepted count 0x7bf08000, exactly 2^48 less
// the address, for all four calls, with one flip in a 4096-point sweep. Only
// then is the count clamped to MAX_RW_COUNT, 0x7FFFF000: write(/dev/null, NULL,
// n) returns 0x7FFFF000 for every n from 0x7FFFF000 to 2^48, and a read of
// /dev/zero or of a sparse file longer than that returns 0x7FFFF000 for counts
// of 0x7FFFF000, 0x7FFFF001, INT_MAX and 2^31. The rest of the ladder is
// unchanged by a count: a closed descriptor or the wrong access mode is EBADF,
// an epoll descriptor's read or write EINVAL, pread/pwrite on a pipe, socket
// or epoll descriptor ESPIPE (and a negative offset EINVAL before all of
// them), whatever the count; a directory is EISDIR, a file at end-of-file
// reads 0 and an unconnected TCP socket reads ENOTCONN (0 at count 0) or
// writes EPIPE, for every count the screen admits, and EFAULT above it.
//
// Note that the screen depends on where the buffer is, and that a real mapping
// can be close to the limit: this one page, from an ordinary mmap, faults for
// any count above about 1.9 GiB.
//
// OVERFLOW (Linux): pread/pwrite whose offset + count exceeds INT64_MAX is
// EINVAL (offset INT64_MAX - 10: 10 bytes read 0 or write, 11 are EINVAL). On
// Darwin pread there reads 0 and pwrite is EFBIG at every offset from 2^62,
// including a zero-length pwrite at INT64_MAX. transfer-counts-position.c has
// the ladder around that check.
//
// Build: `gcc -O1 -Wall -o /tmp/tc transfer-counts.c` on Linux,
// `nix develop -c clang -O1 -Wall -o /tmp/tc transfer-counts.c` on Darwin.
// Run with the scratch directory as argv[1] (a tmpfs on Linux, /dev/shm).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

typedef enum { OP_READ, OP_PREAD, OP_WRITE, OP_PWRITE } op_t;
static const char *opName[] = {"read", "pread", "write", "pwrite"};

static long call(op_t op, int fd, void *buf, size_t n, off_t off) {
    switch (op) {
    case OP_READ: return read(fd, buf, n);
    case OP_PREAD: return pread(fd, buf, n, off);
    case OP_WRITE: return write(fd, buf, n);
    case OP_PWRITE: return pwrite(fd, buf, n, off);
    }
    return -2;
}

static char dir[512];
static char *path(const char *name) {
    static char p[1024];
    snprintf(p, sizeof p, "%s/%s", dir, name);
    return p;
}

// A fresh descriptor for each row, so no row can move another's offset.
typedef enum { T_EOF, T_DATA, T_WRONGMODE, T_CLOSED, T_DIR, T_PORT, T_SOCKET, T_PIPE_R_EOF, T_PIPE_W, T_DEVNULL, T_COUNT } target_t;
static const char *targetName[] = {"eof", "data", "wrongmode", "closed", "dir", "port", "socket", "pipeR-eof", "pipeW", "devnull"};

static int pipeOther = -1;
static int open_target(target_t t, op_t op) {
    int writing = op == OP_WRITE || op == OP_PWRITE;
    int fd = -1;
    switch (t) {
    case T_EOF:
        fd = open(path("eof"), writing ? O_WRONLY : O_RDONLY);
        if (!writing) lseek(fd, 0, SEEK_END);
        return fd;
    case T_DATA: return open(path("data"), writing ? O_WRONLY : O_RDONLY);
    case T_WRONGMODE: return open(path("data"), writing ? O_RDONLY : O_WRONLY);
    case T_CLOSED: return 987;
    case T_DIR: return open(dir, O_RDONLY);
    case T_PORT:
#ifdef __linux__
        return epoll_create1(0);
#else
        return kqueue();
#endif
    case T_SOCKET: return socket(AF_INET, SOCK_STREAM, 0);
    case T_PIPE_R_EOF: {
        int p[2];
        pipe(p);
        close(p[1]);
        return p[0];
    }
    case T_PIPE_W: {
        int p[2];
        pipe(p);
        fcntl(p[1], F_SETFL, O_NONBLOCK);
        pipeOther = p[0];
        return p[1];
    }
    case T_DEVNULL: return open("/dev/null", writing ? O_WRONLY : O_RDONLY);
    default: return -1;
    }
}
static void close_target(target_t t, int fd) {
    if (t != T_CLOSED && fd >= 0) close(fd);
    if (pipeOther >= 0) close(pipeOther), pipeOther = -1;
}

typedef struct { const char *name; uintptr_t addr; } buffer_t;
static buffer_t buffers[5];

static const uint64_t counts[] = {
    0, 1, 5,
    0x7FFFEFFFull, 0x7FFFF000ull, 0x7FFFF001ull,
    0x7FFFFFFEull, 0x7FFFFFFFull, 0x80000000ull, 0x80000001ull,
    0xFFFFFFFFull, 0x100000000ull, 0x100000001ull,
    0x00007FFFFFFFF000ull, 0x0000800000000000ull,
    0x0000FFFFFFFFF000ull, 0x0000FFFFFFFFFFFFull, 0x0001000000000000ull, 0x0001000000000001ull,
    0x00FFFFFFFFFFF000ull,
    0x7FFFFFFFFFFFFFFFull, 0x8000000000000000ull, 0x8000000000000001ull,
    0xFFFFFFFFFFFFFFFEull, 0xFFFFFFFFFFFFFFFFull,
};

static void refresh_files(void) {
    int fd = open(path("data"), O_WRONLY | O_CREAT | O_TRUNC, 0644);
    write(fd, "hello", 5);
    close(fd);
    fd = open(path("eof"), O_WRONLY | O_CREAT | O_TRUNC, 0644);
    write(fd, "hello", 5);
    close(fd);
}

static void row(op_t op, target_t t, buffer_t b, uint64_t n, off_t off) {
    refresh_files();
    int fd = open_target(t, op);
    errno = 0;
    long r = call(op, fd, (void *)b.addr, (size_t)n, off);
    int e = errno;
    close_target(t, fd);
    printf("GRID %-6s %-10s %-5s off=%-3lld count=0x%016llx -> ret=%ld errno=%d %s\n", opName[op], targetName[t],
           b.name, (long long)off, (unsigned long long)n, r, r < 0 ? e : 0, r < 0 ? strerror(e) : "-");
}

// 1 if the call through `buf` with count `n` is not an error.
static int ok_at(op_t op, void *buf, uint64_t n) {
    refresh_files();
    int writing = op == OP_WRITE || op == OP_PWRITE;
    int fd = writing ? open("/dev/null", O_WRONLY) : open(path("eof"), O_RDONLY);
    if (!writing) lseek(fd, 0, SEEK_END);
    long r = call(op, fd, buf, (size_t)n, 5);
    close(fd);
    return r >= 0;
}

static void edge(op_t op, void *buf) {
    // Strided sweep: how many flips over the range, sampled at 4096 points on
    // a log scale plus a linear one.
    int flips = 0, prev = ok_at(op, buf, 0);
    for (int k = 0; k < 64; k++)
        for (int j = 0; j < 64; j++) {
            uint64_t n = ((uint64_t)1 << k) + (((uint64_t)1 << k) / 64) * (uint64_t)j;
            int now = ok_at(op, buf, n);
            if (now != prev) flips++;
            prev = now;
        }
    // Bisect the largest ok count, assuming ok(0).
    uint64_t lo = 0, hi = UINT64_MAX;
    if (ok_at(op, buf, hi)) {
        printf("EDGE %-6s every count succeeds, flips=%d\n", opName[op], flips);
        return;
    }
    while (hi - lo > 1) {
        uint64_t mid = lo + (hi - lo) / 2;
        if (ok_at(op, buf, mid)) lo = mid; else hi = mid;
    }
    printf("EDGE %-6s buf=%p largest-ok=0x%016llx buf+largest=0x%016llx flips-in-sweep=%d\n", opName[op], buf,
           (unsigned long long)lo, (unsigned long long)((uintptr_t)buf + lo), flips);
    // Neighbours, to see the boundary is sharp.
    for (int64_t d = -2; d <= 2; d++) {
        uint64_t n = lo + (uint64_t)d;
        refresh_files();
        int writing = op == OP_WRITE || op == OP_PWRITE;
        int fd = writing ? open("/dev/null", O_WRONLY) : open(path("eof"), O_RDONLY);
        if (!writing) lseek(fd, 0, SEEK_END);
        errno = 0;
        long r = call(op, fd, buf, (size_t)n, 5);
        int e = errno;
        close(fd);
        printf("EDGE %-6s   count=0x%016llx -> ret=%ld errno=%d\n", opName[op], (unsigned long long)n, r, r < 0 ? e : 0);
    }
}

int main(int argc, char **argv) {
    alarm(600);
    signal(SIGPIPE, SIG_IGN);
    snprintf(dir, sizeof dir, "%s", argc > 1 ? argv[1] : "/tmp");
    struct utsname u;
    uname(&u);
    printf("%s %s %s page=%ld\n", u.sysname, u.release, u.machine, sysconf(_SC_PAGESIZE));
    long page = sysconf(_SC_PAGESIZE);
    char *m = mmap(NULL, 2 * page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    mprotect(m + page, page, PROT_NONE);
    char *h = mmap(NULL, page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    munmap(h, page);
    buffers[0] = (buffer_t){"map", (uintptr_t)m};
    buffers[1] = (buffer_t){"null", 0};
    buffers[2] = (buffer_t){"hole", (uintptr_t)h};
    buffers[3] = (buffer_t){"top", ((uintptr_t)1 << 48) - 4096};
    buffers[4] = (buffer_t){"wild", UINTPTR_MAX};
    printf("map=%p hole=%p\n", (void *)m, (void *)h);

    for (int op = 0; op < 4; op++)
        for (int t = 0; t < T_COUNT; t++)
            for (int b = 0; b < 5; b++)
                for (size_t c = 0; c < sizeof counts / sizeof counts[0]; c++) {
                    uint64_t n = counts[c];
                    // A data file written through a mapped page with a count
                    // it can reach: fine, the file is refreshed per row.
                    row((op_t)op, (target_t)t, buffers[b], n, 0);
                }
    // Negative offsets against a huge count, where both screens say EINVAL on
    // one flavour and the question is only which the other answers first.
    for (int op = OP_PREAD; op <= OP_PWRITE; op += 2)
        for (int t = 0; t < T_COUNT; t++)
            for (size_t c = 0; c < sizeof counts / sizeof counts[0]; c += 6)
                row((op_t)op, (target_t)t, buffers[0], counts[c], -1);

    for (int op = 0; op < 4; op++) edge((op_t)op, m);

    for (uint64_t n = 0x7FFFEFFEull; n <= 0x7FFFF002ull; n++) {
        int fd = open("/dev/null", O_WRONLY);
        errno = 0;
        long r = write(fd, m, (size_t)n);
        printf("CLAMP write devnull count=0x%llx -> ret=0x%lx errno=%d\n", (unsigned long long)n, r, r < 0 ? errno : 0);
        close(fd);
    }
    uint64_t bigs[] = {0x7FFFFFFFull, 0x80000000ull, 0x100000000ull, 0x0000800000000000ull, UINT64_MAX};
    for (size_t i = 0; i < 5; i++) {
        int fd = open("/dev/null", O_WRONLY);
        errno = 0;
        long r = write(fd, m, (size_t)bigs[i]);
        printf("CLAMP write devnull count=0x%llx -> ret=0x%lx errno=%d\n", (unsigned long long)bigs[i], r, r < 0 ? errno : 0);
        close(fd);
    }
    if (getenv("BIG")) {
        size_t big = 0x80000000ull + (size_t)page;
        char *bm = mmap(NULL, big, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        uint64_t rs[] = {0x7FFFF000ull, 0x7FFFF001ull, 0x7FFFFFFFull, 0x80000000ull};
        for (size_t i = 0; i < 4; i++) {
            int fd = open("/dev/zero", O_RDONLY);
            errno = 0;
            long r = read(fd, bm, (size_t)rs[i]);
            printf("CLAMP read devzero bigbuf count=0x%llx -> ret=0x%lx errno=%d\n", (unsigned long long)rs[i], r, r < 0 ? errno : 0);
            close(fd);
        }
        // A sparse file longer than the clamp, read in one call.
        int fd = open(path("sparse"), O_RDWR | O_CREAT | O_TRUNC, 0644);
        ftruncate(fd, 0xC0000000ll);
        for (size_t i = 0; i < 4; i++) {
            errno = 0;
            long r = pread(fd, bm, (size_t)rs[i], 0);
            printf("CLAMP pread sparse bigbuf count=0x%llx -> ret=0x%lx errno=%d\n", (unsigned long long)rs[i], r, r < 0 ? errno : 0);
        }
        for (size_t i = 0; i < 4; i++) {
            errno = 0;
            long r = pwrite(fd, bm, (size_t)rs[i], 0);
            printf("CLAMP pwrite sparse bigbuf count=0x%llx -> ret=0x%lx errno=%d\n", (unsigned long long)rs[i], r, r < 0 ? errno : 0);
        }
        close(fd);
        unlink(path("sparse"));
        munmap(bm, big);
    }

    // offset + count past INT64_MAX.
    off_t offs[] = {INT64_MAX - 10, INT64_MAX - 4, INT64_MAX, (off_t)1 << 62};
    uint64_t ns[] = {0, 1, 5, 10, 11, 12, 0x7FFFFFFFull};
    for (int op = OP_PREAD; op <= OP_PWRITE; op += 2)
        for (size_t i = 0; i < 4; i++)
            for (size_t j = 0; j < 7; j++) {
                refresh_files();
                int fd = open(path("data"), op == OP_PREAD ? O_RDONLY : O_WRONLY);
                errno = 0;
                long r = call((op_t)op, fd, m, (size_t)ns[j], offs[i]);
                int e = errno;
                close(fd);
                printf("OVERFLOW %-6s off=0x%016llx count=0x%llx -> ret=%ld errno=%d %s\n", opName[op],
                       (unsigned long long)offs[i], (unsigned long long)ns[j], r, r < 0 ? e : 0, r < 0 ? strerror(e) : "-");
            }
    unlink(path("data"));
    unlink(path("eof"));
    return 0;
}
