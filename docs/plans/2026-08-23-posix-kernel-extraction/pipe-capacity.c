// Pipe capacity and PIPE_BUF: how many bytes a fresh pipe accepts before a
// non-blocking write says EAGAIN, as a function of the write size used to fill
// it; what fpathconf says PIPE_BUF is; and (Linux) what F_GETPIPE_SZ and
// F_SETPIPE_SZ answer across a sweep of requested sizes.
//
// Results: PIPE_BUF is 4096 on Linux and 512 on Darwin. A fresh Linux pipe
// holds 65536 bytes of 1-, 2- or 4096-byte writes but 65520 of 3-byte, 64000 of
// 100-byte, 57456 of 513-byte and 45066 of 4097-byte writes (16 page slots);
// Darwin's holds 65536 of any size except where a write of at most 512 bytes
// would straddle the end (65535 of 3-byte, 65408 of 511-byte). A non-blocking
// 1 MiB write into an empty pipe returns 65536 on both. Linux F_SETPIPE_SZ
// rounds up to a power-of-two number of pages (minimum 4096), is EINVAL for -1,
// EPERM above pipe-max-size even as root in the container (so CAP_SYS_RESOURCE
// was absent; the privileged case is unmeasured), and EBUSY when it would
// shrink below the bytes held. The 1-byte fill of a 1 MiB pipe stops at this
// probe's own bound of 1000000 writes.//
// Build and run: on Darwin with the repository devshell's clang
// (`nix develop -c cc -O0 -o /tmp/p <this file>`); on Linux in Apple's
// `container` (gcc:14 image, `gcc -O0 -o /tmp/p <this file>`). Measured on
// 2026-09-26 on Darwin 27.0.0 arm64 and Linux 6.18.5 aarch64 with 4 KiB pages.
// No x86-64 kernel was measured: `container --arch amd64` would be Rosetta on
// the same aarch64 kernel. `TestPipeBufferAgainstHost` compares the model with
// whichever kernel runs the suite, which in CI is x86-64.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <sys/ioctl.h>

static char buf[1 << 21];

static void nb(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

// Fill with writes of `size` until EAGAIN; report total and the last partial.
static long fill(int w, int size, int *writes, int *shorts, int *lastErr) {
    long total = 0; *writes = 0; *shorts = 0; *lastErr = 0;
    for (int i = 0; i < 1000000; i++) {
        ssize_t n = write(w, buf, size);
        if (n < 0) { *lastErr = errno; break; }
        (*writes)++;
        if (n < size) (*shorts)++;
        total += n;
    }
    return total;
}

int main(void) {
    alarm(60);
    memset(buf, 'x', sizeof buf);
    int p[2];
    if (pipe(p) != 0) { perror("pipe"); return 1; }
    printf("fpathconf(_PC_PIPE_BUF) read end %ld, write end %ld; PIPE_BUF macro %d\n",
           fpathconf(p[0], _PC_PIPE_BUF), fpathconf(p[1], _PC_PIPE_BUF), PIPE_BUF);
#ifdef F_GETPIPE_SZ
    printf("F_GETPIPE_SZ fresh: %d\n", fcntl(p[1], F_GETPIPE_SZ));
#endif
    close(p[0]); close(p[1]);

    int sizes[] = {1, 2, 3, 7, 100, 511, 512, 513, 1000, 1024, 4095, 4096, 4097, 8191, 8192, 8193,
                   16383, 16384, 16385, 32768, 65535, 65536, 65537, 131072, 1 << 20};
    for (size_t k = 0; k < sizeof sizes / sizeof *sizes; k++) {
        if (pipe(p) != 0) { perror("pipe"); return 1; }
        nb(p[1]);
        int writes, shorts, err;
        long total = fill(p[1], sizes[k], &writes, &shorts, &err);
        int avail = -1; ioctl(p[0], FIONREAD, &avail);
        printf("fill size=%7d total=%7ld writes=%6d short=%d stop_errno=%d FIONREAD=%d\n",
               sizes[k], total, writes, shorts, err, avail);
        close(p[0]); close(p[1]);
    }

    // Two-phase: first a large write, then 1-byte writes: does the buffer grow
    // (Darwin's dynamic sizing) depending on history?
    int first[] = {1, 512, 4096, 16384, 16385, 32768, 65536};
    for (size_t k = 0; k < sizeof first / sizeof *first; k++) {
        if (pipe(p) != 0) return 1;
        nb(p[1]);
        ssize_t a = write(p[1], buf, first[k]);
        int e1 = a < 0 ? errno : 0;
        int writes, shorts, err;
        long more = fill(p[1], 1, &writes, &shorts, &err);
        printf("history first=%6d -> %zd (errno %d), then 1-byte fill adds %ld\n", first[k], a, e1, more);
        close(p[0]); close(p[1]);
    }

    // Blocking write larger than the capacity into a pipe drained by nobody,
    // with a non-blocking twin: is a big non-blocking write into an EMPTY pipe
    // partial (writes what fits) or EAGAIN?
    {
        if (pipe(p) != 0) return 1;
        nb(p[1]);
        ssize_t a = write(p[1], buf, 1 << 20);
        printf("nonblocking 1MiB into empty pipe -> %zd (errno %d)\n", a, a < 0 ? errno : 0);
        close(p[0]); close(p[1]);
    }

#ifdef F_SETPIPE_SZ
    {
        FILE *f = fopen("/proc/sys/fs/pipe-max-size", "r");
        long mx = -1; if (f) { if (fscanf(f, "%ld", &mx) != 1) mx = -2; fclose(f); }
        printf("pipe-max-size %ld, euid %d\n", mx, (int)geteuid());
        long req[] = {-1, 0, 1, 4095, 4096, 4097, 8192, 12288, 12289, 16384, 20000, 65536, 65537,
                      100000, 131072, 1048576, 1048577, 2097152, (1L << 31) - 1, 1L << 31};
        for (size_t k = 0; k < sizeof req / sizeof *req; k++) {
            if (pipe(p) != 0) return 1;
            int r = fcntl(p[1], F_SETPIPE_SZ, (int)req[k]);
            int e = r < 0 ? errno : 0;
            int got = fcntl(p[1], F_GETPIPE_SZ);
            nb(p[1]);
            int writes, shorts, err;
            long total = fill(p[1], 1, &writes, &shorts, &err);
            printf("F_SETPIPE_SZ(%ld) -> %d errno %d; F_GETPIPE_SZ %d; 1-byte fill %ld\n", req[k], r, e, got, total);
            close(p[0]); close(p[1]);
        }
        // Shrinking below the bytes held.
        if (pipe(p) != 0) return 1;
        nb(p[1]);
        write(p[1], buf, 10000);
        int r = fcntl(p[1], F_SETPIPE_SZ, 4096);
        printf("shrink to 4096 holding 10000 bytes -> %d errno %d; now %d\n", r, r < 0 ? errno : 0, fcntl(p[1], F_GETPIPE_SZ));
        r = fcntl(p[0], F_SETPIPE_SZ, 16384);
        printf("shrink via read end to 16384 holding 10000 -> %d errno %d; now %d\n", r, r < 0 ? errno : 0, fcntl(p[0], F_GETPIPE_SZ));
        close(p[0]); close(p[1]);
    }
#endif
    return 0;
}
