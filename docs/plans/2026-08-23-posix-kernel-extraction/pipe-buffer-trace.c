// Random traces of non-blocking reads and writes on one pipe, for holding a
// model of the pipe's buffer to the kernel: each line is one call and
// everything observable after it.
//   seed op count -> ret errno | FIONREAD | poll(read end) poll(write end) | st_size(read) st_size(write)
// Usage: pipe-buffer-trace SEEDS OPS [BIAS]  (BIAS > 0 keeps the pipe fuller)
//
// Results (`pipe-buffer-model.py`, 200 seeds x 300 calls, at BIAS 0 and 40000,
// so 120,000 rows per flavour):
//
// * Linux: exact agreement with a ring of 16 page-sized slots. A write first
//   merges its (count mod page) remainder into the newest slot if that slot has
//   room after its end, then takes whole new slots, one page each; a read frees
//   a slot only once it has emptied it. The write end is ready iff a slot is
//   free. A 64 KiB byte-counting model diverged on every seed.
// * Darwin: exact agreement with a byte buffer that grows (512, 1024, ...,
//   16384, then 65536) when a write does not fit, and never shrinks. The write
//   end is ready iff max(16384, size) - held >= 512. A fixed 64 KiB buffer
//   diverged on 7 (unbiased) and 11 (biased) of 200 seeds.
// * Both: writes of at most PIPE_BUF (Linux 4096, Darwin 512) are all or
//   nothing; larger ones take what fits. Reads return min(count, held) and
//   bytes come out in the order they went in.
//
// The rows kept in WoofWare.PosixKernel.Test/pipeBuffer/ are seeds 1-8 of each
// run.//
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
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>

static unsigned long long rng;
static unsigned rnd(void) { rng ^= rng << 13; rng ^= rng >> 7; rng ^= rng << 17; return (unsigned)(rng >> 11); }

static int size_of(void) {
    switch (rnd() % 8) {
    case 0: return 1 + rnd() % 8;
    case 1: return 1 + rnd() % 600;
    case 2: { int c[] = {511, 512, 513, 4095, 4096, 4097, 8191, 8192, 8193}; return c[rnd() % 9]; }
    case 3: return 1 + rnd() % 5000;
    case 4: return 1 + rnd() % 20000;
    case 5: return 1 + rnd() % 70000;
    case 6: return 0;
    default: return 1 + rnd() % 3000;
    }
}

static char wbuf[1 << 17], rbuf[1 << 17];

int main(int argc, char **argv) {
    alarm(120);
    int seeds = argc > 1 ? atoi(argv[1]) : 20, ops = argc > 2 ? atoi(argv[2]) : 200;
    int bias = argc > 3 ? atoi(argv[3]) : 0; // added to the fill level below which we write
#ifdef POLLRDHUP
    short ask = POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND | POLLRDHUP;
#else
    short ask = POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND;
#endif
    for (int s = 1; s <= seeds; s++) {
        rng = 0x9E3779B97F4A7C15ULL * (unsigned long long)s + 1;
        int p[2];
        if (pipe(p) != 0) { perror("pipe"); return 1; }
        fcntl(p[0], F_SETFL, O_NONBLOCK);
        fcntl(p[1], F_SETFL, O_NONBLOCK);
        unsigned char next = 0; // bytes written are a counter, so reads can be checked for order
        unsigned char expect = 0;
        for (int i = 0; i < ops; i++) {
            // Bias towards writing while the pipe is small, reading while it is big.
            int avail = 0; ioctl(p[0], FIONREAD, &avail);
            int doWrite = (int)(rnd() % 65536) + bias >= avail;
            int n = size_of();
            ssize_t r; int e = 0;
            if (doWrite) {
                for (int k = 0; k < n; k++) wbuf[k] = (char)(next + k);
                r = write(p[1], wbuf, n);
                if (r < 0) e = errno; else next = (unsigned char)(next + r);
            } else {
                r = read(p[0], rbuf, n);
                if (r < 0) e = errno;
                else {
                    for (ssize_t k = 0; k < r; k++) if ((unsigned char)rbuf[k] != (unsigned char)(expect + k)) { printf("ORDER VIOLATION seed %d op %d\n", s, i); return 2; }
                    expect = (unsigned char)(expect + r);
                }
            }
            ioctl(p[0], FIONREAD, &avail);
            struct pollfd pf[2] = {{p[0], ask, 0}, {p[1], ask, 0}};
            poll(pf, 2, 0);
            struct stat sr, sw; fstat(p[0], &sr); fstat(p[1], &sw);
            printf("%d %c %d -> %zd %d | %d | %#x %#x | %lld %lld\n", s, doWrite ? 'W' : 'R', n, r, e, avail,
                   pf[0].revents, pf[1].revents, (long long)sr.st_size, (long long)sw.st_size);
        }
        close(p[0]); close(p[1]);
    }
    return 0;
}
