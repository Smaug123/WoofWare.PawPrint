// Darwin's pipe buffer grows; this finds exactly when, near the 16 KiB boundary
// where the initial size and the growth rule are distinguishable. Each seed: a
// fresh non-blocking pipe, six calls whose write sizes are drawn to land the
// held count on and around 16384, with poll on both ends after each. Rows are
// in pipe-buffer-trace.c's format.
// Usage: pipe-buffer-growth SEEDS
//
// Result (3000 seeds, 18000 rows): `pipe-buffer-model.py darwin-dyn` agrees with
// every row for an initial size of 0, 512 or 1024, and an initial size of 16384
// diverged on 1161 seeds. pipe-buffer-doubling.c then separates 512 from 0 and
// 1024. The rows kept in WoofWare.PosixKernel.Test/pipeBuffer/darwin.txt are
// seeds 1-100.//
// Build and run: on Darwin with the repository devshell's clang
// (`nix develop -c cc -O0 -o /tmp/p <this file>`); on Linux in Apple's
// `container` (gcc:14 image, `gcc -O0 -o /tmp/p <this file>`). Measured on
// 2026-09-26 on Darwin 27.0.0 arm64; Darwin only.
// No x86-64 kernel was measured: `container --arch amd64` would be Rosetta on
// the same aarch64 kernel. `TestPipeBufferAgainstHost` compares the model with
// whichever kernel runs the suite, which in CI is x86-64.
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
static char buf[1 << 17];

int main(int argc, char **argv) {
    alarm(120);
    int seeds = argc > 1 ? atoi(argv[1]) : 2000;
    short ask = POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND;
    memset(buf, 'x', sizeof buf);
    for (int s = 1; s <= seeds; s++) {
        rng = 0xD1B54A32D192ED03ULL * (unsigned long long)s + 7;
        int p[2];
        if (pipe(p) != 0) return 1;
        fcntl(p[0], F_SETFL, O_NONBLOCK);
        fcntl(p[1], F_SETFL, O_NONBLOCK);
        for (int i = 0; i < 6; i++) {
            int avail = 0; ioctl(p[0], FIONREAD, &avail);
            int doWrite = rnd() % 6 != 0;
            int n;
            if (doWrite) {
                // Aim at 16384 exactly, a little either side, or anywhere.
                switch (rnd() % 4) {
                case 0: n = 16384 - avail; break;
                case 1: n = 16384 - avail + (int)(rnd() % 33) - 16; break;
                case 2: n = 1 + rnd() % 9000; break;
                default: { int c[] = {1, 511, 512, 513, 4096, 8192, 8193, 15872, 15873, 16383, 16384, 16385}; n = c[rnd() % 12]; }
                }
                if (n < 0) n = 1 + rnd() % 100;
            } else {
                n = 1 + rnd() % 9000;
            }
            ssize_t r = doWrite ? write(p[1], buf, n) : read(p[0], buf, n);
            int e = r < 0 ? errno : 0;
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
