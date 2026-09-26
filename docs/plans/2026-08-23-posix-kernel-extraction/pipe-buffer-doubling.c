// Pins Darwin's initial pipe buffer size: writes that land the held count on
// each power of two from a start value up to 16384 (or straight to 16384 after
// the first write), with poll after each. A block size either absorbs the next
// landing point or grows past it, so the initial sizes 512 and 1024 disagree
// about whether 16384 fits and therefore whether the write end is ready there.
// Rows are in pipe-buffer-trace.c's format.
//
// Result (20 cases, 71 rows): an initial size of 512 agrees with every row; 0
// diverged on 1 case, 1024 on 5. All rows are kept in
// WoofWare.PosixKernel.Test/pipeBuffer/darwin.txt.//
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
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
static char buf[1 << 17];
int main(void) {
    alarm(60);
    short ask = POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND;
    int starts[] = {1, 256, 511, 512, 513, 1024, 2048, 3000, 4096, 8192};
    int seed = 0;
    for (unsigned k = 0; k < sizeof starts / sizeof *starts; k++) {
        for (int firstBig = 0; firstBig <= 1; firstBig++) {
            seed++;
            int p[2];
            pipe(p);
            fcntl(p[0], F_SETFL, O_NONBLOCK);
            fcntl(p[1], F_SETFL, O_NONBLOCK);
            int held = 0;
            int target = starts[k];
            while (held < 16384) {
                int n = target - held;
                ssize_t r = write(p[1], buf, n);
                int e = r < 0 ? errno : 0;
                if (r > 0) held += r;
                int avail = 0; ioctl(p[0], FIONREAD, &avail);
                struct pollfd pf[2] = {{p[0], ask, 0}, {p[1], ask, 0}};
                poll(pf, 2, 0);
                struct stat sr, sw; fstat(p[0], &sr); fstat(p[1], &sw);
                printf("%d W %d -> %zd %d | %d | %#x %#x | %lld %lld\n", seed, n, r, e, avail, pf[0].revents, pf[1].revents,
                       (long long)sr.st_size, (long long)sw.st_size);
                if (r <= 0) break;
                // Next landing point: the next power of two above held (or, on
                // the firstBig pass, straight to 16384 after the first write).
                int t = 512;
                while (t <= held) t *= 2;
                target = firstBig ? 16384 : t;
                if (target > 16384) break;
            }
            close(p[0]); close(p[1]);
        }
    }
    return 0;
}
