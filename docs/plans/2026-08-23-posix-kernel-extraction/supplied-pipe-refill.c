// How a pipe fills while a writer is blocked in one write of more than the
// pipe holds, and the reader drains it: the launcher of a process whose
// standard input it supplies with `write(all)` and then `close()`.
//
// A child writes N patterned bytes with one blocking write(2), then closes.
// The parent, holding the read end O_NONBLOCK, waits SETTLE ms for the writer
// to block, then reads random counts until end of file, waiting SETTLE ms
// after each read so that a woken writer has run before the next observation.
// Each row is one read and everything observable after the wait:
//   case N seed | R count -> ret errno | FIONREAD | poll(read end)
// The first row of a case (op I) is the state before any read. Every byte read
// is checked against the pattern, so order and loss are checked too.
//
// Usage: supplied-pipe-refill SEEDS SETTLE_MS [N...]
//
// Results, 2026-10-01, on Darwin 27.0.0 arm64 and Linux 6.18.5 aarch64 with
// 4 KiB pages (Apple's `container`, gcc:14), with
// `supplied-pipe-refill 10 20 65537 65636 65836 66047 66048 66049 69632 70000
// 100000 131072 200000 1048576`: 120 cases, 3585 rows on Linux and 3581 on
// Darwin, every byte in order and none lost. Every row agrees with this model:
//
// * The writer's first pass is a write into an empty pipe (`PipeBuffer.write`):
//   65536 bytes held before the first read, on both.
// * Linux: after a read, the writer fills every free slot with a page of what
//   it has left, and never merges into the newest slot. A read that frees no
//   slot is followed by no write.
// * Darwin: after a read, the writer writes min(left, 65536 - held), with no
//   all-or-nothing rule even for its last 512 bytes or fewer: with 300 left, a
//   read of 50 from a full pipe is followed by a write of 50. Applying
//   `PipeBuffer.write`'s atomicity to what is left disagrees with 19 rows.
// * The writer closes once its last byte is in: the read end polls HUP from
//   then on (Linux 0x10, Darwin 0xd3 even when empty), and not before.
// * The pipe is never empty while the writer has bytes left.
//
// With SETTLE_MS 0 (`supplied-pipe-refill 5 0 65836 200000 1048576`, 15
// cases), Linux still agrees with every row, but Darwin disagrees with 635 of
// 1117: there the FIONREAD taken straight after a read often sees the pipe
// before the writer has refilled it. No read in either run found the pipe
// empty while the writer had bytes left.
//
// Build: on Darwin with the repository devshell's clang
// (`nix develop -c clang -O0 -o /tmp/p <this file>`); on Linux in Apple's
// `container` (`gcc -O0 -o /tmp/p <this file>`).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <time.h>
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
    case 6: return 1 + rnd() % 64;
    default: return 1 + rnd() % 3000;
    }
}

static unsigned char pattern(long i) { return (unsigned char)((i * 131 + (i >> 8) * 7) & 0xff); }

static void settle(int ms) {
    struct timespec t = {ms / 1000, (long)(ms % 1000) * 1000000L};
    nanosleep(&t, NULL);
}

static int held(int fd) {
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) != 0) return -1;
    return n;
}

static int revents(int fd) {
    struct pollfd p = {fd, POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND, 0};
    poll(&p, 1, 0);
    return p.revents;
}

static unsigned char rbuf[1 << 17];

static void one(long n, unsigned seed, int settle_ms) {
    int fds[2];
    if (pipe(fds) != 0) { perror("pipe"); exit(1); }
    pid_t pid = fork();
    if (pid == 0) {
        close(fds[0]);
        unsigned char *w = malloc((size_t)n);
        for (long i = 0; i < n; i++) w[i] = pattern(i);
        ssize_t r = write(fds[1], w, (size_t)n);
        if (r != n) { fprintf(stderr, "writer: write returned %zd errno %d\n", r, errno); _exit(3); }
        close(fds[1]);
        _exit(0);
    }
    close(fds[1]);
    fcntl(fds[0], F_SETFL, fcntl(fds[0], F_GETFL) | O_NONBLOCK);
    rng = 0x9E3779B97F4A7C15ULL ^ ((unsigned long long)seed * 0x100000001B3ULL) ^ (unsigned long long)n;
    for (int i = 0; i < 4; i++) rnd();
    settle(settle_ms < 100 ? 100 : settle_ms);
    printf("c%ld-%u %ld %u | I 0 -> 0 0 | %d | 0x%x\n", n, seed, n, seed, held(fds[0]), revents(fds[0]));
    long pos = 0;
    for (int step = 0; step < 100000; step++) {
        int count = size_of();
        ssize_t r = read(fds[0], rbuf, (size_t)count);
        int e = r < 0 ? errno : 0;
        if (r > 0) {
            for (ssize_t i = 0; i < r; i++)
                if (rbuf[i] != pattern(pos + i)) { printf("MISMATCH at %ld\n", pos + i); exit(4); }
            pos += r;
        }
        settle(settle_ms);
        printf("c%ld-%u %ld %u | R %d -> %zd %d | %d | 0x%x\n", n, seed, n, seed, count, r, e, held(fds[0]), revents(fds[0]));
        if (r == 0) break;
    }
    if (pos != n) { printf("SHORT: read %ld of %ld\n", pos, n); exit(5); }
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) { printf("WRITER FAILED %d\n", status); exit(6); }
    close(fds[0]);
}

int main(int argc, char **argv) {
    alarm(1800);
    if (argc < 4) { fprintf(stderr, "usage: %s SEEDS SETTLE_MS N...\n", argv[0]); return 2; }
    setvbuf(stdout, NULL, _IOLBF, 0);
    int seeds = atoi(argv[1]);
    int settle_ms = atoi(argv[2]);
    for (int a = 3; a < argc; a++)
        for (int s = 1; s <= seeds; s++)
            one(atol(argv[a]), (unsigned)s, settle_ms);
    return 0;
}
