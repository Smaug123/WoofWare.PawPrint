// What a write that faults leaves behind in a pipe: whether the EFAULT is the
// whole story, or whether the pipe's buffer was changed on the way to it.
//
// For each size, into a fresh empty non-blocking pipe, and into one already
// holding 100 bytes: write(fd, (void *)8, n), then report FIONREAD, whether
// the write end polls ready, and how many bytes the pipe then takes in
// 4096-byte and in 1-byte non-blocking writes until one is EAGAIN -- against
// the same measurements on a pipe that never saw the faulting write.
//
// Results, 2026-09-27 (Linux 6.18.5 aarch64; Darwin 27.0.0 arm64):
//
//   Linux   a faulting write of any size, into an empty pipe or one holding 100
//           bytes, leaves the pipe exactly as the control: every row agrees.
//   Darwin  a faulting write takes no bytes but grows the buffer as the same
//           write would have: after one of 16384 bytes or more, 16000 bytes
//           leave the write end ready where the control's is not, and the
//           chains below show the same for the sizes under 16 KiB that a
//           faulting 513, 4097 and 8193 grow it to.
//
// Build: `nix develop -c clang -O0 -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -o /tmp/p <this file>` on Linux.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

static char buf[1 << 17];

static int writable(int fd) {
    struct pollfd p = {fd, POLLOUT, 0};
    poll(&p, 1, 0);
    return (p.revents & POLLOUT) != 0;
}

static void measure(const char *label, int n, int prefill, int fault) {
    int z[2];
    pipe(z);
    fcntl(z[0], F_SETFL, O_NONBLOCK);
    fcntl(z[1], F_SETFL, O_NONBLOCK);
    if (prefill) write(z[1], buf, prefill);
    ssize_t r = 0;
    int e = 0;
    if (fault) {
        r = write(z[1], (void *)8, n);
        e = r < 0 ? errno : 0;
    }
    int avail = -1;
    ioctl(z[0], FIONREAD, &avail);
    int w = writable(z[1]);
    // One probe write of 16000 bytes, the case where Darwin's POLLOUT
    // depends on the size the buffer has grown to.
    ssize_t p16 = write(z[1], buf, 16000);
    int w16 = writable(z[1]);
    long more4k = 0, more1 = 0;
    for (;;) { ssize_t k = write(z[1], buf, 4096); if (k <= 0) break; more4k += k; }
    for (;;) { ssize_t k = write(z[1], buf, 1); if (k <= 0) break; more1 += k; }
    printf("  %-8s n=%6d prefill=%3d: write -> %zd errno %d; FIONREAD %d; POLLOUT %d; then 16000 -> %zd, POLLOUT %d; then %ld in 4096s and %ld in 1s\n",
           label, n, prefill, r, e, avail, w, p16, w16, more4k, more1);
    close(z[0]);
    close(z[1]);
}

// A buffer size below 16 KiB is invisible to POLLOUT, but not to what a later
// write grows it to, which is the smallest size strictly above what that write
// needs. So write `chain` in order and report POLLOUT after the last: a pipe
// that holds exactly 16384 bytes polls ready only if it had to grow past 16384
// to take them.
static void chain(const char *label, int fault, const int *writes, int count) {
    int z[2];
    pipe(z);
    fcntl(z[1], F_SETFL, O_NONBLOCK);
    ssize_t r = 0;
    if (fault) r = write(z[1], (void *)8, fault);
    long total = 0;
    for (int i = 0; i < count; i++) total += write(z[1], buf, writes[i]);
    printf("  chain %-8s fault %5d (-> %zd): wrote %ld; POLLOUT %d\n", label, fault, r, total, writable(z[1]));
    close(z[0]);
    close(z[1]);
}

int main(void) {
    alarm(30);
    setvbuf(stdout, NULL, _IOLBF, 0);
    memset(buf, 'x', sizeof buf);
    int sizes[] = {1, 511, 512, 513, 4095, 4096, 4097, 8192, 16384, 16385, 65536, 70000};
    for (int prefill = 0; prefill <= 100; prefill += 100)
        for (unsigned i = 0; i < sizeof sizes / sizeof *sizes; i++) {
            measure("control", sizes[i], prefill, 0);
            measure("faulted", sizes[i], prefill, 1);
        }
    // Growth to 16384 (a faulting 8193), then 16384 bytes: fit exactly.
    int a[] = {16384};
    chain("control", 0, a, 1);
    chain("faulted", 8193, a, 1);
    // Growth to 8192 (a faulting 4097), then 8192 and 8192 more.
    int b[] = {8192, 8192};
    chain("control", 0, b, 2);
    chain("faulted", 4097, b, 2);
    // Growth to 1024 (a faulting 513), then the doublings to 16384.
    int c[] = {1024, 1024, 2048, 4096, 8192};
    chain("control", 0, c, 5);
    chain("faulted", 513, c, 5);
    return 0;
}
