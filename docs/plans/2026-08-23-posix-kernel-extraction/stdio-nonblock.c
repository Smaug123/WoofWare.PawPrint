// O_NONBLOCK on the standard streams, under the launch shape a harness uses:
// three distinct pipes, stdin's writer closed before the child runs, stdout
// and stderr read by the launcher as fast as it can.
//
// Measures, per stream: that F_SETFL takes the flag and F_GETFL reads it back;
// that a dup shares it in both directions (it is on the description); that it
// leaves poll's answer alone. Then, with the flag set: what read(0) answers;
// and what a non-blocking write of n bytes to stdout or stderr answers while the
// launcher drains it, each write waiting first until the pipe holds nothing.
// Last, the same sizes into a fresh empty pipe nobody reads.
//
// Results, 2026-09-27 (Darwin 27.0.0 arm64; Linux 6.18.5 aarch64, 4 KiB
// pages, in Apple's `container`), identical on both except where stated:
//
//   F_SETFL(O_NONBLOCK) on 0, 1 and 2       -> 0; F_GETFL shows the flag
//   dup(fd) sees it; clearing through the dup clears it for fd too
//   poll(0), (1), (2), flag clear and set   -> the same revents either way
//                                              (Linux 0x10 0x4 0x4; Darwin
//                                              0x11 0x4 0x4)
//   read(0, buf, 16), flag set              -> 0
//   read(0, NULL, 16), flag set             -> 0
//   read(0, buf, 0), flag set               -> 0
//   write(1 or 2, n), flag set, drained     -> n for every n <= 65536, and
//                                              65536 for every larger n, in
//                                              all 20 writes of every size
//   write(n) into a fresh idle pipe         -> the same: n up to 65536,
//                                              65536 beyond
//
// Sizes swept: 1, 511, 512, 513, 4095, 4096, 4097, 16384, 16385, 65535,
// 65536, 65537, 69632, 70000, 131072 and 1048576, each written REPEATS times
// to the drained stdout and to the drained stderr, and once to a fresh pipe. So in every call measured,
// a non-blocking write took what an empty pipe takes and no more, however fast
// the far reader drained.
//
// Build: `nix develop -c cc -O0 -o /tmp/p <this file>` on Darwin, `gcc -O0 -o
// /tmp/p <this file>` on Linux (gcc:14 image under `container run`).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#define REPEATS 20

static const int sizes[] = {1, 511, 512, 513, 4095, 4096, 4097, 16384, 16385, 65535, 65536, 65537, 69632, 70000, 131072, 1048576};
#define NSIZES ((int)(sizeof sizes / sizeof sizes[0]))

static FILE *out;

// How many bytes the pipe behind `fd` holds, from its write end: FIONREAD
// on Linux (row 12 of stdio-options.md: both ends report it), st_size on
// Darwin (row 11: both ends report the unread bytes). -1 if unknown.
static long held_on(int fd) {
#ifdef __APPLE__
    struct stat st;
    if (fstat(fd, &st) != 0) return -1;
    return (long)st.st_size;
#else
    int n;
    if (ioctl(fd, FIONREAD, &n) != 0) return -1;
    return n;
#endif
}

static int revents(int fd) {
    struct pollfd p = {fd, POLLIN | POLLOUT, 0};
    poll(&p, 1, 0);
    return p.revents;
}

static void child(void) {
    out = fdopen(3, "w");
    if (!out) _exit(2);
    setvbuf(out, NULL, _IONBF, 0);

    for (int fd = 0; fd <= 2; fd++) {
        int before = fcntl(fd, F_GETFL);
        int pollClear = revents(fd);
        int set = fcntl(fd, F_SETFL, before | O_NONBLOCK);
        int setErr = set < 0 ? errno : 0;
        int after = fcntl(fd, F_GETFL);
        int pollSet = revents(fd);
        int d = dup(fd);
        int viaDup = fcntl(d, F_GETFL);
        fcntl(d, F_SETFL, viaDup & ~O_NONBLOCK);
        int clearedOnOriginal = fcntl(fd, F_GETFL);
        close(d);
        fcntl(fd, F_SETFL, before | O_NONBLOCK);
        fprintf(out,
                "fd %d: F_GETFL before %#x (nonblock %d) | F_SETFL -> %d errno %d | after %#x (nonblock %d) | dup sees %d | cleared via dup, original %d | poll clear %#x set %#x\n",
                fd, before, !!(before & O_NONBLOCK), set, setErr, after, !!(after & O_NONBLOCK), !!(viaDup & O_NONBLOCK),
                !!(clearedOnOriginal & O_NONBLOCK), pollClear, pollSet);
    }

    char b[16];
    ssize_t r = read(0, b, sizeof b);
    fprintf(out, "nonblocking read(0, buf, 16) -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = read(0, NULL, 16);
    fprintf(out, "nonblocking read(0, NULL, 16) -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = read(0, b, 0);
    fprintf(out, "nonblocking read(0, buf, 0) -> %zd errno %d\n", r, r < 0 ? errno : 0);

    char *payload = malloc(1048576);
    memset(payload, 'x', 1048576);

    for (int fd = 1; fd <= 2; fd++)
    for (int i = 0; i < NSIZES; i++) {
        int n = sizes[i];
        long min = -1, max = -1;
        int whole = 0, eagain = 0, other = 0, notDrained = 0;
        for (int k = 0; k < REPEATS; k++) {
            int spins = 0;
            long h;
            while ((h = held_on(fd)) != 0 && spins < 5000) {
                usleep(1000);
                spins++;
            }
            if (h != 0) notDrained++;
            ssize_t w = write(fd, payload, n);
            if (w < 0) {
                if (errno == EAGAIN) eagain++; else other++;
                continue;
            }
            if (w == n) whole++;
            if (min < 0 || w < min) min = w;
            if (w > max) max = w;
        }
        fprintf(out, "drained fd %d: write(%d) x%d -> whole %d, eagain %d, other errors %d, min %ld max %ld (not drained first: %d)\n", fd, n,
                REPEATS, whole, eagain, other, min, max, notDrained);
    }

    for (int i = 0; i < NSIZES; i++) {
        int n = sizes[i];
        int p[2];
        if (pipe(p) != 0) { fprintf(out, "pipe failed\n"); break; }
        fcntl(p[1], F_SETFL, fcntl(p[1], F_GETFL) | O_NONBLOCK);
        ssize_t w = write(p[1], payload, n);
        fprintf(out, "fresh idle pipe: write(%d) -> %zd errno %d\n", n, w, w < 0 ? errno : 0);
        close(p[0]);
        close(p[1]);
    }
    fclose(out);
    _exit(0);
}

int main(void) {
    alarm(60);
    int in[2], o[2], e[2], rep[2];
    if (pipe(in) || pipe(o) || pipe(e) || pipe(rep)) return 1;
    pid_t c = fork();
    if (c == 0) {
        dup2(in[0], 0);
        dup2(o[1], 1);
        dup2(e[1], 2);
        dup2(rep[1], 3);
        for (int fd = 4; fd < 64; fd++) close(fd);
        child();
    }
    close(in[0]);
    close(in[1]); // stdin's writer is gone before the child reads
    close(o[1]);
    close(e[1]);
    close(rep[1]);

    // Drain stdout and stderr as fast as possible; relay the report.
    struct pollfd fds[3] = {{o[0], POLLIN, 0}, {e[0], POLLIN, 0}, {rep[0], POLLIN, 0}};
    static char buf[1 << 20];
    int open = 3;
    long drained = 0;
    while (open > 0) {
        if (poll(fds, 3, 30000) <= 0) break;
        for (int i = 0; i < 3; i++) {
            if (fds[i].fd < 0 || !fds[i].revents) continue;
            ssize_t n = read(fds[i].fd, buf, sizeof buf);
            if (n <= 0) {
                close(fds[i].fd);
                fds[i].fd = -1;
                open--;
                continue;
            }
            if (i == 2) fwrite(buf, 1, n, stdout);
            else drained += n;
        }
    }
    int ws;
    waitpid(c, &ws, 0);
    printf("launcher drained %ld bytes; child status %d\n", drained, ws);
    return 0;
}
