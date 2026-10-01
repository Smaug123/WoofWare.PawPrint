/* What an edge-triggered epoll registration on a pipe's write end sees while
 * the read end is drained as fast as it is written (here: by the same thread,
 * immediately after each write), for writes of a range of sizes. Prints, for
 * each size: the events after the write, and the events after the drain.
 * Linux only.
 *
 * Measured 2026-10-01 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14),
 * three repetitions of three writes for each size, every one alike:
 *
 *   size                         at ADD   after a write   after the drain
 *   1 .. 61440 (one slot free)   OUT      nothing         nothing
 *   65535, 65536 (pipe full)     OUT      nothing         OUT
 *
 * So the write end is woken by the reader exactly when it reads from a full
 * pipe, and never by a write. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <unistd.h>

static unsigned waitEvents(int ep) {
    struct epoll_event ev[4];
    int n = epoll_wait(ep, ev, 4, 0);
    if (n < 0) { perror("epoll_wait"); exit(1); }
    unsigned acc = 0;
    for (int i = 0; i < n; i++) acc |= ev[i].events;
    return n == 0 ? 0 : (acc | 0x80000000u);
}

int main(void) {
    alarm(20);
    static char buf[1 << 20];
    memset(buf, 'x', sizeof buf);
    int sizes[] = { 1, 100, 4095, 4096, 4097, 32768, 61440, 65535, 65536 };
    for (unsigned s = 0; s < sizeof sizes / sizeof sizes[0]; s++) {
        for (int rep = 0; rep < 3; rep++) {
            int p[2];
            if (pipe2(p, O_NONBLOCK) != 0) { perror("pipe2"); return 1; }
            int ep = epoll_create1(0);
            struct epoll_event ev = { .events = EPOLLOUT | EPOLLET, .data.u64 = 1 };
            if (epoll_ctl(ep, EPOLL_CTL_ADD, p[1], &ev) != 0) { perror("ctl"); return 1; }
            unsigned atAdd = waitEvents(ep);
            unsigned afterWrites[3], afterDrains[3];
            for (int k = 0; k < 3; k++) {
                ssize_t w = write(p[1], buf, sizes[s]);
                if (w != sizes[s]) { printf("short write %zd of %d\n", w, sizes[s]); }
                afterWrites[k] = waitEvents(ep);
                ssize_t total = 0;
                while (total < w) { ssize_t r = read(p[0], buf, sizeof buf); if (r <= 0) break; total += r; }
                afterDrains[k] = waitEvents(ep);
            }
            printf("size %6d rep %d: atAdd 0x%x", sizes[s], rep, atAdd);
            for (int k = 0; k < 3; k++) printf(" | write 0x%x drain 0x%x", afterWrites[k], afterDrains[k]);
            printf("\n");
            close(ep); close(p[0]); close(p[1]);
        }
    }
    return 0;
}
