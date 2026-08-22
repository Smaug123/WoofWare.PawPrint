// What readiness level does each pipe-end state present?  PawPrint models the
// standard streams as pipe ends: stdin the read end of a pipe whose write end
// the launcher closed, stdout/stderr write ends whose read ends the launcher
// holds open.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/epoll.h>

static void mask_of(int fd, const char *label) {
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP; ev.data.u64 = 1;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev) != 0) { printf("  %-46s ADD failed errno=%d\n", label, errno); close(ep); return; }
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  %-46s -> %d event(s)", label, n);
    for (int i = 0; i < n; i++) printf(" events=0x%x", evs[i].events);
    printf("\n");
    close(ep);
}

int main(void) {
    int a[2]; pipe(a);
    mask_of(a[0], "1. read end, empty, writer open");
    mask_of(a[1], "2. write end, space, reader open");

    int b[2]; pipe(b);
    close(b[1]);
    mask_of(b[0], "3. read end, empty, writer closed (stdin's shape)");

    int c[2]; pipe(c);
    write(c[1], "x", 1);
    close(c[1]);
    mask_of(c[0], "4. read end, data buffered, writer closed");

    int d[2]; pipe(d);
    close(d[0]);
    mask_of(d[1], "5. write end, reader closed");

    return 0;
}
