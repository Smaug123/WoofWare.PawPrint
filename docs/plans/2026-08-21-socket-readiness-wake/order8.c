// Is the peer-FIN wake keyed?  ep_poll_callback skips its interest test when
// the wake carries no key, so an unkeyed FIN would queue even a registration
// whose interest the half-closed level misses — detectable by where a later
// MOD finds the entry: already pending at the FIN's position, or enqueued
// fresh at MOD time behind a newer edge.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>

static int listener(struct sockaddr_in *out) {
    int fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in a; memset(&a, 0, sizeof a);
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK); a.sin_port = 0;
    bind(fd, (struct sockaddr*)&a, sizeof a);
    listen(fd, 8);
    socklen_t sl = sizeof *out;
    getsockname(fd, (struct sockaddr*)out, &sl);
    return fd;
}

int main(void) {
    struct sockaddr_in a1, a2;
    int l1 = listener(&a1), l2 = listener(&a2);

    // An established pair on l1.
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c, (struct sockaddr*)&a1, sizeof a1);
    int srv = accept(l1, NULL, NULL);

    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    // c watches nothing maskable: only the implicit ERR|HUP remain.
    ev.events = EPOLLET;           ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, l2, &ev);

    // The FIN: level becomes IN|OUT|RDHUP, none of it in c's interest. No
    // intermediate wait — a zero-timeout epoll_wait would itself consume a
    // stale candidate and hide exactly the thing being measured.
    close(srv);
    usleep(50000);

    struct epoll_event evs[8];
    int n;

    // A newer edge elsewhere, then widen c's interest.
    int c2 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c2, (struct sockaddr*)&a2, sizeof a2);
    ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET; ev.data.u64 = 1;
    epoll_ctl(ep, EPOLL_CTL_MOD, c, &ev);

    n = epoll_wait(ep, evs, 8, 0);
    printf("  after l2 edge then MOD c -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");
    return 0;
}
