// Does EPOLL_CTL_MOD move a registration's place in the same-signal tie
// order?  The tie order is the socket's wait queue (order4.c: LIFO by
// registration), and ep_modify does not touch the wait queue entry — checked
// here rather than trusted from the source.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
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
    struct sockaddr_in a1;
    int l = listener(&a1);
    int d = dup(l);
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLET;
    ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, l, &ev);
    ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, d, &ev);
    // Without the MOD, order4.c measured [d(2), l(1)].  MOD the original:
    // does it move to the front of the tie?
    ev.events = EPOLLIN | EPOLLOUT | EPOLLET; ev.data.u64 = 1;
    epoll_ctl(ep, EPOLL_CTL_MOD, l, &ev);
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c, (struct sockaddr*)&a1, sizeof a1);
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  ADD l, ADD dup, MOD l, one connect -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu", (unsigned long long)evs[i].data.u64);
    printf("\n");
    return 0;
}
