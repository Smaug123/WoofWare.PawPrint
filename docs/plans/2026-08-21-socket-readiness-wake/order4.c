// One edge, two registrations of the same socket (through dup): what order do
// the two events arrive in, and does registration order or fd order decide?
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

static void run(int low_fd_first) {
    struct sockaddr_in a1;
    int l = listener(&a1);
    int d = dup(l);
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLET;
    if (low_fd_first) {
        ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, l, &ev);
        ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, d, &ev);
    } else {
        ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, d, &ev);
        ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, l, &ev);
    }
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c, (struct sockaddr*)&a1, sizeof a1);
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  registered %s (l fd=%d, dup fd=%d), one connect -> %d:",
           low_fd_first ? "l then dup" : "dup then l", l, d, n);
    for (int i = 0; i < n; i++) printf(" data=%llu", (unsigned long long)evs[i].data.u64);
    printf("\n");
    close(l); close(d); close(ep); close(c);
}

int main(void) { run(1); run(0); return 0; }
