// Is an edge-triggered epoll_wait batch in edge-arrival order?
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

static void connect_to(struct sockaddr_in *a) {
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c, (struct sockaddr*)a, sizeof *a);
}

static void run(int first_is_l1) {
    struct sockaddr_in a1, a2;
    int l1 = listener(&a1), l2 = listener(&a2);
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    // Register l1 first in both runs, so registration order is held fixed and
    // only the order the edges arrive in varies.
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, l1, &ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, l2, &ev);

    if (first_is_l1) { connect_to(&a1); connect_to(&a2); }
    else             { connect_to(&a2); connect_to(&a1); }

    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  edges arrived %s (l1 fd=%d < l2 fd=%d), batch of %d:",
           first_is_l1 ? "l1 then l2" : "l2 then l1", l1, l2, n);
    for (int i = 0; i < n; i++) printf(" data=%llu", (unsigned long long)evs[i].data.u64);
    printf("\n");
    close(l1); close(l2); close(ep);
}

int main(void) { run(1); run(0); return 0; }
