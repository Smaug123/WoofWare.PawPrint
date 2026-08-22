// Does a signal that misses a registration's interest enqueue it anyway?
// A WRITE-only listener receives an IN edge; if the kernel queued it despite
// the interest miss, a later MOD to READ would find it pending at the *edge's*
// position; if not, the MOD enqueues it fresh, behind everything since.
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

int main(void) {
    struct sockaddr_in a1, a2;
    int l1 = listener(&a1), l2 = listener(&a2);
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    // l1 watches WRITE only; l2 watches READ.
    ev.events = EPOLLOUT | EPOLLET; ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, l1, &ev);
    ev.events = EPOLLIN | EPOLLET;  ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, l2, &ev);

    connect_to(&a1);   // IN edge on l1: misses its WRITE-only interest
    connect_to(&a2);   // IN edge on l2: queued

    // Now give l1 an interest its level meets.
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 1;
    epoll_ctl(ep, EPOLL_CTL_MOD, l1, &ev);

    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  IN edge at WRITE-only l1, IN edge at l2, MOD l1 to READ -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu", (unsigned long long)evs[i].data.u64);
    printf("\n");
    return 0;
}
