// Does an edge-triggered epoll report a level that has gone away, and does it
// report a drop-then-rise that happened entirely between two epoll_wait calls?
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static int listener_fd;
static struct sockaddr_in laddr;

static int make_listener(void) {
    int fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in a; memset(&a, 0, sizeof a);
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK); a.sin_port = 0;
    if (bind(fd, (struct sockaddr*)&a, sizeof a) != 0) { perror("bind"); return -1; }
    if (listen(fd, 8) != 0) { perror("listen"); return -1; }
    socklen_t sl = sizeof laddr;
    getsockname(fd, (struct sockaddr*)&laddr, &sl);
    return fd;
}

static int connect_one(void) {
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (connect(c, (struct sockaddr*)&laddr, sizeof laddr) != 0) { perror("connect"); return -1; }
    return c;
}

static int wait0(int ep, const char *label) {
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  %-44s epoll_wait -> %d", label, n);
    for (int i = 0; i < n; i++) printf(" [events=0x%x]", evs[i].events);
    printf("\n");
    return n;
}

int main(void) {
    listener_fd = make_listener();
    if (listener_fd < 0) return 1;
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 0x1234;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, listener_fd, &ev) != 0) { perror("epoll_ctl"); return 1; }

    printf("A. stale edge: connect, accept (drain), then wait\n");
    int c1 = connect_one(); int a1 = accept(listener_fd, NULL, NULL);
    wait0(ep, "queue drained before any wait");

    printf("B. live edge: connect, then wait\n");
    int c2 = connect_one();
    wait0(ep, "queue nonempty");
    wait0(ep, "second wait, queue still nonempty");

    printf("C. drop-then-rise entirely between waits\n");
    int a2 = accept(listener_fd, NULL, NULL);
    int c3 = connect_one();
    wait0(ep, "drained then refilled between waits");

    printf("D. rise with no intervening drop\n");
    int c4 = connect_one();
    wait0(ep, "second connection queued, never drained");

    printf("E. ADD of an already-ready target\n");
    int ep2 = epoll_create1(0);
    struct epoll_event ev2; memset(&ev2, 0, sizeof ev2);
    ev2.events = EPOLLIN | EPOLLET; ev2.data.u64 = 0x5678;
    epoll_ctl(ep2, EPOLL_CTL_ADD, listener_fd, &ev2);
    wait0(ep2, "fresh port, target already readable");

    (void)c1;(void)a1;(void)c2;(void)a2;(void)c3;(void)c4;
    return 0;
}
