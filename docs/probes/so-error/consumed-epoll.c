// Linux: the epoll level and edges of a refused socket whose SO_ERROR has been
// read, and what the ECONNABORTED connect that follows leaves behind.
//
// Linux only (epoll); output beside this file.
//
//     cc -Wall -o p consumed-epoll.c && ./p
//     container run --rm -v "$PWD:/probe" debian:trixie sh -c \
//       'apt-get update -qq && apt-get install -y -qq gcc libc6-dev >/dev/null && gcc -Wall -o /tmp/p /probe/consumed-epoll.c && uname -r && /tmp/p'
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static struct sockaddr_in dead;
static void dead_port(void) {
    int t = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    memset(&dead, 0, sizeof dead); dead.sin_family = AF_INET; dead.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(t, (struct sockaddr *)&dead, sizeof dead);
    socklen_t sl = sizeof dead; getsockname(t, (struct sockaddr *)&dead, &sl); close(t);
}
static void wait_n(int ep, const char *label) {
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  %-48s -> %d:", label, n);
    for (int i = 0; i < n; i++) printf(" events=0x%x", evs[i].events);
    printf("\n");
}
static void add(int ep, int fd, unsigned flags) {
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLPRI | flags; ev.data.u64 = 1;
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev);
}
static void nb(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }
static int soerr(int fd) { int v = -7; socklen_t l = sizeof v; getsockopt(fd, SOL_SOCKET, SO_ERROR, &v, &l); return v; }
static void name(int fd, const char *label) {
    struct sockaddr_in a; socklen_t l = sizeof a; memset(&a, 0, sizeof a);
    getsockname(fd, (struct sockaddr *)&a, &l);
    printf("  getsockname %-36s %s:%u\n", label, inet_ntoa(a.sin_addr), ntohs(a.sin_port));
}
static int refused(int bindaddr) {
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); nb(c);
    if (bindaddr >= 0) {
        struct sockaddr_in b; memset(&b, 0, sizeof b); b.sin_family = AF_INET; b.sin_addr.s_addr = htonl(bindaddr);
        bind(c, (struct sockaddr *)&b, sizeof b);
    }
    connect(c, (struct sockaddr *)&dead, sizeof dead); usleep(50000);
    return c;
}

int main(void) {
    dead_port();
    printf("R1. level-triggered\n");
    {
        int c = refused(-1); int ep = epoll_create1(0); add(ep, c, 0);
        wait_n(ep, "pending");
        printf("  (SO_ERROR %d)\n", soerr(c));
        wait_n(ep, "consumed");
        int r = connect(c, (struct sockaddr *)&dead, sizeof dead);
        printf("  (connect -> %d errno=%d)\n", r, errno);
        wait_n(ep, "after ECONNABORTED");
        close(c); close(ep);
    }
    printf("R2. edge-triggered\n");
    {
        int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); nb(c);
        int ep = epoll_create1(0); add(ep, c, EPOLLET);
        wait_n(ep, "registered idle: consume the edge");
        connect(c, (struct sockaddr *)&dead, sizeof dead); usleep(50000);
        wait_n(ep, "refusal arrived");
        wait_n(ep, "consumed");
        printf("  (SO_ERROR %d)\n", soerr(c));
        wait_n(ep, "after SO_ERROR: new edge?");
        int r = connect(c, (struct sockaddr *)&dead, sizeof dead);
        printf("  (connect -> %d errno=%d)\n", r, errno);
        wait_n(ep, "after ECONNABORTED: new edge?");
        close(c); close(ep);
    }
    printf("R3. edge-triggered, SO_ERROR read before the edge is collected\n");
    {
        int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); nb(c);
        int ep = epoll_create1(0); add(ep, c, EPOLLET);
        wait_n(ep, "registered idle: consume the edge");
        connect(c, (struct sockaddr *)&dead, sizeof dead); usleep(50000);
        printf("  (SO_ERROR %d)\n", soerr(c));
        wait_n(ep, "pending edge, collected after the read");
        close(c); close(ep);
    }
    printf("R4. what the binding is after ECONNABORTED, by provenance\n");
    {
        int provs[3] = { -1, INADDR_LOOPBACK, INADDR_ANY };
        const char *names[3] = { "implicit", "bound 127.0.0.1", "bound 0.0.0.0" };
        for (int i = 0; i < 3; i++) {
            int c = refused(provs[i]);
            printf(" %s\n", names[i]);
            name(c, "pending");
            soerr(c);
            name(c, "consumed");
            connect(c, (struct sockaddr *)&dead, sizeof dead);
            printf("  (errno=%d)\n", errno);
            name(c, "after ECONNABORTED");
            close(c);
        }
    }
    return 0;
}
