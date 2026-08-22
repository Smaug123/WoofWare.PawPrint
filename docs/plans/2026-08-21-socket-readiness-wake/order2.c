// Three refinements of order.c's F/G rows, each aimed at a specific stamp-model
// design: does a re-signal move an entry already on the ready list, where does
// an ADD-of-ready insert relative to older edges, and what happens to the rows
// a too-small maxevents leaves behind?
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

static void wait_n(int ep, int maxevents, const char *label) {
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, maxevents, 0);
    printf("  %-52s -> %d:", label, n);
    for (int i = 0; i < n; i++) printf(" data=%llu", (unsigned long long)evs[i].data.u64);
    printf("\n");
}

static void add(int ep, int fd, unsigned long long data) {
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = data;
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev);
}

int main(void) {
    printf("H. re-signal of an entry already pending: edges l2, l1, l2\n");
    {
        struct sockaddr_in a1, a2;
        int l1 = listener(&a1), l2 = listener(&a2);
        int ep = epoll_create1(0);
        add(ep, l1, 1); add(ep, l2, 2);
        connect_to(&a2); connect_to(&a1); connect_to(&a2);
        wait_n(ep, 8, "does l2's second edge move it behind l1?");
        close(l1); close(l2); close(ep);
    }

    printf("I. ADD of an already-ready target, after another edge\n");
    {
        // l2 becomes readable while unregistered, then l1 (registered) gets an
        // edge, then l2 is ADDed.  Does l2 report at its edge's place or its
        // ADD's place?
        struct sockaddr_in a1, a2;
        int l1 = listener(&a1), l2 = listener(&a2);
        int ep = epoll_create1(0);
        add(ep, l1, 1);
        connect_to(&a2);   // l2 readable, unregistered: no edge recorded anywhere
        connect_to(&a1);   // l1's edge
        add(ep, l2, 2);    // l2 enters, already readable
        wait_n(ep, 8, "edge l1 before ADD of ready l2");
        close(l1); close(l2); close(ep);
    }

    printf("J. truncation: three ready, maxevents=2, then drain the rest\n");
    {
        struct sockaddr_in a1, a2, a3;
        int l1 = listener(&a1), l2 = listener(&a2), l3 = listener(&a3);
        int ep = epoll_create1(0);
        add(ep, l1, 1); add(ep, l2, 2); add(ep, l3, 3);
        connect_to(&a1); connect_to(&a2); connect_to(&a3);
        wait_n(ep, 2, "first wait, maxevents=2");
        wait_n(ep, 8, "second wait, the remainder");
        wait_n(ep, 8, "third wait, expect empty");
        close(l1); close(l2); close(l3); close(ep);
    }

    printf("K. MOD of an already-reported, still-ready target\n");
    {
        struct sockaddr_in a1;
        int l1 = listener(&a1);
        int ep = epoll_create1(0);
        add(ep, l1, 1);
        connect_to(&a1);
        wait_n(ep, 8, "reported once");
        wait_n(ep, 8, "ET: second wait empty");
        struct epoll_event ev; memset(&ev, 0, sizeof ev);
        ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 1;
        epoll_ctl(ep, EPOLL_CTL_MOD, l1, &ev);
        wait_n(ep, 8, "after MOD with identical interest");
        close(l1); close(ep);
    }

    return 0;
}
