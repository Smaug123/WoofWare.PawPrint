// Which state transitions re-signal a consumed edge-triggered registration,
// and does MOD move an entry that is already pending?
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
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

static void wait_n(int ep, const char *label) {
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  %-56s -> %d:", label, n);
    for (int i = 0; i < n; i++) printf(" data=%llu events=0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");
}

#define ALLI (EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET)

static void add(int ep, int fd, unsigned long long data) {
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = ALLI; ev.data.u64 = data;
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev);
}

static void set_nonblock(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

static void dead_port(struct sockaddr_in *out) {
    int tmp = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in ta; memset(&ta, 0, sizeof ta);
    ta.sin_family = AF_INET; ta.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ta.sin_port = 0;
    bind(tmp, (struct sockaddr*)&ta, sizeof ta);
    socklen_t sl = sizeof *out; getsockname(tmp, (struct sockaddr*)out, &sl);
    close(tmp);
}

int main(void) {
    printf("L. MOD of an entry already pending: does it move?\n");
    {
        struct sockaddr_in a1, a2;
        int l1 = listener(&a1), l2 = listener(&a2);
        int ep = epoll_create1(0);
        add(ep, l1, 1); add(ep, l2, 2);
        connect_to(&a2); connect_to(&a1);   // pending order [l2, l1]
        struct epoll_event ev; memset(&ev, 0, sizeof ev);
        ev.events = ALLI; ev.data.u64 = 2;
        epoll_ctl(ep, EPOLL_CTL_MOD, l2, &ev);
        wait_n(ep, "MOD l2 while pending; still [l2, l1]?");
        close(l1); close(l2); close(ep);
    }

    printf("M. refusal delivery (socket reset): does it re-signal?\n");
    {
        struct sockaddr_in dead; dead_port(&dead);
        int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        set_nonblock(c);
        int ep = epoll_create1(0);
        add(ep, c, 1);
        wait_n(ep, "registered idle: consume the OUT|HUP edge");
        connect(c, (struct sockaddr*)&dead, sizeof dead);
        usleep(50000);
        wait_n(ep, "refusal arrived: the error edge");
        wait_n(ep, "consumed");
        int r = connect(c, (struct sockaddr*)&dead, sizeof dead);
        printf("  (delivering connect -> %d errno=%d)\n", r, errno);
        wait_n(ep, "after delivery/reset: new edge?");
        close(c); close(ep);
    }

    printf("N. completion report (SUCCESS retry): does it re-signal?\n");
    {
        struct sockaddr_in a1;
        int l1 = listener(&a1);
        int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        set_nonblock(c);
        int ep = epoll_create1(0);
        add(ep, c, 1);
        wait_n(ep, "registered idle: consume the OUT|HUP edge");
        connect(c, (struct sockaddr*)&a1, sizeof a1);
        usleep(50000);
        wait_n(ep, "completion arrived: the OUT edge");
        wait_n(ep, "consumed");
        int r = connect(c, (struct sockaddr*)&a1, sizeof a1);
        printf("  (reporting connect -> %d errno=%d)\n", r, errno);
        wait_n(ep, "after the SUCCESS report: new edge?");
        close(l1); close(c); close(ep);
    }

    printf("O. UDP re-connect and dissolve: do they re-signal?\n");
    {
        int u = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        int ep = epoll_create1(0);
        add(ep, u, 1);
        wait_n(ep, "registered fresh UDP: consume the OUT edge");
        struct sockaddr_in ua; memset(&ua, 0, sizeof ua);
        ua.sin_family = AF_INET; ua.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ua.sin_port = htons(9);
        connect(u, (struct sockaddr*)&ua, sizeof ua);
        wait_n(ep, "after setting the default peer");
        struct sockaddr sa; memset(&sa, 0, sizeof sa);
        sa.sa_family = AF_UNSPEC;
        connect(u, &sa, sizeof sa);
        wait_n(ep, "after the AF_UNSPEC dissolve");
        close(u); close(ep);
    }

    printf("P. bind on a registered idle socket: does it signal?\n");
    {
        int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        int ep = epoll_create1(0);
        add(ep, c, 1);
        wait_n(ep, "registered idle: consume the OUT|HUP edge");
        struct sockaddr_in ba; memset(&ba, 0, sizeof ba);
        ba.sin_family = AF_INET; ba.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ba.sin_port = 0;
        bind(c, (struct sockaddr*)&ba, sizeof ba);
        wait_n(ep, "after bind");
        close(c); close(ep);
    }

    printf("Q. peer close, measured against the right connection\n");
    {
        struct sockaddr_in a1;
        int l1 = listener(&a1);
        int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        connect(c, (struct sockaddr*)&a1, sizeof a1);
        int srv = accept(l1, NULL, NULL);
        // LT mask after the peer closes.
        int epl = epoll_create1(0);
        struct epoll_event ev; memset(&ev, 0, sizeof ev);
        ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP; ev.data.u64 = 1;
        // ET first: consume, close, look for the edge.
        int ept = epoll_create1(0);
        add(ept, c, 1);
        wait_n(ept, "ET: consume the established OUT edge");
        close(srv);
        usleep(50000);
        wait_n(ept, "ET: after the peer closed");
        epoll_ctl(epl, EPOLL_CTL_ADD, c, &ev);
        wait_n(epl, "LT mask with the peer closed");
        close(l1); close(c); close(epl); close(ept);
    }

    return 0;
}
