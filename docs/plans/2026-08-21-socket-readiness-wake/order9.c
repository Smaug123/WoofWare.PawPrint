// Is the connect-completion wake keyed?  The completion raises OUT; a
// registration watching IN only (whose idle HUP edge is consumed first) misses
// that key — so if the wake is keyed the completion leaves no trace and a
// later MOD enqueues fresh behind newer edges, and if it is unkeyed (a
// sk_state_change wake, like the FIN's) the entry keeps the completion's
// position.  No intermediate wait after the completion: it would consume the
// candidate being measured.
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

int main(void) {
    struct sockaddr_in a1, a2;
    int l1 = listener(&a1), l2 = listener(&a2);

    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    fcntl(c, F_SETFL, fcntl(c, F_GETFL) | O_NONBLOCK);

    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, l2, &ev);

    // Consume the idle ADD edge (the level's HUP is unmaskable).
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  idle drain -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");

    int r = connect(c, (struct sockaddr*)&a1, sizeof a1);
    printf("  (connect -> %d errno=%d)\n", r, errno);
    usleep(50000);

    // A newer edge elsewhere, then widen c's interest to include OUT.
    int c2 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c2, (struct sockaddr*)&a2, sizeof a2);
    ev.events = EPOLLIN | EPOLLOUT | EPOLLET; ev.data.u64 = 1;
    epoll_ctl(ep, EPOLL_CTL_MOD, c, &ev);

    n = epoll_wait(ep, evs, 8, 0);
    printf("  after l2 edge then MOD c -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");
    return 0;
}
