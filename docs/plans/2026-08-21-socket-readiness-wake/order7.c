// One connect makes two registrations ready: the client (completion, OUT) and
// the listener (accept queue, IN). Which edge enters the ready list first?
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>

int main(void) {
    int l = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in a; memset(&a, 0, sizeof a);
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK); a.sin_port = 0;
    bind(l, (struct sockaddr*)&a, sizeof a);
    listen(l, 8);
    socklen_t sl = sizeof a;
    getsockname(l, (struct sockaddr*)&a, &sl);

    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    fcntl(c, F_SETFL, fcntl(c, F_GETFL) | O_NONBLOCK);

    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLOUT | EPOLLET; ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev);
    ev.events = EPOLLIN | EPOLLET;            ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, l, &ev);

    // Consume the client's idle OUT|HUP edge so only the completion remains.
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  pre-connect drain -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");

    int r = connect(c, (struct sockaddr*)&a, sizeof a);
    printf("  (connect -> %d errno=%d)\n", r, errno);
    usleep(50000);

    n = epoll_wait(ep, evs, 8, 0);
    printf("  post-connect batch -> %d:", n);
    for (int i = 0; i < n; i++) printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");
    return 0;
}
