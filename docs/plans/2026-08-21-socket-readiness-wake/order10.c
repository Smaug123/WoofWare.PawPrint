// Does a *parked* epoll_wait, woken by a masked-out state-change wake,
// consume the stale candidate before re-sleeping?  (A Codex review of the
// fuzzer branch conjectured yes: ep_poll's woken scan re-polls the entry,
// harvests nothing, and being EPOLLET it is not re-added — so unlike the
// waiterless order8.c row, the candidate does NOT keep the FIN's position.)
//
// Timeline: a waiter parks with a 300ms timeout; at ~100ms the peer FIN
// lands on a registration whose interest (EPOLLET only) the half-closed
// level misses; the waiter's timeout expires before anything else happens;
// then a newer edge arrives elsewhere and a MOD widens the interest.
// If the woken scan consumed the candidate, the MOD enqueues fresh and the
// batch is [l2; c]; if the candidate survived (as it does with no waiter),
// the batch is [c; l2].
#define _GNU_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
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

static void *waiter(void *arg) {
    int ep = *(int *)arg;
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 300);
    printf("  parked waiter returned %d:", n);
    for (int i = 0; i < n; i++)
        printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");
    return NULL;
}

int main(void) {
    struct sockaddr_in a1, a2;
    int l1 = listener(&a1), l2 = listener(&a2);

    // An established pair on l1.
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c, (struct sockaddr*)&a1, sizeof a1);
    int srv = accept(l1, NULL, NULL);

    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    // c watches nothing maskable: only the implicit ERR|HUP remain.
    ev.events = EPOLLET;           ev.data.u64 = 1; epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev);
    ev.events = EPOLLIN | EPOLLET; ev.data.u64 = 2; epoll_ctl(ep, EPOLL_CTL_ADD, l2, &ev);

    // Park the waiter, then land the FIN while it is parked.
    pthread_t t;
    pthread_create(&t, NULL, waiter, &ep);
    usleep(100000);
    close(srv);
    // Let the waiter's timeout expire before the next edge, so the final
    // batch is measured with no concurrent consumer.
    pthread_join(t, NULL);

    // A newer edge elsewhere, then widen c's interest.
    int c2 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c2, (struct sockaddr*)&a2, sizeof a2);
    usleep(50000);
    ev.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET; ev.data.u64 = 1;
    epoll_ctl(ep, EPOLL_CTL_MOD, c, &ev);

    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  after parked-FIN, l2 edge, MOD c -> %d:", n);
    for (int i = 0; i < n; i++)
        printf(" data=%llu/0x%x", (unsigned long long)evs[i].data.u64, evs[i].events);
    printf("\n");
    return 0;
}
