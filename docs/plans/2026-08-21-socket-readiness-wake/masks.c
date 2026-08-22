// What readiness mask does each socket phase present?  Level-triggered
// epoll_wait with timeout 0 reports the current level directly, so every row
// here registers LT with the widest interest (IN|OUT|RDHUP) unless the row is
// specifically about interest filtering.
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

static void mask_of(int fd, uint32_t interest, const char *label) {
    int ep = epoll_create1(0);
    struct epoll_event ev; memset(&ev, 0, sizeof ev);
    ev.events = interest; ev.data.u64 = 1;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev) != 0) { printf("  %-52s ADD failed errno=%d\n", label, errno); close(ep); return; }
    struct epoll_event evs[8];
    int n = epoll_wait(ep, evs, 8, 0);
    printf("  %-52s -> %d event(s)", label, n);
    for (int i = 0; i < n; i++) printf(" events=0x%x", evs[i].events);
    printf("\n");
    close(ep);
}

#define ALL (EPOLLIN | EPOLLOUT | EPOLLRDHUP)

static int listener(struct sockaddr_in *out, int port) {
    int fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in a; memset(&a, 0, sizeof a);
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK); a.sin_port = htons(port);
    bind(fd, (struct sockaddr*)&a, sizeof a);
    listen(fd, 8);
    if (out) { socklen_t sl = sizeof *out; getsockname(fd, (struct sockaddr*)out, &sl); }
    return fd;
}

static void set_nonblock(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

int main(void) {
    struct sockaddr_in la;
    int l = listener(&la, 0);

    // 1-2: idle stream sockets.
    int fresh = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    mask_of(fresh, ALL, "1. fresh idle TCP, unbound");
    struct sockaddr_in ba; memset(&ba, 0, sizeof ba);
    ba.sin_family = AF_INET; ba.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ba.sin_port = 0;
    int boundidle = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    bind(boundidle, (struct sockaddr*)&ba, sizeof ba);
    mask_of(boundidle, ALL, "2. bound idle TCP");

    // 3-5: the listener across its queue states.
    mask_of(l, ALL, "3. listening, queue empty");
    int c1 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c1, (struct sockaddr*)&la, sizeof la);
    mask_of(l, ALL, "4. listening, queue nonempty");
    int a1 = accept(l, NULL, NULL);
    mask_of(l, ALL, "5. listening, queue drained");

    // 6-8: the established family.  c1 connected blocking, so it is
    // established-after-inline-completion; make a nonblocking one too.
    mask_of(c1, ALL, "6. established client (blocking connect)");
    mask_of(a1, ALL, "7. accepted server end");
    int c2 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c2);
    int r = connect(c2, (struct sockaddr*)&la, sizeof la);
    printf("  (8: first nonblocking connect -> %d errno=%d)\n", r, errno);
    usleep(50000);
    mask_of(c2, ALL, "8. nb connect completed, not yet reported");
    int r2 = connect(c2, (struct sockaddr*)&la, sizeof la);
    printf("  (9: reporting connect -> %d errno=%d)\n", r2, errno);
    mask_of(c2, ALL, "9. nb connect completed, reported (established)");

    // 10-11: refusal, before and after the guest consumes it.  Refused port:
    // bind-then-close so nothing listens there.
    struct sockaddr_in dead; memset(&dead, 0, sizeof dead);
    int tmp = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in ta; memset(&ta, 0, sizeof ta);
    ta.sin_family = AF_INET; ta.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ta.sin_port = 0;
    bind(tmp, (struct sockaddr*)&ta, sizeof ta);
    socklen_t sl = sizeof dead; getsockname(tmp, (struct sockaddr*)&dead, &sl);
    close(tmp);
    int c3 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c3);
    r = connect(c3, (struct sockaddr*)&dead, sizeof dead);
    printf("  (10: refused nb connect -> %d errno=%d)\n", r, errno);
    usleep(50000);
    mask_of(c3, ALL, "10. refusal pending, undelivered");
    r2 = connect(c3, (struct sockaddr*)&dead, sizeof dead);
    printf("  (11: delivering connect -> %d errno=%d)\n", r2, errno);
    mask_of(c3, ALL, "11. refusal delivered (socket reset)");

    // 12: established with a closed peer.
    int c4 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c4, (struct sockaddr*)&la, sizeof la);
    int a4 = accept(l, NULL, NULL);
    close(a4);
    usleep(50000);
    mask_of(c4, ALL, "12. established, peer closed");

    // 13-14: datagram sockets.
    int u1 = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    mask_of(u1, ALL, "13. fresh idle UDP");
    struct sockaddr_in ua; memset(&ua, 0, sizeof ua);
    ua.sin_family = AF_INET; ua.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ua.sin_port = htons(9); // discard
    int u2 = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    connect(u2, (struct sockaddr*)&ua, sizeof ua);
    mask_of(u2, ALL, "14. UDP with a default peer");

    // 15-17: interest filtering.  Does a narrowed interest hide OUT, and are
    // ERR/HUP reported with no interest bits at all?
    mask_of(c1, EPOLLIN, "15. established client, interest IN only");
    int c5 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c5);
    connect(c5, (struct sockaddr*)&dead, sizeof dead);
    usleep(50000);
    mask_of(c5, 0, "16. refusal pending, interest 0");
    mask_of(c5, EPOLLIN, "17. refusal pending, interest IN only");

    (void)a1; (void)c2; (void)c3; (void)c4; (void)u1; (void)u2; (void)fresh; (void)boundidle;
    return 0;
}
