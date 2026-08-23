// What does poll(2) report for each socket phase PawPrint's readiness model
// already knows, and does it agree with the epoll level measured in
// docs/plans/2026-08-21-socket-readiness-wake/masks.c?
//
// Every row polls with the widest interest the PAL can express
// (POLLIN|POLLPRI|POLLOUT -- the PAL converts only IN/PRI/OUT/ERR/HUP/NVAL,
// and ERR/HUP/NVAL are output-only), timeout 0, so what comes back is the
// current level.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define ASK (POLLIN | POLLPRI | POLLOUT)

static void show(short r) {
    printf("0x%04x", (unsigned short)r);
    if (r & POLLIN)   printf(" IN");
    if (r & POLLPRI)  printf(" PRI");
    if (r & POLLOUT)  printf(" OUT");
    if (r & POLLERR)  printf(" ERR");
    if (r & POLLHUP)  printf(" HUP");
    if (r & POLLNVAL) printf(" NVAL");
#ifdef POLLRDHUP
    if (r & POLLRDHUP) printf(" RDHUP");
#endif
}

static void poll_of(int fd, const char *label) {
    struct pollfd p; p.fd = fd; p.events = ASK; p.revents = 0;
    int rv = poll(&p, 1, 0);
    printf("  %-46s rv=%d revents=", label, rv);
    if (rv < 0) printf("errno=%d", errno); else show(p.revents);
    printf("\n");
}

static int listener(struct sockaddr_in *out, int backlog) {
    int fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in a; memset(&a, 0, sizeof a);
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK); a.sin_port = 0;
    bind(fd, (struct sockaddr*)&a, sizeof a);
    listen(fd, backlog);
    if (out) { socklen_t sl = sizeof *out; getsockname(fd, (struct sockaddr*)out, &sl); }
    return fd;
}

static void set_nonblock(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

int main(void) {
    printf("== socket phases ==\n");
    struct sockaddr_in la;
    int l = listener(&la, 8);

    int fresh = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    poll_of(fresh, "1. fresh idle TCP, unbound");

    struct sockaddr_in ba; memset(&ba, 0, sizeof ba);
    ba.sin_family = AF_INET; ba.sin_addr.s_addr = htonl(INADDR_LOOPBACK); ba.sin_port = 0;
    int boundidle = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    bind(boundidle, (struct sockaddr*)&ba, sizeof ba);
    poll_of(boundidle, "2. bound idle TCP");

    int udp = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    poll_of(udp, "3. fresh idle UDP");

    poll_of(l, "4. listening, queue empty");
    int c1 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c1, (struct sockaddr*)&la, sizeof la);
    poll_of(l, "5. listening, queue nonempty");
    poll_of(c1, "6. client, connected (blocking connect)");
    int s1 = accept(l, NULL, NULL);
    poll_of(s1, "7. accepted server end, no data");
    poll_of(l, "8. listening, queue drained again");

    // 8b: data available on the server end.
    write(c1, "z", 1);
    usleep(200000);
    poll_of(s1, "8b. established, one byte readable");
    { char b; read(s1, &b, 1); }
    poll_of(s1, "8c. established, byte drained again");

    // 9: established with a dead peer (peer closed, clean FIN).
    close(c1);
    usleep(200000);
    poll_of(s1, "9. established, peer closed (FIN), drained");
    close(s1);

    // 9b: peer closed with unread data still queued.
    int c1b = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c1b, (struct sockaddr*)&la, sizeof la);
    int s1b = accept(l, NULL, NULL);
    write(c1b, "z", 1);
    close(c1b);
    usleep(200000);
    poll_of(s1b, "9b. peer closed (FIN), one byte still queued");
    close(s1b);

    // 10-12: the non-blocking connect states.
    int c2 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c2);
    int rc = connect(c2, (struct sockaddr*)&la, sizeof la);
    printf("  (nonblocking connect to live listener rc=%d errno=%d)\n", rc, errno);
    poll_of(c2, "10. client, connect in flight (immediately)");
    usleep(200000);
    poll_of(c2, "11. same client, after completion");

    // 12-13: refused connect, before and after the error is consumed.
    int dead = listener(NULL, 1);
    struct sockaddr_in da; socklen_t dl = sizeof da;
    getsockname(dead, (struct sockaddr*)&da, &dl);
    close(dead);
    int c3 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c3);
    rc = connect(c3, (struct sockaddr*)&da, sizeof da);
    printf("  (nonblocking connect to closed port rc=%d errno=%d)\n", rc, errno);
    usleep(200000);
    poll_of(c3, "12. client, connect refused (pending)");
    int soerr = 0; socklen_t sl = sizeof soerr;
    getsockopt(c3, SOL_SOCKET, SO_ERROR, &soerr, &sl);
    printf("  (SO_ERROR read: %d)\n", soerr);
    poll_of(c3, "13. same client, after SO_ERROR consumed it");
    int soerr2 = 0; sl = sizeof soerr2;
    getsockopt(c3, SOL_SOCKET, SO_ERROR, &soerr2, &sl);
    printf("  (second SO_ERROR read: %d)\n", soerr2);
    rc = connect(c3, (struct sockaddr*)&da, sizeof da);
    printf("  (connect after consumed refusal rc=%d errno=%d)\n", rc, errno);

    printf("== SO_ERROR on the success path ==\n");
    int c4 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c4);
    rc = connect(c4, (struct sockaddr*)&la, sizeof la);
    printf("  (nonblocking connect rc=%d errno=%d)\n", rc, errno);
    usleep(200000);
    int se = -1; sl = sizeof se;
    rc = getsockopt(c4, SOL_SOCKET, SO_ERROR, &se, &sl);
    printf("  (SO_ERROR after completion: rc=%d value=%d optlen=%u)\n", rc, se, (unsigned)sl);
    rc = connect(c4, (struct sockaddr*)&la, sizeof la);
    printf("  (connect after SO_ERROR read: rc=%d errno=%d)\n", rc, errno);
    rc = connect(c4, (struct sockaddr*)&la, sizeof la);
    printf("  (connect again: rc=%d errno=%d)\n", rc, errno);

    printf("== non-socket and invalid fds ==\n");
    poll_of(-1, "14. fd -1");
    poll_of(4096, "15. fd 4096 (never opened)");
    int devnull = open("/dev/null", O_RDWR);
    poll_of(devnull, "16. /dev/null (character device)");
    int tmp = open("/tmp", O_RDONLY);
    poll_of(tmp, "17. a directory, O_RDONLY");
    int pfd[2]; pipe(pfd);
    poll_of(pfd[0], "18. pipe read end, empty, writer alive");
    poll_of(pfd[1], "19. pipe write end, space, reader alive");
    close(pfd[1]);
    poll_of(pfd[0], "20. pipe read end, writer closed");

    printf("== argument screens ==\n");
    struct pollfd p; p.fd = fresh; p.events = ASK; p.revents = 0;
    printf("  poll(nfds=0, timeout=0)          rv=%d errno=%d\n", poll(&p, 0, 0), errno);
    errno = 0;
    printf("  poll(fds=NULL, nfds=0)           rv=%d errno=%d\n", poll(NULL, 0, 0), errno);
    errno = 0;
    printf("  poll(fds=NULL, nfds=1)           rv=%d errno=%d\n", poll(NULL, 1, 0), errno);
    errno = 0;
    p.events = 0; p.revents = 0;
    printf("  poll(events=0 on idle TCP)       rv=%d revents=0x%x\n", poll(&p, 1, 0), (unsigned short)p.revents);
    errno = 0;
    p.fd = -3; p.events = ASK; p.revents = 0xff;
    printf("  poll(fd=-3)                      rv=%d revents=0x%x\n", poll(&p, 1, 0), (unsigned short)p.revents);
    return 0;
}
