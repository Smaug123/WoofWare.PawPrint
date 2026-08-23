// Rows the first probe did not cover: the `*triggered` count over a mixed
// array, the "already ready returns immediately whatever the timeout" claim,
// Darwin's post-refusal `Dead` latch level, and the output-only rule at
// events=0 for a refused socket.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <time.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>

static void show(short r) {
    printf("0x%04x", (unsigned short)r);
    if (r & POLLIN)   printf(" IN");
    if (r & POLLPRI)  printf(" PRI");
    if (r & POLLOUT)  printf(" OUT");
    if (r & POLLERR)  printf(" ERR");
    if (r & POLLHUP)  printf(" HUP");
    if (r & POLLNVAL) printf(" NVAL");
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
static double now_ms(void) {
    struct timespec t; clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000.0 + t.tv_nsec / 1e6;
}

int main(void) {
    struct sockaddr_in la;
    int l = listener(&la, 8);
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    connect(c, (struct sockaddr*)&la, sizeof la);
    int s = accept(l, NULL, NULL);
    int emptylistener = listener(NULL, 8);

    printf("== *triggered / rv over a mixed array ==\n");
    // established (OUT), empty listener (0 on both), never-opened (NVAL),
    // fd -1 (ignored), established again (OUT).
    struct pollfd fds[5];
    fds[0].fd = c;             fds[0].events = POLLIN | POLLOUT; fds[0].revents = 0;
    fds[1].fd = emptylistener; fds[1].events = POLLIN | POLLOUT; fds[1].revents = 0;
    fds[2].fd = 4096;          fds[2].events = POLLIN | POLLOUT; fds[2].revents = 0;
    fds[3].fd = -1;            fds[3].events = POLLIN | POLLOUT; fds[3].revents = 0;
    fds[4].fd = s;             fds[4].events = POLLIN | POLLOUT; fds[4].revents = 0;
    int rv = poll(fds, 5, 0);
    printf("  rv=%d (entries with nonzero revents should equal it)\n", rv);
    for (int i = 0; i < 5; i++) { printf("    [%d] fd=%-5d revents=", i, fds[i].fd); show(fds[i].revents); printf("\n"); }

    printf("== does a ready fd return immediately at timeout -1 and 5000? ==\n");
    for (int t = 0; t < 2; t++) {
        int timeout = t == 0 ? -1 : 5000;
        struct pollfd p; p.fd = c; p.events = POLLOUT; p.revents = 0;
        double t0 = now_ms();
        rv = poll(&p, 1, timeout);
        printf("  timeout=%-5d rv=%d elapsed=%.1fms revents=", timeout, rv, now_ms() - t0);
        show(p.revents); printf("\n");
    }

    printf("== events=0 on a refused socket (output-only rule) ==\n");
    int dead = listener(NULL, 1);
    struct sockaddr_in da; socklen_t dl = sizeof da;
    getsockname(dead, (struct sockaddr*)&da, &dl);
    close(dead);
    int r1 = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(r1);
    connect(r1, (struct sockaddr*)&da, sizeof da);
    usleep(300000);
    struct pollfd p0; p0.fd = r1; p0.events = 0; p0.revents = 0;
    rv = poll(&p0, 1, 0);
    printf("  refused, events=0        rv=%d revents=", rv); show(p0.revents); printf("\n");

    printf("== Darwin's post-refusal Dead latch ==\n");
    // Deliver the refusal with a second connect (the transition the model
    // makes), then poll. On Linux this resets to Idle; on Darwin it latches Dead.
    int rc = connect(r1, (struct sockaddr*)&da, sizeof da);
    printf("  (delivering connect rc=%d errno=%d)\n", rc, errno);
    rc = connect(r1, (struct sockaddr*)&da, sizeof da);
    printf("  (connect again rc=%d errno=%d)\n", rc, errno);
    struct pollfd p1; p1.fd = r1; p1.events = POLLIN | POLLPRI | POLLOUT; p1.revents = 0;
    rv = poll(&p1, 1, 0);
    printf("  after refusal delivered  rv=%d revents=", rv); show(p1.revents); printf("\n");

    printf("== large nfds ==\n");
    unsigned counts[] = { 256, 257, 1024 };
    for (unsigned i = 0; i < 3; i++) {
        unsigned n = counts[i];
        struct pollfd *big = calloc(n, sizeof *big);
        for (unsigned j = 0; j < n; j++) { big[j].fd = -1; big[j].events = POLLIN; }
        big[0].fd = c; big[0].events = POLLOUT;
        errno = 0;
        rv = poll(big, n, 0);
        printf("  nfds=%-5u rv=%d errno=%d revents[0]=", n, rv, errno); show(big[0].revents); printf("\n");
        free(big);
    }
    return 0;
}
