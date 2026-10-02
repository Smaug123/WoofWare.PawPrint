// read(2) and write(2) on a socket that a connect left without a peer, which
// this kernel models as the same idle phase a fresh socket is in: whether each
// answers as a fresh socket does (socket-unconnected-transfer.c).
//
// The rows: a TCP socket whose blocking connect was refused; one whose
// non-blocking connect was refused and whose refusal a second connect then
// reported; the same with the refusal reported by getsockopt(SO_ERROR) instead;
// and a UDP socket connected to a peer and then dissolved by an AF_UNSPEC
// connect. Each then reads 1 byte (non-blocking) and writes 1 byte, with
// SIGPIPE caught.
//
// Build: `nix develop -c clang -O0 -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -o /tmp/p <this file>` on Linux.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static volatile sig_atomic_t handled = 0;
static void on_pipe(int s) { (void)s; handled++; }

static struct sockaddr_in closed_port(void) {
    // A port bound and then closed, so nothing listens on it.
    int s = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(s, (struct sockaddr *)&a, sizeof a);
    socklen_t l = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &l);
    close(s);
    return a;
}

static void transfer(const char *label, int fd) {
    char b[1] = {'x'};
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
    handled = 0;
    ssize_t r = read(fd, b, 1);
    int re = r < 0 ? errno : 0;
    ssize_t w = write(fd, b, 1);
    int we = w < 0 ? errno : 0;
    printf("%-48s read -> %zd %s; write -> %zd %s; SIGPIPE %d\n", label, r, r < 0 ? strerror(re) : "", w,
           w < 0 ? strerror(we) : "", (int)handled);
}

int main(void) {
    alarm(30);
    setvbuf(stdout, NULL, _IOLBF, 0);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_pipe;
    sigaction(SIGPIPE, &sa, NULL);

    struct sockaddr_in dead = closed_port();
    int one;

    one = socket(AF_INET, SOCK_STREAM, 0);
    int c = connect(one, (struct sockaddr *)&dead, sizeof dead);
    printf("blocking connect -> %d %s\n", c, c < 0 ? strerror(errno) : "");
    transfer("TCP, blocking connect refused", one);
    close(one);

    one = socket(AF_INET, SOCK_STREAM, 0);
    fcntl(one, F_SETFL, O_NONBLOCK);
    c = connect(one, (struct sockaddr *)&dead, sizeof dead);
    struct pollfd p = {one, POLLOUT, 0};
    poll(&p, 1, 1000);
    c = connect(one, (struct sockaddr *)&dead, sizeof dead);
    printf("second connect -> %d %s\n", c, c < 0 ? strerror(errno) : "");
    transfer("TCP, non-blocking refusal reported by connect", one);
    close(one);

    one = socket(AF_INET, SOCK_STREAM, 0);
    fcntl(one, F_SETFL, O_NONBLOCK);
    c = connect(one, (struct sockaddr *)&dead, sizeof dead);
    p.fd = one;
    poll(&p, 1, 1000);
    int err = 0;
    socklen_t el = sizeof err;
    getsockopt(one, SOL_SOCKET, SO_ERROR, &err, &el);
    printf("SO_ERROR -> %s\n", strerror(err));
    transfer("TCP, non-blocking refusal reported by SO_ERROR", one);
    close(one);

    one = socket(AF_INET, SOCK_DGRAM, 0);
    c = connect(one, (struct sockaddr *)&dead, sizeof dead);
    struct sockaddr unspec;
    memset(&unspec, 0, sizeof unspec);
    unspec.sa_family = AF_UNSPEC;
    int d = connect(one, &unspec, sizeof unspec);
    printf("UDP connect -> %d, AF_UNSPEC connect -> %d %s\n", c, d, d < 0 ? strerror(errno) : "");
    transfer("UDP, connected then dissolved", one);
    close(one);
    return 0;
}
