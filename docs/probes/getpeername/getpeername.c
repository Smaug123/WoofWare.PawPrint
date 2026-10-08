// getpeername(2) in every socket phase this library models, and in the phases
// around them it does not (a reset, a connect still in progress, a shutdown).
//
// Both flavours; outputs beside this file.
//
//     cc -Wall -o p getpeername.c && ./p
//     container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/p /probe/getpeername.c && uname -r && /tmp/p'
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case ENOTCONN: return "ENOTCONN";
    case EINVAL: return "EINVAL";
    case EFAULT: return "EFAULT";
    case ENOTSOCK: return "ENOTSOCK";
    case ECONNRESET: return "ECONNRESET";
    case ECONNREFUSED: return "ECONNREFUSED";
    case EINPROGRESS: return "EINPROGRESS";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

static unsigned short port_of(int fd) {
    struct sockaddr_in a; socklen_t l = sizeof a; memset(&a, 0, sizeof a);
    getsockname(fd, (struct sockaddr *)&a, &l);
    return ntohs(a.sin_port);
}

// One getpeername, with the length cell preset to `cell`.
static void peer_len(const char *label, int fd, int cell) {
    unsigned char buf[128];
    memset(buf, 0xAA, sizeof buf);
    int c = cell;
    errno = 0;
    int r = getpeername(fd, (struct sockaddr *)buf, (socklen_t *)&c);
    int e = r == 0 ? 0 : errno;
    printf("%-58s cell=%-4d -> %s", label, cell, en(e));
    if (r == 0) {
        struct sockaddr_in *a = (struct sockaddr_in *)buf;
        printf(" family=%d %s:%u len=%d", a->sin_family, inet_ntoa(a->sin_addr), ntohs(a->sin_port), c);
    } else {
        printf(" cell-after=%d", c);
    }
    printf("\n");
}
static void peer(const char *label, int fd) { peer_len(label, fd, 16); }

static void nb(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }
static int tcp(void) { return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); }
static int udp(void) { return socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); }

static struct sockaddr_in sin_of(unsigned addr, unsigned short port) {
    struct sockaddr_in a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(addr); a.sin_port = htons(port);
    return a;
}

static int listener(int backlog) {
    int l = tcp();
    struct sockaddr_in a = sin_of(INADDR_LOOPBACK, 0);
    bind(l, (struct sockaddr *)&a, sizeof a);
    listen(l, backlog);
    return l;
}
static int dead_port(void) {
    int t = tcp();
    struct sockaddr_in a = sin_of(INADDR_LOOPBACK, 0);
    bind(t, (struct sockaddr *)&a, sizeof a);
    unsigned short p = port_of(t);
    close(t);
    return p;
}
static int connect_to(int c, unsigned addr, unsigned short port) {
    struct sockaddr_in a = sin_of(addr, port);
    errno = 0;
    int r = connect(c, (struct sockaddr *)&a, sizeof a);
    return r == 0 ? 0 : errno;
}

int main(void) {
    printf("ports are printed as found; the listener's is L, a client's ephemeral one varies\n");

    // TCP, never connected.
    { int s = tcp(); peer("T1 tcp fresh", s); peer_len("T1 tcp fresh", s, -1); peer_len("T1 tcp fresh", s, 0); close(s); }
    { int s = tcp(); struct sockaddr_in a = sin_of(INADDR_LOOPBACK, 0); bind(s, (struct sockaddr *)&a, sizeof a); peer("T2 tcp bound", s); close(s); }
    { int l = listener(4); printf("L=%u\n", port_of(l)); peer("T3 tcp listening", l); peer_len("T3 tcp listening", l, -1); close(l); }

    // TCP, connected.
    {
        int l = listener(4); unsigned short lp = port_of(l);
        printf("L=%u\n", lp);
        int c = tcp();
        printf("T4 connect -> %s; client port %u\n", en(connect_to(c, INADDR_LOOPBACK, lp)), port_of(c));
        peer("T4 client, connection queued", c);
        peer_len("T4 client, connection queued, cell 8", c, 8);
        peer_len("T4 client, connection queued, cell -1", c, -1);
        peer_len("T4 client, connection queued, cell 0", c, 0);
        peer_len("T4 client, connection queued, cell 128", c, 128);
        int s = accept(l, NULL, NULL);
        peer("T5 accepted server end", s);
        peer("T5 client after accept", c);
        close(s); usleep(50000);
        peer("T6 client, peer closed (FIN)", c);
        char b; errno = 0; ssize_t n = read(c, &b, 1);
        printf("T6 client read -> %zd %s\n", n, n < 0 ? en(errno) : "");
        peer("T6 client, peer closed, EOF read", c);
        close(c); close(l);
    }
    {
        int l = listener(4); unsigned short lp = port_of(l);
        int c = tcp(); connect_to(c, INADDR_LOOPBACK, lp);
        int s = accept(l, NULL, NULL);
        close(c); usleep(50000);
        peer("T7 server end, client closed (FIN)", s);
        close(s); close(l);
    }
    {
        // A reset: the peer closes with SO_LINGER {1, 0}.
        int l = listener(4); unsigned short lp = port_of(l);
        int c = tcp(); connect_to(c, INADDR_LOOPBACK, lp);
        int s = accept(l, NULL, NULL);
        struct linger lg = { 1, 0 };
        setsockopt(s, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
        close(s); usleep(50000);
        peer("T8 client, peer reset (linger 0)", c);
        int v = -1; socklen_t vl = sizeof v; getsockopt(c, SOL_SOCKET, SO_ERROR, &v, &vl);
        printf("T8 client SO_ERROR -> %s\n", en(v));
        peer("T8 client, peer reset, error taken", c);
        close(c); close(l);
    }
    {
        // The other direction: unread data at the closing end resets.
        int l = listener(4); unsigned short lp = port_of(l);
        int c = tcp(); connect_to(c, INADDR_LOOPBACK, lp);
        int s = accept(l, NULL, NULL);
        write(c, "x", 1); usleep(50000);
        close(s); usleep(50000);
        peer("T9 client, peer closed over unread data", c);
        close(c); close(l);
    }
    {
        // The listener closes with the connection still queued.
        int l = listener(4); unsigned short lp = port_of(l);
        int c = tcp(); connect_to(c, INADDR_LOOPBACK, lp);
        close(l); usleep(50000);
        peer("T10 client, listener closed with it queued", c);
        close(c);
    }
    {
        // Our own shutdown.
        int l = listener(4); unsigned short lp = port_of(l);
        int c = tcp(); connect_to(c, INADDR_LOOPBACK, lp);
        int s = accept(l, NULL, NULL);
        shutdown(c, SHUT_WR); usleep(20000);
        peer("T11 client, after shutdown(SHUT_WR)", c);
        shutdown(c, SHUT_RD); usleep(20000);
        peer("T11 client, after shutdown(SHUT_RD) too", c);
        close(c); close(s); close(l);
    }
    {
        // Wildcard destination.
        int l = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in a = sin_of(INADDR_ANY, 0); bind(l, (struct sockaddr *)&a, sizeof a); listen(l, 4);
        unsigned short lp = port_of(l);
        int c = tcp();
        printf("T12 connect 0.0.0.0 -> %s\n", en(connect_to(c, INADDR_ANY, lp)));
        peer("T12 client, connected to the wildcard", c);
        close(c); close(l);
    }

    // TCP, non-blocking connects.
    {
        int l = listener(4); unsigned short lp = port_of(l);
        int c = tcp(); nb(c);
        printf("N1 nonblocking connect -> %s\n", en(connect_to(c, INADDR_LOOPBACK, lp)));
        peer("N1 immediately after", c);
        usleep(50000);
        peer("N1 50ms later", c);
        printf("N1 connect again -> %s\n", en(connect_to(c, INADDR_LOOPBACK, lp)));
        peer("N1 after the second connect", c);
        close(c); close(l);
    }
    {
        unsigned short dp = dead_port();
        int c = tcp(); nb(c);
        printf("N2 nonblocking connect to a dead port -> %s\n", en(connect_to(c, INADDR_LOOPBACK, dp)));
        peer("N2 immediately after", c);
        usleep(50000);
        peer("N2 refusal pending", c);
        int v = -1; socklen_t vl = sizeof v; getsockopt(c, SOL_SOCKET, SO_ERROR, &v, &vl);
        printf("N2 SO_ERROR -> %s\n", en(v));
        peer("N2 refusal taken", c);
        close(c);
    }
    {
        unsigned short dp = dead_port();
        int c = tcp();
        printf("N3 blocking connect to a dead port -> %s\n", en(connect_to(c, INADDR_LOOPBACK, dp)));
        peer("N3 after a blocking refusal", c);
        close(c);
    }
    {
        // A connect that stays in progress: the accept queue is full.
        int l = listener(0); unsigned short lp = port_of(l);
        int filler[8]; int k;
        for (k = 0; k < 8; k++) { filler[k] = tcp(); nb(filler[k]); int e = connect_to(filler[k], INADDR_LOOPBACK, lp); usleep(20000);
            int v = -1; socklen_t vl = sizeof v; getsockopt(filler[k], SOL_SOCKET, SO_ERROR, &v, &vl);
            unsigned char buf[16]; socklen_t bl = sizeof buf; int r = getpeername(filler[k], (struct sockaddr *)buf, &bl);
            printf("N4 filler %d connect -> %s, SO_ERROR %s, getpeername %s\n", k, en(e), en(v), r == 0 ? "OK" : en(errno));
        }
        for (k = 0; k < 8; k++) close(filler[k]);
        close(l);
    }

    // UDP.
    { int s = udp(); peer("U1 udp fresh", s); close(s); }
    {
        int r = udp(); struct sockaddr_in a = sin_of(INADDR_LOOPBACK, 0); bind(r, (struct sockaddr *)&a, sizeof a);
        unsigned short rp = port_of(r);
        int s = udp();
        printf("U2 connect -> %s, R=%u\n", en(connect_to(s, INADDR_LOOPBACK, rp)), rp);
        peer("U2 udp connected", s);
        printf("U3 connect 0.0.0.0 -> %s\n", en(connect_to(s, INADDR_ANY, rp)));
        peer("U3 udp connected to the wildcard", s);
        printf("U4 connect port 0 -> %s\n", en(connect_to(s, INADDR_LOOPBACK, 0)));
        peer("U4 udp connected to port 0", s);
        struct sockaddr_in u; memset(&u, 0, sizeof u);
#ifdef __APPLE__
        u.sin_len = sizeof u;
#endif
        u.sin_family = AF_UNSPEC;
        connect_to(s, INADDR_LOOPBACK, rp);
        errno = 0; int x = connect(s, (struct sockaddr *)&u, sizeof u);
        printf("U5 connect AF_UNSPEC -> %s\n", x == 0 ? "OK" : en(errno));
        peer("U5 udp after AF_UNSPEC", s);
        close(s); close(r);
    }
    {
        int r = udp(); struct sockaddr_in a = sin_of(INADDR_LOOPBACK, 0); bind(r, (struct sockaddr *)&a, sizeof a);
        unsigned short rp = port_of(r);
        int s = udp(); struct sockaddr_in b = sin_of(INADDR_ANY, 0); bind(s, (struct sockaddr *)&b, sizeof b);
        connect_to(s, INADDR_ANY, rp);
        peer("U6 wildcard-bound udp connected to the wildcard", s);
        close(s); close(r);
    }

    // Other families, never connected.
    { int s = socket(AF_INET6, SOCK_STREAM, 0); peer("F1 inet6 stream fresh", s); peer_len("F1 inet6 stream fresh", s, -1); close(s); }
    { int s = socket(AF_INET6, SOCK_DGRAM, 0); peer("F2 inet6 dgram fresh", s); close(s); }
    { int s = socket(AF_UNIX, SOCK_STREAM, 0); peer("F3 unix stream fresh", s); peer_len("F3 unix stream fresh", s, -1); close(s); }
    { int s = socket(AF_UNIX, SOCK_DGRAM, 0); peer("F4 unix dgram fresh", s); close(s); }
    return 0;
}
