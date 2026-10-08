// Follow-ups to sockopt-options.c: Darwin's so_linger width, the order of the
// kind check against a null value buffer, negative lengths against the kind
// check, Linux's AF_UNIX SOCK_SEQPACKET, and whether an accepted socket takes
// the listener's options at accept or when its connection completed.
//
// Both flavours; outputs beside this file. Run as sockopt-options.c is.
#define _GNU_SOURCE
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EFAULT: return "EFAULT";
    case EINVAL: return "EINVAL";
    case ENOPROTOOPT: return "ENOPROTOOPT";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ENOTCONN: return "ENOTCONN";
    case EDOM: return "EDOM";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}
static int sso(int fd, int l, int n, const void *v, socklen_t len) { errno = 0; return setsockopt(fd, l, n, v, len) == 0 ? 0 : errno; }
static void gl(const char *label, int fd, int name) {
    struct linger g = { 77, 77 }; socklen_t l = sizeof g;
    int r = getsockopt(fd, SOL_SOCKET, name, &g, &l);
    printf("  %-50s -> %s {%d,%d}\n", label, r == 0 ? "OK" : en(errno), g.l_onoff, g.l_linger);
}
static uint16_t port4(int fd) { struct sockaddr_in a; socklen_t l = sizeof a; getsockname(fd, (struct sockaddr *)&a, &l); return ntohs(a.sin_port); }
static struct sockaddr_in lo4(uint16_t port) {
    struct sockaddr_in a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET; a.sin_port = htons(port); a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    return a;
}

int main(void) {
    printf("W. linger time width\n");
    int ws[] = { 40000, 70000, -40000, 0x7fffffff, (int)0x80000000 };
    for (int i = 0; i < 5; i++) {
        int fd = socket(AF_INET, SOCK_STREAM, 0);
        struct linger g = { 0, ws[i] };
        char label[80]; snprintf(label, sizeof label, "SO_LINGER set {0,%d}: %s, read SO_LINGER", ws[i], en(sso(fd, SOL_SOCKET, SO_LINGER, &g, sizeof g)));
        gl(label, fd, SO_LINGER);
#ifdef SO_LINGER_SEC
        gl("  read SO_LINGER_SEC", fd, SO_LINGER_SEC);
#endif
        close(fd);
    }
#ifdef SO_LINGER_SEC
    int ss[] = { 400, 30000000, -30000000, 21474836, 21474837, 0x7fffffff };
    for (int i = 0; i < 6; i++) {
        int fd = socket(AF_INET, SOCK_STREAM, 0);
        struct linger g = { 0, ss[i] };
        char label[80]; snprintf(label, sizeof label, "SO_LINGER_SEC set {0,%d}: %s, read SO_LINGER", ss[i], en(sso(fd, SOL_SOCKET, SO_LINGER_SEC, &g, sizeof g)));
        gl(label, fd, SO_LINGER);
        gl("  read SO_LINGER_SEC", fd, SO_LINGER_SEC);
        close(fd);
    }
    {
        // An EDOM set leaves what was there?
        int fd = socket(AF_INET, SOCK_STREAM, 0);
        struct linger a = { 1, 9 }, b = { 1, 40000 };
        sso(fd, SOL_SOCKET, SO_LINGER, &a, sizeof a);
        char label[80]; snprintf(label, sizeof label, "{1,9} then {1,40000}: %s", en(sso(fd, SOL_SOCKET, SO_LINGER, &b, sizeof b)));
        gl(label, fd, SO_LINGER);
        close(fd);
    }
#endif

    printf("K. kind check against null and faulting buffers, and negative lengths\n");
    struct { const char *name; int domain, type, level, opt; } ks[] = {
        { "inet dgram TCP_NODELAY", AF_INET, SOCK_DGRAM, IPPROTO_TCP, TCP_NODELAY },
        { "inet6 dgram TCP_NODELAY", AF_INET6, SOCK_DGRAM, IPPROTO_TCP, TCP_NODELAY },
        { "unix stream TCP_NODELAY", AF_UNIX, SOCK_STREAM, IPPROTO_TCP, TCP_NODELAY },
        { "unix dgram TCP_NODELAY", AF_UNIX, SOCK_DGRAM, IPPROTO_TCP, TCP_NODELAY },
        { "inet stream IPV6_V6ONLY", AF_INET, SOCK_STREAM, IPPROTO_IPV6, IPV6_V6ONLY },
        { "unix stream IPV6_V6ONLY", AF_UNIX, SOCK_STREAM, IPPROTO_IPV6, IPV6_V6ONLY },
#ifdef __linux__
        { "unix seqpacket TCP_NODELAY", AF_UNIX, SOCK_SEQPACKET, IPPROTO_TCP, TCP_NODELAY },
        { "unix seqpacket IPV6_V6ONLY", AF_UNIX, SOCK_SEQPACKET, IPPROTO_IPV6, IPV6_V6ONLY },
        { "unix seqpacket SO_LINGER", AF_UNIX, SOCK_SEQPACKET, SOL_SOCKET, SO_LINGER },
#endif
        { "inet stream TCP_NODELAY", AF_INET, SOCK_STREAM, IPPROTO_TCP, TCP_NODELAY },
        { "inet6 stream IPV6_V6ONLY", AF_INET6, SOCK_STREAM, IPPROTO_IPV6, IPV6_V6ONLY },
    };
    for (unsigned i = 0; i < sizeof ks / sizeof ks[0]; i++) {
        int fd = socket(ks[i].domain, ks[i].type, 0);
        int v = 1; socklen_t c; int r;
        c = 4; errno = 0; r = getsockopt(fd, ks[i].level, ks[i].opt, NULL, &c);
        printf("  %-34s get null value, cell 4        -> %s cell=%u\n", ks[i].name, r == 0 ? "OK" : en(errno), c);
        errno = 0; r = getsockopt(fd, ks[i].level, ks[i].opt, NULL, NULL);
        printf("  %-34s get null value, null cell     -> %s\n", ks[i].name, r == 0 ? "OK" : en(errno));
        c = 0x80000000u; errno = 0; r = getsockopt(fd, ks[i].level, ks[i].opt, &v, &c);
        printf("  %-34s get len 0x80000000           -> %s cell=%u\n", ks[i].name, r == 0 ? "OK" : en(errno), c);
        c = 0xffffffffu; errno = 0; r = getsockopt(fd, ks[i].level, ks[i].opt, &v, &c);
        printf("  %-34s get len 0xffffffff           -> %s cell=%u\n", ks[i].name, r == 0 ? "OK" : en(errno), c);
        printf("  %-34s set len 0x80000000           -> %s\n", ks[i].name, en(sso(fd, ks[i].level, ks[i].opt, &v, 0x80000000u)));
        printf("  %-34s set null, len 0x80000000     -> %s\n", ks[i].name, en(sso(fd, ks[i].level, ks[i].opt, NULL, 0x80000000u)));
        printf("  %-34s set null, len 4              -> %s\n", ks[i].name, en(sso(fd, ks[i].level, ks[i].opt, NULL, 4)));
        printf("  %-34s set null, len 0              -> %s\n", ks[i].name, en(sso(fd, ks[i].level, ks[i].opt, NULL, 0)));
        close(fd);
    }

    printf("Q. an option changed on the listener while a connection is queued\n");
    {
        int l = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in a = lo4(0);
        bind(l, (struct sockaddr *)&a, sizeof a); listen(l, 4);
        int c = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in d = lo4(port4(l));
        connect(c, (struct sockaddr *)&d, sizeof d); usleep(20000);
        int one = 1; sso(l, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
        struct linger lg = { 1, 7 }; sso(l, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
        int s = accept(l, NULL, NULL);
        int v = -1; socklen_t vl = sizeof v; getsockopt(s, IPPROTO_TCP, TCP_NODELAY, &v, &vl);
        printf("  accepted TCP_NODELAY = %d (listener set it after the connection queued)\n", v);
        gl("accepted SO_LINGER", s, SO_LINGER);
        close(s); close(c); close(l);
    }
    return 0;
}
