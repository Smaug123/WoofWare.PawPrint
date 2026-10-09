// Dual-mode IPv6 TCP sockets: an AF_INET6 stream socket talking to IPv4
// through v4-mapped addresses (`::ffff:a.b.c.d`).
//
// Both flavours; outputs beside this file.
//
//     cc -Wall -o p dual-mode.c && ./p
//     container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/p /probe/dual-mode.c && uname -r && /tmp/p'
//
// Ports are printed symbolically, so that two runs compare line for line:
// `L` is the listener's, `B` the port the row bound, `eph` any other non-zero
// port. A sockaddr's bytes are printed in full with its port bytes as `pp`.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/wait.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case ENOTCONN: return "ENOTCONN";
    case EINVAL: return "EINVAL";
    case EFAULT: return "EFAULT";
    case EISCONN: return "EISCONN";
    case EADDRINUSE: return "EADDRINUSE";
    case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
    case EAFNOSUPPORT: return "EAFNOSUPPORT";
    case ENETUNREACH: return "ENETUNREACH";
    case EHOSTUNREACH: return "EHOSTUNREACH";
    case ECONNREFUSED: return "ECONNREFUSED";
    case EINPROGRESS: return "EINPROGRESS";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case EACCES: return "EACCES";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EINTR: return "EINTR(timed out after 3s)";
    case EAGAIN: return "EAGAIN";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

static unsigned short L = 0, B = 0;

static const char *sym(unsigned short p) {
    static char b[4][16];
    static int i = 0;
    i = (i + 1) % 4;
    if (p == 0) return "0";
    if (p == L) return "L";
    if (p == B) return "B";
    snprintf(b[i], sizeof b[i], "eph");
    return b[i];
}

// The reported bytes, the port bytes (offset 2 and 3) masked.
static void dump(const unsigned char *buf, int n) {
    printf(" [");
    for (int i = 0; i < n; i++) {
        if (i == 2 || i == 3) printf("%spp", i ? " " : "");
        else printf("%s%02x", i ? " " : "", buf[i]);
    }
    printf("]");
}

static void addr_text(const unsigned char *buf, int n) {
    int family = buf[0] | (buf[1] << 8);
#ifdef __APPLE__
    family = buf[1];
#endif
    char text[64] = "?";
    unsigned short port = (unsigned short)((buf[2] << 8) | buf[3]);
    if (family == AF_INET && n >= 8) {
        inet_ntop(AF_INET, buf + 4, text, sizeof text);
        printf(" family=AF_INET %s:%s", text, sym(port));
    } else if (family == AF_INET6 && n >= 24) {
        inet_ntop(AF_INET6, buf + 8, text, sizeof text);
        unsigned flow, scope = 0;
        memcpy(&flow, buf + 4, 4);
        if (n >= 28) memcpy(&scope, buf + 24, 4);
        printf(" family=AF_INET6 [%s]:%s flowinfo=%u scope=%u", text, sym(port), ntohl(flow), scope);
    } else {
        printf(" family=%d", family);
    }
}

typedef int (*namefn)(int, struct sockaddr *, socklen_t *);

static void name_len(const char *label, namefn f, int fd, int cell) {
    unsigned char buf[128];
    memset(buf, 0xAA, sizeof buf);
    socklen_t c = (socklen_t)cell;
    errno = 0;
    int r = f(fd, (struct sockaddr *)buf, &c);
    int e = r == 0 ? 0 : errno;
    printf("%-60s cell=%-3d -> %s", label, cell, en(e));
    if (r == 0) {
        int shown = (int)c < cell ? (int)c : cell;
        printf(" len=%d", (int)c);
        addr_text(buf, shown);
        dump(buf, cell < 32 ? cell : 32);
    }
    printf("\n");
}

static void sockname(const char *label, int fd) { name_len(label, getsockname, fd, 28); }
static void peername(const char *label, int fd) { name_len(label, getpeername, fd, 28); }

static int tcp4(void) { return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); }
static int tcp6(void) { return socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP); }

static int set_int(int fd, int level, int name, int v) {
    return setsockopt(fd, level, name, &v, sizeof v);
}

static int tcp6_v6only(int v6only) {
    int s = tcp6();
    if (set_int(s, IPPROTO_IPV6, IPV6_V6ONLY, v6only) != 0) perror("IPV6_V6ONLY");
    return s;
}

static struct sockaddr_in sin_of(unsigned addr, unsigned short port) {
    struct sockaddr_in a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(addr); a.sin_port = htons(port);
    return a;
}

// `::ffff:` + the IPv4 address, or `::` when `mapped` is 0 and addr is 0, or
// `::1` when `mapped` is 0 and addr is 1.
static struct sockaddr_in6 sin6_of(int mapped, unsigned addr, unsigned short port) {
    struct sockaddr_in6 a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin6_len = sizeof a;
#endif
    a.sin6_family = AF_INET6;
    a.sin6_port = htons(port);
    if (mapped) {
        a.sin6_addr.s6_addr[10] = 0xff;
        a.sin6_addr.s6_addr[11] = 0xff;
        unsigned n = htonl(addr);
        memcpy(&a.sin6_addr.s6_addr[12], &n, 4);
    } else if (addr == 1) {
        a.sin6_addr.s6_addr[15] = 1;
    }
    return a;
}

static int result(int r) { return r == 0 ? 0 : errno; }

static int listener4(unsigned addr) {
    int l = tcp4();
    struct sockaddr_in a = sin_of(addr, 0);
    if (bind(l, (struct sockaddr *)&a, sizeof a) != 0) perror("listener bind");
    if (listen(l, 8) != 0) perror("listen");
    struct sockaddr_in got; socklen_t n = sizeof got;
    getsockname(l, (struct sockaddr *)&got, &n);
    L = ntohs(got.sin_port);
    return l;
}

static void on_alarm(int sig) { (void)sig; }

// A connect that a kernel leaves pending (a dropped SYN) is cut off by an
// alarm after 3 seconds, and reads EINTR.
static int timed_connect(int s, const struct sockaddr *a, socklen_t len) {
    alarm(3);
    int r = connect(s, a, len);
    int e = errno;
    alarm(0);
    errno = e;
    return r;
}

static void do_connect6(const char *label, int s, struct sockaddr_in6 a, int len) {
    errno = 0;
    int e = result(timed_connect(s, (struct sockaddr *)&a, (socklen_t)len));
    printf("%-60s -> %s\n", label, en(e));
}

// A port nothing holds: bind a v4 socket to the wildcard and port 0, read the
// port, close it.
static unsigned short free_port(void) {
    int s = tcp4();
    struct sockaddr_in a = sin_of(INADDR_ANY, 0);
    bind(s, (struct sockaddr *)&a, sizeof a);
    struct sockaddr_in got; socklen_t n = sizeof got;
    getsockname(s, (struct sockaddr *)&got, &n);
    close(s);
    return ntohs(got.sin_port);
}

// A: the fresh socket, and connect to a v4-mapped loopback.
static void section_a(void) {
    printf("== A: connect to ::ffff:127.0.0.1 ==\n");
    int s = tcp6_v6only(0);
    sockname("A1 fresh V6ONLY=0 getsockname", s);
    peername("A1 fresh V6ONLY=0 getpeername", s);
    name_len("A1 fresh getsockname, cell 16", getsockname, s, 16);
    name_len("A1 fresh getsockname, cell 128", getsockname, s, 128);
    name_len("A1 fresh getsockname, cell 0", getsockname, s, 0);
    close(s);

    int l = listener4(INADDR_LOOPBACK);
    s = tcp6_v6only(0);
    do_connect6("A2 V6ONLY=0 unbound, connect ::ffff:127.0.0.1:L", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    sockname("A2 after: getsockname", s);
    peername("A2 after: getpeername", s);
    name_len("A2 after: getpeername cell 16", getpeername, s, 16);
    name_len("A2 after: getpeername cell 24", getpeername, s, 24);
    name_len("A2 after: getsockname cell 128", getsockname, s, 128);
    int one = -1; socklen_t ol = sizeof one;
    int r = getsockopt(s, IPPROTO_IPV6, IPV6_V6ONLY, &one, &ol);
    printf("%-60s -> %s value=%d\n", "A2 after: getsockopt IPV6_V6ONLY", en(result(r)), one);
    printf("%-60s -> %s\n", "A2 after: setsockopt IPV6_V6ONLY=1", en(result(set_int(s, IPPROTO_IPV6, IPV6_V6ONLY, 1))));
    printf("%-60s -> %s\n", "A2 after: setsockopt IPV6_V6ONLY=0", en(result(set_int(s, IPPROTO_IPV6, IPV6_V6ONLY, 0))));
    do_connect6("A2 again: connect ::ffff:127.0.0.1:L", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    struct sockaddr_in6 peer6; socklen_t pl = sizeof peer6;
    int srv = accept(l, (struct sockaddr *)&peer6, &pl);
    printf("%-60s -> %s len=%d\n", "A2 accept on the AF_INET listener", en(srv >= 0 ? 0 : errno), (int)pl);
    name_len("A2 accepted: getsockname", getsockname, srv, 28);
    name_len("A2 accepted: getpeername", getpeername, srv, 28);
    // The accepted v4 end's peer port is the client's local port.
    {
        struct sockaddr_in6 me; socklen_t ml = sizeof me;
        getsockname(s, (struct sockaddr *)&me, &ml);
        struct sockaddr_in them; socklen_t tl = sizeof them;
        getpeername(srv, (struct sockaddr *)&them, &tl);
        printf("%-60s -> %s\n", "A2 client's local port = server's peer port", me.sin6_port == them.sin_port ? "yes" : "no");
    }
    // Bytes across.
    {
        char c = 'x';
        ssize_t w = write(s, &c, 1);
        char d = 0;
        ssize_t rd = read(srv, &d, 1);
        printf("%-60s -> wrote %zd read %zd '%c'\n", "A2 a byte client to server", w, rd, d);
    }
    close(srv);
    close(s);

    s = tcp6_v6only(1);
    do_connect6("A3 V6ONLY=1, connect ::ffff:127.0.0.1:L", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    sockname("A3 after: getsockname", s);
    do_connect6("A3 V6ONLY=1, again", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    close(s);

    // Non-blocking.
    s = tcp6_v6only(0);
    fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
    do_connect6("A4 V6ONLY=0 non-blocking, connect ::ffff:127.0.0.1:L", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    usleep(50000);
    do_connect6("A4 again", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    do_connect6("A4 and again", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    sockname("A4 getsockname", s);
    peername("A4 getpeername", s);
    srv = accept(l, NULL, NULL);
    close(srv);
    close(s);

    // A closed port.
    unsigned short closed = free_port();
    s = tcp6_v6only(0);
    do_connect6("A5 V6ONLY=0, connect ::ffff:127.0.0.1:<closed>", s, sin6_of(1, INADDR_LOOPBACK, closed), 28);
    sockname("A5 after: getsockname", s);
    peername("A5 after: getpeername", s);
    do_connect6("A5 again, to L", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    close(s);

    // Other v4-mapped destinations.
    s = tcp6_v6only(0);
    do_connect6("A6 V6ONLY=0, connect ::ffff:0.0.0.0:L", s, sin6_of(1, 0, L), 28);
    sockname("A6 after: getsockname", s);
    peername("A6 after: getpeername", s);
    srv = accept(l, NULL, NULL);
    if (srv >= 0) close(srv);
    close(s);

    s = tcp6_v6only(0);
    do_connect6("A7 V6ONLY=0, connect ::ffff:127.0.0.2:L (listener 127.0.0.1)", s, sin6_of(1, 0x7F000002u, L), 28);
    sockname("A7 after: getsockname", s);
    close(s);

    s = tcp6_v6only(0);
    do_connect6("A8 V6ONLY=0, connect ::ffff:127.0.0.1:0", s, sin6_of(1, INADDR_LOOPBACK, 0), 28);
    sockname("A8 after: getsockname", s);
    close(s);

    s = tcp6_v6only(0);
    do_connect6("A9 V6ONLY=0, connect ::ffff:224.0.0.1:L", s, sin6_of(1, 0xE0000001u, L), 28);
    sockname("A9 after: getsockname", s);
    close(s);

    s = tcp6_v6only(0);
    do_connect6("A10 V6ONLY=0, connect ::ffff:255.255.255.255:L", s, sin6_of(1, 0xFFFFFFFFu, L), 28);
    close(s);

    // Flowinfo and scope id on a v4-mapped destination.
    {
        s = tcp6_v6only(0);
        struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, L);
        a.sin6_flowinfo = htonl(0x12345);
        do_connect6("A11 V6ONLY=0, v4-mapped with flowinfo 0x12345", s, a, 28);
        peername("A11 getpeername", s);
        srv = accept(l, NULL, NULL);
        if (srv >= 0) close(srv);
        close(s);
        s = tcp6_v6only(0);
        a = sin6_of(1, INADDR_LOOPBACK, L);
        a.sin6_scope_id = 7;
        do_connect6("A12 V6ONLY=0, v4-mapped with scope id 7", s, a, 28);
        peername("A12 getpeername", s);
        srv = accept(l, NULL, NULL);
        if (srv >= 0) close(srv);
        close(s);
#ifdef __APPLE__
        s = tcp6_v6only(0);
        a = sin6_of(1, INADDR_LOOPBACK, L);
        a.sin6_len = 0;
        do_connect6("A13 V6ONLY=0, v4-mapped with sin6_len 0", s, a, 28);
        srv = accept(l, NULL, NULL);
        if (srv >= 0) close(srv);
        close(s);
#endif
    }

    // Lengths and families on an idle dual-mode socket, to the listener.
    {
        int lens[] = {0, 1, 2, 8, 16, 23, 24, 25, 27, 28, 29, 32, 128, 129, 255, 256};
        for (unsigned i = 0; i < sizeof lens / sizeof lens[0]; i++) {
            unsigned char buf[300];
            memset(buf, 0, sizeof buf);
            struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, L);
            memcpy(buf, &a, sizeof a);
            s = tcp6_v6only(0);
            char label[80];
            snprintf(label, sizeof label, "A14 AF_INET6 v4-mapped at length %d", lens[i]);
            errno = 0;
            int e = result(timed_connect(s, (struct sockaddr *)buf, (socklen_t)lens[i]));
            printf("%-60s -> %s", label, en(e));
            if (e == 0) { srv = accept(l, NULL, NULL); if (srv >= 0) close(srv); }
            unsigned char nb[32]; socklen_t nl = sizeof nb; memset(nb, 0, sizeof nb);
            getsockname(s, (struct sockaddr *)nb, &nl);
            printf(" bound-port=%s\n", sym((unsigned short)((nb[2] << 8) | nb[3])));
            close(s);
        }
        // AF_INET's sockaddr_in on a dual-mode socket.
        int lens4[] = {16, 28};
        for (unsigned i = 0; i < 2; i++) {
            unsigned char buf[64];
            memset(buf, 0, sizeof buf);
            struct sockaddr_in a = sin_of(INADDR_LOOPBACK, L);
            memcpy(buf, &a, sizeof a);
            s = tcp6_v6only(0);
            char label[80];
            snprintf(label, sizeof label, "A15 AF_INET sockaddr_in at length %d", lens4[i]);
            errno = 0;
            int e = result(timed_connect(s, (struct sockaddr *)buf, (socklen_t)lens4[i]));
            printf("%-60s -> %s\n", label, en(e));
            if (e == 0) { srv = accept(l, NULL, NULL); if (srv >= 0) close(srv); }
            close(s);
        }
        // AF_UNSPEC.
        {
            unsigned char buf[64];
            memset(buf, 0, sizeof buf);
            struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, L);
            a.sin6_family = AF_UNSPEC;
            memcpy(buf, &a, sizeof a);
            s = tcp6_v6only(0);
            errno = 0;
            int e = result(timed_connect(s, (struct sockaddr *)buf, 28));
            printf("%-60s -> %s\n", "A16 AF_UNSPEC at length 28, idle", en(e));
            sockname("A16 after: getsockname", s);
            close(s);
        }
    }

    close(l);
}

// N: native IPv6 destinations, which this library does not model; measured to
// know which errno a refusal stands in for.
static void section_n(void) {
    printf("== N: native IPv6 ==\n");
    unsigned short closed = free_port();
    int s = tcp6_v6only(0);
    do_connect6("N1 V6ONLY=0, connect [::1]:<closed>", s, sin6_of(0, 1, closed), 28);
    close(s);
    s = tcp6_v6only(0);
    do_connect6("N2 V6ONLY=0, connect [::]:<closed>", s, sin6_of(0, 0, closed), 28);
    close(s);
    int l = listener4(INADDR_LOOPBACK);
    s = tcp6_v6only(0);
    do_connect6("N3 V6ONLY=0, connect [::1]:L (only a v4 listener)", s, sin6_of(0, 1, L), 28);
    close(s);
    close(l);
}

static int bind4(int s, unsigned addr, unsigned short port) {
    struct sockaddr_in a = sin_of(addr, port);
    return result(bind(s, (struct sockaddr *)&a, sizeof a));
}

static int bind6(int s, int mapped, unsigned addr, unsigned short port) {
    struct sockaddr_in6 a = sin6_of(mapped, addr, port);
    return result(bind(s, (struct sockaddr *)&a, sizeof a));
}

// B: bind of a dual-mode socket on its own.
static void section_b(void) {
    printf("== B: bind ==\n");
    struct { const char *name; int mapped; unsigned addr; } addrs[] = {
        {"::ffff:127.0.0.1", 1, INADDR_LOOPBACK},
        {"::ffff:0.0.0.0", 1, 0},
        {"::", 0, 0},
        {"::1", 0, 1},
        {"::ffff:127.0.0.2", 1, 0x7F000002u},
        {"::ffff:10.255.255.1", 1, 0x0AFFFF01u},
        {"::ffff:224.0.0.1", 1, 0xE0000001u},
    };
    for (int v6only = 0; v6only < 2; v6only++) {
        for (unsigned i = 0; i < sizeof addrs / sizeof addrs[0]; i++) {
            for (int port0 = 0; port0 < 2; port0++) {
                int s = tcp6_v6only(v6only);
                B = port0 ? 0 : free_port();
                char label[100];
                snprintf(label, sizeof label, "B1 V6ONLY=%d bind [%s]:%s", v6only, addrs[i].name, port0 ? "0" : "B");
                printf("%-60s -> %s\n", label, en(bind6(s, addrs[i].mapped, addrs[i].addr, B)));
                sockname("   getsockname", s);
                close(s);
            }
        }
    }
    B = 0;
    // A dual-mode socket bound to a v4-mapped address, then V6ONLY.
    {
        int s = tcp6_v6only(0);
        bind6(s, 1, INADDR_LOOPBACK, 0);
        printf("%-60s -> %s\n", "B2 bound ::ffff:127.0.0.1, set IPV6_V6ONLY=1", en(result(set_int(s, IPPROTO_IPV6, IPV6_V6ONLY, 1))));
        close(s);
    }
    // Twice.
    {
        int s = tcp6_v6only(0);
        bind6(s, 1, INADDR_LOOPBACK, 0);
        printf("%-60s -> %s\n", "B3 bound ::ffff:127.0.0.1, bind again ::ffff:127.0.0.1:0", en(bind6(s, 1, INADDR_LOOPBACK, 0)));
        close(s);
    }
    // A sockaddr_in on a dual-mode socket.
    {
        int s = tcp6_v6only(0);
        printf("%-60s -> %s\n", "B4 V6ONLY=0 bind sockaddr_in 127.0.0.1:0 len 16", en(bind4(s, INADDR_LOOPBACK, 0)));
        close(s);
    }
    // Lengths.
    {
        int lens[] = {0, 8, 16, 23, 24, 27, 28, 29, 128, 129, 255, 256};
        for (unsigned i = 0; i < sizeof lens / sizeof lens[0]; i++) {
            unsigned char buf[300];
            memset(buf, 0, sizeof buf);
            struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, 0);
            memcpy(buf, &a, sizeof a);
            int s = tcp6_v6only(0);
            char label[80];
            snprintf(label, sizeof label, "B5 AF_INET6 ::ffff:127.0.0.1:0 at length %d", lens[i]);
            errno = 0;
            int e = result(bind(s, (struct sockaddr *)buf, (socklen_t)lens[i]));
            printf("%-60s -> %s\n", label, en(e));
            close(s);
        }
    }
    // listen on an unbound dual-mode socket.
    {
        int s = tcp6_v6only(0);
        printf("%-60s -> %s\n", "B6 V6ONLY=0 unbound listen", en(result(listen(s, 1))));
        sockname("B6 getsockname", s);
        close(s);
    }
}

// C: bind conflicts between an AF_INET socket and an AF_INET6 one on one port.
// `first` binds, optionally listens, and then `second` binds.
struct side { const char *name; int v6; int v6only; int mapped; unsigned addr; };

static int make(struct side d, int reuse) {
    int s = d.v6 ? tcp6_v6only(d.v6only) : tcp4();
    if (reuse) set_int(s, SOL_SOCKET, SO_REUSEADDR, 1);
    return s;
}

static int bind_side(int s, struct side d, unsigned short port) {
    return d.v6 ? bind6(s, d.mapped, d.addr, port) : bind4(s, d.addr, port);
}

static void section_c(void) {
    printf("== C: bind conflicts ==\n");
    struct side sides[] = {
        {"4 127.0.0.1", 0, 0, 0, INADDR_LOOPBACK},
        {"4 0.0.0.0", 0, 0, 0, 0},
        {"6d ::ffff:127.0.0.1", 1, 0, 1, INADDR_LOOPBACK},
        {"6d ::ffff:0.0.0.0", 1, 0, 1, 0},
        {"6d ::", 1, 0, 0, 0},
        {"6o ::", 1, 1, 0, 0},
        {"6d ::1", 1, 0, 0, 1},
        {"6o ::1", 1, 1, 0, 1},
    };
    int n = sizeof sides / sizeof sides[0];
    for (int listenFirst = 0; listenFirst < 2; listenFirst++)
        for (int reuse = 0; reuse < 4; reuse++)
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++) {
                    if (!sides[i].v6 && !sides[j].v6) continue;
                    unsigned short port = free_port();
                    int a = make(sides[i], reuse & 1);
                    int ea = bind_side(a, sides[i], port);
                    if (ea == 0 && listenFirst) ea = result(listen(a, 1));
                    int b = make(sides[j], (reuse >> 1) & 1);
                    int eb = ea == 0 ? bind_side(b, sides[j], port) : -1;
                    printf("C listen=%d reuse=%d%d [%-20s] then [%-20s] -> %s%s%s\n",
                        listenFirst, reuse & 1, (reuse >> 1) & 1, sides[i].name, sides[j].name,
                        ea == 0 ? en(eb) : "first-failed:", ea == 0 ? "" : " ", ea == 0 ? "" : en(ea));
                    close(a);
                    close(b);
                }
    // The second socket listening after the bind succeeded, both reuse: does
    // listen refuse (Linux's listen re-checks the port)?
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++) {
            if (!sides[i].v6 && !sides[j].v6) continue;
            unsigned short port = free_port();
            int a = make(sides[i], 1);
            int b = make(sides[j], 1);
            int ea = bind_side(a, sides[i], port);
            int eb = bind_side(b, sides[j], port);
            if (ea != 0 || eb != 0) { close(a); close(b); continue; }
            int la = result(listen(a, 1));
            int lb = result(listen(b, 1));
            printf("CL reuse=11 [%-20s] [%-20s] listen a -> %s, listen b -> %s\n", sides[i].name, sides[j].name, en(la), en(lb));
            close(a);
            close(b);
        }
}

// D: an AF_INET connection accepted by a dual-mode IPv6 listener.
static void section_d(void) {
    printf("== D: accept on a dual-mode listener ==\n");
    struct { const char *name; int mapped; unsigned addr; int v6only; } ls[] = {
        {"6d ::", 0, 0, 0},
        {"6d ::ffff:127.0.0.1", 1, INADDR_LOOPBACK, 0},
        {"6d ::ffff:0.0.0.0", 1, 0, 0},
        {"6o ::", 0, 0, 1},
    };
    for (unsigned i = 0; i < sizeof ls / sizeof ls[0]; i++) {
        int l = tcp6_v6only(ls[i].v6only);
        int eb = bind6(l, ls[i].mapped, ls[i].addr, 0);
        int el = result(listen(l, 4));
        // Non-blocking, so that an accept with nothing queued answers rather
        // than sleeping.
        fcntl(l, F_SETFL, fcntl(l, F_GETFL) | O_NONBLOCK);
        struct sockaddr_in6 me; socklen_t ml = sizeof me;
        getsockname(l, (struct sockaddr *)&me, &ml);
        L = ntohs(me.sin6_port);
        printf("D [%-20s] bind -> %s listen -> %s\n", ls[i].name, en(eb), en(el));
        int c = tcp4();
        struct sockaddr_in to = sin_of(INADDR_LOOPBACK, L);
        int ec = result(timed_connect(c, (struct sockaddr *)&to, sizeof to));
        printf("%-60s -> %s\n", "  AF_INET client connects 127.0.0.1:L", en(ec));
        if (ec == 0) {
            unsigned char buf[64]; memset(buf, 0xAA, sizeof buf);
            socklen_t al = 28;
            int srv = accept(l, (struct sockaddr *)buf, &al);
            printf("%-60s -> %s len=%d", "  accept, cell 28", en(srv >= 0 ? 0 : errno), (int)al);
            if (srv >= 0) { addr_text(buf, (int)al); dump(buf, 28); }
            printf("\n");
            if (srv >= 0) {
                name_len("  accepted: getsockname", getsockname, srv, 28);
                name_len("  accepted: getpeername", getpeername, srv, 28);
                name_len("  client: getpeername", getpeername, c, 16);
                int v = -1; socklen_t vl = sizeof v;
                int r = getsockopt(srv, IPPROTO_IPV6, IPV6_V6ONLY, &v, &vl);
                printf("%-60s -> %s value=%d\n", "  accepted: getsockopt IPV6_V6ONLY", en(result(r)), v);
                close(srv);
            }
        }
        // A dual-mode client to the dual-mode listener.
        int c6 = tcp6_v6only(0);
        do_connect6("  dual-mode client connects ::ffff:127.0.0.1:L", c6, sin6_of(1, INADDR_LOOPBACK, L), 28);
        {
            unsigned char buf[64]; memset(buf, 0xAA, sizeof buf);
            socklen_t al = 16;
            int srv = accept(l, (struct sockaddr *)buf, &al);
            printf("%-60s -> %s len=%d", "  accept, cell 16", en(srv >= 0 ? 0 : errno), (int)al);
            if (srv >= 0) { dump(buf, 16); close(srv); }
            printf("\n");
        }
        close(c6);
        close(c);
        close(l);
    }
}


// E: what a refused connect leaves a dual-mode socket presenting.
static void section_e(void) {
    printf("== E: refusals ==\n");
    unsigned short closed = free_port();
    // Bound explicitly first, blocking.
    int s = tcp6_v6only(0);
    B = 0;
    {
        struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, 0);
        bind(s, (struct sockaddr *)&a, sizeof a);
        unsigned char nb[28]; socklen_t nl = sizeof nb;
        getsockname(s, (struct sockaddr *)nb, &nl);
        B = (unsigned short)((nb[2] << 8) | nb[3]);
    }
    do_connect6("E1 bound ::ffff:127.0.0.1:B, blocking connect to <closed>", s, sin6_of(1, INADDR_LOOPBACK, closed), 28);
    sockname("E1 after: getsockname", s);
    peername("E1 after: getpeername", s);
    close(s);
    B = 0;

    // Non-blocking, unbound: the refusal pending, then taken by SO_ERROR.
    s = tcp6_v6only(0);
    fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
    do_connect6("E2 unbound non-blocking connect to <closed>", s, sin6_of(1, INADDR_LOOPBACK, closed), 28);
    usleep(50000);
    sockname("E2 pending: getsockname", s);
    peername("E2 pending: getpeername", s);
    {
        int err = -1; socklen_t el = sizeof err;
        getsockopt(s, SOL_SOCKET, SO_ERROR, &err, &el);
        printf("%-60s -> %s\n", "E2 SO_ERROR", en(err));
    }
    sockname("E2 taken: getsockname", s);
    close(s);

    // Linux: a refused socket connects again, to a listener.
    int l = listener4(INADDR_LOOPBACK);
    s = tcp6_v6only(0);
    do_connect6("E3 unbound blocking connect to <closed>", s, sin6_of(1, INADDR_LOOPBACK, closed), 28);
    do_connect6("E3 then to L", s, sin6_of(1, INADDR_LOOPBACK, L), 28);
    sockname("E3 after: getsockname", s);
    peername("E3 after: getpeername", s);
    close(s);
    close(l);
}

// One bind of `a` at `len` bytes on a fresh socket that `prep` has set up,
// labelled.
static void bind_row(const char *label, int s, const void *a, int len) {
    errno = 0;
    int e = result(bind(s, (const struct sockaddr *)a, (socklen_t)len));
    printf("%-60s -> %s\n", label, en(e));
}

// F: which of two faults a dual-mode socket's bind reports.
static void section_f(void) {
    printf("== F: bind fault order ==\n");
    struct sockaddr_in6 nonlocal = sin6_of(1, 0x0AFFFF01u, 0);
    struct sockaddr_in6 local = sin6_of(1, INADDR_LOOPBACK, 0);
    struct sockaddr_in6 group = sin6_of(1, 0xE0000001u, 0);
    struct sockaddr_in6 bcast = sin6_of(1, 0xFFFFFFFFu, 0);
    struct sockaddr_in6 native = sin6_of(0, 1, 0);
    struct sockaddr_in v4 = sin_of(INADDR_LOOPBACK, 0);
    unsigned char v4long[28]; memset(v4long, 0, sizeof v4long); memcpy(v4long, &v4, sizeof v4);

    int s = tcp6_v6only(0); bind6(s, 1, INADDR_LOOPBACK, 0);
    bind_row("F1 bound, rebind non-local ::ffff:10.255.255.1", s, &nonlocal, 28); close(s);
    s = tcp6_v6only(0); bind6(s, 1, INADDR_LOOPBACK, 0);
    bind_row("F2 bound, rebind sockaddr_in at 28", s, v4long, 28); close(s);
    s = tcp6_v6only(0); bind6(s, 1, INADDR_LOOPBACK, 0);
    bind_row("F3 bound, rebind at length 16", s, &local, 16); close(s);
    s = tcp6_v6only(0); bind6(s, 1, INADDR_LOOPBACK, 0);
    bind_row("F4 bound, rebind ::ffff:224.0.0.1", s, &group, 28); close(s);
    s = tcp6_v6only(1); bind6(s, 0, 0, 0);
    bind_row("F5 V6ONLY=1 bound [::], rebind ::ffff:127.0.0.1", s, &local, 28); close(s);
    s = tcp6_v6only(1);
    bind_row("F6 V6ONLY=1, ::ffff:10.255.255.1", s, &nonlocal, 28); close(s);
    s = tcp6_v6only(1);
    bind_row("F7 V6ONLY=1, ::ffff:255.255.255.255", s, &bcast, 28); close(s);
    s = tcp6_v6only(1);
    bind_row("F8 V6ONLY=1, ::ffff:127.0.0.1 at length 16", s, &local, 16); close(s);
    s = tcp6_v6only(1);
    bind_row("F9 V6ONLY=1, sockaddr_in at 28", s, v4long, 28); close(s);
    s = tcp6_v6only(0);
    bind_row("F10 V6ONLY=0, ::ffff:255.255.255.255", s, &bcast, 28); close(s);
    s = tcp6_v6only(0);
    bind_row("F11 V6ONLY=0, sockaddr_in at length 16", s, v4long, 16); close(s);
    s = tcp6_v6only(0);
    bind_row("F12 V6ONLY=0, sockaddr_in at length 23", s, v4long, 23); close(s);
    {
        // AF_UNSPEC carrying a v4-mapped address.
        struct sockaddr_in6 u = local; u.sin6_family = AF_UNSPEC;
        s = tcp6_v6only(0);
        bind_row("F13 V6ONLY=0, AF_UNSPEC ::ffff:127.0.0.1 at 28", s, &u, 28);
        sockname("F13 getsockname", s);
        close(s);
    }
    // In use and non-local together, in use and already bound together.
    {
        unsigned short port = free_port();
        int holder = tcp4(); bind4(holder, INADDR_LOOPBACK, port);
        struct sockaddr_in6 taken = sin6_of(1, INADDR_LOOPBACK, port);
        s = tcp6_v6only(0); bind6(s, 1, INADDR_LOOPBACK, 0);
        bind_row("F14 bound, rebind to a port in use", s, &taken, 28); close(s);
        s = tcp6_v6only(1);
        bind_row("F15 V6ONLY=1, a v4-mapped port in use", s, &taken, 28); close(s);
        close(holder);
    }
    s = tcp6_v6only(0);
    bind_row("F16 V6ONLY=0, [::1] (native)", s, &native, 28); close(s);

    // The privileged port, from an unprivileged process.
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        if (getuid() == 0 && setuid(65534) != 0) { perror("setuid"); _exit(1); }
        struct sockaddr_in6 low = sin6_of(1, INADDR_LOOPBACK, 80);
        struct sockaddr_in6 lownonlocal = sin6_of(1, 0x0AFFFF01u, 80);
        int c = tcp6_v6only(0);
        bind_row("F17 unprivileged, ::ffff:127.0.0.1:80", c, &low, 28); close(c);
        c = tcp6_v6only(0);
        bind_row("F18 unprivileged, ::ffff:10.255.255.1:80", c, &lownonlocal, 28); close(c);
        c = tcp6_v6only(0); bind6(c, 1, INADDR_LOOPBACK, 0);
        bind_row("F19 unprivileged, bound, rebind ::ffff:127.0.0.1:80", c, &low, 28); close(c);
        c = tcp6_v6only(1);
        bind_row("F20 unprivileged, V6ONLY=1, ::ffff:127.0.0.1:80", c, &low, 28); close(c);
        fflush(stdout);
        _exit(0);
    }
    waitpid(pid, NULL, 0);
}

// G: connect's ladder on a dual-mode socket, and on a V6ONLY one.
static void section_g(void) {
    printf("== G: connect ladder ==\n");
    int l = listener4(INADDR_LOOPBACK);
    struct sockaddr_in6 to = sin6_of(1, INADDR_LOOPBACK, L);
    struct sockaddr_in v4 = sin_of(INADDR_LOOPBACK, L);
    unsigned char v4long[28]; memset(v4long, 0, sizeof v4long); memcpy(v4long, &v4, sizeof v4);
    struct sockaddr_in6 u = to; u.sin6_family = AF_UNSPEC;
    struct sockaddr_in6 group = sin6_of(1, 0xE0000001u, L);

    int s = tcp6_v6only(1);
    do_connect6("G1 V6ONLY=1, mapped at length 16", s, to, 16); close(s);
    s = tcp6_v6only(1);
    errno = 0;
    printf("%-60s -> %s\n", "G2 V6ONLY=1, sockaddr_in at 28", en(result(timed_connect(s, (struct sockaddr *)v4long, 28)))); close(s);
    s = tcp6_v6only(1);
    do_connect6("G3 V6ONLY=1, AF_UNSPEC mapped at 28", s, u, 28);
    sockname("G3 after: getsockname", s);
    close(s);
    s = tcp6_v6only(1);
    do_connect6("G4 V6ONLY=1, ::ffff:224.0.0.1", s, group, 28); close(s);
    s = tcp6_v6only(1);
    do_connect6("G5 V6ONLY=1, mapped port 0", s, sin6_of(1, INADDR_LOOPBACK, 0), 28);
    sockname("G5 after: getsockname", s);
    close(s);
    // A dual-mode socket explicitly bound, then connecting.
    s = tcp6_v6only(0);
    {
        struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, 0);
        bind(s, (struct sockaddr *)&a, sizeof a);
        unsigned char nb[28]; socklen_t nl = sizeof nb;
        getsockname(s, (struct sockaddr *)nb, &nl);
        B = (unsigned short)((nb[2] << 8) | nb[3]);
    }
    do_connect6("G6 bound ::ffff:127.0.0.1:B, connect to L", s, to, 28);
    sockname("G6 after: getsockname", s);
    peername("G6 after: getpeername", s);
    {
        int srv = accept(l, NULL, NULL);
        name_len("G6 accepted: getpeername", getpeername, srv, 16);
        close(srv);
    }
    close(s);
    B = 0;
    // On an established dual-mode socket.
    s = tcp6_v6only(0);
    do_connect6("G7 connect to L", s, to, 28);
    do_connect6("G7 established, connect at length 16", s, to, 16);
    do_connect6("G7 established, connect AF_UNSPEC", s, u, 28);
    {
        int srv = accept(l, NULL, NULL);
        close(srv);
    }
    close(s);
    // A fresh connection: Linux's AF_UNSPEC above disconnected that one.
    s = tcp6_v6only(0);
    connect(s, (struct sockaddr *)&to, sizeof to);
    errno = 0;
    printf("%-60s -> %s\n", "G7 established, connect sockaddr_in at 28", en(result(timed_connect(s, (struct sockaddr *)v4long, 28))));
    {
        int srv = accept(l, NULL, NULL);
        close(srv);
    }
    close(s);
    // Buffers and segment size, a dual-mode client against a v4 client.
    for (int dual = 0; dual < 2; dual++) {
        s = dual ? tcp6_v6only(0) : tcp4();
        int e = dual ? result(connect(s, (struct sockaddr *)&to, sizeof to)) : result(connect(s, (struct sockaddr *)&v4, sizeof v4));
        int snd = -1, rcv = -1, mss = -1; socklen_t n = sizeof snd;
        getsockopt(s, SOL_SOCKET, SO_SNDBUF, &snd, &n); n = sizeof rcv;
        getsockopt(s, SOL_SOCKET, SO_RCVBUF, &rcv, &n); n = sizeof mss;
        getsockopt(s, IPPROTO_TCP, TCP_MAXSEG, &mss, &n);
        printf("G8 %-57s -> %s SO_SNDBUF=%d SO_RCVBUF=%d TCP_MAXSEG=%d\n", dual ? "dual-mode client" : "AF_INET client", en(e), snd, rcv, mss);
        int srv = accept(l, NULL, NULL);
        close(srv);
        close(s);
    }
    close(l);
}


// H: connect's family and length screens against a dual-mode socket's phase.
static void connect_raw(const char *label, int s, int family, int len, unsigned short port) {
    unsigned char buf[64];
    memset(buf, 0, sizeof buf);
    if (family == AF_INET) {
        struct sockaddr_in a = sin_of(INADDR_LOOPBACK, port);
        memcpy(buf, &a, sizeof a);
    } else {
        struct sockaddr_in6 a = sin6_of(1, INADDR_LOOPBACK, port);
        a.sin6_family = (sa_family_t)family;
        memcpy(buf, &a, sizeof a);
    }
#ifdef __APPLE__
    buf[1] = (unsigned char)family;
#endif
    errno = 0;
    int e = result(timed_connect(s, (struct sockaddr *)buf, (socklen_t)len));
    printf("%-60s -> %s\n", label, en(e));
}

static void section_h(void) {
    printf("== H: connect screens by phase ==\n");
    int l = listener4(INADDR_LOOPBACK);
    // Non-blocking, so that an accept after a failed connect answers.
    fcntl(l, F_SETFL, fcntl(l, F_GETFL) | O_NONBLOCK);
    unsigned short closed = free_port();
    struct { const char *name; int family; int len; } rows[] = {
        {"AF_INET at 16", AF_INET, 16},
        {"AF_INET at 28", AF_INET, 28},
        {"AF_INET6 at 16", AF_INET6, 16},
        {"AF_INET6 at 28", AF_INET6, 28},
        {"family 99 at 28", 99, 28},
        {"family 99 at 16", 99, 16},
    };
    int n = sizeof rows / sizeof rows[0];
    char label[100];
    for (int i = 0; i < n; i++) {
        // Established.
        int s = tcp6_v6only(0);
        struct sockaddr_in6 to = sin6_of(1, INADDR_LOOPBACK, L);
        connect(s, (struct sockaddr *)&to, sizeof to);
        snprintf(label, sizeof label, "H1 established, %s", rows[i].name);
        connect_raw(label, s, rows[i].family, rows[i].len, L);
        int srv = accept(l, NULL, NULL); if (srv >= 0) close(srv); close(s);
        // Refused, the error pending (non-blocking), and taken.
        s = tcp6_v6only(0);
        fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
        struct sockaddr_in6 shut = sin6_of(1, INADDR_LOOPBACK, closed);
        connect(s, (struct sockaddr *)&shut, sizeof shut);
        usleep(50000);
        snprintf(label, sizeof label, "H2 refused pending, %s", rows[i].name);
        connect_raw(label, s, rows[i].family, rows[i].len, closed);
        close(s);
        // Pending report: a non-blocking connect that completed.
        s = tcp6_v6only(0);
        fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
        connect(s, (struct sockaddr *)&to, sizeof to);
        usleep(50000);
        snprintf(label, sizeof label, "H3 completed, unreported, %s", rows[i].name);
        connect_raw(label, s, rows[i].family, rows[i].len, L);
        srv = accept(l, NULL, NULL); if (srv >= 0) close(srv); close(s);
        // Idle.
        s = tcp6_v6only(0);
        snprintf(label, sizeof label, "H4 idle, %s", rows[i].name);
        connect_raw(label, s, rows[i].family, rows[i].len, L);
        srv = accept(l, NULL, NULL); if (srv >= 0) close(srv); close(s);
    }
    close(l);
}

int main(int argc, char **argv) {
    setvbuf(stdout, NULL, _IOLBF, 0);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_alarm;
    sigaction(SIGALRM, &sa, NULL);
#ifdef __APPLE__
    printf("sizeof(struct sockaddr_in6)=%zu AF_INET6=%d\n", sizeof(struct sockaddr_in6), AF_INET6);
#else
    printf("sizeof(struct sockaddr_in6)=%zu AF_INET6=%d\n", sizeof(struct sockaddr_in6), AF_INET6);
#endif
    // `./p EFG` runs those sections only; no argument runs every one.
    const char *only = argc > 1 ? argv[1] : "ANBCDEFGH";
    if (strchr(only, 'A')) section_a();
    if (strchr(only, 'N')) section_n();
    if (strchr(only, 'B')) section_b();
    if (strchr(only, 'C')) section_c();
    if (strchr(only, 'D')) section_d();
    if (strchr(only, 'E')) section_e();
    if (strchr(only, 'F')) section_f();
    if (strchr(only, 'G')) section_g();
    if (strchr(only, 'H')) section_h();
    return 0;
}
