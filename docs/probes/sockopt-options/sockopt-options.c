// setsockopt(2) and getsockopt(2) of TCP_NODELAY, IPV6_V6ONLY and SO_LINGER
// (and Darwin's SO_LINGER_SEC): the numbering, the defaults, what each value
// reads back, the errno for the wrong domain or kind, short and odd lengths,
// faulting buffers, and setting IPV6_V6ONLY after bind, listen and connect.
//
// Both flavours; outputs beside this file.
//
//     cc -Wall -o p sockopt-options.c && ./p
//     container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/p /probe/sockopt-options.c && uname -r && /tmp/p'
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case ENOTSOCK: return "ENOTSOCK";
    case EFAULT: return "EFAULT";
    case EINVAL: return "EINVAL";
    case ENOPROTOOPT: return "ENOPROTOOPT";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ECONNRESET: return "ECONNRESET";
    case EISCONN: return "EISCONN";
    case EADDRINUSE: return "EADDRINUSE";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

static void *unmapped;

struct opt { const char *name; int level; int optname; int size; };

static struct opt NODELAY = { "TCP_NODELAY", IPPROTO_TCP, TCP_NODELAY, 4 };
static struct opt V6ONLY = { "IPV6_V6ONLY", IPPROTO_IPV6, IPV6_V6ONLY, 4 };
static struct opt LINGER = { "SO_LINGER", SOL_SOCKET, SO_LINGER, 8 };
#ifdef SO_LINGER_SEC
static struct opt LINGERSEC = { "SO_LINGER_SEC", SOL_SOCKET, SO_LINGER_SEC, 8 };
#endif

static int sso(int fd, struct opt o, const void *val, socklen_t len) {
    errno = 0;
    int r = setsockopt(fd, o.level, o.optname, val, len);
    return r == 0 ? 0 : errno;
}

// getsockopt with the length cell preset to `len`, through a 16-byte buffer of
// 0x5a; prints errno, the bytes that changed, and the length cell.
static void gso(const char *label, int fd, struct opt o, socklen_t len) {
    unsigned char buf[16];
    memset(buf, 0x5a, sizeof buf);
    socklen_t l = len;
    errno = 0;
    int r = getsockopt(fd, o.level, o.optname, buf, &l);
    int e = r == 0 ? 0 : errno;
    printf("  %-44s %-14s len %-10u -> %-12s", label, o.name, len, en(e));
    printf(" cell=%u bytes=", l);
    for (int i = 0; i < 12; i++) printf("%02x", buf[i]);
    printf("\n");
}

static void sset(const char *label, int fd, struct opt o, const void *val, socklen_t len) {
    printf("  %-44s %-14s set len %-6u -> %s\n", label, o.name, len, en(sso(fd, o, val, len)));
}

static int seti(int fd, struct opt o, int v) { return sso(fd, o, &v, sizeof v); }

static void getv(const char *label, int fd, struct opt o) { gso(label, fd, o, o.size); }

static struct sockaddr_in lo4(uint16_t port) {
    struct sockaddr_in a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET; a.sin_port = htons(port); a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    return a;
}
static struct sockaddr_in6 lo6(uint16_t port) {
    struct sockaddr_in6 a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin6_len = sizeof a;
#endif
    a.sin6_family = AF_INET6; a.sin6_port = htons(port); a.sin6_addr = in6addr_loopback;
    return a;
}
static uint16_t port4(int fd) { struct sockaddr_in a; socklen_t l = sizeof a; getsockname(fd, (struct sockaddr *)&a, &l); return ntohs(a.sin_port); }
static uint16_t port6(int fd) { struct sockaddr_in6 a; socklen_t l = sizeof a; getsockname(fd, (struct sockaddr *)&a, &l); return ntohs(a.sin6_port); }

struct kind { const char *name; int domain; int type; };

int main(void) {
    unmapped = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    printf("numbers: IPPROTO_TCP=%d TCP_NODELAY=%d IPPROTO_IPV6=%d IPV6_V6ONLY=%d SOL_SOCKET=%d SO_LINGER=%d",
           IPPROTO_TCP, TCP_NODELAY, IPPROTO_IPV6, IPV6_V6ONLY, SOL_SOCKET, SO_LINGER);
#ifdef SO_LINGER_SEC
    printf(" SO_LINGER_SEC=%d", SO_LINGER_SEC);
#endif
    printf(" sizeof(struct linger)=%zu sysconf(_SC_CLK_TCK)=%ld\n", sizeof(struct linger), sysconf(_SC_CLK_TCK));

    struct kind kinds[] = {
        { "inet stream", AF_INET, SOCK_STREAM }, { "inet dgram", AF_INET, SOCK_DGRAM },
        { "inet6 stream", AF_INET6, SOCK_STREAM }, { "inet6 dgram", AF_INET6, SOCK_DGRAM },
        { "unix stream", AF_UNIX, SOCK_STREAM }, { "unix dgram", AF_UNIX, SOCK_DGRAM },
    };
    struct opt opts[] = { NODELAY, V6ONLY, LINGER
#ifdef SO_LINGER_SEC
        , LINGERSEC
#endif
    };
    int nopts = sizeof opts / sizeof opts[0];

    printf("D. defaults, wrong domain or kind (get at the option's size, set 1 / {1,5})\n");
    for (int k = 0; k < 6; k++) {
        for (int i = 0; i < nopts; i++) {
            int fd = socket(kinds[k].domain, kinds[k].type, 0);
            getv(kinds[k].name, fd, opts[i]);
            if (opts[i].size == 4) { printf("  %-44s %-14s set 1 -> %s\n", kinds[k].name, opts[i].name, en(seti(fd, opts[i], 1))); }
            else { struct linger lg = { 1, 5 }; printf("  %-44s %-14s set {1,5} -> %s\n", kinds[k].name, opts[i].name, en(sso(fd, opts[i], &lg, sizeof lg))); }
            getv(kinds[k].name, fd, opts[i]);
            close(fd);
        }
    }

    printf("V. int values set then read back\n");
    int values[] = { 0, 1, 2, 4, -1, 0x100, 0x10000, (int)0x80000000 };
    struct opt intopts[] = { NODELAY, V6ONLY };
    int intdomains[] = { AF_INET, AF_INET6 };
    for (int i = 0; i < 2; i++) {
        for (int v = 0; v < 8; v++) {
            int fd = socket(intdomains[i], SOCK_STREAM, 0);
            if (intopts[i].optname == IPV6_V6ONLY) fd = (close(fd), socket(AF_INET6, SOCK_STREAM, 0));
            char label[64]; snprintf(label, sizeof label, "set %d (0x%x)", values[v], values[v]);
            printf("  %-44s %-14s -> %s\n", label, intopts[i].name, en(seti(fd, intopts[i], values[v])));
            getv(label, fd, intopts[i]);
            // and back to 0
            seti(fd, intopts[i], 0);
            getv("then set 0", fd, intopts[i]);
            close(fd);
        }
    }

    printf("L. linger values set then read back, through each name\n");
    struct linger lvals[] = { {0,0}, {1,0}, {1,1}, {1,5}, {2,7}, {-1,3}, {0,5}, {1,-1}, {1,327}, {1,328}, {1,32767}, {1,32768}, {1,65535}, {1,65536}, {1,0x7fffffff}, {0,-1} };
    int nl = sizeof lvals / sizeof lvals[0];
    for (int i = 0; i < nopts; i++) {
        if (opts[i].size != 8) continue;
        for (int v = 0; v < nl; v++) {
            int fd = socket(AF_INET, SOCK_STREAM, 0);
            char label[64]; snprintf(label, sizeof label, "set {%d,%d}", lvals[v].l_onoff, lvals[v].l_linger);
            printf("  %-44s %-14s -> %s\n", label, opts[i].name, en(sso(fd, opts[i], &lvals[v], sizeof lvals[v])));
            for (int j = 0; j < nopts; j++) if (opts[j].size == 8) getv(label, fd, opts[j]);
            close(fd);
        }
    }
    {
        // A linger time survives a set with l_onoff 0?
        int fd = socket(AF_INET, SOCK_STREAM, 0);
        struct linger a = { 1, 9 }, b = { 0, 4 };
        sso(fd, LINGER, &a, sizeof a); sso(fd, LINGER, &b, sizeof b);
        getv("set {1,9} then {0,4}", fd, LINGER);
        close(fd);
    }

    printf("S. set lengths (value buffer of 16 bytes: 1, then zeros)\n");
    {
        unsigned char big[16]; memset(big, 0, sizeof big); big[0] = 1; big[4] = 3;
        unsigned int lens[] = { 0, 1, 2, 3, 4, 5, 7, 8, 9, 16, 0x7fffffff, 0x80000000u, 0xffffffffu };
        for (int i = 0; i < nopts; i++) {
            for (unsigned l = 0; l < sizeof lens / sizeof lens[0]; l++) {
                int fd = socket(opts[i].optname == IPV6_V6ONLY && opts[i].level == IPPROTO_IPV6 ? AF_INET6 : AF_INET, SOCK_STREAM, 0);
                char label[64]; snprintf(label, sizeof label, "len %u", lens[l]);
                int e = sso(fd, opts[i], big, lens[l]);
                printf("  %-44s %-14s set -> %s\n", label, opts[i].name, en(e));
                getv("  read back", fd, opts[i]);
                close(fd);
            }
        }
    }

    printf("N. null and faulting value buffers on set\n");
    for (int i = 0; i < nopts; i++) {
        int fd = socket(opts[i].level == IPPROTO_IPV6 ? AF_INET6 : AF_INET, SOCK_STREAM, 0);
        printf("  %-44s %-14s -> %s\n", "null, len 0", opts[i].name, en(sso(fd, opts[i], NULL, 0)));
        printf("  %-44s %-14s -> %s\n", "null, len size", opts[i].name, en(sso(fd, opts[i], NULL, opts[i].size)));
        printf("  %-44s %-14s -> %s\n", "null, len 1", opts[i].name, en(sso(fd, opts[i], NULL, 1)));
        printf("  %-44s %-14s -> %s\n", "unmapped, len size", opts[i].name, en(sso(fd, opts[i], unmapped, opts[i].size)));
        printf("  %-44s %-14s -> %s\n", "unmapped, len 1", opts[i].name, en(sso(fd, opts[i], unmapped, 1)));
        printf("  %-44s %-14s -> %s\n", "unmapped, len 0", opts[i].name, en(sso(fd, opts[i], unmapped, 0)));
        printf("  %-44s %-14s -> %s\n", "closed fd, null, len size", opts[i].name, en(sso(1000000, opts[i], NULL, opts[i].size)));
        printf("  %-44s %-14s -> %s\n", "closed fd, len size", opts[i].name, en(sso(1000000, opts[i], (int[2]){1, 1}, opts[i].size)));
        int p[2]; pipe(p);
        printf("  %-44s %-14s -> %s\n", "pipe, len size", opts[i].name, en(sso(p[0], opts[i], (int[2]){1, 1}, opts[i].size)));
        printf("  %-44s %-14s -> %s\n", "pipe, len 1", opts[i].name, en(sso(p[0], opts[i], (int[2]){1, 1}, 1)));
        // wrong kind with a bad length / buffer: which comes first?
        int u = socket(AF_UNIX, SOCK_STREAM, 0);
        printf("  %-44s %-14s -> %s\n", "unix stream, len 1", opts[i].name, en(sso(u, opts[i], (int[2]){1, 1}, 1)));
        printf("  %-44s %-14s -> %s\n", "unix stream, unmapped", opts[i].name, en(sso(u, opts[i], unmapped, opts[i].size)));
        int d = socket(AF_INET, SOCK_DGRAM, 0);
        printf("  %-44s %-14s -> %s\n", "inet dgram, len 1", opts[i].name, en(sso(d, opts[i], (int[2]){1, 1}, 1)));
        printf("  %-44s %-14s -> %s\n", "inet dgram, unmapped", opts[i].name, en(sso(d, opts[i], unmapped, opts[i].size)));
        int f = socket(AF_INET, SOCK_STREAM, 0);
        printf("  %-44s %-14s -> %s\n", "inet stream, len 1", opts[i].name, en(sso(f, opts[i], (int[2]){1, 1}, 1)));
        printf("  %-44s %-14s -> %s\n", "inet stream, unmapped", opts[i].name, en(sso(f, opts[i], unmapped, opts[i].size)));
        close(u); close(d); close(f); close(p[0]); close(p[1]); close(fd);
    }

    printf("G. get lengths (option set first: 1 / {1,5})\n");
    {
        unsigned int lens[] = { 0, 1, 2, 3, 4, 5, 7, 8, 9, 16, 0x7fffffff, 0x80000000u, 0xffffffffu };
        for (int i = 0; i < nopts; i++) {
            int fd = socket(opts[i].level == IPPROTO_IPV6 ? AF_INET6 : AF_INET, SOCK_STREAM, 0);
            if (opts[i].size == 4) seti(fd, opts[i], 1); else { struct linger lg = { 1, 5 }; sso(fd, opts[i], &lg, sizeof lg); }
            for (unsigned l = 0; l < sizeof lens / sizeof lens[0]; l++) gso("", fd, opts[i], lens[l]);
            // faulting and null value buffers, at several lengths: errno and the cell
            unsigned int flens[] = { 0, 1, 4, 8, 16 };
            for (unsigned l = 0; l < 5; l++) {
                socklen_t c = flens[l]; errno = 0; int r = getsockopt(fd, opts[i].level, opts[i].optname, unmapped, &c);
                printf("  %-44s %-14s len %-10u -> %-12s cell=%u\n", "unmapped value", opts[i].name, flens[l], r == 0 ? "OK" : en(errno), c);
                c = flens[l]; errno = 0; r = getsockopt(fd, opts[i].level, opts[i].optname, NULL, &c);
                printf("  %-44s %-14s len %-10u -> %-12s cell=%u\n", "null value", opts[i].name, flens[l], r == 0 ? "OK" : en(errno), c);
            }
            unsigned char b[16];
            errno = 0; int r = getsockopt(fd, opts[i].level, opts[i].optname, b, NULL);
            printf("  %-44s %-14s -> %s\n", "null cell", opts[i].name, r == 0 ? "OK" : en(errno));
            errno = 0; r = getsockopt(fd, opts[i].level, opts[i].optname, b, unmapped);
            printf("  %-44s %-14s -> %s\n", "unmapped cell", opts[i].name, r == 0 ? "OK" : en(errno));
            errno = 0; r = getsockopt(fd, opts[i].level, opts[i].optname, NULL, NULL);
            printf("  %-44s %-14s -> %s\n", "null value, null cell", opts[i].name, r == 0 ? "OK" : en(errno));
            close(fd);
            // closed, pipe, wrong kind, with a null cell
            errno = 0; r = getsockopt(1000000, opts[i].level, opts[i].optname, b, NULL);
            printf("  %-44s %-14s -> %s\n", "closed fd, null cell", opts[i].name, r == 0 ? "OK" : en(errno));
            int u = socket(AF_UNIX, SOCK_STREAM, 0);
            errno = 0; r = getsockopt(u, opts[i].level, opts[i].optname, b, NULL);
            printf("  %-44s %-14s -> %s\n", "unix stream, null cell", opts[i].name, r == 0 ? "OK" : en(errno));
            gso("unix stream, len 1", u, opts[i], 1);
            close(u);
            int d = socket(AF_INET, SOCK_DGRAM, 0);
            errno = 0; r = getsockopt(d, opts[i].level, opts[i].optname, b, NULL);
            printf("  %-44s %-14s -> %s\n", "inet dgram, null cell", opts[i].name, r == 0 ? "OK" : en(errno));
            gso("inet dgram, len 1", d, opts[i], 1);
            close(d);
        }
    }

    printf("P. phases: listener, connected client, accepted, refused (set 1 / {1,5} then read)\n");
    {
        int l = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in a = lo4(0);
        bind(l, (struct sockaddr *)&a, sizeof a); listen(l, 4);
        for (int i = 0; i < nopts; i++) {
            if (opts[i].level == IPPROTO_IPV6) continue;
            int v = 1; struct linger lg = { 1, 5 };
            const void *val = opts[i].size == 4 ? (const void *)&v : (const void *)&lg;
            sset("listener", l, opts[i], val, opts[i].size); getv("listener", l, opts[i]);
        }
        // options set on the listener: does an accepted socket inherit them?
        int c = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in d = lo4(port4(l));
        connect(c, (struct sockaddr *)&d, sizeof d);
        int s = accept(l, NULL, NULL);
        for (int i = 0; i < nopts; i++) if (opts[i].level != IPPROTO_IPV6) getv("accepted, listener had it set", s, opts[i]);
        for (int i = 0; i < nopts; i++) if (opts[i].level != IPPROTO_IPV6) getv("client", c, opts[i]);
        for (int i = 0; i < nopts; i++) {
            if (opts[i].level == IPPROTO_IPV6) continue;
            int v = 1; struct linger lg = { 1, 5 };
            const void *val = opts[i].size == 4 ? (const void *)&v : (const void *)&lg;
            sset("client", c, opts[i], val, opts[i].size); getv("client", c, opts[i]);
        }
        close(s); close(c); close(l);
        // refused
        int t = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in z = lo4(0); bind(t, (struct sockaddr *)&z, sizeof z);
        uint16_t dead = port4(t); close(t);
        int r = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in dd = lo4(dead);
        connect(r, (struct sockaddr *)&dd, sizeof dd);
        for (int i = 0; i < nopts; i++) {
            if (opts[i].level == IPPROTO_IPV6) continue;
            getv("refused (blocking)", r, opts[i]);
            int v = 1; struct linger lg = { 1, 5 };
            const void *val = opts[i].size == 4 ? (const void *)&v : (const void *)&lg;
            sset("refused (blocking)", r, opts[i], val, opts[i].size);
        }
        close(r);
    }

    printf("6. IPV6_V6ONLY across an AF_INET6 socket's life\n");
    {
        int s = socket(AF_INET6, SOCK_STREAM, 0); struct sockaddr_in6 a = lo6(0);
        printf("  bind ::1 -> %s\n", bind(s, (struct sockaddr *)&a, sizeof a) == 0 ? "OK" : en(errno));
        printf("  bound: set 1 -> %s\n", en(seti(s, V6ONLY, 1))); getv("bound", s, V6ONLY);
        printf("  bound: set 0 -> %s\n", en(seti(s, V6ONLY, 0))); getv("bound", s, V6ONLY);
        printf("  listen -> %s\n", listen(s, 4) == 0 ? "OK" : en(errno));
        printf("  listening: set 1 -> %s\n", en(seti(s, V6ONLY, 1))); getv("listening", s, V6ONLY);
        int c = socket(AF_INET6, SOCK_STREAM, 0); struct sockaddr_in6 d = lo6(port6(s));
        printf("  client: set 1 -> %s\n", en(seti(c, V6ONLY, 1)));
        printf("  connect [::1] -> %s\n", connect(c, (struct sockaddr *)&d, sizeof d) == 0 ? "OK" : en(errno));
        printf("  connected: set 0 -> %s\n", en(seti(c, V6ONLY, 0))); getv("connected", c, V6ONLY);
        int acc = accept(s, NULL, NULL);
        getv("accepted (listener 0 after the failed set?)", acc, V6ONLY);
        close(acc); close(c); close(s);
        // bound to the wildcard, set 1 then 0
        int w = socket(AF_INET6, SOCK_STREAM, 0); struct sockaddr_in6 wa; memset(&wa, 0, sizeof wa);
#ifdef __APPLE__
        wa.sin6_len = sizeof wa;
#endif
        wa.sin6_family = AF_INET6;
        printf("  bind [::]:0 -> %s\n", bind(w, (struct sockaddr *)&wa, sizeof wa) == 0 ? "OK" : en(errno));
        printf("  wildcard-bound: set 1 -> %s\n", en(seti(w, V6ONLY, 1))); getv("wildcard-bound", w, V6ONLY);
        close(w);
        // datagram, connected
        int u = socket(AF_INET6, SOCK_DGRAM, 0); struct sockaddr_in6 ua = lo6(9);
        printf("  dgram connect [::1]:9 -> %s\n", connect(u, (struct sockaddr *)&ua, sizeof ua) == 0 ? "OK" : en(errno));
        printf("  dgram connected: set 1 -> %s\n", en(seti(u, V6ONLY, 1))); getv("dgram connected", u, V6ONLY);
        close(u);
        // a v4-mapped connect on a dual-mode socket, then the option
        int l4 = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in la = lo4(0); bind(l4, (struct sockaddr *)&la, sizeof la); listen(l4, 4);
        int m = socket(AF_INET6, SOCK_STREAM, 0); seti(m, V6ONLY, 0);
        struct sockaddr_in6 ma = lo6(port4(l4)); memset(&ma.sin6_addr, 0, 16); ma.sin6_addr.s6_addr[10] = 0xff; ma.sin6_addr.s6_addr[11] = 0xff;
        ma.sin6_addr.s6_addr[12] = 127; ma.sin6_addr.s6_addr[15] = 1;
        printf("  dual-mode connect ::ffff:127.0.0.1 -> %s\n", connect(m, (struct sockaddr *)&ma, sizeof ma) == 0 ? "OK" : en(errno));
        printf("  dual-mode connected: set 1 -> %s\n", en(seti(m, V6ONLY, 1))); getv("dual-mode connected", m, V6ONLY);
        close(m); close(l4);
        // v6only=1 then a v4-mapped connect
        int n = socket(AF_INET6, SOCK_STREAM, 0); seti(n, V6ONLY, 1);
        int l5 = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in lb = lo4(0); bind(l5, (struct sockaddr *)&lb, sizeof lb); listen(l5, 4);
        struct sockaddr_in6 nb = ma; nb.sin6_port = htons(port4(l5));
        printf("  v6only connect ::ffff:127.0.0.1 -> %s\n", connect(n, (struct sockaddr *)&nb, sizeof nb) == 0 ? "OK" : en(errno));
        close(n); close(l5);
    }
    return 0;
}
