// Measures setsockopt(2)/getsockopt(2) of SO_REUSEADDR: the numbering, the
// failure ladder, what a value means and reads back, the phases a socket can
// be in, and what clearing the option on a bound socket does to later binds.
//
// Darwin: cc -Wall -o sockopt sockopt.c && ./sockopt
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq && apt-get install -y -qq gcc libc6-dev && gcc -Wall -o /tmp/p /probe/sockopt.c && /tmp/p'
//
// Measured on Darwin 25.6.0 (arm64) and Linux 6.18.5 (arm64); the rows are
// transcribed in WoofWare.PosixKernel.Test/TestSockOpt.fs.
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <poll.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case ENOTSOCK: return "ENOTSOCK";
    case EFAULT: return "EFAULT";
    case EINVAL: return "EINVAL";
    case ENOPROTOOPT: return "ENOPROTOOPT";
    case EADDRINUSE: return "EADDRINUSE";
    case ECONNREFUSED: return "ECONNREFUSED";
    case EINPROGRESS: return "EINPROGRESS";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ETIMEDOUT: return "ETIMEDOUT";
    case ECONNABORTED: return "ECONNABORTED";
    case EISCONN: return "EISCONN";
    default: return "OTHER";
    }
}

static void *unmapped;

static int sso(int fd, int level, int name, const void *val, socklen_t len) {
    errno = 0;
    int r = setsockopt(fd, level, name, val, len);
    return r == 0 ? 0 : errno;
}

// getsockopt; reports errno, the value cell and the length cell afterwards.
static void gso(const char *label, int fd, int level, int name, void *val, socklen_t *len) {
    errno = 0;
    int r = getsockopt(fd, level, name, val, len);
    int e = r == 0 ? 0 : errno;
    printf("%-52s %-10s", label, en(e));
    if (val != NULL && val != unmapped)
        printf(" val=0x%08x", *(uint32_t *)val);
    if (len != NULL && (void *)len != unmapped)
        printf(" len=%u", *len);
    printf("\n");
}

static void row(const char *label, int e) { printf("%-52s %s (%d)\n", label, en(e), e); }

static int tcp(void) { return socket(AF_INET, SOCK_STREAM, 0); }

static struct sockaddr_in lo(uint16_t port) {
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    return a;
}

static uint16_t portOf(int fd) {
    struct sockaddr_in a;
    socklen_t l = sizeof a;
    getsockname(fd, (struct sockaddr *)&a, &l);
    return ntohs(a.sin_port);
}

static int bindTo(int fd, uint16_t port) {
    struct sockaddr_in a = lo(port);
    errno = 0;
    return bind(fd, (struct sockaddr *)&a, sizeof a) == 0 ? 0 : errno;
}

static int readback(int fd) {
    int v = 0x5a5a5a5a;
    socklen_t l = sizeof v;
    if (getsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &v, &l) != 0)
        return -1000 - errno;
    return v;
}

static int sso2(int fd, const void *val, socklen_t len) {
    errno = 0;
    return setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, val, len) == 0 ? 0 : errno;
}
static void gso2(const char *label, int fd, void *val, socklen_t *len, int printval) {
    errno = 0;
    int r = getsockopt(fd, SOL_SOCKET, SO_REUSEADDR, val, len);
    printf("%-44s errno=%d", label, r == 0 ? 0 : errno);
    if (printval) printf(" val=0x%08x", *(uint32_t *)val);
    if (len) printf(" len=%u", *len);
    printf("\n");
}
static void refusedAndEdgeRows(void) {
    printf("\n== after a refused connect, and remaining edges ==\n");
    int one = 1;
    void *unmapped = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    // a port nothing holds
    int t = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a; memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(t, (struct sockaddr *)&a, sizeof a);
    socklen_t al = sizeof a; getsockname(t, (struct sockaddr *)&a, &al); close(t);
    int r = socket(AF_INET, SOCK_STREAM, 0);
    printf("connect: %d\n", connect(r, (struct sockaddr *)&a, sizeof a) == 0 ? 0 : errno);
    printf("dead unmapped len4: %d\n", sso2(r, unmapped, 4));
    printf("dead NULL len0: %d\n", sso2(r, NULL, 0));
    printf("dead ok len4: %d\n", sso2(r, &one, 4));
    printf("dead len -1: %d\n", sso2(r, &one, (socklen_t)-1));
    close(r);
    int s = socket(AF_INET, SOCK_STREAM, 0);
    socklen_t len; int v;
    len = 2; gso2("NULL val len2", s, NULL, &len, 0);
    len = 4; gso2("unmapped val len4 (flag clear)", s, unmapped, &len, 0);
    v = 0x5a5a5a5a; len = 4; gso2("clear, len4", s, &v, &len, 1);
    printf("set NULL len -1: %d\n", sso2(s, NULL, (socklen_t)-1));
    printf("set unmapped len 0x7fffffff: %d\n", sso2(s, unmapped, 0x7fffffff));
    printf("set ok len 0x7fffffff: %d\n", sso2(s, &one, 0x7fffffff));
#ifdef __linux__
    int kinds[] = {SOCK_RAW, SOCK_SEQPACKET};
    for (int i = 0; i < 2; i++) {
        int u = socket(AF_UNIX, kinds[i], 0);
        v = 0x5a5a5a5a; len = 4;
        getsockopt(u, SOL_SOCKET, SO_REUSEADDR, &v, &len);
        int e = sso2(u, &one, 4);
        int w = 0x5a5a5a5a; socklen_t wl = 4;
        getsockopt(u, SOL_SOCKET, SO_REUSEADDR, &w, &wl);
        printf("AF_UNIX kind %d fd=%d fresh=%d set=%d readback=%d len=%u\n", kinds[i], u, v, e, w, wl);
    }
#endif
}

// Set the flag to `before` through real storage, then attempt the row's set,
// then read back -- as three separate statements, so the order is fixed.
static void setThenRead(const char *label, int fd, int before, const void *val, socklen_t len) {
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &before, sizeof before);
    int e = sso(fd, SOL_SOCKET, SO_REUSEADDR, val, len);
    int r = readback(fd);
    printf("%s (from %d) -> %s readback=%d\n", label, before, en(e), r);
}

int main(void) {
    int one = 1;
    char big[16];
    memset(big, 0, sizeof big);
    *(int *)big = 1;

    // PROT_NONE and kept reserved, so nothing (a stdio buffer, say) can be
    // mapped there later.
    unmapped = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);

    printf("SOL_SOCKET=%d (0x%x) SO_REUSEADDR=%d SO_ERROR=%d SO_REUSEPORT=%d sizeof(socklen_t)=%zu\n", SOL_SOCKET,
           SOL_SOCKET, SO_REUSEADDR, SO_ERROR, SO_REUSEPORT, sizeof(socklen_t));

    int bad = 999;
    int file = open("/dev/null", O_RDONLY);
    int pipes[2];
    pipe(pipes);

    printf("\n== setsockopt ladder ==\n");
    row("S badfd", sso(bad, SOL_SOCKET, SO_REUSEADDR, &one, 4));
    row("S badfd NULL val len4", sso(bad, SOL_SOCKET, SO_REUSEADDR, NULL, 4));
    row("S badfd NULL val len0", sso(bad, SOL_SOCKET, SO_REUSEADDR, NULL, 0));
    row("S badfd unmapped val len4", sso(bad, SOL_SOCKET, SO_REUSEADDR, unmapped, 4));
    row("S badfd len0", sso(bad, SOL_SOCKET, SO_REUSEADDR, &one, 0));
    row("S badfd len -1", sso(bad, SOL_SOCKET, SO_REUSEADDR, &one, (socklen_t)-1));
    row("S badfd level 999 NULL len4", sso(bad, 999, 999, NULL, 4));
    row("S file", sso(file, SOL_SOCKET, SO_REUSEADDR, &one, 4));
    row("S pipe", sso(pipes[0], SOL_SOCKET, SO_REUSEADDR, &one, 4));
    row("S file NULL val len4", sso(file, SOL_SOCKET, SO_REUSEADDR, NULL, 4));
    row("S file unmapped val len4", sso(file, SOL_SOCKET, SO_REUSEADDR, unmapped, 4));
    row("S file len0", sso(file, SOL_SOCKET, SO_REUSEADDR, &one, 0));
    row("S file len -1", sso(file, SOL_SOCKET, SO_REUSEADDR, &one, (socklen_t)-1));
#ifdef __linux__
    int port = epoll_create1(0);
#else
    int port = kqueue();
#endif
    row("S event port", sso(port, SOL_SOCKET, SO_REUSEADDR, &one, 4));

    int s = tcp();
    row("S sock len0", sso(s, SOL_SOCKET, SO_REUSEADDR, &one, 0));
    row("S sock len1", sso(s, SOL_SOCKET, SO_REUSEADDR, &one, 1));
    row("S sock len3", sso(s, SOL_SOCKET, SO_REUSEADDR, &one, 3));
    row("S sock NULL len0", sso(s, SOL_SOCKET, SO_REUSEADDR, NULL, 0));
    row("S sock NULL len3", sso(s, SOL_SOCKET, SO_REUSEADDR, NULL, 3));
    row("S sock NULL len4", sso(s, SOL_SOCKET, SO_REUSEADDR, NULL, 4));
    row("S sock unmapped len3", sso(s, SOL_SOCKET, SO_REUSEADDR, unmapped, 3));
    row("S sock unmapped len4", sso(s, SOL_SOCKET, SO_REUSEADDR, unmapped, 4));
    row("S sock unmapped len -1", sso(s, SOL_SOCKET, SO_REUSEADDR, unmapped, (socklen_t)-1));
    printf("  after failures readback=%d\n", readback(s));
    row("S sock len -1", sso(s, SOL_SOCKET, SO_REUSEADDR, &one, (socklen_t)-1));
    printf("  readback=%d\n", readback(s));
    row("S sock len 0x80000000", sso(s, SOL_SOCKET, SO_REUSEADDR, big, (socklen_t)0x80000000u));
    printf("  readback=%d\n", readback(s));
    row("S sock level 999 len -1", sso(s, 999, 999, &one, (socklen_t)-1));
    row("S sock level 999 NULL len4", sso(s, 999, 999, NULL, 4));
    row("S sock SOL_SOCKET optname 9999 len4", sso(s, SOL_SOCKET, 9999, &one, 4));
    row("S sock SOL_SOCKET optname 9999 len0", sso(s, SOL_SOCKET, 9999, &one, 0));

    printf("\n== setsockopt values ==\n");
    int vals[] = {0, 1, 2, 4, -1, 0x100, 0x10000, 0x1000000, 0};
    for (unsigned i = 0; i < sizeof vals / sizeof vals[0]; i++) {
        int v = vals[i];
        int e = sso(s, SOL_SOCKET, SO_REUSEADDR, &v, 4);
        printf("set %-12d -> %-6s readback=%d\n", v, en(e), readback(s));
    }
    // Each row below first sets the opposite value through real storage, so
    // that a readback which merely repeats the previous state cannot pass.
    memset(big, 0, sizeof big);
    big[5] = 1;
    setThenRead("set len8 int=0 tail!=0", s, 1, big, 8);
    memset(big, 0, sizeof big);
    *(int *)big = 1;
    setThenRead("set len16 int=1", s, 0, big, 16);
    // A declared length reaching past the int into a protected page: put the
    // int at the very end of a page whose successor is PROT_NONE.
    {
        long page = sysconf(_SC_PAGESIZE);
        char *two = mmap(NULL, 2 * page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if (two == MAP_FAILED || mprotect(two + page, page, PROT_NONE) != 0) {
            printf("guard page setup failed: %d\n", errno);
            return 1;
        }
        int *at = (int *)(two + page - 4);
        *at = 0;
        setThenRead("set len8 last-4-bytes-mapped int=0", s, 1, at, 8);
        *at = 1;
        setThenRead("set len8 last-4-bytes-mapped int=1", s, 0, at, 8);
        // The control: the int itself straddles into the protected page, so
        // this row must fault, or the guard page is not guarding.
        char *straddle = two + page - 2;
        setThenRead("set len4 straddling (control, must fault)", s, 0, straddle, 4);
    }

    printf("\n== getsockopt ==\n");
    socklen_t len;
    int v;
    int zero = 0;
    sso(s, SOL_SOCKET, SO_REUSEADDR, &one, 4);
    v = 0x5a5a5a5a; len = 4; gso("G badfd", bad, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; gso("G badfd NULL len", bad, SOL_SOCKET, SO_REUSEADDR, &v, NULL);
    v = 0x5a5a5a5a; len = 4; gso("G file", file, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; gso("G file NULL len", file, SOL_SOCKET, SO_REUSEADDR, &v, NULL);
    v = 0x5a5a5a5a; len = 4; gso("G event port", port, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; len = 4; gso("G sock set", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; gso("G sock NULL len", s, SOL_SOCKET, SO_REUSEADDR, &v, NULL);
    v = 0x5a5a5a5a; gso("G sock unmapped len", s, SOL_SOCKET, SO_REUSEADDR, &v, unmapped);
    v = 0x5a5a5a5a; len = 0; gso("G sock len0", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; len = 1; gso("G sock len1", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; len = 2; gso("G sock len2", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; len = 3; gso("G sock len3", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    {
        char b16[16];
        memset(b16, 0x5a, sizeof b16);
        len = 16;
        errno = 0;
        int r = getsockopt(s, SOL_SOCKET, SO_REUSEADDR, b16, &len);
        printf("%-52s %-10s len=%u bytes=", "G sock len16", en(r == 0 ? 0 : errno), len);
        for (int i = 0; i < 16; i++) printf("%02x", (unsigned char)b16[i]);
        printf("\n");
    }
    v = 0x5a5a5a5a; len = (socklen_t)-1; gso("G sock len -1", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; len = (socklen_t)0x80000000u; gso("G sock len 0x80000000", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    len = 4; gso("G sock NULL val len4", s, SOL_SOCKET, SO_REUSEADDR, NULL, &len);
    len = 0; gso("G sock NULL val len0", s, SOL_SOCKET, SO_REUSEADDR, NULL, &len);
    len = 4; gso("G sock unmapped val len4", s, SOL_SOCKET, SO_REUSEADDR, unmapped, &len);
    len = 0; gso("G sock unmapped val len0", s, SOL_SOCKET, SO_REUSEADDR, unmapped, &len);
    len = (socklen_t)-1; gso("G sock unmapped val len -1", s, SOL_SOCKET, SO_REUSEADDR, unmapped, &len);
    len = (socklen_t)-1; gso("G sock NULL val len -1", s, SOL_SOCKET, SO_REUSEADDR, NULL, &len);
    gso("G sock NULL val NULL len", s, SOL_SOCKET, SO_REUSEADDR, NULL, NULL);
    gso("G sock unmapped val unmapped len", s, SOL_SOCKET, SO_REUSEADDR, unmapped, unmapped);
    v = 0x5a5a5a5a; len = 4; gso("G sock level 999", s, 999, 999, &v, &len);
    v = 0x5a5a5a5a; gso("G sock level 999 NULL len", s, 999, 999, &v, NULL);
    v = 0x5a5a5a5a; len = 4; gso("G sock SOL_SOCKET 9999", s, SOL_SOCKET, 9999, &v, &len);
    v = 0x5a5a5a5a; gso("G sock SOL_SOCKET 9999 NULL len", s, SOL_SOCKET, 9999, &v, NULL);
    sso(s, SOL_SOCKET, SO_REUSEADDR, &zero, 4);
    v = 0x5a5a5a5a; len = 4; gso("G sock clear", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);
    v = 0x5a5a5a5a; len = 2; gso("G sock clear len2", s, SOL_SOCKET, SO_REUSEADDR, &v, &len);

    printf("\n== domains and kinds (set 1, readback) ==\n");
    {
        int kinds[][2] = {{AF_INET, SOCK_DGRAM}, {AF_INET6, SOCK_STREAM}, {AF_INET6, SOCK_DGRAM}, {AF_UNIX, SOCK_STREAM},
                          {AF_UNIX, SOCK_DGRAM}};
        for (unsigned i = 0; i < 5; i++) {
            int f = socket(kinds[i][0], kinds[i][1], 0);
            int before = readback(f);
            int e = sso(f, SOL_SOCKET, SO_REUSEADDR, &one, 4);
            printf("domain %d type %d: fresh=%d set->%s readback=%d\n", kinds[i][0], kinds[i][1], before, en(e),
                   readback(f));
            close(f);
        }
    }

    printf("\n== phases (set 1 / set 0 / get) ==\n");
#define PHASE(label, fd)                                                                                               \
    do {                                                                                                               \
        int g0 = readback(fd);                                                                                         \
        int e1 = sso(fd, SOL_SOCKET, SO_REUSEADDR, &one, 4);                                                           \
        int g1 = readback(fd);                                                                                         \
        int e0 = sso(fd, SOL_SOCKET, SO_REUSEADDR, &zero, 4);                                                          \
        int g2 = readback(fd);                                                                                         \
        int e1b = sso(fd, SOL_SOCKET, SO_REUSEADDR, &one, 4);                                                          \
        int g3 = readback(fd);                                                                                         \
        int short1 = sso(fd, SOL_SOCKET, SO_REUSEADDR, &one, 2);                                                       \
        int null1 = sso(fd, SOL_SOCKET, SO_REUSEADDR, NULL, 4);                                                        \
        printf("%-28s get=%d set1=%s get=%d set0=%s get=%d set1=%s get=%d short=%s null=%s\n", label, g0, en(e1), g1,   \
               en(e0), g2, en(e1b), g3, en(short1), en(null1));                                                        \
    } while (0)

    {
        int f = tcp();
        PHASE("fresh", f);
        close(f);
        f = tcp();
        bindTo(f, 0);
        PHASE("bound", f);
        close(f);

        int l = tcp();
        bindTo(l, 0);
        listen(l, 5);
        PHASE("listening", l);
        uint16_t lp = portOf(l);

        int c = tcp();
        struct sockaddr_in la = lo(lp);
        connect(c, (struct sockaddr *)&la, sizeof la);
        PHASE("established client", c);
        int a = accept(l, NULL, NULL);
        PHASE("accepted", a);
        close(a);
        usleep(100000);
        PHASE("established, peer closed", c);
        close(c);
        close(l);

        // refused: a port nothing holds (bound, read, closed)
        int target = tcp();
        bindTo(target, 0);
        uint16_t tp = portOf(target);
        close(target);
        struct sockaddr_in ta = lo(tp);

        int r = tcp();
        int ce = connect(r, (struct sockaddr *)&ta, sizeof ta) == 0 ? 0 : errno;
        printf("blocking connect to closed port: %s (%d)\n", en(ce), ce);
        PHASE("after blocking refusal", r);
        close(r);

        r = tcp();
        fcntl(r, F_SETFL, O_NONBLOCK);
        ce = connect(r, (struct sockaddr *)&ta, sizeof ta) == 0 ? 0 : errno;
        printf("nonblocking connect: %s\n", en(ce));
        usleep(200000);
        PHASE("refused pending delivery", r);
        ce = connect(r, (struct sockaddr *)&ta, sizeof ta) == 0 ? 0 : errno;
        printf("delivering connect: %s (%d)\n", en(ce), ce);
        PHASE("after delivery", r);
        close(r);

        // Same, but getsockopt only, no set before delivery.
        r = tcp();
        fcntl(r, F_SETFL, O_NONBLOCK);
        connect(r, (struct sockaddr *)&ta, sizeof ta);
        usleep(200000);
        printf("pending: get=%d\n", readback(r));
        ce = connect(r, (struct sockaddr *)&ta, sizeof ta) == 0 ? 0 : errno;
        printf("delivering connect (after only a get): %s (%d)\n", en(ce), ce);
        close(r);

        int d = socket(AF_INET, SOCK_DGRAM, 0);
        connect(d, (struct sockaddr *)&ta, sizeof ta);
        PHASE("datagram peer", d);
        close(d);
    }

    printf("\n== clearing SO_REUSEADDR on a bound socket ==\n");
    {
        // A and B both reuse, bound on the same address; A clears; C (reuse) binds.
        int A = tcp();
        sso(A, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        bindTo(A, 0);
        uint16_t p = portOf(A);
        int B = tcp();
        sso(B, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("B(reuse) bind same as A(reuse): %s\n", en(bindTo(B, p)));
        sso(A, SOL_SOCKET, SO_REUSEADDR, &zero, 4);
        int C = tcp();
        sso(C, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("A cleared; C(reuse) bind same: %s\n", en(bindTo(C, p)));
        close(A);
        close(B);
        close(C);

        // A alone, reuse, clears, C(reuse) binds.
        A = tcp();
        sso(A, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        bindTo(A, 0);
        p = portOf(A);
        sso(A, SOL_SOCKET, SO_REUSEADDR, &zero, 4);
        C = tcp();
        sso(C, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("A(reuse) alone then cleared; C(reuse) bind same: %s\n", en(bindTo(C, p)));
        close(A);
        close(C);

        // A bound without reuse, then sets it; C(reuse) binds.
        A = tcp();
        bindTo(A, 0);
        p = portOf(A);
        sso(A, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        C = tcp();
        sso(C, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("A(no reuse) bound then set; C(reuse) bind same: %s\n", en(bindTo(C, p)));
        close(A);
        close(C);

        // A bound without reuse; C(no reuse) binds -> control
        A = tcp();
        bindTo(A, 0);
        p = portOf(A);
        C = tcp();
        printf("control: A(no reuse), C(no reuse): %s\n", en(bindTo(C, p)));
        close(A);
        close(C);

        // A(reuse) and B(reuse) share; A clears; A listens
        A = tcp();
        sso(A, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        bindTo(A, 0);
        p = portOf(A);
        B = tcp();
        sso(B, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        bindTo(B, p);
        sso(B, SOL_SOCKET, SO_REUSEADDR, &zero, 4);
        errno = 0;
        printf("A,B reuse share; B clears; A listens: %s\n", en(listen(A, 5) == 0 ? 0 : errno));
        close(A);
        close(B);
    }
    printf("\n== clearing, wildcard existing, specific candidate ==\n");
    {
        struct sockaddr_in w;
        memset(&w, 0, sizeof w);
#ifdef __APPLE__
        w.sin_len = sizeof w;
#endif
        w.sin_family = AF_INET;
        int A = tcp();
        sso(A, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        bind(A, (struct sockaddr *)&w, sizeof w);
        uint16_t p = portOf(A);
        int C = tcp();
        sso(C, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("wild A(reuse); C(reuse) lo: %s\n", en(bindTo(C, p)));
        close(C);
        sso(A, SOL_SOCKET, SO_REUSEADDR, &zero, 4);
        C = tcp();
        sso(C, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("wild A(reuse) cleared; C(reuse) lo: %s\n", en(bindTo(C, p)));
        close(C);
        close(A);

        A = tcp();
        bind(A, (struct sockaddr *)&w, sizeof w);
        p = portOf(A);
        C = tcp();
        sso(C, SOL_SOCKET, SO_REUSEADDR, &one, 4);
        printf("wild A(no reuse); C(reuse) lo: %s\n", en(bindTo(C, p)));
        close(C);
        close(A);
    }
    refusedAndEdgeRows();
    return 0;
}
