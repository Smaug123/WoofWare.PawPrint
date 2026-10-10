// Which options the socket accept(2) returns carries, when the listener's
// options change while connections wait in its accept queue.
//
// Section O: for each option the kernel model stores, the listener is given
// v0, a client connects (c1), the listener is given v1, a second client
// connects (c2), the listener is given v2, and both connections are accepted,
// oldest first. Each line prints what the socket accepted for c1 and the one
// accepted for c2 read back, and what the listener reads back. A value is an
// int, or a struct linger as {l_onoff,l_linger}.
//
// Section D, Linux only: an accept whose address length is negative takes a
// connection and answers EINVAL, closing the socket it made. The listener's
// SO_LINGER is one value when the connection completes and another at the
// accept; the line prints the accept's answer and what the client then
// reads: 0 for the FIN of an orderly close, ECONNRESET for a reset.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/queued-options queued-options.c && /tmp/queued-options
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/queued-options.c && /tmp/p'
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), twice on each, with the
// same output both times: queued-options.linux-6.18.5-aarch64.txt and
// queued-options.darwin-27.0.txt. On both, each accepted socket carries every
// option as the listener held it when that socket's connection completed,
// whatever the listener holds at the accept. Darwin's one difference is
// tcp_attach's: a connection completing while the listener lingers for no
// time gets XNU's TCP_LINGERTIME, 120 seconds, as its linger time. Section D
// shows Linux's dropped connection closing with the linger it completed with:
// a FIN where that was off, a reset where it was {1, 0}.
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static void settle(void) { usleep(30000); }

static void check(const char *what, int result)
{
    if (result < 0) { perror(what); exit(1); }
}

#ifdef __linux__
static const char *en(int e)
{
    switch (e) {
    case 0: return "0";
    case EINVAL: return "EINVAL";
    case EAGAIN: return "EAGAIN";
    case ECONNRESET: return "ECONNRESET";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}
#endif

struct opt { const char *name; int level, option, size; };

// A value: the first int, and for a struct linger the second.
struct value { int a, b; };

static void set(int fd, struct opt o, struct value v)
{
    struct linger lg = { v.a, v.b };
    int i = v.a;
    const void *p = o.size == 4 ? (const void *)&i : (const void *)&lg;
    check(o.name, setsockopt(fd, o.level, o.option, p, o.size));
}

static const char *render(struct opt o, struct value v)
{
    // Six live at once in one printf.
    static char b[8][32];
    static int k = 0;
    k = (k + 1) % 8;
    if (o.size == 4) snprintf(b[k], sizeof b[k], "%d", v.a);
    else snprintf(b[k], sizeof b[k], "{%d,%d}", v.a, v.b);
    return b[k];
}

static const char *get(int fd, struct opt o)
{
    struct linger lg = { 77, 77 };
    int i = 77;
    void *p = o.size == 4 ? (void *)&i : (void *)&lg;
    socklen_t len = o.size;
    check(o.name, getsockopt(fd, o.level, o.option, p, &len));
    struct value v = o.size == 4 ? (struct value){ i, 0 } : (struct value){ lg.l_onoff, lg.l_linger };
    return render(o, v);
}

static int listener(struct sockaddr_in *a)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    check("socket", l);
    memset(a, 0, sizeof *a);
#ifdef __APPLE__
    a->sin_len = sizeof *a;
#endif
    a->sin_family = AF_INET;
    a->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    check("bind", bind(l, (struct sockaddr *)a, sizeof *a));
    check("listen", listen(l, 8));
    socklen_t al = sizeof *a;
    check("getsockname", getsockname(l, (struct sockaddr *)a, &al));
    return l;
}

static int connect_to(struct sockaddr_in *a)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    check("socket", c);
    check("connect", connect(c, (struct sockaddr *)a, sizeof *a));
    settle();
    return c;
}

static void section_o(void)
{
    struct opt opts[] = {
        { "SO_REUSEADDR", SOL_SOCKET, SO_REUSEADDR, 4 },
        { "TCP_NODELAY", IPPROTO_TCP, TCP_NODELAY, 4 },
        { "SO_LINGER", SOL_SOCKET, SO_LINGER, 8 },
#ifdef SO_LINGER_SEC
        { "SO_LINGER_SEC", SOL_SOCKET, SO_LINGER_SEC, 8 },
#endif
    };
    struct value ints[2][3] = { { { 0, 0 }, { 1, 0 }, { 0, 0 } }, { { 1, 0 }, { 0, 0 }, { 1, 0 } } };
    struct value lingers[2][3] = { { { 0, 0 }, { 1, 7 }, { 1, 3 } }, { { 1, 7 }, { 1, 0 }, { 0, 5 } } };
    for (unsigned k = 0; k < sizeof opts / sizeof opts[0]; k++) {
        struct opt o = opts[k];
        for (int s = 0; s < 2; s++) {
            struct value *v = o.size == 4 ? ints[s] : lingers[s];
            struct sockaddr_in a;
            int l = listener(&a);
            set(l, o, v[0]);
            int c1 = connect_to(&a);
            set(l, o, v[1]);
            int c2 = connect_to(&a);
            set(l, o, v[2]);
            int s1 = accept(l, NULL, NULL);
            check("accept", s1);
            int s2 = accept(l, NULL, NULL);
            check("accept", s2);
            printf("O\t%s\t%s %s %s\tfirst=%s second=%s listener=%s\n", o.name, render(o, v[0]), render(o, v[1]),
                   render(o, v[2]), get(s1, o), get(s2, o), get(l, o));
            // Neither accepted socket lingers for no time when it closes here:
            // a reset would race the clients' closes for nothing measured.
            close(c1);
            close(c2);
            settle();
            close(s1);
            close(s2);
            close(l);
        }
    }
}

#ifdef __linux__
static void section_d(void)
{
    struct opt o = { "SO_LINGER", SOL_SOCKET, SO_LINGER, 8 };
    struct value rows[2][2] = { { { 0, 0 }, { 1, 0 } }, { { 1, 0 }, { 0, 0 } } };
    for (int r = 0; r < 2; r++) {
        struct sockaddr_in a;
        int l = listener(&a);
        set(l, o, rows[r][0]);
        int c = connect_to(&a);
        set(l, o, rows[r][1]);
        struct sockaddr_in peer;
        socklen_t len = (socklen_t)-1;
        errno = 0;
        int s = accept(l, (struct sockaddr *)&peer, &len);
        int acceptErrno = s < 0 ? errno : 0;
        settle();
        check("fcntl", fcntl(c, F_SETFL, O_NONBLOCK));
        char buf[16];
        errno = 0;
        ssize_t n = read(c, buf, sizeof buf);
        char readText[32];
        if (n < 0) snprintf(readText, sizeof readText, "-1 %s", en(errno));
        else snprintf(readText, sizeof readText, "%zd", n);
        printf("D\tcompleted %s accepted %s\taccept=%d %s client-read=%s\n", render(o, rows[r][0]),
               render(o, rows[r][1]), s, en(acceptErrno), readText);
        if (s >= 0) close(s);
        close(c);
        close(l);
    }
}
#endif

int main(void)
{
    section_o();
#ifdef __linux__
    section_d();
#endif
    return 0;
}
