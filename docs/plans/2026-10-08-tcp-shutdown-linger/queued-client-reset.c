// What a listener's accept makes of a connection whose client closed while
// the connection waited in the accept queue: abortively (SO_LINGER {1, 0}) or
// in order, with or without having written first.
//
// For each row the client connects to a non-blocking listener, optionally
// writes 100 bytes, optionally sets SO_LINGER {1, 0}, and closes; nothing has
// accepted yet. Then: the listener's readiness, a bind of the client's
// endpoint, the accept, and if it returns a socket, two reads of 4096 bytes,
// SO_ERROR, getpeername, a write of 100 bytes, and, once what the write set
// in motion has arrived, a further read and SO_ERROR.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/queued-client-reset queued-client-reset.c && /tmp/queued-client-reset
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/queued-client-reset.c && /tmp/p'
//
// Measured 2026-10-13 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), twice on each, with the
// same output both times: queued-client-reset.linux-6.18.5-aarch64.txt and
// queued-client-reset.darwin-27.0.txt. On both, the accept returns the
// connection whatever the client did; after a reset the accepted socket
// reads what was sent, then ECONNRESET, then end of file, its write answers
// EPIPE, and getpeername answers ENOTCONN on Linux and EINVAL on Darwin; and
// the reset frees the client's endpoint at once, where a FIN leaves it held.
// After a FIN, the server's write is taken and resets the connection, which
// Linux reports as EPIPE pending behind end of file and Darwin as ECONNRESET
// at the next read, as for any write after the peer's close. Without the pause
// before that read, Darwin's read can come before the reset and answer 0.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>

static void settle(void) { usleep(30000); }

static const char *ename(int e)
{
    switch (e) {
    case EAGAIN: return "EAGAIN";
    case ECONNRESET: return "ECONNRESET";
    case ECONNABORTED: return "ECONNABORTED";
    case EPIPE: return "EPIPE";
    case ENOTCONN: return "ENOTCONN";
    case EINVAL: return "EINVAL";
    case EADDRINUSE: return "EADDRINUSE";
    default: {
        static char b[32];
        snprintf(b, sizeof b, "errno%d", e);
        return b;
    }
    }
}

static char out[8][64];
static int outi;

static const char *ans(long r, int e)
{
    char *b = out[outi++ % 8];
    if (r < 0)
        snprintf(b, 64, "-1 %s", ename(e));
    else
        snprintf(b, 64, "%ld", r);
    return b;
}

static int port_of(int fd)
{
    struct sockaddr_in a;
    socklen_t l = sizeof a;
    getsockname(fd, (struct sockaddr *)&a, &l);
    return ntohs(a.sin_port);
}

static struct sockaddr_in loopback(int port)
{
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

static const char *bind_free(int port)
{
    int f = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a = loopback(port);
    int r = bind(f, (struct sockaddr *)&a, sizeof a);
    int e = errno;
    close(f);
    if (r == 0) return "bind-ok";
    return e == EADDRINUSE ? "bind-EADDRINUSE" : ename(e);
}

int main(void)
{
    signal(SIGPIPE, SIG_IGN);
    static char buf[4096];
    const char *names[] = { "fin", "fin-wrote", "reset", "reset-wrote" };
    for (int row = 0; row < 4; row++) {
        int abortive = row >= 2;
        int wrote = row % 2 == 1;

        int l = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in la = loopback(0);
        bind(l, (struct sockaddr *)&la, sizeof la);
        listen(l, 4);
        fcntl(l, F_SETFL, O_NONBLOCK);
        int port = port_of(l);

        int c = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in ca = loopback(port);
        if (connect(c, (struct sockaddr *)&ca, sizeof ca) != 0) {
            perror("connect");
            return 1;
        }
        int cport = port_of(c);
        if (wrote) write(c, buf, 100);
        if (abortive) {
            struct linger lg = { 1, 0 };
            setsockopt(c, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
        }
        close(c);
        settle();

        struct pollfd pl = { l, POLLIN, 0 };
        poll(&pl, 1, 0);
        const char *b = bind_free(cport);
        int a = accept(l, NULL, NULL);
        int ae = errno;
        if (a < 0) {
            printf("Q\t%s\tlistener-poll=0x%x %s accept=%s\n", names[row], pl.revents, b, ans(a, ae));
            close(l);
            continue;
        }
        fcntl(a, F_SETFL, O_NONBLOCK);
        long r1 = read(a, buf, sizeof buf);
        int e1 = errno;
        long r2 = read(a, buf, sizeof buf);
        int e2 = errno;
        int soerr = 0;
        socklen_t sl = sizeof soerr;
        getsockopt(a, SOL_SOCKET, SO_ERROR, &soerr, &sl);
        struct sockaddr_in pa;
        socklen_t pal = sizeof pa;
        int pn = getpeername(a, (struct sockaddr *)&pa, &pal);
        int pe = errno;
        long w = write(a, buf, 100);
        int we = errno;
        settle();
        long r3 = read(a, buf, sizeof buf);
        int e3 = errno;
        int soerr2 = 0;
        sl = sizeof soerr2;
        getsockopt(a, SOL_SOCKET, SO_ERROR, &soerr2, &sl);
        printf("Q\t%s\tlistener-poll=0x%x %s accept=ok read=%s,%s soerr=%s getpeername=%s write100=%s read=%s soerr=%s\n",
               names[row], pl.revents, b, ans(r1, e1), ans(r2, e2), soerr ? ename(soerr) : "0",
               pn == 0 ? "ok" : ename(pe), ans(w, we), ans(r3, e3), soerr2 ? ename(soerr2) : "0");
        close(a);
        close(l);
    }
    return 0;
}
