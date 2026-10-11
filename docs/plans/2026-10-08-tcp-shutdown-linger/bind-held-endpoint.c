// What an explicit bind(2) of a connection end's endpoint answers while that
// end still holds it, by the end's state, whether its socket is open or
// closed, SO_REUSEADDR on either side, and the address the candidate names;
// and what listen(2) then answers on a candidate that bound.
//
// c is the connecting socket and p the accepted one; the listener is closed
// once p is accepted. The end `o` whose endpoint is bound is c or p, and `q`
// is the other. Each row makes a fresh pair, and `o`'s state is one of:
//   released        q shutdown(WR); o's FIN (passive) arrives;
//   tw              o's FIN first, then q shutdown(WR) (o in TIME_WAIT);
//   fw2             o's FIN, and q sends none (o in FIN_WAIT2);
//   active-queued   o fills q's window, then o's FIN, queued behind the
//                   bytes (o in FIN_WAIT1);
//   passive-queued  q shutdown(WR); o fills, then o's FIN, queued behind the
//                   bytes (o in LAST_ACK, its FIN unsent).
// "o's FIN" is close(o) in a `closed` row and shutdown(o, WR) in an `open`
// row, where o stays open. q stays open throughout and never reads.
//
// oreuse=1 sets SO_REUSEADDR on o's socket before it is bound: on c before
// its connect, and on the listener before its bind, which p inherits (the
// value read back from o, before its FIN, is printed in brackets).
//
// Each row then binds four fresh sockets in turn, each closed before the
// next: without and with SO_REUSEADDR, at 127.0.0.1 and 0.0.0.0, on o's
// port; and a candidate that bound then calls listen(2). A queued row ends
// `unsent` if bytes o wrote were still unsent after the binds, so that its
// FIN was still waiting behind them, and `all-delivered` otherwise.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/bind-held-endpoint bind-held-endpoint.c && /tmp/bind-held-endpoint
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/bind-held-endpoint.c && /tmp/p'
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) four times and Darwin 27.0.0 arm64 (uid 501) five times,
// with identical output each time; one run of each is
// bind-held-endpoint.linux-6.18.5-aarch64.txt and
// bind-held-endpoint.darwin-27.0.txt. On both, the endpoint is held exactly
// where tcp-shutdown.c's section F says (released once o's FIN, made after
// q's had arrived, has itself arrived), whether o is open or closed; and
// while it is held, a bind is refused exactly as one over a connected socket
// that is still open: on Linux unless both o and the candidate carry
// SO_REUSEADDR, on Darwin unless the candidate does, whatever the address.
// A candidate that bound then listens. A fill that left nothing unsent on
// Darwin (its receive buffer grows under the fill) released a passive-queued
// endpoint, which is why the fill is repeated until something stays unsent.

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>

static void die(const char *what) { perror(what); exit(2); }

static void settle(void) { usleep(30000); }

static const char *ename(int e)
{
    switch (e) {
    case EADDRINUSE: return "EADDRINUSE";
    case EINVAL: return "EINVAL";
    case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
    default: {
        static char b[8][32];
        static int i;
        i = (i + 1) % 8;
        snprintf(b[i], 32, "errno%d", e);
        return b[i];
    }
    }
}

static void set_nb(int fd)
{
    int fl = fcntl(fd, F_GETFL);
    if (fcntl(fd, F_SETFL, fl | O_NONBLOCK) < 0) die("fcntl");
}

static void set_reuse(int fd)
{
    int one = 1;
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one) < 0) die("SO_REUSEADDR");
}

static int get_reuse(int fd)
{
    int v = -1;
    socklen_t l = sizeof v;
    if (getsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &v, &l) < 0) die("getsockopt");
    return v != 0;
}

// Close with SO_LINGER {1, 0}, so that a socket the row has finished with
// leaves no TIME_WAIT behind.
static void discard(int fd)
{
    struct linger lg = { .l_onoff = 1, .l_linger = 0 };
    setsockopt(fd, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
    close(fd);
}

static struct sockaddr_in at(uint32_t address, int port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(address);
    return a;
}

static int port_of(int fd)
{
    struct sockaddr_in a;
    socklen_t l = sizeof a;
    if (getsockname(fd, (struct sockaddr *)&a, &l) < 0) die("getsockname");
    return ntohs(a.sin_port);
}

// A connected pair, both non-blocking, the listener closed. `creuse` sets
// SO_REUSEADDR on c before its connect, `lreuse` on the listener.
static void pair(int creuse, int lreuse, int *c, int *p)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    if (l < 0) die("socket");
    if (lreuse) set_reuse(l);
    struct sockaddr_in a = at(0x7f000001, 0);
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, 4) < 0) die("listen");
    *c = socket(AF_INET, SOCK_STREAM, 0);
    if (*c < 0) die("socket");
    if (creuse) set_reuse(*c);
    struct sockaddr_in d = at(0x7f000001, port_of(l));
    if (connect(*c, (struct sockaddr *)&d, sizeof d) < 0) die("connect");
    settle();
    *p = accept(l, NULL, NULL);
    if (*p < 0) die("accept");
    close(l);
    set_nb(*c);
    set_nb(*p);
}

// Writes from o until q's window is closed with bytes still in o's send
// buffer, so that o's FIN will wait behind them. Darwin's receive buffer
// grows under a fill, so that a fill can end with everything delivered;
// it is filled again until something stays unsent.
static long fill(int o, int q)
{
    static char buf[1 << 16];
    long written = 0;
    for (int round = 0; round < 20; round++) {
        int idle = 0;
        while (idle < 3) {
            long r = write(o, buf, sizeof buf);
            if (r < 0) {
                if (errno != EAGAIN) die("fill");
                idle++;
                settle();
            } else {
                written += r;
            }
        }
        settle();
        int unread = 0;
        if (ioctl(q, FIONREAD, &unread) < 0) die("FIONREAD");
        if (written > unread) return written;
    }
    fprintf(stderr, "fill: nothing stays unsent\n");
    exit(2);
}

// o's FIN: close in a `closed` row, shutdown(WR) in an `open` one.
static void fin(int o, int open)
{
    if (open) shutdown(o, SHUT_WR);
    else close(o);
    settle();
}

static const char *candidate(int reuse, uint32_t address, int port)
{
    static char b[4][64];
    static int i;
    i = (i + 1) % 4;
    int n = socket(AF_INET, SOCK_STREAM, 0);
    if (n < 0) die("socket");
    if (reuse) set_reuse(n);
    struct sockaddr_in a = at(address, port);
    if (bind(n, (struct sockaddr *)&a, sizeof a) < 0) {
        snprintf(b[i], 64, "bind-%s", ename(errno));
    } else if (listen(n, 4) < 0) {
        snprintf(b[i], 64, "bind-ok listen-%s", ename(errno));
    } else {
        snprintf(b[i], 64, "bind-ok listen-ok");
    }
    close(n);
    return b[i];
}

int main(void)
{
    signal(SIGPIPE, SIG_IGN);
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("#\t%s %s %s\n", u.sysname, u.release, u.machine);
    const char *states[] = { "released", "tw", "fw2", "active-queued", "passive-queued" };
    for (int oc = 1; oc >= 0; oc--)
        for (int s = 0; s < 5; s++)
            for (int open = 0; open < 2; open++)
                for (int oreuse = 0; oreuse < 2; oreuse++) {
                    int c, p;
                    pair(oc && oreuse, !oc && oreuse, &c, &p);
                    int o = oc ? c : p, q = oc ? p : c;
                    int port = port_of(o);
                    int got = get_reuse(o);
                    const char *state = states[s];
                    long written = 0;
                    if (!strcmp(state, "released")) {
                        shutdown(q, SHUT_WR);
                        settle();
                        fin(o, open);
                    } else if (!strcmp(state, "tw")) {
                        fin(o, open);
                        shutdown(q, SHUT_WR);
                        settle();
                    } else if (!strcmp(state, "fw2")) {
                        fin(o, open);
                    } else if (!strcmp(state, "active-queued")) {
                        written = fill(o, q);
                        fin(o, open);
                    } else {
                        shutdown(q, SHUT_WR);
                        settle();
                        written = fill(o, q);
                        fin(o, open);
                    }
                    const char *exact = candidate(0, 0x7f000001, port);
                    const char *exactr = candidate(1, 0x7f000001, port);
                    const char *any = candidate(0, 0, port);
                    const char *anyr = candidate(1, 0, port);
                    // Whether bytes o wrote were still unsent after the binds.
                    int unread = 0;
                    if (ioctl(q, FIONREAD, &unread) < 0) die("FIONREAD");
                    const char *unsent = written == 0 ? "" : written > unread ? "\tunsent" : "\tall-delivered";
                    printf("H\t%s\t%s\t%s\toreuse=%d(%d)\tlo %s\tlo+reuse %s\tany %s\tany+reuse %s%s\n",
                           oc ? "c" : "p", state, open ? "open" : "closed", oreuse, got,
                           exact, exactr, any, anyr, unsent);
                    fflush(stdout);
                    if (open) discard(o);
                    discard(q);
                    settle();
                }
    return 0;
}
