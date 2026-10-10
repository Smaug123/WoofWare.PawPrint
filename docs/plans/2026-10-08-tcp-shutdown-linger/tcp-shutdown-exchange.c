// What a loopback TCP pair answers once both FINs have been made, on Linux
// and Darwin: a follow-up to tcp-shutdown.c, whose conventions it keeps (c is
// the connecting socket, p the accepted one, both non-blocking; every call
// that can put a segment on the wire is followed by a 30 ms settle; SIGPIPE
// is caught and counted).
//
// Sections:
//   K   c and p both shut writing, in either order (c-first, p-first), with
//       p having sent c one byte c has not read. Then shutdown(x, how) for
//       each end and how: the answer. On a fresh pair per row.
//   U   the same exchange, then c closes over its unread byte, without
//       linger: what p then reads twice and SO_ERROR(p).
//   Q   c fills its send buffer and shuts writing, so its FIN waits behind
//       its bytes; p shuts writing, so p's FIN reaches c; p closes over the
//       bytes it has not read, which resets c. Then c reads twice and
//       SO_ERROR(c), and the same with SO_ERROR first.
//   V   p shuts writing, and then closes over bytes from c it has not read,
//       while c is otherwise idle: c sent 1 byte (one), or filled its send
//       buffer (full); and c shut writing before p closed (cfin) or not.
//       Then c reads twice, SO_ERROR(c), c writes 100, SO_ERROR(c). The
//       control row (noshut) is p closing over 1 unread byte without
//       having shut writing.
//   W   as V, but p never shuts writing: c writes 1 byte (one) or fills
//       (full), then shuts writing, and p closes over what it has not read.
//   T   V's full-cfin and W's full, then 5 s later: c reads, SO_ERROR(c).
//   G   c closes over bytes from p it has not read, for each state of each
//       FIN: c's none, queued (c filled its send buffer and shut writing) or
//       arrived (c shut writing with nothing pending), and p's likewise
//       (queued: p filled and shut writing; otherwise p wrote 1 byte), the
//       shutdowns in either order (c-first, p-first). Then SO_ERROR(p), what
//       p reads until it is answered anything but bytes, and 2 s later
//       SO_ERROR(p) and a read again. On Darwin a row where c's FIN is
//       queued and p's has arrived is marked `~timing`: no reset comes, p
//       reads what it holds and then EAGAIN, and c's remaining bytes and FIN
//       never arrive, so a timer must end it; the model refuses that close.
//       A Darwin row where both FINs are queued is marked `~timing` too: it
//       usually resets, but in one of eight runs it hung as above, as if
//       p's FIN had arrived (Darwin grows a receive buffer on its own).
//
// A line ending `~timing` is not to be replayed, as in tcp-shutdown.c.
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root)
// and Darwin 27.0.0 arm64 (uid 501), the whole probe twice each with
// identical output; one run of each is tcp-shutdown-exchange.linux-6.18.5-aarch64.txt and
// tcp-shutdown-exchange.darwin-27.0.txt.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/tse tcp-shutdown-exchange.c && /tmp/tse
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/tcp-shutdown-exchange.c && /tmp/p'

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>

static int sigpipes;
static void on_sigpipe(int s) { (void)s; sigpipes++; }
static void die(const char *w) { perror(w); exit(2); }
static void settle(void) { usleep(30000); }
static char buf[1 << 20];

static const char *ename(int e)
{
    switch (e) {
    case 0: return "0";
    case EAGAIN: return "EAGAIN";
    case EPIPE: return "EPIPE";
    case ECONNRESET: return "ECONNRESET";
    case ENOTCONN: return "ENOTCONN";
    case EINVAL: return "EINVAL";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

static char *ans(long r, int e)
{
    static char pool[64][64];
    static int i;
    i = (i + 1) % 64;
    if (r < 0) snprintf(pool[i], 64, "-1 %s", ename(e));
    else snprintf(pool[i], 64, "%ld", r);
    return pool[i];
}

static void set_nb(int fd)
{
    int f = fcntl(fd, F_GETFL);
    if (fcntl(fd, F_SETFL, f | O_NONBLOCK) < 0) die("fcntl");
}

static void pair(int *c, int *p)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a = { .sin_family = AF_INET, .sin_addr.s_addr = htonl(INADDR_LOOPBACK) };
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, 1) < 0) die("listen");
    socklen_t al = sizeof a;
    getsockname(l, (struct sockaddr *)&a, &al);
    *c = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(*c, (struct sockaddr *)&a, sizeof a) < 0) die("connect");
    *p = accept(l, NULL, NULL);
    if (*p < 0) die("accept");
    close(l);
    set_nb(*c);
    set_nb(*p);
    settle();
}

static const char *do_read(int fd)
{
    long r = read(fd, buf, 4096);
    int e = errno;
    settle();
    return ans(r, e);
}

static const char *do_shutdown(int fd, int how)
{
    long r = shutdown(fd, how);
    int e = errno;
    settle();
    return ans(r, e);
}

static const char *do_soerr(int fd)
{
    int v = 0;
    socklen_t l = sizeof v;
    if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &v, &l) < 0) return ans(-1, errno);
    return ename(v);
}

static const char *do_write(int fd, int n)
{
    int before = sigpipes;
    long r = write(fd, buf, n);
    int e = errno;
    settle();
    static char b[64][96];
    static int i;
    i = (i + 1) % 64;
    snprintf(b[i], 96, "%s%s", ans(r, e), sigpipes != before ? "+SIGPIPE" : "");
    return b[i];
}

static void fill(int c)
{
    int dry = 0;
    while (dry < 3) {
        long r = write(c, buf, sizeof buf);
        if (r > 0) { dry = 0; continue; }
        dry++;
        settle();
    }
}

static const int hows[3] = { SHUT_RD, SHUT_WR, SHUT_RDWR };
static const char *hn[3] = { "RD", "WR", "RDWR" };

static void exchange(int c, int p, int cfirst)
{
    write(p, buf, 1);
    settle();
    if (cfirst) {
        shutdown(c, SHUT_WR);
        settle();
        shutdown(p, SHUT_WR);
        settle();
    } else {
        shutdown(p, SHUT_WR);
        settle();
        shutdown(c, SHUT_WR);
        settle();
    }
}

int main(void)
{
    signal(SIGPIPE, on_sigpipe);
    struct utsname u;
    uname(&u);
    printf("#\t%s %s %s\n", u.sysname, u.release, u.machine);

    for (int cfirst = 1; cfirst >= 0; cfirst--)
        for (int who = 0; who < 2; who++)
            for (int h = 0; h < 3; h++) {
                int c, p;
                pair(&c, &p);
                exchange(c, p, cfirst);
                const char *r = do_shutdown(who == 0 ? c : p, hows[h]);
                printf("K\t%s\t%s\t%s\tshutdown=%s\n", cfirst ? "c-first" : "p-first", who == 0 ? "c" : "p", hn[h], r);
                close(c);
                close(p);
            }

    for (int cfirst = 1; cfirst >= 0; cfirst--) {
        int c, p;
        pair(&c, &p);
        exchange(c, p, cfirst);
        close(c);
        settle();
        const char *r1 = do_read(p);
        const char *r2 = do_read(p);
        const char *e = do_soerr(p);
        printf("U\t%s\tp-read=%s,%s soerr(p)=%s\n", cfirst ? "c-first" : "p-first", r1, r2, e);
        close(p);
    }

    for (int soerr_first = 0; soerr_first < 2; soerr_first++) {
        int c, p;
        pair(&c, &p);
        fill(c);
        shutdown(c, SHUT_WR);
        settle();
        shutdown(p, SHUT_WR);
        settle();
        close(p);
        settle();
        if (soerr_first) {
            const char *e = do_soerr(c);
            const char *r1 = do_read(c);
            const char *r2 = do_read(c);
            printf("Q\tsoerr-first\tsoerr(c)=%s c-read=%s,%s\n", e, r1, r2);
        } else {
            const char *r1 = do_read(c);
            const char *r2 = do_read(c);
            const char *e = do_soerr(c);
            printf("Q\tread-first\tc-read=%s,%s soerr(c)=%s\n", r1, r2, e);
        }
        close(c);
    }
    const char *vname[5] = { "noshut-one", "one", "one-cfin", "full", "full-cfin" };
    for (int v = 0; v < 5; v++) {
        int c, p;
        pair(&c, &p);
        int full = v >= 3, cfin = v == 2 || v == 4;
        if (v != 0) {
            shutdown(p, SHUT_WR);
            settle();
        }
        if (full) fill(c);
        else {
            write(c, buf, 1);
            settle();
        }
        if (cfin) {
            shutdown(c, SHUT_WR);
            settle();
        }
        close(p);
        settle();
        const char *r1 = do_read(c);
        const char *r2 = do_read(c);
        const char *e1 = do_soerr(c);
        const char *w = do_write(c, 100);
        const char *e2 = do_soerr(c);
        printf("V\t%s\tc-read=%s,%s soerr(c)=%s c-write100=%s soerr(c)=%s\n", vname[v], r1, r2, e1, w, e2);
        close(c);
    }
    for (int v = 0; v < 2; v++) {
        int c, p;
        pair(&c, &p);
        if (v == 1) fill(c);
        else {
            write(c, buf, 1);
            settle();
        }
        shutdown(c, SHUT_WR);
        settle();
        close(p);
        settle();
        const char *r1 = do_read(c);
        const char *r2 = do_read(c);
        const char *e1 = do_soerr(c);
        const char *w = do_write(c, 100);
        const char *e2 = do_soerr(c);
        printf("W\t%s\tc-read=%s,%s soerr(c)=%s c-write100=%s soerr(c)=%s\n", v ? "full" : "one", r1, r2, e1, w, e2);
        close(c);
    }

    for (int v = 0; v < 2; v++) {
        int c, p;
        pair(&c, &p);
        if (v == 0) {
            shutdown(p, SHUT_WR);
            settle();
        }
        fill(c);
        shutdown(c, SHUT_WR);
        settle();
        close(p);
        settle();
        const char *r1 = do_read(c);
        const char *e1 = do_soerr(c);
        sleep(5);
        const char *r2 = do_read(c);
        const char *e2 = do_soerr(c);
        printf("T\t%s\tat once c-read=%s soerr(c)=%s; 5 s later c-read=%s soerr(c)=%s\n", v ? "W-full" : "V-full-cfin", r1, e1, r2, e2);
        close(c);
    }
    const char *fin[3] = { "none", "queued", "arrived" };
    for (int cf = 0; cf < 3; cf++)
        for (int pf = 0; pf < 3; pf++)
            for (int cfirst = 1; cfirst >= 0; cfirst--) {
                if ((cf == 0 || pf == 0) && !cfirst) continue;
                int c, p;
                pair(&c, &p);
                if (pf == 1) fill(p);
                else {
                    write(p, buf, 1);
                    settle();
                }
                if (cf == 1) fill(c);
                if (cfirst) {
                    if (cf) { shutdown(c, SHUT_WR); settle(); }
                    if (pf) { shutdown(p, SHUT_WR); settle(); }
                } else {
                    if (pf) { shutdown(p, SHUT_WR); settle(); }
                    if (cf) { shutdown(c, SHUT_WR); settle(); }
                }
                close(c);
                settle();
                const char *e1 = do_soerr(p);
                long got = 0;
                const char *last = "";
                for (;;) {
                    long r = read(p, buf, sizeof buf);
                    int e = errno;
                    if (r > 0) { got += r; continue; }
                    last = ans(r, e);
                    break;
                }
                settle();
                sleep(2);
                const char *e2 = do_soerr(p);
                const char *r2 = do_read(p);
                const char *mark = "";
#ifdef __APPLE__
                if (cf == 1 && pf >= 1) mark = "\t~timing";
#endif
                printf("G\tc-%s\tp-%s\t%s\tsoerr(p)=%s p-drained=%s last=%s; 2 s later soerr(p)=%s p-read=%s%s\n", fin[cf],
                       fin[pf], cfirst ? "c-first" : "p-first", e1, got ? "some" : "none", last, e2, r2, mark);
                close(p);
            }
    return 0;
}
