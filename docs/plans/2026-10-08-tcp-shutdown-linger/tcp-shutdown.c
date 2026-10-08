// shutdown(2), and close(2) under SO_LINGER, on loopback TCP sockets, on
// Linux and Darwin.
//
// c is the connecting socket and p the accepted one, both non-blocking unless
// a section says otherwise. SIGPIPE is caught and counted; SIGUSR1 (no
// SA_RESTART) is how a sleeping thread is ended when nothing else ends it.
// Each line is tab-separated, its first field the section. After every action
// the probe sleeps 30 ms, so that Darwin's loopback, which delivers on another
// thread, has settled.
//
// What a line reports:
//   rdy(x)  x's readiness, consuming nothing: poll's revents for
//           IN|OUT|PRI (and RDHUP on Linux); on Linux a level-triggered epoll
//           registration's events for IN|OUT|PRI|RDHUP; on Darwin a
//           level-triggered kqueue's EVFILT_READ and EVFILT_WRITE, each as
//           data/flags/fflags with "EOF" for EV_EOF, or "-" when not ready.
//           Then FIONREAD.
//   read    read(x, buf, 4096): the count, or the errno's name.
//   write   write(x, buf, n) with the number of SIGPIPEs it raised.
//   soerr   getsockopt(SO_ERROR), which takes the error.
//
// Sections:
//   S   shutdown(c, how) for how in RD, WR, RDWR, from each starting state:
//         idle      nothing queued either way;
//         cunread   p wrote 1000 bytes that c has not read;
//         cunsent   c wrote until EAGAIN (repeated until three tries 30 ms
//                   apart take nothing), so c has bytes p's receive buffer
//                   cannot take yet.
//       Then: the shutdown's answer; rdy(c) and rdy(p); c reads twice; p
//       reads; c writes 100; c writes 0; p writes 100; rdy(c), rdy(p), the
//       reads again, and soerr of both. In cunsent, p then drains everything
//       and reads once more.
//   T   shutdown twice: each how followed by each how, on a pair where p
//       has sent c 10 bytes, with rdy of both after; and after the peer's FIN (p closed) or reset (p closed
//       with 100 bytes from c unread), each how on c; and, with p having
//       called shutdown(WR) but still open, each how on c, then whether p
//       sees c's FIN.
//   U   shutdown on sockets that are not connected: fresh, bound, listening
//       (each how; then whether accept answers, whether a new client
//       connects, and whether listen succeeds again), connect refused, a
//       datagram socket, a non-socket, a closed descriptor, and how = 3 and
//       -1 on a fresh socket, a non-socket and a connected socket.
//   P   what the peer reads when c has unread bytes from p and then calls
//       shutdown(RDWR) and close; and SHUT_RD followed by p writing until
//       EAGAIN (capped at 16 MiB), with c's FIONREAD after.
//   R   shutdown(c, RD or RDWR), with or without c having sent 100 bytes
//       first; then p writes 100 bytes four times, 30 ms apart: rdy of both
//       after each, then c reads, p reads, and soerr of both. Then p-unsent:
//       p writes to c until EAGAIN, so bytes wait in p's send buffer, and c
//       shuts RD or RDWR: rdy of both; c reads 4096, and rdy of both; c
//       reads again, p writes 100, soerr of both.
//   E   edges: c and p registered edge-triggered (EPOLLET with
//       IN|OUT|RDHUP; EV_CLEAR on both kqueue filters), drained, then
//       shutdown(c, how): what each registration then reports.
//   B   a thread asleep, blocking, 100 ms in:
//         B-rd-<how>   read(c), and main calls shutdown(c, how);
//         B-prd-<how>  read(p), and main calls shutdown(c, how);
//         B-wr-<how>   write(c, 1 MiB) after c was filled to EAGAIN, and main
//                      calls shutdown(c, how);
//         B-wrpart-<how> write(c, 8 MiB) on a fresh pair, which takes some
//                      and sleeps; 200 ms in, main calls shutdown(c, how).
//         B-acc-<how>  accept(l), and main calls shutdown(l, how).
//       Whether it has returned 200 ms after, and with what; if not, an
//       unblocking step (p writes 1 byte, or c drains, or a client connects)
//       and then SIGUSR1, each followed by the same report.
//   L   close(c) with SO_LINGER {1, 0}:
//         idle, cunread, cunsent (as in S), pdata (p has 1000 bytes from c it
//         has not read), afterwr (c called shutdown(WR) first), afterfin (p
//         had called shutdown(WR) first; p still reads), cunsent-afterwr
//         (cunsent, then shutdown(c, WR), so c's FIN waits behind its bytes).
//       Then rdy(p); whether a fresh socket (no SO_REUSEADDR) binds c's
//       endpoint; p reads twice, p writes 100 twice, soerr(p), p reads; and,
//       30 ms later, the bind again.
//       Control: the same with no linger.
//   Q   a listener l with three connections queued and never accepted: q0
//       idle, q1 wrote 100 bytes, q2 closed. l is closed with linger off,
//       or with {1, 0}; or shutdown(l, RDWR) instead. Then, for q0 and q1,
//       rdy, read, write 100, soerr, read; and whether a fresh socket (no
//       SO_REUSEADDR) binds l's port.
//   G   close(c) with SO_LINGER {1, 1 s}: c has unsent bytes (as cunsent),
//       c blocking or non-blocking. The close's answer and how long it took;
//       then p drains everything, and its last read's answer. And the same
//       with nothing unsent.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -pthread -o /tmp/tcp-shutdown tcp-shutdown.c && /tmp/tcp-shutdown
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -pthread -o /tmp/p /probe/tcp-shutdown.c && /tmp/p'
// An argument restricts the run to the sections it names, e.g. `SL`; the
// default is `LSTUPREBQG`. Sockets a row has finished with are closed with
// SO_LINGER {1, 0}, and L runs first, so that no earlier row's TIME_WAIT
// shares a port with the endpoint L binds.
//
// Measured 2026-10-08 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), the whole probe twice
// on each (and L's "at once" binds in two more runs of L alone). One run of
// each is
// tcp-shutdown.linux-6.18.5-aarch64.txt and tcp-shutdown.darwin-27.0.txt.
// Every answer and readiness bit agreed between runs; byte counts did not.
// docs/plans/2026-10-08-tcp-shutdown-linger.md, section 2, says what they
// mean.

#ifdef __linux__
#define _GNU_SOURCE
#endif

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>

#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static atomic_int sigpipes;

static void on_sigpipe(int signo) { (void)signo; atomic_fetch_add(&sigpipes, 1); }
static void on_sigusr1(int signo) { (void)signo; }

static void die(const char *what) { perror(what); exit(2); }

static void settle(void) { usleep(30000); }

static long now_ms(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000L + t.tv_nsec / 1000000L;
}

static const char *ename(int e)
{
    switch (e) {
    case 0: return "0";
    case EAGAIN: return "EAGAIN";
    case EPIPE: return "EPIPE";
    case ECONNRESET: return "ECONNRESET";
    case ENOTCONN: return "ENOTCONN";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ENOTSOCK: return "ENOTSOCK";
    case ECONNREFUSED: return "ECONNREFUSED";
    case ECONNABORTED: return "ECONNABORTED";
    case EINTR: return "EINTR";
    case EADDRINUSE: return "EADDRINUSE";
    case EISCONN: return "EISCONN";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case EDOM: return "EDOM";
    case EINPROGRESS: return "EINPROGRESS";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

// The answer of a call returning -1/errno or a value.
static const char *ans(long r, int e)
{
    static char b[8][64];
    static int i;
    i = (i + 1) % 8;
    if (r < 0) snprintf(b[i], sizeof b[i], "-1 %s", ename(e));
    else snprintf(b[i], sizeof b[i], "%ld", r);
    return b[i];
}

static void set_nb(int fd, int on)
{
    int fl = fcntl(fd, F_GETFL);
    if (fcntl(fd, F_SETFL, on ? (fl | O_NONBLOCK) : (fl & ~O_NONBLOCK)) < 0) die("fcntl");
}

// Close a socket the probe has finished with, with SO_LINGER {1, 0}, so that
// it leaves no TIME_WAIT behind to collide with a later row's ephemeral port.
static void discard(int fd)
{
    struct linger lg = { .l_onoff = 1, .l_linger = 0 };
    setsockopt(fd, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
    close(fd);
}

static struct sockaddr_in loop_at(int port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(0x7f000001);
    return a;
}

static int port_of(int fd)
{
    struct sockaddr_in a;
    socklen_t l = sizeof a;
    if (getsockname(fd, (struct sockaddr *)&a, &l) < 0) return -1;
    return ntohs(a.sin_port);
}

static int listener(int backlog)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    if (l < 0) die("socket");
    struct sockaddr_in a = loop_at(0);
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, backlog) < 0) die("listen");
    return l;
}

static int connect_to(int port)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (c < 0) die("socket");
    struct sockaddr_in a = loop_at(port);
    if (connect(c, (struct sockaddr *)&a, sizeof a) < 0) die("connect");
    return c;
}

// A connected pair, both non-blocking, and the listener closed.
static void pair(int *c, int *p)
{
    int l = listener(4);
    *c = connect_to(port_of(l));
    *p = accept(l, NULL, NULL);
    if (*p < 0) die("accept");
    close(l);
    set_nb(*c, 1);
    set_nb(*p, 1);
}

// Whether a fresh socket, without SO_REUSEADDR, binds 127.0.0.1:port.
static const char *bind_free(int port)
{
    int n = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a = loop_at(port);
    int r = bind(n, (struct sockaddr *)&a, sizeof a);
    int e = errno;
    close(n);
    return r == 0 ? "bind-ok" : (e == EADDRINUSE ? "bind-EADDRINUSE" : ans(-1, e));
}

static char buf[1 << 20];

static const char *do_read(int fd)
{
    long r = read(fd, buf, 4096);
    return ans(r, errno);
}

static const char *do_write(int fd, int n)
{
    static char b[8][64];
    static int i;
    i = (i + 1) % 8;
    int before = atomic_load(&sigpipes);
    long r = write(fd, buf, n);
    int e = errno;
    int raised = atomic_load(&sigpipes) - before;
    snprintf(b[i], sizeof b[i], "%s%s", ans(r, e), raised ? "+SIGPIPE" : "");
    return b[i];
}

static const char *do_soerr(int fd)
{
    int v = 0;
    socklen_t l = sizeof v;
    if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &v, &l) < 0) return ans(-1, errno);
    return ename(v);
}

static int fionread(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) < 0) return -1;
    return n;
}

// Readiness, consuming nothing.
static const char *rdy(int fd)
{
    static char b[4][256];
    static int i;
    i = (i + 1) % 4;
    char *o = b[i];
    struct pollfd pf = { .fd = fd, .events = POLLIN | POLLOUT | POLLPRI
#ifdef __linux__
                                             | POLLRDHUP
#endif
    };
    poll(&pf, 1, 0);
    int len = snprintf(o, 256, "poll=0x%x", pf.revents);
#ifdef __linux__
    int ep = epoll_create1(0);
    struct epoll_event ev = { .events = EPOLLIN | EPOLLOUT | EPOLLPRI | EPOLLRDHUP, .data.fd = fd };
    epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev);
    struct epoll_event out;
    int n = epoll_wait(ep, &out, 1, 0);
    len += snprintf(o + len, 256 - len, " epoll=0x%x", n > 0 ? out.events : 0);
    close(ep);
#else
    int kq = kqueue();
    struct kevent ch[2];
    EV_SET(&ch[0], fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
    EV_SET(&ch[1], fd, EVFILT_WRITE, EV_ADD, 0, 0, NULL);
    kevent(kq, ch, 2, NULL, 0, NULL);
    struct kevent out[2];
    struct timespec zero = { 0, 0 };
    int n = kevent(kq, NULL, 0, out, 2, &zero);
    char r[64] = "-", w[64] = "-";
    for (int k = 0; k < n; k++) {
        char *t = out[k].filter == EVFILT_READ ? r : w;
        snprintf(t, 64, "%lld%s/%u", (long long)out[k].data, (out[k].flags & EV_EOF) ? "/EOF" : "", out[k].fflags);
    }
    len += snprintf(o + len, 256 - len, " kq-read=%s kq-write=%s", r, w);
    close(kq);
#endif
    snprintf(o + len, 256 - len, " fionread=%d", fionread(fd));
    return o;
}

// Write to c until three tries 30 ms apart take nothing; return the total.
static long fill(int c)
{
    long total = 0;
    int dry = 0;
    while (dry < 3 && total < (64L << 20)) {
        long r = write(c, buf, sizeof buf);
        if (r > 0) { total += r; dry = 0; continue; }
        if (errno != EAGAIN) { printf("#\tfill: %s\n", ans(r, errno)); break; }
        dry++;
        settle();
    }
    return total;
}

// Read everything p has, until EAGAIN or end; return the count and the last answer.
static long drain(int p, const char **last)
{
    long total = 0;
    int dry = 0;
    for (;;) {
        long r = read(p, buf, sizeof buf);
        if (r > 0) { total += r; dry = 0; continue; }
        if (r < 0 && errno == EAGAIN && dry < 3) { dry++; settle(); continue; }
        *last = ans(r, errno);
        return total;
    }
}

static const char *how_name(int how)
{
    switch (how) {
    case SHUT_RD: return "RD";
    case SHUT_WR: return "WR";
    case SHUT_RDWR: return "RDWR";
    default: return "?";
    }
}

static const int hows[3] = { SHUT_RD, SHUT_WR, SHUT_RDWR };

static const char *do_shutdown(int fd, int how)
{
    long r = shutdown(fd, how);
    return ans(r, errno);
}

// ---- S ----

enum start { IDLE, CUNREAD, CUNSENT };
static const char *start_name[] = { "idle", "cunread", "cunsent" };

static void prepare(enum start st, int c, int p)
{
    if (st == CUNREAD) { write(p, buf, 1000); settle(); }
    if (st == CUNSENT) { long n = fill(c); printf("#\tfilled %ld\n", n); }
}

static void section_s(void)
{
    for (int s = IDLE; s <= CUNSENT; s++)
        for (int h = 0; h < 3; h++) {
            int c, p;
            pair(&c, &p);
            prepare(s, c, p);
            const char *tag = start_name[s];
            const char *hn = how_name(hows[h]);
            printf("S\t%s\t%s\tshutdown=%s\n", tag, hn, do_shutdown(c, hows[h]));
            settle();
            printf("S\t%s\t%s\trdy(c)\t%s\n", tag, hn, rdy(c));
            printf("S\t%s\t%s\trdy(p)\t%s\n", tag, hn, rdy(p));
            const char *r1 = do_read(c);
            const char *r2 = do_read(c);
            printf("S\t%s\t%s\tc-read=%s,%s p-read=%s\n", tag, hn, r1, r2, do_read(p));
            printf("S\t%s\t%s\tc-write100=%s", tag, hn, do_write(c, 100));
            printf(" c-write0=%s", do_write(c, 0));
            printf(" p-write100=%s\n", do_write(p, 100));
            settle();
            printf("S\t%s\t%s\tafter rdy(c)\t%s\n", tag, hn, rdy(c));
            printf("S\t%s\t%s\tafter rdy(p)\t%s\n", tag, hn, rdy(p));
            r1 = do_read(c);
            r2 = do_read(c);
            printf("S\t%s\t%s\tafter c-read=%s,%s p-read=%s soerr(c)=%s soerr(p)=%s\n", tag, hn, r1, r2,
                   do_read(p), do_soerr(c), do_soerr(p));
            if (s == CUNSENT) {
                const char *last = "";
                long n = drain(p, &last);
                printf("S\t%s\t%s\tp-drained=%ld last=%s rdy(p)\t%s\n", tag, hn, n, last, rdy(p));
            }
            discard(c);
            discard(p);
        }
}

// ---- T ----

static void section_t(void)
{
    for (int a = 0; a < 3; a++)
        for (int b = 0; b < 3; b++) {
            int c, p;
            pair(&c, &p);
            write(p, buf, 10);
            settle();
            const char *first = do_shutdown(c, hows[a]);
            settle();
            const char *second = do_shutdown(c, hows[b]);
            settle();
            printf("T\tidle\t%s then %s\t%s,%s\trdy(c)\t%s\trdy(p)\t%s\n", how_name(hows[a]), how_name(hows[b]), first, second,
                   rdy(c), rdy(p));
            discard(c);
            discard(p);
        }
    for (int peer = 0; peer < 2; peer++)
        for (int h = 0; h < 3; h++) {
            int c, p;
            pair(&c, &p);
            if (peer == 1) { write(c, buf, 100); settle(); }
            close(p);
            settle();
            printf("T\t%s\t%s\t%s\tsoerr(c) after=%s\n", peer ? "peer-reset" : "peer-fin", how_name(hows[h]),
                   do_shutdown(c, hows[h]), do_soerr(c));
            discard(c);
        }
    // p has sent its FIN (shutdown WR, still open); then shutdown(c, how):
    // whether c's FIN reaches p.
    for (int h = 0; h < 3; h++) {
        int c, p;
        pair(&c, &p);
        shutdown(p, SHUT_WR);
        settle();
        const char *r = do_shutdown(c, hows[h]);
        settle();
        printf("T\tpeer-shut-wr\t%s\t%s\trdy(p)\t%s\trdy(c)\t%s", how_name(hows[h]), r, rdy(p), rdy(c));
        printf("\tp-read=%s c-write100=%s\n", do_read(p), do_write(c, 100));
        discard(c);
        discard(p);
    }
    // A reset whose error has been taken, then shutdown.
    for (int h = 0; h < 3; h++) {
        int c, p;
        pair(&c, &p);
        write(c, buf, 100);
        settle();
        close(p);
        settle();
        const char *e = do_soerr(c);
        printf("T\tpeer-reset-taken\t%s\t%s (soerr first %s)\n", how_name(hows[h]), do_shutdown(c, hows[h]), e);
        discard(c);
    }
}

// ---- U ----

static void section_u(void)
{
    for (int h = 0; h < 3; h++) {
        int f = socket(AF_INET, SOCK_STREAM, 0);
        printf("U\tfresh\t%s\t%s\n", how_name(hows[h]), do_shutdown(f, hows[h]));
        close(f);
        f = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in a = loop_at(0);
        bind(f, (struct sockaddr *)&a, sizeof a);
        printf("U\tbound\t%s\t%s\n", how_name(hows[h]), do_shutdown(f, hows[h]));
        close(f);
        f = socket(AF_INET, SOCK_DGRAM, 0);
        printf("U\tudp-unconnected\t%s\t%s\n", how_name(hows[h]), do_shutdown(f, hows[h]));
        close(f);
        f = open("/dev/null", O_RDONLY);
        printf("U\tnot-socket\t%s\t%s\n", how_name(hows[h]), do_shutdown(f, hows[h]));
        close(f);
        printf("U\tclosed-fd\t%s\t%s\n", how_name(hows[h]), do_shutdown(f, hows[h]));
    }
    // A listener: each how, with one connection queued.
    for (int h = 0; h < 3; h++) {
        int l = listener(4);
        int port = port_of(l);
        int q = connect_to(port);
        set_nb(q, 1);
        settle();
        printf("U\tlistener\t%s\tshutdown=%s\trdy(l)\t%s\n", how_name(hows[h]), do_shutdown(l, hows[h]), rdy(l));
        settle();
        printf("U\tlistener\t%s\tqueued client rdy\t%s\n", how_name(hows[h]), rdy(q));
        printf("U\tlistener\t%s\tqueued client read=%s soerr=%s\n", how_name(hows[h]), do_read(q), do_soerr(q));
        set_nb(l, 1);
        int acc = accept(l, NULL, NULL);
        printf("U\tlistener\t%s\taccept=%s port_kept=%d", how_name(hows[h]), ans(acc, errno), port_of(l) == port);
        if (acc >= 0) close(acc);
        int n = socket(AF_INET, SOCK_STREAM, 0);
        set_nb(n, 1);
        struct sockaddr_in a = loop_at(port);
        int r = connect(n, (struct sockaddr *)&a, sizeof a);
        int e = errno;
        settle();
        printf(" new-connect=%s soerr=%s", ans(r, e), do_soerr(n));
        close(n);
        printf(" relisten=%s", ans(listen(l, 4), errno));
        printf("\n");
        discard(q);
        close(l);
    }
    // A refused connect.
    for (int h = 0; h < 3; h++) {
        int l = listener(4);
        int port = port_of(l);
        close(l);
        int f = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in a = loop_at(port);
        int r = connect(f, (struct sockaddr *)&a, sizeof a);
        int e = errno;
        printf("U\trefused(%s)\t%s\t%s\n", ans(r, e), how_name(hows[h]), do_shutdown(f, hows[h]));
        close(f);
    }
    // A bad how on sockets that are not connected, and on a non-socket.
    for (int k = 0; k < 2; k++) {
        int bh = k == 0 ? 3 : -1;
        int f = socket(AF_INET, SOCK_STREAM, 0);
        printf("U\tfresh\thow=%d\t%s\n", bh, do_shutdown(f, bh));
        close(f);
        f = open("/dev/null", O_RDONLY);
        printf("U\tnot-socket\thow=%d\t%s\n", bh, do_shutdown(f, bh));
        close(f);
    }
    // A bad how on a connected socket.
    int bad[2] = { 3, -1 };
    for (int k = 0; k < 2; k++) {
        int c, p;
        pair(&c, &p);
        printf("U\tconnected\thow=%d\t%s\n", bad[k], do_shutdown(c, bad[k]));
        discard(c);
        discard(p);
    }
}

// ---- P ----

static void section_p(void)
{
    // c has 1000 unread bytes from p, then shutdown(RDWR), then close.
    {
        int c, p;
        pair(&c, &p);
        write(p, buf, 1000);
        settle();
        printf("P\tunread-rdwr\tshutdown=%s fionread(c)=%d\n", do_shutdown(c, SHUT_RDWR), fionread(c));
        settle();
        printf("P\tunread-rdwr\tbefore close rdy(p)\t%s\n", rdy(p));
        close(c);
        settle();
        printf("P\tunread-rdwr\tafter close rdy(p)\t%s\n", rdy(p));
        const char *r1 = do_read(p);
        const char *r2 = do_read(p);
        printf("P\tunread-rdwr\tp-read=%s,%s p-write100=%s", r1, r2, do_write(p, 100));
        printf(" p-write100=%s soerr(p)=%s\n", do_write(p, 100), do_soerr(p));
        discard(p);
    }
    // c: shutdown(RD) with 1000 unread, then close, as the same question without the WR half.
    {
        int c, p;
        pair(&c, &p);
        write(p, buf, 1000);
        settle();
        printf("P\tunread-rd\tshutdown=%s fionread(c)=%d\n", do_shutdown(c, SHUT_RD), fionread(c));
        close(c);
        settle();
        printf("P\tunread-rd\tafter close rdy(p)\t%s\n", rdy(p));
        const char *r1 = do_read(p);
        printf("P\tunread-rd\tp-read=%s p-write100=%s soerr(p)=%s\n", r1, do_write(p, 100), do_soerr(p));
        discard(p);
    }
    // SHUT_RD, then p writes until EAGAIN (capped).
    {
        int c, p;
        pair(&c, &p);
        printf("P\trd-then-fill\tshutdown=%s\n", do_shutdown(c, SHUT_RD));
        long total = 0;
        int dry = 0;
        while (total < (16L << 20) && dry < 3) {
            long r = write(p, buf, 65536);
            if (r > 0) { total += r; dry = 0; continue; }
            if (errno != EAGAIN) { printf("P\trd-then-fill\twrite %s\n", ans(r, errno)); break; }
            dry++;
            settle();
        }
        settle();
        printf("P\trd-then-fill\tp-took=%ld fionread(c)=%d rdy(c)\t%s\n", total, fionread(c), rdy(c));
        printf("P\trd-then-fill\tc-read=%s\n", do_read(c));
        discard(c);
        discard(p);
    }
}

// ---- R ----

// Data arriving after shutdown(c, RD or RDWR), one write of 100 bytes at a
// time: whether c keeps it, and whether c's kernel resets the connection.
static void section_r(void)
{
    for (int h = 0; h < 3; h += 2)
        for (int sent = 0; sent < 2; sent++) {
            int c, p;
            pair(&c, &p);
            if (sent) { write(c, buf, 100); settle(); read(p, buf, 4096); }
            const char *r = do_shutdown(c, hows[h]);
            settle();
            printf("R\t%s\tc-sent-first=%d\tshutdown=%s\trdy(c)\t%s\n", how_name(hows[h]), sent, r, rdy(c));
            for (int k = 1; k <= 4; k++) {
                const char *w = do_write(p, 100);
                settle();
                printf("R\t%s\tc-sent-first=%d\tp-write#%d=%s\trdy(c)\t%s\trdy(p)\t%s\n", how_name(hows[h]), sent, k, w,
                       rdy(c), rdy(p));
            }
            const char *r1 = do_read(c);
            printf("R\t%s\tc-sent-first=%d\tc-read=%s p-read=%s soerr(c)=%s soerr(p)=%s\n", how_name(hows[h]), sent, r1,
                   do_read(p), do_soerr(c), do_soerr(p));
            discard(c);
            discard(p);
        }
    // p filled c's receive buffer and has bytes left in its own; then c shuts RD.
    for (int h = 0; h < 3; h += 2) {
        int c, p;
        pair(&c, &p);
        long n = fill(p);
        const char *r = do_shutdown(c, hows[h]);
        settle();
        printf("R\t%s\tp-unsent(%ld)\tshutdown=%s\trdy(c)\t%s\trdy(p)\t%s\n", how_name(hows[h]), n, r, rdy(c), rdy(p));
        const char *r1 = do_read(c);
        settle();
        printf("R\t%s\tp-unsent\tc-read=%s\trdy(c)\t%s\trdy(p)\t%s\n", how_name(hows[h]), r1, rdy(c), rdy(p));
        r1 = do_read(c);
        printf("R\t%s\tp-unsent\tc-read=%s p-write100=%s soerr(c)=%s soerr(p)=%s\n", how_name(hows[h]), r1, do_write(p, 100),
               do_soerr(c), do_soerr(p));
        discard(c);
        discard(p);
    }
}

// ---- E ----

#ifdef __linux__
static void edges(const char *tag, int c, int p, int ep)
{
    struct epoll_event out[4];
    int n = epoll_wait(ep, out, 4, 0);
    char cs[32] = "-", ps[32] = "-";
    for (int k = 0; k < n; k++) snprintf(out[k].data.fd == c ? cs : ps, 32, "0x%x", out[k].events);
    printf("E\t%s\tc=%s p=%s\n", tag, cs, ps);
}
#else
static void edges(const char *tag, int c, int p, int kq)
{
    struct kevent out[4];
    struct timespec zero = { 0, 0 };
    int n = kevent(kq, NULL, 0, out, 4, &zero);
    char s[256] = "";
    int len = 0;
    for (int k = 0; k < n; k++)
        len += snprintf(s + len, sizeof s - len, " %s-%s=%lld%s/%u", (int)out[k].ident == c ? "c" : "p",
                        out[k].filter == EVFILT_READ ? "read" : "write", (long long)out[k].data,
                        (out[k].flags & EV_EOF) ? "/EOF" : "", out[k].fflags);
    printf("E\t%s\t%s\n", tag, n ? s + 1 : "-");
}
#endif

static int edge_port(int c, int p)
{
#ifdef __linux__
    int ep = epoll_create1(0);
    struct epoll_event ev = { .events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET };
    ev.data.fd = c;
    epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev);
    ev.data.fd = p;
    epoll_ctl(ep, EPOLL_CTL_ADD, p, &ev);
#else
    int ep = kqueue();
    struct kevent ch[4];
    EV_SET(&ch[0], c, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
    EV_SET(&ch[1], c, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
    EV_SET(&ch[2], p, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
    EV_SET(&ch[3], p, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
    kevent(ep, ch, 4, NULL, 0, NULL);
#endif
    return ep;
}

static void section_e(void)
{
    for (int s = IDLE; s <= CUNSENT; s++)
        for (int h = 0; h < 3; h++) {
            int c, p;
            pair(&c, &p);
            prepare(s, c, p);
            int ep = edge_port(c, p);
            char tag[64];
            snprintf(tag, sizeof tag, "%s\t%s\tdrain", start_name[s], how_name(hows[h]));
            edges(tag, c, p, ep);
            snprintf(tag, sizeof tag, "%s\t%s\tagain", start_name[s], how_name(hows[h]));
            edges(tag, c, p, ep);
            shutdown(c, hows[h]);
            settle();
            snprintf(tag, sizeof tag, "%s\t%s\tafter", start_name[s], how_name(hows[h]));
            edges(tag, c, p, ep);
            close(ep);
            discard(c);
            discard(p);
        }
    // Listener: an edge registration on l, then shutdown(l, how).
    for (int h = 0; h < 3; h++) {
        int l = listener(4);
#ifdef __linux__
        int ep = epoll_create1(0);
        struct epoll_event ev = { .events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET, .data.fd = l };
        epoll_ctl(ep, EPOLL_CTL_ADD, l, &ev);
        struct epoll_event out;
        int n0 = epoll_wait(ep, &out, 1, 0);
        printf("E\tlistener\t%s\tdrain n=%d", how_name(hows[h]), n0);
        const char *r = do_shutdown(l, hows[h]);
        settle();
        int n = epoll_wait(ep, &out, 1, 0);
        printf(" shutdown=%s after=0x%x\n", r, n > 0 ? out.events : 0);
#else
        int ep = kqueue();
        struct kevent ch;
        EV_SET(&ch, l, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
        kevent(ep, &ch, 1, NULL, 0, NULL);
        struct kevent out;
        struct timespec zero = { 0, 0 };
        int n0 = kevent(ep, NULL, 0, &out, 1, &zero);
        printf("E\tlistener\t%s\tdrain n=%d", how_name(hows[h]), n0);
        const char *r = do_shutdown(l, hows[h]);
        settle();
        int n = kevent(ep, NULL, 0, &out, 1, &zero);
        if (n > 0)
            printf(" shutdown=%s after=read %lld%s/%u\n", r, (long long)out.data, (out.flags & EV_EOF) ? "/EOF" : "",
                   out.fflags);
        else
            printf(" shutdown=%s after=-\n", r);
#endif
        close(ep);
        close(l);
    }
}

// ---- B ----

struct sleeper {
    int kind; // 0 read, 1 write, 2 accept, 3 write of 8 MiB
    int fd;
    long len;
    atomic_int done;
    long result;
    int err;
    int pipes;
};

static void *sleeper_main(void *arg)
{
    struct sleeper *s = arg;
    static char wbuf[1 << 20];
    int before = atomic_load(&sigpipes);
    long r;
    if (s->kind == 0) r = read(s->fd, buf, 4096);
    else if (s->kind == 1) r = write(s->fd, wbuf, s->len);
    else if (s->kind == 3) { static char big[8 << 20]; r = write(s->fd, big, s->len); }
    else r = accept(s->fd, NULL, NULL);
    s->err = errno;
    s->pipes = atomic_load(&sigpipes) - before;
    s->result = r;
    atomic_store(&s->done, 1);
    return NULL;
}

// SIGPIPE is counted process-wide from just before the step, because Darwin
// directs it at the process, so another thread may take it.
static int pipes_before_step;

static void report(const char *tag, const char *step, struct sleeper *s)
{
    for (int k = 0; k < 20 && !atomic_load(&s->done); k++) usleep(10000);
    int pipes = atomic_load(&sigpipes) - pipes_before_step;
    if (atomic_load(&s->done))
        printf("B\t%s\t%s\treturned %s%s\n", tag, step, ans(s->result, s->err), pipes ? "+SIGPIPE" : "");
    else
        printf("B\t%s\t%s\tasleep%s\n", tag, step, pipes ? " +SIGPIPE" : "");
    pipes_before_step = atomic_load(&sigpipes);
}

static void section_b(void)
{
    // read(c) / read(p) asleep, shutdown(c, how).
    for (int side = 0; side < 2; side++)
        for (int h = 0; h < 3; h++) {
            int c, p;
            pair(&c, &p);
            int target = side == 0 ? c : p;
            set_nb(target, 0);
            struct sleeper s = { .kind = 0, .fd = target };
            pthread_t t;
            pthread_create(&t, NULL, sleeper_main, &s);
            usleep(100000);
            char tag[32];
            snprintf(tag, sizeof tag, "%s-%s", side == 0 ? "rd" : "prd", how_name(hows[h]));
            pipes_before_step = atomic_load(&sigpipes);
            const char *r = do_shutdown(c, hows[h]);
            char step[64];
            snprintf(step, sizeof step, "shutdown=%s", r);
            report(tag, step, &s);
            if (!atomic_load(&s.done)) {
                if (side == 0) write(p, buf, 1); else write(c, buf, 1);
                report(tag, side == 0 ? "p-writes-1" : "c-writes-1", &s);
            }
            if (!atomic_load(&s.done)) { pthread_kill(t, SIGUSR1); report(tag, "SIGUSR1", &s); }
            pthread_join(t, NULL);
            discard(c);
            discard(p);
        }
    // write(c) asleep on a full buffer, shutdown(c, how).
    for (int h = 0; h < 3; h++) {
        int c, p;
        pair(&c, &p);
        long filled = fill(c);
        set_nb(c, 0);
        struct sleeper s = { .kind = 1, .fd = c, .len = 1 << 20 };
        pthread_t t;
        pthread_create(&t, NULL, sleeper_main, &s);
        usleep(100000);
        char tag[32];
        snprintf(tag, sizeof tag, "wr-%s", how_name(hows[h]));
        printf("#\tfilled %ld\n", filled);
        pipes_before_step = atomic_load(&sigpipes);
        const char *r = do_shutdown(c, hows[h]);
        char step[64];
        snprintf(step, sizeof step, "shutdown=%s", r);
        report(tag, step, &s);
        if (!atomic_load(&s.done)) {
            const char *last = "";
            long n = drain(p, &last);
            char d[64];
            snprintf(d, sizeof d, "p-drains(%ld,%s)", n, last);
            report(tag, d, &s);
        }
        if (!atomic_load(&s.done)) { pthread_kill(t, SIGUSR1); report(tag, "SIGUSR1", &s); }
        pthread_join(t, NULL);
        printf("B\t%s\tsoerr(c)=%s\n", tag, do_soerr(c));
        discard(c);
        discard(p);
    }
    // write(c, 8 MiB) asleep having taken some, shutdown(c, how).
    for (int h = 0; h < 3; h++) {
        int c, p;
        pair(&c, &p);
        set_nb(c, 0);
        struct sleeper s = { .kind = 3, .fd = c, .len = 8 << 20 };
        pthread_t t;
        pthread_create(&t, NULL, sleeper_main, &s);
        usleep(200000);
        char tag[32];
        snprintf(tag, sizeof tag, "wrpart-%s", how_name(hows[h]));
        pipes_before_step = atomic_load(&sigpipes);
        const char *r = do_shutdown(c, hows[h]);
        char step[64];
        snprintf(step, sizeof step, "shutdown=%s", r);
        report(tag, step, &s);
        if (!atomic_load(&s.done)) {
            const char *last = "";
            long n = drain(p, &last);
            char d[64];
            snprintf(d, sizeof d, "p-drains(%ld,%s)", n, last);
            report(tag, d, &s);
        }
        if (!atomic_load(&s.done)) { pthread_kill(t, SIGUSR1); report(tag, "SIGUSR1", &s); }
        pthread_join(t, NULL);
        const char *last = "";
        long n = drain(p, &last);
        printf("B\t%s\tp-drained-after=%ld last=%s soerr(c)=%s\n", tag, n, last, do_soerr(c));
        discard(c);
        discard(p);
    }
    // accept(l) asleep, shutdown(l, how).
    for (int h = 0; h < 3; h++) {
        int l = listener(4);
        int port = port_of(l);
        struct sleeper s = { .kind = 2, .fd = l };
        pthread_t t;
        pthread_create(&t, NULL, sleeper_main, &s);
        usleep(100000);
        char tag[32];
        snprintf(tag, sizeof tag, "acc-%s", how_name(hows[h]));
        pipes_before_step = atomic_load(&sigpipes);
        const char *r = do_shutdown(l, hows[h]);
        char step[64];
        snprintf(step, sizeof step, "shutdown=%s", r);
        report(tag, step, &s);
        int q = -1;
        if (!atomic_load(&s.done)) {
            q = socket(AF_INET, SOCK_STREAM, 0);
            set_nb(q, 1);
            struct sockaddr_in a = loop_at(port);
            int cr = connect(q, (struct sockaddr *)&a, sizeof a);
            int ce = errno;
            char st[64];
            snprintf(st, sizeof st, "client-connects(%s)", ans(cr, ce));
            report(tag, st, &s);
        }
        if (!atomic_load(&s.done)) { pthread_kill(t, SIGUSR1); report(tag, "SIGUSR1", &s); }
        pthread_join(t, NULL);
        if (s.result >= 0 && s.kind == 2) close((int)s.result);
        if (q >= 0) discard(q);
        close(l);
    }
}

// ---- L ----

static void set_linger(int fd, int on, int secs)
{
    struct linger lg = { .l_onoff = on, .l_linger = secs };
#ifdef __APPLE__
    if (setsockopt(fd, SOL_SOCKET, SO_LINGER_SEC, &lg, sizeof lg) < 0) die("SO_LINGER_SEC");
#else
    if (setsockopt(fd, SOL_SOCKET, SO_LINGER, &lg, sizeof lg) < 0) die("SO_LINGER");
#endif
}

enum lstart { L_IDLE, L_CUNREAD, L_CUNSENT, L_PDATA, L_AFTERWR, L_AFTERFIN, L_CUNSENT_AFTERWR };
static const char *lstart_name[] = { "idle", "cunread", "cunsent", "pdata", "afterwr", "afterfin", "cunsent-afterwr" };

static void section_l(void)
{
    for (int lin = 1; lin >= 0; lin--)
        for (int s = L_IDLE; s <= L_CUNSENT_AFTERWR; s++) {
            int c, p;
            pair(&c, &p);
            int cport = port_of(c);
            if (s == L_CUNREAD) { write(p, buf, 1000); settle(); }
            if (s == L_CUNSENT) printf("#\tfilled %ld\n", fill(c));
            if (s == L_PDATA) { write(c, buf, 1000); settle(); }
            if (s == L_AFTERWR) { shutdown(c, SHUT_WR); settle(); }
            if (s == L_AFTERFIN) { shutdown(p, SHUT_WR); settle(); }
            if (s == L_CUNSENT_AFTERWR) { printf("#\tfilled %ld\n", fill(c)); shutdown(c, SHUT_WR); settle(); }
            if (lin) set_linger(c, 1, 0);
            const char *tag = lstart_name[s];
            const char *lt = lin ? "linger0" : "nolinger";
            long r = close(c);
            int e = errno;
            settle();
            printf("L\t%s\t%s\tclose=%s rdy(p)\t%s\n", lt, tag, ans(r, e), rdy(p));
            printf("L\t%s\t%s\tclosers-endpoint at once %s\n", lt, tag, bind_free(cport));
            const char *r1 = do_read(p);
            const char *r2 = do_read(p);
            printf("L\t%s\t%s\tp-read=%s,%s", lt, tag, r1, r2);
            printf(" p-write100=%s", do_write(p, 100));
            printf(" p-write100=%s", do_write(p, 100));
            printf(" soerr(p)=%s", do_soerr(p));
            r1 = do_read(p);
            printf(" p-read=%s\n", r1);
            if (s == L_CUNSENT || s == L_PDATA || s == L_CUNSENT_AFTERWR) {
                const char *last = "";
                long n = drain(p, &last);
                printf("L\t%s\t%s\tp-drained=%ld last=%s\n", lt, tag, n, last);
            }
            settle();
            printf("L\t%s\t%s\tclosers-endpoint after p acted %s\n", lt, tag, bind_free(cport));
            discard(p);
        }
}

// ---- Q ----

static void section_q(void)
{
    const char *modes[] = { "close-nolinger", "close-linger0", "shutdown-rdwr" };
    for (int m = 0; m < 3; m++) {
        int l = listener(8);
        int port = port_of(l);
        int q0 = connect_to(port), q1 = connect_to(port), q2 = connect_to(port);
        set_nb(q0, 1);
        set_nb(q1, 1);
        write(q1, buf, 100);
        close(q2);
        settle();
        long r;
        if (m == 1) set_linger(l, 1, 0);
        if (m == 2) r = shutdown(l, SHUT_RDWR); else r = close(l);
        int e = errno;
        settle();
        printf("Q\t%s\t%s\n", modes[m], ans(r, e));
        int qs[2] = { q0, q1 };
        for (int k = 0; k < 2; k++) {
            printf("Q\t%s\tq%d rdy\t%s\n", modes[m], k, rdy(qs[k]));
            const char *r1 = do_read(qs[k]);
            printf("Q\t%s\tq%d read=%s write100=%s", modes[m], k, r1, do_write(qs[k], 100));
            printf(" soerr=%s", do_soerr(qs[k]));
            printf(" read=%s\n", do_read(qs[k]));
        }
        if (m == 2) close(l);
        settle();
        printf("Q\t%s\tlisteners-port %s\n", modes[m], bind_free(port));
        discard(q0);
        discard(q1);
    }
}

// ---- G ----

static void section_g(void)
{
    for (int unsent = 1; unsent >= 0; unsent--)
        for (int blocking = 0; blocking < 2; blocking++) {
            int c, p;
            pair(&c, &p);
            if (unsent) printf("#\tfilled %ld\n", fill(c));
            set_linger(c, 1, 1);
            if (blocking) set_nb(c, 0);
            long t0 = now_ms();
            long r = close(c);
            int e = errno;
            long dt = now_ms() - t0;
            settle();
            printf("G\t%s\t%s\tclose=%s after %ld ms rdy(p)\t%s\n", unsent ? "unsent" : "nothing", blocking ? "blocking" : "nonblocking",
                   ans(r, e), dt, rdy(p));
            const char *last = "";
            long n = drain(p, &last);
            printf("G\t%s\t%s\tp-drained=%ld last=%s soerr(p)=%s\n", unsent ? "unsent" : "nothing", blocking ? "blocking" : "nonblocking",
                   n, last, do_soerr(p));
            discard(p);
        }
}

int main(int argc, char **argv)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_sigpipe;
    sigaction(SIGPIPE, &sa, NULL);
    sa.sa_handler = on_sigusr1;
    sigaction(SIGUSR1, &sa, NULL);
    setvbuf(stdout, NULL, _IOLBF, 0);

    struct utsname u;
    uname(&u);
    printf("#\t%s %s %s\n", u.sysname, u.release, u.machine);

    const char *which = argc > 1 ? argv[1] : "LSTUPREBQG";
    for (const char *w = which; *w; w++) {
        switch (*w) {
        case 'S': section_s(); break;
        case 'T': section_t(); break;
        case 'U': section_u(); break;
        case 'P': section_p(); break;
        case 'R': section_r(); break;
        case 'E': section_e(); break;
        case 'B': section_b(); break;
        case 'L': section_l(); break;
        case 'Q': section_q(); break;
        case 'G': section_g(); break;
        }
    }
    return 0;
}
