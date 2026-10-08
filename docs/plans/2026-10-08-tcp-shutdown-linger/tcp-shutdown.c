// shutdown(2), and close(2) under SO_LINGER, on loopback TCP sockets, on
// Linux and Darwin.
//
// c is the connecting socket and p the accepted one, both non-blocking unless
// a section says otherwise. SIGPIPE is caught and counted; SIGUSR1 (no
// SA_RESTART) is how a sleeping thread is ended when nothing else ends it.
// Each line is tab-separated, its first field the section.
//
// Two rules keep the order of events the order in the source:
//   - Every call that touches a socket is a statement of its own, before the
//     printf that reports it. None is an argument to another call, so the
//     compiler cannot reorder, say, a read that takes a pending error and an
//     SO_ERROR that would otherwise take it.
//   - Every call that can put a segment on the wire (read, write, shutdown,
//     close, connect) is followed by a 30 ms sleep (`settle`), because
//     Darwin's loopback delivers on another thread, and a reset its kernel
//     sends in answer arrives after the call has returned.
//
// A line ending in `~counts` carries byte counts (FIONREAD, kqueue data, a
// fill's or a drain's total, a duration) that depend on timing: compare its
// answers and readiness bits, not its numbers. A line ending in `~timing`
// has an outcome that waits on a TCP timer or on when the kernel advertises
// a window, or comes from a state the model refuses; it is not to be
// replayed.
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
//   bind    whether a fresh socket, without SO_REUSEADDR, binds an endpoint.
//
// Sections:
//   L   close(c) with SO_LINGER {1, 0}:
//         idle, cunread, cunsent (as in S), pdata (p has 1000 bytes from c it
//         has not read), afterwr (c called shutdown(WR) first), afterfin (p
//         had called shutdown(WR) first; p still reads), cunsent-afterwr
//         (cunsent, then shutdown(c, WR), so c's FIN waits behind its bytes),
//         bothfin-cfirst (c shutdown(WR), then p shutdown(WR): both FINs
//         have arrived), bothfin-pfirst (the same, p first), cqueued-pfin
//         (cunsent-afterwr, then p shutdown(WR): p's FIN has arrived and
//         c's waits behind c's bytes), pfin-cqueued (p shutdown(WR), then
//         c fills and shuts writing: the same, with p's FIN first).
//       The answer of setting the linger (early, before the FINs, in the
//       last four rows; and just before the close in every row). Then
//       rdy(p); whether c's endpoint binds; p reads twice, p writes 100
//       twice, soerr(p), p reads; in the rows with bytes unsent, p drains
//       everything; and the bind again.
//       Control: the same with no linger.
//   F   which FIN went first, and whether c's endpoint binds once c has
//       closed while p stays open:
//         p-first-close   p shutdown(WR); c closes (c's FIN after p's);
//         p-first-wr      p shutdown(WR); c shutdown(WR); c closes;
//         p-first-queued-close  p shutdown(WR); c fills (as cunsent); c
//                         closes, so its FIN waits behind its bytes; the
//                         bind is tried, then p drains everything;
//         p-first-queued-wr     the same, with c's shutdown(WR) before its
//                         close;
//         c-first-close   c closes; p shutdown(WR);
//         c-first-wr      c shutdown(WR); p shutdown(WR); c closes;
//         c-queued        c fills (as cunsent); c shutdown(WR), so its FIN
//                         waits behind its bytes; p shutdown(WR); p drains
//                         everything; c closes.
//       The bind is tried at once and 30 ms later, then rdy(p) (in the
//       queued rows, both before and after p drains).
//   X   c fills; c shutdown(WR), so its FIN is queued; p shutdown(RDWR);
//       rdy of both; p drains everything, so c's bytes can arrive at a
//       socket shut both ways; rdy of both; then soerr(c), c reads, c
//       writes 100, soerr(p), p reads. Then rdy of both at intervals up to
//       15 s after, and at the end soerr and a read of both.
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
//       has sent c 10 bytes, with rdy of both after; and after the peer's
//       FIN (p closed) or reset (p closed with 100 bytes from c unread), each
//       how on c; and, with p having called shutdown(WR) but still open,
//       each how on c, then whether p sees c's FIN.
//   U   shutdown on sockets that are not connected: fresh, bound, listening
//       (each how; then whether accept answers, whether a new client
//       connects, and whether listen succeeds again), connect refused, a
//       datagram socket, a non-socket, a closed descriptor, and how = 3 and
//       -1 on a fresh socket, a non-socket and a connected socket.
//   P   what the peer reads when c has unread bytes from p and then calls
//       shutdown(RDWR) and close; and SHUT_RD followed by p writing until
//       EAGAIN (capped at 16 MiB), with c's FIONREAD after.
//   R   shutdown(c, RD or RDWR), with or without c having sent 100 bytes
//       first; then p writes 100 bytes four times: rdy of both after each,
//       then c reads, p reads, and soerr of both. Then p-unsent: p writes to
//       c until EAGAIN, so bytes wait in p's send buffer, and c shuts RD or
//       RDWR: rdy of both; c reads 4096, and rdy of both; c reads again, p
//       writes 100, soerr of both; after RD, 5 s later, rdy and soerr of both.
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
//   Q   a listener l with three connections queued and never accepted: q0
//       idle, q1 wrote 100 bytes, q2 closed. l is closed with linger off,
//       or with {1, 0}; or shutdown(l, RDWR) instead. Then, for q0 and q1,
//       rdy, read, write 100, soerr, read; and whether l's port binds.
//   G   close(c) with SO_LINGER {1, 1 s}: c has unsent bytes (as cunsent),
//       c blocking or non-blocking. The close's answer and how long it took;
//       then p drains everything, and its last read's answer. And the same
//       with nothing unsent.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -pthread -o /tmp/tcp-shutdown tcp-shutdown.c && /tmp/tcp-shutdown
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -pthread -o /tmp/p /probe/tcp-shutdown.c && /tmp/p'
// An argument restricts the run to the sections it names, e.g. `SL`; the
// default is `LFXSTUPREBQG`. Sockets a row has finished with are closed with
// SO_LINGER {1, 0}, and L and F run first, so that no earlier row's
// TIME_WAIT shares a port with an endpoint they bind. Within F, the rows
// expected to leave no TIME_WAIT run before those expected to leave one.
//
// Measured 2026-10-08 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), the whole probe twice
// on each, and L, F and X three more times each. One run of each is
// tcp-shutdown.linux-6.18.5-aarch64.txt and tcp-shutdown.darwin-27.0.txt.
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

// A rotating pool of result strings, large enough that every string one
// printf reports is still intact when it runs.
static char *slot(void)
{
    static char pool[256][256];
    static int i;
    i = (i + 1) % 256;
    return pool[i];
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
    default: {
        char *b = slot();
        snprintf(b, 256, "errno%d", e);
        return b;
    }
    }
}

// The answer of a call returning -1/errno or a value.
static const char *ans(long r, int e)
{
    char *b = slot();
    if (r < 0) snprintf(b, 256, "-1 %s", ename(e));
    else snprintf(b, 256, "%ld", r);
    return b;
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
    settle();
    return c;
}

// A connected pair, both non-blocking, and the listener closed.
static void pair(int *c, int *p)
{
    int l = listener(4);
    int port = port_of(l);
    *c = connect_to(port);
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
    if (r == 0) return "bind-ok";
    if (e == EADDRINUSE) return "bind-EADDRINUSE";
    return ans(-1, e);
}

static char buf[1 << 20];

// read, then settle: a read can open the receive window.
static const char *do_read(int fd)
{
    long r = read(fd, buf, 4096);
    int e = errno;
    settle();
    return ans(r, e);
}

// write, then settle; the SIGPIPEs it raised are counted until the settle ends.
static const char *do_write(int fd, int n)
{
    int before = atomic_load(&sigpipes);
    long r = write(fd, buf, n);
    int e = errno;
    settle();
    int raised = atomic_load(&sigpipes) - before;
    char *b = slot();
    snprintf(b, 256, "%s%s", ans(r, e), raised ? "+SIGPIPE" : "");
    return b;
}

static const char *do_shutdown(int fd, int how)
{
    long r = shutdown(fd, how);
    int e = errno;
    settle();
    return ans(r, e);
}

static const char *do_close(int fd)
{
    long r = close(fd);
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

static int fionread(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) < 0) return -1;
    return n;
}

// Readiness, consuming nothing.
static const char *rdy(int fd)
{
    char *o = slot();
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
    int queued = fionread(fd);
    snprintf(o + len, 256 - len, " fionread=%d", queued);
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
        int e = errno;
        if (e != EAGAIN) {
            const char *a = ans(r, e);
            printf("#\tfill: %s\n", a);
            break;
        }
        dry++;
        settle();
    }
    return total;
}

// Read everything p has, until a read answers anything but bytes (EAGAIN
// three times 30 ms apart, end of file, or an error); return the count and
// that last answer.
static long drain(int p, const char **last)
{
    long total = 0;
    int dry = 0;
    for (;;) {
        long r = read(p, buf, sizeof buf);
        int e = errno;
        if (r > 0) { total += r; dry = 0; continue; }
        if (r < 0 && e == EAGAIN && dry < 3) { dry++; settle(); continue; }
        *last = ans(r, e);
        settle();
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

// setsockopt(SO_LINGER), answering rather than failing: Darwin refuses it
// (EINVAL) once both directions are shut.
static const char *try_linger(int fd, int on, int secs)
{
    struct linger lg = { .l_onoff = on, .l_linger = secs };
#ifdef __APPLE__
    int r = setsockopt(fd, SOL_SOCKET, SO_LINGER_SEC, &lg, sizeof lg);
#else
    int r = setsockopt(fd, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
#endif
    int e = errno;
    return ans(r, e);
}

static void set_linger(int fd, int on, int secs)
{
    struct linger lg = { .l_onoff = on, .l_linger = secs };
#ifdef __APPLE__
    if (setsockopt(fd, SOL_SOCKET, SO_LINGER_SEC, &lg, sizeof lg) < 0) die("SO_LINGER_SEC");
#else
    if (setsockopt(fd, SOL_SOCKET, SO_LINGER, &lg, sizeof lg) < 0) die("SO_LINGER");
#endif
}

// ---- S ----

enum start { IDLE, CUNREAD, CUNSENT };
static const char *start_name[] = { "idle", "cunread", "cunsent" };

static void prepare(enum start st, int c, int p)
{
    if (st == CUNREAD) {
        write(p, buf, 1000);
        settle();
    }
    if (st == CUNSENT) {
        long n = fill(c);
        printf("#\tfilled %ld\n", n);
    }
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
            const char *mark = s == CUNSENT ? "\t~counts" : "";
            const char *sh = do_shutdown(c, hows[h]);
            printf("S\t%s\t%s\tshutdown=%s\n", tag, hn, sh);
            const char *rc = rdy(c);
            printf("S\t%s\t%s\trdy(c)\t%s%s\n", tag, hn, rc, mark);
            const char *rp = rdy(p);
            printf("S\t%s\t%s\trdy(p)\t%s%s\n", tag, hn, rp, mark);
            const char *r1 = do_read(c);
            const char *r2 = do_read(c);
            const char *r3 = do_read(p);
            printf("S\t%s\t%s\tc-read=%s,%s p-read=%s\n", tag, hn, r1, r2, r3);
            const char *w1 = do_write(c, 100);
            const char *w2 = do_write(c, 0);
            const char *w3 = do_write(p, 100);
            // From cunsent, whether c's write finds room depends on how far
            // Darwin's loopback thread has drained c's send buffer.
            printf("S\t%s\t%s\tc-write100=%s c-write0=%s p-write100=%s%s\n", tag, hn, w1, w2, w3,
                   s == CUNSENT ? "\t~timing" : "");
            rc = rdy(c);
            printf("S\t%s\t%s\tafter rdy(c)\t%s%s\n", tag, hn, rc, mark);
            rp = rdy(p);
            printf("S\t%s\t%s\tafter rdy(p)\t%s%s\n", tag, hn, rp, mark);
            r1 = do_read(c);
            r2 = do_read(c);
            r3 = do_read(p);
            const char *e1 = do_soerr(c);
            const char *e2 = do_soerr(p);
            printf("S\t%s\t%s\tafter c-read=%s,%s p-read=%s soerr(c)=%s soerr(p)=%s\n", tag, hn, r1, r2, r3, e1, e2);
            if (s == CUNSENT) {
                const char *last = "";
                long n = drain(p, &last);
                rp = rdy(p);
                printf("S\t%s\t%s\tp-drained=%ld last=%s rdy(p)\t%s\t~counts\n", tag, hn, n, last, rp);
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
            const char *second = do_shutdown(c, hows[b]);
            const char *rc = rdy(c);
            const char *rp = rdy(p);
            printf("T\tidle\t%s then %s\t%s,%s\trdy(c)\t%s\trdy(p)\t%s\n", how_name(hows[a]), how_name(hows[b]), first,
                   second, rc, rp);
            discard(c);
            discard(p);
        }
    for (int peer = 0; peer < 2; peer++)
        for (int h = 0; h < 3; h++) {
            int c, p;
            pair(&c, &p);
            if (peer == 1) {
                write(c, buf, 100);
                settle();
            }
            close(p);
            settle();
            const char *sh = do_shutdown(c, hows[h]);
            const char *e = do_soerr(c);
            printf("T\t%s\t%s\t%s\tsoerr(c) after=%s\n", peer ? "peer-reset" : "peer-fin", how_name(hows[h]), sh, e);
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
        const char *rp = rdy(p);
        const char *rc = rdy(c);
        const char *r1 = do_read(p);
        const char *w1 = do_write(c, 100);
        printf("T\tpeer-shut-wr\t%s\t%s\trdy(p)\t%s\trdy(c)\t%s\tp-read=%s c-write100=%s\n", how_name(hows[h]), r, rp, rc,
               r1, w1);
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
        const char *sh = do_shutdown(c, hows[h]);
        printf("T\tpeer-reset-taken\t%s\t%s (soerr first %s)\n", how_name(hows[h]), sh, e);
        discard(c);
    }
}

// ---- U ----

static void section_u(void)
{
    for (int h = 0; h < 3; h++) {
        const char *hn = how_name(hows[h]);
        int f = socket(AF_INET, SOCK_STREAM, 0);
        const char *r = do_shutdown(f, hows[h]);
        printf("U\tfresh\t%s\t%s\n", hn, r);
        close(f);
        f = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in a = loop_at(0);
        bind(f, (struct sockaddr *)&a, sizeof a);
        r = do_shutdown(f, hows[h]);
        printf("U\tbound\t%s\t%s\n", hn, r);
        close(f);
        f = socket(AF_INET, SOCK_DGRAM, 0);
        r = do_shutdown(f, hows[h]);
        printf("U\tudp-unconnected\t%s\t%s\n", hn, r);
        close(f);
        f = open("/dev/null", O_RDONLY);
        r = do_shutdown(f, hows[h]);
        printf("U\tnot-socket\t%s\t%s\n", hn, r);
        close(f);
        r = do_shutdown(f, hows[h]);
        printf("U\tclosed-fd\t%s\t%s\n", hn, r);
    }
    // A listener: each how, with one connection queued.
    for (int h = 0; h < 3; h++) {
        const char *hn = how_name(hows[h]);
        int l = listener(4);
        int port = port_of(l);
        int q = connect_to(port);
        set_nb(q, 1);
        const char *r = do_shutdown(l, hows[h]);
        const char *rl = rdy(l);
        printf("U\tlistener\t%s\tshutdown=%s\trdy(l)\t%s\n", hn, r, rl);
        const char *rq = rdy(q);
        printf("U\tlistener\t%s\tqueued client rdy\t%s\n", hn, rq);
        const char *r1 = do_read(q);
        const char *e1 = do_soerr(q);
        printf("U\tlistener\t%s\tqueued client read=%s soerr=%s\n", hn, r1, e1);
        set_nb(l, 1);
        int acc = accept(l, NULL, NULL);
        int ae = errno;
        settle();
        const char *acs = ans(acc, ae);
        int kept = port_of(l) == port;
        if (acc >= 0) discard(acc);
        int n = socket(AF_INET, SOCK_STREAM, 0);
        set_nb(n, 1);
        struct sockaddr_in a = loop_at(port);
        int cr = connect(n, (struct sockaddr *)&a, sizeof a);
        int ce = errno;
        settle();
        const char *crs = ans(cr, ce);
        const char *ne = do_soerr(n);
        discard(n);
        int lr = listen(l, 4);
        int le = errno;
        const char *lrs = ans(lr, le);
        printf("U\tlistener\t%s\taccept=%s port_kept=%d new-connect=%s soerr=%s relisten=%s\n", hn, acs, kept, crs, ne,
               lrs);
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
        settle();
        const char *rs = ans(r, e);
        const char *sh = do_shutdown(f, hows[h]);
        printf("U\trefused(%s)\t%s\t%s\n", rs, how_name(hows[h]), sh);
        close(f);
    }
    // A bad how on sockets that are not connected, and on a non-socket.
    for (int k = 0; k < 2; k++) {
        int bh = k == 0 ? 3 : -1;
        int f = socket(AF_INET, SOCK_STREAM, 0);
        const char *r = do_shutdown(f, bh);
        printf("U\tfresh\thow=%d\t%s\n", bh, r);
        close(f);
        f = open("/dev/null", O_RDONLY);
        r = do_shutdown(f, bh);
        printf("U\tnot-socket\thow=%d\t%s\n", bh, r);
        close(f);
    }
    // A bad how on a connected socket.
    int bad[2] = { 3, -1 };
    for (int k = 0; k < 2; k++) {
        int c, p;
        pair(&c, &p);
        const char *r = do_shutdown(c, bad[k]);
        printf("U\tconnected\thow=%d\t%s\n", bad[k], r);
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
        const char *sh = do_shutdown(c, SHUT_RDWR);
        int q = fionread(c);
        printf("P\tunread-rdwr\tshutdown=%s fionread(c)=%d\n", sh, q);
        const char *rp = rdy(p);
        printf("P\tunread-rdwr\tbefore close rdy(p)\t%s\n", rp);
        do_close(c);
        rp = rdy(p);
        printf("P\tunread-rdwr\tafter close rdy(p)\t%s\n", rp);
        const char *r1 = do_read(p);
        const char *r2 = do_read(p);
        const char *w1 = do_write(p, 100);
        const char *w2 = do_write(p, 100);
        const char *e = do_soerr(p);
        printf("P\tunread-rdwr\tp-read=%s,%s p-write100=%s p-write100=%s soerr(p)=%s\n", r1, r2, w1, w2, e);
        discard(p);
    }
    // c: shutdown(RD) with 1000 unread, then close, as the same question without the WR half.
    {
        int c, p;
        pair(&c, &p);
        write(p, buf, 1000);
        settle();
        const char *sh = do_shutdown(c, SHUT_RD);
        int q = fionread(c);
        printf("P\tunread-rd\tshutdown=%s fionread(c)=%d\n", sh, q);
        do_close(c);
        const char *rp = rdy(p);
        printf("P\tunread-rd\tafter close rdy(p)\t%s\n", rp);
        const char *r1 = do_read(p);
        const char *w1 = do_write(p, 100);
        const char *e = do_soerr(p);
        printf("P\tunread-rd\tp-read=%s p-write100=%s soerr(p)=%s\n", r1, w1, e);
        discard(p);
    }
    // SHUT_RD, then p writes until EAGAIN (capped).
    {
        int c, p;
        pair(&c, &p);
        const char *sh = do_shutdown(c, SHUT_RD);
        printf("P\trd-then-fill\tshutdown=%s\n", sh);
        long total = 0;
        int dry = 0;
        while (total < (16L << 20) && dry < 3) {
            long r = write(p, buf, 65536);
            int e = errno;
            if (r > 0) { total += r; dry = 0; continue; }
            if (e != EAGAIN) {
                const char *a = ans(r, e);
                // On Darwin this write races the reset its first bytes provoked,
                // and answers ECONNRESET or EPIPE.
                printf("P\trd-then-fill\twrite %s\t~timing\n", a);
                break;
            }
            dry++;
            settle();
        }
        settle();
        int q = fionread(c);
        const char *rc = rdy(c);
        printf("P\trd-then-fill\tp-took=%ld fionread(c)=%d rdy(c)\t%s\t~counts\n", total, q, rc);
        const char *r1 = do_read(c);
        printf("P\trd-then-fill\tc-read=%s\n", r1);
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
            const char *hn = how_name(hows[h]);
            int c, p;
            pair(&c, &p);
            if (sent) {
                write(c, buf, 100);
                settle();
                read(p, buf, 4096);
                settle();
            }
            const char *r = do_shutdown(c, hows[h]);
            const char *rc = rdy(c);
            printf("R\t%s\tc-sent-first=%d\tshutdown=%s\trdy(c)\t%s\n", hn, sent, r, rc);
            for (int k = 1; k <= 4; k++) {
                const char *w = do_write(p, 100);
                rc = rdy(c);
                const char *rp = rdy(p);
                printf("R\t%s\tc-sent-first=%d\tp-write#%d=%s\trdy(c)\t%s\trdy(p)\t%s\n", hn, sent, k, w, rc, rp);
            }
            const char *r1 = do_read(c);
            const char *r2 = do_read(p);
            const char *e1 = do_soerr(c);
            const char *e2 = do_soerr(p);
            printf("R\t%s\tc-sent-first=%d\tc-read=%s p-read=%s soerr(c)=%s soerr(p)=%s\n", hn, sent, r1, r2, e1, e2);
            discard(c);
            discard(p);
        }
    // p filled c's receive buffer and has bytes left in its own; then c shuts RD.
    for (int h = 0; h < 3; h += 2) {
        const char *hn = how_name(hows[h]);
        // Darwin's answer to SHUT_RD alone here waits on a TCP timer.
#ifdef __APPLE__
        const char *mark = h == 0 ? "\t~timing" : "\t~counts";
        const char *last_mark = h == 0 ? "\t~timing" : "";
#else
        // Linux advertises no window for the room c's reads make here, so
        // p's bytes wait for a zero-window probe.
        const char *mark = "\t~timing";
        const char *last_mark = "\t~timing";
#endif
        int c, p;
        pair(&c, &p);
        long n = fill(p);
        const char *r = do_shutdown(c, hows[h]);
        const char *rc = rdy(c);
        const char *rp = rdy(p);
        printf("R\t%s\tp-unsent(%ld)\tshutdown=%s\trdy(c)\t%s\trdy(p)\t%s%s\n", hn, n, r, rc, rp, mark);
        const char *r1 = do_read(c);
        rc = rdy(c);
        rp = rdy(p);
        printf("R\t%s\tp-unsent\tc-read=%s\trdy(c)\t%s\trdy(p)\t%s%s\n", hn, r1, rc, rp, mark);
        r1 = do_read(c);
        const char *w1 = do_write(p, 100);
        const char *e1 = do_soerr(c);
        const char *e2 = do_soerr(p);
        printf("R\t%s\tp-unsent\tc-read=%s p-write100=%s soerr(c)=%s soerr(p)=%s%s\n", hn, r1, w1, e1, e2, last_mark);
        if (h == 0) {
            // Whether a TCP timer changes anything.
            sleep(5);
            rc = rdy(c);
            rp = rdy(p);
            e1 = do_soerr(c);
            e2 = do_soerr(p);
            printf("R\t%s\tp-unsent\t5 s later\trdy(c)\t%s\trdy(p)\t%s\tsoerr(c)=%s soerr(p)=%s%s\n", hn, rc, rp, e1, e2,
                   mark);
        }
        discard(c);
        discard(p);
    }
}

// ---- E ----

#ifdef __linux__
static void edges(const char *tag, int c, int p, int ep, const char *mark)
{
    (void)p;
    struct epoll_event out[4];
    int n = epoll_wait(ep, out, 4, 0);
    char cs[32] = "-", ps[32] = "-";
    for (int k = 0; k < n; k++) snprintf(out[k].data.fd == c ? cs : ps, 32, "0x%x", out[k].events);
    printf("E\t%s\tc=%s p=%s%s\n", tag, cs, ps, mark);
}
#else
static void edges(const char *tag, int c, int p, int kq, const char *mark)
{
    (void)p;
    struct kevent out[4];
    struct timespec zero = { 0, 0 };
    int n = kevent(kq, NULL, 0, out, 4, &zero);
    char s[256] = "";
    int len = 0;
    for (int k = 0; k < n; k++)
        len += snprintf(s + len, sizeof s - len, " %s-%s=%lld%s/%u", (int)out[k].ident == c ? "c" : "p",
                        out[k].filter == EVFILT_READ ? "read" : "write", (long long)out[k].data,
                        (out[k].flags & EV_EOF) ? "/EOF" : "", out[k].fflags);
    printf("E\t%s\t%s%s\n", tag, n ? s + 1 : "-", mark);
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
            const char *mark = s == CUNSENT ? "\t~counts" : "";
            int c, p;
            pair(&c, &p);
            prepare(s, c, p);
            int ep = edge_port(c, p);
            char tag[64];
            snprintf(tag, sizeof tag, "%s\t%s\tdrain", start_name[s], how_name(hows[h]));
            edges(tag, c, p, ep, mark);
            snprintf(tag, sizeof tag, "%s\t%s\tagain", start_name[s], how_name(hows[h]));
            edges(tag, c, p, ep, mark);
            do_shutdown(c, hows[h]);
            snprintf(tag, sizeof tag, "%s\t%s\tafter", start_name[s], how_name(hows[h]));
            edges(tag, c, p, ep, mark);
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
        const char *r = do_shutdown(l, hows[h]);
        int n = epoll_wait(ep, &out, 1, 0);
        printf("E\tlistener\t%s\tdrain n=%d shutdown=%s after=0x%x\n", how_name(hows[h]), n0, r, n > 0 ? out.events : 0);
#else
        int ep = kqueue();
        struct kevent ch;
        EV_SET(&ch, l, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
        kevent(ep, &ch, 1, NULL, 0, NULL);
        struct kevent out;
        struct timespec zero = { 0, 0 };
        int n0 = kevent(ep, NULL, 0, &out, 1, &zero);
        const char *r = do_shutdown(l, hows[h]);
        int n = kevent(ep, NULL, 0, &out, 1, &zero);
        if (n > 0)
            printf("E\tlistener\t%s\tdrain n=%d shutdown=%s after=read %lld%s/%u\n", how_name(hows[h]), n0, r,
                   (long long)out.data, (out.flags & EV_EOF) ? "/EOF" : "", out.fflags);
        else
            printf("E\tlistener\t%s\tdrain n=%d shutdown=%s after=-\n", how_name(hows[h]), n0, r);
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
};

static void *sleeper_main(void *arg)
{
    struct sleeper *s = arg;
    static char wbuf[1 << 20];
    static char big[8 << 20];
    long r;
    if (s->kind == 0) r = read(s->fd, buf, 4096);
    else if (s->kind == 1) r = write(s->fd, wbuf, s->len);
    else if (s->kind == 3) r = write(s->fd, big, s->len);
    else r = accept(s->fd, NULL, NULL);
    s->err = errno;
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
    if (atomic_load(&s->done)) {
        const char *a = ans(s->result, s->err);
        printf("B\t%s\t%s\treturned %s%s\n", tag, step, a, pipes ? "+SIGPIPE" : "");
    } else
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
                write(side == 0 ? p : c, buf, 1);
                report(tag, side == 0 ? "p-writes-1" : "c-writes-1", &s);
            }
            if (!atomic_load(&s.done)) {
                pthread_kill(t, SIGUSR1);
                report(tag, "SIGUSR1", &s);
            }
            pthread_join(t, NULL);
            discard(c);
            discard(p);
        }
    // write(c) asleep on a full buffer, shutdown(c, how).
    for (int h = 0; h < 3; h++) {
        int c, p;
        pair(&c, &p);
        long filled = fill(c);
        printf("#\tfilled %ld\n", filled);
        set_nb(c, 0);
        struct sleeper s = { .kind = 1, .fd = c, .len = 1 << 20 };
        pthread_t t;
        pthread_create(&t, NULL, sleeper_main, &s);
        usleep(100000);
        char tag[32];
        snprintf(tag, sizeof tag, "wr-%s", how_name(hows[h]));
        pipes_before_step = atomic_load(&sigpipes);
        const char *r = do_shutdown(c, hows[h]);
        char step[64];
        snprintf(step, sizeof step, "shutdown=%s", r);
        report(tag, step, &s);
        if (!atomic_load(&s.done)) {
            const char *last = "";
            long n = drain(p, &last);
            char d[64];
            printf("#\tp drained %ld\n", n);
            snprintf(d, sizeof d, "p-drains(%s)", last);
            report(tag, d, &s);
        }
        if (!atomic_load(&s.done)) {
            pthread_kill(t, SIGUSR1);
            report(tag, "SIGUSR1", &s);
        }
        pthread_join(t, NULL);
        const char *e = do_soerr(c);
        printf("B\t%s\tsoerr(c)=%s\n", tag, e);
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
            printf("#\tp drained %ld\n", n);
            snprintf(d, sizeof d, "p-drains(%s)", last);
            report(tag, d, &s);
        }
        if (!atomic_load(&s.done)) {
            pthread_kill(t, SIGUSR1);
            report(tag, "SIGUSR1", &s);
        }
        pthread_join(t, NULL);
        const char *last = "";
        long n = drain(p, &last);
        const char *e = do_soerr(c);
        printf("B\t%s\tp-drained-after=%ld last=%s soerr(c)=%s\t~counts\n", tag, n, last, e);
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
        if (!atomic_load(&s.done)) {
            pthread_kill(t, SIGUSR1);
            report(tag, "SIGUSR1", &s);
        }
        pthread_join(t, NULL);
        if (s.result >= 0) discard((int)s.result);
        if (q >= 0) discard(q);
        close(l);
    }
}

// ---- L ----

enum lstart {
    L_IDLE,
    L_CUNREAD,
    L_CUNSENT,
    L_PDATA,
    L_AFTERWR,
    L_AFTERFIN,
    L_CUNSENT_AFTERWR,
    L_BOTHFIN_CFIRST,
    L_BOTHFIN_PFIRST,
    L_CQUEUED_PFIN,
    L_PFIN_CQUEUED,
};
static const char *lstart_name[] = { "idle",           "cunread",        "cunsent",      "pdata",
                                     "afterwr",        "afterfin",       "cunsent-afterwr",
                                     "bothfin-cfirst", "bothfin-pfirst", "cqueued-pfin", "pfin-cqueued" };

static void section_l(void)
{
    for (int lin = 1; lin >= 0; lin--)
        for (int s = L_IDLE; s <= L_PFIN_CQUEUED; s++) {
            int c, p;
            pair(&c, &p);
            int cport = port_of(c);
            if (s == L_CUNREAD) {
                write(p, buf, 1000);
                settle();
            }
            // In the rows where both FINs are made before the close, the
            // linger is set first as well, since Darwin refuses to set it once
            // both directions are shut; the close below tries again.
            int late = s >= L_BOTHFIN_CFIRST;
            const char *early = "-";
            if (lin && late) early = try_linger(c, 1, 0);
            if (s == L_PFIN_CQUEUED || s == L_BOTHFIN_PFIRST) do_shutdown(p, SHUT_WR);
            if (s == L_CUNSENT || s == L_CUNSENT_AFTERWR || s == L_CQUEUED_PFIN || s == L_PFIN_CQUEUED) {
                long n = fill(c);
                printf("#\tfilled %ld\n", n);
            }
            if (s == L_PDATA) {
                write(c, buf, 1000);
                settle();
            }
            if (s == L_AFTERWR || s == L_CUNSENT_AFTERWR || s == L_BOTHFIN_CFIRST || s == L_BOTHFIN_PFIRST
                || s == L_CQUEUED_PFIN || s == L_PFIN_CQUEUED)
                do_shutdown(c, SHUT_WR);
            if (s == L_AFTERFIN || s == L_BOTHFIN_CFIRST || s == L_CQUEUED_PFIN) do_shutdown(p, SHUT_WR);
            const char *at_close = "-";
            if (lin) at_close = try_linger(c, 1, 0);
            const char *tag = lstart_name[s];
            const char *lt = lin ? "linger0" : "nolinger";
            int queued = s == L_CUNSENT || s == L_CUNSENT_AFTERWR || s == L_CQUEUED_PFIN || s == L_PFIN_CQUEUED;
            const char *mark = queued ? "\t~counts" : "";
            // Darwin's reset here waits on the peer's reads; the model refuses
            // the close.
            const char *row = "";
#ifdef __APPLE__
            if (lin && s == L_CQUEUED_PFIN) {
                mark = "\t~timing";
                row = "\t~timing";
            }
#endif
            const char *cl = do_close(c);
            const char *rp = rdy(p);
            printf("L\t%s\t%s\tlinger set early=%s at close=%s\tclose=%s rdy(p)\t%s%s\n", lt, tag, early, at_close, cl, rp,
                   mark);
            const char *b1 = bind_free(cport);
            printf("L\t%s\t%s\tclosers-endpoint at once %s%s\n", lt, tag, b1, row);
            const char *r1 = do_read(p);
            const char *r2 = do_read(p);
            const char *w1 = do_write(p, 100);
            const char *w2 = do_write(p, 100);
            const char *e = do_soerr(p);
            const char *r3 = do_read(p);
            printf("L\t%s\t%s\tp-read=%s,%s p-write100=%s p-write100=%s soerr(p)=%s p-read=%s%s\n", lt, tag, r1, r2, w1,
                   w2, e, r3, row);
            if (queued || s == L_PDATA) {
                const char *last = "";
                long n = drain(p, &last);
                printf("L\t%s\t%s\tp-drained=%ld last=%s%s\n", lt, tag, n, last, mark);
            }
            const char *b2 = bind_free(cport);
            printf("L\t%s\t%s\tclosers-endpoint after p acted %s%s\n", lt, tag, b2, row);
            discard(p);
        }
}

// ---- F ----

enum forder {
    F_P_FIRST_CLOSE,
    F_P_FIRST_WR,
    F_P_FIRST_QUEUED_CLOSE,
    F_P_FIRST_QUEUED_WR,
    F_C_FIRST_CLOSE,
    F_C_FIRST_WR,
    F_C_QUEUED,
};
static const char *forder_name[] = { "p-first-close",    "p-first-wr",    "p-first-queued-close", "p-first-queued-wr",
                                     "c-first-close",    "c-first-wr",    "c-queued" };

static void section_f(void)
{
    for (int o = F_P_FIRST_CLOSE; o <= F_C_QUEUED; o++) {
        int c, p;
        pair(&c, &p);
        int cport = port_of(c);
        const char *tag = forder_name[o];
        switch (o) {
        case F_P_FIRST_CLOSE:
            do_shutdown(p, SHUT_WR);
            do_close(c);
            break;
        case F_P_FIRST_WR:
            do_shutdown(p, SHUT_WR);
            do_shutdown(c, SHUT_WR);
            do_close(c);
            break;
        case F_P_FIRST_QUEUED_CLOSE:
        case F_P_FIRST_QUEUED_WR: {
            // c's FIN is made after p's has arrived (passive), but waits
            // behind c's bytes until p drains them, after c has closed.
            do_shutdown(p, SHUT_WR);
            long n = fill(c);
            printf("#\tfilled %ld\n", n);
            if (o == F_P_FIRST_QUEUED_WR) do_shutdown(c, SHUT_WR);
            do_close(c);
            const char *q1 = bind_free(cport);
            settle();
            const char *q2 = bind_free(cport);
            const char *qp = rdy(p);
            printf("F\t%s\tbefore p drains: closers-endpoint at once %s, 30 ms later %s\trdy(p)\t%s\t~counts\n", tag, q1,
                   q2, qp);
            const char *last = "";
            long d = drain(p, &last);
            printf("F\t%s\tp-drained=%ld last=%s\t~counts\n", tag, d, last);
            break;
        }
        case F_C_FIRST_CLOSE:
            do_close(c);
            do_shutdown(p, SHUT_WR);
            break;
        case F_C_FIRST_WR:
            do_shutdown(c, SHUT_WR);
            do_shutdown(p, SHUT_WR);
            do_close(c);
            break;
        case F_C_QUEUED: {
            long n = fill(c);
            printf("#\tfilled %ld\n", n);
            do_shutdown(c, SHUT_WR);
            do_shutdown(p, SHUT_WR);
            const char *last = "";
            long d = drain(p, &last);
            printf("F\t%s\tp-drained=%ld last=%s\t~counts\n", tag, d, last);
            do_close(c);
            break;
        }
        }
        const char *b1 = bind_free(cport);
        settle();
        const char *b2 = bind_free(cport);
        const char *rp = rdy(p);
        printf("F\t%s\tclosers-endpoint at once %s, 30 ms later %s\trdy(p)\t%s\n", tag, b1, b2, rp);
        discard(p);
    }
}

// ---- X ----

static void section_x(void)
{
    int c, p;
    pair(&c, &p);
    long n = fill(c);
    printf("#\tfilled %ld\n", n);
    const char *s1 = do_shutdown(c, SHUT_WR);
    const char *s2 = do_shutdown(p, SHUT_RDWR);
    const char *rc = rdy(c);
    const char *rp = rdy(p);
    // Linux delivers c's bytes on a zero-window probe after p's drain, and
    // Darwin never does; the model refuses Darwin's state outright.
#ifdef __APPLE__
    const char *first = "\t~timing";
#else
    const char *first = "\t~counts";
#endif
    printf("X\tc-shut-wr=%s p-shut-rdwr=%s\trdy(c)\t%s\trdy(p)\t%s%s\n", s1, s2, rc, rp, first);
    const char *last = "";
    long d = drain(p, &last);
    rc = rdy(c);
    rp = rdy(p);
    printf("X\tp-drained=%ld last=%s\trdy(c)\t%s\trdy(p)\t%s\t~timing\n", d, last, rc, rp);
    const char *e1 = do_soerr(c);
    const char *r1 = do_read(c);
    const char *w1 = do_write(c, 100);
    const char *e2 = do_soerr(p);
    const char *r2 = do_read(p);
    printf("X\tsoerr(c)=%s c-read=%s c-write100=%s soerr(p)=%s p-read=%s\t~timing\n", e1, r1, w1, e2, r2);
    // Then whether a TCP timer changes anything: readiness, which consumes
    // nothing, at intervals, and the errors at the end.
    const int at_ms[] = { 250, 500, 1000, 2000, 5000, 15000 };
    int waited = 0;
    for (int k = 0; k < 6; k++) {
        usleep((at_ms[k] - waited) * 1000);
        waited = at_ms[k];
        rc = rdy(c);
        rp = rdy(p);
        printf("X\t%d ms later\trdy(c)\t%s\trdy(p)\t%s\t~timing\n", waited, rc, rp);
    }
    e1 = do_soerr(c);
    e2 = do_soerr(p);
    r1 = do_read(c);
    r2 = do_read(p);
    printf("X\tthen soerr(c)=%s soerr(p)=%s c-read=%s p-read=%s\t~timing\n", e1, e2, r1, r2);
    discard(c);
    discard(p);
}

// ---- Q ----

static void section_q(void)
{
    const char *modes[] = { "close-nolinger", "close-linger0", "shutdown-rdwr" };
    for (int m = 0; m < 3; m++) {
        int l = listener(8);
        int port = port_of(l);
        int q0 = connect_to(port);
        int q1 = connect_to(port);
        int q2 = connect_to(port);
        set_nb(q0, 1);
        set_nb(q1, 1);
        write(q1, buf, 100);
        settle();
        do_close(q2);
        if (m == 1) set_linger(l, 1, 0);
        const char *r = m == 2 ? do_shutdown(l, SHUT_RDWR) : do_close(l);
        printf("Q\t%s\t%s\n", modes[m], r);
        int qs[2] = { q0, q1 };
        for (int k = 0; k < 2; k++) {
            const char *rq = rdy(qs[k]);
            printf("Q\t%s\tq%d rdy\t%s\n", modes[m], k, rq);
            const char *r1 = do_read(qs[k]);
            const char *w1 = do_write(qs[k], 100);
            const char *e = do_soerr(qs[k]);
            const char *r2 = do_read(qs[k]);
            printf("Q\t%s\tq%d read=%s write100=%s soerr=%s read=%s\n", modes[m], k, r1, w1, e, r2);
        }
        if (m == 2) do_close(l);
        const char *b = bind_free(port);
        printf("Q\t%s\tlisteners-port %s\n", modes[m], b);
        discard(q0);
        discard(q1);
    }
}

// ---- G ----

static void section_g(void)
{
    for (int unsent = 1; unsent >= 0; unsent--)
        for (int blocking = 0; blocking < 2; blocking++) {
            const char *u = unsent ? "unsent" : "nothing";
            const char *bl = blocking ? "blocking" : "nonblocking";
            int c, p;
            pair(&c, &p);
            if (unsent) {
                long n = fill(c);
                printf("#\tfilled %ld\n", n);
            }
            set_linger(c, 1, 1);
            if (blocking) set_nb(c, 0);
            long t0 = now_ms();
            long r = close(c);
            int e = errno;
            long dt = now_ms() - t0;
            settle();
            const char *rs = ans(r, e);
            const char *rp = rdy(p);
            // A close that waits for unsent bytes is refused by the model.
            const char *gm = unsent ? "\t~timing" : "\t~counts";
            printf("G\t%s\t%s\tclose=%s after %ld ms rdy(p)\t%s%s\n", u, bl, rs, dt, rp, gm);
            const char *last = "";
            long n = drain(p, &last);
            const char *pe = do_soerr(p);
            printf("G\t%s\t%s\tp-drained=%ld last=%s soerr(p)=%s%s\n", u, bl, n, last, pe, gm);
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

    const char *which = argc > 1 ? argv[1] : "LFXSTUPREBQG";
    for (const char *w = which; *w; w++) {
        switch (*w) {
        case 'L': section_l(); break;
        case 'F': section_f(); break;
        case 'X': section_x(); break;
        case 'S': section_s(); break;
        case 'T': section_t(); break;
        case 'U': section_u(); break;
        case 'P': section_p(); break;
        case 'R': section_r(); break;
        case 'E': section_e(); break;
        case 'B': section_b(); break;
        case 'Q': section_q(); break;
        case 'G': section_g(); break;
        }
    }
    return 0;
}
