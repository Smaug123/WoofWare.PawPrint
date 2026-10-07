// Byte transfer on a connected loopback TCP socket, on Linux and Darwin: how
// much a non-blocking writer can queue before EAGAIN when the peer never reads,
// what makes it writable again, and what read, recv, write and send answer, and
// what poll, epoll and kqueue report, in each state a connection passes through.
//
// Sections (each line is tab-separated, first field the section):
//   C   capacity. For each configuration (defaults; TCP_NODELAY on the writer;
//       SO_SNDBUF set on the writer before connect; SO_RCVBUF set on the
//       listener, so on the reader, before listen; both) and each write size,
//       the client writes that size non-blocking until EAGAIN while the
//       accepted end never reads. Reports the bytes taken, the calls, the first
//       short write, the reader's FIONREAD, the writer's unacknowledged bytes
//       (SIOCOUTQ on Linux, SO_NWRITE on Darwin), both buffers' sizes read
//       back after the fill, Linux's SO_MEMINFO (rmem_alloc, rcvbuf,
//       wmem_queued, sndbuf), and how many more bytes the writer takes after
//       sleeping 0, 10, 100 and 500 ms (whether the fill is asynchronous).
//   W   the write-space wake. A writer filled with 65536-byte writes until
//       EAGAIN is registered edge-triggered for EPOLLOUT (Linux) or EV_CLEAR
//       EVFILT_WRITE (Darwin), and level-triggered too; the reader then reads
//       STEP bytes at a time, and after each read the writer's poll revents,
//       the edge and level events, and the queue sizes are reported, until the
//       writer has been writable for a few steps. Then one more write's
//       answer.
//   S   states. For each state (below) of the connecting socket s and its
//       accepted peer p, a fresh connection is brought to that state, s's
//       readiness is reported (poll with every event asked; epoll level with
//       every event; kqueue READ and WRITE, level), then one operation is
//       applied to s, up to three times in a row, and then s's readiness again.
//       SIGPIPE is caught and counted.
//
// States of s:
//   IDLE         connected, nothing sent either way.
//   DATA_IN      p wrote 1000 bytes, s has read none.
//   SNDFULL      s wrote 65536-byte writes until EAGAIN; p never reads.
//   FIN          p closed, having sent nothing.
//   FIN_DATA     p wrote 1000 bytes and closed; s has read none.
//   FIN_DRAINED  p wrote 1000 bytes and closed; s read all 1000.
//   RST          s wrote 1000 bytes, then p closed without reading them.
//   RST_DATA     p wrote 1000 bytes, s wrote 1000 bytes, p closed without
//                reading; s has read none.
//   LINGER0      p set SO_LINGER {1, 0} and closed.
//   FIN_WRITTEN  p closed, then s wrote 100 bytes (taken), and p's kernel,
//                which has no socket for them, answers with a reset.
//   SNDFULL_RST  s filled its send buffer as SNDFULL, then p closed.
//
// Operations on s (each attempted three times unless noted):
//   none         readiness only, then getsockopt(SO_ERROR).
//   read         read(s, buf, 4096).
//   read0        read(s, buf, 0).
//   recv         recv(s, buf, 4096, 0).
//   recv0        recv(s, buf, 0, 0).
//   peek         recv(s, buf, 4096, MSG_PEEK) twice, then read(s, buf, 4096).
//   write        write(s, buf, 100).
//   write0       write(s, buf, 0).
//   send         send(s, buf, 100, 0).
//   send0        send(s, buf, 0, 0).
//   sendnosig    send(s, buf, 100, MSG_NOSIGNAL).
//   dontwait     with O_NONBLOCK cleared: recv(s, buf, 4096, MSG_DONTWAIT),
//                then send(s, buf, 65536, MSG_DONTWAIT), once each.
//
//   E   edges. Which transfers queue an edge-triggered registration
//       (EPOLLET on Linux, EV_CLEAR on Darwin), each step followed by a 20 ms
//       sleep and a zero-timeout wait:
//       E1  s registered for IN alone (READ alone): p writes 100 bytes, twice;
//           then s reads 50 of the 200; then nothing happens.
//       E2  c registered for OUT alone (WRITE alone), its ADD-time event
//           taken: c writes 1000 bytes (no EAGAIN); p reads them; c fills its
//           send buffer to EAGAIN; p reads 4096 bytes at a time until c's
//           edge comes, then once more.
//       E3  s registered for IN|OUT (Linux only): p writes 100 bytes.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/tcp-transfer tcp-transfer.c && /tmp/tcp-transfer
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/tcp-transfer.c && /tmp/p'
// An argument restricts the run to the sections it names, e.g. `CW`; the
// default is `SWEC`.
//
// Measured 2026-10-07 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501); one run of each is
// tcp-transfer.linux-6.18.5-aarch64.txt and tcp-transfer.darwin-27.0.txt. The
// S section was identical over three runs on Linux, and on Darwin but for the
// SNDFULL rows, whose send buffer Darwin's loopback drains on another thread.
// docs/plans/2026-10-07-tcp-byte-transfer.md reads the results.

#ifdef __linux__
#define _GNU_SOURCE
#endif

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __linux__
#include <linux/sockios.h>
#include <sys/epoll.h>
#ifndef SO_MEMINFO
#define SO_MEMINFO 55
#endif
#endif

#ifdef __APPLE__
#include <sys/event.h>
#endif

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#define NO_MSG_NOSIGNAL 1
#endif

static volatile sig_atomic_t sigpipes;

static void on_sigpipe(int signo)
{
    (void)signo;
    sigpipes++;
}

static void die(const char *what)
{
    perror(what);
    exit(2);
}

static const char *errname(int e)
{
    static char other[32];
    switch (e)
    {
        case EAGAIN: return "EAGAIN";
        case EPIPE: return "EPIPE";
        case ECONNRESET: return "ECONNRESET";
        case ENOTCONN: return "ENOTCONN";
        case EINVAL: return "EINVAL";
        case ESHUTDOWN: return "ESHUTDOWN";
        case ECONNABORTED: return "ECONNABORTED";
        case ETIMEDOUT: return "ETIMEDOUT";
        case EBADF: return "EBADF";
        case EOPNOTSUPP: return "EOPNOTSUPP";
        case EMSGSIZE: return "EMSGSIZE";
        case EFAULT: return "EFAULT";
        case EINTR: return "EINTR";
        default:
            snprintf(other, sizeof other, "errno%d", e);
            return other;
    }
}

static void set_nonblocking(int fd, int on)
{
    int flags = fcntl(fd, F_GETFL);
    if (flags < 0) die("F_GETFL");
    flags = on ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK);
    if (fcntl(fd, F_SETFL, flags) < 0) die("F_SETFL");
}

static int getint(int fd, int level, int name)
{
    int v = -1;
    socklen_t len = sizeof v;
    if (getsockopt(fd, level, name, &v, &len) < 0) return -1000 - errno;
    return v;
}

typedef struct
{
    const char *name;
    int writer_sndbuf;   // 0: leave alone
    int reader_rcvbuf;   // 0: leave alone
    int nodelay;
} config;

// A connected pair over 127.0.0.1: *c the connecting socket, *s the accepted
// one, both non-blocking. `cfg` applies SO_SNDBUF and TCP_NODELAY to the
// connecting socket and SO_RCVBUF to the listener before listen, which the
// accepted socket inherits.
static void make_pair(const config *cfg, int *c, int *s)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    if (l < 0) die("socket");
    int one = 1;
    setsockopt(l, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    if (cfg && cfg->reader_rcvbuf)
        if (setsockopt(l, SOL_SOCKET, SO_RCVBUF, &cfg->reader_rcvbuf, sizeof cfg->reader_rcvbuf) < 0) die("SO_RCVBUF");
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, 8) < 0) die("listen");
    socklen_t al = sizeof a;
    if (getsockname(l, (struct sockaddr *)&a, &al) < 0) die("getsockname");

    *c = socket(AF_INET, SOCK_STREAM, 0);
    if (*c < 0) die("socket");
    if (cfg && cfg->writer_sndbuf)
        if (setsockopt(*c, SOL_SOCKET, SO_SNDBUF, &cfg->writer_sndbuf, sizeof cfg->writer_sndbuf) < 0) die("SO_SNDBUF");
    if (cfg && cfg->nodelay)
        if (setsockopt(*c, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one) < 0) die("TCP_NODELAY");
    if (connect(*c, (struct sockaddr *)&a, sizeof a) < 0) die("connect");
    *s = accept(l, NULL, NULL);
    if (*s < 0) die("accept");
    close(l);
    set_nonblocking(*c, 1);
    set_nonblocking(*s, 1);
}

static int nread(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) < 0) return -1000 - errno;
    return n;
}

// Bytes the socket has sent but not had acknowledged (Linux), or holds in its
// send buffer (Darwin).
static int unacked(int fd)
{
#ifdef __linux__
    int n = -1;
    if (ioctl(fd, SIOCOUTQ, &n) < 0) return -1000 - errno;
    return n;
#else
    return getint(fd, SOL_SOCKET, SO_NWRITE);
#endif
}

static void meminfo(int fd, char *out, size_t len)
{
#ifdef __linux__
    unsigned int m[9];
    socklen_t ml = sizeof m;
    memset(m, 0, sizeof m);
    if (getsockopt(fd, SOL_SOCKET, SO_MEMINFO, m, &ml) < 0)
        snprintf(out, len, "meminfo=%s", errname(errno));
    else
        snprintf(out, len, "rmem_alloc=%u rcvbuf=%u wmem_alloc=%u sndbuf=%u wmem_queued=%u", m[0], m[1], m[2], m[3], m[5]);
#else
    snprintf(out, len, "so_nread=%d", getint(fd, SOL_SOCKET, SO_NREAD));
#endif
}

static int poll_revents(int fd)
{
    struct pollfd p;
    p.fd = fd;
    p.events = POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND
#ifdef __linux__
               | POLLRDHUP | POLLMSG
#endif
        ;
    p.revents = 0;
    if (poll(&p, 1, 0) < 0) die("poll");
    return p.revents;
}

#ifdef __linux__
static unsigned int epoll_level(int fd)
{
    int ep = epoll_create1(0);
    if (ep < 0) die("epoll_create1");
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLPRI | EPOLLOUT | EPOLLRDNORM | EPOLLRDBAND | EPOLLWRNORM | EPOLLWRBAND | EPOLLMSG | EPOLLRDHUP;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev) < 0) die("epoll_ctl");
    struct epoll_event out;
    int n = epoll_wait(ep, &out, 1, 0);
    close(ep);
    return n > 0 ? out.events : 0;
}
#endif

#ifdef __APPLE__
static void kq_filter(int kq, int fd, short filter, char *out, size_t len)
{
    struct kevent ev;
    struct timespec zero = {0, 0};
    EV_SET(&ev, fd, filter, EV_ADD, 0, 0, NULL);
    if (kevent(kq, &ev, 1, NULL, 0, NULL) < 0) die("kevent add");
    struct kevent got;
    int n = kevent(kq, NULL, 0, &got, 1, &zero);
    if (n < 0) die("kevent wait");
    if (n == 0)
        snprintf(out, len, "-");
    else
        snprintf(out, len, "data=%lld%s%s fflags=%u", (long long)got.data,
                 (got.flags & EV_EOF) ? " EOF" : "", (got.flags & EV_ERROR) ? " ERROR" : "", got.fflags);
    EV_SET(&ev, fd, filter, EV_DELETE, 0, 0, NULL);
    kevent(kq, &ev, 1, NULL, 0, NULL);
}
#endif

static void readiness(int fd, char *out, size_t len)
{
    char extra[256];
#ifdef __linux__
    snprintf(extra, sizeof extra, "epoll=0x%x", epoll_level(fd));
#else
    int kq = kqueue();
    if (kq < 0) die("kqueue");
    char r[96], w[96];
    kq_filter(kq, fd, EVFILT_READ, r, sizeof r);
    kq_filter(kq, fd, EVFILT_WRITE, w, sizeof w);
    close(kq);
    snprintf(extra, sizeof extra, "READ(%s) WRITE(%s)", r, w);
#endif
    snprintf(out, len, "poll=0x%x %s fionread=%d", poll_revents(fd), extra, nread(fd));
}

static char payload[1 << 20];

// Write `chunk` bytes at a time until EAGAIN (or 512 MiB, which would mean the
// reader is somehow draining). Returns the total taken; *calls the writes that
// took something, *first_short the first write that took less than asked
// (-1 if none), *final_errno the errno that ended it.
static long long fill(int fd, int chunk, long long *calls, long long *first_short, int *final_errno)
{
    long long total = 0;
    *calls = 0;
    *first_short = -1;
    *final_errno = 0;
    while (total < (512LL << 20))
    {
        ssize_t n = write(fd, payload, chunk);
        if (n < 0)
        {
            *final_errno = errno;
            break;
        }
        (*calls)++;
        if (n < chunk && *first_short < 0) *first_short = n;
        total += n;
    }
    return total;
}

static void section_c(void)
{
    static const config configs[] = {
        {"default", 0, 0, 0},
        {"nodelay", 0, 0, 1},
        {"sndbuf=4096", 4096, 0, 0},
        {"sndbuf=16384", 16384, 0, 0},
        {"sndbuf=65536", 65536, 0, 0},
        {"sndbuf=262144", 262144, 0, 0},
        {"rcvbuf=4096", 0, 4096, 0},
        {"rcvbuf=16384", 0, 16384, 0},
        {"rcvbuf=65536", 0, 65536, 0},
        {"rcvbuf=262144", 0, 262144, 0},
        {"sndbuf=16384,rcvbuf=16384", 16384, 16384, 0},
        {"sndbuf=65536,rcvbuf=65536", 65536, 65536, 0},
    };
    static const int chunks[] = {1, 7, 100, 1000, 1448, 4096, 16384, 65536, 1 << 20};
    for (size_t ci = 0; ci < sizeof configs / sizeof configs[0]; ci++)
    {
        for (size_t k = 0; k < sizeof chunks / sizeof chunks[0]; k++)
        {
            // Single-byte and seven-byte fills are slow; run them only under
            // the default and nodelay configurations.
            if (chunks[k] < 100 && ci > 1) continue;
            for (int trial = 0; trial < 3; trial++)
            {
                int c, s;
                make_pair(&configs[ci], &c, &s);
                int sndbuf_before = getint(c, SOL_SOCKET, SO_SNDBUF);
                int rcvbuf_before = getint(s, SOL_SOCKET, SO_RCVBUF);
                long long calls, first_short;
                int err;
                long long total = fill(c, chunks[k], &calls, &first_short, &err);
                char wm[160], rm[160];
                meminfo(c, wm, sizeof wm);
                meminfo(s, rm, sizeof rm);
                int fion = nread(s), outq = unacked(c);
                int pr = poll_revents(c);
                long long later[4];
                static const int waits[] = {0, 10, 100, 500};
                for (int w = 0; w < 4; w++)
                {
                    usleep(waits[w] * 1000);
                    long long c2, fs2;
                    int e2;
                    later[w] = fill(c, chunks[k], &c2, &fs2, &e2);
                }
                printf("C\t%s\tchunk=%d\ttrial=%d\ttotal=%lld calls=%lld first_short=%lld ended=%s\t"
                       "writer SO_SNDBUF %d->%d, reader SO_RCVBUF %d->%d\treader fionread=%d writer unacked=%d writer poll=0x%x\t"
                       "writer[%s] reader[%s]\tlater(0,10,100,500ms)=%lld,%lld,%lld,%lld\n",
                       configs[ci].name, chunks[k], trial, total, calls, first_short, errname(err),
                       sndbuf_before, getint(c, SOL_SOCKET, SO_SNDBUF), rcvbuf_before, getint(s, SOL_SOCKET, SO_RCVBUF),
                       fion, outq, pr, wm, rm, later[0], later[1], later[2], later[3]);
                fflush(stdout);
                close(c);
                close(s);
            }
        }
    }
}

static void section_w(int step)
{
    int c, s;
    make_pair(NULL, &c, &s);
    long long calls, first_short;
    int err;
    long long total = fill(c, 65536, &calls, &first_short, &err);

#ifdef __linux__
    int et = epoll_create1(0), lt = epoll_create1(0);
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLOUT | EPOLLET;
    if (epoll_ctl(et, EPOLL_CTL_ADD, c, &ev) < 0) die("epoll_ctl et");
    ev.events = EPOLLOUT;
    if (epoll_ctl(lt, EPOLL_CTL_ADD, c, &ev) < 0) die("epoll_ctl lt");
    struct epoll_event out;
    int n0 = epoll_wait(et, &out, 1, 0);
    printf("W\tstep=%d\tfilled %lld bytes (%s); edge registration at ADD: %d event(s) 0x%x\n", step, total, errname(err), n0,
           n0 > 0 ? out.events : 0);
#else
    int kq = kqueue();
    struct kevent ev;
    struct timespec zero = {0, 0};
    EV_SET(&ev, c, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
    if (kevent(kq, &ev, 1, NULL, 0, NULL) < 0) die("kevent add");
    int kl = kqueue();
    EV_SET(&ev, c, EVFILT_WRITE, EV_ADD, 0, 0, NULL);
    if (kevent(kl, &ev, 1, NULL, 0, NULL) < 0) die("kevent add");
    struct kevent got;
    int n0 = kevent(kq, NULL, 0, &got, 1, &zero);
    printf("W\tstep=%d\tfilled %lld bytes (%s); EV_CLEAR registration at ADD: %d event(s) data=%lld\n", step, total, errname(err), n0,
           n0 > 0 ? (long long)got.data : 0LL);
#endif
    char *buf = malloc(step > 0 ? step : 1);
    long long readsofar = 0;
    int writable_steps = 0;
    while (writable_steps < 3 && readsofar < total)
    {
        ssize_t r = read(s, buf, step);
        if (r <= 0)
        {
            printf("W\tstep=%d\tread answered %zd (%s)\n", step, r, r < 0 ? errname(errno) : "-");
            break;
        }
        readsofar += r;
        int pr = poll_revents(c);
        char wm[160];
        meminfo(c, wm, sizeof wm);
#ifdef __linux__
        int ne = epoll_wait(et, &out, 1, 0);
        unsigned int edge = ne > 0 ? out.events : 0;
        int nl = epoll_wait(lt, &out, 1, 0);
        unsigned int level = nl > 0 ? out.events : 0;
        printf("W\tstep=%d\tread %lld\twriter poll=0x%x edge=%d(0x%x) level=%d(0x%x)\treader fionread=%d writer unacked=%d\t%s\n", step,
               readsofar, pr, ne, edge, nl, level, nread(s), unacked(c), wm);
#else
        int ne = kevent(kq, NULL, 0, &got, 1, &zero);
        long long edata = ne > 0 ? (long long)got.data : -1;
        int nl = kevent(kl, NULL, 0, &got, 1, &zero);
        long long ldata = nl > 0 ? (long long)got.data : -1;
        printf("W\tstep=%d\tread %lld\twriter poll=0x%x clear=%d(data=%lld) level=%d(data=%lld)\treader fionread=%d writer unacked=%d\t%s\n",
               step, readsofar, pr, ne, edata, nl, ldata, nread(s), unacked(c), wm);
#endif
        if (pr & POLLOUT) writable_steps++;
    }
    ssize_t w = write(c, payload, 1 << 20);
    printf("W\tstep=%d\tthen a write of 1 MiB answered %zd%s%s\n", step, w, w < 0 ? " " : "", w < 0 ? errname(errno) : "");
    free(buf);
    close(c);
    close(s);
#ifdef __linux__
    close(et);
    close(lt);
#else
    close(kq);
    close(kl);
#endif
}

// Brings a fresh connection to `state`. *s is the socket under observation;
// *p its peer, or -1 once closed.
static void build(const char *state, int *s, int *p)
{
    char buf[4096];
    make_pair(NULL, s, p);
    if (!strcmp(state, "IDLE")) return;
    if (!strcmp(state, "DATA_IN"))
    {
        if (write(*p, payload, 1000) != 1000) die("DATA_IN write");
    }
    else if (!strcmp(state, "SNDFULL") || !strcmp(state, "SNDFULL_RST"))
    {
        long long calls, fs;
        int e;
        fill(*s, 65536, &calls, &fs, &e);
        if (!strcmp(state, "SNDFULL_RST"))
        {
            usleep(20000);
            close(*p);
            *p = -1;
        }
    }
    else if (!strcmp(state, "FIN"))
    {
        close(*p);
        *p = -1;
    }
    else if (!strcmp(state, "FIN_DATA") || !strcmp(state, "FIN_DRAINED"))
    {
        if (write(*p, payload, 1000) != 1000) die("FIN_DATA write");
        close(*p);
        *p = -1;
        if (!strcmp(state, "FIN_DRAINED"))
        {
            usleep(20000);
            if (read(*s, buf, sizeof buf) != 1000) die("FIN_DRAINED read");
        }
    }
    else if (!strcmp(state, "RST"))
    {
        if (write(*s, payload, 1000) != 1000) die("RST write");
        usleep(20000);
        close(*p);
        *p = -1;
    }
    else if (!strcmp(state, "RST_DATA"))
    {
        if (write(*p, payload, 1000) != 1000) die("RST_DATA write p");
        if (write(*s, payload, 1000) != 1000) die("RST_DATA write s");
        usleep(20000);
        close(*p);
        *p = -1;
    }
    else if (!strcmp(state, "LINGER0"))
    {
        struct linger lg = {1, 0};
        if (setsockopt(*p, SOL_SOCKET, SO_LINGER, &lg, sizeof lg) < 0) die("SO_LINGER");
        close(*p);
        *p = -1;
    }
    else if (!strcmp(state, "FIN_WRITTEN"))
    {
        close(*p);
        *p = -1;
        usleep(20000);
        if (write(*s, payload, 100) != 100) die("FIN_WRITTEN write");
    }
    else
    {
        fprintf(stderr, "unknown state %s\n", state);
        exit(2);
    }
    usleep(20000);
}


// One edge-triggered registration of `fd` for `what` ('r', 'w', or 'b' for
// both), on a fresh epoll instance or kqueue.
static int edge_register(int fd, char what)
{
#ifdef __linux__
    int ep = epoll_create1(0);
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLET | (what == 'r' ? EPOLLIN : what == 'w' ? EPOLLOUT : (EPOLLIN | EPOLLOUT));
    if (epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev) < 0) die("epoll_ctl edge");
    return ep;
#else
    int kq = kqueue();
    struct kevent ev;
    if (what != 'w')
    {
        EV_SET(&ev, fd, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
        if (kevent(kq, &ev, 1, NULL, 0, NULL) < 0) die("kevent edge");
    }
    if (what != 'r')
    {
        EV_SET(&ev, fd, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
        if (kevent(kq, &ev, 1, NULL, 0, NULL) < 0) die("kevent edge");
    }
    return kq;
#endif
}

// What a zero-timeout wait on `q` reports, after a 20 ms sleep.
static void edge_wait(int q, char *out, size_t len)
{
    usleep(20000);
#ifdef __linux__
    struct epoll_event got[2];
    int n = epoll_wait(q, got, 2, 0);
    if (n <= 0)
        snprintf(out, len, "none");
    else
        snprintf(out, len, "0x%x", got[0].events);
#else
    struct kevent got[2];
    struct timespec zero = {0, 0};
    int n = kevent(q, NULL, 0, got, 2, &zero);
    out[0] = 0;
    if (n <= 0) snprintf(out, len, "none");
    for (int i = 0; i < n; i++)
    {
        size_t used = strlen(out);
        snprintf(out + used, len - used, "%s%s(data=%lld%s)", i ? " " : "", got[i].filter == EVFILT_READ ? "READ" : "WRITE",
                 (long long)got[i].data, (got[i].flags & EV_EOF) ? " EOF" : "");
    }
#endif
}

static void section_e(void)
{
    char out[256], buf[65536];
    int c, s;

    make_pair(NULL, &c, &s);
    int q = edge_register(c, 'r');
    edge_wait(q, out, sizeof out);
    printf("E1\tafter ADD\t%s\n", out);
    if (write(s, payload, 100) != 100) die("E1 write");
    edge_wait(q, out, sizeof out);
    printf("E1\tpeer wrote 100\t%s\n", out);
    if (write(s, payload, 100) != 100) die("E1 write");
    edge_wait(q, out, sizeof out);
    printf("E1\tpeer wrote 100 more\t%s\n", out);
    if (read(c, buf, 50) != 50) die("E1 read");
    edge_wait(q, out, sizeof out);
    printf("E1\tread 50 of 200\t%s\n", out);
    edge_wait(q, out, sizeof out);
    printf("E1\tnothing\t%s\n", out);
    close(q);
    close(c);
    close(s);

    make_pair(NULL, &c, &s);
    q = edge_register(c, 'w');
    edge_wait(q, out, sizeof out);
    printf("E2\tafter ADD\t%s\n", out);
    if (write(c, payload, 1000) != 1000) die("E2 write");
    edge_wait(q, out, sizeof out);
    printf("E2\twrote 1000\t%s\n", out);
    if (read(s, buf, 1000) != 1000) die("E2 read");
    edge_wait(q, out, sizeof out);
    printf("E2\tpeer read 1000\t%s\n", out);
    long long calls, fs;
    int e;
    long long total = fill(c, 65536, &calls, &fs, &e);
    edge_wait(q, out, sizeof out);
    printf("E2\tfilled %lld to %s\t%s\n", total, errname(e), out);
    long long drained = 0;
    int after = -1;
    while (drained < total && after != 0)
    {
        ssize_t r = read(s, buf, 4096);
        if (r <= 0) break;
        drained += r;
        edge_wait(q, out, sizeof out);
        if (strcmp(out, "none") != 0 || after >= 0)
        {
            printf("E2\tpeer has read %lld\t%s\n", drained, out);
            after = after < 0 ? 1 : after - 1;
        }
    }
    close(q);
    close(c);
    close(s);

#ifdef __linux__
    make_pair(NULL, &c, &s);
    q = edge_register(c, 'b');
    edge_wait(q, out, sizeof out);
    printf("E3\tafter ADD\t%s\n", out);
    if (write(s, payload, 100) != 100) die("E3 write");
    edge_wait(q, out, sizeof out);
    printf("E3\tpeer wrote 100\t%s\n", out);
    close(q);
    close(c);
    close(s);
#endif
}

static void result(char *out, size_t len, ssize_t r, int e, int sig)
{
    if (r < 0)
        snprintf(out, len, "-1 %s%s", errname(e), sig ? " SIGPIPE" : "");
    else
        snprintf(out, len, "%zd%s", r, sig ? " SIGPIPE" : "");
}

static void section_s(void)
{
    static const char *states[] = {"IDLE", "DATA_IN", "SNDFULL", "FIN", "FIN_DATA", "FIN_DRAINED",
                                   "RST", "RST_DATA", "LINGER0", "FIN_WRITTEN", "SNDFULL_RST"};
    static const char *ops[] = {"none", "read", "read0", "recv", "recv0", "peek", "write", "write0", "send", "send0", "sendnosig", "dontwait"};
    char buf[4096];
    for (size_t si = 0; si < sizeof states / sizeof states[0]; si++)
    {
        for (size_t oi = 0; oi < sizeof ops / sizeof ops[0]; oi++)
        {
            int s, p;
            build(states[si], &s, &p);
            char before[400], after[400], res[3][64];
            readiness(s, before, sizeof before);
            const char *op = ops[oi];
            int count = 3;
            for (int i = 0; i < 3; i++) res[i][0] = 0;
            if (!strcmp(op, "none"))
            {
                count = 1;
                int soerr = getint(s, SOL_SOCKET, SO_ERROR);
                snprintf(res[0], sizeof res[0], "SO_ERROR=%s", soerr == 0 ? "0" : soerr > 0 ? errname(soerr) : "getsockopt failed");
            }
            else if (!strcmp(op, "dontwait"))
            {
                count = 2;
                set_nonblocking(s, 0);
                int before_sig = sigpipes;
                errno = 0;
                ssize_t r = recv(s, buf, sizeof buf, MSG_DONTWAIT);
                result(res[0], sizeof res[0], r, errno, sigpipes != before_sig);
                before_sig = sigpipes;
                errno = 0;
                r = send(s, payload, 65536, MSG_DONTWAIT);
                result(res[1], sizeof res[1], r, errno, sigpipes != before_sig);
                set_nonblocking(s, 1);
            }
            else
            {
                for (int i = 0; i < 3; i++)
                {
                    int before_sig = sigpipes;
                    ssize_t r;
                    errno = 0;
                    if (!strcmp(op, "read")) r = read(s, buf, sizeof buf);
                    else if (!strcmp(op, "read0")) r = read(s, buf, 0);
                    else if (!strcmp(op, "recv")) r = recv(s, buf, sizeof buf, 0);
                    else if (!strcmp(op, "recv0")) r = recv(s, buf, 0, 0);
                    else if (!strcmp(op, "peek")) r = i < 2 ? recv(s, buf, sizeof buf, MSG_PEEK) : read(s, buf, sizeof buf);
                    else if (!strcmp(op, "write")) r = write(s, payload, 100);
                    else if (!strcmp(op, "write0")) r = write(s, payload, 0);
                    else if (!strcmp(op, "send")) r = send(s, payload, 100, 0);
                    else if (!strcmp(op, "send0")) r = send(s, payload, 0, 0);
                    else if (!strcmp(op, "sendnosig")) r = send(s, payload, 100, MSG_NOSIGNAL);
                    else
                    {
                        fprintf(stderr, "unknown op %s\n", op);
                        exit(2);
                    }
                    result(res[i], sizeof res[i], r, errno, sigpipes != before_sig);
                    usleep(20000);
                }
            }
            readiness(s, after, sizeof after);
            printf("S\t%s\t%s\tbefore[%s]\t", states[si], op, before);
            for (int i = 0; i < count; i++) printf("%s%s", i ? " ; " : "", res[i]);
            printf("\tafter[%s]\n", after);
            fflush(stdout);
            close(s);
            if (p >= 0) close(p);
        }
    }
}

int main(int argc, char **argv)
{
    const char *sections = argc > 1 ? argv[1] : "SWEC";
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_sigpipe;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGPIPE, &sa, NULL) < 0) die("sigaction");
    for (size_t i = 0; i < sizeof payload; i++) payload[i] = (char)(i * 31 + 7);
#ifdef __linux__
    {
        FILE *f;
        char line[256];
        const char *files[] = {"/proc/sys/net/ipv4/tcp_wmem", "/proc/sys/net/ipv4/tcp_rmem", "/proc/sys/net/core/wmem_default",
                               "/proc/sys/net/core/rmem_default", "/proc/sys/net/core/wmem_max", "/proc/sys/net/core/rmem_max",
                               "/proc/sys/net/ipv4/tcp_notsent_lowat", "/proc/sys/net/ipv4/tcp_moderate_rcvbuf",
                               "/proc/sys/net/ipv4/tcp_adv_win_scale", "/proc/sys/net/ipv4/tcp_autocorking"};
        for (size_t i = 0; i < sizeof files / sizeof files[0]; i++)
        {
            f = fopen(files[i], "r");
            if (f && fgets(line, sizeof line, f))
            {
                line[strcspn(line, "\n")] = 0;
                printf("#\t%s = %s\n", files[i], line);
            }
            if (f) fclose(f);
        }
        f = fopen("/proc/sys/kernel/osrelease", "r");
        if (f && fgets(line, sizeof line, f)) printf("#\tkernel %s", line);
        if (f) fclose(f);
    }
#endif
#ifdef NO_MSG_NOSIGNAL
    printf("#\tMSG_NOSIGNAL is not defined here; sendnosig sends with flags 0\n");
#endif
    printf("#\tMSG_NOSIGNAL=0x%x MSG_DONTWAIT=0x%x MSG_PEEK=0x%x\n", MSG_NOSIGNAL, MSG_DONTWAIT, MSG_PEEK);
    if (strchr(sections, 'S')) section_s();
    if (strchr(sections, 'W'))
    {
        section_w(4096);
        section_w(65536);
    }
    if (strchr(sections, 'E')) section_e();
    if (strchr(sections, 'C')) section_c();
    return 0;
}
