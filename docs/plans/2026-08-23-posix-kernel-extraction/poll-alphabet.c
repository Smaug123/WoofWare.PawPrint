// poll(2)'s event alphabet on this kernel: which bits <poll.h> defines, and
// what `revents` comes back for *every* 16-bit `events` value, over each object
// and state WoofWare.PosixKernel models.
//
// For each state the probe polls all 65536 request masks at timeout 0, and
// reports:
//   - the answer to `events = 0`, to each single bit, and to `events = 0xFFFF`;
//   - how many of the 65536 answers a candidate rule predicts wrongly:
//       KERNEL  revents == full & (events | POLLERR | POLLHUP | POLLNVAL),
//               where `full` is the answer to 0xFFFF (Linux's `do_pollfd`
//               shape: one level, masked by the request plus the output-only
//               bits);
//       ADDITIVE revents == answer(0) | OR of answer(bit) over bits in events
//               (the request's bits act independently);
//   - how many answers had `rv` disagree with `revents != 0`, and how many
//     failed outright (with the errno).
//
// On Linux it also checks the one case in which a socket's own poll handler
// adds a bit outside <poll.h> to its mask: POLL_BUSY_LOOP (0x8000) on a socket
// with SO_BUSY_POLL set.
//
// Build and run, from this directory:
//   Darwin: cc -Wall -o /tmp/pa poll-alphabet.c && /tmp/pa
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c "apt-get update -qq
//             && apt-get install -y -qq gcc libc6-dev && gcc -Wall -o /tmp/pa
//             /probe/poll-alphabet.c && /tmp/pa"
//
// Measured 2026-09-23 on Darwin 25.6.0 arm64 and Linux 6.18.5 aarch64 (glibc
// 2.41); the results are summarised beside `UnixPoll.poll`.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static void header_bits(void)
{
    printf("== <poll.h> ==\n");
#define SHOW(name) printf("  %-16s 0x%04x\n", #name, (unsigned)(unsigned short)(name))
#ifdef POLLIN
    SHOW(POLLIN);
#endif
#ifdef POLLPRI
    SHOW(POLLPRI);
#endif
#ifdef POLLOUT
    SHOW(POLLOUT);
#endif
#ifdef POLLERR
    SHOW(POLLERR);
#endif
#ifdef POLLHUP
    SHOW(POLLHUP);
#endif
#ifdef POLLNVAL
    SHOW(POLLNVAL);
#endif
#ifdef POLLRDNORM
    SHOW(POLLRDNORM);
#endif
#ifdef POLLRDBAND
    SHOW(POLLRDBAND);
#endif
#ifdef POLLWRNORM
    SHOW(POLLWRNORM);
#endif
#ifdef POLLWRBAND
    SHOW(POLLWRBAND);
#endif
#ifdef POLLMSG
    SHOW(POLLMSG);
#endif
#ifdef POLLREMOVE
    SHOW(POLLREMOVE);
#endif
#ifdef POLLRDHUP
    SHOW(POLLRDHUP);
#endif
#ifdef POLLFREE
    SHOW(POLLFREE);
#endif
#ifdef POLL_BUSY_LOOP
    SHOW(POLL_BUSY_LOOP);
#endif
#ifdef POLLEXTEND
    SHOW(POLLEXTEND);
#endif
#ifdef POLLATTRIB
    SHOW(POLLATTRIB);
#endif
#ifdef POLLNLINK
    SHOW(POLLNLINK);
#endif
#ifdef POLLWRITE
    SHOW(POLLWRITE);
#endif
#ifdef POLLSTANDARD
    SHOW(POLLSTANDARD);
#endif
#undef SHOW
}

static unsigned short answers[65536];
static int rvs[65536];
static int errs[65536];

static void sweep(int fd, const char *label)
{
    for (unsigned e = 0; e < 65536; e++)
    {
        struct pollfd p;
        p.fd = fd;
        p.events = (short)e;
        p.revents = 0;
        errno = 0;
        int rv = poll(&p, 1, 0);
        rvs[e] = rv;
        errs[e] = rv < 0 ? errno : 0;
        answers[e] = (unsigned short)p.revents;
    }

    unsigned short full = answers[0xFFFF];
    printf("-- %s\n", label);
    printf("   events=0x0000 -> 0x%04x   events=0xffff -> 0x%04x\n", answers[0], full);
    printf("   single bits:");
    for (int b = 0; b < 16; b++)
        printf(" %04x->%04x", 1u << b, answers[1u << b]);
    printf("\n");

    int kernelMiss = 0, additiveMiss = 0, rvMiss = 0, failures = 0, firstErrno = 0;
    int shown = 0;
    for (unsigned e = 0; e < 65536; e++)
    {
        if (rvs[e] < 0)
        {
            if (failures++ == 0)
                firstErrno = errs[e];
            continue;
        }
        if (rvs[e] != (answers[e] != 0 ? 1 : 0))
            rvMiss++;
        unsigned short kernel = full & (unsigned short)(e | POLLERR | POLLHUP | POLLNVAL);
        if (kernel != answers[e])
        {
            if (kernelMiss++ < 4)
                printf("   KERNEL miss: events=0x%04x revents=0x%04x predicted=0x%04x\n", e, answers[e], kernel);
        }
        unsigned short additive = answers[0];
        for (int b = 0; b < 16; b++)
            if (e & (1u << b))
                additive |= answers[1u << b];
        if (additive != answers[e])
        {
            if (shown++ < 4)
                printf("   ADDITIVE miss: events=0x%04x revents=0x%04x predicted=0x%04x\n", e, answers[e], additive);
            additiveMiss++;
        }
    }
    printf("   KERNEL misses %d, ADDITIVE misses %d, rv misses %d, failures %d (first errno %d)\n",
           kernelMiss, additiveMiss, rvMiss, failures, firstErrno);
}

static int listener(int family, struct sockaddr_storage *out, int backlog)
{
    int fd = socket(family, SOCK_STREAM, 0);
    struct sockaddr_storage a;
    memset(&a, 0, sizeof a);
    socklen_t len;
    if (family == AF_INET)
    {
        struct sockaddr_in *v4 = (struct sockaddr_in *)&a;
        v4->sin_family = AF_INET;
        v4->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        len = sizeof *v4;
    }
    else
    {
        struct sockaddr_in6 *v6 = (struct sockaddr_in6 *)&a;
        v6->sin6_family = AF_INET6;
        v6->sin6_addr = in6addr_loopback;
        len = sizeof *v6;
    }
    if (bind(fd, (struct sockaddr *)&a, len) < 0)
        perror("bind");
    if (listen(fd, backlog) < 0)
        perror("listen");
    socklen_t sl = sizeof *out;
    getsockname(fd, (struct sockaddr *)out, &sl);
    return fd;
}

static socklen_t addr_len(int family)
{
    return family == AF_INET ? sizeof(struct sockaddr_in) : sizeof(struct sockaddr_in6);
}

static void set_nonblock(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

static void socket_states(int family, const char *fam)
{
    char label[128];
    struct sockaddr_storage la;
    int l = listener(family, &la, 8);

    int fresh = socket(family, SOCK_STREAM, 0);
    snprintf(label, sizeof label, "%s TCP fresh idle", fam);
    sweep(fresh, label);

    snprintf(label, sizeof label, "%s TCP listening, queue empty", fam);
    sweep(l, label);

    int c = socket(family, SOCK_STREAM, 0);
    if (connect(c, (struct sockaddr *)&la, addr_len(family)) < 0)
        perror("connect");
    usleep(100000);
    snprintf(label, sizeof label, "%s TCP listening, queue nonempty", fam);
    sweep(l, label);
    snprintf(label, sizeof label, "%s TCP connected client, peer queued", fam);
    sweep(c, label);
    int s = accept(l, NULL, NULL);
    snprintf(label, sizeof label, "%s TCP accepted server end, peer alive", fam);
    sweep(s, label);
    snprintf(label, sizeof label, "%s TCP client end, peer accepted", fam);
    sweep(c, label);
    close(c);
    usleep(100000);
    snprintf(label, sizeof label, "%s TCP server end, peer closed", fam);
    sweep(s, label);
    close(s);

    // A non-blocking connect that completed, not yet reported by a second
    // connect: Linux's EstablishedPendingReport.
    int nb = socket(family, SOCK_STREAM, 0);
    set_nonblock(nb);
    connect(nb, (struct sockaddr *)&la, addr_len(family));
    usleep(100000);
    snprintf(label, sizeof label, "%s TCP non-blocking connect completed, unreported", fam);
    sweep(nb, label);
    int nbs = accept(l, NULL, NULL);
    close(nbs);
    close(nb);

    // A refused non-blocking connect, before and after a second connect
    // delivers the refusal.
    struct sockaddr_storage da;
    int dead = listener(family, &da, 1);
    close(dead);
    int r = socket(family, SOCK_STREAM, 0);
    set_nonblock(r);
    connect(r, (struct sockaddr *)&da, addr_len(family));
    usleep(100000);
    snprintf(label, sizeof label, "%s TCP refused, pending delivery", fam);
    sweep(r, label);
    int rc = connect(r, (struct sockaddr *)&da, addr_len(family));
    printf("   (delivering connect: rc=%d errno=%d)\n", rc, errno);
    snprintf(label, sizeof label, "%s TCP refused, delivered", fam);
    sweep(r, label);
    close(r);
    close(l);

    int u = socket(family, SOCK_DGRAM, 0);
    snprintf(label, sizeof label, "%s UDP fresh idle", fam);
    sweep(u, label);
    struct sockaddr_storage ua;
    memcpy(&ua, &la, sizeof ua);
    if (connect(u, (struct sockaddr *)&ua, addr_len(family)) < 0)
        perror("udp connect");
    snprintf(label, sizeof label, "%s UDP connected (peer set)", fam);
    sweep(u, label);
    close(u);
}

int main(void)
{
    header_bits();
    printf("== sweeps (all 65536 request masks, timeout 0) ==\n");

    socket_states(AF_INET, "INET");
    socket_states(AF_INET6, "INET6");

    int us = socket(AF_UNIX, SOCK_STREAM, 0);
    sweep(us, "UNIX stream fresh idle");
    int ud = socket(AF_UNIX, SOCK_DGRAM, 0);
    sweep(ud, "UNIX dgram fresh idle");
    int ur = socket(AF_UNIX, SOCK_RAW, 0);
    if (ur >= 0)
        sweep(ur, "UNIX raw fresh idle");
    else
        printf("-- UNIX raw: socket() errno %d\n", errno);
    int usp = socket(AF_UNIX, SOCK_SEQPACKET, 0);
    if (usp >= 0)
        sweep(usp, "UNIX seqpacket fresh idle");
    else
        printf("-- UNIX seqpacket: socket() errno %d\n", errno);

    char tmpl[] = "/tmp/pollalphaXXXXXX";
    int rf = mkstemp(tmpl);
    write(rf, "hello", 5);
    sweep(rf, "regular file O_RDWR, at EOF");
    lseek(rf, 0, SEEK_SET);
    sweep(rf, "regular file O_RDWR, at 0");
    int ro = open(tmpl, O_RDONLY);
    sweep(ro, "regular file O_RDONLY");
    int wo = open(tmpl, O_WRONLY);
    sweep(wo, "regular file O_WRONLY");
    int emptyf = open(tmpl, O_RDWR | O_TRUNC);
    sweep(emptyf, "regular file, empty");
    unlink(tmpl);
    int dir = open("/tmp", O_RDONLY);
    sweep(dir, "directory O_RDONLY");

    int in[2];
    pipe(in);
    close(in[1]);
    sweep(in[0], "stdin shape: pipe read end, writer closed");
    int out[2];
    pipe(out);
    sweep(out[1], "stdout shape: pipe write end, reader alive");

#ifdef __linux__
    int port = epoll_create1(0);
    sweep(port, "epoll fd, no registrations");
    int w = socket(AF_INET, SOCK_DGRAM, 0);
    struct epoll_event ev;
    ev.events = EPOLLOUT | EPOLLET;
    ev.data.u64 = 1;
    epoll_ctl(port, EPOLL_CTL_ADD, w, &ev);
    sweep(port, "epoll fd, one ready registration");
#else
    int port = kqueue();
    sweep(port, "kqueue fd, no registrations");
    int w = socket(AF_INET, SOCK_DGRAM, 0);
    struct kevent kev;
    EV_SET(&kev, w, EVFILT_WRITE, EV_ADD, 0, 0, NULL);
    kevent(port, &kev, 1, NULL, 0, NULL);
    sweep(port, "kqueue fd, one ready registration");
#endif

#ifdef __linux__
    {
        int b = socket(AF_INET, SOCK_DGRAM, 0);
        int usec = 50;
        int rc = setsockopt(b, SOL_SOCKET, SO_BUSY_POLL, &usec, sizeof usec);
        printf("-- UDP with SO_BUSY_POLL=50 (setsockopt rc=%d errno=%d)\n", rc, rc < 0 ? errno : 0);
        struct pollfd p;
        p.fd = b;
        p.events = (short)(0x8000 | POLLOUT);
        p.revents = 0;
        rc = poll(&p, 1, 0);
        printf("   events=0x8004 -> rv=%d revents=0x%04x\n", rc, (unsigned short)p.revents);
        close(b);
    }
#endif

    sweep(4000, "never-opened fd 4000");
    int closed = dup(0);
    close(closed);
    sweep(closed, "closed fd");
    sweep(-1, "fd -1");
    return 0;
}
