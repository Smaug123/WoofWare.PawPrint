// kevent(2) changelist registration on Darwin: EVFILT_READ and EVFILT_WRITE on
// sockets, what each producer activates, what a delivered event carries, how
// EV_RECEIPT reports, the delivery order, a re-ADD, EV_DELETE, other kinds of
// descriptor, and what a close does to a registration.
//
// Every poll below is `kevent(kq, NULL, 0, out, n, {0,0})` made 30 ms after the
// last action, so that loopback has settled; every row is about state, not
// timing. An event prints as `name:FILTER flags=0x.. fflags=.. data=..
// udata=0x..`, the ident replaced by the name the probe gave that descriptor.
//
// Sections (each P and O row run twice: mode=clear registers EV_ADD|EV_CLEAR,
// mode=level registers EV_ADD alone):
//   P   per producer:
//       P1  a listener: READ and WRITE registered, then two connections queued,
//           one accepted, the other accepted, and a connection accepted before
//           the poll;
//       P2  a connect completing (registered on the idle socket first), and an
//           ADD on an already-connected socket;
//       P3  a connect refused; the SO_ERROR read; a re-ADD after it;
//       P4  the peer writing 5 then 3 bytes, the reader taking all, and a write
//           read before the poll;
//       P5  the peer shutting down its write side;
//       P6  the peer closing;
//       P7  the peer closing with unread data (a reset);
//       P8  write space: the client's send buffer filled, then the peer reading
//           one byte, then everything;
//       P9  a UDP socket, fresh and connected;
//       P10 READ registered on an idle socket that then listens and is
//           connected to;
//       P11 the listener after the accepted socket's peer state changes (does
//           anything on an accepted connection signal the listener?).
//   R   EV_RECEIPT and errors: receipts with room, with a short eventlist and
//       with none; EV_DELETE of nothing, with and without EV_RECEIPT, with and
//       without room, and whether the changes after a failing one apply;
//       registration and events in one call; a closed ident; odd idents.
//   O   delivery order: three listeners activated in all six orders; READ and
//       WRITE of one socket activated by one event, registered in both orders;
//       a queued registration activated again; level re-delivery with room for
//       one; truncation; an ADD of something already ready.
//   D   a re-ADD of an existing (ident, filter): delivered then re-added, re-
//       added while queued, EV_CLEAR removed and added, on nothing ready; and
//       an EV_DELETE of a queued registration.
//   F   READ and WRITE (EV_ADD|EV_CLEAR|EV_RECEIPT) on descriptors that are not
//       sockets: regular files, a directory, pipe ends, a kqueue, /dev/null.
//   G   close: a registration through a descriptor that is closed while a dup
//       lives, through the dup instead, a reused number, a queued registration,
//       a socket registered in two kqueues, dup2 over a registered number, and
//       an EV_DELETE through a closed descriptor.
//
//   X   SO_SNDBUF and SO_SNDLOWAT of each socket state; which idents an ADD
//       and a DELETE answer which way (k * 2^32 + low, six highs by five lows);
//       EV_DELETE of nothing on every kind of descriptor; a failing change
//       after the receipts have filled the eventlist; the flags an event
//       reports after a re-ADD that adds EV_RECEIPT; an EV_DELETE whose
//       ident's low 32 bits name a registered descriptor.
//   Y   a kqueue drained under a sleeper, with registrations; a sleeper woken
//       by an ADD from another thread; a changelist whose second entry is
//       unreadable (the array ends at a PROT_NONE page).
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -O1 -o /tmp/kr kevent-register.c && /tmp/kr
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64 (xnu-13432.1.9); ten runs (four
// of this version, six of one without O6, P3b and P12) gave identical output
// except P8, whose send-buffer sizes follow TCP autotuning and
// vary from run to run (P8 is not modelled: this kernel has no send buffer).
// kevent-register.darwin-27.0.txt is one run's full output. What it shows:
//
//   Readiness, per socket state (both filters, EV_CLEAR or not):
//     listener          READ when its accept queue is non-empty, data = the
//                       number queued; WRITE never.
//     idle TCP socket   neither (bound, unbound, IPv4, IPv6).
//     connected         WRITE, data = the send buffer's free space (146988 on
//                       IPv4 loopback and 146808 on IPv6 here, which is
//                       SO_SNDBUF after the handshake rounded the 131072
//                       sysctl up to whole segments); READ only with data
//                       waiting, data = bytes waiting.
//     peer's FIN in     READ with EV_EOF, fflags 0, data = bytes waiting (0
//     (shutdown/close)  when none); WRITE as when connected, no EV_EOF.
//     refused           READ and WRITE both EV_EOF with fflags = the pending
//                       error (61, ECONNREFUSED) until SO_ERROR takes it, 0
//                       after; READ data 0, WRITE data 2048. A blocking
//                       refusal leaves the same with fflags 0.
//     reset (P7)        both EV_EOF, fflags 54 (ECONNRESET).
//     UDP               WRITE, data 9216; READ only with a datagram waiting.
//   Activation, under EV_CLEAR: each producer activates only the filters it
//     wakes, and only while the filter is then ready. A connection queued
//     activates the listener's READ (again for a second while the first is
//     unaccepted: the edge is the signal, not a change of state); an accept
//     activates nothing. A connect completing activates the client's WRITE; a
//     refusal its WRITE and then its READ; the peer's FIN its READ alone; a
//     peer writing its READ. An SO_ERROR read activates nothing. What happens
//     to an accepted connection never activates its listener (P11).
//   Delivery: an activated registration is queued once, at the tail, and keeps
//     its place when activated again (O3, D7). A wait walks the queue in order
//     and re-reads each filter: one no longer ready is dropped, and nothing is
//     reported for it (P1.8, P4.7); one that is reported leaves the queue under
//     EV_CLEAR, and without EV_CLEAR goes back to the tail behind what the walk
//     did not reach (O4, O5), so a level registration is reported on every
//     wait while it stays ready. Room for fewer stops the walk; the rest stay
//     queued in order (R14, O5). One event that activates several
//     registrations queues WRITE before READ (O2, O2b: a connect queues the
//     client's WRITE before the listener's READ), and of one socket's
//     registrations for one filter through several descriptors, the newest
//     first (O6). An event's flags are the registration's own from its first
//     ADD (EV_ADD, EV_CLEAR, EV_RECEIPT as given), with EV_EOF added; its
//     ident is the descriptor number, udata the latest ADD's.
//   ADD of an (ident, filter) already registered keeps its flags (EV_CLEAR can
//     be neither added nor removed: D3, D4; nor EV_RECEIPT: X7), replaces its
//     udata, and activates it if the filter is ready (D1, D2, P3.5), without
//     moving it if it is already queued. An ADD of a ready filter queues it at
//     once (O5, R8). EV_DELETE removes the registration and its queue entry
//     (D6).
//   Receipts and errors: a change with EV_RECEIPT, or one that fails, is echoed
//     into the eventlist with EV_ERROR added to its flags and data = 0 or the
//     errno, every other field as passed (R15), while there is room. A call
//     that echoes any change reports no events and does not wait, whatever is
//     queued and whatever the timeout (R1, R6, R9, X5, X6). With no room left a
//     receipt is dropped and its change still applies (R2, R3, R12); a failure
//     with no room ends the call with -1 and that errno, the changes before it
//     applied and those after it not (R4, R5, R7, X4). An unreadable change
//     ends the call with EFAULT, the readable ones before it applied (Y3).
//   Errors: EV_DELETE of something not registered is ENOENT whatever the ident
//     names, a closed descriptor and every non-socket included (R4, R10, X2,
//     X3). EV_ADD reads the ident's low 32 bits as a descriptor: not open
//     (negative included) is EBADF, and an open one with any high bit set is
//     EINVAL (X2, every high by every low). An ADD on a regular file, a pipe end
//     or a kqueue's READ registers; a directory, /dev/null and a kqueue's WRITE
//     are EINVAL (F). EV_ONESHOT, EV_DISPATCH, EV_DISABLE, NOTE_LOWAT and
//     EVFILT_EXCEPT register; EV_ENABLE or EV_DISABLE alone, no flags and
//     EV_ADD|EV_DELETE on nothing registered are ENOENT; filter -100 is EINVAL
//     (R16).
//   Close: closing a descriptor removes every registration made through it, in
//     every kqueue, queued or not, even while a dup keeps the socket open, and
//     a new socket on the same number inherits nothing (G1-G6, dup2 included);
//     a registration made through the dup survives (G2, G5).
//   Drain: on a kqueue a close has drained under a sleeper (kqueue-kevent.c,
//     section E), a wait that gets to the kqueue is EBADF even with a ready
//     registration, while changes still apply, and a call that echoes one
//     succeeds (Y1, three trials).
//   A sleeper is woken by an ADD of a ready filter from another thread (Y2,
//     three trials).
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static void settle(void) { sleep_ms(30); }

static const char *ename(int e)
{
    switch (e) {
    case 0: return "-";
    case EBADF: return "EBADF";
    case EINVAL: return "EINVAL";
    case EFAULT: return "EFAULT";
    case EINTR: return "EINTR";
    case ENOENT: return "ENOENT";
    case EINPROGRESS: return "EINPROGRESS";
    case ECONNREFUSED: return "ECONNREFUSED";
    case EAGAIN: return "EAGAIN";
    case ENOTSUP: return "ENOTSUP";
    case EPIPE: return "EPIPE";
    case ECONNRESET: return "ECONNRESET";
    default: return strerror(e);
    }
}

// ---- naming -------------------------------------------------------------

static struct { int fd; char name[32]; } names[256];
static int nnames;

static void name_fd(int fd, const char *name)
{
    for (int i = 0; i < nnames; i++) {
        if (names[i].fd == fd) {
            snprintf(names[i].name, sizeof names[i].name, "%s", name);
            return;
        }
    }
    if (nnames < 256) {
        names[nnames].fd = fd;
        snprintf(names[nnames].name, sizeof names[nnames].name, "%s", name);
        nnames++;
    }
}

static const char *ident_name(uintptr_t ident)
{
    static char buf[64];
    for (int i = 0; i < nnames; i++) {
        if ((uintptr_t)(intptr_t)names[i].fd == ident) return names[i].name;
    }
    snprintf(buf, sizeof buf, "ident%llu", (unsigned long long)ident);
    return buf;
}

static const char *fname(int16_t filter)
{
    switch (filter) {
    case EVFILT_READ: return "READ";
    case EVFILT_WRITE: return "WRITE";
    default: return "OTHER";
    }
}

static void print_events(const struct kevent *ev, int n)
{
    printf("[");
    for (int i = 0; i < n; i++) {
        printf("%s%s:%s flags=0x%x fflags=%u data=%lld udata=0x%llx", i ? "; " : "", ident_name(ev[i].ident),
               fname(ev[i].filter), ev[i].flags, ev[i].fflags, (long long)ev[i].data,
               (unsigned long long)(uintptr_t)ev[i].udata);
    }
    printf("]");
}

// One poll: no changes, room for `n`, timeout {0,0}.
static void show_n(const char *label, int kq, int n)
{
    struct kevent out[16];
    struct timespec zero = { 0, 0 };
    errno = 0;
    int rv = kevent(kq, NULL, 0, out, n, &zero);
    int err = rv < 0 ? errno : 0;
    printf("%s\trv=%d\t%s\t", label, rv, ename(err));
    print_events(out, rv > 0 ? rv : 0);
    printf("\n");
}

static void show(const char *label, int kq) { show_n(label, kq, 16); }

static struct kevent change(int fd, int16_t filter, uint16_t flags, uintptr_t udata)
{
    struct kevent k;
    EV_SET(&k, (uintptr_t)(intptr_t)fd, filter, flags, 0, 0, (void *)udata);
    return k;
}

// Apply `n` changes with room for `nevents`, timeout {0,0}, and print the result.
static void apply(const char *label, int kq, const struct kevent *changes, int n, int nevents)
{
    struct kevent out[16];
    struct timespec zero = { 0, 0 };
    errno = 0;
    int rv = kevent(kq, changes, n, out, nevents, &zero);
    int err = rv < 0 ? errno : 0;
    printf("%s\trv=%d\t%s\t", label, rv, ename(err));
    print_events(out, rv > 0 ? rv : 0);
    printf("\n");
}

// One change, no room for events: rv and errno only.
static int reg(int kq, int fd, int16_t filter, uint16_t flags, uintptr_t udata)
{
    struct kevent k = change(fd, filter, flags, udata);
    errno = 0;
    int rv = kevent(kq, &k, 1, NULL, 0, NULL);
    if (rv != 0) printf("#\treg %s:%s flags=0x%x rv=%d %s\n", ident_name((uintptr_t)fd), fname(filter), flags, rv, ename(errno));
    return rv;
}

// ---- sockets ------------------------------------------------------------

static int listener(int *port)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    a.sin_port = 0;
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) { perror("bind"); exit(1); }
    if (listen(s, 8) != 0) { perror("listen"); exit(1); }
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    *port = ntohs(a.sin_port);
    return s;
}

static void set_nonblock(int s)
{
    fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
}

static int connect_to(int s, int port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    a.sin_port = htons(port);
    errno = 0;
    int rv = connect(s, (struct sockaddr *)&a, sizeof a);
    return rv < 0 ? errno : 0;
}

// A non-blocking client connected to `port`.
static int client(int port)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    set_nonblock(s);
    int err = connect_to(s, port);
    if (err != 0 && err != EINPROGRESS) { printf("#\tconnect: %s\n", ename(err)); }
    return s;
}

static int accept_one(int l)
{
    int a = accept(l, NULL, NULL);
    if (a < 0) printf("#\taccept: %s\n", ename(errno));
    return a;
}

// A port with nothing listening on it.
static int closed_port(void)
{
    int port;
    int l = listener(&port);
    close(l);
    return port;
}

static int sndbuf(int s)
{
    int v = 0;
    socklen_t len = sizeof v;
    getsockopt(s, SOL_SOCKET, SO_SNDBUF, &v, &len);
    return v;
}

// A connected pair over loopback: `*c` the client, `*a` the accepted end.
static void pair(int *c, int *a)
{
    int port;
    int l = listener(&port);
    *c = client(port);
    settle();
    *a = accept_one(l);
    close(l);
}

// ---- P ------------------------------------------------------------------

static void section_p(uint16_t mode, const char *m)
{
    char label[128];
#define L(x) (snprintf(label, sizeof label, "P\t%s\t%s", m, x), label)
    uint16_t add = EV_ADD | mode;

    // P1
    {
        int port;
        int l = listener(&port);
        name_fd(l, "L");
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, add, 0x11);
        reg(kq, l, EVFILT_WRITE, add, 0x12);
        show(L("P1.0 fresh listener"), kq);
        int c1 = client(port);
        settle();
        show(L("P1.1 one connection queued"), kq);
        show(L("P1.2 again"), kq);
        int c2 = client(port);
        settle();
        show(L("P1.3 a second queued"), kq);
        show(L("P1.4 again"), kq);
        int a1 = accept_one(l);
        settle();
        show(L("P1.5 one accepted, one queued"), kq);
        show(L("P1.6 again"), kq);
        int a2 = accept_one(l);
        settle();
        show(L("P1.7 both accepted"), kq);
        int c3 = client(port);
        settle();
        int a3 = accept_one(l);
        settle();
        show(L("P1.8 connected and accepted before the poll"), kq);
        close(c1); close(c2); close(c3); close(a1); close(a2); close(a3); close(l); close(kq);
    }

    // P2
    {
        int port;
        int l = listener(&port);
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        name_fd(s, "S");
        int kq = kqueue();
        reg(kq, s, EVFILT_READ, add, 0x21);
        reg(kq, s, EVFILT_WRITE, add, 0x22);
        show(L("P2.0 idle socket"), kq);
        int err = connect_to(s, port);
        printf("P\t%s\tP2.1 connect\t%s\n", m, ename(err));
        settle();
        show(L("P2.2 connected"), kq);
        show(L("P2.3 again"), kq);
        printf("P\t%s\tP2.4 SO_SNDBUF=%d\n", m, sndbuf(s));
        int a = accept_one(l);
        settle();
        show(L("P2.5 after the peer accepted"), kq);
        int kq2 = kqueue();
        reg(kq2, s, EVFILT_READ, add, 0x23);
        reg(kq2, s, EVFILT_WRITE, add, 0x24);
        show(L("P2.6 ADD on a connected socket, new kqueue"), kq2);
        name_fd(a, "A");
        reg(kq2, a, EVFILT_WRITE, add, 0x25);
        show(L("P2.7 ADD WRITE on the accepted end"), kq2);
        printf("P\t%s\tP2.8 accepted SO_SNDBUF=%d\n", m, sndbuf(a));
        close(a); close(s); close(l); close(kq); close(kq2);
    }

    // P3
    {
        int port = closed_port();
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        name_fd(s, "S");
        int kq = kqueue();
        reg(kq, s, EVFILT_READ, add, 0x31);
        reg(kq, s, EVFILT_WRITE, add, 0x32);
        int err = connect_to(s, port);
        printf("P\t%s\tP3.0 connect\t%s\n", m, ename(err));
        settle();
        show(L("P3.1 refused"), kq);
        show(L("P3.2 again"), kq);
        int so_error = 0;
        socklen_t len = sizeof so_error;
        getsockopt(s, SOL_SOCKET, SO_ERROR, &so_error, &len);
        printf("P\t%s\tP3.3 SO_ERROR=%s\n", m, ename(so_error));
        show(L("P3.4 after the SO_ERROR read"), kq);
        reg(kq, s, EVFILT_READ, add, 0x33);
        reg(kq, s, EVFILT_WRITE, add, 0x34);
        show(L("P3.5 re-ADD after the SO_ERROR read"), kq);
        close(s); close(kq);
    }

    // P3b
    {
        int port = closed_port();
        int s = socket(AF_INET, SOCK_STREAM, 0);
        name_fd(s, "S");
        int err = connect_to(s, port);
        printf("P\t%s\tP3b.0 blocking connect\t%s\n", m, ename(err));
        int so_error = 0;
        socklen_t len = sizeof so_error;
        getsockopt(s, SOL_SOCKET, SO_ERROR, &so_error, &len);
        printf("P\t%s\tP3b.1 SO_ERROR=%s\n", m, ename(so_error));
        int kq = kqueue();
        reg(kq, s, EVFILT_READ, add, 0x35);
        reg(kq, s, EVFILT_WRITE, add, 0x36);
        show(L("P3b.2 ADD after a blocking refusal"), kq);
        close(s); close(kq);
    }

    // P12
    {
        int s6 = socket(AF_INET6, SOCK_STREAM, 0);
        struct sockaddr_in6 a6;
        memset(&a6, 0, sizeof a6);
        a6.sin6_family = AF_INET6;
        a6.sin6_addr = in6addr_loopback;
        bind(s6, (struct sockaddr *)&a6, sizeof a6);
        listen(s6, 8);
        socklen_t len = sizeof a6;
        getsockname(s6, (struct sockaddr *)&a6, &len);
        name_fd(s6, "L6");
        int kq = kqueue();
        reg(kq, s6, EVFILT_READ, add, 0xc1);
        reg(kq, s6, EVFILT_WRITE, add, 0xc2);
        show(L("P12.0 fresh IPv6 listener"), kq);
        int c6 = socket(AF_INET6, SOCK_STREAM, 0);
        set_nonblock(c6);
        name_fd(c6, "C6");
        reg(kq, c6, EVFILT_READ, add, 0xc3);
        reg(kq, c6, EVFILT_WRITE, add, 0xc4);
        show(L("P12.1 fresh IPv6 socket"), kq);
        int err = connect(c6, (struct sockaddr *)&a6, sizeof a6);
        printf("P\t%s\tP12.2 connect\t%s\n", m, ename(err < 0 ? errno : 0));
        settle();
        show(L("P12.3 IPv6 connected"), kq);
        int a = accept_one(s6);
        name_fd(a, "A6");
        close(c6);
        settle();
        reg(kq, a, EVFILT_READ, add, 0xc5);
        reg(kq, a, EVFILT_WRITE, add, 0xc6);
        show(L("P12.4 accepted end after its peer closed"), kq);
        close(a); close(s6); close(kq);
    }

    // P4
    {
        int c, a;
        pair(&c, &a);
        name_fd(c, "C");
        name_fd(a, "A");
        int kq = kqueue();
        reg(kq, a, EVFILT_READ, add, 0x41);
        reg(kq, a, EVFILT_WRITE, add, 0x42);
        show(L("P4.0 established"), kq);
        show(L("P4.1 again"), kq);
        char buf[64] = "abcdefgh";
        write(c, buf, 5);
        settle();
        show(L("P4.2 peer wrote 5"), kq);
        show(L("P4.3 again"), kq);
        write(c, buf, 3);
        settle();
        show(L("P4.4 peer wrote 3 more, unread"), kq);
        ssize_t got = read(a, buf, sizeof buf);
        printf("P\t%s\tP4.5 read %zd\n", m, got);
        settle();
        show(L("P4.6 after reading all"), kq);
        write(c, buf, 2);
        settle();
        got = read(a, buf, sizeof buf);
        settle();
        show(L("P4.7 written and read before the poll"), kq);
        close(c); close(a); close(kq);
    }

    // P5, P6
    for (int which = 0; which < 2; which++) {
        int c, a;
        pair(&c, &a);
        name_fd(c, "C");
        name_fd(a, "A");
        int kq = kqueue();
        reg(kq, a, EVFILT_READ, add, 0x51);
        reg(kq, a, EVFILT_WRITE, add, 0x52);
        show(which == 0 ? L("P5.0 established") : L("P6.0 established"), kq);
        if (which == 0) shutdown(c, SHUT_WR); else close(c);
        settle();
        show(which == 0 ? L("P5.1 peer shut down writing") : L("P6.1 peer closed"), kq);
        show(which == 0 ? L("P5.2 again") : L("P6.2 again"), kq);
        reg(kq, a, EVFILT_READ, add, 0x53);
        reg(kq, a, EVFILT_WRITE, add, 0x54);
        show(which == 0 ? L("P5.3 re-ADD both") : L("P6.3 re-ADD both"), kq);
        if (which == 0) close(c);
        close(a); close(kq);
    }

    // P7
    {
        int c, a;
        pair(&c, &a);
        name_fd(c, "C");
        name_fd(a, "A");
        int kq = kqueue();
        reg(kq, a, EVFILT_READ, add, 0x71);
        reg(kq, a, EVFILT_WRITE, add, 0x72);
        show(L("P7.0 established"), kq);
        write(a, "hello", 5);
        settle();
        close(c);
        settle();
        show(L("P7.1 peer closed with unread data"), kq);
        show(L("P7.2 again"), kq);
        close(a); close(kq);
    }

    // P8
    {
        int c, a;
        pair(&c, &a);
        name_fd(c, "C");
        name_fd(a, "A");
        int kq = kqueue();
        reg(kq, c, EVFILT_WRITE, add, 0x81);
        show(L("P8.0 established"), kq);
        static char big[65536];
        long total = 0;
        for (int i = 0; i < 1000; i++) {
            ssize_t w = write(c, big, sizeof big);
            if (w < 0) break;
            total += w;
        }
        printf("P\t%s\tP8.1 wrote %ld until %s\n", m, total, ename(errno));
        settle();
        show(L("P8.2 send buffer full"), kq);
        read(a, big, 1);
        settle();
        show(L("P8.3 peer read 1 byte"), kq);
        long drained = 0;
        set_nonblock(a);
        for (int i = 0; i < 10000; i++) {
            ssize_t r = read(a, big, sizeof big);
            if (r <= 0) { settle(); r = read(a, big, sizeof big); if (r <= 0) break; }
            drained += r;
        }
        printf("P\t%s\tP8.4 peer read %ld more\n", m, drained);
        settle();
        show(L("P8.5 after the peer read everything"), kq);
        show(L("P8.6 again"), kq);
        close(c); close(a); close(kq);
    }

    // P9
    {
        int port;
        int l = listener(&port);
        int u = socket(AF_INET, SOCK_DGRAM, 0);
        name_fd(u, "U");
        int kq = kqueue();
        reg(kq, u, EVFILT_READ, add, 0x91);
        reg(kq, u, EVFILT_WRITE, add, 0x92);
        show(L("P9.0 fresh UDP"), kq);
        int err = connect_to(u, port);
        printf("P\t%s\tP9.1 connect %s, SO_SNDBUF=%d\n", m, ename(err), sndbuf(u));
        settle();
        show(L("P9.2 connected UDP"), kq);
        int s = socket(AF_INET, SOCK_STREAM, 0);
        name_fd(s, "S");
        reg(kq, s, EVFILT_READ, add, 0x93);
        reg(kq, s, EVFILT_WRITE, add, 0x94);
        show(L("P9.3 idle TCP"), kq);
        close(s); close(u); close(l); close(kq);
    }

    // P10
    {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        name_fd(s, "S");
        struct sockaddr_in a;
        memset(&a, 0, sizeof a);
        a.sin_family = AF_INET;
        a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        bind(s, (struct sockaddr *)&a, sizeof a);
        socklen_t len = sizeof a;
        getsockname(s, (struct sockaddr *)&a, &len);
        int kq = kqueue();
        reg(kq, s, EVFILT_READ, add, 0xa1);
        show(L("P10.0 bound, idle"), kq);
        listen(s, 8);
        settle();
        show(L("P10.1 listening"), kq);
        int c = client(ntohs(a.sin_port));
        settle();
        show(L("P10.2 connected to"), kq);
        close(c); close(s); close(kq);
    }

    // P11
    {
        int port;
        int l = listener(&port);
        name_fd(l, "L");
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, add, 0xb1);
        int c = client(port);
        settle();
        show(L("P11.0 one queued"), kq);
        int a = accept_one(l);
        name_fd(a, "A");
        name_fd(c, "C");
        settle();
        write(c, "x", 1);
        settle();
        close(c);
        settle();
        show(L("P11.1 the accepted connection's peer wrote and closed"), kq);
        close(a); close(l); close(kq);
    }
#undef L
}

// ---- R ------------------------------------------------------------------

// A fresh scene: a listener with one connection queued (READ ready) and a
// connected client (WRITE ready).
struct scene { int l, c, a2, port, kq; };

static struct scene scene_new(void)
{
    struct scene s;
    s.l = listener(&s.port);
    s.c = client(s.port);
    settle();
    s.a2 = -1;
    s.kq = kqueue();
    name_fd(s.l, "L");
    name_fd(s.c, "C");
    return s;
}

static void scene_free(struct scene s)
{
    close(s.l); close(s.c); close(s.kq);
}

static void section_r(void)
{
    const uint16_t AC = EV_ADD | EV_CLEAR;
    const uint16_t R = EV_RECEIPT;

    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC | R, 1), change(s.c, EVFILT_WRITE, AC | R, 2) };
        apply("R\tR1 two receipts, room for 8", s.kq, ch, 2, 8);
        show("R\tR1 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC | R, 1), change(s.c, EVFILT_WRITE, AC | R, 2) };
        apply("R\tR2 two receipts, room for 1", s.kq, ch, 2, 1);
        show("R\tR2 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC | R, 1), change(s.c, EVFILT_WRITE, AC | R, 2) };
        apply("R\tR3 two receipts, room for 0", s.kq, ch, 2, 0);
        show("R\tR3 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[1] = { change(s.c, EVFILT_READ, EV_DELETE | R, 3) };
        apply("R\tR4 DELETE of nothing with receipt, room for 8", s.kq, ch, 1, 8);
        apply("R\tR4 DELETE of nothing with receipt, room for 0", s.kq, ch, 1, 0);
        struct kevent ch2[1] = { change(s.c, EVFILT_READ, EV_DELETE, 3) };
        apply("R\tR4 DELETE of nothing, room for 8", s.kq, ch2, 1, 8);
        apply("R\tR4 DELETE of nothing, room for 0", s.kq, ch2, 1, 0);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.c, EVFILT_READ, EV_DELETE | R, 3), change(s.l, EVFILT_READ, AC | R, 1) };
        apply("R\tR5 [DELETE of nothing, ADD] with receipts, room for 0", s.kq, ch, 2, 0);
        show("R\tR5 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.c, EVFILT_READ, EV_DELETE, 3), change(s.l, EVFILT_READ, AC, 1) };
        apply("R\tR6 [DELETE of nothing, ADD], room for 8", s.kq, ch, 2, 8);
        show("R\tR6 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.c, EVFILT_READ, EV_DELETE, 3), change(s.l, EVFILT_READ, AC, 1) };
        apply("R\tR7 [DELETE of nothing, ADD], room for 0", s.kq, ch, 2, 0);
        show("R\tR7 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC, 1) };
        apply("R\tR8 ADD of a ready listener, room for 8", s.kq, ch, 1, 8);
        show("R\tR8 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.c, EVFILT_WRITE, AC, 2);
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC | R, 1) };
        apply("R\tR9 ADD with receipt beside a queued registration, room for 8", s.kq, ch, 1, 8);
        show("R\tR9 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.c, EVFILT_WRITE, AC, 2);
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC, 1) };
        apply("R\tR9b ADD without receipt beside a queued registration, room for 8", s.kq, ch, 1, 8);
        show("R\tR9b poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        int closed = dup(s.c);
        close(closed);
        name_fd(closed, "closed");
        struct kevent ch[1] = { change(closed, EVFILT_READ, AC | R, 4) };
        apply("R\tR10 ADD of a closed descriptor with receipt, room for 8", s.kq, ch, 1, 8);
        apply("R\tR10 ADD of a closed descriptor with receipt, room for 0", s.kq, ch, 1, 0);
        struct kevent ch2[1] = { change(closed, EVFILT_READ, AC, 4) };
        apply("R\tR10 ADD of a closed descriptor, room for 8", s.kq, ch2, 1, 8);
        apply("R\tR10 ADD of a closed descriptor, room for 0", s.kq, ch2, 1, 0);
        struct kevent ch3[1] = { change(closed, EVFILT_READ, EV_DELETE | R, 4) };
        apply("R\tR10 DELETE of a closed descriptor with receipt, room for 8", s.kq, ch3, 1, 8);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        uint64_t idents[] = { UINT64_MAX, ((uint64_t)1 << 32) + (uint64_t)s.l, INT_MAX, (uint64_t)INT_MAX + 1,
                              (uint64_t)(int64_t)-1, 1000000 };
        const char *iname[] = { "UINT64_MAX", "2^32+L", "INT_MAX", "INT_MAX+1", "(uint64)-1", "1000000" };
        for (size_t i = 0; i < sizeof idents / sizeof idents[0]; i++) {
            struct kevent k;
            EV_SET(&k, (uintptr_t)idents[i], EVFILT_READ, AC | R, 0, 0, (void *)5);
            char label[96];
            snprintf(label, sizeof label, "R\tR11 ADD of ident %s with receipt, room for 8", iname[i]);
            apply(label, s.kq, &k, 1, 8);
        }
        show("R\tR11 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[3] = { change(s.l, EVFILT_READ, AC | R, 1), change(s.c, EVFILT_READ, EV_DELETE | R, 3),
                                change(s.c, EVFILT_WRITE, AC | R, 2) };
        apply("R\tR12 [ADD, DELETE of nothing, ADD] with receipts, room for 2", s.kq, ch, 3, 2);
        show("R\tR12 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        int closed = dup(s.c);
        close(closed);
        name_fd(closed, "closed");
        struct kevent ch[2] = { change(closed, EVFILT_READ, AC, 4), change(s.l, EVFILT_READ, AC, 1) };
        apply("R\tR13 [ADD closed, ADD], room for 1", s.kq, ch, 2, 1);
        show("R\tR13 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC, 1), change(s.c, EVFILT_WRITE, AC, 2) };
        apply("R\tR14 two ADDs of ready registrations without receipts, room for 1", s.kq, ch, 2, 1);
        show("R\tR14 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC | R, 1) };
        struct timespec zero = { 0, 0 };
        struct kevent out[4];
        errno = 0;
        int rv = kevent(s.kq, ch, 1, out, 4, &zero);
        printf("R\tR15 receipt entry, every field\trv=%d\t%s\t", rv, ename(rv < 0 ? errno : 0));
        if (rv > 0)
            printf("ident=%s filter=%d flags=0x%x fflags=%u data=%lld udata=0x%llx", ident_name(out[0].ident),
                   out[0].filter, out[0].flags, out[0].fflags, (long long)out[0].data,
                   (unsigned long long)(uintptr_t)out[0].udata);
        printf("\n");
        scene_free(s);
    }
    {
        // Changes the model refuses: whatever the real kernel does with them.
        struct scene s = scene_new();
        uint16_t flags[] = { EV_ADD | EV_ONESHOT, EV_ADD | EV_DISPATCH, EV_ADD | EV_DISABLE, EV_ENABLE, EV_DISABLE,
                             0, EV_ADD | EV_DELETE };
        const char *fnames[] = { "ADD|ONESHOT", "ADD|DISPATCH", "ADD|DISABLE", "ENABLE alone", "DISABLE alone",
                                 "no flags", "ADD|DELETE" };
        for (size_t i = 0; i < sizeof flags / sizeof flags[0]; i++) {
            int kq = kqueue();
            struct kevent k = change(s.l, EVFILT_READ, flags[i] | R, 6);
            char label[96];
            snprintf(label, sizeof label, "R\tR16 %s on a ready listener with receipt", fnames[i]);
            apply(label, kq, &k, 1, 8);
            snprintf(label, sizeof label, "R\tR16 %s poll", fnames[i]);
            show(label, kq);
            close(kq);
        }
        struct kevent k;
        EV_SET(&k, (uintptr_t)s.l, EVFILT_READ, AC | R, NOTE_LOWAT, 2, (void *)7);
        apply("R\tR16 ADD with NOTE_LOWAT 2 on a listener with one queued", s.kq, &k, 1, 8);
        show("R\tR16 NOTE_LOWAT poll", s.kq);
        EV_SET(&k, (uintptr_t)s.c, EVFILT_READ, AC | R, 0, 100, (void *)8);
        apply("R\tR16 ADD with data 100 and no fflags", s.kq, &k, 1, 8);
        EV_SET(&k, (uintptr_t)s.c, EVFILT_EXCEPT, AC | R, 0, 0, (void *)9);
        apply("R\tR16 EVFILT_EXCEPT on a socket", s.kq, &k, 1, 8);
        EV_SET(&k, (uintptr_t)s.c, -100, AC | R, 0, 0, (void *)9);
        apply("R\tR16 filter -100", s.kq, &k, 1, 8);
        scene_free(s);
    }
}

// ---- O ------------------------------------------------------------------

static void section_o(uint16_t mode, const char *m)
{
    uint16_t add = EV_ADD | mode;
    char label[128];
    int perms[6][3] = { { 0, 1, 2 }, { 0, 2, 1 }, { 1, 0, 2 }, { 1, 2, 0 }, { 2, 0, 1 }, { 2, 1, 0 } };
    for (int p = 0; p < 6; p++) {
        int port[3], l[3], c[3];
        int kq = kqueue();
        for (int i = 0; i < 3; i++) {
            l[i] = listener(&port[i]);
            char n[8];
            snprintf(n, sizeof n, "L%d", i + 1);
            name_fd(l[i], n);
            reg(kq, l[i], EVFILT_READ, add, 0x100 + i + 1);
        }
        for (int k = 0; k < 3; k++) {
            c[k] = client(port[perms[p][k]]);
            settle();
        }
        snprintf(label, sizeof label, "O\t%s\tO1 registered L1,L2,L3; connected L%d,L%d,L%d", m, perms[p][0] + 1,
                 perms[p][1] + 1, perms[p][2] + 1);
        show(label, kq);
        for (int i = 0; i < 3; i++) { close(c[i]); close(l[i]); }
        close(kq);
    }

    for (int order = 0; order < 2; order++) {
        int port = closed_port();
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        name_fd(s, "S");
        int kq = kqueue();
        if (order == 0) {
            reg(kq, s, EVFILT_READ, add, 1);
            reg(kq, s, EVFILT_WRITE, add, 2);
        } else {
            reg(kq, s, EVFILT_WRITE, add, 2);
            reg(kq, s, EVFILT_READ, add, 1);
        }
        connect_to(s, port);
        settle();
        snprintf(label, sizeof label, "O\t%s\tO2 refused, registered %s", m, order == 0 ? "READ,WRITE" : "WRITE,READ");
        show(label, kq);
        close(s); close(kq);

        int pl;
        int l = listener(&pl);
        s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        name_fd(s, "S");
        name_fd(l, "L");
        kq = kqueue();
        if (order == 0) {
            reg(kq, s, EVFILT_WRITE, add, 2);
            reg(kq, l, EVFILT_READ, add, 1);
        } else {
            reg(kq, l, EVFILT_READ, add, 1);
            reg(kq, s, EVFILT_WRITE, add, 2);
        }
        connect_to(s, pl);
        settle();
        snprintf(label, sizeof label, "O\t%s\tO2b one connect, registered %s", m, order == 0 ? "S:WRITE,L:READ" : "L:READ,S:WRITE");
        show(label, kq);
        close(s); close(l); close(kq);
    }

    {
        int port[2], l[2];
        int kq = kqueue();
        for (int i = 0; i < 2; i++) {
            l[i] = listener(&port[i]);
            char n[8];
            snprintf(n, sizeof n, "L%d", i + 1);
            name_fd(l[i], n);
            reg(kq, l[i], EVFILT_READ, add, 0x100 + i + 1);
        }
        int c1 = client(port[0]);
        settle();
        int c2 = client(port[1]);
        settle();
        int c3 = client(port[0]);
        settle();
        snprintf(label, sizeof label, "O\t%s\tO3 connected L1, L2, L1 again", m);
        show(label, kq);
        snprintf(label, sizeof label, "O\t%s\tO4 room for 1", m);
        show_n(label, kq, 1);
        int c4 = client(port[0]);
        settle();
        show_n(label, kq, 1);
        show_n(label, kq, 1);
        show_n(label, kq, 1);
        close(c1); close(c2); close(c3); close(c4); close(l[0]); close(l[1]); close(kq);
    }

    {
        int port[3], l[3], c[3];
        int kq = kqueue();
        for (int i = 0; i < 3; i++) {
            l[i] = listener(&port[i]);
            char n[8];
            snprintf(n, sizeof n, "L%d", i + 1);
            name_fd(l[i], n);
        }
        reg(kq, l[0], EVFILT_READ, add, 0x101);
        reg(kq, l[1], EVFILT_READ, add, 0x102);
        c[0] = client(port[1]);
        settle();
        c[1] = client(port[2]);
        settle();
        c[2] = client(port[0]);
        settle();
        reg(kq, l[2], EVFILT_READ, add, 0x103);
        snprintf(label, sizeof label, "O\t%s\tO5 L1,L2 registered; connected L2, L3, L1; then L3 ADDed", m);
        show_n(label, kq, 2);
        snprintf(label, sizeof label, "O\t%s\tO5 the rest", m);
        show(label, kq);
        for (int i = 0; i < 3; i++) { close(c[i]); close(l[i]); }
        close(kq);
    }

    for (int order = 0; order < 2; order++) {
        int port;
        int l = listener(&port);
        int d = dup(l);
        name_fd(l, "L");
        name_fd(d, "D");
        int kq = kqueue();
        if (order == 0) {
            reg(kq, l, EVFILT_READ, add, 1);
            reg(kq, d, EVFILT_READ, add, 2);
        } else {
            reg(kq, d, EVFILT_READ, add, 2);
            reg(kq, l, EVFILT_READ, add, 1);
        }
        int c = client(port);
        settle();
        snprintf(label, sizeof label, "O\t%s\tO6 one listener through L and its dup D, registered %s; a connection", m,
                 order == 0 ? "L,D" : "D,L");
        show(label, kq);
        close(c); close(l); close(d); close(kq);
    }
}

// ---- D ------------------------------------------------------------------

static void section_d(void)
{
    const uint16_t AC = EV_ADD | EV_CLEAR;
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        show("D\tD1 ADD|CLEAR udata 1", s.kq);
        reg(s.kq, s.l, EVFILT_READ, AC, 2);
        show("D\tD1 re-ADD|CLEAR udata 2 after delivery", s.kq);
        show("D\tD1 again", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        reg(s.kq, s.l, EVFILT_READ, AC, 2);
        show("D\tD2 ADD udata 1, re-ADD udata 2 while queued", s.kq);
        show("D\tD2 again", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        show("D\tD3 ADD|CLEAR", s.kq);
        reg(s.kq, s.l, EVFILT_READ, EV_ADD, 2);
        show("D\tD3 re-ADD without CLEAR", s.kq);
        show("D\tD3 again", s.kq);
        show("D\tD3 again", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, EV_ADD, 1);
        show("D\tD4 ADD without CLEAR", s.kq);
        show("D\tD4 again", s.kq);
        reg(s.kq, s.l, EVFILT_READ, AC, 2);
        show("D\tD4 re-ADD with CLEAR", s.kq);
        show("D\tD4 again", s.kq);
        scene_free(s);
    }
    {
        int port;
        int l = listener(&port);
        name_fd(l, "L");
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, AC, 1);
        reg(kq, l, EVFILT_READ, AC, 2);
        show("D\tD5 ADD, re-ADD on nothing ready", kq);
        int c = client(port);
        settle();
        show("D\tD5 then a connection", kq);
        close(c); close(l); close(kq);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        reg(s.kq, s.c, EVFILT_WRITE, AC, 2);
        reg(s.kq, s.l, EVFILT_READ, EV_DELETE, 0);
        show("D\tD6 two queued, the first deleted", s.kq);
        reg(s.kq, s.l, EVFILT_READ, AC, 3);
        show("D\tD6 the first ADDed again", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        reg(s.kq, s.c, EVFILT_WRITE, AC, 2);
        reg(s.kq, s.l, EVFILT_READ, AC, 3);
        show("D\tD7 L, C queued, then L re-ADDed", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC | EV_RECEIPT, 1), change(s.l, EVFILT_READ, AC | EV_RECEIPT, 2) };
        apply("D\tD8 the same ADD twice in one changelist, room for 8", s.kq, ch, 2, 8);
        show("D\tD8 poll", s.kq);
        struct kevent ch2[2] = { change(s.l, EVFILT_READ, EV_DELETE | EV_RECEIPT, 0),
                                 change(s.l, EVFILT_READ, EV_DELETE | EV_RECEIPT, 0) };
        apply("D\tD8 the same DELETE twice in one changelist, room for 8", s.kq, ch2, 2, 8);
        scene_free(s);
    }
}

// ---- F ------------------------------------------------------------------

static void section_f(void)
{
    char path[] = "/tmp/kevent-register-XXXXXX";
    int file = mkstemp(path);
    write(file, "0123456789", 10);
    int ro = open(path, O_RDONLY);
    int wo = open(path, O_WRONLY);
    unlink(path);
    int dir = open("/tmp", O_RDONLY);
    int p[2];
    pipe(p);
    int q[2];
    pipe(q);
    write(q[1], "abc", 3);
    int kqt = kqueue();
    int devnull = open("/dev/null", O_RDWR);
    struct { const char *name; int fd; } targets[] = {
        { "file-rdwr(at end)", file }, { "file-rdonly", ro }, { "file-wronly", wo }, { "directory", dir },
        { "pipe-read-empty", p[0] }, { "pipe-read-3", q[0] }, { "pipe-write", p[1] }, { "kqueue", kqt },
        { "dev-null", devnull },
    };
    for (size_t i = 0; i < sizeof targets / sizeof targets[0]; i++) {
        name_fd(targets[i].fd, targets[i].name);
        for (int f = 0; f < 2; f++) {
            int16_t filter = f == 0 ? EVFILT_READ : EVFILT_WRITE;
            int kq = kqueue();
            struct kevent k = change(targets[i].fd, filter, EV_ADD | EV_CLEAR | EV_RECEIPT, 0xf0 + i);
            char label[96];
            snprintf(label, sizeof label, "F\t%s %s ADD with receipt", targets[i].name, fname(filter));
            apply(label, kq, &k, 1, 8);
            snprintf(label, sizeof label, "F\t%s %s poll", targets[i].name, fname(filter));
            show(label, kq);
            close(kq);
        }
    }
    close(file); close(ro); close(wo); close(dir); close(p[0]); close(p[1]); close(q[0]); close(q[1]);
    close(kqt); close(devnull);
}

// ---- G ------------------------------------------------------------------

static void section_g(void)
{
    const uint16_t AC = EV_ADD | EV_CLEAR;
    {
        int port;
        int l = listener(&port);
        int d = dup(l);
        name_fd(l, "L");
        name_fd(d, "D");
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, AC, 1);
        close(l);
        int c = client(port);
        settle();
        show("G\tG1 registered through L, L closed, D lives, then a connection", kq);
        reg(kq, d, EVFILT_READ, AC, 2);
        show("G\tG1 then ADD through D", kq);
        struct kevent ch[1] = { change(l, EVFILT_READ, EV_DELETE | EV_RECEIPT, 0) };
        apply("G\tG1 DELETE through the closed L", kq, ch, 1, 8);
        close(c); close(d); close(kq);
    }
    {
        int port;
        int l = listener(&port);
        int d = dup(l);
        name_fd(l, "L");
        name_fd(d, "D");
        int kq = kqueue();
        reg(kq, d, EVFILT_READ, AC, 1);
        close(l);
        int c = client(port);
        settle();
        show("G\tG2 registered through D, L closed, then a connection", kq);
        close(c); close(d); close(kq);
    }
    {
        int port;
        int l = listener(&port);
        name_fd(l, "L");
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, AC, 1);
        int was = l;
        close(l);
        int port2;
        int l2 = listener(&port2);
        printf("G\tG3 the new listener reused the number: %d\n", l2 == was);
        int c = client(port2);
        settle();
        show("G\tG3 registered on L, L closed, a new listener on the same number connected to", kq);
        close(c); close(l2); close(kq);
    }
    {
        int port;
        int l = listener(&port);
        int d = dup(l);
        name_fd(l, "L");
        name_fd(d, "D");
        int c = client(port);
        settle();
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, AC, 1);
        close(l);
        show("G\tG4 queued through L, then L closed while D lives", kq);
        close(c); close(d); close(kq);
    }
    {
        int port;
        int l = listener(&port);
        int d = dup(l);
        name_fd(l, "L");
        name_fd(d, "D");
        int kq1 = kqueue();
        int kq2 = kqueue();
        reg(kq1, l, EVFILT_READ, AC, 1);
        reg(kq2, l, EVFILT_READ, AC, 2);
        reg(kq2, d, EVFILT_READ, AC, 3);
        close(l);
        int c = client(port);
        settle();
        show("G\tG5 L in two kqueues and D in the second; L closed; first kqueue", kq1);
        show("G\tG5 second kqueue", kq2);
        close(c); close(d); close(kq1); close(kq2);
    }
    {
        int port;
        int l = listener(&port);
        name_fd(l, "L");
        int kq = kqueue();
        reg(kq, l, EVFILT_READ, AC, 1);
        int other = open("/dev/null", O_RDONLY);
        int lcopy = dup(l);
        name_fd(lcopy, "Lcopy");
        dup2(other, l);
        close(other);
        int c = client(port);
        settle();
        show("G\tG6 registered on L, then dup2 over L (the listener lives on as Lcopy)", kq);
        close(c); close(l); close(lcopy); close(kq);
    }
}

// ---- X ------------------------------------------------------------------

static void section_x(void)
{
    const uint16_t AC = EV_ADD | EV_CLEAR;
    const uint16_t R = EV_RECEIPT;

    // The send buffer each socket state reports through SO_SNDBUF and
    // SO_SNDLOWAT, to name the WRITE event's `data`.
    {
        int fresh = socket(AF_INET, SOCK_STREAM, 0);
        int lowat = 0;
        socklen_t len = sizeof lowat;
        getsockopt(fresh, SOL_SOCKET, SO_SNDLOWAT, &lowat, &len);
        printf("X\tX1 fresh TCP SO_SNDBUF=%d SO_SNDLOWAT=%d\n", sndbuf(fresh), lowat);
        int port = closed_port();
        set_nonblock(fresh);
        connect_to(fresh, port);
        settle();
        printf("X\tX1 refused TCP SO_SNDBUF=%d\n", sndbuf(fresh));
        close(fresh);
        int l6 = socket(AF_INET6, SOCK_STREAM, 0);
        printf("X\tX1 fresh TCP6 SO_SNDBUF=%d\n", sndbuf(l6));
        close(l6);
        int port2;
        int l = listener(&port2);
        printf("X\tX1 listener SO_SNDBUF=%d\n", sndbuf(l));
        close(l);
    }

    // Which idents an ADD answers which way: k * 2^32 + low, for an open
    // descriptor, a closed one, and the edges of the 32-bit range.
    {
        int port;
        int l = listener(&port);
        int kq = kqueue();
        int closed = dup(l);
        close(closed);
        uint64_t highs[] = { 0, 1, 2, 0x7fffffffULL, 0x80000000ULL, 0xffffffffULL };
        uint64_t lows[] = { (uint64_t)l, (uint64_t)closed, 0x7fffffffULL, 0x80000000ULL, 0xffffffffULL };
        const char *lname[] = { "open", "closed", "0x7fffffff", "0x80000000", "0xffffffff" };
        for (size_t h = 0; h < sizeof highs / sizeof highs[0]; h++)
        for (size_t i = 0; i < sizeof lows / sizeof lows[0]; i++) {
            for (int del = 0; del < 2; del++) {
                struct kevent k;
                uint64_t ident = (highs[h] << 32) | lows[i];
                EV_SET(&k, (uintptr_t)ident, EVFILT_READ, (del ? EV_DELETE : AC) | R, 0, 0, (void *)5);
                struct kevent out[2];
                struct timespec zero = { 0, 0 };
                errno = 0;
                int rv = kevent(kq, &k, 1, out, 2, &zero);
                printf("X\tX2 %s high=0x%llx low=%s\trv=%d\t%s\tdata=%lld\n", del ? "DELETE" : "ADD",
                       (unsigned long long)highs[h], lname[i], rv, ename(rv < 0 ? errno : 0),
                       rv > 0 ? (long long)out[0].data : -1LL);
                if (!del && rv > 0 && out[0].data == 0) {
                    // Registered: take it out again, through the same ident.
                    EV_SET(&k, (uintptr_t)ident, EVFILT_READ, EV_DELETE | R, 0, 0, NULL);
                    kevent(kq, &k, 1, out, 2, &zero);
                }
            }
        }
        // Registered under 2^32 + L, does the event carry the full ident?
        int c = client(port);
        settle();
        close(c);
        close(l); close(kq);
    }

    // EV_DELETE of nothing on descriptors of every kind.
    {
        char path[] = "/tmp/kevent-register-XXXXXX";
        int file = mkstemp(path);
        unlink(path);
        int p[2];
        pipe(p);
        int kqt = kqueue();
        int dir = open("/tmp", O_RDONLY);
        int u = socket(AF_INET, SOCK_DGRAM, 0);
        struct { const char *name; int fd; } targets[] = {
            { "file", file }, { "pipe-read", p[0] }, { "pipe-write", p[1] }, { "kqueue", kqt },
            { "directory", dir }, { "udp", u },
        };
        int kq = kqueue();
        for (size_t i = 0; i < sizeof targets / sizeof targets[0]; i++) {
            name_fd(targets[i].fd, targets[i].name);
            for (int f = 0; f < 2; f++) {
                struct kevent k = change(targets[i].fd, f == 0 ? EVFILT_READ : EVFILT_WRITE, EV_DELETE | R, 0);
                char label[96];
                snprintf(label, sizeof label, "X\tX3 DELETE of nothing on %s %s", targets[i].name, f == 0 ? "READ" : "WRITE");
                apply(label, kq, &k, 1, 8);
            }
        }
        close(kq); close(file); close(p[0]); close(p[1]); close(kqt); close(dir); close(u);
    }

    // A failing change after the receipts have filled the eventlist.
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC | R, 1), change(s.c, EVFILT_READ, EV_DELETE | R, 3) };
        apply("X\tX4 [ADD, DELETE of nothing] with receipts, room for 1", s.kq, ch, 2, 1);
        show("X\tX4 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[3] = { change(s.l, EVFILT_READ, AC | R, 1), change(s.c, EVFILT_READ, EV_DELETE, 3),
                                change(s.c, EVFILT_WRITE, AC, 2) };
        apply("X\tX4 [ADD rcpt, DELETE of nothing, ADD] room for 1", s.kq, ch, 3, 1);
        show("X\tX4 poll", s.kq);
        scene_free(s);
    }
    {
        struct scene s = scene_new();
        struct kevent ch[2] = { change(s.l, EVFILT_READ, AC, 1), change(s.c, EVFILT_READ, EV_DELETE, 3) };
        apply("X\tX4 [ADD of ready, DELETE of nothing] no receipts, room for 1", s.kq, ch, 2, 1);
        show("X\tX4 poll", s.kq);
        scene_free(s);
    }
    // Errors and events: a failing change with room, beside a queued registration.
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        struct kevent ch[1] = { change(s.c, EVFILT_READ, EV_DELETE, 3) };
        apply("X\tX5 DELETE of nothing beside a queued registration, room for 8", s.kq, ch, 1, 8);
        show("X\tX5 poll", s.kq);
        scene_free(s);
    }
    // EV_RECEIPT on a change applied while an EV_CLEAR registration sits in the
    // queue, then the wait with nevents of 0 after receipts.
    {
        struct scene s = scene_new();
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC | R, 1) };
        struct timespec ts = { 0, 0 };
        struct kevent out[4];
        errno = 0;
        int rv = kevent(s.kq, ch, 1, out, 4, NULL);
        printf("X\tX6 receipt with a NULL timeout and something queued\trv=%d\t%s\n", rv, ename(rv < 0 ? errno : 0));
        (void)ts;
        scene_free(s);
    }
    // The event's `flags` after a re-ADD that adds EV_RECEIPT.
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        show("X\tX7 ADD|CLEAR", s.kq);
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC | R, 2) };
        apply("X\tX7 re-ADD|CLEAR|RECEIPT", s.kq, ch, 1, 8);
        show("X\tX7 poll", s.kq);
        scene_free(s);
    }
    // EV_DELETE of an ident whose low 32 bits name a registered descriptor and
    // whose high ones do not: does it reach the registration?
    {
        struct scene s = scene_new();
        reg(s.kq, s.l, EVFILT_READ, AC, 1);
        struct kevent k;
        EV_SET(&k, (uintptr_t)(((uint64_t)1 << 32) | (uint64_t)s.l), EVFILT_READ, EV_DELETE | R, 0, 0, NULL);
        apply("X\tX9 DELETE of 2^32+L with L registered, room for 8", s.kq, &k, 1, 8);
        show("X\tX9 poll", s.kq);
        scene_free(s);
    }
    // A waiter asleep in kevent with an empty kqueue, woken by an ADD from
    // another thread? (Not threaded here: an ADD then a NULL-timeout wait.)
    {
        struct scene s = scene_new();
        struct kevent ch[1] = { change(s.l, EVFILT_READ, AC, 1) };
        struct kevent out[4];
        int rv = kevent(s.kq, ch, 1, out, 4, NULL);
        printf("X\tX8 ADD of a ready listener with a NULL timeout\trv=%d\t", rv);
        print_events(out, rv > 0 ? rv : 0);
        printf("\n");
        scene_free(s);
    }
}

// ---- Y ------------------------------------------------------------------

#include <pthread.h>
#include <sys/mman.h>

static int sleeper_fd;
static volatile int sleeper_rv = -2, sleeper_errno;

static void *sleeper(void *arg)
{
    (void)arg;
    struct kevent out[4];
    errno = 0;
    int rv = kevent(sleeper_fd, NULL, 0, out, 4, NULL);
    sleeper_errno = rv < 0 ? errno : 0;
    sleeper_rv = rv;
    return NULL;
}

static void section_y(void)
{
    const uint16_t AC = EV_ADD | EV_CLEAR;
    // A kqueue drained by a close under a sleeper, with registrations.
    for (int trial = 0; trial < 3; trial++) {
        int port, port2;
        int l = listener(&port);
        int l2 = listener(&port2);
        name_fd(l, "L");
        name_fd(l2, "L2");
        int k = kqueue();
        int d = dup(k);
        reg(k, l, EVFILT_READ, AC, 1);
        sleeper_fd = k;
        sleeper_rv = -2;
        pthread_t t;
        pthread_create(&t, NULL, sleeper, NULL);
        sleep_ms(50);
        close(k);
        pthread_join(t, NULL);
        printf("Y\tY1 trial %d: sleeper through K, K closed\trv=%d\t%s\n", trial, sleeper_rv, ename(sleeper_errno));
        int c = client(port);
        settle();
        show("Y\tY1 then L ready; a {0,0} wait through D", d);
        struct kevent ch[1] = { change(l2, EVFILT_READ, AC | EV_RECEIPT, 2) };
        apply("Y\tY1 an ADD with receipt through D, room for 8", d, ch, 1, 8);
        apply("Y\tY1 an ADD with receipt through D, room for 0", d, ch, 1, 0);
        struct kevent ch2[1] = { change(l2, EVFILT_READ, AC, 3) };
        apply("Y\tY1 an ADD without receipt through D, room for 0", d, ch2, 1, 0);
        close(c); close(l); close(l2); close(d);
    }

    // A sleeper woken by a registration made from another thread.
    for (int trial = 0; trial < 3; trial++) {
        int port;
        int l = listener(&port);
        name_fd(l, "L");
        int c = client(port);
        settle();
        int k = kqueue();
        sleeper_fd = k;
        sleeper_rv = -2;
        pthread_t t;
        pthread_create(&t, NULL, sleeper, NULL);
        sleep_ms(50);
        int before = sleeper_rv;
        reg(k, l, EVFILT_READ, AC, 1);
        sleep_ms(50);
        int after = sleeper_rv;
        if (after == -2) { close(c); c = client(port); }
        pthread_join(t, NULL);
        printf("Y\tY2 trial %d: asleep before the ADD %d; returned after it %d\trv=%d\t%s\n", trial, before == -2,
               after != -2, sleeper_rv, ename(sleeper_errno));
        close(c); close(l); close(k);
    }

    // A changelist whose second entry is unreadable.
    {
        long page = sysconf(_SC_PAGESIZE);
        char *mem = mmap(NULL, 2 * page, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
        mprotect(mem + page, page, PROT_NONE);
        struct kevent *last = (struct kevent *)(mem + page - sizeof(struct kevent));
        struct scene s = scene_new();
        *last = change(s.l, EVFILT_READ, AC | EV_RECEIPT, 1);
        apply("Y\tY3 [ADD with receipt, unreadable], room for 8", s.kq, last, 2, 8);
        show("Y\tY3 poll", s.kq);
        scene_free(s);
        s = scene_new();
        *last = change(s.l, EVFILT_READ, AC, 1);
        apply("Y\tY3 [ADD of a ready listener, unreadable], room for 8", s.kq, last, 2, 8);
        show("Y\tY3 poll", s.kq);
        scene_free(s);
        s = scene_new();
        *last = change(s.c, EVFILT_READ, EV_DELETE, 1);
        apply("Y\tY3 [DELETE of nothing, unreadable], room for 8", s.kq, last, 2, 8);
        apply("Y\tY3 [DELETE of nothing, unreadable], room for 0", s.kq, last, 2, 0);
        scene_free(s);
        s = scene_new();
        *last = change(s.l, EVFILT_READ, AC, 1);
        apply("Y\tY3 [ADD, unreadable], room for 0", s.kq, last, 2, 0);
        show("Y\tY3 poll", s.kq);
        scene_free(s);
        munmap(mem, 2 * page);
    }
}

int main(void)
{
    alarm(120);
    signal(SIGPIPE, SIG_IGN);
    setvbuf(stdout, NULL, _IOLBF, 0);
    section_p(EV_CLEAR, "clear");
    section_p(0, "level");
    section_r();
    section_o(EV_CLEAR, "clear");
    section_o(0, "level");
    section_d();
    section_f();
    section_g();
    section_x();
    section_y();
    return 0;
}
