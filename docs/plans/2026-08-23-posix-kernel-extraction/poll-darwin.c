// poll(2) on Darwin, against the kqueue filters it is built from.
//
// XNU's `poll_nocancel` (bsd/kern/sys_generic.c) makes a private kqueue per
// call, registers EV_ADD|EV_ONESHOT|EV_POLL filters for each entry -- READ for
// any of IN/RDNORM/PRI/RDBAND/HUP (with EV_OOBAND for PRI/RDBAND), WRITE for
// any of OUT/WRNORM/WRBAND, VNODE for any of EXTEND/ATTRIB/NLINK/WRITE, in that
// order, stopping at the first that fails -- and `poll_callback` folds each
// event the private kqueue reports back into `revents`. This probe tests that
// description, rather than assuming it.
//
// Sections:
//   S  every state below, polled with all 65536 request masks at timeout 0.
//      Each state's filters are first observed directly: a fresh kqueue, one
//      `kevent` registering EV_ADD|EV_ONESHOT|EV_POLL (and EV_OOBAND, and each
//      VNODE fflags) with room for one event and timeout {0,0}, which answers
//      "fails with errno", "registered, not ready" or "ready, with these
//      flags". PREDICT then applies poll_nocancel's registration rule and
//      poll_callback to those observations, and the row reports how many of
//      the 65536 masks poll answered differently, how many returned an rv other
//      than (revents != 0), and how many failed. Also printed: the answer to
//      0, to each single bit, to IN|OUT and to 0xFFFF.
//   M  several entries in one call: one descriptor twice, a descriptor and its
//      dup, an entry that fails beside one that does not, negative and closed
//      descriptors.
//   W  waits: what wakes a sleeping poll, and what a wake that adds nothing
//      to revents does. W6 holds the poller off the CPU (SIGSTOP of a forked
//      child sharing the socket) while two events arrive, to see whether the
//      order the filters were activated in decides the answer.
//   N  nfds against RLIMIT_NOFILE and OPEN_MAX, ahead of the buffer; and a
//      buffer that can be read but not written.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -O1 -o /tmp/pd poll-darwin.c && /tmp/pd
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64 (xnu-13432.1.9), uid 501, twice,
// identical but for timings and send-buffer sizes;
// poll-darwin.darwin-27.0-uid501.txt is one run's full output. What it shows:
//   S  all 65536 masks over 51 states (TCP over IPv4 and IPv6 unbound, bound,
//      listening empty and queued, connected, accepted, each half shut down,
//      peer closed, data waiting, reset, refused before and after SO_ERROR;
//      UDP; AF_UNIX stream and datagram; every pipe end state; regular files
//      at each access mode and offset; a directory; /dev/null; a kqueue empty
//      and with an event; a closed descriptor; -1): every answer is
//      poll_nocancel's registration rule and poll_callback applied to the
//      filters' own reports, once two things user-space kevent cannot show are
//      added -- EV_POLL makes a regular file's READ always ready, and EV_OOBAND
//      stays in the flags of every READ but a socket's, so a ready pipe, file
//      or kqueue READ answers PRI|RDBAND when asked. rv was always
//      (revents != 0); no mask failed.
//   M  one (descriptor, filter) pair named by several entries reports to the
//      last of them alone (M1, M12), each filter separately (M13-M16); a dup
//      is a different descriptor (M2). An entry whose registration fails is
//      POLLNVAL, and a filter it registered before failing still reports into
//      it (IN|NVAL, M3, M5, M9) unless every entry failed (M4). The count is
//      the entries with anything in revents. A later entry's re-ADD keeps the
//      first ADD's EV_OOBAND, as kevent's re-ADD keeps its flags: IN then PRI
//      on a pipe holding data reports nothing to either entry (M17-M24).
//   W  every modelled socket producer wakes a sleeping poll (W1, W4, W5). A
//      wake whose event adds nothing to revents is consumed, and the poll
//      sleeps on to its timeout (W2, W3, W10), as is a READ reported at
//      registration that added nothing (W7: a HUP-only poll of a pipe holding
//      data never sees the writer's close). With the poller held off the CPU
//      (W11) while a connect completes and the peer closes, 33-37 of 40
//      answered IN|OUT|HUP, which no poll made in the final state answers
//      (IN|HUP), and the rest OUT: the order the private kqueue activated its
//      filters in decides the answer. SIGSTOP does not hold a sleeper off
//      (W6: the woken poll finishes in the kernel and stops on the way out).
//   N  nfds above OPEN_MAX (10240), or above RLIMIT_NOFILE, is EINVAL whatever
//      the buffer (not root; root's FD_SETSIZE allowance is source-only); else
//      a NULL buffer is EFAULT, except for nfds 0. A buffer that can be read but
//      not written is EFAULT at the copy-out, whatever the entries hold.
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define RBITS (POLLIN | POLLRDNORM | POLLPRI | POLLRDBAND | POLLHUP)
#define WBITS (POLLOUT | POLLWRNORM | POLLWRBAND)
#define VBITS (POLLEXTEND | POLLATTRIB | POLLNLINK | POLLWRITE)

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static void settle(void) { sleep_ms(30); }

static long long now_us(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000LL + ts.tv_nsec / 1000;
}

static const char *ename(int e)
{
    switch (e) {
    case 0: return "-";
    case EBADF: return "EBADF";
    case EINVAL: return "EINVAL";
    case EFAULT: return "EFAULT";
    case EINTR: return "EINTR";
    case EAGAIN: return "EAGAIN";
    case ENOTSUP: return "ENOTSUP";
    default: return strerror(e);
    }
}

static void bits(char *buf, size_t n, int v)
{
    static const struct { int bit; const char *name; } names[] = {
        { POLLIN, "IN" }, { POLLPRI, "PRI" }, { POLLOUT, "OUT" }, { POLLERR, "ERR" },
        { POLLHUP, "HUP" }, { POLLNVAL, "NVAL" }, { POLLRDNORM, "RDNORM" }, { POLLRDBAND, "RDBAND" },
        { POLLWRBAND, "WRBAND" }, { POLLEXTEND, "EXTEND" }, { POLLATTRIB, "ATTRIB" },
        { POLLNLINK, "NLINK" }, { POLLWRITE, "WRITE" }, { 0x2000, "0x2000" }, { 0x4000, "0x4000" },
        { 0x8000, "0x8000" },
    };
    buf[0] = 0;
    if (v == 0) { snprintf(buf, n, "0"); return; }
    for (size_t i = 0; i < sizeof names / sizeof names[0]; i++) {
        if (v & names[i].bit) {
            size_t len = strlen(buf);
            snprintf(buf + len, n - len, "%s%s", len ? "|" : "", names[i].name);
        }
    }
}

static const char *b(int v)
{
    static char ring[8][128];
    static int at;
    at = (at + 1) % 8;
    bits(ring[at], sizeof ring[at], v & 0xFFFF);
    return ring[at];
}

// ---- observing one filter -----------------------------------------------

struct obs {
    int error;       // registration's errno, 0 if it registered
    int active;      // the filter reported an event at once
    uint16_t flags;  // the event's flags
    uint32_t fflags; // the event's fflags
    int64_t data;
};

static struct obs observe(int fd, int16_t filter, uint16_t extra, uint32_t fflags)
{
    struct obs o = { 0, 0, 0, 0, 0 };
    int kq = kqueue();
    struct kevent ch, out;
    EV_SET(&ch, (uintptr_t)(intptr_t)fd, filter, EV_ADD | EV_ONESHOT | EV_POLL | extra, fflags, 0, NULL);
    struct timespec zero = { 0, 0 };
    errno = 0;
    int rv = kevent(kq, &ch, 1, &out, 1, &zero);
    if (rv < 0) {
        o.error = errno;
    } else if (rv == 1 && (out.flags & EV_ERROR)) {
        o.error = (int)out.data;
    } else if (rv == 1) {
        o.active = 1;
        o.flags = out.flags;
        o.fflags = out.fflags;
        o.data = out.data;
    }
    close(kq);
    return o;
}

struct filters {
    struct obs read, read_oob, write, vnode[16];
};

static struct filters observe_all(int fd)
{
    struct filters f;
    f.read = observe(fd, EVFILT_READ, 0, 0);
    f.read_oob = observe(fd, EVFILT_READ, EV_OOBAND, 0);
    f.write = observe(fd, EVFILT_WRITE, 0, 0);
    for (int k = 0; k < 16; k++) {
        uint32_t ff = ((k & 1) ? NOTE_EXTEND : 0) | ((k & 2) ? NOTE_ATTRIB : 0) | ((k & 4) ? NOTE_LINK : 0) |
                      ((k & 8) ? NOTE_WRITE : 0);
        f.vnode[k] = k == 0 ? (struct obs){ 0, 0, 0, 0, 0 } : observe(fd, EVFILT_VNODE, 0, ff);
    }
    return f;
}

static void print_obs(const char *what, struct obs o)
{
    if (o.error) printf(" %s=%s", what, ename(o.error));
    else if (!o.active) printf(" %s=idle", what);
    else printf(" %s=ready(flags=0x%x fflags=%u data=%lld)", what, o.flags, o.fflags, (long long)o.data);
}

// poll_callback, applied to one event `o` of `filter` for an entry asking
// `events`.
static void callback(int16_t filter, struct obs o, int events, int *revents)
{
    if (o.flags & EV_EOF) *revents |= POLLHUP;
    if (o.flags & EV_ERROR) *revents |= POLLERR;
    switch (filter) {
    case EVFILT_READ: {
        int mask;
        if (*revents & POLLHUP) mask = POLLIN | POLLRDNORM | POLLPRI | POLLRDBAND;
        else {
            mask = POLLIN | POLLRDNORM;
            if (o.flags & EV_OOBAND) mask |= POLLPRI | POLLRDBAND;
        }
        *revents |= events & mask;
        break;
    }
    case EVFILT_WRITE:
        if (!(*revents & POLLHUP)) *revents |= events & (POLLOUT | POLLWRNORM | POLLWRBAND);
        break;
    case EVFILT_VNODE:
        if (o.fflags & NOTE_EXTEND) *revents |= events & POLLEXTEND;
        if (o.fflags & NOTE_ATTRIB) *revents |= events & POLLATTRIB;
        if (o.fflags & NOTE_LINK) *revents |= events & POLLNLINK;
        if (o.fflags & NOTE_WRITE) *revents |= events & POLLWRITE;
        break;
    }
}

// What a one-entry poll at timeout 0 answers for `events`, by poll_nocancel's
// rule over the observed filters: register READ, WRITE, VNODE as asked, stop
// at the first failure (POLLNVAL, and with every entry failed no scan at all),
// otherwise report each registered filter that is ready, in registration
// order, through poll_callback.
static int predict(const struct filters *f, int fd, int events)
{
    if (fd < 0) return 0;
    // Two things a kevent made from user space cannot show: EV_POLL, which
    // makes a regular file's READ always ready (vnode_readable_data_count),
    // and EV_OOBAND, which poll sets on READ for PRI|RDBAND and which only a
    // socket's filter clears (filt_sockattach) -- on anything else it stays
    // in the event's flags, and poll_callback then reports PRI|RDBAND.
    struct stat st;
    int is_socket = fstat(fd, &st) == 0 && S_ISSOCK(st.st_mode);
    int is_regular = fstat(fd, &st) == 0 && S_ISREG(st.st_mode);
    struct { int16_t filter; struct obs o; } queue[3];
    int queued = 0;
    if (events & RBITS) {
        int oob = (events & (POLLPRI | POLLRDBAND)) != 0;
        struct obs o = oob ? f->read_oob : f->read;
        if (o.error) return POLLNVAL;
        if (is_regular && !o.active) { o.active = 1; o.flags = EV_ADD | EV_ONESHOT; }
        if (oob && !is_socket) o.flags |= EV_OOBAND;
        if (o.active) { queue[queued].filter = EVFILT_READ; queue[queued].o = o; queued++; }
    }
    if (events & WBITS) {
        if (f->write.error) return POLLNVAL;
        if (f->write.active) { queue[queued].filter = EVFILT_WRITE; queue[queued].o = f->write; queued++; }
    }
    if (events & VBITS) {
        int k = ((events & POLLEXTEND) ? 1 : 0) | ((events & POLLATTRIB) ? 2 : 0) | ((events & POLLNLINK) ? 4 : 0) |
                ((events & POLLWRITE) ? 8 : 0);
        if (f->vnode[k].error) return POLLNVAL;
        if (f->vnode[k].active) { queue[queued].filter = EVFILT_VNODE; queue[queued].o = f->vnode[k]; queued++; }
    }
    int revents = 0;
    for (int i = 0; i < queued; i++) callback(queue[i].filter, queue[i].o, events, &revents);
    return revents;
}

static void sweep(const char *state, int fd)
{
    struct filters f = observe_all(fd);
    printf("S\t%s\tfilters", state);
    print_obs("READ", f.read);
    print_obs("READ+OOBAND", f.read_oob);
    print_obs("WRITE", f.write);
    print_obs("VNODE(EXTEND)", f.vnode[1]);
    int vnode_uniform = 1;
    for (int k = 2; k < 16; k++)
        if (f.vnode[k].error != f.vnode[1].error || f.vnode[k].active != f.vnode[1].active) vnode_uniform = 0;
    printf(" vnode-uniform=%d\n", vnode_uniform);

    int mismatches = 0, rv_mismatches = 0, failures = 0, first = 0;
    int distinct[65536];
    int ndistinct = 0;
    for (int ev = 0; ev < 65536; ev++) {
        struct pollfd p = { fd, (short)ev, 0 };
        errno = 0;
        int rv = poll(&p, 1, 0);
        if (rv < 0) { failures++; continue; }
        int got = (unsigned short)p.revents;
        if (rv != (got != 0)) rv_mismatches++;
        int want = predict(&f, fd, ev);
        if (got != want) {
            mismatches++;
            if (first++ < 4) printf("S\t%s\tMISMATCH events=%s poll=%s predicted=%s\n", state, b(ev), b(got), b(want));
        }
        int seen = 0;
        for (int i = 0; i < ndistinct; i++) if (distinct[i] == got) { seen = 1; break; }
        if (!seen) distinct[ndistinct++] = got;
    }
    printf("S\t%s\tsweep 65536 masks: mismatches=%d rv-mismatches=%d failures=%d distinct-revents=%d\n", state,
           mismatches, rv_mismatches, failures, ndistinct);

    int show[] = { 0, POLLIN, POLLPRI, POLLOUT, POLLERR, POLLHUP, POLLNVAL, POLLRDNORM, POLLRDBAND, POLLWRBAND,
                   POLLEXTEND, POLLATTRIB, POLLNLINK, POLLWRITE, 0x2000, 0x4000, 0x8000, POLLIN | POLLOUT,
                   POLLIN | POLLEXTEND, 0xFFFF };
    printf("S\t%s\tanswers", state);
    for (size_t i = 0; i < sizeof show / sizeof show[0]; i++) {
        struct pollfd p = { fd, (short)show[i], 0 };
        int rv = poll(&p, 1, 0);
        printf(" %s->%s(rv=%d)", b(show[i]), b((unsigned short)p.revents), rv);
    }
    printf("\n");
}

// ---- sockets ------------------------------------------------------------

static socklen_t loopback(int family, struct sockaddr_storage *ss, int port)
{
    memset(ss, 0, sizeof *ss);
    if (family == AF_INET) {
        struct sockaddr_in *a = (struct sockaddr_in *)ss;
        a->sin_family = AF_INET;
        a->sin_len = sizeof *a;
        a->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        a->sin_port = htons(port);
        return sizeof *a;
    }
    struct sockaddr_in6 *a = (struct sockaddr_in6 *)ss;
    a->sin6_family = AF_INET6;
    a->sin6_len = sizeof *a;
    a->sin6_addr = in6addr_loopback;
    a->sin6_port = htons(port);
    return sizeof *a;
}

static int port_of(int s)
{
    struct sockaddr_storage ss;
    socklen_t len = sizeof ss;
    getsockname(s, (struct sockaddr *)&ss, &len);
    return ss.ss_family == AF_INET ? ntohs(((struct sockaddr_in *)&ss)->sin_port)
                                   : ntohs(((struct sockaddr_in6 *)&ss)->sin6_port);
}

static int listener(int family)
{
    int s = socket(family, SOCK_STREAM, 0);
    struct sockaddr_storage ss;
    socklen_t len = loopback(family, &ss, 0);
    if (bind(s, (struct sockaddr *)&ss, len) != 0) { perror("bind"); exit(1); }
    if (listen(s, 8) != 0) { perror("listen"); exit(1); }
    return s;
}

static void set_nonblock(int s) { fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK); }

static int connect_to(int s, int family, int port)
{
    struct sockaddr_storage ss;
    socklen_t len = loopback(family, &ss, port);
    errno = 0;
    int rv = connect(s, (struct sockaddr *)&ss, len);
    return rv < 0 ? errno : 0;
}

static int client(int family, int port)
{
    int s = socket(family, SOCK_STREAM, 0);
    set_nonblock(s);
    connect_to(s, family, port);
    return s;
}

static int closed_port(int family)
{
    int l = listener(family);
    int port = port_of(l);
    close(l);
    return port;
}

static void pair(int family, int *c, int *a)
{
    int l = listener(family);
    *c = client(family, port_of(l));
    settle();
    *a = accept(l, NULL, NULL);
    close(l);
}

static void socket_states(int family)
{
    const char *fam = family == AF_INET ? "v4" : "v6";
    char name[96];
#define N(s) (snprintf(name, sizeof name, "tcp%s %s", fam, s), name)
    {
        int s = socket(family, SOCK_STREAM, 0);
        sweep(N("unbound"), s);
        struct sockaddr_storage ss;
        socklen_t len = loopback(family, &ss, 0);
        bind(s, (struct sockaddr *)&ss, len);
        sweep(N("bound"), s);
        close(s);
    }
    {
        int l = listener(family);
        sweep(N("listener empty"), l);
        int c = client(family, port_of(l));
        settle();
        sweep(N("listener queued"), l);
        close(c);
        close(l);
    }
    {
        int c, a;
        pair(family, &c, &a);
        sweep(N("connected client"), c);
        sweep(N("accepted"), a);
        shutdown(a, SHUT_WR);
        settle();
        sweep(N("peer shut down writing"), c);
        sweep(N("own write side shut down"), a);
        close(a);
        settle();
        sweep(N("peer closed"), c);
        close(c);
    }
    {
        int c, a;
        pair(family, &c, &a);
        sweep(N("own write side shut down, peer alive"), (shutdown(c, SHUT_WR), settle(), c));
        close(c);
        close(a);
    }
    {
        int c, a;
        pair(family, &c, &a);
        write(a, "hello", 5);
        settle();
        sweep(N("data waiting"), c);
        close(c);
        close(a);
    }
    {
        int c, a;
        pair(family, &c, &a);
        struct linger lg = { 1, 0 };
        setsockopt(a, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
        close(a);
        settle();
        sweep(N("reset by peer"), c);
        close(c);
    }
    {
        int s = socket(family, SOCK_STREAM, 0);
        set_nonblock(s);
        int err = connect_to(s, family, closed_port(family));
        printf("#\t%s connect to closed port: %s\n", fam, ename(err));
        settle();
        sweep(N("refused, error pending"), s);
        int so_error = 0;
        socklen_t len = sizeof so_error;
        getsockopt(s, SOL_SOCKET, SO_ERROR, &so_error, &len);
        printf("#\t%s SO_ERROR=%s\n", fam, ename(so_error));
        sweep(N("refused, error taken"), s);
        close(s);
    }
#undef N
}

static void other_sockets(void)
{
    int u = socket(AF_INET, SOCK_DGRAM, 0);
    sweep("udp fresh", u);
    struct sockaddr_storage ss;
    socklen_t len = loopback(AF_INET, &ss, 9);
    connect(u, (struct sockaddr *)&ss, len);
    sweep("udp connected", u);
    close(u);

    int sv[2];
    socketpair(AF_UNIX, SOCK_STREAM, 0, sv);
    sweep("unix stream pair", sv[0]);
    close(sv[1]);
    settle();
    sweep("unix stream peer closed", sv[0]);
    close(sv[0]);
    socketpair(AF_UNIX, SOCK_DGRAM, 0, sv);
    sweep("unix dgram pair", sv[0]);
    close(sv[0]);
    close(sv[1]);
}

// ---- pipes, files, and the rest -----------------------------------------

static void pipes(void)
{
    int p[2];
    pipe(p);
    sweep("pipe read end, empty", p[0]);
    sweep("pipe write end, room", p[1]);
    write(p[1], "abc", 3);
    sweep("pipe read end, holding 3", p[0]);
    close(p[1]);
    sweep("pipe read end, holding 3, writer closed", p[0]);
    char buf[8];
    read(p[0], buf, sizeof buf);
    sweep("pipe read end, empty, writer closed", p[0]);
    close(p[0]);

    pipe(p);
    set_nonblock(p[1]);
    static char block[65536];
    long total = 0;
    for (;;) {
        ssize_t n = write(p[1], block, sizeof block);
        if (n <= 0) break;
        total += n;
    }
    for (;;) {
        ssize_t n = write(p[1], block, 1);
        if (n <= 0) break;
        total += n;
    }
    printf("#\tpipe filled with %ld bytes\n", total);
    sweep("pipe write end, full", p[1]);
    close(p[0]);
    sweep("pipe write end, reader closed", p[1]);
    close(p[1]);
}

static void files(void)
{
    char path[] = "/tmp/poll-darwin-XXXXXX";
    int fd = mkstemp(path);
    close(fd);
    fd = open(path, O_RDONLY);
    sweep("regular file O_RDONLY, empty", fd);
    close(fd);
    fd = open(path, O_WRONLY);
    write(fd, "data", 4);
    sweep("regular file O_WRONLY, 4 bytes", fd);
    close(fd);
    fd = open(path, O_RDONLY);
    sweep("regular file O_RDONLY, 4 bytes, offset 0", fd);
    lseek(fd, 0, SEEK_END);
    sweep("regular file O_RDONLY, 4 bytes, at end", fd);
    close(fd);
    fd = open(path, O_RDWR);
    sweep("regular file O_RDWR", fd);
    close(fd);
    unlink(path);

    fd = open("/tmp", O_RDONLY | O_DIRECTORY);
    sweep("directory", fd);
    close(fd);
    fd = open("/dev/null", O_RDWR);
    sweep("/dev/null", fd);
    close(fd);

    int kq = kqueue();
    sweep("kqueue, empty", kq);
    int l = listener(AF_INET);
    int c = client(AF_INET, port_of(l));
    settle();
    struct kevent ch;
    EV_SET(&ch, l, EVFILT_READ, EV_ADD, 0, 0, NULL);
    kevent(kq, &ch, 1, NULL, 0, NULL);
    sweep("kqueue, an event pending", kq);
    close(kq);
    close(c);
    close(l);

    // A number well above any this probe opens, so that the kqueues `observe`
    // makes cannot take it.
    int spare = 900;
    if (fcntl(spare, F_GETFD) != -1) { printf("#\tfd 900 is open\n"); exit(1); }
    sweep("closed descriptor", spare);
    sweep("descriptor -1", -1);
}

// ---- M: several entries -------------------------------------------------

static void multi(const char *label, struct pollfd *p, int n)
{
    int rv = poll(p, n, 0);
    printf("M\t%s\trv=%d\t", label, rv);
    for (int i = 0; i < n; i++) printf("%s[%d] %s", i ? "; " : "", i, b((unsigned short)p[i].revents));
    printf("\n");
}

static void section_m(void)
{
    int l = listener(AF_INET);
    int c0 = client(AF_INET, port_of(l));
    settle();
    int ld = dup(l);
    int idle = socket(AF_INET, SOCK_STREAM, 0);
    int spare = open("/dev/null", O_RDONLY);
    close(spare);

    {
        struct pollfd p[] = { { l, POLLIN, 0 }, { l, POLLIN, 0 } };
        multi("M1 queued listener twice, IN and IN", p, 2);
    }
    {
        struct pollfd p[] = { { l, POLLIN, 0 }, { ld, POLLIN, 0 } };
        multi("M2 queued listener and its dup, IN and IN", p, 2);
    }
    {
        struct pollfd p[] = { { l, POLLIN | POLLEXTEND, 0 }, { idle, POLLIN, 0 } };
        multi("M3 queued listener IN|EXTEND beside an idle socket IN", p, 2);
    }
    {
        struct pollfd p[] = { { l, POLLIN | POLLEXTEND, 0 } };
        multi("M4 queued listener IN|EXTEND alone", p, 1);
    }
    {
        struct pollfd p[] = { { l, POLLIN | POLLEXTEND, 0 }, { -1, POLLIN, 0 } };
        multi("M5 queued listener IN|EXTEND beside fd -1", p, 2);
    }
    {
        struct pollfd p[] = { { -1, POLLIN, 0 }, { spare, POLLIN, 0 } };
        multi("M6 fd -1 IN beside a closed fd IN", p, 2);
    }
    {
        struct pollfd p[] = { { spare, 0, 0 }, { spare, POLLIN, 0 } };
        multi("M7 closed fd 0 beside closed fd IN", p, 2);
    }
    {
        struct pollfd p[] = { { spare, POLLIN, 0 }, { l, POLLIN, 0 } };
        multi("M8 closed fd IN beside queued listener IN", p, 2);
    }
    {
        struct pollfd p[] = { { l, POLLIN, 0 }, { l, POLLIN | POLLEXTEND, 0 } };
        multi("M9 queued listener IN, then the same IN|EXTEND", p, 2);
    }
    {
        struct pollfd p[] = { { l, POLLIN | POLLEXTEND, 0 }, { l, POLLIN, 0 } };
        multi("M10 queued listener IN|EXTEND, then the same IN", p, 2);
    }

    int c, a;
    pair(AF_INET, &c, &a);
    {
        struct pollfd p[] = { { c, POLLOUT, 0 }, { c, POLLIN, 0 } };
        multi("M11 connected client OUT, then the same IN", p, 2);
    }
    {
        struct pollfd p[] = { { c, POLLOUT, 0 }, { c, POLLOUT, 0 } };
        multi("M12 connected client OUT twice", p, 2);
    }
    close(a);
    settle();
    {
        struct pollfd p[] = { { c, POLLIN | POLLOUT, 0 }, { c, POLLIN, 0 } };
        multi("M13 peer-closed client IN|OUT, then the same IN", p, 2);
    }
    {
        struct pollfd p[] = { { c, POLLIN | POLLOUT, 0 }, { c, POLLOUT, 0 } };
        multi("M14 peer-closed client IN|OUT, then the same OUT", p, 2);
    }
    {
        struct pollfd p[] = { { c, POLLOUT, 0 }, { c, POLLIN, 0 } };
        multi("M15 peer-closed client OUT, then the same IN", p, 2);
    }
    {
        struct pollfd p[] = { { c, POLLIN, 0 }, { c, POLLOUT, 0 } };
        multi("M16 peer-closed client IN, then the same OUT", p, 2);
    }
    close(c);

    // A READ re-registered by a later entry with a different EV_OOBAND, on
    // targets whose READ keeps the flag: does the re-ADD take the later flag?
    {
        int pp[2];
        pipe(pp);
        write(pp[1], "abc", 3);
        struct pollfd p1[] = { { pp[0], POLLIN, 0 }, { pp[0], POLLPRI, 0 } };
        multi("M17 pipe holding 3, IN then PRI", p1, 2);
        struct pollfd p2[] = { { pp[0], POLLPRI, 0 }, { pp[0], POLLIN, 0 } };
        multi("M18 pipe holding 3, PRI then IN", p2, 2);
        struct pollfd p3[] = { { pp[0], POLLIN, 0 }, { pp[0], POLLIN | POLLPRI, 0 } };
        multi("M19 pipe holding 3, IN then IN|PRI", p3, 2);
        struct pollfd p4[] = { { pp[0], POLLPRI, 0 }, { pp[0], POLLIN | POLLRDNORM, 0 } };
        multi("M20 pipe holding 3, PRI then IN|RDNORM", p4, 2);
        struct pollfd p5[] = { { pp[0], POLLIN | POLLOUT, 0 }, { pp[0], POLLOUT, 0 } };
        multi("M21 pipe read end holding 3, IN|OUT then OUT", p5, 2);
        close(pp[1]);
        struct pollfd p6[] = { { pp[0], POLLIN, 0 }, { pp[0], POLLPRI, 0 } };
        multi("M22 pipe holding 3, writer closed, IN then PRI", p6, 2);
        close(pp[0]);
        char path[] = "/tmp/poll-darwin-m-XXXXXX";
        int f = mkstemp(path);
        struct pollfd p7[] = { { f, POLLIN, 0 }, { f, POLLPRI, 0 } };
        multi("M23 regular file, IN then PRI", p7, 2);
        struct pollfd p8[] = { { f, POLLPRI, 0 }, { f, POLLIN, 0 } };
        multi("M24 regular file, PRI then IN", p8, 2);
        struct pollfd p9[] = { { f, POLLIN | POLLEXTEND, 0 }, { f, POLLEXTEND, 0 } };
        multi("M25 regular file, IN|EXTEND then EXTEND", p9, 2);
        close(f);
        unlink(path);
    }
    close(c0);
    close(ld);
    close(l);
    close(idle);
}

// ---- W: waits -----------------------------------------------------------

struct act { int what; int fd; int fd2; int family; int delay_ms; };

static void *actor(void *arg)
{
    struct act *x = arg;
    sleep_ms(x->delay_ms);
    switch (x->what) {
    case 0: x->fd2 = client(x->family, x->fd); break;      // connect to port x->fd
    case 1: close(x->fd); break;                           // close
    case 2: write(x->fd, "z", 1); break;                   // write a byte
    }
    return NULL;
}

static void timed(const char *label, struct pollfd *p, int n, int timeout)
{
    long long t0 = now_us();
    errno = 0;
    int rv = poll(p, n, timeout);
    int e = errno;
    long long dt = (now_us() - t0) / 1000;
    printf("W\t%s\trv=%d\t%s\tafter %lld ms\t", label, rv, rv < 0 ? ename(e) : "-", dt);
    for (int i = 0; i < n; i++) printf("%s%s", i ? "; " : "", b((unsigned short)p[i].revents));
    printf("\n");
}

static void with_actor(struct act *x)
{
    pthread_t t;
    pthread_create(&t, NULL, actor, x);
    pthread_detach(t);
}

// A forked child sharing socket `s` polls it for `events` (2000 ms); the
// parent stops the child once it sleeps, runs `events_while_stopped`, lets
// loopback settle, and resumes it.
static void stopped_poller(const char *label, int s, int events, void (*events_while_stopped)(int, void *), void *ctx)
{
    int out[2];
    pipe(out);
    pid_t child = fork();
    if (child == 0) {
        alarm(10);
        struct pollfd p = { s, (short)events, 0 };
        long long t0 = now_us();
        int rv = poll(&p, 1, 2000);
        long long dt = (now_us() - t0) / 1000;
        char line[256];
        int len = snprintf(line, sizeof line, "rv=%d\t%s\tafter %lld ms", rv, b((unsigned short)p.revents), dt);
        write(out[1], line, (size_t)len);
        _exit(0);
    }
    sleep_ms(100);
    kill(child, SIGSTOP);
    int status;
    waitpid(child, &status, WUNTRACED);
    events_while_stopped(s, ctx);
    sleep_ms(100);
    kill(child, SIGCONT);
    close(out[1]);
    char line[256] = { 0 };
    read(out[0], line, sizeof line - 1);
    waitpid(child, &status, 0);
    close(out[0]);
    printf("W\t%s\t%s\n", label, line);
}

struct w6 { int listener; int accepted; int fin; int refuse; };

static void w6_events(int s, void *ctx)
{
    struct w6 *x = ctx;
    if (x->refuse) {
        connect_to(s, AF_INET, closed_port(AF_INET));
        return;
    }
    connect_to(s, AF_INET, port_of(x->listener));
    settle();
    x->accepted = accept(x->listener, NULL, NULL);
    if (x->fin) close(x->accepted);
}

static volatile int hogging;

static void *hog(void *arg)
{
    (void)arg;
    while (hogging) { }
    return NULL;
}

// W11: the W6 events again, with the poller made a background thread and every
// CPU busy while the events arrive, so that the poller is likely to be woken by
// the first and to run only after the second. Tallies the answers.
static void race_trials(const char *label, int fin, int trials)
{
    int ncpu = (int)sysconf(_SC_NPROCESSORS_ONLN);
    char seen[8][64];
    int counts[8] = { 0 };
    int nseen = 0;
    for (int t = 0; t < trials; t++) {
        int l = listener(AF_INET);
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        int out[2];
        pipe(out);
        pid_t child = fork();
        if (child == 0) {
            alarm(10);
            setpriority(PRIO_DARWIN_THREAD, 0, PRIO_DARWIN_BG);
            struct pollfd p = { s, POLLIN | POLLOUT, 0 };
            poll(&p, 1, 3000);
            char line[64];
            int len = snprintf(line, sizeof line, "%s", b((unsigned short)p.revents));
            write(out[1], line, (size_t)len);
            _exit(0);
        }
        close(out[1]);
        sleep_ms(100);
        hogging = 1;
        pthread_t hogs[64];
        int nh = ncpu < 64 ? ncpu : 64;
        for (int i = 0; i < nh; i++) pthread_create(&hogs[i], NULL, hog, NULL);
        sleep_ms(5);
        connect_to(s, AF_INET, port_of(l));
        int a = accept(l, NULL, NULL);
        if (fin) close(a);
        sleep_ms(20);
        hogging = 0;
        for (int i = 0; i < nh; i++) pthread_join(hogs[i], NULL);
        char line[64] = { 0 };
        read(out[0], line, sizeof line - 1);
        int status;
        waitpid(child, &status, 0);
        close(out[0]);
        if (!fin) close(a);
        close(s);
        close(l);
        int k;
        for (k = 0; k < nseen; k++) if (strcmp(seen[k], line) == 0) break;
        if (k == nseen && nseen < 8) { snprintf(seen[nseen], sizeof seen[nseen], "%s", line); nseen++; }
        if (k < 8) counts[k]++;
    }
    printf("W\t%s\t%d trials:", label, trials);
    for (int k = 0; k < nseen; k++) printf(" %s x%d", seen[k], counts[k]);
    printf("\n");
}

static void section_w(void)
{
    race_trials("W11 idle socket IN|OUT, background poller, CPUs busy; connect, accept, peer close", 1, 40);
    race_trials("W11b the same without the peer's close", 0, 20);
    {
        int l = listener(AF_INET);
        struct act x = { 0, port_of(l), -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { l, POLLIN, 0 };
        timed("W1 empty listener IN, a connection at 50 ms", &p, 1, 1000);
        sleep_ms(20);
        close(x.fd2);
        close(l);
    }
    {
        int l = listener(AF_INET);
        struct act x = { 0, port_of(l), -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { l, POLLHUP, 0 };
        timed("W2 empty listener HUP, a connection at 50 ms", &p, 1, 300);
        close(x.fd2);
        close(l);
    }
    {
        int l = listener(AF_INET);
        struct act x = { 0, port_of(l), -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { l, POLLHUP | POLLOUT, 0 };
        timed("W3 empty listener HUP|OUT, a connection at 50 ms", &p, 1, 300);
        close(x.fd2);
        close(l);
    }
    {
        int c, a;
        pair(AF_INET, &c, &a);
        struct act x = { 1, a, -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { c, POLLIN, 0 };
        timed("W4 connected client IN, the peer closes at 50 ms", &p, 1, 1000);
        close(c);
    }
    {
        int c, a;
        pair(AF_INET, &c, &a);
        struct act x = { 1, a, -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { c, POLLHUP, 0 };
        timed("W5 connected client HUP, the peer closes at 50 ms", &p, 1, 1000);
        close(c);
    }
    {
        int l = listener(AF_INET);
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        struct w6 x = { l, -1, 1, 0 };
        stopped_poller("W6 idle socket IN|OUT; connect, accept, peer close while stopped", s, POLLIN | POLLOUT,
                       w6_events, &x);
        struct pollfd p = { s, POLLIN | POLLOUT, 0 };
        timed("W6 control: a fresh poll IN|OUT in that state", &p, 1, 0);
        close(s);
        close(l);
    }
    {
        int l = listener(AF_INET);
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        struct w6 x = { l, -1, 0, 0 };
        stopped_poller("W6b idle socket IN|OUT; connect, accept while stopped", s, POLLIN | POLLOUT, w6_events, &x);
        close(x.accepted);
        close(s);
        close(l);
    }
    {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        struct w6 x = { -1, -1, 0, 1 };
        stopped_poller("W6c idle socket IN|OUT; connect refused while stopped", s, POLLIN | POLLOUT, w6_events, &x);
        close(s);
    }
    {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        struct w6 x = { -1, -1, 0, 1 };
        stopped_poller("W6d idle socket OUT; connect refused while stopped", s, POLLOUT, w6_events, &x);
        close(s);
    }
    {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        set_nonblock(s);
        // Refused by this thread's own connect, then poll: control for W6c.
        connect_to(s, AF_INET, closed_port(AF_INET));
        settle();
        struct pollfd p = { s, POLLIN | POLLOUT, 0 };
        timed("W6c control: a fresh poll IN|OUT on a refused socket", &p, 1, 0);
        close(s);
    }
    {
        int pp[2];
        pipe(pp);
        write(pp[1], "abc", 3);
        struct act x = { 1, pp[1], -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { pp[0], POLLHUP, 0 };
        timed("W7 pipe read end holding 3, HUP, the writer closes at 50 ms", &p, 1, 300);
        close(pp[0]);
    }
    {
        int pp[2];
        pipe(pp);
        struct act x = { 1, pp[1], -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { pp[0], POLLHUP, 0 };
        timed("W7 control: empty pipe read end, HUP, the writer closes at 50 ms", &p, 1, 300);
        close(pp[0]);
    }
    {
        int pp[2];
        pipe(pp);
        struct act x = { 2, pp[1], -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { pp[0], POLLIN, 0 };
        timed("W8 empty pipe read end IN, a write at 50 ms", &p, 1, 1000);
        close(pp[0]);
        close(pp[1]);
    }
    {
        int spare = open("/dev/null", O_RDONLY);
        close(spare);
        struct pollfd p = { spare, 0, 0 };
        timed("W9 a closed descriptor asking 0, timeout 200", &p, 1, 200);
        timed("W9 nfds 0, timeout 200", NULL, 0, 200);
    }
    {
        int l = listener(AF_INET);
        int c = client(AF_INET, port_of(l));
        settle();
        struct act x = { 0, port_of(l), -1, AF_INET, 50 };
        with_actor(&x);
        struct pollfd p = { l, POLLHUP, 0 };
        timed("W10 queued listener HUP, another connection at 50 ms", &p, 1, 300);
        close(x.fd2);
        close(c);
        close(l);
    }
}

// ---- N: nfds and the buffer ---------------------------------------------

static void section_n(void)
{
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    printf("N\tRLIMIT_NOFILE cur=%llu max=%llu, uid %d\n", (unsigned long long)rl.rlim_cur,
           (unsigned long long)rl.rlim_max, (int)getuid());
    size_t room = 70000;
    struct pollfd *many = calloc(room, sizeof *many);
    for (size_t i = 0; i < room; i++) { many[i].fd = -1; many[i].events = POLLIN; }
    unsigned int counts[] = { 0, 1, 64, 65, 1024, 1025, 2048, 2049, 10240, 10241, 65536, 0x7fffffffu, 0x80000000u,
                              0xffffffffu };
    rlim_t limits[] = { rl.rlim_cur, 64, 2048 };
    for (size_t li = 0; li < sizeof limits / sizeof limits[0]; li++) {
        struct rlimit low = { limits[li], rl.rlim_max };
        if (setrlimit(RLIMIT_NOFILE, &low) != 0) { printf("N\tsetrlimit %llu failed\n", (unsigned long long)limits[li]); continue; }
        for (size_t k = 0; k < sizeof counts / sizeof counts[0]; k++) {
            for (int buffer = 0; buffer < 2; buffer++) {
                errno = 0;
                int rv = poll(buffer ? many : NULL, counts[k], 0);
                int e = errno;
                printf("N\tlimit=%llu nfds=%u buffer=%s\trv=%d\t%s\n", (unsigned long long)limits[li], counts[k],
                       buffer ? "70000 entries" : "NULL", rv, rv < 0 ? ename(e) : "-");
            }
        }
    }
    setrlimit(RLIMIT_NOFILE, &rl);
    free(many);

    // A buffer that can be read but not written: revents cannot be copied out.
    long page = sysconf(_SC_PAGESIZE);
    struct pollfd *ro = mmap(NULL, (size_t)page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    int l = listener(AF_INET);
    int c = client(AF_INET, port_of(l));
    settle();
    ro[0].fd = -1; ro[0].events = POLLIN; ro[0].revents = 0;
    ro[1].fd = l; ro[1].events = POLLIN; ro[1].revents = 0;
    mprotect(ro, (size_t)page, PROT_READ);
    errno = 0;
    int rv = poll(ro, 1, 0);
    printf("N\tread-only buffer, fd -1\trv=%d\t%s\n", rv, rv < 0 ? ename(errno) : "-");
    errno = 0;
    rv = poll(ro + 1, 1, 0);
    printf("N\tread-only buffer, a ready listener\trv=%d\t%s\n", rv, rv < 0 ? ename(errno) : "-");
    errno = 0;
    rv = poll(ro + 1, 1, 100);
    printf("N\tread-only buffer, a ready listener, timeout 100\trv=%d\t%s\n", rv, rv < 0 ? ename(errno) : "-");
    errno = 0;
    long long t0 = now_us();
    rv = poll(ro, 1, 100);
    printf("N\tread-only buffer, fd -1, timeout 100\trv=%d\t%s\tafter %lld ms\n", rv, rv < 0 ? ename(errno) : "-",
           (now_us() - t0) / 1000);
    close(c);
    close(l);
}

int main(void)
{
    setvbuf(stdout, NULL, _IOLBF, 0);
    alarm(120);
    signal(SIGPIPE, SIG_IGN);
    printf("# poll-darwin\n");
    socket_states(AF_INET);
    socket_states(AF_INET6);
    other_sockets();
    pipes();
    files();
    section_m();
    section_w();
    section_n();
    return 0;
}
