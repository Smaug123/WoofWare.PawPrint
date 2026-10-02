// What an EVFILT_WRITE event's `data` holds for a TCP socket on Darwin: the
// free space in the socket's send buffer, `sbspace(&so->so_snd)`. Measured for
// every state in which the filter is ready and nothing has been sent: an
// established connection, at either end, and a refused connect.
//
// Sections:
//   E   established: for each route (IPv4 127.0.0.1, IPv4 to 0.0.0.0, the
//       machine's own non-loopback IPv4 address, IPv6 ::1, and an IPv6 socket
//       to ::ffff:127.0.0.1), TRIALS connections, and for each end (the
//       connecting one and the accepted one): the WRITE data under EV_CLEAR
//       and level-triggered registration, on first delivery and again (a
//       second level wait, and an EV_CLEAR re-ADD); SO_SNDBUF; TCP_MAXSEG; and
//       whether it equals the prediction below. Then, on the same connection,
//       after the peer has sent 1000 bytes (unread, and then read), and after
//       this end has read them.
//   B   SO_SNDBUF set on the connecting socket before connect, to each of a
//       sweep of sizes, over 127.0.0.1, the machine's own address and ::1: the
//       established WRITE data against the prediction. SO_SNDBUF stands in for
//       net.inet.tcp.sendspace, which cannot be changed without root: both only
//       set the buffer's sb_hiwat before the handshake, and tcp_mss reads
//       nothing else of it.
//   R   refused: a non-blocking connect to a port nothing listens on, IPv4
//       and IPv6, TRIALS times each; then with SO_SNDBUF set before connect to
//       each of a sweep of sizes; then a blocking refusal. The WRITE data
//       against min(SO_SNDBUF, 2048).
//
// The predictions, from XNU (xnu-12377.121.6, the newest published source;
// this machine runs xnu-13432.1.9):
//   * filt_sowrite_common (bsd/kern/uipc_socket.c) reports
//     data = sbspace(&so->so_snd), whatever the readiness.
//   * sbspace (bsd/kern/uipc_socket2.c) is
//     min(sb_hiwat - sb_cc, sb_mbmax - sb_mbcnt), further capped at
//     sb_preconn_hiwat - sb_cc when sb_preconn_hiwat is non-zero. With nothing
//     queued sb_cc = sb_mbcnt = 0, and sb_mbmax = 8 * sb_hiwat (sbreserve's
//     sb_efficiency), so it is sb_hiwat, or min(sb_hiwat, sb_preconn_hiwat).
//   * tcp_attach (bsd/netinet/tcp_usrreq.c) reserves sb_hiwat =
//     net.inet.tcp.sendspace, and sets sb_preconn_hiwat = 2048, a constant;
//     only soisconnected clears it (soreserve_preconnect(so, 0)). A refused
//     socket never connected, so its space is min(sb_hiwat, 2048).
//   * tcp_mss (bsd/netinet/tcp_input.c), on the handshake, takes
//     b = max(the route's sendpipe, sb_hiwat), and unless b < mss sets
//     sb_hiwat = ceil(b / mss) * mss, which sbreserve caps at sb_max
//     (kern.ipc.maxsockbuf). mss = the route's MTU - the IP and TCP headers (40
//     for IPv4, 60 for IPv6) - 12 for the timestamp option when both ends use
//     it. When b < mss, sb_hiwat is left alone and the segment shrinks to it.
//     An accepted socket's sb_hiwat starts as its listener's (sonewconn), and
//     goes through the same rounding.
//   * Every route here is on lo0, MTU 16384 (LOMTU, bsd/net/if_loop.c). The
//     host route to 127.0.0.1 has sendpipe 3 * LOMTU = 49152 (lo_rtrequest);
//     the routes to ::1 and to the machine's own address have none
//     (`route -n get` shows 0).
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -O1 -o /tmp/kwd kevent-write-data.c && /tmp/kwd
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64 (xnu-13432.1.9), sendspace
// 131072, kern.ipc.maxsockbuf 8388608, lo0 MTU 16384;
// kevent-write-data.darwin-27.0.txt is one run's output, and three runs were
// identical. Every row matched its prediction:
//   * established (E): both ends, every route, EV_CLEAR and level alike, on
//     first delivery and again, and with bytes received unread and read, 50
//     connections per route: SO_SNDBUF, which is 146988 over IPv4 (mss 16332 =
//     16384 - 40 - 12, nine segments) and 146808 over ::1 (mss 16312). The
//     IPv6 socket connected to ::ffff:127.0.0.1 is an IPv4 connection, so
//     146988. (Over the machine's own address the peer's write answers EPIPE,
//     in every run: something on this machine ends that flow once data moves,
//     so that route's receiving rows measure nothing.)
//   * SO_SNDBUF before connect (B): every size followed the rule, including
//     sendpipe's floor on 127.0.0.1 (every size up to 65328 gives 65328 there,
//     and keeps a size below a segment on the other routes) and sb_max's cap.
//   * refused (R): 2048 with the default buffer, 50 refusals per family, and
//     min(SO_SNDBUF, 2048) for every size swept, under EV_CLEAR and level, and
//     after a blocking refusal as after a non-blocking one: the constant
//     preconnect cap, not the configured send space.
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <time.h>
#include <unistd.h>

#define TRIALS 50

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static void die(const char *what)
{
    printf("#\tFATAL %s: %s\n", what, strerror(errno));
    exit(1);
}

static int sysctl_int(const char *name)
{
    int v = -1;
    size_t len = sizeof v;
    if (sysctlbyname(name, &v, &len, NULL, 0) != 0) return -1;
    return v;
}

static int sndbuf(int s)
{
    int v = -1;
    socklen_t len = sizeof v;
    getsockopt(s, SOL_SOCKET, SO_SNDBUF, &v, &len);
    return v;
}

static int maxseg(int s)
{
    int v = -1;
    socklen_t len = sizeof v;
    getsockopt(s, IPPROTO_TCP, TCP_MAXSEG, &v, &len);
    return v;
}

static void set_sndbuf(int s, int size)
{
    if (setsockopt(s, SOL_SOCKET, SO_SNDBUF, &size, sizeof size) != 0) die("SO_SNDBUF");
}

static void set_nonblock(int s, int on)
{
    int fl = fcntl(s, F_GETFL);
    fcntl(s, F_SETFL, on ? (fl | O_NONBLOCK) : (fl & ~O_NONBLOCK));
}

// The WRITE event a fresh registration on `s` reports, waiting up to a second.
// `flags` is EV_ADD or EV_ADD|EV_CLEAR. Fills `again` with what a second wait
// reports (-1 for no event), which for EV_CLEAR is after a re-ADD.
struct write_report {
    int ok;
    int64_t data;
    uint16_t flags;
    int64_t again;
};

static struct write_report write_event(int s, uint16_t flags)
{
    struct write_report r = { 0, -1, 0, -1 };
    int kq = kqueue();
    if (kq < 0) die("kqueue");
    struct kevent k;
    EV_SET(&k, (uintptr_t)s, EVFILT_WRITE, flags, 0, 0, NULL);
    if (kevent(kq, &k, 1, NULL, 0, NULL) != 0) die("kevent ADD");
    struct kevent out[2];
    struct timespec second = { 1, 0 };
    int n = kevent(kq, NULL, 0, out, 2, &second);
    if (n == 1 && out[0].filter == EVFILT_WRITE) {
        r.ok = 1;
        r.data = out[0].data;
        r.flags = out[0].flags;
    }
    if (flags & EV_CLEAR) {
        if (kevent(kq, &k, 1, NULL, 0, NULL) != 0) die("kevent re-ADD");
    }
    n = kevent(kq, NULL, 0, out, 2, &second);
    if (n == 1) r.again = out[0].data;
    close(kq);
    return r;
}

// ---- addresses ----------------------------------------------------------

struct route {
    const char *name;
    int listen_family;
    struct sockaddr_storage listen_at; // port 0
    int connect_family;
    // The destination, its port filled in from the listener.
    struct sockaddr_storage connect_to;
    int usable;
    // The route's sendpipe, as `route -n get` reports it.
    int sendpipe;
};

static socklen_t sa_len(const struct sockaddr_storage *a)
{
    return a->ss_family == AF_INET ? sizeof(struct sockaddr_in) : sizeof(struct sockaddr_in6);
}

static void set_port(struct sockaddr_storage *a, int port)
{
    if (a->ss_family == AF_INET) ((struct sockaddr_in *)a)->sin_port = htons(port);
    else ((struct sockaddr_in6 *)a)->sin6_port = htons(port);
}

static void v4(struct sockaddr_storage *a, uint32_t host_order)
{
    memset(a, 0, sizeof *a);
    struct sockaddr_in *s = (struct sockaddr_in *)a;
    s->sin_len = sizeof *s;
    s->sin_family = AF_INET;
    s->sin_addr.s_addr = htonl(host_order);
}

static void v6(struct sockaddr_storage *a, const char *text)
{
    memset(a, 0, sizeof *a);
    struct sockaddr_in6 *s = (struct sockaddr_in6 *)a;
    s->sin6_len = sizeof *s;
    s->sin6_family = AF_INET6;
    inet_pton(AF_INET6, text, &s->sin6_addr);
}

// The first non-loopback IPv4 address of an interface that is up, or 0.
static uint32_t own_address(char *ifname, size_t n)
{
    struct ifaddrs *list;
    if (getifaddrs(&list) != 0) return 0;
    uint32_t found = 0;
    for (struct ifaddrs *i = list; i; i = i->ifa_next) {
        if (!i->ifa_addr || i->ifa_addr->sa_family != AF_INET) continue;
        if ((i->ifa_flags & IFF_UP) == 0 || (i->ifa_flags & IFF_LOOPBACK)) continue;
        found = ntohl(((struct sockaddr_in *)i->ifa_addr)->sin_addr.s_addr);
        snprintf(ifname, n, "%s", i->ifa_name);
        break;
    }
    freeifaddrs(list);
    return found;
}

static int make_listener(const struct route *r)
{
    int l = socket(r->listen_family, SOCK_STREAM, 0);
    if (l < 0) die("socket");
    int one = 1;
    setsockopt(l, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    if (bind(l, (const struct sockaddr *)&r->listen_at, sa_len(&r->listen_at)) != 0) die("bind");
    if (listen(l, 8) != 0) die("listen");
    return l;
}

static int listener_port(int l)
{
    struct sockaddr_storage a;
    socklen_t len = sizeof a;
    getsockname(l, (struct sockaddr *)&a, &len);
    return a.ss_family == AF_INET ? ntohs(((struct sockaddr_in *)&a)->sin_port)
                                  : ntohs(((struct sockaddr_in6 *)&a)->sin6_port);
}

// A connection over `r`: the connecting end in *c, the accepted one in *a.
// `presize` > 0 sets SO_SNDBUF on the connecting socket before connect.
static int connect_pair(const struct route *r, int presize, int *c, int *a, int *before)
{
    int l = make_listener(r);
    struct sockaddr_storage to = r->connect_to;
    set_port(&to, listener_port(l));
    *c = socket(r->connect_family, SOCK_STREAM, 0);
    if (*c < 0) die("socket");
    if (presize > 0) set_sndbuf(*c, presize);
    *before = sndbuf(*c);
    if (connect(*c, (struct sockaddr *)&to, sa_len(&to)) != 0) {
        printf("#\t%s connect: %s\n", r->name, strerror(errno));
        close(*c);
        close(l);
        return 0;
    }
    *a = accept(l, NULL, NULL);
    close(l);
    if (*a < 0) die("accept");
    return 1;
}

static int sb_max;

static int64_t predicted_established(int hiwat, int sendpipe, int mss)
{
    int64_t b = sendpipe > hiwat ? sendpipe : hiwat;
    if (b < mss) return hiwat;
    int64_t rounded = ((b + mss - 1) / mss) * mss;
    return rounded > sb_max ? sb_max : rounded;
}

// ---- E ------------------------------------------------------------------

struct end_result {
    int64_t clear, clear_again, level, level_again;
    int sndbuf, mss;
};

static struct end_result measure_end(int s)
{
    struct end_result e;
    struct write_report c = write_event(s, EV_ADD | EV_CLEAR);
    struct write_report l = write_event(s, EV_ADD);
    e.clear = c.ok ? c.data : -1;
    e.clear_again = c.again;
    e.level = l.ok ? l.data : -1;
    e.level_again = l.again;
    e.sndbuf = sndbuf(s);
    e.mss = maxseg(s);
    return e;
}

static int end_matches(const struct end_result *e, int64_t want)
{
    return e->clear == want && e->clear_again == want && e->level == want && e->level_again == want
           && e->sndbuf == want;
}

static void section_e(struct route *routes, int nroutes, int sendspace)
{
    for (int ri = 0; ri < nroutes; ri++) {
        struct route *r = &routes[ri];
        if (!r->usable) {
            printf("E\t%s\tskipped (no such address)\n", r->name);
            continue;
        }
        int matched[2] = { 0, 0 };
        int done = 0;
        int64_t first_value[2] = { -1, -1 };
        int first_mss[2] = { -1, -1 };
        int distinct_mismatch = 0;
        for (int t = 0; t < TRIALS; t++) {
            int c, a, before;
            if (!connect_pair(r, 0, &c, &a, &before)) break;
            int ends[2] = { c, a };
            for (int which = 0; which < 2; which++) {
                struct end_result e = measure_end(ends[which]);
                int64_t want = predicted_established(sendspace, r->sendpipe, e.mss);
                if (t == 0) {
                    first_value[which] = e.clear;
                    first_mss[which] = e.mss;
                    printf("E\t%s\t%s\tSO_SNDBUF before connect=%d clear=%lld clear-again=%lld level=%lld "
                           "level-again=%lld SO_SNDBUF=%d TCP_MAXSEG=%d predicted=%lld\n",
                           r->name, which ? "accepted" : "connecting", which ? -1 : before, (long long)e.clear,
                           (long long)e.clear_again, (long long)e.level, (long long)e.level_again, e.sndbuf, e.mss,
                           (long long)want);
                }
                if (end_matches(&e, want) && e.clear == first_value[which] && e.mss == first_mss[which])
                    matched[which]++;
                else if (distinct_mismatch++ < 5)
                    printf("E\t%s\tMISMATCH trial %d %s clear=%lld level=%lld SO_SNDBUF=%d mss=%d want=%lld\n",
                           r->name, t, which ? "accepted" : "connecting", (long long)e.clear, (long long)e.level,
                           e.sndbuf, e.mss, (long long)want);
            }

            if (t == 0) {
                // Receiving: the accepted end sends 1000 bytes to the connecting
                // one, which first leaves them unread and then reads them.
                char buf[1000];
                memset(buf, 'x', sizeof buf);
                ssize_t sent = write(a, buf, sizeof buf);
                if (sent != (ssize_t)sizeof buf)
                    printf("E\t%s\twrite answered %zd: %s\n", r->name, sent, sent < 0 ? strerror(errno) : "-");
                sleep_ms(30);
                struct end_result unread = measure_end(c);
                ssize_t got = 0;
                set_nonblock(c, 1);
                while (got < (ssize_t)sizeof buf) {
                    ssize_t n = read(c, buf, sizeof buf);
                    if (n <= 0) break;
                    got += n;
                }
                struct end_result after = measure_end(c);
                // And the accepted end, which has now sent: its own space once
                // the peer acknowledged everything.
                sleep_ms(30);
                struct end_result sender = measure_end(a);
                printf("E\t%s\tconnecting end, 1000 bytes waiting: clear=%lld level=%lld SO_SNDBUF=%d\n", r->name,
                       (long long)unread.clear, (long long)unread.level, unread.sndbuf);
                printf("E\t%s\tconnecting end, after reading %zd: clear=%lld level=%lld SO_SNDBUF=%d\n", r->name,
                       got, (long long)after.clear, (long long)after.level, after.sndbuf);
                printf("E\t%s\taccepted end, after sending 1000: clear=%lld level=%lld SO_SNDBUF=%d\n", r->name,
                       (long long)sender.clear, (long long)sender.level, sender.sndbuf);
            }
            close(c);
            close(a);
            done++;
        }
        printf("E\t%s\tsweep: %d connections, connecting end matched %d, accepted end matched %d\n", r->name, done,
               matched[0], matched[1]);
    }
}

// ---- B ------------------------------------------------------------------

static void section_b(const struct route *r)
{
    int sizes[] = { 1000, 8192, 16000, 16332, 16333, 20000, 32664, 48996, 48997, 49152, 49153, 50000, 65328, 65329,
                    100000, 131072, 146988, 146989, 200000, 1000000, 8380000, 8388608 };
    for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        int c, a, before;
        if (!connect_pair(r, sizes[i], &c, &a, &before)) continue;
        struct end_result e = measure_end(c);
        // The handshake's rounding uses the MSS before the timestamp option is
        // known to be in use on the first call, and after it on the second;
        // the prediction uses the MSS the connection ended with.
        int64_t want = predicted_established(before, r->sendpipe, e.mss);
        printf("B\t%s\tSO_SNDBUF set %d, read back %d\tclear=%lld level=%lld SO_SNDBUF=%d TCP_MAXSEG=%d "
               "predicted=%lld\t%s\n",
               r->name, sizes[i], before, (long long)e.clear, (long long)e.level, e.sndbuf, e.mss, (long long)want,
               end_matches(&e, want) ? "match" : "MISMATCH");
        close(c);
        close(a);
    }
}

// ---- R ------------------------------------------------------------------

static int closed_port(int family)
{
    struct sockaddr_storage at;
    if (family == AF_INET) v4(&at, INADDR_LOOPBACK);
    else v6(&at, "::1");
    int s = socket(family, SOCK_STREAM, 0);
    if (bind(s, (struct sockaddr *)&at, sa_len(&at)) != 0) die("bind");
    int port = listener_port(s);
    close(s);
    return port;
}

// A non-blocking connect refused; the WRITE event's data under both modes.
static void refused(int family, int presize, int blocking, struct write_report *clear, struct write_report *level,
                    int *buf)
{
    struct sockaddr_storage to;
    if (family == AF_INET) v4(&to, INADDR_LOOPBACK);
    else v6(&to, "::1");
    set_port(&to, closed_port(family));
    int s = socket(family, SOCK_STREAM, 0);
    if (presize > 0) set_sndbuf(s, presize);
    if (!blocking) set_nonblock(s, 1);
    int rv = connect(s, (struct sockaddr *)&to, sa_len(&to));
    int err = rv < 0 ? errno : 0;
    if (blocking ? err != ECONNREFUSED : err != EINPROGRESS) printf("#\tconnect answered %s\n", strerror(err));
    sleep_ms(30);
    *clear = write_event(s, EV_ADD | EV_CLEAR);
    *level = write_event(s, EV_ADD);
    *buf = sndbuf(s);
    close(s);
}

static void section_r(void)
{
    const int families[] = { AF_INET, AF_INET6 };
    for (int f = 0; f < 2; f++) {
        const char *name = families[f] == AF_INET ? "IPv4" : "IPv6";
        int matched = 0;
        for (int t = 0; t < TRIALS; t++) {
            struct write_report c, l;
            int buf;
            refused(families[f], 0, 0, &c, &l, &buf);
            int64_t want = buf < 2048 ? buf : 2048;
            int ok = c.ok && l.ok && c.data == want && c.again == want && l.data == want && l.again == want
                     && (c.flags & EV_EOF) && (l.flags & EV_EOF);
            if (t == 0)
                printf("R\t%s\tclear=%lld (flags 0x%x) clear-again=%lld level=%lld level-again=%lld SO_SNDBUF=%d "
                       "predicted=%lld\n",
                       name, (long long)c.data, c.flags, (long long)c.again, (long long)l.data, (long long)l.again,
                       buf, (long long)want);
            if (ok) matched++;
            else
                printf("R\t%s\tMISMATCH trial %d clear=%lld level=%lld SO_SNDBUF=%d\n", name, t, (long long)c.data,
                       (long long)l.data, buf);
        }
        printf("R\t%s\tsweep: %d refusals, matched %d\n", name, TRIALS, matched);

        int sizes[] = { 1, 512, 1000, 2047, 2048, 2049, 4096, 16384, 100000, 131072, 262144, 1000000 };
        for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
            struct write_report c, l;
            int buf;
            refused(families[f], sizes[i], 0, &c, &l, &buf);
            int64_t want = buf < 2048 ? buf : 2048;
            printf("R\t%s\tSO_SNDBUF set %d, read back %d\tclear=%lld level=%lld predicted=%lld\t%s\n", name,
                   sizes[i], buf, (long long)c.data, (long long)l.data, (long long)want,
                   c.data == want && l.data == want ? "match" : "MISMATCH");
        }

        struct write_report c, l;
        int buf;
        refused(families[f], 0, 1, &c, &l, &buf);
        printf("R\t%s\tblocking refusal\tclear=%lld level=%lld SO_SNDBUF=%d\n", name, (long long)c.data,
               (long long)l.data, buf);
    }
}

int main(void)
{
    alarm(300);
    signal(SIGPIPE, SIG_IGN);
    setvbuf(stdout, NULL, _IOLBF, 0);

    int sendspace = sysctl_int("net.inet.tcp.sendspace");
    sb_max = sysctl_int("kern.ipc.maxsockbuf");
    char ifname[IFNAMSIZ] = "";
    uint32_t own = own_address(ifname, sizeof ifname);
    struct ifreq ifr;
    memset(&ifr, 0, sizeof ifr);
    snprintf(ifr.ifr_name, sizeof ifr.ifr_name, "lo0");
    int probe = socket(AF_INET, SOCK_DGRAM, 0);
    int lo_mtu = ioctl(probe, SIOCGIFMTU, &ifr) == 0 ? ifr.ifr_mtu : -1;
    close(probe);
    printf("#\tnet.inet.tcp.sendspace=%d kern.ipc.maxsockbuf=%d lo0 mtu=%d own address on %s: %s\n", sendspace,
           sysctl_int("kern.ipc.maxsockbuf"), lo_mtu, ifname[0] ? ifname : "(none)", own ? "present" : "absent");

    struct route routes[5];
    memset(routes, 0, sizeof routes);

    routes[0].name = "IPv4 127.0.0.1";
    routes[0].listen_family = routes[0].connect_family = AF_INET;
    v4(&routes[0].listen_at, INADDR_LOOPBACK);
    v4(&routes[0].connect_to, INADDR_LOOPBACK);
    routes[0].usable = 1;
    routes[0].sendpipe = 3 * 16384;

    routes[1].name = "IPv4 to 0.0.0.0";
    routes[1].listen_family = routes[1].connect_family = AF_INET;
    v4(&routes[1].listen_at, INADDR_ANY);
    v4(&routes[1].connect_to, INADDR_ANY);
    routes[1].usable = 1;
    routes[1].sendpipe = 3 * 16384;

    routes[2].name = "IPv4 own address";
    routes[2].listen_family = routes[2].connect_family = AF_INET;
    v4(&routes[2].listen_at, own);
    v4(&routes[2].connect_to, own);
    routes[2].usable = own != 0;

    routes[3].name = "IPv6 ::1";
    routes[3].listen_family = routes[3].connect_family = AF_INET6;
    v6(&routes[3].listen_at, "::1");
    v6(&routes[3].connect_to, "::1");
    routes[3].usable = 1;

    routes[4].name = "IPv6 to ::ffff:127.0.0.1";
    routes[4].listen_family = AF_INET;
    routes[4].connect_family = AF_INET6;
    v4(&routes[4].listen_at, INADDR_LOOPBACK);
    v6(&routes[4].connect_to, "::ffff:127.0.0.1");
    routes[4].usable = 1;
    routes[4].sendpipe = 3 * 16384;

    section_e(routes, 5, sendspace);
    section_b(&routes[0]);
    if (routes[2].usable) section_b(&routes[2]);
    section_b(&routes[3]);
    section_r();
    return 0;
}
