// connect(2) to an AF_INET destination whose address is the wildcard, not
// this machine's, a multicast group or the broadcast address, or whose port
// is 0, on both kinds of socket in every state the kernel library models; and
// a connected datagram socket's connect at every length. Follows
// sockaddr-connect-ladder.c, which swept the family and the length.
//
// States, each made fresh for every call: fresh, bound (127.0.0.1, ephemeral
// port), wildbound (0.0.0.0, ephemeral port), connected (stream: to a
// listener; dgram: to 127.0.0.1:P, which binds it implicitly), listening
// (stream). After each call: the errno; the local address getsockname reads,
// with its port class (0, "same" as before, or "new"); and the peer
// getpeername reads, as an address and a port class ("P" for the port asked
// for, "old" for the peer the socket had before, the number otherwise), or
// "none".
//
// A stream connect that would send a SYN somewhere nothing answers is ended
// by a 2-second timer: its answer prints as EINTR.
//
// Sections:
//   D  AF_INET at length 16 to 0.0.0.0, 127.0.0.1, 127.0.0.2, 8.8.8.8,
//      224.0.0.1 and 255.255.255.255, each at port 0 and at port P (a listener
//      for a stream socket, a bound datagram socket for a datagram one), in
//      every state of both kinds. On Linux a stream connect to 8.8.8.8 at port
//      P is not made: it would reach the network.
//   M  AF_INET and AF_UNSPEC to 224.0.0.1 and 255.255.255.255 at port P, at every length
//      0..300, on a fresh socket of each kind.
//   B  a fresh datagram socket's connect at every length 0..300, to
//      127.0.0.1:P, for families 1, AF_INET, AF_INET6 and 255.
//   L  a connected datagram socket's connect at every length 0..300, for
//      AF_INET to 127.0.0.1:P and for family 1.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/sdc sockaddr-dgram-connect.c && /tmp/sdc
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/sdc /probe/sockaddr-dgram-connect.c && /tmp/sdc'
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 (uid 501) and on Linux 6.18.5
// aarch64 (Apple `container`, debian:trixie, root), twice each; the runs of a
// platform agreed line for line but for the host's own interface address,
// which a connect to 8.8.8.8 or a group resolves as its source. One of each
// is checked in beside this file as sockaddr-dgram-connect.darwin-27.0-uid501.txt
// and sockaddr-dgram-connect.linux-6.18.5-aarch64-root.txt. In short:
//   Both: a wildcard destination is loopback, the source 127.0.0.1.
//   Darwin, dgram: a port of 0 is EADDRNOTAVAIL whatever the address, binding
//     nothing. A connected socket is disconnected first by every connect whose
//     length the copy takes, whatever the answer. A group destination
//     connects.
//   Linux, dgram: a socket with no port is bound to the wildcard and an
//     ephemeral port before the length or the family is judged (every family
//     but AF_UNSPEC), keeping it on failure. A port of 0 connects to no peer
//     getpeername reads. The broadcast address is EACCES; a multicast one
//     connects.
//   Stream: a group destination is ENETUNREACH on Linux, binding nothing, and
//     EAFNOSUPPORT on Darwin, binding nothing: for AF_INET from a length that
//     reaches the address's distinguishing bytes (5 for 224.0.0.1, 8 for
//     255.255.255.255), reading the rest as zero, and for AF_UNSPEC only at
//     16.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/utsname.h>
#include <unistd.h>

static const char *en(int e)
{
    switch (e) {
    case 0: return "OK";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case EINVAL: return "EINVAL";
    case EAGAIN: return "EAGAIN";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EAFNOSUPPORT: return "EAFNOSUPPORT";
    case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
    case EADDRINUSE: return "EADDRINUSE";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ECONNREFUSED: return "ECONNREFUSED";
    case EISCONN: return "EISCONN";
    case ENOTCONN: return "ENOTCONN";
    case EACCES: return "EACCES";
    case ENETUNREACH: return "ENETUNREACH";
    case EHOSTUNREACH: return "EHOSTUNREACH";
    case EINPROGRESS: return "EINPROGRESS";
    case ETIMEDOUT: return "ETIMEDOUT";
    case EALREADY: return "EALREADY";
    default: {
        static char buf[32];
        snprintf(buf, sizeof buf, "errno%d", e);
        return buf;
    }
    }
}

static unsigned char blob[512];

static void fill(unsigned family, uint32_t address, uint16_t port)
{
    memset(blob, 0, sizeof blob);
#ifdef __APPLE__
    blob[0] = 16;
    blob[1] = (unsigned char)family;
#else
    blob[0] = (unsigned char)(family & 0xff);
    blob[1] = (unsigned char)(family >> 8);
#endif
    uint16_t p = htons(port);
    uint32_t a = htonl(address);
    memcpy(blob + 2, &p, 2);
    memcpy(blob + 4, &a, 4);
}

static struct sockaddr_in at(uint32_t address, uint16_t port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(address);
    return a;
}

static uint16_t port_of(int fd)
{
    struct sockaddr_in a;
    socklen_t len = sizeof a;
    if (getsockname(fd, (struct sockaddr *)&a, &len) != 0)
        return 0;
    return ntohs(a.sin_port);
}

static void must(int ok, const char *what)
{
    if (!ok) {
        perror(what);
        exit(1);
    }
}

static int listener_fd;
static uint16_t listener_port;
static int dgram_peer_fd;
static uint16_t dgram_peer_port;

enum state { FRESH, BOUND, WILDCARD_BOUND, CONNECTED, LISTENING };
static const char *state_name[] = { "fresh", "bound", "wildbound", "connected", "listening" };

// A socket of `type` in `state`, or -1 where the state does not apply.
static int make(int type, enum state state)
{
    int s = socket(AF_INET, type, 0);
    must(s >= 0, "socket");
    struct sockaddr_in a;
    switch (state) {
    case FRESH: break;
    case BOUND:
        a = at(INADDR_LOOPBACK, 0);
        must(bind(s, (struct sockaddr *)&a, sizeof a) == 0, "bind");
        break;
    case WILDCARD_BOUND:
        a = at(INADDR_ANY, 0);
        must(bind(s, (struct sockaddr *)&a, sizeof a) == 0, "bind wildcard");
        break;
    case CONNECTED:
        a = type == SOCK_STREAM ? at(INADDR_LOOPBACK, listener_port) : at(INADDR_LOOPBACK, dgram_peer_port);
        must(connect(s, (struct sockaddr *)&a, sizeof a) == 0, "connect");
        break;
    case LISTENING:
        if (type != SOCK_STREAM) {
            close(s);
            return -1;
        }
        a = at(INADDR_LOOPBACK, 0);
        must(bind(s, (struct sockaddr *)&a, sizeof a) == 0, "bind listening");
        must(listen(s, 8) == 0, "listen");
        break;
    }
    return s;
}

// Drain whatever the listener has queued, so it never fills.
static void drain(void)
{
    int flags = fcntl(listener_fd, F_GETFL);
    fcntl(listener_fd, F_SETFL, flags | O_NONBLOCK);
    for (;;) {
        int a = accept(listener_fd, NULL, NULL);
        if (a < 0)
            break;
        close(a);
    }
    fcntl(listener_fd, F_SETFL, flags);
}

static void on_alarm(int signo) { (void)signo; }

static void arm(int seconds)
{
    struct itimerval t;
    memset(&t, 0, sizeof t);
    t.it_value.tv_sec = seconds;
    setitimer(ITIMER_REAL, &t, NULL);
}

static void describe_peer(int s, uint16_t asked, const struct sockaddr_in *old, int had_peer, char *out, size_t n)
{
    struct sockaddr_in peer;
    socklen_t plen = sizeof peer;
    if (getpeername(s, (struct sockaddr *)&peer, &plen) != 0) {
        snprintf(out, n, "none");
        return;
    }
    char addr[32];
    inet_ntop(AF_INET, &peer.sin_addr, addr, sizeof addr);
    uint16_t port = ntohs(peer.sin_port);
    if (had_peer && peer.sin_port == old->sin_port && peer.sin_addr.s_addr == old->sin_addr.s_addr)
        snprintf(out, n, "old");
    else if (asked != 0 && port == asked)
        snprintf(out, n, "%s:P", addr);
    else
        snprintf(out, n, "%s:%u", addr, port);
}

// The call's answer and what it left, as one string.
static const char *attempt(int type, enum state state, socklen_t length, uint16_t asked)
{
    static char out[200];
    int s = make(type, state);
    if (s < 0)
        return NULL;
    struct sockaddr_in before;
    socklen_t blen = sizeof before;
    getsockname(s, (struct sockaddr *)&before, &blen);
    struct sockaddr_in old_peer;
    socklen_t olen = sizeof old_peer;
    int had_peer = getpeername(s, (struct sockaddr *)&old_peer, &olen) == 0;

    if (type == SOCK_STREAM)
        arm(2);
    int r = connect(s, (struct sockaddr *)blob, length);
    int e = r == 0 ? 0 : errno;
    arm(0);

    struct sockaddr_in after;
    socklen_t alen = sizeof after;
    getsockname(s, (struct sockaddr *)&after, &alen);
    char addr[32];
    inet_ntop(AF_INET, &after.sin_addr, addr, sizeof addr);
    const char *port_class = after.sin_port == 0 ? "0" : after.sin_port == before.sin_port ? "same" : "new";
    char peer[64];
    describe_peer(s, asked, &old_peer, had_peer, peer, sizeof peer);

    snprintf(out, sizeof out, "%s local=%s:%s peer=%s", en(e), addr, port_class, peer);
    close(s);
    drain();
    return out;
}

int main(void)
{
    signal(SIGPIPE, SIG_IGN);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_alarm;
    // No SA_RESTART: the timer ends a sleeping connect with EINTR.
    sigaction(SIGALRM, &sa, NULL);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);

    listener_fd = make(SOCK_STREAM, LISTENING);
    listener_port = port_of(listener_fd);
    dgram_peer_fd = make(SOCK_DGRAM, BOUND);
    dgram_peer_port = port_of(dgram_peer_fd);

    struct {
        const char *name;
        uint32_t address;
    } addresses[] = {
        { "0.0.0.0", INADDR_ANY },
        { "127.0.0.1", INADDR_LOOPBACK },
        { "127.0.0.2", 0x7f000002 },
        { "8.8.8.8", 0x08080808 },
        { "224.0.0.1", 0xe0000001 },
        { "255.255.255.255", 0xffffffff },
    };

    // D
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned a = 0; a < sizeof addresses / sizeof addresses[0]; a++)
            for (int p = 0; p < 2; p++) {
#ifndef __APPLE__
                if (type == SOCK_STREAM && p && addresses[a].address == 0x08080808)
                    continue;
#endif
                for (enum state st = FRESH; st <= LISTENING; st++) {
                    uint16_t port = p ? (type == SOCK_STREAM ? listener_port : dgram_peer_port) : 0;
                    fill(AF_INET, addresses[a].address, port);
                    const char *r = attempt(type, st, 16, port);
                    if (r == NULL)
                        continue;
                    printf("D %-6s %-15s %-1s %-9s: %s\n", type == SOCK_STREAM ? "stream" : "dgram",
                           addresses[a].name, p ? "P" : "0", state_name[st], r);
                }
            }

    // M
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned mf = 0; mf < 2; mf++)
        for (unsigned a = 4; a < 6; a++) {
            unsigned family = mf ? AF_UNSPEC : AF_INET;
            char prev[200] = "";
            unsigned start = 0;
            uint16_t port = type == SOCK_STREAM ? listener_port : dgram_peer_port;
            for (unsigned length = 0; length <= 301; length++) {
                const char *now = "";
                if (length <= 300) {
                    fill(family, addresses[a].address, port);
                    now = attempt(type, FRESH, length, port);
                }
                if (length == 301 || strcmp(now, prev) != 0) {
                    if (prev[0])
                        printf("M %-6s %-9s %-15s fresh len %u..%u: %s\n", type == SOCK_STREAM ? "stream" : "dgram",
                               mf ? "AF_UNSPEC" : "AF_INET", addresses[a].name, start, length - 1, prev);
                    start = length;
                    snprintf(prev, sizeof prev, "%s", now);
                }
            }
        }

    // B
    unsigned bfamilies[] = { 1, 2, AF_INET6, 255 };
    for (unsigned f = 0; f < 4; f++) {
        char prev[200] = "";
        unsigned start = 0;
        for (unsigned length = 0; length <= 301; length++) {
            const char *now = "";
            if (length <= 300) {
                fill(bfamilies[f], INADDR_LOOPBACK, dgram_peer_port);
                now = attempt(SOCK_DGRAM, FRESH, length, dgram_peer_port);
            }
            if (length == 301 || strcmp(now, prev) != 0) {
                if (prev[0])
                    printf("B dgram family=%-3u fresh len %u..%u: %s\n", bfamilies[f], start, length - 1, prev);
                start = length;
                snprintf(prev, sizeof prev, "%s", now);
            }
        }
    }

    // L
    unsigned families[] = { AF_INET, 1 };
    for (unsigned f = 0; f < 2; f++) {
        char prev[200] = "";
        unsigned start = 0;
        for (unsigned length = 0; length <= 301; length++) {
            const char *now = "";
            if (length <= 300) {
                fill(families[f], INADDR_LOOPBACK, dgram_peer_port);
                now = attempt(SOCK_DGRAM, CONNECTED, length, dgram_peer_port);
            }
            if (length == 301 || strcmp(now, prev) != 0) {
                if (prev[0])
                    printf("L dgram family=%-3u connected len %u..%u: %s\n", families[f], start, length - 1, prev);
                start = length;
                snprintf(prev, sizeof prev, "%s", now);
            }
        }
    }
    return 0;
}
