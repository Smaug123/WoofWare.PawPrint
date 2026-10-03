// connect(2)'s answers for a sockaddr whose family or length is not a plain
// AF_INET sockaddr_in, in every socket state the kernel library models, and
// what the call leaves behind. Follows sockaddr-decoding.c, whose F section
// swept the family against a handful of lengths on fresh sockets only.
//
// Every socket is IPv4 on 127.0.0.1. The blob is a 512-byte buffer, zero but
// for the fields a row sets (on Darwin byte 0 is sa_len, set to 16; it is
// ignored, see sockaddr-decoding.c L).
//
// States, each made fresh for every call:
//   stream: fresh (unbound), bound (127.0.0.1, ephemeral port), connected (to
//     a listener), listening.
//   dgram: fresh, bound (127.0.0.1, ephemeral port), wildcard-bound (0.0.0.0,
//     a port chosen by bind), connected (to 127.0.0.1:P, implicitly bound).
// After each call: the errno; the local address getsockname reads, as an
// address and a port class (0, "same" as before the call, or "new"); and
// whether getpeername still finds a peer.
//
// Sections:
//   G  the family against every length 0..300, for families 0, 1, 2, 3, 30
//      and 255 (and, on Linux, 0x0102), on a fresh stream socket, to a
//      closed loopback port. Consecutive lengths with one answer print as a
//      range.
//   U  AF_UNSPEC against every length 0..300, in every state of both kinds,
//      with the address 127.0.0.1 and a port: a listener's for stream, a
//      bound datagram socket's for dgram. Then the same with an all-zero
//      blob (address and port 0).
//   Z  AF_INET and AF_UNSPEC at length 16, to 0.0.0.0:0, 127.0.0.1:0,
//      0.0.0.0:<listener> and 127.0.0.1:<listener>, in every state of both
//      kinds.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/scl sockaddr-connect-ladder.c && /tmp/scl
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/scl /probe/sockaddr-connect-ladder.c && /tmp/scl'
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 (uid 501) and on Linux 6.18.5
// aarch64 (Apple `container`, debian:trixie, root), twice each; the runs of a
// platform agreed line for line. One of each is checked in beside this file
// as sockaddr-connect-ladder.darwin-27.0-uid501.txt and
// sockaddr-connect-ladder.linux-6.18.5-aarch64-root.txt. In short:
//   Darwin, stream: a family other than AF_UNSPEC and AF_INET is EAFNOSUPPORT
//     at every length 2..255, and binds nothing. AF_UNSPEC is AF_INET in every
//     state: an idle socket with no address is first bound to the wildcard and
//     an ephemeral port, and then a length other than 16 is EINVAL and a port
//     of 0 (to 0.0.0.0 or 127.0.0.1) EADDRNOTAVAIL, the binding staying; a
//     connected socket answers EISCONN, a listening one EOPNOTSUPP.
//   Darwin, dgram: every length but 16 is EINVAL, whatever the family; at 16,
//     AF_UNSPEC is EAFNOSUPPORT. A connected socket is disconnected by every
//     AF_UNSPEC connect at a length the copy takes (2..255), whatever the
//     answer, and by an AF_INET one to port 0: its peer goes and its address
//     reverts to the wildcard, port kept. AF_INET to port 0 is EADDRNOTAVAIL,
//     binding nothing.
//   Linux, stream: AF_UNSPEC at every length 2..128 is accepted in every
//     state, an idle socket keeping its address; a connected one is
//     disconnected (peer gone, address back to the wildcard, port kept), a
//     listening one keeps its address. AF_INET to port 0 is ECONNREFUSED.
//   Linux, dgram: AF_UNSPEC at every length 2..128 dissolves the socket
//     exactly as at 16. AF_INET to port 0 succeeds and leaves no peer
//     getpeername can read, binding an unbound socket.
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
static uint16_t closed_port;

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

// The call's answer and what it left, as one string.
static const char *attempt(int type, enum state state, socklen_t length)
{
    static char out[160];
    int s = make(type, state);
    if (s < 0)
        return NULL;
    struct sockaddr_in before;
    socklen_t blen = sizeof before;
    getsockname(s, (struct sockaddr *)&before, &blen);
    int r = connect(s, (struct sockaddr *)blob, length);
    int e = r == 0 ? 0 : errno;

    struct sockaddr_in after;
    socklen_t alen = sizeof after;
    getsockname(s, (struct sockaddr *)&after, &alen);
    char addr[32];
    inet_ntop(AF_INET, &after.sin_addr, addr, sizeof addr);
    const char *port_class = after.sin_port == 0 ? "0" : after.sin_port == before.sin_port ? "same" : "new";

    struct sockaddr_in peer;
    socklen_t plen = sizeof peer;
    int has_peer = getpeername(s, (struct sockaddr *)&peer, &plen) == 0;

    snprintf(out, sizeof out, "%s local=%s:%s peer=%s", en(e), addr, port_class, has_peer ? "yes" : "no");
    close(s);
    drain();
    return out;
}

static void sweep(char section, const char *what, int type, enum state state, unsigned family, uint32_t address,
                  uint16_t port)
{
    char prev[160] = "";
    unsigned start = 0;
    for (unsigned length = 0; length <= 301; length++) {
        const char *now = "";
        if (length <= 300) {
            fill(family, address, port);
            now = attempt(type, state, length);
            if (now == NULL)
                return;
        }
        if (length == 301 || strcmp(now, prev) != 0) {
            if (prev[0])
                printf("%c %-30s %-9s len %u..%u: %s\n", section, what, state_name[state], start, length - 1, prev);
            start = length;
            snprintf(prev, sizeof prev, "%s", now);
        }
    }
}

int main(void)
{
    alarm(900);
    signal(SIGPIPE, SIG_IGN);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);

    listener_fd = make(SOCK_STREAM, LISTENING);
    listener_port = port_of(listener_fd);
    dgram_peer_fd = make(SOCK_DGRAM, BOUND);
    dgram_peer_port = port_of(dgram_peer_fd);
    {
        int c = make(SOCK_STREAM, BOUND);
        closed_port = port_of(c);
        close(c);
    }

    // G
    unsigned families[] = { 0, 1, 2, 3, 30, 255,
#ifndef __APPLE__
                            0x0102,
#endif
    };
    for (unsigned i = 0; i < sizeof families / sizeof families[0]; i++) {
        char what[40];
        snprintf(what, sizeof what, "stream family=%u", families[i]);
        sweep('G', what, SOCK_STREAM, FRESH, families[i], INADDR_LOOPBACK, closed_port);
    }

    // U
    for (enum state st = FRESH; st <= LISTENING; st++) {
        sweep('U', "stream AF_UNSPEC 127.0.0.1:lst", SOCK_STREAM, st, AF_UNSPEC, INADDR_LOOPBACK, listener_port);
        sweep('U', "stream AF_UNSPEC zero", SOCK_STREAM, st, AF_UNSPEC, 0, 0);
        sweep('U', "dgram AF_UNSPEC 127.0.0.1:peer", SOCK_DGRAM, st, AF_UNSPEC, INADDR_LOOPBACK, dgram_peer_port);
        sweep('U', "dgram AF_UNSPEC zero", SOCK_DGRAM, st, AF_UNSPEC, 0, 0);
    }

    // Z
    struct {
        const char *name;
        uint32_t address;
        int port_is_listener;
    } dests[] = {
        { "0.0.0.0:0", INADDR_ANY, 0 },
        { "127.0.0.1:0", INADDR_LOOPBACK, 0 },
        { "0.0.0.0:lst", INADDR_ANY, 1 },
        { "127.0.0.1:lst", INADDR_LOOPBACK, 1 },
    };
    unsigned zfamilies[] = { AF_INET, AF_UNSPEC };
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned f = 0; f < 2; f++)
            for (unsigned d = 0; d < 4; d++)
                for (enum state st = FRESH; st <= LISTENING; st++) {
                    uint16_t port = dests[d].port_is_listener ? (type == SOCK_STREAM ? listener_port : dgram_peer_port) : 0;
                    fill(zfamilies[f], dests[d].address, port);
                    const char *r = attempt(type, st, 16);
                    if (r == NULL)
                        continue;
                    printf("Z %-6s %-9s %-13s %-9s: %s\n", type == SOCK_STREAM ? "stream" : "dgram",
                           zfamilies[f] == AF_INET ? "AF_INET" : "AF_UNSPEC", dests[d].name, state_name[st], r);
                }

    return 0;
}
