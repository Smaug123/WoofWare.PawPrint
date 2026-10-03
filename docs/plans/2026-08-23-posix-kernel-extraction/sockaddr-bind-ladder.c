// bind(2)'s answers for a sockaddr of every family and length, in every
// socket state the kernel library models, and the local address each call
// leaves. The bind counterpart of sockaddr-connect-ladder.c.
//
// Every socket is IPv4. The blob is a 512-byte buffer, zero but for the
// fields a row sets (on Darwin byte 0 is sa_len, set to 16; it is ignored, see
// sockaddr-decoding.c L).
//
// States, each made fresh for every call:
//   stream: fresh (unbound), bound (127.0.0.1, ephemeral port), wildbound
//     (0.0.0.0, ephemeral port), connected (to a listener), listening.
//   dgram: fresh, bound, wildbound, connected (to 127.0.0.1:P, which binds it
//     implicitly).
// After each call: the errno, and the local address getsockname reads, as an
// address and a port class: 0, "same" as before the call, "asked" for the
// nonzero port the blob asked for, or "new".
//
// The port a row asks for is 0, or P: a port nothing holds, found by binding
// a socket and closing it.
//
// Sections:
//   G  the family against every length 0..300, on a fresh socket of each
//      kind, asking for 127.0.0.1:P. Families 0, 1, 2, 3, AF_INET6 (10 on
//      Linux, 30 on Darwin), 255, and on Linux 0x0102. Consecutive lengths
//      with one answer print as a range.
//   S  the same sweep in every other state, for families 0, 2 and AF_INET6.
//   M  a multicast (224.0.0.1) and the broadcast (255.255.255.255) address,
//      as AF_INET and as AF_UNSPEC, at every length 0..300, on a fresh and a
//      bound socket of each kind, at port P.
//   Z  families AF_INET, AF_UNSPEC and AF_INET6 at length 16, in every state
//      of both kinds, asking for each of 0.0.0.0, 127.0.0.1, 8.8.8.8,
//      224.0.0.1 and 255.255.255.255 at ports 0 and P.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/sbl sockaddr-bind-ladder.c && /tmp/sbl
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/sbl /probe/sockaddr-bind-ladder.c && /tmp/sbl'
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 (uid 501) and on Linux 6.18.5
// aarch64 (Apple `container`, debian:trixie, root), twice each; the runs of a
// platform agreed line for line but for P. One of each is checked in beside
// this file as sockaddr-bind-ladder.darwin-27.0-uid501.txt and
// sockaddr-bind-ladder.linux-6.18.5-aarch64-root.txt. In short:
//   Linux judges every length before the family: EINVAL outside 16..128, then
//     EAFNOSUPPORT for every family but AF_INET (AF_UNSPEC only with an
//     all-zero address), in every state, before the socket's binding. A
//     multicast or the broadcast address binds, on either kind of socket; on a
//     bound socket it is EINVAL like any other address.
//   Darwin judges the family first: EAFNOSUPPORT at every length 2..255 for
//     every family but AF_UNSPEC and AF_INET, and on a datagram socket
//     AF_INET6's number, 30, which it reads as AF_INET. Then EINVAL for a
//     length other than 16, or for a socket already bound. A stream socket
//     answers EAFNOSUPPORT for a multicast or the broadcast address, before
//     it asks whether it is bound, and for AF_INET before the length too,
//     reading sin_addr with every byte past the copy as zero (a multicast
//     first byte counts from a length of 5, the broadcast address from 8); for
//     AF_UNSPEC only at 16. A datagram socket binds a multicast address and
//     answers EADDRNOTAVAIL for the broadcast address.
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
static const char *attempt(int type, enum state state, socklen_t length, uint16_t asked)
{
    static char out[160];
    int s = make(type, state);
    if (s < 0)
        return NULL;
    struct sockaddr_in before;
    socklen_t blen = sizeof before;
    getsockname(s, (struct sockaddr *)&before, &blen);
    int r = bind(s, (struct sockaddr *)blob, length);
    int e = r == 0 ? 0 : errno;

    struct sockaddr_in after;
    socklen_t alen = sizeof after;
    getsockname(s, (struct sockaddr *)&after, &alen);
    char addr[32];
    inet_ntop(AF_INET, &after.sin_addr, addr, sizeof addr);
    const char *port_class = after.sin_port == 0 ? "0"
                             : after.sin_port == before.sin_port ? "same"
                             : (asked != 0 && ntohs(after.sin_port) == asked) ? "asked"
                                                                               : "new";

    snprintf(out, sizeof out, "%s local=%s:%s", en(e), addr, port_class);
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
            now = attempt(type, state, length, port);
            if (now == NULL)
                return;
        }
        if (length == 301 || strcmp(now, prev) != 0) {
            if (prev[0])
                printf("%c %-32s %-9s len %u..%u: %s\n", section, what, state_name[state], start, length - 1, prev);
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
    printf("# P is %u\n", closed_port);

    // G
    unsigned families[] = { 0, 1, 2, 3, AF_INET6, 255,
#ifndef __APPLE__
                            0x0102,
#endif
    };
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned i = 0; i < sizeof families / sizeof families[0]; i++) {
            char what[40];
            snprintf(what, sizeof what, "%s family=%u", type == SOCK_STREAM ? "stream" : "dgram", families[i]);
            sweep('G', what, type, FRESH, families[i], INADDR_LOOPBACK, closed_port);
        }

    // S
    unsigned sfamilies[] = { 0, 2, AF_INET6 };
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (enum state st = BOUND; st <= LISTENING; st++)
            for (unsigned i = 0; i < 3; i++) {
                char what[40];
                snprintf(what, sizeof what, "%s family=%u", type == SOCK_STREAM ? "stream" : "dgram", sfamilies[i]);
                sweep('S', what, type, st, sfamilies[i], INADDR_LOOPBACK, closed_port);
            }

    // M
    struct {
        const char *name;
        uint32_t address;
    } special[] = {
        { "224.0.0.1", 0xe0000001 },
        { "255.255.255.255", 0xffffffff },
    };
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned a = 0; a < 2; a++)
            for (unsigned f = 0; f < 2; f++)
                for (enum state st = FRESH; st <= BOUND; st++) {
                    char what[48];
                    snprintf(what, sizeof what, "%s %s %s", type == SOCK_STREAM ? "stream" : "dgram",
                             f ? "AF_UNSPEC" : "AF_INET", special[a].name);
                    sweep('M', what, type, st, f ? AF_UNSPEC : AF_INET, special[a].address, closed_port);
                }

    // Z
    struct {
        const char *name;
        uint32_t address;
    } addresses[] = {
        { "0.0.0.0", INADDR_ANY },
        { "127.0.0.1", INADDR_LOOPBACK },
        { "8.8.8.8", 0x08080808 },
        { "224.0.0.1", 0xe0000001 },
        { "255.255.255.255", 0xffffffff },
    };
    unsigned zfamilies[] = { AF_INET, AF_UNSPEC, AF_INET6 };
    const char *zfamily_names[] = { "AF_INET", "AF_UNSPEC", "AF_INET6" };
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned f = 0; f < 3; f++)
            for (unsigned a = 0; a < sizeof addresses / sizeof addresses[0]; a++)
                for (int p = 0; p < 2; p++)
                    for (enum state st = FRESH; st <= LISTENING; st++) {
                        uint16_t port = p ? closed_port : 0;
                        fill(zfamilies[f], addresses[a].address, port);
                        const char *r = attempt(type, st, 16, port);
                        if (r == NULL)
                            continue;
                        printf("Z %-6s %-9s %-15s %-4s %-9s: %s\n", type == SOCK_STREAM ? "stream" : "dgram",
                               zfamily_names[f], addresses[a].name, p ? "P" : "0", state_name[st], r);
                    }

    return 0;
}
