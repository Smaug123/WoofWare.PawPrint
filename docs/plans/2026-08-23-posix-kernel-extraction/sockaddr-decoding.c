// What each kernel reads out of the bytes of a `struct sockaddr` that bind(2)
// and connect(2) copy in, and what getsockname(2), getpeername(2) and accept(2)
// write back when the caller's buffer is shorter than the address. Written for
// the decision to have the kernel library decode a sockaddr's bytes itself
// rather than take a family and an endpoint its client read out of them.
//
// Every socket is IPv4 on 127.0.0.1. The copied-in blob is a 128-byte buffer,
// zero except for the fields each section sets; on Darwin byte 0 is `sa_len`
// and the family the one byte at 1, on Linux the family is the two-byte
// host-order (little-endian) word at 0.
//
// Sections:
//   F  the family sweep. For each of bind (stream), bind (datagram), connect
//      (stream, to a closed loopback port) and connect (datagram, to a
//      loopback port), at the declared lengths 0, 1, 2, 8, 15, 16, 17, 28 and
//      128, every family value the field can hold on Darwin (0..255, sa_len
//      16) and on Linux 0..65535 at length 16, 0..300 plus a few words with a
//      high byte at the other lengths. A fresh socket per call. Consecutive
//      families with the same answer print as one range. A stream connect that
//      parsed an AF_INET address reaches the closed port and answers
//      ECONNREFUSED, which is what distinguishes "parsed" from "rejected".
//   L  Darwin only: the `sa_len` byte, every value 0..255, at lengths 15, 16
//      and 17, through the same four calls.
//   Z  `sin_zero`: bind 127.0.0.1:0, bind 0.0.0.0:0, connect (stream) to a
//      listener, connect (datagram), with `sin_zero` all zero, all 0xFF, and
//      one byte at a time set to 1. Reports the errno and, on success, the
//      address getsockname then reads.
//   D  bind with AF_INET6's family number on an AF_INET socket, at length 16,
//      to 127.0.0.1, 0.0.0.0 and 8.8.8.8: the errno, and where it bound.
//   X  bytes after the 16 of a sockaddr_in, at lengths 17..128, all 0xFF:
//      whether they change anything.
//   T  the outbound copy: getsockname, getpeername and accept with declared
//      lengths 0..20 into a 0xAA-filled buffer. Prints the bytes written, the
//      cell afterwards, and whether the written bytes are a prefix of the full
//      address.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/sd sockaddr-decoding.c && /tmp/sd
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/sd /probe/sockaddr-decoding.c && /tmp/sd'
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 (uid 501), three runs of F, L, Z
// and X and two of D and T, and on Linux 6.18.5 aarch64 (Apple `container`,
// debian:trixie, root), three runs of every section. Every run of a platform
// agreed line for line, bar the ports and the T section's skipped rows. One
// run of each is checked in beside this file as
// sockaddr-decoding.darwin-27.0-uid501.txt and
// sockaddr-decoding.linux-6.18.5-aarch64-root.txt. In short, at the lengths
// swept (0, 1, 2, 8, 15, 16, 17, 28, 128):
//   Linux reads the family as the two-byte word (0x0202 is not AF_INET) and
//     judges every length below 16 before the family: bind EINVAL at 0..15
//     whatever the family, connect EINVAL at 0..1 and, for every family but
//     AF_UNSPEC, at 2..15. AF_UNSPEC connects (a no-op on an idle socket) at
//     every length from 2, stream and datagram. From 16 on, AF_INET binds or
//     connects, an AF_UNSPEC bind of 127.0.0.1 is EAFNOSUPPORT (an all-zero
//     one binds, measured earlier and not here), and every other family,
//     AF_INET6's 10 included, is EAFNOSUPPORT. Bytes past the 16 of a sockaddr_in are ignored up to 128.
//   Darwin judges nothing below 2 (EINVAL whatever the family). From 2, bind
//     and stream connect check the family before the length: a family that
//     is neither AF_UNSPEC nor AF_INET is EAFNOSUPPORT at every length from 2,
//     and AF_UNSPEC and AF_INET are EINVAL at every length but 16. Both
//     read an AF_UNSPEC sockaddr as AF_INET: a stream connect to 127.0.0.1 on
//     a closed port is ECONNREFUSED. A datagram bind also takes AF_INET6's
//     number, 30, reading the blob as a sockaddr_in (D: it binds 127.0.0.1 or
//     0.0.0.0, and 8.8.8.8 is EADDRNOTAVAIL); a stream bind does not. A
//     datagram connect checks the length first: EINVAL at every length but
//     16 whatever the family, then EAFNOSUPPORT for every family but AF_INET,
//     AF_UNSPEC included. `sa_len` is ignored: all 256 values answer as the
//     length argument says, at 15, 16 and 17.
//   Both ignore `sin_zero`: every pattern binds and connects as all-zero does.
//   Both copy out the first min(declared, 16) bytes of the full address, and
//     store 16 in the length cell at every declared length 0..20, for
//     getsockname, getpeername and accept alike.
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
    case ENOTSOCK: return "ENOTSOCK";
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
    case EPROTOTYPE: return "EPROTOTYPE";
    default: {
        static char buf[32];
        snprintf(buf, sizeof buf, "errno%d", e);
        return buf;
    }
    }
}

static unsigned char blob[128];

#ifdef __APPLE__
#define FAMILY_MAX 255
#else
#define FAMILY_MAX 65535
#endif

static void set_family(unsigned family, unsigned sa_len)
{
#ifdef __APPLE__
    blob[0] = (unsigned char)sa_len;
    blob[1] = (unsigned char)family;
#else
    (void)sa_len;
    blob[0] = (unsigned char)(family & 0xff);
    blob[1] = (unsigned char)(family >> 8);
#endif
}

static void fill(unsigned family, unsigned sa_len, uint32_t address, uint16_t port)
{
    memset(blob, 0, sizeof blob);
    set_family(family, sa_len);
    uint16_t p = htons(port);
    uint32_t a = htonl(address);
    memcpy(blob + 2, &p, 2);
    memcpy(blob + 4, &a, 4);
}

static struct sockaddr_in loopback(uint16_t port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
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

static int bound(int type)
{
    int s = socket(AF_INET, type, 0);
    struct sockaddr_in a = loopback(0);
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) {
        perror("bound");
        exit(1);
    }
    return s;
}

// A loopback port nothing is bound to: bind one, read it, close it.
static uint16_t closed_port(void)
{
    int s = bound(SOCK_STREAM);
    uint16_t p = port_of(s);
    close(s);
    return p;
}

enum call { BIND_STREAM, BIND_DGRAM, CONNECT_STREAM, CONNECT_DGRAM, CALLS };
static const char *call_name[] = { "bind/stream", "bind/dgram", "connect/stream", "connect/dgram" };

static uint16_t refused_port;
static uint16_t dgram_port;

// One call on a fresh socket with the current blob. Answers the errno, 0 for
// success.
static int one(enum call c, socklen_t length)
{
    int type = (c == BIND_STREAM || c == CONNECT_STREAM) ? SOCK_STREAM : SOCK_DGRAM;
    int s = socket(AF_INET, type, 0);
    if (s < 0) {
        perror("socket");
        exit(1);
    }
    int r = (c == BIND_STREAM || c == BIND_DGRAM) ? bind(s, (struct sockaddr *)blob, length)
                                                   : connect(s, (struct sockaddr *)blob, length);
    int e = r == 0 ? 0 : errno;
    close(s);
    return e;
}

static uint16_t port_for(enum call c)
{
    switch (c) {
    case CONNECT_STREAM: return refused_port;
    case CONNECT_DGRAM: return dgram_port;
    default: return 0;
    }
}

static void sweep_families(enum call c, socklen_t length, const unsigned *families, int count)
{
    int prev = -2;
    unsigned start = 0, last = 0;
    for (int i = 0; i <= count; i++) {
        int e = -1;
        if (i < count) {
            fill(families[i], 16, INADDR_LOOPBACK, port_for(c));
            e = one(c, length);
        }
        if (i == count || e != prev || families[i] != last + 1) {
            if (prev != -2)
                printf("F %-14s len=%-3u family %u..%u (0x%04x..0x%04x): %s\n", call_name[c], (unsigned)length, start,
                       last, start, last, en(prev));
            if (i < count) {
                start = families[i];
                prev = e;
            }
        }
        if (i < count)
            last = families[i];
    }
}

static void section_f(void)
{
    static unsigned all[65536];
    static unsigned some[512];
    int nall = 0, nsome = 0;
    for (unsigned f = 0; f <= FAMILY_MAX; f++)
        all[nall++] = f;
#ifdef __APPLE__
    for (unsigned f = 0; f <= 255; f++)
        some[nsome++] = f;
#else
    for (unsigned f = 0; f <= 300; f++)
        some[nsome++] = f;
    unsigned extra[] = { 0x0200, 0x0202, 0x0302, 0x0a02, 0x8002, 0xff02, 0xfffe, 0xffff };
    for (unsigned i = 0; i < sizeof extra / sizeof extra[0]; i++)
        some[nsome++] = extra[i];
#endif
    socklen_t lengths[] = { 0, 1, 2, 8, 15, 16, 17, 28, 128 };
    for (enum call c = 0; c < CALLS; c++)
        for (unsigned i = 0; i < sizeof lengths / sizeof lengths[0]; i++) {
            if (lengths[i] == 16)
                sweep_families(c, 16, all, nall);
            else
                sweep_families(c, lengths[i], some, nsome);
        }
}

#ifdef __APPLE__
static void section_l(void)
{
    socklen_t lengths[] = { 15, 16, 17 };
    for (enum call c = 0; c < CALLS; c++)
        for (unsigned i = 0; i < 3; i++) {
            int prev = -2;
            unsigned start = 0;
            for (unsigned sa_len = 0; sa_len <= 256; sa_len++) {
                int e = -1;
                if (sa_len < 256) {
                    fill(AF_INET, sa_len, INADDR_LOOPBACK, port_for(c));
                    e = one(c, lengths[i]);
                }
                if (sa_len == 256 || e != prev) {
                    if (prev != -2)
                        printf("L %-14s len=%-3u sa_len %u..%u: %s\n", call_name[c], (unsigned)lengths[i], start,
                               sa_len - 1, en(prev));
                    start = sa_len;
                    prev = e;
                }
            }
        }
}
#endif

// Z: what sin_zero does.
static void z_row(const char *what, const char *pattern, int e, int s)
{
    if (e == 0 && s >= 0) {
        struct sockaddr_in a;
        socklen_t len = sizeof a;
        getsockname(s, (struct sockaddr *)&a, &len);
        char text[32];
        inet_ntop(AF_INET, &a.sin_addr, text, sizeof text);
        printf("Z %-22s %-10s OK local=%s:%s\n", what, pattern, text, ntohs(a.sin_port) ? "nonzero" : "0");
    } else
        printf("Z %-22s %-10s %s\n", what, pattern, en(e));
}

static void section_z(void)
{
    int l = bound(SOCK_STREAM);
    listen(l, 128);
    uint16_t lport = port_of(l);

    for (int pattern = -2; pattern < 8; pattern++) {
        char name[16];
        if (pattern == -2)
            snprintf(name, sizeof name, "zero");
        else if (pattern == -1)
            snprintf(name, sizeof name, "all-ff");
        else
            snprintf(name, sizeof name, "byte%d=1", 8 + pattern);

        struct {
            const char *what;
            int type;
            int is_bind;
            uint32_t address;
            uint16_t port;
        } rows[] = {
            { "bind 127.0.0.1:0", SOCK_STREAM, 1, INADDR_LOOPBACK, 0 },
            { "bind 0.0.0.0:0", SOCK_STREAM, 1, INADDR_ANY, 0 },
            { "bind/dgram 127.0.0.1:0", SOCK_DGRAM, 1, INADDR_LOOPBACK, 0 },
            { "connect listener", SOCK_STREAM, 0, INADDR_LOOPBACK, lport },
            { "connect/dgram", SOCK_DGRAM, 0, INADDR_LOOPBACK, dgram_port },
        };
        for (unsigned r = 0; r < sizeof rows / sizeof rows[0]; r++) {
            fill(AF_INET, 16, rows[r].address, rows[r].port);
            if (pattern == -1)
                memset(blob + 8, 0xff, 8);
            else if (pattern >= 0)
                blob[8 + pattern] = 1;
            int s = socket(AF_INET, rows[r].type, 0);
            int rc = rows[r].is_bind ? bind(s, (struct sockaddr *)blob, 16) : connect(s, (struct sockaddr *)blob, 16);
            int e = rc == 0 ? 0 : errno;
            z_row(rows[r].what, name, e, s);
            close(s);
            if (!rows[r].is_bind && rows[r].type == SOCK_STREAM && e == 0) {
                int a = accept(l, NULL, NULL);
                if (a >= 0)
                    close(a);
            }
        }
    }
    close(l);
}

// D: a datagram bind whose family is AF_INET6's number, on an AF_INET socket,
// which the F sweep found Darwin accepts at length 16. Where does it bind?
static void section_d(void)
{
    uint32_t addresses[] = { INADDR_LOOPBACK, INADDR_ANY, 0x08080808 };
    const char *names[] = { "127.0.0.1", "0.0.0.0", "8.8.8.8" };
    for (int type = SOCK_STREAM; type <= SOCK_DGRAM; type++)
        for (unsigned i = 0; i < 3; i++) {
            fill(AF_INET6, 16, addresses[i], 0);
            int s = socket(AF_INET, type, 0);
            int rc = bind(s, (struct sockaddr *)blob, 16);
            int e = rc == 0 ? 0 : errno;
            char what[48];
            snprintf(what, sizeof what, "%s AF_INET6(%d) %s", type == SOCK_STREAM ? "stream" : "dgram", AF_INET6,
                     names[i]);
            if (e == 0) {
                struct sockaddr_in a;
                socklen_t len = sizeof a;
                getsockname(s, (struct sockaddr *)&a, &len);
                char text[32];
                inet_ntop(AF_INET, &a.sin_addr, text, sizeof text);
                printf("D %-32s OK local=%s:%s family=%d len=%u\n", what, text, ntohs(a.sin_port) ? "nonzero" : "0",
                       a.sin_family, (unsigned)len);
            } else
                printf("D %-32s %s\n", what, en(e));
            close(s);
        }
}

// X: trailing bytes past the sockaddr_in.
static void section_x(void)
{
    for (enum call c = 0; c < CALLS; c++) {
        int prev = -2;
        unsigned start = 0;
        for (unsigned length = 17; length <= 129; length++) {
            int e = -1;
            if (length <= 128) {
                fill(AF_INET, 16, INADDR_LOOPBACK, port_for(c));
                memset(blob + 16, 0xff, sizeof blob - 16);
                e = one(c, length);
            }
            if (length == 129 || e != prev) {
                if (prev != -2)
                    printf("X %-14s len %u..%u, bytes 16.. = 0xff: %s\n", call_name[c], start, length - 1, en(prev));
                start = length;
                prev = e;
            }
        }
    }
}

// T: the outbound copy.
static void t_row(const char *what, int declared, int rc, int e, const unsigned char *out, socklen_t cell,
                  const unsigned char *full)
{
    printf("T %-11s declared=%-2d %s cell=%-3u bytes=", what, declared, rc == 0 ? "OK" : en(e), (unsigned)cell);
    for (int i = 0; i < 20; i++)
        printf("%02x", out[i]);
    int written = 0;
    while (written < 20 && out[written] != 0xAA)
        written++;
    // A written byte may itself be 0xAA only by accident; the ports are chosen so
    // none is.
    int prefix = memcmp(out, full, written < 16 ? written : 16) == 0;
    printf(" written=%d prefix-of-full=%s\n", written, prefix ? "yes" : "no");
}

static void section_t(void)
{
    int l, client;
    unsigned char full_local[16], full_peer[16];
    for (;;) {
        l = bound(SOCK_STREAM);
        listen(l, 128);
        client = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in a = loopback(port_of(l));
        connect(client, (struct sockaddr *)&a, sizeof a);
        uint16_t lp = htons(port_of(l)), cp = htons(port_of(client));
        unsigned char *b1 = (unsigned char *)&lp, *b2 = (unsigned char *)&cp;
        if (b1[0] != 0xAA && b1[1] != 0xAA && b2[0] != 0xAA && b2[1] != 0xAA)
            break;
        close(client);
        close(l);
    }
    // Take the client's own connection off the queue, so that each accept below
    // returns the connection made for it.
    int server = accept(l, NULL, NULL);
    socklen_t len = 16;
    getsockname(client, (struct sockaddr *)full_local, &len);
    len = 16;
    getpeername(client, (struct sockaddr *)full_peer, &len);

    for (int declared = 0; declared <= 20; declared++) {
        unsigned char out[32];
        socklen_t cell;

        memset(out, 0xAA, sizeof out);
        cell = declared;
        int rc = getsockname(client, (struct sockaddr *)out, &cell);
        t_row("getsockname", declared, rc, errno, out, cell, full_local);

        memset(out, 0xAA, sizeof out);
        cell = declared;
        rc = getpeername(client, (struct sockaddr *)out, &cell);
        t_row("getpeername", declared, rc, errno, out, cell, full_peer);
    }

    // accept: the peer of the accepted socket is the client's local address.
    for (int declared = 0; declared <= 20; declared++) {
        int c = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in a = loopback(port_of(l));
        connect(c, (struct sockaddr *)&a, sizeof a);
        unsigned char full[16];
        socklen_t flen = 16;
        getsockname(c, (struct sockaddr *)full, &flen);
        unsigned char *pb = full + 2;
        if (pb[0] == 0xAA || pb[1] == 0xAA) {
            // Skip a port with a sentinel byte; count the row as skipped.
            printf("T accept      declared=%-2d skipped (port byte 0xAA)\n", declared);
            int x = accept(l, NULL, NULL);
            close(x);
            close(c);
            continue;
        }
        unsigned char out[32];
        memset(out, 0xAA, sizeof out);
        socklen_t cell = declared;
        int x = accept(l, (struct sockaddr *)out, &cell);
        t_row("accept", declared, x >= 0 ? 0 : -1, errno, out, cell, full);
        if (x >= 0)
            close(x);
        close(c);
    }
    close(server);
    close(client);
    close(l);
}

int main(void)
{
    alarm(900);
    signal(SIGPIPE, SIG_IGN);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);

    refused_port = closed_port();
    int d = bound(SOCK_DGRAM);
    dgram_port = port_of(d);
    printf("# refused port %u, datagram peer port %u\n", refused_port, dgram_port);

    section_f();
#ifdef __APPLE__
    section_l();
#endif
    section_z();
    section_d();
    section_x();
    section_t();
    close(d);
    return 0;
}
