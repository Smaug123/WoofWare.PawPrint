// The local address a datagram socket bound to an address other than
// 127.0.0.1 holds after it connects to loopback, connected before or not.
// Darwin disconnects a connected datagram socket before a connect, reverting
// its local address to the wildcard (sockaddr-dgram-connect.c, L); this asks
// whether a later successful connect then resolves the source afresh.
//
// The address is the machine's first IPv4 interface address other than
// loopback, so the rows print it as "iface". For each of: bound to it and not
// connected; bound to it and connected to a datagram socket on it; the call
// connects to 127.0.0.1:Q and to 0.0.0.0:Q, Q a datagram socket on loopback.
// Each row prints the errno, the local address afterwards ("iface",
// "127.0.0.1" or "0.0.0.0") with whether its port is the one bound, and the
// address getpeername reads ("none" if it reads none).
//
// Then the same for a stream socket bound to the interface address, connecting
// to 127.0.0.1:L and to 0.0.0.0:L, L a port a listener holds on the wildcard,
// so that a connection to either address is accepted and the peer shows which
// address the kernel aimed at.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/sdr sockaddr-dgram-reconnect.c && /tmp/sdr
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/sdr /probe/sockaddr-dgram-reconnect.c && /tmp/sdr'
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 (uid 501) and on Linux 6.18.5
// aarch64 (Apple `container`, debian:trixie, root), twice each, the runs of a
// platform agreeing; outputs beside this file. A socket that was not connected
// keeps the interface address on both. A connected one keeps it on Linux, and
// on Darwin connects from 127.0.0.1, port kept: the disconnect reverts its
// address to the wildcard, and the connect resolves the source afresh. A
// connect to 0.0.0.0 reaches the socket's own interface address on Linux, for
// stream and datagram sockets alike, and 127.0.0.1 on Darwin.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>

static struct sockaddr_in at(uint32_t address_net, uint16_t port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
#ifdef __APPLE__
    a.sin_len = sizeof a;
#endif
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = address_net;
    return a;
}

static uint16_t bound_port(int s)
{
    struct sockaddr_in a;
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    return ntohs(a.sin_port);
}

static const char *name_of(uint32_t address_net, uint32_t iface)
{
    return address_net == iface                       ? "iface"
           : address_net == htonl(INADDR_LOOPBACK) ? "127.0.0.1"
           : address_net == 0                      ? "0.0.0.0"
                                                   : "other";
}

static const char *peer_of(int s, uint32_t iface)
{
    struct sockaddr_in p;
    socklen_t len = sizeof p;
    if (getpeername(s, (struct sockaddr *)&p, &len) != 0)
        return "none";
    return name_of(p.sin_addr.s_addr, iface);
}

static int on(int type, uint32_t address_net)
{
    int s = socket(AF_INET, type, 0);
    struct sockaddr_in a = at(address_net, 0);
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) {
        perror("bind");
        exit(1);
    }
    return s;
}

static int udp_on(uint32_t address_net) { return on(SOCK_DGRAM, address_net); }

int main(void)
{
    alarm(60);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);

    struct ifaddrs *list;
    if (getifaddrs(&list) != 0) {
        perror("getifaddrs");
        return 1;
    }
    uint32_t iface = 0;
    for (struct ifaddrs *i = list; i; i = i->ifa_next)
        if (i->ifa_addr && i->ifa_addr->sa_family == AF_INET) {
            uint32_t a = ((struct sockaddr_in *)i->ifa_addr)->sin_addr.s_addr;
            if ((ntohl(a) >> 24) != 127) {
                iface = a;
                break;
            }
        }
    freeifaddrs(list);
    if (iface == 0) {
        printf("# no non-loopback IPv4 address\n");
        return 1;
    }

    int ifacePeer = udp_on(iface);
    int loopPeer = udp_on(htonl(INADDR_LOOPBACK));
    uint16_t P = bound_port(ifacePeer), Q = bound_port(loopPeer);

    struct {
        const char *name;
        uint32_t address;
    } dests[] = { { "127.0.0.1", htonl(INADDR_LOOPBACK) }, { "0.0.0.0", htonl(INADDR_ANY) } };

    for (int connected = 0; connected < 2; connected++)
        for (int d = 0; d < 2; d++) {
            int s = udp_on(iface);
            uint16_t port = bound_port(s);
            if (connected) {
                struct sockaddr_in p = at(iface, P);
                if (connect(s, (struct sockaddr *)&p, sizeof p) != 0) {
                    perror("connect");
                    return 1;
                }
            }
            struct sockaddr_in to = at(dests[d].address, Q);
            int r = connect(s, (struct sockaddr *)&to, sizeof to);
            int e = r == 0 ? 0 : errno;
            struct sockaddr_in after;
            socklen_t len = sizeof after;
            getsockname(s, (struct sockaddr *)&after, &len);
            printf("R %-13s to %-9s: %s local=%s port=%s peer=%s\n", connected ? "connected" : "not-connected",
                   dests[d].name, e ? strerror(e) : "OK", name_of(after.sin_addr.s_addr, iface),
                   ntohs(after.sin_port) == port ? "same" : "other", peer_of(s, iface));
            close(s);
        }

    int listener = on(SOCK_STREAM, htonl(INADDR_ANY));
    listen(listener, 8);
    uint16_t L = bound_port(listener);
    for (int d = 0; d < 2; d++) {
        int s = on(SOCK_STREAM, iface);
        uint16_t port = bound_port(s);
        struct sockaddr_in to = at(dests[d].address, L);
        int r = connect(s, (struct sockaddr *)&to, sizeof to);
        int e = r == 0 ? 0 : errno;
        struct sockaddr_in after;
        socklen_t len = sizeof after;
        getsockname(s, (struct sockaddr *)&after, &len);
        printf("S %-13s to %-9s: %s local=%s port=%s peer=%s\n", "not-connected", dests[d].name,
               e ? strerror(e) : "OK", name_of(after.sin_addr.s_addr, iface),
               ntohs(after.sin_port) == port ? "same" : "other", peer_of(s, iface));
        close(s);
        int a = accept(listener, NULL, NULL);
        if (a >= 0)
            close(a);
    }
    return 0;
}
