// Whether a datagram connect may give a socket the source and peer another
// datagram socket already holds. Two UDP sockets set SO_REUSEADDR and bind
// 127.0.0.1:P; the first connects to 127.0.0.1:Q, then the second does too.
// Prints the second connect's errno and the local address and peer it is left
// with. Then, as a check that bind itself admits the pair, the second bind's
// errno.
//
// Measured 2026-10-04 on Darwin 27.0.0 arm64 (uid 501) and Linux 6.18.5 aarch64
// (Apple `container`, debian:trixie, root), twice each, agreeing; outputs
// beside this file. Linux admits both binds and both connects: two datagram
// sockets may share a four-tuple. Darwin refuses the second bind with
// EADDRINUSE (SO_REUSEADDR alone does not share a port there), so this route
// to a shared four-tuple does not exist on it.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/sdd sockaddr-dgram-duplicate.c && /tmp/sdd
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -o /tmp/sdd /probe/sockaddr-dgram-duplicate.c && /tmp/sdd'
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>

static struct sockaddr_in at(uint16_t port)
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

static int reusable(void)
{
    int s = socket(AF_INET, SOCK_DGRAM, 0);
    int one = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    return s;
}

int main(void)
{
    alarm(30);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s\n", u.sysname, u.release, u.machine);

    int peer = socket(AF_INET, SOCK_DGRAM, 0);
    struct sockaddr_in p = at(0);
    bind(peer, (struct sockaddr *)&p, sizeof p);
    socklen_t len = sizeof p;
    getsockname(peer, (struct sockaddr *)&p, &len);

    int a = reusable();
    struct sockaddr_in local = at(0);
    bind(a, (struct sockaddr *)&local, sizeof local);
    len = sizeof local;
    getsockname(a, (struct sockaddr *)&local, &len);

    int b = reusable();
    int rb = bind(b, (struct sockaddr *)&local, sizeof local);
    printf("second bind: %s\n", rb == 0 ? "OK" : strerror(errno));

    int ra = connect(a, (struct sockaddr *)&p, sizeof p);
    printf("first connect: %s\n", ra == 0 ? "OK" : strerror(errno));
    int r = connect(b, (struct sockaddr *)&p, sizeof p);
    int e = r == 0 ? 0 : errno;
    struct sockaddr_in after, peerAfter;
    len = sizeof after;
    getsockname(b, (struct sockaddr *)&after, &len);
    socklen_t plen = sizeof peerAfter;
    int hasPeer = getpeername(b, (struct sockaddr *)&peerAfter, &plen) == 0;
    printf("second connect: %s local=%s:%s peer=%s\n", e ? strerror(e) : "OK", inet_ntoa(after.sin_addr),
           after.sin_port == local.sin_port ? "same" : "other", hasPeer ? "yes" : "none");
    return 0;
}
