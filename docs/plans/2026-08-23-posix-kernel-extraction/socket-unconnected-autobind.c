// Whether a failed read(2) or write(2) on a socket with no peer binds it: the
// local address getsockname(2) reports before and after, for every domain and
// type, at write counts 0, 1 and 65536 and a non-blocking 1-byte read. A
// datagram socket that sends picks an ephemeral port before it looks for a
// destination on some kernels, which would make even a failing write change
// the socket.
//
// Build: `nix develop -c clang -O0 -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -o /tmp/p <this file>` on Linux.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

static char buf[65536];

static void name(int fd, char *out, size_t len) {
    struct sockaddr_storage s;
    socklen_t l = sizeof s;
    memset(&s, 0, sizeof s);
    if (getsockname(fd, (struct sockaddr *)&s, &l) != 0) {
        snprintf(out, len, "getsockname %s", strerror(errno));
        return;
    }
    char a[64] = "";
    if (s.ss_family == AF_INET) {
        struct sockaddr_in *i = (struct sockaddr_in *)&s;
        inet_ntop(AF_INET, &i->sin_addr, a, sizeof a);
        snprintf(out, len, "%s:%s", a, ntohs(i->sin_port) ? "nonzero" : "0");
    } else if (s.ss_family == AF_INET6) {
        struct sockaddr_in6 *i = (struct sockaddr_in6 *)&s;
        inet_ntop(AF_INET6, &i->sin6_addr, a, sizeof a);
        snprintf(out, len, "[%s]:%s", a, ntohs(i->sin6_port) ? "nonzero" : "0");
    } else if (s.ss_family == AF_UNIX) {
        snprintf(out, len, "unix len %u", (unsigned)l);
    } else {
        snprintf(out, len, "family %d len %u", s.ss_family, (unsigned)l);
    }
}

int main(void) {
    signal(SIGPIPE, SIG_IGN);
    alarm(30);
    setvbuf(stdout, NULL, _IOLBF, 0);
    int domains[] = {AF_INET, AF_INET6, AF_UNIX};
    const char *dn[] = {"INET", "INET6", "UNIX"};
    int types[] = {SOCK_STREAM, SOCK_DGRAM};
    const char *tn[] = {"STREAM", "DGRAM"};
    // op 0..2: write of 0, 1, 65536 bytes; op 3: non-blocking read of 1.
    const char *on[] = {"write 0", "write 1", "write 65536", "read 1 nonblock"};
    size_t counts[] = {0, 1, 65536, 1};
    for (int d = 0; d < 3; d++)
        for (int t = 0; t < 2; t++)
            for (int op = 0; op < 4; op++) {
                int fd = socket(domains[d], types[t], 0);
                if (fd < 0) { printf("%s %s: socket %s\n", dn[d], tn[t], strerror(errno)); continue; }
                char before[96], after[96];
                name(fd, before, sizeof before);
                ssize_t r;
                if (op == 3) {
                    fcntl(fd, F_SETFL, O_NONBLOCK);
                    r = read(fd, buf, counts[op]);
                } else {
                    r = write(fd, buf, counts[op]);
                }
                int e = errno;
                name(fd, after, sizeof after);
                printf("%-5s %-6s %-15s -> %zd %-28s before %-20s after %s\n", dn[d], tn[t], on[op], r,
                       r < 0 ? strerror(e) : "", before, after);
                close(fd);
            }
    return 0;
}
