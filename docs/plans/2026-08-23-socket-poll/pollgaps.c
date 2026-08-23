// The three rows the first two probes got wrong or missed:
//  - a *regular* file (probe 1 measured /dev/null, a character device, and a
//    directory; `OpenFileTarget.File` is regular files and directories);
//  - a connect genuinely still in flight (probe 1's loopback connect had
//    already completed by the time it polled, so its "in flight" row was a
//    duplicate of the completed one);
//  - AF_UNIX SOCK_RAW / SOCK_SEQPACKET, which PawPrint can create on Linux
//    and which poll can therefore reach with no registration step.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static void show(short r) {
    printf("0x%04x", (unsigned short)r);
    if (r & POLLIN)   printf(" IN");
    if (r & POLLPRI)  printf(" PRI");
    if (r & POLLOUT)  printf(" OUT");
    if (r & POLLERR)  printf(" ERR");
    if (r & POLLHUP)  printf(" HUP");
    if (r & POLLNVAL) printf(" NVAL");
}
static void poll_of(int fd, const char *label) {
    struct pollfd p; p.fd = fd; p.events = POLLIN | POLLPRI | POLLOUT; p.revents = 0;
    int rv = poll(&p, 1, 0);
    printf("  %-46s rv=%d revents=", label, rv);
    if (rv < 0) printf("errno=%d", errno); else show(p.revents);
    printf("\n");
}
static void set_nonblock(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

int main(void) {
    printf("== regular files ==\n");
    char tmpl[] = "/tmp/pollgapsXXXXXX";
    int rf = mkstemp(tmpl);
    write(rf, "hello", 5);
    poll_of(rf, "1. regular file, O_RDWR, at EOF");
    lseek(rf, 0, SEEK_SET);
    poll_of(rf, "2. regular file, O_RDWR, at offset 0");
    int ro = open(tmpl, O_RDONLY);
    poll_of(ro, "3. regular file, O_RDONLY");
    close(ro); close(rf); unlink(tmpl);

    printf("== connect genuinely in flight (TEST-NET-1 blackhole) ==\n");
    int c = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    set_nonblock(c);
    struct sockaddr_in ua; memset(&ua, 0, sizeof ua);
    ua.sin_family = AF_INET; ua.sin_port = htons(80);
    inet_pton(AF_INET, "192.0.2.1", &ua.sin_addr);
    int rc = connect(c, (struct sockaddr*)&ua, sizeof ua);
    printf("  (connect rc=%d errno=%d)\n", rc, errno);
    poll_of(c, "4. connect in flight, immediately");
    usleep(300000);
    poll_of(c, "5. connect in flight, 300ms later");
    close(c);

    printf("== AF_UNIX SOCK_RAW / SOCK_SEQPACKET ==\n");
    int raw = socket(AF_UNIX, SOCK_RAW, 0);
    printf("  (AF_UNIX SOCK_RAW socket() rc=%d errno=%d)\n", raw, raw < 0 ? errno : 0);
    if (raw >= 0) { poll_of(raw, "6. AF_UNIX SOCK_RAW, fresh"); close(raw); }
    int sp = socket(AF_UNIX, SOCK_SEQPACKET, 0);
    printf("  (AF_UNIX SOCK_SEQPACKET socket() rc=%d errno=%d)\n", sp, sp < 0 ? errno : 0);
    if (sp >= 0) { poll_of(sp, "7. AF_UNIX SOCK_SEQPACKET, fresh"); close(sp); }
    int us = socket(AF_UNIX, SOCK_STREAM, 0);
    if (us >= 0) { poll_of(us, "8. AF_UNIX SOCK_STREAM, fresh (context)"); close(us); }
    int ud = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (ud >= 0) { poll_of(ud, "9. AF_UNIX SOCK_DGRAM, fresh (context)"); close(ud); }
    return 0;
}
