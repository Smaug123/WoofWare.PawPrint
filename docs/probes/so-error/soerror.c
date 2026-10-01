// What getsockopt(SOL_SOCKET, SO_ERROR) reads in each socket phase, what a read
// consumes, and what a refused socket answers afterwards.
//
// Both flavours; outputs beside this file.
//
//     cc -Wall -o p soerror.c && ./p
//     container run --rm -v "$PWD:/probe" debian:trixie sh -c \
//       'apt-get update -qq && apt-get install -y -qq gcc libc6-dev >/dev/null && gcc -Wall -o /tmp/p /probe/soerror.c && uname -r && /tmp/p'
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static struct sockaddr_in la, da;
static int l;
static void *unmapped;

static void show(short r) {
    printf("0x%04x", (unsigned short)r);
    if (r & POLLIN) printf(" IN");
    if (r & POLLPRI) printf(" PRI");
    if (r & POLLOUT) printf(" OUT");
    if (r & POLLERR) printf(" ERR");
    if (r & POLLHUP) printf(" HUP");
#ifdef POLLRDHUP
    if (r & POLLRDHUP) printf(" RDHUP");
#endif
}
static void pollrow(int fd, const char *label) {
    struct pollfd p = { fd, POLLIN | POLLPRI | POLLOUT, 0 };
    int rv = poll(&p, 1, 0);
    printf("    poll %-28s rv=%d ", label, rv); show(p.revents); printf("\n");
}
static void soerr(int fd, const char *label) {
    int v = -7; socklen_t len = sizeof v; errno = 0;
    int rc = getsockopt(fd, SOL_SOCKET, SO_ERROR, &v, &len);
    printf("    SO_ERROR %-24s rc=%d errno=%d value=%d len=%u\n", label, rc, rc ? errno : 0, v, (unsigned)len);
}
static void soerr_len(int fd, const char *label, int declared, int nullval, int badval) {
    int v = -7; socklen_t len = (socklen_t)declared; errno = 0;
    void *p = nullval ? NULL : badval ? unmapped : &v;
    int rc = getsockopt(fd, SOL_SOCKET, SO_ERROR, p, &len);
    printf("    SO_ERROR %-24s rc=%d errno=%d value=%d len=%u\n", label, rc, rc ? errno : 0, v, (unsigned)len);
}
static void conn(int fd, struct sockaddr_in *a, const char *label) {
    errno = 0; int rc = connect(fd, (struct sockaddr *)a, sizeof *a);
    printf("    connect %-25s rc=%d errno=%d\n", label, rc, rc ? errno : 0);
}
static void reuse(int fd) {
    int one = 1; errno = 0;
    int rc = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    printf("    setsockopt REUSEADDR             rc=%d errno=%d\n", rc, rc ? errno : 0);
}
static void lst(int fd) {
    errno = 0; int rc = listen(fd, 4);
    printf("    listen                           rc=%d errno=%d\n", rc, rc ? errno : 0);
}
static void nb(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }
static int tcp(void) { return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); }
static int refused_nonblocking(void) {
    int c = tcp(); nb(c);
    connect(c, (struct sockaddr *)&da, sizeof da);
    usleep(100000);
    return c;
}

int main(void) {
    signal(SIGPIPE, SIG_IGN);
    unmapped = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    l = tcp();
    memset(&la, 0, sizeof la); la.sin_family = AF_INET; la.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(l, (struct sockaddr *)&la, sizeof la); listen(l, 16);
    socklen_t sl = sizeof la; getsockname(l, (struct sockaddr *)&la, &sl);
    int d = tcp(); da = la; da.sin_port = 0;
    bind(d, (struct sockaddr *)&da, sizeof da); sl = sizeof da; getsockname(d, (struct sockaddr *)&da, &sl); close(d);

    printf("== A. phases that hold nothing ==\n");
    int f = tcp(); printf("  fresh TCP\n"); soerr(f, "");
    int b = tcp(); struct sockaddr_in ba = la; ba.sin_port = 0; bind(b, (struct sockaddr *)&ba, sizeof ba);
    printf("  bound TCP\n"); soerr(b, "");
    printf("  listening, queue empty\n"); soerr(l, "");
    int qc = tcp(); connect(qc, (struct sockaddr *)&la, sizeof la);
    printf("  listening, queue holds one\n"); soerr(l, "");
    printf("  blocking-connected client\n"); soerr(qc, "");
    int qs = accept(l, NULL, NULL);
    printf("  accepted server end\n"); soerr(qs, "");
    int u = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); printf("  fresh UDP\n"); soerr(u, "");
    connect(u, (struct sockaddr *)&la, sizeof la); printf("  UDP with a peer\n"); soerr(u, "");
    int u2 = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); connect(u2, (struct sockaddr *)&da, sizeof da);
    send(u2, "x", 1, 0); usleep(100000);
    printf("  UDP with a peer, after a send to a closed port\n"); soerr(u2, "first"); soerr(u2, "second");

    printf("== B. nonblocking connect that completed ==\n");
    int c = tcp(); nb(c); conn(c, &la, "first"); usleep(100000);
    pollrow(c, "before read");
    soerr(c, "first"); soerr(c, "second");
    pollrow(c, "after read");
    conn(c, &la, "after read"); conn(c, &la, "again");

    printf("== C. nonblocking connect refused, never read ==\n");
    c = refused_nonblocking();
    pollrow(c, "pending"); reuse(c); lst(c);
    conn(c, &da, "delivers"); pollrow(c, "after delivery"); soerr(c, "after delivery"); conn(c, &da, "after delivery");
    close(c);

    printf("== D. nonblocking connect refused, then SO_ERROR read ==\n");
    c = refused_nonblocking();
    soerr(c, "first"); soerr(c, "second");
    pollrow(c, "after read"); reuse(c); lst(c);
    conn(c, &da, "1st after read"); pollrow(c, "after 1st connect"); soerr(c, "after 1st connect");
    conn(c, &da, "2nd after read"); usleep(100000); pollrow(c, "after 2nd connect"); soerr(c, "after 2nd connect");
    conn(c, &da, "3rd after read");
    close(c);
    printf("  D2: read, then connect to the live listener\n");
    c = refused_nonblocking(); soerr(c, "first"); conn(c, &la, "to listener"); usleep(100000); conn(c, &la, "again");
    close(c);

    printf("== E. blocking connect refused ==\n");
    c = tcp(); conn(c, &da, "refused inline");
    soerr(c, "first"); pollrow(c, "after"); reuse(c); lst(c);
    conn(c, &da, "next"); conn(c, &la, "to listener");
    close(c);

    printf("== F. does a read consume, by length and buffer? ==\n");
    c = refused_nonblocking(); soerr_len(c, "declared 0", 0, 0, 0); soerr(c, "then");
    close(c);
    c = refused_nonblocking(); soerr_len(c, "declared 2", 2, 0, 0); soerr(c, "then");
    close(c);
    c = refused_nonblocking(); soerr_len(c, "declared 8", 8, 0, 0); soerr(c, "then");
    close(c);
    c = refused_nonblocking(); soerr_len(c, "declared -1", -1, 0, 0); soerr(c, "then");
    close(c);
    c = refused_nonblocking(); soerr_len(c, "null value, declared 4", 4, 1, 0); soerr(c, "then");
    close(c);
    c = refused_nonblocking(); soerr_len(c, "unmapped value, declared 4", 4, 0, 1); soerr(c, "then");
    close(c);
    c = refused_nonblocking();
    { errno = 0; int rc = getsockopt(c, SOL_SOCKET, SO_ERROR, &(int){0}, unmapped);
      printf("    SO_ERROR %-24s rc=%d errno=%d\n", "unmapped length cell", rc, rc ? errno : 0); }
    soerr(c, "then");
    close(c);

    printf("== G. established, peer closed with unread data (RST) ==\n");
    int cc = tcp(); connect(cc, (struct sockaddr *)&la, sizeof la);
    int ss = accept(l, NULL, NULL);
    write(cc, "z", 1); usleep(50000);
    close(ss); usleep(100000);
    pollrow(cc, "before read");
    soerr(cc, "first"); soerr(cc, "second"); pollrow(cc, "after read");
    printf("== H. established, peer closed cleanly (FIN) ==\n");
    cc = tcp(); connect(cc, (struct sockaddr *)&la, sizeof la);
    ss = accept(l, NULL, NULL); close(ss); usleep(100000);
    soerr(cc, "");
    printf("== I. setsockopt(SO_ERROR) ==\n");
    { int z = 0; errno = 0; int rc = setsockopt(f, SOL_SOCKET, SO_ERROR, &z, sizeof z);
      printf("    rc=%d errno=%d\n", rc, rc ? errno : 0); }
    return 0;
}
