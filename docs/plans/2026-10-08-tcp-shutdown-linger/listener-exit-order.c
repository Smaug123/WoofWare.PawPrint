// Where a listener's release falls among the releases a process's exit makes.
//
// The child listens, and connects a socket to the parent's listener, which the
// parent accepts as c. The parent connects q to the child's listener, which
// the child never accepts. The child moves its listener and its connected
// socket to descriptors 10 and 11, in one order or the other, and exits
// without closing anything. The parent watches c and q in one epoll instance
// (Linux, EPOLLET IN|OUT|RDHUP) or kqueue (Darwin, EVFILT_READ EV_CLEAR),
// registered c first, and prints the events in the order they reached it: c's
// FIN, and q's reset.
//
// The release of a listener falls where its descriptor puts it among the
// rest, as exit-close-order.c measured for connected sockets
// (docs/plans/2026-08-23-posix-kernel-extraction): Linux drops the
// descriptors lowest first and releases the last let go of first, and Darwin
// releases them highest first.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/listener-exit-order listener-exit-order.c && /tmp/listener-exit-order
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/listener-exit-order.c && /tmp/p'
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), five trials of each
// order, the same every time: listener-exit-order.linux-6.18.5-aarch64.txt
// and listener-exit-order.darwin-27.0.txt.
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static void die(const char *what) { perror(what); exit(2); }

static int listener_on_loopback(struct sockaddr_in *bound)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, 16) < 0) die("listen");
    socklen_t len = sizeof *bound;
    getsockname(l, (struct sockaddr *)bound, &len);
    return l;
}

static int connect_to(const struct sockaddr_in *to)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(c, (const struct sockaddr *)to, sizeof *to) < 0) die("connect");
    return c;
}

static void move_to(int from, int to)
{
    if (dup2(from, to) < 0) die("dup2");
    close(from);
}

static void trial(int listenerFirst)
{
    struct sockaddr_in parentAt;
    int pl = listener_on_loopback(&parentAt);
    int ready[2];
    int go[2];
    if (pipe(ready) < 0 || pipe(go) < 0) die("pipe");
    pid_t child = fork();
    if (child == 0) {
        struct sockaddr_in childAt;
        int cl = listener_on_loopback(&childAt);
        int cs = connect_to(&parentAt);
        // Tell the parent the child's port, then wait for it to have connected.
        if (write(ready[1], &childAt, sizeof childAt) != sizeof childAt) die("write");
        char b;
        if (read(go[0], &b, 1) != 1) die("read");
        int lfd = listenerFirst ? 10 : 11;
        int sfd = listenerFirst ? 11 : 10;
        move_to(cl, lfd);
        move_to(cs, sfd);
        if (write(ready[1], &b, 1) != 1) die("write");
        if (read(go[0], &b, 1) != 1) die("read");
        _exit(0);
    }
    struct sockaddr_in childAt;
    if (read(ready[0], &childAt, sizeof childAt) != sizeof childAt) die("read");
    int c = accept(pl, NULL, NULL);
    if (c < 0) die("accept");
    int q = connect_to(&childAt);
    char b = 0;
    if (write(go[1], &b, 1) != 1) die("write");
    if (read(ready[0], &b, 1) != 1) die("read");
#ifdef __linux__
    int ep = epoll_create1(0);
    struct epoll_event ev = { .events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET, .data.u32 = 'c' };
    epoll_ctl(ep, EPOLL_CTL_ADD, c, &ev);
    ev.data.u32 = 'q';
    epoll_ctl(ep, EPOLL_CTL_ADD, q, &ev);
    struct epoll_event out[4];
    epoll_wait(ep, out, 4, 0);
#else
    int kq = kqueue();
    struct kevent ch[2];
    EV_SET(&ch[0], c, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, (void *)(long)'c');
    EV_SET(&ch[1], q, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, (void *)(long)'q');
    kevent(kq, ch, 2, NULL, 0, NULL);
    struct kevent out[4];
    struct timespec zero = { 0, 0 };
    kevent(kq, NULL, 0, out, 4, &zero);
#endif
    if (write(go[1], &b, 1) != 1) die("write");
    waitpid(child, NULL, 0);
    usleep(50000);
    printf("listener at %s: ", listenerFirst ? "10, connected socket at 11" : "11, connected socket at 10");
#ifdef __linux__
    int n = epoll_wait(ep, out, 4, 0);
    for (int k = 0; k < n; k++) printf("%c(0x%x) ", out[k].data.u32, out[k].events);
    close(ep);
#else
    int n = kevent(kq, NULL, 0, out, 4, &zero);
    for (int k = 0; k < n; k++) printf("%c(%s/%u) ", (int)(long)out[k].udata, (out[k].flags & EV_EOF) ? "EOF" : "", out[k].fflags);
    close(kq);
#endif
    printf("\n");
    close(c);
    close(q);
    close(pl);
    close(ready[0]); close(ready[1]); close(go[0]); close(go[1]);
}

int main(void)
{
    for (int t = 0; t < 5; t++) {
        trial(1);
        trial(0);
    }
    return 0;
}
