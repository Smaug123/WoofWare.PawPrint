// What a reserved park for a blocking datagram receive (and, perhaps, send)
// must answer, measured on both flavours. Design note:
// pawprint-kernel-design/datagram-park-options.md.
//
// Build and run:
//   Darwin: cc -Wall -o /tmp/dp datagram-park.c -lpthread && /tmp/dp
//   Linux:  gcc -Wall -o /tmp/dp datagram-park.c -lpthread && /tmp/dp
//
// Sections:
//   R  a blocking receive on a datagram socket: what ends it, through each
//      receiving call, and how a caught signal ends it
//   S  whether a blocking datagram send sleeps: UDP over loopback, and
//      Unix-domain datagrams, connected and not
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>

static volatile sig_atomic_t handled = 0;
static void on_usr1(int sig) { (void)sig; handled++; }

static double now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1e3 + ts.tv_nsec / 1e6;
}

static void install(int restart)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sa.sa_flags = restart ? SA_RESTART : 0;
    sigaction(SIGUSR1, &sa, NULL);
}

enum action { SIGNAL, SEND_TO, CLOSE_FD, SHUTDOWN_RD, CLOSE_PEER };

struct later {
    pthread_t target;
    int ms;
    enum action what;
    int fd;
    struct sockaddr_storage to;
    socklen_t to_len;
};

static void *do_later(void *p)
{
    struct later *l = p;
    usleep(l->ms * 1000);
    switch (l->what) {
    case SIGNAL: pthread_kill(l->target, SIGUSR1); break;
    case SEND_TO:
        if (l->to_len == 0) (void)!send(l->fd, "hello", 5, 0);
        else (void)!sendto(l->fd, "hello", 5, 0, (struct sockaddr *)&l->to, l->to_len);
        break;
    case CLOSE_FD: close(l->fd); break;
    case SHUTDOWN_RD: shutdown(l->fd, SHUT_RD); break;
    case CLOSE_PEER: close(l->fd); break;
    }
    return NULL;
}

static pthread_t later(struct later *l, int ms, enum action what, int fd)
{
    pthread_t t;
    l->target = pthread_self();
    l->ms = ms;
    l->what = what;
    l->fd = fd;
    pthread_create(&t, NULL, do_later, l);
    return t;
}

static int udp_bound(struct sockaddr_in *addr)
{
    int s = socket(AF_INET, SOCK_DGRAM, 0);
    memset(addr, 0, sizeof *addr);
    addr->sin_family = AF_INET;
    addr->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(s, (struct sockaddr *)addr, sizeof *addr);
    socklen_t len = sizeof *addr;
    getsockname(s, (struct sockaddr *)addr, &len);
    return s;
}

static const char *call_name[] = { "read", "recv", "recvfrom", "recvmsg" };

static ssize_t receive(int call, int s, char *buf, size_t n)
{
    struct sockaddr_storage from;
    socklen_t from_len = sizeof from;
    struct iovec iov = { buf, n };
    struct msghdr msg;
    memset(&msg, 0, sizeof msg);
    msg.msg_name = &from;
    msg.msg_namelen = sizeof from;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    switch (call) {
    case 0: return read(s, buf, n);
    case 1: return recv(s, buf, n, 0);
    case 2: return recvfrom(s, buf, n, 0, (struct sockaddr *)&from, &from_len);
    default: return recvmsg(s, &msg, 0);
    }
}

static void report(const char *row, ssize_t r, int e, double took)
{
    printf("%s: ret=%zd errno=%s handled=%d took_ms=%.0f\n", row, r, r < 0 ? strerror(e) : "-", (int)handled, took);
}

static void section_r(void)
{
    char buf[64];
    struct later l;
    pthread_t t;
    double t0;
    ssize_t r;
    int e;
    char row[160];

    // R1: each receiving call on an unconnected, bound UDP socket sleeps until
    // a datagram arrives at 100 ms.
    for (int call = 0; call < 4; call++) {
        struct sockaddr_in addr;
        int s = udp_bound(&addr);
        int sender = socket(AF_INET, SOCK_DGRAM, 0);
        memcpy(&l.to, &addr, sizeof addr);
        l.to_len = sizeof addr;
        t = later(&l, 100, SEND_TO, sender);
        t0 = now_ms();
        r = receive(call, s, buf, sizeof buf); e = errno;
        snprintf(row, sizeof row, "R1 UDP %s, a datagram at 100ms", call_name[call]);
        report(row, r, e, now_ms() - t0);
        pthread_join(t, NULL);
        close(s); close(sender);
    }

    // R2: a caught signal at 100 ms, without and with SA_RESTART; with it, a
    // datagram follows at 300 ms in case the call restarts.
    for (int domain = 0; domain < 2; domain++) {
        for (int restart = 0; restart <= 1; restart++) {
            install(restart);
            handled = 0;
            int s, peer;
            struct sockaddr_in addr;
            if (domain == 0) {
                s = udp_bound(&addr);
                peer = socket(AF_INET, SOCK_DGRAM, 0);
            } else {
                int sv[2];
                socketpair(AF_UNIX, SOCK_DGRAM, 0, sv);
                s = sv[0]; peer = sv[1];
            }
            t = later(&l, 100, SIGNAL, -1);
            struct later l2;
            if (domain == 0) { memcpy(&l2.to, &addr, sizeof addr); l2.to_len = sizeof addr; }
            else l2.to_len = 0;
            pthread_t t2;
            if (domain == 0) t2 = later(&l2, 300, SEND_TO, peer);
            else { l2.target = pthread_self(); l2.ms = 300; l2.what = SEND_TO; l2.fd = peer; pthread_create(&t2, NULL, do_later, &l2); }
            t0 = now_ms();
            r = recv(s, buf, sizeof buf, 0); e = errno;
            snprintf(row, sizeof row, "R2 %s recv, caught signal at 100ms, datagram at 300ms, SA_RESTART=%d",
                     domain == 0 ? "UDP" : "Unix-domain socketpair", restart);
            report(row, r, e, now_ms() - t0);
            pthread_join(t, NULL); pthread_join(t2, NULL);
            close(s); close(peer);
        }
    }
    install(0);

    // R3: another thread closes the only descriptor at 100 ms; a datagram
    // follows at 300 ms (sent to the address, which the socket still holds if
    // the call keeps it alive).
    {
        struct sockaddr_in addr;
        int s = udp_bound(&addr);
        int sender = socket(AF_INET, SOCK_DGRAM, 0);
        t = later(&l, 100, CLOSE_FD, s);
        struct later l2;
        memcpy(&l2.to, &addr, sizeof addr); l2.to_len = sizeof addr;
        pthread_t t2 = later(&l2, 300, SEND_TO, sender);
        t0 = now_ms();
        r = recv(s, buf, sizeof buf, 0); e = errno;
        report("R3 UDP recv, close of its descriptor at 100ms, datagram at 300ms", r, e, now_ms() - t0);
        pthread_join(t, NULL); pthread_join(t2, NULL);
        close(sender);
    }

    // R4: shutdown(SHUT_RD) of the socket at 100 ms.
    {
        struct sockaddr_in addr;
        int s = udp_bound(&addr);
        handled = 0;
        t = later(&l, 100, SHUTDOWN_RD, s);
        struct later l3;
        pthread_t t3 = later(&l3, 600, SIGNAL, -1);
        t0 = now_ms();
        r = recv(s, buf, sizeof buf, 0); e = errno;
        report("R4 UDP recv, shutdown(SHUT_RD) at 100ms (signal at 600ms)", r, e, now_ms() - t0);
        pthread_join(t, NULL); pthread_join(t3, NULL);
        close(s);
    }

    // R5: the peer of a Unix-domain datagram socketpair closes at 100 ms.
    {
        int sv[2];
        socketpair(AF_UNIX, SOCK_DGRAM, 0, sv);
        handled = 0;
        t = later(&l, 100, CLOSE_PEER, sv[1]);
        struct later l3;
        pthread_t t3 = later(&l3, 600, SIGNAL, -1);
        t0 = now_ms();
        r = recv(sv[0], buf, sizeof buf, 0); e = errno;
        report("R5 Unix-domain socketpair recv, peer closes at 100ms (signal at 600ms)", r, e, now_ms() - t0);
        pthread_join(t, NULL); pthread_join(t3, NULL);
        close(sv[0]);
    }

    // R6: SO_RCVTIMEO of 200 ms on a UDP socket.
    {
        struct sockaddr_in addr;
        int s = udp_bound(&addr);
        struct timeval tv = { 0, 200000 };
        int set = setsockopt(s, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv);
        t0 = now_ms();
        r = recv(s, buf, sizeof buf, 0); e = errno;
        snprintf(row, sizeof row, "R6 UDP recv, SO_RCVTIMEO 200ms (setsockopt=%d)", set);
        report(row, r, e, now_ms() - t0);
        close(s);
    }
}

// Sends 100-byte datagrams with a 2 s alarm-free watchdog: reports how many
// went before one blocked (the watchdog signal ends it) or failed.
static volatile int watchdog_fired = 0;
static void *watchdog(void *p)
{
    pthread_t target = *(pthread_t *)p;
    usleep(1000 * 1000);
    watchdog_fired = 1;
    pthread_kill(target, SIGUSR1);
    return NULL;
}

static void fill(const char *row, int s, const struct sockaddr *to, socklen_t to_len)
{
    install(0);
    handled = 0;
    watchdog_fired = 0;
    pthread_t self = pthread_self(), t;
    pthread_create(&t, NULL, watchdog, &self);
    char buf[100];
    memset(buf, 'x', sizeof buf);
    long sent = 0;
    ssize_t r = 0;
    int e = 0;
    double t0 = now_ms();
    for (; sent < 200000; sent++) {
        r = to ? sendto(s, buf, sizeof buf, 0, to, to_len) : send(s, buf, sizeof buf, 0);
        if (r < 0) { e = errno; break; }
        if (watchdog_fired) break;
    }
    double took = now_ms() - t0;
    pthread_join(t, NULL);
    printf("%s: sent=%ld last_ret=%zd errno=%s blocked_until_watchdog=%d took_ms=%.0f\n", row, sent, r,
           r < 0 ? strerror(e) : "-", r < 0 && e == EINTR && watchdog_fired, took);
}

static void section_s(void)
{
    // S1: UDP over loopback to a bound socket nobody reads.
    {
        struct sockaddr_in addr;
        int s = udp_bound(&addr);
        int sender = socket(AF_INET, SOCK_DGRAM, 0);
        fill("S1 UDP sendto a bound socket nobody reads", sender, (struct sockaddr *)&addr, sizeof addr);
        close(s); close(sender);
    }
    // S2: a Unix-domain datagram socketpair, the peer reading nothing.
    {
        int sv[2];
        socketpair(AF_UNIX, SOCK_DGRAM, 0, sv);
        fill("S2 Unix-domain socketpair send, peer reads nothing", sv[0], NULL, 0);
        close(sv[0]); close(sv[1]);
    }
    // S3: an unconnected Unix-domain datagram socket sending to a bound one
    // that reads nothing.
    {
        struct sockaddr_un addr;
        memset(&addr, 0, sizeof addr);
        addr.sun_family = AF_UNIX;
        snprintf(addr.sun_path, sizeof addr.sun_path, "/tmp/dp-%d.sock", (int)getpid());
        unlink(addr.sun_path);
        int s = socket(AF_UNIX, SOCK_DGRAM, 0);
        bind(s, (struct sockaddr *)&addr, sizeof addr);
        int sender = socket(AF_UNIX, SOCK_DGRAM, 0);
        fill("S3 Unix-domain sendto a bound socket nobody reads", sender, (struct sockaddr *)&addr, sizeof addr);
        close(s); close(sender);
        unlink(addr.sun_path);
    }
}

// S4: a blocking Unix-domain datagram send that sleeps (filled first through
// O_NONBLOCK), with a caught signal at 100 ms, without and with SA_RESTART,
// and the receiver taking every queued datagram at 300 ms.
struct drain { int fd; int ms; };
static void *do_drain(void *p)
{
    struct drain *d = p;
    usleep(d->ms * 1000);
    char buf[200];
    // Every queued datagram: a socketpair's sender waits for half its send
    // buffer to be free, not for room for one more.
    while (recv(d->fd, buf, sizeof buf, MSG_DONTWAIT) >= 0) {}
    return NULL;
}

static void section_s4(void)
{
    for (int connected = 1; connected >= 0; connected--) {
        for (int restart = 0; restart <= 1; restart++) {
            int sender, receiver;
            struct sockaddr_un addr;
            memset(&addr, 0, sizeof addr);
            if (connected) {
                int sv[2];
                socketpair(AF_UNIX, SOCK_DGRAM, 0, sv);
                sender = sv[0]; receiver = sv[1];
            } else {
                addr.sun_family = AF_UNIX;
                snprintf(addr.sun_path, sizeof addr.sun_path, "/tmp/dp4-%d.sock", (int)getpid());
                unlink(addr.sun_path);
                receiver = socket(AF_UNIX, SOCK_DGRAM, 0);
                bind(receiver, (struct sockaddr *)&addr, sizeof addr);
                sender = socket(AF_UNIX, SOCK_DGRAM, 0);
            }
            char buf[100];
            memset(buf, 'x', sizeof buf);
            int flags = fcntl(sender, F_GETFL);
            fcntl(sender, F_SETFL, flags | O_NONBLOCK);
            long filled = 0;
            int fill_errno = 0;
            for (; filled < 100000; filled++) {
                ssize_t r = connected ? send(sender, buf, sizeof buf, 0)
                                      : sendto(sender, buf, sizeof buf, 0, (struct sockaddr *)&addr, sizeof addr);
                if (r < 0) { fill_errno = errno; break; }
            }
            fcntl(sender, F_SETFL, flags);
            install(restart);
            handled = 0;
            struct later l;
            pthread_t t = later(&l, 100, SIGNAL, -1);
            struct drain d = { receiver, 300 };
            pthread_t t2;
            pthread_create(&t2, NULL, do_drain, &d);
            double t0 = now_ms();
            ssize_t r = connected ? send(sender, buf, sizeof buf, 0)
                                  : sendto(sender, buf, sizeof buf, 0, (struct sockaddr *)&addr, sizeof addr);
            int e = errno;
            double took = now_ms() - t0;
            pthread_join(t, NULL); pthread_join(t2, NULL);
            printf("S4 Unix-domain %s, filled with %ld (then %s), blocking send, signal at 100ms, the queue drained at 300ms, "
                   "SA_RESTART=%d: ret=%zd errno=%s handled=%d took_ms=%.0f\n",
                   connected ? "socketpair" : "sendto a bound socket", filled, strerror(fill_errno), restart, r,
                   r < 0 ? strerror(e) : "-", (int)handled, took);
            close(sender); close(receiver);
            if (!connected) unlink(addr.sun_path);
        }
    }
    install(0);
}

int main(void)
{
    setvbuf(stdout, NULL, _IONBF, 0);
    install(0);
    section_r();
    section_s();
    section_s4();
    return 0;
}
