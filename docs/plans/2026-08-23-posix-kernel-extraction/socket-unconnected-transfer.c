// read(2) and write(2) on a socket that is not connected: what each answers,
// for every domain, type and unconnected phase this kernel can produce, with
// and without O_NONBLOCK, and whether the write raises SIGPIPE and at whom.
//
// The sweep: domain {AF_INET, AF_INET6, AF_UNIX} x type {SOCK_STREAM,
// SOCK_DGRAM, SOCK_SEQPACKET} x phase {idle, bound, listening without a bind,
// listening after a bind} x {blocking, O_NONBLOCK} x call {read of 0, 1 and
// 65536 bytes; write of 0, 1, 4096, 65507, 65508, 65527, 65528, 65535, 65536,
// 212960 and 1048576 bytes}, the write counts straddling each limit a datagram
// socket's size check could apply (IPv4's and IPv6's largest UDP payloads, the
// 16-bit UDP length, Linux's default send buffer). A socket(2), bind(2) or listen(2)
// that fails ends the row with its errno: the phase does not exist there. Every
// call runs in its own child under alarm(2), so a call that sleeps is reported
// as "sleeps" rather than hanging the sweep; a write runs on a worker thread,
// with a SIGPIPE handler installed, so the report says whether the handler ran
// and on which thread.
//
// Build: `nix develop -c clang -O0 -pthread -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -pthread -o /tmp/p <this file>` on Linux.
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
#include <sys/stat.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

static char buf[1 << 20];
static volatile sig_atomic_t handled = 0;
static volatile sig_atomic_t returned = 0;
static volatile sig_atomic_t handled_before_return = -1;
static pthread_t handler_thread;
static char tmpdir[64];

static void on_pipe(int s) {
    (void)s;
    handled++;
    if (handled_before_return < 0) handled_before_return = !returned;
    handler_thread = pthread_self();
}

static void on_alarm(int s) {
    (void)s;
    static const char msg[] = "sleeps (alarm fired)\n";
    write(1, msg, sizeof msg - 1);
    _exit(3);
}

static const char *errname(int e) {
    switch (e) {
    case 0: return "0";
    case EAGAIN: return "EAGAIN";
    case ENOTCONN: return "ENOTCONN";
    case EPIPE: return "EPIPE";
    case EINVAL: return "EINVAL";
    case EDESTADDRREQ: return "EDESTADDRREQ";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case EPROTONOSUPPORT: return "EPROTONOSUPPORT";
    case EPROTOTYPE: return "EPROTOTYPE";
    case ESOCKTNOSUPPORT: return "ESOCKTNOSUPPORT";
    case EAFNOSUPPORT: return "EAFNOSUPPORT";
    case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
    case EMSGSIZE: return "EMSGSIZE";
    case ENOBUFS: return "ENOBUFS";
    case EADDRINUSE: return "EADDRINUSE";
    default: return "other";
    }
}

typedef struct { int fd; size_t n; ssize_t r; int e; } job;

static void *worker_write(void *arg) {
    job *j = arg;
    j->r = write(j->fd, buf, j->n);
    j->e = j->r < 0 ? errno : 0;
    returned = 1;
    return NULL;
}

static int bind_somewhere(int fd, int domain) {
    if (domain == AF_INET) {
        struct sockaddr_in a;
        memset(&a, 0, sizeof a);
        a.sin_family = AF_INET;
        a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        return bind(fd, (struct sockaddr *)&a, sizeof a);
    }
    if (domain == AF_INET6) {
        struct sockaddr_in6 a;
        memset(&a, 0, sizeof a);
        a.sin6_family = AF_INET6;
        a.sin6_addr = in6addr_loopback;
        return bind(fd, (struct sockaddr *)&a, sizeof a);
    }
    struct sockaddr_un a;
    memset(&a, 0, sizeof a);
    a.sun_family = AF_UNIX;
    // One path per child: each row runs in a child of its own.
    snprintf(a.sun_path, sizeof a.sun_path, "%s/s%d", tmpdir, (int)getpid());
    return bind(fd, (struct sockaddr *)&a, sizeof a);
}

static const char *domname(int d) { return d == AF_INET ? "INET" : d == AF_INET6 ? "INET6" : "UNIX"; }
static const char *typename(int t) {
    return t == SOCK_STREAM ? "STREAM" : t == SOCK_DGRAM ? "DGRAM" : "SEQPACKET";
}
static const char *phasename(int p) {
    return p == 0 ? "idle" : p == 1 ? "bound" : p == 2 ? "listening-unbound" : "listening-bound";
}

// Makes the socket for one row, or reports why the phase does not exist.
static int make(int domain, int type, int phase, int nonblock, char *why, size_t whylen) {
    int fd = socket(domain, type, 0);
    if (fd < 0) {
        snprintf(why, whylen, "socket %s", errname(errno));
        return -1;
    }
    if (phase == 1 || phase == 3) {
        if (bind_somewhere(fd, domain) != 0) {
            snprintf(why, whylen, "bind %s", errname(errno));
            return -1;
        }
    }
    if (phase >= 2) {
        if (listen(fd, 4) != 0) {
            snprintf(why, whylen, "listen %s", errname(errno));
            return -1;
        }
    }
    if (nonblock) fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
    return fd;
}

static const size_t counts[] = {0, 1, 65536, 0, 1, 4096, 65507, 65508, 65527, 65528, 65535, 65536, 212960, 1048576};
#define READS 3
#define CALLS (sizeof counts / sizeof counts[0])

static void one(int domain, int type, int phase, int nonblock, int op) {
    const char *opname = op < READS ? "read" : "write";
    printf("%-5s %-9s %-17s %-8s %-5s %7zu: ", domname(domain), typename(type), phasename(phase),
           nonblock ? "nonblock" : "block", opname, counts[op]);
    fflush(stdout);
    pid_t c = fork();
    if (c == 0) {
        signal(SIGALRM, on_alarm);
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_pipe;
        sigaction(SIGPIPE, &sa, NULL);
        alarm(2);
        char why[64];
        int fd = make(domain, type, phase, nonblock, why, sizeof why);
        if (fd < 0) {
            printf("no such phase (%s)\n", why);
            fflush(stdout);
            _exit(0);
        }
        if (op < READS) {
            ssize_t r = read(fd, buf, counts[op]);
            printf("-> %zd %s\n", r, r < 0 ? errname(errno) : "");
        } else {
            job j = {fd, counts[op], 0, 0};
            pthread_t t;
            pthread_create(&t, NULL, worker_write, &j);
            pthread_join(t, NULL);
            usleep(20 * 1000);
            printf("-> %zd %s; SIGPIPE handler ran %d time(s)", j.r, j.r < 0 ? errname(j.e) : "", (int)handled);
            if (handled)
                printf(", on the %s thread, %s the write returned", pthread_equal(handler_thread, t) ? "writing" : "main",
                       handled_before_return ? "before" : "after");
            printf("\n");
        }
        fflush(stdout);
        _exit(0);
    }
    int ws = 0;
    waitpid(c, &ws, 0);
    if (WIFSIGNALED(ws)) printf("  child died of signal %d\n", WTERMSIG(ws));
    fflush(stdout);
}

int main(void) {
    alarm(600);
    setvbuf(stdout, NULL, _IOLBF, 0);
    memset(buf, 'x', sizeof buf);
    snprintf(tmpdir, sizeof tmpdir, "/tmp/sp%d", (int)getpid());
    mkdir(tmpdir, 0700);
    static const int domains[] = {AF_INET, AF_INET6, AF_UNIX};
    static const int types[] = {SOCK_STREAM, SOCK_DGRAM, SOCK_SEQPACKET};
    for (int d = 0; d < 3; d++)
        for (int t = 0; t < 3; t++)
            for (int p = 0; p < 4; p++)
                for (int nb = 0; nb < 2; nb++)
                    for (int op = 0; op < (int)CALLS; op++) one(domains[d], types[t], p, nb, op);
    return 0;
}
