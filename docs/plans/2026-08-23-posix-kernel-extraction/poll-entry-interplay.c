// poll(2) rows where Darwin's answer depends on more than one entry's own
// state, run on both kernels for comparison: one descriptor named by several
// entries of one call, a wake that adds nothing to `revents`, a peer's close
// asked about with POLLHUP alone, and two events arriving while the poller is
// off the CPU. `poll-darwin.c` has the Darwin side in full; this is the subset
// that compiles on Linux too.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -O1 -o /tmp/pe poll-entry-interplay.c && /tmp/pe
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c "apt-get update -qq
//             && apt-get install -y -qq gcc libc6-dev && gcc -Wall -O1 -pthread -o /tmp/pe
//             /probe/poll-entry-interplay.c && /tmp/pe"
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64 and Linux 6.18.5 aarch64 (glibc
// 2.41); poll-entry-interplay.{darwin-27.0,linux-6.18.5-aarch64}.txt are the
// outputs. Linux answers each entry from its own descriptor's level: M1 and
// M12 report both entries, M13 IN|OUT and IN; W5 (HUP alone, peer closes)
// sleeps to its timeout, W7 (HUP alone, pipe holding data, writer closes)
// answers HUP at once. Darwin answers the opposite of each (see
// poll-darwin.c). W11 is meaningless on Linux, where an idle TCP socket is
// already OUT|HUP, so the poll returns before anything happens.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static long long now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000LL + ts.tv_nsec / 1000000;
}

static const char *b(int v)
{
    static char ring[8][96];
    static int at;
    at = (at + 1) % 8;
    char *buf = ring[at];
    buf[0] = 0;
    if (v == 0) return "0";
    static const struct { int bit; const char *name; } names[] = {
        { POLLIN, "IN" }, { POLLPRI, "PRI" }, { POLLOUT, "OUT" }, { POLLERR, "ERR" }, { POLLHUP, "HUP" },
        { POLLNVAL, "NVAL" }, { POLLRDNORM, "RDNORM" }, { POLLRDBAND, "RDBAND" }, { POLLWRBAND, "WRBAND" },
#ifdef POLLRDHUP
        { POLLRDHUP, "RDHUP" },
#endif
    };
    for (size_t i = 0; i < sizeof names / sizeof names[0]; i++)
        if (v & names[i].bit) {
            size_t len = strlen(buf);
            snprintf(buf + len, 96 - len, "%s%s", len ? "|" : "", names[i].name);
        }
    return buf;
}

static int listener(void)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(s, (struct sockaddr *)&a, sizeof a);
    listen(s, 8);
    return s;
}

static int port_of(int s)
{
    struct sockaddr_in a;
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    return ntohs(a.sin_port);
}

static void connect_to(int s, int port)
{
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    a.sin_port = htons(port);
    connect(s, (struct sockaddr *)&a, sizeof a);
}

static int client(int port)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
    connect_to(s, port);
    return s;
}

static void pair(int *c, int *a)
{
    int l = listener();
    *c = client(port_of(l));
    sleep_ms(30);
    *a = accept(l, NULL, NULL);
    close(l);
}

static void multi(const char *label, struct pollfd *p, int n)
{
    int rv = poll(p, n, 0);
    printf("M\t%s\trv=%d\t", label, rv);
    for (int i = 0; i < n; i++) printf("%s[%d] %s", i ? "; " : "", i, b(p[i].revents));
    printf("\n");
}

struct act { int what; int fd; int port; int delay; int made; };

static void *actor(void *arg)
{
    struct act *x = arg;
    sleep_ms(x->delay);
    if (x->what == 0) x->made = client(x->port);
    else close(x->fd);
    return NULL;
}

static void timed(const char *label, int fd, int events, int timeout, struct act *x)
{
    pthread_t t;
    pthread_create(&t, NULL, actor, x);
    struct pollfd p = { fd, (short)events, 0 };
    long long t0 = now_ms();
    int rv = poll(&p, 1, timeout);
    printf("W\t%s\trv=%d\t%s\tafter %lld ms\n", label, rv, b(p.revents), now_ms() - t0);
    pthread_join(t, NULL);
}

static volatile int hogging;

static void *hog(void *arg)
{
    (void)arg;
    while (hogging) { }
    return NULL;
}

static void race(const char *label, int fin, int trials)
{
    int ncpu = (int)sysconf(_SC_NPROCESSORS_ONLN);
    char seen[8][96];
    int counts[8] = { 0 }, nseen = 0;
    for (int t = 0; t < trials; t++) {
        int l = listener();
        int s = socket(AF_INET, SOCK_STREAM, 0);
        fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
        int out[2];
        if (pipe(out) != 0) exit(1);
        pid_t child = fork();
        if (child == 0) {
            alarm(10);
#ifdef __APPLE__
            setpriority(PRIO_DARWIN_THREAD, 0, PRIO_DARWIN_BG);
#else
            struct sched_param sp = { 0 };
            sched_setscheduler(0, SCHED_IDLE, &sp);
#endif
            struct pollfd p = { s, POLLIN | POLLOUT, 0 };
            poll(&p, 1, 3000);
            const char *r = b(p.revents);
            if (write(out[1], r, strlen(r)) < 0) _exit(1);
            _exit(0);
        }
        close(out[1]);
        sleep_ms(100);
        hogging = 1;
        pthread_t hogs[64];
        int nh = ncpu < 64 ? ncpu : 64;
        for (int i = 0; i < nh; i++) pthread_create(&hogs[i], NULL, hog, NULL);
        sleep_ms(5);
        connect_to(s, port_of(l));
        int a = accept(l, NULL, NULL);
        if (fin) close(a);
        sleep_ms(20);
        hogging = 0;
        for (int i = 0; i < nh; i++) pthread_join(hogs[i], NULL);
        char line[96] = { 0 };
        if (read(out[0], line, sizeof line - 1) < 0) line[0] = 0;
        int status;
        waitpid(child, &status, 0);
        close(out[0]);
        if (!fin) close(a);
        close(s);
        close(l);
        int k;
        for (k = 0; k < nseen; k++) if (strcmp(seen[k], line) == 0) break;
        if (k == nseen && nseen < 8) { snprintf(seen[nseen], sizeof seen[nseen], "%s", line); nseen++; }
        if (k < 8) counts[k]++;
    }
    printf("W\t%s\t%d trials:", label, trials);
    for (int k = 0; k < nseen; k++) printf(" %s x%d", seen[k], counts[k]);
    printf("\n");
}

int main(void)
{
    setvbuf(stdout, NULL, _IOLBF, 0);
    alarm(120);
    signal(SIGPIPE, SIG_IGN);

    int l = listener();
    int c0 = client(port_of(l));
    sleep_ms(30);
    int ld = dup(l);
    {
        struct pollfd p[] = { { l, POLLIN, 0 }, { l, POLLIN, 0 } };
        multi("M1 queued listener twice, IN and IN", p, 2);
    }
    {
        struct pollfd p[] = { { l, POLLIN, 0 }, { ld, POLLIN, 0 } };
        multi("M2 queued listener and its dup, IN and IN", p, 2);
    }
    close(ld);
    close(c0);
    close(l);

    int c, a;
    pair(&c, &a);
    {
        struct pollfd p[] = { { c, POLLOUT, 0 }, { c, POLLOUT, 0 } };
        multi("M12 connected client OUT twice", p, 2);
    }
    close(a);
    sleep_ms(30);
    {
        struct pollfd p[] = { { c, POLLIN | POLLOUT, 0 }, { c, POLLIN, 0 } };
        multi("M13 peer-closed client IN|OUT, then the same IN", p, 2);
    }
    {
        struct pollfd p[] = { { c, POLLIN | POLLOUT, 0 } };
        multi("M13 control: peer-closed client IN|OUT alone", p, 1);
    }
    close(c);

    {
        int l = listener();
        struct act x = { 0, -1, port_of(l), 50, -1 };
        timed("W2 empty listener HUP, a connection at 50 ms", l, POLLHUP, 300, &x);
        close(x.made);
        close(l);
    }
    {
        int c, a;
        pair(&c, &a);
        struct act x = { 1, a, 0, 50, -1 };
        timed("W5 connected client HUP, the peer closes at 50 ms", c, POLLHUP, 300, &x);
        close(c);
    }
    {
        int pp[2];
        if (pipe(pp) != 0) return 1;
        if (write(pp[1], "abc", 3) != 3) return 1;
        struct act x = { 1, pp[1], 0, 50, -1 };
        timed("W7 pipe read end holding 3, HUP, the writer closes at 50 ms", pp[0], POLLHUP, 300, &x);
        close(pp[0]);
    }
    race("W11 idle socket IN|OUT, poller deprioritised, CPUs busy; connect, accept, peer close", 1, 40);
    return 0;
}
