// poll(2)'s timeout: when a wait that finds nothing ready returns, what a
// negative timeout means, whether readiness ends the wait early, the order of
// the argument checks, and what happens to a wait whose descriptor another
// thread closes.
//
// Every wait here is on a bound UDP socket asked only for POLLIN, which
// presents nothing (it is writable, and OUT was not asked for) until a datagram
// arrives. Elapsed times are CLOCK_MONOTONIC, in microseconds.
//
// Sections:
//   A  expiry: timeouts 1, 2, 3, 5, 10, 20 and 50 ms, 20 waits each; rv,
//      revents, and the least and greatest elapsed time. Also timeout 0.
//   B  negative timeouts -1, -2, -3, -1000 and INT_MIN, each cut short by a
//      250 ms interval timer whose SIGALRM handler does not restart: EINTR
//      after ~250 ms means "infinite", an immediate answer is the timeout
//      being screened. Each is also asked of a ready descriptor, and with
//      nfds = 0, to place the screen relative to the scan.
//   C  a datagram sent by another thread 50 ms into a 2000 ms wait.
//   D  the order of the argument checks: nfds above RLIMIT_NOFILE, a buffer
//      that is not mapped, and timeout -2, in each combination.
//   E  another thread acts on the polled descriptor 50 ms into a 300 ms wait:
//      E1 closes it; E2 closes it and a new socket takes its number, then
//      receives a datagram; E3 closes it while a dup keeps the description
//      alive; E4 closes it and then sends a datagram to the closed socket's
//      address; E5 (control) sends a datagram without closing anything.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/pt poll-timeout.c && /tmp/pt
//   Linux:  container run --rm -v "$PWD:/probe" debian:trixie sh -c "apt-get update -qq
//             && apt-get install -y -qq gcc libc6-dev && gcc -Wall -pthread -o /tmp/pt
//             /probe/poll-timeout.c && /tmp/pt"
//
// Measured 2026-09-26 on Linux 6.18.5 aarch64 (glibc 2.41, Apple `container`)
// and Darwin 27.0.0 arm64:
//   A  both: no wait returned before its timeout, and every one returned 0 with
//      revents 0. Least/greatest elapsed for 1 ms: Linux 1019/1418 us, Darwin
//      1012/1326; for 50 ms: 50101/55128 and 50016/51477. Timeout 0 returned in
//      at most 16 us.
//   B  both: every negative timeout was infinite (EINTR at ~250 ms), with a
//      ready entry answered at once and nfds = 0 sleeping too. Darwin did not
//      answer EINVAL for any of them.
//   C  both: rv 1, revents POLLIN, after ~53 ms.
//   D  both: nfds > RLIMIT_NOFILE is EINVAL whatever the buffer and timeout;
//      nfds = 1 with a bad buffer is EFAULT; no timeout is screened; nfds = 0
//      never touches the buffer.
//   E  Linux: E1 rv 1 POLLNVAL at the timeout; E2 rv 1 POLLIN (the new
//      socket's) at the timeout, not when it became ready; E3 rv 1 POLLNVAL at
//      the timeout; E4 rv 1 POLLNVAL at ~79 ms, woken by the datagram to the
//      closed socket; E5 rv 1 POLLIN at ~55 ms.
//      Darwin: E1-E4 rv 0 at the timeout; E5 rv 1 POLLIN at ~53 ms.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

static int64_t now_us(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

static int bound_udp(struct sockaddr_in *addr)
{
    int s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s < 0) { perror("socket"); exit(1); }
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) { perror("bind"); exit(1); }
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    if (addr) *addr = a;
    return s;
}

static void send_to(const struct sockaddr_in *addr)
{
    int t = socket(AF_INET, SOCK_DGRAM, 0);
    if (sendto(t, "x", 1, 0, (const struct sockaddr *)addr, sizeof *addr) != 1) perror("sendto");
    close(t);
}

static void on_alarm(int sig) { (void)sig; }

static void arm_timer_ms(int ms)
{
    struct itimerval it;
    memset(&it, 0, sizeof it);
    it.it_value.tv_sec = ms / 1000;
    it.it_value.tv_usec = (ms % 1000) * 1000;
    setitimer(ITIMER_REAL, &it, NULL);
}

static void disarm_timer(void) { arm_timer_ms(0); }

static void section_a(void)
{
    int ms_values[] = { 1, 2, 3, 5, 10, 20, 50 };
    int s = bound_udp(NULL);
    struct pollfd p = { .fd = s, .events = POLLIN };

    {
        int64_t lo = INT64_MAX, hi = 0;
        int rvs = 0;
        for (int i = 0; i < 20; i++)
        {
            p.revents = 0x7fff;
            int64_t t0 = now_us();
            int rv = poll(&p, 1, 0);
            int64_t dt = now_us() - t0;
            if (dt < lo) lo = dt;
            if (dt > hi) hi = dt;
            rvs |= rv;
        }
        printf("A timeout=0 rv|=%d revents=0x%x elapsed_us=[%lld,%lld]\n", rvs, p.revents, (long long)lo, (long long)hi);
    }

    for (size_t k = 0; k < sizeof ms_values / sizeof ms_values[0]; k++)
    {
        int ms = ms_values[k];
        int64_t lo = INT64_MAX, hi = 0;
        int nonzero = 0, early = 0;
        for (int i = 0; i < 20; i++)
        {
            p.revents = 0x7fff;
            int64_t t0 = now_us();
            int rv = poll(&p, 1, ms);
            int64_t dt = now_us() - t0;
            if (rv != 0 || p.revents != 0) nonzero++;
            if (dt < (int64_t)ms * 1000) early++;
            if (dt < lo) lo = dt;
            if (dt > hi) hi = dt;
        }
        printf("A timeout=%d nonzero_answers=%d before_deadline=%d elapsed_us=[%lld,%lld]\n", ms, nonzero, early,
               (long long)lo, (long long)hi);
    }
    close(s);
}

static void section_b(void)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_alarm; // no SA_RESTART
    sigaction(SIGALRM, &sa, NULL);

    int values[] = { -1, -2, -3, -1000, INT_MIN };
    int s = bound_udp(NULL);

    for (size_t k = 0; k < sizeof values / sizeof values[0]; k++)
    {
        int t = values[k];

        struct pollfd idle = { .fd = s, .events = POLLIN };
        arm_timer_ms(250);
        int64_t t0 = now_us();
        errno = 0;
        int rv = poll(&idle, 1, t);
        int e = errno;
        int64_t dt = now_us() - t0;
        disarm_timer();
        printf("B idle timeout=%d rv=%d errno=%d(%s) elapsed_us=%lld\n", t, rv, rv < 0 ? e : 0,
               rv < 0 ? strerror(e) : "-", (long long)dt);

        // stdout's pipe (or terminal) is writable: a ready entry.
        struct pollfd ready = { .fd = s, .events = POLLOUT };
        arm_timer_ms(250);
        t0 = now_us();
        errno = 0;
        rv = poll(&ready, 1, t);
        e = errno;
        dt = now_us() - t0;
        disarm_timer();
        printf("B ready timeout=%d rv=%d revents=0x%x errno=%d(%s) elapsed_us=%lld\n", t, rv, ready.revents,
               rv < 0 ? e : 0, rv < 0 ? strerror(e) : "-", (long long)dt);

        arm_timer_ms(250);
        t0 = now_us();
        errno = 0;
        rv = poll(NULL, 0, t);
        e = errno;
        dt = now_us() - t0;
        disarm_timer();
        printf("B nfds=0 timeout=%d rv=%d errno=%d(%s) elapsed_us=%lld\n", t, rv, rv < 0 ? e : 0,
               rv < 0 ? strerror(e) : "-", (long long)dt);
    }
    close(s);
}

struct later
{
    int delay_ms;
    int action;
    int fd;
    int dup_fd;
    struct sockaddr_in addr;
    int new_fd;
};

static void *act_later(void *arg)
{
    struct later *l = arg;
    usleep((useconds_t)l->delay_ms * 1000);
    switch (l->action)
    {
    case 0: // C, E5: send without closing
        send_to(&l->addr);
        break;
    case 1: // E1: close
        close(l->fd);
        break;
    case 2: // E2: close, reuse the number, make the new socket readable
    {
        close(l->fd);
        struct sockaddr_in fresh;
        l->new_fd = bound_udp(&fresh);
        send_to(&fresh);
        break;
    }
    case 3: // E3: close while a dup keeps the description
        close(l->fd);
        break;
    case 4: // E4: close, then send to the closed socket's address
        close(l->fd);
        usleep(20000);
        send_to(&l->addr);
        break;
    }
    return NULL;
}

static void wait_with(const char *label, int action, int timeout_ms, int dup_first)
{
    struct later l;
    memset(&l, 0, sizeof l);
    l.delay_ms = 50;
    l.action = action;
    l.new_fd = -1;
    l.fd = bound_udp(&l.addr);
    l.dup_fd = dup_first ? dup(l.fd) : -1;

    struct pollfd p = { .fd = l.fd, .events = POLLIN };
    pthread_t th;
    pthread_create(&th, NULL, act_later, &l);
    int64_t t0 = now_us();
    errno = 0;
    int rv = poll(&p, 1, timeout_ms);
    int e = errno;
    int64_t dt = now_us() - t0;
    pthread_join(th, NULL);
    printf("%s rv=%d revents=0x%x errno=%d(%s) elapsed_us=%lld polled_fd=%d new_fd=%d\n", label, rv, p.revents,
           rv < 0 ? e : 0, rv < 0 ? strerror(e) : "-", (long long)dt, p.fd, l.new_fd);

    if (action == 0) close(l.fd);
    if (l.new_fd >= 0) close(l.new_fd);
    if (l.dup_fd >= 0) close(l.dup_fd);
}

static void section_d(void)
{
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    struct rlimit low = { .rlim_cur = 64, .rlim_max = rl.rlim_max };
    setrlimit(RLIMIT_NOFILE, &low);

    struct pollfd good[1] = { { .fd = -1, .events = POLLIN } };
    void *bad = (void *)(uintptr_t)8;
    struct { const char *label; nfds_t n; void *buf; int timeout; } cases[] = {
        { "nfds>limit bad-buffer timeout=-2", 65, bad, -2 },
        { "nfds>limit bad-buffer timeout=0", 65, bad, 0 },
        { "nfds>limit good-buffer timeout=-2", 65, good, -2 },
        { "nfds>limit good-buffer timeout=0", 65, good, 0 },
        { "nfds=1 bad-buffer timeout=-2", 1, bad, -2 },
        { "nfds=1 bad-buffer timeout=0", 1, bad, 0 },
        { "nfds=1 good-buffer timeout=-2", 1, good, -2 },
        { "nfds=0 bad-buffer timeout=-2", 0, bad, -2 },
        { "nfds=0 bad-buffer timeout=0", 0, bad, 0 },
    };
    // A 65-entry buffer for the "good" rows above that name 65 entries.
    static struct pollfd many[65];
    for (int i = 0; i < 65; i++) { many[i].fd = -1; many[i].events = POLLIN; }

    for (size_t k = 0; k < sizeof cases / sizeof cases[0]; k++)
    {
        void *buf = cases[k].buf == good && cases[k].n == 65 ? (void *)many : cases[k].buf;
        arm_timer_ms(250);
        errno = 0;
        int rv = poll(buf, cases[k].n, cases[k].timeout);
        int e = errno;
        disarm_timer();
        printf("D %s rv=%d errno=%d(%s)\n", cases[k].label, rv, rv < 0 ? e : 0, rv < 0 ? strerror(e) : "-");
    }
    setrlimit(RLIMIT_NOFILE, &rl);
}

int main(void)
{
    alarm(60);
    setvbuf(stdout, NULL, _IONBF, 0);
    section_a();
    section_b();

    {
        struct sockaddr_in addr;
        int s = bound_udp(&addr);
        struct later l;
        memset(&l, 0, sizeof l);
        l.delay_ms = 50;
        l.action = 0;
        l.addr = addr;
        struct pollfd p = { .fd = s, .events = POLLIN };
        pthread_t th;
        pthread_create(&th, NULL, act_later, &l);
        int64_t t0 = now_us();
        int rv = poll(&p, 1, 2000);
        int64_t dt = now_us() - t0;
        pthread_join(th, NULL);
        printf("C rv=%d revents=0x%x elapsed_us=%lld\n", rv, p.revents, (long long)dt);
        close(s);
    }

    section_d();

    wait_with("E1 close", 1, 300, 0);
    wait_with("E2 close+reuse+ready", 2, 300, 0);
    wait_with("E3 close-with-dup", 3, 300, 1);
    wait_with("E4 close+send-to-old", 4, 300, 0);
    wait_with("E5 control send", 0, 300, 0);
    return 0;
}
