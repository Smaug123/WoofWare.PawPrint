// epoll_create1(2)'s flag screen, and epoll_wait(2)'s timeout, argument order
// and wake order.
//
// Every wait below is on an epoll instance holding one bound UDP socket
// registered EPOLLIN|EPOLLET, which has nothing to report until a datagram
// arrives. Elapsed times are CLOCK_MONOTONIC, in microseconds.
//
// Sections:
//   A  epoll_create1 with flags 0, every single bit 0..31, EPOLL_CLOEXEC with
//      every other single bit, -1 and INT_MIN: rv/errno, and on success
//      FD_CLOEXEC, the status flags, and whether the lowest free fd came back.
//   B  expiry: timeout 0 (20 waits), and 1, 2, 3, 5, 10, 20 and 50 ms (20
//      waits each): rv, how many returned before the deadline, least and
//      greatest elapsed.
//   C  negative timeouts -1, -2, -3, -1000 and INT_MIN, each cut short by a
//      250 ms interval timer whose SIGALRM handler does not restart: EINTR at
//      ~250 ms means "infinite". Each is asked of a ready port too, and of a
//      port with nothing registered.
//   D  a datagram sent by another thread 50 ms into a 2000 ms wait.
//   E  readiness and the deadline both holding when the waiter next runs: the
//      waiter and a hog share CPU 0 under SCHED_FIFO, the hog at the higher
//      priority, and the hog spins until 40 ms past the start of a 10 ms wait.
//      E1 the datagram is sent 5 ms in (before the deadline); E2 35 ms in
//      (after it). 20 trials each; rv tallied.
//   F  several threads waiting on one port, maxevents 1 unless stated:
//      F1 three threads park 30 ms apart in the order given, then three
//         datagrams are sent 100 ms apart (each drained by the sender before
//         the next): which thread returns after each; 10 trials of each of
//         the six orders.
//      F2 two threads that wait again as soon as they return: the order of
//         six returns, 20 trials.
//      F3 two threads parked, maxevents 4, two sockets registered, and one
//         datagram sent to each back to back: how many threads return and
//         with how many events, 50 trials.
//      F4 two threads parked, the same socket registered through two
//         descriptors (a dup), one datagram: how many threads return, 50
//         trials.
//   G  argument order with a negative maxevents: bad fd, kernel-range buffer,
//      a descriptor that is not a port; -1 and INT_MIN.
//
// Build and run, from this directory (as root with CAP_SYS_NICE, which
// section E's SCHED_FIFO needs; without it E reports the failure and moves on):
//   Linux:  container run --rm --cap-add CAP_SYS_NICE -v "$PWD:/probe" gcc:14 sh -c
//             "gcc -Wall -O1 -pthread -o /tmp/ew /probe/epoll-wait.c && /tmp/ew"
//
// Darwin has no epoll.
//
// Measured 2026-09-27 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14
// image, glibc 2.41), twice with the same answers:
//   A  flags 0 and EPOLL_CLOEXEC (0x80000, which is O_CLOEXEC) create a port on
//      the lowest free descriptor, O_RDWR, with FD_CLOEXEC set exactly for
//      EPOLL_CLOEXEC. Every other single bit, EPOLL_CLOEXEC with any other
//      bit, -1 and INT_MIN are EINVAL.
//   B  timeout 0 returned 0 within 1 us every time. No positive-timeout wait
//      returned before its deadline, and every one returned 0: least/greatest
//      elapsed for 1 ms 1206/1350 us, for 50 ms 50167/55168 us.
//   C  every negative timeout is infinite (EINTR at ~250 ms), on a port whose
//      registration is idle and on a port with nothing registered; a ready
//      port answers 1 at once whatever the negative timeout.
//   D  rv 1, the registration's data and EPOLLIN, after ~53 ms.
//   E  E1 and E2 each returned 1 in 20 of 20 trials, the waiter returning
//      ~40 ms in: when readiness and the expired deadline both hold as the
//      waiter next runs, it reports the event rather than 0.
//   F  F1: in all 60 trials (10 of each of the six park orders) each datagram
//      woke exactly one thread, and it was the thread that parked *last*:
//      the order of returns was the reverse of the order of parking.
//      F2: all 20 trials returned 111111 -- a thread that waits again goes
//      back to the front, and so wins every time.
//      F3: in all 50 trials the last-parked thread returned once with both
//      events and the other never returned.
//      F4: in all 50 trials both threads returned, one event each.
//   G  a negative maxevents is screened as zero is: EBADF for a bad fd
//      (whatever the buffer), then EINVAL, ahead of a kernel-range buffer's
//      EFAULT and of a socket's "not a port" EINVAL. A kernel-range buffer
//      with maxevents 1 is EFAULT. A ready port answers 1 under timeout
//      INT_MIN.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
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

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
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

static void drain_socket(int s)
{
    char buf[16];
    while (recv(s, buf, sizeof buf, MSG_DONTWAIT) > 0) { }
}

static int port_with(int s, uint64_t data)
{
    int ep = epoll_create1(0);
    if (ep < 0) { perror("epoll_create1"); exit(1); }
    struct epoll_event ev = { .events = EPOLLIN | EPOLLET, .data.u64 = data };
    if (epoll_ctl(ep, EPOLL_CTL_ADD, s, &ev) != 0) { perror("epoll_ctl"); exit(1); }
    return ep;
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

// ---- A ------------------------------------------------------------------

static int lowest_free(void)
{
    int fd = dup(0);
    close(fd);
    return fd;
}

static void create_row(const char *label, int flags)
{
    int expected = lowest_free();
    errno = 0;
    int ep = epoll_create1(flags);
    int e = errno;
    if (ep < 0)
    {
        printf("A %s flags=0x%x rv=-1 errno=%d(%s)\n", label, (unsigned)flags, e, strerror(e));
        return;
    }
    int fdflags = fcntl(ep, F_GETFD);
    int flflags = fcntl(ep, F_GETFL);
    printf("A %s flags=0x%x rv=ok lowest=%s FD_CLOEXEC=%d F_GETFL=0x%x\n", label, (unsigned)flags,
           ep == expected ? "yes" : "no", (fdflags & FD_CLOEXEC) ? 1 : 0, (unsigned)flflags);
    close(ep);
}

static void section_a(void)
{
    printf("A EPOLL_CLOEXEC=0x%x O_CLOEXEC=0x%x\n", (unsigned)EPOLL_CLOEXEC, (unsigned)O_CLOEXEC);
    create_row("zero", 0);
    for (int bit = 0; bit < 32; bit++) create_row("bit", (int)(1u << bit));
    for (int bit = 0; bit < 32; bit++)
    {
        if ((1u << bit) == (unsigned)EPOLL_CLOEXEC) continue;
        create_row("cloexec|bit", (int)(EPOLL_CLOEXEC | (1u << bit)));
    }
    create_row("minus-one", -1);
    create_row("int-min", INT_MIN);
}

// ---- B ------------------------------------------------------------------

static void section_b(void)
{
    int ms_values[] = { 1, 2, 3, 5, 10, 20, 50 };
    int s = bound_udp(NULL);
    int ep = port_with(s, 1);
    struct epoll_event out[4];

    {
        int64_t lo = INT64_MAX, hi = 0;
        int nonzero = 0;
        for (int i = 0; i < 20; i++)
        {
            int64_t t0 = now_us();
            int rv = epoll_wait(ep, out, 4, 0);
            int64_t dt = now_us() - t0;
            if (rv != 0) nonzero++;
            if (dt < lo) lo = dt;
            if (dt > hi) hi = dt;
        }
        printf("B timeout=0 nonzero=%d elapsed_us=[%lld,%lld]\n", nonzero, (long long)lo, (long long)hi);
    }

    for (size_t k = 0; k < sizeof ms_values / sizeof ms_values[0]; k++)
    {
        int ms = ms_values[k];
        int64_t lo = INT64_MAX, hi = 0;
        int nonzero = 0, early = 0;
        for (int i = 0; i < 20; i++)
        {
            int64_t t0 = now_us();
            int rv = epoll_wait(ep, out, 4, ms);
            int64_t dt = now_us() - t0;
            if (rv != 0) nonzero++;
            if (dt < (int64_t)ms * 1000) early++;
            if (dt < lo) lo = dt;
            if (dt > hi) hi = dt;
        }
        printf("B timeout=%d nonzero=%d before_deadline=%d elapsed_us=[%lld,%lld]\n", ms, nonzero, early,
               (long long)lo, (long long)hi);
    }
    close(ep);
    close(s);
}

// ---- C ------------------------------------------------------------------

static void negative_row(const char *label, int ep, int timeout)
{
    struct epoll_event out[4];
    arm_timer_ms(250);
    int64_t t0 = now_us();
    errno = 0;
    int rv = epoll_wait(ep, out, 4, timeout);
    int e = errno;
    int64_t dt = now_us() - t0;
    arm_timer_ms(0);
    printf("C %s timeout=%d rv=%d errno=%d(%s) elapsed_us=%lld\n", label, timeout, rv, rv < 0 ? e : 0,
           rv < 0 ? strerror(e) : "-", (long long)dt);
}

static void section_c(void)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_alarm; // no SA_RESTART
    sigaction(SIGALRM, &sa, NULL);

    int timeouts[] = { -1, -2, -3, -1000, INT_MIN };
    struct sockaddr_in addr;
    int s = bound_udp(&addr);
    int ep = port_with(s, 1);
    int empty = epoll_create1(0);

    for (size_t k = 0; k < sizeof timeouts / sizeof timeouts[0]; k++)
    {
        negative_row("idle", ep, timeouts[k]);
        send_to(&addr);
        sleep_ms(5);
        negative_row("ready", ep, timeouts[k]);
        drain_socket(s);
        negative_row("unregistered", empty, timeouts[k]);
    }
    close(empty);
    close(ep);
    close(s);
}

// ---- D ------------------------------------------------------------------

struct delayed_send { struct sockaddr_in addr; int ms; };

static void *send_later(void *p)
{
    struct delayed_send *d = p;
    sleep_ms(d->ms);
    send_to(&d->addr);
    return NULL;
}

static void section_d(void)
{
    struct delayed_send d;
    int s = bound_udp(&d.addr);
    int ep = port_with(s, 7);
    d.ms = 50;
    pthread_t t;
    pthread_create(&t, NULL, send_later, &d);
    struct epoll_event out[4];
    int64_t t0 = now_us();
    int rv = epoll_wait(ep, out, 4, 2000);
    int64_t dt = now_us() - t0;
    pthread_join(t, NULL);
    printf("D rv=%d data=%llu events=0x%x elapsed_us=%lld\n", rv, rv > 0 ? (unsigned long long)out[0].data.u64 : 0ULL,
           rv > 0 ? out[0].events : 0u, (long long)dt);
    close(ep);
    close(s);
}

// ---- E ------------------------------------------------------------------

static int pin_fifo(int prio)
{
    cpu_set_t set;
    CPU_ZERO(&set);
    CPU_SET(0, &set);
    if (pthread_setaffinity_np(pthread_self(), sizeof set, &set) != 0) return -1;
    struct sched_param sp = { .sched_priority = prio };
    return pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp);
}

struct e_wait { int ep; int rv; int64_t started; int64_t returned; atomic_int parked; };

static void *e_waiter(void *p)
{
    struct e_wait *w = p;
    int pinned = pin_fifo(10);
    if (pinned != 0)
    {
        w->rv = -100 - pinned;
        atomic_store(&w->parked, 1);
        return NULL;
    }
    struct epoll_event out[4];
    w->started = now_us();
    atomic_store(&w->parked, 1);
    w->rv = epoll_wait(w->ep, out, 4, 10);
    w->returned = now_us();
    return NULL;
}

static void section_e(void)
{
    for (int variant = 1; variant <= 2; variant++)
    {
        int send_at_ms = variant == 1 ? 5 : 35;
        int tally[3] = { 0, 0, 0 };
        int failures = 0;
        int64_t latest_start_to_return = 0;
        for (int trial = 0; trial < 20; trial++)
        {
            struct sockaddr_in addr;
            int s = bound_udp(&addr);
            int ep = port_with(s, 3);
            struct e_wait w = { .ep = ep, .rv = -99 };
            atomic_init(&w.parked, 0);

            // The hog is this thread, at a higher FIFO priority on the same CPU.
            pthread_t t;
            pthread_create(&t, NULL, e_waiter, &w);
            while (!atomic_load(&w.parked)) { }
            if (w.rv <= -100)
            {
                printf("E%d waiter could not take SCHED_FIFO on CPU 0: error %d\n", variant, -100 - w.rv);
                pthread_join(t, NULL);
                close(ep);
                close(s);
                return;
            }
            sleep_ms(2); // let the waiter reach its sleep
            int hog = pin_fifo(20);
            if (hog != 0)
            {
                printf("E%d hog could not take SCHED_FIFO on CPU 0: error %d\n", variant, hog);
                pthread_join(t, NULL);
                close(ep);
                close(s);
                return;
            }
            int64_t t0 = w.started;
            int sent = 0;
            while (now_us() - t0 < 40000)
            {
                if (!sent && now_us() - t0 >= send_at_ms * 1000) { send_to(&addr); sent = 1; }
            }
            // Back to normal scheduling, so the waiter can run.
            struct sched_param sp = { .sched_priority = 0 };
            pthread_setschedparam(pthread_self(), SCHED_OTHER, &sp);
            pthread_join(t, NULL);
            if (w.rv >= 0 && w.rv <= 2) tally[w.rv]++;
            else failures++;
            if (w.returned - w.started > latest_start_to_return) latest_start_to_return = w.returned - w.started;
            close(ep);
            close(s);
        }
        printf("E%d send_at_ms=%d rv0=%d rv1=%d failures=%d max_elapsed_us=%lld\n", variant, send_at_ms, tally[0],
               tally[1], failures, (long long)latest_start_to_return);
    }
}

// ---- F ------------------------------------------------------------------

struct f_shared
{
    int ep;
    int maxevents;
    int loop;          // wait again after returning
    atomic_int log[64];
    atomic_int log_len;
    atomic_int events_of[8];
    atomic_int returns_of[8];
    atomic_int parked;
    atomic_int stop;
};

struct f_arg { struct f_shared *sh; int id; };

static void *f_waiter(void *p)
{
    struct f_arg *a = p;
    struct f_shared *sh = a->sh;
    struct epoll_event out[8];
    do
    {
        atomic_fetch_add(&sh->parked, 1);
        int rv = epoll_wait(sh->ep, out, sh->maxevents, 1500);
        if (atomic_load(&sh->stop)) break;
        if (rv > 0)
        {
            int at = atomic_fetch_add(&sh->log_len, 1);
            if (at < 64) atomic_store(&sh->log[at], a->id);
            atomic_fetch_add(&sh->events_of[a->id], rv);
            atomic_fetch_add(&sh->returns_of[a->id], 1);
        }
        else
        {
            break;
        }
    } while (sh->loop);
    return NULL;
}

static void reset_shared(struct f_shared *sh, int ep, int maxevents, int loop)
{
    memset(sh, 0, sizeof *sh);
    sh->ep = ep;
    sh->maxevents = maxevents;
    sh->loop = loop;
}

static void section_f1(void)
{
    int orders[6][3] = { { 0, 1, 2 }, { 0, 2, 1 }, { 1, 0, 2 }, { 1, 2, 0 }, { 2, 0, 1 }, { 2, 1, 0 } };
    for (int o = 0; o < 6; o++)
    {
        int fifo = 0, lifo = 0, other = 0;
        char first_other[128] = "";
        for (int trial = 0; trial < 10; trial++)
        {
            struct sockaddr_in addr;
            int s = bound_udp(&addr);
            int ep = port_with(s, 1);
            struct f_shared sh;
            reset_shared(&sh, ep, 1, 0);
            pthread_t t[3];
            struct f_arg args[3];
            for (int k = 0; k < 3; k++)
            {
                int id = orders[o][k];
                args[k].sh = &sh;
                args[k].id = id;
                pthread_create(&t[k], NULL, f_waiter, &args[k]);
                sleep_ms(30);
            }
            char seen[64] = "";
            for (int d = 0; d < 3; d++)
            {
                int before = atomic_load(&sh.log_len);
                send_to(&addr);
                sleep_ms(100);
                drain_socket(s);
                int after = atomic_load(&sh.log_len);
                char step[16];
                snprintf(step, sizeof step, "%d:", after - before);
                strcat(seen, step);
                for (int i = before; i < after && i < 64; i++)
                {
                    snprintf(step, sizeof step, "%d", atomic_load(&sh.log[i]));
                    strcat(seen, step);
                }
                strcat(seen, " ");
            }
            char in_order[64], reversed[64];
            snprintf(in_order, sizeof in_order, "1:%d 1:%d 1:%d ", orders[o][0], orders[o][1], orders[o][2]);
            snprintf(reversed, sizeof reversed, "1:%d 1:%d 1:%d ", orders[o][2], orders[o][1], orders[o][0]);
            if (strcmp(seen, in_order) == 0) fifo++;
            else if (strcmp(seen, reversed) == 0) lifo++;
            else
            {
                other++;
                if (!first_other[0]) snprintf(first_other, sizeof first_other, "%s", seen);
            }
            atomic_store(&sh.stop, 1);
            for (int k = 0; k < 3; k++) pthread_join(t[k], NULL);
            close(ep);
            close(s);
        }
        printf("F1 park_order=%d,%d,%d one_per_datagram_earliest_first=%d latest_first=%d other=%d first_other=[%s]\n",
               orders[o][0], orders[o][1], orders[o][2], fifo, lifo, other, first_other);
    }
}

static void section_f2(void)
{
    int alternating = 0, latest_every_time = 0, other = 0;
    char first_other[64] = "";
    for (int trial = 0; trial < 20; trial++)
    {
        struct sockaddr_in addr;
        int s = bound_udp(&addr);
        int ep = port_with(s, 1);
        struct f_shared sh;
        reset_shared(&sh, ep, 1, 1);
        pthread_t t[2];
        struct f_arg args[2];
        for (int k = 0; k < 2; k++)
        {
            args[k].sh = &sh;
            args[k].id = k;
            pthread_create(&t[k], NULL, f_waiter, &args[k]);
            sleep_ms(30);
        }
        for (int d = 0; d < 6; d++)
        {
            send_to(&addr);
            sleep_ms(60);
            drain_socket(s);
        }
        char seen[32] = "";
        int n = atomic_load(&sh.log_len);
        for (int i = 0; i < n && i < 30; i++)
        {
            char step[4];
            snprintf(step, sizeof step, "%d", atomic_load(&sh.log[i]));
            strcat(seen, step);
        }
        if (strcmp(seen, "010101") == 0) alternating++;
        else if (strcmp(seen, "111111") == 0) latest_every_time++;
        else
        {
            other++;
            if (!first_other[0]) snprintf(first_other, sizeof first_other, "%s", seen);
        }
        atomic_store(&sh.stop, 1);
        // Wake both so they see `stop`.
        send_to(&addr);
        sleep_ms(20);
        send_to(&addr);
        for (int k = 0; k < 2; k++) pthread_join(t[k], NULL);
        close(ep);
        close(s);
    }
    printf("F2 returns_alternate_010101=%d latest_parked_every_time_111111=%d other=%d first_other=[%s]\n", alternating,
           latest_every_time, other, first_other);
}

static void section_f3(void)
{
    // Tally of outcomes: "which threads returned, with how many events each".
    char outcomes[8][32];
    int counts[8];
    int n_outcomes = 0;
    for (int trial = 0; trial < 50; trial++)
    {
        struct sockaddr_in a1, a2;
        int s1 = bound_udp(&a1);
        int s2 = bound_udp(&a2);
        int ep = port_with(s1, 1);
        struct epoll_event ev = { .events = EPOLLIN | EPOLLET, .data.u64 = 2 };
        epoll_ctl(ep, EPOLL_CTL_ADD, s2, &ev);
        struct f_shared sh;
        reset_shared(&sh, ep, 4, 0);
        pthread_t t[2];
        struct f_arg args[2];
        for (int k = 0; k < 2; k++)
        {
            args[k].sh = &sh;
            args[k].id = k;
            pthread_create(&t[k], NULL, f_waiter, &args[k]);
            sleep_ms(30);
        }
        send_to(&a1);
        send_to(&a2);
        sleep_ms(100);
        char outcome[32];
        snprintf(outcome, sizeof outcome, "w0:%d/%d w1:%d/%d", atomic_load(&sh.returns_of[0]),
                 atomic_load(&sh.events_of[0]), atomic_load(&sh.returns_of[1]), atomic_load(&sh.events_of[1]));
        int found = -1;
        for (int i = 0; i < n_outcomes; i++)
            if (strcmp(outcomes[i], outcome) == 0) found = i;
        if (found < 0 && n_outcomes < 8)
        {
            snprintf(outcomes[n_outcomes], sizeof outcomes[n_outcomes], "%s", outcome);
            counts[n_outcomes] = 0;
            found = n_outcomes++;
        }
        if (found >= 0) counts[found]++;
        atomic_store(&sh.stop, 1);
        drain_socket(s1);
        drain_socket(s2);
        send_to(&a1);
        sleep_ms(20);
        send_to(&a2);
        for (int k = 0; k < 2; k++) pthread_join(t[k], NULL);
        close(ep);
        close(s1);
        close(s2);
    }
    for (int i = 0; i < n_outcomes; i++) printf("F3 returns/events %s trials=%d\n", outcomes[i], counts[i]);
}

static void section_f4(void)
{
    char outcomes[8][32];
    int counts[8];
    int n_outcomes = 0;
    for (int trial = 0; trial < 50; trial++)
    {
        struct sockaddr_in addr;
        int s = bound_udp(&addr);
        int s_dup = dup(s);
        int ep = port_with(s, 1);
        struct epoll_event ev = { .events = EPOLLIN | EPOLLET, .data.u64 = 2 };
        epoll_ctl(ep, EPOLL_CTL_ADD, s_dup, &ev);
        struct f_shared sh;
        reset_shared(&sh, ep, 1, 0);
        pthread_t t[2];
        struct f_arg args[2];
        for (int k = 0; k < 2; k++)
        {
            args[k].sh = &sh;
            args[k].id = k;
            pthread_create(&t[k], NULL, f_waiter, &args[k]);
            sleep_ms(30);
        }
        send_to(&addr);
        sleep_ms(100);
        char outcome[32];
        snprintf(outcome, sizeof outcome, "w0:%d/%d w1:%d/%d", atomic_load(&sh.returns_of[0]),
                 atomic_load(&sh.events_of[0]), atomic_load(&sh.returns_of[1]), atomic_load(&sh.events_of[1]));
        int found = -1;
        for (int i = 0; i < n_outcomes; i++)
            if (strcmp(outcomes[i], outcome) == 0) found = i;
        if (found < 0 && n_outcomes < 8)
        {
            snprintf(outcomes[n_outcomes], sizeof outcomes[n_outcomes], "%s", outcome);
            counts[n_outcomes] = 0;
            found = n_outcomes++;
        }
        if (found >= 0) counts[found]++;
        atomic_store(&sh.stop, 1);
        drain_socket(s);
        send_to(&addr);
        sleep_ms(20);
        drain_socket(s);
        send_to(&addr);
        for (int k = 0; k < 2; k++) pthread_join(t[k], NULL);
        close(ep);
        close(s_dup);
        close(s);
    }
    for (int i = 0; i < n_outcomes; i++) printf("F4 returns/events %s trials=%d\n", outcomes[i], counts[i]);
}

// ---- G ------------------------------------------------------------------

static void g_row(const char *label, int fd, void *buf, int maxevents, int timeout)
{
    errno = 0;
    int rv = epoll_wait(fd, buf, maxevents, timeout);
    int e = errno;
    printf("G %s maxevents=%d timeout=%d rv=%d errno=%d(%s)\n", label, maxevents, timeout, rv, rv < 0 ? e : 0,
           rv < 0 ? strerror(e) : "-");
}

static void section_g(void)
{
    struct sockaddr_in addr;
    int s = bound_udp(&addr);
    int ep = port_with(s, 1);
    struct epoll_event out[4];
    void *kernel = (void *)(uintptr_t)0xffff800000000000ULL;
    int negatives[] = { -1, INT_MIN };
    for (int k = 0; k < 2; k++)
    {
        int m = negatives[k];
        g_row("bad-fd", 999, out, m, 0);
        g_row("bad-fd+kernel-buffer", 999, kernel, m, 0);
        g_row("port+kernel-buffer", ep, kernel, m, 0);
        g_row("socket+kernel-buffer", s, kernel, m, 0);
        g_row("socket", s, out, m, 0);
        g_row("port", ep, out, m, 0);
    }
    g_row("port+kernel-buffer", ep, kernel, 1, 0);
    send_to(&addr);
    sleep_ms(5);
    g_row("ready-port timeout INT_MIN", ep, out, 4, INT_MIN);
    close(ep);
    close(s);
}

// A watchdog thread rather than alarm(2): section C's interval timer shares
// ITIMER_REAL with alarm, and its SIGALRM handler does nothing.
static void *watchdog(void *p)
{
    (void)p;
    // So that section C's SIGALRM interrupts the main thread's wait, not this.
    sigset_t alrm;
    sigemptyset(&alrm);
    sigaddset(&alrm, SIGALRM);
    pthread_sigmask(SIG_BLOCK, &alrm, NULL);
    sleep_ms(180000);
    fprintf(stderr, "watchdog: probe still running after 180 s\n");
    _exit(3);
}

int main(void)
{
    setvbuf(stdout, NULL, _IONBF, 0);
    pthread_t dog;
    pthread_create(&dog, NULL, watchdog, NULL);
    section_a();
    section_b();
    section_c();
    section_d();
    section_e();
    section_f1();
    section_f2();
    section_f3();
    section_f4();
    section_g();
    return 0;
}
