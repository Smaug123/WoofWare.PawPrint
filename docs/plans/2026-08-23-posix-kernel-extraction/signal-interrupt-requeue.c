// What a caught signal does to a thread asleep in a syscall, beyond the
// EINTR-or-restart rows `signal-sigaction-flags.c` measured: a poll with
// nothing to watch, where an interrupted waiter stands in its queue afterwards,
// and what a waiter that is both ready and signalled answers.
//
// Every signal is SIGUSR1, sent by pthread_kill to the sleeping thread. Its
// handler does nothing; "restart" means it was installed with SA_RESTART, and
// "loop" means it was not and the sleeping thread calls again on EINTR, as a
// wrapper that retries EINTR does. Elapsed times are CLOCK_MONOTONIC.
//
// Sections:
//   A  poll(NULL, 0, -1), signalled 50 ms in, under "restart" and without it:
//      rv, errno and elapsed, 5 trials each.
//   B  three threads park in a blocking accept on one listener, 30 ms apart
//      in the order 0, 1, 2; 50 ms later thread k is signalled, under
//      "restart" and under "loop"; 50 ms after that three connects 100 ms
//      apart: the order the threads return in. 10 trials of each k and mode.
//   C  the same with a wait on a socket event port (epoll_wait on Linux,
//      kevent on Darwin) holding one UDP socket (EPOLLIN|EPOLLET, or
//      EVFILT_READ with EV_CLEAR), under "loop", and three datagrams 100 ms
//      apart, each drained by the sender before the next: the order of
//      returns. 10 trials of each k.
//   D  (Linux only; needs CAP_SYS_NICE) the sleeper and a hog share CPU 0
//      under SCHED_FIFO, the hog at the higher priority, so that both the
//      call's own condition and the signal hold before the sleeper next runs.
//      For epoll_wait(-1), poll(-1), accept and flock (LOCK_EX, against a
//      lock the hog holds and releases), under "loop" with no loop
//      (the first answer is recorded): the readiness is made 5 ms into the
//      hog and the signal sent 10 ms in (D-ready-first), or the other way
//      round (D-signal-first); the hog spins to 40 ms. 20 trials each:
//      how many answered with the readiness and how many with EINTR.
//   E  (Linux only, as D) the deadline and the signal: epoll_wait and poll
//      with a 10 ms timeout on a socket that never becomes ready, signalled
//      5 ms in (before the deadline, E-signal-first) or 20 ms in (after it,
//      E-deadline-first), the hog spinning to 40 ms. 20 trials each: how many
//      answered 0 and how many EINTR.
//   F  readiness and a signal both arriving while the whole process is
//      stopped, for a flavour with no SCHED_FIFO to hold the sleeper off the
//      CPU: a thread sleeps in accept (on a listener) or flock (LOCK_EX, on a
//      file another process holds locked); a helper process sends the
//      sleeper's process SIGSTOP, then makes the call's condition hold
//      (connects, or unlocks) and sends SIGUSR1 to the process, in either
//      order, then SIGCONT. Controls make the condition hold alone and send
//      the signal alone. 10 trials each: how many completed and how many
//      answered EINTR.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -pthread -o /tmp/sir signal-interrupt-requeue.c && /tmp/sir
//   Linux:  container run --rm --cap-add CAP_SYS_NICE -v "$PWD:/probe" gcc:14 sh -c
//             "gcc -Wall -O1 -pthread -o /tmp/sir /probe/signal-interrupt-requeue.c && /tmp/sir"
//
// Measured 2026-10-01 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14
// image, glibc 2.41) and Darwin 27.0.0 arm64, at least twice each, with the
// same answers every time except where stated:
//   A  both: EINTR at ~50 ms, with and without SA_RESTART, every trial.
//   B  both: in all 60 trials of each flavour the signalled accepter returned
//      *last*, the other two in the order they parked (signalling 0 gave
//      "120", 1 gave "021", 2 gave "012"), whether the kernel restarted the
//      call (SA_RESTART, no EINTR seen) or the thread called again on EINTR:
//      either way the waiter goes to the back of the listener's queue.
//   C  Linux: in all 30 trials the signalled waiter returned *first*, and the
//      other two in the reverse of the order they parked (0 gave "021", 1
//      "120", 2 "210"): the call it makes again on EINTR queues it at the
//      front, as any new epoll_wait does. Darwin: no fixed order in either
//      run (signalling 0 gave 120, 102, 210 and 201), so a kevent wait is not
//      a LIFO queue there, signalled or not.
//   D  Linux: every one of the 160 trials answered with the readiness, never
//      EINTR, whichever of the readiness and the signal came first: a sleeper
//      whose condition holds when it runs finishes the call, and the signal
//      is delivered as it returns.
//   E  Linux: epoll_wait answered 0 in all 40 trials and poll EINTR in all 40,
//      whichever of the deadline and the signal came first: when both hold as
//      the sleeper runs, epoll_wait's deadline beats the signal, and poll's
//      signal beats the deadline.
//   F  Darwin: whichever reached the sleeper first decided, in every trial
//      (10 of each shape and call in each run): readiness then the signal
//      completed the call, the signal then readiness answered EINTR. The controls behaved (readiness alone
//      completed, the signal alone EINTR), so the stop holds the sleeper off
//      without waking it. Linux: the stop itself interrupts the sleep, so
//      every shape with a signal answered EINTR and readiness alone completed
//      (the stop's interruption is restarted, there being no handler); D is
//      the measurement for Linux.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

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

static void on_usr1(int sig) { (void)sig; }

static void install(int restart)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sa.sa_flags = restart ? SA_RESTART : 0;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGUSR1, &sa, NULL) != 0) { perror("sigaction"); exit(1); }
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
    *addr = a;
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

static int listener(struct sockaddr_in *addr)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0) { perror("socket"); exit(1); }
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(s, (struct sockaddr *)&a, sizeof a) != 0) { perror("bind"); exit(1); }
    if (listen(s, 16) != 0) { perror("listen"); exit(1); }
    socklen_t len = sizeof a;
    getsockname(s, (struct sockaddr *)&a, &len);
    *addr = a;
    return s;
}

static int connect_to(const struct sockaddr_in *addr)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(c, (const struct sockaddr *)addr, sizeof *addr) != 0) perror("connect");
    return c;
}

static int make_port(int s)
{
#ifdef __linux__
    int ep = epoll_create1(0);
    struct epoll_event ev = { .events = EPOLLIN | EPOLLET, .data.u64 = 1 };
    if (epoll_ctl(ep, EPOLL_CTL_ADD, s, &ev) != 0) { perror("epoll_ctl"); exit(1); }
    return ep;
#else
    int kq = kqueue();
    struct kevent ev;
    EV_SET(&ev, s, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
    if (kevent(kq, &ev, 1, NULL, 0, NULL) != 0) { perror("kevent"); exit(1); }
    return kq;
#endif
}

// One wait on the port, for ever: rv, with errno in *error.
static int port_wait(int port, int *error)
{
#ifdef __linux__
    struct epoll_event out[1];
    int rv = epoll_wait(port, out, 1, -1);
#else
    struct kevent out[1];
    int rv = kevent(port, NULL, 0, out, 1, NULL);
#endif
    *error = errno;
    return rv;
}

// ---- A ------------------------------------------------------------------

struct a_arg { pthread_t target; };

static void *a_signal_later(void *p)
{
    struct a_arg *a = p;
    sleep_ms(50);
    pthread_kill(a->target, SIGUSR1);
    return NULL;
}

static void section_a(void)
{
    for (int restart = 0; restart <= 1; restart++)
    {
        install(restart);
        for (int trial = 0; trial < 5; trial++)
        {
            struct a_arg a = { pthread_self() };
            pthread_t t;
            pthread_create(&t, NULL, a_signal_later, &a);
            int64_t t0 = now_us();
            errno = 0;
            int rv = poll(NULL, 0, -1);
            int e = errno;
            int64_t dt = now_us() - t0;
            pthread_join(t, NULL);
            printf("A SA_RESTART=%d rv=%d errno=%d(%s) elapsed_ms=%lld\n", restart, rv, rv < 0 ? e : 0,
                   rv < 0 ? strerror(e) : "-", (long long)(dt / 1000));
        }
    }
}

// ---- B and C ------------------------------------------------------------

struct q_shared
{
    int fd;          // the listener (B) or the port (C)
    int loop;        // call again on EINTR
    atomic_int log[8];
    atomic_int log_len;
    atomic_int eintr_of[3];
};

struct q_arg { struct q_shared *sh; int id; };

static void *b_accepter(void *p)
{
    struct q_arg *a = p;
    for (;;)
    {
        int c = accept(a->sh->fd, NULL, NULL);
        if (c >= 0)
        {
            int at = atomic_fetch_add(&a->sh->log_len, 1);
            if (at < 8) atomic_store(&a->sh->log[at], a->id);
            close(c);
            return NULL;
        }
        if (errno == EINTR)
        {
            atomic_fetch_add(&a->sh->eintr_of[a->id], 1);
            if (a->sh->loop) continue;
        }
        return NULL;
    }
}

static void *c_waiter(void *p)
{
    struct q_arg *a = p;
    for (;;)
    {
        int e = 0;
        int rv = port_wait(a->sh->fd, &e);
        if (rv > 0)
        {
            int at = atomic_fetch_add(&a->sh->log_len, 1);
            if (at < 8) atomic_store(&a->sh->log[at], a->id);
            return NULL;
        }
        if (rv < 0 && e == EINTR)
        {
            atomic_fetch_add(&a->sh->eintr_of[a->id], 1);
            if (a->sh->loop) continue;
        }
        return NULL;
    }
}

static void log_string(struct q_shared *sh, char *out, size_t size)
{
    out[0] = 0;
    int n = atomic_load(&sh->log_len);
    for (int i = 0; i < n && i < 8; i++)
    {
        char step[4];
        snprintf(step, sizeof step, "%d", atomic_load(&sh->log[i]));
        strncat(out, step, size - strlen(out) - 1);
    }
}

static void section_b(void)
{
    for (int mode = 0; mode <= 1; mode++)
    {
        // mode 0: "restart"; mode 1: "loop".
        install(mode == 0);
        for (int k = 0; k < 3; k++)
        {
            char tally_seen[10][8];
            int n_seen = 0;
            int tally_count[10] = { 0 };
            int interrupted = 0;
            for (int trial = 0; trial < 10; trial++)
            {
                struct sockaddr_in addr;
                struct q_shared sh;
                memset(&sh, 0, sizeof sh);
                sh.fd = listener(&addr);
                sh.loop = mode == 1;
                pthread_t t[3];
                struct q_arg args[3];
                for (int i = 0; i < 3; i++)
                {
                    args[i].sh = &sh;
                    args[i].id = i;
                    pthread_create(&t[i], NULL, b_accepter, &args[i]);
                    sleep_ms(30);
                }
                sleep_ms(50);
                pthread_kill(t[k], SIGUSR1);
                sleep_ms(50);
                int cs[3];
                for (int i = 0; i < 3; i++)
                {
                    cs[i] = connect_to(&addr);
                    sleep_ms(100);
                }
                for (int i = 0; i < 3; i++) pthread_join(t[i], NULL);
                for (int i = 0; i < 3; i++) close(cs[i]);
                close(sh.fd);
                interrupted += atomic_load(&sh.eintr_of[k]);
                char seen[8];
                log_string(&sh, seen, sizeof seen);
                int found = -1;
                for (int i = 0; i < n_seen; i++)
                    if (strcmp(tally_seen[i], seen) == 0) found = i;
                if (found < 0) { found = n_seen++; snprintf(tally_seen[found], 8, "%s", seen); }
                tally_count[found]++;
            }
            printf("B mode=%s signalled=%d eintr_seen_by_signalled=%d orders:", mode == 0 ? "restart" : "loop", k,
                   interrupted);
            for (int i = 0; i < n_seen; i++) printf(" %s x%d", tally_seen[i], tally_count[i]);
            printf("\n");
        }
    }
}

static void section_c(void)
{
    install(0);
    for (int k = 0; k < 3; k++)
    {
        char tally_seen[10][8];
        int n_seen = 0;
        int tally_count[10] = { 0 };
        int interrupted = 0;
        for (int trial = 0; trial < 10; trial++)
        {
            struct sockaddr_in addr;
            int s = bound_udp(&addr);
            struct q_shared sh;
            memset(&sh, 0, sizeof sh);
            sh.fd = make_port(s);
            sh.loop = 1;
            pthread_t t[3];
            struct q_arg args[3];
            for (int i = 0; i < 3; i++)
            {
                args[i].sh = &sh;
                args[i].id = i;
                pthread_create(&t[i], NULL, c_waiter, &args[i]);
                sleep_ms(30);
            }
            sleep_ms(50);
            pthread_kill(t[k], SIGUSR1);
            sleep_ms(50);
            for (int i = 0; i < 3; i++)
            {
                send_to(&addr);
                sleep_ms(100);
                drain_socket(s);
            }
            // Anyone still waiting (a wake that was not one per datagram) is
            // released by more datagrams, and shows in the log as extra entries.
            for (int extra = 0; extra < 3 && atomic_load(&sh.log_len) < 3; extra++)
            {
                send_to(&addr);
                sleep_ms(100);
                drain_socket(s);
            }
            for (int i = 0; i < 3; i++) pthread_join(t[i], NULL);
            close(sh.fd);
            close(s);
            interrupted += atomic_load(&sh.eintr_of[k]);
            char seen[8];
            log_string(&sh, seen, sizeof seen);
            int found = -1;
            for (int i = 0; i < n_seen; i++)
                if (strcmp(tally_seen[i], seen) == 0) found = i;
            if (found < 0) { found = n_seen++; snprintf(tally_seen[found], 8, "%s", seen); }
            tally_count[found]++;
        }
        printf("C mode=loop signalled=%d eintr_seen_by_signalled=%d orders:", k, interrupted);
        for (int i = 0; i < n_seen; i++) printf(" %s x%d", tally_seen[i], tally_count[i]);
        printf("\n");
    }
}

// ---- D ------------------------------------------------------------------

#ifdef __linux__
static int pin_fifo(int prio)
{
    cpu_set_t set;
    CPU_ZERO(&set);
    CPU_SET(0, &set);
    if (pthread_setaffinity_np(pthread_self(), sizeof set, &set) != 0) return -1;
    struct sched_param sp = { .sched_priority = prio };
    return pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp);
}

enum d_call { D_EPOLL, D_POLL, D_ACCEPT, D_FLOCK };

struct d_wait
{
    enum d_call call;
    int timeout;
    int fd;
    int rv;
    int error;
    atomic_int parked;
};

static void *d_sleeper(void *p)
{
    struct d_wait *w = p;
    int pinned = pin_fifo(10);
    if (pinned != 0)
    {
        w->rv = -100 - pinned;
        atomic_store(&w->parked, 1);
        return NULL;
    }
    atomic_store(&w->parked, 1);
    errno = 0;
    switch (w->call)
    {
    case D_EPOLL:
    {
        struct epoll_event out[1];
        w->rv = epoll_wait(w->fd, out, 1, w->timeout);
        break;
    }
    case D_POLL:
    {
        struct pollfd pfd = { .fd = w->fd, .events = POLLIN };
        w->rv = poll(&pfd, 1, w->timeout);
        break;
    }
    case D_ACCEPT:
        w->rv = accept(w->fd, NULL, NULL);
        break;
    case D_FLOCK:
        w->rv = flock(w->fd, LOCK_EX);
        break;
    }
    w->error = errno;
    return NULL;
}

static void section_d(void)
{
    install(0);
    const char *names[] = { "epoll_wait", "poll", "accept", "flock" };
    char path[] = "/tmp/sir-flock-XXXXXX";
    int tmp = mkstemp(path);
    if (tmp < 0) { perror("mkstemp"); return; }
    close(tmp);
    for (int call = 0; call < 4; call++)
    {
        for (int signal_first = 0; signal_first <= 1; signal_first++)
        {
            int ready = 0, eintr = 0, other = 0;
            for (int trial = 0; trial < 20; trial++)
            {
                struct sockaddr_in addr;
                int s = -1, port = -1, lst = -1, client = -1, held = -1;
                struct d_wait w;
                memset(&w, 0, sizeof w);
                w.call = (enum d_call)call;
                w.timeout = -1;
                atomic_init(&w.parked, 0);
                if (call == D_ACCEPT)
                {
                    lst = listener(&addr);
                    w.fd = lst;
                }
                else if (call == D_FLOCK)
                {
                    held = open(path, O_RDWR);
                    if (flock(held, LOCK_EX) != 0) { perror("flock"); exit(1); }
                    w.fd = open(path, O_RDWR);
                }
                else
                {
                    s = bound_udp(&addr);
                    if (call == D_EPOLL)
                    {
                        port = make_port(s);
                        w.fd = port;
                    }
                    else
                    {
                        w.fd = s;
                    }
                }
                pthread_t t;
                pthread_create(&t, NULL, d_sleeper, &w);
                while (!atomic_load(&w.parked)) { }
                if (w.rv <= -100)
                {
                    printf("D sleeper could not take SCHED_FIFO on CPU 0: error %d\n", -100 - w.rv);
                    pthread_join(t, NULL);
                    return;
                }
                sleep_ms(2);
                int hog = pin_fifo(20);
                if (hog != 0)
                {
                    printf("D hog could not take SCHED_FIFO on CPU 0: error %d\n", hog);
                    pthread_kill(t, SIGUSR1);
                    pthread_join(t, NULL);
                    return;
                }
                int64_t t0 = now_us();
                int made_ready = 0, signalled = 0;
                int ready_at = signal_first ? 10000 : 5000;
                int signal_at = signal_first ? 5000 : 10000;
                while (now_us() - t0 < 40000)
                {
                    int64_t dt = now_us() - t0;
                    if (!made_ready && dt >= ready_at)
                    {
                        if (call == D_ACCEPT) client = connect_to(&addr);
                        else if (call == D_FLOCK) flock(held, LOCK_UN);
                        else send_to(&addr);
                        made_ready = 1;
                    }
                    if (!signalled && dt >= signal_at)
                    {
                        pthread_kill(t, SIGUSR1);
                        signalled = 1;
                    }
                }
                struct sched_param sp = { .sched_priority = 0 };
                pthread_setschedparam(pthread_self(), SCHED_OTHER, &sp);
                pthread_join(t, NULL);
                if (w.rv >= 0) ready++;
                else if (w.error == EINTR) eintr++;
                else other++;
                if (call == D_ACCEPT && w.rv >= 0) close(w.rv);
                if (call == D_FLOCK)
                {
                    close(w.fd);
                    close(held);
                }
                if (client >= 0) close(client);
                if (port >= 0) close(port);
                if (s >= 0) close(s);
                if (lst >= 0) close(lst);
            }
            printf("D %s %s ready_answered=%d eintr=%d other=%d\n", names[call],
                   signal_first ? "signal-first" : "ready-first", ready, eintr, other);
        }
    }
    unlink(path);
}

static void section_e(void)
{
    install(0);
    const char *names[] = { "epoll_wait", "poll" };
    for (int call = 0; call < 2; call++)
    {
        for (int signal_first = 0; signal_first <= 1; signal_first++)
        {
            int zero = 0, eintr = 0, other = 0;
            for (int trial = 0; trial < 20; trial++)
            {
                struct sockaddr_in addr;
                int s = bound_udp(&addr);
                int port = -1;
                struct d_wait w;
                memset(&w, 0, sizeof w);
                w.call = (enum d_call)call;
                w.timeout = 10;
                atomic_init(&w.parked, 0);
                if (call == D_EPOLL)
                {
                    port = make_port(s);
                    w.fd = port;
                }
                else
                {
                    w.fd = s;
                }
                pthread_t t;
                pthread_create(&t, NULL, d_sleeper, &w);
                while (!atomic_load(&w.parked)) { }
                if (w.rv <= -100)
                {
                    printf("E sleeper could not take SCHED_FIFO on CPU 0: error %d\n", -100 - w.rv);
                    pthread_join(t, NULL);
                    return;
                }
                int64_t t0 = now_us();
                sleep_ms(2);
                int hog = pin_fifo(20);
                if (hog != 0)
                {
                    printf("E hog could not take SCHED_FIFO on CPU 0: error %d\n", hog);
                    pthread_join(t, NULL);
                    return;
                }
                int signal_at = signal_first ? 5000 : 20000;
                int signalled = 0;
                while (now_us() - t0 < 40000)
                {
                    if (!signalled && now_us() - t0 >= signal_at)
                    {
                        pthread_kill(t, SIGUSR1);
                        signalled = 1;
                    }
                }
                struct sched_param sp = { .sched_priority = 0 };
                pthread_setschedparam(pthread_self(), SCHED_OTHER, &sp);
                pthread_join(t, NULL);
                if (w.rv == 0) zero++;
                else if (w.rv < 0 && w.error == EINTR) eintr++;
                else other++;
                if (port >= 0) close(port);
                close(s);
            }
            printf("E %s %s zero=%d eintr=%d other=%d\n", names[call],
                   signal_first ? "signal-first" : "deadline-first", zero, eintr, other);
        }
    }
}
#endif

// ---- F ------------------------------------------------------------------

struct f_wait { int call; int fd; int rv; int error; };

static void *f_sleeper(void *p)
{
    struct f_wait *w = p;
    sigset_t usr1;
    sigemptyset(&usr1);
    sigaddset(&usr1, SIGUSR1);
    pthread_sigmask(SIG_UNBLOCK, &usr1, NULL);
    errno = 0;
    if (w->call == 0) w->rv = accept(w->fd, NULL, NULL);
    else w->rv = flock(w->fd, LOCK_EX);
    w->error = errno;
    return NULL;
}

static void section_f(void)
{
    install(0);
    // The signal is sent to the process, so every thread but the sleeper
    // blocks it, and the sleeper is the one that takes it.
    sigset_t usr1, before;
    sigemptyset(&usr1);
    sigaddset(&usr1, SIGUSR1);
    pthread_sigmask(SIG_BLOCK, &usr1, &before);
    const char *names[] = { "accept", "flock" };
    const char *shapes[] = { "ready-then-signal", "signal-then-ready", "ready-only", "signal-only" };
    char path[] = "/tmp/sir-flock-XXXXXX";
    int tmp = mkstemp(path);
    if (tmp < 0) { perror("mkstemp"); return; }
    close(tmp);
    for (int call = 0; call < 2; call++)
    {
        for (int shape = 0; shape < 4; shape++)
        {
            int completed = 0, eintr = 0, other = 0;
            for (int trial = 0; trial < 10; trial++)
            {
                struct sockaddr_in addr;
                int lst = -1;
                if (call == 0) lst = listener(&addr);
                int go[2], ready[2];
                if (pipe(go) != 0 || pipe(ready) != 0) { perror("pipe"); exit(1); }
                pid_t parent = getpid();
                pid_t helper = fork();
                if (helper == 0)
                {
                    alarm(10);
                    int held = -1;
                    if (call == 1)
                    {
                        held = open(path, O_RDWR);
                        if (flock(held, LOCK_EX) != 0) _exit(2);
                    }
                    char b = 1;
                    if (write(ready[1], &b, 1) != 1) _exit(3);
                    if (read(go[0], &b, 1) != 1) _exit(4);
                    kill(parent, SIGSTOP);
                    sleep_ms(50);
                    int client = -1;
                    for (int step = 0; step < 2; step++)
                    {
                        int signal_step = shape == 1 ? 0 : 1;
                        if (step == signal_step)
                        {
                            if (shape != 2) kill(parent, SIGUSR1);
                        }
                        else if (shape != 3)
                        {
                            if (call == 0) client = connect_to(&addr);
                            else flock(held, LOCK_UN);
                        }
                        sleep_ms(50);
                    }
                    kill(parent, SIGCONT);
                    sleep_ms(200);
                    if (client >= 0) close(client);
                    _exit(0);
                }
                char b;
                if (read(ready[0], &b, 1) != 1) { perror("read"); exit(1); }
                struct f_wait w = { .call = call, .rv = -99 };
                if (call == 0) w.fd = lst;
                else w.fd = open(path, O_RDWR);
                pthread_t t;
                pthread_create(&t, NULL, f_sleeper, &w);
                sleep_ms(50);
                b = 1;
                if (write(go[1], &b, 1) != 1) { perror("write"); exit(1); }
                int status;
                waitpid(helper, &status, 0);
                // A sleeper that the controls leave blocked is released here.
                if (call == 0)
                {
                    int extra = connect_to(&addr);
                    pthread_join(t, NULL);
                    close(extra);
                }
                else
                {
                    pthread_join(t, NULL);
                }
                if (w.rv >= 0) completed++;
                else if (w.error == EINTR) eintr++;
                else other++;
                if (call == 0 && w.rv >= 0) close(w.rv);
                if (call == 1) close(w.fd);
                if (lst >= 0) close(lst);
                close(go[0]); close(go[1]); close(ready[0]); close(ready[1]);
            }
            printf("F %s %s completed=%d eintr=%d other=%d\n", names[call], shapes[shape], completed, eintr, other);
        }
    }
    unlink(path);
    pthread_sigmask(SIG_SETMASK, &before, NULL);
}

int main(void)
{
    alarm(600);
    setvbuf(stdout, NULL, _IONBF, 0);
    section_a();
    section_b();
    section_c();
    section_f();
#ifdef __linux__
    section_d();
    section_e();
#endif
    return 0;
}
