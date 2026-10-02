// What a sleeping syscall does once another thread has closed the last
// descriptor onto the open file description it sleeps on, and what becomes of
// that description when the call returns.
//
// Each section parks one thread in a blocking call, waits 50 ms, closes the
// descriptor the call was made through (with nothing else onto its
// description unless the row says "dup kept"), waits 100 ms to see whether the
// close woke it, then gives it what it waits for and records its answer. Last,
// it probes whether the description outlived the call.
//
// Sections:
//   A  accept on an IPv4 stream listener: A1 the close, then a connect: the
//      accept's rv; then a second connect to the same port, after the accept
//      returned: its rv/errno. A2 the same with a dup of the listener kept,
//      which is the control for the second connect.
//   B  read of an empty pipe: the close of the read end, then 3 bytes written:
//      the read's rv; then a 1-byte write (SIGPIPE ignored): rv/errno, which
//      says whether the read end outlived the read. B2 with a dup kept.
//   C  1-byte write into a full pipe: the close of the write end, then 4096
//      bytes drained: the write's rv; then the reader drains the rest and
//      reads once more: rv, which says whether the write end outlived the
//      write. C2 with a dup kept.
//   D  flock(LOCK_EX) of a file another description holds LOCK_EX on: the
//      close, made from a third thread, then the holder's LOCK_UN: whether the
//      close returned within 100 ms, the flock's rv, the close's rv; then a
//      third description's flock(LOCK_EX|LOCK_NB): rv/errno, which says
//      whether the granted lock outlived the call. D2 with a dup kept.
//   E  epoll_wait(-1) on a port holding one bound UDP socket registered
//      EPOLLIN: the close of the port, then a datagram to the socket: the
//      wait's rv and the event's data.
//   F  flock(LOCK_EX) as D, with the close and then SIGUSR1 (no SA_RESTART)
//      instead of the release: the flock's rv/errno; then the holder's
//      LOCK_UN and a third description's flock(LOCK_EX|LOCK_NB): rv/errno.
//
// A sleeper still asleep 300 ms after it was given what it waits for is
// reported as such and interrupted with SIGUSR1; alarm(30) bounds the run.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -pthread -o /tmp/ofr open-file-references.c && /tmp/ofr
//   Linux:  container run --rm -v "$PWD:/probe" gcc:14 sh -c
//             "gcc -Wall -O1 -pthread -o /tmp/ofr /probe/open-file-references.c && /tmp/ofr"
//
// Measured 2026-10-02 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14
// image, glibc 2.41), five times in full (twice with D and F as they stand,
// three times with their close made from the main thread), and on Darwin
// 27.0.0 arm64 three times, with the same answers every time:
//   Linux: no close woke its sleeper, and every close returned 0 at once.
//      Every sleeper then finished as if nothing had been closed: A the accept
//      returned the connection, B the read 3, C the write 1, D the flock 0, E
//      the wait 1 with the registration's data 0x5eed, F the flock EINTR. The
//      description went as the call returned: A1's second connect was
//      ECONNREFUSED (111), B1's write EPIPE (32), C1's read 0 (end of file),
//      and D1's and F's third description was granted its lock. Each control
//      answered as for a description still open: A2 connected, B2 wrote 1, C2
//      was EAGAIN (11), D2 EWOULDBLOCK (11).
//   Darwin: the close ended the sleeping accept (ECONNABORTED, 53), read (0)
//      and write (EPIPE, 32) at once, with a dup kept or not: the descriptor
//      closed was the one the call was entered through. The flock it did not
//      end, and the close itself did not return until the flock had: D1 and
//      D2 granted when the holder released (rv 0), F EINTR at the signal, the
//      close 0 just after. The lock then went with the description as on
//      Linux (D1 and F granted, D2 EWOULDBLOCK, 35). With the close made from
//      the main thread, as D first was, the process deadlocked in close: the
//      alarm's SIGALRM did not end it, and SIGKILL did.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#endif

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static void on_usr1(int sig) { (void)sig; }

struct sleeper
{
    int kind; // 0 accept, 1 read, 2 write, 3 flock, 4 epoll_wait
    int fd;
    long rv;
    int error;
    uint64_t data;
    atomic_int returned;
};

static void *sleeper_main(void *arg)
{
    struct sleeper *s = arg;
    char buf[16];
    errno = 0;
    switch (s->kind)
    {
    case 0: s->rv = accept(s->fd, NULL, NULL); break;
    case 1: s->rv = read(s->fd, buf, sizeof buf); break;
    case 2: s->rv = write(s->fd, "w", 1); break;
    case 3: s->rv = flock(s->fd, LOCK_EX); break;
#ifdef __linux__
    case 4:
    {
        struct epoll_event ev;
        s->rv = epoll_wait(s->fd, &ev, 1, -1);
        if (s->rv == 1) s->data = ev.data.u64;
        break;
    }
#endif
    }
    s->error = errno;
    atomic_store(&s->returned, 1);
    return NULL;
}

static pthread_t start(struct sleeper *s)
{
    pthread_t t;
    atomic_store(&s->returned, 0);
    pthread_create(&t, NULL, sleeper_main, s);
    sleep_ms(50);
    return t;
}

// Close, then report whether the close woke the sleeper.
static int close_and_watch(const char *label, struct sleeper *s, int fd)
{
    int closed = close(fd);
    sleep_ms(100);
    int woke = atomic_load(&s->returned);
    printf("%s close=%d woke-on-close=%d", label, closed, woke);
    if (woke) printf(" rv=%ld errno=%d", s->rv, s->error);
    printf("\n");
    return woke;
}

static void finish(const char *label, pthread_t t, struct sleeper *s)
{
    sleep_ms(300);
    if (!atomic_load(&s->returned))
    {
        printf("%s still asleep 300 ms after being given what it waits for\n", label);
        pthread_kill(t, SIGUSR1);
    }
    pthread_join(t, NULL);
    printf("%s returned rv=%ld errno=%d\n", label, s->rv, s->error);
}

static int listener(struct sockaddr_in *addr)
{
    int s = socket(AF_INET, SOCK_STREAM, 0);
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

static int connect_to(struct sockaddr_in *addr, int *error)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    errno = 0;
    int rv = connect(c, (struct sockaddr *)addr, sizeof *addr);
    *error = errno;
    if (rv != 0) { close(c); return -1; }
    return c;
}

static void section_a(int dup_kept)
{
    const char *label = dup_kept ? "A2" : "A1";
    struct sockaddr_in addr;
    int l = listener(&addr);
    int other = dup_kept ? dup(l) : -1;
    struct sleeper s = { .kind = 0, .fd = l };
    pthread_t t = start(&s);
    if (close_and_watch(label, &s, l)) { pthread_join(t, NULL); return; }
    int error;
    int c1 = connect_to(&addr, &error);
    printf("%s first connect rv=%d errno=%d\n", label, c1 >= 0 ? 0 : -1, c1 >= 0 ? 0 : error);
    finish(label, t, &s);
    int c2 = connect_to(&addr, &error);
    printf("%s second connect, after the accept returned: rv=%d errno=%d\n", label, c2 >= 0 ? 0 : -1,
           c2 >= 0 ? 0 : error);
    if (s.rv >= 0) close((int)s.rv);
    if (c1 >= 0) close(c1);
    if (c2 >= 0) close(c2);
    if (other >= 0) close(other);
}

static void fill(int w)
{
    int flags = fcntl(w, F_GETFL);
    fcntl(w, F_SETFL, flags | O_NONBLOCK);
    char buf[4096];
    memset(buf, 'f', sizeof buf);
    while (write(w, buf, sizeof buf) > 0) { }
    while (write(w, buf, 1) > 0) { }
    fcntl(w, F_SETFL, flags);
}

static void section_b(int dup_kept)
{
    const char *label = dup_kept ? "B2" : "B1";
    int fds[2];
    pipe(fds);
    int other = dup_kept ? dup(fds[0]) : -1;
    struct sleeper s = { .kind = 1, .fd = fds[0] };
    pthread_t t = start(&s);
    if (close_and_watch(label, &s, fds[0])) { pthread_join(t, NULL); return; }
    write(fds[1], "abc", 3);
    finish(label, t, &s);
    errno = 0;
    ssize_t rv = write(fds[1], "x", 1);
    printf("%s write after the read returned: rv=%zd errno=%d\n", label, rv, rv < 0 ? errno : 0);
    close(fds[1]);
    if (other >= 0) close(other);
}

static void section_c(int dup_kept)
{
    const char *label = dup_kept ? "C2" : "C1";
    int fds[2];
    pipe(fds);
    fill(fds[1]);
    int other = dup_kept ? dup(fds[1]) : -1;
    struct sleeper s = { .kind = 2, .fd = fds[1] };
    pthread_t t = start(&s);
    if (close_and_watch(label, &s, fds[1])) { pthread_join(t, NULL); return; }
    char buf[4096];
    ssize_t got = 0;
    while (got < 4096)
    {
        ssize_t k = read(fds[0], buf, (size_t)(4096 - got));
        if (k <= 0) break;
        got += k;
    }
    finish(label, t, &s);
    // Drain what is left without blocking, then ask once more.
    int flags = fcntl(fds[0], F_GETFL);
    fcntl(fds[0], F_SETFL, flags | O_NONBLOCK);
    while (read(fds[0], buf, sizeof buf) > 0) { }
    errno = 0;
    ssize_t rv = read(fds[0], buf, 1);
    printf("%s read of the drained pipe after the write returned: rv=%zd errno=%d\n", label, rv,
           rv < 0 ? errno : 0);
    close(fds[0]);
    if (other >= 0) close(other);
}

struct closer
{
    int fd;
    int rv;
    atomic_int returned;
};

static void *closer_main(void *arg)
{
    struct closer *c = arg;
    c->rv = close(c->fd);
    atomic_store(&c->returned, 1);
    return NULL;
}

// The close is made from a thread of its own, because on Darwin it does not
// return while the flock sleeps, and the main thread must stay free to end the
// flock.
static void section_d(int dup_kept, int interrupt)
{
    const char *label = interrupt ? "F" : dup_kept ? "D2" : "D1";
    char path[] = "/tmp/ofr-XXXXXX";
    int holder = mkstemp(path);
    int waiter = open(path, O_RDWR);
    int third = open(path, O_RDWR);
    unlink(path);
    if (flock(holder, LOCK_EX) != 0) { perror("flock holder"); exit(1); }
    int other = dup_kept ? dup(waiter) : -1;
    struct sleeper s = { .kind = 3, .fd = waiter };
    pthread_t t = start(&s);
    struct closer c = { .fd = waiter };
    atomic_store(&c.returned, 0);
    pthread_t ct;
    pthread_create(&ct, NULL, closer_main, &c);
    sleep_ms(100);
    printf("%s 100 ms after the close began: close-returned=%d woke-on-close=%d\n", label,
           atomic_load(&c.returned), atomic_load(&s.returned));
    if (interrupt)
    {
        pthread_kill(t, SIGUSR1);
        sleep_ms(100);
        printf("%s 100 ms after SIGUSR1: close-returned=%d flock-returned=%d\n", label, atomic_load(&c.returned),
               atomic_load(&s.returned));
        flock(holder, LOCK_UN);
    }
    else
    {
        flock(holder, LOCK_UN);
    }
    finish(label, t, &s);
    pthread_join(ct, NULL);
    printf("%s close rv=%d\n", label, c.rv);
    errno = 0;
    int rv = flock(third, LOCK_EX | LOCK_NB);
    printf("%s third description's LOCK_EX|LOCK_NB after the flock returned: rv=%d errno=%d\n", label, rv,
           rv < 0 ? errno : 0);
    close(holder);
    close(third);
    if (other >= 0) close(other);
}

#ifdef __linux__
static void section_e(void)
{
    int u = socket(AF_INET, SOCK_DGRAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(u, (struct sockaddr *)&a, sizeof a);
    socklen_t len = sizeof a;
    getsockname(u, (struct sockaddr *)&a, &len);
    int ep = epoll_create1(0);
    struct epoll_event ev = { .events = EPOLLIN, .data.u64 = 0x5eedULL };
    epoll_ctl(ep, EPOLL_CTL_ADD, u, &ev);
    struct sleeper s = { .kind = 4, .fd = ep };
    pthread_t t = start(&s);
    if (close_and_watch("E", &s, ep)) { pthread_join(t, NULL); return; }
    int sender = socket(AF_INET, SOCK_DGRAM, 0);
    sendto(sender, "d", 1, 0, (struct sockaddr *)&a, sizeof a);
    finish("E", t, &s);
    printf("E event data=0x%llx\n", (unsigned long long)s.data);
    close(sender);
    close(u);
}
#endif

int main(void)
{
    alarm(30);
    setvbuf(stdout, NULL, _IONBF, 0);
    signal(SIGPIPE, SIG_IGN);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sigaction(SIGUSR1, &sa, NULL);
    section_a(0);
    section_a(1);
    section_b(0);
    section_b(1);
    section_c(0);
    section_c(1);
    section_d(0, 0);
    section_d(1, 0);
#ifdef __linux__
    section_e();
#endif
    section_d(0, 1);
    return 0;
}
