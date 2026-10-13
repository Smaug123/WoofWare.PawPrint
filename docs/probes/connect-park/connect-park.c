// What a blocking connect(2) to a listener whose accept queue is full does,
// measured on both flavours. Design note:
// pawprint-kernel-design/connect-park-options.md.
//
// Build and run:
//   Darwin: cc -Wall -o /tmp/cp connect-park.c -lpthread && /tmp/cp
//   Linux:  gcc -Wall -o /tmp/cp connect-park.c -lpthread && /tmp/cp
//
// Each row fills a listener's accept queue with one connection (Linux
// listen(fd, 0) admits one; Darwin listen(fd, 1) admits one), then makes a
// second connect, and does something to the listener or the caller later.
//
//   T  TCP over loopback
//   U  Unix-domain stream
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>

#ifdef __linux__
#define BACKLOG 0
#else
#define BACKLOG 1
#endif

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

enum action { NOTHING, ACCEPT, CLOSE_LISTENER, SIGNAL };
struct later { pthread_t target; int ms; enum action what; int fd; };

static void *do_later(void *p)
{
    struct later *l = p;
    usleep(l->ms * 1000);
    switch (l->what) {
    case NOTHING: break;
    case ACCEPT: { int a = accept(l->fd, NULL, NULL); (void)a; break; }
    case CLOSE_LISTENER: close(l->fd); break;
    case SIGNAL: pthread_kill(l->target, SIGUSR1); break;
    }
    return NULL;
}

static pthread_t later(struct later *l, int ms, enum action what, int fd)
{
    pthread_t t;
    l->target = pthread_self(); l->ms = ms; l->what = what; l->fd = fd;
    pthread_create(&t, NULL, do_later, l);
    return t;
}

static struct sockaddr_storage addr;
static socklen_t addr_len;

static int listener(int unix_domain)
{
    int l;
    memset(&addr, 0, sizeof addr);
    if (unix_domain) {
        struct sockaddr_un *un = (struct sockaddr_un *)&addr;
        un->sun_family = AF_UNIX;
        snprintf(un->sun_path, sizeof un->sun_path, "/tmp/cp-%d.sock", (int)getpid());
        unlink(un->sun_path);
        addr_len = sizeof *un;
        l = socket(AF_UNIX, SOCK_STREAM, 0);
    } else {
        struct sockaddr_in *in = (struct sockaddr_in *)&addr;
        in->sin_family = AF_INET;
        in->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addr_len = sizeof *in;
        l = socket(AF_INET, SOCK_STREAM, 0);
    }
    bind(l, (struct sockaddr *)&addr, addr_len);
    getsockname(l, (struct sockaddr *)&addr, &addr_len);
    listen(l, BACKLOG);
    return l;
}

static int client(int unix_domain) { return socket(unix_domain ? AF_UNIX : AF_INET, SOCK_STREAM, 0); }

// One row: a listener filled with one connection, then the measured connect.
static void row(const char *name, int unix_domain, int nonblocking, enum action first, int first_ms,
                enum action second, int second_ms, int restart)
{
    install(restart);
    handled = 0;
    int l = listener(unix_domain);
    int filler = client(unix_domain);
    int fr = connect(filler, (struct sockaddr *)&addr, addr_len);
    int fe = errno;
    int c = client(unix_domain);
    if (nonblocking) fcntl(c, F_SETFL, fcntl(c, F_GETFL) | O_NONBLOCK);
    struct later l1, l2;
    pthread_t t1 = later(&l1, first_ms, first, first == SIGNAL ? -1 : l);
    pthread_t t2 = later(&l2, second_ms, second, second == SIGNAL ? -1 : l);
    double t0 = now_ms();
    int r = connect(c, (struct sockaddr *)&addr, addr_len);
    int e = errno;
    double took = now_ms() - t0;
    char extra[200] = "";
    if (r < 0 && e == EINPROGRESS) {
        struct pollfd p = { c, POLLOUT, 0 };
        int pr = poll(&p, 1, 5000);
        int soerr = 0;
        socklen_t len = sizeof soerr;
        getsockopt(c, SOL_SOCKET, SO_ERROR, &soerr, &len);
        snprintf(extra, sizeof extra, " then poll=%d writable_at_ms=%.0f so_error=%s", pr, now_ms() - t0,
                 soerr ? strerror(soerr) : "0");
    }
    if (r < 0 && e == EINTR) {
        // What a second connect on the same socket answers, as a C library
        // that retries on EINTR would make.
        double t1s = now_ms();
        int r2 = connect(c, (struct sockaddr *)&addr, addr_len);
        int e2 = errno;
        snprintf(extra, sizeof extra, " then a second connect: ret=%d errno=%s after_ms=%.0f", r2,
                 r2 < 0 ? strerror(e2) : "-", now_ms() - t1s);
    }
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    // What became of the filler: a reset shows as ECONNRESET here.
    char b;
    ssize_t fr2 = recv(filler, &b, 1, MSG_DONTWAIT);
    int fe2 = errno;
    printf("%s: filler=%d%s%s ret=%d errno=%s handled=%d took_ms=%.0f%s filler_recv=%zd/%s\n", name, fr,
           fr < 0 ? "/" : "", fr < 0 ? strerror(fe) : "", r, r < 0 ? strerror(e) : "-", (int)handled, took, extra,
           fr2, fr2 < 0 ? strerror(fe2) : "-");
    close(c); close(filler); close(l);
    if (unix_domain) unlink(((struct sockaddr_un *)&addr)->sun_path);
}

int main(int argc, char **argv)
{
    setvbuf(stdout, NULL, _IONBF, 0);
    int runs = argc > 1 ? atoi(argv[1]) : 1;
    for (int run = 0; run < runs; run++)
    for (int u = 0; u <= 1; u++) {
        const char *d = u ? "U Unix-domain" : "T TCP";
        char name[200];
        snprintf(name, sizeof name, "%s1 blocking, the filler accepted at 200ms", d);
        row(name, u, 0, ACCEPT, 200, NOTHING, 0, 0);
        snprintf(name, sizeof name, "%s2 non-blocking, the filler accepted at 200ms", d);
        row(name, u, 1, ACCEPT, 200, NOTHING, 0, 0);
        snprintf(name, sizeof name, "%s3 blocking, signal at 100ms, accept at 300ms, SA_RESTART=0", d);
        row(name, u, 0, SIGNAL, 100, ACCEPT, 300, 0);
        snprintf(name, sizeof name, "%s3 blocking, signal at 100ms, accept at 300ms, SA_RESTART=1", d);
        row(name, u, 0, SIGNAL, 100, ACCEPT, 300, 1);
        snprintf(name, sizeof name, "%s4 blocking, the listener closed at 200ms", d);
        row(name, u, 0, CLOSE_LISTENER, 200, NOTHING, 0, 0);
    }
    return 0;
}
