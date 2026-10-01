// Copied from the stdio design's probes (2026-09-26), where it was run on
// Linux 6.18.5 aarch64 (Apple `container`, gcc:14, root) and Darwin 27.0.0
// arm64. The rows the kernel library relies on:
//
//   a write to a pipe whose read end is closed   EPIPE and SIGPIPE, on both,
//                                                ahead of EAGAIN (full,
//                                                non-blocking) and of EFAULT
//   the same write of 0 bytes                    Linux: 0, and no signal.
//                                                Darwin: EPIPE and SIGPIPE.
//   who receives the signal                      Linux the writing thread,
//                                                Darwin the process
//   the reader leaves during a blocking write    Linux: the partial count and
//                                                SIGPIPE. Darwin: EPIPE.
//
// The kernel library answers every row but the last, which only a write that
// sleeps can reach (`UnixReadWrite.admitWrite`); pipe-epipe-sweep.c sweeps the
// first two further.
//
// Build: `nix develop -c clang -O0 -pthread -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -pthread -o /tmp/p <this file>` on Linux.
// EPIPE and SIGPIPE: a write to a pipe with no reader, under each disposition,
// from the main thread and from a worker, blocking and not, and partial writes
// whose reader leaves mid-write. Each case runs in its own child.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static char buf[1 << 20];
static volatile sig_atomic_t handled = 0;
static volatile sig_atomic_t returned = 0;
static volatile sig_atomic_t handled_before_return = -1;
static pthread_t handler_thread;

static void on_pipe(int s) {
    (void)s;
    handled++;
    if (handled_before_return < 0) handled_before_return = !returned;
    handler_thread = pthread_self();
}

static void install(int how) {
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = how == 0 ? SIG_DFL : how == 1 ? SIG_IGN : on_pipe;
    sigaction(SIGPIPE, &sa, NULL);
}

static const char *dispname(int how) { return how == 0 ? "SIG_DFL" : how == 1 ? "SIG_IGN" : "handler"; }

static int pending_pipe(void) {
    sigset_t s;
    sigpending(&s);
    return sigismember(&s, SIGPIPE);
}

static void report(const char *label, ssize_t r, int e) {
    printf("  %-44s write -> %zd errno %d; handler ran %d time(s), before return %d; SIGPIPE pending %d\n", label, r, e,
           (int)handled, (int)handled_before_return, pending_pipe());
    fflush(stdout);
}

typedef struct { int fd; size_t n; ssize_t r; int e; } job;

static void *worker_write(void *arg) {
    job *j = arg;
    j->r = write(j->fd, buf, j->n);
    j->e = j->r < 0 ? errno : 0;
    returned = 1;
    return NULL;
}

static void *drain_then_close(void *arg) {
    int r = *(int *)arg;
    usleep(100 * 1000);
    ssize_t got = read(r, buf, 1000);
    usleep(100 * 1000);
    close(r);
    (void)got;
    return NULL;
}

static void run(const char *name, void (*body)(int how)) {
    for (int how = 0; how <= 2; how++) {
        fflush(stdout);
        pid_t c = fork();
        if (c == 0) {
            alarm(10);
            install(how);
            printf("%s, %s\n", name, dispname(how));
            body(how);
            fflush(stdout);
            _exit(0);
        }
        int ws = 0;
        waitpid(c, &ws, 0);
        if (WIFSIGNALED(ws)) printf("  child died of signal %d\n", WTERMSIG(ws));
        else if (WEXITSTATUS(ws) != 0) printf("  child exited %d\n", WEXITSTATUS(ws));
        fflush(stdout);
    }
}

static void broken_main(int how) {
    (void)how;
    int p[2]; pipe(p); close(p[0]);
    returned = 0;
    ssize_t r = write(p[1], "x", 1); int e = r < 0 ? errno : 0; returned = 1;
    report("main thread, 1 byte", r, e);
    r = write(p[1], buf, 0); e = r < 0 ? errno : 0;
    report("then 0 bytes", r, e);
    r = write(p[1], (void *)8, 10); e = r < 0 ? errno : 0;
    report("then bad pointer, 10 bytes", r, e);
}

static void broken_blocked(int how) {
    (void)how;
    sigset_t s; sigemptyset(&s); sigaddset(&s, SIGPIPE);
    pthread_sigmask(SIG_BLOCK, &s, NULL);
    int p[2]; pipe(p); close(p[0]);
    ssize_t r = write(p[1], "x", 1); int e = r < 0 ? errno : 0; returned = 1;
    report("SIGPIPE blocked, 1 byte", r, e);
    pthread_sigmask(SIG_UNBLOCK, &s, NULL);
    report("after unblocking", r, e);
}

static void broken_nonblocking_full(int how) {
    (void)how;
    int p[2]; pipe(p);
    fcntl(p[1], F_SETFL, O_NONBLOCK);
    while (write(p[1], buf, 4096) > 0) {}
    close(p[0]);
    ssize_t r = write(p[1], "x", 1); int e = r < 0 ? errno : 0; returned = 1;
    report("full non-blocking pipe, reader then closed", r, e);
}

static void broken_worker(int how) {
    (void)how;
    int p[2]; pipe(p); close(p[0]);
    job j = {p[1], 1, 0, 0};
    pthread_t t;
    pthread_create(&t, NULL, worker_write, &j);
    pthread_join(t, NULL);
    report("worker thread, 1 byte", j.r, j.e);
    if (handled) printf("  handler ran on the %s thread\n", pthread_equal(handler_thread, t) ? "writing (worker)" : "main");
}

static void partial(int how) {
    (void)how;
    int p[2]; pipe(p);
    pthread_t t;
    pthread_create(&t, NULL, drain_then_close, &p[0]);
    returned = 0;
    ssize_t r = write(p[1], buf, 200000); int e = r < 0 ? errno : 0; returned = 1;
    pthread_join(t, NULL);
    report("blocking 200000 bytes; reader takes 1000, closes", r, e);
}

#ifdef F_SETNOSIGPIPE
static void nosigpipe(int how) {
    (void)how;
    int p[2]; pipe(p); close(p[0]);
    int rc = fcntl(p[1], F_SETNOSIGPIPE, 1);
    ssize_t r = write(p[1], "x", 1); int e = r < 0 ? errno : 0; returned = 1;
    printf("  F_SETNOSIGPIPE -> %d, F_GETNOSIGPIPE -> %d\n", rc, fcntl(p[1], F_GETNOSIGPIPE));
    report("F_SETNOSIGPIPE, 1 byte", r, e);
}
#endif

int main(void) {
    alarm(60);
    memset(buf, 'x', sizeof buf);
    setvbuf(stdout, NULL, _IOLBF, 0);
    run("write to a pipe whose reader is closed", broken_main);
    run("same, SIGPIPE blocked", broken_blocked);
    run("same, non-blocking and full", broken_nonblocking_full);
    run("same, from a worker thread", broken_worker);
    run("reader leaves mid-write", partial);
#ifdef F_SETNOSIGPIPE
    run("Darwin per-descriptor no-SIGPIPE", nosigpipe);
#endif
    // Reading: the read end after the writer closed, with data left.
    int p[2]; pipe(p); write(p[1], "abc", 3); close(p[1]);
    ssize_t a = read(p[0], buf, 2), b = read(p[0], buf, 2), c = read(p[0], buf, 2);
    printf("read end, 3 bytes then writer closed: reads of 2 -> %zd %zd %zd\n", a, b, c);
    return 0;
}
