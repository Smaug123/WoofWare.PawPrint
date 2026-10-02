// A write into a pipe whose read end is closed, swept: what it answers, whether
// SIGPIPE is raised, and what it leaves behind, for every combination of
//
//   what the pipe held when the reader closed   nothing, 1000 bytes, full
//   O_NONBLOCK on the write end                 clear, set
//   the count                                   0, 1, PIPE_BUF, PIPE_BUF + 1,
//                                               65536, 100000
//   the buffer                                  valid, NULL, (void *)8
//   SIGPIPE's disposition                       SIG_IGN, a handler, SIG_DFL
//
// 324 rows per flavour, each in a child of its own, from the main thread (who
// receives the signal from a worker is pipe-sigpipe.c's). A row reports the
// result and errno, how many times the handler ran and whether it ran before
// write returned, whether SIGPIPE is left pending, whether the write moved the
// write end's mtime, ctime or atime (fstat before and after, 20 ms apart), and
// `poll` of the write end afterwards; or that the child died of a signal. The
// summary at the end counts the rows that break the rule stated below.
//
// Results, 2026-10-02 (Linux 6.18.5 aarch64 in Apple's `container`, gcc:14,
// root; Darwin 27.0.0 arm64, uid 501): no row of the 324 on either breaks the
// rule the summary checks, which is
//
//   Linux   a count of 0 answers 0 and raises nothing, whatever the pipe held,
//           the flag, the buffer (NULL and (void *)8 included) and the
//           disposition. Every other count answers EPIPE and raises SIGPIPE:
//           ahead of EAGAIN (a full non-blocking pipe), of EFAULT (both bad
//           buffers), and of a short count (a pipe with room for part of
//           it). Under SIG_DFL the child dies of signal 13; under the handler
//           it runs once, before write returns; under SIG_IGN or the handler
//           nothing is left pending. No timestamp of the write end moves.
//   Darwin  every count, 0 included, answers EPIPE and raises SIGPIPE, the
//           rest as on Linux. So an EPIPE moves no timestamp on Darwin either,
//           though every other answer a write reaching a pipe gives there
//           does: the control row, a write with the reader open, moved mtime
//           and ctime on Darwin (and nothing on Linux), as an EAGAIN into a
//           full pipe also did when checked by hand the same day.
//
// And `poll(POLLOUT)` of the write end afterwards: on Linux 0xc (POLLERR|
// POLLOUT) where the pipe held nothing or 1000 bytes and 0x8 (POLLERR) where
// it was full; on Darwin 0x10 (POLLHUP) in every row.
//
// Build: `nix develop -c clang -O0 -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -o /tmp/p <this file>` on Linux.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __APPLE__
#define MTIME(s) ((s).st_mtimespec)
#define CTIME(s) ((s).st_ctimespec)
#define ATIME(s) ((s).st_atimespec)
#else
#define MTIME(s) ((s).st_mtim)
#define CTIME(s) ((s).st_ctim)
#define ATIME(s) ((s).st_atim)
#endif

static char buf[1 << 17];
static volatile sig_atomic_t handled = 0;
static volatile sig_atomic_t returned = 0;
static volatile sig_atomic_t handled_before_return = -1;

static void on_pipe(int s) {
    (void)s;
    handled++;
    if (handled_before_return < 0) handled_before_return = !returned;
}

static int same(struct timespec a, struct timespec b) { return a.tv_sec == b.tv_sec && a.tv_nsec == b.tv_nsec; }

static const char *fills[] = {"empty", "1000", "full"};
static const char *buffers[] = {"valid", "NULL", "bad"};
static const char *dispositions[] = {"SIG_IGN", "handler", "SIG_DFL"};

// The child's report, written into a pipe for the parent, which knows whether
// the child then died.
typedef struct {
    long r;
    int e, handled, before, pending, mtime, ctime, atime, revents;
} row;

static void child(int report, int fill, int nonblock, size_t n, int which, int how) {
    alarm(5);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = how == 0 ? SIG_IGN : how == 1 ? on_pipe : SIG_DFL;
    sigaction(SIGPIPE, &sa, NULL);

    int p[2];
    if (pipe(p) != 0) _exit(10);
    if (fill == 1) write(p[1], buf, 1000);
    if (fill == 2) {
        fcntl(p[1], F_SETFL, O_NONBLOCK);
        while (write(p[1], buf, 4096) > 0) {}
        while (write(p[1], buf, 1) > 0) {}
        fcntl(p[1], F_SETFL, 0);
    }
    if (nonblock) fcntl(p[1], F_SETFL, O_NONBLOCK);
    close(p[0]);

    struct stat before, after;
    fstat(p[1], &before);
    usleep(20 * 1000);

    const void *b = which == 0 ? (const void *)buf : which == 1 ? NULL : (const void *)8;
    row out;
    memset(&out, 0, sizeof out);
    returned = 0;
    ssize_t r = write(p[1], b, n);
    out.e = r < 0 ? errno : 0;
    returned = 1;
    out.r = (long)r;
    fstat(p[1], &after);
    sigset_t pending;
    sigpending(&pending);
    struct pollfd pf = {p[1], POLLOUT, 0};
    poll(&pf, 1, 0);
    out.handled = handled;
    out.before = handled_before_return;
    out.pending = sigismember(&pending, SIGPIPE);
    out.mtime = !same(MTIME(before), MTIME(after));
    out.ctime = !same(CTIME(before), CTIME(after));
    out.atime = !same(ATIME(before), ATIME(after));
    out.revents = pf.revents;
    write(report, &out, sizeof out);
    _exit(0);
}

int main(void) {
    alarm(120);
    memset(buf, 'x', sizeof buf);
    setvbuf(stdout, NULL, _IOLBF, 0);
    long pipe_buf = fpathconf(0, _PC_PIPE_BUF);
    int probe[2];
    pipe(probe);
    pipe_buf = fpathconf(probe[1], _PC_PIPE_BUF);
    close(probe[0]);
    close(probe[1]);
#ifdef __APPLE__
    int on_linux = 0;
    printf("# Darwin, PIPE_BUF %ld\n", pipe_buf);
#else
    int on_linux = 1;
    printf("# Linux, PIPE_BUF %ld\n", pipe_buf);
#endif
    // The control for the timestamp columns: the same fstat pair around a
    // write whose reader is open, which on Darwin moves mtime and ctime (on
    // Linux nothing moves either way).
    {
        int p[2];
        pipe(p);
        struct stat before, after;
        fstat(p[1], &before);
        usleep(20 * 1000);
        ssize_t r = write(p[1], buf, 1);
        fstat(p[1], &after);
        printf("# control, reader open: write -> %zd; moved m%d c%d a%d\n", r, !same(MTIME(before), MTIME(after)),
               !same(CTIME(before), CTIME(after)), !same(ATIME(before), ATIME(after)));
        close(p[0]);
        close(p[1]);
    }
    size_t counts[] = {0, 1, (size_t)pipe_buf, (size_t)pipe_buf + 1, 65536, 100000};
    int rows = 0, broken = 0;
    for (int fill = 0; fill < 3; fill++)
        for (int nonblock = 0; nonblock < 2; nonblock++)
            for (int c = 0; c < 6; c++)
                for (int which = 0; which < 3; which++)
                    for (int how = 0; how < 3; how++) {
                        int report[2];
                        pipe(report);
                        fflush(stdout);
                        pid_t pid = fork();
                        if (pid == 0) {
                            close(report[0]);
                            child(report[1], fill, nonblock, counts[c], which, how);
                        }
                        close(report[1]);
                        row got;
                        ssize_t k = read(report[0], &got, sizeof got);
                        close(report[0]);
                        int ws = 0;
                        waitpid(pid, &ws, 0);
                        int died = WIFSIGNALED(ws) ? WTERMSIG(ws) : 0;
                        printf("fill %-5s nonblock %d count %6zu buffer %-5s %-7s ", fills[fill], nonblock, counts[c],
                               buffers[which], dispositions[how]);
                        // The rule: a write of 0 bytes on Linux answers 0
                        // and raises nothing; every other write answers
                        // EPIPE and raises SIGPIPE before returning; nothing
                        // stays pending; no timestamp moves.
                        int raises = !(on_linux && counts[c] == 0);
                        int ok;
                        if (died) {
                            printf("died of signal %d\n", died);
                            ok = raises && how == 2 && died == SIGPIPE;
                        } else if (k != sizeof got) {
                            printf("no report (read %zd), status 0x%x\n", k, ws);
                            ok = 0;
                        } else {
                            printf("-> %ld errno %d; handler %d before return %d; pending %d; moved m%d c%d a%d; "
                                   "revents 0x%x\n",
                                   got.r, got.e, got.handled, got.before, got.pending, got.mtime, got.ctime,
                                   got.atime, got.revents);
                            ok = !got.pending && !got.mtime && !got.ctime && !got.atime &&
                                 (raises ? (got.r == -1 && got.e == EPIPE && how != 2 &&
                                            got.handled == (how == 1) && (how != 1 || got.before == 1))
                                         : (got.r == 0 && got.handled == 0));
                        }
                        rows++;
                        if (!ok) {
                            broken++;
                            printf("  ^ breaks the rule\n");
                        }
                    }
    printf("# %d rows, %d break the rule\n", rows, broken);
    return 0;
}
