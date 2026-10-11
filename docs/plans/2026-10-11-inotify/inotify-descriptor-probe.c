// What Linux answers for the calls that reach an inotify descriptor without
// being inotify's own, for stage 3 of docs/plans/2026-10-11-inotify.md: the
// arms every descriptor kind needs, and what a sleeping read does when it
// wakes to find its description changed or its record too large.
//
// Every line is `SECTION<TAB>case<TAB>result`, as in inotify-probe.c.
//
// Sections:
//   D  calls through the descriptor: isatty, pread, pwrite, FIONREAD to NULL,
//      fadvise, ftruncate, FICLONE, getdents64, recv, write of nothing
//   N  F_SETFL O_NONBLOCK on the description
//   S  sleeping reads: woken by a record larger than the count; two sleepers
//      on one record after the description became non-blocking
//   P  readiness asked with every bit, through poll and epoll
//
// Build and run, from this directory:
//   container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -pthread -o /tmp/probe-bin /probe/inotify-descriptor-probe.c && mkdir -p /tmp/run && cd /tmp/run && /tmp/probe-bin'
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/fs.h>
#include <poll.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <unistd.h>

static void check(const char *what, long result)
{
    if (result < 0) { perror(what); exit(1); }
}

static const char *en(int e)
{
    switch (e) {
    case 0: return "0";
    case EINVAL: return "EINVAL";
    case EAGAIN: return "EAGAIN";
    case EBADF: return "EBADF";
    case ENOTTY: return "ENOTTY";
    case ESPIPE: return "ESPIPE";
    case EFAULT: return "EFAULT";
    case ENOTDIR: return "ENOTDIR";
    case ENOTSOCK: return "ENOTSOCK";
    case EXDEV: return "EXDEV";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

static const char *rc(long result)
{
    static char b[8][48];
    static int k = 0;
    k = (k + 1) % 8;
    if (result < 0) snprintf(b[k], sizeof b[k], "-1 %s", en(errno));
    else snprintf(b[k], sizeof b[k], "%ld", result);
    return b[k];
}

static void line(const char *section, const char *name, const char *fmt, ...)
{
    printf("%s\t%s\t", section, name);
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
    fflush(stdout);
}

static void fresh(const char *dir)
{
    char cmd[512];
    snprintf(cmd, sizeof cmd, "rm -rf %s && mkdir -p %s", dir, dir);
    if (system(cmd) != 0) exit(1);
}

static void touch(const char *path)
{
    int fd = open(path, O_WRONLY | O_CREAT, 0644);
    check(path, fd);
    close(fd);
}

static void sectionD(void)
{
    fresh("d");
    int fd = inotify_init1(IN_NONBLOCK);
    int other = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "d", IN_CREATE));
    char buf[256];
    line("D", "isatty", "isatty=%d errno=%s", isatty(fd), en(errno));
    line("D", "pread-empty", "%s", rc(pread(fd, buf, sizeof buf, 0)));
    touch("d/a");
    line("D", "pread-with-record", "%s", rc(pread(fd, buf, sizeof buf, 0)));
    int avail = -1;
    ioctl(fd, FIONREAD, &avail);
    line("D", "after-pread", "FIONREAD=%d", avail);
    line("D", "pwrite", "%s", rc(pwrite(fd, "x", 1, 0)));
    line("D", "write-0", "%s", rc(write(fd, NULL, 0)));
    line("D", "fionread-null", "%s", rc(ioctl(fd, FIONREAD, NULL)));
    line("D", "fadvise", "%s", en(posix_fadvise(fd, 0, 0, POSIX_FADV_NORMAL)));
    line("D", "ftruncate", "%s", rc(ftruncate(fd, 0)));
    line("D", "ficlone-two-instances", "%s", rc(ioctl(fd, FICLONE, other)));
    line("D", "getdents64", "%s", rc(syscall(SYS_getdents64, fd, buf, sizeof buf)));
    line("D", "recv", "%s", rc(recv(fd, buf, sizeof buf, 0)));
    line("D", "fsync", "%s", rc(fsync(fd)));
    close(other);
    close(fd);
}

static void sectionN(void)
{
    fresh("n");
    int fd = inotify_init1(0);
    check("inotify_add_watch", inotify_add_watch(fd, "n", IN_CREATE));
    line("N", "setfl-nonblock", "%s", rc(fcntl(fd, F_SETFL, O_NONBLOCK)));
    line("N", "getfl", "0%o", fcntl(fd, F_GETFL));
    char buf[256];
    line("N", "read-empty", "%s", rc(read(fd, buf, sizeof buf)));
    close(fd);
}

static int sharedFd;
static volatile long results[2];
static volatile int errnos[2];
static volatile int done[2];
static size_t counts[2];

static void *reader(void *arg)
{
    int i = (int)(long)arg;
    char buf[1024];
    long n = read(sharedFd, buf, counts[i]);
    errnos[i] = errno;
    results[i] = n;
    done[i] = 1;
    return NULL;
}

static void sectionS(void)
{
    fresh("s");
    sharedFd = inotify_init1(0);
    check("inotify_add_watch", inotify_add_watch(sharedFd, "s", IN_CREATE));
    pthread_t t[2];
    counts[0] = 10;
    done[0] = 0;
    check("pthread_create", pthread_create(&t[0], NULL, reader, (void *)0));
    usleep(100000);
    line("S", "small-count-asleep", "returned=%d", done[0]);
    touch("s/a");
    usleep(100000);
    line("S", "small-count-woken", "returned=%d result=%s", done[0], done[0] ? (results[0] < 0 ? en(errnos[0]) : "bytes") : "-");
    pthread_join(t[0], NULL);
    int avail = -1;
    ioctl(sharedFd, FIONREAD, &avail);
    line("S", "small-count-after", "FIONREAD=%d", avail);
    char drainBuf[1024];
    check("read", read(sharedFd, drainBuf, sizeof drainBuf));

    // Two sleepers; the description becomes non-blocking; one record.
    for (int i = 0; i < 2; i++) { counts[i] = 1024; done[i] = 0; }
    for (int i = 0; i < 2; i++) check("pthread_create", pthread_create(&t[i], NULL, reader, (void *)(long)i));
    usleep(100000);
    check("fcntl", fcntl(sharedFd, F_SETFL, O_NONBLOCK));
    usleep(50000);
    line("S", "after-setfl", "returned=%d,%d", done[0], done[1]);
    touch("s/b");
    usleep(100000);
    int got = 0, eagain = 0, other = 0;
    for (int i = 0; i < 2; i++) {
        if (!done[i]) continue;
        if (results[i] == 32) got++;
        else if (results[i] < 0 && errnos[i] == EAGAIN) eagain++;
        else other++;
    }
    line("S", "two-sleepers-one-record", "returned=%d,%d record=%d EAGAIN=%d other=%d", done[0], done[1], got, eagain, other);
    // Release any still asleep.
    touch("s/c");
    touch("s/d");
    for (int i = 0; i < 2; i++) pthread_join(t[i], NULL);
    close(sharedFd);
}

static void sectionP(void)
{
    fresh("p");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "p", IN_CREATE));
    touch("p/x");
    struct pollfd p = { fd, (short)0xffff, 0 };
    int r = poll(&p, 1, 0);
    line("P", "poll-every-bit", "poll=%d revents=0x%x", r, p.revents);
    int ep = epoll_create1(0);
    struct epoll_event ev = { 0xffffffffu & ~(EPOLLEXCLUSIVE | EPOLLWAKEUP | EPOLLONESHOT | EPOLLET), { .u64 = 1 } };
    line("P", "epoll-ctl-every-bit", "%s", rc(epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev)));
    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 0);
    line("P", "epoll-every-bit", "n=%d events=0x%x", n, n > 0 ? out[0].events : 0);
    close(ep);
    close(fd);
}

int main(void)
{
    if (system("uname -rm") != 0) return 1;
    sectionD();
    sectionN();
    sectionS();
    sectionP();
    return 0;
}
