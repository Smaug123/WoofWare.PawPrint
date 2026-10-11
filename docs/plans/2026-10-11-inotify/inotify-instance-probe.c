// What Linux's inotify answers for the questions stage 3 of
// docs/plans/2026-10-11-inotify.md left open: the instance itself, the queue
// rules, and the order of inotify_add_watch's screens.
//
// Every line is `SECTION<TAB>case<TAB>result`, as in inotify-probe.c.
//
// Sections:
//   F  inotify_init1's flags: which bits are accepted
//   G  inotify_add_watch's screens, and their order
//   M  what a mask of only unknown or kernel-added bits does
//   U  per-user limits, from a uid that holds nothing else (65534)
//   Q  the queue: an identical record arriving at a full queue, records after
//      a partial read of an overflowed queue, a merge of records differing
//      only in cookie
//   T  edge-triggered epoll: does each queued record wake, and a merged one?
//   X  read: a partial fault, a count of 0 with a null buffer
//
// Build and run, from this directory, as root (section U drops to 65534 in a
// child):
//   container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/probe-bin /probe/inotify-instance-probe.c && mkdir -p /tmp/run && chmod 0777 /tmp/run && cd /tmp/run && /tmp/probe-bin'
//
// Lines ending `~boot` depend on the machine and are not compared between runs.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>
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
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EMFILE: return "EMFILE";
    case ENOSPC: return "ENOSPC";
    case EACCES: return "EACCES";
    case EFAULT: return "EFAULT";
    case EEXIST: return "EEXIST";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case ELOOP: return "ELOOP";
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
    snprintf(cmd, sizeof cmd, "rm -rf %s && mkdir -p %s && chmod 0777 %s", dir, dir, dir);
    if (system(cmd) != 0) exit(1);
}

static void touch(const char *path)
{
    int fd = open(path, O_WRONLY | O_CREAT, 0644);
    check(path, fd);
    close(fd);
}

static void sectionF(void)
{
    char accepted[512] = "";
    for (int bit = 0; bit < 32; bit++) {
        int fd = inotify_init1(1 << bit);
        if (fd >= 0) {
            char b[16];
            snprintf(b, sizeof b, "%s0%o", accepted[0] ? "," : "", 1u << bit);
            strcat(accepted, b);
            close(fd);
        } else if (errno != EINVAL) {
            char b[48];
            snprintf(b, sizeof b, "%sbit%d:%s", accepted[0] ? "," : "", bit, en(errno));
            strcat(accepted, b);
        }
    }
    line("F", "single-bits-accepted", "%s (others EINVAL)", accepted);
    int both = inotify_init1(IN_CLOEXEC | IN_NONBLOCK);
    line("F", "cloexec-nonblock", "%s", both >= 0 ? "ok" : rc(both));
    if (both >= 0) close(both);
    int extra = inotify_init1(IN_CLOEXEC | 1);
    line("F", "cloexec-plus-bit0", "%s", rc(extra));
    int zero = inotify_init1(0);
    int fl = fcntl(zero, F_GETFL), fdf = fcntl(zero, F_GETFD);
    line("F", "zero", "F_GETFL=0%o F_GETFD=%d", fl, fdf);
    close(zero);
    line("F", "negative-one", "%s", rc(inotify_init1(-1)));
}

static void sectionG(void)
{
    fresh("g");
    touch("g/f");
    int fd = inotify_init1(IN_NONBLOCK);
    int reg = open("g/f", O_RDONLY);
    line("G", "bad-fd-mask-0", "add(999, g, 0)=%s", rc(inotify_add_watch(999, "g", 0)));
    line("G", "bad-fd-add-and-create", "add(999, g, CREATE|MASK_ADD|MASK_CREATE)=%s", rc(inotify_add_watch(999, "g", IN_CREATE | IN_MASK_ADD | IN_MASK_CREATE)));
    line("G", "add-and-create", "add(fd, g, CREATE|MASK_ADD|MASK_CREATE)=%s", rc(inotify_add_watch(fd, "g", IN_CREATE | IN_MASK_ADD | IN_MASK_CREATE)));
    line("G", "bad-fd-missing-path", "add(999, missing, CREATE)=%s", rc(inotify_add_watch(999, "g/missing", IN_CREATE)));
    line("G", "regular-fd-missing-path", "add(regular, missing, CREATE)=%s", rc(inotify_add_watch(reg, "g/missing", IN_CREATE)));
    line("G", "regular-fd-mask-0", "add(regular, g, 0)=%s", rc(inotify_add_watch(reg, "g", 0)));
    line("G", "null-path", "add(fd, NULL, CREATE)=%s", rc(inotify_add_watch(fd, NULL, IN_CREATE)));
    line("G", "null-path-mask-0", "add(fd, NULL, 0)=%s", rc(inotify_add_watch(fd, NULL, 0)));
    line("G", "empty-path", "add(fd, \"\", CREATE)=%s", rc(inotify_add_watch(fd, "", IN_CREATE)));
    line("G", "through-file", "add(fd, g/f/x, CREATE)=%s", rc(inotify_add_watch(fd, "g/f/x", IN_CREATE)));
    line("G", "missing-mask-0", "add(fd, missing, 0)=%s", rc(inotify_add_watch(fd, "g/missing", 0)));
    int epfd = epoll_create1(0);
    line("G", "epoll-fd", "add(epoll, g, CREATE)=%s", rc(inotify_add_watch(epfd, "g", IN_CREATE)));
    line("G", "rm-bad-fd", "rm(999, 1)=%s", rc(inotify_rm_watch(999, 1)));
    line("G", "rm-regular-fd", "rm(regular, 1)=%s", rc(inotify_rm_watch(reg, 1)));
    line("G", "rm-zero", "rm(fd, 0)=%s", rc(inotify_rm_watch(fd, 0)));
    // Read permission on the watched inode, from a uid that is not the owner.
    fresh("g/perm");
    check("mkdir", mkdir("g/perm/r", 0755));
    check("mkdir", mkdir("g/perm/x-only", 0711));
    check("mkdir", mkdir("g/perm/none", 0700));
    check("mkdir", mkdir("g/perm/w-only", 0733));
    int wfile = open("g/perm/file-0200", O_WRONLY | O_CREAT, 0200);
    close(wfile);
    pid_t child = fork();
    if (child == 0) {
        if (setresgid(65534, 65534, 65534) != 0 || setresuid(65534, 65534, 65534) != 0) { perror("setresuid"); _exit(1); }
        int cfd = inotify_init1(IN_NONBLOCK);
        line("G", "perm-readable", "uid 65534 add(0755 dir)=%s", rc(inotify_add_watch(cfd, "g/perm/r", IN_CREATE)));
        line("G", "perm-search-only", "uid 65534 add(0711 dir)=%s", rc(inotify_add_watch(cfd, "g/perm/x-only", IN_CREATE)));
        line("G", "perm-none", "uid 65534 add(0700 dir)=%s", rc(inotify_add_watch(cfd, "g/perm/none", IN_CREATE)));
        line("G", "perm-write-search", "uid 65534 add(0733 dir)=%s", rc(inotify_add_watch(cfd, "g/perm/w-only", IN_CREATE)));
        line("G", "perm-write-only-file", "uid 65534 add(0200 file)=%s", rc(inotify_add_watch(cfd, "g/perm/file-0200", IN_CREATE)));
        line("G", "perm-none-mask-0", "uid 65534 add(0700 dir, 0)=%s", rc(inotify_add_watch(cfd, "g/perm/none", 0)));
        _exit(0);
    }
    int status;
    waitpid(child, &status, 0);
    close(epfd); close(reg); close(fd);
}

static void sectionM(void)
{
    fresh("m");
    int fd = inotify_init1(IN_NONBLOCK);
    struct { uint32_t mask; const char *name; } masks[] = {
        { 0x1000, "0x1000" }, { 0x08000000, "0x08000000" }, { IN_IGNORED, "IGNORED" },
        { IN_ISDIR, "ISDIR" }, { IN_Q_OVERFLOW, "Q_OVERFLOW" }, { IN_UNMOUNT, "UNMOUNT" },
        { IN_ONESHOT, "ONESHOT" }, { IN_EXCL_UNLINK, "EXCL_UNLINK" }, { IN_DONT_FOLLOW, "DONT_FOLLOW" },
        { IN_MASK_ADD, "MASK_ADD" }, { IN_MASK_CREATE, "MASK_CREATE" },
        { IN_CREATE | 0x1000, "CREATE|0x1000" }, { IN_CREATE | 0x08000000, "CREATE|0x08000000" },
    };
    for (size_t i = 0; i < sizeof masks / sizeof masks[0]; i++) {
        char sub[64];
        snprintf(sub, sizeof sub, "m/%zu", i);
        check("mkdir", mkdir(sub, 0755));
        line("M", masks[i].name, "%s", rc(inotify_add_watch(fd, sub, masks[i].mask)));
    }
    // A mask of only kernel-added bits, re-added over an existing watch: does it clear the events?
    check("mkdir", mkdir("m/replace", 0755));
    int wd = inotify_add_watch(fd, "m/replace", IN_CREATE);
    int again = inotify_add_watch(fd, "m/replace", IN_ISDIR);
    touch("m/replace/x");
    char buf[4096];
    ssize_t n = read(fd, buf, sizeof buf);
    line("M", "replace-with-isdir-alone", "wd=%d again=%s then create: read=%s", wd, rc(again), rc(n));
    close(fd);
}

static void sectionU(void)
{
    fresh("u");
    pid_t child = fork();
    if (child == 0) {
        if (setresgid(65534, 65534, 65534) != 0 || setresuid(65534, 65534, 65534) != 0) { perror("setresuid"); _exit(1); }
        int fds[1024];
        int n = 0, err = 0;
        while (n < 1024) {
            int fd = inotify_init1(IN_CLOEXEC);
            if (fd < 0) { err = errno; break; }
            fds[n++] = fd;
        }
        line("U", "instances", "uid 65534 created %d before %s", n, en(err));
        for (int i = 1; i < n; i++) close(fds[i]);
        // Watches against one instance, counted to the limit.
        long watches = 0;
        err = 0;
        for (long i = 0; i < 20000; i++) {
            char path[64];
            snprintf(path, sizeof path, "u/%ld", i);
            if (mkdir(path, 0755) != 0) { err = -errno; break; }
            if (inotify_add_watch(fds[0], path, IN_CREATE) < 0) { err = errno; break; }
            watches++;
        }
        line("U", "watches", "uid 65534 added %ld before %s", watches, err > 0 ? en(err) : "mkdir failure");
        // A second instance: its watches share the per-user count.
        int second = inotify_init1(IN_CLOEXEC);
        line("U", "watch-on-second-instance", "%s", rc(inotify_add_watch(second, "u", IN_CREATE)));
        // Re-adding an existing watch at the limit: an update, not a new watch.
        line("U", "re-add-at-limit", "%s", rc(inotify_add_watch(fds[0], "u/0", IN_DELETE)));
        // After removing one, one more fits.
        check("rm", inotify_rm_watch(fds[0], 1));
        line("U", "after-rm", "%s", rc(inotify_add_watch(second, "u", IN_CREATE)));
        _exit(0);
    }
    int status;
    waitpid(child, &status, 0);
    if (system("rm -rf u") != 0) exit(1);
}

// Read up to `size` bytes; the number of records and how many are Q_OVERFLOW, with the index of the first.
static void countRecords(int fd, size_t size, int *records, int *overflows, int *firstOverflow)
{
    static char buf[1 << 20];
    *records = 0; *overflows = 0; *firstOverflow = -1;
    for (;;) {
        ssize_t n = read(fd, buf, size);
        if (n <= 0) return;
        ssize_t pos = 0;
        while (pos < n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            if (e->mask & IN_Q_OVERFLOW) { if (*firstOverflow < 0) *firstOverflow = *records; (*overflows)++; }
            (*records)++;
            pos += sizeof *e + e->len;
        }
    }
}

static void sectionQ(void)
{
    fresh("q");
    touch("q/f");
    touch("q/g");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "q", IN_ATTRIB));
    int max = 16384;
    // Fill exactly to the limit with alternating records, the last being g's.
    for (int i = 0; i < max; i++) check("chmod", chmod(i % 2 == 0 ? "q/f" : "q/g", (i / 2) % 2 == 0 ? 0600 : 0644));
    int avail = -1;
    ioctl(fd, FIONREAD, &avail);
    line("Q", "full", "FIONREAD=%d", avail);
    // A record identical to the last, at a full queue.
    check("chmod", chmod("q/g", 0600));
    ioctl(fd, FIONREAD, &avail);
    line("Q", "identical-at-full", "FIONREAD=%d (full was %d, +16 is an overflow record)", avail, max * 32);
    int records, overflows, first;
    countRecords(fd, 1 << 20, &records, &overflows, &first);
    line("Q", "identical-at-full-drained", "records=%d overflows=%d first-overflow=%d", records, overflows, first);
    // Overflow, then read two records, then a new record.
    for (int i = 0; i < max + 10; i++) check("chmod", chmod(i % 2 == 0 ? "q/f" : "q/g", (i / 2) % 2 == 0 ? 0600 : 0644));
    char small[64];
    ssize_t n = read(fd, small, 64);
    line("Q", "partial-read", "read(64)=%s", rc(n));
    touch("q/h");
    check("chmod", chmod("q/h", 0600));
    check("chmod", chmod("q/f", 0600));
    countRecords(fd, 1 << 20, &records, &overflows, &first);
    line("Q", "after-partial-read", "records=%d overflows=%d first-overflow=%d (two read before)", records, overflows, first);
    // Overflow, read one record, then a new record.
    for (int i = 0; i < max + 10; i++) check("chmod", chmod(i % 2 == 0 ? "q/f" : "q/g", (i / 2) % 2 == 0 ? 0600 : 0644));
    n = read(fd, small, 32);
    line("Q", "read-one", "read(32)=%s", rc(n));
    check("chmod", chmod("q/h", 0644));
    countRecords(fd, 1 << 20, &records, &overflows, &first);
    line("Q", "after-read-one", "records=%d overflows=%d first-overflow=%d (one read before)", records, overflows, first);
    close(fd);
    // Two MOVED_FROM records for one name, differing only in cookie.
    fresh("q2");
    fresh("q2out");
    fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "q2", IN_MOVED_FROM));
    touch("q2/x");
    check("rename", rename("q2/x", "q2out/x1"));
    touch("q2/x");
    check("rename", rename("q2/x", "q2out/x2"));
    countRecords(fd, 1 << 20, &records, &overflows, &first);
    line("Q", "moved-from-twice", "records=%d", records);
    close(fd);
}

static void sectionT(void)
{
    fresh("t");
    touch("t/f");
    touch("t/g");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "t", IN_ATTRIB));
    int ep = epoll_create1(0);
    struct epoll_event ev = { EPOLLIN | EPOLLET, { .u64 = 9 } };
    check("epoll_ctl", epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev));
    struct epoll_event out[4];
    check("chmod", chmod("t/f", 0600));
    int a = epoll_wait(ep, out, 4, 0);
    int b = epoll_wait(ep, out, 4, 0);
    check("chmod", chmod("t/g", 0600));
    int c = epoll_wait(ep, out, 4, 0);
    check("chmod", chmod("t/g", 0644));
    int d = epoll_wait(ep, out, 4, 0);
    line("T", "edge", "first record=%d again=%d second record=%d merged record=%d", a, b, c, d);
    close(ep);
    close(fd);
}

static void sectionX(void)
{
    fresh("x");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "x", IN_CREATE));
    touch("x/a");
    line("X", "null-count-0", "read(NULL, 0)=%s", rc(read(fd, NULL, 0)));
    int avail = -1;
    ioctl(fd, FIONREAD, &avail);
    line("X", "after-null-count-0", "FIONREAD=%d", avail);
    touch("x/b");
    // A buffer of two pages, the second unmapped: the first record fits in the
    // first page's last 32 bytes, the second would cross into the hole.
    long page = sysconf(_SC_PAGESIZE);
    char *region = mmap(NULL, 2 * page, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    check("munmap", munmap(region + page, page));
    ioctl(fd, FIONREAD, &avail);
    ssize_t n = read(fd, region + page - 32, 1024);
    int after = -1;
    ioctl(fd, FIONREAD, &after);
    line("X", "partial-fault", "FIONREAD before=%d read=%s FIONREAD after=%d", avail, rc(n), after);
    close(fd);
}

int main(void)
{
    if (system("uname -rm") != 0) return 1;
    sectionF();
    sectionG();
    sectionM();
    sectionU();
    sectionQ();
    sectionT();
    sectionX();
    return 0;
}
