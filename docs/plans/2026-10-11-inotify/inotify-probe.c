// What Linux's inotify answers, measured for docs/plans/2026-10-11-inotify.md.
//
// Every line is `SECTION<TAB>case<TAB>result`. An event is rendered as
// `wd=N mask=NAMES cookie=C len=L name="..."`, where a cookie is numbered in the
// order it was first seen (k1, k2, ...) since its absolute value counts every
// rename since boot, and `|` separates the events of one read. `drain` reads a
// non-blocking descriptor until EAGAIN, one 64 KiB read at a time.
//
// Sections:
//   I  identity of the descriptor: fstat, flock contention with an epoll
//      descriptor, lseek, F_GETFL/F_GETFD, poll and FIONREAD on an empty queue
//   W  watches: wd numbering, re-adding, ONLYDIR, DONT_FOLLOW, MASK_ADD,
//      rm_watch and IN_IGNORED, errors
//   E  the events each filesystem operation raises under IN_ALL_EVENTS, on a
//      watched directory
//   N  the same operations under the mask .NET's FileSystemWatcher uses
//   F  a watch on a file rather than a directory
//   D  deleting and renaming watched things: event order, IN_DELETE_SELF,
//      IN_IGNORED, an unlinked file still open
//   H  hard links and the dentry a file was opened through: which name an
//      IN_MODIFY carries, after a link, a rename and an unlink
//   L  record layout: the len field for names of several lengths
//   C  coalescing of identical consecutive events
//   R  read: a buffer too small, one that fits one event of two, count 0, EAGAIN
//   P  poll and epoll on the descriptor
//   K  cookies: adjacent renames, and one rename seen by two instances
//   A  two instances on one directory, and a dup of the descriptor
//   O  queue overflow: /proc/sys/fs/inotify, how many events fit, the
//      IN_Q_OVERFLOW record, and events after draining
//   B  a thread blocked in read: woken by rm_watch, left asleep by close,
//      ended by a signal
//   S  per-user instance limit
//
// Build and run, from this directory:
//   container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -pthread -o /tmp/probe-bin /probe/inotify-probe.c && mkdir -p /tmp/run && cd /tmp/run && /tmp/probe-bin'
//
// Lines ending `~boot` carry values that depend on the machine or on what
// happened since it booted, and are not to be compared between runs.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/file.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

static void check(const char *what, int result)
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
    case ESPIPE: return "ESPIPE";
    case EINTR: return "EINTR";
    case EPERM: return "EPERM";
    case EEXIST: return "EEXIST";
    case EFAULT: return "EFAULT";
    default: { static char b[32]; snprintf(b, sizeof b, "errno%d", e); return b; }
    }
}

// The result of a call that returns -1 and sets errno, or a non-negative number.
static const char *rc(long result)
{
    static char b[8][48];
    static int k = 0;
    k = (k + 1) % 8;
    if (result < 0) snprintf(b[k], sizeof b[k], "-1 %s", en(errno));
    else snprintf(b[k], sizeof b[k], "%ld", result);
    return b[k];
}

static const char *maskNames(uint32_t mask)
{
    static char b[4][256];
    static int k = 0;
    k = (k + 1) % 4;
    char *out = b[k];
    out[0] = 0;
    struct { uint32_t bit; const char *name; } bits[] = {
        { IN_ACCESS, "ACCESS" }, { IN_MODIFY, "MODIFY" }, { IN_ATTRIB, "ATTRIB" },
        { IN_CLOSE_WRITE, "CLOSE_WRITE" }, { IN_CLOSE_NOWRITE, "CLOSE_NOWRITE" },
        { IN_OPEN, "OPEN" }, { IN_MOVED_FROM, "MOVED_FROM" }, { IN_MOVED_TO, "MOVED_TO" },
        { IN_CREATE, "CREATE" }, { IN_DELETE, "DELETE" }, { IN_DELETE_SELF, "DELETE_SELF" },
        { IN_MOVE_SELF, "MOVE_SELF" }, { IN_UNMOUNT, "UNMOUNT" }, { IN_Q_OVERFLOW, "Q_OVERFLOW" },
        { IN_IGNORED, "IGNORED" }, { IN_ISDIR, "ISDIR" },
    };
    uint32_t seen = 0;
    for (size_t i = 0; i < sizeof bits / sizeof bits[0]; i++) {
        if (mask & bits[i].bit) {
            if (out[0]) strcat(out, "+");
            strcat(out, bits[i].name);
            seen |= bits[i].bit;
        }
    }
    if (mask & ~seen) { char extra[32]; snprintf(extra, sizeof extra, "%s0x%x", out[0] ? "+" : "", mask & ~seen); strcat(out, extra); }
    if (!out[0]) strcpy(out, "0");
    return out;
}

// Cookies, numbered in order of first sight.
static uint32_t cookies[64];
static int cookieCount = 0;
static const char *cookieName(uint32_t cookie)
{
    static char b[4][16];
    static int k = 0;
    k = (k + 1) % 4;
    if (cookie == 0) return "0";
    for (int i = 0; i < cookieCount; i++) if (cookies[i] == cookie) { snprintf(b[k], sizeof b[k], "k%d", i + 1); return b[k]; }
    cookies[cookieCount++] = cookie;
    snprintf(b[k], sizeof b[k], "k%d", cookieCount);
    return b[k];
}

static char rendered[1 << 16];

// Render the events in buf[0, n) into `rendered`, appending.
static int renderEvents(const char *buf, ssize_t n)
{
    int count = 0;
    ssize_t pos = 0;
    while (pos + (ssize_t)sizeof(struct inotify_event) <= n) {
        const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
        char line[512];
        snprintf(line, sizeof line, "%swd=%d mask=%s cookie=%s len=%u name=\"%s\"",
                 rendered[0] ? " | " : "", e->wd, maskNames(e->mask), cookieName(e->cookie), e->len, e->len ? e->name : "");
        strcat(rendered, line);
        pos += sizeof(struct inotify_event) + e->len;
        count++;
    }
    if (pos != n) { char line[64]; snprintf(line, sizeof line, " | TRAILING %zd BYTES", n - pos); strcat(rendered, line); }
    return count;
}

// Read a non-blocking descriptor until EAGAIN; the rendered events, and how many.
static int drainCount;
static const char *drain(int fd)
{
    static char buf[1 << 16];
    rendered[0] = 0;
    drainCount = 0;
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) {
            if (errno == EAGAIN) break;
            char line[64]; snprintf(line, sizeof line, "%sREAD %s", rendered[0] ? " | " : "", en(errno)); strcat(rendered, line);
            break;
        }
        if (n == 0) { strcat(rendered, rendered[0] ? " | READ 0" : "READ 0"); break; }
        drainCount += renderEvents(buf, n);
    }
    if (!rendered[0]) strcpy(rendered, "(none)");
    return rendered;
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

static void writeFile(const char *path, const char *text)
{
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    check(path, fd);
    check("write", (int)write(fd, text, strlen(text)));
    check("close", close(fd));
}

static void fresh(const char *dir)
{
    char cmd[512];
    snprintf(cmd, sizeof cmd, "rm -rf %s && mkdir -p %s", dir, dir);
    if (system(cmd) != 0) exit(1);
}

static const uint32_t dotnetMask = IN_CREATE | IN_DELETE | IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | IN_ONLYDIR | IN_EXCL_UNLINK;

static void sectionI(void)
{
    int a = inotify_init1(IN_CLOEXEC);
    int b = inotify_init1(IN_CLOEXEC | IN_NONBLOCK);
    int ep = epoll_create1(0);
    check("inotify_init1", a); check("inotify_init1", b); check("epoll_create1", ep);
    struct stat sa, sb, se;
    check("fstat", fstat(a, &sa)); check("fstat", fstat(b, &sb)); check("fstat", fstat(ep, &se));
    line("I", "fstat", "two inotify fds: same st_dev=%d same st_ino=%d; inotify vs epoll: same st_dev=%d same st_ino=%d; mode=%o nlink=%d size=%ld",
         sa.st_dev == sb.st_dev, sa.st_ino == sb.st_ino, sa.st_dev == se.st_dev, sa.st_ino == se.st_ino, (unsigned)sa.st_mode, (int)sa.st_nlink, (long)sa.st_size);
    line("I", "fstat-values", "st_dev=%lu st_ino=%lu ~boot", (unsigned long)sa.st_dev, (unsigned long)sa.st_ino);
    line("I", "flock", "epoll LOCK_EX|LOCK_NB=%s; then inotify LOCK_EX|LOCK_NB=%s", rc(flock(ep, LOCK_EX | LOCK_NB)), rc(flock(a, LOCK_EX | LOCK_NB)));
    check("flock", flock(ep, LOCK_UN));
    line("I", "flock-after-unlock", "inotify LOCK_EX|LOCK_NB=%s; second inotify LOCK_EX|LOCK_NB=%s", rc(flock(a, LOCK_EX | LOCK_NB)), rc(flock(b, LOCK_EX | LOCK_NB)));
    line("I", "lseek", "SEEK_SET 0=%s SEEK_CUR 0=%s SEEK_END 0=%s SEEK_SET 5=%s", rc(lseek(a, 0, SEEK_SET)), rc(lseek(a, 0, SEEK_CUR)), rc(lseek(a, 0, SEEK_END)), rc(lseek(a, 5, SEEK_SET)));
    int fl = fcntl(a, F_GETFL), fd = fcntl(a, F_GETFD), flb = fcntl(b, F_GETFL);
    line("I", "fcntl", "IN_CLOEXEC: F_GETFL=0%o (O_NONBLOCK=%d accmode=%d) F_GETFD=%d; IN_CLOEXEC|IN_NONBLOCK: F_GETFL=0%o (O_NONBLOCK=%d)", fl, (fl & O_NONBLOCK) != 0, fl & O_ACCMODE, fd, flb, (flb & O_NONBLOCK) != 0);
    struct pollfd p = { a, POLLIN | POLLOUT | POLLPRI | POLLRDHUP, 0 };
    int pr = poll(&p, 1, 0);
    int avail = -1;
    int ir = ioctl(a, FIONREAD, &avail);
    line("I", "empty", "poll(IN|OUT|PRI|RDHUP, 0)=%d revents=0x%x; FIONREAD=%s avail=%d", pr, p.revents, rc(ir), avail);
    char buf[64];
    line("I", "read-nonblocking-empty", "read(64)=%s", rc(read(b, buf, sizeof buf)));
    line("I", "write", "write(inotify, 1 byte)=%s", rc(write(a, "x", 1)));
    close(a); close(b); close(ep);
}

static void sectionW(void)
{
    fresh("w");
    check("mkdir", mkdir("w/d1", 0755));
    check("mkdir", mkdir("w/d2", 0755));
    writeFile("w/f", "x");
    check("symlink", symlink("d1", "w/l"));
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_init1", fd);
    int wd1 = inotify_add_watch(fd, "w/d1", IN_ALL_EVENTS);
    int wd2 = inotify_add_watch(fd, "w/d2", IN_ALL_EVENTS);
    int again = inotify_add_watch(fd, "w/d1", IN_ALL_EVENTS);
    int dotted = inotify_add_watch(fd, "./w/d1/.", IN_ALL_EVENTS);
    line("W", "numbering", "d1=%s d2=%s d1-again=%s ./w/d1/.=%s", rc(wd1), rc(wd2), rc(again), rc(dotted));
    line("W", "events-from-adding", "%s", drain(fd));
    line("W", "onlydir-file", "%s", rc(inotify_add_watch(fd, "w/f", IN_ALL_EVENTS | IN_ONLYDIR)));
    int wf = inotify_add_watch(fd, "w/f", IN_ALL_EVENTS);
    line("W", "file", "%s", rc(wf));
    line("W", "missing", "%s", rc(inotify_add_watch(fd, "w/missing", IN_ALL_EVENTS)));
    line("W", "mask-0", "%s", rc(inotify_add_watch(fd, "w/d1", 0)));
    line("W", "mask-flags-only", "ONLYDIR alone=%s", rc(inotify_add_watch(fd, "w/d2", IN_ONLYDIR)));
    line("W", "symlink-followed", "add w/l (no DONT_FOLLOW)=%s (d1's wd=%d)", rc(inotify_add_watch(fd, "w/l", IN_ALL_EVENTS)), wd1);
    int wl = inotify_add_watch(fd, "w/l", IN_ALL_EVENTS | IN_DONT_FOLLOW);
    line("W", "symlink-dont-follow", "add w/l DONT_FOLLOW=%s", rc(wl));
    line("W", "symlink-dont-follow-onlydir", "add w/l DONT_FOLLOW|ONLYDIR=%s", rc(inotify_add_watch(fd, "w/l", IN_ALL_EVENTS | IN_DONT_FOLLOW | IN_ONLYDIR)));
    line("W", "bad-fd", "add on fd 999=%s; add on a regular file's fd=%s", rc(inotify_add_watch(999, "w/d1", IN_ALL_EVENTS)), rc(inotify_add_watch(open("w/f", O_RDONLY), "w/d1", IN_ALL_EVENTS)));
    // MASK_ADD: replace d2's mask with MODIFY, then add ATTRIB with MASK_ADD, then replace with CREATE.
    int m1 = inotify_add_watch(fd, "w/d2", IN_MODIFY);
    writeFile("w/d2/x", "1");
    check("chmod", chmod("w/d2/x", 0600));
    line("W", "mask-replace", "wd=%d; create+write+chmod under MODIFY only: %s", m1, drain(fd));
    int m2 = inotify_add_watch(fd, "w/d2", IN_ATTRIB | IN_MASK_ADD);
    check("chmod", chmod("w/d2/x", 0644));
    writeFile("w/d2/x", "2");
    line("W", "mask-add", "wd=%d; chmod then write under MODIFY+ATTRIB: %s", m2, drain(fd));
    int m3 = inotify_add_watch(fd, "w/d2", IN_CREATE);
    check("chmod", chmod("w/d2/x", 0600));
    writeFile("w/d2/y", "1");
    line("W", "mask-replaced-again", "wd=%d; chmod then create under CREATE: %s", m3, drain(fd));
    line("W", "mask-create-existing", "add d2 with IN_MASK_CREATE=%s", rc(inotify_add_watch(fd, "w/d2", IN_CREATE | IN_MASK_CREATE)));
    line("W", "rm", "rm wd1=%s; rm wd1 again=%s; rm 999=%s; rm -1=%s", rc(inotify_rm_watch(fd, wd1)), rc(inotify_rm_watch(fd, wd1)), rc(inotify_rm_watch(fd, 999)), rc(inotify_rm_watch(fd, -1)));
    line("W", "rm-events", "%s", drain(fd));
    int wd1b = inotify_add_watch(fd, "w/d1", IN_ALL_EVENTS);
    line("W", "re-add-after-rm", "d1=%s (was %d, highest so far %d)", rc(wd1b), wd1, wl);
    line("W", "re-add-events", "%s", drain(fd));
    close(fd);
}

static void operations(const char *section, const char *dir, uint32_t mask)
{
    char cmd[512];
    snprintf(cmd, sizeof cmd, "rm -rf %s u && mkdir -p %s u && echo seed > u/incoming && echo seed > %s/existing && echo seed > %s/victim && mkdir %s/sub-victim", dir, dir, dir, dir, dir);
    if (system(cmd) != 0) exit(1);
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_init1", fd);
    int wd = inotify_add_watch(fd, dir, mask);
    check("inotify_add_watch", wd);
    char p[256], q[256];
#define P(name) (snprintf(p, sizeof p, "%s/%s", dir, name), p)
#define Q(name) (snprintf(q, sizeof q, "%s/%s", dir, name), q)
    int f = open(P("f"), O_WRONLY | O_CREAT | O_EXCL, 0644);
    check("open", f);
    line(section, "open-creat", "%s", drain(fd));
    check("write", (int)write(f, "hello", 5));
    line(section, "write", "%s", drain(fd));
    check("write", (int)write(f, "", 0));
    line(section, "write-0", "%s", drain(fd));
    check("pwrite", (int)pwrite(f, "p", 1, 0));
    line(section, "pwrite", "%s", drain(fd));
    check("ftruncate", ftruncate(f, 2));
    line(section, "ftruncate", "%s", drain(fd));
    check("close", close(f));
    line(section, "close-write", "%s", drain(fd));
    f = open(P("f"), O_RDONLY);
    check("open", f);
    line(section, "open-rdonly", "%s", drain(fd));
    char buf[8];
    check("read", (int)read(f, buf, sizeof buf));
    line(section, "read", "%s", drain(fd));
    check("read", (int)read(f, buf, sizeof buf));
    line(section, "read-at-eof", "%s", drain(fd));
    check("close", close(f));
    line(section, "close-nowrite", "%s", drain(fd));
    f = open(P("existing"), O_WRONLY | O_TRUNC);
    check("open", f);
    line(section, "open-trunc-existing", "%s", drain(fd));
    close(f);
    line(section, "close-after-trunc", "%s", drain(fd));
    check("chmod", chmod(P("f"), 0600));
    line(section, "chmod", "%s", drain(fd));
    check("chown", chown(P("f"), 1000, 1000));
    line(section, "chown", "%s", drain(fd));
    struct timespec ts[2] = { { 1000, 0 }, { 2000, 0 } };
    check("utimensat", utimensat(AT_FDCWD, P("f"), ts, 0));
    line(section, "utimensat", "%s", drain(fd));
    check("truncate", truncate(P("f"), 1));
    line(section, "truncate", "%s", drain(fd));
    check("chmod-dir", chmod(dir, 0755));
    line(section, "chmod-watched-dir", "%s", drain(fd));
    check("mkdir", mkdir(P("sub"), 0755));
    line(section, "mkdir", "%s", drain(fd));
    writeFile(P("sub/inner"), "x");
    line(section, "write-in-unwatched-subdir", "%s", drain(fd));
    check("unlink", unlink(P("sub/inner")));
    check("rmdir", rmdir(P("sub")));
    line(section, "rmdir", "%s", drain(fd));
    check("symlink", symlink("f", P("lnk")));
    line(section, "symlink", "%s", drain(fd));
    check("link", link(P("f"), Q("hard")));
    line(section, "link", "%s", drain(fd));
    check("unlink", unlink(P("hard")));
    line(section, "unlink-one-of-two-names", "%s", drain(fd));
    check("unlink", unlink(P("lnk")));
    line(section, "unlink-symlink", "%s", drain(fd));
    check("rename", rename(P("f"), Q("g")));
    line(section, "rename-within", "%s", drain(fd));
    check("rename", rename(P("g"), Q("existing")));
    line(section, "rename-over-existing", "%s", drain(fd));
    check("rename", rename("u/incoming", Q("arrived")));
    line(section, "rename-in", "%s", drain(fd));
    check("rename", rename(P("arrived"), "u/departed"));
    line(section, "rename-out", "%s", drain(fd));
    check("rename", rename(P("sub-victim"), Q("sub-renamed")));
    line(section, "rename-dir-within", "%s", drain(fd));
    check("rename", rename(P("existing"), Q("existing")));
    line(section, "rename-noop", "%s", drain(fd));
    check("unlink", unlink(P("existing")));
    line(section, "unlink", "%s", drain(fd));
    check("unlink", unlink(P("victim")));
    check("rmdir", rmdir(P("sub-renamed")));
    line(section, "rmdir-renamed-dir", "%s", drain(fd));
    int wdAgain = inotify_add_watch(fd, dir, mask);
    line(section, "wd-after-all", "add again=%d (was %d)", wdAgain, wd);
    close(fd);
#undef P
#undef Q
}

static void sectionF(void)
{
    fresh("fw");
    writeFile("fw/f", "seed");
    check("link", link("fw/f", "fw/f2"));
    int fd = inotify_init1(IN_NONBLOCK);
    int wd = inotify_add_watch(fd, "fw/f", IN_ALL_EVENTS);
    int wd2 = inotify_add_watch(fd, "fw/f2", IN_ALL_EVENTS);
    line("F", "two-names-one-inode", "wd via f=%d via f2=%d", wd, wd2);
    int f = open("fw/f2", O_WRONLY);
    check("write", (int)write(f, "x", 1));
    close(f);
    line("F", "open-write-close-via-f2", "%s", drain(fd));
    check("chmod", chmod("fw/f", 0600));
    line("F", "chmod", "%s", drain(fd));
    check("rename", rename("fw/f", "fw/g"));
    line("F", "rename-of-watched-file", "%s", drain(fd));
    check("unlink", unlink("fw/f2"));
    line("F", "unlink-other-name", "%s", drain(fd));
    f = open("fw/g", O_RDONLY);
    check("unlink", unlink("fw/g"));
    line("F", "unlink-last-name-while-open", "%s", drain(fd));
    check("close", close(f));
    line("F", "close-last-reference", "%s", drain(fd));
    line("F", "rm-after-delete-self", "rm wd=%s", rc(inotify_rm_watch(fd, wd)));
    close(fd);
}

static void sectionD(void)
{
    fresh("dd");
    check("mkdir", mkdir("dd/sub", 0755));
    writeFile("dd/sub/inner", "x");
    int fd = inotify_init1(IN_NONBLOCK);
    int p = inotify_add_watch(fd, "dd", IN_ALL_EVENTS);
    int s = inotify_add_watch(fd, "dd/sub", IN_ALL_EVENTS);
    line("D", "wds", "parent=%d sub=%d", p, s);
    check("rename", rename("dd/sub", "dd/moved"));
    line("D", "rename-watched-dir", "%s", drain(fd));
    writeFile("dd/moved/after", "x");
    line("D", "create-in-renamed-watched-dir", "%s", drain(fd));
    check("unlink", unlink("dd/moved/inner"));
    check("unlink", unlink("dd/moved/after"));
    line("D", "unlink-inside", "%s", drain(fd));
    check("rmdir", rmdir("dd/moved"));
    line("D", "rmdir-watched-dir", "%s", drain(fd));
    line("D", "rm-after-ignored", "rm sub's wd=%s", rc(inotify_rm_watch(fd, s)));
    check("mkdir", mkdir("dd/sub2", 0755));
    int s2 = inotify_add_watch(fd, "dd/sub2", IN_ALL_EVENTS);
    line("D", "new-watch-wd", "sub2=%d", s2);
    line("D", "mkdir-sub2-events", "%s", drain(fd));
    check("rename", rename("dd/sub2", "u/sub2-gone"));
    line("D", "watched-dir-moved-out", "%s", drain(fd));
    writeFile("u/sub2-gone/x", "x");
    line("D", "create-in-moved-out-dir", "%s", drain(fd));
    // An unlinked file still open, under a parent watch with and without IN_EXCL_UNLINK.
    writeFile("dd/open", "x");
    drain(fd);
    int f = open("dd/open", O_WRONLY);
    check("unlink", unlink("dd/open"));
    line("D", "unlink-open-file", "%s", drain(fd));
    check("write", (int)write(f, "y", 1));
    line("D", "write-unlinked-no-excl", "%s", drain(fd));
    int p2 = inotify_add_watch(fd, "dd", IN_ALL_EVENTS | IN_EXCL_UNLINK);
    check("write", (int)write(f, "z", 1));
    line("D", "write-unlinked-excl-unlink", "wd=%d (parent was %d): %s", p2, p, drain(fd));
    close(f);
    line("D", "close-unlinked-excl-unlink", "%s", drain(fd));
    close(fd);
}

static void sectionH(void)
{
    fresh("h");
    check("mkdir", mkdir("h/other", 0755));
    writeFile("h/a", "seed");
    check("link", link("h/a", "h/b"));
    check("link", link("h/a", "h/other/c"));
    int fd = inotify_init1(IN_NONBLOCK);
    int wh = inotify_add_watch(fd, "h", IN_ALL_EVENTS);
    int wo = inotify_add_watch(fd, "h/other", IN_ALL_EVENTS);
    line("H", "wds", "h=%d other=%d", wh, wo);
    drain(fd);
    int fa = open("h/a", O_WRONLY);
    int fb = open("h/b", O_WRONLY);
    int fc = open("h/other/c", O_WRONLY);
    line("H", "open-three-names", "%s", drain(fd));
    check("write", (int)write(fa, "1", 1));
    line("H", "write-via-a", "%s", drain(fd));
    check("write", (int)write(fb, "2", 1));
    line("H", "write-via-b", "%s", drain(fd));
    check("write", (int)write(fc, "3", 1));
    line("H", "write-via-other/c", "%s", drain(fd));
    check("fchmod", fchmod(fa, 0600));
    line("H", "fchmod-via-a", "%s", drain(fd));
    check("rename", rename("h/a", "h/renamed"));
    drain(fd);
    check("write", (int)write(fa, "4", 1));
    line("H", "write-via-a-after-rename-to-renamed", "%s", drain(fd));
    check("rename", rename("h/renamed", "h/other/moved"));
    drain(fd);
    check("write", (int)write(fa, "5", 1));
    line("H", "write-via-a-after-move-to-other/moved", "%s", drain(fd));
    check("unlink", unlink("h/b"));
    drain(fd);
    check("write", (int)write(fb, "6", 1));
    line("H", "write-via-b-after-unlink-of-b", "%s", drain(fd));
    close(fa); close(fb); close(fc);
    line("H", "closes", "%s", drain(fd));
    close(fd);
}

static void sectionL(void)
{
    fresh("l");
    int fd = inotify_init1(IN_NONBLOCK);
    int wd = inotify_add_watch(fd, "l", IN_CREATE);
    (void)wd;
    int lengths[] = { 1, 2, 11, 12, 13, 15, 16, 17, 31, 32, 255 };
    for (size_t i = 0; i < sizeof lengths / sizeof lengths[0]; i++) {
        char name[300];
        memset(name, 'a' + (i % 26), lengths[i]);
        name[lengths[i]] = 0;
        char path[320];
        snprintf(path, sizeof path, "l/%s", name);
        writeFile(path, "x");
        char buf[1024];
        ssize_t n = read(fd, buf, sizeof buf);
        check("read", (int)n);
        const struct inotify_event *e = (const struct inotify_event *)buf;
        int zeroPadded = 1;
        for (uint32_t k = strlen(e->name); k < e->len; k++) if (e->name[k] != 0) zeroPadded = 0;
        line("L", "name-length", "%d chars: record=%zd bytes len=%u zero-padded=%d", lengths[i], n, e->len, zeroPadded);
        unlink(path);
    }
    line("L", "sizeof", "sizeof(struct inotify_event)=%zu", sizeof(struct inotify_event));
    close(fd);
}

static void sectionC(void)
{
    fresh("c");
    writeFile("c/f", "seed");
    writeFile("c/g", "seed");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "c", IN_ALL_EVENTS & ~(IN_OPEN | IN_CLOSE)));
    int f = open("c/f", O_WRONLY);
    drain(fd);
    check("write", (int)write(f, "1", 1));
    check("write", (int)write(f, "2", 1));
    check("write", (int)write(f, "3", 1));
    line("C", "three-writes", "%d events: %s", (drain(fd), drainCount), rendered);
    check("write", (int)write(f, "4", 1));
    check("chmod", chmod("c/f", 0600));
    check("write", (int)write(f, "5", 1));
    line("C", "write-chmod-write", "%d events: %s", (drain(fd), drainCount), rendered);
    check("chmod", chmod("c/f", 0644));
    check("chmod", chmod("c/g", 0644));
    check("chmod", chmod("c/f", 0600));
    line("C", "chmod-f-g-f", "%d events: %s", (drain(fd), drainCount), rendered);
    check("chmod", chmod("c/f", 0644));
    check("chmod", chmod("c/f", 0600));
    line("C", "chmod-f-f", "%d events: %s", (drain(fd), drainCount), rendered);
    close(f);
    drain(fd);
    // Does a merge happen when the identical event is not the newest in the queue? Create, modify, modify, create.
    writeFile("c/h", "x");
    int g = open("c/g", O_WRONLY);
    check("write", (int)write(g, "a", 1));
    check("write", (int)write(g, "b", 1));
    writeFile("c/i", "x");
    close(g);
    line("C", "create-write-write-create", "%d events: %s", (drain(fd), drainCount), rendered);
    // Identical events separated by a read are not merged.
    check("chmod", chmod("c/f", 0644));
    char buf[1024];
    ssize_t n1 = read(fd, buf, sizeof buf);
    check("chmod", chmod("c/f", 0600));
    ssize_t n2 = read(fd, buf, sizeof buf);
    line("C", "separated-by-read", "first read=%zd second read=%zd", n1, n2);
    close(fd);
}

static void sectionR(void)
{
    fresh("r");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "r", IN_CREATE));
    writeFile("r/abc", "x");     // one event of 16 + 16 = 32 bytes
    char buf[1024];
    line("R", "count-0-with-event-queued", "read(0)=%s", rc(read(fd, buf, 0)));
    line("R", "count-15", "read(15)=%s", rc(read(fd, buf, 15)));
    line("R", "count-16", "read(16)=%s", rc(read(fd, buf, 16)));
    line("R", "count-31", "read(31)=%s", rc(read(fd, buf, 31)));
    int avail = -1;
    ioctl(fd, FIONREAD, &avail);
    line("R", "fionread-one-event", "%d", avail);
    line("R", "count-32", "read(32)=%s", rc(read(fd, buf, 32)));
    writeFile("r/def", "x");
    writeFile("r/ghi", "x");
    ioctl(fd, FIONREAD, &avail);
    line("R", "fionread-two-events", "%d", avail);
    line("R", "count-48-of-64", "read(48)=%s", rc(read(fd, buf, 48)));
    line("R", "count-1024-rest", "read(1024)=%s", rc(read(fd, buf, 1024)));
    line("R", "count-0-empty", "read(0)=%s", rc(read(fd, buf, 0)));
    line("R", "count-1024-empty", "read(1024)=%s", rc(read(fd, buf, 1024)));
    writeFile("r/jkl", "x");
    line("R", "null-buffer", "read(NULL, 1024)=%s", rc(read(fd, NULL, 1024)));
    line("R", "then-valid", "read(1024)=%s", rc(read(fd, buf, 1024)));
    close(fd);
}

static void sectionP(void)
{
    fresh("p");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "p", IN_CREATE));
    int ep = epoll_create1(0);
    struct epoll_event ev = { EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLPRI, { .u64 = 7 } };
    line("P", "epoll-ctl-add", "%s", rc(epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev)));
    struct epoll_event out[4];
    line("P", "epoll-wait-empty", "%s", rc(epoll_wait(ep, out, 4, 0)));
    writeFile("p/x", "x");
    struct pollfd pf = { fd, POLLIN | POLLOUT | POLLPRI | POLLRDHUP, 0 };
    int pr = poll(&pf, 1, 0);
    line("P", "poll-with-event", "poll=%d revents=0x%x", pr, pf.revents);
    int er = epoll_wait(ep, out, 4, 0);
    line("P", "epoll-wait-with-event", "n=%d events=0x%x data=%llu", er, er > 0 ? out[0].events : 0, er > 0 ? (unsigned long long)out[0].data.u64 : 0ULL);
    drain(fd);
    pr = poll(&pf, 1, 0);
    line("P", "poll-after-drain", "poll=%d revents=0x%x", pr, pf.revents);
    pf.events = POLLOUT;
    pr = poll(&pf, 1, 0);
    line("P", "poll-out-only", "poll=%d revents=0x%x", pr, pf.revents);
    // A level-triggered epoll registration stays reported while the queue is non-empty.
    writeFile("p/y", "x");
    er = epoll_wait(ep, out, 4, 0);
    int er2 = epoll_wait(ep, out, 4, 0);
    line("P", "epoll-level", "first=%d second=%d", er, er2);
    drain(fd);
    close(ep); close(fd);
}

static void sectionK(void)
{
    fresh("k");
    writeFile("k/a", "x");
    writeFile("k/b", "x");
    int fd = inotify_init1(IN_NONBLOCK);
    int fd2 = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "k", IN_MOVE));
    check("inotify_add_watch", inotify_add_watch(fd2, "k", IN_MOVE));
    check("rename", rename("k/a", "k/a2"));
    check("rename", rename("k/b", "k/b2"));
    char buf[1024];
    ssize_t n = read(fd, buf, sizeof buf);
    check("read", (int)n);
    const struct inotify_event *e1 = (const struct inotify_event *)buf;
    const struct inotify_event *e2 = (const struct inotify_event *)(buf + sizeof *e1 + e1->len);
    const struct inotify_event *e3 = (const struct inotify_event *)(buf + 2 * (sizeof *e1 + e1->len));
    line("K", "adjacent-renames", "events=%d cookie1=cookie2:%d cookie3-cookie1=%d", renderEvents(buf, n), e1->cookie == e2->cookie, (int)(e3->cookie - e1->cookie));
    line("K", "first-cookie", "%u ~boot", e1->cookie);
    ssize_t n2 = read(fd2, buf, sizeof buf);
    const struct inotify_event *o1 = (const struct inotify_event *)buf;
    line("K", "second-instance", "events=%d same cookie as first instance=%d", renderEvents(buf, n2), o1->cookie == e1->cookie);
    close(fd); close(fd2);
}

static void sectionA(void)
{
    fresh("a");
    int fd = inotify_init1(IN_NONBLOCK);
    int fd2 = inotify_init1(IN_NONBLOCK);
    int d = dup(fd);
    int w1 = inotify_add_watch(fd, "a", IN_CREATE);
    int w2 = inotify_add_watch(fd2, "a", IN_CREATE);
    int wd = inotify_add_watch(d, "a", IN_CREATE | IN_DELETE);
    line("A", "wds", "instance1=%d instance2=%d via-dup=%d", w1, w2, wd);
    writeFile("a/x", "x");
    unlink("a/x");
    line("A", "instance1-via-dup", "%s", drain(d));
    line("A", "instance1-original-after-dup-drained", "%s", drain(fd));
    line("A", "instance2", "%s", drain(fd2));
    close(d);
    writeFile("a/y", "x");
    line("A", "after-dup-closed", "%s", drain(fd));
    close(fd); close(fd2);
}

static long readProc(const char *path)
{
    FILE *f = fopen(path, "r");
    if (!f) return -1;
    long v = -1;
    if (fscanf(f, "%ld", &v) != 1) v = -2;
    fclose(f);
    return v;
}

static void sectionO(void)
{
    line("O", "sysctl", "max_user_instances=%ld max_user_watches=%ld max_queued_events=%ld ~boot",
         readProc("/proc/sys/fs/inotify/max_user_instances"), readProc("/proc/sys/fs/inotify/max_user_watches"), readProc("/proc/sys/fs/inotify/max_queued_events"));
    long maxQueued = readProc("/proc/sys/fs/inotify/max_queued_events");
    fresh("o");
    writeFile("o/f", "x");
    writeFile("o/g", "x");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "o", IN_ATTRIB | IN_CREATE));
    long produced = maxQueued + 5000;
    for (long i = 0; i < produced; i++) {
        check("chmod", chmod(i % 2 ? "o/f" : "o/g", 0600 + (int)(i % 2)));
    }
    int avail = -1;
    ioctl(fd, FIONREAD, &avail);
    line("O", "fionread-full", "%d bytes after %ld alternating chmods", avail, produced);
    static char buf[1 << 20];
    long total = 0, overflowAt = -1, afterOverflow = 0;
    char overflowRecord[128] = "none";
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) { if (errno != EAGAIN) line("O", "drain-error", "%s", en(errno)); break; }
        ssize_t pos = 0;
        while (pos < n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            if (e->mask & IN_Q_OVERFLOW) {
                overflowAt = total;
                snprintf(overflowRecord, sizeof overflowRecord, "wd=%d mask=%s cookie=%u len=%u", e->wd, maskNames(e->mask), e->cookie, e->len);
            } else if (overflowAt >= 0) afterOverflow++;
            total++;
            pos += sizeof *e + e->len;
        }
    }
    line("O", "drained", "records=%ld overflow-record-index=%ld (0-based) records-after-overflow=%ld overflow=%s", total, overflowAt, afterOverflow, overflowRecord);
    check("chmod", chmod("o/f", 0644));
    line("O", "after-overflow-drained", "%s", drain(fd));
    // A second overflow does not merge with events before it: fill again and see one overflow record.
    for (long i = 0; i < produced; i++) check("chmod", chmod(i % 2 ? "o/f" : "o/g", 0600 + (int)(i % 2)));
    total = 0; long overflows = 0;
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) break;
        ssize_t pos = 0;
        while (pos < n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            if (e->mask & IN_Q_OVERFLOW) overflows++;
            total++;
            pos += sizeof *e + e->len;
        }
    }
    line("O", "second-fill", "records=%ld overflow-records=%ld", total, overflows);
    // A watch added while the queue is full: does its wd work afterwards?
    for (long i = 0; i < produced; i++) check("chmod", chmod(i % 2 ? "o/f" : "o/g", 0600 + (int)(i % 2)));
    check("mkdir", mkdir("o/late", 0755));
    int late = inotify_add_watch(fd, "o/late", IN_CREATE);
    writeFile("o/late/x", "x");
    total = 0; long lateSeen = 0;
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) break;
        ssize_t pos = 0;
        while (pos < n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            if (e->wd == late) lateSeen++;
            total++;
            pos += sizeof *e + e->len;
        }
    }
    line("O", "event-for-new-watch-while-full", "late wd=%d records=%ld records-for-late=%ld", late, total, lateSeen);
    writeFile("o/late/y", "x");
    line("O", "late-after-drain", "%s", drain(fd));
    close(fd);
}

// Section B: a thread blocked in read.
static int blockingFd;
static volatile int phase = 0;
static volatile long threadResult = -2;
static volatile int threadErrno = 0;
static struct timeval blockedSince;
static volatile long elapsedMs = -1;

static void onSignal(int signo) { (void)signo; }

static long msSince(struct timeval from)
{
    struct timeval now;
    gettimeofday(&now, NULL);
    return (now.tv_sec - from.tv_sec) * 1000 + (now.tv_usec - from.tv_usec) / 1000;
}

static void *blockedReader(void *arg)
{
    (void)arg;
    static char buf[4096];
    for (;;) {
        int myPhase = phase;
        gettimeofday(&blockedSince, NULL);
        ssize_t n = read(blockingFd, buf, sizeof buf);
        int e = errno;
        elapsedMs = msSince(blockedSince);
        threadResult = n;
        threadErrno = e;
        rendered[0] = 0;
        if (n > 0) renderEvents(buf, n);
        phase = myPhase + 1;
        if (n < 0) return NULL;
        while (phase == myPhase + 1) usleep(1000);
    }
}

static void sectionB(void)
{
    fresh("b");
    blockingFd = inotify_init1(IN_CLOEXEC);
    check("inotify_init1", blockingFd);
    int wd = inotify_add_watch(blockingFd, "b", IN_CREATE);
    struct sigaction sa = { 0 };
    sa.sa_handler = onSignal;
    check("sigaction", sigaction(SIGUSR1, &sa, NULL));
    pthread_t t;
    check("pthread_create", pthread_create(&t, NULL, blockedReader, NULL));
    usleep(100000);
    line("B", "blocked-before-event", "thread returned=%d after 100 ms", phase != 0);
    writeFile("b/x", "x");
    usleep(50000);
    line("B", "event", "returned=%d result=%ld %s events=%s", phase == 1, threadResult, threadResult < 0 ? en(threadErrno) : "", rendered);
    phase = 2; // let the thread read again
    usleep(100000);
    line("B", "blocked-again", "returned=%d", phase != 2);
    check("inotify_rm_watch", inotify_rm_watch(blockingFd, wd));
    usleep(50000);
    line("B", "rm-watch-wakes", "returned=%d result=%ld %s events=%s", phase == 3, threadResult, threadResult < 0 ? en(threadErrno) : "", rendered);
    phase = 4; // read on an instance with no watches
    usleep(150000);
    line("B", "no-watches", "returned after 150 ms=%d", phase != 4);
    int closed = close(blockingFd);
    usleep(150000);
    line("B", "close-under-read", "close=%s; thread returned after 150 ms=%d", rc(closed), phase != 4);
    check("pthread_kill", pthread_kill(t, SIGUSR1));
    usleep(50000);
    line("B", "signal", "returned=%d result=%ld %s", phase == 5, threadResult, threadResult < 0 ? en(threadErrno) : "");
    pthread_join(t, NULL);
    // A read that has already copied events is not interrupted; a blocked one restarts under SA_RESTART.
    blockingFd = inotify_init1(IN_CLOEXEC);
    check("inotify_add_watch", inotify_add_watch(blockingFd, "b", IN_CREATE));
    sa.sa_flags = SA_RESTART;
    check("sigaction", sigaction(SIGUSR1, &sa, NULL));
    phase = 0;
    check("pthread_create", pthread_create(&t, NULL, blockedReader, NULL));
    usleep(100000);
    check("pthread_kill", pthread_kill(t, SIGUSR1));
    usleep(100000);
    line("B", "signal-sa-restart", "returned=%d", phase != 0);
    writeFile("b/y", "x");
    usleep(50000);
    line("B", "event-after-restart", "returned=%d result=%ld events=%s", phase == 1, threadResult, rendered);
    phase = 2;
    usleep(50000);
    pthread_kill(t, SIGUSR1);
    sa.sa_flags = 0;
    sigaction(SIGUSR1, &sa, NULL);
    usleep(50000);
    pthread_kill(t, SIGUSR1);
    pthread_join(t, NULL);
    close(blockingFd);
}

static void sectionS(void)
{
    int fds[512];
    int n = 0;
    int err = 0;
    while (n < 512) {
        int fd = inotify_init1(IN_CLOEXEC);
        if (fd < 0) { err = errno; break; }
        fds[n++] = fd;
    }
    line("S", "instances", "created %d before %s (uid %d) ~boot", n, en(err), (int)getuid());
    for (int i = 0; i < n; i++) close(fds[i]);
    // Watches: count how many distinct inodes one instance can watch, up to 20000.
    fresh("s");
    int fd = inotify_init1(IN_NONBLOCK);
    long watches = 0;
    err = 0;
    for (long i = 0; i < 20000; i++) {
        char path[64];
        snprintf(path, sizeof path, "s/%ld", i);
        check("mkdir", mkdir(path, 0755));
        if (inotify_add_watch(fd, path, IN_CREATE) < 0) { err = errno; break; }
        watches++;
    }
    line("S", "watches", "added %ld before %s (stopped at 20000) ~boot", watches, err ? en(err) : "no error");
    close(fd);
    if (system("rm -rf s") != 0) exit(1);
}

int main(void)
{
    if (system("uname -rm") != 0) return 1;
    sectionI();
    sectionW();
    operations("E", "e", IN_ALL_EVENTS);
    operations("N", "n", dotnetMask);
    sectionF();
    sectionD();
    sectionH();
    sectionL();
    sectionC();
    sectionR();
    sectionP();
    sectionK();
    sectionA();
    sectionO();
    sectionB();
    sectionS();
    return 0;
}
