// What Linux's inotify reports for the attribute and content changes stage 4a
// of docs/plans/2026-10-11-inotify.md delivers, where inotify-probe.c left it
// open: which watches hear of a change, and with which events.
//
// Every line is `SECTION<TAB>case<TAB>result`, rendered as in inotify-probe.c:
// an event is `wd=N mask=NAMES cookie=C len=L name="..."`, cookies numbered in
// the order first seen, and `|` separating the events of one drain.
//
// Sections:
//   V  utimensat setting both times, one of them, or neither, under
//      IN_ALL_EVENTS and under the mask .NET's FileSystemWatcher uses
//   Z  a truncation or chown that strips set-ID bits, from uid 1000: the
//      events, the mode left, whether it is one record or two (a queue one
//      short of full overflows on a second), and the bits its record carries
//      to a watch asking for only one of them
//   Y  a change of attributes or contents when the inode and its parent are
//      both watched, by one instance: which records, in which order
//   X  link counts: link and unlink of a name of a watched file, with the
//      parent watched by the same instance
//   R  rename over a watched file and over a watched empty directory: the
//      displaced inode's records
//   P  which name the parent's record carries for a path that is not a plain
//      name: a symbolic link followed, "d/.", "d/..", a mount point, a
//      directory removed while it is the current directory
//   T  open's O_CREAT and O_TRUNC: a new file, an existing file, an existing
//      empty file
//   C  chown and chmod that change nothing
//   M  a change made by another process
//
// Build and run, from this directory, as root:
//   container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/probe-bin /probe/inotify-delivery-probe.c && mkdir -p /tmp/run && chmod 0777 /tmp/run && cd /tmp/run && /tmp/probe-bin'
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

static void check(const char *what, long result)
{
    if (result < 0) { perror(what); exit(1); }
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
static long drainCount;
static long drainOverflows;

// Read a non-blocking descriptor until EAGAIN: the rendered events, at most
// the first 16 and the last 4 of them.
static const char *drain(int fd)
{
    static char buf[1 << 20];
    rendered[0] = 0;
    drainCount = 0;
    drainOverflows = 0;
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) {
            if (errno != EAGAIN) { char line[64]; snprintf(line, sizeof line, "%sREAD errno%d", rendered[0] ? " | " : "", errno); strcat(rendered, line); }
            break;
        }
        ssize_t pos = 0;
        while (pos + (ssize_t)sizeof(struct inotify_event) <= n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            if (e->mask & IN_Q_OVERFLOW) drainOverflows++;
            if (drainCount < 16) {
                char line[512];
                snprintf(line, sizeof line, "%swd=%d mask=%s cookie=%s len=%u name=\"%s\"",
                         rendered[0] ? " | " : "", e->wd, maskNames(e->mask), cookieName(e->cookie), e->len, e->len ? e->name : "");
                strcat(rendered, line);
            }
            drainCount++;
            pos += sizeof(struct inotify_event) + e->len;
        }
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

static void fresh(const char *dir)
{
    char cmd[512];
    snprintf(cmd, sizeof cmd, "rm -rf %s && mkdir -p %s && chmod 0777 %s", dir, dir, dir);
    if (system(cmd) != 0) exit(1);
}

static void writeFile(const char *path, const char *text)
{
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    check(path, fd);
    check("write", (int)write(fd, text, strlen(text)));
    check("close", close(fd));
}

static const uint32_t dotnetMask = IN_CREATE | IN_DELETE | IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | IN_ONLYDIR | IN_EXCL_UNLINK;

// IN_ALL_EVENTS without the events opening, reading and closing raise.
static const uint32_t graphEvents = IN_ALL_EVENTS & ~(IN_OPEN | IN_CLOSE | IN_ACCESS);

static void timesUnder(const char *section, const char *maskName, uint32_t mask)
{
    char dir[64];
    snprintf(dir, sizeof dir, "v-%s", maskName);
    fresh(dir);
    char f[96];
    snprintf(f, sizeof f, "%s/f", dir);
    writeFile(f, "x");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, dir, mask));
    struct { const char *name; long a; long m; int null; } cases[] = {
        { "both", 1000, 2000, 0 },
        { "null", 0, 0, 1 },
        { "atime-only", 3000, UTIME_OMIT, 0 },
        { "mtime-only", UTIME_OMIT, 4000, 0 },
        { "atime-now-only", UTIME_NOW, UTIME_OMIT, 0 },
        { "mtime-now-only", UTIME_OMIT, UTIME_NOW, 0 },
        { "neither", UTIME_OMIT, UTIME_OMIT, 0 },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        struct timespec ts[2];
        ts[0].tv_sec = cases[i].a == UTIME_NOW || cases[i].a == UTIME_OMIT ? 0 : cases[i].a;
        ts[0].tv_nsec = cases[i].a == UTIME_NOW || cases[i].a == UTIME_OMIT ? cases[i].a : 0;
        ts[1].tv_sec = cases[i].m == UTIME_NOW || cases[i].m == UTIME_OMIT ? 0 : cases[i].m;
        ts[1].tv_nsec = cases[i].m == UTIME_NOW || cases[i].m == UTIME_OMIT ? cases[i].m : 0;
        check("utimensat", utimensat(AT_FDCWD, f, cases[i].null ? NULL : ts, 0));
        char name[64];
        snprintf(name, sizeof name, "%s-%s", maskName, cases[i].name);
        line(section, name, "%s", drain(fd));
    }
    // The watched directory itself.
    check("utimensat", utimensat(AT_FDCWD, dir, NULL, 0));
    char name[64];
    snprintf(name, sizeof name, "%s-watched-dir", maskName);
    line(section, name, "%s", drain(fd));
    close(fd);
}

static void sectionV(void)
{
    timesUnder("V", "all", graphEvents | IN_ACCESS);
    timesUnder("V", "dotnet", dotnetMask);
}

// Fill `fd`'s queue to one short of `max` records, by chmods alternating
// between two names in `dir` that nothing else watches the records of.
static void fillToOneShort(int fd, const char *a, const char *b, long max)
{
    for (long i = 0; i < max - 1; i++) check("chmod", chmod(i % 2 ? a : b, 0600 + (int)(i % 2)));
    int avail = -1;
    ioctl(fd, FIONREAD, &avail);
    if (avail != (max - 1) * 32) { fprintf(stderr, "filled to %d bytes, not %ld\n", avail, (max - 1) * 32); exit(1); }
}

static long maxQueued(void)
{
    FILE *f = fopen("/proc/sys/fs/inotify/max_queued_events", "r");
    long v = -1;
    if (!f || fscanf(f, "%ld", &v) != 1) exit(1);
    fclose(f);
    return v;
}

// The set-ID rows, as uid 1000 in a directory it owns.
static void sectionZ(void)
{
    fresh("z");
    check("chown", chown("z", 1000, 1000));
    pid_t child = fork();
    if (child == 0) {
        if (setresgid(1000, 1000, 1000) != 0 || setgroups(0, NULL) != 0 || setresuid(1000, 1000, 1000) != 0) { perror("setresuid"); _exit(1); }
        long max = maxQueued();
        const char *ops[] = { "truncate", "open-trunc", "write" };
        for (size_t o = 0; o < sizeof ops / sizeof ops[0]; o++) {
            for (int full = 0; full < 2; full++) {
                fresh("z/d");
                writeFile("z/d/f", "hello");
                writeFile("z/d/a", "x");
                writeFile("z/d/b", "x");
                check("chmod", chmod("z/d/f", 06755));
                struct stat st;
                check("stat", stat("z/d/f", &st));
                int fd = inotify_init1(IN_NONBLOCK);
                check("inotify_add_watch", inotify_add_watch(fd, "z/d", graphEvents));
                if (full) fillToOneShort(fd, "z/d/a", "z/d/b", max); else drain(fd);
                if (strcmp(ops[o], "truncate") == 0) check("truncate", truncate("z/d/f", 1));
                else if (strcmp(ops[o], "open-trunc") == 0) { int t = open("z/d/f", O_WRONLY | O_TRUNC); check("open", t); close(t); }
                else { int w = open("z/d/f", O_WRONLY); check("open", w); check("write", (int)write(w, "y", 1)); close(w); }
                struct stat after;
                check("stat", stat("z/d/f", &after));
                char name[64];
                if (full) {
                    drain(fd);
                    snprintf(name, sizeof name, "%s-at-one-short", ops[o]);
                    line("Z", name, "records=%ld overflows=%ld (filled %ld)", drainCount, drainOverflows, max - 1);
                } else {
                    snprintf(name, sizeof name, "%s", ops[o]);
                    line("Z", name, "mode %o -> %o: %s", (unsigned)(st.st_mode & 07777), (unsigned)(after.st_mode & 07777), drain(fd));
                }
                close(fd);
            }
        }
        // Under the FileSystemWatcher mask.
        fresh("z/d");
        writeFile("z/d/f", "hello");
        check("chmod", chmod("z/d/f", 06755));
        int fd = inotify_init1(IN_NONBLOCK);
        check("inotify_add_watch", inotify_add_watch(fd, "z/d", dotnetMask));
        check("truncate", truncate("z/d/f", 1));
        line("Z", "truncate-dotnet", "%s", drain(fd));
        close(fd);
        // A watch asking for one of the two events: which bits the record carries.
        struct { uint32_t mask; const char *name; } single[] = {
            { IN_MODIFY, "open-trunc-modify-watch" }, { IN_ATTRIB, "open-trunc-attrib-watch" },
        };
        for (size_t i = 0; i < sizeof single / sizeof single[0]; i++) {
            fresh("z/d");
            writeFile("z/d/f", "hello");
            check("chmod", chmod("z/d/f", 06755));
            fd = inotify_init1(IN_NONBLOCK);
            check("inotify_add_watch", inotify_add_watch(fd, "z/d", single[i].mask));
            int t = open("z/d/f", O_WRONLY | O_TRUNC);
            check("open", t);
            close(t);
            check("chmod", chmod("z/d/f", 06755));
            t = open("z/d/f", O_WRONLY | O_TRUNC);
            check("open", t);
            close(t);
            line("Z", single[i].name, "%s", drain(fd));
            close(fd);
        }
        _exit(0);
    }
    int status;
    waitpid(child, &status, 0);
    // chown by root of a set-ID file: one record or two?
    long max = maxQueued();
    for (int full = 0; full < 2; full++) {
        fresh("z/r");
        writeFile("z/r/f", "hello");
        writeFile("z/r/a", "x");
        writeFile("z/r/b", "x");
        check("chmod", chmod("z/r/f", 06755));
        int fd = inotify_init1(IN_NONBLOCK);
        check("inotify_add_watch", inotify_add_watch(fd, "z/r", graphEvents));
        if (full) fillToOneShort(fd, "z/r/a", "z/r/b", max);
        check("chown", chown("z/r/f", 1000, 1000));
        struct stat after;
        check("stat", stat("z/r/f", &after));
        if (full) {
            drain(fd);
            line("Z", "chown-at-one-short", "records=%ld overflows=%ld (filled %ld)", drainCount, drainOverflows, max - 1);
        } else {
            line("Z", "chown", "mode 6755 -> %o: %s", (unsigned)(after.st_mode & 07777), drain(fd));
        }
        close(fd);
    }
}

static void sectionY(void)
{
    fresh("y");
    check("mkdir", mkdir("y/d", 0755));
    writeFile("y/f", "x");
    int fd = inotify_init1(IN_NONBLOCK);
    int wp = inotify_add_watch(fd, "y", graphEvents);
    int wf = inotify_add_watch(fd, "y/f", graphEvents);
    int wd = inotify_add_watch(fd, "y/d", graphEvents);
    line("Y", "wds", "parent=%d f=%d d=%d", wp, wf, wd);
    check("chmod", chmod("y/f", 0600));
    line("Y", "chmod-file", "%s", drain(fd));
    check("chmod", chmod("y/d", 0700));
    line("Y", "chmod-dir", "%s", drain(fd));
    check("truncate", truncate("y/f", 0));
    line("Y", "truncate-file", "%s", drain(fd));
    int t = open("y/f", O_WRONLY | O_TRUNC);
    check("open", t);
    close(t);
    line("Y", "open-trunc-file", "%s", drain(fd));
    check("utimensat", utimensat(AT_FDCWD, "y/f", NULL, 0));
    line("Y", "utimensat-file", "%s", drain(fd));
    // A second instance watching the parent only.
    int fd2 = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd2, "y", graphEvents));
    check("chmod", chmod("y/f", 0644));
    line("Y", "two-instances-first", "%s", drain(fd));
    line("Y", "two-instances-second", "%s", drain(fd2));
    close(fd2);
    close(fd);
}

static void sectionX(void)
{
    fresh("x");
    writeFile("x/f", "x");
    int fd = inotify_init1(IN_NONBLOCK);
    int wp = inotify_add_watch(fd, "x", graphEvents);
    int wf = inotify_add_watch(fd, "x/f", graphEvents);
    line("X", "wds", "parent=%d f=%d", wp, wf);
    check("link", link("x/f", "x/g"));
    line("X", "link", "%s", drain(fd));
    check("unlink", unlink("x/g"));
    line("X", "unlink-other-name", "%s", drain(fd));
    // A directory gaining and losing a subdirectory: its link count moves.
    check("mkdir", mkdir("x/d", 0755));
    int wd = inotify_add_watch(fd, "x/d", graphEvents);
    drain(fd);
    check("mkdir", mkdir("x/d/sub", 0755));
    line("X", "mkdir-in-watched-dir", "wd=%d: %s", wd, drain(fd));
    check("rmdir", rmdir("x/d/sub"));
    line("X", "rmdir-in-watched-dir", "%s", drain(fd));
    close(fd);
}

static void sectionR(void)
{
    fresh("r");
    writeFile("r/a", "a");
    writeFile("r/b", "b");
    check("mkdir", mkdir("r/sd", 0755));
    check("mkdir", mkdir("r/td", 0755));
    int fd = inotify_init1(IN_NONBLOCK);
    int wp = inotify_add_watch(fd, "r", graphEvents);
    int wb = inotify_add_watch(fd, "r/b", graphEvents);
    int wt = inotify_add_watch(fd, "r/td", graphEvents);
    line("R", "wds", "parent=%d b=%d td=%d", wp, wb, wt);
    check("rename", rename("r/a", "r/b"));
    line("R", "over-watched-file", "%s", drain(fd));
    check("rename", rename("r/sd", "r/td"));
    line("R", "over-watched-empty-dir", "%s", drain(fd));
    // The displaced file still open: its records wait for the close.
    writeFile("r/c", "c");
    writeFile("r/e", "e");
    int we = inotify_add_watch(fd, "r/e", graphEvents);
    int open_e = open("r/e", O_RDONLY);
    drain(fd);
    check("rename", rename("r/c", "r/e"));
    line("R", "over-watched-open-file", "wd=%d: %s", we, drain(fd));
    close(open_e);
    line("R", "close-displaced", "%s", drain(fd));
    close(fd);
}

static void sectionP(void)
{
    fresh("p");
    check("mkdir", mkdir("p/d", 0755));
    check("mkdir", mkdir("p/d/e", 0755));
    writeFile("p/d/f", "x");
    check("symlink", symlink("d/f", "p/lf"));
    int fd = inotify_init1(IN_NONBLOCK);
    int wp = inotify_add_watch(fd, "p", graphEvents);
    int wd = inotify_add_watch(fd, "p/d", graphEvents);
    line("P", "wds", "p=%d d=%d", wp, wd);
    check("chmod", chmod("p/lf", 0600));
    line("P", "chmod-through-symlink", "%s", drain(fd));
    check("chmod", chmod("p/d/.", 0700));
    line("P", "chmod-dot", "%s", drain(fd));
    check("chmod", chmod("p/d/e/..", 0755));
    line("P", "chmod-dotdot", "%s", drain(fd));
    check("chmod", chmod("p/d/e/", 0755));
    line("P", "chmod-trailing-slash", "%s", drain(fd));
    close(fd);
    // A mount point: the root's watch, and /dev mounted on it.
    struct stat st;
    check("stat", stat("/dev", &st));
    int root = inotify_init1(IN_NONBLOCK);
    int wr = inotify_add_watch(root, "/", graphEvents);
    int wdev = inotify_add_watch(root, "/dev", graphEvents);
    check("chmod", chmod("/dev", st.st_mode & 07777));
    line("P", "chmod-mount-point", "root=%d dev=%d: %s", wr, wdev, drain(root));
    close(root);
    // A directory removed while it is the current directory.
    check("mkdir", mkdir("p/gone", 0755));
    int gone = inotify_init1(IN_NONBLOCK);
    int wpg = inotify_add_watch(gone, "p", graphEvents);
    int wg = inotify_add_watch(gone, "p/gone", graphEvents);
    char cwd[4096];
    if (!getcwd(cwd, sizeof cwd)) exit(1);
    check("chdir", chdir("p/gone"));
    check("rmdir", rmdir("../gone"));
    line("P", "rmdir-of-current", "p=%d gone=%d: %s", wpg, wg, drain(gone));
    int r = chmod(".", 0700);
    line("P", "chmod-removed-current", "chmod=%d: %s", r, drain(gone));
    check("chdir", chdir(cwd));
    close(gone);
}

static void sectionT(void)
{
    fresh("t");
    writeFile("t/existing", "seed");
    int empty = open("t/empty", O_WRONLY | O_CREAT, 0644);
    close(empty);
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "t", graphEvents));
    int f = open("t/new", O_WRONLY | O_CREAT | O_TRUNC, 0644);
    check("open", f);
    close(f);
    line("T", "creat-trunc-new", "%s", drain(fd));
    f = open("t/existing", O_WRONLY | O_CREAT, 0644);
    check("open", f);
    close(f);
    line("T", "creat-existing", "%s", drain(fd));
    f = open("t/existing", O_WRONLY | O_CREAT | O_TRUNC, 0644);
    check("open", f);
    close(f);
    line("T", "creat-trunc-existing", "%s", drain(fd));
    f = open("t/empty", O_WRONLY | O_TRUNC);
    check("open", f);
    close(f);
    line("T", "trunc-empty", "%s", drain(fd));
    f = open("t/existing", O_RDONLY | O_TRUNC);
    line("T", "rdonly-trunc", "open=%d: %s", f >= 0, drain(fd));
    if (f >= 0) close(f);
    close(fd);
}

static void sectionC(void)
{
    fresh("c");
    writeFile("c/f", "x");
    check("chown", chown("c/f", 1000, 1000));
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "c", graphEvents));
    check("chown", chown("c/f", -1, -1));
    line("C", "chown-neither", "%s", drain(fd));
    check("chown", chown("c/f", 1000, 1000));
    line("C", "chown-same", "%s", drain(fd));
    check("chown", chown("c/f", 1000, -1));
    line("C", "chown-same-user-only", "%s", drain(fd));
    check("chmod", chmod("c/f", 0644));
    line("C", "chmod-same", "%s", drain(fd));
    close(fd);
}

static void sectionM(void)
{
    fresh("mp");
    int fd = inotify_init1(IN_NONBLOCK);
    check("inotify_add_watch", inotify_add_watch(fd, "mp", graphEvents));
    pid_t child = fork();
    if (child == 0) {
        writeFile("mp/x", "x");
        check("mkdir", mkdir("mp/d", 0755));
        check("rename", rename("mp/x", "mp/y"));
        check("chmod", chmod("mp/y", 0600));
        check("unlink", unlink("mp/y"));
        check("rmdir", rmdir("mp/d"));
        _exit(0);
    }
    int status;
    waitpid(child, &status, 0);
    line("M", "child-changes", "%s", drain(fd));
    close(fd);
}

int main(void)
{
    FILE *u = popen("uname -rm", "r");
    char release[128] = "";
    if (u && fgets(release, sizeof release, u)) fputs(release, stdout);
    if (u) pclose(u);
    sectionV();
    sectionZ();
    sectionY();
    sectionX();
    sectionR();
    sectionP();
    sectionT();
    sectionC();
    sectionM();
    return 0;
}
