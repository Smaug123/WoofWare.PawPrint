// What Linux's inotify reports for a change made through a descriptor, where
// the name the descriptor was opened through has since been renamed over,
// unlinked, or given again, which stage 4b of docs/plans/2026-10-11-inotify.md
// records on the description; and the Kestrel handoff under
// FileSystemWatcher's mask.
//
// Every line is `SECTION<TAB>case<TAB>result`, rendered as in inotify-probe.c:
// an event is `wd=N mask=NAMES cookie=C len=L name="..."`, cookies numbered in
// the order first seen, and `|` separating the events of one drain.
//
// Sections:
//   E  each change a descriptor can make (write, pwrite, copy_file_range,
//      ftruncate, fchmod, fchown, futimens, and the *at calls' empty path
//      under AT_EMPTY_PATH), through a name since unlinked, heard at a watch
//      on the old directory or on the file itself, with and without
//      IN_EXCL_UNLINK
//   L  a write through a name unlinked and then linked again to the same
//      inode, under a watch with and without IN_EXCL_UNLINK
//   O  a write and an fchmod through a name a rename has since displaced the
//      file from, with and without IN_EXCL_UNLINK
//   Q  fchmod through a directory's descriptor, after the directory is
//      renamed, moved, and removed, with and without IN_EXCL_UNLINK
//   U  a file unlinked while open in a directory then removed: what the
//      directory's watch hears of the rmdir, of a write through the file,
//      and of its close
//   V  a write to /dev/null under a watch on /dev
//   K  the Kestrel handoff: File.WriteAllText of "server-address.tmp" and
//      File.Move of it to "server-address", under FileSystemWatcher's mask
//
// Build and run, from this directory, as root:
//   container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/probe-bin /probe/inotify-dentry-probe.c && mkdir -p /tmp/run && cd /tmp/run && /tmp/probe-bin'
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/inotify.h>
#include <sys/stat.h>
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

// Read a non-blocking descriptor until EAGAIN: the rendered events.
static const char *drain(int fd)
{
    static char buf[1 << 16];
    rendered[0] = 0;
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) {
            if (errno != EAGAIN) { char l[64]; snprintf(l, sizeof l, "%sREAD errno%d", rendered[0] ? " | " : "", errno); strcat(rendered, l); }
            break;
        }
        ssize_t pos = 0;
        while (pos + (ssize_t)sizeof(struct inotify_event) <= n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            char l[512];
            snprintf(l, sizeof l, "%swd=%d mask=%s cookie=%s len=%u name=\"%s\"",
                     rendered[0] ? " | " : "", e->wd, maskNames(e->mask), cookieName(e->cookie), e->len, e->len ? e->name : "");
            strcat(rendered, l);
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
    snprintf(cmd, sizeof cmd, "rm -rf %s && mkdir -p %s && chmod 0755 %s", dir, dir, dir);
    if (system(cmd) != 0) exit(1);
}

static void writeFile(const char *path, const char *text)
{
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    check(path, fd);
    check("write", (int)write(fd, text, strlen(text)));
    check("close", close(fd));
}

// The answer of a call: 0 or a count, or `-1 errno`.
static const char *rc(long result)
{
    static char b[4][32];
    static int k = 0;
    k = (k + 1) % 4;
    if (result < 0) snprintf(b[k], sizeof b[k], "-1 errno%d", errno);
    else snprintf(b[k], sizeof b[k], "%ld", result);
    return b[k];
}

// IN_ALL_EVENTS without the events opening, reading and closing raise.
static const uint32_t graphEvents = IN_ALL_EVENTS & ~(IN_OPEN | IN_CLOSE | IN_ACCESS);

static const uint32_t dotnetMask = IN_CREATE | IN_DELETE | IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | IN_ONLYDIR | IN_EXCL_UNLINK;

// Section E, under one arrangement of watches: `parent` watches the
// directory, `self` the file, each with IN_EXCL_UNLINK when `excl`.
static void throughUnlinked(const char *config, int parent, int self, int excl)
{
    fresh("e");
    writeFile("e/f", "hello");
    writeFile("e/src", "abc");
    int f = open("e/f", O_RDWR);
    check("open", f);
    int src = open("e/src", O_RDONLY);
    check("open", src);
    int in = inotify_init1(IN_NONBLOCK);
    uint32_t mask = graphEvents | (excl ? IN_EXCL_UNLINK : 0);
    if (parent) check("watch", inotify_add_watch(in, "e", mask));
    if (self) check("watch", inotify_add_watch(in, "e/f", mask));
    check("unlink", unlink("e/f"));
    char name[96];
    snprintf(name, sizeof name, "%s-unlink", config);
    line("E", name, "%s", drain(in));

    struct timespec times[2] = { { 1000, 0 }, { 2000, 0 } };
    loff_t offset = 0;
#define OP(label, call) do { long r_ = (call); snprintf(name, sizeof name, "%s-%s", config, label); line("E", name, "%s: %s", rc(r_), drain(in)); } while (0)
    OP("write", write(f, "y", 1));
    OP("pwrite", pwrite(f, "z", 1, 3));
    OP("copy_file_range", (offset = 0, copy_file_range(src, &offset, f, NULL, 3, 0)));
    OP("ftruncate", ftruncate(f, 2));
    OP("fchmod", fchmod(f, 0600));
    OP("fchown", fchown(f, 0, 0));
    OP("futimens", futimens(f, times));
    OP("utimensat-empty-path", utimensat(f, "", NULL, AT_EMPTY_PATH));
    OP("fchmodat-empty-path", fchmodat(f, "", 0644, AT_EMPTY_PATH));
    OP("fchownat-empty-path", fchownat(f, "", 0, 0, AT_EMPTY_PATH));
#undef OP
    close(f);
    close(src);
    snprintf(name, sizeof name, "%s-close", config);
    line("E", name, "%s", drain(in));
    close(in);
}

static void sectionE(void)
{
    throughUnlinked("parent", 1, 0, 0);
    throughUnlinked("parent-excl", 1, 0, 1);
    throughUnlinked("self", 0, 1, 0);
    throughUnlinked("self-excl", 0, 1, 1);
    throughUnlinked("both-excl", 1, 1, 1);
}

static void sectionL(void)
{
    for (int excl = 0; excl < 2; excl++) {
        const char *config = excl ? "excl" : "no-excl";
        fresh("l");
        writeFile("l/a", "x");
        check("link", link("l/a", "l/b"));
        int fb = open("l/b", O_WRONLY);
        check("open", fb);
        int in = inotify_init1(IN_NONBLOCK);
        check("watch", inotify_add_watch(in, "l", graphEvents | (excl ? IN_EXCL_UNLINK : 0)));
        check("unlink", unlink("l/b"));
        check("link", link("l/a", "l/b"));
        drain(in);
        check("write", (int)write(fb, "1", 1));
        char name[64];
        snprintf(name, sizeof name, "%s-write-through-relinked-name", config);
        line("L", name, "%s", drain(in));
        // The name given again, opened again: a fresh name.
        int again = open("l/b", O_WRONLY);
        check("open", again);
        drain(in);
        check("write", (int)write(again, "2", 1));
        snprintf(name, sizeof name, "%s-write-through-fresh-open", config);
        line("L", name, "%s", drain(in));
        close(again);
        close(fb);
        close(in);
    }
}

static void sectionO(void)
{
    for (int excl = 0; excl < 2; excl++) {
        const char *config = excl ? "excl" : "no-excl";
        fresh("o");
        writeFile("o/s", "s");
        writeFile("o/t", "t");
        int ft = open("o/t", O_WRONLY);
        check("open", ft);
        int fs = open("o/s", O_WRONLY);
        check("open", fs);
        int in = inotify_init1(IN_NONBLOCK);
        check("watch", inotify_add_watch(in, "o", graphEvents | (excl ? IN_EXCL_UNLINK : 0)));
        check("rename", rename("o/s", "o/t"));
        char name[64];
        snprintf(name, sizeof name, "%s-rename-over", config);
        line("O", name, "%s", drain(in));
        check("write", (int)write(ft, "1", 1));
        snprintf(name, sizeof name, "%s-write-through-displaced", config);
        line("O", name, "%s", drain(in));
        check("fchmod", fchmod(ft, 0600));
        snprintf(name, sizeof name, "%s-fchmod-through-displaced", config);
        line("O", name, "%s", drain(in));
        check("write", (int)write(fs, "2", 1));
        snprintf(name, sizeof name, "%s-write-through-moved", config);
        line("O", name, "%s", drain(in));
        close(ft);
        close(fs);
        close(in);
    }
}

static void sectionQ(void)
{
    for (int excl = 0; excl < 2; excl++) {
        const char *config = excl ? "excl" : "no-excl";
        fresh("q");
        check("mkdir", mkdir("q/d", 0755));
        check("mkdir", mkdir("q/other", 0755));
        int dfd = open("q/d", O_RDONLY | O_DIRECTORY);
        check("open", dfd);
        int in = inotify_init1(IN_NONBLOCK);
        uint32_t mask = graphEvents | (excl ? IN_EXCL_UNLINK : 0);
        int wq = inotify_add_watch(in, "q", mask);
        int wo = inotify_add_watch(in, "q/other", mask);
        char name[64];
        snprintf(name, sizeof name, "%s-wds", config);
        line("Q", name, "q=%d other=%d", wq, wo);
        check("fchmod", fchmod(dfd, 0700));
        snprintf(name, sizeof name, "%s-fchmod", config);
        line("Q", name, "%s", drain(in));
        check("rename", rename("q/d", "q/d2"));
        drain(in);
        check("fchmod", fchmod(dfd, 0755));
        snprintf(name, sizeof name, "%s-fchmod-after-rename", config);
        line("Q", name, "%s", drain(in));
        check("rename", rename("q/d2", "q/other/d3"));
        drain(in);
        check("fchmod", fchmod(dfd, 0700));
        snprintf(name, sizeof name, "%s-fchmod-after-move", config);
        line("Q", name, "%s", drain(in));
        check("rmdir", rmdir("q/other/d3"));
        snprintf(name, sizeof name, "%s-rmdir", config);
        line("Q", name, "%s", drain(in));
        long r = fchmod(dfd, 0755);
        snprintf(name, sizeof name, "%s-fchmod-after-rmdir", config);
        line("Q", name, "%s: %s", rc(r), drain(in));
        struct timespec times[2] = { { 1000, 0 }, { 2000, 0 } };
        r = futimens(dfd, times);
        snprintf(name, sizeof name, "%s-futimens-after-rmdir", config);
        line("Q", name, "%s: %s", rc(r), drain(in));
        close(dfd);
        snprintf(name, sizeof name, "%s-close", config);
        line("Q", name, "%s", drain(in));
        close(in);
    }
}

static void sectionU(void)
{
    fresh("u");
    check("mkdir", mkdir("u/sub", 0755));
    writeFile("u/sub/f", "x");
    int f = open("u/sub/f", O_WRONLY);
    check("open", f);
    int in = inotify_init1(IN_NONBLOCK);
    int wu = inotify_add_watch(in, "u", graphEvents);
    int ws = inotify_add_watch(in, "u/sub", graphEvents);
    line("U", "wds", "u=%d sub=%d", wu, ws);
    check("unlink", unlink("u/sub/f"));
    line("U", "unlink", "%s", drain(in));
    check("rmdir", rmdir("u/sub"));
    line("U", "rmdir-parent", "%s", drain(in));
    check("write", (int)write(f, "y", 1));
    line("U", "write-in-removed-parent", "%s", drain(in));
    close(f);
    line("U", "close", "%s", drain(in));
    line("U", "rm-sub-watch-after", "%s", rc(inotify_rm_watch(in, ws)));
    line("U", "rm-sub-watch-events", "%s", drain(in));
    close(in);
}

static void sectionV(void)
{
    int in = inotify_init1(IN_NONBLOCK);
    int wd = inotify_add_watch(in, "/dev", graphEvents);
    int null = open("/dev/null", O_WRONLY);
    check("open", null);
    drain(in);
    long r = write(null, "x", 1);
    line("V", "write-dev-null", "wd=%d write=%s: %s", wd, rc(r), drain(in));
    close(null);
    drain(in);
    close(in);
}

static void sectionK(void)
{
    fresh("k");
    int in = inotify_init1(IN_NONBLOCK);
    int wd = inotify_add_watch(in, "k", dotnetMask);
    // File.WriteAllText: FileMode.Create.
    int f = open("k/server-address.tmp", O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0666);
    check("open", f);
    const char *address = "http://127.0.0.1:5000";
    check("write", (int)write(f, address, strlen(address)));
    check("close", close(f));
    line("K", "write-all-text", "wd=%d: %s", wd, drain(in));
    // File.Move, the destination absent: rename(2).
    check("rename", rename("k/server-address.tmp", "k/server-address"));
    line("K", "move", "%s", drain(in));
    close(in);
}

int main(void)
{
    FILE *u = popen("uname -rm", "r");
    char release[128] = "";
    if (u && fgets(release, sizeof release, u)) fputs(release, stdout);
    if (u) pclose(u);
    sectionE();
    sectionL();
    sectionO();
    sectionQ();
    sectionU();
    sectionV();
    sectionK();
    return 0;
}
