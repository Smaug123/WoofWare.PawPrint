// What Linux's inotify reports when a watched inode itself goes or moves,
// which stage 4c of docs/plans/2026-10-11-inotify.md delivers: the
// IN_DELETE_SELF and IN_IGNORED of an inode's going, and when they come
// relative to the other records of the call that made them; IN_MOVE_SELF;
// and whether a watch outlives its inode.
//
// Every line is `SECTION<TAB>case<TAB>result`, rendered as in inotify-probe.c:
// an event is `wd=N mask=NAMES cookie=C len=L name="..."`, cookies numbered in
// the order first seen, and `|` separating the events of one drain.
//
// Sections:
//   U  unlink of a watched file's last name, nothing else holding it, with
//      its directory watched by the same instance: the order of the file's
//      records and the directory's; under a watch asking for no self event;
//      and inotify_rm_watch of the watch afterwards
//   N  a file with two names, opened through one or both, then unlinked
//      name by name: when its IN_DELETE_SELF comes, if each name a
//      descriptor was opened through is its own holder; and one name
//      unlinked and linked again, opened before and after
//   R  rename: of a watched file over another, of one of two names of a
//      watched file, of a name onto another name of the same file, of a
//      watched directory over a watched empty one, under a watch asking for
//      no IN_MOVE_SELF
//   D  rmdir of a watched directory: under FileSystemWatcher's mask, while a
//      descriptor holds it, while it is the current directory, and a chain
//      of two removed while the current directory is the inner one
//   W  two instances watching one file, which is unlinked
//   Q  a file's going at a queue one short of full
//   X  files held by another process's descriptors, unlinked, at that
//      process's exit: which file goes first
//
// Build and run, from this directory, as root:
//   container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/probe-bin /probe/inotify-self-probe.c && mkdir -p /tmp/run && cd /tmp/run && /tmp/probe-bin'
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

static const uint32_t selfEvents = IN_DELETE_SELF | IN_MOVE_SELF;

static void sectionU(void)
{
    fresh("u");
    writeFile("u/f", "x");
    writeFile("u/g", "x");
    int in = inotify_init1(IN_NONBLOCK);
    int wu = inotify_add_watch(in, "u", graphEvents);
    int wf = inotify_add_watch(in, "u/f", graphEvents);
    int wg = inotify_add_watch(in, "u/g", IN_MODIFY);
    line("U", "wds", "u=%d f=%d g=%d", wu, wf, wg);
    check("unlink", unlink("u/f"));
    line("U", "unlink-last-name", "%s", drain(in));
    line("U", "rm-after", "%s", rc(inotify_rm_watch(in, wf)));
    line("U", "rm-after-events", "%s", drain(in));
    check("unlink", unlink("u/g"));
    line("U", "unlink-last-name-no-self-event", "%s", drain(in));
    // A name re-created where the watched file was: another inode.
    writeFile("u/f", "y");
    line("U", "recreate", "%s", drain(in));
    int again = inotify_add_watch(in, "u/f", graphEvents);
    line("U", "rewatch", "wd=%d: %s", again, drain(in));
    close(in);
}

// A file `n/a`, also named `n/b`, watched with its directory.
static int namesSetup(int *in)
{
    fresh("n");
    writeFile("n/a", "x");
    check("link", link("n/a", "n/b"));
    *in = inotify_init1(IN_NONBLOCK);
    check("watch", inotify_add_watch(*in, "n", graphEvents));
    int wf = inotify_add_watch(*in, "n/a", graphEvents);
    check("watch", wf);
    return wf;
}

static void sectionN(void)
{
    int in;
    // Opened through a, a unlinked, then b: the last name goes while a
    // descriptor holds the file through the other.
    int wf = namesSetup(&in);
    int fa = open("n/a", O_WRONLY);
    check("open", fa);
    line("N", "one-wds", "dir=1 file=%d", wf);
    check("unlink", unlink("n/a"));
    line("N", "one-unlink-held-name", "%s", drain(in));
    check("unlink", unlink("n/b"));
    line("N", "one-unlink-last-name", "%s", drain(in));
    check("write", (int)write(fa, "1", 1));
    line("N", "one-write", "%s", drain(in));
    close(fa);
    line("N", "one-close", "%s", drain(in));
    line("N", "one-rm-after", "%s", rc(inotify_rm_watch(in, wf)));
    drain(in);
    close(in);

    // Opened through a and through b, both unlinked, then closed in turn.
    wf = namesSetup(&in);
    fa = open("n/a", O_WRONLY);
    check("open", fa);
    int fb = open("n/b", O_WRONLY);
    check("open", fb);
    check("unlink", unlink("n/a"));
    line("N", "two-unlink-a", "%s", drain(in));
    check("unlink", unlink("n/b"));
    line("N", "two-unlink-b", "%s", drain(in));
    close(fa);
    line("N", "two-close-a", "%s", drain(in));
    close(fb);
    line("N", "two-close-b", "%s", drain(in));
    close(in);

    // Opened through a twice: one name, two descriptions.
    wf = namesSetup(&in);
    fa = open("n/a", O_WRONLY);
    check("open", fa);
    int fa2 = open("n/a", O_WRONLY);
    check("open", fa2);
    check("unlink", unlink("n/b"));
    line("N", "same-unlink-b", "%s", drain(in));
    check("unlink", unlink("n/a"));
    line("N", "same-unlink-a", "%s", drain(in));
    close(fa);
    line("N", "same-close-first", "%s", drain(in));
    close(fa2);
    line("N", "same-close-second", "%s", drain(in));
    close(in);

    // Opened through b, b unlinked and linked again, and opened again: two
    // descriptions through one name, which are two names.
    wf = namesSetup(&in);
    int old = open("n/b", O_WRONLY);
    check("open", old);
    check("unlink", unlink("n/b"));
    check("link", link("n/a", "n/b"));
    int renewed = open("n/b", O_WRONLY);
    check("open", renewed);
    check("unlink", unlink("n/a"));
    drain(in);
    check("unlink", unlink("n/b"));
    line("N", "relinked-unlink-last-name", "%s", drain(in));
    close(old);
    line("N", "relinked-close-old", "%s", drain(in));
    close(renewed);
    line("N", "relinked-close-renewed", "%s", drain(in));
    close(in);

    // Opened through a, a unlinked, and the last name b renamed over.
    wf = namesSetup(&in);
    writeFile("n/x", "x");
    fa = open("n/a", O_WRONLY);
    check("open", fa);
    check("unlink", unlink("n/a"));
    drain(in);
    check("rename", rename("n/x", "n/b"));
    line("N", "rename-over-last-name", "%s", drain(in));
    close(fa);
    line("N", "rename-over-close", "%s", drain(in));
    close(in);
}

static void sectionR(void)
{
    fresh("r");
    writeFile("r/s", "s");
    writeFile("r/t", "t");
    int in = inotify_init1(IN_NONBLOCK);
    int wr = inotify_add_watch(in, "r", graphEvents);
    int ws = inotify_add_watch(in, "r/s", graphEvents);
    int wt = inotify_add_watch(in, "r/t", graphEvents);
    line("R", "wds", "r=%d s=%d t=%d", wr, ws, wt);
    check("rename", rename("r/s", "r/t"));
    line("R", "watched-over-watched", "%s", drain(in));
    // r/t is now the file s was. Give it a second name and rename that.
    check("link", link("r/t", "r/u"));
    drain(in);
    check("rename", rename("r/u", "r/v"));
    line("R", "other-name-of-watched", "%s", drain(in));
    check("rename", rename("r/v", "r/t"));
    line("R", "name-onto-same-file", "%s", drain(in));
    check("rename", rename("r/t", "r/t"));
    line("R", "name-onto-itself", "%s", drain(in));
    check("unlink", unlink("r/v"));
    drain(in);
    close(in);

    fresh("r");
    check("mkdir", mkdir("r/sd", 0755));
    check("mkdir", mkdir("r/td", 0755));
    in = inotify_init1(IN_NONBLOCK);
    wr = inotify_add_watch(in, "r", graphEvents);
    int wsd = inotify_add_watch(in, "r/sd", graphEvents);
    int wtd = inotify_add_watch(in, "r/td", graphEvents);
    line("R", "dir-wds", "r=%d sd=%d td=%d", wr, wsd, wtd);
    check("rename", rename("r/sd", "r/td"));
    line("R", "watched-dir-over-watched-dir", "%s", drain(in));
    close(in);

    fresh("r");
    writeFile("r/s", "s");
    in = inotify_init1(IN_NONBLOCK);
    ws = inotify_add_watch(in, "r/s", graphEvents & ~IN_MOVE_SELF);
    check("rename", rename("r/s", "r/s2"));
    line("R", "no-move-self-asked", "wd=%d: %s", ws, drain(in));
    close(in);
}

static void sectionD(void)
{
    fresh("d");
    check("mkdir", mkdir("d/sub", 0755));
    int in = inotify_init1(IN_NONBLOCK);
    int wd = inotify_add_watch(in, "d", dotnetMask);
    int ws = inotify_add_watch(in, "d/sub", dotnetMask);
    line("D", "dotnet-wds", "d=%d sub=%d", wd, ws);
    check("rmdir", rmdir("d/sub"));
    line("D", "dotnet-rmdir", "%s", drain(in));
    close(in);

    fresh("d");
    check("mkdir", mkdir("d/sub", 0755));
    int dfd = open("d/sub", O_RDONLY | O_DIRECTORY);
    check("open", dfd);
    in = inotify_init1(IN_NONBLOCK);
    wd = inotify_add_watch(in, "d", graphEvents);
    ws = inotify_add_watch(in, "d/sub", graphEvents);
    line("D", "held-wds", "d=%d sub=%d", wd, ws);
    check("rmdir", rmdir("d/sub"));
    line("D", "held-rmdir", "%s", drain(in));
    close(dfd);
    line("D", "held-close", "%s", drain(in));
    close(in);

    char cwd[4096];
    if (!getcwd(cwd, sizeof cwd)) { perror("getcwd"); exit(1); }
    fresh("d");
    check("mkdir", mkdir("d/sub", 0755));
    in = inotify_init1(IN_NONBLOCK);
    wd = inotify_add_watch(in, "d", graphEvents);
    ws = inotify_add_watch(in, "d/sub", graphEvents);
    line("D", "cwd-wds", "d=%d sub=%d", wd, ws);
    check("chdir", chdir("d/sub"));
    char sub[4200];
    snprintf(sub, sizeof sub, "%s/d/sub", cwd);
    check("rmdir", rmdir(sub));
    line("D", "cwd-rmdir", "%s", drain(in));
    check("chdir", chdir(cwd));
    line("D", "cwd-leave", "%s", drain(in));
    close(in);

    fresh("d");
    check("mkdir", mkdir("d/a", 0755));
    check("mkdir", mkdir("d/a/b", 0755));
    in = inotify_init1(IN_NONBLOCK);
    wd = inotify_add_watch(in, "d", graphEvents);
    int wa = inotify_add_watch(in, "d/a", graphEvents);
    int wb = inotify_add_watch(in, "d/a/b", graphEvents);
    line("D", "chain-wds", "d=%d a=%d b=%d", wd, wa, wb);
    check("chdir", chdir("d/a/b"));
    char a[4200], b[4200];
    snprintf(a, sizeof a, "%s/d/a", cwd);
    snprintf(b, sizeof b, "%s/d/a/b", cwd);
    check("rmdir", rmdir(b));
    line("D", "chain-rmdir-inner", "%s", drain(in));
    check("rmdir", rmdir(a));
    line("D", "chain-rmdir-outer", "%s", drain(in));
    check("chdir", chdir(cwd));
    line("D", "chain-leave", "%s", drain(in));
    close(in);
}

static void sectionW(void)
{
    fresh("w");
    writeFile("w/f", "x");
    int one = inotify_init1(IN_NONBLOCK);
    check("init", one);
    check("watch", inotify_add_watch(one, "w/f", selfEvents));
    int two = inotify_init1(IN_NONBLOCK);
    check("init", two);
    check("watch", inotify_add_watch(two, "w", graphEvents));
    check("watch", inotify_add_watch(two, "w/f", graphEvents));
    check("unlink", unlink("w/f"));
    line("W", "first", "%s", drain(one));
    line("W", "second", "%s", drain(two));
    close(one);
    close(two);
}

// The last few records of a drain, and how many there were.
static void tailDrain(const char *section, const char *name, int fd)
{
    static char buf[1 << 16];
    char tail[3][512];
    int count = 0;
    for (;;) {
        ssize_t n = read(fd, buf, sizeof buf);
        if (n < 0) break;
        ssize_t pos = 0;
        while (pos + (ssize_t)sizeof(struct inotify_event) <= n) {
            const struct inotify_event *e = (const struct inotify_event *)(buf + pos);
            snprintf(tail[count % 3], sizeof tail[0], "wd=%d mask=%s cookie=%s len=%u name=\"%s\"",
                     e->wd, maskNames(e->mask), cookieName(e->cookie), e->len, e->len ? e->name : "");
            count++;
            pos += sizeof(struct inotify_event) + e->len;
        }
    }
    char out[2048];
    snprintf(out, sizeof out, "records=%d", count);
    for (int i = count < 3 ? 0 : count - 3; i < count; i++) {
        strcat(out, " | ");
        strcat(out, tail[i % 3]);
    }
    line(section, name, "%s", out);
}

static void sectionQ(void)
{
    // Read the limit rather than assume it.
    FILE *limit = fopen("/proc/sys/fs/inotify/max_queued_events", "r");
    int max = 0;
    if (!limit || fscanf(limit, "%d", &max) != 1) { perror("max_queued_events"); exit(1); }
    fclose(limit);
    for (int shortBy = 1; shortBy <= 2; shortBy++) {
        fresh("q");
        writeFile("q/x", "x");
        writeFile("q/y", "y");
        writeFile("q/f", "f");
        int in = inotify_init1(IN_NONBLOCK);
        check("watch", inotify_add_watch(in, "q/x", IN_ATTRIB));
        check("watch", inotify_add_watch(in, "q/y", IN_ATTRIB));
        int wf = inotify_add_watch(in, "q/f", IN_DELETE_SELF);
        check("watch", wf);
        for (int i = 0; i < max - shortBy; i++) check("chmod", chmod(i % 2 ? "q/y" : "q/x", 0644));
        check("unlink", unlink("q/f"));
        char name[64];
        snprintf(name, sizeof name, "short-by-%d", shortBy);
        tailDrain("Q", name, in);
        snprintf(name, sizeof name, "short-by-%d-rm-after", shortBy);
        line("Q", name, "%s", rc(inotify_rm_watch(in, wf)));
        close(in);
    }
}

// A child opens each of `order`'s files in x/ in turn (a letter per open),
// unlinks every one, and exits; the parent watches each file.
static void exitCase(const char *name, const char *order)
{
    fresh("x");
    writeFile("x/a", "a");
    writeFile("x/b", "b");
    int in = inotify_init1(IN_NONBLOCK);
    int wa = inotify_add_watch(in, "x/a", selfEvents);
    int wb = inotify_add_watch(in, "x/b", selfEvents);
    pid_t child = fork();
    if (child == 0) {
        for (const char *c = order; *c; c++) {
            char path[8];
            snprintf(path, sizeof path, "x/%c", *c);
            if (open(path, O_RDONLY) < 0) _exit(2);
        }
        if (unlink("x/a") < 0 || unlink("x/b") < 0) _exit(3);
        _exit(0);
    }
    int status;
    check("waitpid", waitpid(child, &status, 0));
    line("X", name, "a=%d b=%d status=%d: %s", wa, wb, status, drain(in));
    close(in);
}

static void sectionX(void)
{
    exitCase("open-a-b-a", "aba");
    exitCase("open-a-b", "ab");
    exitCase("open-b-a", "ba");
}

int main(void)
{
    FILE *u = popen("uname -rm", "r");
    char release[128] = "";
    if (u && fgets(release, sizeof release, u)) fputs(release, stdout);
    if (u) pclose(u);
    sectionU();
    sectionN();
    sectionR();
    sectionD();
    sectionW();
    sectionQ();
    sectionX();
    return 0;
}
