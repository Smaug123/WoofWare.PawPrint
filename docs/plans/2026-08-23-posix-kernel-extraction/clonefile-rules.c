// Measures Darwin's clonefile(2) (clonefileat(AT_FDCWD, src, AT_FDCWD, dst,
// flags)) and libc's fcopyfile(3) with COPYFILE_ALL, which are what a Darwin
// CoreLib's File.Copy reaches: what the new name gets (mode, owner, the four
// timestamps), which errno each refusal carries, and the order the checks are
// made in.
//
// Darwin: nix develop -c clang -Wall -o clonefile-rules clonefile-rules.c && ./clonefile-rules "$(mktemp -d /private/tmp/clonep.XXXXXX)"
//         As an ordinary user. The base directory is chowned to the caller's
//         own group first, since /private/tmp is group wheel and an ordinary
//         user cannot set S_ISGID on a file in a group it is not in
//         (reference/probing.md); a second directory, `w`, is left in wheel.
//
// Every value printed is read back with stat(2) after the call, so a bit that
// did not stick reads as absent rather than as preserved.
#include <copyfile.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/clonefile.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

static void die(const char *what) {
    perror(what);
    exit(2);
}

static const char *en(int e) {
    static char buf[32];
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case EEXIST: return "EEXIST";
    case ELOOP: return "ELOOP";
    case EINVAL: return "EINVAL";
    case EXDEV: return "EXDEV";
    case ENOTSUP: return "ENOTSUP";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static void p(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    fflush(stdout);
}

static void wipe(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return;
    if (S_ISDIR(st.st_mode)) {
        chmod(path, 0700);
        char cmd[PATH_MAX + 32];
        snprintf(cmd, sizeof cmd, "chmod -R u+rwx '%s'; rm -rf '%s'", path, path);
        if (system(cmd) != 0) die("rm -rf");
    } else {
        if (unlink(path) != 0) die("unlink");
    }
}

static void mkfile(const char *path, const char *content, mode_t mode) {
    wipe(path);
    int fd = open(path, O_CREAT | O_EXCL | O_WRONLY, 0600);
    if (fd < 0) die(path);
    if (write(fd, content, strlen(content)) != (ssize_t)strlen(content)) die("write");
    if (fchmod(fd, mode) != 0) die("fchmod");
    close(fd);
}

// Old, distinct, sub-second times, so that a copied time cannot be mistaken
// for a coincidence with the clock.
static void settimes(const char *path, long atime, long mtime) {
    struct timespec ts[2] = { { atime, 111111111 }, { mtime, 222222222 } };
    if (utimensat(AT_FDCWD, path, ts, 0) != 0) die("utimensat");
}

static void show(const char *label, const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) {
        p("  %s: %s\n", label, en(errno));
        return;
    }
    p("  %s: type=%s mode=%04o uid=%u gid=%u nlink=%u size=%lld a=%ld.%09ld m=%ld.%09ld c=%ld.%09ld b=%ld.%09ld ino=%llu\n",
      label, S_ISDIR(st.st_mode) ? "dir" : S_ISLNK(st.st_mode) ? "link" : S_ISREG(st.st_mode) ? "file" : "other",
      st.st_mode & 07777, st.st_uid, st.st_gid, st.st_nlink, (long long)st.st_size,
      st.st_atimespec.tv_sec, st.st_atimespec.tv_nsec, st.st_mtimespec.tv_sec, st.st_mtimespec.tv_nsec,
      st.st_ctimespec.tv_sec, st.st_ctimespec.tv_nsec, st.st_birthtimespec.tv_sec, st.st_birthtimespec.tv_nsec,
      (unsigned long long)st.st_ino);
}

static int clone_errno(const char *src, const char *dst, int flags) {
    errno = 0;
    int r = clonefile(src, dst, flags);
    return r == 0 ? 0 : errno;
}

// ---------------------------------------------------------------------------
// MODE: every mode the caller can give a source it owns, in a directory of its
// own group (so S_ISGID sticks) and in a wheel directory (so it cannot).
static void modes(const char *dir, const char *tag) {
    int mismatches_keep_all = 0, mismatches_drop_suid = 0, mismatches_drop_both = 0, refusals = 0, total = 0;
    char src[PATH_MAX], dst[PATH_MAX];
    snprintf(src, sizeof src, "%s/msrc", dir);
    snprintf(dst, sizeof dst, "%s/mdst", dir);
    for (int m = 0; m <= 07777; m++) {
        mkfile(src, "x", m);
        struct stat s;
        if (stat(src, &s) != 0) die("stat src");
        int have = s.st_mode & 07777;
        wipe(dst);
        int e = clone_errno(src, dst, CLONE_ACL);
        total++;
        if (e != 0) {
            if (refusals < 4) p("  MODE %s src=%04o (asked %04o): %s\n", tag, have, m, en(e));
            refusals++;
            continue;
        }
        struct stat d;
        if (stat(dst, &d) != 0) die("stat dst");
        int got = d.st_mode & 07777;
        if (got != have) { if (mismatches_keep_all < 4) p("  MODE %s keep-all mismatch src=%04o dst=%04o\n", tag, have, got); mismatches_keep_all++; }
        if (got != (have & ~04000)) { if (mismatches_drop_suid < 4) p("  MODE %s drop-suid mismatch src=%04o dst=%04o\n", tag, have, got); mismatches_drop_suid++; }
        if (got != (have & ~06000)) { if (mismatches_drop_both < 4) p("  MODE %s drop-both mismatch src=%04o dst=%04o\n", tag, have, got); mismatches_drop_both++; }
    }
    wipe(src);
    wipe(dst);
    p("MODE %s: %d sources, %d refused; mismatches: keep-all %d, drop-S_ISUID %d, drop-both %d\n",
      tag, total, refusals, mismatches_keep_all, mismatches_drop_suid, mismatches_drop_both);
}

// ---------------------------------------------------------------------------
// META: the owner, the timestamps and the umask.
static void meta(const char *base) {
    char d1[PATH_MAX], d0[PATH_MAX], src[PATH_MAX], dst[PATH_MAX];
    snprintf(d1, sizeof d1, "%s/meta", base);      // caller's group
    snprintf(d0, sizeof d0, "%s/w/meta", base);    // wheel
    wipe(d1);
    if (mkdir(d1, 0755) != 0) die("mkdir meta");
    wipe(d0);
    if (mkdir(d0, 0755) != 0) die("mkdir w/meta");

    // Source in the caller's group, clone in the same directory.
    snprintf(src, sizeof src, "%s/src", d1);
    snprintf(dst, sizeof dst, "%s/dst", d1);
    mkfile(src, "hello", 0640);
    settimes(src, 1000000000, 900000000);
    sleep(1);
    p("META same-group directory\n");
    show("src before", src);
    int e = clone_errno(src, dst, CLONE_ACL);
    p("  clonefile: %s\n", en(e));
    show("src after", src);
    show("dst", dst);

    // Source in the caller's group, clone into a wheel directory.
    char dstw[PATH_MAX];
    snprintf(dstw, sizeof dstw, "%s/dst", d0);
    p("META clone into a wheel directory of a staff-group source\n");
    e = clone_errno(src, dstw, CLONE_ACL);
    p("  clonefile: %s\n", en(e));
    show("dst", dstw);

    // A wheel-group source (created in the wheel directory), cloned into the
    // caller's-group directory.
    char srcw[PATH_MAX], dst2[PATH_MAX];
    snprintf(srcw, sizeof srcw, "%s/srcw", d0);
    snprintf(dst2, sizeof dst2, "%s/fromwheel", d1);
    mkfile(srcw, "wheel", 0644);
    p("META clone of a wheel-group source into a staff directory\n");
    show("src", srcw);
    e = clone_errno(srcw, dst2, CLONE_ACL);
    p("  clonefile: %s\n", en(e));
    show("dst", dst2);

    // The umask plays no part.
    p("META umask 0777\n");
    char dstu[PATH_MAX];
    snprintf(dstu, sizeof dstu, "%s/dstu", d1);
    mode_t old = umask(0777);
    e = clone_errno(src, dstu, CLONE_ACL);
    umask(old);
    p("  clonefile: %s\n", en(e));
    show("dst", dstu);

    // An empty source, and one whose mtime is later than its birth.
    p("META empty source\n");
    char se[PATH_MAX], de[PATH_MAX];
    snprintf(se, sizeof se, "%s/empty", d1);
    snprintf(de, sizeof de, "%s/dempty", d1);
    mkfile(se, "", 0600);
    e = clone_errno(se, de, 0);
    p("  clonefile: %s\n", en(e));
    show("src", se);
    show("dst", de);

    // fcopyfile(COPYFILE_ALL) onto an existing file the caller owns, of a
    // different mode, group and times: what COPYFILE_ALL carries across.
    p("FCOPYFILE onto an existing file\n");
    char fd1[PATH_MAX];
    snprintf(fd1, sizeof fd1, "%s/existing", d0);   // wheel group, so the gid can move
    mkfile(fd1, "previous content, longer", 0606);
    settimes(fd1, 1200000000, 1300000000);
    for (int round = 0; round < 3; round++) {
        int srcmode = round == 0 ? 0640 : round == 1 ? 04755 : 02750;
        mkfile(src, "hello", srcmode);
        settimes(src, 1000000000, 900000000);
        mkfile(fd1, "previous content, longer", 0606);
        settimes(fd1, 1200000000, 1300000000);
        show("src", src);
        show("dst before", fd1);
        int in = open(src, O_RDONLY), out = open(fd1, O_RDWR);
        if (in < 0 || out < 0) die("open fcopyfile");
        errno = 0;
        int r = fcopyfile(in, out, NULL, COPYFILE_ALL);
        int ce = r == 0 ? 0 : errno;
        p("  fcopyfile: r=%d %s errno-after=%s\n", r, en(ce), en(errno));
        off_t ipos = lseek(in, 0, SEEK_CUR), opos = lseek(out, 0, SEEK_CUR);
        p("  offsets after: in=%lld out=%lld\n", (long long)ipos, (long long)opos);
        close(in);
        close(out);
        show("dst after", fd1);
    }
    // fcopyfile onto a fresh empty file the caller just created (the BCL's
    // fallback shape: dst opened O_CREAT and truncated).
    p("FCOPYFILE onto a freshly created file\n");
    char fresh[PATH_MAX];
    snprintf(fresh, sizeof fresh, "%s/fresh", d1);
    mkfile(src, "hello", 0640);
    settimes(src, 1000000000, 900000000);
    wipe(fresh);
    {
        int in = open(src, O_RDONLY), out = open(fresh, O_RDWR | O_CREAT | O_EXCL, 0640);
        if (in < 0 || out < 0) die("open fresh");
        errno = 0;
        int r = fcopyfile(in, out, NULL, COPYFILE_ALL);
        p("  fcopyfile: r=%d %s\n", r, en(r == 0 ? 0 : errno));
        close(in);
        close(out);
    }
    show("src", src);
    show("dst", fresh);
    // fcopyfile of an empty source.
    p("FCOPYFILE of an empty source\n");
    mkfile(se, "", 0600);
    mkfile(fresh, "longer previous", 0644);
    {
        int in = open(se, O_RDONLY), out = open(fresh, O_RDWR);
        int r = fcopyfile(in, out, NULL, COPYFILE_ALL);
        p("  fcopyfile: r=%d %s\n", r, en(r == 0 ? 0 : errno));
        close(in);
        close(out);
    }
    show("dst", fresh);
    // fcopyfile when the destination was opened read-only, and the source
    // write-only.
    p("FCOPYFILE descriptor modes\n");
    mkfile(src, "hello", 0640);
    mkfile(fresh, "previous", 0644);
    {
        int in = open(src, O_RDONLY), out = open(fresh, O_RDONLY);
        int r = fcopyfile(in, out, NULL, COPYFILE_ALL);
        p("  out read-only: r=%d %s\n", r, en(r == 0 ? 0 : errno));
        close(in);
        close(out);
        show("dst", fresh);
        in = open(src, O_WRONLY);
        out = open(fresh, O_RDWR);
        r = fcopyfile(in, out, NULL, COPYFILE_ALL);
        p("  in write-only: r=%d %s\n", r, en(r == 0 ? 0 : errno));
        close(in);
        close(out);
        show("dst", fresh);
    }
}

// ---------------------------------------------------------------------------
// ERR: one row per refusal, each from a fresh tree.
struct row {
    const char *name;
    const char *src;
    const char *dst;
    int flags;
};

static char ebase[PATH_MAX];

static void tree(void) {
    wipe(ebase);
    if (mkdir(ebase, 0755) != 0) die("mkdir ebase");
    if (chdir(ebase) != 0) die("chdir");
    mkfile("f", "hello", 0644);
    mkfile("g", "other", 0644);
    if (link("f", "hf") != 0) die("link");
    if (mkdir("d", 0755) != 0) die("mkdir d");
    mkfile("d/in", "in", 0644);
    if (mkdir("e", 0755) != 0) die("mkdir e");
    if (symlink("f", "lf") != 0) die("symlink lf");
    if (symlink("d", "ld") != 0) die("symlink ld");
    if (symlink("nx", "dang") != 0) die("symlink dang");
    if (symlink("d/nx", "dangd") != 0) die("symlink dangd");
    if (symlink("cyc", "cyc") != 0) die("symlink cyc");
    if (mkdir("ro", 0755) != 0) die("mkdir ro");
    mkfile("ro/x", "x", 0666);
    if (chmod("ro", 0555) != 0) die("chmod ro");
    if (mkdir("ns", 0755) != 0) die("mkdir ns");
    mkfile("ns/x", "x", 0644);
    if (chmod("ns", 0666) != 0) die("chmod ns");
    mkfile("unr", "secret", 0000);
    mkfile("wo", "secret", 0200);
}

static void errors(void) {
    char name300[300];
    memset(name300, 'n', 299);
    name300[299] = 0;
    char longdst[310];
    snprintf(longdst, sizeof longdst, "%s", name300);

    struct row rows[] = {
        { "new name", "f", "new", CLONE_ACL },
        { "dst exists (file)", "f", "g", CLONE_ACL },
        { "dst exists (dir)", "f", "e", CLONE_ACL },
        { "dst exists (dir/)", "f", "e/", CLONE_ACL },
        { "dst exists (link to file)", "f", "lf", CLONE_ACL },
        { "dst exists (link to dir)", "f", "ld", CLONE_ACL },
        { "dst exists (dangling link)", "f", "dang", CLONE_ACL },
        { "dst exists (dangling link into d)", "f", "dangd", CLONE_ACL },
        { "dst exists (dangling link), NOFOLLOW", "f", "dang", CLONE_ACL | CLONE_NOFOLLOW },
        { "dst exists (cyclic link)", "f", "cyc", CLONE_ACL },
        { "dst is src", "f", "f", CLONE_ACL },
        { "dst is a hard link to src", "f", "hf", CLONE_ACL },
        { "dst trailing slash, absent", "f", "new/", CLONE_ACL },
        { "dst parent absent", "f", "nx/new", CLONE_ACL },
        { "dst parent a file", "f", "f/new", CLONE_ACL },
        { "dst parent a link to dir", "f", "ld/new", CLONE_ACL },
        { "dst parent unwritable", "f", "ro/new", CLONE_ACL },
        { "dst existing in unwritable", "f", "ro/x", CLONE_ACL },
        { "dst parent unsearchable", "f", "ns/new", CLONE_ACL },
        { "dst empty", "f", "", CLONE_ACL },
        { "dst name 299 bytes", "f", longdst, CLONE_ACL },
        { "src absent", "nx", "new", CLONE_ACL },
        { "src absent, dst exists", "nx", "g", CLONE_ACL },
        { "src absent, dst parent unwritable", "nx", "ro/new", CLONE_ACL },
        { "src empty", "", "new", CLONE_ACL },
        { "src trailing slash", "f/", "new", CLONE_ACL },
        { "src through a file", "f/x", "new", CLONE_ACL },
        { "src link to file", "lf", "new", CLONE_ACL },
        { "src link to file, NOFOLLOW", "lf", "new", CLONE_ACL | CLONE_NOFOLLOW },
        { "src dangling link", "dang", "new", CLONE_ACL },
        { "src dangling link, NOFOLLOW", "dang", "new", CLONE_ACL | CLONE_NOFOLLOW },
        { "src cyclic link", "cyc", "new", CLONE_ACL },
        { "src directory", "d", "new", CLONE_ACL },
        { "src link to directory", "ld", "new", CLONE_ACL },
        { "src unreadable (0000)", "unr", "new", CLONE_ACL },
        { "src write-only (0200)", "wo", "new", CLONE_ACL },
        { "src unreadable, dst exists", "unr", "g", CLONE_ACL },
        { "src in unsearchable", "ns/x", "new", CLONE_ACL },
        { "src in unsearchable, dst exists", "ns/x", "g", CLONE_ACL },
        { "dst exists, dst parent unwritable, src absent", "nx", "ro/x", CLONE_ACL },
        { "src on another volume", "/etc/hosts", "new", CLONE_ACL },
        { "flags 0", "f", "new", 0 },
        { "flags NOOWNERCOPY", "f", "new", CLONE_NOOWNERCOPY },
        { "flags bad bit, src absent", "nx", "new", 1 << 20 },
        { "flags bad bit, dst exists", "f", "g", 1 << 20 },
        { "dst name not UTF-8", "f", "\xff\xfe", CLONE_ACL },
        { "dst name not UTF-8, parent unwritable", "f", "ro/\xff\xfe", CLONE_ACL },
        { "dst name not UTF-8, src unreadable", "unr", "\xff\xfe", CLONE_ACL },
        { "dst name 299 bytes, src unreadable", "unr", longdst, CLONE_ACL },
        { "dst parent absent, src unreadable", "unr", "nx/new", CLONE_ACL },
        { "dst parent unsearchable, src unreadable", "unr", "ns/new", CLONE_ACL },
        { "dst cyclic link, src unreadable", "unr", "cyc", CLONE_ACL },
        { "dst root", "f", "/", CLONE_ACL },
        { "dst dot", "f", ".", CLONE_ACL },
    };
    for (size_t i = 0; i < sizeof rows / sizeof rows[0]; i++) {
        tree();
        int e = clone_errno(rows[i].src, rows[i].dst, rows[i].flags);
        p("ERR %-48s clonefile(%s, %.20s%s, 0x%x) = %s\n", rows[i].name, rows[i].src, rows[i].dst,
          strlen(rows[i].dst) > 20 ? "..." : "", rows[i].flags, en(e));
        // What the call created or changed.
        if (strcmp(rows[i].name, "new name") == 0 || e == 0) show("dst", rows[i].dst);
        if (strstr(rows[i].name, "dangling link)") != NULL || strstr(rows[i].name, "dangling link into") != NULL) {
            show("nx", "nx");
            show("d/nx", "d/nx");
            show("dang", "dang");
        }
        if (strstr(rows[i].name, "NOFOLLOW") != NULL && strstr(rows[i].name, "src") != NULL) show("new", "new");
        if (chdir("/") != 0) die("chdir /");
    }
    // The parent's timestamps, around a clone into it.
    tree();
    {
        struct timespec ts[2] = { { 1000000000, 0 }, { 1000000000, 0 } };
        if (utimensat(AT_FDCWD, "e", ts, 0) != 0) die("utimensat e");
        show("parent e before", "e");
        int e = clone_errno("f", "e/new", CLONE_ACL);
        p("PARENT clonefile(f, e/new) = %s\n", en(e));
        show("parent e after", "e");
        show("src f after", "f");
    }
    if (chdir("/") != 0) die("chdir /");
    // From inside a directory whose last name has gone.
    tree();
    {
        char srcabs[PATH_MAX];
        snprintf(srcabs, sizeof srcabs, "%s/f", ebase);
        if (mkdir("orph", 0755) != 0) die("mkdir orph");
        if (chdir("orph") != 0) die("chdir orph");
        if (rmdir("../orph") != 0) die("rmdir orph");
        int e = clone_errno(srcabs, "new", CLONE_ACL);
        p("ORPHAN clonefile(abs f, new) from a removed cwd = %s\n", en(e));
    }
    if (chdir("/") != 0) die("chdir /");
    // Every single flag bit, on a fresh name.
    p("FLAGS:");
    for (int bit = 0; bit < 32; bit++) {
        tree();
        int e = clone_errno("f", "new", 1u << bit);
        p(" %d=%s", bit, en(e));
        if (chdir("/") != 0) die("chdir /");
    }
    p("\n");
    wipe(ebase);
}

int main(int argc, char **argv) {
    alarm(600);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <empty directory on APFS>\n", argv[0]);
        return 2;
    }
    const char *base = argv[1];
    if (chown(base, -1, getegid()) != 0) die("chown base");
    char w[PATH_MAX];
    snprintf(w, sizeof w, "%s/w", base);
    if (mkdir(w, 0755) != 0) die("mkdir w");
    if (chown(w, -1, 0) != 0) {
        // An ordinary user may not give a directory to wheel; a directory
        // created under /private/tmp is wheel already, so use one there.
        char cmd[PATH_MAX];
        rmdir(w);
        snprintf(cmd, sizeof cmd, "/private/tmp/clonep-wheel-%d", getpid());
        if (mkdir(cmd, 0755) != 0) die("mkdir wheel dir");
        if (symlink(cmd, w) != 0) die("symlink w");
    }
    struct stat st, bs;
    if (stat(w, &st) != 0) die("stat w");
    if (stat(base, &bs) != 0) die("stat base");
    p("base gid=%u, w gid=%u, egid=%u, euid=%u\n", bs.st_gid, st.st_gid, getegid(), geteuid());
    char mdir[PATH_MAX];
    snprintf(mdir, sizeof mdir, "%s/m", base);
    if (mkdir(mdir, 0755) != 0) die("mkdir m");
    modes(mdir, "staff-dir");
    char mw[PATH_MAX];
    snprintf(mw, sizeof mw, "%s/w/m", base);
    if (mkdir(mw, 0755) != 0) die("mkdir w/m");
    modes(mw, "wheel-dir");
    meta(base);
    snprintf(ebase, sizeof ebase, "%s/err", base);
    errors();
    return 0;
}
