// Measures the permission rules that depend on how a caller stands towards an
// inode it does not necessarily own: which permission triple applies, the
// sticky directory's EPERM and where it falls among the other refusals, the
// set-group-ID bit a write or truncation strips, and the set-group-ID bit a
// creating open strips.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/permission-standing.c && /tmp/p /tmp/standing && /tmp/p /dev/shm/standing'
//         Run it as root: each row forks a child that takes the credentials
//         under test, against inodes root has chowned to owners and groups the
//         child is and is not a member of.
// Darwin: nix develop -c clang -Wall -o permission-standing permission-standing.c && ./permission-standing "$(mktemp -d /private/tmp/standing.XXXXXX)"
//         As an ordinary user, which cannot chown. The sticky rows use hard
//         links to a root-owned file in root-owned sticky /private/tmp, and
//         root-owned directories the caller may not remove; every one of them
//         is harmless whatever the kernel answers (it removes or moves at most
//         the probe's own link). The two links it makes in /private/tmp
//         survive the run, because the sticky bit forbids their removal; they
//         go when /private/tmp is next cleaned.
//
// Measured on Linux 6.18.5 (aarch64, root in the container, ext4 and tmpfs
// alike) and Darwin 27.0 (arm64, uid 501) on 2026-09-27. The output is beside
// this file, and the rows are transcribed in
// WoofWare.PosixKernel.Test/TestPermissionStanding.fs.
//
// Each sweep compares every row against the rule this library models and
// prints the mismatch count, and the first few mismatches.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static char base[PATH_MAX];

static void die(const char *what) {
    perror(what);
    exit(2);
}

static void p(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    fflush(stdout);
}

static const char *en(int e) {
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case ENOTEMPTY: return "ENOTEMPTY";
    case EEXIST: return "EEXIST";
    case EINVAL: return "EINVAL";
    case EXDEV: return "EXDEV";
    case EBUSY: return "EBUSY";
    default: return strerror(e);
    }
}

static int R(int rc) { return rc == 0 ? 0 : errno; }

static void path(char *out, const char *dir, const char *name) { snprintf(out, PATH_MAX, "%s/%s", dir, name); }

static int mode_of(const char *pth) {
    struct stat st;
    if (lstat(pth, &st) != 0) return -1;
    return st.st_mode & 07777;
}

static void mkfile(const char *pth) {
    int fd = open(pth, O_CREAT | O_EXCL | O_WRONLY, 0600);
    if (fd < 0) die(pth);
    if (write(fd, "abcd", 4) != 4) die("write");
    close(fd);
}

#ifdef __linux__
// ---------------------------------------------------------------- credentials

typedef struct {
    const char *name;
    uid_t ruid, euid;
    gid_t rgid, egid;
    int ng;
    gid_t g[4];
} cred;

static const cred ROOT = {"root", 0, 0, 0, 0, 1, {0}};
static const cred U1000 = {"u1000(g1000;groups 1000,2000)", 1000, 1000, 1000, 1000, 2, {1000, 2000}};
// Real and effective ids differ; nothing else does.
static const cred R1001E1000 = {"r1001/e1000(rg1001/eg1000;groups none)", 1001, 1000, 1001, 1000, 0, {0}};
static const cred R1000E1000G2000 = {"u1000(rg2000/eg1000;groups none)", 1000, 1000, 2000, 1000, 0, {0}};

static void become(const cred *c) {
    if (setgroups(c->ng, c->g) != 0) die("setgroups");
    if (setresgid(c->rgid, c->egid, c->egid) != 0) die("setresgid");
    if (setresuid(c->ruid, c->euid, c->euid) != 0) die("setresuid");
}

typedef void (*body)(void *);

static void as(const cred *c, body fn, void *arg) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(120);
        become(c);
        fn(arg);
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) p("CHILD-FAILED\t%s\tstatus=%d\n", c->name, status);
}

static void rmrf(const char *pth) {
    char cmd[PATH_MAX + 32];
    snprintf(cmd, sizeof cmd, "rm -rf '%s'", pth);
    if (system(cmd) != 0) die("rm -rf");
}

static void owned(const char *pth, uid_t u, gid_t g, int mode) {
    if (lchown(pth, u, g) != 0) die("chown");
    if (chmod(pth, mode) != 0) die("chmod");
    if (mode_of(pth) != mode) {
        p("SETUP-FAILED\t%s\twanted %04o got %04o\n", pth, mode, mode_of(pth));
        exit(2);
    }
}

// ---------------------------------------------------------------- which triple applies, over all 4096 modes

// standing: 0 owner, 1 group, 2 other, 3 privileged
static int predict_access(int standing, int isdir, int mode) {
    if (standing == 3) return isdir ? 7 : 3;
    int bits = standing == 0 ? (mode >> 6) & 7 : standing == 1 ? (mode >> 3) & 7 : mode & 7;
    int r = (bits & 4) != 0, w = (bits & 2) != 0, x = (bits & 1) != 0;
    if (!isdir) return (r ? 1 : 0) | (w ? 2 : 0);
    // search; read (opendir); create an entry, which needs write and search
    return (x ? 1 : 0) | (r ? 2 : 0) | ((w && x) ? 4 : 0);
}

typedef struct {
    const char *label;
    int standing;
} access_arg;

static void access_body(void *v) {
    const access_arg *a = v;
    for (int isdir = 0; isdir <= 1; isdir++) {
        int mismatches = 0;
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "acc_%s_%04o", isdir ? "d" : "f", mode);
            path(t, base, nm);
            int got = 0;
            if (!isdir) {
                int fd = open(t, O_RDONLY);
                if (fd >= 0) { got |= 1; close(fd); }
                fd = open(t, O_WRONLY);
                if (fd >= 0) { got |= 2; close(fd); }
            } else {
                char kid[PATH_MAX], c[PATH_MAX];
                struct stat st;
                path(kid, t, "kid");
                path(c, t, "new");
                if (stat(kid, &st) == 0) got |= 1;
                DIR *d = opendir(t);
                if (d) { got |= 2; closedir(d); }
                int fd = open(c, O_CREAT | O_EXCL | O_WRONLY, 0600);
                if (fd >= 0) { got |= 4; close(fd); unlink(c); }
            }
            int want = predict_access(a->standing, isdir, mode);
            if (got != want) {
                if (mismatches < 8) p("ACCESS-MISMATCH\t%s\t%s\tmode=%04o\tgot=%d\twant=%d\n", a->label, isdir ? "dir" : "file", mode, got, want);
                mismatches++;
            }
        }
        p("ACCESS-SWEEP\t%s\t%s\tmodes=4096\tmismatches=%d\n", a->label, isdir ? "dir" : "file", mismatches);
    }
}

static void access_sweep(const char *label, const cred *c, int standing, uid_t ou, gid_t og) {
    for (int isdir = 0; isdir <= 1; isdir++) {
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "acc_%s_%04o", isdir ? "d" : "f", mode);
            path(t, base, nm);
            if (isdir) {
                if (mkdir(t, 0700) != 0) die("mkdir acc");
                char kid[PATH_MAX];
                path(kid, t, "kid");
                mkfile(kid);
                if (chmod(kid, 0666) != 0) die("chmod kid");
            } else mkfile(t);
            owned(t, ou, og, mode);
        }
    }
    access_arg a = {label, standing};
    as(c, access_body, &a);
    for (int isdir = 0; isdir <= 1; isdir++)
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "acc_%s_%04o", isdir ? "d" : "f", mode);
            path(t, base, nm);
            if (isdir) {
                chmod(t, 0700);
                char kid[PATH_MAX];
                path(kid, t, "kid");
                unlink(kid);
                rmdir(t);
            } else unlink(t);
        }
}

// ---------------------------------------------------------------- write and truncate strip, over all 4096 modes

// op: 0 write one byte, 1 ftruncate(0), 2 ftruncate to the current length
static int predict_strip(int privileged, int in_group, int mode) {
    if (privileged) return mode;
    int cleared = 04000;
    if ((mode & 010) || !in_group) cleared |= 02000;
    return mode & ~cleared;
}

typedef struct {
    int *fds;
    int op;
} strip_arg;

static void strip_body(void *v) {
    const strip_arg *a = v;
    for (int mode = 0; mode <= 07777; mode++) {
        int fd = a->fds[mode];
        int rc;
        if (a->op == 0) rc = pwrite(fd, "z", 1, 0) == 1 ? 0 : errno;
        else if (a->op == 1) rc = R(ftruncate(fd, 0));
        else rc = R(ftruncate(fd, 4));
        if (rc != 0) p("STRIP-OP-FAILED\tmode=%04o\t%s\n", mode, en(rc));
    }
}

static const char *strip_ops[] = {"write(1 byte)", "ftruncate(0)", "ftruncate(same length)"};

static void strip_sweep(const char *label, const cred *c, int privileged, int in_group, uid_t ou, gid_t og) {
    for (int op = 0; op <= 2; op++) {
        static int fds[4096];
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "strip_%04o", mode);
            path(t, base, nm);
            mkfile(t);
            owned(t, ou, og, mode);
            // Opened by root, so any mode can be written through it; the strip
            // is decided by the credentials of whoever writes, which is the
            // child.
            fds[mode] = open(t, O_RDWR);
            if (fds[mode] < 0) die("open strip");
        }
        strip_arg a = {fds, op};
        as(c, strip_body, &a);
        int mismatches = 0;
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "strip_%04o", mode);
            path(t, base, nm);
            int got = mode_of(t);
            int want = predict_strip(privileged, in_group, mode);
            if (got != want) {
                if (mismatches < 8) p("STRIP-MISMATCH\t%s\t%s\tbefore=%04o\tgot=%04o\twant=%04o\n", label, strip_ops[op], mode, got, want);
                mismatches++;
            }
            close(fds[mode]);
            unlink(t);
        }
        p("STRIP-SWEEP\t%s\t%s\tmodes=4096\tmismatches=%d\n", label, strip_ops[op], mismatches);
    }
}

// O_TRUNC by the child itself, over the modes whose class grants it write.
typedef struct {
    const char *label;
    int privileged, in_group;
} otrunc_arg;

static void otrunc_body(void *v) {
    const otrunc_arg *a = v;
    int mismatches = 0, opened = 0;
    for (int mode = 0; mode <= 07777; mode++) {
        char t[PATH_MAX], nm[64];
        snprintf(nm, sizeof nm, "strip_%04o", mode);
        path(t, base, nm);
        int fd = open(t, O_WRONLY | O_TRUNC);
        if (fd < 0) continue;
        opened++;
        close(fd);
        int got = mode_of(t);
        int want = predict_strip(a->privileged, a->in_group, mode);
        if (got != want) {
            if (mismatches < 8) p("STRIP-MISMATCH\t%s\topen(O_TRUNC)\tbefore=%04o\tgot=%04o\twant=%04o\n", a->label, mode, got, want);
            mismatches++;
        }
    }
    p("STRIP-SWEEP\t%s\topen(O_TRUNC)\tmodes=%d (those the caller may open for writing)\tmismatches=%d\n", a->label, opened, mismatches);
}

static void otrunc_sweep(const char *label, const cred *c, int privileged, int in_group, uid_t ou, gid_t og) {
    for (int mode = 0; mode <= 07777; mode++) {
        char t[PATH_MAX], nm[64];
        snprintf(nm, sizeof nm, "strip_%04o", mode);
        path(t, base, nm);
        mkfile(t);
        owned(t, ou, og, mode);
    }
    otrunc_arg a = {label, privileged, in_group};
    as(c, otrunc_body, &a);
    for (int mode = 0; mode <= 07777; mode++) {
        char t[PATH_MAX], nm[64];
        snprintf(nm, sizeof nm, "strip_%04o", mode);
        path(t, base, nm);
        unlink(t);
    }
}

// ---------------------------------------------------------------- set-group-ID stripped by a creating open

static int predict_create(int parent_sgid, int privileged, int in_parent_group, int mode, int mask) {
    int m = mode & 07777;
    if ((m & 02010) == 02010 && parent_sgid && !privileged && !in_parent_group) m &= ~02000;
    return m & ~(mask & 0777);
}

typedef struct {
    const char *label;
    const char *dir;
    int parent_sgid, privileged, in_parent_group;
} create_arg;

static void create_body(void *v) {
    const create_arg *a = v;
    int masks[] = {0, 010, 022, 07777};
    for (size_t mi = 0; mi < sizeof masks / sizeof masks[0]; mi++) {
        umask(masks[mi]);
        int mismatches = 0;
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "c_%04o_%04o", masks[mi], mode);
            path(t, a->dir, nm);
            int fd = open(t, O_CREAT | O_EXCL | O_WRONLY, mode);
            if (fd < 0) { p("CREATE-FAILED\t%s\t%s\n", nm, en(errno)); continue; }
            struct stat st;
            fstat(fd, &st);
            close(fd);
            int got = st.st_mode & 07777;
            int want = predict_create(a->parent_sgid, a->privileged, a->in_parent_group, mode, masks[mi]);
            if (got != want) {
                if (mismatches < 8) p("CREATE-MISMATCH\t%s\tumask=%04o\tmode=%04o\tgot=%04o\twant=%04o\n", a->label, masks[mi], mode, got, want);
                mismatches++;
            }
            unlink(t);
        }
        p("CREATE-SWEEP\t%s\tumask=%04o\tmodes=4096\tmismatches=%d\n", a->label, masks[mi], mismatches);
    }
}

static void create_sweep(const char *label, const cred *c, gid_t parent_group, int parent_mode, int privileged, int in_parent_group) {
    char d[PATH_MAX];
    path(d, base, "cparent");
    rmrf(d);
    if (mkdir(d, 0700) != 0) die("mkdir cparent");
    owned(d, 0, parent_group, parent_mode);
    create_arg a = {label, d, (parent_mode & 02000) != 0, privileged, in_parent_group};
    as(c, create_body, &a);
    rmrf(d);
}

// ---------------------------------------------------------------- sticky directories: which refusal wins

typedef struct {
    const char *label;
} sticky_arg;

static void row(const char *what, int e) { p("STICKY-ORDER\t%s\t%s\n", what, en(e)); }

static void sticky_body(void *v) {
    (void)v;
    char s[PATH_MAX], u[PATH_MAX], w[PATH_MAX], a[PATH_MAX], b[PATH_MAX], t[PATH_MAX];
    path(s, base, "S");
    path(u, base, "U");
    path(w, base, "W");
#define IN(buf, dir, name) path(buf, dir, name)
    // S is root's, 01777. Entries: f (u1001's file), e (u1001's empty dir), n
    // (u1001's non-empty dir), fl (a second name for f), g and h (the caller's
    // file and empty directory), T (u1001's 01777 dir holding the caller's g2).
    IN(a, s, "e"); row("unlink(S/e: other's empty dir)", R(unlink(a)));
    IN(a, s, "f/"); row("unlink(S/f/: other's file, trailing separator)", R(unlink(a)));
    IN(a, s, "f"); row("rmdir(S/f: other's file)", R(rmdir(a)));
    IN(a, s, "n"); row("rmdir(S/n: other's non-empty dir)", R(rmdir(a)));
    IN(a, s, "f"); IN(b, s, "h"); row("rename(S/f -> S/h: other's file over caller's dir)", R(rename(a, b)));
    IN(a, s, "g"); IN(b, s, "e"); row("rename(S/g -> S/e: caller's file over other's dir)", R(rename(a, b)));
    IN(a, s, "h"); IN(b, s, "f"); row("rename(S/h -> S/f: caller's dir over other's file)", R(rename(a, b)));
    IN(a, s, "h"); IN(b, s, "n"); row("rename(S/h -> S/n: caller's empty dir over other's non-empty dir)", R(rename(a, b)));
    IN(a, s, "e"); IN(b, s, "e/x"); row("rename(S/e -> S/e/x: other's dir into itself)", R(rename(a, b)));
    IN(a, s, "f"); IN(b, s, "f"); row("rename(S/f -> S/f: other's file onto itself)", R(rename(a, b)));
    IN(a, s, "f"); IN(b, s, "fl"); row("rename(S/f -> S/fl: other's file onto another name for it)", R(rename(a, b)));
    IN(a, s, "e"); IN(b, w, "moved"); row("rename(S/e -> W/moved: other's 0755 dir to another parent)", R(rename(a, b)));
    IN(a, s, "g"); IN(b, s, "f/"); row("rename(S/g -> S/f/: caller's file onto other's file, trailing separator)", R(rename(a, b)));
    IN(a, s, "T/g2"); IN(b, s, "T"); row("rename(S/T/g2 -> S/T: onto its own parent, which is other's, in sticky S)", R(rename(a, b)));
    IN(a, s, "f"); IN(b, u, "new"); row("rename(S/f -> U/new: other's file, source sticky; U unwritable)", R(rename(a, b)));
    // U is root's, 01755: the caller may not write it.
    IN(a, u, "f"); row("unlink(U/f: other's file in an unwritable sticky dir)", R(unlink(a)));
    IN(a, u, "e"); row("rmdir(U/e: other's dir in an unwritable sticky dir)", R(rmdir(a)));
    IN(a, u, "f"); IN(b, s, "new2"); row("rename(U/f -> S/new2: unwritable sticky source parent)", R(rename(a, b)));
    IN(a, s, "g"); IN(b, u, "f"); row("rename(S/g -> U/f: onto other's file in an unwritable sticky dir)", R(rename(a, b)));
    // W is the caller's own, 0755, not sticky.
    IN(a, w, "mine"); IN(b, s, "f"); row("rename(W/mine -> S/f: onto other's file; source parent not sticky)", R(rename(a, b)));
    IN(a, w, "mine"); IN(b, s, "g"); row("rename(W/mine -> S/g: onto caller's file in sticky S)", R(rename(a, b)));
    (void)t;
#undef IN
}

static void sticky_order(const char *label, const cred *c) {
    char s[PATH_MAX], u[PATH_MAX], w[PATH_MAX], a[PATH_MAX];
    path(s, base, "S");
    path(u, base, "U");
    path(w, base, "W");
    rmrf(s);
    rmrf(u);
    rmrf(w);
    if (mkdir(s, 0700) != 0 || mkdir(u, 0700) != 0 || mkdir(w, 0700) != 0) die("mkdir sticky");
    path(a, s, "f"); mkfile(a); owned(a, 1001, 1001, 0666);
    char fl[PATH_MAX];
    path(fl, s, "fl");
    if (link(a, fl) != 0) die("link");
    path(a, s, "e"); if (mkdir(a, 0755) != 0) die("mkdir e"); owned(a, 1001, 1001, 0755);
    path(a, s, "n"); if (mkdir(a, 0777) != 0) die("mkdir n");
    { char k[PATH_MAX]; path(k, a, "x"); mkfile(k); owned(k, 1001, 1001, 0666); }
    owned(a, 1001, 1001, 0777);
    path(a, s, "g"); mkfile(a); owned(a, 1000, 1000, 0666);
    path(a, s, "h"); if (mkdir(a, 0777) != 0) die("mkdir h"); owned(a, 1000, 1000, 0777);
    path(a, s, "T"); if (mkdir(a, 0777) != 0) die("mkdir T");
    { char k[PATH_MAX]; path(k, a, "g2"); mkfile(k); owned(k, 1000, 1000, 0666); }
    owned(a, 1001, 1001, 01777);
    owned(s, 0, 0, 01777);
    path(a, u, "f"); mkfile(a); owned(a, 1001, 1001, 0666);
    path(a, u, "e"); if (mkdir(a, 0777) != 0) die("mkdir U/e"); owned(a, 1001, 1001, 0777);
    owned(u, 0, 0, 01755);
    path(a, w, "mine"); mkfile(a); owned(a, 1000, 1000, 0666);
    owned(w, 1000, 1000, 0755);
    p("STICKY-SETUP\t%s\n", label);
    sticky_arg sa = {label};
    as(c, sticky_body, &sa);
    rmrf(s);
    rmrf(u);
    rmrf(w);
}

// ---------------------------------------------------------------- which ids the class selection reads

typedef struct {
    const char *pth;
} single;

static void open_rw_body(void *v) {
    const single *s = v;
    int fd = open(s->pth, O_RDONLY);
    int e = fd < 0 ? errno : 0;
    if (fd >= 0) close(fd);
    p("IDS\topen(O_RDONLY)\t%s\n", en(e));
}

static void ids_rows(void) {
    char t[PATH_MAX];
    path(t, base, "ids");
    mkfile(t);
    owned(t, 1000, 3000, 0400);
    p("IDS-ROW\t%s against 1000:3000 0400 (effective uid is the owner)\n", R1001E1000.name);
    single s = {t};
    as(&R1001E1000, open_rw_body, &s);
    owned(t, 1001, 2000, 0040);
    p("IDS-ROW\t%s against 1001:2000 0040 (only the real gid is the file's group)\n", R1000E1000G2000.name);
    as(&R1000E1000G2000, open_rw_body, &s);
    owned(t, 1001, 1000, 0040);
    p("IDS-ROW\t%s against 1001:1000 0040 (the effective gid is the file's group)\n", R1000E1000G2000.name);
    as(&R1000E1000G2000, open_rw_body, &s);
    unlink(t);
}

int main(int argc, char **argv) {
    alarm(1800);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <scratch directory>\n", argv[0]);
        return 2;
    }
    if (geteuid() != 0) {
        fprintf(stderr, "run as root\n");
        return 2;
    }
    snprintf(base, sizeof base, "%s", argv[1]);
    rmrf(base);
    if (mkdir(base, 0755) != 0) die("mkdir base");
    struct rlimit rl = {8192, 8192};
    if (setrlimit(RLIMIT_NOFILE, &rl) != 0) die("setrlimit");
    p("# permission-standing: base=%s\n", base);

    // The caller is u1000, in groups 1000 (effective) and 2000 (supplementary).
    access_sweep("owner (1000:3000)", &U1000, 0, 1000, 3000);
    access_sweep("group, effective gid (1001:1000)", &U1000, 1, 1001, 1000);
    access_sweep("group, supplementary (1001:2000)", &U1000, 1, 1001, 2000);
    access_sweep("other (1001:3000)", &U1000, 2, 1001, 3000);
    access_sweep("root (1001:3000)", &ROOT, 3, 1001, 3000);

    strip_sweep("owner in group (1000:1000)", &U1000, 0, 1, 1000, 1000);
    strip_sweep("owner not in group (1000:3000)", &U1000, 0, 0, 1000, 3000);
    strip_sweep("non-owner, effective gid (1001:1000)", &U1000, 0, 1, 1001, 1000);
    strip_sweep("non-owner, supplementary (1001:2000)", &U1000, 0, 1, 1001, 2000);
    strip_sweep("non-owner not in group (1001:3000)", &U1000, 0, 0, 1001, 3000);
    strip_sweep("root, non-owner not in group (1001:3000)", &ROOT, 1, 0, 1001, 3000);
    otrunc_sweep("owner not in group (1000:3000)", &U1000, 0, 0, 1000, 3000);
    otrunc_sweep("non-owner, supplementary (1001:2000)", &U1000, 0, 1, 1001, 2000);
    otrunc_sweep("non-owner not in group (1001:3000)", &U1000, 0, 0, 1001, 3000);

    create_sweep("set-group-ID parent, caller not in its group (root:3000 02777)", &U1000, 3000, 02777, 0, 0);
    create_sweep("set-group-ID parent, supplementary group (root:2000 02777)", &U1000, 2000, 02777, 0, 1);
    create_sweep("set-group-ID parent, effective gid (root:1000 02777)", &U1000, 1000, 02777, 0, 1);
    create_sweep("plain parent, caller not in its group (root:3000 0777)", &U1000, 3000, 0777, 0, 0);
    create_sweep("set-group-ID parent, root not in its group (root:3000 02777)", &ROOT, 3000, 02777, 1, 0);

    sticky_order("u1000 (groups 1000,2000)", &U1000);
    sticky_order("root", &ROOT);

    ids_rows();

    rmrf(base);
    p("# done\n");
    return 0;
}
#else
// ---------------------------------------------------------------- Darwin, at an ordinary uid

// The caller owns every file here.
static int predict_strip_darwin(int op, int mode) {
    // write: StripAlways; truncate: Preserve
    if (op == 0) return mode & ~06000;
    return mode;
}

static const char *strip_ops[] = {"write(1 byte)", "ftruncate(0)", "ftruncate(same length)", "open(O_TRUNC)"};

static void strip_sweep_darwin(const char *label, const char *dir) {
    for (int op = 0; op <= 3; op++) {
        int mismatches = 0, rows = 0, dropped = 0;
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "strip_%04o", mode);
            path(t, dir, nm);
            mkfile(t);
            // Opened before the mode is set, so that any mode can be written
            // through it; open(O_TRUNC) is the exception and opens after.
            int fd = op == 3 ? -1 : open(t, O_RDWR);
            if (op != 3 && fd < 0) die("open strip");
            int ce = R(chmod(t, mode));
            if (ce != 0 || mode_of(t) != mode) {
                // A bit the caller may not set (S_ISGID outside its group) was
                // dropped or refused, so the row measures nothing about the strip.
                dropped++;
                if (fd >= 0) close(fd);
                unlink(t);
                continue;
            }
            int rc;
            if (op == 0) rc = pwrite(fd, "z", 1, 0) == 1 ? 0 : errno;
            else if (op == 1) rc = R(ftruncate(fd, 0));
            else if (op == 2) rc = R(ftruncate(fd, 4));
            else {
                fd = open(t, O_WRONLY | O_TRUNC);
                if (fd < 0) { unlink(t); continue; }
                rc = 0;
            }
            close(fd);
            rows++;
            int got = mode_of(t);
            int want = predict_strip_darwin(op == 0 ? 0 : 1, mode);
            if (rc != 0 || got != want) {
                if (mismatches < 8) p("STRIP-MISMATCH\t%s\t%s\tbefore=%04o\trc=%s\tgot=%04o\twant=%04o\n", label, strip_ops[op], mode, en(rc), got, want);
                mismatches++;
            }
            chmod(t, 0600);
            unlink(t);
        }
        p("STRIP-SWEEP\t%s\t%s\trows=%d\tdropped=%d (modes the caller could not set)\tmismatches=%d\n", label, strip_ops[op], rows, dropped, mismatches);
    }
}

static void row(const char *what, int e) { p("STICKY-ROW\t%s\t%s\n", what, en(e)); }

static void describe(const char *what, const char *pth) {
    struct stat st;
    if (lstat(pth, &st) != 0) { p("DESCRIBE\t%s\t%s\t%s\n", what, pth, en(errno)); return; }
    p("DESCRIBE\t%s\t%s\tuid=%u gid=%u mode=%04o\n", what, pth, (unsigned)st.st_uid, (unsigned)st.st_gid, (unsigned)(st.st_mode & 07777));
}

int main(int argc, char **argv) {
    alarm(600);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <scratch directory under /private/tmp>\n", argv[0]);
        return 2;
    }
    snprintf(base, sizeof base, "%s", argv[1]);
    p("# permission-standing: uid=%u egid=%u base=%s\n", (unsigned)getuid(), (unsigned)getegid(), base);

    // Owner in the file's group: a directory chgrp'd to the caller's own group.
    char mine[PATH_MAX], wheel[PATH_MAX];
    path(mine, base, "mine");
    if (mkdir(mine, 0755) != 0 || chown(mine, (uid_t)-1, getegid()) != 0) die("mine");
    // Owner outside the file's group: the base is under /private/tmp, so it
    // and everything created in it is group wheel.
    path(wheel, base, "wheel");
    if (mkdir(wheel, 0755) != 0) die("wheel");
    describe("owner-in-group parent", mine);
    describe("owner-outside-group parent", wheel);
    strip_sweep_darwin("owner in group", mine);
    strip_sweep_darwin("owner not in group", wheel);

    // Sticky rows. /private/tmp is root's, 01777.
    const char *rootfile = "/private/tmp/.AppleMiniSetupDidRun";
    describe("sticky dir", "/private/tmp");
    describe("root's file", rootfile);
    char l1[PATH_MAX], l2[PATH_MAX], l1b[PATH_MAX], myf[PATH_MAX], myd[PATH_MAX], own[PATH_MAX], ownl[PATH_MAX], x[PATH_MAX];
    snprintf(l1, sizeof l1, "%s-L1", base);
    snprintf(l2, sizeof l2, "%s-L2", base);
    snprintf(l1b, sizeof l1b, "%s-L1b", base);
    snprintf(myf, sizeof myf, "%s-myfile", base);
    snprintf(myd, sizeof myd, "%s-mydir", base);
    row("link(root's file -> /private/tmp/L1)", R(link(rootfile, l1)));
    row("link(root's file -> /private/tmp/L2)", R(link(rootfile, l2)));
    describe("L1", l1);
    mkfile(myf);
    if (mkdir(myd, 0755) != 0) die("mydir");
    row("unlink(L1: root's inode, root's sticky dir)", R(unlink(l1)));
    row("rmdir(L1)", R(rmdir(l1)));
    row("rename(L1 -> L1b)", R(rename(l1, l1b)));
    row("rename(myfile -> L1: onto root's inode)", R(rename(myf, l1)));
    row("rename(L1 -> mydir: root's file over caller's dir)", R(rename(l1, myd)));
    row("rename(mydir -> L1: caller's dir over root's file)", R(rename(myd, l1)));
    char slash[PATH_MAX];
    snprintf(slash, sizeof slash, "%s-free/", base);
    row("rename(L1 -> free/: a file onto a free name with a trailing separator)", R(rename(l1, slash)));
    row("rename(L1 -> L2: two names for one inode)", R(rename(l1, l2)));
    row("rename(L1 -> /Users/x: unwritable destination parent)", R(rename(l1, "/Users/permission-standing-probe")));
    path(x, mine, "moved");
    row("rename(L1 -> mine/moved: to the caller's non-sticky dir)", R(rename(l1, x)));
    // The caller's own sticky directory holding root's inode.
    path(own, base, "own-sticky");
    if (mkdir(own, 0777) != 0 || chmod(own, 01777) != 0) die("own-sticky");
    path(ownl, own, "L3");
    row("link(root's file -> own-sticky/L3)", R(link(rootfile, ownl)));
    row("unlink(own-sticky/L3: root's inode, caller's sticky dir)", R(unlink(ownl)));
    // The caller's own file in root's sticky dir.
    row("unlink(myfile: caller's file, root's sticky dir)", R(unlink(myf)));
    // An unwritable sticky directory. /Library/PrivilegedHelperTools is
    // root:wheel 01755 on this machine; none of these can succeed.
    const char *uw = "/Library/PrivilegedHelperTools";
    DIR *d = opendir(uw);
    char victim[PATH_MAX] = "";
    if (d) {
        struct dirent *de;
        while ((de = readdir(d))) if (de->d_name[0] != '.') { path(victim, uw, de->d_name); break; }
        closedir(d);
    }
    describe("unwritable sticky dir", uw);
    if (victim[0]) {
        describe("its entry", victim);
        row("unlink(entry of an unwritable sticky dir)", R(unlink(victim)));
        row("rmdir(entry of an unwritable sticky dir)", R(rmdir(victim)));
        char dst[PATH_MAX];
        path(dst, base, "stolen");
        row("rename(entry of an unwritable sticky dir -> base/stolen)", R(rename(victim, dst)));
    }
    // A foreign, non-empty directory in root's sticky /private/var/db/mds.
    // unlink(2) cannot remove a directory, and rmdir(2) cannot remove a
    // non-empty one, so neither row can change anything; rmdir is only tried
    // once the directory is seen to hold an entry.
    const char *fd_ = "/private/var/db/mds/messages";
    describe("foreign dir's sticky parent", "/private/var/db/mds");
    describe("foreign dir", fd_);
    int nonempty = 0;
    DIR *fdd = opendir(fd_);
    if (fdd) {
        struct dirent *de;
        while ((de = readdir(fdd))) if (strcmp(de->d_name, ".") && strcmp(de->d_name, "..")) { nonempty = 1; break; }
        closedir(fdd);
    }
    row("unlink(foreign dir in a writable sticky dir)", R(unlink(fd_)));
    if (nonempty) row("rmdir(foreign non-empty dir in a writable sticky dir)", R(rmdir(fd_)));
    else p("SKIPPED\trmdir: %s is empty or unreadable\n", fd_);
    // Moving a directory into its own subtree cannot succeed whoever may
    // write what, so this only asks which refusal comes first.
    char into[PATH_MAX];
    path(into, fd_, "x");
    row("rename(foreign dir -> into itself, in a writable sticky dir)", R(rename(fd_, into)));
    // The caller's own non-sticky directory holding root's inode.
    char l4[PATH_MAX];
    path(l4, mine, "L4");
    row("link(root's file -> mine/L4)", R(link(rootfile, l4)));
    row("unlink(mine/L4: root's inode, caller's plain dir)", R(unlink(l4)));
    // Control: an unwritable directory that is not sticky.
    describe("unwritable plain dir", "/private/etc");
    describe("its entry", "/private/etc/hosts");
    row("unlink(/private/etc/hosts: unwritable plain dir)", R(unlink("/private/etc/hosts")));
    // Tidy what survived.
    unlink(l1); unlink(l2); unlink(l1b); unlink(x); unlink(ownl); rmdir(own); rmdir(myd); unlink(myf);
    rmdir(mine); rmdir(wheel);
    p("# done\n");
    return 0;
}
#endif
