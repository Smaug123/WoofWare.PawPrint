// Measures what at-dirfd.c left open about fchmodat(2) and fchownat(2): their
// flags, beyond which bits each accepts.
//
// Sections:
//   NOFOLLOW   fchmodat(AT_FDCWD, P, M, AT_SYMLINK_NOFOLLOW) for P a file, a
//              directory, a link to each, a dangling link, a link to itself,
//              "lf/", "ld/", an absent name, the empty path, and another
//              user's link and file (asked for the mode each already has).
//              Linux through glibc and through the raw fchmodat2 syscall.
//              Each answer, then the mode lstat and stat report of P, before
//              > after.
//   LCHOWN     fchownat(AT_FDCWD, P, -1, egid, AT_SYMLINK_NOFOLLOW) against
//              lchown(P, -1, egid), each in a fresh cell, over the same P:
//              both answers, and whether the owners lstat then reports agree.
//   LINKMODE   every mode 0 to 07777 asked of a link the caller owns, through
//              fchmodat(AT_SYMLINK_NOFOLLOW), with the link in the caller's
//              group and out of it, counted by answer, and, for each success,
//              against "every bit asked for, less S_ISGID outside the
//              group". Then another user's link, asked for its own mode.
//   LINKTIMES  which of a link's and its target's timestamps a successful (or
//              failed) fchmodat(AT_SYMLINK_NOFOLLOW) of the link moves.
//   LINKUSE    (Darwin) what a link's own mode changes for its owner: stat,
//              readlink and open through it, a walk through a link to a
//              directory, and faccessat of the link itself.
//   LINKSETID  (Darwin) a link given 06777 or 01777, then lchown(-1, -1) or
//              lchown(-1, egid): the mode after; a regular file beside it.
//   EMPTY      (Linux) AT_EMPTY_PATH with the empty path and NULL, with and
//              without AT_SYMLINK_NOFOLLOW, for each kind of dirfd: fchmodat2
//              against fchmod(fd), fchownat to the caller's own group and to
//              uid 2000 against fchown(fd), each in a fresh cell; "same" if
//              the two answer alike and leave the same mode, owner and group
//              (for AT_FDCWD the reference is chmod(".") or chown(".")).
//   EMPTYPATH  (Linux) a path that is not empty, with AT_EMPTY_PATH and
//              without.
//   FLAGMIX    a rejected flag bit together with each accepted one.
//
// Each cell runs in a forked child, in a fresh directory of its own, under
// alarm(10). On Linux every row is run as root and again as uid 1000 with no
// supplementary groups, which root drops to in the child once it has made
// what the row needs: another user's (uid 2000's) link and file, and a file
// it opens on uid 1000's behalf.
//
// Linux, as root:
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/chmod-chown-at.c && /tmp/p "$(mktemp -d)" > /probe/chmod-chown-at.linux-6.18.5-aarch64-root.txt'
// Darwin, as an ordinary user; another user's link and file are /tmp (a link
// root owns) and /private/etc/hosts:
//   nix develop -c clang -Wall -Werror -o /tmp/p chmod-chown-at.c && /tmp/p "$(mktemp -d /private/tmp/chmodat.XXXXXX)" > chmod-chown-at.darwin-27.0-uid501.txt
//
// Measured 2026-10-04 on Linux 6.18.5 aarch64 (gcc:14, glibc, ext4; root, and
// uid 1000 dropping in the child) and Darwin 27.0 arm64 (uid 501). The outputs
// are beside this file, and WoofWare.PosixKernel.Test/TestAttributeChangeAt.fs
// replays them.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#ifndef SYS_fchmodat2
#define SYS_fchmodat2 452
#endif
#endif

static const char *en(int e) {
    static char buf[32];
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case ELOOP: return "ELOOP";
    case EOPNOTSUPP: return "EOPNOTSUPP";
#if defined(ENOTSUP) && ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
    case EISDIR: return "EISDIR";
    case EROFS: return "EROFS";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static uid_t caller;
static gid_t callergid;
static char cellpath[PATH_MAX];

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static int rc(long r) { return r < 0 ? errno : 0; }

#ifdef __linux__
static const char *volatile null_path = NULL;
static const uid_t other_uid = 2000;
static int raw_fchmodat(int d, const char *p, mode_t m, int f) { return rc(syscall(SYS_fchmodat2, d, p, m, f)); }
static int raw_fchownat(int d, const char *p, uid_t u, gid_t g, int f) { return rc(syscall(SYS_fchownat, d, p, u, g, f)); }
static const char *their_link = "tl";
static const char *their_file = "tf";
#else
static int raw_fchmodat(int d, const char *p, mode_t m, int f) { return rc(fchmodat(d, p, m, f)); }
static int raw_fchownat(int d, const char *p, uid_t u, gid_t g, int f) { return rc(fchownat(d, p, u, g, f)); }
static const char *their_link = "/tmp";
static const char *their_file = "/private/etc/hosts";
#endif

// The cell, as the cwd: f (0644), d/ (0755) holding x, lf -> f, ld -> d,
// dang -> nx, cyc -> cyc, and, on Linux, uid 2000's link tl -> f and file tf.
// The caller has dropped to its credentials on return.
static void cell(void (*as_root)(void)) {
    cellno++;
    snprintf(cellpath, sizeof cellpath, "%s/c%05d", base, cellno);
    if (mkdir(cellpath, 0777) < 0 || chmod(cellpath, 0777) < 0) die("mkdir cell");
    if (chdir(cellpath) < 0) die("chdir cell");
#ifdef __linux__
    if (symlink("f", "tl") < 0 || lchown("tl", other_uid, other_uid) < 0) die("tl");
    int t = open("tf", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (t < 0 || fchown(t, other_uid, other_uid) < 0) die("tf");
    close(t);
    if (as_root) as_root();
    if (caller != 0) {
        if (setgroups(0, NULL) < 0 || setresgid(caller, caller, caller) < 0 || setresuid(caller, caller, caller) < 0)
            die("drop");
    }
#else
    if (as_root) as_root();
#endif
    int fd = open("f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("f");
    close(fd);
    if (mkdir("d", 0755) < 0) die("d");
    fd = open("d/x", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("d/x");
    close(fd);
    if (symlink("f", "lf") < 0 || symlink("d", "ld") < 0 || symlink("nx", "dang") < 0 || symlink("cyc", "cyc") < 0)
        die("symlink");
}

static void in_child(void (*body)(int, int), int a, int b) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(10);
        body(a, b);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    cellno++;
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

static int lmode(const char *p) {
    struct stat st;
    return lstat(p, &st) < 0 ? -1 : (int)(st.st_mode & 07777);
}

static int smode(const char *p) {
    struct stat st;
    return stat(p, &st) < 0 ? -1 : (int)(st.st_mode & 07777);
}

static void pmode(int m) {
    if (m < 0) printf("-");
    else printf("%04o", m);
}

// ------------------------------------------------------------------ NOFOLLOW
static const char *nofollow_paths[] = {"f", "d", "lf", "ld", "dang", "cyc", "lf/", "ld/", "nx", "", "theirs-link", "theirs-file"};
#define NNOFOLLOW (int)(sizeof nofollow_paths / sizeof nofollow_paths[0])

static const char *resolve_label(const char *label) {
    if (strcmp(label, "theirs-link") == 0) return their_link;
    if (strcmp(label, "theirs-file") == 0) return their_file;
    return label;
}

static void nofollow_body(int which, int raw) {
    cell(NULL);
    const char *p = resolve_label(nofollow_paths[which]);
    int theirs = p == their_link || p == their_file;
    int lb = lmode(p), sb = smode(p);
    mode_t m = theirs ? (mode_t)lb : 0640;
    int r = raw ? raw_fchmodat(AT_FDCWD, p, m, AT_SYMLINK_NOFOLLOW) : rc(fchmodat(AT_FDCWD, p, m, AT_SYMLINK_NOFOLLOW));
    printf("%s l:", en(r));
    pmode(lb);
    printf(">");
    pmode(lmode(p));
    printf(" s:");
    pmode(sb);
    printf(">");
    pmode(smode(p));
}

// -------------------------------------------------------------------- LCHOWN
static void lchown_body(int which, int via_at) {
    cell(NULL);
    const char *p = resolve_label(nofollow_paths[which]);
    int r = via_at ? raw_fchownat(AT_FDCWD, p, (uid_t)-1, getegid(), AT_SYMLINK_NOFOLLOW) : rc(lchown(p, (uid_t)-1, getegid()));
    struct stat st;
    if (lstat(p, &st) < 0) printf("%s -", en(r));
    else printf("%s %d:%d", en(r), (int)st.st_uid, (int)st.st_gid);
}

// ------------------------------------------------------------------ LINKMODE
static int is_member(gid_t g) {
    if (g == getegid()) return 1;
    gid_t groups[256];
    int n = getgroups(256, groups);
    for (int i = 0; i < n; i++)
        if (groups[i] == g) return 1;
    return 0;
}

#ifdef __linux__
static void link_out_of_group(void) {
    if (symlink("f", "sl") < 0 || lchown("sl", caller, other_uid) < 0) die("sl out");
}
static void link_in_group(void) {
    if (symlink("f", "sl") < 0 || lchown("sl", caller, caller) < 0) die("sl in");
}
#endif

static void linkmode_body(int in_group, int unused) {
    (void)unused;
#ifdef __linux__
    cell(in_group ? link_in_group : link_out_of_group);
#else
    cell(NULL);
    if (symlink("f", "sl") < 0) die("sl");
    if (in_group && lchown("sl", (uid_t)-1, getegid()) < 0) die("lchown sl");
#endif
    struct stat st;
    if (lstat("sl", &st) < 0) die("lstat sl");
    int member = is_member(st.st_gid);
    if (member != in_group) {
        printf("group %d member=%d: not the case asked for", (int)st.st_gid, member);
        return;
    }
    int counts[256] = {0};
    int mismatches = 0, unchanged_failures = 0;
    for (int mode = 0; mode <= 07777; mode++) {
        int before = lmode("sl");
        int r = raw_fchmodat(AT_FDCWD, "sl", (mode_t)mode, AT_SYMLINK_NOFOLLOW);
        int after = lmode("sl");
        counts[r < 256 ? r : 255]++;
        if (r == 0) {
            int want = mode & (member ? 07777 : ~02000 & 07777);
            if (after != want) {
                if (mismatches < 5) printf("[mismatch %04o got %04o want %04o] ", mode, after, want);
                mismatches++;
            }
        } else if (after == before) {
            unchanged_failures++;
        }
    }
    printf("group=%d", (int)st.st_gid);
    for (int e = 0; e < 256; e++)
        if (counts[e]) printf("\t%s=%d", en(e), counts[e]);
    printf("\tmismatches=%d\tfailures-left-mode=%d", mismatches, unchanged_failures);
}

static void their_link_body(int unused1, int unused2) {
    (void)unused1;
    (void)unused2;
    cell(NULL);
    int before = lmode(their_link);
    int r = raw_fchmodat(AT_FDCWD, their_link, (mode_t)before, AT_SYMLINK_NOFOLLOW);
    printf("%s ", en(r));
    pmode(before);
    printf(">");
    pmode(lmode(their_link));
}

// ----------------------------------------------------------------- LINKTIMES
static int same_ts(struct timespec a, struct timespec b) { return a.tv_sec == b.tv_sec && a.tv_nsec == b.tv_nsec; }
#ifdef __APPLE__
#define ATIM st_atimespec
#define MTIM st_mtimespec
#define CTIM st_ctimespec
#else
#define ATIM st_atim
#define MTIM st_mtim
#define CTIM st_ctim
#endif

static void times_body(int mode, int unused) {
    (void)unused;
    cell(NULL);
    struct stat lb, tb, la, ta;
    if (lstat("lf", &lb) < 0 || stat("f", &tb) < 0) die("stat before");
    usleep(20000);
    int r = raw_fchmodat(AT_FDCWD, "lf", (mode_t)mode, AT_SYMLINK_NOFOLLOW);
    if (lstat("lf", &la) < 0 || stat("f", &ta) < 0) die("stat after");
    printf("%s\tlink atime=%s mtime=%s ctime=%s\ttarget atime=%s mtime=%s ctime=%s", en(r),
           same_ts(lb.ATIM, la.ATIM) ? "kept" : "moved", same_ts(lb.MTIM, la.MTIM) ? "kept" : "moved",
           same_ts(lb.CTIM, la.CTIM) ? "kept" : "moved", same_ts(tb.ATIM, ta.ATIM) ? "kept" : "moved",
           same_ts(tb.MTIM, ta.MTIM) ? "kept" : "moved", same_ts(tb.CTIM, ta.CTIM) ? "kept" : "moved");
}

#ifdef __APPLE__
// ------------------------------------------------------------------- LINKUSE
static void use_body(int mode, int unused) {
    (void)unused;
    cell(NULL);
    if (lchmod("lf", (mode_t)mode) < 0 || lchmod("ld", (mode_t)mode) < 0) die("lchmod");
    struct stat st;
    char buf[64];
    int s = rc(stat("lf", &st));
    int rl = rc(readlink("lf", buf, sizeof buf));
    int fd = open("lf", O_RDONLY);
    int o = fd < 0 ? errno : 0;
    if (fd >= 0) close(fd);
    int w = rc(stat("ld/x", &st));
    int ar = rc(faccessat(AT_FDCWD, "lf", R_OK, AT_SYMLINK_NOFOLLOW));
    printf("lmode=");
    pmode(lmode("lf"));
    printf("\tstat=%s\treadlink=%s\topen=%s\twalk=%s\tfaccessat(R_OK,NOFOLLOW)=%s", en(s), en(rl), en(o), en(w), en(ar));
}

// ----------------------------------------------------------------- LINKSETID
static void setid_body(int row, int on_link) {
    cell(NULL);
    const char *p = on_link ? "lf" : "f";
    if (on_link) {
        if (lchown("lf", (uid_t)-1, getegid()) < 0) die("lchown lf");
    } else if (chown("f", (uid_t)-1, getegid()) < 0) die("chown f");
    int mode = row < 2 ? 06777 : 01777;
    int r0 = on_link ? rc(lchmod(p, (mode_t)mode)) : rc(chmod(p, (mode_t)mode));
    int before = lmode(p);
    gid_t g = (row % 2 == 0) ? (gid_t)-1 : getegid();
    int r = on_link ? rc(lchown(p, (uid_t)-1, g)) : rc(chown(p, (uid_t)-1, g));
    printf("chmod(%04o)=%s ", mode, en(r0));
    pmode(before);
    printf(" chown(-1,%s)=%s ", row % 2 == 0 ? "-1" : "egid", en(r));
    pmode(lmode(p));
}
#endif

#ifdef __linux__
// --------------------------------------------------------------------- EMPTY
static const char *kinds[] = {
    "cwd", "minus1", "closed", "dir", "file", "pipe-read", "pipe-write", "socket", "epoll", "devnull",
    "orphan", "unlinked", "locked", "moved", "theirs", "root-opened", "opath-file", "opath-link",
};
#define NKINDS (int)(sizeof kinds / sizeof kinds[0])

static int root_opened_fd = -1;
static void open_for_caller(void) {
    int fd = open("ro", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0 || fchown(fd, caller, caller) < 0) die("ro");
    close(fd);
    root_opened_fd = open("ro", O_RDONLY);
    if (root_opened_fd < 0) die("open ro");
}

static int fixture(int k) {
    cell(k == 15 ? open_for_caller : NULL);
    int fd, p[2], s;
    if (mkdir("gone", 0755) < 0) die("gone");
    fd = open("g", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("g");
    close(fd);
    switch (k) {
    case 0: return AT_FDCWD;
    case 1: return -1;
    case 2: return 999;
    case 3: return open("d", O_RDONLY | O_DIRECTORY);
    case 4: return open("f", O_RDONLY);
    case 5: if (pipe(p) < 0) die("pipe"); return p[0];
    case 6: if (pipe(p) < 0) die("pipe"); return p[1];
    case 7: s = socket(AF_UNIX, SOCK_STREAM, 0); if (s < 0) die("socket"); return s;
    case 8: s = epoll_create1(0); if (s < 0) die("epoll"); return s;
    case 9: return open("/dev/null", O_RDONLY);
    case 10: fd = open("gone", O_RDONLY | O_DIRECTORY); if (rmdir("gone") < 0) die("rmdir"); return fd;
    case 11: fd = open("g", O_RDONLY); if (unlink("g") < 0) die("unlink"); return fd;
    case 12: fd = open("d", O_RDONLY | O_DIRECTORY); if (chmod("d", 0) < 0) die("chmod"); return fd;
    case 13: fd = open("d", O_RDONLY | O_DIRECTORY); if (rename("d", "d-moved") < 0) die("rename"); return fd;
    case 14: return open("tf", O_RDONLY);
    case 15: return root_opened_fd;
    case 16: return open("f", O_PATH);
    case 17: return open("lf", O_PATH | O_NOFOLLOW);
    }
    return -1;
}

struct outcome {
    int answer;
    int known;
    int mode, uid, gid;
};

static struct outcome observe(int fd, int answer) {
    struct outcome o = {answer, 0, 0, 0, 0};
    struct stat st;
    int r = fd == AT_FDCWD ? stat(".", &st) : fstat(fd, &st);
    if (r == 0) {
        o.known = 1;
        o.mode = (int)(st.st_mode & 07777);
        o.uid = (int)st.st_uid;
        o.gid = (int)st.st_gid;
    }
    return o;
}

// op: 0 chmod, 1 chown to the caller's own group, 2 chown to uid 2000.
// shape: 0 "", 1 NULL, 2 "" NOFOLLOW, 3 NULL NOFOLLOW; 4 the reference.
static int is_devnull;
static struct outcome empty_once(int k, int op, int shape) {
    int fd = fixture(k);
    is_devnull = k == 9;
    struct stat st;
    int current = 0644;
    if ((fd == AT_FDCWD ? stat(".", &st) : fstat(fd, &st)) == 0) current = (int)(st.st_mode & 07777);
    // /dev/null is the VM's own: ask for the mode it has, and give it to no one.
    mode_t m = is_devnull ? (mode_t)current : (mode_t)(current ^ 0040);
    uid_t u = op == 2 ? (is_devnull ? (uid_t)-1 : other_uid) : (uid_t)-1;
    gid_t g = op == 1 ? callergid : (gid_t)-1;
    int r;
    if (shape < 4) {
        const char *p = shape % 2 == 0 ? "" : null_path;
        int flags = AT_EMPTY_PATH | (shape >= 2 ? AT_SYMLINK_NOFOLLOW : 0);
        r = op == 0 ? raw_fchmodat(fd, p, m, flags) : raw_fchownat(fd, p, u, g, flags);
    } else if (fd == AT_FDCWD) {
        r = op == 0 ? rc(chmod(".", m)) : rc(chown(".", u, g));
    } else {
        r = op == 0 ? rc(fchmod(fd, m)) : rc(fchown(fd, u, g));
    }
    return observe(fd, r);
}

static int pipefd[2];
static void empty_shape(int k, int opshape) {
    struct outcome o = empty_once(k, opshape / 8, opshape % 8);
    if (write(pipefd[1], &o, sizeof o) != sizeof o) die("write");
}

static struct outcome run_empty(int k, int op, int shape) {
    if (pipe(pipefd) < 0) die("pipe");
    in_child(empty_shape, k, op * 8 + shape);
    // Only the child writes: with the parent's end closed, a child that died
    // before writing gives EOF rather than a read that never returns.
    close(pipefd[1]);
    struct outcome o;
    memset(&o, 0, sizeof o);
    o.answer = -1;
    if (read(pipefd[0], &o, sizeof o) != sizeof o) o.answer = -1;
    close(pipefd[0]);
    return o;
}

static const char *emptylabel[] = {"\"\"", "NULL", "\"\" NOFOLLOW", "NULL NOFOLLOW"};
static const char *oplabel[] = {"fchmodat", "fchownat(-1,own)", "fchownat(2000,-1)"};

static void empty_rows(void) {
    for (int op = 0; op < 3; op++) {
        for (int k = 0; k < NKINDS; k++) {
            if (caller == 0 && k == 15) continue;
            struct outcome ref = run_empty(k, op, 4);
            printf("EMPTY\tcaller=%d\t%s\t%s\tref=%s", (int)caller, oplabel[op], kinds[k], ref.answer < 0 ? "died" : en(ref.answer));
            for (int shape = 0; shape < 4; shape++) {
                struct outcome o = run_empty(k, op, shape);
                printf("\t%s=", emptylabel[shape]);
                if (o.answer < 0) printf("died");
                else if (o.answer == ref.answer && o.known == ref.known && o.mode == ref.mode && o.uid == ref.uid && o.gid == ref.gid)
                    printf("same");
                else
                    printf("%s(%04o %d:%d)/ref(%04o %d:%d)", en(o.answer), o.mode, o.uid, o.gid, ref.mode, ref.uid, ref.gid);
            }
            printf("\n");
        }
    }
}

// ----------------------------------------------------------------- EMPTYPATH
static void emptypath_body(int k, int row) {
    int fd = fixture(k);
    char rooted[PATH_MAX + 8];
    snprintf(rooted, sizeof rooted, "%s/f", cellpath);
    int which = row / 2;
    const char *p = which == 0 ? "f" : which == 1 ? "." : which == 2 ? "nx" : rooted;
    int flags = row % 2 ? AT_EMPTY_PATH : 0;
    int r = raw_fchmodat(fd, p, 0600, flags);
    struct stat st;
    printf("%s:", en(r));
    pmode(fstatat(fd, p, &st, 0) < 0 ? -1 : (int)(st.st_mode & 07777));
}

static const char *pathlabel[] = {"f", ".", "nx", "rooted f"};
#endif

// ------------------------------------------------------------------- FLAGMIX
static void flagmix_body(int flags, int chown_) {
    cell(NULL);
    int r = chown_ ? raw_fchownat(AT_FDCWD, "f", (uid_t)-1, (gid_t)-1, flags) : raw_fchmodat(AT_FDCWD, "f", 0644, flags);
    printf("%s", en(r));
}

static void run_caller(void) {
    callergid = caller == 0 ? 0 : (gid_t)caller;
#ifdef __APPLE__
    callergid = getegid();
#endif
    for (int raw = 0; raw < 2; raw++) {
        printf("NOFOLLOW\tcaller=%d\t%s", (int)caller, raw ? "syscall" : "libc");
        for (int i = 0; i < NNOFOLLOW; i++) {
            printf("\t%s=", nofollow_paths[i]);
            in_child(nofollow_body, i, raw);
        }
        printf("\n");
    }
    printf("LCHOWN\tcaller=%d", (int)caller);
    for (int i = 0; i < NNOFOLLOW; i++) {
        printf("\t%s=", nofollow_paths[i]);
        in_child(lchown_body, i, 1);
        printf(" / lchown ");
        in_child(lchown_body, i, 0);
    }
    printf("\n");
    for (int in_group = 1; in_group >= 0; in_group--) {
        printf("LINKMODE\tcaller=%d\t%s\t", (int)caller, in_group ? "in-group" : "out-of-group");
        in_child(linkmode_body, in_group, 0);
        printf("\n");
    }
    printf("LINKMODE\tcaller=%d\ttheirs\t", (int)caller);
    in_child(their_link_body, 0, 0);
    printf("\n");
    printf("LINKTIMES\tcaller=%d\t0700\t", (int)caller);
    in_child(times_body, 0700, 0);
    printf("\n");
#ifdef __APPLE__
    const int usemodes[] = {0, 0111, 0444, 0222, 0755};
    for (int i = 0; i < 5; i++) {
        printf("LINKUSE\tcaller=%d\t%04o\t", (int)caller, usemodes[i]);
        in_child(use_body, usemodes[i], 0);
        printf("\n");
    }
    for (int on_link = 1; on_link >= 0; on_link--)
        for (int row = 0; row < 4; row++) {
            printf("LINKSETID\tcaller=%d\t%s\t", (int)caller, on_link ? "link" : "file");
            in_child(setid_body, row, on_link);
            printf("\n");
        }
#endif
#ifdef __linux__
    empty_rows();
    for (int k = 0; k < NKINDS; k++) {
        if (caller == 0 && k == 15) continue;
        printf("EMPTYPATH\tcaller=%d\t%s", (int)caller, kinds[k]);
        for (int row = 0; row < 8; row++) {
            printf("\t%s%s=", pathlabel[row / 2], row % 2 ? " AT_EMPTY_PATH" : "");
            in_child(emptypath_body, k, row);
        }
        printf("\n");
    }
    const int accepted[] = {0x100, 0x1000};
#else
    const int accepted[] = {0x20, 0x800, 0x2000, 0x8000};
#endif
    for (int c = 0; c < 2; c++) {
        printf("FLAGMIX\tcaller=%d\t%s", (int)caller, c ? "fchownat" : "fchmodat");
        for (int i = 0; i < (int)(sizeof accepted / sizeof accepted[0]); i++) {
            printf("\t0x%x=", accepted[i]);
            in_child(flagmix_body, accepted[i], c);
            printf("\t0x%x|0x1=", accepted[i]);
            in_child(flagmix_body, accepted[i] | 0x1, c);
            printf("\t0x%x|0x40000000=", accepted[i]);
            in_child(flagmix_body, accepted[i] | 0x40000000, c);
        }
        printf("\n");
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\n", u.sysname, u.release, u.machine, (int)geteuid());
    printf("CONST\tAT_FDCWD=%d\tAT_SYMLINK_NOFOLLOW=0x%x", AT_FDCWD, AT_SYMLINK_NOFOLLOW);
#ifdef AT_EMPTY_PATH
    printf("\tAT_EMPTY_PATH=0x%x", AT_EMPTY_PATH);
#endif
    printf("\n");
#ifdef __linux__
    if (geteuid() != 0) {
        fprintf(stderr, "run as root\n");
        return 2;
    }
    const uid_t callers[] = {0, 1000};
    for (int c = 0; c < 2; c++) {
        caller = callers[c];
        run_caller();
    }
#else
    caller = geteuid();
    run_caller();
#endif
    return 0;
}
