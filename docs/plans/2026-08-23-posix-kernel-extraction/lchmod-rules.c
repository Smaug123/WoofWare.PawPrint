// Measures lchmod(3) against fchmodat(2) with AT_SYMLINK_NOFOLLOW, which
// chmod-chown-at.c measured alone: a symbolic link's own mode.
//
// Sections:
//   CALLS   lchmod(P, 0640), and fchmodat(AT_FDCWD, P, 0640,
//           AT_SYMLINK_NOFOLLOW) through libc and (Linux) through the raw
//           fchmodat2 syscall, for P a file, a directory, a link to each, a
//           dangling link, a link to itself, "lf/", "ld/", an absent name, the
//           empty path, and another user's link, file and directory. Each
//           answer, then the mode lstat and stat report of P, before > after.
//   MODES   every mode 0 to 07777 given by lchmod to one subject and by the
//           reference call to its twin: for a link the reference is
//           fchmodat(AT_SYMLINK_NOFOLLOW), for a file or a directory chmod(2).
//           Subjects in the caller's group and out of it. Counted by answer;
//           "differs" counts modes where the two calls' answers or resulting
//           modes differ, "unexpected" successes whose mode is not "every bit
//           asked for, less S_ISGID outside the group" (Linux's root keeps
//           every bit), and the three
//           "kept-*" columns the successes asked for that bit and kept it.
//   TIMES   which timestamps of the link, its target and the directory
//           holding it each call moves: to a new mode, to the mode it already
//           has, on a dangling link, and on another user's link (asked for
//           its own mode, so nothing could change).
//   UMASK   lchmod under umask 077 of a link to 0777: the umask plays no part.
//   HIGH    each call asked for 0170640, bits above 07777 set, of f and lf.
//   DIRFD   fchmodat(fd, "l", 0640, flags) from a descriptor on a directory
//           holding l -> ../f, with AT_SYMLINK_NOFOLLOW and without.
//
// Each cell runs in a forked child, in a fresh directory of its own, under
// alarm(10). On Linux every row is run as root and again as uid 1000 with no
// supplementary groups, which root drops to in the child once it has made
// what the row needs: uid 2000's link, file and directory, and the subjects
// of MODES, chowned to the caller and to its group or uid 2000's.
//
// Linux, as root, in the container's own /tmp (ext4) rather than the bind
// mount:
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/lchmod-rules.c && /tmp/p "$(mktemp -d)" > /probe/lchmod-rules.linux-6.18.5-aarch64-root.txt'
// Darwin, as an ordinary user; another user's link, file and directory are
// root's /private/etc/localtime (a link), /private/etc/hosts and
// /private/var/root, none of them SIP-restricted (`ls -lOd` shows "-"):
//   nix develop -c clang -Wall -Werror -o /tmp/p lchmod-rules.c && /tmp/p "$(mktemp -d /private/tmp/lchmod.XXXXXX)" > lchmod-rules.darwin-27.0-uid501.txt
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (gcc:14, glibc 2.41, ext4;
// root, and uid 1000 dropping in the child) and Darwin 27.0.0 arm64 (uid 501;
// no root). The outputs are beside this file, and
// WoofWare.PosixKernel.Test/TestAttributeChangeAt.fs replays them.
//
// What they say:
// - Linux: lchmod is fchmodat(AT_SYMLINK_NOFOLLOW) is fchmodat2, row for row:
//   EOPNOTSUPP on every link, owner or not, root or not, at every mode,
//   moving no timestamp; anything else changes as chmod would.
// - Darwin: fchmodat(AT_SYMLINK_NOFOLLOW) changes a link's own mode by
//   chmod's rule (set-ID and sticky bits kept, S_ISGID dropped outside the
//   link's group, EPERM for a non-owner), moving the link's ctime alone, even
//   to the mode it already has.
// - Darwin's lchmod is not that call. libsystem_c's lchmod disassembles to
//   setattrlist(path, {ATTR_BIT_MAP_COUNT, ATTR_CMN_ACCESSMASK}, &mode, 4,
//   FSOPT_NOFOLLOW), and answers as setattrlist does: EACCES, not EPERM, for
//   a non-owner, and EPERM, not a silent drop, for S_ISGID asked outside the
//   group, of a link, a file and a directory alike. Otherwise it agrees with
//   fchmodat(AT_SYMLINK_NOFOLLOW), timestamps included.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
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
    case ENOSYS: return "ENOSYS";
    case EROFS: return "EROFS";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static uid_t caller;
static char cellpath[PATH_MAX];

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static int rc(long r) { return r < 0 ? errno : 0; }

#ifdef __linux__
static const uid_t other_uid = 2000;
static int raw_fchmodat(int d, const char *p, mode_t m, int f) { return rc(syscall(SYS_fchmodat2, d, p, m, f)); }
static const char *their_link = "tl";
static const char *their_file = "tf";
static const char *their_dir = "td";
#else
// Root's, and none of them SIP-restricted. /tmp is restricted, and an EPERM
// from it does not say whose answer it is: a run against /tmp had lchmod
// answer EPERM there, where it answers EACCES on /private/etc/localtime.
static const char *their_link = "/private/etc/localtime";
static const char *their_file = "/private/etc/hosts";
static const char *their_dir = "/private/var/root";
#endif

// The three ways of asking for a link's own mode. Darwin's libc is its
// syscall, so it has no third.
enum call { LCHMOD, LIBC_AT, RAW_AT };
static const char *calllabel[] = {"lchmod", "fchmodat", "fchmodat2"};
#ifdef __linux__
#define NCALLS 3
#else
#define NCALLS 2
#endif

static int ask(int call, int dirfd, const char *p, mode_t m, int flags) {
    switch (call) {
    case LCHMOD: return rc(lchmod(p, m));
    case LIBC_AT: return rc(fchmodat(dirfd, p, m, flags));
#ifdef __linux__
    case RAW_AT: return raw_fchmodat(dirfd, p, m, flags);
#endif
    }
    die("ask");
    return -1;
}

// The cell, as the cwd: f (0644), d/ (0755) holding x and l -> ../f, lf -> f,
// ld -> d, dang -> nx, cyc -> cyc, and, on Linux, uid 2000's link tl -> f,
// file tf and directory td. The caller has dropped to its credentials on
// return.
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
    if (mkdir("td", 0755) < 0 || chown("td", other_uid, other_uid) < 0) die("td");
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
    if (symlink("f", "lf") < 0 || symlink("d", "ld") < 0 || symlink("nx", "dang") < 0 || symlink("cyc", "cyc") < 0 ||
        symlink("../f", "d/l") < 0)
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

// --------------------------------------------------------------------- CALLS
static const char *call_paths[] = {"f",  "d", "lf", "ld", "dang",        "cyc",         "lf/",
                                   "ld/", "nx", "",   "theirs-link", "theirs-file", "theirs-dir"};
#define NCALLPATHS (int)(sizeof call_paths / sizeof call_paths[0])

static const char *resolve_label(const char *label) {
    if (strcmp(label, "theirs-link") == 0) return their_link;
    if (strcmp(label, "theirs-file") == 0) return their_file;
    if (strcmp(label, "theirs-dir") == 0) return their_dir;
    return label;
}

static void calls_body(int which, int call) {
    cell(NULL);
    const char *p = resolve_label(call_paths[which]);
    int lb = lmode(p), sb = smode(p);
    int r = ask(call, AT_FDCWD, p, 0640, AT_SYMLINK_NOFOLLOW);
    printf("%s l:", en(r));
    pmode(lb);
    printf(">");
    pmode(lmode(p));
    printf(" s:");
    pmode(sb);
    printf(">");
    pmode(smode(p));
}

// --------------------------------------------------------------------- MODES
static int is_member(gid_t g) {
    if (g == getegid()) return 1;
    gid_t groups[256];
    int n = getgroups(256, groups);
    for (int i = 0; i < n; i++)
        if (groups[i] == g) return 1;
    return 0;
}

enum subject { LINK, FILE_, DIR_ };
static const char *subjectlabel[] = {"link", "file", "dir"};
static int modes_subject;

static void make_subject(const char *name) {
    int fd;
    switch (modes_subject) {
    case LINK:
        if (symlink("f", name) < 0) die("subject link");
        break;
    case FILE_:
        fd = open(name, O_WRONLY | O_CREAT | O_EXCL, 0644);
        if (fd < 0) die("subject file");
        close(fd);
        break;
    case DIR_:
        if (mkdir(name, 0755) < 0) die("subject dir");
        break;
    }
}

#ifdef __linux__
// Made by root and handed to the caller, in its own group or in uid 2000's.
static gid_t modes_group;
static void make_twins(void) {
    make_subject("a");
    make_subject("b");
    if (lchown("a", caller, modes_group) < 0 || lchown("b", caller, modes_group) < 0) die("chown twins");
}
#endif

static void modes_body(int subject, int in_group) {
    modes_subject = subject;
#ifdef __linux__
    modes_group = in_group ? caller : other_uid;
    cell(make_twins);
#else
    // Made by the caller, in a directory of group 0, which it is not in, and
    // handed to its own group for the in-group case.
    cell(NULL);
    make_subject("a");
    make_subject("b");
    if (in_group && (lchown("a", (uid_t)-1, getegid()) < 0 || lchown("b", (uid_t)-1, getegid()) < 0))
        die("lchown twins");
#endif
    struct stat st;
    if (lstat("a", &st) < 0) die("lstat a");
    int member = is_member(st.st_gid);
    if (member != in_group) {
        printf("group %d member=%d: not the case asked for", (int)st.st_gid, member);
        return;
    }
    int counts[256] = {0};
    int differs = 0, unexpected = 0, kept_suid = 0, kept_sgid = 0, kept_svtx = 0;
    for (int mode = 0; mode <= 07777; mode++) {
        int ra = rc(lchmod("a", (mode_t)mode));
        int rb = subject == LINK ? ask(NCALLS - 1, AT_FDCWD, "b", (mode_t)mode, AT_SYMLINK_NOFOLLOW)
                                 : rc(chmod("b", (mode_t)mode));
        int after_a = lmode("a"), after_b = lmode("b");
        counts[ra < 256 ? ra : 255]++;
        if (ra != rb || after_a != after_b) {
            if (differs < 5) printf("[differs %04o: %s %04o against %s %04o] ", mode, en(ra), after_a, en(rb), after_b);
            differs++;
        }
        if (ra == 0) {
            // Linux's root keeps every bit (chmod-rules.c); Darwin's was not run.
            int want = mode & (member || caller == 0 ? 07777 : ~02000 & 07777);
            if (after_a != want) {
                if (unexpected < 5) printf("[unexpected %04o got %04o want %04o] ", mode, after_a, want);
                unexpected++;
            }
            if ((mode & 04000) && (after_a & 04000)) kept_suid++;
            if ((mode & 02000) && (after_a & 02000)) kept_sgid++;
            if ((mode & 01000) && (after_a & 01000)) kept_svtx++;
        }
    }
    printf("group=%d", (int)st.st_gid);
    for (int e = 0; e < 256; e++)
        if (counts[e]) printf("\t%s=%d", en(e), counts[e]);
    printf("\tdiffers=%d\tunexpected=%d\tkept-suid=%d\tkept-sgid=%d\tkept-svtx=%d", differs, unexpected, kept_suid,
           kept_sgid, kept_svtx);
}

// --------------------------------------------------------------------- TIMES
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

static const char *times_rows[] = {"lf 0700", "lf same", "dang 0700", "theirs-link same"};
#define NTIMES (int)(sizeof times_rows / sizeof times_rows[0])

static void print_times(const char *what, int ok, struct stat *b, struct stat *a) {
    if (!ok) {
        printf("\t%s -", what);
        return;
    }
    printf("\t%s atime=%s mtime=%s ctime=%s", what, same_ts(b->ATIM, a->ATIM) ? "kept" : "moved",
           same_ts(b->MTIM, a->MTIM) ? "kept" : "moved", same_ts(b->CTIM, a->CTIM) ? "kept" : "moved");
}

static void times_body(int row, int call) {
    cell(NULL);
    const char *p = row == 0 || row == 1 ? "lf" : row == 2 ? "dang" : their_link;
    // The directory holding the link: the cell, or Darwin's /private/etc.
    const char *parent = p[0] == '/' ? "/private/etc" : ".";
    // The target is stat'd by its own name, never through the link: reading
    // the link moves its atime on Linux (relatime, while its atime is not past
    // its mtime), which would be the probe's doing, and would hide the call's.
    // The cell's links are fresh and unread; Darwin's other user's is not the
    // probe's, and is read here, before anything is measured.
    char target_name[PATH_MAX];
    if (p[0] == '/') {
        ssize_t n = readlink(p, target_name, sizeof target_name - 1);
        if (n < 0) die("readlink");
        target_name[n] = 0;
        if (target_name[0] != '/') {
            char joined[PATH_MAX];
            snprintf(joined, sizeof joined, "%s/%s", parent, target_name);
            strcpy(target_name, joined);
        }
    } else {
        // lf -> f, dang -> nx, and Linux's tl -> f.
        strcpy(target_name, row == 2 ? "nx" : "f");
    }
    struct stat lb, tb, db, la, ta, da;
    if (lstat(p, &lb) < 0 || lstat(parent, &db) < 0) die("stat before");
    int target = stat(target_name, &tb) == 0;
    mode_t m = row == 0 || row == 2 ? 0700 : (mode_t)(lb.st_mode & 07777);
    usleep(20000);
    int r = ask(call, AT_FDCWD, p, m, AT_SYMLINK_NOFOLLOW);
    if (lstat(p, &la) < 0 || lstat(parent, &da) < 0) die("stat after");
    if (target && stat(target_name, &ta) < 0) die("stat target after");
    printf("%s", en(r));
    print_times("link", 1, &lb, &la);
    print_times("target", target, &tb, &ta);
    print_times("directory", 1, &db, &da);
}

// --------------------------------------------------------------------- UMASK
static void umask_body(int call, int unused) {
    (void)unused;
    cell(NULL);
    umask(077);
    int r = ask(call, AT_FDCWD, "lf", 0777, AT_SYMLINK_NOFOLLOW);
    printf("%s ", en(r));
    pmode(lmode("lf"));
}

// ---------------------------------------------------------------------- HIGH
static void high_body(int call, int on_link) {
    cell(NULL);
    const char *p = on_link ? "lf" : "f";
    int r = ask(call, AT_FDCWD, p, 0170640, AT_SYMLINK_NOFOLLOW);
    printf("%s l:", en(r));
    pmode(lmode(p));
    printf(" s:");
    pmode(smode(p));
}

// --------------------------------------------------------------------- DIRFD
static void dirfd_body(int call, int nofollow) {
    cell(NULL);
    int fd = open("d", O_RDONLY | O_DIRECTORY);
    if (fd < 0) die("open d");
    int r = ask(call, fd, "l", 0640, nofollow ? AT_SYMLINK_NOFOLLOW : 0);
    printf("%s l:", en(r));
    pmode(lmode("d/l"));
    printf(" f:");
    pmode(lmode("f"));
}

static void run_caller(void) {
    for (int c = 0; c < NCALLS; c++) {
        printf("CALLS\tcaller=%d\t%s", (int)caller, calllabel[c]);
        for (int i = 0; i < NCALLPATHS; i++) {
            printf("\t%s=", call_paths[i]);
            in_child(calls_body, i, c);
        }
        printf("\n");
    }
    for (int s = 0; s < 3; s++)
        for (int in_group = 1; in_group >= 0; in_group--) {
            printf("MODES\tcaller=%d\t%s\t%s\t", (int)caller, subjectlabel[s], in_group ? "in-group" : "out-of-group");
            in_child(modes_body, s, in_group);
            printf("\n");
        }
    for (int c = 0; c < NCALLS; c++)
        for (int row = 0; row < NTIMES; row++) {
            printf("TIMES\tcaller=%d\t%s\t%s\t", (int)caller, calllabel[c], times_rows[row]);
            in_child(times_body, row, c);
            printf("\n");
        }
    for (int c = 0; c < NCALLS; c++) {
        printf("UMASK\tcaller=%d\t%s\t077\t", (int)caller, calllabel[c]);
        in_child(umask_body, c, 0);
        printf("\n");
    }
    for (int c = 0; c < NCALLS; c++)
        for (int on_link = 0; on_link < 2; on_link++) {
            printf("HIGH\tcaller=%d\t%s\t%s\t", (int)caller, calllabel[c], on_link ? "lf" : "f");
            in_child(high_body, c, on_link);
            printf("\n");
        }
    for (int c = 1; c < NCALLS; c++)
        for (int nofollow = 1; nofollow >= 0; nofollow--) {
            printf("DIRFD\tcaller=%d\t%s\t%s\t", (int)caller, calllabel[c], nofollow ? "AT_SYMLINK_NOFOLLOW" : "0");
            in_child(dirfd_body, c, nofollow);
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
    printf("CONST\tAT_FDCWD=%d\tAT_SYMLINK_NOFOLLOW=0x%x\n", AT_FDCWD, AT_SYMLINK_NOFOLLOW);
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
    if (geteuid() == 0) {
        fprintf(stderr, "run as an ordinary user: the other user's rows name the machine's own inodes\n");
        return 2;
    }
    caller = geteuid();
    run_caller();
#endif
    return 0;
}
