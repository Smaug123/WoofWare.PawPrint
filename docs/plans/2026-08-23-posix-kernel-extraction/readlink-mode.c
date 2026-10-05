// Measures whether readlink(2) and readlinkat(2) consult the symbolic link's
// own permission bits, and if so, which of them, for whom, and where that
// refusal falls among the call's other answers.
//
// Darwin sections (an ordinary user; a link it owns gets any mode through
// fchmodat(AT_SYMLINK_NOFOLLOW)):
//   OWNER      every mode 0 to 0777 given to a link the caller owns, in the
//              caller's group: the mode lstat then reports and readlink's
//              answer. OWNERSWEEP counts, over all 4096 modes, in the
//              caller's group and out of it, the answers that differ from
//              "ok exactly when the owner's read bit is set".
//   ORDER      one mode-0000 link of each kind (to a file, to a directory,
//              dangling, to itself), and a readable link reached through an
//              unreadable one: readlink with each size and buffer, a
//              trailing separator, readlinkat from AT_FDCWD and from a
//              directory descriptor, the empty path, and a link opened with
//              O_SYMLINK; then lstat, stat and open through the link.
//   CHMOD      the same link given each of a few modes in turn: whether
//              readlink follows the change, both ways.
//   FOREIGN    a symbolic link another user owns (each argument): its owner,
//              group, mode and whether the caller is in its group, readlink
//              of it where it is, and of a hard link to it in a directory
//              the caller owns (so no directory of the original path can be
//              what refuses).
//
// Linux sections (root; each reader drops in a child):
//   PLAIN      links made by symlink(2) (always 0777 here): root's link
//              read by root and by uid 1000, and uid 1000's read by itself.
//   CRAFTED    links whose mode, owner and group were set with debugfs on an
//              ext4 image, since nothing on Linux changes a link's mode: every
//              mode 0 to 07777 for each of three owners (uid 1000 itself;
//              uid 2000 in group 1000; uid 2000 in group 2000), read by root
//              and by uid 1000 (gid 1000, no supplementary groups). One count
//              row per reader and owner; CRAFTEDROW for a few modes; and
//              CRAFTEDEMPTY, readlinkat(fd, "") on an O_PATH|O_NOFOLLOW
//              descriptor of a mode-0000 link.
//
// Darwin, as an ordinary user, with links root owns on the machine at hand
// (2026-10-04: lrwx------ root:wheel, lrw-r--r-- root:wheel, lrwxr-xr-x
// root:wheel, and lrwxr-xr-x root:staff, staff being the caller's group):
//   nix develop -c clang -Wall -Werror -o /tmp/readlink-mode readlink-mode.c && /tmp/readlink-mode "$(mktemp -d /private/tmp/rlmode.XXXXXX)" /Applications/Signal.app/Contents/Frameworks/ReactiveObjC.framework/Resources "/Library/User Pictures/Flowers/Lotus.heic" /private/var/run/current-system /Library/Trial/v7/AssetStore/assets/b3/6aab10b1d5c03b0470684309/refs/link-50D41774-BA5E-4D78-BDF8-08EA0853BC82 > readlink-mode.darwin-27.0-uid501.txt
// Linux, as root with every capability (for the loop mount):
//   container run --rm --cap-add ALL -v "$PWD":/probe gcc:14 sh -c 'apt-get update -qq && apt-get install -y -qq e2fsprogs >/dev/null && gcc -Wall -Werror -O1 -o /tmp/p /probe/readlink-mode.c && /tmp/p "$(mktemp -d)" > /probe/readlink-mode.linux-6.18.5-aarch64.txt'
//
// Measured 2026-10-04 on Darwin 27.0 arm64 (uid 501) and Linux 6.18.5
// aarch64 (gcc:14, glibc; /tmp and an ext4 image). The outputs are beside this
// file, and WoofWare.PosixKernel.Test/TestReadLinkMode.fs replays them.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>

static const char *en(int e) {
    static char buf[32];
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case ELOOP: return "ELOOP";
    case ENOTSUP: return "ENOTSUP";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static void die(const char *what) {
    perror(what);
    _exit(2);
}

// snprintf into an array, dying rather than truncating.
#define FMT(dst, ...)                                                    \
    do {                                                                 \
        if (snprintf(dst, sizeof dst, __VA_ARGS__) >= (int)sizeof dst) \
            die("path too long");                                        \
    } while (0)

// "ok:<target>", or the errno.
static const char *answer(ssize_t r, const char *buf) {
    static char out[PATH_MAX + 8];
    if (r < 0)
        snprintf(out, sizeof out, "%s", en(errno));
    else
        snprintf(out, sizeof out, "ok:%.*s", (int)r, buf);
    return out;
}

static const char *base;
static int cellno;

static void in_child(void (*body)(void)) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(60);
        body();
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("CHILD-DIED\tstatus=%d\n", status);
}

static int lmode(const char *p) {
    struct stat st;
    if (lstat(p, &st) < 0) return -1;
    return (int)(st.st_mode & 07777);
}

#ifdef __APPLE__

// "ok(<length>)", or the errno: for targets that are not ours to print.
static const char *answer_len(ssize_t r) {
    static char out[32];
    if (r < 0)
        snprintf(out, sizeof out, "%s", en(errno));
    else
        snprintf(out, sizeof out, "ok(%zd)", r);
    return out;
}

// A fresh directory of its own, made the cwd.
static void enter_cell(void) {
    char c[PATH_MAX];
    snprintf(c, sizeof c, "%s/c%05d", base, cellno);
    if (mkdir(c, 0777) < 0 || chmod(c, 0777) < 0) die("mkdir cell");
    if (chdir(c) < 0) die("chdir cell");
}


static int argc_;
static char **argv_;

static int in_group(gid_t g) {
    if (g == getegid()) return 1;
    gid_t gs[NGROUPS_MAX + 1];
    int n = getgroups(NGROUPS_MAX + 1, gs);
    for (int i = 0; i < n; i++)
        if (gs[i] == g) return 1;
    return 0;
}

static void touch(const char *p) {
    int fd = open(p, O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die(p);
    close(fd);
}

static void owner_body(void) {
    enter_cell();
    touch("t");
    if (symlink("t", "l") < 0) die("symlink");
    char buf[64];
    // In the caller's group first (a new link takes the directory's group,
    // wheel in /private/tmp), then left out of it.
    for (int pass = 0; pass < 2; pass++) {
        if (pass == 0 && lchown("l", (uid_t)-1, getegid()) < 0) die("lchown");
        if (pass == 1) {
            if (unlink("l") < 0 || symlink("t", "l") < 0) die("relink");
        }
        struct stat st;
        if (lstat("l", &st) < 0) die("lstat");
        int grouped = in_group(st.st_gid);
        if (grouped != (pass == 0)) die("link group is not what this pass needs");
        int mismatches = 0, failures = 0;
        for (int m = 0; m <= 07777; m++) {
            if (fchmodat(AT_FDCWD, "l", (mode_t)m, AT_SYMLINK_NOFOLLOW) < 0) {
                failures++;
                continue;
            }
            int lm = lmode("l");
            ssize_t r = readlink("l", buf, sizeof buf);
            int e = errno;
            int predicted_ok = (lm & 0400) != 0;
            int ok = r >= 0;
            if (ok != predicted_ok || (!ok && e != EACCES)) mismatches++;
            errno = e;
            if (pass == 0 && m <= 0777) printf("OWNER\tcaller=%d\tmode=%04o\tlmode=%04o\treadlink=%s\n", (int)getuid(), m, lm, answer(r, buf));
        }
        printf("OWNERSWEEP\tcaller=%d\t%s\tgroup=%d\tmodes=4096\tchmod-failures=%d\tmismatches=%d\n", (int)getuid(),
               pass == 0 ? "in-group" : "out-of-group", (int)st.st_gid, failures, mismatches);
    }
}

static void row(const char *what, const char *result) { printf("ORDER\tcaller=%d\t%s\t%s\n", (int)getuid(), what, result); }

static void order_body(void) {
    enter_cell();
    touch("t");
    if (mkdir("d", 0755) < 0) die("d");
    if (symlink("../t", "d/x") < 0) die("d/x");
    if (symlink("t", "l") < 0 || symlink("d", "ld") < 0 || symlink("nx", "dang") < 0 || symlink("cyc", "cyc") < 0) die("symlink");
    const char *zeroed[] = {"l", "ld", "dang", "cyc"};
    for (int i = 0; i < 4; i++) {
        if (fchmodat(AT_FDCWD, zeroed[i], 0, AT_SYMLINK_NOFOLLOW) < 0) die("fchmodat 0");
        if (lmode(zeroed[i]) != 0) die("mode 0 did not stick");
    }
    printf("ORDER\tcaller=%d\tfixture\tl=%04o ld=%04o dang=%04o cyc=%04o d/x=%04o\n", (int)getuid(), lmode("l"), lmode("ld"), lmode("dang"),
           lmode("cyc"), lmode("d/x"));
    char buf[64];
    char *volatile bad = (char *)8;
    char *volatile null = NULL;
    row("readlink(l, buf, 64)", answer(readlink("l", buf, 64), buf));
    row("readlink(l, buf, 1)", answer(readlink("l", buf, 1), buf));
    row("readlink(l, buf, 0)", answer(readlink("l", buf, 0), buf));
    row("readlink(l, NULL, 0)", answer(readlink("l", null, 0), buf));
    row("readlink(l, buf, -1)", answer(readlink("l", buf, (size_t)-1), buf));
    row("readlink(l, (char*)8, 16)", answer(readlink("l", bad, 16), buf));
    row("readlink(l, NULL, 16)", answer(readlink("l", null, 16), buf));
    row("readlink(ld, buf, 64)", answer(readlink("ld", buf, 64), buf));
    row("readlink(dang, buf, 64)", answer(readlink("dang", buf, 64), buf));
    row("readlink(cyc, buf, 64)", answer(readlink("cyc", buf, 64), buf));
    row("readlink(ld/, buf, 64)", answer(readlink("ld/", buf, 64), buf));
    row("readlink(ld/x, buf, 64)", answer(readlink("ld/x", buf, 64), buf));
    row("readlink(l/, buf, 64)", answer(readlink("l/", buf, 64), buf));
    row("readlink(dang/, buf, 64)", answer(readlink("dang/", buf, 64), buf));
    row("readlink(cyc/, buf, 64)", answer(readlink("cyc/", buf, 64), buf));
    row("readlink(t, buf, 64)", answer(readlink("t", buf, 64), buf));
    row("readlink(nx, buf, 64)", answer(readlink("nx", buf, 64), buf));
    row("readlinkat(AT_FDCWD, l, buf, 64)", answer(readlinkat(AT_FDCWD, "l", buf, 64), buf));
    int dot = open(".", O_RDONLY | O_DIRECTORY);
    int dfd = open("d", O_RDONLY | O_DIRECTORY);
    if (dot < 0 || dfd < 0) die("open dirs");
    row("readlinkat(cell, l, buf, 64)", answer(readlinkat(dot, "l", buf, 64), buf));
    row("readlinkat(cell, l, buf, 0)", answer(readlinkat(dot, "l", buf, 0), buf));
    row("readlinkat(d, ../l, buf, 64)", answer(readlinkat(dfd, "../l", buf, 64), buf));
    row("readlinkat(d, x, buf, 64)", answer(readlinkat(dfd, "x", buf, 64), buf));
    row("readlinkat(cell, \"\", buf, 64)", answer(readlinkat(dot, "", buf, 64), buf));
    int sfd = open("l", O_SYMLINK | O_RDONLY);
    row("open(l, O_SYMLINK|O_RDONLY)", sfd < 0 ? en(errno) : "ok");
    if (sfd >= 0) row("readlinkat(O_SYMLINK l, \"\", buf, 64)", answer(readlinkat(sfd, "", buf, 64), buf));
    struct stat st;
    row("lstat(l)", lstat("l", &st) < 0 ? en(errno) : "ok");
    row("stat(l)", stat("l", &st) < 0 ? en(errno) : "ok");
    int ofd = open("l", O_RDONLY);
    row("open(l, O_RDONLY)", ofd < 0 ? en(errno) : "ok");
}

static void chmod_body(void) {
    enter_cell();
    touch("t");
    if (symlink("t", "l") < 0) die("symlink");
    if (lchown("l", (uid_t)-1, getegid()) < 0) die("lchown");
    char buf[64];
    const int modes[] = {0, 0400, 0, 0040, 0004, 0444, 0333, 0755, 0};
    for (unsigned i = 0; i < sizeof modes / sizeof modes[0]; i++) {
        int r = fchmodat(AT_FDCWD, "l", (mode_t)modes[i], AT_SYMLINK_NOFOLLOW);
        printf("CHMOD\tcaller=%d\tmode=%04o\tfchmodat=%s\tlmode=%04o\treadlink=%s\n", (int)getuid(), modes[i], r < 0 ? en(errno) : "ok",
               lmode("l"), answer(readlink("l", buf, sizeof buf), buf));
    }
}

static void foreign_body(void) {
    enter_cell();
    char buf[PATH_MAX];
    for (int i = 2; i < argc_; i++) {
        const char *p = argv_[i];
        struct stat st;
        if (lstat(p, &st) < 0 || !S_ISLNK(st.st_mode) || st.st_uid == getuid()) {
            printf("FOREIGN\tcaller=%d\tpath=%s\tnot another user's link\n", (int)getuid(), p);
            continue;
        }
        char direct[32];
        snprintf(direct, sizeof direct, "%s", answer_len(readlink(p, buf, sizeof buf)));
        char h[32];
        snprintf(h, sizeof h, "h%d", i);
        int lr = linkat(AT_FDCWD, p, AT_FDCWD, h, 0);
        char via[32] = "-", same[32] = "-";
        if (lr == 0) {
            struct stat hs;
            if (lstat(h, &hs) < 0) die("lstat h");
            snprintf(same, sizeof same, "%s", hs.st_ino == st.st_ino && S_ISLNK(hs.st_mode) && hs.st_uid == st.st_uid ? "same-inode" : "DIFFERENT");
            snprintf(via, sizeof via, "%s", answer_len(readlink(h, buf, sizeof buf)));
        }
        printf("FOREIGN\tcaller=%d\tuid=%u\tgid=%u\tin-group=%d\tlmode=%04o\tdirect=%s\tlinkat=%s\thardlink=%s\tvia-hardlink=%s\tpath=%s\n", (int)getuid(),
               (unsigned)st.st_uid, (unsigned)st.st_gid, in_group(st.st_gid), (int)(st.st_mode & 07777), direct, lr < 0 ? en(errno) : "ok", same,
               via, p);
        if (lr == 0) unlink(h);
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch> [another user's symlink...]\n", argv[0]);
        return 2;
    }
    argc_ = argc;
    argv_ = argv;
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    gid_t gs[NGROUPS_MAX + 1];
    int n = getgroups(NGROUPS_MAX + 1, gs);
    printf("RUN\t%s %s %s\tuid=%d\tegid=%d\tgroups=", u.sysname, u.release, u.machine, (int)geteuid(), (int)getegid());
    for (int i = 0; i < n; i++) printf("%s%d", i ? "," : "", (int)gs[i]);
    printf("\n");
    in_child(owner_body);
    in_child(order_body);
    in_child(chmod_body);
    in_child(foreign_body);
    return 0;
}

#else // Linux

#include <sys/statfs.h>

static uid_t reader;

static void drop(void) {
    if (reader == 0) return;
    if (setgroups(0, NULL) < 0 || setgid(1000) < 0 || setuid(reader) < 0) die("drop");
}

static char plain_dir[PATH_MAX];

static void plain_read_body(void) {
    drop();
    char buf[64], p[PATH_MAX + 8];
    FMT(p, "%s/rl", plain_dir);
    printf("PLAIN\treader=%d\tlink-owner=0\tlmode=%04o\treadlink=%s\n", (int)getuid(), lmode(p), answer(readlink(p, buf, sizeof buf), buf));
}

static void plain_own_body(void) {
    drop();
    char buf[64], p[PATH_MAX + 8];
    FMT(p, "%s/ul", plain_dir);
    if (symlink("t", p) < 0) die("symlink ul");
    struct stat st;
    if (lstat(p, &st) < 0) die("lstat ul");
    printf("PLAIN\treader=%d\tlink-owner=%d\tlmode=%04o\treadlink=%s\n", (int)getuid(), (int)st.st_uid, lmode(p), answer(readlink(p, buf, sizeof buf), buf));
}

// The three owners a crafted link has, and the ids debugfs gives each.
static const char *classes[] = {"owner", "group", "other"};
static const char class_letter[] = {'o', 'g', 'x'};
static const unsigned class_uid[] = {1000, 2000, 2000};
static const unsigned class_gid[] = {1000, 1000, 2000};
static char crafted_dir[PATH_MAX + 8];

static void crafted_body(void) {
    drop();
    char buf[64], p[PATH_MAX + 16];
    for (int c = 0; c < 3; c++) {
        int ok = 0, eacces = 0, other = 0, mismatch = 0;
        for (int m = 0; m <= 07777; m++) {
            FMT(p, "%s/%c%04o", crafted_dir, class_letter[c], m);
            struct stat st;
            if (lstat(p, &st) < 0 || !S_ISLNK(st.st_mode) || (int)(st.st_mode & 07777) != m || st.st_uid != class_uid[c] || st.st_gid != class_gid[c]) {
                mismatch++;
                continue;
            }
            ssize_t r = readlink(p, buf, sizeof buf);
            if (r == 1 && buf[0] == 't')
                ok++;
            else if (r < 0 && errno == EACCES)
                eacces++;
            else
                other++;
            if (m == 0 || m == 0444 || m == 0400 || m == 0040 || m == 0004 || m == 0333 || m == 0777 || m == 07000)
                printf("CRAFTEDROW\treader=%d\tclass=%s\tuid=%u\tgid=%u\tlmode=%04o\treadlink=%s\n", (int)getuid(), classes[c], class_uid[c], class_gid[c],
                       m, answer(r, buf));
        }
        printf("CRAFTED\treader=%d\tclass=%s\tuid=%u\tgid=%u\tmodes=4096\tok=%d\tEACCES=%d\tother=%d\tsetup-mismatch=%d\n", (int)getuid(), classes[c],
               class_uid[c], class_gid[c], ok, eacces, other, mismatch);
    }
    FMT(p, "%s/x0000", crafted_dir);
    int fd = open(p, O_PATH | O_NOFOLLOW);
    if (fd < 0)
        printf("CRAFTEDEMPTY\treader=%d\tlmode=0000\topen=%s\n", (int)getuid(), en(errno));
    else
        printf("CRAFTEDEMPTY\treader=%d\tlmode=0000\treadlinkat(fd, \"\")=%s\n", (int)getuid(), answer(readlinkat(fd, "", buf, sizeof buf), buf));
}

static void sh(const char *cmd) {
    fflush(stdout);
    int r = system(cmd);
    if (r != 0) {
        fprintf(stderr, "failed (%d): %s\n", r, cmd);
        exit(2);
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch>\n", argv[0]);
        return 2;
    }
    if (geteuid() != 0) {
        fprintf(stderr, "run as root\n");
        return 2;
    }
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    struct statfs sf;
    if (statfs(base, &sf) < 0) die("statfs");
    printf("RUN\t%s %s %s\tuid=%d\tscratch-f_type=0x%lx\n", u.sysname, u.release, u.machine, (int)geteuid(), (unsigned long)sf.f_type);

    // PLAIN: root's link, read by root and by uid 1000; uid 1000's own link.
    FMT(plain_dir, "%s/plain", base);
    if (mkdir(plain_dir, 0777) < 0 || chmod(plain_dir, 0777) < 0) die("plain");
    char p[PATH_MAX + 8];
    FMT(p, "%s/rl", plain_dir);
    if (symlink("t", p) < 0) die("symlink rl");
    for (int i = 0; i < 2; i++) {
        reader = i == 0 ? 0 : 1000;
        in_child(plain_read_body);
    }
    reader = 1000;
    in_child(plain_own_body);

    // CRAFTED: an ext4 image whose links get their mode, owner and group
    // from debugfs, since no syscall changes a Linux link's mode.
    char img[PATH_MAX + 8], mnt[PATH_MAX + 8], cmds[PATH_MAX + 16], cmd[6 * PATH_MAX];
    FMT(img, "%s/img", base);
    FMT(mnt, "%s/mnt", base);
    FMT(cmds, "%s/debugfs.cmds", base);
    if (mkdir(mnt, 0755) < 0) die("mnt");
    FMT(cmd, "truncate -s 64M '%s' && mkfs.ext4 -q -F -N 16384 '%s' && mount -o loop '%s' '%s'", img, img, img, mnt);
    sh(cmd);
    FMT(crafted_dir, "%s/c", mnt);
    if (mkdir(crafted_dir, 0755) < 0 || chmod(crafted_dir, 0755) < 0) die("crafted dir");
    char t[PATH_MAX + 8];
    FMT(t, "%s/t", crafted_dir);
    int fd = open(t, O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("t");
    close(fd);
    FILE *f = fopen(cmds, "w");
    if (!f) die("cmds");
    for (int c = 0; c < 3; c++)
        for (int m = 0; m <= 07777; m++) {
            FMT(p, "%s/%c%04o", crafted_dir, class_letter[c], m);
            if (symlink("t", p) < 0) die("symlink crafted");
            fprintf(f, "sif /c/%c%04o mode 0x%x\n", class_letter[c], m, 0120000 | m);
            fprintf(f, "sif /c/%c%04o uid %u\n", class_letter[c], m, class_uid[c]);
            fprintf(f, "sif /c/%c%04o gid %u\n", class_letter[c], m, class_gid[c]);
        }
    fclose(f);
    FMT(cmd, "umount '%s' && debugfs -w -f '%s' '%s' >/dev/null 2>&1 && mount -o loop '%s' '%s'", mnt, cmds, img, img, mnt);
    sh(cmd);
    for (int i = 0; i < 2; i++) {
        reader = i == 0 ? 0 : 1000;
        in_child(crafted_body);
    }
    FMT(cmd, "umount '%s'", mnt);
    sh(cmd);
    return 0;
}

#endif
