// Measures access(2) and faccessat(2): which mode words and flag words each
// kernel accepts and in what order it screens them, what the dirfd does, how
// the path resolves, which IDs decide the answer, which permission triple is
// consulted, and what root is granted.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/access-rules.c && /tmp/p /tmp/accessp && /tmp/p /dev/shm/accessp'
//         Run it as root: each credential row forks a child that takes the
//         credentials under test, real and effective IDs set independently,
//         against inodes root has chowned to an owner and group the child is
//         and is not.
// Darwin: nix develop -c clang -Wall -o access-rules access-rules.c && ./access-rules "$(mktemp -d /private/tmp/accessp.XXXXXX)"
//         As an ordinary user: only the caller's own inodes are swept, and
//         inodes owned by others are read where the system already has them.
//         Root's rows, and any row whose real and effective IDs differ, need
//         root and are not measured there.
//
// Measured on Linux 6.18.5 (aarch64, root in the container) on ext4 (/tmp)
// and tmpfs (/dev/shm), and on Darwin 27.0 (arm64, uid 501) on 2026-10-01; the
// output of each is beside this file. The two Linux runs agree except where
// /dev/shm is mounted noexec, which refuses X_OK on every regular file to
// everyone, root included: a mount option, which the tmpfs run counts as
// `noexec-refusals` rather than mismatches. The rows are transcribed in
// WoofWare.PosixKernel.Test/TestAccess.fs.
//
// Each sweep compares every row against the rule WoofWare.PosixKernel models
// and prints the mismatch count, and the first few mismatches. On Darwin the
// scan of other users' inodes also meets refusals that are not permission
// bits at all (the sealed system volume answers a write EROFS or EPERM, and
// the sandbox refuses /private/var/OOPJit); it counts and prints them, and the
// model has neither.
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
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>
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
    case ELOOP: return "ELOOP";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EROFS: return "EROFS";
    case EFAULT: return "EFAULT";
    default: return strerror(e);
    }
}

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

// The kernel's own faccessat, not a libc wrapper that might screen the flags
// itself. On Linux that is faccessat2, the only one taking flags; glibc's
// `access` is the flagless faccessat, which the kernel answers as flags 0.
static int kfaccessat(int dirfd, const char *pth, int amode, int flags) {
#ifdef __linux__
    long rc = syscall(SYS_faccessat2, dirfd, pth, amode, flags);
#else
    int rc = faccessat(dirfd, pth, amode, flags);
#endif
    return rc == 0 ? 0 : errno;
}

static int kaccess(const char *pth, int amode) { return access(pth, amode) == 0 ? 0 : errno; }

// The rule modelled. A caller stands towards an inode as privileged or not,
// owner or not, member of its group or not; `kind` is 0 for a regular file, 1
// for a directory and 2 for a symbolic link (asked about itself). F_OK asks
// nothing. Unprivileged: every requested bit must be set in the one triple the
// standing selects (owner, else group, else other). Privileged: read and
// write are granted, and so is execute on a directory; execute on anything
// else needs at least one of the three execute bits.
static int predict(int priv, int owns, int ingroup, int kind, int mode, int amode) {
    if (amode == 0) return 0;
    if (priv) {
        if ((amode & X_OK) && kind != 1 && !(mode & 0111)) return EACCES;
        return 0;
    }
    int triple = owns ? (mode >> 6) & 7 : ingroup ? (mode >> 3) & 7 : mode & 7;
    return (amode & 7 & ~triple) ? EACCES : 0;
}

static const char *kinds[] = {"file", "dir", "symlink"};

// ---------------------------------------------------------------- argument screens

// A descriptor open on `pth`, opened once and kept for the run.
static int ffd_for_order(const char *pth) {
    static int fd = -1;
    if (fd < 0) fd = open(pth, O_RDONLY);
    if (fd < 0) die("open order fd");
    return fd;
}

static void screens(void) {
    char f[PATH_MAX], z[PATH_MAX], nx[PATH_MAX];
    path(f, base, "screen_f");
    path(z, base, "screen_z");
    path(nx, base, "screen_nx");
    mkfile(f);
    mkfile(z);
    if (chmod(f, 0777) != 0) die("chmod screen_f");
    if (chmod(z, 0) != 0) die("chmod screen_z");

    p("CONST\tAT_FDCWD=%d\tAT_EACCESS=0x%x\tAT_SYMLINK_NOFOLLOW=0x%x", AT_FDCWD, AT_EACCESS, AT_SYMLINK_NOFOLLOW);
#ifdef AT_EMPTY_PATH
    p("\tAT_EMPTY_PATH=0x%x", AT_EMPTY_PATH);
#endif
    p("\n");

    // Every single bit of the mode word, against a 0777 file and a 0000 file
    // the caller owns, through access and through faccessat with no flags.
    for (int k = 0; k < 32; k++) {
        int amode = (int)(1u << k);
        p("MODE-BIT\tbit=%d\tamode=0x%08x\taccess(0777)=%s\taccess(0000)=%s\tfaccessat(0777)=%s\tfaccessat(0000)=%s\n", k,
          (unsigned)amode, en(kaccess(f, amode)), en(kaccess(z, amode)), en(kfaccessat(AT_FDCWD, f, amode, 0)),
          en(kfaccessat(AT_FDCWD, z, amode, 0)));
    }
    int specials[] = {-1, 0x7, 0xF, 0x8, 0x7FFFFFFF, (int)0x80000000};
    for (unsigned i = 0; i < sizeof specials / sizeof specials[0]; i++)
        p("MODE-WORD\tamode=0x%08x\taccess(0777)=%s\taccess(0000)=%s\n", (unsigned)specials[i],
          en(kaccess(f, specials[i])), en(kaccess(z, specials[i])));

    // Every single bit of the flag word, asking F_OK of the 0777 file.
    for (int k = 0; k < 32; k++) {
        int flags = (int)(1u << k);
        p("FLAG-BIT\tbit=%d\tflags=0x%08x\tfaccessat(F_OK)=%s\tfaccessat(R_OK on 0000)=%s\n", k, (unsigned)flags,
          en(kfaccessat(AT_FDCWD, f, F_OK, flags)), en(kfaccessat(AT_FDCWD, z, R_OK, flags)));
    }

    // Order: which of a bad mode, bad flags, a bad dirfd and an absent path
    // wins. 0x40000000 is a flag bit neither kernel accepts.
    int badflag = 0x40000000;
    p("ORDER\tbad mode, absent path\taccess=%s\tfaccessat=%s\n", en(kaccess(nx, 8)), en(kfaccessat(AT_FDCWD, nx, 8, 0)));
    p("ORDER\tbad flags, absent path\t%s\n", en(kfaccessat(AT_FDCWD, nx, F_OK, badflag)));
    p("ORDER\tbad mode, bad flags\t%s\n", en(kfaccessat(AT_FDCWD, f, 8, badflag)));
    p("ORDER\tbad mode, bad dirfd, relative\t%s\n", en(kfaccessat(12345, "screen_f", 8, 0)));
    p("ORDER\tbad flags, bad dirfd, relative\t%s\n", en(kfaccessat(12345, "screen_f", F_OK, badflag)));
    p("ORDER\tbad dirfd, relative, absent\t%s\n", en(kfaccessat(12345, "nx", F_OK, 0)));
    p("ORDER\tbad mode, empty path\taccess=%s\n", en(kaccess("", 8)));
    // Darwin's extended rights (_READ_OK, 1 << 9, and _CHOWN_OK, 1 << 21)
    // against a path that does not resolve: whether the path is looked up
    // before the rights are.
    p("ORDER\tbit 9, absent path\taccess=%s\tbit 21, absent path\taccess=%s\tbit 21, screen_f/x\taccess=%s\n", en(kaccess(nx, 1 << 9)),
      en(kaccess(nx, 1 << 21)), en(kaccess("screen_f/x", 1 << 21)));
    p("ORDER\tbad mode, NULL path\taccess=%s\n", en(kaccess(NULL, 8)));
    p("ORDER\tgood mode, NULL path\taccess=%s\tfaccessat=%s\n", en(kaccess(NULL, F_OK)), en(kfaccessat(AT_FDCWD, NULL, F_OK, 0)));

    // Where the copy-in of the path falls among the screens: an unreadable
    // pointer (NULL) and a path of PATH_MAX bytes with no NUL inside it,
    // against a bad mode, bad flags and a bad dirfd.
    static char toolong[PATH_MAX + 1];
    memset(toolong, 'a', PATH_MAX);
    toolong[PATH_MAX] = 0;
    p("COPYIN\tNULL\tbad mode=%s\tbad flags=%s\tbad dirfd=%s\tfile dirfd=%s\n", en(kfaccessat(AT_FDCWD, NULL, 8, 0)),
      en(kfaccessat(AT_FDCWD, NULL, F_OK, badflag)), en(kfaccessat(12345, NULL, F_OK, 0)), en(kfaccessat(ffd_for_order(f), NULL, F_OK, 0)));
    p("COPYIN\tPATH_MAX bytes\tgood=%s\tbad mode=%s\tbad flags=%s\tbad dirfd=%s\n", en(kfaccessat(AT_FDCWD, toolong, F_OK, 0)),
      en(kfaccessat(AT_FDCWD, toolong, 8, 0)), en(kfaccessat(AT_FDCWD, toolong, F_OK, badflag)), en(kfaccessat(12345, toolong, F_OK, 0)));
    p("COPYIN\tempty\tbad mode=%s\tbad flags=%s\tbad dirfd=%s\n", en(kfaccessat(AT_FDCWD, "", 8, 0)), en(kfaccessat(AT_FDCWD, "", F_OK, badflag)),
      en(kfaccessat(12345, "", F_OK, 0)));
#ifdef AT_EMPTY_PATH
    p("COPYIN\tNULL+AT_EMPTY_PATH\tAT_FDCWD=%s\tfile dirfd=%s\tbad dirfd=%s\n", en(kfaccessat(AT_FDCWD, NULL, F_OK, AT_EMPTY_PATH)),
      en(kfaccessat(ffd_for_order(f), NULL, F_OK, AT_EMPTY_PATH)), en(kfaccessat(12345, NULL, F_OK, AT_EMPTY_PATH)));
    p("COPYIN\tnonempty+AT_EMPTY_PATH\tfile dirfd, relative=%s\tfile dirfd, absolute=%s\n", en(kfaccessat(ffd_for_order(f), "screen_f", F_OK, AT_EMPTY_PATH)),
      en(kfaccessat(ffd_for_order(f), f, F_OK, AT_EMPTY_PATH)));
#endif

    // The dirfd. An absolute path, then a relative one, against each kind of
    // dirfd.
    int dfd = open(base, O_RDONLY);
    int ffd = open(f, O_RDONLY);
    if (dfd < 0 || ffd < 0) die("open dirfd");
    struct {
        const char *label;
        int fd;
    } dirfds[] = {{"AT_FDCWD", AT_FDCWD}, {"-1", -1}, {"12345 (closed)", 12345}, {"-100", -100}, {"-2", -2},
                  {"a directory", dfd},   {"a file", ffd}};
    for (unsigned i = 0; i < sizeof dirfds / sizeof dirfds[0]; i++) {
        p("DIRFD\t%s\tabsolute=%s\trelative(screen_f)=%s\trelative(absent)=%s\tempty=%s", dirfds[i].label,
          en(kfaccessat(dirfds[i].fd, f, F_OK, 0)), en(kfaccessat(dirfds[i].fd, "screen_f", F_OK, 0)),
          en(kfaccessat(dirfds[i].fd, "screen_nx", F_OK, 0)), en(kfaccessat(dirfds[i].fd, "", F_OK, 0)));
#ifdef AT_EMPTY_PATH
        p("\tempty+AT_EMPTY_PATH(F_OK)=%s\tempty+AT_EMPTY_PATH(W_OK)=%s", en(kfaccessat(dirfds[i].fd, "", F_OK, AT_EMPTY_PATH)),
          en(kfaccessat(dirfds[i].fd, "", W_OK, AT_EMPTY_PATH)));
#endif
        p("\n");
    }
    close(dfd);
    close(ffd);
}

// ---------------------------------------------------------------- paths

static void paths(void) {
    char d[PATH_MAX], f[PATH_MAX], t[PATH_MAX];
    path(d, base, "pd");
    if (mkdir(d, 0755) != 0) die("mkdir pd");
    path(f, d, "f");
    mkfile(f);
    if (chmod(f, 0644) != 0) die("chmod pd/f");
    path(t, d, "sub");
    if (mkdir(t, 0755) != 0) die("mkdir pd/sub");
    char l[PATH_MAX];
    path(l, d, "lf");
    if (symlink("f", l) != 0) die("symlink lf");
    path(l, d, "ld");
    if (symlink("sub", l) != 0) die("symlink ld");
    path(l, d, "dang");
    if (symlink("nx", l) != 0) die("symlink dang");
    path(l, d, "cyc");
    if (symlink("cyc", l) != 0) die("symlink cyc");
    path(l, d, "lx");
    if (symlink("f", l) != 0) die("symlink lx");

    if (chdir(d) != 0) die("chdir pd");
    char longname[300];
    memset(longname, 'a', 299);
    longname[299] = 0;
    const char *names[] = {"f", "f/", "f/.", "f/x", "sub", "sub/", "sub/.", "sub/..", "lf", "lf/", "ld", "ld/", "dang", "dang/", "cyc", "cyc/", "nx", "nx/", ".", "..", "/", longname};
    for (unsigned i = 0; i < sizeof names / sizeof names[0]; i++) {
        const char *nm = i == sizeof names / sizeof names[0] - 1 ? "<299 a>" : names[i];
        p("PATH\t%s\taccess(F_OK)=%s\taccess(W_OK)=%s\tnofollow(F_OK)=%s\tnofollow(W_OK)=%s\n", nm, en(kaccess(names[i], F_OK)),
          en(kaccess(names[i], W_OK)), en(kfaccessat(AT_FDCWD, names[i], F_OK, AT_SYMLINK_NOFOLLOW)),
          en(kfaccessat(AT_FDCWD, names[i], W_OK, AT_SYMLINK_NOFOLLOW)));
    }

    // Timestamps: access reads nothing and writes nothing.
    struct stat before, after;
    if (stat("f", &before) != 0) die("stat f");
    struct timespec ts = {0, 30 * 1000 * 1000};
    nanosleep(&ts, NULL);
    for (int amode = 0; amode <= 7; amode++) kaccess("f", amode);
    if (stat("f", &after) != 0) die("stat f");
#ifdef __APPLE__
    int atime = before.st_atimespec.tv_sec == after.st_atimespec.tv_sec && before.st_atimespec.tv_nsec == after.st_atimespec.tv_nsec;
    int ctime_ = before.st_ctimespec.tv_sec == after.st_ctimespec.tv_sec && before.st_ctimespec.tv_nsec == after.st_ctimespec.tv_nsec;
#else
    int atime = before.st_atim.tv_sec == after.st_atim.tv_sec && before.st_atim.tv_nsec == after.st_atim.tv_nsec;
    int ctime_ = before.st_ctim.tv_sec == after.st_ctim.tv_sec && before.st_ctim.tv_nsec == after.st_ctim.tv_nsec;
#endif
    p("TIMES\taccess(f, 0..7)\tatime=%s\tctime=%s\n", atime ? "kept" : "moved", ctime_ ? "kept" : "moved");

    // The symbolic link's own mode, asked about through AT_SYMLINK_NOFOLLOW.
    // Darwin can give a link a mode (lchmod); Linux cannot, and every link is
    // 0777 there.
    int lmode = mode_of("lx");
    p("SYMLINK-MODE\tlx\t%04o\n", lmode);
#ifdef __APPLE__
    int mismatches = 0, rows = 0, failed = 0;
    for (int mode = 0; mode <= 0777; mode++) {
        if (lchmod("lx", mode) != 0 || mode_of("lx") != mode) {
            failed++;
            continue;
        }
        for (int amode = 0; amode <= 7; amode++) {
            int got = kfaccessat(AT_FDCWD, "lx", amode, AT_SYMLINK_NOFOLLOW);
            int want = predict(0, 1, 1, 2, mode, amode);
            rows++;
            if (got != want) {
                if (mismatches < 8) p("SYMLINK-MISMATCH\tmode=%04o\tamode=%d\tgot=%s\twant=%s\n", mode, amode, en(got), en(want));
                mismatches++;
            }
        }
    }
    p("SYMLINK-SWEEP\towner\trows=%d\tmismatches=%d\tlchmod-failed=%d\n", rows, mismatches, failed);
#endif
    if (chdir(base) != 0) die("chdir base");
}

// ---------------------------------------------------------------- the caller's own inodes, over every mode

// Every one of the 4096 modes on a file, a directory and (Darwin) a link the
// caller owns, in its own group, asked every amode through access and through
// faccessat(AT_EACCESS).
static void own_sweep(void) {
    char d[PATH_MAX];
    path(d, base, "own");
    if (mkdir(d, 0755) != 0) die("mkdir own");
    // So that S_ISGID sticks for an ordinary user: the inodes made here take
    // this directory's group on Darwin.
    if (chown(d, (uid_t)-1, getegid()) != 0) die("chgrp own");
    int priv = geteuid() == 0;
    for (int kind = 0; kind <= 1; kind++) {
        char t[PATH_MAX];
        path(t, d, kind ? "d" : "f");
        if (kind) {
            if (mkdir(t, 0700) != 0) die("mkdir own/d");
        } else
            mkfile(t);
        int mismatches = 0, rows = 0, unset = 0;
        for (int mode = 0; mode <= 07777; mode++) {
            if (chmod(t, mode) != 0 || mode_of(t) != mode) {
                unset++;
                continue;
            }
            for (int amode = 0; amode <= 7; amode++) {
                int want = predict(priv, 1, 1, kind, mode, amode);
                int got1 = kaccess(t, amode);
                int got2 = kfaccessat(AT_FDCWD, t, amode, AT_EACCESS);
                rows++;
                if (got1 != want || got2 != want) {
                    if (mismatches < 8)
                        p("OWN-MISMATCH\t%s\tmode=%04o\tamode=%d\taccess=%s\teaccess=%s\twant=%s\n", kinds[kind], mode, amode, en(got1), en(got2), en(want));
                    mismatches++;
                }
            }
        }
        chmod(t, 0700);
        p("OWN-SWEEP\t%s\tuid=%d\trows=%d\tmismatches=%d\tmodes-not-settable=%d\n", kinds[kind], (int)geteuid(), rows, mismatches, unset);
    }
}

#ifdef __APPLE__
// ---------------------------------------------------------------- Darwin: the mode word's other bits

// Darwin answers no mode word EINVAL. Its extended rights (_READ_OK and the
// rest, bits 9 to 21) ask questions of their own; every other bit above the
// low three is asked here whether it changes any answer: each of them added
// to every low amode, over every permission word of a file and a directory
// the caller owns.
static void ignored_bits(void) {
    char d[PATH_MAX];
    path(d, base, "ign");
    if (mkdir(d, 0755) != 0) die("mkdir ign");
    for (int kind = 0; kind <= 1; kind++) {
        char t[PATH_MAX];
        path(t, d, kind ? "d" : "f");
        if (kind) {
            if (mkdir(t, 0700) != 0) die("mkdir ign/d");
        } else
            mkfile(t);
        int rows = 0, differ = 0;
        for (int mode = 0; mode <= 0777; mode++) {
            if (chmod(t, mode) != 0) die("chmod ign");
            for (int k = 3; k < 32; k++) {
                if (k >= 9 && k <= 21) continue;
                for (int amode = 0; amode <= 7; amode++) {
                    int with = kaccess(t, amode | (int)(1u << k));
                    int without = kaccess(t, amode);
                    rows++;
                    if (with != without) {
                        if (differ < 8) p("IGNORED-DIFFERS\t%s\tmode=%04o\tbit=%d\tamode=%d\twith=%s\twithout=%s\n", kinds[kind], mode, k, amode, en(with), en(without));
                        differ++;
                    }
                }
            }
        }
        chmod(t, 0700);
        p("IGNORED-SWEEP\t%s\tbits=3..8,22..31\trows=%d\tdiffer=%d\n", kinds[kind], rows, differ);
    }
}

// ---------------------------------------------------------------- Darwin: inodes that already belong to others

static int in_groups(gid_t g) {
    gid_t groups[64];
    int n = getgroups(64, groups);
    if (g == getegid()) return 1;
    for (int i = 0; i < n; i++)
        if (groups[i] == g) return 1;
    return 0;
}

// Every entry of a few system directories that the caller does not own,
// compared with the rule for its standing. A mismatch can be an ACL or a
// sandbox refusal, neither of which is modelled; they are printed, not
// interpreted.
static void foreign_scan(void) {
    const char *dirs[] = {"/etc", "/private/etc", "/usr/bin", "/usr/sbin", "/usr/lib", "/private/var", "/Library", "/Applications", "/private/tmp", "/usr/share"};
    int rows = 0, mismatches = 0, sealed = 0, entries = 0, classes[3] = {0, 0, 0};
    for (unsigned i = 0; i < sizeof dirs / sizeof dirs[0]; i++) {
        DIR *dd = opendir(dirs[i]);
        if (!dd) continue;
        struct dirent *e;
        while ((e = readdir(dd)) != NULL) {
            if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
            char t[PATH_MAX];
            path(t, dirs[i], e->d_name);
            struct stat st;
            if (lstat(t, &st) != 0) continue;
            int kind;
            if (S_ISREG(st.st_mode)) kind = 0;
            else if (S_ISDIR(st.st_mode)) kind = 1;
            else continue;
            if (st.st_uid == geteuid()) continue;
            int ing = in_groups(st.st_gid);
            entries++;
            classes[ing ? 1 : 2]++;
            for (int amode = 0; amode <= 7; amode++) {
                int got = kaccess(t, amode);
                int want = predict(0, 0, ing, kind, st.st_mode & 07777, amode);
                rows++;
                if (got != want) {
                    // A write refused EPERM or EROFS where the bits refuse it
                    // too, or allow it: the sealed system volume and SIP's
                    // restricted flag, neither of them a permission bit.
                    if ((amode & W_OK) && (got == EPERM || got == EROFS)) {
                        sealed++;
                        continue;
                    }
                    if (mismatches < 40)
                        p("FOREIGN-MISMATCH\t%s\t%s\tuid=%d gid=%d%s\tmode=%04o\tamode=%d\tgot=%s\twant=%s\n", t, kinds[kind], (int)st.st_uid,
                          (int)st.st_gid, ing ? "(member)" : "", st.st_mode & 07777, amode, en(got), en(want));
                    mismatches++;
                }
            }
        }
        closedir(dd);
    }
    p("FOREIGN-SCAN\tentries=%d\tgroup-member=%d\tother=%d\trows=%d\twrites-refused-EPERM-or-EROFS=%d\tother-mismatches=%d\n", entries, classes[1], classes[2], rows,
      sealed, mismatches);
}
#endif

#ifdef __linux__
// ---------------------------------------------------------------- Linux: credentials

typedef struct {
    const char *name;
    uid_t ruid, euid;
    gid_t rgid, egid;
    int ng;
    gid_t g[4];
} cred;

// The inodes swept are owned by 1001:2000.
static const uid_t OWNER = 1001;
static const gid_t GROUP = 2000;

static const cred creds[] = {
    {"owner", 1001, 1001, 3000, 3000, 0, {0}},
    {"group-primary", 1000, 1000, 2000, 2000, 0, {0}},
    {"group-supplementary", 1000, 1000, 1000, 1000, 1, {2000}},
    {"other", 1000, 1000, 1000, 1000, 1, {3000}},
    {"root", 0, 0, 0, 0, 1, {0}},
    {"real-root/effective-other", 0, 1000, 1000, 1000, 0, {0}},
    {"real-other/effective-root", 1000, 0, 1000, 1000, 0, {0}},
    {"real-owner/effective-other", 1001, 1000, 1000, 1000, 0, {0}},
    {"real-other/effective-owner", 1000, 1001, 1000, 1000, 0, {0}},
    {"real-group/effective-other", 1000, 1000, 2000, 1000, 0, {0}},
    {"real-other/effective-group", 1000, 1000, 1000, 2000, 0, {0}},
    {"real-root-group/effective-other", 1000, 1000, 0, 1000, 0, {0}},
};

static void become(const cred *c) {
    if (setgroups(c->ng, c->g) != 0) die("setgroups");
    if (setresgid(c->rgid, c->egid, c->egid) != 0) die("setresgid");
    if (setresuid(c->ruid, c->euid, c->euid) != 0) die("setresuid");
}

static int member(gid_t g, gid_t primary, const cred *c) {
    if (g == primary) return 1;
    for (int i = 0; i < c->ng; i++)
        if (c->g[i] == g) return 1;
    return 0;
}

// The modelled IDs for one call: access(2) takes the real ones, privilege
// included; faccessat(AT_EACCESS) the effective ones.
static void standing(const cred *c, int effective, int *priv, int *owns, int *ing) {
    uid_t u = effective ? c->euid : c->ruid;
    gid_t g = effective ? c->egid : c->rgid;
    *priv = u == 0;
    *owns = u == OWNER;
    *ing = member(GROUP, g, c);
}

static void cred_body(const cred *c, const char *dir, int noexec) {
    for (int kind = 0; kind <= 2; kind++) {
        int mismatches = 0, rows = 0, noexec_rows = 0;
        int top = kind == 2 ? 0 : 07777;
        for (int mode = 0; mode <= top; mode++) {
            char t[PATH_MAX], nm[64];
            if (kind == 2) snprintf(nm, sizeof nm, "link");
            else snprintf(nm, sizeof nm, "%s_%04o", kind ? "d" : "f", mode);
            path(t, dir, nm);
            int effective_mode = kind == 2 ? 0777 : mode;
            for (int amode = 0; amode <= 7; amode++) {
                for (int eff = 0; eff <= 1; eff++) {
                    int priv, owns, ing;
                    standing(c, eff, &priv, &owns, &ing);
                    int want = predict(priv, owns, ing, kind, effective_mode, amode);
                    int flags = (eff ? AT_EACCESS : 0) | (kind == 2 ? AT_SYMLINK_NOFOLLOW : 0);
                    int got = !eff && kind != 2 ? kaccess(t, amode) : kfaccessat(AT_FDCWD, t, amode, flags);
                    // `noexec` refuses X_OK on a regular file to everyone; it
                    // is a mount option this library does not model.
                    if (noexec && kind == 0 && (amode & X_OK) && got == EACCES && want == 0) {
                        noexec_rows++;
                        continue;
                    }
                    rows++;
                    if (got != want) {
                        if (mismatches < 8)
                            p("CRED-MISMATCH\t%s\t%s\tmode=%04o\tamode=%d\t%s\tgot=%s\twant=%s\n", c->name, kinds[kind], effective_mode, amode,
                              eff ? "AT_EACCESS" : "access", en(got), en(want));
                        mismatches++;
                    }
                }
            }
        }
        p("CRED-SWEEP\t%s\t%s\trows=%d\tmismatches=%d\tnoexec-refusals=%d\n", c->name, kinds[kind], rows, mismatches, noexec_rows);
    }

    // The walk: a directory only one of the two IDs may search, holding a
    // file anyone may read. access(2) walks with the real IDs.
    char w[PATH_MAX];
    path(w, dir, "walk_owner_0700/f");
    p("WALK\t%s\twalk_owner_0700/f\taccess(F_OK)=%s\tAT_EACCESS(F_OK)=%s\n", c->name, en(kaccess(w, F_OK)),
      en(kfaccessat(AT_FDCWD, w, F_OK, AT_EACCESS)));
}

static void cred_sweep(const char *dir) {
    struct statvfs sv;
    int noexec = statvfs(dir, &sv) == 0 && (sv.f_flag & ST_NOEXEC);
    p("MOUNT\t%s\tnoexec=%d\n", dir, noexec);
    for (int kind = 0; kind <= 1; kind++)
        for (int mode = 0; mode <= 07777; mode++) {
            char t[PATH_MAX], nm[64];
            snprintf(nm, sizeof nm, "%s_%04o", kind ? "d" : "f", mode);
            path(t, dir, nm);
            if (kind) {
                if (mkdir(t, 0700) != 0) die("mkdir sweep");
            } else
                mkfile(t);
            if (lchown(t, OWNER, GROUP) != 0) die("chown sweep");
            if (chmod(t, mode) != 0) die("chmod sweep");
            if (mode_of(t) != mode) {
                p("SETUP-FAILED\t%s\twanted %04o got %04o\n", t, mode, mode_of(t));
                exit(2);
            }
        }
    char l[PATH_MAX];
    path(l, dir, "link");
    if (symlink("f_0000", l) != 0) die("symlink");
    if (lchown(l, OWNER, GROUP) != 0) die("lchown link");
    char w[PATH_MAX], wf[PATH_MAX];
    path(w, dir, "walk_owner_0700");
    if (mkdir(w, 0700) != 0) die("mkdir walk");
    path(wf, w, "f");
    mkfile(wf);
    if (chmod(wf, 0666) != 0) die("chmod walk/f");
    if (lchown(w, OWNER, GROUP) != 0) die("chown walk");

    for (unsigned i = 0; i < sizeof creds / sizeof creds[0]; i++) {
        fflush(stdout);
        pid_t pid = fork();
        if (pid < 0) die("fork");
        if (pid == 0) {
            alarm(300);
            become(&creds[i]);
            cred_body(&creds[i], dir, noexec);
            fflush(stdout);
            _exit(0);
        }
        int status;
        waitpid(pid, &status, 0);
        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) p("CHILD-FAILED\t%s\tstatus=%d\n", creds[i].name, status);
    }
}
#endif

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <fresh directory>\n", argv[0]);
        return 2;
    }
    alarm(900);
    struct utsname u;
    uname(&u);
    mkdir(argv[1], 0755);
    if (chmod(argv[1], 0755) != 0) die("chmod base");
    if (realpath(argv[1], base) == NULL) die("realpath");
    p("# access-rules: %s %s %s\tuid=%d euid=%d gid=%d egid=%d\tbase=%s\n", u.sysname, u.release, u.machine, (int)getuid(), (int)geteuid(),
      (int)getgid(), (int)getegid(), base);
    umask(022);
    if (chdir(base) != 0) die("chdir base");
    screens();
    paths();
    own_sweep();
#ifdef __APPLE__
    ignored_bits();
    foreign_scan();
#endif
#ifdef __linux__
    if (geteuid() == 0) {
        char d[PATH_MAX];
        path(d, base, "creds");
        if (mkdir(d, 0755) != 0) die("mkdir creds");
        cred_sweep(d);
    } else
        p("# not root: the credential sweep needs root\n");
#endif
    p("# done\n");
    return 0;
}
