// Measures chmod(2) and fchmod(2): who may change a mode, what happens to the
// bits asked for (the word's high bits, set-group-ID for a caller outside the
// file's group, the sticky bit on a non-directory), which timestamps move,
// how a path through a symbolic link resolves, and what fchmod answers on each
// descriptor kind.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/chmod-rules.c && /tmp/p /tmp/chmodp && /tmp/p /dev/shm/chmodp'
//         Run it as root: each standing row forks a child that takes the
//         credentials under test, against inodes root has chowned to owners and
//         groups the child is and is not a member of.
// Darwin: nix develop -c clang -Wall -o chmod-rules chmod-rules.c && ./chmod-rules "$(mktemp -d /private/tmp/chmodp.XXXXXX)"
//         As an ordinary user, which cannot chown to anyone else. The non-owner
//         rows chmod existing inodes owned by someone else to the mode they
//         already have, so nothing changes whatever the kernel answers. Edit
//         the FOREIGN_* paths below to inodes on the machine at hand.
//
// Measured on Linux 6.18.5 (aarch64, root in the container) and Darwin 27.0
// (arm64, uid 501) on 2026-09-27. The Linux output on tmpfs (/dev/shm) was
// identical to the ext4 output beside this file, line for line, once the base
// directory's name is set aside, except for one PIPE-TIMES row: on ext4's run
// the supplementary-group row's read-end fchmod left ctime where it was, and
// on tmpfs's it moved. chmod-pipe-ctime.c repeats that call 400 times, and
// ctime moved every time, so that one row is taken as a clock artefact of the
// VM rather than a rule. The rows are transcribed in
// WoofWare.PosixKernel.Test/TestModeChange.fs.
//
// Every sweep compares each row against the rule WoofWare.PosixKernel models
// and prints the mismatch count, and the first few mismatches.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static char base[PATH_MAX];
// A file the caller does not own, which it may open for reading.
static const char *foreign_file = NULL;

static void paths(void);
static void descriptors(void);

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
    case EOPNOTSUPP: return "EOPNOTSUPP";
#if defined(ENOTSUP) && ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
#ifdef EFTYPE
    case EFTYPE: return "EFTYPE";
#endif
    default: return strerror(e);
    }
}

static int R(int rc) { return rc == 0 ? 0 : errno; }

static void path(char *out, const char *dir, const char *name) { snprintf(out, PATH_MAX, "%s/%s", dir, name); }

static int lmode_of(const char *pth) {
    struct stat st;
    if (lstat(pth, &st) != 0) return -1;
    return st.st_mode & 07777;
}

static int fmode_of(int fd) {
    struct stat st;
    if (fstat(fd, &st) != 0) return -errno;
    return st.st_mode;
}

static void mkfile(const char *pth) {
    int fd = open(pth, O_CREAT | O_EXCL | O_WRONLY, 0600);
    if (fd < 0) die(pth);
    if (write(fd, "abcd", 4) != 4) die("write");
    close(fd);
}

static void mkthing(const char *pth, int isdir) {
    if (isdir) {
        if (mkdir(pth, 0700) != 0) die(pth);
    } else
        mkfile(pth);
}

static void settle(void) {
    // Long enough that a timestamp taken after it differs from one taken
    // before it on every filesystem this runs on.
    struct timespec ts = {0, 30 * 1000 * 1000};
    nanosleep(&ts, NULL);
}

#ifdef __APPLE__
#define ATIM(st) ((st).st_atimespec)
#define MTIM(st) ((st).st_mtimespec)
#define CTIM(st) ((st).st_ctimespec)
#else
#define ATIM(st) ((st).st_atim)
#define MTIM(st) ((st).st_mtim)
#define CTIM(st) ((st).st_ctim)
#endif

static int same(struct timespec a, struct timespec b) { return a.tv_sec == b.tv_sec && a.tv_nsec == b.tv_nsec; }

static const char *moved(const struct stat *before, const struct stat *after) {
    static char out[64];
    snprintf(out, sizeof out, "atime=%s mtime=%s ctime=%s", same(ATIM(*before), ATIM(*after)) ? "kept" : "moved",
             same(MTIM(*before), MTIM(*after)) ? "kept" : "moved", same(CTIM(*before), CTIM(*after)) ? "kept" : "moved");
    return out;
}

// The rule modelled, for an unprivileged caller: the owner may set any of the
// twelve bits except that S_ISGID is dropped when the caller is not in the
// file's group; anyone else is EPERM. A privileged caller (Linux only) sets
// all twelve. The mode word's bits above 07777 are ignored.
//
// Returns the errno predicted, and the resulting mode through *result.
static int predict(int privileged, int owns, int ingroup, int before, int requested, int *result) {
    int asked = requested & 07777;
    if (privileged) {
        *result = asked;
        return 0;
    }
    if (!owns) {
        *result = before;
        return EPERM;
    }
    *result = ingroup ? asked : asked & ~02000;
    return 0;
}

// ---------------------------------------------------------------- the mode word's high bits

static const unsigned highs[] = {0u,          010000u,  020000u,  030000u,  040000u,  050000u,  060000u,
                                 070000u,     0100000u, 0110000u, 0120000u, 0140000u, 0170000u, 0200000u,
                                 0x10000u,    0x20000u, 0x100000u, 0x40000000u, 0x80000000u, 0xFFFF0000u, 0xFFFFF000u};

// As the owner in the file's group: every low word, with each high pattern.
static void high_bits_sweep(void) {
    // In a directory of the caller's effective group, so that the inodes made
    // in it are in the caller's group on both flavours.
    char hdir[PATH_MAX];
    path(hdir, base, "high");
    if (mkdir(hdir, 0755) != 0) die(hdir);
    if (chown(hdir, (uid_t)-1, getegid()) != 0) die("chgrp high");
    for (int isdir = 0; isdir <= 1; isdir++) {
        char t[PATH_MAX];
        path(t, hdir, isdir ? "high_d" : "high_f");
        mkthing(t, isdir);
        int mismatches = 0, rows = 0;
        for (unsigned h = 0; h < sizeof highs / sizeof highs[0]; h++) {
            for (int low = 0; low <= 07777; low++) {
                // The argument as the shim receives it, a 32-bit int, handed to
                // chmod as mode_t: 16 bits on Darwin, 32 on Linux.
                int arg = (int)(highs[h] | (unsigned)low);
                chmod(t, 0700);
                int e = R(chmod(t, (mode_t)arg));
                int got = lmode_of(t);
                rows++;
                if (e != 0 || got != low) {
                    if (mismatches < 8) p("HIGH-MISMATCH\t%s\targ=0x%08x\t%s\tgot=%04o\n", isdir ? "dir" : "file", (unsigned)arg, en(e), got);
                    mismatches++;
                }
                if (!isdir && low == 0644) {
                    // fchmod with the same argument, through a descriptor.
                    int fd = open(t, O_RDONLY);
                    if (fd < 0) die("open high");
                    chmod(t, 0700);
                    int fe = R(fchmod(fd, (mode_t)arg));
                    int fgot = lmode_of(t);
                    close(fd);
                    if (fe != 0 || fgot != low) {
                        p("HIGH-MISMATCH\tfchmod\targ=0x%08x\t%s\tgot=%04o\n", (unsigned)arg, en(fe), fgot);
                        mismatches++;
                    }
                }
            }
        }
        chmod(t, 0700);
        p("HIGH-SWEEP\t%s\trows=%d\tmismatches=%d\n", isdir ? "dir" : "file", rows, mismatches);
    }
    // -1 on its own, which is every bit.
    char t[PATH_MAX];
    path(t, hdir, "high_f");
    chmod(t, 0600);
    int e = R(chmod(t, (mode_t)-1));
    p("HIGH-ALL-ONES\tfile\tchmod((mode_t)-1)\t%s\tafter=%04o\n", en(e), lmode_of(t));
    chmod(t, 0600);
}

// ---------------------------------------------------------------- standing, over all 4096 modes

typedef struct {
    const char *label;
    int privileged, owns, ingroup;
    uid_t ou;
    gid_t og;
} standing_row;

static void standing_body(const standing_row *row, const char *dir) {
    for (int isdir = 0; isdir <= 1; isdir++) {
        char t[PATH_MAX];
        path(t, dir, isdir ? "d" : "f");
        int start = isdir ? 0700 : 0600;
        int mismatches = 0;
        for (int mode = 0; mode <= 07777; mode++) {
            int before = lmode_of(t);
            int e = R(chmod(t, (mode_t)mode));
            int got = lmode_of(t);
            int want;
            int we = predict(row->privileged, row->owns, row->ingroup, before, mode, &want);
            if (e != we || got != want) {
                if (mismatches < 8) p("STANDING-MISMATCH\t%s\t%s\tmode=%04o\tgot=%s/%04o\twant=%s/%04o\n", row->label, isdir ? "dir" : "file", mode, en(e), got, en(we), want);
                mismatches++;
            }
            // Put the start mode back the way the owner or root would, so every
            // row starts from the same place. A non-owner cannot, and needs
            // nothing putting back.
            if (e == 0) chmod(t, (mode_t)start);
        }
        p("STANDING-SWEEP\t%s\t%s\tmodes=4096\tmismatches=%d\n", row->label, isdir ? "dir" : "file", mismatches);
    }
}

// The timestamps a chmod moves, as whoever this is: to a different mode, to
// the same mode, and (where predicted to be refused) a refused one.
static void timestamps_body(const standing_row *row, const char *dir) {
    for (int isdir = 0; isdir <= 1; isdir++) {
        char t[PATH_MAX];
        path(t, dir, isdir ? "d" : "f");
        int start = lmode_of(t);
        const int targets[] = {start == 0644 ? 0640 : 0644, -1, 02755};
        const char *names[] = {"different", "same", "02755"};
        for (int i = 0; i < 3; i++) {
            int target = targets[i] < 0 ? lmode_of(t) : targets[i];
            struct stat b, a;
            if (lstat(t, &b) != 0) die("lstat before");
            settle();
            int e = R(chmod(t, (mode_t)target));
            if (lstat(t, &a) != 0) die("lstat after");
            p("TIMES\t%s\t%s\tchmod(%s %04o)\t%s\tafter=%04o\t%s\n", row->label, isdir ? "dir" : "file", names[i], target, en(e), a.st_mode & 07777, moved(&b, &a));
        }
        // And fchmod, through a descriptor, to the same mode.
        int fd = open(t, O_RDONLY);
        if (fd >= 0) {
            struct stat b, a;
            fstat(fd, &b);
            settle();
            int e = R(fchmod(fd, b.st_mode & 07777));
            fstat(fd, &a);
            p("TIMES\t%s\t%s\tfchmod(same %04o)\t%s\tafter=%04o\t%s\n", row->label, isdir ? "dir" : "file", b.st_mode & 07777, en(e), a.st_mode & 07777, moved(&b, &a));
            close(fd);
        } else
            p("TIMES\t%s\t%s\tfchmod: open %s\n", row->label, isdir ? "dir" : "file", en(errno));
        chmod(t, (mode_t)start);
    }
}

#ifdef __linux__
// ---------------------------------------------------------------- credentials

typedef struct {
    const char *name;
    uid_t uid;
    gid_t gid;
    int ng;
    gid_t g[4];
} cred;

static const cred ROOT = {"root", 0, 0, 1, {0}};
static const cred U1000 = {"u1000(g1000;groups 1000,2000)", 1000, 1000, 2, {1000, 2000}};

static void become(const cred *c) {
    if (setgroups(c->ng, c->g) != 0) die("setgroups");
    if (setresgid(c->gid, c->gid, c->gid) != 0) die("setresgid");
    if (setresuid(c->uid, c->uid, c->uid) != 0) die("setresuid");
}

typedef void (*body)(const standing_row *, const char *);

static void as(const cred *c, body fn, const standing_row *row, const char *dir) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(300);
        become(c);
        fn(row, dir);
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) p("CHILD-FAILED\t%s\tstatus=%d\n", c->name, status);
}

// ---------------------------------------------------------------- pipes

// A pipe is one pipefs inode that both ends name, with an owner and a mode like
// any other; root hands it to the owner and group each row wants, and the row's
// caller then asks every mode of it, alternating the end it asks through.
static int pipe_fds[2];

static void pipe_body(const standing_row *row, const char *dir) {
    (void)dir;
    int mismatches = 0;
    for (int mode = 0; mode <= 07777; mode++) {
        struct stat b, r, w;
        if (fstat(pipe_fds[0], &b) != 0) die("fstat pipe");
        int before = b.st_mode & 07777;
        int e = R(fchmod(pipe_fds[mode & 1], (mode_t)mode));
        fstat(pipe_fds[0], &r);
        fstat(pipe_fds[1], &w);
        int want;
        int we = predict(row->privileged, row->owns, row->ingroup, before, mode, &want);
        int got = r.st_mode & 07777;
        if (e != we || got != want || (int)(w.st_mode & 07777) != got || r.st_ino != w.st_ino) {
            if (mismatches < 8) p("PIPE-MISMATCH\t%s\tmode=%04o\tgot=%s/%04o (write end %04o)\twant=%s/%04o\n", row->label, mode, en(e), got, w.st_mode & 07777, en(we), want);
            mismatches++;
        }
        if (e == 0) fchmod(pipe_fds[0], 0600);
    }
    p("PIPE-SWEEP\t%s\tmodes=4096\tmismatches=%d\n", row->label, mismatches);
    for (int end = 0; end <= 1; end++) {
        struct stat b, a, ob, oa;
        fstat(pipe_fds[end], &b);
        fstat(pipe_fds[1 - end], &ob);
        settle();
        int e = R(fchmod(pipe_fds[end], b.st_mode & 07777));
        fstat(pipe_fds[end], &a);
        fstat(pipe_fds[1 - end], &oa);
        p("PIPE-TIMES\t%s\tfchmod(%s end, same %04o)\t%s\tthat end: %s\tthe other end: %s\n", row->label, end ? "write" : "read", b.st_mode & 07777, en(e), moved(&b, &a), moved(&ob, &oa));
    }
}

static void pipes(void) {
    const standing_row rows[] = {
        {"pipe: owner, group = egid", 0, 1, 1, 1000, 1000},
        {"pipe: owner, group = supplementary", 0, 1, 1, 1000, 2000},
        {"pipe: owner, group not a member", 0, 1, 0, 1000, 3000},
        {"pipe: non-owner, group = egid", 0, 0, 1, 1001, 1000},
        {"pipe: non-owner, group not a member", 0, 0, 0, 1001, 3000},
        {"pipe: root, non-owner, group not a member", 1, 0, 0, 1001, 3000},
    };
    {
        // Who owns a pipe its creator made, and with which mode.
        int fds[2];
        if (pipe(fds) != 0) die("pipe");
        struct stat st;
        fstat(fds[0], &st);
        p("PIPE\tmade by uid=%d egid=%d\tuid=%d gid=%d mode=0%o\n", (int)geteuid(), (int)getegid(), (int)st.st_uid, (int)st.st_gid, st.st_mode);
        close(fds[0]);
        close(fds[1]);
    }
    for (unsigned i = 0; i < sizeof rows / sizeof rows[0]; i++) {
        if (pipe(pipe_fds) != 0) die("pipe");
        if (fchown(pipe_fds[0], rows[i].ou, rows[i].og) != 0) die("fchown pipe");
        if (fchmod(pipe_fds[1], 0600) != 0) die("fchmod pipe");
        as(rows[i].privileged ? &ROOT : &U1000, pipe_body, &rows[i], NULL);
        close(pipe_fds[0]);
        close(pipe_fds[1]);
    }
}

static void rest_body(const standing_row *row, const char *dir) {
    p("RUN\t%s\tuid=%d egid=%d\n", row->label, (int)geteuid(), (int)getegid());
    snprintf(base, sizeof base, "%s", dir);
    if (!row->privileged) high_bits_sweep();
    paths();
    descriptors();
}

static void prepare(const char *dir, const standing_row *row) {
    if (mkdir(dir, 0755) != 0) die(dir);
    char t[PATH_MAX];
    path(t, dir, "f");
    mkfile(t);
    if (chown(t, row->ou, row->og) != 0) die("chown f");
    if (chmod(t, 0600) != 0) die("chmod f");
    path(t, dir, "d");
    if (mkdir(t, 0700) != 0) die("mkdir d");
    if (chown(t, row->ou, row->og) != 0) die("chown d");
    if (chmod(t, 0700) != 0) die("chmod d");
}

static void standings(void) {
    // Unprivileged rows run as u1000 (egid 1000, supplementary 2000).
    const standing_row rows[] = {
        {"owner, group = egid", 0, 1, 1, 1000, 1000},
        {"owner, group = supplementary", 0, 1, 1, 1000, 2000},
        {"owner, group not a member", 0, 1, 0, 1000, 3000},
        {"non-owner, group = egid", 0, 0, 1, 1001, 1000},
        {"non-owner, group = supplementary", 0, 0, 1, 1001, 2000},
        {"non-owner, group not a member", 0, 0, 0, 1001, 3000},
        // Privileged rows run as root (egid 0, groups 0).
        {"root, owner, in group", 1, 1, 1, 0, 0},
        {"root, owner, group not a member", 1, 1, 0, 0, 3000},
        {"root, non-owner, in group", 1, 0, 1, 1001, 0},
        {"root, non-owner, group not a member", 1, 0, 0, 1001, 3000},
    };
    for (unsigned i = 0; i < sizeof rows / sizeof rows[0]; i++) {
        char dir[PATH_MAX], nm[32];
        snprintf(nm, sizeof nm, "standing%u", i);
        path(dir, base, nm);
        prepare(dir, &rows[i]);
        as(rows[i].privileged ? &ROOT : &U1000, standing_body, &rows[i], dir);
        char tdir[PATH_MAX];
        snprintf(nm, sizeof nm, "times%u", i);
        path(tdir, base, nm);
        prepare(tdir, &rows[i]);
        as(rows[i].privileged ? &ROOT : &U1000, timestamps_body, &rows[i], tdir);
    }
}
#else
// Darwin: the owner rows run on inodes this user made, in directories whose
// group decides the new inode's; the non-owner rows chmod someone else's
// inode to the mode it already has.
static const char *FOREIGN_INGROUP_FILE = "/Users/steam/.DS_Store";   // steam:staff 0644
static const char *FOREIGN_INGROUP_DIR = "/Users/steam";              // steam:staff 0755
static const char *FOREIGN_OTHER_FILE = "/etc/hosts";                 // root:wheel 0644
static const char *FOREIGN_OTHER_DIR = "/private/tmp";                // root:wheel 01777 (via /private)

static void foreign_row(const char *label, const char *pth) {
    struct stat b, a;
    if (stat(pth, &b) != 0) {
        p("FOREIGN\t%s\t%s\tstat %s\n", label, pth, en(errno));
        return;
    }
    settle();
    int e = R(chmod(pth, b.st_mode & 07777));
    stat(pth, &a);
    p("FOREIGN\t%s\t%s\tuid=%d gid=%d\tchmod(current %04o)\t%s\t%s\n", label, pth, (int)b.st_uid, (int)b.st_gid, b.st_mode & 07777, en(e), moved(&b, &a));
    int fd = open(pth, O_RDONLY);
    if (fd >= 0) {
        e = R(fchmod(fd, b.st_mode & 07777));
        p("FOREIGN\t%s\t%s\tfchmod(current %04o)\t%s\n", label, pth, b.st_mode & 07777, en(e));
        close(fd);
    } else
        p("FOREIGN\t%s\t%s\tfchmod: open %s\n", label, pth, en(errno));
}

static void standings(void) {
    // `base` is under /private/tmp, whose group is wheel, so every inode made
    // in it is wheel's: the owner outside its group.
    const standing_row out = {"owner, group not a member (wheel)", 0, 1, 0, 0, 0};
    const standing_row egid = {"owner, group = egid", 0, 1, 1, 0, 0};
    const standing_row supp = {"owner, group = supplementary 12", 0, 1, 1, 0, 0};
    const standing_row *rows[] = {&egid, &supp, &out};
    const gid_t groups[] = {getegid(), 12, (gid_t)-1};
    for (int i = 0; i < 3; i++) {
        for (int pass = 0; pass < 2; pass++) {
            char dir[PATH_MAX], nm[32];
            snprintf(nm, sizeof nm, "%s%d", pass ? "times" : "standing", i);
            path(dir, base, nm);
            if (mkdir(dir, 0755) != 0) die(dir);
            char f[PATH_MAX], d[PATH_MAX];
            path(f, dir, "f");
            path(d, dir, "d");
            mkfile(f);
            if (mkdir(d, 0700) != 0) die(d);
            if (groups[i] != (gid_t)-1) {
                if (chown(f, (uid_t)-1, groups[i]) != 0) die("chgrp f");
                if (chown(d, (uid_t)-1, groups[i]) != 0) die("chgrp d");
            }
            chmod(f, 0600);
            chmod(d, 0700);
            struct stat st;
            stat(f, &st);
            p("SETUP\t%s\tfile gid=%d\n", rows[i]->label, (int)st.st_gid);
            if (pass) timestamps_body(rows[i], dir);
            else standing_body(rows[i], dir);
        }
    }
    foreign_row("non-owner, group = egid", FOREIGN_INGROUP_FILE);
    foreign_row("non-owner, group = egid", FOREIGN_INGROUP_DIR);
    foreign_row("non-owner, group not a member", FOREIGN_OTHER_FILE);
    foreign_row("non-owner, group not a member", FOREIGN_OTHER_DIR);
}
#endif

// ---------------------------------------------------------------- paths and symbolic links

static void path_row(const char *label, const char *pth, int mode, const char *watch) {
    int before = watch ? lmode_of(watch) : -1;
    int e = R(chmod(pth, (mode_t)mode));
    p("PATH\t%s\tchmod(%04o)\t%s", label, mode, en(e));
    if (watch) p("\t%s: %04o -> %04o", watch + strlen(base) + 1, before, lmode_of(watch));
    p("\n");
}

static void paths(void) {
    char dir[PATH_MAX], f[PATH_MAX], d[PATH_MAX], lf[PATH_MAX], ld[PATH_MAX], dang[PATH_MAX], loop[PATH_MAX], a[PATH_MAX];
    path(dir, base, "paths");
    if (mkdir(dir, 0755) != 0) die(dir);
    path(f, dir, "f");
    mkfile(f);
    chmod(f, 0600);
    path(d, dir, "d");
    mkdir(d, 0700);
    path(lf, dir, "lf");
    if (symlink("f", lf) != 0) die("symlink lf");
    path(ld, dir, "ld");
    if (symlink("d", ld) != 0) die("symlink ld");
    path(dang, dir, "dang");
    if (symlink("nowhere", dang) != 0) die("symlink dang");
    path(loop, dir, "loop");
    if (symlink("loop", loop) != 0) die("symlink loop");

    int linkmode = lmode_of(lf);
    struct stat lb, la, tb, ta;
    lstat(lf, &lb);
    stat(lf, &tb);
    settle();
    path_row("a link to a file: the target changes", lf, 0640, f);
    lstat(lf, &la);
    stat(lf, &ta);
    p("PATH\tthe link itself\tmode %04o -> %04o\t%s\n", linkmode, la.st_mode & 07777, moved(&lb, &la));
    p("PATH\tthe target\t%s\n", moved(&tb, &ta));
    path_row("a link to a directory", ld, 0750, d);
    snprintf(a, sizeof a, "%s/", ld);
    path_row("a link to a directory, trailing separator", a, 0755, d);
    snprintf(a, sizeof a, "%s/", d);
    path_row("a directory, trailing separator", a, 0700, d);
    snprintf(a, sizeof a, "%s/", f);
    path_row("a file, trailing separator", a, 0644, f);
    snprintf(a, sizeof a, "%s/", lf);
    path_row("a link to a file, trailing separator", a, 0644, f);
    path_row("a dangling link", dang, 0644, NULL);
    path_row("a link to itself", loop, 0644, NULL);
    path(a, dir, "absent");
    path_row("an absent name", a, 0644, NULL);
    path_row("the empty path", "", 0644, NULL);
    path(a, f, "under");
    path_row("a file as a directory", a, 0644, NULL);
}

// ---------------------------------------------------------------- fchmod on each descriptor kind

static void fd_row(const char *label, int fd, int mode) {
    int before = fmode_of(fd);
    int e = R(fchmod(fd, (mode_t)mode));
    int after = fmode_of(fd);
    p("FCHMOD\t%s\tfchmod(%04o)\t%s\tst_mode 0%o -> 0%o\n", label, mode, en(e), before, after);
}

static void descriptors(void) {
    char dir[PATH_MAX], f[PATH_MAX];
    path(dir, base, "fds");
    if (mkdir(dir, 0755) != 0) die(dir);
    path(f, dir, "f");
    mkfile(f);
    chmod(f, 0600);

    int fd = open(f, O_RDONLY);
    fd_row("a file open O_RDONLY", fd, 0640);
    close(fd);
    fd = open(f, O_WRONLY);
    fd_row("a file open O_WRONLY", fd, 0600);
    close(fd);
    fd = open(f, O_RDWR);
    fd_row("a file open O_RDWR", fd, 0644);
    close(fd);
    fd = open(dir, O_RDONLY);
    fd_row("a directory", fd, 0750);
    fchmod(fd, 0755);
    close(fd);
    fd = open(f, O_RDONLY);
    unlink(f);
    fd_row("an unlinked file", fd, 0400);
    close(fd);
    fd = open(f, O_RDONLY);
    if (foreign_file) {
        fd = open(foreign_file, O_RDONLY);
        if (fd < 0) p("FCHMOD\ta file someone else owns\topen: %s\n", en(errno));
        else {
            fd_row("a file someone else owns, open O_RDONLY, to its own mode", fd, fmode_of(fd) & 07777);
            close(fd);
        }
    }
    p("FCHMOD\ta closed descriptor\tfchmod(0644)\t%s\n", en(R(fchmod(1000, 0644))));
    p("FCHMOD\tdescriptor -1\tfchmod(0644)\t%s\n", en(R(fchmod(-1, 0644))));

    int pfd[2];
    if (pipe(pfd) != 0) die("pipe");
    fd_row("a pipe's read end", pfd[0], 0600);
    fd_row("a pipe's write end", pfd[1], 02750);
    close(pfd[0]);
    close(pfd[1]);

    const struct {
        const char *label;
        int domain, type;
    } socks[] = {
        {"an AF_INET stream socket", AF_INET, SOCK_STREAM},
        {"an AF_INET datagram socket", AF_INET, SOCK_DGRAM},
        {"an AF_INET6 stream socket", AF_INET6, SOCK_STREAM},
        {"an AF_UNIX stream socket", AF_UNIX, SOCK_STREAM},
    };
    for (unsigned i = 0; i < sizeof socks / sizeof socks[0]; i++) {
        int s = socket(socks[i].domain, socks[i].type, 0);
        if (s < 0) {
            p("FCHMOD\t%s\tsocket: %s\n", socks[i].label, en(errno));
            continue;
        }
        fd_row(socks[i].label, s, 0600);
        fd_row(socks[i].label, s, 02750);
        close(s);
    }
    {
        // A listening TCP socket, bound to loopback.
        int s = socket(AF_INET, SOCK_STREAM, 0);
        struct sockaddr_in sa;
        memset(&sa, 0, sizeof sa);
        sa.sin_family = AF_INET;
        sa.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        if (bind(s, (struct sockaddr *)&sa, sizeof sa) == 0 && listen(s, 1) == 0) fd_row("a listening AF_INET socket", s, 0600);
        close(s);
    }
#ifdef __linux__
    int ep = epoll_create1(0);
    fd_row("an epoll instance", ep, 0600);
    fd_row("an epoll instance", ep, 0644);
    close(ep);
#else
    int kq = kqueue();
    fd_row("a kqueue", kq, 0600);
    fd_row("a kqueue", kq, 0644);
    close(kq);
#endif
}

int main(int argc, char **argv) {
    alarm(1200);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <scratch directory>\n", argv[0]);
        return 2;
    }
    snprintf(base, sizeof base, "%s", argv[1]);
    mkdir(base, 0755);
    chmod(base, 0755);
    umask(022);
    p("RUN\tuid=%d gid=%d\tbase=%s\n", (int)getuid(), (int)getgid(), base);
#ifdef __linux__
    // The unprivileged rows run as u1000 in a directory it owns; the same
    // paths and descriptors then run again as root.
    char foreign[PATH_MAX];
    path(foreign, base, "rootfile");
    mkfile(foreign);
    chmod(foreign, 0644);
    foreign_file = foreign;
    char mine[PATH_MAX];
    path(mine, base, "u1000");
    if (mkdir(mine, 0755) != 0 || chown(mine, 1000, 1000) != 0) die("mkdir u1000");
    const standing_row unprivileged = {"u1000", 0, 1, 1, 1000, 1000};
    as(&U1000, rest_body, &unprivileged, mine);
    standings();
    pipes();
    char rootdir[PATH_MAX];
    path(rootdir, base, "root");
    if (mkdir(rootdir, 0755) != 0) die("mkdir root");
    p("RUN\tas root\n");
    foreign_file = NULL;
    const standing_row privileged = {"root", 1, 1, 1, 0, 0};
    rest_body(&privileged, rootdir);
#else
    foreign_file = "/etc/hosts";
    high_bits_sweep();
    standings();
    // The rest in a directory of this user's effective group.
    char mine[PATH_MAX];
    path(mine, base, "mine");
    if (mkdir(mine, 0755) != 0 || chown(mine, (uid_t)-1, getegid()) != 0) die("mkdir mine");
    snprintf(base, sizeof base, "%s", mine);
    paths();
    descriptors();
#endif
    return 0;
}
