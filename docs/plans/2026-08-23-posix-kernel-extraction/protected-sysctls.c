// Measures Linux's fs.protected_symlinks, fs.protected_regular and
// fs.protected_fifos (and which values fs.protected_hardlinks admits): which
// symbolic links a caller may follow, and which
// existing files and FIFOs an O_CREAT open may land on, in a sticky directory,
// for each value of each knob, each combination of the directory's sticky,
// group-write and other-write bits, and each relationship between the
// directory's owner, the object's owner and the caller (root included).
//
// Linux: run as root.
//
//   container run --rm --cap-add ALL -v "$PWD":/probe gcc:14 sh -c 'mount -o remount,rw /proc/sys && gcc -Wall -O1 -o /tmp/p /probe/protected-sysctls.c && /tmp/p /tmp/protp && /tmp/p /dev/shm/protp'
//
// `/proc/sys` is mounted read-only in the container, and remounting it needs
// CAP_SYS_ADMIN, which `container run` grants only under `--cap-add ALL`. Run
// as root: the probe writes each knob, then forks a child per caller, which
// drops to the caller's uid with setresuid before it makes any call. The knobs
// are restored to the values read at start before the probe exits.
//
// Each line is one child's answers. Every answer is compared against the rule
// WoofWare.PosixKernel models (`predict_*` below), and each part ends with a
// SWEEP line counting the mismatches. The rows are replayed in
// WoofWare.PosixKernel.Test/TestProtectedFiles.fs, which embeds this output.
//
// Darwin: nix develop -c clang -Wall -o protected-sysctls protected-sysctls.c && ./protected-sysctls "$(mktemp -d /private/tmp/protp.XXXXXX)" <root's symlink> <root's file>
//
// Darwin has no such sysctls (`sysctl -a` names no `protected_*`), so there is
// nothing to set; what is measured is that it applies none of these rules. An
// ordinary user cannot make an inode another user owns, so the two inodes
// under test are hard links, in a sticky directory the caller owns, to a
// symbolic link and a regular file root owns on the same volume (as
// permission-standing.c does). Edit the arguments to inodes on the machine at
// hand: /private/tmp/FTABHarvest/centauri-symlink-ftab.bin and
// /private/tmp/.AppleMiniSetupDidRun were used.
//
// The ADMITS row for protected_hardlinks was measured on 2026-10-04 and added
// to both Linux outputs; that rerun's other rows matched them, but for one
// ORDER row the dentry cache decides (see below).
//
// Measured on Linux 6.18.5 (aarch64, root in the container) on ext4 (/tmp)
// and tmpfs (/dev/shm), and on Darwin 27.0 (arm64, uid 501), on 2026-10-01;
// the output of each is beside this file. Every sweep reported no mismatch.
// The two Linux outputs are identical once the base directory is set aside,
// except in the ORDER rows whose answer the kernel's cache decides: with the
// caches dropped, a walk refused at the 21st to 40th link was EACCES on ext4
// and mostly ELOOP on tmpfs, whose dentries dropping the caches does not drop
// (which of those rows were ELOOP there changed from run to run).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>

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
    switch (e) {
    case 0: return "ok";
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case ELOOP: return "ELOOP";
    case EEXIST: return "EEXIST";
    case EINVAL: return "EINVAL";
    case -1: return "-";
    default: return strerror(e);
    }
}

static int R(int rc) { return rc >= 0 ? 0 : errno; }

static int Rclose(int fd) {
    if (fd < 0) return errno;
    close(fd);
    return 0;
}

static void ensure(int rc, const char *what) {
    if (rc != 0) die(what);
}

#ifdef __linux__
#define NOPS 32

static char base[PATH_MAX];
// Shared with a forked child: the errno each of its calls answered.
static int *results;

// ------------------------------------------------------------ the sysctls

static const char *knob_path(const char *name) {
    static char out[128];
    snprintf(out, sizeof out, "/proc/sys/fs/%s", name);
    return out;
}

static int read_knob(const char *name) {
    FILE *f = fopen(knob_path(name), "r");
    if (!f) die(knob_path(name));
    int v;
    if (fscanf(f, "%d", &v) != 1) die("fscanf");
    fclose(f);
    return v;
}

// The errno writing `value` answers, or 0.
static int write_knob(const char *name, const char *value) {
    int fd = open(knob_path(name), O_WRONLY);
    if (fd < 0) return errno;
    ssize_t n = write(fd, value, strlen(value));
    int e = n < 0 ? errno : 0;
    close(fd);
    return e;
}

static void set_knob(const char *name, int value) {
    char buf[16];
    snprintf(buf, sizeof buf, "%d", value);
    int e = write_knob(name, buf);
    if (e != 0) {
        errno = e;
        die(knob_path(name));
    }
    if (read_knob(name) != value) {
        fprintf(stderr, "%s did not read back %d\n", name, value);
        exit(2);
    }
}

// ------------------------------------------------------------ helpers

static const unsigned users[] = {0, 1000, 1001, 1002};
#define NUSERS 4
// Every caller is in group 100 alone, and every inode is in group 100: the
// rules measured here read only user IDs, which the group-write rows confirm
// by letting a caller that is in the directory's group meet them.
#define GROUP 100

static void become(unsigned uid) {
    gid_t g = GROUP;
    ensure(setgroups(1, &g), "setgroups");
    ensure(setresgid(GROUP, GROUP, GROUP), "setresgid");
    ensure(setresuid(uid, uid, uid), "setresuid");
}

static void path(char *out, const char *dir, const char *name) { snprintf(out, PATH_MAX, "%s/%s", dir, name); }

static void mkfile(const char *pth, unsigned owner, int mode) {
    int fd = open(pth, O_CREAT | O_EXCL | O_WRONLY, 0600);
    if (fd < 0) die(pth);
    if (write(fd, "abcd", 4) != 4) die("write");
    close(fd);
    ensure(chown(pth, owner, GROUP), "chown file");
    ensure(chmod(pth, mode), "chmod file");
}

static void mklink(const char *target, const char *pth, unsigned owner) {
    ensure(symlink(target, pth), pth);
    ensure(lchown(pth, owner, GROUP), "lchown");
}

static void mkdir_owned(const char *pth, unsigned owner, int mode) {
    ensure(mkdir(pth, 0700), pth);
    ensure(chown(pth, owner, GROUP), "chown dir");
    ensure(chmod(pth, mode), "chmod dir");
}

static off_t size_of(const char *pth) {
    struct stat st;
    if (stat(pth, &st) != 0) return -1;
    return st.st_size;
}

// The directory modes swept: 0755 with every combination of the sticky bit,
// group write and other write.
static int dir_mode(int i) { return 0755 | ((i & 1) ? 01000 : 0) | ((i & 2) ? 020 : 0) | ((i & 4) ? 002 : 0); }

// Run `body` in a child that has become `uid`; the child records into
// `results`, which is cleared to -1 (not asked) first.
static void in_child(unsigned uid, void (*body)(void *), void *arg) {
    for (int i = 0; i < NOPS; i++) results[i] = -1;
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        become(uid);
        body(arg);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) != pid) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        fprintf(stderr, "child for uid %u failed (status %d)\n", uid, status);
        exit(2);
    }
}

// ------------------------------------------------------------ protected_symlinks

// The operations each child performs, in order, against links in the sticky
// directory under test `s` (owner D, mode M), each link owned by L:
//   lf    -> T/file      (an absolute link to a file in an ordinary directory)
//   ld    -> T/dir       (to a directory holding `x`)
//   ldang -> C/<unique>  (dangling; C is a 0777 directory, so creating there is permitted)
//   lcyc  -> lcyc        (a link to itself)
// and V/<unique> -> s/lf, a root-owned link in an ordinary directory whose
// target is the link under test.
enum {
    S_STAT,        // stat("s/lf"): the final link, followed
    S_LSTAT,       // lstat("s/lf"): not followed
    S_OPEN,        // open("s/lf", O_RDONLY)
    S_OPEN_NOFOL,  // open("s/lf", O_RDONLY|O_NOFOLLOW)
    S_READLINK,    // readlink("s/lf")
    S_ACCESS,      // access("s/lf", R_OK)
    S_ACCESS_NOFOL, // faccessat("s/lf", F_OK, AT_SYMLINK_NOFOLLOW)
    S_CHOWN,       // chown("s/lf", -1, -1)
    S_LCHOWN,      // lchown("s/lf", -1, -1)
    S_DIR_SLASH,   // stat("s/ld/"): a trailing separator makes the link final and followed
    S_DIR_DOT,     // stat("s/ld/."): the link is interior
    S_DIR_CHILD,   // stat("s/ld/x"): the link is interior
    S_CHDIR,       // chdir("s/ld")
    S_OPENDIR,     // open("s/ld", O_RDONLY|O_DIRECTORY)
    S_DANG_STAT,   // stat("s/ldang"): a dangling final link
    S_DANG_CREAT,  // open("s/ldang", O_CREAT|O_WRONLY): creates the target if followed
    S_DANG_CREATED, // whether that created the target (0 if not, EEXIST if so)
    S_CYC_STAT,    // stat("s/lcyc"): ELOOP if followed to the end
    S_VIA,         // stat("V/<unique>"): the link under test, reached as the final link of a chain
    S_UNLINK,      // unlink("s/lf") -- last, since it may remove the link
    S_NOPS
};

static const char *s_names[] = {
    "stat", "lstat", "open", "open-nofollow", "readlink", "access", "access-nofollow", "chown", "lchown", "dir-slash", "dir-dot",
    "dir-child", "chdir", "opendir", "dangling-stat", "dangling-creat", "dangling-created", "cycle-stat", "via-chain", "unlink",
};

typedef struct {
    char s[PATH_MAX], v[PATH_MAX], dangtarget[PATH_MAX];
} s_paths;

static void s_body(void *arg) {
    s_paths *ps = arg;
    char pth[PATH_MAX + 32];
    struct stat st;
    char buf[PATH_MAX];
    snprintf(pth, sizeof pth, "%s/lf", ps->s);
    results[S_STAT] = R(stat(pth, &st));
    results[S_LSTAT] = R(lstat(pth, &st));
    results[S_OPEN] = Rclose(open(pth, O_RDONLY));
    results[S_OPEN_NOFOL] = Rclose(open(pth, O_RDONLY | O_NOFOLLOW));
    results[S_READLINK] = R((int)readlink(pth, buf, sizeof buf));
    results[S_ACCESS] = R(access(pth, R_OK));
    results[S_ACCESS_NOFOL] = R(faccessat(AT_FDCWD, pth, F_OK, AT_SYMLINK_NOFOLLOW));
    results[S_CHOWN] = R(chown(pth, (uid_t)-1, (gid_t)-1));
    results[S_LCHOWN] = R(lchown(pth, (uid_t)-1, (gid_t)-1));
    snprintf(pth, sizeof pth, "%s/ld/", ps->s);
    results[S_DIR_SLASH] = R(stat(pth, &st));
    snprintf(pth, sizeof pth, "%s/ld/.", ps->s);
    results[S_DIR_DOT] = R(stat(pth, &st));
    snprintf(pth, sizeof pth, "%s/ld/x", ps->s);
    results[S_DIR_CHILD] = R(stat(pth, &st));
    snprintf(pth, sizeof pth, "%s/ld", ps->s);
    results[S_CHDIR] = R(chdir(pth));
    results[S_OPENDIR] = Rclose(open(pth, O_RDONLY | O_DIRECTORY));
    snprintf(pth, sizeof pth, "%s/ldang", ps->s);
    results[S_DANG_STAT] = R(stat(pth, &st));
    results[S_DANG_CREAT] = Rclose(open(pth, O_CREAT | O_WRONLY, 0644));
    results[S_DANG_CREATED] = lstat(ps->dangtarget, &st) == 0 ? EEXIST : 0;
    snprintf(pth, sizeof pth, "%s/lcyc", ps->s);
    results[S_CYC_STAT] = R(stat(pth, &st));
    results[S_VIA] = R(stat(ps->v, &st));
    snprintf(pth, sizeof pth, "%s/lf", ps->s);
    results[S_UNLINK] = R(unlink(pth));
}

static int s_follows(int op) {
    switch (op) {
    case S_STAT:
    case S_OPEN:
    case S_ACCESS:
    case S_CHOWN:
    case S_DIR_SLASH:
    case S_CHDIR:
    case S_OPENDIR:
    case S_DANG_STAT:
    case S_DANG_CREAT:
    case S_CYC_STAT:
    case S_VIA:
        return 1;
    default:
        return 0;
    }
}

// The rule modelled: following a link in the final position of a walk is
// EACCES when the knob is 1, the directory holding the link is sticky and
// other-writable, and the link is owned by neither the follower nor the
// directory's owner. Root is not exempt. Otherwise each call answers what it
// would answer anyway.
static int predict_s(int knob, int mode, unsigned d, unsigned l, unsigned f, int op) {
    int refused = knob == 1 && (mode & 01002) == 01002 && l != f && l != d;
    if (refused && s_follows(op)) return EACCES;
    switch (op) {
    case S_OPEN_NOFOL: return ELOOP;
    case S_DANG_STAT: return ENOENT;
    case S_DANG_CREATED: return refused ? 0 : EEXIST;
    case S_CYC_STAT: return ELOOP;
    case S_UNLINK:
        // The directory's write bit (every caller is in the directory's
        // group, so a non-owner reads the group triple), then the sticky bit's
        // own rule. Root is exempt from both.
        if (f == 0) return 0;
        if (!(f == d ? (mode & 0200) : (mode & 020))) return EACCES;
        if ((mode & 01000) && f != d && f != l) return EPERM;
        return 0;
    default: return 0;
    }
}

static int part_symlinks(void) {
    int mismatches = 0, rows = 0;
    char t[PATH_MAX], c[PATH_MAX], v[PATH_MAX], tf[PATH_MAX], td[PATH_MAX], tx[PATH_MAX];
    path(t, base, "t");
    path(c, base, "c");
    path(v, base, "v");
    mkdir_owned(t, 0, 0755);
    mkdir_owned(c, 0, 0777);
    mkdir_owned(v, 0, 0755);
    path(tf, t, "file");
    path(td, t, "dir");
    mkfile(tf, 0, 0644);
    mkdir_owned(td, 0, 0755);
    path(tx, td, "x");
    mkfile(tx, 0, 0644);

    for (int knob = 0; knob <= 1; knob++) {
        set_knob("protected_symlinks", knob);
        for (int mi = 0; mi < 8; mi++)
            for (int di = 0; di < 3; di++)
                for (int li = 0; li < NUSERS; li++)
                    for (int fi = 0; fi < NUSERS; fi++) {
                        int mode = dir_mode(mi);
                        unsigned d = users[di], l = users[li], f = users[fi];
                        s_paths ps;
                        char name[64], pth[PATH_MAX + 32];
                        snprintf(name, sizeof name, "s-%d-%04o-%u-%u-%u", knob, mode, d, l, f);
                        path(ps.s, base, name);
                        path(ps.v, v, name);
                        path(ps.dangtarget, c, name);
                        mkdir_owned(ps.s, d, mode);
                        snprintf(pth, sizeof pth, "%s/lf", ps.s);
                        mklink(tf, pth, l);
                        mklink(pth, ps.v, 0);
                        snprintf(pth, sizeof pth, "%s/ld", ps.s);
                        mklink(td, pth, l);
                        snprintf(pth, sizeof pth, "%s/ldang", ps.s);
                        mklink(ps.dangtarget, pth, l);
                        snprintf(pth, sizeof pth, "%s/lcyc", ps.s);
                        mklink("lcyc", pth, l);
                        in_child(f, s_body, &ps);
                        p("SYMLINKS\tknob=%d\tdir=%04o\tdirowner=%u\tlinkowner=%u\tcaller=%u", knob, mode, d, l, f);
                        int bad = 0;
                        for (int op = 0; op < S_NOPS; op++) {
                            int want = predict_s(knob, mode, d, l, f, op);
                            p("\t%s=%s", s_names[op], en(results[op]));
                            if (results[op] != want) bad++;
                        }
                        p("%s\n", bad ? "\tMISMATCH" : "");
                        mismatches += bad;
                        rows++;
                        unlink(ps.dangtarget);
                    }
    }
    p("SWEEP\tsymlinks\trows=%d\tmismatches=%d\n", rows, mismatches);
    return mismatches;
}

// access(2) judges with the real IDs and faccessat(AT_EACCESS) with the
// effective ones; which of them does the link check use?
static int part_access_ids(void) {
    int mismatches = 0;
    set_knob("protected_symlinks", 1);
    char s[PATH_MAX], lf[PATH_MAX], tf[PATH_MAX], t[PATH_MAX];
    path(s, base, "ids");
    path(t, base, "t");
    path(tf, t, "file");
    mkdir_owned(s, 0, 01777);
    path(lf, s, "lf");
    mklink(tf, lf, 1000);
    static const unsigned pairs[][2] = {{1000, 1001}, {1001, 1000}, {0, 1001}, {1001, 0}};
    for (int i = 0; i < 4; i++) {
        unsigned real = pairs[i][0], eff = pairs[i][1];
        pid_t pid = fork();
        if (pid < 0) die("fork");
        if (pid == 0) {
            gid_t g = GROUP;
            ensure(setgroups(1, &g), "setgroups");
            ensure(setresgid(GROUP, GROUP, GROUP), "setresgid");
            ensure(setresuid(real, eff, eff), "setresuid");
            struct stat st;
            results[0] = R(access(lf, R_OK));
            results[1] = R(faccessat(AT_FDCWD, lf, R_OK, AT_EACCESS));
            results[2] = R(stat(lf, &st));
            _exit(0);
        }
        int status;
        if (waitpid(pid, &status, 0) != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) die("child");
        // The link (owned by 1000) in a root-owned 01777 directory is followed
        // only by uid 1000: by the real uid for access, the effective uid
        // otherwise.
        int want_access = real == 1000 ? 0 : EACCES, want_eaccess = eff == 1000 ? 0 : EACCES;
        int bad = (results[0] != want_access) + (results[1] != want_eaccess) + (results[2] != want_eaccess);
        p("IDS\treal=%u\teffective=%u\taccess=%s\taccess-eaccess=%s\tstat=%s%s\n", real, eff, en(results[0]), en(results[1]),
          en(results[2]), bad ? "\tMISMATCH" : "");
        mismatches += bad;
    }
    p("SWEEP\taccess-ids\trows=4\tmismatches=%d\n", mismatches);
    return mismatches;
}

// Where the traversal limit (40) falls when the knob refuses a link: a chain
// of `n` root-owned links in an ordinary directory, the last naming a link
// owned by 1000 in a root-owned 01777 directory, so that the protected link is
// traversal n + 1. Each chain is followed by 1000 (whom the knob lets
// through), and by 1001 (whom it refuses) twice: once with the walk's dentries
// cached, as they are after the links were made, and once after the kernel's
// caches were dropped.
//
// The two refusals can differ, and what differs between them is only the
// kernel's cache. A walk that meets the refusal while it holds no references (the RCU
// walk, which a warm cache allows) starts again holding them, still carrying
// the links it counted, so each link counts twice; a walk that had already
// taken references (a cold cache makes it) refuses at once.
static void drop_caches(void) {
    sync();
    int fd = open("/proc/sys/vm/drop_caches", O_WRONLY);
    if (fd < 0) die("drop_caches");
    if (write(fd, "3", 1) != 1) die("write drop_caches");
    close(fd);
}

static int part_order(void) {
    int mismatches = 0, rows = 0;
    set_knob("protected_symlinks", 1);
    char s[PATH_MAX], lf[PATH_MAX], t[PATH_MAX], tf[PATH_MAX];
    path(s, base, "ord");
    path(t, base, "t");
    path(tf, t, "file");
    mkdir_owned(s, 0, 01777);
    path(lf, s, "lf");
    mklink(tf, lf, 1000);
    static const int chains[] = {0, 1, 18, 19, 20, 21, 38, 39, 40};
    for (int ni = 0; ni < (int)(sizeof chains / sizeof chains[0]); ni++) {
        int n = chains[ni];
        char chain[PATH_MAX + 16], link[PATH_MAX + 32], target[PATH_MAX + 32];
        snprintf(chain, sizeof chain, "%s/chain%02d", base, n);
        mkdir_owned(chain, 0, 0755);
        for (int i = 1; i <= n; i++) {
            snprintf(link, sizeof link, "%s/c%d", chain, i);
            if (i == n)
                snprintf(target, sizeof target, "%s", lf);
            else
                snprintf(target, sizeof target, "c%d", i + 1);
            mklink(target, link, 0);
        }
        if (n == 0)
            snprintf(link, sizeof link, "%s", lf);
        else
            snprintf(link, sizeof link, "%s/c1", chain);
        static const unsigned callers[] = {1000, 1001, 1001};
        static const char *caches[] = {"warm", "warm", "dropped"};
        for (int ci = 0; ci < 3; ci++) {
            unsigned f = callers[ci];
            int cold = ci == 2;
            if (cold) drop_caches();
            pid_t pid = fork();
            if (pid < 0) die("fork");
            if (pid == 0) {
                become(f);
                struct stat st;
                results[0] = R(stat(link, &st));
                _exit(0);
            }
            int status;
            if (waitpid(pid, &status, 0) != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) die("child");
            // The protected link is traversal k = n + 1. Past the limit it is
            // ELOOP for everyone. Within it, the knob lets 1000 through and
            // refuses 1001, unless the walk started again with each link
            // counted twice, which runs out at the protected link once
            // 2k - 1 >= 40.
            //
            // Whether dropping the caches makes the walk start with no
            // references depends on the filesystem: tmpfs keeps its dentries
            // (they are its storage), so there the walk is as warm as before.
            // In that window either answer is the kernel's.
            int k = n + 1, want, either = 0;
            if (k > 40)
                want = ELOOP;
            else if (f == 1000)
                want = 0;
            else if (2 * k - 1 >= 40) {
                want = ELOOP;
                either = cold;
            } else
                want = EACCES;
            int bad = either ? (results[0] != EACCES && results[0] != ELOOP) : results[0] != want;
            p("ORDER\tchain=%d\tcaches=%s\tcaller=%u\tstat=%s%s\n", n, caches[ci], f, en(results[0]), bad ? "\tMISMATCH" : "");
            mismatches += bad;
            rows++;
        }
    }
    p("SWEEP\torder\trows=%d\tmismatches=%d\n", rows, mismatches);
    return mismatches;
}

// ------------------------------------------------------------ protected_regular and protected_fifos

// The operations against an existing object `o` (owner I, mode 0666) in the
// sticky directory under test `s` (owner D, mode M):
enum {
    R_OPEN,         // open("s/o", O_RDONLY)
    R_CREAT,        // open("s/o", O_CREAT|O_RDONLY)
    R_CREAT_RDWR,   // open("s/o", O_CREAT|O_RDWR)
    R_CREAT_TRUNC,  // open("s/o", O_CREAT|O_WRONLY|O_TRUNC)
    R_TRUNCATED,    // whether that emptied the file (EEXIST if its four bytes went, 0 if not)
    R_EXCL,         // open("s/o", O_CREAT|O_EXCL|O_RDONLY)
    R_VIA,          // open("V/<unique>", O_CREAT|O_RDONLY): through a root-owned link in an ordinary directory
    R_LINK_NOFOL,   // open("s/l", O_CREAT|O_NOFOLLOW|O_RDONLY), where l is a link owned by I
    R_DIR,          // open("s/sub", O_CREAT|O_RDONLY), where sub is a directory owned by I
    R_NOPS
};

static const char *r_names[] = {
    "open", "creat", "creat-rdwr", "creat-trunc", "truncated", "excl", "via-link", "link-nofollow", "directory",
};

typedef struct {
    char s[PATH_MAX], v[PATH_MAX];
    int fifo;
} r_paths;

static void r_body(void *arg) {
    r_paths *ps = arg;
    char o[PATH_MAX + 8], pth[PATH_MAX + 8];
    snprintf(o, sizeof o, "%s/o", ps->s);
    // A FIFO opened for reading alone would wait for a writer.
    int rd = ps->fifo ? O_RDONLY | O_NONBLOCK : O_RDONLY;
    results[R_OPEN] = Rclose(open(o, rd));
    results[R_CREAT] = Rclose(open(o, O_CREAT | rd, 0644));
    results[R_CREAT_RDWR] = Rclose(open(o, O_CREAT | O_RDWR, 0644));
    if (!ps->fifo) {
        results[R_CREAT_TRUNC] = Rclose(open(o, O_CREAT | O_WRONLY | O_TRUNC, 0644));
        results[R_TRUNCATED] = size_of(o) == 0 ? EEXIST : 0;
    }
    results[R_EXCL] = Rclose(open(o, O_CREAT | O_EXCL | rd, 0644));
    results[R_VIA] = Rclose(open(ps->v, O_CREAT | rd, 0644));
    snprintf(pth, sizeof pth, "%s/l", ps->s);
    results[R_LINK_NOFOL] = Rclose(open(pth, O_CREAT | O_NOFOLLOW | O_RDONLY, 0644));
    snprintf(pth, sizeof pth, "%s/sub", ps->s);
    results[R_DIR] = Rclose(open(pth, O_CREAT | O_RDONLY, 0644));
}

// The rule modelled, for an existing object whose kind the knob governs: an
// O_CREAT open is EACCES when the directory is sticky, the object is owned by
// neither the directory's owner nor the caller, and either the directory is
// other-writable and the knob is at least 1, or it is group-writable and the
// knob is 2. Root is not exempt. O_EXCL's EEXIST and a directory's EISDIR
// come first.
static int sticky_create_refused(int knob, int mode, unsigned d, unsigned owner, unsigned caller) {
    if (!(mode & 01000) || owner == d || owner == caller) return 0;
    if (knob >= 1 && (mode & 002)) return 1;
    if (knob >= 2 && (mode & 020)) return 1;
    return 0;
}

static int predict_r(int knob, int mode, unsigned d, unsigned i, unsigned c, int fifo, int op) {
    int refused = sticky_create_refused(knob, mode, d, i, c);
    switch (op) {
    case R_OPEN: return 0;
    case R_CREAT:
    case R_CREAT_RDWR:
    case R_CREAT_TRUNC:
    case R_VIA:
        if (fifo && op == R_CREAT_TRUNC) return -1;
        return refused ? EACCES : 0;
    case R_TRUNCATED:
        if (fifo) return -1;
        return refused ? 0 : EEXIST;
    case R_EXCL: return EEXIST;
    // Neither a regular file nor a FIFO, so no knob governs it: the check
    // applies whatever the knobs say, but only where the directory is
    // other-writable, and then ELOOP for a link not followed.
    case R_LINK_NOFOL:
        return ((mode & 01002) == 01002 && i != d && i != c) ? EACCES : ELOOP;
    case R_DIR: return EISDIR;
    }
    return -2;
}

static int part_create(int fifo) {
    const char *knob_name = fifo ? "protected_fifos" : "protected_regular";
    int mismatches = 0, rows = 0;
    char v[PATH_MAX];
    path(v, base, fifo ? "vf" : "vr");
    mkdir_owned(v, 0, 0755);
    for (int knob = 0; knob <= 2; knob++) {
        set_knob(knob_name, knob);
        for (int mi = 0; mi < 8; mi++)
            for (int di = 0; di < 3; di++)
                for (int ii = 0; ii < NUSERS; ii++)
                    for (int ci = 0; ci < NUSERS; ci++) {
                        int mode = dir_mode(mi);
                        unsigned d = users[di], i = users[ii], c = users[ci];
                        r_paths ps;
                        ps.fifo = fifo;
                        char name[64], o[PATH_MAX + 8], pth[PATH_MAX + 8];
                        snprintf(name, sizeof name, "%s-%d-%04o-%u-%u-%u", fifo ? "f" : "r", knob, mode, d, i, c);
                        path(ps.s, base, name);
                        path(ps.v, v, name);
                        mkdir_owned(ps.s, d, mode);
                        snprintf(o, sizeof o, "%s/o", ps.s);
                        if (fifo) {
                            ensure(mkfifo(o, 0600), "mkfifo");
                            ensure(chown(o, i, GROUP), "chown fifo");
                            ensure(chmod(o, 0666), "chmod fifo");
                        } else
                            mkfile(o, i, 0666);
                        mklink(o, ps.v, 0);
                        snprintf(pth, sizeof pth, "%s/l", ps.s);
                        mklink(o, pth, i);
                        snprintf(pth, sizeof pth, "%s/sub", ps.s);
                        mkdir_owned(pth, i, 0777);
                        in_child(c, r_body, &ps);
                        p("%s\tknob=%d\tdir=%04o\tdirowner=%u\towner=%u\tcaller=%u", fifo ? "FIFOS" : "REGULAR", knob, mode, d, i, c);
                        int bad = 0;
                        for (int op = 0; op < R_NOPS; op++) {
                            int want = predict_r(knob, mode, d, i, c, fifo, op);
                            p("\t%s=%s", r_names[op], en(results[op]));
                            if (results[op] != want) bad++;
                        }
                        p("%s\n", bad ? "\tMISMATCH" : "");
                        mismatches += bad;
                        rows++;
                    }
    }
    set_knob(knob_name, 0);
    p("SWEEP\t%s\trows=%d\tmismatches=%d\n", fifo ? "fifos" : "regular", rows, mismatches);
    return mismatches;
}

// ------------------------------------------------------------ main

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <fresh directory>\n", argv[0]);
        return 2;
    }
    alarm(600);
    if (geteuid() != 0) {
        fprintf(stderr, "run as root\n");
        return 2;
    }
    snprintf(base, sizeof base, "%s", argv[1]);
    ensure(mkdir(base, 0755), base);
    results = mmap(NULL, NOPS * sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (results == MAP_FAILED) die("mmap");
    umask(0);

    struct utsname u;
    uname(&u);
    p("RUN\t%s %s %s\tbase=%s\n", u.sysname, u.release, u.machine, base);
    const char *knobs[] = {"protected_symlinks", "protected_regular", "protected_fifos", "protected_hardlinks"};
    int initial[4];
    for (int i = 0; i < 4; i++) {
        initial[i] = read_knob(knobs[i]);
        p("DEFAULT\t%s=%d\n", knobs[i], initial[i]);
    }
    // Which values each knob admits.
    const char *candidates[] = {"-1", "0", "1", "2", "3"};
    for (int i = 0; i < 4; i++) {
        p("ADMITS\t%s", knobs[i]);
        for (int j = 0; j < 5; j++) {
            int e = write_knob(knobs[i], candidates[j]);
            p("\t%s=%s", candidates[j], e == 0 ? "ok" : en(e));
        }
        p("\n");
        set_knob(knobs[i], 0);
    }

    int mismatches = 0;
    mismatches += part_symlinks();
    set_knob("protected_symlinks", 0);
    mismatches += part_access_ids();
    mismatches += part_order();
    set_knob("protected_symlinks", 0);
    mismatches += part_create(0);
    mismatches += part_create(1);

    for (int i = 0; i < 4; i++) set_knob(knobs[i], initial[i]);
    p("TOTAL\tmismatches=%d\n", mismatches);
    return 0;
}
#else
// ------------------------------------------------------------ Darwin

static void row(const char *what, int e) { p("DARWIN\t%s\t%s\n", what, en(e)); }

int main(int argc, char **argv) {
    if (argc != 4) {
        fprintf(stderr, "usage: %s <fresh directory> <root's symlink> <root's regular file>\n", argv[0]);
        return 2;
    }
    alarm(60);
    struct utsname u;
    uname(&u);
    p("RUN\t%s %s %s\tuid=%u\n", u.sysname, u.release, u.machine, getuid());
    struct stat st;
    if (lstat(argv[2], &st) != 0 || !S_ISLNK(st.st_mode) || st.st_uid == getuid()) die("argv[2] is not another user's symlink");
    unsigned linkowner = st.st_uid;
    if (lstat(argv[3], &st) != 0 || !S_ISREG(st.st_mode) || st.st_uid == getuid()) die("argv[3] is not another user's file");
    unsigned fileowner = st.st_uid;
    static const int modes[] = {01777, 01775};
    for (int m = 0; m < 2; m++) {
        char s[PATH_MAX], l[PATH_MAX + 8], f[PATH_MAX + 8];
        snprintf(s, sizeof s, "%s/%04o", argv[1], modes[m]);
        ensure(mkdir(s, 0700), s);
        ensure(chmod(s, modes[m]), "chmod");
        snprintf(l, sizeof l, "%s/l", s);
        snprintf(f, sizeof f, "%s/f", s);
        ensure(linkat(AT_FDCWD, argv[2], AT_FDCWD, l, 0), "link symlink");
        ensure(link(argv[3], f), "link file");
        lstat(s, &st);
        p("DIR\t%04o\towner=%u\tlinkowner=%u\tfileowner=%u\n", (unsigned)(st.st_mode & 07777), st.st_uid, linkowner, fileowner);
        // Each of these is refused on Linux under the knob named, by a caller
        // who owns the directory but not the inode.
        row("stat(l) [Linux protected_symlinks=1: EACCES]", R(stat(l, &st)));
        row("open(l, O_CREAT|O_NOFOLLOW|O_RDONLY) [Linux, every knob: EACCES where other-writable]",
            Rclose(open(l, O_CREAT | O_NOFOLLOW | O_RDONLY, 0644)));
        row("open(f, O_CREAT|O_RDONLY) [Linux protected_regular=1 where other-writable, 2 also where group-writable: EACCES]",
            Rclose(open(f, O_CREAT | O_RDONLY, 0644)));
        row("open(f, O_RDONLY)", Rclose(open(f, O_RDONLY)));
        ensure(unlink(l), "unlink l");
        ensure(unlink(f), "unlink f");
        ensure(rmdir(s), "rmdir");
    }
    return 0;
}
#endif
