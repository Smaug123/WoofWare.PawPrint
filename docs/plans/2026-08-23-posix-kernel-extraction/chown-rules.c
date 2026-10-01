// Measures chown(2), lchown(2) and fchown(2): who may change an inode's owner
// and group, which set-ID bits a change clears (for which callers, on which
// file kinds, and depending on which other bits), which timestamps move, how a
// path through a symbolic link resolves for each, and what fchown answers on
// each descriptor kind.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/chown-rules.c && /tmp/p /tmp/chownp && /tmp/p /dev/shm/chownp'
//         Run it as root: each standing row forks a child that takes the
//         credentials under test, against inodes root has chowned to owners and
//         groups the child is and is not a member of, carrying every one of the
//         4096 modes.
// Darwin: nix develop -c clang -Wall -o chown-rules chown-rules.c && ./chown-rules "$(mktemp -d /private/tmp/chownp.XXXXXX)"
//         As an ordinary user, which cannot give an inode to anyone else. The
//         owner rows run on inodes this user made, in directories whose group
//         decides the new inode's group. The non-owner rows run against other
//         users' inodes (see FOREIGN_* below), and ask only what is harmless if
//         the kernel allows it; each request that would change such an inode is
//         first asked of a root-owned marker file, and skipped everywhere else
//         if the marker allowed it. Edit the FOREIGN_* paths to inodes on the
//         machine at hand.
//
// Measured on Linux 6.18.5 (aarch64, root in the container) and Darwin 27.0
// (arm64, uid 501) on 2026-10-01. The Linux output on tmpfs (/dev/shm, given
// room with `container run --shm-size 2G`) was identical to the ext4 output
// beside this file, line for line, once the base directory's name is set
// aside. Every sweep reported no mismatch. On Darwin a root-owned symbolic
// link under /private/var/db answered EPERM to lchown(-1, -1), which no other
// inode did, so the link measured is one under /private/tmp instead. The rows
// are replayed in WoofWare.PosixKernel.Test/TestOwnerChange.fs, which embeds
// the output.
//
// Every sweep compares each row against the rule WoofWare.PosixKernel models
// (`predict` below) and prints the mismatch count and the first few
// mismatches. Rows for a handful of modes are printed in full as literals.
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
#include <sys/mman.h>
#include <sys/resource.h>
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

#define NMODES 4096
#define NONE ((unsigned)-1)

static char base[PATH_MAX];
// Shared with a forked child: the errno each of its calls answered.
static int *results;

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
    case EOPNOTSUPP: return "EOPNOTSUPP";
#if defined(ENOTSUP) && ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
    default: return strerror(e);
    }
}

static int R(int rc) { return rc == 0 ? 0 : errno; }

static void path(char *out, const char *dir, const char *name) { snprintf(out, PATH_MAX, "%s/%s", dir, name); }

static void nth(char *out, const char *dir, int i) { snprintf(out, PATH_MAX, "%s/%04o", dir, i); }

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

static int sid(unsigned id) { return id == NONE ? -1 : (int)id; }

// ---------------------------------------------------------------- the rule modelled

// How the caller stands towards the inode, and what it asks for, in the terms
// the model's rule reads.
typedef struct {
    int privileged, owns, ingroup;
    // The uid asked for: 0 unchanged (-1), 1 the inode's current owner, 2 the
    // caller's own uid (not the current owner), 3 anyone else.
    int user;
    // The gid asked for: 0 unchanged (-1), 1 the inode's current group, 2 a
    // group the caller is in (not the current group), 3 anyone else's.
    int group;
} req_class;

static const char *user_names[] = {"-1", "current", "caller", "other"};
static const char *group_names[] = {"-1", "current", "member", "other"};

// The rule modelled. Returns the errno predicted, and the resulting mode
// through *result (the mode before, when the errno is not 0).
static int predict(const req_class *c, int isdir, int before, int *result) {
    *result = before;
#ifdef __linux__
    if (!c->privileged) {
        // Only the owner may name a uid, and only its own.
        if (c->user != 0 && !(c->owns && c->user == 1)) return EPERM;
        // Only the owner may name a gid, and only the current one or one of
        // its own groups.
        if (c->group != 0 && !(c->owns && (c->group == 1 || c->group == 2))) return EPERM;
    }
    if (isdir) return 0;
    // Anything but a directory: S_ISUID always goes, and S_ISGID goes when
    // the file is group-executable or the caller is neither in its group nor
    // privileged. Clearing a bit is a mode change, which only the owner or a
    // privileged caller may make, even when no id was named.
    int kill = before & 04000;
    if ((before & 02000) && ((before & 010) || !(c->ingroup || c->privileged))) kill |= 02000;
    if (kill && !c->owns && !c->privileged) return EPERM;
    *result = before & ~kill;
    return 0;
#else
    (void)isdir;
    if (c->privileged) return -1; // not measured
    // A uid may be named only if it is the current owner's; a gid only if it
    // is the current group, or the owner names one of its own groups.
    if (c->user >= 2) return EPERM;
    if (c->group == 3 || (c->group == 2 && !c->owns)) return EPERM;
    // Naming either id clears both set-ID bits, on a directory too; naming
    // neither clears nothing.
    if (c->user != 0 || c->group != 0) *result = before & ~06000;
    return 0;
#endif
}

static int member_of(gid_t g) {
    if (getegid() == g) return 1;
    gid_t gs[256];
    int n = getgroups(256, gs);
    for (int i = 0; i < n; i++)
        if (gs[i] == g) return 1;
    return 0;
}

static req_class classify(uid_t ou, gid_t og, unsigned ru, unsigned rg) {
    req_class c;
    c.privileged = geteuid() == 0;
    c.owns = geteuid() == ou;
    c.ingroup = member_of(og);
    c.user = ru == NONE ? 0 : ru == ou ? 1 : ru == geteuid() ? 2 : 3;
    c.group = rg == NONE ? 0 : rg == og ? 1 : member_of(rg) ? 2 : 3;
    return c;
}

// The modes whose rows are printed in full.
static const int literal_modes[] = {0644, 02644, 02654, 04644, 06755, 06745, 01755};
#define NLITERAL (sizeof literal_modes / sizeof literal_modes[0])

static int is_literal(int m) {
    for (unsigned i = 0; i < NLITERAL; i++)
        if (literal_modes[i] == m) return 1;
    return 0;
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

typedef struct {
    const char *label;
    int privileged;
    uid_t ou;
    gid_t og;
} standing_row;

static const standing_row standing_rows[] = {
    {"owner, group = egid", 0, 1000, 1000},
    {"owner, group = supplementary", 0, 1000, 2000},
    {"owner, group not a member", 0, 1000, 3000},
    {"non-owner, group = egid", 0, 1001, 1000},
    {"non-owner, group = supplementary", 0, 1001, 2000},
    {"non-owner, group not a member", 0, 1001, 3000},
    {"root, owner, in group", 1, 0, 0},
    {"root, owner, group not a member", 1, 0, 3000},
    {"root, non-owner, in group", 1, 1001, 0},
    {"root, non-owner, group not a member", 1, 1001, 3000},
};
#define NROWS (sizeof standing_rows / sizeof standing_rows[0])

static const unsigned user_asks[] = {NONE, 0, 1000, 1001, 1002};
static const unsigned group_asks[] = {NONE, 0, 1000, 2000, 3000, 3001};

// Whether (ru, rg) is a request this row should ask: each distinct class of
// request, the caller's own uid and the current owner being one request for
// an owner.
static int wanted(const standing_row *row, unsigned ru, unsigned rg) {
    uid_t me = row->privileged ? 0 : 1000;
    // uids: -1, the current owner, the caller, and 1002 as "anyone else".
    if (ru != NONE && ru != row->ou && ru != me && ru != 1002) return 0;
    // gids: -1, the current group, 1000 and 2000 (u1000's), 0 (root's) and
    // 3001 as "anyone else's".
    if (rg == 3000 && row->og != 3000) return 0;
    return 1;
}

// Root prepares `dir`'s 4096 inodes as `row` owns them, one per mode; the
// row's caller then asks chown(ru, rg) of each; root reads back what each
// became.
static void sweep(const standing_row *row, const char *dir, int isdir, unsigned ru, unsigned rg, int lchown_it) {
    for (int m = 0; m < NMODES; m++) {
        char t[PATH_MAX];
        nth(t, dir, m);
        if (chown(t, row->ou, row->og) != 0) die("prepare chown");
        if (chmod(t, m) != 0) die("prepare chmod");
    }
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(300);
        become(row->privileged ? &ROOT : &U1000);
        for (int m = 0; m < NMODES; m++) {
            char t[PATH_MAX];
            nth(t, dir, m);
            results[m] = lchown_it ? R(lchown(t, ru, rg)) : R(chown(t, ru, rg));
        }
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) p("CHILD-FAILED\t%s\tstatus=%d\n", row->label, status);

    // Classify as the child stood: its credentials, the row's owner.
    req_class c;
    c.privileged = row->privileged;
    c.owns = row->privileged ? row->ou == 0 : row->ou == 1000;
    c.ingroup = row->privileged ? row->og == 0 : (row->og == 1000 || row->og == 2000);
    uid_t me = row->privileged ? 0 : 1000;
    c.user = ru == NONE ? 0 : ru == row->ou ? 1 : ru == me ? 2 : 3;
    int memberrg = row->privileged ? rg == 0 : (rg == 1000 || rg == 2000);
    c.group = rg == NONE ? 0 : rg == row->og ? 1 : memberrg ? 2 : 3;

    int mismatches = 0;
    for (int m = 0; m < NMODES; m++) {
        char t[PATH_MAX];
        nth(t, dir, m);
        struct stat st;
        if (lstat(t, &st) != 0) die("lstat after");
        int got = st.st_mode & 07777;
        int want;
        int we = predict(&c, isdir, m, &want);
        unsigned wu = (we == 0 && ru != NONE) ? ru : row->ou;
        unsigned wg = (we == 0 && rg != NONE) ? rg : row->og;
        if (results[m] != we || got != want || st.st_uid != wu || st.st_gid != wg) {
            if (mismatches < 8)
                p("SWEEP-MISMATCH\t%s\t%s\tchown(%d,%d)\tmode=%04o\tgot=%s/%04o %d:%d\twant=%s/%04o %u:%u\n", row->label, isdir ? "dir" : "file", sid(ru), sid(rg), m, en(results[m]), got,
                  (int)st.st_uid, (int)st.st_gid, en(we), want, wu, wg);
            mismatches++;
        }
        if (is_literal(m))
            p("ROW\t%s\t%s\tchown(%d,%d) [user %s, group %s]\t%04o\t%s\tafter=%04o %d:%d\n", row->label, isdir ? "dir" : "file", sid(ru), sid(rg), user_names[c.user], group_names[c.group], m,
              en(results[m]), got, (int)st.st_uid, (int)st.st_gid);
    }
    p("SWEEP\t%s\t%s\tchown(%d,%d) [user %s, group %s]\tmodes=4096\tmismatches=%d\n", row->label, isdir ? "dir" : "file", sid(ru), sid(rg), user_names[c.user], group_names[c.group], mismatches);
}

// The timestamps a chown moves, asked by `row`'s caller of an inode whose
// mode is `mode`.
static void times(const standing_row *row, const char *t, int isdir, int mode, unsigned ru, unsigned rg) {
    if (chown(t, row->ou, row->og) != 0) die("prepare chown");
    if (chmod(t, mode) != 0) die("prepare chmod");
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(60);
        become(row->privileged ? &ROOT : &U1000);
        struct stat b, a;
        if (lstat(t, &b) != 0) die("lstat before");
        settle();
        int e = R(chown(t, ru, rg));
        if (lstat(t, &a) != 0) die("lstat after");
        p("TIMES\t%s\t%s %04o\tchown(%d,%d)\t%s\tafter=%04o %d:%d\t%s\n", row->label, isdir ? "dir" : "file", mode, sid(ru), sid(rg), en(e), a.st_mode & 07777, (int)a.st_uid, (int)a.st_gid,
          moved(&b, &a));
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
}

static void standings(void) {
    for (unsigned r = 0; r < NROWS; r++) {
        const standing_row *row = &standing_rows[r];
        char dir[PATH_MAX], nm[32];
        snprintf(nm, sizeof nm, "row%u", r);
        path(dir, base, nm);
        if (mkdir(dir, 0755) != 0) die(dir);
        for (int isdir = 0; isdir <= 1; isdir++) {
            char kdir[PATH_MAX];
            path(kdir, dir, isdir ? "dirs" : "files");
            if (mkdir(kdir, 0755) != 0) die(kdir);
            for (int m = 0; m < NMODES; m++) {
                char t[PATH_MAX];
                nth(t, kdir, m);
                mkthing(t, isdir);
            }
            for (unsigned u = 0; u < sizeof user_asks / sizeof user_asks[0]; u++)
                for (unsigned g = 0; g < sizeof group_asks / sizeof group_asks[0]; g++) {
                    unsigned ru = user_asks[u];
                    unsigned rg = group_asks[g];
                    if (!wanted(row, ru, rg)) continue;
                    sweep(row, kdir, isdir, ru, rg, 0);
                }
            char t[PATH_MAX];
            path(t, dir, isdir ? "timed" : "timef");
            mkthing(t, isdir);
            int modes[] = {isdir ? 0755 : 0644, 06755, 02745};
            for (int mi = 0; mi < 3; mi++)
                for (unsigned u = 0; u < sizeof user_asks / sizeof user_asks[0]; u++)
                    for (unsigned g = 0; g < sizeof group_asks / sizeof group_asks[0]; g++) {
                        if (!wanted(row, user_asks[u], group_asks[g])) continue;
                        times(row, t, isdir, modes[mi], user_asks[u], group_asks[g]);
                    }
        }
    }
}

// lchown of a symbolic link, over the same rows: a link's mode is 0777 on
// Linux and cannot be changed, so only who may change its owner is asked.
static void symlink_rows(void) {
    for (unsigned r = 0; r < NROWS; r++) {
        const standing_row *row = &standing_rows[r];
        char dir[PATH_MAX], nm[32];
        snprintf(nm, sizeof nm, "links%u", r);
        path(dir, base, nm);
        if (mkdir(dir, 0755) != 0) die(dir);
        char l[PATH_MAX];
        path(l, dir, "l");
        if (symlink("nowhere", l) != 0) die("symlink");
        for (unsigned u = 0; u < sizeof user_asks / sizeof user_asks[0]; u++)
            for (unsigned g = 0; g < sizeof group_asks / sizeof group_asks[0]; g++) {
                unsigned ru = user_asks[u], rg = group_asks[g];
                if (!wanted(row, ru, rg)) continue;
                if (lchown(l, row->ou, row->og) != 0) die("prepare lchown");
                struct stat b;
                lstat(l, &b);
                fflush(stdout);
                pid_t pid = fork();
                if (pid == 0) {
                    become(row->privileged ? &ROOT : &U1000);
                    settle();
                    results[0] = R(lchown(l, ru, rg));
                    _exit(0);
                }
                int status;
                waitpid(pid, &status, 0);
                struct stat a;
                lstat(l, &a);
                p("LINK\t%s\tlchown(%d,%d)\t%s\tmode %04o -> %04o\t%d:%d -> %d:%d\t%s\n", row->label, sid(ru), sid(rg), en(results[0]), b.st_mode & 07777, a.st_mode & 07777, (int)b.st_uid,
                  (int)b.st_gid, (int)a.st_uid, (int)a.st_gid, moved(&b, &a));
            }
    }
}

// ---------------------------------------------------------------- pipes

// A pipe is one pipefs inode both ends name, with an owner and a mode like any
// other: root hands 4096 pipes to the row's owner and group, one per mode, and
// the row's caller then asks fchown of each, alternating the end it asks
// through.
static void pipes(void) {
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    rl.rlim_cur = rl.rlim_max;
    if (setrlimit(RLIMIT_NOFILE, &rl) != 0 || rl.rlim_cur < 2 * NMODES + 64) {
        p("PIPE\tcannot hold %d descriptors (limit %ld): skipped\n", 2 * NMODES, (long)rl.rlim_cur);
        return;
    }
    static int fds[NMODES][2];
    const unsigned asks[][2] = {{NONE, NONE}, {1000, NONE}, {NONE, 1000}, {NONE, 2000}, {1002, NONE}, {NONE, 3001}, {0, NONE}};
    for (unsigned r = 0; r < NROWS; r++) {
        const standing_row *row = &standing_rows[r];
        for (unsigned a = 0; a < sizeof asks / sizeof asks[0]; a++) {
            unsigned ru = asks[a][0], rg = asks[a][1];
            for (int m = 0; m < NMODES; m++) {
                if (pipe(fds[m]) != 0) die("pipe");
                if (fchown(fds[m][0], row->ou, row->og) != 0) die("fchown pipe");
                if (fchmod(fds[m][1], m) != 0) die("fchmod pipe");
            }
            fflush(stdout);
            pid_t pid = fork();
            if (pid == 0) {
                alarm(120);
                become(row->privileged ? &ROOT : &U1000);
                for (int m = 0; m < NMODES; m++) results[m] = R(fchown(fds[m][m & 1], ru, rg));
                _exit(0);
            }
            int status;
            waitpid(pid, &status, 0);
            req_class c;
            c.privileged = row->privileged;
            c.owns = row->privileged ? row->ou == 0 : row->ou == 1000;
            c.ingroup = row->privileged ? row->og == 0 : (row->og == 1000 || row->og == 2000);
            uid_t me = row->privileged ? 0 : 1000;
            c.user = ru == NONE ? 0 : ru == row->ou ? 1 : ru == me ? 2 : 3;
            int memberrg = row->privileged ? rg == 0 : (rg == 1000 || rg == 2000);
            c.group = rg == NONE ? 0 : rg == row->og ? 1 : memberrg ? 2 : 3;
            int mismatches = 0;
            for (int m = 0; m < NMODES; m++) {
                struct stat rs, ws;
                fstat(fds[m][0], &rs);
                fstat(fds[m][1], &ws);
                int want;
                int we = predict(&c, 0, m, &want);
                unsigned wu = (we == 0 && ru != NONE) ? ru : row->ou;
                unsigned wg = (we == 0 && rg != NONE) ? rg : row->og;
                int got = rs.st_mode & 07777;
                if (results[m] != we || got != want || rs.st_uid != wu || rs.st_gid != wg || ws.st_mode != rs.st_mode || ws.st_uid != rs.st_uid || ws.st_gid != rs.st_gid) {
                    if (mismatches < 8)
                        p("PIPE-MISMATCH\t%s\tfchown(%d,%d)\tmode=%04o\tgot=%s/%04o %d:%d\twant=%s/%04o %u:%u\n", row->label, sid(ru), sid(rg), m, en(results[m]), got, (int)rs.st_uid,
                          (int)rs.st_gid, en(we), want, wu, wg);
                    mismatches++;
                }
                close(fds[m][0]);
                close(fds[m][1]);
            }
            p("PIPE-SWEEP\t%s\tfchown(%d,%d) [user %s, group %s]\tmodes=4096\tmismatches=%d\n", row->label, sid(ru), sid(rg), user_names[c.user], group_names[c.group], mismatches);
        }
    }
    // Which timestamps an fchown of a pipe moves, seen through both ends.
    int pf[2];
    if (pipe(pf) != 0) die("pipe");
    for (int end = 0; end <= 1; end++) {
        struct stat b, a, ob, oa;
        fstat(pf[end], &b);
        fstat(pf[1 - end], &ob);
        settle();
        int e = R(fchown(pf[end], (uid_t)-1, (gid_t)-1));
        fstat(pf[end], &a);
        fstat(pf[1 - end], &oa);
        p("PIPE-TIMES\troot\tfchown(%s end, -1, -1)\t%s\tthat end: %s\tthe other end: %s\n", end ? "write" : "read", en(e), moved(&b, &a), moved(&ob, &oa));
        fstat(pf[end], &b);
        fstat(pf[1 - end], &ob);
        settle();
        e = R(fchown(pf[end], 1002, 3001));
        fstat(pf[end], &a);
        fstat(pf[1 - end], &oa);
        p("PIPE-TIMES\troot\tfchown(%s end, 1002, 3001)\t%s\tthat end: %s\tthe other end: %s\n", end ? "write" : "read", en(e), moved(&b, &a), moved(&ob, &oa));
    }
    close(pf[0]);
    close(pf[1]);
}
#else
// ---------------------------------------------------------------- Darwin owner rows

// `dir` is a directory of the group the row wants; the inodes made in it take
// that group. Each is made, given its mode (an ordinary user's chmod silently
// drops S_ISGID outside its groups, so the mode that stuck is what is
// compared), and asked chown(ru, rg) once.
static void darwin_sweep(const char *label, const char *dir, int isdir, unsigned ru, unsigned rg) {
    char batch[PATH_MAX];
    path(batch, dir, "batch");
    if (mkdir(batch, 0755) != 0) die(batch);
    struct stat dst;
    stat(batch, &dst);
    int mismatches = 0, skipped = 0;
    req_class c = classify(geteuid(), dst.st_gid, ru, rg);
    for (int m = 0; m < NMODES; m++) {
        char t[PATH_MAX];
        nth(t, batch, m);
        mkthing(t, isdir);
        chmod(t, m);
        struct stat b;
        lstat(t, &b);
        int before = b.st_mode & 07777;
        if (before != m) skipped++;
        int e = R(chown(t, ru, rg));
        struct stat a;
        lstat(t, &a);
        int got = a.st_mode & 07777;
        int want;
        int we = predict(&c, isdir, before, &want);
        unsigned wu = (we == 0 && ru != NONE) ? ru : b.st_uid;
        unsigned wg = (we == 0 && rg != NONE) ? rg : b.st_gid;
        if (e != we || got != want || a.st_uid != wu || a.st_gid != wg) {
            if (mismatches < 8)
                p("SWEEP-MISMATCH\t%s\t%s\tchown(%d,%d)\tmode=%04o\tgot=%s/%04o %d:%d\twant=%s/%04o %u:%u\n", label, isdir ? "dir" : "file", sid(ru), sid(rg), before, en(e), got, (int)a.st_uid,
                  (int)a.st_gid, en(we), want, wu, wg);
            mismatches++;
        }
        if (is_literal(m))
            p("ROW\t%s\t%s\tchown(%d,%d) [user %s, group %s]\t%04o\t%s\tafter=%04o %d:%d\n", label, isdir ? "dir" : "file", sid(ru), sid(rg), user_names[c.user], group_names[c.group], before, en(e),
              got, (int)a.st_uid, (int)a.st_gid);
        if (isdir) rmdir(t);
        else unlink(t);
    }
    rmdir(batch);
    p("SWEEP\t%s\t%s\tchown(%d,%d) [user %s, group %s]\tmodes=4096\tnot settable=%d\tmismatches=%d\n", label, isdir ? "dir" : "file", sid(ru), sid(rg), user_names[c.user], group_names[c.group], skipped,
      mismatches);
}

static void darwin_times(const char *label, const char *dir, int isdir, int mode, unsigned ru, unsigned rg) {
    char t[PATH_MAX];
    path(t, dir, "timed");
    mkthing(t, isdir);
    chmod(t, mode);
    struct stat b, a;
    lstat(t, &b);
    settle();
    int e = R(chown(t, ru, rg));
    lstat(t, &a);
    p("TIMES\t%s\t%s %04o\tchown(%d,%d)\t%s\tafter=%04o %d:%d\t%s\n", label, isdir ? "dir" : "file", b.st_mode & 07777, sid(ru), sid(rg), en(e), a.st_mode & 07777, (int)a.st_uid, (int)a.st_gid,
      moved(&b, &a));
    if (isdir) rmdir(t);
    else unlink(t);
}

static void standings(void) {
    uid_t me = geteuid();
    gid_t eg = getegid();
    // `base` is under /private/tmp, whose group is wheel, so every inode made
    // in it is wheel's: the owner outside its group.
    struct {
        const char *label;
        gid_t g;
    } rows[] = {{"owner, group = egid", eg}, {"owner, group = supplementary 12", 12}, {"owner, group not a member (wheel)", (gid_t)-1}};
    for (int i = 0; i < 3; i++) {
        char dir[PATH_MAX], nm[32];
        snprintf(nm, sizeof nm, "owner%d", i);
        path(dir, base, nm);
        if (mkdir(dir, 0755) != 0) die(dir);
        if (rows[i].g != (gid_t)-1 && chown(dir, (uid_t)-1, rows[i].g) != 0) die("chgrp row");
        struct stat st;
        stat(dir, &st);
        gid_t og = st.st_gid;
        p("SETUP\t%s\tgroup=%d\n", rows[i].label, (int)og);
        const unsigned uids[] = {NONE, me, 0, 502};
        const unsigned gids[] = {NONE, og, eg, 12, 0, 4242};
        for (int isdir = 0; isdir <= 1; isdir++)
            for (unsigned u = 0; u < 4; u++)
                for (unsigned g = 0; g < 6; g++) {
                    if (g >= 2 && gids[g] == og) continue;
                    darwin_sweep(rows[i].label, dir, isdir, uids[u], gids[g]);
                }
        int modes[] = {0644, 06755, 02745, 0755};
        for (int isdir = 0; isdir <= 1; isdir++)
            for (int mi = 0; mi < 4; mi++)
                for (unsigned u = 0; u < 4; u++)
                    for (unsigned g = 0; g < 6; g++) {
                        if (g >= 2 && gids[g] == og) continue;
                        darwin_times(rows[i].label, dir, isdir, modes[mi], uids[u], gids[g]);
                    }
    }
}

// ---------------------------------------------------------------- Darwin non-owner rows

// A root-owned 0644 marker whose owner and group nobody reads; hard links to
// it in the scratch directory are what the requests are asked through.
static const char *FOREIGN_MARKER = "/private/tmp/.AppleMiniSetupDidRun"; // root:wheel 0644
static const char *FOREIGN_INGROUP_FILE = "/Users/steam/.DS_Store";        // steam:staff 0644
static const char *FOREIGN_INGROUP_DIR = "/Users/steam";                   // steam:staff 0755
static const char *FOREIGN_OTHER_DIR = "/private/tmp/powerlog";            // root:wheel 0755
// A symbolic link someone else owns. Not one under /private/var/db: there,
// lchown answered EPERM even for (-1, -1), which no other inode did, so
// something other than the ownership rules is deciding.
static const char *FOREIGN_LINK = "/private/tmp/FTABHarvest/centauri-symlink-ftab.bin"; // root:wheel symbolic link
// Someone else's set-user-ID file, outside the system volume. Only chown(-1,
// -1) is asked of it, which changes nothing if the kernel allows it.
static const char *FOREIGN_SETID_FILE = "/Applications/VirtualBox.app/Contents/MacOS/VBoxNetAdpCtl"; // root:wheel 04755

static int foreign_ask(const char *label, const char *pth, unsigned ru, unsigned rg) {
    struct stat b, a;
    if (lstat(pth, &b) != 0) {
        p("FOREIGN\t%s\t%s\tlstat %s\n", label, pth, en(errno));
        return -1;
    }
    req_class c = classify(b.st_uid, b.st_gid, ru, rg);
    settle();
    int e = R(chown(pth, ru, rg));
    lstat(pth, &a);
    int want;
    int we = predict(&c, S_ISDIR(b.st_mode), b.st_mode & 07777, &want);
    p("FOREIGN\t%s\t%s\t%d:%d %04o\tchown(%d,%d) [user %s, group %s]\t%s\tafter=%d:%d %04o\t%s\t%s\n", label, pth, (int)b.st_uid, (int)b.st_gid, b.st_mode & 07777, sid(ru), sid(rg),
      user_names[c.user], group_names[c.group], en(e), (int)a.st_uid, (int)a.st_gid, a.st_mode & 07777, moved(&b, &a), e == we ? "as predicted" : "MISMATCH");
    return e;
}

static void foreigners(void) {
    uid_t me = geteuid();
    gid_t eg = getegid();
    char marker[PATH_MAX], ingroup[PATH_MAX];
    path(marker, base, "marker");
    path(ingroup, base, "ingroup");
    if (link(FOREIGN_MARKER, marker) != 0) p("FOREIGN\tlink %s: %s\n", FOREIGN_MARKER, en(errno));
    if (link(FOREIGN_INGROUP_FILE, ingroup) != 0) p("FOREIGN\tlink %s: %s\n", FOREIGN_INGROUP_FILE, en(errno));
    struct stat ms, gs;
    lstat(marker, &ms);
    lstat(ingroup, &gs);
    // Requests that cannot change anything whatever the kernel says, first.
    foreign_ask("non-owner, group not a member", marker, NONE, NONE);
    foreign_ask("non-owner, group not a member", marker, 1002, NONE);
    foreign_ask("non-owner, group not a member", marker, NONE, 4242);
    foreign_ask("non-owner, group = egid", ingroup, NONE, NONE);
    foreign_ask("non-owner, group = egid", ingroup, 1002, NONE);
    foreign_ask("non-owner, group = egid", ingroup, NONE, 4242);
    // Naming the current ids changes nothing on an inode with no set-ID bit,
    // whatever the kernel says.
    foreign_ask("non-owner, group not a member", marker, ms.st_uid, NONE);
    foreign_ask("non-owner, group not a member", marker, NONE, ms.st_gid);
    foreign_ask("non-owner, group not a member", marker, ms.st_uid, ms.st_gid);
    foreign_ask("non-owner, group = egid", ingroup, gs.st_uid, NONE);
    foreign_ask("non-owner, group = egid", ingroup, NONE, gs.st_gid);
    foreign_ask("non-owner, group = egid", ingroup, gs.st_uid, gs.st_gid);
    // Taking the inode, or moving it to one of the caller's groups: the marker
    // first, and the in-group file only if the marker refused.
    int take = foreign_ask("non-owner, group not a member", marker, me, NONE);
    int move = foreign_ask("non-owner, group not a member", marker, NONE, eg);
    if (take == EPERM) foreign_ask("non-owner, group = egid", ingroup, me, NONE);
    else p("FOREIGN\tskipped taking %s: the marker answered %s\n", FOREIGN_INGROUP_FILE, en(take));
    if (move == EPERM) foreign_ask("non-owner, group = egid", ingroup, NONE, 12);
    else p("FOREIGN\tskipped moving %s: the marker answered %s\n", FOREIGN_INGROUP_FILE, en(move));
    // Directories, asked through their own paths.
    foreign_ask("non-owner dir, group not a member", (char *)FOREIGN_OTHER_DIR, NONE, NONE);
    foreign_ask("non-owner dir, group not a member", (char *)FOREIGN_OTHER_DIR, 1002, NONE);
    foreign_ask("non-owner dir, group not a member", (char *)FOREIGN_OTHER_DIR, NONE, 4242);
    foreign_ask("non-owner dir, group not a member", (char *)FOREIGN_OTHER_DIR, 0, NONE);
    foreign_ask("non-owner dir, group not a member", (char *)FOREIGN_OTHER_DIR, NONE, 0);
    foreign_ask("non-owner dir, group = egid", (char *)FOREIGN_INGROUP_DIR, NONE, NONE);
    foreign_ask("non-owner dir, group = egid", (char *)FOREIGN_INGROUP_DIR, 1002, NONE);
    foreign_ask("non-owner dir, group = egid", (char *)FOREIGN_INGROUP_DIR, NONE, 4242);
    struct stat ds;
    lstat(FOREIGN_INGROUP_DIR, &ds);
    foreign_ask("non-owner dir, group = egid", (char *)FOREIGN_INGROUP_DIR, ds.st_uid, NONE);
    foreign_ask("non-owner dir, group = egid", (char *)FOREIGN_INGROUP_DIR, NONE, ds.st_gid);
    foreign_ask("non-owner, set-ID, group not a member", (char *)FOREIGN_SETID_FILE, NONE, NONE);
    // A symbolic link someone else owns, through lchown: naming its current
    // ids changes nothing but its ctime, whatever the kernel says.
    struct stat ls;
    if (lstat(FOREIGN_LINK, &ls) != 0) p("FOREIGN\tlstat %s: %s\n", FOREIGN_LINK, en(errno));
    else {
        const unsigned asks[][2] = {{NONE, NONE}, {ls.st_uid, NONE}, {NONE, ls.st_gid}, {1002, NONE}, {NONE, 4242}, {me, NONE}, {NONE, eg}};
        for (unsigned i = 0; i < sizeof asks / sizeof asks[0]; i++) {
            struct stat b, a;
            lstat(FOREIGN_LINK, &b);
            req_class c = classify(b.st_uid, b.st_gid, asks[i][0], asks[i][1]);
            settle();
            int e = R(lchown(FOREIGN_LINK, asks[i][0], asks[i][1]));
            lstat(FOREIGN_LINK, &a);
            int want;
            int we = predict(&c, 0, b.st_mode & 07777, &want);
            p("FOREIGN\tnon-owner link, group not a member\t%s\t%d:%d %04o\tlchown(%d,%d) [user %s, group %s]\t%s\tafter=%d:%d %04o\t%s\t%s\n", FOREIGN_LINK, (int)b.st_uid, (int)b.st_gid,
              b.st_mode & 07777, sid(asks[i][0]), sid(asks[i][1]), user_names[c.user], group_names[c.group], en(e), (int)a.st_uid, (int)a.st_gid, a.st_mode & 07777, moved(&b, &a),
              e == we ? "as predicted" : "MISMATCH");
        }
    }
    unlink(marker);
    unlink(ingroup);
}
#endif

// ---------------------------------------------------------------- paths and symbolic links

static void owner_of(const char *pth, char *out, size_t n, int follow) {
    struct stat st;
    if ((follow ? stat(pth, &st) : lstat(pth, &st)) != 0) snprintf(out, n, "(%s)", en(errno));
    else snprintf(out, n, "%d:%d", (int)st.st_uid, (int)st.st_gid);
}

static void path_row(const char *label, const char *call, int e, const char *watch, const char *before) {
    p("PATH\t%s\t%s\t%s", label, call, en(e));
    if (watch) {
        char after[64];
        owner_of(watch, after, sizeof after, 0);
        p("\t%s: %s -> %s", watch + strlen(base) + 1, before, after);
    }
    p("\n");
}

static void ask_path(const char *label, const char *pth, unsigned ru, unsigned rg, const char *watch) {
    char before[64] = "";
    if (watch) owner_of(watch, before, sizeof before, 0);
    char call[64];
    snprintf(call, sizeof call, "chown(%d,%d)", sid(ru), sid(rg));
    path_row(label, call, R(chown(pth, ru, rg)), watch, before);
}

static void ask_lpath(const char *label, const char *pth, unsigned ru, unsigned rg, const char *watch) {
    char before[64] = "";
    if (watch) owner_of(watch, before, sizeof before, 0);
    char call[64];
    snprintf(call, sizeof call, "lchown(%d,%d)", sid(ru), sid(rg));
    path_row(label, call, R(lchown(pth, ru, rg)), watch, before);
}

// `other` is a group the caller is in other than its effective one; `foreign`
// a regular file someone else owns, or NULL.
static void paths(gid_t other, const char *foreign) {
    char dir[PATH_MAX], f[PATH_MAX], d[PATH_MAX], lf[PATH_MAX], ld[PATH_MAX], dang[PATH_MAX], loop[PATH_MAX], a[PATH_MAX], shut[PATH_MAX], inner[PATH_MAX];
    path(dir, base, "paths");
    if (mkdir(dir, 0755) != 0) die(dir);
    path(f, dir, "f");
    mkfile(f);
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
    path(shut, dir, "shut");
    mkdir(shut, 0700);
    path(inner, shut, "inner");
    mkfile(inner);
    chmod(shut, 0600);
    gid_t eg = getegid();

    // Through a link to a file: the target changes, and the link does not.
    struct stat lb, la, tb, ta;
    lstat(lf, &lb);
    stat(lf, &tb);
    settle();
    ask_path("a link to a file: the target changes", lf, NONE, other, f);
    lstat(lf, &la);
    stat(lf, &ta);
    p("PATH\tthe link itself\t%d:%d -> %d:%d\t%s\n", (int)lb.st_uid, (int)lb.st_gid, (int)la.st_uid, (int)la.st_gid, moved(&lb, &la));
    p("PATH\tthe target\t%s\n", moved(&tb, &ta));
    // lchown of the same link: the link changes, and the target does not.
    lstat(lf, &lb);
    stat(lf, &tb);
    settle();
    ask_lpath("a link to a file, lchown: the link changes", lf, NONE, eg, lf);
    lstat(lf, &la);
    stat(lf, &ta);
    p("PATH\tthe target, after lchown\t%d:%d -> %d:%d\t%s\n", (int)tb.st_uid, (int)tb.st_gid, (int)ta.st_uid, (int)ta.st_gid, moved(&tb, &ta));
    p("PATH\tthe link, after lchown\t%s\n", moved(&lb, &la));
    // An lchown asking nothing, of a link.
    lstat(lf, &lb);
    settle();
    ask_lpath("a link, lchown(-1,-1)", lf, NONE, NONE, lf);
    lstat(lf, &la);
    p("PATH\tthe link, after lchown(-1,-1)\t%s\n", moved(&lb, &la));

    ask_path("a link to a directory", ld, NONE, other, d);
    ask_lpath("a link to a directory, lchown", ld, NONE, other, ld);
    snprintf(a, sizeof a, "%s/", ld);
    ask_lpath("a link to a directory, trailing separator, lchown", a, NONE, eg, d);
    ask_path("a link to a directory, trailing separator", a, NONE, eg, d);
    snprintf(a, sizeof a, "%s/", d);
    ask_path("a directory, trailing separator", a, NONE, other, d);
    ask_lpath("a directory, trailing separator, lchown", a, NONE, eg, d);
    snprintf(a, sizeof a, "%s/", f);
    ask_path("a file, trailing separator", a, NONE, eg, f);
    ask_lpath("a file, trailing separator, lchown", a, NONE, eg, f);
    snprintf(a, sizeof a, "%s/", lf);
    ask_path("a link to a file, trailing separator", a, NONE, eg, f);
    ask_lpath("a link to a file, trailing separator, lchown", a, NONE, eg, f);
    ask_path("a dangling link", dang, NONE, other, dang);
    ask_lpath("a dangling link, lchown", dang, NONE, other, dang);
    ask_path("a link to itself", loop, NONE, other, loop);
    ask_lpath("a link to itself, lchown", loop, NONE, eg, loop);
    path(a, dir, "absent");
    ask_path("an absent name", a, NONE, NONE, NULL);
    ask_lpath("an absent name, lchown", a, NONE, NONE, NULL);
    ask_path("the empty path", "", NONE, NONE, NULL);
    ask_lpath("the empty path, lchown", "", NONE, NONE, NULL);
    path(a, f, "under");
    ask_path("a file as a directory", a, NONE, NONE, NULL);
    ask_lpath("a file as a directory, lchown", a, NONE, NONE, NULL);
    ask_path("through an unsearchable directory", inner, NONE, NONE, NULL);
    ask_lpath("through an unsearchable directory, lchown", inner, NONE, NONE, NULL);
    ask_path("through an unsearchable directory, naming another's uid", inner, 1002, NONE, NULL);
    if (foreign) {
        // Someone else's file: the walk's errors come before EPERM.
        snprintf(a, sizeof a, "%s/", foreign);
        ask_path("someone else's file, trailing separator, naming a uid", a, 1002, NONE, NULL);
        path(a, foreign, "under");
        ask_path("someone else's file as a directory, naming a uid", a, 1002, NONE, NULL);
        ask_path("someone else's file, naming a uid", foreign, 1002, NONE, NULL);
    }
    chmod(shut, 0700);
}

// ---------------------------------------------------------------- fchown on each descriptor kind

static void fd_row(const char *label, int fd, unsigned ru, unsigned rg) {
    struct stat b, a;
    int be = fstat(fd, &b) == 0 ? 0 : errno;
    int e = R(fchown(fd, ru, rg));
    int ae = fstat(fd, &a) == 0 ? 0 : errno;
    if (be || ae) p("FCHOWN\t%s\tfchown(%d,%d)\t%s\t(fstat %s)\n", label, sid(ru), sid(rg), en(e), en(be ? be : ae));
    else
        p("FCHOWN\t%s\tfchown(%d,%d)\t%s\tst_mode 0%o -> 0%o\towner %d:%d -> %d:%d\n", label, sid(ru), sid(rg), en(e), b.st_mode, a.st_mode, (int)b.st_uid, (int)b.st_gid, (int)a.st_uid,
          (int)a.st_gid);
}

static void descriptors(gid_t other, const char *foreign) {
    char dir[PATH_MAX], f[PATH_MAX];
    path(dir, base, "fds");
    if (mkdir(dir, 0755) != 0) die(dir);
    path(f, dir, "f");
    mkfile(f);
    gid_t eg = getegid();

    int fd = open(f, O_RDONLY);
    fd_row("a file open O_RDONLY", fd, NONE, other);
    close(fd);
    fd = open(f, O_WRONLY);
    fd_row("a file open O_WRONLY", fd, NONE, eg);
    close(fd);
    fd = open(f, O_RDWR);
    fd_row("a file open O_RDWR", fd, geteuid(), NONE);
    close(fd);
    fd = open(dir, O_RDONLY);
    fd_row("a directory", fd, NONE, other);
    close(fd);
    fd = open(f, O_RDONLY);
    unlink(f);
    fd_row("an unlinked file", fd, NONE, eg);
    close(fd);
    if (foreign) {
        fd = open(foreign, O_RDONLY);
        if (fd < 0) p("FCHOWN\ta file someone else owns\topen: %s\n", en(errno));
        else {
            fd_row("a file someone else owns, open O_RDONLY", fd, NONE, NONE);
            fd_row("a file someone else owns, open O_RDONLY", fd, 1002, NONE);
            fd_row("a file someone else owns, open O_RDONLY", fd, NONE, 4242);
            close(fd);
        }
    }
    p("FCHOWN\ta closed descriptor\tfchown(-1,-1)\t%s\n", en(R(fchown(1000, (uid_t)-1, (gid_t)-1))));
    p("FCHOWN\tdescriptor -1\tfchown(-1,-1)\t%s\n", en(R(fchown(-1, (uid_t)-1, (gid_t)-1))));

    int pfd[2];
    if (pipe(pfd) != 0) die("pipe");
    fd_row("a pipe's read end", pfd[0], NONE, NONE);
    fd_row("a pipe's read end", pfd[0], NONE, other);
    fd_row("a pipe's write end", pfd[1], NONE, eg);
    fd_row("a pipe's write end", pfd[1], 1002, NONE);
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
            p("FCHOWN\t%s\tsocket: %s\n", socks[i].label, en(errno));
            continue;
        }
        fd_row(socks[i].label, s, NONE, NONE);
        fd_row(socks[i].label, s, NONE, other);
        fd_row(socks[i].label, s, 1002, NONE);
        close(s);
    }
#ifdef __linux__
    int ep = epoll_create1(0);
    fd_row("an epoll instance", ep, NONE, NONE);
    fd_row("an epoll instance", ep, NONE, other);
    fd_row("an epoll instance", ep, 1002, NONE);
    close(ep);
#else
    int kq = kqueue();
    fd_row("a kqueue", kq, NONE, NONE);
    fd_row("a kqueue", kq, NONE, other);
    fd_row("a kqueue", kq, 1002, NONE);
    close(kq);
#endif
}

int main(int argc, char **argv) {
    alarm(3000);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <scratch directory>\n", argv[0]);
        return 2;
    }
    snprintf(base, sizeof base, "%s", argv[1]);
    mkdir(base, 0755);
    chmod(base, 0755);
    umask(022);
    results = mmap(NULL, NMODES * sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0);
    if (results == MAP_FAILED) die("mmap");
    p("RUN\tuid=%d gid=%d\tbase=%s\n", (int)getuid(), (int)getgid(), base);
#ifdef __linux__
    standings();
    symlink_rows();
    pipes();
    // Paths and descriptors as u1000, in a directory it owns, and as root.
    char foreign[PATH_MAX];
    path(foreign, base, "rootfile");
    mkfile(foreign);
    chmod(foreign, 0644);
    char mine[PATH_MAX];
    path(mine, base, "u1000");
    if (mkdir(mine, 0755) != 0 || chown(mine, 1000, 1000) != 0) die("mkdir u1000");
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(120);
        become(&U1000);
        snprintf(base, sizeof base, "%s", mine);
        p("RUN\tas u1000\n");
        paths(2000, foreign);
        descriptors(2000, foreign);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    char rootdir[PATH_MAX];
    path(rootdir, base, "root");
    if (mkdir(rootdir, 0755) != 0) die("mkdir root");
    snprintf(base, sizeof base, "%s", rootdir);
    p("RUN\tas root\n");
    paths(3001, NULL);
    descriptors(3001, NULL);
#else
    standings();
    foreigners();
    // The rest in a directory of this user's effective group.
    char mine[PATH_MAX];
    path(mine, base, "mine");
    if (mkdir(mine, 0755) != 0 || chown(mine, (uid_t)-1, getegid()) != 0) die("mkdir mine");
    snprintf(base, sizeof base, "%s", mine);
    // Someone else's file, for the rows that need one: another hard link to
    // the root-owned marker.
    char foreign[PATH_MAX];
    path(foreign, mine, "marker");
    if (link(FOREIGN_MARKER, foreign) != 0) die("link marker");
    paths(12, foreign);
    descriptors(12, foreign);
    unlink(foreign);
#endif
    return 0;
}
