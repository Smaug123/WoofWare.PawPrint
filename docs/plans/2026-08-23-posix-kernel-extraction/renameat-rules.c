// Measures whether renameat(2) from descriptors on directories decides
// anything differently from rename(2) with the same paths written relative to
// the current directory, past what at-dirfd.c measured (which kinds of dirfd
// start a walk on each side, the empty path's place against them, and ORDER2's
// five states per side); and the order in which the two sides' copy-ins,
// dirfds and walks are checked against each other, crossed more widely than
// ORDER2 crossed them.
//
// Rows:
//   ROW    one renameat and one rename, each in a fresh cell. The renameat runs
//          with the cwd at w, from a descriptor on the directory `ofd` names
//          (relative to w) for the old path and on `nfd` for the new one, or
//          from AT_FDCWD where either is "cwd". The rename runs with the cwd
//          at `cwd` (relative to w) on `pold` and `pnew`. Sections:
//            SAME  both descriptors on d, against rename from d as the cwd
//                  with the same paths: a file, a hard link to it, an empty
//                  and a full directory, links that dangle, loop and name a
//                  file, a directory and "/", each source and destination
//                  with and without a trailing separator; ".", "..",
//                  "sub/.", "sub/..", "lroot/.", "/"; moving a directory
//                  into itself, d into its own subdirectory through "..",
//                  d itself renamed through its own descriptor, climbing out
//                  of d; an unwritable directory on either side; a name APFS
//                  will not bind; the empty path on either side; and, where
//                  the probe starts as root, a sticky directory (01777,
//                  root's) holding another user's file and empty directory
//                  and one of the caller's files.
//            TWO   the old path from d and the new one from d2, against
//                  rename from w with "d/" and "d2/" put in front.
//            MIXED one side from a descriptor and the other from AT_FDCWD.
//            NEST  descriptors on d/nest/in and d: a directory moved into its
//                  own descendant through "..", and ".." as a source.
//   SELF   the directory a descriptor names moved, or removed, through that
//          descriptor: renameat(fd on d/sub, "../sub", fd, "../sub2"), or
//          unlinkat(fd on d/sub, "../sub", AT_REMOVEDIR); then a second
//          renameat from the same descriptor on both sides. Against
//          rename("../sub", "../sub2") or rmdir("../sub") from the cwd d/sub,
//          then the matching rename.
//   ORDER  renameat with the cwd at d, crossing fourteen kinds of old side
//          with fourteen kinds of new side: which side's failure is reported,
//          and so where each side's copy-in, dirfd and walk fall against the
//          other's.
//   DEV    (Linux) renameat with a descriptor on /dev on one side.
//
// An answer is the errno, or "ok" with every path under the cell that went
// ("gone") and every path whose inode is not the one it had before, with the
// first path (in byte order) that held that inode before ("moved"). A byte of
// a path outside printable ASCII, or a backslash, is printed as \xNN.
//
// The cell, with every entry the caller's (but the sticky directory's) and the
// umask 022:
//   w/            the cwd for renameat
//   w/d/          the directory most calls start from
//     f, g        a file and a hard link to it
//     sub/ e/     empty directories
//     full/       a directory holding x
//     nest/in/    a directory holding an empty directory
//     dang -> nx2, cyc -> cyc, lf -> f, ld -> sub, lroot -> /
//     ro/  (0555) holding kid (a file), kdir/ (empty), kfull/ (holding x)
//     st/  (01777, root's; made only where the probe starts as root) holding
//          of (a file) and od/ (empty), both uid 2000's, and mine, the
//          caller's file
//   w/d2/         the other starting directory
//     h           a file
//     hd/         an empty directory
//     hfull/      a directory holding x
//
// Linux, as root (each cell drops to uid 1000 in its child for the second
// caller):
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/renameat-rules.c && /tmp/p "$(mktemp -d)" > /probe/renameat-rules.linux-6.18.5-aarch64.txt'
// Darwin, as an ordinary user:
//   nix develop -c clang -Wall -Werror -o renameat-rules renameat-rules.c && ./renameat-rules "$(mktemp -d /private/tmp/renat.XXXXXX)" > renameat-rules.darwin-27.0-uid501.txt
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
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
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case EEXIST: return "EEXIST";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case ELOOP: return "ELOOP";
    case EPERM: return "EPERM";
    case EBUSY: return "EBUSY";
    case ENOTEMPTY: return "ENOTEMPTY";
    case ENOTSUP: return "ENOTSUP";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EXDEV: return "EXDEV";
    case EILSEQ: return "EILSEQ";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static int started_as_root;
static uid_t caller;
static char cell[PATH_MAX];

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static void mkfile(const char *p) {
    int fd = open(p, O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die(p);
    close(fd);
}

// A path, printed with every byte outside printable ASCII (and the
// backslash) as \xNN.
static void put(const char *p) {
    for (const unsigned char *c = (const unsigned char *)p; *c; c++) {
        if (*c < 0x20 || *c >= 0x7f || *c == '\\') printf("\\x%02x", *c);
        else putchar(*c);
    }
}

// Every path under the cell, relative to it, with its inode, collected by
// `walk`. Physical: never follow a link, so a move through one shows what
// moved.
#define MAXSEEN 128
struct seen_entry { char path[PATH_MAX]; ino_t ino; };
static struct seen_entry seen[MAXSEEN];
static int nseen;
static size_t celllen;

static int collect(const char *p, const struct stat *st, int flag, struct FTW *ftw) {
    (void)flag;
    (void)ftw;
    if (nseen >= MAXSEEN) die("too many paths");
    if (strlen(p) > celllen) {
        snprintf(seen[nseen].path, PATH_MAX, "%s", p + celllen + 1);
        seen[nseen].ino = st->st_ino;
        nseen++;
    }
    return 0;
}

static int by_path(const void *a, const void *b) {
    return strcmp(((const struct seen_entry *)a)->path, ((const struct seen_entry *)b)->path);
}

static void walk(void) {
    nseen = 0;
    celllen = strlen(cell);
    if (nftw(cell, collect, 16, FTW_PHYS) < 0) die("nftw");
    qsort(seen, (size_t)nseen, sizeof seen[0], by_path);
}

static struct seen_entry before[MAXSEEN];
static int nbefore;

static void remember(void) {
    walk();
    memcpy(before, seen, sizeof seen);
    nbefore = nseen;
}

// What changed since `remember`: the paths that went, and every path whose
// inode it did not have before, with the first path that had that inode.
static void changes(void) {
    walk();
    printf(":gone=");
    int any = 0;
    for (int i = 0; i < nbefore; i++) {
        int found = 0;
        for (int j = 0; j < nseen; j++)
            if (strcmp(before[i].path, seen[j].path) == 0) found = 1;
        if (!found) {
            if (any) putchar(',');
            put(before[i].path);
            any = 1;
        }
    }
    printf(":moved=");
    any = 0;
    for (int j = 0; j < nseen; j++) {
        int same = 0;
        for (int i = 0; i < nbefore; i++)
            if (strcmp(before[i].path, seen[j].path) == 0 && before[i].ino == seen[j].ino) same = 1;
        if (same) continue;
        const char *origin = "?";
        for (int i = 0; i < nbefore; i++)
            if (before[i].ino == seen[j].ino) {
                origin = before[i].path;
                break;
            }
        if (any) putchar(',');
        put(seen[j].path);
        printf("<-");
        put(origin);
        any = 1;
    }
}

// Make the cell, drop to the caller, and leave the cwd at w.
static void fixture(void) {
    snprintf(cell, sizeof cell, "%s/c%05d", base, cellno);
    if (mkdir(cell, 0777) < 0 || chmod(cell, 0777) < 0) die("mkdir cell");
    if (chdir(cell) < 0) die("chdir cell");
    if (mkdir("w", 0755) < 0 || mkdir("w/d", 0755) < 0 || mkdir("w/d2", 0755) < 0) die("w/d");
    if (started_as_root) {
        // The sticky directory and another user's entries in it, made while
        // still root.
        if (mkdir("w/d/st", 0755) < 0 || chmod("w/d/st", 01777) < 0) die("w/d/st");
        mkfile("w/d/st/of");
        if (mkdir("w/d/st/od", 0755) < 0) die("w/d/st/od");
        mkfile("w/d/st/mine");
        if (chown("w/d/st/of", 2000, 2000) < 0 || chown("w/d/st/od", 2000, 2000) < 0 ||
            chown("w/d/st/mine", caller, caller) < 0)
            die("chown st");
        if (chown("w", caller, caller) < 0 || chown("w/d", caller, caller) < 0 || chown("w/d2", caller, caller) < 0)
            die("chown w/d");
        if (caller != 0) {
            if (setgroups(0, NULL) < 0 || setgid(caller) < 0 || setuid(caller) < 0) die("drop");
        }
    }
    umask(022);
    if (chdir("w") < 0) die("chdir w");
    mkfile("d/f");
    if (link("d/f", "d/g") < 0) die("link d/g");
    if (mkdir("d/sub", 0755) < 0 || mkdir("d/e", 0755) < 0) die("d/sub");
    if (mkdir("d/full", 0755) < 0) die("d/full");
    mkfile("d/full/x");
    if (mkdir("d/nest", 0755) < 0 || mkdir("d/nest/in", 0755) < 0) die("d/nest");
    if (symlink("nx2", "d/dang") < 0 || symlink("cyc", "d/cyc") < 0 || symlink("f", "d/lf") < 0 ||
        symlink("sub", "d/ld") < 0 || symlink("/", "d/lroot") < 0)
        die("symlink");
    if (mkdir("d/ro", 0755) < 0 || mkdir("d/ro/kdir", 0755) < 0 || mkdir("d/ro/kfull", 0755) < 0) die("d/ro");
    mkfile("d/ro/kid");
    mkfile("d/ro/kfull/x");
    if (chmod("d/ro", 0555) < 0) die("chmod d/ro");
    mkfile("d2/h");
    if (mkdir("d2/hd", 0755) < 0 || mkdir("d2/hfull", 0755) < 0) die("d2/hd");
    mkfile("d2/hfull/x");
}

static void show(int r) {
    if (r < 0) {
        printf("%s", en(errno));
        return;
    }
    printf("ok");
    changes();
}

static void in_child(void (*body)(const void *, int), const void *arg, int how) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(5);
        body(arg, how);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

static int dirfd_of(const char *dir) {
    if (strcmp(dir, "cwd") == 0) return AT_FDCWD;
    int fd = open(dir, O_RDONLY | O_DIRECTORY);
    if (fd < 0) die(dir);
    return fd;
}

struct row {
    const char *section;
    const char *ofd, *old, *nfd, *new_;
    const char *cwd, *pold, *pnew;
};

// how: 0 is renameat with the cwd at w, 1 is rename with the cwd at row->cwd.
static void row_body(const void *v, int how) {
    const struct row *r = v;
    fixture();
    int ofd = -1, nfd = -1;
    if (how == 0) {
        ofd = dirfd_of(r->ofd);
        nfd = dirfd_of(r->nfd);
    } else if (strcmp(r->cwd, ".") != 0 && chdir(r->cwd) < 0) {
        die("chdir");
    }
    remember();
    show(how == 0 ? renameat(ofd, r->old, nfd, r->new_) : rename(r->pold, r->pnew));
}

static int caller_id(void) { return (int)(started_as_root ? caller : geteuid()); }

static void row(const struct row *r) {
    printf("ROW\tcaller=%d\t%s\tofd=%s\told=", caller_id(), r->section, r->ofd);
    put(r->old);
    printf("\tnfd=%s\tnew=", r->nfd);
    put(r->new_);
    printf("\tcwd=%s\tpold=", r->cwd);
    put(r->pold);
    printf("\tpnew=");
    put(r->pnew);
    printf("\tat=");
    in_child(row_body, r, 0);
    printf("\tplain=");
    in_child(row_body, r, 1);
    printf("\n");
}

// `dir` with a separator, or nothing for the cwd.
static void prefixed(char *out, const char *dir, const char *p) {
    if (strcmp(dir, "cwd") == 0) snprintf(out, PATH_MAX, "%s", p);
    else snprintf(out, PATH_MAX, "%s/%s", dir, p);
}

// A row whose rename runs from w, on the paths written relative to it.
static void from_w(const char *section, const char *ofd, const char *old, const char *nfd, const char *new_) {
    static char pold[PATH_MAX], pnew[PATH_MAX];
    prefixed(pold, ofd, old);
    prefixed(pnew, nfd, new_);
    struct row r = {section, ofd, old, nfd, new_, ".", pold, pnew};
    row(&r);
}

struct self_cell { const char *first, *old, *new_; };

// how: 0 is through a descriptor on d/sub with the cwd at w, 1 is from the
// cwd d/sub.
static void self_body(const void *v, int how) {
    const struct self_cell *c = v;
    fixture();
    int fd = -1;
    if (how == 0) {
        fd = open("d/sub", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d/sub");
    } else if (chdir("d/sub") < 0) {
        die("chdir d/sub");
    }
    remember();
    int first;
    if (strcmp(c->first, "rename") == 0)
        first = how == 0 ? renameat(fd, "../sub", fd, "../sub2") : rename("../sub", "../sub2");
    else
        first = how == 0 ? unlinkat(fd, "../sub", AT_REMOVEDIR) : rmdir("../sub");
    printf("%s;", first < 0 ? en(errno) : "ok");
    show(how == 0 ? renameat(fd, c->old, fd, c->new_) : rename(c->old, c->new_));
}

// ORDER: one kind of argument per side.
enum { K_GOOD, K_EXISTING, K_NULL, K_EMPTY, K_BADFD, K_FILEFD, K_PIPEFD, K_NODIR, K_NOTDIR, K_DOT, K_ROOT, K_TRAIL, K_LONG, K_OVERLONG, K_COUNT };
static const char *kname[K_COUNT] = {"good", "existing", "NULL", "empty", "badfd", "filefd", "pipefd",
                                     "nodir", "notdir", "dot", "root", "trail", "long", "overlong"};
static const char *volatile null_path = NULL;
static char longname[301];
static char *overlong;

static void side(int kind, int is_old, int *fd, const char **p) {
    int pp[2];
    *fd = AT_FDCWD;
    switch (kind) {
    case K_GOOD: *p = is_old ? "f" : "new"; break;
    case K_EXISTING: *p = is_old ? "nx" : "g"; break;
    case K_NULL: *p = null_path; break;
    case K_EMPTY: *p = ""; break;
    case K_BADFD: *fd = -1; *p = is_old ? "f" : "new"; break;
    case K_FILEFD:
        *fd = open("f", O_RDONLY);
        if (*fd < 0) die("open f");
        *p = is_old ? "f" : "new";
        break;
    case K_PIPEFD:
        if (pipe(pp) < 0) die("pipe");
        *fd = pp[0];
        *p = is_old ? "f" : "new";
        break;
    case K_NODIR: *p = is_old ? "nxdir/f" : "nxdir/new"; break;
    case K_NOTDIR: *p = is_old ? "f/x" : "f/new"; break;
    case K_DOT: *p = "."; break;
    case K_ROOT: *p = "/"; break;
    case K_TRAIL: *p = is_old ? "f/" : "new/"; break;
    case K_LONG: *p = longname; break;
    case K_OVERLONG: *p = overlong; break;
    }
}

static void order_body(const void *v, int how) {
    (void)how;
    const int *kinds = v;
    fixture();
    if (chdir("d") < 0) die("chdir d");
    int ofd, nfd;
    const char *op, *np;
    side(kinds[0], 1, &ofd, &op);
    side(kinds[1], 0, &nfd, &np);
    remember();
    show(renameat(ofd, op, nfd, np));
}

#ifdef __linux__
static void dev_body(const void *v, int how) {
    (void)how;
    const char *const *args = v;
    fixture();
    int dev = open("/dev", O_RDONLY | O_DIRECTORY);
    if (dev < 0) die("open /dev");
    int ofd = strcmp(args[0], "dev") == 0 ? dev : AT_FDCWD;
    int nfd = strcmp(args[2], "dev") == 0 ? dev : AT_FDCWD;
    if (chdir("d") < 0) die("chdir d");
    remember();
    show(renameat(ofd, args[1], nfd, args[3]));
}
#endif

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    started_as_root = geteuid() == 0;
    memset(longname, 'a', 300);
    longname[300] = '\0';
    overlong = malloc(PATH_MAX + 1);
    for (int i = 0; i < PATH_MAX; i++) overlong[i] = (i % 2 == 0) ? 'a' : '/';
    overlong[PATH_MAX] = '\0';
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\tAT_FDCWD=%d\n", u.sysname, u.release, u.machine, (int)geteuid(), AT_FDCWD);

    // SAME: (old, new) from d, both ways.
    const char *same[][2] = {
        {"f", "nx"},        {"f", "g"},          {"g", "f"},           {"f", "sub"},       {"f", "full"},
        {"sub", "e"},       {"sub", "full"},     {"sub", "f"},         {"full", "nx"},     {"f", "nx/"},
        {"sub", "nx/"},     {"f/", "nx"},        {"sub/", "nx"},       {"nx", "x"},        {"nx/", "x"},
        {"nx", "f/x"},      {"lf", "nx"},        {"lf/", "nx"},        {"ld", "nx"},       {"ld/", "nx"},
        {"dang/", "nx"},    {"cyc/", "nx"},      {"lroot/", "nx"},     {"f", "ld/"},       {"sub", "ld/"},
        {"f", "lf/"},       {"f", "dang/"},      {"sub", "dang/"},     {".", "nx"},        {"..", "nx"},
        {"f", "."},         {"f", ".."},         {"sub/.", "nx"},      {"sub/..", "nx"},   {"lroot/.", "nx"},
        {"f", "sub/."},     {"/", "x"},          {"sub", "sub/x"},     {"nest", "nest/in/x"}, {"../d", "sub/x"},
        {"../d", "../d3"},  {"f", "../f3"},      {"../d/f", "x"},      {"../d2/h", "x"},   {"ro/kid", "x"},
        {"f", "ro/x"},      {"ro/kdir", "x"},    {"sub", "ro/kdir"},   {"ro/kid", "ro/kid"}, {"f", "ro/kid"},
        {"e", "ro/kfull"},  {"f", "\xff\xff\xff"}, {"", "x"},          {"f", ""},
    };
    const char *sticky[][2] = {{"st/of", "x"}, {"st/od", "x"}, {"st/mine", "x"},
                               {"f", "st/of"}, {"f", "st/mine"}, {"sub", "st/od"}};
    // TWO: the old path from d, the new from d2.
    const char *two[][2] = {
        {"f", "h"},       {"f", "nx"},      {"f", "hd"},       {"sub", "hd"},    {"sub", "hfull"},
        {"sub", "h"},     {"sub", "nx"},    {"full", "nx"},    {"f", "nx/"},     {"sub", "nx/"},
        {"ld/", "nx"},    {"lf", "nx"},     {".", "nx"},       {"..", "nx"},     {"f", "."},
        {"f", ".."},      {"../d", "nx"},   {"../d2", "x"},    {"../d2", "../d2/x"}, {"nest", "../d/nest/in/x"},
        {"ro/kid", "h"},  {"f", "../d/ro/x"}, {"sub", "../d/sub"}, {"f", "\xff\xff\xff"}, {"nx", "h"},
    };
    // MIXED: (ofd, old, nfd, new).
    const char *mixed[][4] = {
        {"d", "f", "cwd", "x"},          {"d", "sub", "cwd", "d2/hd"}, {"d", "f", "cwd", "d/nx"},
        {"cwd", "d/f", "d2", "nx"},      {"cwd", "d2", "d", "sub/x"},  {"d", "..", "cwd", "x"},
    };
    // NEST: (ofd, old, nfd, new).
    const char *nest[][4] = {
        {"d/nest/in", "../../nest", "d/nest/in", "x"},
        {"d", "nest", "d/nest/in", "x"},
        {"d/nest/in", "..", "d", "y"},
        {"d/nest/in", "../../../d", "d/nest/in", "../x"},
        {"d/nest/in", "../../../d", "d2", "x"},
        {"d/nest", "in", "d", "in2"},
    };
    struct self_cell self[] = {
        {"rename", "../f", "x"},   {"rename", ".", "../sub3"}, {"rename", "x", "y"},
        {"rename", "../e", "x"},   {"rename", "../sub2", "../sub3"},
        {"rmdir", "../f", "x"},    {"rmdir", "../f", "../x"},  {"rmdir", ".", "../x"},
        {"rmdir", "../f", "."},    {"rmdir", "..", "../x"},    {"rmdir", "x", "../y"},
        {"rmdir", "../e", "../e2"},
    };

    int ncallers = started_as_root ? 2 : 1;
    for (int c = 0; c < ncallers; c++) {
        caller = c == 0 ? (started_as_root ? 0 : geteuid()) : 1000;
        for (size_t i = 0; i < sizeof same / sizeof same[0]; i++) {
            struct row r = {"SAME", "d", same[i][0], "d", same[i][1], "d", same[i][0], same[i][1]};
            row(&r);
        }
        if (started_as_root)
            for (size_t i = 0; i < sizeof sticky / sizeof sticky[0]; i++) {
                struct row r = {"SAME", "d", sticky[i][0], "d", sticky[i][1], "d", sticky[i][0], sticky[i][1]};
                row(&r);
            }
        for (size_t i = 0; i < sizeof two / sizeof two[0]; i++) from_w("TWO", "d", two[i][0], "d2", two[i][1]);
        for (size_t i = 0; i < sizeof mixed / sizeof mixed[0]; i++)
            from_w("MIXED", mixed[i][0], mixed[i][1], mixed[i][2], mixed[i][3]);
        for (size_t i = 0; i < sizeof nest / sizeof nest[0]; i++)
            from_w("NEST", nest[i][0], nest[i][1], nest[i][2], nest[i][3]);
        for (size_t i = 0; i < sizeof self / sizeof self[0]; i++) {
            printf("SELF\tcaller=%d\tfirst=%s\told=%s\tnew=%s\tat=", caller_id(), self[i].first, self[i].old,
                   self[i].new_);
            in_child(self_body, &self[i], 0);
            printf("\tplain=");
            in_child(self_body, &self[i], 1);
            printf("\n");
        }
        for (int o = 0; o < K_COUNT; o++) {
            printf("ORDER\tcaller=%d\told=%s", caller_id(), kname[o]);
            for (int n = 0; n < K_COUNT; n++) {
                int kinds[2] = {o, n};
                printf("\tnew:%s=", kname[n]);
                in_child(order_body, kinds, 0);
            }
            printf("\n");
        }
#ifdef __linux__
        const char *dev[][4] = {{"dev", "null", "cwd", "x"}, {"dev", "nx", "cwd", "x"},
                                {"cwd", "f", "dev", "x"},    {"cwd", "f", "dev", "null"},
                                {"cwd", "nx", "dev", "x"}};
        for (size_t i = 0; i < sizeof dev / sizeof dev[0]; i++) {
            printf("DEV\tcaller=%d\tofd=%s\told=%s\tnfd=%s\tnew=%s\tat=", caller_id(), dev[i][0], dev[i][1],
                   dev[i][2], dev[i][3]);
            in_child(dev_body, dev[i], 0);
            printf("\n");
        }
#endif
    }
    return 0;
}
