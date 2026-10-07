// Measures whether mkdirat(2) from a descriptor on a directory D decides
// anything differently from mkdir(2) run with D as the current directory,
// past what at-dirfd.c measured (which kinds of dirfd start a walk, and the
// empty path's place against them).
//
// Rows:
//   PATH   mkdirat(fd on d, path, 0777) with the cwd at d's parent w, and
//          mkdir(path, 0777) with the cwd at d, each in a fresh cell, for
//          pathnames that reach each of mkdir's own rules: the trailing
//          separator on a file, a free name, a dangling link (whose target is
//          relative to d, so a creation resolved from the wrong directory
//          lands somewhere else), a link loop, links to a file and to a
//          directory; ".", ".." and climbing out of d; an unwritable
//          directory (0555, holding e/); a set-group-ID directory; a name
//          APFS will not bind, alone and in the unwritable directory.
//   MODE   the same pair for "nx" and for "sg/x" (in the set-group-ID
//          directory), under each mode argument.
//   ORDER  mkdirat with mode 0xffff against a NULL path, the empty path and
//          a free name, for a dirfd on a directory, on a file, and -1:
//          whether anything about the mode is screened ahead of the copy-in
//          or the dirfd.
//
// An answer is the errno, or "ok" with the new directory's permission bits,
// whose gid it took ("parent" for its directory's, "egid" for the caller's,
// "both" when they are the same), and every path under the cell that the call
// created (a creation through the dangling link shows where it landed).
// A byte of a created name outside printable ASCII is printed as \xNN.
// The cell, with every entry the caller's and the umask 022:
//   w/            the cwd for mkdirat
//   w/d/          the directory both calls start from
//     f           a file
//     sub/        a directory
//     dang -> nx2, cyc -> cyc, lf -> f, ld -> sub
//     ro/  (0555) holding e/
//     sg/  (02775; as root its group is 4321, which is not the caller's)
//
// Linux, as root (each cell drops to uid 1000 in its child for the second
// caller):
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/mkdirat-rules.c && /tmp/p "$(mktemp -d)" > /probe/mkdirat-rules.linux-6.18.5-aarch64.txt'
// Darwin, as an ordinary user:
//   nix develop -c clang -Wall -Werror -o mkdirat-rules mkdirat-rules.c && ./mkdirat-rules "$(mktemp -d /private/tmp/mkat.XXXXXX)" > mkdirat-rules.darwin-27.0-uid501.txt
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
    case EEXIST: return "EEXIST";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case ELOOP: return "ELOOP";
    case EPERM: return "EPERM";
    case ENOTSUP: return "ENOTSUP";
    case EILSEQ: return "EILSEQ";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
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

// Every path under the cell, relative to it, collected by `walk`.
#define MAXSEEN 64
static char seen[MAXSEEN][PATH_MAX];
static int nseen;
static size_t celllen;

static int collect(const char *p, const struct stat *st, int flag, struct FTW *ftw) {
    (void)st;
    (void)flag;
    (void)ftw;
    if (nseen < MAXSEEN && strlen(p) > celllen) snprintf(seen[nseen++], PATH_MAX, "%s", p + celllen + 1);
    return 0;
}

static void walk(void) {
    nseen = 0;
    celllen = strlen(cell);
    // Physical: never follow a link, so a created directory shows where it is.
    if (nftw(cell, collect, 16, FTW_PHYS) < 0) die("nftw");
}

// Make the cell, drop to the caller, and leave the cwd at w.
static void fixture(void) {
    snprintf(cell, sizeof cell, "%s/c%05d", base, cellno);
    if (mkdir(cell, 0777) < 0 || chmod(cell, 0777) < 0) die("mkdir cell");
    if (caller != 0) {
        if (setgroups(0, NULL) < 0 || setgid(caller) < 0 || setuid(caller) < 0) die("drop");
    }
    umask(022);
    if (chdir(cell) < 0) die("chdir cell");
    if (mkdir("w", 0755) < 0 || mkdir("w/d", 0755) < 0) die("w/d");
    if (chdir("w") < 0) die("chdir w");
    mkfile("d/f");
    if (mkdir("d/sub", 0755) < 0) die("d/sub");
    if (symlink("nx2", "d/dang") < 0 || symlink("cyc", "d/cyc") < 0 || symlink("f", "d/lf") < 0 ||
        symlink("sub", "d/ld") < 0)
        die("symlink");
    if (mkdir("d/ro", 0755) < 0 || mkdir("d/ro/e", 0755) < 0 || chmod("d/ro", 0555) < 0) die("d/ro");
    if (mkdir("d/sg", 0755) < 0) die("d/sg");
    if (geteuid() == 0 && chown("d/sg", (uid_t)-1, 4321) < 0) die("chown d/sg");
    if (chmod("d/sg", 02775) < 0) die("chmod d/sg");
}

static void show(int r, const char *path, int fd) {
    if (r < 0) {
        printf("%s", en(errno));
        return;
    }
    char before[MAXSEEN][PATH_MAX];
    int nbefore = nseen;
    memcpy(before, seen, sizeof seen);
    walk();
    printf("ok");
    struct stat st, parent;
    // The new directory and the one holding it, found by name from where the
    // call started.
    char p[PATH_MAX], dir[PATH_MAX];
    snprintf(p, sizeof p, "%s", path);
    size_t n = strlen(p);
    while (n > 1 && p[n - 1] == '/') p[--n] = '\0';
    char *slash = strrchr(p, '/');
    if (slash == NULL)
        snprintf(dir, sizeof dir, ".");
    else
        snprintf(dir, sizeof dir, "%.*s", (int)(slash - p), p);
    if (fstatat(fd, p, &st, 0) == 0 && fstatat(fd, dir, &parent, 0) == 0) {
        const char *gid = st.st_gid == parent.st_gid ? (st.st_gid == getegid() ? "both" : "parent")
                          : st.st_gid == getegid()   ? "egid"
                                                     : "other";
        printf(":mode=%04o:gid=%s", (unsigned)(st.st_mode & 07777), gid);
    }
    printf(":new=");
    int any = 0;
    for (int i = 0; i < nseen; i++) {
        int found = 0;
        for (int j = 0; j < nbefore; j++)
            if (strcmp(seen[i], before[j]) == 0) found = 1;
        if (!found) {
            printf("%s", any ? "," : "");
            // A byte outside printable ASCII as \xNN, so the output is text.
            for (const unsigned char *b = (const unsigned char *)seen[i]; *b; b++)
                if (*b < 0x20 || *b >= 0x7f)
                    printf("\\x%02x", *b);
                else
                    putchar(*b);
            any = 1;
        }
    }
}

static const char *volatile null_path = NULL;

static void in_child(void (*body)(const char *, unsigned, int), const char *path, unsigned mode, int how) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(5);
        body(path, mode, how);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

// how: 0 is mkdirat from a descriptor on d, 1 is mkdir from d as the cwd.
static void pair_body(const char *path, unsigned mode, int how) {
    fixture();
    int fd;
    if (how == 0) {
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d");
    } else {
        if (chdir("d") < 0) die("chdir d");
        fd = AT_FDCWD;
    }
    walk();
    int r = how == 0 ? mkdirat(fd, path, (mode_t)mode) : mkdir(path, (mode_t)mode);
    show(r, path, fd);
}

// how: the dirfd: 0 a directory, 1 a file, 2 is -1. path "NULL" is the null
// pointer.
static void order_body(const char *path, unsigned mode, int how) {
    fixture();
    int fd = how == 0 ? open("d", O_RDONLY | O_DIRECTORY) : how == 1 ? open("d/f", O_RDONLY) : -1;
    if (how != 2 && fd < 0) die("open dirfd");
    const char *p = strcmp(path, "NULL") == 0 ? null_path : path;
    walk();
    int r = mkdirat(fd, p, (mode_t)mode);
    show(r, p == NULL ? "" : p, fd);
}

static void pair(const char *table, const char *label, const char *path, unsigned mode) {
    printf("%s\tcaller=%d\t%s\tmode=%04o\tat=", table, (int)(caller == 0 ? geteuid() : caller), label, mode);
    in_child(pair_body, path, mode, 0);
    printf("\tplain=");
    in_child(pair_body, path, mode, 1);
    printf("\n");
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

    const char *names[][2] = {
        {"nx", "nx"},       {"nx/", "nx/"},       {"nx//", "nx//"},   {"f", "f"},         {"f/", "f/"},
        {"f/.", "f/."},     {"dang", "dang"},     {"dang/", "dang/"}, {"cyc", "cyc"},     {"cyc/", "cyc/"},
        {"lf", "lf"},       {"lf/", "lf/"},       {"ld", "ld"},       {"ld/", "ld/"},     {"ld/x", "ld/x"},
        {"sub/x", "sub/x"}, {"sub/x/y", "sub/x/y"}, {".", "."},       {"..", ".."},       {"../nx", "../nx"},
        {"../d/nx", "../d/nx"}, {"ro/x", "ro/x"}, {"ro/e", "ro/e"},   {"sg/x", "sg/x"},
        {"xff3", "\xff\xff\xff"}, {"ro/xff3", "ro/\xff\xff\xff"},
    };
    const unsigned modes[] = {0, 0777, 01777, 02777, 07777, 0xffff};

    int ncallers = geteuid() == 0 ? 2 : 1;
    for (int c = 0; c < ncallers; c++) {
        caller = c == 0 ? 0 : 1000;
        for (size_t i = 0; i < sizeof names / sizeof names[0]; i++) pair("PATH", names[i][0], names[i][1], 0777);
        for (size_t i = 0; i < sizeof modes / sizeof modes[0]; i++) {
            pair("MODE", "nx", "nx", modes[i]);
            pair("MODE", "sg/x", "sg/x", modes[i]);
        }
        const char *dirfds[] = {"dir", "file", "minus1"};
        const char *paths[] = {"NULL", "", "nx"};
        for (int d = 0; d < 3; d++) {
            for (int p = 0; p < 3; p++) {
                printf("ORDER\tcaller=%d\tdirfd=%s\tpath=%s\tmode=ffff\tat=", (int)(caller == 0 ? geteuid() : caller),
                       dirfds[d], p == 1 ? "empty" : paths[p]);
                in_child(order_body, paths[p], 0xffff, d);
                printf("\n");
            }
        }
    }
    return 0;
}
