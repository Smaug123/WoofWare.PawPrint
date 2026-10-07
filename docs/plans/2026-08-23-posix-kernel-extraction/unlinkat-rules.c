// Measures whether unlinkat(2) from a descriptor on a directory D decides
// anything differently from unlink(2) and rmdir(2) run with D as the current
// directory, past what at-dirfd.c measured (which kinds of dirfd start a walk,
// the empty path's place against them, and which single flag bits each
// flavour accepts); and how the flag word is screened when it carries more
// than one bit.
//
// Rows:
//   PATH   unlinkat(fd on d, path, F) with the cwd at d's parent w, and
//          unlink(path) (F = 0) or rmdir(path) (F = AT_REMOVEDIR) with the
//          cwd at d, each in a fresh cell, for pathnames that reach each of
//          the two calls' own rules: a file, a free name, an empty and a full
//          directory, each with and without a trailing separator; links that
//          dangle, loop, and name a file, a directory and "/"; ".", "..",
//          "sub/.", "sub/..", "lroot/."; climbing out of d, back into it, and
//          onto d itself; an unwritable directory (0555) holding a file, an
//          empty and a full directory; and, where the probe starts as root, a
//          sticky directory (01777, root's) holding another user's file and
//          empty directory and one of the caller's files.
//   SELF   the directory a descriptor names removed through that descriptor:
//          unlinkat(fd on d/sub, "../sub", AT_REMOVEDIR), then a second
//          unlinkat from the same descriptor; against rmdir("../sub") from the
//          cwd d/sub, then the matching unlink or rmdir.
//   FLAGS2 flag words of more than one bit, from AT_FDCWD (the cwd at d):
//          AT_REMOVEDIR with a bit the flavour rejects, and, on Darwin, each
//          bit it accepts beyond AT_REMOVEDIR with a rejected bit and with
//          AT_REMOVEDIR. Each word against a file, an empty directory, a NULL
//          path, and -1 as dirfd with the file.
//
// An answer is the errno, or "ok" with every path under the cell the call
// removed (a removal through a link shows what it removed). For SELF, the two
// calls' answers are joined by ";", and "gone" lists what both removed.
//
// The cell, with every entry the caller's (but the sticky directory's) and
// the umask 022:
//   w/            the cwd for unlinkat
//   w/d/          the directory every call starts from
//     f           a file
//     sub/ e/     empty directories
//     full/       a directory holding x
//     dang -> nx2, cyc -> cyc, lf -> f, ld -> sub, lroot -> /
//     ro/  (0555) holding kid (a file), kdir/ (empty), kfull/ (holding x)
//     st/  (01777, root's; made only where the probe starts as root) holding
//          of (a file) and od/ (empty), both uid 2000's, and mine, the
//          caller's file
//
// Linux, as root (each cell drops to uid 1000 in its child for the second
// caller):
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/unlinkat-rules.c && /tmp/p "$(mktemp -d)" > /probe/unlinkat-rules.linux-6.18.5-aarch64.txt'
// Darwin, as an ordinary user:
//   nix develop -c clang -Wall -Werror -o unlinkat-rules unlinkat-rules.c && ./unlinkat-rules "$(mktemp -d /private/tmp/unat.XXXXXX)" > unlinkat-rules.darwin-27.0-uid501.txt
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
    // Physical: never follow a link, so a removal through one shows what went.
    if (nftw(cell, collect, 16, FTW_PHYS) < 0) die("nftw");
}

static char before[MAXSEEN][PATH_MAX];
static int nbefore;

static void remember(void) {
    walk();
    memcpy(before, seen, sizeof seen);
    nbefore = nseen;
}

// Every path the cell held at `remember` and holds no longer.
static void gone(void) {
    walk();
    printf(":gone=");
    int any = 0;
    for (int i = 0; i < nbefore; i++) {
        int found = 0;
        for (int j = 0; j < nseen; j++)
            if (strcmp(before[i], seen[j]) == 0) found = 1;
        if (!found) {
            printf("%s%s", any ? "," : "", before[i]);
            any = 1;
        }
    }
}

// Make the cell, drop to the caller, and leave the cwd at w.
static void fixture(void) {
    snprintf(cell, sizeof cell, "%s/c%05d", base, cellno);
    if (mkdir(cell, 0777) < 0 || chmod(cell, 0777) < 0) die("mkdir cell");
    if (chdir(cell) < 0) die("chdir cell");
    if (mkdir("w", 0755) < 0 || mkdir("w/d", 0755) < 0) die("w/d");
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
        if (chown("w", caller, caller) < 0 || chown("w/d", caller, caller) < 0) die("chown w/d");
        if (caller != 0) {
            if (setgroups(0, NULL) < 0 || setgid(caller) < 0 || setuid(caller) < 0) die("drop");
        }
    }
    umask(022);
    if (chdir("w") < 0) die("chdir w");
    mkfile("d/f");
    if (mkdir("d/sub", 0755) < 0 || mkdir("d/e", 0755) < 0) die("d/sub");
    if (mkdir("d/full", 0755) < 0) die("d/full");
    mkfile("d/full/x");
    if (symlink("nx2", "d/dang") < 0 || symlink("cyc", "d/cyc") < 0 || symlink("f", "d/lf") < 0 ||
        symlink("sub", "d/ld") < 0 || symlink("/", "d/lroot") < 0)
        die("symlink");
    if (mkdir("d/ro", 0755) < 0 || mkdir("d/ro/kdir", 0755) < 0 || mkdir("d/ro/kfull", 0755) < 0) die("d/ro");
    mkfile("d/ro/kid");
    mkfile("d/ro/kfull/x");
    if (chmod("d/ro", 0555) < 0) die("chmod d/ro");
}

static void show(int r) {
    if (r < 0) {
        printf("%s", en(errno));
        return;
    }
    printf("ok");
    gone();
}

static void in_child(void (*body)(const char *, int, int), const char *path, int flags, int how) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(5);
        body(path, flags, how);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

static int plain(const char *path, int flags) { return flags == AT_REMOVEDIR ? rmdir(path) : unlink(path); }

// how: 0 is unlinkat from a descriptor on d, 1 is unlink or rmdir from d as
// the cwd.
static void pair_body(const char *path, int flags, int how) {
    fixture();
    int fd = -1;
    if (how == 0) {
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d");
    } else if (chdir("d") < 0) {
        die("chdir d");
    }
    remember();
    show(how == 0 ? unlinkat(fd, path, flags) : plain(path, flags));
}

// The second call after removing d/sub through what holds it. `path` and
// `flags` are that second call's.
static void self_body(const char *path, int flags, int how) {
    fixture();
    int fd = -1;
    if (how == 0) {
        fd = open("d/sub", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d/sub");
    } else if (chdir("d/sub") < 0) {
        die("chdir d/sub");
    }
    remember();
    int first = how == 0 ? unlinkat(fd, "../sub", AT_REMOVEDIR) : rmdir("../sub");
    printf("%s;", first < 0 ? en(errno) : "ok");
    int second = how == 0 ? unlinkat(fd, path, flags) : plain(path, flags);
    printf("%s", second < 0 ? en(errno) : "ok");
    gone();
}

static const char *volatile null_path = NULL;

// how: 0 is "f", 1 is "sub", 2 is NULL, 3 is "f" with -1 as dirfd.
static void word_body(const char *unused, int flags, int how) {
    (void)unused;
    fixture();
    if (chdir("d") < 0) die("chdir d");
    remember();
    const char *p = how == 0 || how == 3 ? "f" : how == 1 ? "sub" : null_path;
    show(unlinkat(how == 3 ? -1 : AT_FDCWD, p, flags));
}

static int caller_id(void) { return (int)(started_as_root ? caller : geteuid()); }

static void pair(const char *label, const char *path, int flags) {
    printf("PATH\tcaller=%d\t%s\t%s\tat=", caller_id(), flags == AT_REMOVEDIR ? "rmdir" : "unlink", label);
    in_child(pair_body, path, flags, 0);
    printf("\tplain=");
    in_child(pair_body, path, flags, 1);
    printf("\n");
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    started_as_root = geteuid() == 0;
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\tAT_REMOVEDIR=0x%x\n", u.sysname, u.release, u.machine, (int)geteuid(),
           AT_REMOVEDIR);

    const char *paths[] = {
        "f",      "f/",     "nx",      "nx/",     "sub",     "sub/",    "full",   "full/",  "dang",
        "dang/",  "cyc",    "cyc/",    "lf",      "lf/",     "ld",      "ld/",    "lroot",  "lroot/",
        ".",      "..",     "sub/.",   "sub/..",  "lroot/.", "../d",    "../d/f", "../d/sub", "../../w",
        "ro/kid", "ro/kdir", "ro/kfull", "ro/nx",
    };
    const char *sticky[] = {"st/of", "st/od", "st/mine"};
    const char *self[][2] = {{".", "rmdir"}, {"..", "rmdir"}, {".", "unlink"}, {"..", "unlink"},
                             {"x", "unlink"}, {"../f", "unlink"}, {"../e", "rmdir"}};

    int ncallers = started_as_root ? 2 : 1;
    for (int c = 0; c < ncallers; c++) {
        caller = c == 0 ? (started_as_root ? 0 : geteuid()) : 1000;
        for (int k = 0; k < 2; k++) {
            int flags = k == 0 ? 0 : AT_REMOVEDIR;
            for (size_t i = 0; i < sizeof paths / sizeof paths[0]; i++) pair(paths[i], paths[i], flags);
            if (started_as_root)
                for (size_t i = 0; i < sizeof sticky / sizeof sticky[0]; i++) pair(sticky[i], sticky[i], flags);
        }
        for (size_t i = 0; i < sizeof self / sizeof self[0]; i++) {
            int flags = strcmp(self[i][1], "rmdir") == 0 ? AT_REMOVEDIR : 0;
            printf("SELF\tcaller=%d\tthen=%s\t%s\tat=", caller_id(), self[i][1], self[i][0]);
            in_child(self_body, self[i][0], flags, 0);
            printf("\tplain=");
            in_child(self_body, self[i][0], flags, 1);
            printf("\n");
        }
    }

    // FLAGS2, as the first caller only: the screen depends on no credential.
    caller = started_as_root ? 0 : geteuid();
    int words[64];
    int nwords = 0;
    words[nwords++] = AT_REMOVEDIR | 0x1;
    words[nwords++] = AT_REMOVEDIR | 0x40000000;
#ifdef __APPLE__
    const int others[] = {0x100, 0x800, 0x1000, 0x2000, 0x4000, 0x8000};
    for (size_t i = 0; i < sizeof others / sizeof others[0]; i++) {
        words[nwords++] = others[i] | 0x1;
        words[nwords++] = others[i] | 0x40000000;
        words[nwords++] = others[i] | AT_REMOVEDIR;
    }
#else
    // AT_SYMLINK_NOFOLLOW and AT_EMPTY_PATH, which other calls accept.
    words[nwords++] = AT_REMOVEDIR | 0x100;
    words[nwords++] = AT_REMOVEDIR | 0x1000;
#endif
    const char *labels[] = {"f", "sub", "NULL", "minus1+f"};
    for (int w = 0; w < nwords; w++) {
        printf("FLAGS2\tflags=0x%x", (unsigned)words[w]);
        for (int h = 0; h < 4; h++) {
            printf("\t%s=", labels[h]);
            in_child(word_body, NULL, words[w], h);
        }
        printf("\n");
    }
    return 0;
}
