// Measures what link(2), linkat(2), symlink(2) and mknod(2) do: which errno
// each refusal carries, whether link follows a symbolic link it is given, what
// a new symbolic link's mode and group are, and what changes on success.
//
// Sections:
//   LINKSRC   link, linkat(0) and linkat(AT_SYMLINK_FOLLOW) from each source
//             kind to a free name "n": the errno, and on success what "n" is
//             (the link itself or its target), the source's st_nlink after,
//             and whether the source's ctime and the directory's mtime moved.
//   LINKDST   link("f", destination) for each destination kind.
//   LINKDEV   link across the root filesystem and /dev, each way.
//   LINKPERM  (non-root, or root dropping to uid 1000 in a child) an
//             unwritable destination directory, an unsearchable source
//             directory, and another user's file (Linux's protected_hardlinks).
//   SYMLINK   symlink(target, name) for each target and name kind, and on
//             success whether anything else appeared.
//   SYMEMPTY  where symlink("", name) succeeds, what walking through it gives.
//   SYMMODE   a new symbolic link's permission bits under each umask.
//   SYMGROUP  a new symbolic link's group in a directory whose group is not
//             the caller's, with and without the set-group-ID bit.
//   MKNOD     mknod for each file type, and mkfifo.
// Each row runs in a forked child, in a fresh directory of its own, under
// alarm(5).
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/link-symlink.c && d=$(mktemp -d) && /tmp/p "$d" && d=$(mktemp -d) && chown 1000:1000 "$d" && setpriv --reuid=1000 --regid=1000 --clear-groups /tmp/p "$d"'
// Darwin: nix develop -c clang -Wall -o link-symlink link-symlink.c && ./link-symlink "$(mktemp -d /private/tmp/linksym.XXXXXX)"
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/sysmacros.h>
#define MTIM(s) ((s).st_mtim)
#define CTIM(s) ((s).st_ctim)
#else
#define MTIM(s) ((s).st_mtimespec)
#define CTIM(s) ((s).st_ctimespec)
#endif

static const char *en(int e) {
    static char buf[32];
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case EEXIST: return "EEXIST";
    case ELOOP: return "ELOOP";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EFAULT: return "EFAULT";
    case EXDEV: return "EXDEV";
    case ENOTEMPTY: return "ENOTEMPTY";
    case EBUSY: return "EBUSY";
    case ENOTSUP: return "ENOTSUP";
    case EMLINK: return "EMLINK";
    case EROFS: return "EROFS";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static void mkfile(const char *p, mode_t mode) {
    int fd = open(p, O_WRONLY | O_CREAT | O_EXCL, mode);
    if (fd < 0) die(p);
    close(fd);
}

static int rc(int r) { return r < 0 ? errno : 0; }

static int newer(struct timespec a, struct timespec b) {
    return a.tv_sec > b.tv_sec || (a.tv_sec == b.tv_sec && a.tv_nsec > b.tv_nsec);
}

// A fresh directory, entered as the cwd, holding:
//   f (file)  g (file)  d/ (dir)  e/ (empty dir)
//   lf -> f   ld -> d   dang -> nx   cyc -> cyc   dl -> nx2
static void fixture(void) {
    char c[PATH_MAX];
    cellno++;
    snprintf(c, sizeof c, "%s/c%05d", base, cellno);
    if (mkdir(c, 0777) < 0) die("mkdir cell");
    if (chdir(c) < 0) die("chdir cell");
    mkfile("f", 0644);
    mkfile("g", 0644);
    if (mkdir("d", 0755) < 0) die("mkdir d");
    if (mkdir("e", 0755) < 0) die("mkdir e");
    if (symlink("f", "lf") < 0) die("lf");
    if (symlink("d", "ld") < 0) die("ld");
    if (symlink("nx", "dang") < 0) die("dang");
    if (symlink("cyc", "cyc") < 0) die("cyc");
    if (symlink("nx2", "dl") < 0) die("dl");
}

typedef void (*body_fn)(const void *);
static void in_child(body_fn body, const void *arg) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        exit(2);
    }
    if (pid == 0) {
        alarm(5);
        body(arg);
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    if (WIFSIGNALED(status)) printf("SIG%d", WTERMSIG(status));
    else if (WEXITSTATUS(status) != 0) printf("EXIT%d", WEXITSTATUS(status));
    // Keep the parent's cell counter in step with the child's.
    cellno++;
}

static const char *kind(const char *p) {
    struct stat s;
    if (lstat(p, &s) < 0) return "absent";
    if (S_ISLNK(s.st_mode)) return "symlink";
    if (S_ISDIR(s.st_mode)) return "dir";
    if (S_ISREG(s.st_mode)) return "file";
    return "other";
}

// ------------------------------------------------------------------ LINKSRC
struct linksrc { const char *src; int how; };
static const char *howname[] = {"link", "linkat(0)", "linkat(FOLLOW)"};
static void linksrc_body(const void *v) {
    const struct linksrc *a = v;
    fixture();
    struct stat src0, dir0, f0;
    int had = lstat(a->src, &src0) == 0;
    stat(".", &dir0);
    lstat("f", &f0);
    struct timespec pause = {0, 30 * 1000 * 1000};
    nanosleep(&pause, NULL);
    int r;
    switch (a->how) {
    case 0: r = rc(link(a->src, "n")); break;
    case 1: r = rc(linkat(AT_FDCWD, a->src, AT_FDCWD, "n", 0)); break;
    default: r = rc(linkat(AT_FDCWD, a->src, AT_FDCWD, "n", AT_SYMLINK_FOLLOW)); break;
    }
    printf("%s", en(r));
    if (r != 0) return;
    struct stat n, src1, dir1, f1;
    lstat("n", &n);
    lstat(a->src, &src1);
    stat(".", &dir1);
    lstat("f", &f1);
    const char *what = (had && n.st_ino == src0.st_ino) ? "the-name-itself" : (n.st_ino == f0.st_ino ? "f" : "other");
    printf("(n=%s:%s nlink=%ld srcctime=%s dirmtime=%s fctime=%s)", kind("n"), what, (long)src1.st_nlink,
           newer(CTIM(src1), CTIM(src0)) ? "moved" : "same", newer(MTIM(dir1), MTIM(dir0)) ? "moved" : "same",
           newer(CTIM(f1), CTIM(f0)) ? "moved" : "same");
}

// ------------------------------------------------------------------ LINKDST
static void linkdst_body(const void *v) {
    const char *dst = v;
    fixture();
    int r = rc(link("f", dst));
    printf("%s", en(r));
    if (r == 0) printf("(nlink-of-f-now-2:%s)", kind(dst));
}

// ------------------------------------------------------------------ LINKDEV
struct pair { const char *a, *b; };
static void linkdev_body(const void *v) {
    const struct pair *p = v;
    fixture();
    printf("%s", en(rc(link(p->a, p->b))));
}

// ------------------------------------------------------------------ LINKPERM
struct linkperm { int row; };
static void linkperm_body(const void *v) {
    const struct linkperm *a = v;
    fixture();
    if (geteuid() == 0) {
        // Root builds the fixture's other-owned file, then drops to uid 1000
        // so the row is asked by an unprivileged caller.
        mkfile("rootfile600", 0600);
        mkfile("rootfile644", 0644);
        mkfile("rootfile666", 0666);
        // The umask has narrowed the modes mkfile asked for.
        chmod("rootfile644", 0644);
        chmod("rootfile666", 0666);
        const char *mine[] = {".", "f", "g", "d", "e"};
        for (int i = 0; i < 5; i++)
            if (chown(mine[i], 1000, 1000) < 0) die("chown");
        if (setgroups(0, NULL) < 0 || setgid(1000) < 0 || setuid(1000) < 0) die("drop");
    }
    switch (a->row) {
    case 0:
        chmod("e", 0555);
        printf("%s", en(rc(link("f", "e/n"))));
        break;
    case 1:
        mkfile("d/x", 0644);
        chmod("d", 0600);
        printf("%s", en(rc(link("d/x", "n"))));
        break;
    case 2:
        mkfile("d/x", 0644);
        chmod("d", 0600);
        printf("%s", en(rc(link("d/nx", "n"))));
        break;
    case 3: printf("%s", en(rc(link("rootfile600", "n")))); break;
    case 4: printf("%s", en(rc(link("rootfile644", "n")))); break;
    case 5: printf("%s", en(rc(link("rootfile666", "n")))); break;
    case 6:
        // The source missing and the destination directory unwritable.
        chmod("e", 0555);
        printf("%s", en(rc(link("nx", "e/n"))));
        break;
    case 7:
        // The destination existing and its directory unwritable.
        mkfile("e/n", 0644);
        chmod("e", 0555);
        printf("%s", en(rc(link("f", "e/n"))));
        break;
    }
}
static const char *linkpermname[] = {
    "dest-dir-0555",       "src-dir-0600",           "src-dir-0600-absent", "others-file-0600",
    "others-file-0644",    "others-file-0666",       "absent-src+dest-dir-0555", "existing-dest+dest-dir-0555",
};

// ------------------------------------------------------------------ SYMLINK
static char longtarget[PATH_MAX + 2];
static char fulltarget[PATH_MAX + 2];
struct symlinkrow { const char *target, *name; };
static void symlink_body(const void *v) {
    const struct symlinkrow *a = v;
    fixture();
    int r = rc(symlink(a->target, a->name));
    printf("%s", en(r));
    // Did a name appear that the caller did not ask for (a dangling link's
    // target, say)?
    printf("(nx2=%s nx=%s n=%s)", kind("nx2"), kind("nx"), kind("n"));
    if (r == 0) {
        char buf[PATH_MAX + 2];
        const char *name = a->name;
        char trimmed[PATH_MAX];
        size_t len = strlen(name);
        if (len > 0 && name[len - 1] == '/') {
            memcpy(trimmed, name, len - 1);
            trimmed[len - 1] = 0;
            name = trimmed;
        }
        ssize_t got = readlink(name, buf, sizeof buf);
        struct stat s;
        lstat(name, &s);
        printf("(readlink=%zd size=%lld)", got, (long long)s.st_size);
    }
}

// ------------------------------------------------------------------ SYMEMPTY
// Where a link with an empty target can be made at all: what each walk
// through it answers.
static void symempty_body(const void *v) {
    (void)v;
    fixture();
    int r = rc(symlink("", "n"));
    printf("create=%s", en(r));
    if (r != 0) return;
    struct stat s;
    printf(" stat=%s", en(rc(stat("n", &s))));
    printf(" lstat=%s", en(rc(lstat("n", &s))));
    printf(" stat(n/)=%s", en(rc(stat("n/", &s))));
    printf(" stat(n/x)=%s", en(rc(stat("n/x", &s))));
    printf(" open=%s", en(rc(open("n", O_RDONLY))));
    printf(" open(O_CREAT)=%s", en(rc(open("n", O_RDONLY | O_CREAT, 0644))));
    printf(" mkdir(n/)=%s", en(rc(mkdir("n/", 0755))));
    printf(" chdir=%s", en(rc(chdir("n"))));
}

// ------------------------------------------------------------------ SYMMODE
static void symmode_body(const void *v) {
    mode_t mask = *(const mode_t *)v;
    fixture();
    umask(mask);
    int r = rc(symlink("t", "n"));
    struct stat s;
    lstat("n", &s);
    printf("%s mode=0%o", en(r), (unsigned)(s.st_mode & 07777));
}

// ------------------------------------------------------------------ SYMGROUP
static void symgroup_body(const void *v) {
    int setgid_bit = *(const int *)v;
    fixture();
    gid_t other = (gid_t)-1;
    if (geteuid() == 0) {
        other = 1234;
    } else {
        gid_t groups[64];
        int n = getgroups(64, groups);
        for (int i = 0; i < n; i++)
            if (groups[i] != getegid()) {
                other = groups[i];
                break;
            }
    }
    if (other == (gid_t)-1) {
        printf("no-other-group");
        return;
    }
    if (chown("e", (uid_t)-1, other) < 0) die("chown e");
    if (chmod("e", setgid_bit ? 02777 : 0777) < 0) die("chmod e");
    struct stat de;
    stat("e", &de);
    int r = rc(symlink("t", "e/n"));
    struct stat s;
    lstat("e/n", &s);
    mkfile("e/file", 0644);
    struct stat sf;
    lstat("e/file", &sf);
    printf("%s dir-gid=%d dir-mode=0%o egid=%d link-gid=%d file-gid=%d", en(r), (int)de.st_gid, (unsigned)(de.st_mode & 07777),
           (int)getegid(), (int)s.st_gid, (int)sf.st_gid);
}

// ------------------------------------------------------------------ MKNOD
struct mknodrow { const char *name; mode_t mode; int fifo; };
static void mknod_body(const void *v) {
    const struct mknodrow *a = v;
    fixture();
    int r;
    if (a->fifo) r = rc(mkfifo("n", 0644));
    else r = rc(mknod("n", a->mode, makedev(1, 3)));
    struct stat s;
    int there = lstat("n", &s) == 0;
    printf("%s", en(r));
    if (there) printf("(type=0%o perm=0%o rdev=%lld)", (unsigned)(s.st_mode & S_IFMT), (unsigned)(s.st_mode & 07777), (long long)s.st_rdev);
    int r2 = a->fifo ? rc(mkfifo("f", 0644)) : rc(mknod("f", a->mode, makedev(1, 3)));
    printf(" onto-existing=%s", en(r2));
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <empty scratch directory>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    alarm(600);
    umask(022);
    struct utsname u;
    uname(&u);
    printf("UNAME\t%s %s %s\teuid=%d egid=%d\n", u.sysname, u.release, u.machine, (int)geteuid(), (int)getegid());
#ifdef __linux__
    {
        const char *names[] = {"/proc/sys/fs/protected_hardlinks", "/proc/sys/fs/protected_symlinks"};
        for (int i = 0; i < 2; i++) {
            FILE *fp = fopen(names[i], "r");
            int v = -1;
            if (fp) {
                if (fscanf(fp, "%d", &v) != 1) v = -1;
                fclose(fp);
            }
            printf("SYSCTL\t%s=%d\n", names[i], v);
        }
    }
#endif

    const char *sources[] = {"f", "d", "lf", "ld", "dang", "cyc", "nx", "f/", "lf/", "ld/", "d/"};
    for (size_t s = 0; s < sizeof sources / sizeof sources[0]; s++) {
        printf("LINKSRC\t%s", sources[s]);
        for (int how = 0; how < 3; how++) {
            struct linksrc a = {sources[s], how};
            printf("\t%s=", howname[how]);
            in_child(linksrc_body, &a);
        }
        printf("\n");
    }

    const char *dests[] = {"n", "g", "dl", "e", "lf", "n/", "g/", "e/", "dl/", "nxdir/n", "g/n", "d/n", "ld/n", "."};
    for (size_t d = 0; d < sizeof dests / sizeof dests[0]; d++) {
        printf("LINKDST\t%s\t", dests[d]);
        in_child(linkdst_body, dests[d]);
        printf("\n");
    }

    struct pair devs[] = {
        {"/dev/null", "n"}, {"f", "/dev/n"}, {"nx", "/dev/n"}, {"f", "/dev/null"}, {"/dev/null", "/dev/n"}, {"/dev/nx", "n"},
    };
    for (size_t i = 0; i < sizeof devs / sizeof devs[0]; i++) {
        printf("LINKDEV\t%s -> %s\t", devs[i].a, devs[i].b);
        in_child(linkdev_body, &devs[i]);
        printf("\n");
        // A root run can create /dev/n; no later row may see it.
        unlink("/dev/n");
    }

    for (int row = 0; row < 8; row++) {
        struct linkperm a = {row};
        printf("LINKPERM\t%s\t", linkpermname[row]);
        in_child(linkperm_body, &a);
        printf("\n");
    }

    memset(longtarget, 'a', PATH_MAX - 1);
    longtarget[PATH_MAX - 1] = 0;
    memset(fulltarget, 'a', PATH_MAX);
    fulltarget[PATH_MAX] = 0;
    struct {
        const char *label;
        struct symlinkrow row;
    } syms[] = {
        {"t -> n", {"t", "n"}},
        {"'' -> n", {"", "n"}},
        {"PATH_MAX-1 bytes -> n", {longtarget, "n"}},
        {"PATH_MAX bytes -> n", {fulltarget, "n"}},
        {"t -> f", {"t", "f"}},
        {"t -> dl (dangling)", {"t", "dl"}},
        {"t -> lf", {"t", "lf"}},
        {"t -> d", {"t", "d"}},
        {"t -> n/", {"t", "n/"}},
        {"t -> dl/ (dangling)", {"t", "dl/"}},
        {"t -> f/", {"t", "f/"}},
        {"t -> d/", {"t", "d/"}},
        {"t -> nxdir/n", {"t", "nxdir/n"}},
        {"t -> f/n", {"t", "f/n"}},
        {"t -> ld/n", {"t", "ld/n"}},
        {"t -> .", {"t", "."}},
        {"t -> ''", {"t", ""}},
    };
    for (size_t i = 0; i < sizeof syms / sizeof syms[0]; i++) {
        printf("SYMLINK\t%s\t", syms[i].label);
        in_child(symlink_body, &syms[i].row);
        printf("\n");
    }

    printf("SYMEMPTY\t");
    in_child(symempty_body, NULL);
    printf("\n");

    mode_t masks[] = {0, 022, 077, 0777, 0700};
    for (size_t i = 0; i < sizeof masks / sizeof masks[0]; i++) {
        printf("SYMMODE\tumask=0%o\t", (unsigned)masks[i]);
        in_child(symmode_body, &masks[i]);
        printf("\n");
    }

    for (int sg = 0; sg < 2; sg++) {
        printf("SYMGROUP\tsetgid=%d\t", sg);
        in_child(symgroup_body, &sg);
        printf("\n");
    }

    struct mknodrow nodes[] = {
        {"S_IFREG", S_IFREG | 0644, 0}, {"type0", 0644, 0},           {"S_IFIFO", S_IFIFO | 0644, 0},
        {"S_IFCHR", S_IFCHR | 0644, 0}, {"S_IFBLK", S_IFBLK | 0644, 0}, {"S_IFDIR", S_IFDIR | 0755, 0},
        {"S_IFLNK", S_IFLNK | 0777, 0}, {"S_IFSOCK", S_IFSOCK | 0644, 0}, {"S_IFMT", S_IFMT | 0644, 0},
        {"mkfifo", 0, 1},
    };
    for (size_t i = 0; i < sizeof nodes / sizeof nodes[0]; i++) {
        printf("MKNOD\t%s\t", nodes[i].name);
        in_child(mknod_body, &nodes[i]);
        printf("\n");
    }
    return 0;
}
