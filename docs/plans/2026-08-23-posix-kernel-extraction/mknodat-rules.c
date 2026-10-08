// Measures what mknodat(2) and mknod(2) do past what at-dirfd.c measured
// (which kinds of dirfd start a walk, and the empty path's place against
// them) and link-symlink.c's MKNOD rows (one answer per file type): whether
// creating a regular file with mknod decides anything differently from
// open(O_CREAT|O_EXCL), and where each file type's answer falls against the
// path's own failures.
//
// Rows:
//   PATH   mknodat(fd on d, path, S_IFREG|0644, 0) with the cwd at d's parent
//          w, mknod(path, S_IFREG|0644, 0) with the cwd at d, and
//          open(path, O_WRONLY|O_CREAT|O_EXCL, 0644) with the cwd at d, each
//          in a fresh cell, for pathnames that reach each rule a creation
//          has: trailing separators on a file, a free name, a dangling link,
//          a link loop, links to a file and to a directory; ".", ".." and
//          climbing out of d; an unwritable directory (0555, holding e/); a
//          set-group-ID directory; a name APFS will not bind.
//   PATHTYPE  mknod(path, type|0644, dev) from d for each PATH pathname and
//          each type other than a regular file's: whether every answer a
//          regular file gets on the way to being created is every type's.
//   MODE   the same three for "nx", for "sg/x" (in the set-group-ID
//          directory) and for "../../xg/x" (in one whose group is never the
//          caller's), under each file type a regular file may be asked for
//          by (S_IFREG and 0), each permission word, and each umask.
//   TYPE   mknod(name, type|0644, dev) from d for every value of the type
//          field, dev 0 and makedev(1,3), onto a free name, an existing file,
//          a free name in the unwritable directory, a free name with a
//          trailing separator and, from a descriptor on a removed directory,
//          a free name: the answer, and what was made.
//   WIDE   mode words with bits set above the sixteen of mode_t: whether
//          anything but the type field and the twelve permission bits is
//          read.
//   ORDER  mknodat with a few file types against a NULL path, a PROT_NONE
//          page, PATH_MAX bytes with no NUL, the empty path, -1 and a file as
//          dirfd: which is answered first.
//   TIMES  what a creating mknod moves, beside what a creating open moves:
//          whether the directory's mtime and ctime moved, and whether the new
//          inode's three times equal each other and the directory's new mtime.
//
// An answer is the errno, or "ok" with what was made: its type ("reg",
// "fifo", "chr", "blk", "sock", "dir", "lnk"), its permission bits, its rdev
// as major,minor where it is not 0, whose gid it took ("parent" for its
// directory's, "egid" for the caller's, "both" when they are the same), and
// every path under the cell that the call created. A byte of a created name
// outside printable ASCII is printed as \xNN.
//
// The cell, with every entry the caller's and the umask 022 unless a MODE
// row says otherwise:
//   w/            the cwd for mknodat
//   w/d/          the directory every call starts from
//     f           a file
//     sub/        a directory
//     dang -> nx2, cyc -> cyc, lf -> f, ld -> sub
//     ro/  (0555) holding e/
//     sg/  (02775; as root its group is 4321, which is not the caller's)
//   xg/           (02777, made before the caller drops, so on Linux it is
//                 root's and of group 4321 for both callers)
//
// Linux, as root, on ext4 (/tmp) and on tmpfs (/dev/shm); each cell drops to
// uid 1000 in its child for the second caller:
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/mknodat-rules.c && /tmp/p "$(mktemp -d)" ext4 && /tmp/p "$(mktemp -d -p /dev/shm)" tmpfs' > mknodat-rules.linux-6.18.5-aarch64.txt
// Darwin, as an ordinary user, on APFS:
//   nix develop -c clang -Wall -Werror -o mknodat-rules mknodat-rules.c && ./mknodat-rules "$(mktemp -d /private/tmp/mknod.XXXXXX)" apfs > mknodat-rules.darwin-27.0-uid501.txt
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/sysmacros.h>
#define MTIM(s) ((s).st_mtim)
#define CTIM(s) ((s).st_ctim)
#define ATIM(s) ((s).st_atim)
#else
#define MTIM(s) ((s).st_mtimespec)
#define CTIM(s) ((s).st_ctimespec)
#define ATIM(s) ((s).st_atimespec)
#endif

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
static char *protnone;
static char *overlong;

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
    // Physical: never follow a link, so a created node shows where it is.
    if (nftw(cell, collect, 16, FTW_PHYS) < 0) die("nftw");
}

// Make the cell, drop to the caller, and leave the cwd at w.
static void fixture(mode_t mask) {
    snprintf(cell, sizeof cell, "%s/c%05d", base, cellno);
    if (mkdir(cell, 0777) < 0 || chmod(cell, 0777) < 0) die("mkdir cell");
    // xg/ is made before the drop, so that as root it is root's, of group
    // 4321, whoever the caller is.
    char xg[PATH_MAX + 4];
    snprintf(xg, sizeof xg, "%s/xg", cell);
    if (mkdir(xg, 0755) < 0) die("mkdir xg");
    if (geteuid() == 0 && chown(xg, (uid_t)-1, 4321) < 0) die("chown xg");
    if (chmod(xg, 02777) < 0) die("chmod xg");
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
    umask(mask);
}

static const char *kind(mode_t m) {
    switch (m & S_IFMT) {
    case S_IFREG: return "reg";
    case S_IFIFO: return "fifo";
    case S_IFCHR: return "chr";
    case S_IFBLK: return "blk";
    case S_IFSOCK: return "sock";
    case S_IFDIR: return "dir";
    case S_IFLNK: return "lnk";
    }
    return "?";
}

static void show(int r, int err, const char *path, int fd) {
    if (r < 0) {
        printf("%s", en(err));
        return;
    }
    char before[MAXSEEN][PATH_MAX];
    int nbefore = nseen;
    memcpy(before, seen, sizeof seen);
    walk();
    printf("ok");
    struct stat st, parent;
    // The new node and the directory holding it, found by name from where
    // the call started.
    char p[PATH_MAX], dir[PATH_MAX];
    snprintf(p, sizeof p, "%s", path);
    size_t n = strlen(p);
    while (n > 1 && p[n - 1] == '/') p[--n] = '\0';
    char *slash = strrchr(p, '/');
    if (slash == NULL)
        snprintf(dir, sizeof dir, ".");
    else
        snprintf(dir, sizeof dir, "%.*s", (int)(slash - p), p);
    if (fstatat(fd, p, &st, AT_SYMLINK_NOFOLLOW) == 0 && fstatat(fd, dir, &parent, 0) == 0) {
        const char *gid = st.st_gid == parent.st_gid ? (st.st_gid == getegid() ? "both" : "parent")
                          : st.st_gid == getegid()   ? "egid"
                                                     : "other";
        printf(":%s:mode=%04o", kind(st.st_mode), (unsigned)(st.st_mode & 07777));
        if (st.st_rdev != 0) printf(":rdev=%u,%u", (unsigned)major(st.st_rdev), (unsigned)minor(st.st_rdev));
        printf(":gid=%s", gid);
    }
    printf(":new=");
    int any = 0;
    for (int i = 0; i < nseen; i++) {
        int found = 0;
        for (int j = 0; j < nbefore; j++)
            if (strcmp(seen[i], before[j]) == 0) found = 1;
        if (!found) {
            printf("%s", any ? "," : "");
            for (const unsigned char *b = (const unsigned char *)seen[i]; *b; b++)
                if (*b < 0x20 || *b >= 0x7f)
                    printf("\\x%02x", *b);
                else
                    putchar(*b);
            any = 1;
        }
    }
}

struct args {
    const char *path;
    unsigned mode;
    unsigned dev;
    mode_t mask;
    // PATH and MODE: 0 mknodat from a descriptor on d, 1 mknod from d as the
    // cwd, 2 open(O_CREAT|O_EXCL) from d as the cwd. TYPE: 0 from d as the
    // cwd, 1 mknodat from a descriptor on a removed directory. ORDER: the
    // dirfd, 0 a directory on d, 1 a file, 2 is -1.
    int how;
};

static void in_child(void (*body)(const struct args *), const struct args *a) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(5);
        body(a);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

static void triple_body(const struct args *a) {
    fixture(a->mask);
    int fd;
    if (a->how == 0) {
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d");
    } else {
        if (chdir("d") < 0) die("chdir d");
        fd = AT_FDCWD;
    }
    walk();
    int r;
    if (a->how == 0)
        r = mknodat(fd, a->path, (mode_t)a->mode, (dev_t)a->dev);
    else if (a->how == 1)
        r = mknod(a->path, (mode_t)a->mode, (dev_t)a->dev);
    else {
        r = open(a->path, O_WRONLY | O_CREAT | O_EXCL, (mode_t)a->mode);
        if (r >= 0) close(r);
    }
    int err = errno;
    show(r, err, a->path, fd);
}

static void type_body(const struct args *a) {
    fixture(022);
    if (chdir("d") < 0) die("chdir d");
    int fd = AT_FDCWD;
    if (a->how == 1) {
        if (mkdir("gone", 0755) < 0) die("mkdir gone");
        fd = open("gone", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open gone");
        if (rmdir("gone") < 0) die("rmdir gone");
    }
    walk();
    int r = mknodat(fd, a->path, (mode_t)a->mode, (dev_t)a->dev);
    int err = errno;
    show(r, err, a->path, fd);
}

static const char *volatile null_path = NULL;

static void order_body(const struct args *a) {
    fixture(022);
    int fd = a->how == 0 ? open("d", O_RDONLY | O_DIRECTORY) : a->how == 1 ? open("d/f", O_RDONLY) : -1;
    if (a->how != 2 && fd < 0) die("open dirfd");
    const char *p = strcmp(a->path, "NULL") == 0       ? null_path
                    : strcmp(a->path, "PROT_NONE") == 0 ? protnone
                    : strcmp(a->path, "overlong") == 0  ? overlong
                                                        : a->path;
    walk();
    int r = mknodat(fd, p, (mode_t)a->mode, (dev_t)a->dev);
    int err = errno;
    show(r, err, p == a->path ? p : "", fd);
}

static long long ns(struct timespec t) { return (long long)t.tv_sec * 1000000000LL + t.tv_nsec; }

static void times_body(const struct args *a) {
    fixture(022);
    if (chdir("d") < 0) die("chdir d");
    struct stat before, after, made;
    if (stat(".", &before) < 0) die("stat d");
    struct timespec pause = {0, 50 * 1000 * 1000};
    nanosleep(&pause, NULL);
    int r;
    if (a->how == 0)
        r = mknod("nx", S_IFREG | 0644, 0);
    else {
        r = open("nx", O_WRONLY | O_CREAT | O_EXCL, 0644);
        if (r >= 0) close(r);
    }
    if (r < 0) {
        printf("%s", en(errno));
        return;
    }
    if (stat(".", &after) < 0 || stat("nx", &made) < 0) die("stat after");
    printf("ok:dir-mtime=%s:dir-ctime=%s:new-a=m=c=%s:new-m=dir-m=%s:size=%lld:nlink=%d",
           ns(MTIM(after)) != ns(MTIM(before)) ? "moved" : "kept",
           ns(CTIM(after)) != ns(CTIM(before)) ? "moved" : "kept",
           ns(ATIM(made)) == ns(MTIM(made)) && ns(MTIM(made)) == ns(CTIM(made)) ? "yes" : "no",
           ns(MTIM(made)) == ns(MTIM(after)) ? "yes" : "no", (long long)made.st_size, (int)made.st_nlink);
}

static int callerid(void) { return (int)(caller == 0 ? geteuid() : caller); }

static void triple(const char *table, const char *fs, const char *label, const char *path, unsigned mode, mode_t mask) {
    printf("%s\t%s\tcaller=%d\t%s\tmode=0%o\tumask=%03o", table, fs, callerid(), label, mode, (unsigned)mask);
    const char *cols[] = {"at", "plain", "open"};
    for (int how = 0; how < 3; how++) {
        // open's mode is the permission bits alone.
        struct args a = {path, how == 2 ? (mode & 07777) : mode, 0, mask, how};
        printf("\t%s=", cols[how]);
        in_child(triple_body, &a);
    }
    printf("\n");
}

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: %s <scratch> <filesystem label>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    const char *fs = argv[2];
    if (chmod(base, 0755) < 0) die("chmod base");
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s\t%s %s %s\teuid=%d\n", fs, u.sysname, u.release, u.machine, (int)geteuid());

    long page = sysconf(_SC_PAGESIZE);
    protnone = mmap(NULL, (size_t)page, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (protnone == MAP_FAILED) die("mmap");
    overlong = malloc(PATH_MAX + 1);
    for (int i = 0; i < PATH_MAX; i++) overlong[i] = (i % 2 == 0) ? 'a' : '/';
    overlong[PATH_MAX] = '\0';

    const char *names[][2] = {
        {"nx", "nx"},       {"nx/", "nx/"},       {"nx//", "nx//"},   {"f", "f"},         {"f/", "f/"},
        {"f/.", "f/."},     {"dang", "dang"},     {"dang/", "dang/"}, {"cyc", "cyc"},     {"cyc/", "cyc/"},
        {"lf", "lf"},       {"lf/", "lf/"},       {"ld", "ld"},       {"ld/", "ld/"},     {"ld/x", "ld/x"},
        {"sub", "sub"},     {"sub/x", "sub/x"},   {"sub/x/y", "sub/x/y"}, {".", "."},     {"..", ".."},
        {"../nx", "../nx"}, {"/", "/"},           {"ro/x", "ro/x"},   {"ro/e", "ro/e"},   {"sg/x", "sg/x"},
        {"xff3", "\xff\xff\xff"}, {"ro/xff3", "ro/\xff\xff\xff"},
    };
    const unsigned perms[] = {0, 0644, 0777, 01777, 02777, 02775, 02755, 04755, 07777};
    const unsigned regtypes[] = {S_IFREG, 0};
    const mode_t masks[] = {022, 010, 0, 077};
    // Every value of the four-bit type field.
    const unsigned types[] = {0, 0010000, 0020000, 0030000, 0040000, 0050000, 0060000, 0070000,
                              0100000, 0110000, 0120000, 0130000, 0140000, 0150000, 0160000, 0170000};
    const char *targets[][2] = {{"nx", "nx"}, {"f", "f"}, {"ro/x", "ro/x"}, {"nx/", "nx/"}};
    const unsigned wide[] = {0x10000 | S_IFREG | 0644, 0x10000 | 0644, 0xffff0000u | S_IFREG | 0644,
                             0x10000 | S_IFIFO | 0644, 0x10000 | S_IFDIR | 0644, 0xffffffffu};
    const unsigned ordertypes[] = {S_IFREG, 0, S_IFIFO, S_IFCHR, S_IFDIR, S_IFLNK, S_IFSOCK, S_IFMT};
    const char *orderpaths[] = {"NULL", "PROT_NONE", "overlong", "", "nx", "f", "ro/x", "nx/"};

    int ncallers = geteuid() == 0 ? 2 : 1;
    for (int c = 0; c < ncallers; c++) {
        caller = c == 0 ? 0 : 1000;
        for (size_t i = 0; i < sizeof names / sizeof names[0]; i++)
            triple("PATH", fs, names[i][0], names[i][1], S_IFREG | 0644, 022);
        const unsigned pathtypes[][2] = {{S_IFIFO, 0}, {S_IFSOCK, 0}, {S_IFCHR, 0x103}, {S_IFBLK, 0x103}, {S_IFCHR, 0}};
        const char *pathtypename[] = {"fifo", "sock", "chr:1,3", "blk:1,3", "chr:0,0"};
        for (size_t i = 0; i < sizeof names / sizeof names[0]; i++) {
            printf("PATHTYPE\t%s\tcaller=%d\t%s", fs, callerid(), names[i][0]);
            for (size_t t = 0; t < sizeof pathtypes / sizeof pathtypes[0]; t++) {
                unsigned dev = pathtypes[t][1] == 0 ? 0 : (unsigned)makedev(1, 3);
                struct args a = {names[i][1], pathtypes[t][0] | 0644, dev, 022, 1};
                printf("\t%s=", pathtypename[t]);
                in_child(triple_body, &a);
            }
            printf("\n");
        }
        for (size_t t = 0; t < sizeof regtypes / sizeof regtypes[0]; t++)
            for (size_t m = 0; m < sizeof masks / sizeof masks[0]; m++)
                for (size_t p = 0; p < sizeof perms / sizeof perms[0]; p++) {
                    triple("MODE", fs, "nx", "nx", regtypes[t] | perms[p], masks[m]);
                    triple("MODE", fs, "sg/x", "sg/x", regtypes[t] | perms[p], masks[m]);
                    triple("MODE", fs, "xg/x", "../../xg/x", regtypes[t] | perms[p], masks[m]);
                }
        for (size_t t = 0; t < sizeof types / sizeof types[0]; t++)
            for (int d = 0; d < 2; d++) {
                unsigned dev = d == 0 ? 0 : (unsigned)makedev(1, 3);
                printf("TYPE\t%s\tcaller=%d\ttype=0%06o\tdev=%s", fs, callerid(), types[t], d == 0 ? "0" : "1,3");
                for (size_t g = 0; g < sizeof targets / sizeof targets[0]; g++) {
                    struct args a = {targets[g][1], types[t] | 0644, dev, 022, 0};
                    printf("\t%s=", targets[g][0]);
                    in_child(type_body, &a);
                }
                struct args orphan = {"nx", types[t] | 0644, dev, 022, 1};
                printf("\torphan:nx=");
                in_child(type_body, &orphan);
                printf("\n");
            }
        for (size_t w = 0; w < sizeof wide / sizeof wide[0]; w++) {
            printf("WIDE\t%s\tcaller=%d\tmode=0x%08x", fs, callerid(), wide[w]);
            struct args a = {"nx", wide[w], 0, 022, 0};
            printf("\tnx=");
            in_child(type_body, &a);
            printf("\n");
        }
        const char *dirfds[] = {"dir", "file", "minus1"};
        for (size_t t = 0; t < sizeof ordertypes / sizeof ordertypes[0]; t++)
            for (int d = 0; d < 3; d++) {
                printf("ORDER\t%s\tcaller=%d\ttype=0%06o\tdirfd=%s", fs, callerid(), ordertypes[t], dirfds[d]);
                for (size_t p = 0; p < sizeof orderpaths / sizeof orderpaths[0]; p++) {
                    // A device's number, so an unprivileged S_IFCHR is not a
                    // whiteout.
                    struct args a = {orderpaths[p], ordertypes[t] | 0644, (unsigned)makedev(1, 3), 022, d};
                    printf("\t%s=", orderpaths[p][0] ? orderpaths[p] : "empty");
                    in_child(order_body, &a);
                }
                printf("\n");
            }
        const char *timecalls[] = {"mknod", "open"};
        for (int how = 0; how < 2; how++) {
            printf("TIMES\t%s\tcaller=%d\t%s=", fs, callerid(), timecalls[how]);
            struct args a = {"nx", 0, 0, 022, how};
            in_child(times_body, &a);
            printf("\n");
        }
    }
    return 0;
}
