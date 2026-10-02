// Measures the struct stat fields beyond those the PAL carries: st_nlink,
// st_rdev, st_blksize, st_blocks, and Darwin's st_flags, for every kind of
// object the kernel model has (regular file, directory, symbolic link, pipe)
// and, for the record, the descriptor kinds it refuses to fstat (sockets, an
// epoll or kqueue descriptor).
//
// Usage: stat-fields <scratch-dir> [<fresh-root>]
//   <scratch-dir>  an empty directory on the filesystem under test
//   <fresh-root>   optionally, the root of a freshly made filesystem of the
//                  same type, which the probe populates to measure a root
//                  directory's count
//
// Linux (tmpfs): the scratch directory must be under /dev/shm, never under the
// bind mount (virtiofs). A fresh tmpfs root needs CAP_SYS_ADMIN:
//   container run --rm --cap-add ALL --shm-size 2G -v "$PWD":/probe gcc:14 sh -c
//     'gcc -Wall -O1 -o /tmp/p /probe/stat-fields.c && mkdir /mnt/t && mount -t tmpfs none /mnt/t
//      && /tmp/p "$(mktemp -d -p /dev/shm)" /mnt/t'
// Darwin (APFS): a fresh root is an APFS disk image:
//   hdiutil create -size 64m -fs APFS -volname StatFields /tmp/sf.dmg && hdiutil attach /tmp/sf.dmg
//   nix develop -c clang -Wall -o stat-fields stat-fields.c
//   ./stat-fields "$(mktemp -d /private/tmp/stat-fields.XXXXXX)" /Volumes/StatFields
//
// The sweep:
//  - a regular file with 1, 2 and 3 names, its count read through every name
//    and a held descriptor as names are added and removed, down to 0 with the
//    descriptor still open; the same for a symbolic link hard-linked with
//    linkat(..., 0);
//  - a directory's count against its number of names and of subdirectories,
//    over a seeded random history of 4000 steps (creat, mkdir, symlink, link,
//    mkfifo, unlink, rmdir, and rename within, into, out of and over the
//    directory under test, including directories moved between it and a
//    sibling), each step read through stat and through a held descriptor;
//    a candidate rule is reported as holding only if it held at every step;
//  - a directory removed by rmdir, and one displaced by rename, each while a
//    descriptor holds it; and one that held subdirectories until just before;
//  - a fresh filesystem's root, as subdirectories and files are added;
//  - st_rdev and (Darwin) st_flags for every kind;
//  - st_blksize and st_blocks for regular files of every size from 0 to 70000
//    bytes written with one write(2) (reporting only where the pair changes),
//    1 MiB, files extended by ftruncate or by a write past the end, a file
//    shrunk by ftruncate, before and after fsync; directories of 0, 100 and
//    1000 names; symbolic links with targets of 1 to 4095 bytes.
//
// Measured on Linux 6.18.5 (aarch64, 4 KiB pages, root in the container,
// tmpfs) and Darwin 27.0 (arm64, uid 501, APFS) on 2026-10-02; the output of
// each is beside this file. stat-nlink-limit.c measures the count past 65535.
//
// What they say:
//  - a regular file's or a symbolic link's count is its number of names, on
//    both, and 0 once the last has gone while a descriptor holds it;
//  - a directory's count is 2 plus its subdirectories on tmpfs, and 2 plus all
//    of its names on APFS, at every one of the 4000 steps, its root included
//    (an APFS volume's root holds .fseventsd); a symbolic link to a directory
//    is not a subdirectory;
//  - a directory removed by rmdir or displaced by rename reports 0 on tmpfs
//    and 2 on APFS (which is 2 plus its names, since it is empty);
//  - a pipe reports 1 on Linux and 0 on Darwin;
//  - st_rdev is 0 for everything but the device node, and st_flags is 0 for
//    every fresh object on Darwin, dot-files included (chflags(UF_HIDDEN) does
//    set it, so the field is live);
//  - st_blksize is 4096 for files, directories and symbolic links on both;
//    a pipe's is 4096 on Linux and 16384 on Darwin;
//  - st_blocks counts the 512-byte units of the pages or blocks allocated,
//    which a file's size does not determine: a file extended by ftruncate has
//    none, one written a byte at 1 MiB has 8 (and on APFS 2056 once it has
//    been fsync'd), a tmpfs symbolic link of 128 bytes or more has 8, and a
//    directory has 0.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#include <sys/statfs.h>
#else
#include <sys/event.h>
#endif

static char base[PATH_MAX];

static void die(const char *what) {
    printf("FATAL\t%s\t%s\n", what, strerror(errno));
    exit(1);
}

static void at(char *out, const char *dir, const char *name) {
    snprintf(out, PATH_MAX, "%s/%s", dir, name);
}

static const char *kind(mode_t m) {
    switch (m & S_IFMT) {
    case S_IFREG: return "reg";
    case S_IFDIR: return "dir";
    case S_IFLNK: return "lnk";
    case S_IFIFO: return "fifo";
    case S_IFSOCK: return "sock";
    case S_IFCHR: return "chr";
    case S_IFBLK: return "blk";
    case 0: return "none";
    default: return "?";
    }
}

static void row(const char *label, const struct stat *st) {
    printf("ROW\t%s\tkind=%s\tnlink=%lu\trdev=%llu\tsize=%lld\tblksize=%ld\tblocks=%lld",
           label, kind(st->st_mode), (unsigned long)st->st_nlink, (unsigned long long)st->st_rdev,
           (long long)st->st_size, (long)st->st_blksize, (long long)st->st_blocks);
#ifdef __APPLE__
    printf("\tflags=0x%x", (unsigned)st->st_flags);
#endif
    printf("\n");
}

static void lrow(const char *label, const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) { printf("ROW\t%s\tlstat=%s\n", label, strerror(errno)); return; }
    row(label, &st);
}

static void frow(const char *label, int fd) {
    struct stat st;
    if (fstat(fd, &st) != 0) { printf("ROW\t%s\tfstat=%s\n", label, strerror(errno)); return; }
    row(label, &st);
}

static unsigned long nlinkOf(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) die(path);
    return (unsigned long)st.st_nlink;
}

static unsigned long fnlinkOf(int fd) {
    struct stat st;
    if (fstat(fd, &st) != 0) die("fstat");
    return (unsigned long)st.st_nlink;
}

static void header(void) {
    struct utsname u;
    uname(&u);
    struct statfs sf;
    if (statfs(base, &sf) != 0) die("statfs");
#ifdef __linux__
    printf("KERNEL\t%s %s %s\tuid=%d\tfs_type=0x%lx\tpagesize=%ld\n", u.sysname, u.release, u.machine, (int)getuid(),
           (long)sf.f_type, sysconf(_SC_PAGESIZE));
#else
    printf("KERNEL\t%s %s %s\tuid=%d\tfs_type=%s\tpagesize=%ld\n", u.sysname, u.release, u.machine, (int)getuid(),
           sf.f_fstypename, sysconf(_SC_PAGESIZE));
#endif
}

static void writeAll(int fd, const char *buf, size_t n) {
    while (n > 0) {
        ssize_t w = write(fd, buf, n);
        if (w <= 0) die("write");
        buf += w; n -= (size_t)w;
    }
}

// ---------------------------------------------------------------- regular files and symlinks

static void hardLinks(void) {
    char d[PATH_MAX], a[PATH_MAX], b[PATH_MAX], c[PATH_MAX];
    at(d, base, "links");
    if (mkdir(d, 0777) != 0) die("mkdir links");
    at(a, d, "a"); at(b, d, "b"); at(c, d, "c");
    int fd = open(a, O_CREAT | O_EXCL | O_RDWR, 0644);
    if (fd < 0) die("creat a");
    printf("NLINK\treg\tnames=1\tvia_a=%lu\tvia_fd=%lu\n", nlinkOf(a), fnlinkOf(fd));
    if (link(a, b) != 0) die("link b");
    printf("NLINK\treg\tnames=2\tvia_a=%lu\tvia_b=%lu\tvia_fd=%lu\n", nlinkOf(a), nlinkOf(b), fnlinkOf(fd));
    if (link(b, c) != 0) die("link c");
    printf("NLINK\treg\tnames=3\tvia_a=%lu\tvia_b=%lu\tvia_c=%lu\tvia_fd=%lu\n", nlinkOf(a), nlinkOf(b), nlinkOf(c),
           fnlinkOf(fd));
    // A rename over one of its own other names is the no-op: both names stay.
    if (rename(a, b) != 0) die("rename a b");
    printf("NLINK\treg\tafter_rename_onto_own_link\tvia_a=%lu\tvia_fd=%lu\n", nlinkOf(a), fnlinkOf(fd));
    if (unlink(a) != 0) die("unlink a");
    printf("NLINK\treg\tnames=2_after_unlink\tvia_b=%lu\tvia_fd=%lu\n", nlinkOf(b), fnlinkOf(fd));
    // Rename another file over one of its names: that name is displaced.
    char o[PATH_MAX];
    at(o, d, "other");
    int ofd = open(o, O_CREAT | O_EXCL | O_RDWR, 0644);
    if (ofd < 0) die("creat other");
    close(ofd);
    if (rename(o, c) != 0) die("rename other c");
    printf("NLINK\treg\tnames=1_after_displacement\tvia_b=%lu\tvia_fd=%lu\n", nlinkOf(b), fnlinkOf(fd));
    if (unlink(b) != 0) die("unlink b");
    printf("NLINK\treg\tnames=0_held_open\tvia_fd=%lu\n", fnlinkOf(fd));
    frow("reg_unlinked_held_open", fd);
    close(fd);

    char l[PATH_MAX], l2[PATH_MAX];
    at(l, d, "l"); at(l2, d, "l2");
    if (symlink("target", l) != 0) die("symlink");
    printf("NLINK\tlnk\tnames=1\tvia_l=%lu\n", nlinkOf(l));
    if (linkat(AT_FDCWD, l, AT_FDCWD, l2, 0) != 0) {
        printf("NLINK\tlnk\tlinkat=%s\n", strerror(errno));
    } else {
        struct stat st;
        lstat(l2, &st);
        printf("NLINK\tlnk\tnames=2\tvia_l=%lu\tvia_l2=%lu\tl2_kind=%s\n", nlinkOf(l), nlinkOf(l2), kind(st.st_mode));
    }
}

// ---------------------------------------------------------------- directory history

static unsigned long long rng;
static unsigned rnd(void) {
    rng ^= rng << 13; rng ^= rng >> 7; rng ^= rng << 17;
    return (unsigned)(rng >> 11);
}

// Counts the names in `dir` other than "." and "..", and how many of them are
// directories (by lstat, so a symlink to a directory is not one).
static void census(const char *dir, int *names, int *subdirs) {
    DIR *dp = opendir(dir);
    if (!dp) die("opendir");
    *names = 0; *subdirs = 0;
    struct dirent *e;
    while ((e = readdir(dp)) != NULL) {
        if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
        (*names)++;
        char p[PATH_MAX];
        at(p, dir, e->d_name);
        struct stat st;
        if (lstat(p, &st) == 0 && S_ISDIR(st.st_mode)) (*subdirs)++;
    }
    closedir(dp);
}

// Pick a name in `dir` (other than the dots) uniformly, or return 0.
static int pickName(const char *dir, char *out) {
    DIR *dp = opendir(dir);
    if (!dp) die("opendir pick");
    char names[512][32];
    int n = 0;
    struct dirent *e;
    while ((e = readdir(dp)) != NULL && n < 512) {
        if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
        snprintf(names[n++], 32, "%s", e->d_name);
    }
    closedir(dp);
    if (n == 0) return 0;
    strcpy(out, names[rnd() % n]);
    return 1;
}

static void freshName(char *out) {
    snprintf(out, 32, "n%u", rnd() % 100000);
}

static void directoryHistory(unsigned seed, int steps) {
    char d[PATH_MAX], s[PATH_MAX], name[32], p[PATH_MAX], q[PATH_MAX];
    snprintf(d, sizeof d, "%s/hist%u", base, seed);
    snprintf(s, sizeof s, "%s/side%u", base, seed);
    if (mkdir(d, 0777) != 0 || mkdir(s, 0777) != 0) die("mkdir hist");
    int dfd = open(d, O_RDONLY | O_DIRECTORY);
    if (dfd < 0) die("open hist");
    rng = 0x9E3779B97F4A7C15ULL ^ seed;
    long observations = 0, subdirRule = 0, namesRule = 0, viaFdAgrees = 0;
    long maxNames = 0, maxSubdirs = 0;
    int printed = 0;
    for (int i = 0; i < steps; i++) {
        unsigned op = rnd() % 11;
        const char *dir = (rnd() % 4 == 0) ? s : d;
        const char *other = dir == d ? s : d;
        freshName(name);
        at(p, dir, name);
        switch (op) {
        case 0: case 1: { int fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644); if (fd >= 0) close(fd); break; }
        case 2: case 3: mkdir(p, 0777); break;
        case 4: symlink(rnd() % 2 ? "x" : ".", p); break;
        case 5: mkfifo(p, 0644); break;
        case 6: if (pickName(dir, name)) { at(q, dir, name); char n2[32]; freshName(n2); at(p, dir, n2); link(q, p); } break;
        case 7: if (pickName(dir, name)) { at(q, dir, name); unlink(q); } break;
        case 8: if (pickName(dir, name)) { at(q, dir, name); rmdir(q); } break;
        case 9: // rename within, or over an existing name
            if (pickName(dir, name)) {
                at(q, dir, name);
                char n2[32];
                if (rnd() % 2 && pickName(dir, n2)) at(p, dir, n2); else { freshName(n2); at(p, dir, n2); }
                rename(q, p);
            }
            break;
        case 10: // rename into or out of the directory under test
            if (pickName(dir, name)) {
                at(q, dir, name);
                char n2[32];
                if (rnd() % 2 && pickName(other, n2)) at(p, other, n2); else { freshName(n2); at(p, other, n2); }
                rename(q, p);
            }
            break;
        }
        int names, subdirs;
        census(d, &names, &subdirs);
        unsigned long viaPath = nlinkOf(d), viaFd = fnlinkOf(dfd);
        observations++;
        if (viaPath == (unsigned long)(2 + subdirs)) subdirRule++;
        if (viaPath == (unsigned long)(2 + names)) namesRule++;
        if (viaPath == viaFd) viaFdAgrees++;
        if (names > maxNames) maxNames = names;
        if (subdirs > maxSubdirs) maxSubdirs = subdirs;
        if ((viaPath != (unsigned long)(2 + subdirs) && viaPath != (unsigned long)(2 + names)) && printed < 10) {
            printf("DIRMISS\tseed=%u\tstep=%d\tnames=%d\tsubdirs=%d\tnlink=%lu\n", seed, i, names, subdirs, viaPath);
            printed++;
        }
    }
    printf("DIRHIST\tseed=%u\tsteps=%d\tmax_names=%ld\tmax_subdirs=%ld\tnlink==2+subdirs:%ld/%ld\tnlink==2+names:%ld/%ld\tfstat==stat:%ld/%ld\n",
           seed, steps, maxNames, maxSubdirs, subdirRule, observations, namesRule, observations, viaFdAgrees,
           observations);
    close(dfd);
}

static void directoryShapes(void) {
    char d[PATH_MAX], p[PATH_MAX];
    at(d, base, "shapes");
    if (mkdir(d, 0777) != 0) die("mkdir shapes");
    printf("DIR\tempty\tnlink=%lu\n", nlinkOf(d));
    at(p, d, "f"); close(open(p, O_CREAT | O_WRONLY, 0644));
    printf("DIR\t1_file\tnlink=%lu\n", nlinkOf(d));
    at(p, d, "s"); mkdir(p, 0777);
    printf("DIR\t1_file_1_subdir\tnlink=%lu\n", nlinkOf(d));
    at(p, d, "s/inner"); mkdir(p, 0777);
    at(p, d, "s/innerfile"); close(open(p, O_CREAT | O_WRONLY, 0644));
    printf("DIR\t1_file_1_subdir_with_contents\tnlink=%lu\tsubdir_nlink=%lu\n", nlinkOf(d), (at(p, d, "s"), nlinkOf(p)));
    at(p, d, "l"); symlink("s", p);
    printf("DIR\t+symlink_to_subdir\tnlink=%lu\n", nlinkOf(d));
    at(p, d, "fifo"); mkfifo(p, 0644);
    printf("DIR\t+fifo\tnlink=%lu\n", nlinkOf(d));
    at(p, d, "sock");
    int us = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un sun;
    memset(&sun, 0, sizeof sun);
    sun.sun_family = AF_UNIX;
    snprintf(sun.sun_path, sizeof sun.sun_path, "%s", p);
    if (strlen(p) < sizeof sun.sun_path && bind(us, (struct sockaddr *)&sun, sizeof sun) == 0)
        printf("DIR\t+socket_node\tnlink=%lu\n", nlinkOf(d));
    else
        printf("DIR\t+socket_node\tbind=%s\n", strerror(errno));
    close(us);

    // A removed directory, through a held descriptor.
    char r[PATH_MAX];
    at(r, base, "removed");
    mkdir(r, 0777);
    int rfd = open(r, O_RDONLY | O_DIRECTORY);
    printf("ORPHAN\trmdir\tbefore=%lu\t", fnlinkOf(rfd));
    if (rmdir(r) != 0) die("rmdir removed");
    printf("after=%lu\n", fnlinkOf(rfd));
    close(rfd);

    // A directory that held a subdirectory until just before its rmdir.
    at(r, base, "removed2");
    mkdir(r, 0777);
    at(p, r, "child"); mkdir(p, 0777);
    rfd = open(r, O_RDONLY | O_DIRECTORY);
    printf("ORPHAN\trmdir_after_child_removed\twith_child=%lu\t", fnlinkOf(rfd));
    rmdir(p);
    printf("child_removed=%lu\t", fnlinkOf(rfd));
    if (rmdir(r) != 0) die("rmdir removed2");
    printf("after=%lu\n", fnlinkOf(rfd));
    close(rfd);

    // A directory displaced by rename, through a held descriptor.
    char src[PATH_MAX], dst[PATH_MAX];
    at(src, base, "mover"); at(dst, base, "displaced");
    mkdir(src, 0777); mkdir(dst, 0777);
    int xfd = open(dst, O_RDONLY | O_DIRECTORY);
    printf("ORPHAN\trename_displaced\tbefore=%lu\t", fnlinkOf(xfd));
    if (rename(src, dst) != 0) die("rename displace");
    printf("after=%lu\tsurvivor=%lu\n", fnlinkOf(xfd), nlinkOf(dst));
    frow("dir_displaced_held_open", xfd);
    close(xfd);

    // A directory held as the cwd while removed.
    char cw[PATH_MAX];
    at(cw, base, "cwdgone");
    mkdir(cw, 0777);
    int here = open(".", O_RDONLY | O_DIRECTORY);
    if (chdir(cw) != 0) die("chdir");
    rmdir(cw);
    struct stat st;
    if (stat(".", &st) == 0) printf("ORPHAN\tcwd_rmdir\tstat_dot=%lu\n", (unsigned long)st.st_nlink);
    else printf("ORPHAN\tcwd_rmdir\tstat_dot=%s\n", strerror(errno));
    fchdir(here);
    close(here);
}

static void freshRoot(const char *root) {
    int names, subdirs;
    census(root, &names, &subdirs);
    printf("ROOT\tinitial\tnames=%d\tsubdirs=%d\tnlink=%lu\n", names, subdirs, nlinkOf(root));
    char p[PATH_MAX];
    for (int i = 0; i < 3; i++) {
        char n[32];
        snprintf(n, sizeof n, "sfdir%d", i);
        at(p, root, n);
        mkdir(p, 0777);
        census(root, &names, &subdirs);
        printf("ROOT\t+dir\tnames=%d\tsubdirs=%d\tnlink=%lu\n", names, subdirs, nlinkOf(root));
    }
    for (int i = 0; i < 3; i++) {
        char n[32];
        snprintf(n, sizeof n, "sffile%d", i);
        at(p, root, n);
        close(open(p, O_CREAT | O_WRONLY, 0644));
        census(root, &names, &subdirs);
        printf("ROOT\t+file\tnames=%d\tsubdirs=%d\tnlink=%lu\n", names, subdirs, nlinkOf(root));
    }
    lrow("root", root);
}

// ---------------------------------------------------------------- rdev and flags of every kind

static void everyKind(void) {
    char d[PATH_MAX], p[PATH_MAX];
    at(d, base, "kinds");
    mkdir(d, 0777);
    at(p, d, "file"); close(open(p, O_CREAT | O_WRONLY, 0644)); lrow("reg_fresh", p);
    at(p, d, ".dotfile"); close(open(p, O_CREAT | O_WRONLY, 0644)); lrow("reg_dotfile_fresh", p);
    lrow("dir_fresh_empty", d);
    at(p, d, "sub"); mkdir(p, 0777); lrow("dir_fresh_sub", p);
    at(p, d, "link"); symlink("file", p); lrow("lnk_fresh", p);
    at(p, d, "fifo"); mkfifo(p, 0644); lrow("fifo_node_fresh", p);
    lrow("dev_null", "/dev/null");
    lrow("base", base);

#ifdef __APPLE__
    // Control: chflags must be able to set UF_HIDDEN, or a zero above proves
    // nothing about the field.
    at(p, d, "file");
    if (chflags(p, UF_HIDDEN) != 0) printf("ROW\tchflags_UF_HIDDEN\t%s\n", strerror(errno));
    lrow("reg_after_chflags_UF_HIDDEN", p);
    chflags(p, 0);
#endif

    int pfd[2];
    if (pipe(pfd) != 0) die("pipe");
    frow("pipe_read_end", pfd[0]);
    frow("pipe_write_end", pfd[1]);
    close(pfd[1]);
    frow("pipe_read_end_writer_closed", pfd[0]);
    close(pfd[0]);

    int s = socket(AF_INET, SOCK_STREAM, 0); frow("socket_inet_stream", s); close(s);
    s = socket(AF_INET, SOCK_DGRAM, 0); frow("socket_inet_dgram", s); close(s);
    s = socket(AF_UNIX, SOCK_STREAM, 0); frow("socket_unix_stream", s); close(s);
#ifdef __linux__
    int e = epoll_create1(0); frow("epoll", e); close(e);
#else
    int k = kqueue(); frow("kqueue", k); close(k);
#endif
}

// ---------------------------------------------------------------- block accounting

static char buffer[1 << 20];

static void blocksOfSizes(void) {
    char d[PATH_MAX], p[PATH_MAX];
    at(d, base, "sizes");
    mkdir(d, 0777);
    memset(buffer, 'x', sizeof buffer);
    long lastBlksize = -1;
    long long lastBlocks = -1;
    for (int size = 0; size <= 70000; size++) {
        snprintf(p, sizeof p, "%s/s%d", d, size);
        int fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644);
        if (fd < 0) die("creat size");
        if (size > 0) writeAll(fd, buffer, (size_t)size);
        struct stat st;
        fstat(fd, &st);
        if ((long)st.st_blksize != lastBlksize || (long long)st.st_blocks != lastBlocks) {
            printf("BLOCKS\tsize=%d\tblksize=%ld\tblocks=%lld\n", size, (long)st.st_blksize, (long long)st.st_blocks);
            lastBlksize = (long)st.st_blksize;
            lastBlocks = (long long)st.st_blocks;
        }
        close(fd);
        unlink(p);
    }
    printf("BLOCKS\tswept sizes 0..70000, one write(2) each, fstat before close\n");

    int sizes[] = { 0, 1, 4095, 4096, 4097, 1 << 20 };
    for (unsigned i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        snprintf(p, sizeof p, "%s/f%d", d, sizes[i]);
        int fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644);
        if (sizes[i] > 0) writeAll(fd, buffer, (size_t)sizes[i]);
        char label[64];
        snprintf(label, sizeof label, "reg_size_%d_nosync", sizes[i]);
        frow(label, fd);
        fsync(fd);
        snprintf(label, sizeof label, "reg_size_%d_fsync", sizes[i]);
        frow(label, fd);
        close(fd);
        snprintf(label, sizeof label, "reg_size_%d_closed", sizes[i]);
        lrow(label, p);
    }

    at(p, d, "truncated_up");
    int fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644);
    ftruncate(fd, 1 << 20);
    frow("reg_ftruncate_to_1MiB", fd);
    close(fd);

    at(p, d, "hole");
    fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644);
    pwrite(fd, "x", 1, 1 << 20);
    frow("reg_one_byte_at_1MiB", fd);
    fsync(fd);
    frow("reg_one_byte_at_1MiB_fsync", fd);
    close(fd);

    at(p, d, "shrunk");
    fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644);
    writeAll(fd, buffer, 1 << 20);
    ftruncate(fd, 1);
    frow("reg_1MiB_then_ftruncate_to_1", fd);
    fsync(fd);
    frow("reg_1MiB_then_ftruncate_to_1_fsync", fd);
    close(fd);

    at(p, d, "overwritten");
    fd = open(p, O_CREAT | O_EXCL | O_RDWR, 0644);
    writeAll(fd, buffer, 8192);
    pwrite(fd, "y", 1, 0);
    frow("reg_8192_then_overwrite_first_byte", fd);
    close(fd);

    char dd[PATH_MAX];
    int counts[] = { 0, 100, 1000 };
    for (unsigned i = 0; i < 3; i++) {
        snprintf(dd, sizeof dd, "%s/dir%d", d, counts[i]);
        mkdir(dd, 0777);
        for (int j = 0; j < counts[i]; j++) {
            snprintf(p, sizeof p, "%s/e%d", dd, j);
            close(open(p, O_CREAT | O_WRONLY, 0644));
        }
        char label[64];
        snprintf(label, sizeof label, "dir_%d_names", counts[i]);
        lrow(label, dd);
    }

    static char target[4096];
    int lens[] = { 1, 59, 60, 61, 127, 128, 255, 256, 1000, 4000, 4095 };
    for (unsigned i = 0; i < sizeof lens / sizeof lens[0]; i++) {
        memset(target, 'a', (size_t)lens[i]);
        target[lens[i]] = 0;
        snprintf(p, sizeof p, "%s/l%d", d, lens[i]);
        char label[64];
        if (symlink(target, p) != 0) { printf("ROW\tlnk_target_%d\tsymlink=%s\n", lens[i], strerror(errno)); continue; }
        snprintf(label, sizeof label, "lnk_target_%d", lens[i]);
        lrow(label, p);
    }
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s <scratch-dir> [<fresh-root>]\n", argv[0]); return 2; }
    alarm(120);
    setvbuf(stdout, NULL, _IOLBF, 0);
    snprintf(base, sizeof base, "%s", argv[1]);
    header();
    hardLinks();
    directoryShapes();
    for (unsigned seed = 1; seed <= 8; seed++) directoryHistory(seed, 500);
    everyKind();
    blocksOfSizes();
    if (argc >= 3) freshRoot(argv[2]);
    return 0;
}
