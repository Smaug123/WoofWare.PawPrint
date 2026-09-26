// Every field of `struct statfs`, as `statfs(2)` and `fstatfs(2)` report it
// for each kind of object, on each mount this probe is pointed at.
//
// Usage: statfs-fields <dir>...   (an unwritable <dir> gets its own rows only)
//
// For each <dir>: a regular file and a subdirectory are created in it, and
// `statfs` (by path) and `fstatfs` (by descriptor) are asked about the file,
// the subdirectory and <dir> itself. Then, once, `fstatfs` is asked about both
// ends of a pipe, an AF_INET and an AF_UNIX socket, an epoll port / kqueue,
// an eventfd (Linux), and a descriptor that is not open; and `statfs` about a
// missing path, a path through a regular file, a dangling symlink and a
// symlink loop, so the path arm's errnos are visible.
//
// Measured 2026-09-26 on Linux 6.18.5 aarch64 (Apple's `container`, debian
// trixie, root, `--cap-add CAP_SYS_ADMIN` so the probe could mount its own
// tmpfs; `--memory 1G` and `2G`), on Linux 6.12.107 x86-64 (Debian's kernel
// under `qemu-system-x86_64 -cpu max`, the probe linked into the initramfs's
// /init; recipe in architecture-facts-linux.c), and on Darwin 27.0.0 arm64
// (seven APFS volumes of the internal container, plus a case-sensitive and a
// case-insensitive APFS disk image attached by uid 501). Each field is
// classed as (a) a fact of the filesystem type, the same on every mount of
// it; (b) a fact of the mount or the machine, which a model must be
// configured with or refuse; (c) derivable from the model's own state.
//
// Linux, tmpfs (eleven mounts: `/dev/shm` at size=64m, and size=1m/100
// inodes twice, 7m/5000 nosuid noexec, size=0 nr_inodes=0, size=0 alone,
// defaults, noatime nodev, strictatime ro, under each memory size):
//   f_type     0x01021994                                              (a)
//   f_bsize    4096, the page size; f_frsize equal to it               (a)
//   f_namelen  255; f_spare all 0                                      (a)
//   f_blocks   the `size=` option in pages; the default is half the
//              RAM (142352 pages at 1.09 GiB, 270477 at 2.06 GiB);
//              0 for size=0, "unlimited"                               (b)
//   f_files    the `nr_inodes=` option; the default is the same half
//              of RAM; 0 for nr_inodes=0. size=0 alone leaves it at
//              the default                                            (b)
//   f_bfree    f_blocks less the pages in use; f_bavail equal to it;
//              0 when unlimited (statfs-accounting.c has the rule)     (c)
//   f_ffree    f_files less the inodes in use; 0 when unlimited        (c)
//   f_fsid     random per mount: the two identical size=1m mounts
//              differ, and every boot differs. Unrelated to st_dev    (b)
//   f_flags    ST_VALID (0x20) always, then the mount's options: ro
//              0x1, nosuid 0x2, nodev 0x4, noexec 0x8, noatime 0x400,
//              relatime 0x1000 (the default), strictatime none. So it
//              is (b), constrained by (c): the atime flag is a claim
//              about when atime moves                                 (b)
//
// Linux, the objects not on a mount (both pipe ends, AF_INET stream and
// datagram and AF_UNIX sockets, epoll, eventfd):
//   f_type     pipefs 0x50495045, sockfs 0x534F434B, anon_inodefs
//              0x09041934 (epoll and eventfd alike)                    (a)
//   f_bsize    4096 and f_frsize equal, f_namelen 255, every count 0,
//              f_flags 0x20 (ST_VALID alone), f_spare 0                (a)
//   f_fsid     { that pseudo-filesystem's st_dev, 0 }, and the st_dev
//              is a boot-time anonymous device number: pipefs 0xc,
//              sockfs 0x8, anon_inodefs 0xd on 6.18.5 aarch64 (native
//              and Rosetta alike), but 0xf, 0x9, 0x10 on 6.12.107
//              x86-64. A fact of the kernel image, not of the type    (b)
//
// Darwin, APFS:
//   f_type     0x1a; f_fstypename "apfs"                               (a)
//   f_bsize    4096 on all nine volumes                                (a)
//   f_iosize   1048576 on the internal container's seven, 2097152 on
//              both disk images                                        (b)
//   f_blocks   the whole container's size, the same for every volume
//              in it                                                   (b)
//   f_bfree    the container's free blocks, shared by all its volumes
//              and moving with every process's writes; f_bavail equal
//              to it                                                   (b)
//   f_ffree    exactly 40 x f_bfree on eight volumes, both containers;
//              the sealed read-only system snapshot at / is otherwise  (a rule over (b))
//   f_files    f_ffree plus this volume's count of objects in use      (b), (c)
//   f_fsid     { the volume's st_dev, f_type }                         (c)
//   f_owner    0 for the internal volumes, 501 for the images uid 501
//              attached                                                (b)
//   f_flags    the MNT_* options, per volume: 0x4909080 on the Data
//              volume, 0x4480d001 on /, 0x4b09218 on a user image      (b)
//   f_fssubtype  1 for a case-insensitive volume (all seven internal
//              ones, one image), 0 for the case-sensitive image. So it
//              is a claim about how names compare                      (b), constrained by (c)
//   f_mntonname, f_mntfromname  where it is mounted, and the device    (b)
//   f_flags_ext  0x1 on the Data volume, 0 on every other              (b)
//   f_reserved all 0                                                   (a)
// Darwin, the objects not on a mount: EINVAL for both pipe ends, every
// socket and a kqueue (a devfs node such as a terminal *is* on a mount, and
// answers devfs's fields).
//
// Both flavours: EBADF for 4242 and for -1. `statfs` by path follows a final
// symlink, and answers ENOENT for a missing name, a dangling link and "",
// ENOTDIR for a path through a file and for a file with a trailing slash,
// and ELOOP (40 on Linux, 62 on Darwin) for a loop. `statfs` and `fstatfs`
// agreed on every field of every row, file, directory and mount alike, apart
// from the free counts of APFS moving between the two calls.
//
// Build (Linux):  gcc -Wall -o statfs-fields statfs-fields.c
// Build (Darwin): clang -Wall -o statfs-fields statfs-fields.c
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/vfs.h>
#else
#include <sys/event.h>
#include <sys/mount.h>
#include <sys/param.h>
#endif

static void dump(const char *label, int rc, int err, const struct statfs *s) {
    if (rc != 0) {
        printf("%-40s FAILED errno=%d (%s)\n", label, err, strerror(err));
        return;
    }
#ifdef __linux__
    printf("%-40s f_type=0x%lx f_bsize=%ld f_blocks=%lu f_bfree=%lu f_bavail=%lu f_files=%lu f_ffree=%lu "
           "f_fsid=%08x:%08x f_namelen=%ld f_frsize=%ld f_flags=0x%lx f_spare=%ld,%ld,%ld,%ld\n",
           label, (long)s->f_type, (long)s->f_bsize, (unsigned long)s->f_blocks, (unsigned long)s->f_bfree,
           (unsigned long)s->f_bavail, (unsigned long)s->f_files, (unsigned long)s->f_ffree,
           (unsigned)s->f_fsid.__val[0], (unsigned)s->f_fsid.__val[1], (long)s->f_namelen, (long)s->f_frsize,
           (long)s->f_flags, (long)s->f_spare[0], (long)s->f_spare[1], (long)s->f_spare[2], (long)s->f_spare[3]);
#else
    printf("%-40s f_bsize=%u f_iosize=%d f_blocks=%llu f_bfree=%llu f_bavail=%llu f_files=%llu f_ffree=%llu "
           "f_fsid=%08x:%08x f_owner=%u f_type=0x%x f_flags=0x%x f_fssubtype=%u f_fstypename=\"%s\" "
           "f_mntonname=\"%s\" f_mntfromname=\"%s\" f_flags_ext=0x%x f_reserved=%u,%u,%u,%u,%u,%u,%u\n",
           label, s->f_bsize, s->f_iosize, s->f_blocks, s->f_bfree, s->f_bavail, s->f_files, s->f_ffree,
           (unsigned)s->f_fsid.val[0], (unsigned)s->f_fsid.val[1], s->f_owner, s->f_type, s->f_flags, s->f_fssubtype,
           s->f_fstypename, s->f_mntonname, s->f_mntfromname, s->f_flags_ext, s->f_reserved[0], s->f_reserved[1],
           s->f_reserved[2], s->f_reserved[3], s->f_reserved[4], s->f_reserved[5], s->f_reserved[6]);
#endif
}

static void by_path(const char *label, const char *path) {
    struct statfs s;
    memset(&s, 0xA5, sizeof s);
    errno = 0;
    int rc = statfs(path, &s);
    dump(label, rc, errno, &s);
}

static void by_fd(const char *label, int fd) {
    struct statfs s;
    memset(&s, 0xA5, sizeof s);
    errno = 0;
    int rc = fstatfs(fd, &s);
    dump(label, rc, errno, &s);
}

static void on_mount(const char *dir) {
    char file[4096], sub[4096], label[256];
    snprintf(file, sizeof file, "%s/statfs-probe-file", dir);
    snprintf(sub, sizeof sub, "%s/statfs-probe-dir", dir);
    printf("== %s\n", dir);
    int rootfd = open(dir, O_RDONLY);
    snprintf(label, sizeof label, "statfs(dir)");
    by_path(label, dir);
    snprintf(label, sizeof label, "fstatfs(dir)");
    by_fd(label, rootfd);
    close(rootfd);
    unlink(file);
    rmdir(sub);
    int fd = open(file, O_CREAT | O_RDWR | O_TRUNC, 0644);
    if (fd < 0) {
        printf("%s: cannot create a file (errno %d); directory rows only\n", dir, errno);
        return;
    }
    if (mkdir(sub, 0755) != 0) {
        printf("%s: cannot create a directory (errno %d)\n", dir, errno);
        return;
    }
    int dfd = open(sub, O_RDONLY);
    by_path("statfs(file)", file);
    by_fd("fstatfs(file)", fd);
    by_path("statfs(subdir)", sub);
    by_fd("fstatfs(subdir)", dfd);
    close(fd);
    close(dfd);
    unlink(file);
    rmdir(sub);
}

int main(int argc, char **argv) {
    alarm(60);
    struct utsname u;
    uname(&u);
    printf("uname: %s %s %s; sizeof(struct statfs) = %zu; euid %d\n", u.sysname, u.release, u.machine,
           sizeof(struct statfs), (int)geteuid());

    for (int i = 1; i < argc; i++)
        on_mount(argv[i]);

    printf("== objects not on a mount\n");
    int ends[2];
    if (pipe(ends) != 0)
        return 1;
    by_fd("fstatfs(pipe read end)", ends[0]);
    by_fd("fstatfs(pipe write end)", ends[1]);
    int inet = socket(AF_INET, SOCK_STREAM, 0);
    by_fd("fstatfs(AF_INET stream socket)", inet);
    int inetd = socket(AF_INET, SOCK_DGRAM, 0);
    by_fd("fstatfs(AF_INET datagram socket)", inetd);
    int unixs = socket(AF_UNIX, SOCK_STREAM, 0);
    by_fd("fstatfs(AF_UNIX stream socket)", unixs);
#ifdef __linux__
    int port = epoll_create1(0);
    by_fd("fstatfs(epoll port)", port);
    int ev = eventfd(0, 0);
    by_fd("fstatfs(eventfd)", ev);
#else
    int port = kqueue();
    by_fd("fstatfs(kqueue)", port);
#endif
    by_fd("fstatfs(4242, not open)", 4242);
    by_fd("fstatfs(-1)", -1);
    by_fd("fstatfs(stdin)", 0);

    printf("== path errors\n");
    if (argc > 1) {
        char path[4096], file[4096], link1[4096], link2[4096];
        const char *dir = argv[1];
        snprintf(file, sizeof file, "%s/statfs-probe-err-file", dir);
        int fd = open(file, O_CREAT | O_RDWR | O_TRUNC, 0644);
        close(fd);
        snprintf(path, sizeof path, "%s/no-such-name", dir);
        by_path("statfs(missing)", path);
        snprintf(path, sizeof path, "%s/statfs-probe-err-file/x", dir);
        by_path("statfs(through a file)", path);
        snprintf(path, sizeof path, "%s/statfs-probe-err-file/", dir);
        by_path("statfs(file with trailing slash)", path);
        snprintf(link1, sizeof link1, "%s/statfs-probe-dangling", dir);
        unlink(link1);
        symlink("no-such-target", link1);
        by_path("statfs(dangling symlink)", link1);
        snprintf(link2, sizeof link2, "%s/statfs-probe-loop", dir);
        unlink(link2);
        symlink("statfs-probe-loop", link2);
        by_path("statfs(symlink loop)", link2);
        by_path("statfs(\"\")", "");
        unlink(file);
        unlink(link1);
        unlink(link2);
    }
    return 0;
}
