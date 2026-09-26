// How `statfs(2)`'s counts move as a directory's contents change, and how
// `f_fsid` relates to `st_dev`.
//
// Usage: statfs-accounting <dir>   (writable; on the mount under test)
//
// Each step below is applied inside <dir>, and the change it made to
// f_blocks, f_bfree, f_bavail, f_files and f_ffree is printed. On a mount that
// nothing else writes to (a tmpfs of the probe's own) every delta is the
// step's alone; on a shared volume (APFS's container, a busy ext4) other
// writers' deltas are mixed in, which is itself the finding.
//
// Measured 2026-09-26 (the environments are in statfs-fields.c).
//
// Linux tmpfs, 6.18.5 aarch64 (`/dev/shm` and a 7m/5000-inode mount of the
// probe's own) and 6.12.107 x86-64 (a 1m/100-inode mount), every delta
// identical across the three:
//   * f_blocks and f_files never move; f_bavail moves with f_bfree.
//   * A regular file costs one block per page holding data: 1 to 4096 bytes
//     cost 1, 4097 and 8192 cost 2, 12289 costs 4, 64 KiB 16, 1 MiB 256.
//     A hole costs nothing:
//     ftruncate to 1 MiB moves nothing, and one byte written in its middle
//     costs one block.
//   * A symbolic link costs a block when its target is 128 bytes or more
//     (127 costs none).
//   * Every inode costs one from f_ffree: file, directory, symlink, FIFO,
//     O_TMPFILE. So does every *extra hard link*. A name's length costs
//     nothing. An unlinked file keeps its blocks and its inode until the
//     last descriptor closes.
//   * On a mount with size=0 nr_inodes=0 every count is 0 and nothing moves.
// So f_bfree and f_ffree are derivable from a model's state only if the
// model knows which pages of a file hold data; one that stores a file as
// its bytes cannot tell a hole from written zeros.
//
// Darwin APFS, 27.0.0: in a private 64 MiB disk image, where nothing else
// writes, a regular file costs one block per started 4 KiB of data and holes
// cost nothing, as on tmpfs; symlinks (to 200 bytes; APFS refuses 4095),
// directories, FIFOs, hard links and long names cost no block. f_ffree moves
// by exactly 40 per block. f_files is f_ffree plus the objects in use, so it
// moves by +1 per file, directory, symlink or FIFO created and by -39 for a
// file's first block. It is not deterministic even there: of two identical
// back-to-back runs, one freed 258 blocks for a 256-block file and took 2 for
// an empty one, and the block of a file unlinked while open was not returned
// within the run. On the shared internal container every row carries other
// processes' movements besides.
//
// Linux tmpfs's f_fsid is unrelated to the mount's st_dev (0x96bbdf64:... for
// st_dev 0x18). Darwin's is { st_dev, f_type }: 01000012:0000001a for
// st_dev 0x1000012.
//
// Build (Linux):  gcc -Wall -o statfs-accounting statfs-accounting.c
// Build (Darwin): clang -Wall -o statfs-accounting statfs-accounting.c
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/vfs.h>
#else
#include <sys/mount.h>
#include <sys/param.h>
#endif

static const char *dir;
static struct statfs last;

static void snap(struct statfs *s) {
    if (statfs(dir, s) != 0) {
        printf("statfs(%s) failed: errno %d\n", dir, errno);
        exit(1);
    }
}

static void step(const char *label) {
    struct statfs now;
    snap(&now);
    printf("%-44s blocks%+lld bfree%+lld bavail%+lld files%+lld ffree%+lld\n", label,
           (long long)now.f_blocks - (long long)last.f_blocks, (long long)now.f_bfree - (long long)last.f_bfree,
           (long long)now.f_bavail - (long long)last.f_bavail, (long long)now.f_files - (long long)last.f_files,
           (long long)now.f_ffree - (long long)last.f_ffree);
    last = now;
}

static void path(char *out, const char *name) { snprintf(out, 4096, "%s/%s", dir, name); }

static void write_n(const char *name, size_t n) {
    char p[4096];
    path(p, name);
    int fd = open(p, O_CREAT | O_WRONLY | O_TRUNC, 0644);
    char *buf = malloc(n ? n : 1);
    memset(buf, 'x', n ? n : 1);
    if (n && write(fd, buf, n) != (ssize_t)n)
        printf("short write of %zu\n", n);
    free(buf);
    close(fd);
}

int main(int argc, char **argv) {
    alarm(120);
    if (argc != 2)
        return 2;
    dir = argv[1];
    char p[4096], q[4096];

    struct statfs s;
    snap(&s);
    struct stat st;
    stat(dir, &st);
#ifdef __linux__
    printf("f_fsid=%08x:%08x st_dev=0x%llx\n", (unsigned)s.f_fsid.__val[0], (unsigned)s.f_fsid.__val[1],
           (unsigned long long)st.st_dev);
#else
    printf("f_fsid=%08x:%08x st_dev=0x%llx\n", (unsigned)s.f_fsid.val[0], (unsigned)s.f_fsid.val[1],
           (unsigned long long)st.st_dev);
#endif
    snap(&last);
    step("(nothing: a control)");

    size_t sizes[] = {0, 1, 4095, 4096, 4097, 8192, 12289, 65536, 1048576};
    for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        char label[128];
        snprintf(label, sizeof label, "create a file of %zu bytes", sizes[i]);
        write_n("f", sizes[i]);
        step(label);
        path(p, "f");
        unlink(p);
        step("  unlink it");
    }

    path(p, "sparse");
    int fd = open(p, O_CREAT | O_RDWR | O_TRUNC, 0644);
    step("create an empty file");
    ftruncate(fd, 1048576);
    step("  ftruncate it to 1 MiB (a hole)");
    pwrite(fd, "x", 1, 524288);
    step("  write 1 byte in the middle of the hole");
    ftruncate(fd, 0);
    step("  ftruncate it to 0");
    close(fd);
    unlink(p);
    step("  unlink it");

    path(p, "d");
    mkdir(p, 0755);
    step("mkdir");
    rmdir(p);
    step("  rmdir it");

    size_t targets[] = {1, 127, 128, 129, 200, 4095};
    for (size_t i = 0; i < sizeof targets / sizeof targets[0]; i++) {
        char target[4096], label[128];
        memset(target, 'a', targets[i]);
        target[targets[i]] = 0;
        path(p, "l");
        symlink(target, p);
        snprintf(label, sizeof label, "symlink with a %zu-byte target", targets[i]);
        step(label);
        unlink(p);
        step("  unlink it");
    }

    write_n("h", 0);
    step("create an empty file");
    path(p, "h");
    path(q, "h2");
    link(p, q);
    step("  hard link it");
    path(q, "h3");
    link(p, q);
    step("  hard link it again");
    unlink(q);
    step("  unlink one link");
    path(q, "h2");
    unlink(q);
    step("  unlink another");
    unlink(p);
    step("  unlink the last");

    path(p, "fifo");
    mkfifo(p, 0644);
    step("mkfifo");
    unlink(p);
    step("  unlink it");

    char longname[256];
    memset(longname, 'n', 255);
    longname[255] = 0;
    write_n(longname, 0);
    step("create an empty file with a 255-byte name");
    path(p, longname);
    unlink(p);
    step("  unlink it");

    write_n("open", 4096);
    step("create a file of 4096 bytes");
    path(p, "open");
    fd = open(p, O_RDONLY);
    unlink(p);
    step("  unlink it while it is open");
    close(fd);
    step("  close it");

#ifdef O_TMPFILE
    fd = open(dir, O_TMPFILE | O_RDWR, 0644);
    step("O_TMPFILE");
    close(fd);
    step("  close it");
#endif
    return 0;
}
