// Measures what open(2) does with every bit of its flag word, on each flavour,
// and what a descriptor opened with access mode 3 can do on Linux.
//
// The kernel's own entry point is called, not the C library's: openat(2)
// through syscall(2) on Linux, so glibc adds nothing to the word, and libc's
// open(2) on Darwin, which passes the word to the kernel unchanged.
//
// FLAGS rows. Each flag word is tried against eight pathnames:
//   NULL  the null pointer, so EINVAL ahead of EFAULT shows a screen of the
//         word made before the path is copied in
//   nx    a name that does not exist (removed again if the call created it)
//   f     a regular file the caller owns, mode 0644, holding "abc"
//   d     a directory the caller owns, mode 0755
//   l     a symbolic link to f
//   abs   f by its absolute path
//   up    f by a path that climbs out of the directory and back in
//   h     a second link to another file, which therefore has two links
// and each answer is an errno name, or "ok/0x.../0x..." with the
// description's F_GETFL word, which says which bits the kernel kept, and the
// descriptor's F_GETFD word; ",trunc" if f was emptied, ",created" if nx
// exists afterwards. The words swept:
//   - each access mode 0..3 alone, and with each bit 2..31 alone;
//   - each access mode with each pair of bits from 2..31;
//   - the single-bit words again with O_CREAT|O_EXCL (EEXIST on f) and with
//     O_CREAT (on d), for the order of EINVAL against EEXIST and EISDIR;
//   - 2000 words from a fixed-seed generator, every bit equally likely.
//
// MODE3 rows: on Linux, what a descriptor opened with access mode 3 can do.
// PERM rows: which permission bits access mode 3 demands, from a caller
// without privilege (uid 65534 on Linux, which needs root to switch to; the
// invoking user on Darwin).
//
// Linux aarch64: container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/open-flags.c && cd /tmp && /tmp/p "$(mktemp -d)"'
//   (the directory is on the container's ext4, not the bind mount).
// Linux x86-64: built with `gcc -static` in a `container --arch amd64`
//   Debian trixie, packed as the /init of an initramfs, and booted under
//   qemu-system-x86_64 -m 2G -cpu max -kernel vmlinuz -initrd init.cpio.gz \
//     -append "console=ttyS0 panic=-1 quiet" -nographic -no-reboot
//   on that image's linux-image-amd64 (6.12.111). Run with no argument it
//   works in /probe-work on the initramfs's root and powers off when done.
//   Not under Rosetta, which runs on the aarch64 kernel (see
//   architecture-facts-linux.c).
// Darwin: nix develop -c clang -Wall -o open-flags open-flags.c && ./open-flags "$(mktemp -d /private/tmp/openflags.XXXXXX)"
//
// Measured 2026-10-02 on Linux 6.18.5 aarch64 (root), Linux 6.12.111 x86-64
// (root) and Darwin 27.0 arm64 (uid 501); the output of each is beside this
// file, and each was identical on a second run (Darwin: three more).
// WoofWare.PosixKernel.Test/TestOpenFlagWord.fs replays them.
//
// What they say:
// - Every bit that neither kernel defines is ignored, on both: Darwin does not
//   answer EINVAL for one, whatever its man page suggests. A single-bit row
//   differs from the bare access mode's exactly for the bits each flavour
//   defines (the macOS 26.4 SDK's <sys/fcntl.h>, and O_CLOFORK below),
//   except O_EXCL (nothing without O_CREAT), O_NOCTTY (nothing on a
//   file that is not a terminal), Linux's O_LARGEFILE (always set on a 64-bit
//   kernel, so F_GETFL shows it on every row), and Darwin's O_SYMLINK (opens
//   the link itself, which F_GETFL cannot tell from its target) and O_POPUP.
// - Linux's numbering differs by architecture in four bits: O_DIRECTORY
//   0x4000 on aarch64 and 0x10000 on x86-64, O_NOFOLLOW 0x8000 and 0x20000,
//   O_DIRECT 0x10000 and 0x4000, O_LARGEFILE 0x20000 and 0x8000. Every other
//   bit agrees.
// - The word is screened before the path is copied in, and so ahead of EEXIST
//   and EISDIR: among words of modelled and undefined bits only, the screen is
//   EINVAL for O_CREAT|O_DIRECTORY on both, and for access mode 3 on Darwin.
//   Linux's __O_TMPFILE without O_DIRECTORY, and Darwin's O_EXEC with a write
//   access mode, are screened the same way; O_PATH lifts Linux's screen.
// - Linux opens access mode 3, demanding both the read and the write bit of a
//   caller without privilege. The descriptor can neither read nor write (EBADF,
//   for a zero count too), nor take a flock (EBADF), nor be truncated
//   (EINVAL) or mapped (EACCES), nor copy_file_range either way (EBADF); it
//   can lseek, fstat, fsync, fchmod, fchown, futimens, posix_fadvise and
//   ioctl(FIONREAD). With O_TRUNC it truncates.
// - Darwin's O_CLOFORK (0x8000000), absent from the SDK headers, sets
//   FD_CLOFORK (2), and its O_RESOLVE_BENEATH and O_UNIQUE refuse a path out
//   of the directory and a file with two links with errno 107 (ENOTCAPABLE).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/reboot.h>
#endif

static void die(const char *what) {
    perror(what);
    exit(2);
}

static const char *en(int e) {
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
    case EFAULT: return "EFAULT";
    case ENXIO: return "ENXIO";
    case EOPNOTSUPP: return "EOPNOTSUPP";
#if defined(ENOTSUP) && ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
    case ETXTBSY: return "ETXTBSY";
    case EAGAIN: return "EAGAIN";
    case ENOTTY: return "ENOTTY";
    case ESPIPE: return "ESPIPE";
    case EROFS: return "EROFS";
    case EFBIG: return "EFBIG";
    case EOVERFLOW: return "EOVERFLOW";
    default: {
        static char buf[32];
        snprintf(buf, sizeof buf, "errno%d", e);
        return buf;
    }
    }
}

static int raw_open(const char *path, int flags, int mode) {
#ifdef __linux__
    return (int)syscall(SYS_openat, AT_FDCWD, path, flags, mode);
#else
    return open(path, flags, mode);
#endif
}

static void seed(void) {
    int fd = open("f", O_CREAT | O_WRONLY | O_TRUNC, 0644);
    if (fd < 0) die("seed f");
    if (write(fd, "abc", 3) != 3) die("seed write");
    close(fd);
    if (fchmodat(AT_FDCWD, "f", 0644, 0) != 0) die("chmod f");
    struct stat st;
    if (lstat("d", &st) != 0) {
        if (mkdir("d", 0755) != 0) die("mkdir d");
    }
    if (lstat("l", &st) != 0) {
        if (symlink("f", "l") != 0) die("symlink l");
    }
    if (lstat("g", &st) != 0) {
        int gfd = open("g", O_CREAT | O_WRONLY, 0644);
        if (gfd < 0) die("seed g");
        close(gfd);
    }
    if (lstat("h", &st) != 0) {
        if (link("g", "h") != 0) die("link h");
    }
}

static char abs_path[2048];
static char up_path[2048];

static const char *target_names[] = {"NULL", "nx", "f", "d", "l", "abs", "up", "h"};
#define NTARGETS 8

static void one(unsigned word, const char *name) {
    const char *path = strcmp(name, "NULL") == 0 ? NULL
                       : strcmp(name, "abs") == 0 ? abs_path
                       : strcmp(name, "up") == 0  ? up_path
                                                  : name;
    errno = 0;
    int fd = raw_open(path, (int)word, 0644);
    int e = errno;
    char extra[64] = "";
    if (fd >= 0) {
        int fl = fcntl(fd, F_GETFL);
        int fdfl = fcntl(fd, F_GETFD);
        close(fd);
        printf("\t%s=ok/0x%x/0x%x", name, (unsigned)fl, (unsigned)fdfl);
    } else {
        printf("\t%s=%s", name, en(e));
    }
    struct stat st;
    if (stat("f", &st) != 0 || !S_ISREG(st.st_mode) || st.st_size != 3) {
        strcat(extra, ",trunc");
    }
    if (lstat("nx", &st) == 0) {
        strcat(extra, ",created");
        if (S_ISDIR(st.st_mode)) rmdir("nx");
        else unlink("nx");
    }
    if (lstat("d", &st) != 0 || !S_ISDIR(st.st_mode)) strcat(extra, ",d-changed");
    if (lstat("l", &st) != 0 || !S_ISLNK(st.st_mode)) strcat(extra, ",l-changed");
    if (lstat("h", &st) != 0 || !S_ISREG(st.st_mode) || st.st_nlink != 2) strcat(extra, ",h-changed");
    printf("%s", extra);
    // Restore anything the call changed, so the next call sees the seed.
    seed();
}

static void flags_row(const char *section, unsigned word) {
    printf("FLAGS\t%s\t0x%08x", section, word);
    for (int i = 0; i < NTARGETS; i++) one(word, target_names[i]);
    printf("\n");
}

static uint64_t rng = 0x9e3779b97f4a7c15ULL;
static uint32_t next_word(void) {
    rng ^= rng << 13;
    rng ^= rng >> 7;
    rng ^= rng << 17;
    return (uint32_t)(rng >> 16);
}

// What one call on a descriptor opened with access mode 3 answers.
static void mode3_call(const char *what, int r) {
    if (r < 0) printf("MODE3\t%s\t%s\n", what, en(errno));
    else printf("MODE3\t%s\tok %d\n", what, r);
}

static void mode3(void) {
    int fd = raw_open("f", 3, 0);
    if (fd < 0) {
        printf("MODE3\topen\t%s\n", en(errno));
        return;
    }
    printf("MODE3\topen\tok F_GETFL=0x%x\n", (unsigned)fcntl(fd, F_GETFL));
    char buf[8];
    errno = 0;
    mode3_call("read 1", (int)read(fd, buf, 1));
    mode3_call("read 0", (int)read(fd, buf, 0));
    mode3_call("write 1", (int)write(fd, "x", 1));
    mode3_call("write 0", (int)write(fd, "x", 0));
    mode3_call("pread 1", (int)pread(fd, buf, 1, 0));
    mode3_call("pwrite 1", (int)pwrite(fd, "x", 1, 0));
    mode3_call("lseek end", (int)lseek(fd, 0, SEEK_END));
    struct stat st;
    mode3_call("fstat", fstat(fd, &st));
    mode3_call("ftruncate 0", ftruncate(fd, 0));
    mode3_call("fsync", fsync(fd));
    mode3_call("flock LOCK_SH", flock(fd, LOCK_SH | LOCK_NB));
    mode3_call("flock LOCK_EX", flock(fd, LOCK_EX | LOCK_NB));
    mode3_call("flock LOCK_UN", flock(fd, LOCK_UN));
    int avail = -1;
    mode3_call("ioctl FIONREAD", ioctl(fd, FIONREAD, &avail));
    mode3_call("fchmod 0644", fchmod(fd, 0644));
    mode3_call("fchown -1 -1", fchown(fd, (uid_t)-1, (gid_t)-1));
    mode3_call("futimens NULL", futimens(fd, NULL));
    void *m = mmap(NULL, 4096, PROT_READ, MAP_SHARED, fd, 0);
    mode3_call("mmap PROT_READ MAP_SHARED", m == MAP_FAILED ? -1 : 0);
    if (m != MAP_FAILED) munmap(m, 4096);
#ifdef __linux__
    int advised = posix_fadvise(fd, 0, 0, POSIX_FADV_NORMAL);
    errno = advised;
    mode3_call("posix_fadvise", advised == 0 ? 0 : -1);
    int other = raw_open("f", O_RDWR, 0);
    loff_t off = 0;
    mode3_call("copy_file_range from", (int)syscall(SYS_copy_file_range, fd, &off, other, NULL, 1, 0));
    off = 0;
    mode3_call("copy_file_range to", (int)syscall(SYS_copy_file_range, other, &off, fd, NULL, 1, 0));
    close(other);
#endif
    fstat(fd, &st);
    printf("MODE3\tsize afterwards\t%lld\n", (long long)st.st_size);
    close(fd);
}

// Which permission bits access mode 3 (and the three ordinary modes, as a
// control) demand, of a caller without privilege that owns the file.
static void perm(void) {
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        if (geteuid() == 0) {
            // The probe's directory may be private to root, so open it up.
            if (chmod(".", 0711) != 0) die("chmod .");
            if (mkdir("p", 0777) != 0 || chmod("p", 0777) != 0) die("mkdir p");
            if (setgid(65534) != 0 || setuid(65534) != 0) die("setuid");
        } else {
            if (mkdir("p", 0777) != 0) die("mkdir p");
        }
        if (chdir("p") != 0) die("chdir p");
        printf("PERM\tuid=%d\n", (int)geteuid());
        int modes[] = {0000, 0200, 0400, 0600, 0644};
        unsigned words[] = {0, 1, 2, 3, 0 | O_TRUNC, 3 | O_TRUNC};
        for (size_t i = 0; i < sizeof modes / sizeof *modes; i++) {
            printf("PERM\t0%03o", modes[i]);
            for (size_t j = 0; j < sizeof words / sizeof *words; j++) {
                unlink("g");
                int fd = open("g", O_CREAT | O_WRONLY | O_EXCL, 0600);
                if (fd < 0 || write(fd, "abc", 3) != 3) die("seed g");
                close(fd);
                if (chmod("g", (mode_t)modes[i]) != 0) die("chmod g");
                errno = 0;
                fd = raw_open("g", (int)words[j], 0);
                struct stat st;
                stat("g", &st);
                if (fd >= 0) {
                    close(fd);
                    printf("\t0x%x=ok%s", words[j], st.st_size == 0 ? ",trunc" : "");
                } else {
                    printf("\t0x%x=%s%s", words[j], en(errno), st.st_size == 0 ? ",trunc" : "");
                }
            }
            printf("\n");
        }
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
}

int main(int argc, char **argv) {
    alarm(600);
    const char *dir = argc >= 2 ? argv[1] : NULL;
    if (dir == NULL) {
        // Run as an initramfs's /init: work in a fresh directory of its root.
        mkdir("/probe-work", 0755);
        dir = "/probe-work";
    }
    if (chdir(dir) != 0) die("chdir");
    seed();
    if (getcwd(abs_path, sizeof abs_path - 8) == NULL) die("getcwd");
    const char *base = strrchr(abs_path, '/');
    snprintf(up_path, sizeof up_path, "..%s/f", base);
    strcat(abs_path, "/f");

    struct utsname u;
    if (uname(&u) != 0) die("uname");
    printf("KERNEL\t%s %s %s\tuid=%d\n", u.sysname, u.release, u.machine, (int)geteuid());

    // The C library's names for the bits, for the reader.
#define NAME(x) printf("HEADER\t%s\t0x%x\n", #x, (unsigned)(x));
    NAME(O_ACCMODE) NAME(O_RDONLY) NAME(O_WRONLY) NAME(O_RDWR) NAME(O_CREAT) NAME(O_EXCL)
    NAME(O_NOCTTY) NAME(O_TRUNC) NAME(O_APPEND) NAME(O_NONBLOCK) NAME(O_DSYNC) NAME(O_SYNC)
    NAME(O_DIRECTORY) NAME(O_NOFOLLOW) NAME(O_CLOEXEC)
#ifdef O_ASYNC
    NAME(O_ASYNC)
#endif
#ifdef O_DIRECT
    NAME(O_DIRECT)
#endif
#ifdef O_LARGEFILE
    NAME(O_LARGEFILE)
#endif
#ifdef O_NOATIME
    NAME(O_NOATIME)
#endif
#ifdef O_PATH
    NAME(O_PATH)
#endif
#ifdef O_TMPFILE
    NAME(O_TMPFILE)
#endif
#ifdef O_SHLOCK
    NAME(O_SHLOCK)
#endif
#ifdef O_EXLOCK
    NAME(O_EXLOCK)
#endif
#ifdef O_EVTONLY
    NAME(O_EVTONLY)
#endif
#ifdef O_SYMLINK
    NAME(O_SYMLINK)
#endif
#ifdef O_NOFOLLOW_ANY
    NAME(O_NOFOLLOW_ANY)
#endif
#ifdef O_RESOLVE_BENEATH
    NAME(O_RESOLVE_BENEATH)
#endif
#ifdef O_EXEC
    NAME(O_EXEC)
#endif
#ifdef O_SEARCH
    NAME(O_SEARCH)
#endif
#ifdef O_CLOFORK
    NAME(O_CLOFORK)
#endif
#ifdef O_POPUP
    NAME(O_POPUP)
#endif
#ifdef O_ALERT
    NAME(O_ALERT)
#endif
#ifdef O_DP_GETRAWENCRYPTED
    NAME(O_DP_GETRAWENCRYPTED)
#endif
#ifdef O_DP_GETRAWUNENCRYPTED
    NAME(O_DP_GETRAWUNENCRYPTED)
#endif
#ifdef O_DP_AUTHENTICATE
    NAME(O_DP_AUTHENTICATE)
#endif
#ifdef O_UNIQUE
    NAME(O_UNIQUE)
#endif
#ifdef FD_CLOEXEC
    NAME(FD_CLOEXEC)
#endif
#ifdef FD_CLOFORK
    NAME(FD_CLOFORK)
#endif

    printf("COLUMNS\tsection\tword");
    for (int i = 0; i < NTARGETS; i++) printf("\t%s", target_names[i]);
    printf("\n");
    for (unsigned a = 0; a < 4; a++) {
        flags_row("single", a);
        for (int b = 2; b < 32; b++) flags_row("single", a | (1u << b));
    }
    for (unsigned a = 0; a < 4; a++)
        for (int b1 = 2; b1 < 32; b1++)
            for (int b2 = b1 + 1; b2 < 32; b2++) flags_row("pair", a | (1u << b1) | (1u << b2));
    for (unsigned a = 0; a < 4; a++) {
        flags_row("creat-excl", a | O_CREAT | O_EXCL);
        flags_row("creat", a | O_CREAT);
        for (int b = 2; b < 32; b++) {
            flags_row("creat-excl", a | O_CREAT | O_EXCL | (1u << b));
            flags_row("creat", a | O_CREAT | (1u << b));
        }
    }
    for (int i = 0; i < 2000; i++) flags_row("random", next_word());
    fflush(stdout);

    mode3();
    fflush(stdout);
    perm();
    printf("DONE\n");
    fflush(stdout);
#ifdef __linux__
    if (argc < 2) reboot(RB_POWER_OFF);
#endif
    return 0;
}
