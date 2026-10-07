// What a Darwin process sees of devfs at /dev beyond the four memory devices
// that `devices.c` covers: the terminal (`/dev/tty`), the standard-stream
// links (`/dev/stdin`, `/dev/stdout`, `/dev/stderr`) and the descriptor
// directory (`/dev/fd`, `/dev/fd/N`); the path walk at and inside /dev;
// directory operations in /dev as an unprivileged user; and whether a second
// boot moved the numbers `devices.c` recorded. Portable, so the same binary's
// rows can be set against Linux's.
//
// The probe makes its own standard streams, so that the rows do not depend on
// how it was launched: fd 0 is the read end of a pipe whose writer is closed,
// and fds 1 and 2 are the write ends of two pipes whose readers it holds,
// which is how PawPrint's harness launches a guest. Output goes to what was
// fd 1 at launch, moved to fd 60.
//
// Descriptors the /dev/fd rows name, all opened before any section runs:
//   10  regular file "0123456789", O_RDWR, offset 3
//   11  directory (the scratch directory), O_RDONLY
//   12  pipe read end   13  pipe write end
//   14  /dev/null, O_RDONLY
//   15  the file of fd 10, O_RDONLY
//   16  the file of fd 10, O_WRONLY|O_APPEND
//   17  one end of an AF_UNIX stream socketpair
//
// Usage: darwin-devfs [section...] with sections
//   stat statfs walk fdsem open ns list tty times
// No arguments runs every section except `times`, which sleeps.
//
// Darwin, natively as uid 501 (no controlling terminal; the `tty` section
// makes one in a child):
//   nix develop -c clang -Wall -O0 -o /tmp/darwin-devfs darwin-devfs.c
//   /tmp/darwin-devfs > darwin-devfs.darwin-27.0-uid501.txt
//   /tmp/darwin-devfs times >> darwin-devfs.darwin-27.0-uid501.txt
// Linux, under Apple's `container` (devtmpfs at /dev), as root (with `times`)
// and as uid 1000:
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O0 -o /tmp/p
//     /probe/darwin-devfs.c && { /tmp/p; /tmp/p times; } > /probe/root.txt &&
//     setpriv --reuid=1000 --regid=1000 --clear-groups /tmp/p > /probe/uid1000.txt'
//
// Side effects outside the scratch directory: as uid 501 on Darwin,
// `utimes(NULL)` succeeds on /dev/null and /dev/tty and stamps them now. As
// root on Linux, the creations in /dev succeed and are removed again, and the
// rows that would remove, rename or change an existing node print "skipped".
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#if defined(__linux__)
#include <sys/statfs.h>
#include <sys/sysmacros.h>
#define ATIM(s) ((s).st_atim)
#define MTIM(s) ((s).st_mtim)
#define CTIM(s) ((s).st_ctim)
#else
#define ATIM(s) ((s).st_atimespec)
#define MTIM(s) ((s).st_mtimespec)
#define CTIM(s) ((s).st_ctimespec)
#endif

static FILE *o;
#define P(...) fprintf(o, __VA_ARGS__)

static char scratch[] = "/tmp/darwin-devfsXXXXXX";
static char filepath[256];

static const char *en(int e) {
    switch (e) {
    case 0: return "0";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case EBADF: return "EBADF";
    case EACCES: return "EACCES";
    case EFAULT: return "EFAULT";
    case EBUSY: return "EBUSY";
    case EEXIST: return "EEXIST";
    case EXDEV: return "EXDEV";
    case ENODEV: return "ENODEV";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case EINVAL: return "EINVAL";
    case ENOTTY: return "ENOTTY";
    case ESPIPE: return "ESPIPE";
    case EROFS: return "EROFS";
    case EAGAIN: return "EAGAIN";
    case ENXIO: return "ENXIO";
    case ELOOP: return "ELOOP";
    case ENOTEMPTY: return "ENOTEMPTY";
    case EOVERFLOW: return "EOVERFLOW";
    case ENOTSUP: return "ENOTSUP";
#if defined(EOPNOTSUPP) && EOPNOTSUPP != ENOTSUP
    case EOPNOTSUPP: return "EOPNOTSUPP";
#endif
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EMFILE: return "EMFILE";
    case EINTR: return "EINTR";
    case ENOSYS: return "ENOSYS";
    case EPIPE: return "EPIPE";
    default: {
        static char b[4][32];
        static int k;
        char *s = b[k++ & 3];
        snprintf(s, 32, "errno%d", e);
        return s;
    }
    }
}

static char rbuf[16][64];
static int rslot;
// "<value>" or "-1 <errno>"; -2 is the sentinel for a row not run as root.
static const char *res(long long r) {
    char *b = rbuf[rslot++ & 15];
    if (r == -2)
        snprintf(b, 64, "skipped (root)");
    else if (r < 0)
        snprintf(b, 64, "-1 %s", en(errno));
    else
        snprintf(b, 64, "%lld", r);
    return b;
}

static const char *kind(mode_t m) {
    switch (m & S_IFMT) {
    case S_IFREG: return "reg";
    case S_IFDIR: return "dir";
    case S_IFLNK: return "lnk";
    case S_IFCHR: return "chr";
    case S_IFBLK: return "blk";
    case S_IFIFO: return "fifo";
    case S_IFSOCK: return "sock";
    default: return "?";
    }
}

static void ts(const char *label, struct timespec t) { P(" %s=%lld.%09ld", label, (long long)t.tv_sec, t.tv_nsec); }

static void pstat(const struct stat *s, int times) {
    P(" %s mode=0%o dev=%u,%u(raw 0x%llx) ino=%llu nlink=%llu uid=%u gid=%u rdev=%u,%u(raw 0x%llx) size=%lld blksize=%ld blocks=%lld",
      kind(s->st_mode), (unsigned)s->st_mode, (unsigned)major(s->st_dev), (unsigned)minor(s->st_dev),
      (unsigned long long)(uint32_t)s->st_dev, (unsigned long long)s->st_ino, (unsigned long long)s->st_nlink,
      (unsigned)s->st_uid, (unsigned)s->st_gid, (unsigned)major(s->st_rdev), (unsigned)minor(s->st_rdev),
      (unsigned long long)(uint32_t)s->st_rdev, (long long)s->st_size, (long)s->st_blksize, (long long)s->st_blocks);
#if defined(__APPLE__)
    P(" flags=0x%x gen=%u", s->st_flags, s->st_gen);
#endif
    if (times) {
        ts("atime", ATIM(*s));
        ts("mtime", MTIM(*s));
        ts("ctime", CTIM(*s));
#if defined(__APPLE__)
        ts("birth", s->st_birthtimespec);
#endif
    }
}

static int same_id(const struct stat *a, const struct stat *b) {
    return a->st_dev == b->st_dev && a->st_ino == b->st_ino && a->st_mode == b->st_mode && a->st_rdev == b->st_rdev &&
           a->st_size == b->st_size && a->st_uid == b->st_uid && a->st_nlink == b->st_nlink;
}

// stat, lstat, and fstatat relative to an open /dev; full fields for stat,
// and lstat/fstatat as "=stat" when their identity fields agree.
static void stat3(const char *path, int times) {
    struct stat s, l;
    int rs = stat(path, &s);
    P("STAT %s stat", path);
    if (rs == 0) pstat(&s, times); else P(" %s", res(-1));
    int rl = lstat(path, &l);
    P(" | lstat");
    if (rl == 0) {
        if (rs == 0 && same_id(&s, &l)) P(" =stat");
        else pstat(&l, 0);
    } else P(" %s", res(-1));
    if (!strncmp(path, "/dev/", 5) && path[5]) {
        int dfd = open("/dev", O_RDONLY | O_DIRECTORY);
        struct stat a;
        int ra = fstatat(dfd, path + 5, &a, AT_SYMLINK_NOFOLLOW);
        P(" | fstatat(/dev,\"%s\",NOFOLLOW)", path + 5);
        if (ra == 0) {
            if (rl == 0 && same_id(&a, &l)) P(" =lstat");
            else pstat(&a, 0);
        } else P(" %s", res(-1));
        close(dfd);
    }
    P("\n");
}

static void sec_stat(void) {
    struct timespec now;
    clock_gettime(CLOCK_REALTIME, &now);
    P("NOW %lld.%09ld\n", (long long)now.tv_sec, now.tv_nsec);
    const char *paths[] = {"/", "/dev", "/dev/null", "/dev/zero", "/dev/random", "/dev/urandom", "/dev/tty",
                           "/dev/console", "/dev/stdin", "/dev/stdout", "/dev/stderr", "/dev/fd", "/dev/fd/0",
                           "/dev/fd/1", "/dev/fd/2", "/dev/fd/10", "/dev/fd/11", "/dev/fd/12", "/dev/fd/13",
                           "/dev/fd/14", "/dev/fd/15", "/dev/fd/16", "/dev/fd/17", "/dev/fd/60", "/dev/fd/99",
                           "/dev/ptmx", "/dev/full", "/dev/shm", "/dev/pts"};
    for (size_t i = 0; i < sizeof paths / sizeof *paths; i++) stat3(paths[i], 1);
    struct stat s;
    const int fds[] = {0, 1, 2, 10, 11, 12, 13, 14, 15, 16, 17};
    for (size_t i = 0; i < sizeof fds / sizeof *fds; i++) {
        P("FSTAT fd %d", fds[i]);
        if (fstat(fds[i], &s) == 0) pstat(&s, 1); else P(" %s", res(-1));
        P("\n");
    }
    char b[256];
    const char *links[] = {"/dev/stdin", "/dev/stdout", "/dev/stderr", "/dev/fd", "/dev/fd/0", "/dev/fd/10", "/dev/null"};
    for (size_t i = 0; i < sizeof links / sizeof *links; i++) {
        ssize_t n = readlink(links[i], b, sizeof b - 1);
        if (n >= 0) b[n] = 0;
        P("READLINK %s %s%s%s\n", links[i], res(n), n >= 0 ? " -> " : "", n >= 0 ? b : "");
    }
    const char *acc[] = {"/dev", "/dev/null", "/dev/tty", "/dev/stdin", "/dev/fd", "/dev/fd/10", "/dev/fd/15", "/dev/fd/16", "/dev/fd/99"};
    for (size_t i = 0; i < sizeof acc / sizeof *acc; i++)
        P("ACCESS %s F=%s R=%s W=%s X=%s\n", acc[i], res(access(acc[i], F_OK)), res(access(acc[i], R_OK)),
          res(access(acc[i], W_OK)), res(access(acc[i], X_OK)));
}

static void pstatfs(const char *label, const struct statfs *f) {
#if defined(__linux__)
    P("STATFS %s type=0x%lx bsize=%ld blocks=%llu bfree=%llu bavail=%llu files=%llu ffree=%llu fsid=%08x:%08x namelen=%ld frsize=%ld flags=0x%lx\n",
      label, (long)f->f_type, (long)f->f_bsize, (unsigned long long)f->f_blocks, (unsigned long long)f->f_bfree,
      (unsigned long long)f->f_bavail, (unsigned long long)f->f_files, (unsigned long long)f->f_ffree,
      (unsigned)f->f_fsid.__val[0], (unsigned)f->f_fsid.__val[1], (long)f->f_namelen, (long)f->f_frsize,
      (long)f->f_flags);
#else
    P("STATFS %s type=%u subtype=%u bsize=%u iosize=%d blocks=%llu bfree=%llu bavail=%llu files=%llu ffree=%llu fsid=%08x:%08x owner=%u flags=0x%x fstypename=%s on=%s from=%s\n",
      label, f->f_type, f->f_fssubtype, f->f_bsize, f->f_iosize, (unsigned long long)f->f_blocks,
      (unsigned long long)f->f_bfree, (unsigned long long)f->f_bavail, (unsigned long long)f->f_files,
      (unsigned long long)f->f_ffree, (unsigned)f->f_fsid.val[0], (unsigned)f->f_fsid.val[1], f->f_owner, f->f_flags,
      f->f_fstypename, f->f_mntonname, f->f_mntfromname);
#endif
}

static void sec_statfs(void) {
    const char *paths[] = {"/", "/dev", "/dev/null", "/dev/tty", "/dev/stdin", "/dev/fd", "/dev/fd/0", "/dev/fd/10",
                           "/dev/fd/11", "/dev/fd/12", "/dev/fd/17"};
    for (size_t i = 0; i < sizeof paths / sizeof *paths; i++) {
        struct statfs f;
        if (statfs(paths[i], &f) == 0) pstatfs(paths[i], &f);
        else P("STATFS %s %s\n", paths[i], res(-1));
    }
    const int fds[] = {10, 14, 12};
    for (size_t i = 0; i < sizeof fds / sizeof *fds; i++) {
        struct statfs f;
        char l[32];
        snprintf(l, sizeof l, "fstatfs(fd %d)", fds[i]);
        if (fstatfs(fds[i], &f) == 0) pstatfs(l, &f);
        else P("STATFS %s %s\n", l, res(-1));
    }
    // a descriptor obtained by opening /dev/fd/10
    int g = open("/dev/fd/10", O_RDONLY);
    struct statfs f;
    if (g >= 0 && fstatfs(g, &f) == 0) pstatfs("fstatfs(open(/dev/fd/10))", &f);
    else P("STATFS fstatfs(open(/dev/fd/10)) %s\n", res(-1));
    if (g >= 0) close(g);
}

// The path walk at /dev's boundary and inside it: each path's stat identity
// (kind, dev, ino) or errno, for stat and lstat.
static void sec_walk(void) {
    const char *paths[] = {"/dev",          "/dev/",          "/dev/.",        "/dev/..",       "/dev/../dev",
                           "//dev",         "/./dev",         "/dev//null",    "/dev/./null",   "/dev/../dev/null",
                           "/dev/null/",    "/dev/null/.",    "/dev/null/..",  "/dev/null/x",   "/dev/nonexistent",
                           "/dev/nonexistent/", "/dev/fd/",   "/dev/fd/.",     "/dev/fd/..",    "/dev/fd/../null",
                           "/dev/fd/0/",    "/dev/fd/10/",    "/dev/fd/10/.",  "/dev/fd/11/",   "/dev/fd/11/.",
                           "/dev/fd/11/..", "/dev/fd/11/file", "/dev/fd//10",  "/dev/fd/./10",  "/dev/fd/010",
                           "/dev/fd/+10",   "/dev/fd/1a",     "/dev/fd/-1",    "/dev/fd/4294967306",
                           "/dev/fd/99999999999999999999", "/dev/stdin/", "/dev/stdin/.", "/dev/stdout/", "/dev/tty/"};
    for (size_t i = 0; i < sizeof paths / sizeof *paths; i++) {
        struct stat s, l;
        P("WALK %s stat", paths[i]);
        if (stat(paths[i], &s) == 0) P(" %s dev=0x%llx ino=%llu", kind(s.st_mode), (unsigned long long)(uint32_t)s.st_dev, (unsigned long long)s.st_ino);
        else P(" %s", res(-1));
        P(" | lstat");
        if (lstat(paths[i], &l) == 0) P(" %s dev=0x%llx ino=%llu", kind(l.st_mode), (unsigned long long)(uint32_t)l.st_dev, (unsigned long long)l.st_ino);
        else P(" %s", res(-1));
        P("\n");
    }
    // chdir and getcwd
    char cwd[PATH_MAX], keep[PATH_MAX];
    getcwd(keep, sizeof keep);
    const char *dirs[] = {"/dev", "..", "/dev/fd", "..", "/dev/fd/11", "/dev/fd/10", "/dev/stdin", "/dev/null"};
    for (size_t i = 0; i < sizeof dirs / sizeof *dirs; i++) {
        int r = chdir(dirs[i]);
        P("CHDIR %s %s", dirs[i], res(r));
        char *c = getcwd(cwd, sizeof cwd);
        P(" getcwd=%s\n", c ? c : en(errno));
    }
    // fchdir to /dev and to /dev/fd
    int d = open("/dev", O_RDONLY | O_DIRECTORY);
    P("FCHDIR /dev %s", res(fchdir(d)));
    P(" getcwd=%s\n", getcwd(cwd, sizeof cwd) ? cwd : en(errno));
    close(d);
    d = open("/dev/fd", O_RDONLY | O_DIRECTORY);
    P("OPEN /dev/fd O_DIRECTORY %s", res(d));
    if (d >= 0) {
        P(" fchdir %s", res(fchdir(d)));
        P(" getcwd=%s", getcwd(cwd, sizeof cwd) ? cwd : en(errno));
        struct stat s;
        P(" fstatat(fd,\"10\")");
        if (fstatat(d, "10", &s, 0) == 0) P(" %s ino=%llu", kind(s.st_mode), (unsigned long long)s.st_ino);
        else P(" %s", res(-1));
        int g = openat(d, "10", O_RDONLY);
        P(" openat(fd,\"10\") %s", res(g));
        if (g >= 0) { P(" offset=%s", res(lseek(g, 0, SEEK_CUR))); close(g); }
        close(d);
    }
    P("\n");
    chdir(keep);
    // pathconf at /dev
    P("PATHCONF /dev NAME_MAX=%s PATH_MAX=%s LINK_MAX=%s\n", res(pathconf("/dev", _PC_NAME_MAX)),
      res(pathconf("/dev", _PC_PATH_MAX)), res(pathconf("/dev", _PC_LINK_MAX)));
    P("PATHCONF /dev/null NAME_MAX=%s\n", res(pathconf("/dev/null", _PC_NAME_MAX)));
    // a long name in /dev
    char longname[300] = "/dev/";
    memset(longname + 5, 'a', 256);
    longname[261] = 0;
    struct stat s;
    P("WALK /dev/<256 a> stat %s\n", res(stat(longname, &s)));
    longname[260] = 0;
    P("WALK /dev/<255 a> stat %s\n", res(stat(longname, &s)));
    char longer[1100] = "/dev/";
    memset(longer + 5, 'a', 1000);
    longer[1005] = 0;
    P("WALK /dev/<1000 a> stat %s\n", res(stat(longer, &s)));
    longer[261] = '/'; longer[262] = 'x'; longer[263] = 0;
    P("WALK /dev/<256 a>/x stat %s\n", res(stat(longer, &s)));
    char rootlong[300] = "/tmp/";
    memset(rootlong + 5, 'a', 256);
    rootlong[261] = 0;
    P("WALK /tmp/<256 a> stat %s\n", res(stat(rootlong, &s)));
    struct statfs fs;
    char fdlong[300] = "/dev/fd/";
    memset(fdlong + 8, '1', 256);
    fdlong[264] = 0;
    P("WALK /dev/fd/<256 digits> stat %s statfs %s\n", res(stat(fdlong, &s)), res(statfs(fdlong, &fs)));
    // Root on Linux succeeds here, so the name is removed again.
    int made = open("/dev/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", O_CREAT | O_WRONLY, 0644);
    P("WALK /dev/<32 a> open(O_CREAT) %s\n", res(made));
    if (made >= 0) { close(made); unlink("/dev/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"); }
}

// Is opening /dev/fd/N a dup of N (one open file description), or a fresh
// open of the object N names?
static void sec_fdsem(void) {
    char b[16];
    struct stat a, s;
    // offset sharing
    lseek(10, 3, SEEK_SET);
    int g = open("/dev/fd/10", O_RDONLY);
    P("FDSEM open(/dev/fd/10, O_RDONLY) %s", res(g));
    if (g >= 0) {
        P(" newfd_offset=%s", res(lseek(g, 0, SEEK_CUR)));
        ssize_t n = read(g, b, 2);
        P(" read2=%s", res(n));
        P(" fd10_offset_after=%s", res(lseek(10, 0, SEEK_CUR)));
        P(" getfl=0x%x getfd=%d", fcntl(g, F_GETFL), fcntl(g, F_GETFD));
        P(" write=%s", res(write(g, "W", 1)));
        fstat(g, &s);
        fstat(10, &a);
        P(" fstat_same_as_fd10=%d", same_id(&s, &a));
        // flock contention: one description or two?
        flock(10, LOCK_EX | LOCK_NB);
        P(" flock(new,EX|NB) after flock(10,EX)=%s", res(flock(g, LOCK_EX | LOCK_NB)));
        flock(10, LOCK_UN);
        close(g);
    }
    P("\n");
    lseek(10, 3, SEEK_SET);
    g = open("/dev/fd/10", O_RDONLY | O_CLOEXEC);
    P("FDSEM open(/dev/fd/10, O_RDONLY|O_CLOEXEC) %s", res(g));
    if (g >= 0) { P(" getfl=0x%x getfd=%d", fcntl(g, F_GETFL), fcntl(g, F_GETFD)); close(g); }
    P("\n");
    // status flags set on the original after the fact
    fcntl(10, F_SETFL, fcntl(10, F_GETFL) | O_NONBLOCK);
    g = open("/dev/fd/10", O_RDWR);
    P("FDSEM fd10 O_NONBLOCK set, open(/dev/fd/10, O_RDWR) %s", res(g));
    if (g >= 0) {
        P(" getfl=0x%x", fcntl(g, F_GETFL));
        fcntl(g, F_SETFL, 0);
        P(" after F_SETFL(new,0) fd10_getfl=0x%x", fcntl(10, F_GETFL));
        close(g);
    }
    fcntl(10, F_SETFL, fcntl(10, F_GETFL) & ~O_NONBLOCK);
    P("\n");
    // access modes against the original's
    struct {
        int fd;
        const char *fdname;
    } origs[] = {{10, "10(O_RDWR)"}, {15, "15(O_RDONLY)"}, {16, "16(O_WRONLY|O_APPEND)"}};
    int modes[] = {O_RDONLY, O_WRONLY, O_RDWR, O_WRONLY | O_APPEND};
    const char *mn[] = {"O_RDONLY", "O_WRONLY", "O_RDWR", "O_WRONLY|O_APPEND"};
    for (int i = 0; i < 3; i++)
        for (int m = 0; m < 4; m++) {
            char path[32];
            snprintf(path, sizeof path, "/dev/fd/%d", origs[i].fd);
            g = open(path, modes[m]);
            P("FDMODE orig=%s open %s %s", origs[i].fdname, mn[m], res(g));
            if (g >= 0) { P(" getfl=0x%x", fcntl(g, F_GETFL)); close(g); }
            P("\n");
        }
    // O_TRUNC, O_CREAT, O_EXCL, O_DIRECTORY, O_NOFOLLOW through /dev/fd/N
    struct {
        int flags;
        const char *name;
    } extra[] = {{O_WRONLY | O_TRUNC, "O_WRONLY|O_TRUNC"}, {O_RDONLY | O_TRUNC, "O_RDONLY|O_TRUNC"},
                 {O_RDONLY | O_CREAT, "O_RDONLY|O_CREAT"}, {O_RDONLY | O_CREAT | O_EXCL, "O_RDONLY|O_CREAT|O_EXCL"},
                 {O_RDONLY | O_DIRECTORY, "O_RDONLY|O_DIRECTORY"}, {O_RDONLY | O_NOFOLLOW, "O_RDONLY|O_NOFOLLOW"}};
    for (size_t i = 0; i < sizeof extra / sizeof *extra; i++) {
        fstat(10, &a);
        g = open("/dev/fd/10", extra[i].flags, 0600);
        P("FDFLAGS /dev/fd/10 %s %s", extra[i].name, res(g));
        if (g >= 0) close(g);
        fstat(10, &s);
        P(" file_size_before=%lld after=%lld\n", (long long)a.st_size, (long long)s.st_size);
        if (s.st_size != a.st_size) { lseek(10, 0, SEEK_SET); write(10, "0123456789", 10); }
    }
    const char *others[] = {"/dev/fd/11", "/dev/fd/12", "/dev/fd/13", "/dev/fd/14", "/dev/fd/17", "/dev/fd/99", "/dev/fd/0", "/dev/fd/1"};
    for (size_t i = 0; i < sizeof others / sizeof *others; i++) {
        for (int m = 0; m < 3; m++) {
            g = open(others[i], modes[m] | (i == 0 && m == 0 ? O_DIRECTORY : 0));
            P("FDOPEN %s %s%s %s", others[i], mn[m], i == 0 && m == 0 ? "|O_DIRECTORY" : "", res(g));
            if (g >= 0) {
                fstat(g, &s);
                P(" getfl=0x%x kind=%s ino=%llu", fcntl(g, F_GETFL), kind(s.st_mode), (unsigned long long)s.st_ino);
                close(g);
            }
            P("\n");
        }
    }
    g = open("/dev/fd/11", O_RDONLY);
    P("FDOPEN /dev/fd/11 O_RDONLY %s", res(g));
    if (g >= 0) {
        fstat(g, &s);
        P(" getfl=0x%x kind=%s ino=%llu offset=%s", fcntl(g, F_GETFL), kind(s.st_mode), (unsigned long long)s.st_ino, res(lseek(g, 0, SEEK_CUR)));
        close(g);
    }
    P("\n");
    // a pipe through /dev/fd: one pipe or a fresh open of it?
    g = open("/dev/fd/12", O_RDONLY | O_NONBLOCK);
    if (g >= 0) {
        write(13, "pq", 2);
        ssize_t n = read(g, b, 1);
        ssize_t n2 = read(12, b + 1, 1);
        P("FDPIPE via /dev/fd/12 read1=%s then fd12 read1=%s getfl(fd12)=0x%x\n", res(n), res(n2), fcntl(12, F_GETFL));
        fcntl(12, F_SETFL, fcntl(12, F_GETFL) & ~O_NONBLOCK);
        close(g);
    } else P("FDPIPE open %s\n", res(-1));
    // /dev/stdin when fd 0 is a pipe; and the standard links when their fd is closed
    g = open("/dev/stdin", O_RDONLY);
    P("FDSTD open(/dev/stdin) %s", res(g));
    if (g >= 0) {
        fstat(g, &s); fstat(0, &a);
        P(" same_as_fd0=%d getfl=0x%x read=%s", same_id(&s, &a), fcntl(g, F_GETFL), res(read(g, b, 1)));
        close(g);
    }
    P("\n");
    g = open("/dev/stdout", O_WRONLY);
    P("FDSTD open(/dev/stdout, O_WRONLY) %s", res(g));
    if (g >= 0) { P(" write=%s", res(write(g, "x", 1))); close(g); }
    P("\n");
    g = open("/dev/stdout", O_RDONLY);
    P("FDSTD open(/dev/stdout, O_RDONLY) %s\n", res(g));
    if (g >= 0) close(g);
    fflush(o);
    pid_t pid = fork();
    if (pid == 0) {
        close(0);
        struct stat t;
        P("FDCLOSED fd0 closed: stat(/dev/stdin) %s", res(stat("/dev/stdin", &t)));
        P(" lstat(/dev/stdin) %s", res(lstat("/dev/stdin", &t)));
        P(" stat(/dev/fd/0) %s", res(stat("/dev/fd/0", &t)));
        int h = open("/dev/stdin", O_RDONLY);
        P(" open(/dev/stdin) %s", res(h));
        P("\n");
        fflush(o);
        _exit(0);
    }
    waitpid(pid, NULL, 0);
    // symlinks to /dev/fd/N from a regular filesystem
    char l1[300], l2[300], l3[300];
    snprintf(l1, sizeof l1, "%s/l-fd10", scratch);
    snprintf(l2, sizeof l2, "%s/l-stdin", scratch);
    snprintf(l3, sizeof l3, "%s/l-devnull", scratch);
    symlink("/dev/fd/10", l1);
    symlink("/dev/stdin", l2);
    symlink("/dev/null", l3);
    lseek(10, 3, SEEK_SET);
    g = open(l1, O_RDONLY);
    P("FDLINK open(link -> /dev/fd/10) %s", res(g));
    if (g >= 0) { P(" offset=%s", res(lseek(g, 0, SEEK_CUR))); close(g); }
    stat(l1, &s); fstat(10, &a);
    P(" stat_same_as_fd10=%d\n", same_id(&s, &a));
    g = open(l2, O_RDONLY);
    P("FDLINK open(link -> /dev/stdin) %s", res(g));
    if (g >= 0) { fstat(g, &s); fstat(0, &a); P(" same_as_fd0=%d", same_id(&s, &a)); close(g); }
    P("\n");
    g = open(l3, O_WRONLY);
    P("FDLINK open(link -> /dev/null) %s write=%s\n", res(g), g >= 0 ? res(write(g, "x", 1)) : "-");
    if (g >= 0) close(g);
    // /dev/fd listing: which numbers it lists
    int extra_fd = dup2(14, 40);
    DIR *dd = opendir("/dev/fd");
    P("FDLIST opendir %s:", dd ? "ok" : res(-1));
    struct dirent *e;
    int n = 0;
    while (dd && (e = readdir(dd))) {
        n++;
        if (n <= 40) P(" %s(t%d,i%llu)", e->d_name, e->d_type, (unsigned long long)e->d_ino);
    }
    P(" total=%d (fd 40 open: %d)\n", n, extra_fd);
    if (dd) closedir(dd);
    close(40);
}

// Each node opened every way, and what one read, write, lseek, ioctl and
// isatty say about the descriptor.
static void node_ops(const char *path) {
    int modes[] = {O_RDONLY, O_WRONLY, O_RDWR, O_RDONLY | O_CREAT, O_RDONLY | O_CREAT | O_EXCL, O_RDONLY | O_DIRECTORY,
                   O_RDONLY | O_NOFOLLOW, O_WRONLY | O_TRUNC, O_RDONLY | O_NONBLOCK};
    const char *mn[] = {"O_RDONLY", "O_WRONLY", "O_RDWR", "O_RDONLY|O_CREAT", "O_RDONLY|O_CREAT|O_EXCL", "O_RDONLY|O_DIRECTORY",
                        "O_RDONLY|O_NOFOLLOW", "O_WRONLY|O_TRUNC", "O_RDONLY|O_NONBLOCK"};
    for (int m = 0; m < 9; m++) {
        int fd = open(path, modes[m], 0600);
        P("OPEN %s %s %s", path, mn[m], res(fd));
        if (fd >= 0) {
            struct stat s;
            fstat(fd, &s);
            P(" getfl=0x%x fstat:", fcntl(fd, F_GETFL));
            pstat(&s, 0);
            if (m == 0 || m == 1 || m == 2) {
                char b[8];
                int avail = -12345;
                // O_NONBLOCK so that a read cannot sleep. Restored below: a
                // descriptor opened through /dev/stdin shares fd 0's
                // description, so the flag would otherwise reach fd 0.
                int saved = fcntl(fd, F_GETFL);
                fcntl(fd, F_SETFL, saved | O_NONBLOCK);
                P(" | read=%s", (m == 0 || m == 2) ? res(read(fd, b, sizeof b)) : "-");
                P(" write=%s", (m == 1 || m == 2) ? res(write(fd, "", 0)) : "-");
                P(" lseek(0,CUR)=%s lseek(5,SET)=%s", res(lseek(fd, 0, SEEK_CUR)), res(lseek(fd, 5, SEEK_SET)));
                P(" FIONREAD=%s(%d)", res(ioctl(fd, FIONREAD, &avail)), avail);
                struct termios t;
                P(" tcgetattr=%s isatty=%d", res(tcgetattr(fd, &t)), isatty(fd));
                fcntl(fd, F_SETFL, saved);
            }
            close(fd);
        }
        P("\n");
    }
}

static void sec_open(void) {
    const char *paths[] = {"/dev/null", "/dev/tty", "/dev/stdin", "/dev/stdout", "/dev/stderr", "/dev/fd"};
    for (size_t i = 0; i < sizeof paths / sizeof *paths; i++) node_ops(paths[i]);
}

// Directory operations in /dev as the running user: every name a fresh one
// or an existing node; nothing is left behind if a call unexpectedly succeeds.
static void sec_ns(void) {
    P("NS euid=%u egid=%u\n", (unsigned)geteuid(), (unsigned)getegid());
    char f[300], d[300];
    snprintf(f, sizeof f, "%s/nsfile", scratch);
    snprintf(d, sizeof d, "%s/nsdir", scratch);
    close(open(f, O_CREAT | O_WRONLY, 0644));
    mkdir(d, 0755);
#define NS(label, call) P("NS %s %s\n", label, res(call))
    NS("open(/dev/pawprobe, O_CREAT|O_WRONLY)", open("/dev/pawprobe", O_CREAT | O_WRONLY, 0644));
    unlink("/dev/pawprobe");
    NS("mkdir(/dev/pawprobedir)", mkdir("/dev/pawprobedir", 0755));
    rmdir("/dev/pawprobedir");
    NS("mkfifo(/dev/pawprobefifo)", mkfifo("/dev/pawprobefifo", 0644));
    unlink("/dev/pawprobefifo");
    NS("symlink(x, /dev/pawprobe)", symlink("x", "/dev/pawprobe"));
    unlink("/dev/pawprobe");
    NS("link(/dev/null, /dev/pawprobe)", link("/dev/null", "/dev/pawprobe"));
    unlink("/dev/pawprobe");
    NS("mkdir(/dev/fd/pawprobe)", mkdir("/dev/fd/pawprobe", 0755));
    NS("open(/dev/fd/pawprobe, O_CREAT)", open("/dev/fd/pawprobe", O_CREAT | O_WRONLY, 0644));
    NS("open(/dev/fd/77, O_CREAT)", open("/dev/fd/77", O_CREAT | O_WRONLY, 0644));
    NS("unlink(/dev/null)", geteuid() == 0 ? -2 : unlink("/dev/null"));
    NS("unlink(/dev/stdin)", geteuid() == 0 ? -2 : unlink("/dev/stdin"));
    NS("unlink(/dev/fd)", geteuid() == 0 ? -2 : unlink("/dev/fd"));
    NS("unlink(/dev/fd/10)", unlink("/dev/fd/10"));
    NS("unlink(/dev/nonexistent)", unlink("/dev/pawnonexistent"));
    NS("rmdir(/dev/fd)", geteuid() == 0 ? -2 : rmdir("/dev/fd"));
    NS("rmdir(/dev/fd/11)", rmdir("/dev/fd/11"));
    NS("rmdir(/dev/null)", rmdir("/dev/null"));
    NS("rmdir(/dev/nonexistent)", rmdir("/dev/pawnonexistent"));
    NS("rename(/dev/null, /dev/pawprobe)", geteuid() == 0 ? -2 : rename("/dev/null", "/dev/pawprobe"));
    NS("rename(/dev/null, /dev/zero)", geteuid() == 0 ? -2 : rename("/dev/null", "/dev/zero"));
    NS("rename(/dev/nonexistent, /dev/pawprobe)", rename("/dev/pawnonexistent", "/dev/pawprobe"));
    NS("rename(/dev/fd/10, tmp/new)", rename("/dev/fd/10", f));
    NS("rename(tmpfile, /dev/fd/10)", rename(f, "/dev/fd/10"));
    NS("rename(/dev/stdin, tmp/new)", rename("/dev/stdin", d));
    NS("chmod(/dev, 0555)", geteuid() == 0 ? -2 : chmod("/dev", 0555));
    NS("chmod(/dev/null, 0666)", geteuid() == 0 ? -2 : chmod("/dev/null", 0666));
    NS("chmod(/dev/tty, 0666)", geteuid() == 0 ? -2 : chmod("/dev/tty", 0666));
    NS("chmod(/dev/fd, 0555)", geteuid() == 0 ? -2 : chmod("/dev/fd", 0555));
    struct stat before, after;
    fstat(10, &before);
    NS("chmod(/dev/fd/10, 0640)", chmod("/dev/fd/10", 0640));
    fstat(10, &after);
    P("NS   file mode via fd 10 before=0%o after=0%o\n", (unsigned)before.st_mode, (unsigned)after.st_mode);
    fchmod(10, 0644);
    NS("chown(/dev/null, -1, -1)", chown("/dev/null", (uid_t)-1, (gid_t)-1));
    NS("chown(/dev/null, euid, egid)", geteuid() == 0 ? -2 : chown("/dev/null", geteuid(), getegid()));
    NS("chown(/dev, -1, -1)", chown("/dev", (uid_t)-1, (gid_t)-1));
    NS("lchown(/dev/stdin, -1, -1)", lchown("/dev/stdin", (uid_t)-1, (gid_t)-1));
    NS("chown(/dev/fd/10, -1, -1)", chown("/dev/fd/10", (uid_t)-1, (gid_t)-1));
    NS("chown(/dev/fd/10, euid, egid)", chown("/dev/fd/10", geteuid(), getegid()));
    struct stat t0, t1;
    stat("/dev/null", &t0);
    NS("utimes(/dev/null, NULL)", utimes("/dev/null", NULL));
    struct timeval tv[2] = {{1000000000, 0}, {1000000000, 0}};
    NS("utimes(/dev/null, 2001)", geteuid() == 0 ? -2 : utimes("/dev/null", tv));
    stat("/dev/null", &t1);
    P("NS   /dev/null mtime before=%lld after=%lld\n", (long long)MTIM(t0).tv_sec, (long long)MTIM(t1).tv_sec);
    NS("utimes(/dev, NULL)", geteuid() == 0 ? -2 : utimes("/dev", NULL));
    NS("utimes(/dev/tty, NULL)", utimes("/dev/tty", NULL));
    fstat(10, &t0);
    NS("utimes(/dev/fd/10, 2001)", utimes("/dev/fd/10", tv));
    fstat(10, &t1);
    P("NS   file mtime via fd 10 before=%lld after=%lld\n", (long long)MTIM(t0).tv_sec, (long long)MTIM(t1).tv_sec);
    NS("truncate(/dev/fd/10, 4)", truncate("/dev/fd/10", 4));
    fstat(10, &t1);
    P("NS   file size via fd 10 after=%lld\n", (long long)t1.st_size);
    NS("truncate(/dev/fd/15, 2) (fd 15 O_RDONLY)", truncate("/dev/fd/15", 2));
    fstat(10, &t1);
    P("NS   file size via fd 10 after=%lld\n", (long long)t1.st_size);
    NS("truncate(/dev/stdin, 3) (fd 0 a pipe)", truncate("/dev/stdin", 3));
    ftruncate(10, 0);
    lseek(10, 0, SEEK_SET);
    write(10, "0123456789", 10);
    NS("truncate(/dev/null, 0)", truncate("/dev/null", 0));
    unlink(f);
    rmdir(d);
}

// readdir of /dev and of /: its shape, and the entries a client could use.
static void sec_list(void) {
    DIR *d = opendir("/dev");
    if (!d) { P("LIST opendir %s\n", res(-1)); return; }
    struct dirent *e;
    int n = 0, types[16] = {0};
    const char *wanted[] = {".", "..", "console", "tty", "null", "zero", "random", "urandom", "fd", "stdin", "stdout", "stderr", "ptmx", "dtracehelper", "klog"};
    P("LIST /dev:");
    while ((e = readdir(d)) != NULL) {
        for (size_t i = 0; i < sizeof wanted / sizeof *wanted; i++)
            if (!strcmp(e->d_name, wanted[i])) P(" %d:%s(t%d,i%llu)", n, e->d_name, e->d_type, (unsigned long long)e->d_ino);
        types[e->d_type & 15]++;
        n++;
    }
    P("\nLIST /dev total=%d by d_type:", n);
    for (int i = 0; i < 16; i++) if (types[i]) P(" t%d=%d", i, types[i]);
    P("\n");
    closedir(d);
    // name prefixes, to show what the listing is made of
    d = opendir("/dev");
    char seen[512][16];
    int counts[512] = {0}, ns = 0;
    while ((e = readdir(d)) != NULL) {
        char pre[16];
        size_t k = 0;
        while (e->d_name[k] && k < 15 && !(e->d_name[k] >= '0' && e->d_name[k] <= '9')) { pre[k] = e->d_name[k]; k++; }
        pre[k] = 0;
        int j;
        for (j = 0; j < ns; j++) if (!strcmp(seen[j], pre)) break;
        if (j == ns && ns < 512) { strcpy(seen[ns], pre); ns++; }
        if (j < 512) counts[j]++;
    }
    closedir(d);
    P("LIST /dev prefixes:");
    for (int j = 0; j < ns; j++) P(" %s*%d", seen[j], counts[j]);
    P("\n");
    d = opendir("/");
    while ((e = readdir(d)) != NULL)
        if (!strcmp(e->d_name, "dev")) P("LIST / dev: d_ino=%llu d_type=%d\n", (unsigned long long)e->d_ino, e->d_type);
    closedir(d);
}

// /dev/tty with no controlling terminal (a fresh session), and with a pty as
// the controlling terminal.
static void sec_tty(void) {
    P("TTY parent isatty(0,1,2)=%d,%d,%d\n", isatty(0), isatty(1), isatty(2));
    fflush(o);
    pid_t pid = fork();
    if (pid == 0) {
        setsid();
        int fd = open("/dev/tty", O_RDWR);
        P("TTY no-ctty open(/dev/tty, O_RDWR) %s", res(fd));
        fd = open("/dev/tty", O_RDONLY | O_NONBLOCK);
        P(" O_RDONLY|O_NONBLOCK %s", res(fd));
        struct stat s;
        P(" stat(/dev/tty) %s\n", res(stat("/dev/tty", &s)));
        fflush(o);
        _exit(0);
    }
    waitpid(pid, NULL, 0);
    pid = fork();
    if (pid == 0) {
        setsid();
        int m = posix_openpt(O_RDWR | O_NOCTTY);
        if (m < 0) { P("TTY posix_openpt %s\n", res(-1)); fflush(o); _exit(0); }
        grantpt(m);
        unlockpt(m);
        char *name = ptsname(m);
        int sl = open(name, O_RDWR);
        int sc = ioctl(sl, TIOCSCTTY, 0);
        P("TTY pty slave=%s open %s TIOCSCTTY %s\n", name, res(sl), res(sc));
        struct stat ss, ts_, ns_;
        fstat(sl, &ss);
        P("TTY pty slave fstat:");
        pstat(&ss, 0);
        P("\n");
        int fd = open("/dev/tty", O_RDWR);
        P("TTY ctty open(/dev/tty, O_RDWR) %s", res(fd));
        if (fd >= 0) {
            fstat(fd, &ts_);
            stat("/dev/tty", &ns_);
            P(" fstat:");
            pstat(&ts_, 0);
            P(" | same_as_slave=%d same_as_node=%d isatty=%d", same_id(&ts_, &ss), same_id(&ts_, &ns_), isatty(fd));
            int avail = -1;
            P(" FIONREAD=%s(%d)", res(ioctl(fd, FIONREAD, &avail)), avail);
            P(" lseek=%s", res(lseek(fd, 0, SEEK_CUR)));
            P(" write=%s", res(write(fd, "hi\n", 3)));
            fcntl(fd, F_SETFL, O_NONBLOCK);
            char b[8];
            P(" read(nonblock)=%s", res(read(fd, b, sizeof b)));
            struct statfs f;
            if (fstatfs(fd, &f) == 0) {
#if defined(__APPLE__)
                P(" fstatfs=%s", f.f_fstypename);
#else
                P(" fstatfs=0x%lx", (long)f.f_type);
#endif
            } else P(" fstatfs=%s", res(-1));
            close(fd);
        }
        P("\n");
        fflush(o);
        _exit(0);
    }
    waitpid(pid, NULL, 0);
}

// Which calls move /dev's own timestamps, and what /dev/fd/N and the
// standard links report as theirs.
static void sec_times(void) {
    struct stat a, b, c, d;
    stat("/dev", &a);
    sleep(2);
    struct stat x;
    stat("/dev/null", &x);
    stat("/dev", &b);
    sleep(2);
    DIR *dd = opendir("/dev");
    while (readdir(dd)) {}
    closedir(dd);
    stat("/dev", &c);
    sleep(2);
    int fd = open("/dev/pawprobe", O_CREAT | O_WRONLY, 0644);
    if (fd >= 0) { close(fd); unlink("/dev/pawprobe"); }
    stat("/dev", &d);
    P("TIMES /dev start:");
    ts("a", ATIM(a)); ts("m", MTIM(a)); ts("c", CTIM(a));
    P("\nTIMES /dev after lookup of null:");
    ts("a", ATIM(b)); ts("m", MTIM(b)); ts("c", CTIM(b));
    P("\nTIMES /dev after readdir:");
    ts("a", ATIM(c)); ts("m", MTIM(c)); ts("c", CTIM(c));
    P("\nTIMES /dev after failed create:");
    ts("a", ATIM(d)); ts("m", MTIM(d)); ts("c", CTIM(d));
    P("\n");
    const char *paths[] = {"/dev/tty", "/dev/fd", "/dev/stdin", "/dev/fd/10", "/dev/fd/12"};
    for (size_t i = 0; i < sizeof paths / sizeof *paths; i++) {
        struct stat s0, s1;
        lstat(paths[i], &s0);
        sleep(2);
        lstat(paths[i], &s1);
        P("TIMES %s lstat twice 2s apart:", paths[i]);
        ts("a0", ATIM(s0)); ts("a1", ATIM(s1)); ts("m0", MTIM(s0)); ts("m1", MTIM(s1)); ts("c0", CTIM(s0)); ts("c1", CTIM(s1));
        P("\n");
    }
    // a read through /dev/stdin: does it stamp the pipe, the link, or nothing?
    struct stat l0, l1, p0, p1;
    lstat("/dev/stdin", &l0);
    fstat(0, &p0);
    sleep(2);
    int g = open("/dev/stdin", O_RDONLY);
    char ch;
    read(g, &ch, 1);
    close(g);
    lstat("/dev/stdin", &l1);
    fstat(0, &p1);
    P("TIMES read via /dev/stdin: link");
    ts("a0", ATIM(l0)); ts("a1", ATIM(l1));
    P(" pipe");
    ts("a0", ATIM(p0)); ts("a1", ATIM(p1));
    P("\n");
}

static void setup(void) {
    if (!mkdtemp(scratch)) { perror("mkdtemp"); exit(1); }
    snprintf(filepath, sizeof filepath, "%s/file", scratch);
    int f = open(filepath, O_CREAT | O_RDWR, 0644);
    write(f, "0123456789", 10);
    dup2(f, 10); close(f);
    lseek(10, 3, SEEK_SET);
    int d = open(scratch, O_RDONLY | O_DIRECTORY);
    dup2(d, 11); close(d);
    int p[2];
    pipe(p);
    dup2(p[0], 12); dup2(p[1], 13); close(p[0]); close(p[1]);
    int n = open("/dev/null", O_RDONLY);
    dup2(n, 14); close(n);
    int r = open(filepath, O_RDONLY);
    dup2(r, 15); close(r);
    int w = open(filepath, O_WRONLY | O_APPEND);
    dup2(w, 16); close(w);
    int sp[2];
    socketpair(AF_UNIX, SOCK_STREAM, 0, sp);
    dup2(sp[0], 17); close(sp[0]);
    dup2(sp[1], 18); close(sp[1]);
    // standard streams: stdin a pipe whose writer is closed; stdout and stderr
    // pipes whose readers are held at 61 and 62.
    int in[2], out[2], err[2];
    pipe(in); pipe(out); pipe(err);
    close(in[1]);
    dup2(in[0], 0); close(in[0]);
    dup2(out[1], 1); close(out[1]);
    dup2(err[1], 2); close(err[1]);
    dup2(out[0], 61); close(out[0]);
    dup2(err[0], 62); close(err[0]);
    // keep the probe's own descriptors from moving the lowest free number
    for (int i = 3; i < 10; i++) close(i);
}

static void teardown(void) {
    char p[300];
    const char *names[] = {"file", "l-fd10", "l-stdin", "l-devnull", "nsfile"};
    for (size_t i = 0; i < sizeof names / sizeof *names; i++) {
        snprintf(p, sizeof p, "%s/%s", scratch, names[i]);
        unlink(p);
    }
    snprintf(p, sizeof p, "%s/nsdir", scratch);
    rmdir(p);
    rmdir(scratch);
}

int main(int argc, char **argv) {
    alarm(120);
    signal(SIGPIPE, SIG_IGN);
    int out = dup2(1, 60);
    o = fdopen(out, "w");
    setvbuf(o, NULL, _IOLBF, 0);
    setup();
#if defined(__APPLE__)
    P("# Darwin probe, euid=%u\n", (unsigned)geteuid());
#else
    P("# Linux probe, euid=%u\n", (unsigned)geteuid());
#endif
    int all = argc < 2;
    for (int i = 1; i <= (all ? 1 : argc - 1); i++) {
        const char *s = all ? NULL : argv[i];
        if (all || !strcmp(s, "stat")) sec_stat();
        if (all || !strcmp(s, "statfs")) sec_statfs();
        if (all || !strcmp(s, "walk")) sec_walk();
        if (all || !strcmp(s, "fdsem")) sec_fdsem();
        if (all || !strcmp(s, "open")) sec_open();
        if (all || !strcmp(s, "ns")) sec_ns();
        if (all || !strcmp(s, "list")) sec_list();
        if (all || !strcmp(s, "tty")) sec_tty();
        if (!all && !strcmp(s, "times")) sec_times();
    }
    teardown();
    return 0;
}
