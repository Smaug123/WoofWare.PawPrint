// Measures what at-dirfd.c left open about utimensat(2): what its times mean,
// who may set them, which of an object's timestamps move, and in what order
// its failures are reported.
//
// Sections:
//   CONST    UTIME_NOW, UTIME_OMIT and the AT_* bits utimensat reads.
//   EFFECT   utimensat(AT_FDCWD, P, T, F) for every pathname P of the cell,
//            every times argument T below and F 0 or AT_SYMLINK_NOFOLLOW:
//            the answer, then every timestamp of every object in the cell
//            that moved.
//   NULLFD   a NULL pathname with each kind of dirfd, each times argument
//            and each flag word: the answer and what moved (for a pipe,
//            what fstat of each end reports; for a socket, fstat's).
//   EMPTY    (Linux) AT_EMPTY_PATH with the empty pathname, NULL and "f",
//            for each kind of dirfd.
//   ORDER    two failures at once: which is reported.
//   NSECX    (Linux, root) the extreme seconds of NSEC, set through a NULL
//            pathname on a pipe and on /dev/null.
//   NSECOBJ  (Linux) a NULL pathname on a file, another user's file,
//            /dev/null, each end of the caller's pipe and of root's, a
//            socket and an epoll instance, with a nanosecond field of 1e9
//            (BAD) or -1 (NEG) beside NOW, OMIT and an explicit time, and
//            NOW/NOW, X/X and NOW/OMIT for reference: whether EINVAL comes
//            before what the object answers.
//   NSEC     one times argument whose atime is (S, N), mtime UTIME_OMIT, for
//            a sweep of S and N: the answer and the atime stored. Then the
//            same for mtime, with the birth time (Darwin).
//   DFLAGS   (Darwin) every single flag bit against a final link, a link
//            in the middle of the path, a path through "..", and an
//            absolute path: the answer and what moved.
//   TRAIL    "f/", "d/", "lf/" and "ld/", with and without
//            AT_SYMLINK_NOFOLLOW.
//
// Times arguments: NULL (a null pointer), FAULT (a PROT_NONE page), or a pair
// "access/modification" of NOW (UTIME_NOW), OMIT (UTIME_OMIT), X (1500000000
// .333333333 for access, 1600000000.444444444 for modification), or E
// (1100000000.555555555, before every initial time).
//
// The cell is the cwd ("."; root's on Linux, the caller's on Darwin, 0777):
// f (0644), ro (0444), d/ (0755, holding x), lf -> f, ld -> d, dang -> nx,
// all the caller's. On Linux also uid 2000's tw (0666), tr (0644), tg (group
// 1000, 0664) and link tl -> f, which root makes before the child drops to
// the caller's credentials. On Darwin, another user's objects are
// /Users/Shared (root's, 1777), /private/etc/hosts (root's, 0644) and /tmp
// (root's link to private/tmp). Every object of the cell, and on Linux
// /dev/null, is given access time 1000000000.111111111 and modification time
// 1200000000.222222222 by its owner before the call (links with
// AT_SYMLINK_NOFOLLOW); Darwin's birth time then follows the modification
// time back.
//
// A timestamp that moved is printed as its value, or as "now" if it lies
// within 50ms before the call and the call's return; "now/us" is a "now"
// with no digits below the microsecond.
//
// Each cell runs in a forked child, in a fresh directory of its own, under
// alarm(10). On Linux every row is run as root and again as uid 1000 with
// group 1000 and no supplementary groups, which root drops to in the child.
// Linux calls the raw syscall, so glibc decides nothing; Darwin's utimensat
// is its libc's (setattrlistat underneath), so it is called through libc.
//
// Linux, as root (ext4 at /tmp, tmpfs at /dev/shm):
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/utimensat-rules.c && /tmp/p "$(mktemp -d)" "$(mktemp -d -p /dev/shm)"' > utimensat-rules.linux-6.18.5-aarch64.txt
// Darwin, as an ordinary user:
//   nix develop -c clang -Wall -Werror -o /tmp/p utimensat-rules.c && /tmp/p "$(mktemp -d)" > utimensat-rules.darwin-27.0-uid501.txt
//
// Measured 2026-10-07 on Linux 6.18.5 aarch64 (gcc:14, glibc; ext4 at /tmp
// and tmpfs at /dev/shm; root, and uid 1000 dropping in the child) and
// Darwin 27.0 arm64 (APFS, uid 501). The outputs are beside this file, and
// WoofWare.PosixKernel.Test/TestUTimensAt.fs replays them.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

#ifdef __APPLE__
#define ATIM st_atimespec
#define MTIM st_mtimespec
#define CTIM st_ctimespec
#define BTIM st_birthtimespec
#else
#define ATIM st_atim
#define MTIM st_mtim
#define CTIM st_ctim
#endif

static const char *en(int e) {
    static char buf[32];
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case ELOOP: return "ELOOP";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EOPNOTSUPP: return "EOPNOTSUPP";
#if defined(ENOTSUP) && ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
    case EISDIR: return "EISDIR";
    case EROFS: return "EROFS";
    case EXDEV: return "EXDEV";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static uid_t caller;
static char cellpath[PATH_MAX];
static char *protnone;
static char *overlong;

static void die(const char *what) {
    perror(what);
    _exit(2);
}

#ifdef __linux__
static const uid_t other_uid = 2000;
static int call(int d, const char *p, const struct timespec *t, int f) {
    return syscall(SYS_utimensat, d, p, t, f) < 0 ? errno : 0;
}
static const int nofollow = AT_SYMLINK_NOFOLLOW;
#else
static int call(int d, const char *p, const struct timespec *t, int f) {
    return utimensat(d, p, t, f) < 0 ? errno : 0;
}
static const int nofollow = AT_SYMLINK_NOFOLLOW;
#endif

// ------------------------------------------------------------------ the cell
static const struct timespec initial_a = {1000000000, 111111111};
static const struct timespec initial_m = {1200000000, 222222222};

static void set_initial(const char *p) {
    struct timespec t[2] = {initial_a, initial_m};
    if (utimensat(AT_FDCWD, p, t, AT_SYMLINK_NOFOLLOW) < 0) die(p);
}

#ifdef __linux__
static const char *objects[] = {".", "f", "ro", "d", "d/x", "lf", "ld", "dang", "tw", "tr", "tg", "tl"};
#else
static const char *objects[] = {".", "f", "ro", "d", "d/x", "lf", "ld", "dang", "/Users/Shared", "/private/etc/hosts", "/tmp"};
#endif
#define NOBJECTS (int)(sizeof objects / sizeof objects[0])

static int root_pipe[2] = {-1, -1};

static void cell(void) {
    cellno++;
    snprintf(cellpath, sizeof cellpath, "%s/c%05d", base, cellno);
    if (mkdir(cellpath, 0777) < 0 || chmod(cellpath, 0777) < 0) die("mkdir cell");
    if (chdir(cellpath) < 0) die("chdir cell");
#ifdef __linux__
    int t;
    t = open("tw", O_WRONLY | O_CREAT | O_EXCL, 0666);
    if (t < 0 || fchown(t, other_uid, other_uid) < 0 || fchmod(t, 0666) < 0) die("tw");
    close(t);
    t = open("tr", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (t < 0 || fchown(t, other_uid, other_uid) < 0) die("tr");
    close(t);
    t = open("tg", O_WRONLY | O_CREAT | O_EXCL, 0664);
    if (t < 0 || fchown(t, other_uid, 1000) < 0 || fchmod(t, 0664) < 0) die("tg");
    close(t);
    if (symlink("f", "tl") < 0 || lchown("tl", other_uid, other_uid) < 0) die("tl");
    set_initial("tw");
    set_initial("tr");
    set_initial("tg");
    set_initial("tl");
    // /dev/null is the machine's, shared by every cell: give it the same
    // times in each, so that what a cell sets is not what the last one left.
    set_initial("/dev/null");
    // A pipe root made: both its ends, held across the drop.
    if (pipe(root_pipe) < 0) die("root pipe");
    if (caller != 0) {
        if (setgroups(0, NULL) < 0 || setresgid(caller, caller, caller) < 0 || setresuid(caller, caller, caller) < 0)
            die("drop");
    }
#endif
    int fd = open("f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("f");
    close(fd);
    fd = open("ro", O_WRONLY | O_CREAT | O_EXCL, 0444);
    if (fd < 0) die("ro");
    close(fd);
    if (mkdir("d", 0755) < 0) die("d");
    fd = open("d/x", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("d/x");
    close(fd);
    if (symlink("f", "lf") < 0 || symlink("d", "ld") < 0 || symlink("nx", "dang") < 0) die("symlink");
    set_initial("f");
    set_initial("ro");
    set_initial("d/x");
    set_initial("d");
    set_initial("lf");
    set_initial("ld");
    set_initial("dang");
}

// Let the clock move on, so that a ctime the call sets is not the one the
// fixture left: Linux stamps from a clock that ticks every few milliseconds.
static void settle(void) { usleep(25000); }

static void in_child(void (*body)(long, long), long a, long b) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(10);
        body(a, b);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    cellno++;
    if (WIFSIGNALED(status)) printf("SIG%d", WTERMSIG(status));
    else if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

// ------------------------------------------------------------- what moved
struct snap {
    int ok;
    struct timespec a, m, c, b;
};

static struct snap snap_of(const struct stat *st) {
    struct snap s = {1, st->ATIM, st->MTIM, st->CTIM, {0, 0}};
#ifdef __APPLE__
    s.b = st->BTIM;
#endif
    return s;
}

static struct snap lsnap(const char *p) {
    struct stat st;
    struct snap none = {0};
    if (lstat(p, &st) < 0) return none;
    return snap_of(&st);
}

static struct timespec t_before, t_after;

static void now(struct timespec *t) {
    if (clock_gettime(CLOCK_REALTIME, t) < 0) die("clock_gettime");
}

static int64_t ns(struct timespec t) { return (int64_t)t.tv_sec * 1000000000 + t.tv_nsec; }

static void pts(struct timespec t) {
    int64_t v = ns(t);
    int near = t.tv_sec > 1700000000 && t.tv_sec < 4000000000LL;
    if (near && v >= ns(t_before) - 50000000 && v <= ns(t_after)) {
#ifdef __APPLE__
        printf(t.tv_nsec % 1000 == 0 ? "now/us" : "now");
#else
        printf("now");
#endif
        return;
    }
    // A timespec's nanoseconds are never negative, so print it as the instant
    // seconds + nanoseconds/1e9 it names.
    if (t.tv_sec < 0 && t.tv_nsec != 0) {
        long long whole = (long long)t.tv_sec + 1;
        long fraction = 1000000000 - t.tv_nsec;
        if (whole == 0) printf("-0.%09ld", fraction);
        else printf("%lld.%09ld", whole, fraction);
    } else {
        printf("%lld.%09ld", (long long)t.tv_sec, (long)t.tv_nsec);
    }
}

static int same(struct timespec x, struct timespec y) { return x.tv_sec == y.tv_sec && x.tv_nsec == y.tv_nsec; }

// Whether anything has been printed for the current row.
static int any_moved;

static void pfield(const char *name, int *first, const char *label, struct timespec b, struct timespec a) {
    if (same(b, a)) return;
    if (*first) printf("%s%s:", any_moved ? " " : "", name);
    else printf(",");
    printf("%s=", label);
    pts(a);
    *first = 0;
    any_moved = 1;
}

static void pdiff(const char *name, struct snap b, struct snap a) {
    if (!b.ok || !a.ok) {
        if (b.ok != a.ok) {
            printf("%s%s:%s", any_moved ? " " : "", name, a.ok ? "appeared" : "vanished");
            any_moved = 1;
        }
        return;
    }
    int first = 1;
    pfield(name, &first, "a", b.a, a.a);
    pfield(name, &first, "m", b.m, a.m);
    pfield(name, &first, "c", b.c, a.c);
#ifdef __APPLE__
    pfield(name, &first, "b", b.b, a.b);
#endif
}

static void pdone(void) {
    if (!any_moved) printf("-");
    any_moved = 0;
}

static struct snap before[NOBJECTS];

static void snap_all(void) {
    for (int i = 0; i < NOBJECTS; i++) before[i] = lsnap(objects[i]);
}

static void diff_all(void) {
    for (int i = 0; i < NOBJECTS; i++) pdiff(objects[i], before[i], lsnap(objects[i]));
    pdone();
}

// --------------------------------------------------------------- times args
static const char *times_labels[] = {"NULL", "NOW/NOW", "NOW/OMIT", "OMIT/NOW", "X/X", "X/OMIT", "OMIT/X", "OMIT/OMIT", "NOW/X", "X/NOW", "OMIT/E", "E/OMIT", "E/E"};
#define NTIMES (int)(sizeof times_labels / sizeof times_labels[0])

static struct timespec one(const char *label, int is_access) {
    struct timespec t = {0, 0};
    if (strncmp(label, "NOW", 3) == 0) t.tv_nsec = UTIME_NOW;
    else if (strncmp(label, "OMIT", 4) == 0) t.tv_nsec = UTIME_OMIT;
    else if (label[0] == 'X') {
        t.tv_sec = is_access ? 1500000000 : 1600000000;
        t.tv_nsec = is_access ? 333333333 : 444444444;
    } else if (label[0] == 'E') {
        t.tv_sec = 1100000000;
        t.tv_nsec = 555555555;
    } else if (strncmp(label, "BAD", 3) == 0) {
        t.tv_sec = 1500000000;
        t.tv_nsec = 1000000000;
    } else if (strncmp(label, "NEG", 3) == 0) {
        t.tv_sec = 1500000000;
        t.tv_nsec = -1;
    } else die("times label");
    return t;
}

static struct timespec times_buf[2];

static const struct timespec *times_of(const char *label) {
    if (strcmp(label, "NULL") == 0) return NULL;
    if (strcmp(label, "FAULT") == 0) return (const struct timespec *)protnone;
    const char *slash = strchr(label, '/');
    times_buf[0] = one(label, 1);
    times_buf[1] = one(slash + 1, 0);
    return times_buf;
}

// -------------------------------------------------------------------- EFFECT
#ifdef __linux__
static const char *effect_paths[] = {"f", "ro", "d", "lf", "ld", "dang", "nx", "tw", "tr", "tg", "tl"};
#else
static const char *effect_paths[] = {"f", "ro", "d", "lf", "ld", "dang", "nx", "/Users/Shared", "/private/etc/hosts", "/tmp"};
#endif
#define NEFFECT (int)(sizeof effect_paths / sizeof effect_paths[0])

static void effect_body(long pt, long flagsix) {
    cell();
    const char *p = effect_paths[pt / NTIMES];
    const char *tl = times_labels[pt % NTIMES];
    settle();
    snap_all();
    now(&t_before);
    int r = call(AT_FDCWD, p, times_of(tl), flagsix ? nofollow : 0);
    now(&t_after);
    printf("%s\t", en(r));
    diff_all();
}

static void section_effect(void) {
    for (int pi = 0; pi < NEFFECT; pi++)
        for (int ti = 0; ti < NTIMES; ti++)
            for (int fi = 0; fi < 2; fi++) {
                printf("EFFECT\t%d\t%s\t%s\t%s\t", (int)caller, effect_paths[pi], times_labels[ti], fi ? "NOFOLLOW" : "0");
                in_child(effect_body, pi * NTIMES + ti, fi);
                printf("\n");
            }
}

// -------------------------------------------------------------------- NULLFD
#ifdef __linux__
static const char *kinds[] = {"cwd", "minus1", "closed", "dir", "file", "file-wronly", "ro", "tw", "tr", "tg", "pipe-read", "pipe-write", "pipe-theirs", "socket", "eventq", "devnull", "unlinked", "orphan"};
#else
static const char *kinds[] = {"cwd", "minus1", "closed", "dir", "file", "pipe-read", "socket", "eventq"};
#endif
#define NKINDS (int)(sizeof kinds / sizeof kinds[0])

static int pipe_other;

static int dirfd_of(const char *k) {
    int fd, p[2];
    if (strcmp(k, "cwd") == 0) return AT_FDCWD;
    if (strcmp(k, "minus1") == 0) return -1;
    if (strcmp(k, "closed") == 0) return 999;
    if (strcmp(k, "dir") == 0) return open("d", O_RDONLY | O_DIRECTORY);
    if (strcmp(k, "file") == 0) return open("f", O_RDONLY);
    if (strcmp(k, "file-wronly") == 0) return open("f", O_WRONLY);
    if (strcmp(k, "ro") == 0) return open("ro", O_RDONLY);
    if (strcmp(k, "tw") == 0) return open("tw", O_RDONLY);
    if (strcmp(k, "tr") == 0) return open("tr", O_RDONLY);
    if (strcmp(k, "tg") == 0) return open("tg", O_RDONLY);
    if (strcmp(k, "pipe-read") == 0) {
        if (pipe(p) < 0) die("pipe");
        pipe_other = p[1];
        return p[0];
    }
    if (strcmp(k, "pipe-write") == 0) {
        if (pipe(p) < 0) die("pipe");
        pipe_other = p[0];
        return p[1];
    }
    if (strcmp(k, "pipe-theirs") == 0) {
        pipe_other = root_pipe[1];
        return root_pipe[0];
    }
    if (strcmp(k, "socket") == 0) return socket(AF_UNIX, SOCK_STREAM, 0);
#ifdef __linux__
    if (strcmp(k, "eventq") == 0) return epoll_create1(0);
    if (strcmp(k, "devnull") == 0) return open("/dev/null", O_RDONLY);
    if (strcmp(k, "unlinked") == 0) {
        fd = open("f", O_RDONLY);
        if (unlink("f") < 0) die("unlink");
        return fd;
    }
    if (strcmp(k, "orphan") == 0) {
        if (mkdir("gone", 0755) < 0) die("gone");
        fd = open("gone", O_RDONLY | O_DIRECTORY);
        if (rmdir("gone") < 0) die("rmdir");
        return fd;
    }
#else
    if (strcmp(k, "eventq") == 0) return kqueue();
#endif
    (void)fd;
    die(k);
    return -1;
}

static const char *nullfd_times[] = {"NULL", "NOW/NOW", "NOW/OMIT", "X/X", "OMIT/X", "OMIT/OMIT"};
#define NNULLTIMES (int)(sizeof nullfd_times / sizeof nullfd_times[0])
#ifdef __linux__
static const int nullfd_flags[] = {0, AT_SYMLINK_NOFOLLOW, AT_EMPTY_PATH, 0x1};
static const char *nullfd_flag_labels[] = {"0", "NOFOLLOW", "EMPTY_PATH", "0x1"};
#else
static const int nullfd_flags[] = {0, AT_SYMLINK_NOFOLLOW, 0x1};
static const char *nullfd_flag_labels[] = {"0", "NOFOLLOW", "0x1"};
#endif
#define NNULLFLAGS (int)(sizeof nullfd_flags / sizeof nullfd_flags[0])

static void pfd(const char *name, struct snap b, struct snap a) {
    if (!b.ok && !a.ok) return;
    pdiff(name, b, a);
}

static struct snap fsnap(int fd) {
    struct stat st;
    struct snap none = {0};
    if (fd < 0 || fstat(fd, &st) < 0) return none;
    return snap_of(&st);
}

static void nullfd_body(long kt, long fi) {
    cell();
    const char *k = kinds[kt / NNULLTIMES];
    const char *tl = nullfd_times[kt % NNULLTIMES];
    pipe_other = -1;
    int fd = dirfd_of(k);
    int pipe_kind = strncmp(k, "pipe", 4) == 0;
    int sock_kind = strcmp(k, "socket") == 0 || strcmp(k, "eventq") == 0 || strcmp(k, "devnull") == 0 ||
                    strcmp(k, "unlinked") == 0 || strcmp(k, "orphan") == 0;
    struct snap fb = fsnap(fd), ob = fsnap(pipe_other);
    settle();
    snap_all();
    now(&t_before);
    int r = call(fd, NULL, times_of(tl), nullfd_flags[fi]);
    now(&t_after);
    printf("%s\t", en(r));
    if (pipe_kind || sock_kind) {
        struct stat st;
        if (fstat(fd, &st) == 0) printf("[uid=%d mode=%04o] ", (int)st.st_uid, (int)(st.st_mode & 07777));
    }
    if (pipe_kind) {
        pfd("this-end", fb, fsnap(fd));
        pfd("other-end", ob, fsnap(pipe_other));
    } else if (sock_kind) {
        pfd("fd", fb, fsnap(fd));
    }
    diff_all();
}

static void section_nullfd(void) {
    for (int ki = 0; ki < NKINDS; ki++)
        for (int ti = 0; ti < NNULLTIMES; ti++)
            for (int fi = 0; fi < NNULLFLAGS; fi++) {
                printf("NULLFD\t%d\t%s\t%s\t%s\t", (int)caller, kinds[ki], nullfd_times[ti], nullfd_flag_labels[fi]);
                in_child(nullfd_body, ki * NNULLTIMES + ti, fi);
                printf("\n");
            }
}

// --------------------------------------------------------------------- EMPTY
#ifdef __linux__
static const char *empty_paths[] = {"", "NULL", "f"};
static const int empty_flags[] = {AT_EMPTY_PATH, AT_EMPTY_PATH | AT_SYMLINK_NOFOLLOW};
static const char *empty_flag_labels[] = {"EMPTY_PATH", "EMPTY_PATH|NOFOLLOW"};
static const char *empty_times[] = {"NULL", "X/X"};

static void empty_body(long kp, long ft) {
    cell();
    const char *k = kinds[kp / 3];
    const char *path = empty_paths[kp % 3];
    pipe_other = -1;
    int fd = dirfd_of(k);
    int pipe_kind = strncmp(k, "pipe", 4) == 0;
    struct snap fb = fsnap(fd), ob = fsnap(pipe_other);
    settle();
    snap_all();
    now(&t_before);
    int r = call(fd, strcmp(path, "NULL") == 0 ? NULL : path, times_of(empty_times[ft % 2]), empty_flags[ft / 2]);
    now(&t_after);
    printf("%s\t", en(r));
    if (pipe_kind) {
        pfd("this-end", fb, fsnap(fd));
        pfd("other-end", ob, fsnap(pipe_other));
    } else {
        pfd("fd", fb, fsnap(fd));
    }
    diff_all();
}

static void section_empty(void) {
    for (int ki = 0; ki < NKINDS; ki++)
        for (int pi = 0; pi < 3; pi++)
            for (int fi = 0; fi < 2; fi++)
                for (int ti = 0; ti < 2; ti++) {
                    printf("EMPTY\t%d\t%s\t%s\t%s\t%s\t", (int)caller, kinds[ki], empty_paths[pi][0] ? empty_paths[pi] : "empty",
                           empty_times[ti], empty_flag_labels[fi]);
                    in_child(empty_body, ki * 3 + pi, fi * 2 + ti);
                    printf("\n");
                }
}
#endif

// --------------------------------------------------------------------- ORDER
// Each row: a dirfd, a pathname, a times argument and a flag word, every one
// of which may be bad.
struct order_row {
    const char *label;
    const char *dirfd; // "cwd", "minus1", "file", "dir", "locked"
    const char *path;  // a name, "NULL", "PROT_NONE", "overlong"
    const char *times; // a label above, or "BAD" (atime nanoseconds 1e9, mtime X), "BADNEG" (-3)
    int flags;         // raw
};

#ifdef __linux__
#define BADFLAG 0x1
#else
#define BADFLAG 0x40000000
#endif

static const struct order_row order_rows[] = {
    {"badflags+NULLpath", "cwd", "NULL", "NULL", BADFLAG},
    {"badflags+badfd", "minus1", "f", "NULL", BADFLAG},
    {"badflags+nx", "cwd", "nx", "NULL", BADFLAG},
    {"badflags+PROT_NONE", "cwd", "PROT_NONE", "NULL", BADFLAG},
    {"badflags+FAULT", "cwd", "f", "FAULT", BADFLAG},
    {"badflags+OMIT", "cwd", "f", "OMIT/OMIT", BADFLAG},
    {"badflags+BAD", "cwd", "f", "BAD", BADFLAG},
    {"FAULT+NULLpath", "cwd", "NULL", "FAULT", 0},
    {"FAULT+badfd", "minus1", "f", "FAULT", 0},
    {"FAULT+nx", "cwd", "nx", "FAULT", 0},
    {"FAULT+overlong", "cwd", "overlong", "FAULT", 0},
    {"FAULT+f", "cwd", "f", "FAULT", 0},
    {"FAULT+NULLpath+fd", "file", "NULL", "FAULT", 0},
    {"OMIT+NULLpath", "cwd", "NULL", "OMIT/OMIT", 0},
    {"OMIT+NULLpath+badfd", "minus1", "NULL", "OMIT/OMIT", 0},
    {"OMIT+NULLpath+fd+badflags", "file", "NULL", "OMIT/OMIT", BADFLAG},
    {"OMIT+badfd", "minus1", "f", "OMIT/OMIT", 0},
    {"OMIT+nx", "cwd", "nx", "OMIT/OMIT", 0},
    {"OMIT+PROT_NONE", "cwd", "PROT_NONE", "OMIT/OMIT", 0},
    {"OMIT+overlong", "cwd", "overlong", "OMIT/OMIT", 0},
    {"OMIT+notdir", "file", "f", "OMIT/OMIT", 0},
    {"OMIT+empty", "cwd", "", "OMIT/OMIT", 0},
    {"OMIT+theirs", "cwd", "THEIRS-RO", "OMIT/OMIT", 0},
    {"OMIT+locked", "locked", "x", "OMIT/OMIT", 0},
    {"BAD+f", "cwd", "f", "BAD", 0},
    {"BADNEG+f", "cwd", "f", "BADNEG", 0},
    {"BAD+nx", "cwd", "nx", "BAD", 0},
    {"BAD+badfd", "minus1", "f", "BAD", 0},
    {"BAD+NULLpath", "cwd", "NULL", "BAD", 0},
    {"BAD+NULLpath+badfd", "minus1", "NULL", "BAD", 0},
    {"BAD+NULLpath+fd", "file", "NULL", "BAD", 0},
    {"BAD+PROT_NONE", "cwd", "PROT_NONE", "BAD", 0},
    {"BAD+empty", "cwd", "", "BAD", 0},
    {"BAD+notdir", "file", "f", "BAD", 0},
    {"BAD+trailing", "cwd", "f/", "BAD", 0},
    {"BAD+locked", "locked", "x", "BAD", 0},
    {"BAD+theirs", "cwd", "THEIRS-RO", "BAD", 0},
    {"BAD+theirs-writable", "cwd", "THEIRS-RW", "BAD", 0},
    {"BAD+NOW", "cwd", "f", "BADNOW", 0},
    {"NULLpath+fd+NOFOLLOW", "file", "NULL", "NULL", nofollow},
    {"NULLpath+badfd+NOFOLLOW", "minus1", "NULL", "NULL", nofollow},
#ifdef __linux__
    {"NULLpath+fd+EMPTY_PATH", "file", "NULL", "NULL", AT_EMPTY_PATH},
    {"NULLpath+badfd+EMPTY_PATH", "minus1", "NULL", "NULL", AT_EMPTY_PATH},
    {"NULLpath+badfd+badflags", "minus1", "NULL", "NULL", BADFLAG},
    {"NULLpath+cwd+EMPTY_PATH", "cwd", "NULL", "NULL", AT_EMPTY_PATH},
    {"NULLpath+fd+badflags+BAD", "file", "NULL", "BAD", BADFLAG},
    {"NULLpath+badfd+BAD+badflags", "minus1", "NULL", "BAD", BADFLAG},
#endif
    {"theirs-ro+NOW", "cwd", "THEIRS-RO", "NOW/NOW", 0},
    {"theirs-ro+X", "cwd", "THEIRS-RO", "X/X", 0},
    {"theirs-rw+X", "cwd", "THEIRS-RW", "X/X", 0},
    {"locked+X", "locked", "x", "X/X", 0},
};
#define NORDER (int)(sizeof order_rows / sizeof order_rows[0])

static const char *path_of(const char *label) {
    if (strcmp(label, "NULL") == 0) return NULL;
    if (strcmp(label, "PROT_NONE") == 0) return protnone;
    if (strcmp(label, "overlong") == 0) return overlong;
#ifdef __linux__
    if (strcmp(label, "THEIRS-RO") == 0) return "tr";
    if (strcmp(label, "THEIRS-RW") == 0) return "tw";
#else
    if (strcmp(label, "THEIRS-RO") == 0) return "/private/etc/hosts";
    if (strcmp(label, "THEIRS-RW") == 0) return "/Users/Shared";
#endif
    return label;
}

static void order_body(long row, long unused) {
    (void)unused;
    cell();
    const struct order_row *o = &order_rows[row];
    int fd;
    if (strcmp(o->dirfd, "locked") == 0) {
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (chmod("d", 0) < 0) die("chmod d");
    } else {
        fd = dirfd_of(o->dirfd);
    }
    const struct timespec *t;
    struct timespec bad[2] = {{1500000000, 1000000000}, {1600000000, 444444444}};
    if (strcmp(o->times, "BAD") == 0) t = bad;
    else if (strcmp(o->times, "BADNEG") == 0) {
        bad[0].tv_nsec = -3;
        t = bad;
    } else if (strcmp(o->times, "BADNOW") == 0) {
        bad[1].tv_nsec = UTIME_NOW;
        t = bad;
    } else t = times_of(o->times);
    settle();
    snap_all();
    now(&t_before);
    int r = call(fd, path_of(o->path), t, o->flags);
    now(&t_after);
    printf("%s\t", en(r));
    diff_all();
    // Give the directory back, so the cell can be removed.
    if (strcmp(o->dirfd, "locked") == 0) fchmod(fd, 0755);
}

static void section_order(void) {
    for (int i = 0; i < NORDER; i++) {
        printf("ORDER\t%d\t%s\t", (int)caller, order_rows[i].label);
        in_child(order_body, i, 0);
        printf("\n");
    }
}

// ---------------------------------------------------------------------- NSEC
static const long nsec_values[] = {
    0, 1, 999999999, 1000000000, 1000000001, 1073741821, 1073741822, 1073741823, 1073741824,
    1999999999, 2000000000, 5000000000L, -1, -2, -3, -999999999, -1000000000, -1000000001, -5000000000L,
    LONG_MAX, LONG_MIN, INT_MAX, INT_MIN,
};
#define NNSEC (int)(sizeof nsec_values / sizeof nsec_values[0])

static const long long sec_values[] = {
    0, -1, 1, 2147483647LL, 2147483648LL, 4294967296LL, 253402300799LL, 9223372035LL, 9223372036LL, 9223372037LL,
    -1000000000LL, -2147483648LL, -9223372036LL, -9223372037LL, -9223372038LL, LLONG_MAX, LLONG_MIN,
};
#define NSEC_S (int)(sizeof sec_values / sizeof sec_values[0])

// which: 0 atime, 1 mtime. s, n: the field.
static void nsec_one(int which, long long s, long n) {
    cell();
    struct timespec t[2] = {{0, UTIME_OMIT}, {0, UTIME_OMIT}};
    t[which].tv_sec = (time_t)s;
    t[which].tv_nsec = n;
    settle();
    snap_all();
    now(&t_before);
    int r = call(AT_FDCWD, "f", t, 0);
    now(&t_after);
    printf("%s\t", en(r));
    diff_all();
}

static long long nsec_s;
static long nsec_n;
static void nsec_body(long which, long unused) {
    (void)unused;
    nsec_one((int)which, nsec_s, nsec_n);
}

static void section_nsec(void) {
    for (int which = 0; which < 2; which++) {
        for (int i = 0; i < NNSEC; i++) {
            nsec_s = 1500000000;
            nsec_n = nsec_values[i];
            printf("NSEC\t%d\t%s\t%lld\t%ld\t", (int)caller, which ? "m" : "a", nsec_s, nsec_n);
            in_child(nsec_body, which, 0);
            printf("\n");
        }
        for (int i = 0; i < NSEC_S; i++) {
            for (int j = 0; j < 3; j++) {
                static const long ns_for_seconds[] = {0, 145224192, 854775807};
                nsec_s = sec_values[i];
                nsec_n = ns_for_seconds[j];
                printf("NSEC\t%d\t%s\t%lld\t%ld\t", (int)caller, which ? "m" : "a", nsec_s, nsec_n);
                in_child(nsec_body, which, 0);
                printf("\n");
            }
        }
    }
}

// --------------------------------------------------------------------- NSECX
#ifdef __linux__
// The extreme seconds again, through a NULL pathname on a pipe and on
// /dev/null (devtmpfs), as root: what those filesystems store.
static const char *nsecx_kinds[] = {"pipe-read", "devnull"};
static const long long nsecx_s[] = {LLONG_MAX, LLONG_MIN, 9223372036LL, -9223372037LL, 253402300799LL, -2147483649LL};
#define NNSECX_S (int)(sizeof nsecx_s / sizeof nsecx_s[0])

static void nsecx_body(long ki, long si) {
    cell();
    pipe_other = -1;
    int fd = dirfd_of(nsecx_kinds[ki]);
    struct timespec t[2] = {{(time_t)nsecx_s[si], 854775807}, {0, UTIME_OMIT}};
    settle();
    struct snap b = fsnap(fd);
    now(&t_before);
    int r = call(fd, NULL, t, 0);
    now(&t_after);
    printf("%s\t", en(r));
    pfd("fd", b, fsnap(fd));
    pdone();
}

static void section_nsecx(void) {
    for (int ki = 0; ki < 2; ki++)
        for (int si = 0; si < NNSECX_S; si++) {
            printf("NSECX\t%d\t%s\t%lld\t854775807\t", (int)caller, nsecx_kinds[ki], nsecx_s[si]);
            in_child(nsecx_body, ki, si);
            printf("\n");
        }
}
#endif

// -------------------------------------------------------------------- NSECOBJ
#ifdef __linux__
// A null pathname on each kind of descriptor whose object answers something
// of its own, with an out-of-range nanosecond field beside each kind of
// other time, and the in-range answers for reference: whether EINVAL comes
// before or after what the object answers.
static const char *nsecobj_kinds[] = {"file", "tr", "devnull", "pipe-read", "pipe-write", "pipe-theirs", "socket", "eventq"};
#define NNSECOBJ_KINDS (int)(sizeof nsecobj_kinds / sizeof nsecobj_kinds[0])
static const char *nsecobj_times[] = {"NOW/NOW", "X/X", "NOW/OMIT", "BAD/NOW", "BAD/X", "BAD/OMIT", "NOW/BAD", "OMIT/BAD", "NEG/NOW", "X/NEG"};
#define NNSECOBJ_TIMES (int)(sizeof nsecobj_times / sizeof nsecobj_times[0])

static void nsecobj_body(long ki, long ti) {
    cell();
    pipe_other = -1;
    const char *k = nsecobj_kinds[ki];
    int fd = dirfd_of(k);
    int pipe_kind = strncmp(k, "pipe", 4) == 0;
    settle();
    struct snap fb = fsnap(fd), ob = fsnap(pipe_other);
    snap_all();
    now(&t_before);
    int r = call(fd, NULL, times_of(nsecobj_times[ti]), 0);
    now(&t_after);
    printf("%s\t", en(r));
    if (pipe_kind) {
        pfd("this-end", fb, fsnap(fd));
        pfd("other-end", ob, fsnap(pipe_other));
    } else {
        pfd("fd", fb, fsnap(fd));
    }
    diff_all();
}

static void section_nsecobj(void) {
    for (int ki = 0; ki < NNSECOBJ_KINDS; ki++)
        for (int ti = 0; ti < NNSECOBJ_TIMES; ti++) {
            printf("NSECOBJ\t%d\t%s\t%s\t", (int)caller, nsecobj_kinds[ki], nsecobj_times[ti]);
            in_child(nsecobj_body, ki, ti);
            printf("\n");
        }
}
#endif

// -------------------------------------------------------------------- DFLAGS
#ifdef __APPLE__
static const char *dflag_paths[] = {"lf", "ld/x", "d/../f", "ABS"};
#define NDFLAG (int)(sizeof dflag_paths / sizeof dflag_paths[0])

static void dflags_body(long pi, long bit) {
    cell();
    char abs[PATH_MAX];
    const char *p = dflag_paths[pi];
    if (strcmp(p, "ABS") == 0) {
        snprintf(abs, sizeof abs, "%s/f", cellpath);
        p = abs;
    }
    int flags = bit < 0 ? 0 : (int)(1u << bit);
    settle();
    snap_all();
    now(&t_before);
    int r = call(AT_FDCWD, p, NULL, flags);
    now(&t_after);
    printf("%s\t", en(r));
    diff_all();
}

static void section_dflags(void) {
    for (int pi = 0; pi < NDFLAG; pi++)
        for (long bit = -1; bit < 32; bit++) {
            printf("DFLAGS\t%d\t%s\t0x%x\t", (int)caller, dflag_paths[pi], bit < 0 ? 0 : (unsigned)(1u << bit));
            in_child(dflags_body, pi, bit);
            printf("\n");
        }
}
#endif

// --------------------------------------------------------------------- TRAIL
static const char *trail_paths[] = {"f/", "d/", "lf/", "ld/", "dang/", "nx/"};
#define NTRAIL (int)(sizeof trail_paths / sizeof trail_paths[0])

static void trail_body(long pi, long fi) {
    cell();
    settle();
    snap_all();
    now(&t_before);
    int r = call(AT_FDCWD, trail_paths[pi], NULL, fi ? nofollow : 0);
    now(&t_after);
    printf("%s\t", en(r));
    diff_all();
}

static void section_trail(void) {
    for (int pi = 0; pi < NTRAIL; pi++)
        for (int fi = 0; fi < 2; fi++) {
            printf("TRAIL\t%d\t%s\t%s\t", (int)caller, trail_paths[pi], fi ? "NOFOLLOW" : "0");
            in_child(trail_body, pi, fi);
            printf("\n");
        }
}

// ----------------------------------------------------------------------- main
static void run_all(void) {
    section_effect();
    section_nullfd();
#ifdef __linux__
    section_empty();
    section_nsecobj();
#endif
    section_order();
    section_nsec();
#ifdef __APPLE__
    section_dflags();
#endif
    section_trail();
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <empty directory> [second empty directory, another filesystem]\n", argv[0]);
        return 2;
    }
    setvbuf(stdout, NULL, _IOFBF, 1 << 16);
    umask(022);
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, euid %d\n", u.sysname, u.release, u.machine, (int)geteuid());
    printf("CONST\tUTIME_NOW=%ld\tUTIME_OMIT=%ld\tAT_SYMLINK_NOFOLLOW=0x%x", (long)UTIME_NOW, (long)UTIME_OMIT, AT_SYMLINK_NOFOLLOW);
#ifdef __linux__
    printf("\tAT_EMPTY_PATH=0x%x", AT_EMPTY_PATH);
#endif
    printf("\n");

    long page = sysconf(_SC_PAGESIZE);
    protnone = mmap(NULL, (size_t)page, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (protnone == MAP_FAILED) die("mmap");
    overlong = malloc(PATH_MAX + 16);
    for (int i = 0; i < PATH_MAX + 8; i++) overlong[i] = i % 2 == 0 ? 'a' : '/';
    overlong[PATH_MAX + 8] = 0;

    base = argv[1];
    if (chmod(base, 0777) < 0) die("chmod base");
#ifdef __linux__
    if (geteuid() != 0) {
        fprintf(stderr, "run this as root: it drops to uid 1000 itself\n");
        return 2;
    }
    caller = 0;
    printf("# filesystem: %s\n", base);
    run_all();
    section_nsecx();
    caller = 1000;
    run_all();
    if (argc > 2) {
        base = argv[2];
        if (chmod(base, 0777) < 0) die("chmod base");
        printf("# filesystem: %s\n", base);
        caller = 0;
        section_nsec();
        caller = 1000;
        section_nsec();
    }
#else
    caller = geteuid();
    run_all();
#endif
    return 0;
}
