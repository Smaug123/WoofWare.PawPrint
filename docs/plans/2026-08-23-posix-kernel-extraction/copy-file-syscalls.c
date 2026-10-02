// Measures the syscalls a Linux libSystem.Native's SystemNative_CopyFile issues
// (ioctl(FICLONE), copy_file_range(2) with NULL offsets, futimens(2)), and
// futimens(2) on Darwin too: which errno each refusal carries on every kind of
// descriptor, the order the checks are made in, what moves (offsets, sizes,
// timestamps, set-ID bits), and who may set a file's times.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/copy-file-syscalls.c && /tmp/p /dev/shm/cfs && /tmp/p /tmp/cfs'
//         As root: the non-owner rows fork a child at uid/gid 1000.
// Darwin: nix develop -c clang -Wall -o copy-file-syscalls copy-file-syscalls.c && ./copy-file-syscalls "$(mktemp -d /private/tmp/cfs.XXXXXX)"
//         As an ordinary user; the non-owner futimens row uses /etc/hosts,
//         which root owns and anyone may open for reading.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#include <sys/syscall.h>
#ifndef FICLONE
#define FICLONE _IOW(0x94, 9, int)
#endif
#else
#include <sys/event.h>
#endif

static char base[PATH_MAX];

static void die(const char *what) {
    perror(what);
    exit(2);
}

static void p(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    fflush(stdout);
}

static const char *en(int e) {
    static char buf[32];
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case EISDIR: return "EISDIR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EXDEV: return "EXDEV";
    case EOPNOTSUPP: return "EOPNOTSUPP";
#if defined(ENOTSUP) && ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
    case ENOTTY: return "ENOTTY";
    case ESPIPE: return "ESPIPE";
    case EOVERFLOW: return "EOVERFLOW";
    case EFBIG: return "EFBIG";
    case EFAULT: return "EFAULT";
    case ENOSYS: return "ENOSYS";
    case EROFS: return "EROFS";
    case ETXTBSY: return "ETXTBSY";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static void path(char *out, const char *name) {
    snprintf(out, PATH_MAX, "%s/%s", base, name);
}

static void mkfile(const char *name, const char *content, mode_t mode) {
    char pth[PATH_MAX];
    path(pth, name);
    unlink(pth);
    int fd = open(pth, O_CREAT | O_EXCL | O_WRONLY, 0600);
    if (fd < 0) die(pth);
    if (write(fd, content, strlen(content)) != (ssize_t)strlen(content)) die("write");
    if (fchmod(fd, mode) != 0) die("fchmod");
    close(fd);
}

static int openat_base(const char *name, int flags) {
    char pth[PATH_MAX];
    path(pth, name);
    int fd = open(pth, flags);
    if (fd < 0) die(pth);
    return fd;
}

static void showfd(const char *label, int fd) {
    struct stat st;
    if (fstat(fd, &st) != 0) {
        p("  %s: fstat %s\n", label, en(errno));
        return;
    }
#ifdef __APPLE__
    struct timespec a = st.st_atimespec, m = st.st_mtimespec, c = st.st_ctimespec, b = st.st_birthtimespec;
#else
    struct timespec a = st.st_atim, m = st.st_mtim, c = st.st_ctim, b = { 0, 0 };
    struct statx sx;
    if (statx(fd, "", AT_EMPTY_PATH, STATX_BTIME, &sx) == 0 && (sx.stx_mask & STATX_BTIME)) {
        b.tv_sec = sx.stx_btime.tv_sec;
        b.tv_nsec = sx.stx_btime.tv_nsec;
    }
#endif
    p("  %s: mode=%04o uid=%u size=%lld a=%ld.%09ld m=%ld.%09ld c=%ld.%09ld b=%ld.%09ld pos=%lld\n", label,
      (unsigned)(st.st_mode & 07777), st.st_uid, (long long)st.st_size, (long)a.tv_sec, a.tv_nsec, (long)m.tv_sec,
      m.tv_nsec, (long)c.tv_sec, c.tv_nsec, (long)b.tv_sec, b.tv_nsec, (long long)lseek(fd, 0, SEEK_CUR));
}

static void settimes(int fd, long atime, long mtime) {
    struct timespec ts[2] = { { atime, 111111111 }, { mtime, 222222222 } };
    if (futimens(fd, ts) != 0) die("futimens settimes");
}

// The descriptor kinds, by name, each freshly made.
enum kind { K_FILE_RO, K_FILE_WO, K_FILE_RW, K_DIR, K_PIPE_R, K_PIPE_W, K_SOCKET, K_PORT, K_CLOSED, K_HUGE, K_COUNT };
static const char *kname[K_COUNT] = { "file-ro", "file-wo", "file-rw", "dir", "pipe-r", "pipe-w", "socket", "port", "closed", "fd9999" };

static int make(enum kind k, const char *file) {
    int fds[2];
    switch (k) {
    case K_FILE_RO: return openat_base(file, O_RDONLY);
    case K_FILE_WO: return openat_base(file, O_WRONLY);
    case K_FILE_RW: return openat_base(file, O_RDWR);
    case K_DIR: return openat_base(".", O_RDONLY);
    case K_PIPE_R:
        if (pipe(fds) != 0) die("pipe");
        if (write(fds[1], "hello", 5) != 5) die("pipe write");
        return fds[0];
    case K_PIPE_W:
        if (pipe(fds) != 0) die("pipe");
        return fds[1];
    case K_SOCKET: {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        if (s < 0) die("socket");
        return s;
    }
    case K_PORT:
#ifdef __linux__
        return epoll_create1(0);
#else
        return kqueue();
#endif
    case K_CLOSED: {
        // A number nothing else is given: the next open would reuse a freshly
        // closed lowest number, and so alias the other argument.
        int fd = openat_base(file, O_RDONLY);
        if (dup2(fd, 700) != 700) die("dup2");
        close(fd);
        close(700);
        return 700;
    }
    case K_HUGE: return 9999;
    default: return -1;
    }
}

#ifdef __linux__
static long cfr(int in, int out, size_t len, unsigned flags) {
    return syscall(__NR_copy_file_range, in, NULL, out, NULL, len, flags);
}

// ---------------------------------------------------------------------------
static void copy_file_range_kinds(void) {
    p("CFR-KINDS: copy_file_range(in, NULL, out, NULL, len, 0) for every pair of kinds; len 5 / len 0 / flags 1\n");
    for (int i = 0; i < K_COUNT; i++) {
        for (int o = 0; o < K_COUNT; o++) {
            char row[256];
            int n = 0;
            n += snprintf(row + n, sizeof row - n, "  in=%-8s out=%-8s", kname[i], kname[o]);
            long variants[3][2] = { { 5, 0 }, { 0, 0 }, { 5, 1 } };
            for (int v = 0; v < 3; v++) {
                mkfile("ksrc", "hello", 0644);
                mkfile("kdst", "", 0644);
                int in = make(i, "ksrc");
                int out = make(o, "kdst");
                errno = 0;
                long r = cfr(in, out, (size_t)variants[v][0], (unsigned)variants[v][1]);
                int e = r < 0 ? errno : 0;
                if (r >= 0)
                    n += snprintf(row + n, sizeof row - n, " | %s=%ld", v == 0 ? "len5" : v == 1 ? "len0" : "flags1", r);
                else
                    n += snprintf(row + n, sizeof row - n, " | %s=%s", v == 0 ? "len5" : v == 1 ? "len0" : "flags1", en(e));
                if (i != K_CLOSED && i != K_HUGE) close(in);
                if (o != K_CLOSED && o != K_HUGE) close(out);
            }
            p("%s\n", row);
        }
    }
}

static void copy_file_range_behaviour(void) {
    p("CFR-BEHAVIOUR\n");
    // The whole of a short file, from both description's offsets.
    mkfile("src", "hello world", 0640);
    mkfile("dst", "", 0644);
    int in = openat_base("src", O_RDONLY), out = openat_base("dst", O_WRONLY);
    settimes(in, 1000000000, 900000000);
    settimes(out, 1200000000, 1300000000);
    sleep(1);
    showfd("src before", in);
    showfd("dst before", out);
    long r = cfr(in, out, 11, 0);
    p("  cfr(11) = %ld %s\n", r, r < 0 ? en(errno) : "");
    showfd("src after", in);
    showfd("dst after", out);
    r = cfr(in, out, 11, 0);
    p("  cfr at EOF (11) = %ld %s\n", r, r < 0 ? en(errno) : "");
    showfd("dst after EOF call", out);
    close(in);
    close(out);

    // A length longer than what remains; a source mid-file; a destination past
    // its end.
    mkfile("src", "0123456789", 0640);
    mkfile("dst", "abc", 0644);
    in = openat_base("src", O_RDONLY);
    out = openat_base("dst", O_WRONLY);
    lseek(in, 4, SEEK_SET);
    lseek(out, 6, SEEK_SET);
    r = cfr(in, out, 100, 0);
    p("  cfr(100) from 4 into offset 6 of a 3-byte file = %ld %s\n", r, r < 0 ? en(errno) : "");
    showfd("src", in);
    showfd("dst", out);
    {
        char buf[32] = { 0 };
        ssize_t got = pread(out, buf, sizeof buf, 0);
        (void)got;
        int rd = openat_base("dst", O_RDONLY);
        got = read(rd, buf, sizeof buf);
        p("  dst bytes (%zd):", got);
        for (ssize_t k = 0; k < got; k++) p(" %02x", (unsigned char)buf[k]);
        p("\n");
        close(rd);
    }
    close(in);
    close(out);

    // Lengths: SSIZE_MAX and SIZE_MAX, and a large file in one call.
    mkfile("src", "hello", 0640);
    size_t lens[] = { (size_t)SSIZE_MAX, SIZE_MAX, (size_t)0x7ffff000 + 1 };
    for (int k = 0; k < 3; k++) {
        mkfile("dst", "", 0644);
        in = openat_base("src", O_RDONLY);
        out = openat_base("dst", O_WRONLY);
        errno = 0;
        r = cfr(in, out, lens[k], 0);
        p("  cfr(len=%zu) = %ld %s\n", lens[k], r, r < 0 ? en(errno) : "");
        close(in);
        close(out);
    }
    {
        size_t big = 3u << 20;
        char *buf = malloc(big);
        memset(buf, 'z', big);
        char pth[PATH_MAX];
        path(pth, "big");
        unlink(pth);
        int fd = open(pth, O_CREAT | O_WRONLY, 0644);
        if (write(fd, buf, big) != (ssize_t)big) die("big write");
        close(fd);
        free(buf);
        mkfile("dst", "", 0644);
        in = openat_base("big", O_RDONLY);
        out = openat_base("dst", O_WRONLY);
        r = cfr(in, out, big, 0);
        p("  cfr of a 3 MiB file in one call = %ld %s\n", r, r < 0 ? en(errno) : "");
        close(in);
        close(out);
    }
    // A sparse source longer than MAX_RW_COUNT: how much one call moves.
    {
        char pth[PATH_MAX];
        path(pth, "sparse");
        unlink(pth);
        int fd = open(pth, O_CREAT | O_WRONLY, 0644);
        if (ftruncate(fd, (off_t)0x80000000LL + 4096) != 0) die("ftruncate sparse");
        close(fd);
        mkfile("dst", "", 0644);
        in = openat_base("sparse", O_RDONLY);
        out = openat_base("dst", O_WRONLY);
        r = cfr(in, out, (size_t)0x80000000LL + 4096, 0);
        p("  cfr of a sparse 2 GiB + 4096 file in one call = %ld %s\n", r, r < 0 ? en(errno) : "");
        close(in);
        close(out);
        path(pth, "dst");
        unlink(pth);
        path(pth, "sparse");
        unlink(pth);
    }
    // Source offset near INT64_MAX, and destination offset near it.
    mkfile("src", "hello", 0640);
    mkfile("dst", "", 0644);
    in = openat_base("src", O_RDONLY);
    out = openat_base("dst", O_WRONLY);
    lseek(out, INT64_MAX - 2, SEEK_SET);
    errno = 0;
    r = cfr(in, out, 5, 0);
    p("  cfr(5) into offset INT64_MAX-2 = %ld %s\n", r, r < 0 ? en(errno) : "");
    lseek(in, INT64_MAX - 2, SEEK_SET);
    lseek(out, 0, SEEK_SET);
    errno = 0;
    r = cfr(in, out, 5, 0);
    p("  cfr(5) from offset INT64_MAX-2 = %ld %s\n", r, r < 0 ? en(errno) : "");
    close(in);
    close(out);

    // The same file: one description, two descriptions overlapping, two not.
    mkfile("same", "0123456789", 0644);
    int a = openat_base("same", O_RDWR);
    r = cfr(a, a, 5, 0);
    p("  same description, offset 0 to 0, len 5 = %ld %s\n", r, r < 0 ? en(errno) : "");
    int b = openat_base("same", O_RDWR);
    lseek(b, 3, SEEK_SET);
    errno = 0;
    r = cfr(a, b, 5, 0);
    p("  two descriptions, 0 to 3, len 5 (overlap) = %ld %s\n", r, r < 0 ? en(errno) : "");
    lseek(a, 0, SEEK_SET);
    lseek(b, 5, SEEK_SET);
    errno = 0;
    r = cfr(a, b, 5, 0);
    p("  two descriptions, 0 to 5, len 5 (adjacent) = %ld %s\n", r, r < 0 ? en(errno) : "");
    lseek(a, 0, SEEK_SET);
    lseek(b, 5, SEEK_SET);
    errno = 0;
    r = cfr(a, b, 6, 0);
    p("  two descriptions, 0 to 5, len 6 (overlap by one) = %ld %s\n", r, r < 0 ? en(errno) : "");
    lseek(a, 8, SEEK_SET);
    lseek(b, 0, SEEK_SET);
    errno = 0;
    r = cfr(a, b, 5, 0);
    p("  two descriptions, 8 to 0, len 5 (2 remain, no overlap) = %ld %s\n", r, r < 0 ? en(errno) : "");
    lseek(a, 2, SEEK_SET);
    lseek(b, 0, SEEK_SET);
    errno = 0;
    r = cfr(a, b, 5, 0);
    p("  two descriptions, 2 to 0, len 5 (overlap) = %ld %s\n", r, r < 0 ? en(errno) : "");
    // Whether the overlap is judged on the count asked for or on the count
    // shortened to the source's end: 0 to 7 of a 10-byte file, asking 20, is
    // [0,10) against [7,17) shortened (overlap) and [0,20) against [7,27)
    // asked (overlap too); 0 to 12, asking 20, is [0,10) against [12,22)
    // shortened (apart) and [0,20) against [12,32) asked (overlap).
    lseek(a, 0, SEEK_SET);
    lseek(b, 12, SEEK_SET);
    errno = 0;
    r = cfr(a, b, 20, 0);
    p("  two descriptions, 0 to 12, len 20 of a 10-byte file = %ld %s\n", r, r < 0 ? en(errno) : "");
    close(a);
    close(b);
    // A hard link of the same inode.
    {
        char p1[PATH_MAX], p2[PATH_MAX];
        path(p1, "same");
        path(p2, "samelink");
        unlink(p2);
        link(p1, p2);
        a = openat_base("same", O_RDONLY);
        b = openat_base("samelink", O_WRONLY);
        errno = 0;
        r = cfr(a, b, 5, 0);
        p("  hard link, 0 to 0, len 5 = %ld %s\n", r, r < 0 ? en(errno) : "");
        close(a);
        close(b);
    }
    // Every flag bit, on good descriptors.
    p("  flags:");
    for (int bit = 0; bit < 32; bit++) {
        mkfile("dst", "", 0644);
        in = openat_base("src", O_RDONLY);
        out = openat_base("dst", O_WRONLY);
        errno = 0;
        r = cfr(in, out, 5, 1u << bit);
        p(" %d=%s", bit, r < 0 ? en(errno) : "ok");
        close(in);
        close(out);
    }
    p("\n");
    // Order: bad flags with a closed in; len 0 with a directory; len 0 same file.
    in = make(K_CLOSED, "src");
    out = openat_base("dst", O_WRONLY);
    errno = 0;
    r = cfr(in, out, 0, 1);
    p("  closed in, len 0, flags 1 = %ld %s\n", r, r < 0 ? en(errno) : "");
    close(out);
    a = openat_base("same", O_RDWR);
    errno = 0;
    r = cfr(a, a, 0, 0);
    p("  same description, len 0 = %ld %s\n", r, r < 0 ? en(errno) : "");
    close(a);
}

static void copy_file_range_setid(void) {
    p("CFR-SETID: a non-root copy into a set-ID destination (each mode, then root)\n");
    int modes[] = { 06755, 06745, 04644, 02755, 02644, 01644 };
    for (int pass = 0; pass < 2; pass++) {
        for (size_t k = 0; k < sizeof modes / sizeof modes[0]; k++) {
            mkfile("src", "hello", 0644);
            mkfile("dst", "", 0600);
            char pth[PATH_MAX];
            path(pth, "dst");
            if (pass == 0 && chown(pth, 1000, 1000) != 0) die("chown dst");
            if (chmod(pth, modes[k]) != 0) die("chmod dst");
            path(pth, "src");
            if (chmod(pth, 0644) != 0) die("chmod src");
            pid_t pid = fork();
            if (pid == 0) {
                if (pass == 0) {
                    if (setgroups(0, NULL) != 0 || setresgid(1000, 1000, 1000) != 0 || setresuid(1000, 1000, 1000) != 0) die("drop");
                }
                int in = openat_base("src", O_RDONLY), out = openat_base("dst", O_WRONLY);
                long r = cfr(in, out, 5, 0);
                struct stat st;
                fstat(out, &st);
                p("  %s: dst %04o -> cfr=%ld -> %04o\n", pass == 0 ? "uid1000" : "root", modes[k], r, (unsigned)(st.st_mode & 07777));
                _exit(0);
            }
            int status;
            waitpid(pid, &status, 0);
        }
    }
}

static void ficlone(void) {
    p("FICLONE: ioctl(out, FICLONE, in) for every pair of kinds\n");
    for (int i = 0; i < K_COUNT; i++) {
        char row[512];
        int n = snprintf(row, sizeof row, "  in=%-8s:", kname[i]);
        for (int o = 0; o < K_COUNT; o++) {
            mkfile("ksrc", "hello", 0644);
            mkfile("kdst", "", 0644);
            int in = make(i, "ksrc");
            int out = make(o, "kdst");
            errno = 0;
            int r = ioctl(out, FICLONE, in);
            n += snprintf(row + n, sizeof row - n, " %s=%s", kname[o], r < 0 ? en(errno) : "ok");
            if (i != K_CLOSED && i != K_HUGE) close(in);
            if (o != K_CLOSED && o != K_HUGE) close(out);
        }
        p("%s\n", row);
    }
    mkfile("ksrc", "", 0644);
    mkfile("kdst", "", 0644);
    int in = openat_base("ksrc", O_RDONLY), out = openat_base("kdst", O_WRONLY);
    errno = 0;
    int r = ioctl(out, FICLONE, in);
    p("  empty file-ro into file-wo: %s\n", r < 0 ? en(errno) : "ok");
    close(in);
    close(out);
    int a = openat_base("ksrc", O_RDWR);
    errno = 0;
    r = ioctl(a, FICLONE, a);
    p("  same description: %s\n", r < 0 ? en(errno) : "ok");
    close(a);
}
#endif

// ---------------------------------------------------------------------------
static void futimens_rows(void) {
    p("FUTIMENS-KINDS: futimens(fd, explicit times) on each kind, as the owner where there is one\n");
    for (int k = 0; k < K_COUNT; k++) {
        mkfile("t", "hello", 0644);
        int fd = make(k, "t");
        struct timespec ts[2] = { { 1000000000, 111111111 }, { 900000000, 222222222 } };
        errno = 0;
        int r = futimens(fd, ts);
        p("  %-8s: %s\n", kname[k], r < 0 ? en(errno) : "ok");
        if (r == 0) showfd("after", fd);
        if (k != K_CLOSED && k != K_HUGE) close(fd);
    }
    p("FUTIMENS-VALUES\n");
    long nsecs[] = { 0, 999999999, 1000000000, -1, (1l << 30) - 1, (1l << 30) - 2, -2 };
    for (size_t i = 0; i < sizeof nsecs / sizeof nsecs[0]; i++) {
        mkfile("t", "hello", 0644);
        int fd = openat_base("t", O_RDONLY);
        struct timespec ts[2] = { { 1000000000, nsecs[i] }, { 900000000, 5 } };
        errno = 0;
        int r = futimens(fd, ts);
        p("  atime nsec %ld: %s\n", nsecs[i], r < 0 ? en(errno) : "ok");
        if (r == 0) showfd("after", fd);
        close(fd);
    }
    // Bad nsec on a closed descriptor: which first?
    {
        int fd = make(K_CLOSED, "t");
        struct timespec ts[2] = { { 1, 1000000000 }, { 1, 0 } };
        errno = 0;
        int r = futimens(fd, ts);
        p("  closed fd with nsec 1e9: %s\n", r < 0 ? en(errno) : "ok");
    }
    // Negative seconds, and the extremes.
    {
        long secs[] = { -1, -1000000000L, 0, INT32_MAX, (long)INT32_MAX + 1, 253402300799L };
        for (size_t i = 0; i < sizeof secs / sizeof secs[0]; i++) {
            mkfile("t", "hello", 0644);
            int fd = openat_base("t", O_RDONLY);
            struct timespec ts[2] = { { secs[i], 7 }, { secs[i], 8 } };
            errno = 0;
            int r = futimens(fd, ts);
            p("  seconds %ld: %s\n", secs[i], r < 0 ? en(errno) : "ok");
            if (r == 0) showfd("after", fd);
            close(fd);
        }
    }
    p("FUTIMENS-BIRTH: mtime set before and after the birth time\n");
    {
        mkfile("t", "hello", 0644);
        int fd = openat_base("t", O_RDONLY);
        showfd("fresh", fd);
        struct timespec later[2] = { { 4000000000L, 1 }, { 4000000000L, 2 } };
        futimens(fd, later);
        showfd("after setting both later than birth", fd);
        struct timespec atimeEarly[2] = { { 900000000, 3 }, { 4000000000L, 4 } };
        futimens(fd, atimeEarly);
        showfd("after setting atime earlier than birth", fd);
        struct timespec mtimeEarly[2] = { { 4000000000L, 5 }, { 900000000, 6 } };
        futimens(fd, mtimeEarly);
        showfd("after setting mtime earlier than birth", fd);
        struct timespec back[2] = { { 4000000000L, 7 }, { 4100000000L, 8 } };
        futimens(fd, back);
        showfd("after setting mtime later again", fd);
        close(fd);
    }
    p("FUTIMENS-STANDING\n");
#ifdef __linux__
    for (int pass = 0; pass < 3; pass++) {
        // pass 0: uid 1000 on root's 0666 file; 1: uid 1000 on its own 0444 file
        // opened read-only; 2: root on uid 1000's 0600 file.
        mkfile("t", "hello", pass == 1 ? 0444 : pass == 0 ? 0666 : 0600);
        char pth[PATH_MAX];
        path(pth, "t");
        if (pass >= 1 && chown(pth, 1000, 1000) != 0) die("chown t");
        pid_t pid = fork();
        if (pid == 0) {
            if (pass < 2 && (setgroups(0, NULL) != 0 || setresgid(1000, 1000, 1000) != 0 || setresuid(1000, 1000, 1000) != 0)) die("drop");
            int fd = open(pth, pass == 0 ? O_RDWR : O_RDONLY);
            if (fd < 0) die("open t");
            struct timespec ts[2] = { { 1000000000, 1 }, { 900000000, 2 } };
            errno = 0;
            int r = futimens(fd, ts);
            p("  %s: %s\n", pass == 0 ? "uid1000, root's 0666 file opened O_RDWR" : pass == 1 ? "uid1000, own 0444 file opened O_RDONLY" : "root, uid1000's 0600 file", r < 0 ? en(errno) : "ok");
            showfd("after", fd);
            _exit(0);
        }
        int status;
        waitpid(pid, &status, 0);
    }
#else
    {
        int fd = open("/etc/hosts", O_RDONLY);
        if (fd < 0) die("/etc/hosts");
        struct timespec ts[2] = { { 1000000000, 1 }, { 900000000, 2 } };
        errno = 0;
        int r = futimens(fd, ts);
        p("  uid %u, root's /etc/hosts opened O_RDONLY: %s\n", geteuid(), r < 0 ? en(errno) : "ok");
        close(fd);
        mkfile("t", "hello", 0444);
        fd = openat_base("t", O_RDONLY);
        r = futimens(fd, ts);
        p("  own 0444 file opened O_RDONLY: %s\n", r < 0 ? en(errno) : "ok");
        close(fd);
    }
#endif
}

int main(int argc, char **argv) {
    alarm(600);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <directory to create>\n", argv[0]);
        return 2;
    }
    snprintf(base, sizeof base, "%s", argv[1]);
    mkdir(base, 0777);
    chmod(base, 0777);
    if (chdir(base) != 0) die("chdir");
    struct stat st;
    stat(base, &st);
    p("base %s, euid %u\n", base, geteuid());
#ifdef __linux__
    copy_file_range_kinds();
    copy_file_range_behaviour();
    if (geteuid() == 0) copy_file_range_setid();
    ficlone();
#endif
    futimens_rows();
    return 0;
}
