// What fcntl(2)'s F_GETFL, F_SETFL, F_GETFD, F_SETFD, F_DUPFD and
// F_DUPFD_CLOEXEC, and dup2(2) and dup3(2), answer on each flavour, for every
// descriptor kind the library models.
//
// Every row is one line, tab-separated, its first field naming the section.
// A call's answer is "ok <value>" (hex for a flag word) or an errno name.
//
// Sections:
//   KIND   a fresh descriptor of each kind: its F_GETFL and F_GETFD words.
//          The kinds are a regular file opened 0, 1 and 2, each with and
//          without O_CLOEXEC; one opened O_RDWR|O_SYNC; a directory opened
//          O_RDONLY and O_RDONLY|O_DIRECTORY; a file outside the caller's
//          ownership opened O_RDONLY ("foreign"); each end of pipe() and of
//          pipe2() with O_NONBLOCK and with O_CLOEXEC; sockets (IPv4, IPv6
//          and Unix-domain stream and datagram, with SOCK_NONBLOCK and
//          SOCK_CLOEXEC on Linux); an accepted socket, from a blocking and a
//          non-blocking listener (and accept4 with each flag on Linux); an
//          epoll instance with flags 0 and EPOLL_CLOEXEC, or a kqueue; and
//          /dev/null and /dev/urandom opened 0, 1 and 2.
//   SETFL  for a fresh descriptor of each of a subset of those kinds, and each
//          bit 0..31: F_GETFL ("before"); F_SETFL(before | bit), its answer and
//          F_GETFL after; F_SETFL(before), its answer and F_GETFL after. Then
//          the words 0, -1 and 3, each against a fresh descriptor.
//   WRITTEN  which calls move a status word F_SETFL cannot: a write, a pwrite,
//          an ftruncate, a zero-length write, a refused write, and an flock
//          taken, converted, released and refused, each against a fresh
//          regular file, with F_GETFL before and after.
//   SETFD  for each bit 0..31 and the words -1 and 0: F_SETFD's answer and
//          F_GETFD after, on a fresh regular file descriptor.
//   CLOEXEC  where the descriptor flags go: a dup, F_DUPFD, F_DUPFD_CLOEXEC,
//          F_DUPFD_CLOFORK (Darwin), dup2 onto a free and onto an open
//          descriptor, dup2 of a descriptor onto itself, dup3 with and without
//          O_CLOEXEC (Linux), and F_SETFD on one of a dup pair; each row shows
//          F_GETFD of the old and the new descriptor.
//   LIMIT  RLIMIT_NOFILE as found, then lowered to 64: F_DUPFD and
//          F_DUPFD_CLOEXEC from an open and a closed descriptor at each of
//          INT_MIN, -1, 0, a gap, 62, 63, 64, 65, 1<<20 and INT_MAX; the same
//          with the table full; and F_GETFL, F_SETFL, F_GETFD and F_SETFD on
//          descriptors -1, 63, 64 and INT_MAX, none open.
//   DUP2   with the soft limit at 64: dup2 of open, closed and negative
//          descriptors onto themselves, onto free, open, negative and
//          out-of-range targets, and whether a closed or open target was
//          changed; dup3 (Linux) with each flag bit, the same descriptor on
//          both sides, and the screens in each order.
//   ONTO   dup2 onto an open descriptor: whether the two share an offset
//          afterwards, whether the target's description was released (a pipe
//          whose last write end it was reads end of file; a file it was the
//          last descriptor of releases its flock), and whether a dup of the
//          target keeps its description alive.
//   SLEEP  dup2 onto a descriptor a sleeping call was entered through: a pipe
//          read, a pipe write into a full pipe, an accept, and an flock
//          waiting on another description's lock; with and without a dup of
//          the target kept. Each reports how long dup2 took and the sleeper's
//          answer 100 ms later ("asleep" if none; it is then woken by
//          SIGUSR1, without SA_RESTART). Also a poll(POLLIN, -1) of the target
//          and what it reports.
//
// Build and run, from this directory, in a scratch directory on a local
// filesystem (not a bind mount):
//   Darwin:  nix develop -c clang -Wall -pthread -o /tmp/fcntl-dup fcntl-dup.c
//            && cd "$(mktemp -d /private/tmp/fcntldup.XXXXXX)" && /tmp/fcntl-dup
//   Linux:   container run --rm -v "$PWD:/probe" gcc:14 sh -c
//              'gcc -Wall -O1 -pthread -o /tmp/p /probe/fcntl-dup.c &&
//               cd "$(mktemp -d)" && /tmp/p'
//            and again as uid 1000:
//              '... && useradd -m u && cd "$(su u -c "mktemp -d")" &&
//               su u -c /tmp/p'
//
// Measured 2026-10-03 on Darwin 27.0.0 arm64 (uid 501) three times and on
// Linux 6.18.5 aarch64 (Apple `container`, gcc:14 image, ext4) twice as root
// and twice as uid 1000; the first run of each is beside this file. Runs
// agreed but for the milliseconds and the SLEEP rows' "afterwards" lines for
// flock, which race the probe's own cleanup (it releases the holder and
// signals the sleeper together) and are not measurements.
// WoofWare.PosixKernel.Test/TestFcntlMeasured.fs replays them.
//
// What they say:
// - F_GETFL reports the access mode, O_NONBLOCK and O_SYNC/O_DSYNC on both.
//   Linux adds O_LARGEFILE to every description open(2) made (a file, a
//   directory, a device) and to none other (a pipe, a socket, an epoll
//   instance), and keeps O_DIRECTORY and O_NOFOLLOW as open was given them
//   (open-flags.c has the rest). It never reports O_CLOEXEC, EPOLL_CLOEXEC
//   included. Darwin reports neither O_DIRECTORY nor O_NOFOLLOW, and adds two
//   bits no F_SETFL can change: 0x10000 (FWASWRITTEN) once a write(2) or
//   pwrite(2) has returned having moved bytes (a socket's write too, but not
//   its send(2); an interrupted or close-ended write that had moved some, not
//   one asleep part way), an ftruncate(2) has succeeded, or open's O_TRUNC
//   has truncated a file that existed; and 0x4000 (FHASLOCK) once an flock(2)
//   has been granted through the description, which LOCK_UN does not clear.
//   Neither is set by a call that moved nothing or failed.
// - F_SETFL changes exactly these bits and ignores every other, the access
//   mode included: Linux O_APPEND, O_NONBLOCK, O_DIRECT (EINVAL on a
//   directory, a socket, an epoll instance and a device; a pipe and a file on
//   ext4 take it), O_NOATIME (EPERM unless the caller owns the inode, which
//   uid 1000 does not own for an epoll instance or a device, and ahead of
//   O_DIRECT's EINVAL) and FASYNC (only where the file has a fasync
//   operation: a pipe, a socket, /dev/urandom; ignored on a file, a
//   directory, an epoll instance and /dev/null). Darwin O_NONBLOCK, O_APPEND,
//   O_ASYNC, O_SYNC and O_DSYNC, on every kind; on a kqueue every F_SETFL
//   answers ENOTTY and still stores what it was given.
// - F_GETFD is FD_CLOEXEC on Linux, FD_CLOEXEC|FD_CLOFORK (1|2) on Darwin;
//   F_SETFD stores those bits of its argument and ignores the rest. They are
//   per descriptor: F_SETFD on one of a dup pair leaves the other. A new
//   descriptor from dup, F_DUPFD and dup2 has neither; F_DUPFD_CLOEXEC and
//   dup3(O_CLOEXEC) set FD_CLOEXEC alone; dup2 of a descriptor onto itself
//   changes nothing, and onto another descriptor of the same description
//   clears the target's. Darwin's kqueue() is FD_CLOEXEC|FD_CLOFORK, and its
//   F_DUPFD_CLOFORK (115) answers EBADF on an open descriptor.
// - F_DUPFD and F_DUPFD_CLOEXEC take the lowest free descriptor at or above
//   the argument; a negative argument, or one at or above RLIMIT_NOFILE's soft
//   limit, is EINVAL, a full table EMFILE, and a descriptor that is not open
//   EBADF ahead of all of them. An unknown command is EBADF on a closed
//   descriptor and EINVAL (Linux) or ENOTTY (Darwin) on an open one.
// - dup2: a target that is negative or at or above the soft limit, or a
//   source that is not open, is EBADF, and leaves an open target open. A
//   source onto itself answers itself. Onto an open target the target's
//   description is released as close(2) releases it (a pipe's last writer
//   gone, an flock dropped; a dup of the target keeps it), and a call asleep
//   through the target fares as under close(2): on Linux it sleeps on; on
//   Darwin a pipe read ends with 0, a pipe write with EPIPE, an accept with
//   ECONNABORTED, a poll of it is not woken, and dup2 itself blocks while an
//   flock waits through it (close-ends-call.c has close's rows).
// - dup3 (Linux): flags other than O_CLOEXEC are EINVAL, then the same
//   descriptor on both sides EINVAL, then dup2's EBADFs.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>
#ifdef __APPLE__
#include <dlfcn.h>
#include <sys/event.h>
// Darwin's libc has pipe2, which the SDK's headers may not declare.
static int pipe2(int p[2], int flags)
{
    static int (*fn)(int[2], int);
    if (!fn) fn = (int (*)(int[2], int))dlsym(RTLD_DEFAULT, "pipe2");
    if (!fn) {
        errno = ENOSYS;
        return -1;
    }
    return fn(p, flags);
}
#else
#include <sys/epoll.h>
#endif

#ifndef F_DUPFD_CLOFORK
#define F_DUPFD_CLOFORK (-12345)
#endif

static const char *en(int e)
{
    switch (e) {
    case EBADF: return "EBADF";
    case EINVAL: return "EINVAL";
    case EMFILE: return "EMFILE";
    case EPERM: return "EPERM";
    case ENOTTY: return "ENOTTY";
    case EAGAIN: return "EAGAIN";
    case EINTR: return "EINTR";
    case EACCES: return "EACCES";
    case EPIPE: return "EPIPE";
    case ENOTSUP: return "ENOTSUP";
    case ECONNABORTED: return "ECONNABORTED";
    case EFAULT: return "EFAULT";
    case ENXIO: return "ENXIO";
    case ENODEV: return "ENODEV";
    default: {
        if (e == EOPNOTSUPP) return "EOPNOTSUPP";
        static char buf[32];
        snprintf(buf, sizeof buf, "errno%d", e);
        return buf;
    }
    }
}

// "ok 0x..." or the errno, for a call answering a flag word.
static const char *word(int r)
{
    static char buf[4][64];
    static int which;
    char *b = buf[which++ % 4];
    if (r < 0) snprintf(b, 64, "%s", en(errno));
    else snprintf(b, 64, "ok 0x%x", (unsigned)r);
    return b;
}

// "ok <n>" or the errno, for a call answering a number.
static const char *num(long r)
{
    static char buf[4][64];
    static int which;
    char *b = buf[which++ % 4];
    if (r < 0) snprintf(b, 64, "%s", en(errno));
    else snprintf(b, 64, "ok %ld", r);
    return b;
}

static void die(const char *what)
{
    printf("DIE\t%s\t%s\n", what, en(errno));
    exit(2);
}

static int64_t now_ms(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (int64_t)t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

static void sleep_ms(int ms)
{
    struct timespec t = {ms / 1000, (long)(ms % 1000) * 1000000};
    while (nanosleep(&t, &t) != 0 && errno == EINTR) {}
}

static void on_usr1(int sig) { (void)sig; }

static void fill(int w);

// ---------------------------------------------------------------- kinds

enum kind {
    K_FILE_RDONLY, K_FILE_WRONLY, K_FILE_RDWR,
    K_FILE_RDONLY_CLOEXEC, K_FILE_RDWR_CLOEXEC, K_FILE_RDWR_SYNC,
    K_DIR, K_DIR_DIRECTORY, K_FOREIGN,
    K_PIPE_R, K_PIPE_W, K_PIPE2_NB_R, K_PIPE2_NB_W, K_PIPE2_CLOEXEC_R, K_PIPE2_CLOEXEC_W,
    K_INET_STREAM, K_INET_DGRAM, K_INET6_STREAM, K_UNIX_STREAM, K_UNIX_DGRAM,
    K_SOCK_NONBLOCK, K_SOCK_CLOEXEC,
    K_ACCEPTED, K_ACCEPTED_FROM_NONBLOCKING, K_ACCEPT4_NONBLOCK, K_ACCEPT4_CLOEXEC,
    K_PORT, K_PORT_CLOEXEC,
    K_NULL_RDONLY, K_NULL_WRONLY, K_NULL_RDWR, K_URANDOM_RDONLY, K_URANDOM_RDWR,
    K_COUNT
};

static const char *kind_names[K_COUNT] = {
    "file-rdonly", "file-wronly", "file-rdwr",
    "file-rdonly-cloexec", "file-rdwr-cloexec", "file-rdwr-sync",
    "dir", "dir-o_directory", "foreign-rdonly",
    "pipe-r", "pipe-w", "pipe2-nonblock-r", "pipe2-nonblock-w", "pipe2-cloexec-r", "pipe2-cloexec-w",
    "inet-stream", "inet-dgram", "inet6-stream", "unix-stream", "unix-dgram",
    "inet-stream-sock_nonblock", "inet-stream-sock_cloexec",
    "accepted", "accepted-from-nonblocking", "accept4-sock_nonblock", "accept4-sock_cloexec",
    "port", "port-cloexec",
    "null-rdonly", "null-wronly", "null-rdwr", "urandom-rdonly", "urandom-rdwr",
};

// Every descriptor a kind's construction opened besides the one it returns,
// closed by `release`.
static int extra[4];
static int nextra;

static void keep(int fd) { extra[nextra++] = fd; }

static void release(void)
{
    for (int i = 0; i < nextra; i++) close(extra[i]);
    nextra = 0;
}

static int seed_file(const char *name)
{
    int fd = open(name, O_CREAT | O_RDWR | O_TRUNC, 0644);
    if (fd < 0) die("seed file");
    if (write(fd, "abcdefgh", 8) != 8) die("seed write");
    close(fd);
    return 0;
}

static int listener_on(struct sockaddr_in *addr)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    if (l < 0) die("socket listener");
    memset(addr, 0, sizeof *addr);
    addr->sin_family = AF_INET;
    addr->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(l, (struct sockaddr *)addr, sizeof *addr) != 0) die("bind");
    socklen_t len = sizeof *addr;
    getsockname(l, (struct sockaddr *)addr, &len);
    if (listen(l, 16) != 0) die("listen");
    return l;
}

static int connect_to(const struct sockaddr_in *addr)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (c < 0) die("socket client");
    if (connect(c, (const struct sockaddr *)addr, sizeof *addr) != 0) die("connect");
    return c;
}

static const char *foreign_path(void)
{
#ifdef __APPLE__
    return "/etc/hosts";
#else
    return "/etc/passwd";
#endif
}

// A fresh descriptor of `k`, or -1 if this flavour has no such kind (or the
// construction failed, which is reported).
static int make(enum kind k)
{
    int p[2];
    struct sockaddr_in addr;
    int l, c, a;
    switch (k) {
    case K_FILE_RDONLY: seed_file("f"); return open("f", O_RDONLY);
    case K_FILE_WRONLY: seed_file("f"); return open("f", O_WRONLY);
    case K_FILE_RDWR: seed_file("f"); return open("f", O_RDWR);
    case K_FILE_RDONLY_CLOEXEC: seed_file("f"); return open("f", O_RDONLY | O_CLOEXEC);
    case K_FILE_RDWR_CLOEXEC: seed_file("f"); return open("f", O_RDWR | O_CLOEXEC);
    case K_FILE_RDWR_SYNC: seed_file("f"); return open("f", O_RDWR | O_SYNC);
    case K_DIR: mkdir("d", 0755); return open("d", O_RDONLY);
    case K_DIR_DIRECTORY: mkdir("d", 0755); return open("d", O_RDONLY | O_DIRECTORY);
    case K_FOREIGN: return open(foreign_path(), O_RDONLY);
    case K_PIPE_R: if (pipe(p) != 0) die("pipe"); keep(p[1]); return p[0];
    case K_PIPE_W: if (pipe(p) != 0) die("pipe"); keep(p[0]); return p[1];
    case K_PIPE2_NB_R: if (pipe2(p, O_NONBLOCK) != 0) die("pipe2"); keep(p[1]); return p[0];
    case K_PIPE2_NB_W: if (pipe2(p, O_NONBLOCK) != 0) die("pipe2"); keep(p[0]); return p[1];
    case K_PIPE2_CLOEXEC_R: if (pipe2(p, O_CLOEXEC) != 0) die("pipe2"); keep(p[1]); return p[0];
    case K_PIPE2_CLOEXEC_W: if (pipe2(p, O_CLOEXEC) != 0) die("pipe2"); keep(p[0]); return p[1];
    case K_INET_STREAM: return socket(AF_INET, SOCK_STREAM, 0);
    case K_INET_DGRAM: return socket(AF_INET, SOCK_DGRAM, 0);
    case K_INET6_STREAM: return socket(AF_INET6, SOCK_STREAM, 0);
    case K_UNIX_STREAM: return socket(AF_UNIX, SOCK_STREAM, 0);
    case K_UNIX_DGRAM: return socket(AF_UNIX, SOCK_DGRAM, 0);
#ifdef __APPLE__
    case K_SOCK_NONBLOCK:
    case K_SOCK_CLOEXEC:
    case K_ACCEPT4_NONBLOCK:
    case K_ACCEPT4_CLOEXEC:
    case K_PORT_CLOEXEC:
    case K_URANDOM_RDONLY:
    case K_URANDOM_RDWR:
    case K_NULL_RDONLY:
    case K_NULL_WRONLY:
    case K_NULL_RDWR:
        return -2;
    case K_PORT: return kqueue();
#else
    case K_SOCK_NONBLOCK: return socket(AF_INET, SOCK_STREAM | SOCK_NONBLOCK, 0);
    case K_SOCK_CLOEXEC: return socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
    case K_ACCEPT4_NONBLOCK:
    case K_ACCEPT4_CLOEXEC:
        l = listener_on(&addr);
        c = connect_to(&addr);
        a = accept4(l, NULL, NULL, k == K_ACCEPT4_NONBLOCK ? SOCK_NONBLOCK : SOCK_CLOEXEC);
        keep(l);
        keep(c);
        return a;
    case K_PORT: return epoll_create1(0);
    case K_PORT_CLOEXEC: return epoll_create1(EPOLL_CLOEXEC);
    case K_NULL_RDONLY: return open("/dev/null", O_RDONLY);
    case K_NULL_WRONLY: return open("/dev/null", O_WRONLY);
    case K_NULL_RDWR: return open("/dev/null", O_RDWR);
    case K_URANDOM_RDONLY: return open("/dev/urandom", O_RDONLY);
    case K_URANDOM_RDWR: return open("/dev/urandom", O_RDWR);
#endif
    case K_ACCEPTED:
    case K_ACCEPTED_FROM_NONBLOCKING:
        l = listener_on(&addr);
        if (k == K_ACCEPTED_FROM_NONBLOCKING) fcntl(l, F_SETFL, fcntl(l, F_GETFL) | O_NONBLOCK);
        c = connect_to(&addr);
        sleep_ms(10);
        a = accept(l, NULL, NULL);
        keep(l);
        keep(c);
        return a;
    case K_COUNT: break;
    }
    return -2;
}

static void section_kind(void)
{
    for (int k = 0; k < K_COUNT; k++) {
        int fd = make((enum kind)k);
        if (fd == -2) continue;
        if (fd < 0) {
            printf("KIND\t%s\tmake %s\n", kind_names[k], en(errno));
            release();
            continue;
        }
        int fl = fcntl(fd, F_GETFL);
        const char *getfl = word(fl);
        int fdfl = fcntl(fd, F_GETFD);
        printf("KIND\t%s\tF_GETFL %s\tF_GETFD %s\n", kind_names[k], getfl, word(fdfl));
        close(fd);
        release();
    }
}

// ---------------------------------------------------------------- SETFL

static const enum kind setfl_kinds[] = {
    K_FILE_RDONLY, K_FILE_RDWR, K_DIR, K_FOREIGN, K_PIPE_R, K_PIPE_W, K_INET_STREAM, K_INET_DGRAM,
    K_UNIX_STREAM, K_ACCEPTED, K_PORT, K_NULL_RDWR, K_URANDOM_RDONLY,
};

static void setfl_row(enum kind k, const char *label, int requested, int relative)
{
    int fd = make(k);
    if (fd == -2) return;
    if (fd < 0) {
        printf("SETFL\t%s\t%s\tmake %s\n", kind_names[k], label, en(errno));
        release();
        return;
    }
    int before = fcntl(fd, F_GETFL);
    int w = relative ? (before | requested) : requested;
    int set = fcntl(fd, F_SETFL, w);
    const char *setr = word(set);
    int after = fcntl(fd, F_GETFL);
    const char *afterr = word(after);
    int clear = fcntl(fd, F_SETFL, before);
    const char *clearr = word(clear);
    int after2 = fcntl(fd, F_GETFL);
    printf("SETFL\t%s\t%s\tbefore 0x%x\tset 0x%x %s\tafter %s\tclear %s\tafter %s\n", kind_names[k], label,
           (unsigned)before, (unsigned)w, setr, afterr, clearr, word(after2));
    close(fd);
    release();
}

static void section_setfl(void)
{
    for (size_t i = 0; i < sizeof setfl_kinds / sizeof setfl_kinds[0]; i++) {
        enum kind k = setfl_kinds[i];
        for (int b = 0; b < 32; b++) {
            char label[16];
            snprintf(label, sizeof label, "bit%d", b);
            setfl_row(k, label, (int)(1u << b), 1);
        }
        setfl_row(k, "word0", 0, 0);
        setfl_row(k, "word-1", -1, 0);
        setfl_row(k, "word3", 3, 0);
    }
}

// ---------------------------------------------------------------- WRITTEN

static void written_row(const char *label, int (*act)(int fd, int other), int mode)
{
    seed_file("f");
    int fd = open("f", mode);
    int other = open("f", O_RDWR);
    if (fd < 0 || other < 0) die("written open");
    int before = fcntl(fd, F_GETFL);
    errno = 0;
    int r = act(fd, other);
    int e = errno;
    int after = fcntl(fd, F_GETFL);
    errno = e;
    printf("WRITTEN\t%s\tbefore 0x%x\tcall %s\tafter 0x%x\n", label, (unsigned)before, num(r), (unsigned)after);
    close(fd);
    close(other);
}

static int act_write(int fd, int o) { (void)o; return (int)write(fd, "x", 1); }
static int act_write0(int fd, int o) { (void)o; return (int)write(fd, "x", 0); }
static int act_pwrite(int fd, int o) { (void)o; return (int)pwrite(fd, "x", 1, 3); }
static int act_ftruncate(int fd, int o) { (void)o; return ftruncate(fd, 2); }
static int act_ftruncate_same(int fd, int o) { (void)o; return ftruncate(fd, 8); }
static int act_read(int fd, int o) { (void)o; char b[4]; return (int)read(fd, b, 4); }
static int act_flock_sh(int fd, int o) { (void)o; return flock(fd, LOCK_SH | LOCK_NB); }
static int act_flock_ex(int fd, int o) { (void)o; return flock(fd, LOCK_EX | LOCK_NB); }
static int act_flock_un(int fd, int o) { (void)o; return flock(fd, LOCK_UN); }
static int act_flock_sh_un(int fd, int o) { (void)o; flock(fd, LOCK_SH | LOCK_NB); return flock(fd, LOCK_UN); }
static int act_flock_sh_ex(int fd, int o) { (void)o; flock(fd, LOCK_SH | LOCK_NB); return flock(fd, LOCK_EX | LOCK_NB); }
static int act_flock_refused(int fd, int o) { flock(o, LOCK_EX | LOCK_NB); return flock(fd, LOCK_SH | LOCK_NB); }
static int act_flock_conversion_refused(int fd, int o)
{
    flock(fd, LOCK_SH | LOCK_NB);
    flock(o, LOCK_SH | LOCK_NB);
    return flock(fd, LOCK_EX | LOCK_NB);
}
static int act_flock_via_dup(int fd, int o)
{
    (void)o;
    int d = dup(fd);
    int r = flock(d, LOCK_SH | LOCK_NB);
    close(d);
    return r;
}

static void section_written(void)
{
    written_row("write rdwr", act_write, O_RDWR);
    written_row("write wronly", act_write, O_WRONLY);
    written_row("write0 rdwr", act_write0, O_RDWR);
    written_row("write rdonly (refused)", act_write, O_RDONLY);
    written_row("pwrite rdwr", act_pwrite, O_RDWR);
    written_row("ftruncate rdwr", act_ftruncate, O_RDWR);
    written_row("ftruncate to same length", act_ftruncate_same, O_RDWR);
    written_row("read rdwr", act_read, O_RDWR);
    written_row("flock LOCK_SH", act_flock_sh, O_RDWR);
    written_row("flock LOCK_EX", act_flock_ex, O_RDWR);
    written_row("flock LOCK_UN unheld", act_flock_un, O_RDWR);
    written_row("flock LOCK_SH then LOCK_UN", act_flock_sh_un, O_RDWR);
    written_row("flock LOCK_SH then LOCK_EX", act_flock_sh_ex, O_RDWR);
    written_row("flock refused", act_flock_refused, O_RDWR);
    written_row("flock conversion refused", act_flock_conversion_refused, O_RDWR);
    written_row("flock through a dup", act_flock_via_dup, O_RDWR);
    // O_TRUNC on an existing file, and on a file it creates.
    seed_file("f");
    int fd = open("f", O_RDWR | O_TRUNC);
    printf("WRITTEN\topen O_TRUNC existing\tafter %s\n", word(fcntl(fd, F_GETFL)));
    close(fd);
    unlink("g");
    fd = open("g", O_RDWR | O_CREAT | O_TRUNC, 0644);
    printf("WRITTEN\topen O_CREAT|O_TRUNC new\tafter %s\n", word(fcntl(fd, F_GETFL)));
    close(fd);
    // A pipe end written.
    int p[2];
    pipe(p);
    write(p[1], "x", 1);
    printf("WRITTEN\tpipe write end after a write\tafter %s\n", word(fcntl(p[1], F_GETFL)));
    close(p[0]);
    close(p[1]);
}

// What moves the written and locked bits beyond the WRITTEN rows: writes to
// sockets and pipes that transfer some, none, or fail; failing truncations;
// and blocking flocks that wait, or are interrupted.
struct blocked_write {
    int fd;
    size_t count;
    long rv;
    int err;
    volatile int done;
};

static void *blocked_write_main(void *p)
{
    struct blocked_write *b = p;
    static char big[200000];
    errno = 0;
    b->rv = write(b->fd, big, b->count);
    b->err = errno;
    b->done = 1;
    return NULL;
}

struct blocked_flock {
    int fd;
    long rv;
    int err;
    volatile int done;
};

static void *blocked_flock_main(void *p)
{
    struct blocked_flock *b = p;
    errno = 0;
    b->rv = flock(b->fd, LOCK_EX);
    b->err = errno;
    b->done = 1;
    return NULL;
}

static void section_written_more(void)
{
    char buf[70000];
    struct sockaddr_in addr;
    // A connected socket: a write that transfers, and one with no peer.
    int l = listener_on(&addr);
    int c = connect_to(&addr);
    int a = accept(l, NULL, NULL);
    long r = write(c, "abc", 3);
    printf("WRITTEN\tsocket write\tcall %s\tafter %s\n", num(r), word(fcntl(c, F_GETFL)));
    r = send(a, "abc", 3, 0);
    printf("WRITTEN\tsocket send\tcall %s\tafter %s\n", num(r), word(fcntl(a, F_GETFL)));
    int u = socket(AF_INET, SOCK_STREAM, 0);
    r = write(u, "abc", 3);
    printf("WRITTEN\tunconnected socket write\tcall %s\tafter %s\n", num(r), word(fcntl(u, F_GETFL)));
    close(u);
    close(a);
    close(c);
    close(l);
    // A pipe end can be written through only by its one description, so a
    // full pipe's writer is always marked already; only a pipe with no reader
    // tests a write that moves nothing.
    int p[2];
    pthread_t t;
    pipe(p);
    close(p[0]);
    r = write(p[1], "x", 1);
    printf("WRITTEN\tpipe write EPIPE\tcall %s\tafter %s\n", num(r), word(fcntl(p[1], F_GETFL)));
    close(p[1]);
    // A blocking write into an empty pipe larger than it holds, interrupted by
    // a signal once some is in.
    pipe(p);
    struct blocked_write bw = {p[1], 200000, 0, 0, 0};
    pthread_create(&t, NULL, blocked_write_main, &bw);
    sleep_ms(50);
    pthread_kill(t, SIGUSR1);
    sleep_ms(50);
    printf("WRITTEN\tblocking pipe write interrupted part way\tcall %s\tafter %s\n",
           bw.done ? (bw.rv < 0 ? en(bw.err) : num(bw.rv)) : "asleep", word(fcntl(p[1], F_GETFL)));
    fcntl(p[0], F_SETFL, O_NONBLOCK);
    for (int i = 0; i < 200 && !bw.done; i++) {
        read(p[0], buf, sizeof buf);
        sleep_ms(5);
    }
    pthread_join(t, NULL);
    close(p[0]);
    close(p[1]);
    // A blocking write into an empty pipe larger than it holds, so that some
    // goes in before it sleeps, whose descriptor is then closed under it, a
    // dup kept to look through.
    pipe(p);
    int keep_dup = dup(p[1]);
    struct blocked_write cw = {p[1], 200000, 0, 0, 0};
    pthread_create(&t, NULL, blocked_write_main, &cw);
    sleep_ms(50);
    printf("WRITTEN\tblocking pipe write asleep part way\tdup's F_GETFL %s\n", word(fcntl(keep_dup, F_GETFL)));
    close(p[1]);
    sleep_ms(50);
    printf("WRITTEN\tblocking pipe write part way, ended by a close\tcall %s\tdup's F_GETFL %s\n",
           cw.done ? (cw.rv < 0 ? en(cw.err) : num(cw.rv)) : "asleep", word(fcntl(keep_dup, F_GETFL)));
    if (!cw.done) pthread_kill(t, SIGUSR1);
    fcntl(p[0], F_SETFL, O_NONBLOCK);
    for (int i = 0; i < 200 && !cw.done; i++) {
        read(p[0], buf, sizeof buf);
        sleep_ms(5);
    }
    pthread_join(t, NULL);
    printf("WRITTEN\tblocking pipe write part way, after it returned\tcall %s\tdup's F_GETFL %s\n",
           cw.rv < 0 ? en(cw.err) : num(cw.rv), word(fcntl(keep_dup, F_GETFL)));
    close(keep_dup);
    close(p[0]);
    // Truncations that fail.
    seed_file("f");
    int fd = open("f", O_RDWR);
    r = ftruncate(fd, -1);
    printf("WRITTEN\tftruncate -1\tcall %s\tafter %s\n", num(r), word(fcntl(fd, F_GETFL)));
    close(fd);
    fd = open("f", O_RDONLY);
    r = ftruncate(fd, 0);
    printf("WRITTEN\tftruncate through O_RDONLY\tcall %s\tafter %s\n", num(r), word(fcntl(fd, F_GETFL)));
    close(fd);
    pipe(p);
    r = ftruncate(p[1], 0);
    printf("WRITTEN\tftruncate of a pipe\tcall %s\tafter %s\n", num(r), word(fcntl(p[1], F_GETFL)));
    close(p[0]);
    close(p[1]);
    // pwrite at a negative offset (EINVAL).
    fd = open("f", O_RDWR);
    r = pwrite(fd, "x", 1, -1);
    printf("WRITTEN\tpwrite at -1\tcall %s\tafter %s\n", num(r), word(fcntl(fd, F_GETFL)));
    close(fd);
    // A blocking flock that waits and is granted, and one that is interrupted.
    int holder = open("f", O_RDONLY);
    flock(holder, LOCK_EX);
    fd = open("f", O_RDONLY);
    struct blocked_flock bf = {fd, 0, 0, 0};
    pthread_create(&t, NULL, blocked_flock_main, &bf);
    sleep_ms(50);
    flock(holder, LOCK_UN);
    pthread_join(t, NULL);
    printf("WRITTEN\tblocking flock granted after a wait\tcall %s\tafter %s\n", bf.rv < 0 ? en(bf.err) : num(bf.rv),
           word(fcntl(fd, F_GETFL)));
    close(fd);
    flock(holder, LOCK_EX);
    fd = open("f", O_RDONLY);
    struct blocked_flock bi = {fd, 0, 0, 0};
    pthread_create(&t, NULL, blocked_flock_main, &bi);
    sleep_ms(50);
    pthread_kill(t, SIGUSR1);
    sleep_ms(50);
    printf("WRITTEN\tblocking flock interrupted\tcall %s\tafter %s\n",
           bi.done ? (bi.rv < 0 ? en(bi.err) : num(bi.rv)) : "asleep", word(fcntl(fd, F_GETFL)));
    flock(holder, LOCK_UN);
    pthread_join(t, NULL);
    close(fd);
    close(holder);
    // flock of a pipe and a socket (Darwin answers ENOTSUP).
    pipe(p);
    r = flock(p[0], LOCK_SH | LOCK_NB);
    printf("WRITTEN\tflock of a pipe\tcall %s\tafter %s\n", num(r), word(fcntl(p[0], F_GETFL)));
    close(p[0]);
    close(p[1]);
}

// ---------------------------------------------------------------- SETFD

static void section_setfd(void)
{
    for (int b = -2; b < 32; b++) {
        seed_file("f");
        int fd = open("f", O_RDWR);
        int w = b == -2 ? 0 : b == -1 ? -1 : (int)(1u << b);
        int r = fcntl(fd, F_SETFD, w);
        const char *rr = word(r);
        printf("SETFD\t0x%x\tset %s\tF_GETFD %s\n", (unsigned)w, rr, word(fcntl(fd, F_GETFD)));
        close(fd);
    }
    // Clearing again.
    seed_file("f");
    int fd = open("f", O_RDWR | O_CLOEXEC);
    int r = fcntl(fd, F_SETFD, 0);
    printf("SETFD\tclear from O_CLOEXEC\tset %s\tF_GETFD %s\n", word(r), word(fcntl(fd, F_GETFD)));
    close(fd);
#ifdef __APPLE__
    fd = open("f", O_RDWR);
    fcntl(fd, F_SETFD, 3);
    r = fcntl(fd, F_SETFD, 1);
    printf("SETFD\t3 then 1\tset %s\tF_GETFD %s\n", word(r), word(fcntl(fd, F_GETFD)));
    close(fd);
#endif
}

// ---------------------------------------------------------------- CLOEXEC

static void flags_row(const char *label, int oldfd, int newfd)
{
    printf("CLOEXEC\t%s\told %s\tnew %s\n", label, word(fcntl(oldfd, F_GETFD)), word(fcntl(newfd, F_GETFD)));
}

static void section_cloexec(void)
{
    seed_file("f");
    int set[] = {0, 1, 2, 3};
    for (int i = 0; i < 4; i++) {
        int w = set[i];
#ifndef __APPLE__
        if (w & 2) continue;
#endif
        char label[96];
        int fd = open("f", O_RDWR);
        fcntl(fd, F_SETFD, w);
        int n = dup(fd);
        snprintf(label, sizeof label, "old 0x%x\tdup", w);
        flags_row(label, fd, n);
        close(n);
        n = fcntl(fd, F_DUPFD, 0);
        snprintf(label, sizeof label, "old 0x%x\tF_DUPFD", w);
        flags_row(label, fd, n);
        close(n);
        n = fcntl(fd, F_DUPFD_CLOEXEC, 0);
        snprintf(label, sizeof label, "old 0x%x\tF_DUPFD_CLOEXEC", w);
        flags_row(label, fd, n);
        close(n);
#ifdef __APPLE__
        n = fcntl(fd, F_DUPFD_CLOFORK, 0);
        snprintf(label, sizeof label, "old 0x%x\tF_DUPFD_CLOFORK", w);
        flags_row(label, fd, n);
        close(n);
#endif
        n = dup2(fd, 40);
        snprintf(label, sizeof label, "old 0x%x\tdup2 onto free", w);
        flags_row(label, fd, 40);
        close(40);
        int target = open("f", O_RDWR | O_CLOEXEC);
#ifdef __APPLE__
        fcntl(target, F_SETFD, 3);
#endif
        n = dup2(fd, target);
        snprintf(label, sizeof label, "old 0x%x\tdup2 onto open (cloexec target)", w);
        flags_row(label, fd, target);
        close(target);
        n = dup2(fd, fd);
        snprintf(label, sizeof label, "old 0x%x\tdup2 onto itself %s", w, num(n));
        flags_row(label, fd, fd);
#ifndef __APPLE__
        n = dup3(fd, 40, 0);
        snprintf(label, sizeof label, "old 0x%x\tdup3 flags 0 %s", w, num(n));
        flags_row(label, fd, 40);
        close(40);
        n = dup3(fd, 40, O_CLOEXEC);
        snprintf(label, sizeof label, "old 0x%x\tdup3 O_CLOEXEC %s", w, num(n));
        flags_row(label, fd, 40);
        close(40);
#endif
        close(fd);
    }
    int fd = open("f", O_RDWR);
    int n = dup(fd);
    fcntl(n, F_SETFD, FD_CLOEXEC);
    flags_row("F_SETFD on the dup of a pair", fd, n);
    close(fd);
    close(n);
}

// ---------------------------------------------------------------- LIMIT

static const int limit_args[] = {INT_MIN, -1, 0, 4, 6, 62, 63, 64, 65, 1 << 20, INT_MAX};

static void section_limit(void)
{
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    printf("LIMIT\tfound\tsoft %lld\thard %lld\n", (long long)rl.rlim_cur, (long long)rl.rlim_max);
    rl.rlim_cur = 64;
    if (setrlimit(RLIMIT_NOFILE, &rl) != 0) die("setrlimit");
    // Leave 0, 1 and 2 and open 3..12, then close 5 and 9, so that a scan
    // from 4 finds 5 and one from 6 finds 9.
    for (int i = 3; i < 64; i++) close(i);
    seed_file("f");
    for (int i = 3; i <= 12; i++) {
        if (open("f", O_RDONLY) != i) die("limit seed");
    }
    close(5);
    close(9);
    int cmds[] = {F_DUPFD, F_DUPFD_CLOEXEC};
    const char *cmd_names[] = {"F_DUPFD", "F_DUPFD_CLOEXEC"};
    for (int c = 0; c < 2; c++) {
        for (size_t i = 0; i < sizeof limit_args / sizeof limit_args[0]; i++) {
            int a = limit_args[i];
            int r = fcntl(3, cmds[c], a);
            const char *rr = num(r);
            printf("LIMIT\t%s\tfrom 3\targ %d\t%s\n", cmd_names[c], a, rr);
            if (r >= 0) close(r);
            r = fcntl(30, cmds[c], a);
            printf("LIMIT\t%s\tfrom closed 30\targ %d\t%s\n", cmd_names[c], a, num(r));
            if (r >= 0) close(r);
            r = fcntl(-1, cmds[c], a);
            printf("LIMIT\t%s\tfrom -1\targ %d\t%s\n", cmd_names[c], a, num(r));
            if (r >= 0) close(r);
        }
    }
    // Fill the table to 63.
    int filled = 0;
    while (fcntl(3, F_DUPFD, 0) >= 0) filled++;
    printf("LIMIT\tfilled\t%d more\t%s\n", filled, en(errno));
    for (size_t i = 0; i < sizeof limit_args / sizeof limit_args[0]; i++) {
        int a = limit_args[i];
        printf("LIMIT\tF_DUPFD full\targ %d\t%s\n", a, num(fcntl(3, F_DUPFD, a)));
    }
    printf("LIMIT\tdup full\t%s\n", num(dup(3)));
    printf("LIMIT\tdup2 full onto open 40\t%s\n", num(dup2(3, 40)));
    printf("LIMIT\topen full\t%s\n", num(open("f", O_RDONLY)));
    for (int i = 3; i < 64; i++) close(i);
    int bad[] = {-1, 63, 64, INT_MAX};
    for (size_t i = 0; i < 4; i++) {
        int b = bad[i];
        printf("LIMIT\tclosed %d\tF_GETFL %s\tF_SETFL %s\tF_GETFD %s\tF_SETFD %s\n", b, word(fcntl(b, F_GETFL)),
               word(fcntl(b, F_SETFL, 0)), word(fcntl(b, F_GETFD)), word(fcntl(b, F_SETFD, 0)));
    }
    // An unknown command, on an open and a closed descriptor.
    int fd = open("f", O_RDONLY);
    printf("LIMIT\tcommand 9999\topen %s\tclosed %s\n", num(fcntl(fd, 9999, 0)), num(fcntl(60, 9999, 0)));
    close(fd);
}

// ---------------------------------------------------------------- DUP2

static int is_open(int fd) { return fcntl(fd, F_GETFD) >= 0; }

static void dup2_row(const char *label, int oldfd, int newfd)
{
    int was = is_open(newfd);
    int r = dup2(oldfd, newfd);
    const char *rr = num(r);
    printf("DUP2\tdup2\t%s\t%d -> %d\t%s\ttarget was %s, is %s\n", label, oldfd, newfd, rr, was ? "open" : "closed",
           is_open(newfd) ? "open" : "closed");
}

#ifndef __APPLE__
static void dup3_row(const char *label, int oldfd, int newfd, int flags)
{
    int was = is_open(newfd);
    int r = dup3(oldfd, newfd, flags);
    const char *rr = num(r);
    printf("DUP2\tdup3\t%s\t%d -> %d flags 0x%x\t%s\ttarget was %s, is %s\n", label, oldfd, newfd, (unsigned)flags,
           rr, was ? "open" : "closed", is_open(newfd) ? "open" : "closed");
    if (r >= 0 && r != oldfd) close(r);
}
#endif

static void reset_table(void)
{
    for (int i = 3; i < 64; i++) close(i);
    seed_file("f");
    if (open("f", O_RDONLY) != 3) die("dup2 seed 3");
    if (open("f", O_RDONLY) != 4) die("dup2 seed 4");
}

static void section_dup2(void)
{
    reset_table();
    dup2_row("open onto itself", 3, 3);
    dup2_row("closed onto itself", 20, 20);
    dup2_row("-1 onto itself", -1, -1);
    dup2_row("open onto free", 3, 20);
    close(20);
    dup2_row("open onto open", 3, 4);
    reset_table();
    dup2_row("closed onto open", 20, 4);
    dup2_row("closed onto free", 20, 21);
    dup2_row("open onto -1", 3, -1);
    dup2_row("open onto INT_MIN", 3, INT_MIN);
    dup2_row("open onto 63 (soft limit 64)", 3, 63);
    close(63);
    dup2_row("open onto 64", 3, 64);
    dup2_row("open onto 65", 3, 65);
    dup2_row("open onto INT_MAX", 3, INT_MAX);
    dup2_row("closed onto 64", 20, 64);
    dup2_row("-1 onto 64", -1, 64);
    dup2_row("-1 onto open", -1, 4);
    dup2_row("closed onto -1", 20, -1);
#ifndef __APPLE__
    reset_table();
    for (int b = 0; b < 32; b++) {
        char label[32];
        snprintf(label, sizeof label, "bit%d", b);
        dup3_row(label, 3, 20, (int)(1u << b));
    }
    dup3_row("open onto itself, 0", 3, 3, 0);
    dup3_row("open onto itself, O_CLOEXEC", 3, 3, O_CLOEXEC);
    dup3_row("closed onto itself, 0", 20, 20, 0);
    dup3_row("closed onto itself, bad flag", 20, 20, 1);
    dup3_row("open onto itself, bad flag", 3, 3, 1);
    dup3_row("closed onto free, bad flag", 20, 21, 1);
    dup3_row("closed onto free, 0", 20, 21, 0);
    dup3_row("open onto 64, 0", 3, 64, 0);
    dup3_row("open onto 64, bad flag", 3, 64, 1);
    dup3_row("closed onto 64, 0", 20, 64, 0);
    dup3_row("open onto -1, 0", 3, -1, 0);
    dup3_row("closed onto open, 0", 20, 4, 0);
    dup3_row("open onto open, O_CLOEXEC", 3, 4, O_CLOEXEC);
    printf("DUP2\tdup3 onto open\tnew F_GETFD %s\n", word(fcntl(4, F_GETFD)));
#endif
}

// ---------------------------------------------------------------- ONTO

static void section_onto(void)
{
    for (int i = 3; i < 64; i++) close(i);
    seed_file("f");
    // Offsets: X at 5, Y a fresh description at 0; dup2(X, Y).
    int x = open("f", O_RDONLY);
    int y = open("f", O_RDONLY);
    lseek(x, 5, SEEK_SET);
    int r = dup2(x, y);
    printf("ONTO\toffset\tdup2 %s\ttarget at %lld\n", num(r), (long long)lseek(y, 0, SEEK_CUR));
    lseek(y, 2, SEEK_SET);
    printf("ONTO\toffset\tsource at %lld after the target moved to 2\n", (long long)lseek(x, 0, SEEK_CUR));
    close(x);
    close(y);
    // A pipe whose only write end is the target.
    int p[2], q[2];
    pipe(p);
    pipe(q);
    fcntl(p[0], F_SETFL, O_NONBLOCK);
    char b[4];
    printf("ONTO\tpipe last writer\tread before %s\n", num(read(p[0], b, 4)));
    r = dup2(q[1], p[1]);
    printf("ONTO\tpipe last writer\tdup2 %s\tread after %s\n", num(r), num(read(p[0], b, 4)));
    close(p[0]);
    close(p[1]);
    close(q[0]);
    close(q[1]);
    // The same with a dup of the target kept.
    pipe(p);
    pipe(q);
    fcntl(p[0], F_SETFL, O_NONBLOCK);
    int kept = dup(p[1]);
    r = dup2(q[1], p[1]);
    printf("ONTO\tpipe writer with a dup kept\tdup2 %s\tread after %s\n", num(r), num(read(p[0], b, 4)));
    close(kept);
    printf("ONTO\tpipe writer with a dup kept\tread after the dup closes %s\n", num(read(p[0], b, 4)));
    close(p[0]);
    close(p[1]);
    close(q[0]);
    close(q[1]);
    // A flock held through the target's description.
    int holder = open("f", O_RDONLY);
    int other = open("f", O_RDONLY);
    int source = open("f", O_RDONLY);
    flock(holder, LOCK_EX | LOCK_NB);
    printf("ONTO\tflock\tbefore, other's LOCK_EX|LOCK_NB %s\n", num(flock(other, LOCK_EX | LOCK_NB)));
    r = dup2(source, holder);
    printf("ONTO\tflock\tdup2 %s\tother's LOCK_EX|LOCK_NB %s\n", num(r), num(flock(other, LOCK_EX | LOCK_NB)));
    close(holder);
    close(other);
    close(source);
    // dup2 onto one of a dup pair: is the pair's description still alive?
    x = open("f", O_RDONLY);
    y = dup(x);
    int z = open("f", O_RDONLY);
    lseek(x, 6, SEEK_SET);
    r = dup2(z, y);
    printf("ONTO\tdup pair\tdup2 %s\tthe other of the pair still at %lld\n", num(r), (long long)lseek(x, 0, SEEK_CUR));
    close(x);
    close(y);
    close(z);
    // dup2 of a dup pair's member onto the other: they already name one
    // description.
    x = open("f", O_RDONLY);
    y = dup(x);
    fcntl(y, F_SETFD, FD_CLOEXEC);
    r = dup2(x, y);
    printf("ONTO\tsame description\tdup2 %s\ttarget F_GETFD %s\n", num(r), word(fcntl(y, F_GETFD)));
    close(x);
    close(y);
}

// ---------------------------------------------------------------- SLEEP

enum sleep_kind { S_READ, S_WRITE, S_ACCEPT, S_FLOCK, S_POLL };
static const char *sleep_names[] = {"read", "write", "accept", "flock", "poll"};

struct sleeper {
    pthread_t thread;
    enum sleep_kind kind;
    int fd;
    volatile int done;
    long rv;
    int err;
    short revents;
};

static void *sleeper_main(void *p)
{
    struct sleeper *s = p;
    char buf[8];
    errno = 0;
    switch (s->kind) {
    case S_READ: s->rv = read(s->fd, buf, 8); break;
    case S_WRITE: s->rv = write(s->fd, "y", 1); break;
    case S_ACCEPT: s->rv = accept(s->fd, NULL, NULL); break;
    case S_FLOCK: s->rv = flock(s->fd, LOCK_EX); break;
    case S_POLL: {
        struct pollfd pfd = {s->fd, POLLIN, 0};
        s->rv = poll(&pfd, 1, -1);
        s->revents = pfd.revents;
        break;
    }
    }
    s->err = errno;
    s->done = 1;
    return NULL;
}

static void fill(int w)
{
    int fl = fcntl(w, F_GETFL);
    fcntl(w, F_SETFL, fl | O_NONBLOCK);
    static char big[4096];
    while (write(w, big, sizeof big) > 0) {}
    fcntl(w, F_SETFL, fl);
}

struct dup2_job {
    int source, target;
    long rv;
    int err;
    int64_t took;
    volatile int done;
};

static void *dup2_main(void *p)
{
    struct dup2_job *j = p;
    int64_t t0 = now_ms();
    errno = 0;
    j->rv = dup2(j->source, j->target);
    j->err = errno;
    j->took = now_ms() - t0;
    j->done = 1;
    return NULL;
}

static void sleep_case(enum sleep_kind kind, int keep_dup)
{
    for (int i = 3; i < 64; i++) close(i);
    int target = -1, other_end = -1, holder = -1, client = -1;
    struct sockaddr_in addr;
    int p[2];
    seed_file("f");
    int source = open("f", O_RDONLY);
    switch (kind) {
    case S_READ:
    case S_POLL:
        pipe(p);
        target = p[0];
        other_end = p[1];
        break;
    case S_WRITE:
        pipe(p);
        target = p[1];
        other_end = p[0];
        fill(target);
        break;
    case S_ACCEPT: target = listener_on(&addr); break;
    case S_FLOCK:
        holder = open("f", O_RDONLY);
        flock(holder, LOCK_EX);
        target = open("f", O_RDONLY);
        break;
    }
    int dupfd = keep_dup ? dup(target) : -1;
    struct sleeper s = {0};
    s.kind = kind;
    s.fd = target;
    pthread_create(&s.thread, NULL, sleeper_main, &s);
    sleep_ms(50);
    struct dup2_job j = {source, target, 0, 0, 0, 0};
    pthread_t jt;
    pthread_create(&jt, NULL, dup2_main, &j);
    sleep_ms(100);
    char label[64];
    snprintf(label, sizeof label, "%s%s", sleep_names[kind], keep_dup ? ", dup kept" : "");
    if (j.done) printf("SLEEP\t%s\tdup2 %s in %lld ms", label, j.rv < 0 ? en(j.err) : "ok", (long long)j.took);
    else printf("SLEEP\t%s\tdup2 still blocked at 100 ms", label);
    if (s.done) {
        if (kind == S_POLL) printf("\tsleeper %s revents 0x%x\n", s.rv < 0 ? en(s.err) : "ok", (unsigned)s.revents);
        else printf("\tsleeper %s\n", s.rv < 0 ? en(s.err) : num(s.rv));
    } else {
        printf("\tsleeper asleep\n");
    }
    // Release everything, then wake whatever is still asleep.
    if (holder >= 0) flock(holder, LOCK_UN);
    if (client >= 0) close(client);
    if (!s.done) pthread_kill(s.thread, SIGUSR1);
    sleep_ms(20);
    if (!s.done && other_end >= 0) {
        if (kind == S_WRITE) {
            char drain[65536];
            read(other_end, drain, sizeof drain);
        } else {
            write(other_end, "z", 1);
        }
    }
    if (!s.done && kind == S_ACCEPT) client = connect_to(&addr);
    sleep_ms(50);
    if (!s.done) printf("SLEEP\t%s\tsleeper still asleep after SIGUSR1 and a wake\n", label);
    else if (kind != S_POLL) printf("SLEEP\t%s\tafterwards the sleeper answered %s\n", label, s.rv < 0 ? en(s.err) : num(s.rv));
    pthread_join(jt, NULL);
    if (s.done) pthread_join(s.thread, NULL);
    else pthread_detach(s.thread);
    for (int i = 3; i < 64; i++) close(i);
    (void)dupfd;
}

static void section_sleep(void)
{
    for (int k = S_READ; k <= S_POLL; k++) {
        sleep_case((enum sleep_kind)k, 0);
        sleep_case((enum sleep_kind)k, 1);
    }
}

int main(int argc, char **argv)
{
    alarm(120);
    setvbuf(stdout, NULL, _IONBF, 0);
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sigaction(SIGUSR1, &sa, NULL);
    signal(SIGPIPE, SIG_IGN);
    char host[256] = "";
    FILE *u = popen("uname -srm", "r");
    if (u) {
        if (!fgets(host, sizeof host, u)) host[0] = 0;
        pclose(u);
        host[strcspn(host, "\n")] = 0;
    }
    printf("KERNEL\t%s\tuid=%d\n", host, (int)getuid());
    printf("HEADER\tO_NONBLOCK 0x%x\tO_APPEND 0x%x\tO_SYNC 0x%x\tO_DSYNC 0x%x\tO_ASYNC 0x%x\tO_CLOEXEC 0x%x\tF_DUPFD_CLOEXEC %d\n",
           O_NONBLOCK, O_APPEND, O_SYNC, O_DSYNC, O_ASYNC, O_CLOEXEC, F_DUPFD_CLOEXEC);
    const char *only = argc > 1 ? argv[1] : "KSWXFCLDOZ";
    if (strchr(only, 'K')) section_kind();
    if (strchr(only, 'S')) section_setfl();
    if (strchr(only, 'W')) section_written();
    if (strchr(only, 'X')) section_written_more();
    if (strchr(only, 'F')) section_setfd();
    if (strchr(only, 'C')) section_cloexec();
    if (strchr(only, 'L')) section_limit();
    if (strchr(only, 'D')) section_dup2();
    if (strchr(only, 'O')) section_onto();
    if (strchr(only, 'Z')) section_sleep();
    return 0;
}
