// What pipe2(2), and the calls a pipe answers without its buffer, say on each
// flavour: the pipe2 flag word bit by bit, pipe2's order against a bad
// pointer, the fstat fields a model has to mint, which calls move a Darwin
// pipe's timestamps, a zero-length read that could block, FIONREAD's
// screens, and isatty/tcgetattr on every descriptor kind the kernel models.
//
// Results, 2026-09-27, on Linux 6.18.5 aarch64 (Apple `container`, gcc:14,
// root) and Darwin 27.0.0 arm64 (uid 501):
//
//   pipe2, each of the 32 single flag bits
//     Linux   0x800 (O_NONBLOCK), 0x80000 (O_CLOEXEC) and 0x10000 (aarch64's
//             O_DIRECT; a packet pipe) make a pipe; 0x80 (O_NOTIFICATION_PIPE)
//             is ENOPKG in this build; every other bit is EINVAL, 0x4000 (which
//             aarch64 calls O_DIRECTORY) included. An x86-64 build under
//             Rosetta reaches the same aarch64 kernel, so it is no measurement
//             of x86-64.
//     Darwin  0x4 (O_NONBLOCK), 0x1000000 (O_CLOEXEC) and 0x8000000
//             (O_CLOFORK, F_GETFD 2) make a pipe; every other bit is EINVAL.
//   pipe2 through NULL or (int *)8
//     Linux   EINVAL for a bad flag word, else EFAULT; the next descriptor
//             allocated is 3 either way, so no pipe is left behind.
//     Darwin  EINVAL for a bad flag word, else the process dies of SIGSEGV:
//             the C library stores the descriptors itself.
//   fstat mode and owner under umask 0 and 0777
//     Linux 010600, Darwin 010660, whatever the umask; the owner is the
//     effective user and group (Linux: ruid 0, euid 1000, egid 2000 gives
//     1000 and 2000). Linux: one inode for both ends, device 0xc. Darwin: an
//     inode per end, device 0, birth time 0.
//   which calls move a pipe's timestamps
//     Linux   none: a read, a write, a short write, EAGAIN and EFAULT either
//             way, and a close all leave every timestamp of both ends alone.
//     Darwin  every read reaching the pipe moves the read end's atime (0
//             bytes, EAGAIN, EFAULT, end of file and a transfer alike); every
//             write reaching it moves mtime and ctime on both ends (0 bytes,
//             EAGAIN, EFAULT, short and whole alike). Nothing else moves.
//   a blocking read(fd, NULL, 0) of an empty pipe whose writer stays open,
//   and a blocking write(fd, buf, 0) into a full pipe
//     0 at once, on both.
//   FIONREAD
//     NULL or (int *)8 is EFAULT on either end, (int *)8 on a closed
//     descriptor EBADF, on both. The write end reports the bytes held on
//     Linux (reader closed or not) and 0 on Darwin.
//   isatty, tcgetattr and TIOCGWINSZ, which agree on every row
//     not open                             EBADF    EBADF
//     regular file, directory, pipe end    ENOTTY   ENOTTY
//     TCP or UDP socket, IPv4 or IPv6      ENOTTY   ENXIO
//     Unix-domain socket, either kind      ENOTTY   EOPNOTSUPP
//     epoll / kqueue                       EINVAL   ENOTTY
//
// Build: `nix develop -c clang -O0 -pthread -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -pthread -o /tmp/p <this file>` on Linux (gcc:14 image under
// `container run`, as root, so that the credentials section can change IDs).
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static int (*pipe2p)(int[2], int);

static void times_of(const char *label, int fd) {
    struct stat st;
    if (fstat(fd, &st) != 0) {
        printf("  %-28s fstat errno %d\n", label, errno);
        return;
    }
#ifdef __APPLE__
    printf("  %-28s size=%lld atime=%ld.%09ld mtime=%ld.%09ld ctime=%ld.%09ld birth=%ld.%09ld\n", label,
           (long long)st.st_size, (long)st.st_atimespec.tv_sec, st.st_atimespec.tv_nsec, (long)st.st_mtimespec.tv_sec,
           st.st_mtimespec.tv_nsec, (long)st.st_ctimespec.tv_sec, st.st_ctimespec.tv_nsec,
           (long)st.st_birthtimespec.tv_sec, st.st_birthtimespec.tv_nsec);
#else
    printf("  %-28s size=%lld atime=%ld.%09ld mtime=%ld.%09ld ctime=%ld.%09ld\n", label, (long long)st.st_size,
           (long)st.st_atim.tv_sec, st.st_atim.tv_nsec, (long)st.st_mtim.tv_sec, st.st_mtim.tv_nsec,
           (long)st.st_ctim.tv_sec, st.st_ctim.tv_nsec);
#endif
}

static void pause_ms(int ms) { usleep(ms * 1000); }

static void *close_later(void *arg) {
    pause_ms(300);
    close(*(int *)arg);
    return NULL;
}

static void terminal_row(const char *kind, int fd) {
    errno = 0;
    int t = isatty(fd);
    int te = errno;
    struct termios tio;
    errno = 0;
    int g = tcgetattr(fd, &tio);
    int ge = g < 0 ? errno : 0;
    struct winsize ws;
    int w = ioctl(fd, TIOCGWINSZ, &ws);
    int we = w < 0 ? errno : 0;
    int avail = -12345;
    int f = ioctl(fd, FIONREAD, &avail);
    int fe = f < 0 ? errno : 0;
    printf("  %-22s isatty=%d errno %d; tcgetattr=%d errno %d; TIOCGWINSZ=%d errno %d; FIONREAD=%d errno %d (%d)\n", kind,
           t, te, g, ge, w, we, f, fe, avail);
}

int main(void) {
    alarm(60);
    setvbuf(stdout, NULL, _IOLBF, 0);
#ifdef __linux__
    pipe2p = pipe2;
#else
    pipe2p = (int (*)(int[2], int))dlsym(RTLD_DEFAULT, "pipe2");
    if (!pipe2p) {
        printf("no pipe2 in this libc\n");
        return 1;
    }
#endif

    printf("## pipe2: each single flag bit\n");
    for (int bit = 0; bit < 32; bit++) {
        int flag = (int)(1u << bit);
        int z[2] = {-1, -1};
        int rc = pipe2p(z, flag);
        printf("  bit %2d (%#010x) -> %d errno %d", bit, (unsigned)flag, rc, rc < 0 ? errno : 0);
        if (rc == 0) {
            printf(" fds %d %d F_GETFL %#x %#x F_GETFD %d %d", z[0], z[1], fcntl(z[0], F_GETFL), fcntl(z[1], F_GETFL),
                   fcntl(z[0], F_GETFD), fcntl(z[1], F_GETFD));
            close(z[0]);
            close(z[1]);
        }
        printf("\n");
    }
    {
        int z[2] = {-1, -1};
        int rc = pipe2p(z, O_CLOEXEC | O_NONBLOCK);
        printf("  O_CLOEXEC|O_NONBLOCK -> %d errno %d", rc, rc < 0 ? errno : 0);
        if (rc == 0) {
            printf(" F_GETFL %#x %#x F_GETFD %d %d", fcntl(z[0], F_GETFL), fcntl(z[1], F_GETFL), fcntl(z[0], F_GETFD),
                   fcntl(z[1], F_GETFD));
            close(z[0]);
            close(z[1]);
        }
        printf("\n");
    }

    printf("## pipe2 against a bad pointer, in a child each (a libc that stores the fds itself faults)\n");
    {
        int flagsv[] = {0, O_NONBLOCK, 1, 0x7fffffff};
        const char *names[] = {"0", "O_NONBLOCK", "1 (invalid)", "INT_MAX (invalid)"};
        void *ptrs[] = {(void *)8, NULL};
        for (int pi = 0; pi < 2; pi++) {
            for (size_t k = 0; k < sizeof flagsv / sizeof *flagsv; k++) {
                fflush(stdout);
                pid_t c = fork();
                if (c == 0) {
                    int rc = pipe2p((int *)ptrs[pi], flagsv[k]);
                    int e = rc < 0 ? errno : 0;
                    // The lowest free descriptor afterwards: a pipe created and
                    // then abandoned by a failed copy-out would have taken 3.
                    int probe = dup(0);
                    printf("  pipe2(%s, %s) -> %d errno %d; next dup -> %d\n", pi ? "NULL" : "(int*)8", names[k], rc, e,
                           probe);
                    fflush(stdout);
                    _exit(0);
                }
                int ws = 0;
                waitpid(c, &ws, 0);
                if (WIFSIGNALED(ws))
                    printf("  pipe2(%s, %s) -> killed by signal %d\n", pi ? "NULL" : "(int*)8", names[k], WTERMSIG(ws));
            }
        }
    }

    printf("## fstat: mode under two umasks, and owner\n");
    {
        mode_t masks[] = {0, 0777};
        for (int m = 0; m < 2; m++) {
            umask(masks[m]);
            int z[2];
            pipe(z);
            struct stat r, w;
            fstat(z[0], &r);
            fstat(z[1], &w);
            printf("  umask %04o: read mode %#o uid %d gid %d; write mode %#o uid %d gid %d; same ino %d; dev %#llx %#llx\n",
                   (unsigned)masks[m], (unsigned)r.st_mode, (int)r.st_uid, (int)r.st_gid, (unsigned)w.st_mode,
                   (int)w.st_uid, (int)w.st_gid, r.st_ino == w.st_ino, (unsigned long long)r.st_dev,
                   (unsigned long long)w.st_dev);
            close(z[0]);
            close(z[1]);
        }
        umask(022);
#ifdef __linux__
        if (geteuid() == 0) {
            fflush(stdout);
            pid_t c = fork();
            if (c == 0) {
                // Real and saved IDs root, effective IDs not: whose IDs does the
                // new pipe carry?
                if (setresgid(0, 2000, 0) != 0 || setresuid(0, 1000, 0) != 0) {
                    printf("  setres*id failed: %d\n", errno);
                    _exit(1);
                }
                int z[2];
                pipe(z);
                struct stat r;
                fstat(z[0], &r);
                printf("  ruid 0, euid 1000, rgid 0, egid 2000: pipe uid %d gid %d\n", (int)r.st_uid, (int)r.st_gid);
                fflush(stdout);
                _exit(0);
            }
            int ws;
            waitpid(c, &ws, 0);
        }
#endif
    }

    printf("## which calls move a pipe's timestamps (20 ms apart)\n");
    {
        int z[2];
        pipe(z);
        fcntl(z[0], F_SETFL, O_NONBLOCK);
        fcntl(z[1], F_SETFL, O_NONBLOCK);
        char buf[70000];
        memset(buf, 'y', sizeof buf);
        times_of("read, fresh", z[0]);
        times_of("write, fresh", z[1]);
        pause_ms(20);
        ssize_t n = read(z[0], buf, 10);
        printf("  read(10) of empty -> %zd errno %d\n", n, n < 0 ? errno : 0);
        times_of("read, after EAGAIN read", z[0]);
        pause_ms(20);
        n = read(z[0], buf, 0);
        printf("  read(0) of empty -> %zd\n", n);
        times_of("read, after read(0)", z[0]);
        pause_ms(20);
        n = write(z[1], buf, 0);
        printf("  write(0) -> %zd\n", n);
        times_of("read, after write(0)", z[0]);
        times_of("write, after write(0)", z[1]);
        pause_ms(20);
        n = write(z[1], buf, 70000);
        printf("  write(70000), short -> %zd\n", n);
        times_of("read, after short write", z[0]);
        times_of("write, after short write", z[1]);
        pause_ms(20);
        n = write(z[1], buf, 10);
        printf("  write(10) into a full pipe -> %zd errno %d\n", n, n < 0 ? errno : 0);
        times_of("read, after EAGAIN write", z[0]);
        times_of("write, after EAGAIN write", z[1]);
        pause_ms(20);
        n = read(z[0], buf, 5);
        printf("  read(5) -> %zd\n", n);
        times_of("read, after read(5)", z[0]);
        times_of("write, after read(5)", z[1]);
        pause_ms(20);
        n = read(z[0], buf, 0);
        printf("  read(0) with data held -> %zd\n", n);
        times_of("read, after read(0) held", z[0]);
        close(z[0]);
        pause_ms(20);
        times_of("write, reader closed", z[1]);
        close(z[1]);

        pipe(z);
        write(z[1], "abc", 3);
        pause_ms(20);
        close(z[1]);
        times_of("read, writer closed", z[0]);
        pause_ms(20);
        n = read(z[0], buf, 10);
        printf("  read(10) of 3 held, writer closed -> %zd\n", n);
        times_of("read, after draining read", z[0]);
        pause_ms(20);
        n = read(z[0], buf, 10);
        printf("  read(10) at end of file -> %zd\n", n);
        times_of("read, after EOF read", z[0]);
        close(z[0]);
    }

    printf("## timestamps after a faulting read and a faulting write\n");
    {
        int z[2];
        pipe(z);
        fcntl(z[0], F_SETFL, O_NONBLOCK);
        fcntl(z[1], F_SETFL, O_NONBLOCK);
        write(z[1], "abcde", 5);
        pause_ms(20);
        times_of("read, before", z[0]);
        times_of("write, before", z[1]);
        pause_ms(20);
        ssize_t n = read(z[0], (void *)8, 5);
        printf("  read((void*)8, 5) of 5 held -> %zd errno %d\n", n, n < 0 ? errno : 0);
        times_of("read, after faulting read", z[0]);
        pause_ms(20);
        n = write(z[1], (void *)8, 5);
        printf("  write((void*)8, 5) -> %zd errno %d\n", n, n < 0 ? errno : 0);
        times_of("read, after faulting write", z[0]);
        times_of("write, after faulting write", z[1]);
        close(z[0]);
        close(z[1]);
    }

    printf("## a blocking read of zero bytes from an empty pipe whose writer is open\n");
    {
        int z[2];
        pipe(z);
        pthread_t t;
        pthread_create(&t, NULL, close_later, &z[1]);
        struct timespec a, b;
        clock_gettime(CLOCK_MONOTONIC, &a);
        ssize_t n = read(z[0], NULL, 0);
        clock_gettime(CLOCK_MONOTONIC, &b);
        printf("  read(fd, NULL, 0) -> %zd errno %d after %ld ms (writer closes at 300 ms)\n", n, n < 0 ? errno : 0,
               (long)((b.tv_sec - a.tv_sec) * 1000 + (b.tv_nsec - a.tv_nsec) / 1000000));
        pthread_join(t, NULL);
        close(z[0]);
    }

    printf("## a blocking write of zero bytes into a full pipe\n");
    {
        int z[2];
        pipe(z);
        char big[4096];
        memset(big, 'z', sizeof big);
        fcntl(z[1], F_SETFL, O_NONBLOCK);
        while (write(z[1], big, sizeof big) > 0) {
        }
        while (write(z[1], big, 1) > 0) {
        }
        fcntl(z[1], F_SETFL, 0);
        struct timespec a, b;
        clock_gettime(CLOCK_MONOTONIC, &a);
        ssize_t n = write(z[1], big, 0);
        clock_gettime(CLOCK_MONOTONIC, &b);
        printf("  write(fd, buf, 0), blocking, full -> %zd errno %d after %ld ms\n", n, n < 0 ? errno : 0,
               (long)((b.tv_sec - a.tv_sec) * 1000 + (b.tv_nsec - a.tv_nsec) / 1000000));
        close(z[0]);
        close(z[1]);
    }

    printf("## FIONREAD screens\n");
    {
        int z[2];
        pipe(z);
        write(z[1], "hello", 5);
        int avail = -1;
        int r = ioctl(z[0], FIONREAD, NULL);
        printf("  read end, NULL -> %d errno %d\n", r, r < 0 ? errno : 0);
        r = ioctl(z[0], FIONREAD, (int *)8);
        printf("  read end, (int*)8 -> %d errno %d\n", r, r < 0 ? errno : 0);
        r = ioctl(z[1], FIONREAD, (int *)8);
        printf("  write end, (int*)8 -> %d errno %d\n", r, r < 0 ? errno : 0);
        r = ioctl(999, FIONREAD, (int *)8);
        printf("  fd 999, (int*)8 -> %d errno %d\n", r, r < 0 ? errno : 0);
        r = ioctl(z[1], FIONREAD, &avail);
        printf("  write end, 5 held -> %d (%d)\n", r, avail);
        close(z[0]);
        r = ioctl(z[1], FIONREAD, &avail);
        printf("  write end, 5 held, reader closed -> %d (%d)\n", r, avail);
        close(z[1]);
    }

    printf("## isatty, tcgetattr, TIOCGWINSZ and FIONREAD per descriptor kind\n");
    {
        char path[] = "/tmp/pipe-syscalls-XXXXXX";
        int f = mkstemp(path);
        write(f, "0123456789", 10);
        lseek(f, 3, SEEK_SET);
        terminal_row("regular file", f);
        close(f);
        unlink(path);
        int d = open("/tmp", O_RDONLY | O_DIRECTORY);
        terminal_row("directory", d);
        close(d);
        int z[2];
        pipe(z);
        terminal_row("pipe read end", z[0]);
        terminal_row("pipe write end", z[1]);
        close(z[0]);
        close(z[1]);
        int s = socket(AF_INET, SOCK_STREAM, 0);
        terminal_row("TCP socket", s);
        close(s);
        s = socket(AF_INET, SOCK_DGRAM, 0);
        terminal_row("UDP socket", s);
        close(s);
        s = socket(AF_INET6, SOCK_STREAM, 0);
        terminal_row("TCP6 socket", s);
        close(s);
        s = socket(AF_INET6, SOCK_DGRAM, 0);
        terminal_row("UDP6 socket", s);
        close(s);
        s = socket(AF_UNIX, SOCK_STREAM, 0);
        terminal_row("Unix stream socket", s);
        close(s);
        s = socket(AF_UNIX, SOCK_DGRAM, 0);
        terminal_row("Unix datagram socket", s);
        close(s);
#ifdef __linux__
        int e = epoll_create1(0);
        terminal_row("epoll", e);
#else
        int e = kqueue();
        terminal_row("kqueue", e);
#endif
        close(e);
        terminal_row("closed fd 999", 999);
    }
    return 0;
}
