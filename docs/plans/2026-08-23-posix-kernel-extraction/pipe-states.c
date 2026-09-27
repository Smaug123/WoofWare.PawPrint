// Copied from the stdio design's probes (2026-09-26), where it was run on
// Linux 6.18.5 aarch64 (Apple `container`, gcc:14, root) and Darwin 27.0.0
// arm64. The rows the kernel library states for a pipe that came from here:
//
//   poll, both ends open   Linux: read end IN|RDNORM iff it holds bytes; write
//                          end OUT|WRNORM iff a slot is free. No PRI, RDBAND or
//                          WRBAND.
//   poll, other end gone   Linux: read end HUP (+IN|RDNORM with data); write
//                          end ERR (+OUT|WRNORM with a free slot), reported for
//                          a request of 0 too.
//   FIONREAD               Linux: the bytes held, on both ends. Darwin: the
//                          bytes held on the read end, 0 on the write end.
//   st_size                Linux 0. Darwin the bytes held, on both ends, and 0
//                          on the write end once the reader has closed.
//   reads                  a read takes what is held up to its count without
//                          waiting for more; writes do not keep boundaries; a
//                          non-blocking read of an empty pipe with a writer is
//                          EAGAIN, NULL buffer included; with no writer it is 0;
//                          a bad pointer with bytes held is EFAULT and the bytes
//                          stay.
//   writes                 write(0) is 0; a bad pointer into an empty pipe is
//                          EFAULT, into a full non-blocking one EAGAIN.
//   fstat                  Linux S_IFIFO|0600, nlink 1, one inode for both ends,
//                          device 0xc, nothing moves. Darwin S_IFIFO|0660,
//                          nlink 0, an inode per end, device 0.
//   isatty, tcgetattr      ENOTTY on either end, on both flavours.
//   lseek                  ESPIPE on either end for whence 0-4; whence 99 is
//                          EINVAL on Linux and ESPIPE on Darwin.
//
// Build: `nix develop -c clang -O0 -pthread -o /tmp/p <this file>` on Darwin,
// `gcc -O0 -pthread -o /tmp/p <this file> -lutil` on Linux.
// Each end of a pipe in each state: what poll reports, what read/write/lseek/
// fstat/isatty/tcgetattr/ioctl answer. States: empty, part full, full (by
// non-blocking fill), other end closed (empty and with data).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#ifndef O_NOTIFICATION_PIPE
#define O_NOTIFICATION_PIPE O_EXCL
#endif

static char buf[1 << 20];

#ifdef POLLRDHUP
#define ASK (POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND | POLLRDHUP)
#else
#define ASK (POLLIN | POLLPRI | POLLOUT | POLLRDNORM | POLLRDBAND | POLLWRNORM | POLLWRBAND)
#endif

static void nb(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

static int pollof(int fd, short events) {
    struct pollfd p = {fd, events, 0};
    int n = poll(&p, 1, 0);
    return n < 0 ? -errno : p.revents;
}

static void describe(const char *state, const char *end, int fd) {
    int avail = -1;
    int fi = ioctl(fd, FIONREAD, &avail);
    struct stat st;
    fstat(fd, &st);
    printf("%-26s %-5s poll(ASK)=%#06x poll(0)=%#x poll(IN)=%#x poll(OUT)=%#x FIONREAD=%d(%d) st_size=%lld\n", state, end,
           pollof(fd, ASK), pollof(fd, 0), pollof(fd, POLLIN), pollof(fd, POLLOUT), fi < 0 ? -errno : avail, fi,
           (long long)st.st_size);
}

static void fill(int w) {
    int fl = fcntl(w, F_GETFL);
    fcntl(w, F_SETFL, fl | O_NONBLOCK);
    for (int i = 0; i < 100000; i++)
        if (write(w, buf, 4096) < 0) break;
    fcntl(w, F_SETFL, fl);
}

static void stat_line(const char *label, int fd) {
    struct stat st;
    if (fstat(fd, &st) != 0) { printf("fstat %s -> errno %d\n", label, errno); return; }
    printf("fstat %-12s mode=%#o nlink=%ld uid=%d gid=%d size=%lld blksize=%ld blocks=%lld dev=%#llx ino=%llu rdev=%#llx "
           "atime=%ld.%09ld mtime=%ld.%09ld ctime=%ld.%09ld\n",
           label, (unsigned)st.st_mode, (long)st.st_nlink, (int)st.st_uid, (int)st.st_gid, (long long)st.st_size,
           (long)st.st_blksize, (long long)st.st_blocks, (unsigned long long)st.st_dev, (unsigned long long)st.st_ino,
           (unsigned long long)st.st_rdev,
#ifdef __APPLE__
           (long)st.st_atimespec.tv_sec, st.st_atimespec.tv_nsec, (long)st.st_mtimespec.tv_sec, st.st_mtimespec.tv_nsec,
           (long)st.st_ctimespec.tv_sec, st.st_ctimespec.tv_nsec
#else
           (long)st.st_atim.tv_sec, st.st_atim.tv_nsec, (long)st.st_mtim.tv_sec, st.st_mtim.tv_nsec,
           (long)st.st_ctim.tv_sec, st.st_ctim.tv_nsec
#endif
    );
}

static void *late_writer(void *arg) {
    int w = *(int *)arg;
    usleep(200 * 1000);
    write(w, "abc", 3);
    return NULL;
}

static void *late_closer(void *arg) {
    int w = *(int *)arg;
    usleep(200 * 1000);
    close(w);
    return NULL;
}

static double now(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec + t.tv_nsec / 1e9;
}

static void on_alarm(int s) { (void)s; }

int main(void) {
    alarm(60);
    setvbuf(stdout, NULL, _IOLBF, 0);
    memset(buf, 'x', sizeof buf);
    int p[2];

    printf("## poll and FIONREAD per state (ASK=%#x)\n", ASK);
    pipe(p);
    describe("empty, both open", "read", p[0]);
    describe("empty, both open", "write", p[1]);
    write(p[1], buf, 100);
    describe("100 bytes, both open", "read", p[0]);
    describe("100 bytes, both open", "write", p[1]);
    fill(p[1]);
    describe("full, both open", "read", p[0]);
    describe("full, both open", "write", p[1]);
    read(p[0], buf, 1);
    describe("full less 1 byte", "read", p[0]);
    describe("full less 1 byte", "write", p[1]);
    read(p[0], buf, 4095);
    describe("full less 4096 bytes", "read", p[0]);
    describe("full less 4096 bytes", "write", p[1]);
    close(p[0]);
    describe("data, reader closed", "write", p[1]);
    close(p[1]);

    pipe(p);
    close(p[0]);
    describe("empty, reader closed", "write", p[1]);
    close(p[1]);

    pipe(p);
    close(p[1]);
    describe("empty, writer closed", "read", p[0]);
    close(p[0]);

    pipe(p);
    write(p[1], buf, 100);
    close(p[1]);
    describe("100 bytes, writer closed", "read", p[0]);
    close(p[0]);

    // Darwin's buffer grows: POLLOUT near 16 KiB before and after growth.
    pipe(p);
    write(p[1], buf, 16000);
    describe("16000 written at once", "write", p[1]);
    write(p[1], buf, 1000);
    describe("then 1000 more (17000)", "write", p[1]);
    read(p[0], buf, 17000);
    describe("then drained to 0", "write", p[1]);
    write(p[1], buf, 16000);
    describe("then 16000 again", "write", p[1]);
    close(p[0]); close(p[1]);

    printf("## reads\n");
    pipe(p);
    nb(p[0]);
    ssize_t r = read(p[0], buf, 10);
    printf("nonblocking read, empty, writer open -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = read(p[0], buf, 0);
    printf("nonblocking read(0), empty, writer open -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = read(p[0], NULL, 0);
    printf("nonblocking read(NULL, 0), empty, writer open -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = read(p[0], NULL, 10);
    printf("nonblocking read(NULL, 10), empty, writer open -> %zd errno %d\n", r, r < 0 ? errno : 0);
    write(p[1], "abcde", 5);
    r = read(p[0], (void *)8, 10);
    printf("nonblocking read(bad ptr, 10), 5 bytes held -> %zd errno %d; FIONREAD after ", r, r < 0 ? errno : 0);
    { int a = -1; ioctl(p[0], FIONREAD, &a); printf("%d\n", a); }
    write(p[1], "fghij", 5);
    write(p[1], "klmno", 5);
    r = read(p[0], buf, 100);
    printf("read(100) after writes of 5,5,5 -> %zd (writes coalesce: %.*s)\n", r, (int)(r > 0 ? r : 0), buf);
    write(p[1], "0123456789", 10);
    r = read(p[0], buf, 3);
    printf("read(3) of 10 held -> %zd; ", r);
    r = read(p[0], buf, 100);
    printf("then read(100) -> %zd\n", r);
    close(p[0]); close(p[1]);

    pipe(p);
    close(p[1]);
    r = read(p[0], buf, 10);
    printf("blocking read, empty, writer closed -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = read(p[0], NULL, 10);
    printf("blocking read(NULL, 10), empty, writer closed -> %zd errno %d\n", r, r < 0 ? errno : 0);
    close(p[0]);

    pipe(p);
    {
        pthread_t t;
        pthread_create(&t, NULL, late_writer, &p[1]);
        double t0 = now();
        r = read(p[0], buf, 100);
        printf("blocking read, empty, writer open, 3 bytes arrive after 200ms -> %zd after %.0f ms\n", r, (now() - t0) * 1000);
        pthread_join(t, NULL);
    }
    {
        pthread_t t;
        pthread_create(&t, NULL, late_closer, &p[1]);
        double t0 = now();
        r = read(p[0], buf, 100);
        printf("blocking read, empty, writer closed after 200ms -> %zd after %.0f ms\n", r, (now() - t0) * 1000);
        pthread_join(t, NULL);
    }
    close(p[0]);

    // A signal interrupting a blocked pipe read: with and without SA_RESTART.
    for (int restart = 0; restart <= 1; restart++) {
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_alarm;
        sa.sa_flags = restart ? SA_RESTART : 0;
        sigaction(SIGALRM, &sa, NULL);
        pipe(p);
        struct itimerval it = {{0, 0}, {0, 100 * 1000}};
        struct itimerval off = {{0, 0}, {0, 0}};
        pthread_t t;
        if (restart) pthread_create(&t, NULL, late_writer, &p[1]); // restarted read needs a wake
        setitimer(ITIMER_REAL, &it, NULL);
        double t0 = now();
        r = read(p[0], buf, 100);
        int e = r < 0 ? errno : 0;
        setitimer(ITIMER_REAL, &off, NULL);
        printf("blocking read interrupted by SIGALRM, SA_RESTART=%d -> %zd errno %d after %.0f ms\n", restart, r, e, (now() - t0) * 1000);
        if (restart) pthread_join(t, NULL);
        close(p[0]); close(p[1]);
    }
    signal(SIGALRM, SIG_DFL);
    alarm(60);

    printf("## writes\n");
    pipe(p);
    nb(p[1]);
    r = write(p[1], buf, 0);
    printf("write(0), empty -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = write(p[1], NULL, 0);
    printf("write(NULL, 0) -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = write(p[1], (void *)8, 10);
    printf("write(bad ptr, 10), empty -> %zd errno %d\n", r, r < 0 ? errno : 0);
    fill(p[1]);
    r = write(p[1], buf, 0);
    printf("write(0), full, nonblocking -> %zd errno %d\n", r, r < 0 ? errno : 0);
    r = write(p[1], (void *)8, 10);
    printf("write(bad ptr, 10), full, nonblocking -> %zd errno %d\n", r, r < 0 ? errno : 0);
    close(p[0]); close(p[1]);

    printf("## fstat\n");
    pipe(p);
    stat_line("read end", p[0]);
    stat_line("write end", p[1]);
    int q[2];
    pipe(q);
    stat_line("2nd pipe r", q[0]);
    stat_line("2nd pipe w", q[1]);
    usleep(20 * 1000);
    write(p[1], "hello", 5);
    stat_line("r after write", p[0]);
    stat_line("w after write", p[1]);
    usleep(20 * 1000);
    read(p[0], buf, 2);
    stat_line("r after read", p[0]);
    stat_line("w after read", p[1]);
    close(q[0]); close(q[1]);

    printf("## terminal and seek questions on a pipe\n");
    for (int end = 0; end <= 1; end++) {
        int fd = p[end];
        errno = 0;
        int t = isatty(fd);
        printf("%s end: isatty=%d errno=%d; ", end ? "write" : "read", t, errno);
        struct termios tio;
        int g = tcgetattr(fd, &tio);
        printf("tcgetattr=%d errno=%d; ", g, g < 0 ? errno : 0);
        struct winsize ws;
        int w = ioctl(fd, TIOCGWINSZ, &ws);
        printf("TIOCGWINSZ=%d errno=%d; ", w, w < 0 ? errno : 0);
        printf("fpathconf(PIPE_BUF)=%ld\n", fpathconf(fd, _PC_PIPE_BUF));
        for (int whence = 0; whence <= 4; whence++) {
            off_t o = lseek(fd, 0, whence);
            printf("  lseek(0, %d) -> %lld errno %d\n", whence, (long long)o, o < 0 ? errno : 0);
        }
        off_t o = lseek(fd, -1, SEEK_SET);
        printf("  lseek(-1, SET) -> %lld errno %d\n", (long long)o, o < 0 ? errno : 0);
        o = lseek(fd, 0, 99);
        printf("  lseek(0, 99) -> %lld errno %d\n", (long long)o, o < 0 ? errno : 0);
        errno = 0;
        ssize_t pr = pread(fd, buf, 1, 0);
        printf("  pread -> %zd errno %d\n", pr, pr < 0 ? errno : 0);
        int fl = fcntl(fd, F_GETFL);
        printf("  F_GETFL -> %#x (accmode %d)\n", fl, fl & O_ACCMODE);
    }
    close(p[0]); close(p[1]);

    // Pipe fd numbering, and the flag set pipe2 accepts.
#ifdef __linux__
    {
        int flagsv[] = {0, O_CLOEXEC, O_NONBLOCK, O_DIRECT, O_NOTIFICATION_PIPE, 1, O_RDWR, O_APPEND, 0x7fffffff};
        const char *names[] = {"0", "O_CLOEXEC", "O_NONBLOCK", "O_DIRECT", "O_NOTIFICATION_PIPE", "1", "O_RDWR", "O_APPEND", "INT_MAX"};
        for (size_t k = 0; k < sizeof flagsv / sizeof *flagsv; k++) {
            int z[2] = {-1, -1};
            int rc = pipe2(z, flagsv[k]);
            printf("pipe2(%s=%#x) -> %d errno %d fds %d %d", names[k], flagsv[k], rc, rc < 0 ? errno : 0, z[0], z[1]);
            if (rc == 0) {
                printf(" F_GETFL %#x %#x F_GETFD %d %d", fcntl(z[0], F_GETFL), fcntl(z[1], F_GETFL), fcntl(z[0], F_GETFD), fcntl(z[1], F_GETFD));
                close(z[0]); close(z[1]);
            }
            printf("\n");
        }
        int rc = pipe2((int *)8, 0);
        printf("pipe2(bad ptr) -> %d errno %d\n", rc, rc < 0 ? errno : 0);
    }
#endif
    {
        // In a child: Darwin's libc stores the two fds itself, so a bad
        // pointer may fault in user space rather than answer EFAULT.
        pid_t c = fork();
        if (c == 0) {
            int rc = pipe((int *)8);
            printf("pipe(bad ptr) -> %d errno %d\n", rc, rc < 0 ? errno : 0);
            fflush(stdout);
            _exit(0);
        }
        int ws = 0;
        waitpid(c, &ws, 0);
        printf("pipe(bad ptr) child: exited=%d status=%d signalled=%d sig=%d\n", WIFEXITED(ws), WIFEXITED(ws) ? WEXITSTATUS(ws) : -1, WIFSIGNALED(ws), WIFSIGNALED(ws) ? WTERMSIG(ws) : 0);
        int z[2];
        pipe(z);
        printf("pipe fds after 0,1,2 + %d more: %d %d; ", 0, z[0], z[1]);
        close(z[0]);
        int y[2];
        pipe(y);
        printf("close read end, pipe again -> %d %d\n", y[0], y[1]);
        close(z[1]); close(y[0]); close(y[1]);
    }
    return 0;
}
