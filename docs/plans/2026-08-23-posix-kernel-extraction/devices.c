// What a process sees of /dev/null, /dev/zero, /dev/random and /dev/urandom:
// their stat fields, /dev's statfs, the namespace rules at /dev's boundary, and
// each device's answer to open, read, write, lseek, pread/pwrite, fstat (and
// whether reads or writes move a timestamp), fcntl, O_NONBLOCK, ftruncate,
// poll, epoll or kqueue, flock, fadvise, fsync, FIONREAD and close.
//
// Built and run natively on Darwin as uid 501, and in Apple's `container` on
// Linux as root and (after setpriv) as uid 1000.
//
// Usage: devices [section...] with sections stat statfs ns ops times list.
// No arguments runs everything except `times`, which sleeps.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <time.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#if defined(__linux__)
#include <sys/epoll.h>
#include <sys/statfs.h>
#include <sys/sysmacros.h>
#define ATIM(s) ((s).st_atim)
#define MTIM(s) ((s).st_mtim)
#define CTIM(s) ((s).st_ctim)
#else
#include <sys/event.h>
#define ATIM(s) ((s).st_atimespec)
#define MTIM(s) ((s).st_mtimespec)
#define CTIM(s) ((s).st_ctimespec)
#endif

static const char *devs[] = {"/dev/null", "/dev/zero", "/dev/random", "/dev/urandom"};
#define NDEVS 4

static const char *en(int e) {
    switch (e) {
    case 0: return "0";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case EBADF: return "EBADF";
    case EACCES: return "EACCES";
    case EFAULT: return "EFAULT";
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
    case EOVERFLOW: return "EOVERFLOW";
    case ENOTSUP: return "ENOTSUP";
#if defined(EOPNOTSUPP) && EOPNOTSUPP != ENOTSUP
    case EOPNOTSUPP: return "EOPNOTSUPP";
#endif
    case EINTR: return "EINTR";
    case ENOSYS: return "ENOSYS";
    default: {
        static char b[32];
        snprintf(b, sizeof b, "errno%d", e);
        return b;
    }
    }
}

// "r=<value>" or "-1 <errno>", in a static buffer per call slot.
static char rbuf[8][64];
static int rslot;
static const char *res(long long r) {
    char *b = rbuf[rslot++ & 7];
    if (r < 0)
        snprintf(b, 64, "-1 %s", en(errno));
    else
        snprintf(b, 64, "%lld", r);
    return b;
}

static void ts(const char *label, struct timespec t) { printf(" %s=%lld.%09ld", label, (long long)t.tv_sec, t.tv_nsec); }

static void print_stat(const char *how, const char *path, const struct stat *s) {
    printf("STAT %s %s dev=%u,%u(raw %llu) ino=%llu mode=0%o nlink=%llu uid=%u gid=%u rdev=%u,%u(raw %llu) size=%lld blksize=%ld blocks=%lld",
           how, path, (unsigned)major(s->st_dev), (unsigned)minor(s->st_dev), (unsigned long long)s->st_dev,
           (unsigned long long)s->st_ino, (unsigned)s->st_mode, (unsigned long long)s->st_nlink, (unsigned)s->st_uid,
           (unsigned)s->st_gid, (unsigned)major(s->st_rdev), (unsigned)minor(s->st_rdev),
           (unsigned long long)s->st_rdev, (long long)s->st_size, (long)s->st_blksize, (long long)s->st_blocks);
    ts("atime", ATIM(*s));
    ts("mtime", MTIM(*s));
    ts("ctime", CTIM(*s));
#if defined(__APPLE__)
    ts("birth", s->st_birthtimespec);
    printf(" flags=0x%x gen=%u", s->st_flags, s->st_gen);
#endif
    printf("\n");
}

static void sec_stat(void) {
    struct timespec now;
    clock_gettime(CLOCK_REALTIME, &now);
    printf("NOW %lld.%09ld\n", (long long)now.tv_sec, now.tv_nsec);
    const char *paths[] = {"/", "/tmp", "/dev", "/dev/null", "/dev/zero", "/dev/random", "/dev/urandom"};
    for (int i = 0; i < 7; i++) {
        struct stat s;
        if (lstat(paths[i], &s) == 0)
            print_stat("lstat", paths[i], &s);
        else
            printf("STAT lstat %s %s\n", paths[i], res(-1));
        int fd = open(paths[i], O_RDONLY);
        if (fd >= 0) {
            if (fstat(fd, &s) == 0) print_stat("fstat", paths[i], &s);
            close(fd);
        }
    }
}

static void sec_statfs(void) {
    const char *paths[] = {"/", "/tmp", "/dev", "/dev/null", "/dev/urandom"};
    for (int i = 0; i < 5; i++) {
        struct statfs f;
        if (statfs(paths[i], &f) != 0) {
            printf("STATFS %s %s\n", paths[i], res(-1));
            continue;
        }
#if defined(__linux__)
        printf("STATFS %s type=0x%lx bsize=%ld blocks=%llu bfree=%llu bavail=%llu files=%llu ffree=%llu fsid=%08x:%08x namelen=%ld frsize=%ld flags=0x%lx\n",
               paths[i], (long)f.f_type, (long)f.f_bsize, (unsigned long long)f.f_blocks, (unsigned long long)f.f_bfree,
               (unsigned long long)f.f_bavail, (unsigned long long)f.f_files, (unsigned long long)f.f_ffree,
               (unsigned)f.f_fsid.__val[0], (unsigned)f.f_fsid.__val[1], (long)f.f_namelen, (long)f.f_frsize,
               (long)f.f_flags);
#else
        printf("STATFS %s type=%u subtype=%u bsize=%u iosize=%d blocks=%llu bfree=%llu bavail=%llu files=%llu ffree=%llu fsid=%08x:%08x owner=%u flags=0x%x fstypename=%s on=%s from=%s\n",
               paths[i], f.f_type, f.f_fssubtype, f.f_bsize, f.f_iosize, (unsigned long long)f.f_blocks,
               (unsigned long long)f.f_bfree, (unsigned long long)f.f_bavail, (unsigned long long)f.f_files,
               (unsigned long long)f.f_ffree, (unsigned)f.f_fsid.val[0], (unsigned)f.f_fsid.val[1], f.f_owner,
               f.f_flags, f.f_fstypename, f.f_mntonname, f.f_mntfromname);
#endif
    }
}

// The namespace at /dev's boundary. Nothing here touches an existing device
// node: every source or destination inside /dev is a fresh name.
static void sec_ns(void) {
    char dir[] = "/tmp/devprobeXXXXXX";
    if (!mkdtemp(dir)) { perror("mkdtemp"); return; }
    char f[256], f2[256], d[256];
    snprintf(f, sizeof f, "%s/file", dir);
    snprintf(f2, sizeof f2, "%s/file2", dir);
    snprintf(d, sizeof d, "%s/sub", dir);
    int fd = open(f, O_CREAT | O_WRONLY, 0644);
    close(fd);
    mkdir(d, 0755);
    printf("NS euid=%u\n", (unsigned)geteuid());
    printf("NS rename(tmpfile, /dev/pawprobe) %s\n", res(rename(f, "/dev/pawprobe")));
    unlink("/dev/pawprobe");
    printf("NS rename(tmpdir, /dev/pawprobedir) %s\n", res(rename(d, "/dev/pawprobedir")));
    rmdir("/dev/pawprobedir");
    printf("NS link(tmpfile, /dev/pawprobe) %s\n", res(link(f, "/dev/pawprobe")));
    unlink("/dev/pawprobe");
    printf("NS rename(/dev/null, tmp/new) %s\n", res(rename("/dev/null", f2)));
    printf("NS rename(/dev/null, tmpfile-existing) %s\n", res(rename("/dev/null", f)));
    printf("NS link(/dev/null, tmp/new) %s\n", res(link("/dev/null", f2)));
    unlink(f2);
    printf("NS rename(/dev/nonexistent, tmp/new) %s\n", res(rename("/dev/pawnonexistent", f2)));
    printf("NS rename(tmp/nonexistent, /dev/new) %s\n", res(rename("/tmp/pawnonexistent", "/dev/pawprobe")));
    printf("NS rename(tmpfile, /dev/null) %s\n", res(rename(f, "/dev/null")));
    printf("NS symlink(x, /dev/pawprobe) %s\n", res(symlink("x", "/dev/pawprobe")));
    unlink("/dev/pawprobe");
    int c = open("/dev/pawprobe", O_CREAT | O_EXCL | O_WRONLY, 0644);
    printf("NS open(/dev/pawprobe, O_CREAT|O_EXCL) %s\n", res(c));
    if (c >= 0) {
        struct stat s;
        fstat(c, &s);
        print_stat("fstat", "/dev/pawprobe", &s);
        printf("NS write(new /dev file) %s\n", res(write(c, "hello", 5)));
        close(c);
        printf("NS unlink(/dev/pawprobe) %s\n", res(unlink("/dev/pawprobe")));
    }
    printf("NS mkdir(/dev/pawprobedir) %s\n", res(mkdir("/dev/pawprobedir", 0755)));
    rmdir("/dev/pawprobedir");
    printf("NS mknod(/dev/pawprobenod, S_IFCHR 1,3) %s\n", res(mknod("/dev/pawprobenod", S_IFCHR | 0666, makedev(1, 3))));
    unlink("/dev/pawprobenod");
    printf("NS mknod(tmp/nod, S_IFCHR 1,3) %s\n", res(mknod(f2, S_IFCHR | 0666, makedev(1, 3))));
    unlink(f2);
    printf("NS unlink(/dev/pawnonexistent) %s\n", res(unlink("/dev/pawnonexistent")));
    printf("NS open(/dev/null, O_CREAT|O_EXCL) %s\n", res(open("/dev/null", O_CREAT | O_EXCL | O_WRONLY, 0644)));
    printf("NS open(/dev/null, O_DIRECTORY) %s\n", res(open("/dev/null", O_RDONLY | O_DIRECTORY)));
    printf("NS open(/dev/null/x) %s\n", res(open("/dev/null/x", O_RDONLY)));
    printf("NS access(/dev/urandom, R_OK|W_OK) %s\n", res(access("/dev/urandom", R_OK | W_OK)));
    printf("NS access(/dev/urandom, X_OK) %s\n", res(access("/dev/urandom", X_OK)));
    printf("NS chmod(/dev/null, 0666) %s\n", res(geteuid() == 0 ? 0 : chmod("/dev/null", 0666)));
    unlink(f);
    rmdir(d);
    rmdir(dir);
}

static void sec_list(void) {
    DIR *d = opendir("/dev");
    if (!d) { printf("LIST opendir %s\n", res(-1)); return; }
    struct dirent *e;
    int n = 0, i = 0;
    printf("LIST order:");
    while ((e = readdir(d)) != NULL) {
        n++;
        if (i < 12 || !strcmp(e->d_name, "null") || !strcmp(e->d_name, "zero") || !strcmp(e->d_name, "random") ||
            !strcmp(e->d_name, "urandom"))
            printf(" %d:%s(t%d,i%llu)", i, e->d_name, e->d_type, (unsigned long long)e->d_ino);
        i++;
    }
    printf("\nLIST total=%d\n", n);
    closedir(d);
}

static int allzero(const unsigned char *b, ssize_t n) {
    for (ssize_t i = 0; i < n; i++)
        if (b[i]) return 0;
    return 1;
}

static void dev_ops(const char *path) {
    static unsigned char buf[(1 << 20) + 64];
    printf("== %s\n", path);
    int modes[] = {O_RDONLY, O_WRONLY, O_RDWR};
    const char *mn[] = {"O_RDONLY", "O_WRONLY", "O_RDWR"};
    for (int m = 0; m < 3; m++) {
        int fd = open(path, modes[m] | O_CLOEXEC);
        printf("OPEN %s %s %s", path, mn[m], res(fd));
        if (fd >= 0) printf(" getfl=0x%x getfd=%d", fcntl(fd, F_GETFL), fcntl(fd, F_GETFD));
        printf("\n");
        if (fd >= 0) close(fd);
    }
    {
        int fd = open(path, O_WRONLY | O_TRUNC);
        printf("OPEN %s O_WRONLY|O_TRUNC %s\n", path, res(fd));
        if (fd >= 0) close(fd);
        fd = open(path, O_RDONLY | O_TRUNC);
        printf("OPEN %s O_RDONLY|O_TRUNC %s\n", path, res(fd));
        if (fd >= 0) close(fd);
        fd = open(path, O_WRONLY | O_APPEND);
        printf("OPEN %s O_WRONLY|O_APPEND %s\n", path, res(fd));
        if (fd >= 0) {
            printf("WRITE-APPEND %s 3 %s\n", path, res(write(fd, "abc", 3)));
            close(fd);
        }
        fd = open(path, O_RDONLY | O_CREAT, 0600);
        printf("OPEN %s O_RDONLY|O_CREAT %s\n", path, res(fd));
        if (fd >= 0) close(fd);
        fd = open(path, O_RDONLY | O_NOFOLLOW);
        printf("OPEN %s O_RDONLY|O_NOFOLLOW %s\n", path, res(fd));
        if (fd >= 0) close(fd);
    }
    int fd = open(path, O_RDWR);
    int fdr = open(path, O_RDONLY);
    if (fd < 0) fd = fdr;
    // reads
    size_t sizes[] = {0, 1, 7, 8, 9, 31, 32, 33, 64, 255, 256, 257, 512, 513, 4095, 4096, 4097, 65536, 1 << 20};
    for (unsigned i = 0; i < sizeof sizes / sizeof *sizes; i++) {
        memset(buf, 0xAA, sizes[i] + 1);
        ssize_t r = read(fdr, buf, sizes[i]);
        printf("READ %s %zu %s allzero=%d untouched_after=%d\n", path, sizes[i], res(r), r > 0 ? allzero(buf, r) : -1,
               buf[sizes[i]] == 0xAA);
    }
    printf("READ %s NULL,0 %s\n", path, res(read(fdr, NULL, 0)));
    printf("READ %s NULL,1 %s\n", path, res(read(fdr, NULL, 1)));
    printf("READ %s NULL,100 %s\n", path, res(read(fdr, NULL, 100)));
    // Partially mapped buffer: one page mapped, the next not.
    long pg = sysconf(_SC_PAGESIZE);
    unsigned char *two = mmap(NULL, 2 * pg, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    munmap(two + pg, pg);
    printf("READ %s partial(pg=%ld, ask 2pg) %s\n", path, pg, res(read(fdr, two, 2 * pg)));
    printf("READ %s partial(ask 2pg from pg-100) %s\n", path, res(read(fdr, two + pg - 100, 2 * pg)));
    printf("READ %s unmapped(ask 16) %s\n", path, res(read(fdr, two + pg, 16)));
    // writes
    size_t wsizes[] = {0, 1, 255, 256, 257, 4096, 65536, 1 << 20};
    for (unsigned i = 0; i < sizeof wsizes / sizeof *wsizes; i++)
        printf("WRITE %s %zu %s\n", path, wsizes[i], res(write(fd, buf, wsizes[i])));
    printf("WRITE %s NULL,0 %s\n", path, res(write(fd, NULL, 0)));
    printf("WRITE %s NULL,1 %s\n", path, res(write(fd, NULL, 1)));
    printf("WRITE %s partial(ask 2pg) %s\n", path, res(write(fd, two, 2 * pg)));
    printf("WRITE %s unmapped(ask 16) %s\n", path, res(write(fd, two + pg, 16)));
    printf("WRITE %s via O_RDONLY fd %s\n", path, res(write(fdr, buf, 1)));
    int fdw = open(path, O_WRONLY);
    printf("READ %s via O_WRONLY fd %s\n", path, res(fdw >= 0 ? read(fdw, buf, 1) : -1));
    // lseek
    off_t offs[] = {0, 1, -1, 100, INT64_MAX};
    for (int w = 0; w <= 4; w++)
        for (int o = 0; o < 5; o++) {
            errno = 0;
            off_t r = lseek(fdr, offs[o], w);
            printf("LSEEK %s whence=%d off=%lld %s\n", path, w, (long long)offs[o], res(r));
        }
    lseek(fdr, 0, SEEK_SET);
    read(fdr, buf, 100);
    printf("LSEEK %s SEEK_CUR after read 100 %s\n", path, res(lseek(fdr, 0, SEEK_CUR)));
    lseek(fdr, 1000, SEEK_SET);
    printf("LSEEK %s SEEK_CUR after SEEK_SET 1000 %s\n", path, res(lseek(fdr, 0, SEEK_CUR)));
    read(fdr, buf, 100);
    printf("LSEEK %s SEEK_CUR after SEEK_SET 1000 + read 100 %s\n", path, res(lseek(fdr, 0, SEEK_CUR)));
    if (fdw >= 0) {
        write(fdw, buf, 50);
        printf("LSEEK %s SEEK_CUR on O_WRONLY fd after write 50 %s\n", path, res(lseek(fdw, 0, SEEK_CUR)));
    }
    // pread/pwrite
    printf("PREAD %s 16@0 %s\n", path, res(pread(fdr, buf, 16, 0)));
    printf("PREAD %s 16@2^40 %s\n", path, res(pread(fdr, buf, 16, (off_t)1 << 40)));
    printf("PREAD %s 16@-1 %s\n", path, res(pread(fdr, buf, 16, -1)));
    printf("PREAD %s 16@INT64_MAX %s\n", path, res(pread(fdr, buf, 16, INT64_MAX)));
    printf("PWRITE %s 16@0 %s\n", path, res(pwrite(fd, buf, 16, 0)));
    printf("PWRITE %s 16@-1 %s\n", path, res(pwrite(fd, buf, 16, -1)));
    printf("PWRITE %s 16@INT64_MAX %s\n", path, res(pwrite(fd, buf, 16, INT64_MAX)));
    // ftruncate
    printf("FTRUNCATE %s rdwr 0 %s\n", path, res(ftruncate(fd, 0)));
    printf("FTRUNCATE %s rdwr 10 %s\n", path, res(ftruncate(fd, 10)));
    printf("FTRUNCATE %s rdwr -1 %s\n", path, res(ftruncate(fd, -1)));
    printf("FTRUNCATE %s rdonly 0 %s\n", path, res(ftruncate(fdr, 0)));
    printf("TRUNCATE %s path 0 %s\n", path, res(truncate(path, 0)));
    // fcntl and O_NONBLOCK
    int fl = fcntl(fdr, F_GETFL);
    printf("FCNTL %s F_SETFL O_NONBLOCK %s", path, res(fcntl(fdr, F_SETFL, fl | O_NONBLOCK)));
    printf(" then read 64 %s\n", res(read(fdr, buf, 64)));
    fcntl(fdr, F_SETFL, fl);
    int fdnb = open(path, O_RDONLY | O_NONBLOCK);
    printf("OPEN %s O_RDONLY|O_NONBLOCK %s read 64 %s\n", path, res(fdnb), res(read(fdnb, buf, 64)));
    close(fdnb);
    // poll
    for (int which = 0; which < 3; which++) {
        int pfd = which == 0 ? fdr : which == 1 ? fdw : fd;
        struct pollfd p = {pfd, POLLIN | POLLOUT | POLLPRI | POLLRDNORM | POLLWRNORM | POLLRDBAND | POLLWRBAND, 0};
        int r = poll(&p, 1, 0);
        printf("POLL %s %s %s revents=0x%x\n", path, mn[which], res(r), p.revents);
        struct pollfd p2 = {pfd, 0, 0};
        r = poll(&p2, 1, 0);
        printf("POLL %s %s events=0 %s revents=0x%x\n", path, mn[which], res(r), p2.revents);
    }
#if defined(__linux__)
    for (int which = 0; which < 2; which++) {
        int pfd = which == 0 ? fdr : fdw;
        int ep = epoll_create1(0);
        struct epoll_event ev = {.events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLPRI, .data.u64 = 7};
        int r = epoll_ctl(ep, EPOLL_CTL_ADD, pfd, &ev);
        printf("EPOLL %s %s ADD %s", path, mn[which], res(r));
        struct epoll_event out[4];
        int n = epoll_wait(ep, out, 4, 0);
        printf(" wait %s events=0x%x\n", res(n), n > 0 ? out[0].events : 0);
        ev.events = EPOLLIN | EPOLLET;
        r = epoll_ctl(ep, EPOLL_CTL_MOD, pfd, &ev);
        printf("EPOLL %s %s MOD ET %s\n", path, mn[which], res(r));
        close(ep);
    }
    printf("FADVISE %s SEQUENTIAL %d\n", path, posix_fadvise(fdr, 0, 0, POSIX_FADV_SEQUENTIAL));
    printf("FADVISE %s 99 %d\n", path, posix_fadvise(fdr, 0, 0, 99));
#else
    for (int which = 0; which < 2; which++) {
        int pfd = which == 0 ? fdr : fdw;
        int kq = kqueue();
        struct kevent ch[2], out[4];
        EV_SET(&ch[0], pfd, EVFILT_READ, EV_ADD | EV_RECEIPT, 0, 0, 0);
        EV_SET(&ch[1], pfd, EVFILT_WRITE, EV_ADD | EV_RECEIPT, 0, 0, 0);
        int n = kevent(kq, ch, 2, out, 4, &(struct timespec){0, 0});
        printf("KQUEUE %s %s ADD %s", path, mn[which], res(n));
        for (int i = 0; i < n; i++) printf(" [filt=%d flags=0x%x data=%lld]", out[i].filter, out[i].flags, (long long)out[i].data);
        n = kevent(kq, NULL, 0, out, 4, &(struct timespec){0, 0});
        printf(" wait %s", res(n));
        for (int i = 0; i < n; i++) printf(" [filt=%d flags=0x%x data=%lld]", out[i].filter, out[i].flags, (long long)out[i].data);
        printf("\n");
        close(kq);
    }
#endif
    // flock: two descriptions of one device
    int a = open(path, O_RDONLY), b = open(path, O_RDONLY);
    printf("FLOCK %s first EX %s", path, res(flock(a, LOCK_EX | LOCK_NB)));
    printf(" second EX %s", res(flock(b, LOCK_EX | LOCK_NB)));
    printf(" second SH %s\n", res(flock(b, LOCK_SH | LOCK_NB)));
    close(a);
    close(b);
    printf("FSYNC %s %s fdatasync %s\n", path, res(fsync(fd)),
#if defined(__linux__)
           res(fdatasync(fd))
#else
           "n/a"
#endif
    );
    int avail = -12345;
    printf("FIONREAD %s %s value=%d\n", path, res(ioctl(fdr, FIONREAD, &avail)), avail);
    printf("ISATTY %s %d\n", path, isatty(fdr));
    {
        struct stat s;
        fstat(fdr, &s);
        print_stat("fstat-after-ops", path, &s);
    }
    printf("CLOSE %s %s %s\n", path, res(close(fdr)), res(fd != fdr ? close(fd) : 0));
    if (fdw >= 0) close(fdw);
    // the next descriptor after an open/close pair
    int x = open(path, O_RDONLY);
    int y = open("/dev/null", O_RDONLY);
    printf("FDNUM %s %d then %d\n", path, x, y);
    close(x);
    close(y);
}

static void sec_ops(void) {
    for (int i = 0; i < NDEVS; i++) dev_ops(devs[i]);
}

// Which timestamps a read and a write move, and when.
static void sec_times(void) {
    static unsigned char buf[4096];
    for (int i = 0; i < NDEVS; i++) {
        struct stat s0, s1, s2;
        int fd = open(devs[i], O_RDWR);
        if (fd < 0) fd = open(devs[i], O_RDONLY);
        stat(devs[i], &s0);
        sleep(2);
        struct timespec t;
        clock_gettime(CLOCK_REALTIME, &t);
        ssize_t r = read(fd, buf, 16);
        stat(devs[i], &s1);
        printf("TIMES %s read %zd at %lld.%09ld:", devs[i], r, (long long)t.tv_sec, t.tv_nsec);
        ts("a0", ATIM(s0)); ts("a1", ATIM(s1)); ts("m0", MTIM(s0)); ts("m1", MTIM(s1)); ts("c0", CTIM(s0)); ts("c1", CTIM(s1));
        printf("\n");
        sleep(2);
        clock_gettime(CLOCK_REALTIME, &t);
        ssize_t w = write(fd, buf, 16);
        stat(devs[i], &s2);
        printf("TIMES %s write %s at %lld.%09ld:", devs[i], res(w), (long long)t.tv_sec, t.tv_nsec);
        ts("a1", ATIM(s1)); ts("a2", ATIM(s2)); ts("m1", MTIM(s1)); ts("m2", MTIM(s2)); ts("c1", CTIM(s1)); ts("c2", CTIM(s2));
        printf("\n");
        close(fd);
    }
}

int main(int argc, char **argv) {
    alarm(120);
    setvbuf(stdout, NULL, _IOLBF, 0);
    int all = argc < 2;
    for (int i = 1; i <= (all ? 1 : argc - 1); i++) {
        const char *s = all ? NULL : argv[i];
        if (all || !strcmp(s, "stat")) sec_stat();
        if (all || !strcmp(s, "statfs")) sec_statfs();
        if (all || !strcmp(s, "ns")) sec_ns();
        if (all || !strcmp(s, "list")) sec_list();
        if (all || !strcmp(s, "ops")) sec_ops();
        if (!all && !strcmp(s, "times")) sec_times();
    }
    return 0;
}
