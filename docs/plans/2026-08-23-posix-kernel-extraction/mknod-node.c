// Linux, as root: a character device node made with mknod on tmpfs and on ext4,
// against the devtmpfs node for the same device. Which stat fields and which
// timestamp movements belong to the filesystem holding the node, and which to
// the driver behind it.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <sys/sysmacros.h>
#include <unistd.h>
static void st(const char *tag, const char *p) {
    struct stat s; stat(p, &s);
    printf("%s %s dev=%u,%u ino=%llu mode=0%o rdev=%u,%u size=%lld blksize=%ld atime=%lld.%09ld mtime=%lld.%09ld ctime=%lld.%09ld\n", tag, p,
           major(s.st_dev), minor(s.st_dev), (unsigned long long)s.st_ino, s.st_mode, major(s.st_rdev), minor(s.st_rdev), (long long)s.st_size, (long)s.st_blksize,
           (long long)s.st_atim.tv_sec, s.st_atim.tv_nsec, (long long)s.st_mtim.tv_sec, s.st_mtim.tv_nsec, (long long)s.st_ctim.tv_sec, s.st_ctim.tv_nsec);
}
int main(void) {
    alarm(30);
    const char *paths[] = {"/dev/urandom", "/dev/shm/urandom-node", "/tmp/urandom-node"};
    unlink(paths[1]); unlink(paths[2]);
    printf("mknod tmpfs %d, ext4 %d\n", mknod(paths[1], S_IFCHR | 0666, makedev(1, 9)), mknod(paths[2], S_IFCHR | 0666, makedev(1, 9)));
    for (int i = 0; i < 3; i++) {
        st("BEFORE", paths[i]);
        sleep(1);
        int fd = open(paths[i], O_RDWR);
        char b[16];
        printf("READ %zd ", read(fd, b, 16));
        st("AFTER-READ", paths[i]);
        sleep(1);
        printf("WRITE %zd ", write(fd, b, 16));
        st("AFTER-WRITE", paths[i]);
        printf("LSEEK %lld ", (long long)lseek(fd, 100, SEEK_SET));
        struct pollfd p = {fd, POLLIN | POLLOUT, 0};
        poll(&p, 1, 0);
        int ep = epoll_create1(0);
        struct epoll_event ev = {.events = EPOLLIN};
        int r = epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev);
        struct statfs f; fstatfs(fd, &f);
        printf("POLL 0x%x EPOLL %d(%s) FSTATFS type=0x%lx FTRUNCATE %d(%s)\n", p.revents, r, r ? strerror(errno) : "-", (long)f.f_type, ftruncate(fd, 0), strerror(errno));
        close(fd);
    }
    unlink(paths[1]); unlink(paths[2]);
    return 0;
}
