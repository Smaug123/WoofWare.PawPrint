// Linux device rows the first probe did not cover, for /dev/null and
// /dev/urandom: lseek's whence outside 0..4; tcgetattr's errno; getdents64 on a
// device; copy_file_range and FICLONE between a device and a regular file, and
// between two devices; posix_fadvise's whole advice range; a read of 0 through
// a NULL buffer; and FIONREAD through each access mode.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <termios.h>
#include <stdlib.h>
#include <sys/file.h>
#include <unistd.h>
#include <linux/fs.h>
static const char *e(long r) { static char b[8][48]; static int k; char *s = b[k++ & 7]; if (r < 0) snprintf(s, 48, "-1 %s", strerrorname_np(errno)); else snprintf(s, 48, "%ld", r); return s; }
int main(void) {
    alarm(20);
    const char *devs[] = {"/dev/null", "/dev/urandom"};
    char tmpl[] = "/tmp/l2XXXXXX";
    int file = mkstemp(tmpl);
    write(file, "hello", 5);
    for (int d = 0; d < 2; d++) {
        const char *p = devs[d];
        int fd = open(p, O_RDWR);
        printf("== %s\n", p);
        for (int w = -2; w <= 8; w++) {
            errno = 0;
            printf("LSEEK whence=%d off=0 %s off=-5 %s\n", w, e(lseek(fd, 0, w)), e(lseek(fd, -5, w)));
        }
        struct termios t;
        printf("TCGETATTR %s isatty=%d errno=%s\n", e(tcgetattr(fd, &t)), isatty(fd), strerrorname_np(errno));
        char buf[4096];
        printf("GETDENTS64 %s\n", e(syscall(SYS_getdents64, fd, buf, sizeof buf)));
        int rd = open(p, O_RDONLY), wr = open(p, O_WRONLY);
        int n;
        printf("FIONREAD rdonly %s wronly %s\n", e(ioctl(rd, FIONREAD, &n)), e(ioctl(wr, FIONREAD, &n)));
        printf("READ NULL,0 %s\n", e(read(fd, NULL, 0)));
        off64_t o1 = 0, o2 = 0;
        printf("COPY_FILE_RANGE file->dev %s dev->file %s dev->dev %s\n",
               e(copy_file_range(file, &o1, fd, NULL, 3, 0)), e(copy_file_range(fd, NULL, file, &o2, 3, 0)),
               e(copy_file_range(fd, NULL, wr, NULL, 3, 0)));
        printf("FICLONE dev<-file %s file<-dev %s dev<-dev %s\n", e(ioctl(fd, FICLONE, file)), e(ioctl(file, FICLONE, fd)), e(ioctl(wr, FICLONE, rd)));
        printf("FADVISE");
        for (int a = -1; a <= 7; a++) printf(" %d:%d", a, posix_fadvise(fd, 0, 0, a));
        printf(" off-1:%d len-1:%d\n", posix_fadvise(fd, -1, 0, 0), posix_fadvise(fd, 0, -1, 0));
        printf("FLOCK SH %s UN %s\n", e(flock(rd, LOCK_SH | LOCK_NB)), e(flock(rd, LOCK_UN)));
        printf("FTRUNCATE wronly 0 %s\n", e(ftruncate(wr, 0)));
        close(rd); close(wr); close(fd);
    }
    unlink(tmpl);
    return 0;
}
