// /dev as a mount point: what readdir("/") reports for "dev" against what
// stat("/dev") reports; what `..` resolves to from inside /dev; and what
// rmdir, rename, unlink and chmod do to the mount point itself.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
static const char *e(int r) { static char b[8][32]; static int k; char *s = b[k++ & 7]; if (r < 0) snprintf(s, 32, "-1 %s", strerror(errno)); else snprintf(s, 32, "%d", r); return s; }
int main(void) {
    alarm(20);
    struct stat dev, root, devdotdot;
    stat("/dev", &dev); stat("/", &root); stat("/dev/..", &devdotdot);
    printf("MP euid=%u stat(/dev) dev=%llu ino=%llu; stat(/) dev=%llu ino=%llu; stat(/dev/..) dev=%llu ino=%llu\n", geteuid(),
           (unsigned long long)dev.st_dev, (unsigned long long)dev.st_ino, (unsigned long long)root.st_dev, (unsigned long long)root.st_ino,
           (unsigned long long)devdotdot.st_dev, (unsigned long long)devdotdot.st_ino);
    DIR *d = opendir("/");
    struct dirent *x;
    while ((x = readdir(d)))
        if (!strcmp(x->d_name, "dev")) printf("MP readdir(/) dev: d_ino=%llu d_type=%d\n", (unsigned long long)x->d_ino, x->d_type);
    closedir(d);
    d = opendir("/dev");
    while ((x = readdir(d)))
        if (!strcmp(x->d_name, "..") || !strcmp(x->d_name, ".")) printf("MP readdir(/dev) %s: d_ino=%llu\n", x->d_name, (unsigned long long)x->d_ino);
    closedir(d);
    printf("MP rmdir(/dev) %s\n", e(rmdir("/dev")));
    printf("MP rename(/dev, /devx) %s\n", e(rename("/dev", "/devx")));
    printf("MP unlink(/dev) %s\n", e(unlink("/dev")));
    printf("MP chmod(/dev, mode unchanged) %s\n", e(chmod("/dev", dev.st_mode & 07777)));
    printf("MP mkdir(/dev) %s\n", e(mkdir("/dev", 0755)));
    printf("MP rmdir(/dev/.) %s\n", e(rmdir("/dev/.")));
    printf("MP chdir(/dev) %s", e(chdir("/dev")));
    char cwd[256];
    printf(" getcwd=%s chdir(..) %s", getcwd(cwd, sizeof cwd) ? cwd : "NULL", e(chdir("..")));
    printf(" getcwd=%s\n", getcwd(cwd, sizeof cwd) ? cwd : "NULL");
    return 0;
}
