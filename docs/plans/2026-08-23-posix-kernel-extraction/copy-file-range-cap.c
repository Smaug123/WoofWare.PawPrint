// How much one copy_file_range(2) call moves from a sparse tmpfs file around
// MAX_RW_COUNT (0x7ffff000) long. A container's /dev/shm holds 64 MiB, so this
// needs a larger tmpfs of its own, and memory for the destination, which the
// copy makes dense:
//
// container run --rm -m 8G --cap-add CAP_SYS_ADMIN -v "$PWD":/probe gcc:14 sh -c 'mkdir -p /mnt/big && mount -t tmpfs -o size=7g tmpfs /mnt/big && gcc -O1 -o /tmp/c /probe/copy-file-range-cap.c && /tmp/c /mnt/big'
//
// Measured on Linux 6.18.5 aarch64 on 2026-10-01:
//   size=2147475456 one call=2147475456
//   size=2147479552 one call=2147479552
//   size=2147479553 one call=2147479552
//   size=2147487744 one call=2147479552
#define _GNU_SOURCE
#include <fcntl.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <unistd.h>
int main(int argc, char **argv) {
    alarm(300);
    char src[4096], dst[4096];
    snprintf(src, sizeof src, "%s/src", argv[1]);
    snprintf(dst, sizeof dst, "%s/dst", argv[1]);
    long long sizes[] = { 0x7ffff000LL - 4096, 0x7ffff000LL, 0x7ffff000LL + 1, 0x80000000LL + 4096 };
    for (int i = 0; i < 4; i++) {
        unlink(src); unlink(dst);
        int fd = open(src, O_CREAT | O_WRONLY, 0644);
        if (ftruncate(fd, sizes[i]) != 0) return 2;
        close(fd);
        int in = open(src, O_RDONLY), out = open(dst, O_CREAT | O_WRONLY, 0644);
        long r = syscall(__NR_copy_file_range, in, NULL, out, NULL, (size_t)sizes[i], 0);
        printf("size=%lld one call=%ld\n", sizes[i], r);
        close(in); close(out);
    }
    return 0;
}
