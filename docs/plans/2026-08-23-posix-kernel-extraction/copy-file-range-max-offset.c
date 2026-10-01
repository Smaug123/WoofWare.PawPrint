// copy_file_range(2) into a destination at and near offset INT64_MAX, from
// sources of 1, 5 and 0 bytes, at lengths 1, 5 and 0.
//
// container run --rm -v "$PWD":/p gcc:14 sh -c 'gcc -w -o /tmp/c /p/copy-file-range-max-offset.c && /tmp/c /dev/shm && /tmp/c /tmp'
//
// Measured on Linux 6.18.5 aarch64 on 2026-10-02. On tmpfs (/dev/shm), a
// destination at INT64_MAX is EFBIG for every source and every length, 0
// included, and the source's offset does not move; at INT64_MAX - 1 and - 2 the
// copy is cut to what fits below INT64_MAX (1 and 2 bytes of a 5-byte
// source), and an empty source or a length of 0 is 0. ext4 (/tmp) copied the
// whole request even at INT64_MAX, so the boundary is the filesystem's.
// copy_file_range(2) into a destination offset at and near INT64_MAX.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>
int main(int argc, char **argv) {
    alarm(60);
    char src[4096], dst[4096];
    snprintf(src, sizeof src, "%s/src", argv[1]);
    snprintf(dst, sizeof dst, "%s/dst", argv[1]);
    int64_t offs[] = { INT64_MAX, INT64_MAX - 1, INT64_MAX - 2 };
    const char *contents[] = { "h", "hello", "" };
    size_t lens[] = { 1, 5, 0 };
    for (int c = 0; c < 3; c++)
        for (int o = 0; o < 3; o++)
            for (int l = 0; l < 3; l++) {
                unlink(src); unlink(dst);
                int fd = open(src, O_CREAT | O_WRONLY, 0644);
                write(fd, contents[c], strlen(contents[c]));
                close(fd);
                int in = open(src, O_RDONLY), out = open(dst, O_CREAT | O_WRONLY, 0644);
                lseek(out, offs[o], SEEK_SET);
                errno = 0;
                long r = syscall(__NR_copy_file_range, in, NULL, out, NULL, lens[l], 0);
                printf("src=%zu bytes, dst offset INT64_MAX-%lld, len %zu: %ld %s, in at %lld\n", strlen(contents[c]), (long long)(INT64_MAX - offs[o]), lens[l], r, r < 0 ? strerror(errno) : "", (long long)lseek(in, 0, SEEK_CUR));
                close(in); close(out);
            }
    return 0;
}
