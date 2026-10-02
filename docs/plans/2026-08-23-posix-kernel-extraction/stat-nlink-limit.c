// Measures a directory's st_nlink past 65535: Darwin's nlink_t is 16 bits wide.
//
// Usage: stat-nlink-limit <empty-dir> <count> [dirs]
// Creates <count> names in <empty-dir> (regular files, or subdirectories with
// a third argument) and reports the directory's count every 10000 names and
// at every name from 65530 to 65540.
//
// Linux:  container run --rm --shm-size 2G -v "$PWD":/probe gcc:14 sh -c
//           'gcc -O1 -o /tmp/b /probe/stat-nlink-limit.c && /tmp/b "$(mktemp -d -p /dev/shm)" 70000 dirs'
// Darwin: nix develop -c clang -Wall -o stat-nlink-limit stat-nlink-limit.c
//           && ./stat-nlink-limit "$(mktemp -d /private/tmp/nlink.XXXXXX)" 70000
//
// Measured on 2026-10-02, outputs beside this file. On APFS (files, since
// every name counts there) the count is 2 plus the names up to 65535 and then
// stays at 65535; on tmpfs (subdirectories, since only they count there) it
// keeps counting, to 70002.
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc < 3) { fprintf(stderr, "usage: %s <empty-dir> <count> [dirs]\n", argv[0]); return 2; }
    alarm(600);
    char p[4096];
    int n = atoi(argv[2]);
    int dirs = argc > 3;
    struct stat st;
    for (int i = 0; i < n; i++) {
        snprintf(p, sizeof p, "%s/e%d", argv[1], i);
        if (dirs) {
            if (mkdir(p, 0777) != 0) { perror("mkdir"); return 1; }
        } else {
            int fd = open(p, O_CREAT | O_EXCL | O_WRONLY, 0644);
            if (fd < 0) { perror("open"); return 1; }
            close(fd);
        }
        int names = i + 1;
        if ((names >= 65530 && names <= 65540) || names % 10000 == 0 || names == n) {
            if (stat(argv[1], &st) != 0) { perror("stat"); return 1; }
            printf("names=%d\tnlink=%lu\tsizeof(st_nlink)=%zu\n", names, (unsigned long)st.st_nlink, sizeof st.st_nlink);
        }
    }
    return 0;
}
