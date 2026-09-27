// Whether Linux's fchmod on a pipe end moves the pipe's ctime, seen through the
// other end: the same call 200 times per run, each 30 ms after the last.
// Written because one row of chmod-rules.c's Linux output disagreed with the
// rest. Linux 6.18.5 (aarch64, container), two runs on 2026-09-27: moved 200,
// kept 0, both times.
//
// container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -O1 -o /tmp/q /probe/chmod-pipe-ctime.c && /tmp/q && /tmp/q'
#include <stdio.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
int main(void) {
    alarm(60);
    int fds[2];
    pipe(fds);
    int moved = 0, kept = 0;
    for (int i = 0; i < 200; i++) {
        struct stat b, a;
        fstat(fds[i & 1], &b);
        struct timespec ts = {0, 30 * 1000 * 1000};
        nanosleep(&ts, NULL);
        fchmod(fds[i & 1], 0600);
        fstat(fds[1 - (i & 1)], &a);
        if (a.st_ctim.tv_sec == b.st_ctim.tv_sec && a.st_ctim.tv_nsec == b.st_ctim.tv_nsec) kept++; else moved++;
    }
    printf("pipe fchmod same mode after 30ms: ctime moved %d, kept %d\n", moved, kept);
    return 0;
}
