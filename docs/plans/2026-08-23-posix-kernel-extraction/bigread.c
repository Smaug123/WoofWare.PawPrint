// The largest single read of /dev/zero, /dev/random and /dev/urandom, and
// whether a read is ever short: (1) one read per count at the edges of
// INT_MAX, MAX_RW_COUNT and 2^32, into storage that holds all of it; (2) reads
// of 16 MiB, 1 MiB and 4097 bytes while an interval timer delivers SIGALRM to
// a handler installed without SA_RESTART, counting whole, short (with the
// distinct short counts) and EINTR answers.
//
// Usage: bigread [big|signal]
#define _GNU_SOURCE
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
static volatile sig_atomic_t hits;
static void on_alarm(int s) { (void)s; hits++; }
static const char *devs[] = {"/dev/zero", "/dev/random", "/dev/urandom"};
int main(int argc, char **argv) {
    alarm(300);
    setvbuf(stdout, NULL, _IOLBF, 0);
    int big = argc < 2 || !strcmp(argv[1], "big");
    int sig = argc < 2 || !strcmp(argv[1], "signal");
    if (big) {
        size_t span = (size_t)5 << 30;
        unsigned char *m = mmap(NULL, span, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON
#ifdef MAP_NORESERVE
                                | MAP_NORESERVE
#endif
                                , -1, 0);
        if (m == MAP_FAILED) { perror("mmap"); return 1; }
        size_t counts[] = {0x7FFFF000UL, 0x7FFFF001UL, 0x7FFFFFFFUL, 0x80000000UL, 0x100000000UL, (size_t)SSIZE_MAX, SIZE_MAX};
        for (int d = 0; d < 3; d++) {
            int fd = open(devs[d], O_RDONLY);
            for (unsigned i = 0; i < sizeof counts / sizeof *counts; i++) {
                errno = 0;
                ssize_t r = read(fd, m, counts[i]);
                printf("BIG %s count=0x%zx -> %zd (0x%zx) errno=%d\n", devs[d], counts[i], r, (size_t)r, r < 0 ? errno : 0);
                // Give the pages back so the next read does not need twice the memory.
                madvise(m, span, MADV_DONTNEED);
            }
            close(fd);
        }
    }
    if (sig) {
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_alarm;
        sigaction(SIGALRM, &sa, NULL);
        static unsigned char buf[16 << 20];
        size_t sizes[] = {16 << 20, 1 << 20, 4097};
        for (int d = 0; d < 3; d++)
            for (int s = 0; s < 3; s++) {
                int fd = open(devs[d], O_RDONLY);
                struct itimerval it = {{0, 50}, {0, 50}};
                setitimer(ITIMER_REAL, &it, NULL);
                int whole = 0, shortn = 0, eintr = 0, other = 0, reps = sizes[s] > 4097 ? 300 : 20000;
                ssize_t shorts[16];
                int ns = 0;
                hits = 0;
                for (int i = 0; i < reps; i++) {
                    ssize_t r = read(fd, buf, sizes[s]);
                    if (r == (ssize_t)sizes[s]) whole++;
                    else if (r >= 0) {
                        shortn++;
                        int seen = 0;
                        for (int k = 0; k < ns; k++) seen |= shorts[k] == r;
                        if (!seen && ns < 16) shorts[ns++] = r;
                    } else if (errno == EINTR) eintr++;
                    else other++;
                }
                struct itimerval off = {{0, 0}, {0, 0}};
                setitimer(ITIMER_REAL, &off, NULL);
                printf("SIGNAL %s size=%zu reps=%d signals=%d whole=%d short=%d eintr=%d other=%d short_counts:", devs[d], sizes[s], reps, (int)hits, whole, shortn, eintr, other);
                for (int k = 0; k < ns; k++) printf(" %zd", shorts[k]);
                printf("\n");
                close(fd);
            }
    }
    return 0;
}
