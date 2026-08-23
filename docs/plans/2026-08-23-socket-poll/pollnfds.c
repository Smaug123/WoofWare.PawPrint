#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <poll.h>
#include <sys/resource.h>
int main(void) {
    struct rlimit rl; getrlimit(RLIMIT_NOFILE, &rl);
    printf("RLIMIT_NOFILE soft=%llu hard=%llu\n",
           (unsigned long long)rl.rlim_cur, (unsigned long long)rl.rlim_max);
    unsigned counts[] = { 1023, 1024, 1025, 2048, 65536, 1048576 };
    for (unsigned i = 0; i < sizeof counts / sizeof *counts; i++) {
        unsigned n = counts[i];
        struct pollfd *big = calloc(n, sizeof *big);
        if (!big) { printf("  nfds=%-8u calloc failed\n", n); continue; }
        for (unsigned j = 0; j < n; j++) { big[j].fd = -1; big[j].events = POLLIN; }
        errno = 0;
        int rv = poll(big, n, 0);
        printf("  nfds=%-8u rv=%d errno=%d\n", n, rv, errno);
        free(big);
    }
    return 0;
}
