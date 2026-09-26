// A non-blocking write of a huge count into a pipe already holding 512 bytes,
// or 1: how much does the pipe take, and where does Linux's per-call clamp
// (MAX_RW_COUNT, INT_MAX rounded down to a page: 0x7FFFF000) bite?
//
// Results, 2026-09-26 (Darwin 27.0.0 arm64; Linux 6.18.5 aarch64, 4 KiB pages,
// in Apple's `container`):
//
//   held  count        Darwin   Linux
//   512   2147483500   65024    61440
//   512   2147483647   65024    61440
//   512   65536        65024    61440
//   512   70000        65024    61808
//   512   0x7FFFF000   65024    61440
//   512   0x7FFFF001   65024    61440
//   512   0x7FFFEFFF   65024    61440
//   1     2147483500   65535    61440
//   1     2147483647   65535    61440
//   1     65536        65535    61440
//   1     70000        65535    61808
//   1     0x7FFFF000   65535    61440
//   1     0x7FFFF001   65535    61440
//   1     0x7FFFEFFF   65535    65535
//
// Darwin fills its 64 KiB buffer whatever the count. Linux takes 15 free slots,
// plus the count's sub-page remainder when that fits after the bytes already
// in the first slot: 368 of 70000's, and all 4095 of 0x7FFFEFFF's after 1 byte.
// 0x7FFFF001 has a 1-byte remainder that would fit after 1 byte, yet takes
// 61440: the count is clamped to 0x7FFFF000, remainder 0, before the pipe
// sees it. So is 2147483500, whose own remainder is 3948.
//
// Build: `nix develop -c cc -O0 -o /tmp/p <this file>` on Darwin, `gcc -O0` on
// Linux. It allocates 2 GiB.
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
int main(void) {
    alarm(30);
    size_t sizes[] = {2147483500u, 2147483647u, 65536, 70000, 0x7FFFF000u, 0x7FFFF001u, 0x7FFFEFFFu};
    size_t helds[] = {512, 1};
    char *big = calloc(1, 2147483647u);
    if (!big) { perror("calloc"); return 1; }
    for (unsigned h = 0; h < 2; h++)
    for (unsigned k = 0; k < 7; k++) {
        int p[2]; pipe(p);
        fcntl(p[1], F_SETFL, O_NONBLOCK);
        ssize_t a = write(p[1], big, helds[h]);
        ssize_t b = write(p[1], big, sizes[k]);
        printf("held %zd, then write(%zu) -> %zd errno %d\n", a, sizes[k], b, b < 0 ? errno : 0);
        close(p[0]); close(p[1]);
    }
    return 0;
}
