// A non-blocking write whose count, added to the 512 bytes already held,
// exceeds INT_MAX: how much does the pipe take?
//
// Results, 2026-09-26 (Darwin 27.0.0 arm64; Linux 6.18.5 aarch64, 4 KiB pages,
// in Apple's `container`):
//
//   count        Darwin   Linux
//   2147483500   65024    61440
//   2147483647   65024    61440
//   65536        65024    61440
//   70000        65024    61808
//
// So the count's size beyond what fits changes nothing: Darwin fills its 64 KiB
// buffer, and Linux takes 15 free slots plus, for 70000, the 368-byte remainder
// that fits after the 512 bytes in the first slot.
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
    size_t sizes[] = {2147483500u, 2147483647u, 65536, 70000};
    char *big = calloc(1, 2147483647u);
    if (!big) { perror("calloc"); return 1; }
    for (unsigned k = 0; k < 4; k++) {
        int p[2]; pipe(p);
        fcntl(p[1], F_SETFL, O_NONBLOCK);
        ssize_t a = write(p[1], big, 512);
        ssize_t b = write(p[1], big, sizes[k]);
        printf("held %zd, then write(%zu) -> %zd errno %d\n", a, sizes[k], b, b < 0 ? errno : 0);
        close(p[0]); close(p[1]);
    }
    return 0;
}
