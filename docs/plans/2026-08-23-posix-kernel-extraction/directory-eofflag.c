// Directory-read probe (Darwin only): whether __getdirentries64 writes a flags
// word into the last four bytes of the caller's buffer, and from which buffer
// size. Measured 2026-09-26 on Darwin 27.0.0 (arm64, APFS): from 1024 bytes up,
// with bit 0 set at end-of-directory.
//
// Usage: directory-eofflag <base-dir>
#include "directory-common.h"
static unsigned char buf[8192];
int main(int argc, char **argv) {
    alarm(30);
    char d[4096]; snprintf(d, sizeof d, "%s/eof", argv[1]); rmrf(d); mkdir(d, 0755);
    for (int i = 0; i < 40; i++) { char p[4096]; snprintf(p, sizeof p, "%s/n%03d", d, i); touch(p); }
    size_t sizes[] = {64, 512, 1020, 1023, 1024, 1025, 1100, 1400, 4096, 8192};
    for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
        int fd = open(d, O_RDONLY | O_DIRECTORY);
        for (int call = 0; call < 3; call++) {
            memset(buf, 0xAA, sizeof buf);
            long r = rawgetdents(fd, buf, sizes[i], NULL);
            uint32_t tail; memcpy(&tail, buf + sizes[i] - 4, 4);
            int untouched_after = 1; for (size_t j = r > 0 ? r : 0; j < sizes[i] - 4; j++) if (buf[j] != 0xAA) untouched_after = 0;
            printf("EOFFLAG size=%zu call=%d ret=%ld tail_word=0x%08x gap_untouched=%d\n", sizes[i], call, r, tail, untouched_after);
            if (r <= 0) break;
        }
        close(fd);
    }
    rmrf(d);
}
