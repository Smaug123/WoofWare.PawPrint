/* Produces the plan's §1.1 and the Darwin half of §1.5.
 * Which byte strings APFS will bind as a directory entry name, and whether a
 * symlink *target* is subject to the same rule (it is not).
 * Run: cc -o p darwin-which-names-bind.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

static void try_create(const char *label, const char *name) {
    errno = 0;
    int fd = open(name, O_CREAT | O_WRONLY | O_EXCL, 0644);
    if (fd >= 0) { printf("%-28s create OK\n", label); close(fd); unlink(name); }
    else printf("%-28s create FAIL errno=%d (%s)\n", label, errno, strerror(errno));
}

static void try_symlink(const char *label, const char *target) {
    errno = 0;
    unlink("probe-link");
    if (symlink(target, "probe-link") == 0) {
        char buf[512]; ssize_t n = readlink("probe-link", buf, sizeof buf);
        printf("%-28s symlink OK, readlink %zd bytes, roundtrip=%s\n", label, n,
               (n == (ssize_t)strlen(target) && memcmp(buf, target, n) == 0) ? "exact" : "DIFFERENT");
        unlink("probe-link");
    } else printf("%-28s symlink FAIL errno=%d (%s)\n", label, errno, strerror(errno));
}

int main(void) {
    try_create("ascii", "probe-ok");
    try_create("lone 0xFF", "probe-\xff");
    try_create("truncated UTF-8 (0xE4 0xB8)", "probe-\xe4\xb8");
    try_create("overlong 0xC0 0x80", "probe-\xc0\x80");
    try_create("surrogate enc ED A0 80", "probe-\xed\xa0\x80");
    try_create("valid CJK", "probe-\xe4\xb8\xad");
    try_symlink("target lone 0xFF", "/tmp/\xff");
    try_symlink("target truncated", "/tmp/\xe4\xb8");
    return 0;
}
