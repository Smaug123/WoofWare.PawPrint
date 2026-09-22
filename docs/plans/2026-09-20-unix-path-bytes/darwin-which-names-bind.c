/* Produces the plan's §1.1 and the Darwin half of §1.6.
 * Which byte strings APFS will bind as a directory entry name, and whether a
 * symlink *target* is subject to the same rule (it is not).
 * Run: cc -o p darwin-which-names-bind.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <stdlib.h>
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


/* Every probe runs inside a fresh mkdtemp directory and removes only that, so
   re-running one cannot touch anything the caller owns. */
static char probeDir[64];
static void enterProbeDir(void) {
    strcpy (probeDir, "/tmp/pawprint-path-probe.XXXXXX");
    if (mkdtemp (probeDir) == NULL) { perror ("mkdtemp"); exit (1); }
    if (chdir (probeDir) != 0) { perror ("chdir"); exit (1); }
    printf ("probe directory: %s\n", probeDir);
}
static void leaveProbeDir(void) {
    char cmd[256];
    if (chdir ("/tmp") != 0) { perror ("chdir /tmp"); return; }
    /* u+w first: some probes deliberately create mode-0555 directories. */
    snprintf (cmd, sizeof cmd, "chmod -R u+w '%s' >/dev/null 2>&1; rm -rf '%s'", probeDir, probeDir);
    if (system (cmd) != 0) fprintf (stderr, "warning: could not clean up %s\n", probeDir);
}

int main(void) {
    enterProbeDir ();
    try_create("ascii", "probe-ok");
    try_create("lone 0xFF", "probe-\xff");
    try_create("truncated UTF-8 (0xE4 0xB8)", "probe-\xe4\xb8");
    try_create("overlong 0xC0 0x80", "probe-\xc0\x80");
    try_create("surrogate enc ED A0 80", "probe-\xed\xa0\x80");
    try_create("valid CJK", "probe-\xe4\xb8\xad");
    try_symlink("target lone 0xFF", "/tmp/\xff");
    try_symlink("target truncated", "/tmp/\xe4\xb8");
    leaveProbeDir ();
    return 0;
}
