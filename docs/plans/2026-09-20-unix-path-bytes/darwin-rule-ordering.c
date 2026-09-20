/* Produces the plan's §1.3 and the Darwin rows of §1.4.
 * Where the encoding check sits among the other rules a creating operation
 * applies. Every row has a control, because the ordering claim is only
 * meaningful against one: EILSEQ at 300 invalid bytes says nothing until the
 * same length of valid bytes is shown to give ENAMETOOLONG.
 * Run: cc -o p darwin-rule-ordering.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdlib.h>

#define R(label, expr) do { errno = 0; long r = (long)(expr); \
  if (r >= 0) printf("%-52s OK (%ld)\n", label, r); \
  else printf("%-52s errno=%-3d %s\n", label, errno, strerror(errno)); } while (0)


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
    mkdir("d", 0755);
    int fd = open("d/f", O_CREAT|O_WRONLY, 0644); if (fd>=0) close(fd);

    puts("-- EILSEQ vs ENAMETOOLONG (name both too long and invalid) --");
    char nm[600]; strcpy(nm, "d/"); memset(nm+2, 0xff, 300); nm[302]=0;
    R("mkdir(300 x 0xff)", mkdir(nm, 0755));
    char ok[600]; strcpy(ok, "d/"); memset(ok+2, 'a', 300); ok[302]=0;
    R("mkdir(300 x 'a')   [control: ENAMETOOLONG]", mkdir(ok, 0755));

    puts("-- EILSEQ vs EACCES (parent not writable) --");
    mkdir("ro", 0555);
    R("mkdir(\"ro/\\xff\")   [EACCES or EILSEQ?]", mkdir("ro/\xff", 0755));
    R("mkdir(\"ro/plain\")  [control: EACCES]", mkdir("ro/plain", 0755));

    puts("-- APFS NAME_MAX unit, valid UTF-8 (CJK = 3 bytes, 1 UTF-16 unit) --");
    char cjk[2048]; strcpy(cjk, "d/");
    for (int i = 0; i < 255; i++) memcpy(cjk + 2 + 3*i, "\xe4\xb8\xad", 3);
    cjk[2 + 3*255] = 0;
    R("mkdir(255 CJK = 765 bytes / 255 UTF-16 units)", mkdir(cjk, 0755));
    for (int i = 0; i < 256; i++) memcpy(cjk + 2 + 3*i, "\xe4\xb8\xad", 3);
    cjk[2 + 3*256] = 0;
    R("mkdir(256 CJK = 768 bytes / 256 UTF-16 units)", mkdir(cjk, 0755));

    puts("-- rename: source missing AND destination invalid --");
    R("rename(\"d/missing\", \"d/\\xff\")", rename("d/missing", "d/\xff"));
    R("rename(\"d/f\", \"d/\\xff\")  [control: EILSEQ]", rename("d/f", "d/\xff"));

    puts("-- trailing separator on an invalid name --");
    R("mkdir(\"d/\\xff/\")", mkdir("d/\xff/", 0755));

    puts("-- O_CREAT on an invalid name that ALSO has a bad parent --");
    R("open(\"d/nodir/\\xff\", O_CREAT)", open("d/nodir/\xff", O_CREAT|O_WRONLY, 0644));

    leaveProbeDir ();
    return 0;
}
