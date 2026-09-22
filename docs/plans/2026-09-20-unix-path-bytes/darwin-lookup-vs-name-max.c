/* Produces the plan's §1.4 lookup claim.
 * On a *lookup*, an undecodable component of 300 bytes is ENOENT, where a valid
 * component of the same length is ENAMETOOLONG -- so the two are not measured by
 * the same rule.
 *
 * Do NOT read this as "an undecodable component is never length-checked": it is,
 * against a different limit. Darwin caps a decodable name at 255 UTF-16 units and
 * an undecodable one at 765 raw bytes, so 766 x 0xFF *is* ENAMETOOLONG. This
 * probe only reaches 300 bytes and so cannot see that; darwin-name-max-boundary-
 * per-unit.c bisects both limits, and the plan's §1.5 has the tables.
 * Run: cc -o p darwin-lookup-vs-name-max.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdlib.h>
#define R(l,e) do{errno=0;long r=(long)(e); if(r>=0)printf("%-56s OK\n",l); else printf("%-56s errno=%-3d %s\n",l,errno,strerror(errno));}while(0)

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

int main(void){
    enterProbeDir ();
    system("rm -rf p4 && mkdir p4"); chdir("p4"); mkdir("d",0755);
    char bad[600], good[600], badmid[600];
    strcpy(bad ,"d/"); memset(bad +2,0xff,300); bad [302]=0;   // 300 invalid bytes, final comp
    strcpy(good,"d/"); memset(good+2,'a' ,300); good[302]=0;   // 300 valid bytes, final comp
    strcpy(badmid,"d/"); memset(badmid+2,0xff,300); strcpy(badmid+302,"/x"); // 300 invalid, NON-final

    puts("-- LOOKUP (non-creating) of an over-long component --");
    R("open(300 x 'a',  O_RDONLY)  [control]", open(good,O_RDONLY));
    R("open(300 x 0xff, O_RDONLY)  [ENAMETOOLONG or ENOENT?]", open(bad,O_RDONLY));
    R("access(300 x 0xff, F_OK)", access(bad,F_OK));
    R("open(300 x 0xff + \"/x\", O_RDONLY) [non-final]", open(badmid,O_RDONLY));

    puts("-- LOOKUP of an over-long component: valid multi-byte UTF-8 --");
    char cjk[2048]; strcpy(cjk,"d/");
    for(int i=0;i<256;i++) memcpy(cjk+2+3*i,"\xe4\xb8\xad",3); cjk[2+3*256]=0;
    R("open(256 CJK = 768 bytes / 256 units, O_RDONLY)", open(cjk,O_RDONLY));
    for(int i=0;i<255;i++) memcpy(cjk+2+3*i,"\xe4\xb8\xad",3); cjk[2+3*255]=0;
    R("open(255 CJK = 765 bytes / 255 units, O_RDONLY) [ENOENT?]", open(cjk,O_RDONLY));

    puts("-- unlink/rmdir of an over-long invalid name --");
    R("unlink(300 x 0xff)", unlink(bad));
    R("rmdir(300 x 0xff)", rmdir(bad));

    chdir(".."); system("rm -rf p4"); return 0;
}
