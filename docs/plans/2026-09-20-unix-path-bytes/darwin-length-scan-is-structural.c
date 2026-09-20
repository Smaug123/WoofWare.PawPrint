/* Produces the plan's §1.6.
 * Darwin's NAME_MAX scanner is a *structural* UTF-8 length scan, not a strict
 * decode. A sequence with a valid lead byte and the right number of
 * continuation bytes is length-checked even when what it encodes is illegal --
 * a surrogate (ED A0 80), an overlong 3-byte form (E0 80 81), a value above
 * U+10FFFF (F5 80 80 80) all give ENAMETOOLONG at 300 repetitions. Bytes that
 * are not lead bytes at all (FF, FE, 80), the overlong 2-byte lead C0, and an
 * incomplete sequence (E4 B8) are not length-checked and give ENOENT.
 * The 10-repetition rows are the control: under NAME_MAX every one is
 * ENOENT/EILSEQ, so it is the length check that differs and not admissibility.
 * Run: cc -o p darwin-length-scan-is-structural.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
/* Which undecodable byte strings does Darwin still LENGTH-CHECK?
   If some do, the NAME_MAX skip cannot be keyed on strict-UTF-8 decodability. */
static void run(const char*label,const char*unit,int units){
    char nm[4096]; int k=0; strcpy(nm,"d/"); k=2;
    int ul=strlen(unit);
    for(int i=0;i<units;i++){ memcpy(nm+k,unit,ul); k+=ul; }
    nm[k]=0;
    errno=0; int r=open(nm,O_RDONLY); int e=errno; if(r>=0)close(r);
    printf("  %-34s x%-4d (%4d bytes) open: %-26s", label, units, k-2, r>=0?"OK":strerror(e));
    errno=0; r=mkdir(nm,0755); printf("mkdir: %s\n", r==0?"OK":strerror(errno));
}
int main(void){
    char t[]="/tmp/p8.XXXXXX"; char*d=mkdtemp(t); if(!d)return 1; chdir(d); mkdir("d",0755);
    puts("-- undecodable units, 300 repetitions (well over NAME_MAX either way) --");
    run("ED A0 80 (UTF-16 surrogate enc)","\xed\xa0\x80",300);
    run("E0 80 81 (overlong 3-byte)","\xe0\x80\x81",300);
    run("C0 80 (overlong NUL)","\xc0\x80",300);
    run("F5 80 80 80 (above U+10FFFF)","\xf5\x80\x80\x80",300);
    run("FF (not a lead byte)","\xff",300);
    run("FE (not a lead byte)","\xfe",300);
    run("80 (bare continuation)","\x80",300);
    run("E4 B8 (truncated seq)","\xe4\xb8",300);
    puts("-- the same units, only 10 repetitions (well UNDER NAME_MAX) --");
    run("ED A0 80","\xed\xa0\x80",10);
    run("E0 80 81","\xe0\x80\x81",10);
    run("FF","\xff",10);
    puts("-- controls --");
    run("'a' (valid ASCII)","a",300);
    run("E4 B8 AD (valid CJK)","\xe4\xb8\xad",300);
    chdir("/tmp"); { char c[256]; snprintf(c,sizeof c,"rm -rf '%s'",d); system(c);} return 0;
}
