/* Produces the plan's §1.5 classification.
 * Prefixing 300 ASCII characters separates the two limits for a 3-byte unit:
 * the name is then 301 characters (over the 255-unit cap) but only ~303 bytes
 * (under the 765-byte cap), so ENAMETOOLONG means it was counted in units and
 * ENOENT means it was counted in bytes.
 * Result: the unit-counted set is exactly strictly-valid UTF-8. Note U+FFFF
 * is counted in units despite APFS refusing to *bind* it -- the length rule
 * and the admissibility rule (§1.1) are independent.
 * Run: cc -o p darwin-name-max-which-unit.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
/* Disambiguate: a 3-byte unit makes "255 UTF-16 units" and "765 raw bytes" flip
   at the same repetition count. Prefix with 300 ASCII to break the tie: that is
   301 units (over the 255-unit cap) but only 303 bytes (under the 765 cap). */
static void t(const char*label,const char*tail,int tl){
    char nm[4096]; int k=0; nm[k++]='d'; nm[k++]='/';
    memset(nm+k,'a',300); k+=300; memcpy(nm+k,tail,tl); k+=tl; nm[k]=0;
    errno=0; int r=open(nm,O_RDONLY); if(r>=0)close(r);
    printf("  300'a' + %-26s %3d bytes, %3d units-if-scannable : %s\n",
           label,k-2,301,r>=0?"OK":strerror(errno));
}
int main(void){
    char p[]="/tmp/pa.XXXXXX"; char*d=mkdtemp(p); if(!d)return 1; chdir(d); mkdir("d",0755);
    puts("ENAMETOOLONG => counted in UTF-16 units (scannable)");
    puts("ENOENT       => counted in raw bytes    (not scannable)\n");
    t("E4 B8 AD (valid CJK)","\xe4\xb8\xad",3);
    t("F0 9F 98 80 (valid emoji)","\xf0\x9f\x98\x80",4);
    t("ED A0 80 (surrogate enc)","\xed\xa0\x80",3);
    t("E0 80 81 (overlong 3-byte)","\xe0\x80\x81",3);
    t("C0 80 (overlong 2-byte)","\xc0\x80",2);
    t("F5 80 80 80 (>U+10FFFF)","\xf5\x80\x80\x80",4);
    t("FF (not a lead byte)","\xff",1);
    t("E4 B8 (truncated)","\xe4\xb8",2);
    t("EF BF BF (U+FFFF noncharacter)","\xef\xbf\xbf",3);
    chdir("/tmp"); { char c[256]; snprintf(c,sizeof c,"rm -rf '%s'",d); system(c);} return 0;
}
