/* Produces the plan's §1.5 boundary table.
 * Bisects, per repeated unit, the repetition count at which a lookup flips
 * ENOENT -> ENAMETOOLONG. The emoji row is the decisive one: 4 bytes and 2
 * UTF-16 units, it flips at 128 reps = 512 bytes = 256 units, so the limit
 * that bound it counts units and not bytes.
 * A 3-byte unit cannot distinguish the two limits (255 units and 765 bytes
 * flip at the same count); darwin-name-max-which-unit.c breaks that tie.
 * Run: cc -o p darwin-name-max-boundary-per-unit.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
/* Where exactly does each unit type flip ENOENT -> ENAMETOOLONG?
   Separates "bytes" from "UTF-16 units" from "scannable". */
static int tooLong(const char*unit,int ul,int n){
    char nm[8192]; int k=0; nm[k++]='d'; nm[k++]='/';
    for(int i=0;i<n;i++){ memcpy(nm+k,unit,ul); k+=ul; }
    nm[k]=0;
    errno=0; int r=open(nm,O_RDONLY); if(r>=0){close(r);return 0;}
    return errno==ENAMETOOLONG;
}
static void bisect(const char*label,const char*unit,int ul,int u16){
    int lo=1, hi=2000;
    if(!tooLong(unit,ul,hi)){ printf("  %-32s never ENAMETOOLONG up to %d reps\n",label,hi); return; }
    while(lo<hi){ int m=(lo+hi)/2; if(tooLong(unit,ul,m)) hi=m; else lo=m+1; }
    printf("  %-32s flips at %4d reps = %4d bytes, %4d UTF-16 units (if scanned)\n",
           label,lo,lo*ul,lo*u16);
}
int main(void){
    char t[]="/tmp/p9.XXXXXX"; char*d=mkdtemp(t); if(!d)return 1; chdir(d); mkdir("d",0755);
    puts("-- boundary per repeated unit (ul = bytes/unit, u16 = UTF-16 units/unit if scannable) --");
    bisect("'a'            ul=1 u16=1","a",1,1);
    bisect("E4 B8 AD (CJK) ul=3 u16=1","\xe4\xb8\xad",3,1);
    bisect("F0 9F 98 80 (emoji) ul=4 u16=2","\xf0\x9f\x98\x80",4,2);
    bisect("ED A0 80 (surrogate) ul=3","\xed\xa0\x80",3,1);
    bisect("E0 80 81 (overlong3) ul=3","\xe0\x80\x81",3,1);
    bisect("F5 80 80 80 (>10FFFF) ul=4","\xf5\x80\x80\x80",4,2);
    bisect("C0 80 (overlong2) ul=2","\xc0\x80",2,1);
    bisect("FF (not lead) ul=1","\xff",1,1);
    bisect("80 (continuation) ul=1","\x80",1,1);
    bisect("E4 B8 (truncated) ul=2","\xe4\xb8",2,1);
    puts("-- Codex's mixed case: 300 x 'a' then one bad unit --");
    { char nm[4096]; int k=0; nm[k++]='d'; nm[k++]='/';
      memset(nm+k,'a',300); k+=300; memcpy(nm+k,"\xed\xa0\x80",3); k+=3; nm[k]=0;
      errno=0; int r=open(nm,O_RDONLY); printf("  300'a' + ED A0 80 (%d bytes): %s\n",k-2,r>=0?"OK":strerror(errno)); }
    { char nm[4096]; int k=0; nm[k++]='d'; nm[k++]='/';
      memset(nm+k,'a',300); k+=300; memcpy(nm+k,"\xe4\xb8\xad",3); k+=3; nm[k]=0;
      errno=0; int r=open(nm,O_RDONLY); printf("  300'a' + E4 B8 AD (%d bytes): %s\n",k-2,r>=0?"OK":strerror(errno)); }
    { char nm[4096]; int k=0; nm[k++]='d'; nm[k++]='/';
      memset(nm+k,'a',300); k+=300; nm[k++]=(char)0xff; nm[k]=0;
      errno=0; int r=open(nm,O_RDONLY); printf("  300'a' + FF       (%d bytes): %s\n",k-2,r>=0?"OK":strerror(errno)); }
    chdir("/tmp"); { char c[256]; snprintf(c,sizeof c,"rm -rf '%s'",d); system(c);} return 0;
}
