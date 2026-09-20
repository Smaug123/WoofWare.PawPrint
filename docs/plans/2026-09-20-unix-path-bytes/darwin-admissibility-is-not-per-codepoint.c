/* Produces the plan's §1.1 second refutation, and the reason Stage 7 is descoped.
 * APFS admissibility is not a per-code-point property at all: it limits a
 * *combining sequence* to 32 characters. 'b' + 31 combining marks is accepted
 * and 'b' + 32 is EILSEQ; 32 bare marks are accepted and 33 are EILSEQ; 33
 * ordinary CJK characters are fine. This is XNU's decomposition limit, so no
 * table of admissible code points can express the rule.
 * Run: cc -o p darwin-admissibility-is-not-per-codepoint.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
/* Is APFS admissibility a property of the whole NAME, not of its code points? */
int main(void){
    char t[]="/tmp/p7.XXXXXX"; char*d=mkdtemp(t); if(!d)return 1; chdir(d);
    puts("-- U+0301 COMBINING ACUTE (CC 81) repeated, bare --");
    for(int n=1;n<=40;n++){
        char nm[512]; int k=0;
        for(int i=0;i<n;i++){ nm[k++]=(char)0xCC; nm[k++]=(char)0x81; } nm[k]=0;
        errno=0; int ok=mkdir(nm,0755)==0; int e=errno;
        if(n<=2||(n>=30&&n<=36)||n==40) printf("  %2d marks (%3d bytes): %s\n",n,k,ok?"OK":strerror(e));
        if(!ok&&e!=EEXIST){ if(n>2&&n<30) printf("  first refusal at n=%d (%d bytes): %s\n",n,k,strerror(e)); }
    }
    puts("-- with a base character 'a' in front --");
    for(int n=30;n<=36;n++){
        char nm[512]; int k=0; nm[k++]='b';
        for(int i=0;i<n;i++){ nm[k++]=(char)0xCC; nm[k++]=(char)0x81; } nm[k]=0;
        errno=0; int ok=mkdir(nm,0755)==0; printf("  'b' + %2d marks (%3d bytes): %s\n",n,k,ok?"OK":strerror(errno));
    }
    puts("-- does a base character RESET the run? 'a'+32marks+'a'+32marks --");
    { char nm[512]; int k=0;
      for(int r=0;r<2;r++){ nm[k++]='c'+r; for(int i=0;i<32;i++){nm[k++]=(char)0xCC;nm[k++]=(char)0x81;} }
      nm[k]=0; errno=0; printf("  two runs of 32 (%d bytes): %s\n",k,mkdir(nm,0755)==0?"OK":strerror(errno)); }
    puts("-- a DIFFERENT combining mark: U+0308 (CC 88) x 33 --");
    { char nm[512]; int k=0; for(int i=0;i<33;i++){nm[k++]=(char)0xCC;nm[k++]=(char)0x88;} nm[k]=0;
      errno=0; printf("  33 x U+0308 (%d bytes): %s\n",k,mkdir(nm,0755)==0?"OK":strerror(errno)); }
    puts("-- control: 33 ordinary CJK (not combining) --");
    { char nm[512]; int k=0; for(int i=0;i<33;i++){memcpy(nm+k,"\xe4\xb8\xad",3);k+=3;} nm[k]=0;
      errno=0; printf("  33 x U+4E2D (%d bytes): %s\n",k,mkdir(nm,0755)==0?"OK":strerror(errno)); }
    chdir("/tmp"); { char c[256]; snprintf(c,sizeof c,"rm -rf '%s'",d); system(c);} return 0;
}
