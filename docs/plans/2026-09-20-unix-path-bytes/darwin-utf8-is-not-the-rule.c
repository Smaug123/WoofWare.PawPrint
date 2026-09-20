/* Produces the plan's §1.1 refutation.
 * APFS admits strictly fewer names than strict UTF-8 does: U+FFFF and the
 * other Unicode noncharacters decode fine and are refused with EILSEQ. The
 * obvious closed form -- "accept iff not a noncharacter" -- is itself refuted
 * by U+1FFFD, which is merely unassigned and is also refused. The real
 * boundary is unmapped; see Stage 7, which opens by sweeping for it.
 * Run: cc -o p darwin-utf8-is-not-the-rule.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdlib.h>
/* Is APFS's extra rule exactly "no Unicode noncharacter"?
   Noncharacters are U+FDD0..U+FDEF and U+xFFFE/U+xFFFF in every plane. */
static void utf8(unsigned cp, char *out){
    if(cp<0x800){ out[0]=0xC0|(cp>>6); out[1]=0x80|(cp&0x3F); out[2]=0; }
    else if(cp<0x10000){ out[0]=0xE0|(cp>>12); out[1]=0x80|((cp>>6)&0x3F); out[2]=0x80|(cp&0x3F); out[3]=0; }
    else { out[0]=0xF0|(cp>>18); out[1]=0x80|((cp>>12)&0x3F); out[2]=0x80|((cp>>6)&0x3F); out[3]=0x80|(cp&0x3F); out[4]=0; }
}
static int isnonchar(unsigned cp){ return (cp>=0xFDD0&&cp<=0xFDEF)||((cp&0xFFFE)==0xFFFE); }
int main(void){
    char tmpl[]="/tmp/p6.XXXXXX"; char*dir=mkdtemp(tmpl); if(!dir)return 1; chdir(dir);
    unsigned cps[]={0xFDCF,0xFDD0,0xFDEF,0xFDF0,0xFFFD,0xFFFE,0xFFFF,0x10000,0x1FFFD,0x1FFFE,0x1FFFF,0x1F600,0x10FFFD,0x10FFFE,0x10FFFF,0xE000,0x200B};
    int mismatch=0;
    for(unsigned i=0;i<sizeof cps/sizeof*cps;i++){
        char name[16]="n"; utf8(cps[i], name+1);
        errno=0; int ok = mkdir(name,0755)==0; int e=errno;
        int pred = !isnonchar(cps[i]);          /* predicted accept */
        if(ok!=pred) mismatch=1;
        printf("U+%-6X %-9s actual=%-14s predicted=%s%s\n", cps[i],
               isnonchar(cps[i])?"noncharX":"ordinary",
               ok?"OK":strerror(e), pred?"OK":"EILSEQ", ok==pred?"":"   <-- MISMATCH");
    }
    printf("\n\"APFS accepts iff not a Unicode noncharacter\": %s\n", mismatch?"REFUTED":"holds for all 17 probed");
    chdir("/tmp"); { char c[256]; snprintf(c,sizeof c,"rm -rf '%s'",dir); system(c);} return 0;
}
