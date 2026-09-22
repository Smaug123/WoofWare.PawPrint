/* Produces the plan's §1.3 two-by-two.
 * NAME_MAX and the encoding check are NOT adjacent: NAME_MAX precedes the
 * parent's write check and the encoding check follows it. Only the four-cell
 * table (writable/unwritable parent x decodable/undecodable name) separates
 * them; three of the four cells agree with the wrong ordering.
 * The undecodable names here are 300 bytes, which is under Darwin's 765-byte
 * limit for such names (plan §1.5) -- so the length rule passes and the cells
 * really do isolate the permission/encoding order.
 * Run: cc -o p darwin-name-max-vs-permission.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdlib.h>
#define R(l,e) do{errno=0;long r=(long)(e); if(r>=0)printf("%-58s OK\n",l); else printf("%-58s errno=%-3d %s\n",l,errno,strerror(errno));}while(0)
int main(void){
    char tmpl[] = "/tmp/p5.XXXXXX"; char *dir = mkdtemp(tmpl);
    if(!dir){perror("mkdtemp");return 1;} chdir(dir); printf("probe directory: %s\n", dir);
    mkdir("d",0755);

    puts("-- NAME_MAX vs EACCES on an unwritable parent --");
    mkdir("ro",0555);
    char lv[600], li[600];
    strcpy(lv,"ro/"); memset(lv+3,'a' ,300); lv[303]=0;   // 300 VALID bytes
    strcpy(li,"ro/"); memset(li+3,0xff,300); li[303]=0;   // 300 INVALID bytes
    R("mkdir(ro/ 300 x 'a')   [ENAMETOOLONG or EACCES?]", mkdir(lv,0755));
    R("mkdir(ro/ 300 x 0xff)  [EILSEQ or EACCES?]", mkdir(li,0755));
    R("mkdir(ro/ 1 x 0xff)    [control: 1 invalid byte]", mkdir("ro/\xff",0755));
    R("mkdir(ro/plain)        [control: EACCES]", mkdir("ro/plain",0755));

    puts("-- and on a WRITABLE parent, for contrast --");
    char wv[600], wi[600];
    strcpy(wv,"d/"); memset(wv+2,'a' ,300); wv[302]=0;
    strcpy(wi,"d/"); memset(wi+2,0xff,300); wi[302]=0;
    R("mkdir(d/ 300 x 'a')", mkdir(wv,0755));
    R("mkdir(d/ 300 x 0xff)", mkdir(wi,0755));

    puts("-- does a MISSING parent beat NAME_MAX? --");
    char nv[600]; strcpy(nv,"nodir/"); memset(nv+6,'a',300); nv[306]=0;
    R("mkdir(nodir/ 300 x 'a') [ENOENT or ENAMETOOLONG?]", mkdir(nv,0755));

    chdir("/tmp"); { char cmd[256]; snprintf(cmd,sizeof cmd,"chmod -R u+w '%s' && rm -rf '%s'",dir,dir); system(cmd);}
    return 0;
}
