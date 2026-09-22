/* Produces Stage 7's claim that EILSEQ is the LAST refusal of every binding
 * verdict -- open(O_CREAT), mkdir and rename's destination -- not just mkdir's,
 * which is all §1.3 measured. Each row pairs an undecodable name (\xff) with one
 * earlier refusal of that verdict; the controls show each earlier refusal
 * really fires for a decodable name, and that \xff alone is EILSEQ.
 * Run: cc -o p darwin-eilseq-is-last.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS, uid 501.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdlib.h>
#define R(l,e) do{errno=0;long r=(long)(e); if(r>=0)printf("%-62s OK\n",l); else printf("%-62s errno=%-3d %s\n",l,errno,strerror(errno));}while(0)
int main(void){
    char tmpl[] = "/tmp/p7.XXXXXX"; char *dir = mkdtemp(tmpl);
    if(!dir){perror("mkdtemp");return 1;} chdir(dir); printf("probe directory: %s\n", dir);
    mkdir("d",0755); close(open("d/f",O_CREAT|O_WRONLY,0644));
    mkdir("ro",0555);
    mkdir("rosrc",0755); close(open("rosrc/f",O_CREAT|O_WRONLY,0644)); chmod("rosrc",0555);
    mkdir("a",0755); mkdir("a/b",0755);
    mkdir("m",0555); mkdir("q",0755);

    puts("-- open(O_CREAT) --");
    R("open(d/\\xff, O_CREAT)            [control: EILSEQ]", open("d/\xff",O_CREAT|O_WRONLY,0644));
    R("open(ro/g, O_CREAT)               [control: EACCES]", open("ro/g",O_CREAT|O_WRONLY,0644));
    R("open(ro/\\xff, O_CREAT)           [EACCES or EILSEQ?]", open("ro/\xff",O_CREAT|O_WRONLY,0644));
    R("open(d/g/, O_CREAT)               [control: ENOENT]", open("d/g/",O_CREAT|O_WRONLY,0644));
    R("open(d/\\xff/, O_CREAT)           [ENOENT or EILSEQ?]", open("d/\xff/",O_CREAT|O_WRONLY,0644));

    puts("-- mkdir --");
    R("mkdir(d/\\xff/)                   [EILSEQ, as §1.2]", mkdir("d/\xff/",0755));

    puts("-- rename(src, bad destination) --");
    R("rename(d/f, d/\\xff)              [control: EILSEQ]", rename("d/f","d/\xff"));
    R("rename(nx, d/\\xff)               [ENOENT or EILSEQ?]", rename("nx","d/\xff"));
    R("rename(d/f, ro/g)                 [control: EACCES]", rename("d/f","ro/g"));
    R("rename(d/f, ro/\\xff)             [EACCES or EILSEQ?]", rename("d/f","ro/\xff"));
    R("rename(rosrc/f, d/\\xff)          [EACCES or EILSEQ?]", rename("rosrc/f","d/\xff"));
    R("rename(d/f, d/g/)                 [control: ENOENT]", rename("d/f","d/g/"));
    R("rename(d/f, d/\\xff/)             [ENOENT or EILSEQ?]", rename("d/f","d/\xff/"));
    R("rename(a, a/b/g)                  [control: EINVAL]", rename("a","a/b/g"));
    R("rename(a, a/b/\\xff)              [EINVAL or EILSEQ?]", rename("a","a/b/\xff"));
    R("rename(m, q/g)                    [control: EACCES]", rename("m","q/g"));
    R("rename(m, q/\\xff)                [EACCES or EILSEQ?]", rename("m","q/\xff"));
    R("rename(a, d/\\xff/)               [directory source: EILSEQ?]", rename("a","d/\xff/"));

    puts("-- orphaned current directory --");
    mkdir("gone",0755); chdir("gone"); R("rmdir(../gone)                    [setup]", rmdir("../gone"));
    R("open(g, O_CREAT) in orphan        [control: ENOENT]", open("g",O_CREAT|O_WRONLY,0644));
    R("open(\\xff, O_CREAT) in orphan    [ENOENT or EILSEQ?]", open("\xff",O_CREAT|O_WRONLY,0644));
    R("mkdir(g) in orphan                [control: ENOENT]", mkdir("g",0755));
    R("mkdir(\\xff) in orphan            [ENOENT or EILSEQ?]", mkdir("\xff",0755));
    { char src[512]; snprintf(src,sizeof src,"%s/d/f",dir);
      R("rename(<abs d/f>, g) in orphan    [control: ENOENT]", rename(src,"g"));
      R("rename(<abs d/f>, \\xff) in orphan [ENOENT or EILSEQ?]", rename(src,"\xff")); }

    chdir("/tmp"); { char cmd[256]; snprintf(cmd,sizeof cmd,"chmod -R u+w '%s' && rm -rf '%s'",dir,dir); system(cmd);}
    return 0;
}
