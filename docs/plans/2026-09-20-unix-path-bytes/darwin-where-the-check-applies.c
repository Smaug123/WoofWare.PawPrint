/* Produces the plan's §1.2.
 * Where APFS applies its encoding rule: on *binding* a name (EILSEQ), never on
 * reading a pathname (a lookup of an unrepresentable name is plain ENOENT).
 * Run: cc -o p darwin-where-the-check-applies.c && ./p
 * Measured: Darwin 25.6.0 / macOS 26.6, APFS.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define R(label, expr) do { errno = 0; long r = (long)(expr); \
  if (r >= 0) printf("%-46s OK (%ld)\n", label, r); \
  else printf("%-46s errno=%-3d %s\n", label, errno, strerror(errno)); } while (0)

int main(void) {
    mkdir("d", 0755);
    int fd = open("d/f", O_CREAT|O_WRONLY, 0644); if (fd>=0) close(fd);

    puts("-- non-creating lookup of a bad final component --");
    R("open(\"d/\\xff\", O_RDONLY)", open("d/\xff", O_RDONLY));
    R("access(\"d/\\xff\", F_OK)", access("d/\xff", F_OK));
    R("stat(\"d/\\xff\")", ({ struct stat st; stat("d/\xff", &st); }));
    R("readlink(\"d/\\xff\")", ({ char b[16]; readlink("d/\xff", b, 16); }));

    puts("-- bad component vs a parent that does not exist --");
    R("open(\"nodir/\\xff\", O_RDONLY)  [ENOENT or EILSEQ?]", open("nodir/\xff", O_RDONLY));
    R("open(\"nodir/\\xff\", O_CREAT)   [ENOENT or EILSEQ?]", open("nodir/\xff", O_CREAT|O_WRONLY, 0644));

    puts("-- bad NON-final component --");
    R("open(\"\\xff/f\", O_RDONLY)", open("\xff/f", O_RDONLY));
    R("open(\"\\xff/f\", O_CREAT)", open("\xff/f", O_CREAT|O_WRONLY, 0644));

    puts("-- bad component vs a parent that is a FILE (ENOTDIR?) --");
    R("open(\"d/f/\\xff\", O_RDONLY)", open("d/f/\xff", O_RDONLY));

    puts("-- other creating/removing calls --");
    R("mkdir(\"d/\\xff\")", mkdir("d/\xff", 0755));
    R("unlink(\"d/\\xff\")", unlink("d/\xff"));
    R("rmdir(\"d/\\xff\")", rmdir("d/\xff"));
    R("symlink(\"t\", \"d/\\xff\")   [link NAME is bad]", symlink("t", "d/\xff"));
    R("rename(\"d/f\", \"d/\\xff\")", rename("d/f", "d/\xff"));
    R("rename(\"d/\\xff\", \"d/g\")", rename("d/\xff", "d/g"));

    puts("-- over-long AND invalid: which wins? --");
    char big[2048]; memset(big, 'a', sizeof big); big[0]='/'; big[2047]=0; big[100]=(char)0xff;
    R("open(2047-byte path w/ 0xff) [ENAMETOOLONG?]", open(big, O_RDONLY));

    puts("-- chdir --");
    R("chdir(\"d/\\xff\")", chdir("d/\xff"));

    unlink("d/f"); rmdir("d");
    return 0;
}
