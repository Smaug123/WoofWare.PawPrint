// Measures what Linux's fstatat(2) makes of AT_EMPTY_PATH, which
// at-dirfd.c measured only as a flag fstatat accepts.
//
// Sections:
//   EMPTY  for each kind of dirfd: fstatat(fd, P, AT_EMPTY_PATH | F) with P
//          the empty path or NULL, and F 0 or AT_SYMLINK_NOFOLLOW. Each
//          answer is printed as its errno, or as "same" or "differs" by
//          comparing its struct stat byte for byte against fstat(fd), or
//          against stat(".") for AT_FDCWD.
//   PATH   for each kind of dirfd and the pathnames "f", ".", "nx" and
//          "/" + the cell's "f": whether AT_EMPTY_PATH changes the answer of
//          a non-empty path. Printed as errno-or-"ok" without the flag, "/",
//          and with it, plus "differs" if both succeeded and their struct
//          stats did not agree.
//
// Each cell runs in a forked child, in a fresh directory of its own, under
// alarm(5). The rows are run as root and again as uid 1000, which drops in
// the child before it makes anything.
// Linux only, as root:
//   container run --rm --cap-add ALL -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/fstatat-empty-path.c && /tmp/p "$(mktemp -d)" > /probe/fstatat-empty-path.linux-6.18.5-aarch64.txt'
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>

static const char *en(int e) {
    static char buf[32];
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static uid_t caller;
static const char *volatile null_path = NULL;
static char cellpath[PATH_MAX];

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static const char *kinds[] = {
    "cwd", "minus1", "closed", "dir", "file", "pipe-read", "pipe-write", "socket",
    "epoll", "devnull", "orphan", "unlinked", "locked", "moved",
};
#define NKINDS (int)(sizeof kinds / sizeof kinds[0])

// The cell, as the cwd: f (0644), d/ holding f, gone/ and g (0644). Returns
// the dirfd of kind k, made as at-dirfd.c makes it.
static int fixture(int k) {
    snprintf(cellpath, sizeof cellpath, "%s/c%05d", base, cellno);
    if (mkdir(cellpath, 0777) < 0 || chmod(cellpath, 0777) < 0) die("mkdir cell");
    if (caller != 0 && (setresgid(caller, caller, caller) < 0 || setresuid(caller, caller, caller) < 0)) die("drop");
    if (chdir(cellpath) < 0) die("chdir cell");
    int fd = open("f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("f");
    close(fd);
    fd = open("g", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("g");
    close(fd);
    if (mkdir("d", 0755) < 0 || mkdir("gone", 0755) < 0) die("mkdir");
    fd = open("d/f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("d/f");
    close(fd);
    int p[2], s;
    switch (k) {
    case 0: return AT_FDCWD;
    case 1: return -1;
    case 2: return 999;
    case 3: return open("d", O_RDONLY | O_DIRECTORY);
    case 4: return open("f", O_RDONLY);
    case 5: if (pipe(p) < 0) die("pipe"); return p[0];
    case 6: if (pipe(p) < 0) die("pipe"); return p[1];
    case 7: s = socket(AF_UNIX, SOCK_STREAM, 0); if (s < 0) die("socket"); return s;
    case 8: s = epoll_create1(0); if (s < 0) die("epoll"); return s;
    case 9: return open("/dev/null", O_RDONLY);
    case 10: fd = open("gone", O_RDONLY | O_DIRECTORY); if (rmdir("gone") < 0) die("rmdir"); return fd;
    case 11: fd = open("g", O_RDONLY); if (unlink("g") < 0) die("unlink"); return fd;
    case 12: fd = open("d", O_RDONLY | O_DIRECTORY); if (chmod("d", 0) < 0) die("chmod"); return fd;
    case 13: fd = open("d", O_RDONLY | O_DIRECTORY); if (rename("d", "d-moved") < 0) die("rename"); return fd;
    }
    return -1;
}

static int reference(int fd, struct stat *st) {
    if (fd == AT_FDCWD) return stat(".", st) < 0 ? errno : 0;
    return fstat(fd, st) < 0 ? errno : 0;
}

static void in_child(void (*body)(int, int), int k, int row) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(5);
        body(k, row);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

// --------------------------------------------------------------------- EMPTY
static const char *emptylabel[] = {"\"\"", "NULL", "\"\" NOFOLLOW", "NULL NOFOLLOW"};

static void empty_body(int k, int row) {
    int fd = fixture(k);
    struct stat got, want;
    memset(&got, 0xA5, sizeof got);
    memset(&want, 0xA5, sizeof want);
    const char *p = (row % 2 == 0) ? "" : null_path;
    int flags = AT_EMPTY_PATH | (row >= 2 ? AT_SYMLINK_NOFOLLOW : 0);
    int r = fstatat(fd, p, &got, flags) < 0 ? errno : 0;
    if (r != 0) {
        printf("%s", en(r));
        return;
    }
    int w = reference(fd, &want);
    if (w != 0)
        printf("ok(reference %s)", en(w));
    else
        printf("%s", memcmp(&got, &want, sizeof got) == 0 ? "same" : "differs");
}

// ---------------------------------------------------------------------- PATH
static const char *pathlabel[] = {"f", ".", "nx", "rooted f"};

static void path_body(int k, int row) {
    int fd = fixture(k);
    char rooted[PATH_MAX + 8];
    snprintf(rooted, sizeof rooted, "%s/f", cellpath);
    const char *p = row == 0 ? "f" : row == 1 ? "." : row == 2 ? "nx" : rooted;
    struct stat a, b;
    memset(&a, 0xA5, sizeof a);
    memset(&b, 0xA5, sizeof b);
    int ra = fstatat(fd, p, &a, 0) < 0 ? errno : 0;
    int rb = fstatat(fd, p, &b, AT_EMPTY_PATH) < 0 ? errno : 0;
    printf("%s/%s", en(ra), en(rb));
    if (ra == 0 && rb == 0 && memcmp(&a, &b, sizeof a) != 0) printf(" differs");
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch>\n", argv[0]);
        return 2;
    }
    if (geteuid() != 0) {
        fprintf(stderr, "run as root\n");
        return 2;
    }
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\n", u.sysname, u.release, u.machine, (int)geteuid());
    const uid_t callers[] = {0, 1000};
    for (int c = 0; c < 2; c++) {
        caller = callers[c];
        for (int k = 0; k < NKINDS; k++) {
            printf("EMPTY\tcaller=%d\t%s", (int)caller, kinds[k]);
            for (int row = 0; row < 4; row++) {
                printf("\t%s=", emptylabel[row]);
                in_child(empty_body, k, row);
            }
            printf("\n");
        }
        for (int k = 0; k < NKINDS; k++) {
            printf("PATH\tcaller=%d\t%s", (int)caller, kinds[k]);
            for (int row = 0; row < 4; row++) {
                printf("\t%s=", pathlabel[row]);
                in_child(path_body, k, row);
            }
            printf("\n");
        }
    }
    return 0;
}
