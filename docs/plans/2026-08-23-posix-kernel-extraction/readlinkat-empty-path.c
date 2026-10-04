// Measures what readlinkat(2) makes of an empty path when dirfd names a
// symbolic link itself, which at-dirfd.c could not open: Linux through
// O_PATH | O_NOFOLLOW, Darwin through O_SYMLINK. Both flavours also get the
// descriptor kinds at-dirfd.c tried, against an empty path and against
// the size screen.
//
// Rows:
//   EMPTY  readlinkat(fd, "", buf, 64) for each kind of fd, printed as the
//          errno, or "ok:" and the bytes read.
//   SIZE   the same with a size of 0, and of -1 (cast to size_t on Linux;
//          Darwin's is a size_t too), for a bad fd and a link fd: whether
//          the size is screened before the dirfd.
//   NULL   readlinkat(AT_FDCWD, NULL, buf, size) for sizes 0, -1 and 64:
//          whether the size is screened before the path is copied in.
//
// Each cell runs in a forked child in a fresh directory of its own.
// Linux, as root (each cell drops to uid 1000 in its child too):
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/readlinkat-empty-path.c && /tmp/p "$(mktemp -d)" > /probe/readlinkat-empty-path.linux-6.18.5-aarch64.txt'
// Darwin, as an ordinary user:
//   nix develop -c clang -Wall -Werror -o readlinkat-empty-path readlinkat-empty-path.c && ./readlinkat-empty-path "$(mktemp -d /private/tmp/rl.XXXXXX)" > readlinkat-empty-path.darwin-27.0-uid501.txt
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __linux__
#define LINK_OPEN (O_PATH | O_NOFOLLOW)
#else
#define LINK_OPEN (O_SYMLINK | O_RDONLY)
#endif

static const char *en(int e) {
    static char buf[32];
    switch (e) {
    case EACCES: return "EACCES";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case ELOOP: return "ELOOP";
    case ENOTSUP: return "ENOTSUP";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static uid_t caller;

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static const char *kinds[] = {"cwd", "minus1", "dir", "file", "pipe", "socket", "link", "dangling-link", "dir-link"};
#define NKINDS (int)(sizeof kinds / sizeof kinds[0])

// The cell, as the cwd: f (0644), d/, l -> f, dl -> nx, ld -> d.
static int fixture(int k) {
    char c[PATH_MAX];
    snprintf(c, sizeof c, "%s/c%05d", base, cellno);
    if (mkdir(c, 0777) < 0 || chmod(c, 0777) < 0) die("mkdir cell");
    if (caller != 0 && (setgid(caller) < 0 || setuid(caller) < 0)) die("drop");
    if (chdir(c) < 0) die("chdir cell");
    int fd = open("f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("f");
    close(fd);
    if (mkdir("d", 0755) < 0) die("d");
    if (symlink("f", "l") < 0 || symlink("nx", "dl") < 0 || symlink("d", "ld") < 0) die("symlink");
    int p[2], s;
    switch (k) {
    case 0: return AT_FDCWD;
    case 1: return -1;
    case 2: return open("d", O_RDONLY | O_DIRECTORY);
    case 3: return open("f", O_RDONLY);
    case 4: if (pipe(p) < 0) die("pipe"); return p[0];
    case 5: s = socket(AF_UNIX, SOCK_STREAM, 0); if (s < 0) die("socket"); return s;
    case 6: fd = open("l", LINK_OPEN); if (fd < 0) die("open l"); return fd;
    case 7: fd = open("dl", LINK_OPEN); if (fd < 0) die("open dl"); return fd;
    case 8: fd = open("ld", LINK_OPEN); if (fd < 0) die("open ld"); return fd;
    }
    return -1;
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

static void show(ssize_t r, const char *buf) {
    if (r < 0)
        printf("%s", en(errno));
    else
        printf("ok:%.*s", (int)r, buf);
}

static void empty_body(int k, int row) {
    (void)row;
    int fd = fixture(k);
    char buf[64];
    show(readlinkat(fd, "", buf, sizeof buf), buf);
}

static const size_t sizes[] = {0, (size_t)-1, 64};
static void size_body(int k, int row) {
    int fd = fixture(k);
    char buf[64];
    show(readlinkat(fd, "", buf, sizes[row]), buf);
}

static const char *volatile null_path = NULL;
static void null_body(int k, int row) {
    (void)k;
    fixture(0);
    char buf[64];
    show(readlinkat(AT_FDCWD, null_path, buf, sizes[row]), buf);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <scratch>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    if (chmod(base, 0755) < 0) die("chmod base");
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\n", u.sysname, u.release, u.machine, (int)geteuid());
    uid_t callers[] = {geteuid(), 1000};
    int ncallers = geteuid() == 0 ? 2 : 1;
    for (int c = 0; c < ncallers; c++) {
        caller = c == 0 ? 0 : callers[c];
        if (geteuid() != 0) caller = 0; // already unprivileged: no drop
        int shown = c == 0 ? (int)geteuid() : (int)callers[c];
        printf("EMPTY\tcaller=%d", shown);
        for (int k = 0; k < NKINDS; k++) {
            printf("\t%s=", kinds[k]);
            in_child(empty_body, k, 0);
        }
        printf("\n");
        for (int row = 0; row < 2; row++) {
            printf("SIZE\tcaller=%d\tsize=%s", shown, row == 0 ? "0" : "-1");
            int ks[] = {1, 6};
            for (int i = 0; i < 2; i++) {
                printf("\t%s=", kinds[ks[i]]);
                in_child(size_body, ks[i], row);
            }
            printf("\n");
        }
        printf("NULL\tcaller=%d", shown);
        for (int row = 0; row < 3; row++) {
            printf("\tsize=%s=", row == 0 ? "0" : row == 1 ? "-1" : "64");
            in_child(null_body, 0, row);
        }
        printf("\n");
    }
    return 0;
}
