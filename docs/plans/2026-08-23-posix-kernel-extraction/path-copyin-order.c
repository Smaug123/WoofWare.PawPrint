// Measures where each single-pathname call copies its pathname in, relative to
// the call's other checks: which of EFAULT (an unreadable pointer),
// ENAMETOOLONG (no NUL within PATH_MAX bytes) and ENOENT (the empty path) a
// call answers, and whether any other argument is screened first.
//
// Every call goes through libc, which is what a client of the kernel sees:
// open, mkdir, unlink, rmdir, chdir, chmod, chown, lchown, stat, lstat,
// readlink, statfs and opendir(3). Each is made in a forked child (a call that
// moves the cwd, or a libc that dies on a NULL, cannot disturb the next row),
// under alarm(5).
//
// The sweep: each call, with each of its other arguments set both to a value
// the call accepts and to every value it screens (an open flag word Linux
// rejects, a non-positive readlink size, a NULL output buffer, a mode word with
// every bit set), crossed with seven pathnames:
//   NULL       the null pointer
//   PROT_NONE  a mapped page the caller may not read
//   overlong   PATH_MAX bytes of "a/a/..." and then a NUL
//   inside     PATH_MAX-1 bytes of "a/a/...", so the walk answers it
//   empty      ""
//   nx         a name that does not exist
//   f          an existing regular file the caller owns
// and, for readlink, a symbolic link "l" as well.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/path-copyin-order.c && /tmp/p "$(mktemp -d)"'
// Darwin: nix develop -c clang -Wall -o path-copyin-order path-copyin-order.c && ./path-copyin-order "$(mktemp -d /private/tmp/copyin.XXXXXX)"
//
// Measured on Linux 6.18.5 (aarch64, root in the container, glibc) and Darwin
// 27.0 (arm64, uid 501) on 2026-10-02; the output of each is beside this file.
// The rows are transcribed in WoofWare.PosixKernel.Test/TestPathCopyIn.fs.
//
// What they say: every call copies its pathname in before it looks at
// anything else, and the empty path is ENOENT on every call, with three
// exceptions, each a screen of another argument made first: open(2)'s flag
// word (O_CREAT|O_DIRECTORY is EINVAL on both; Darwin also rejects an access
// mode of 3, which Linux accepts), and readlink(2)'s size (any size below 1 on
// Linux, a negative one on Darwin). An output buffer (stat, lstat, statfs,
// readlink) is looked at only after the walk. glibc's opendir(3) reads the
// name's first byte itself before any syscall, so a NULL or unreadable name
// kills the process with SIGSEGV there, where Darwin's answers EFAULT.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/statfs.h>
#endif

static void die(const char *what) {
    perror(what);
    exit(2);
}

static const char *en(int e) {
    if (e == 0) return "ok";
    switch (e) {
    case EACCES: return "EACCES";
    case EPERM: return "EPERM";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case EISDIR: return "EISDIR";
    case EEXIST: return "EEXIST";
    case ELOOP: return "ELOOP";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EFAULT: return "EFAULT";
    case ENOTEMPTY: return "ENOTEMPTY";
    case EBUSY: return "EBUSY";
    default: return strerror(e);
    }
}

// The pathnames, by name. `paths` is filled in by `main`.
#define NPATHS 7
static const char *path_names[NPATHS] = {"NULL", "PROT_NONE", "overlong", "inside", "empty", "nx", "f"};
static const char *paths[NPATHS];

// What one call does to one pathname. Returns 0 or an errno; a call that
// returns a descriptor or a DIR* closes it.
typedef int (*call_fn)(const char *pth, long arg);

static int rc(int r) { return r < 0 ? errno : 0; }

static int c_open(const char *pth, long flags) {
    int fd = open(pth, (int)flags, 0644);
    if (fd < 0) return errno;
    close(fd);
    return 0;
}
static int c_mkdir(const char *pth, long mode) { return rc(mkdir(pth, (mode_t)mode)); }
static int c_unlink(const char *pth, long _) { return rc(unlink(pth)); }
static int c_rmdir(const char *pth, long _) { return rc(rmdir(pth)); }
static int c_chdir(const char *pth, long _) { return rc(chdir(pth)); }
static int c_chmod(const char *pth, long mode) { return rc(chmod(pth, (mode_t)mode)); }
static int c_chown(const char *pth, long uid) { return rc(chown(pth, (uid_t)uid, (gid_t)-1)); }
static int c_lchown(const char *pth, long uid) { return rc(lchown(pth, (uid_t)uid, (gid_t)-1)); }
static int c_stat(const char *pth, long null_buf) {
    struct stat st;
    return rc(stat(pth, null_buf ? NULL : &st));
}
static int c_lstat(const char *pth, long null_buf) {
    struct stat st;
    return rc(lstat(pth, null_buf ? NULL : &st));
}
static char rlbuf[64];
static int c_readlink(const char *pth, long size) {
    ssize_t r = readlink(pth, size == 999 ? NULL : rlbuf, size == 999 ? 16 : (size_t)size);
    return r < 0 ? errno : 0;
}
static int c_statfs(const char *pth, long null_buf) {
    struct statfs st;
    return rc(statfs(pth, null_buf ? NULL : &st));
}
static int c_opendir(const char *pth, long _) {
    DIR *d = opendir(pth);
    if (!d) return errno;
    closedir(d);
    return 0;
}

// One row: the call made on each pathname in a child of its own.
static void row(const char *call, const char *argdesc, call_fn fn, long arg, const char *extra_path, const char *extra_name) {
    printf("%s\t%s", call, argdesc);
    int n = extra_path ? NPATHS + 1 : NPATHS;
    for (int i = 0; i < n; i++) {
        const char *pth = i < NPATHS ? paths[i] : extra_path;
        const char *nm = i < NPATHS ? path_names[i] : extra_name;
        int pipefd[2];
        if (pipe(pipefd) != 0) die("pipe");
        pid_t pid = fork();
        if (pid < 0) die("fork");
        if (pid == 0) {
            close(pipefd[0]);
            alarm(5);
            // A fresh directory per child, so that what one call creates,
            // removes or changes is never what the next one sees.
            char dir[64];
            snprintf(dir, sizeof dir, "r%d", (int)getpid());
            if (mkdir(dir, 0755) != 0 || chdir(dir) != 0) _exit(4);
            int ffd = open("f", O_CREAT | O_EXCL | O_WRONLY, 0644);
            if (ffd < 0 || close(ffd) != 0 || symlink("f", "l") != 0) _exit(5);
            int e = fn(pth, arg);
            if (write(pipefd[1], &e, sizeof e) != sizeof e) _exit(3);
            _exit(0);
        }
        close(pipefd[1]);
        int e = -1;
        ssize_t got = read(pipefd[0], &e, sizeof e);
        close(pipefd[0]);
        int status;
        if (waitpid(pid, &status, 0) != pid) die("waitpid");
        if (got == sizeof e) {
            printf("\t%s=%s", nm, en(e));
        } else if (WIFSIGNALED(status)) {
            printf("\t%s=signal %d", nm, WTERMSIG(status));
        } else {
            printf("\t%s=exit %d", nm, WEXITSTATUS(status));
        }
    }
    printf("\n");
    fflush(stdout);
}

// A pathname of exactly `length` bytes, "a/a/a/...", so every component is
// short and only the whole argument's length can be what refuses it.
static char *slashed(size_t length) {
    char *s = malloc(length + 1);
    if (!s) die("malloc");
    for (size_t i = 0; i < length; i++) s[i] = (i % 2 == 0) ? 'a' : '/';
    if (length > 0 && s[length - 1] == '/') s[length - 1] = 'a';
    s[length] = 0;
    return s;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <empty directory>\n", argv[0]);
        return 2;
    }
    if (chdir(argv[1]) != 0) die("chdir base");

    struct utsname u;
    if (uname(&u) != 0) die("uname");
    printf("KERNEL\t%s %s %s\tuid=%d\tPATH_MAX=%d\n", u.sysname, u.release, u.machine, (int)geteuid(), PATH_MAX);

    long page = sysconf(_SC_PAGESIZE);
    void *none = mmap(NULL, (size_t)page, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (none == MAP_FAILED) die("mmap");

    paths[0] = NULL;
    paths[1] = (const char *)none;
    paths[2] = slashed(PATH_MAX);
    paths[3] = slashed(PATH_MAX - 1);
    paths[4] = "";
    paths[5] = "nx";
    paths[6] = "f";

    // Columns, so the reader need not count.
    printf("CALL\tARGUMENTS");
    for (int i = 0; i < NPATHS; i++) printf("\t%s", path_names[i]);
    printf("\t(extra)\n");

    row("open", "O_RDONLY", c_open, O_RDONLY, NULL, NULL);
    row("open", "O_WRONLY|O_CREAT", c_open, O_WRONLY | O_CREAT, NULL, NULL);
    row("open", "O_RDONLY|O_DIRECTORY", c_open, O_RDONLY | O_DIRECTORY, NULL, NULL);
    row("open", "O_RDONLY|O_CREAT|O_DIRECTORY", c_open, O_RDONLY | O_CREAT | O_DIRECTORY, NULL, NULL);
    row("open", "O_ACCMODE", c_open, O_ACCMODE, NULL, NULL);
    row("open", "O_RDONLY|O_TRUNC", c_open, O_RDONLY | O_TRUNC, NULL, NULL);
    row("open", "O_WRONLY|O_CREAT|O_EXCL", c_open, O_WRONLY | O_CREAT | O_EXCL, NULL, NULL);
    row("open", "all bits", c_open, -1L & 0x7fffffff, NULL, NULL);
    row("mkdir", "mode 0777", c_mkdir, 0777, NULL, NULL);
    row("mkdir", "mode all bits", c_mkdir, 0xffff, NULL, NULL);
    row("unlink", "-", c_unlink, 0, NULL, NULL);
    row("rmdir", "-", c_rmdir, 0, NULL, NULL);
    row("chdir", "-", c_chdir, 0, NULL, NULL);
    row("chmod", "mode 0644", c_chmod, 0644, NULL, NULL);
    row("chmod", "mode all bits", c_chmod, 0xffff, NULL, NULL);
    row("chown", "uid -1", c_chown, -1, NULL, NULL);
    row("chown", "uid own", c_chown, (long)geteuid(), NULL, NULL);
    row("chown", "uid 4242", c_chown, 4242, NULL, NULL);
    row("lchown", "uid -1", c_lchown, -1, NULL, NULL);
    row("lchown", "uid 4242", c_lchown, 4242, NULL, NULL);
    row("stat", "buffer", c_stat, 0, NULL, NULL);
    row("stat", "NULL buffer", c_stat, 1, NULL, NULL);
    row("lstat", "buffer", c_lstat, 0, NULL, NULL);
    row("lstat", "NULL buffer", c_lstat, 1, NULL, NULL);
    row("readlink", "size 16", c_readlink, 16, "l", "l");
    row("readlink", "size 0", c_readlink, 0, "l", "l");
    row("readlink", "size -1", c_readlink, -1, "l", "l");
    row("readlink", "NULL buffer size 16", c_readlink, 999, "l", "l");
    row("statfs", "buffer", c_statfs, 0, NULL, NULL);
    row("statfs", "NULL buffer", c_statfs, 1, NULL, NULL);
    row("opendir", "-", c_opendir, 0, NULL, NULL);
    return 0;
}
