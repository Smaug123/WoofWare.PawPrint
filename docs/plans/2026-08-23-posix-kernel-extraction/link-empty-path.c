// Measures two things about Linux's linkat(2) with AT_EMPTY_PATH that
// link-rules.c's EMPTY section did not.
//
// Sections:
//   CRED      whose credentials AT_EMPTY_PATH compares. Without
//             CAP_DAC_READ_SEARCH, the descriptor's open-time credentials must
//             be the caller's own. Every row is called as uid 1000, except the
//             last two, which root calls through a descriptor that uid 1000
//             opened. The rows cover:
//             - a dirfd, a cwd or a file that root opened, against an empty
//               path, a relative path and a rooted one;
//             - a dirfd the caller opened itself;
//             - one it opened before a credential change that keeps every ID
//               (prctl(PR_SET_KEEPCAPS));
//             - one it opened before a fork.
//   UNLINKED  where an unlinked file's ENOENT falls among the destination's
//             failures, for a descriptor the caller opened itself. Called as
//             root and as uid 1000.
//
// Each cell runs in a forked child, in a fresh directory of its own, under
// alarm(5). Linux only, as root:
//   container run --rm --cap-add ALL -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/link-empty-path.c && /tmp/p "$(mktemp -d)" > /probe/link-empty-path.linux-6.18.5-aarch64-root.txt'
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
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
    case EEXIST: return "EEXIST";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EFAULT: return "EFAULT";
    case EXDEV: return "EXDEV";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static int cellno;
static char overlong[PATH_MAX + 1];
static const char *volatile null_path = NULL;

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static int rc(int r) { return r < 0 ? errno : 0; }

static void mkfile(const char *p, mode_t mode, uid_t owner) {
    int fd = open(p, O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (fd < 0) die(p);
    close(fd);
    if (chmod(p, mode) < 0 || chown(p, owner, owner) < 0) die(p);
}

static void drop(uid_t uid) {
    if (uid == 0) return;
    if (setresgid(uid, uid, uid) < 0 || setresuid(uid, uid, uid) < 0) die("drop");
}

// The cell, entered as the cwd, 0777: f, g (0644, the caller's), u/ (0555, the
// caller's), d/ (0777, the caller's). Returns its absolute path.
static const char *fixture(uid_t caller) {
    static char c[PATH_MAX];
    snprintf(c, sizeof c, "%s/c%05d", base, cellno);
    if (mkdir(c, 0777) < 0 || chmod(c, 0777) < 0) die("mkdir cell");
    if (chdir(c) < 0) die("chdir cell");
    mkfile("f", 0644, caller);
    mkfile("g", 0644, caller);
    if (mkdir("u", 0555) < 0 || chmod("u", 0555) < 0 || chown("u", caller, caller) < 0) die("u");
    if (mkdir("d", 0777) < 0 || chmod("d", 0777) < 0 || chown("d", caller, caller) < 0) die("d");
    return c;
}

static void in_child(void (*body)(int), int row) {
    cellno++;
    fflush(stdout);
    pid_t pid = fork();
    if (pid < 0) die("fork");
    if (pid == 0) {
        alarm(5);
        body(row);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("child-died(%d)", status);
}

// ------------------------------------------------------------------- CRED
static const char *credlabel[] = {
    "root's dirfd, \"f\"",
    "root's dirfd, \"f\", flags 0",
    "root's dirfd, \"\"",
    "root's dirfd, rooted path to f",
    "root's cwd (AT_FDCWD), \"f\"",
    "root's cwd (AT_FDCWD), \"\"",
    "root's file descriptor, \"\"",
    "root's file descriptor, \"x\"",
    "own dirfd, \"f\"",
    "own file descriptor, \"\"",
    "own dirfd opened before PR_SET_KEEPCAPS, \"f\"",
    "own file descriptor opened before PR_SET_KEEPCAPS, \"\"",
    "own dirfd opened before fork, \"f\" in the child",
    "own file descriptor opened before fork, \"\" in the child",
    "root calling: uid 1000's dirfd, \"f\"",
    "root calling: uid 1000's file descriptor, \"\"",
};
#define NCRED (int)(sizeof credlabel / sizeof credlabel[0])

static void cred_body(int row) {
    const uid_t user = 1000;
    const char *cell = fixture(user);
    char rooted[PATH_MAX];
    snprintf(rooted, sizeof rooted, "%s/f", cell);
    int dirfd = -1, filefd = -1, r = -1;
    if (row < 8) {
        // Opened as root; the cwd is root's too.
        dirfd = open(cell, O_RDONLY | O_DIRECTORY);
        filefd = open("f", O_RDONLY);
        if (dirfd < 0 || filefd < 0) die("open as root");
        drop(user);
    } else if (row < 14) {
        drop(user);
        dirfd = open(cell, O_RDONLY | O_DIRECTORY);
        filefd = open("f", O_RDONLY);
        if (dirfd < 0 || filefd < 0) die("open as user");
        if (row == 10 || row == 11)
            if (prctl(PR_SET_KEEPCAPS, 1, 0, 0, 0) < 0) die("PR_SET_KEEPCAPS");
    } else {
        // uid 1000 opens; root calls.
        if (setresuid(-1, user, 0) < 0) die("seteuid user");
        dirfd = open(cell, O_RDONLY | O_DIRECTORY);
        filefd = open("f", O_RDONLY);
        if (dirfd < 0 || filefd < 0) die("open as user for root");
        if (setresuid(-1, 0, -1) < 0) die("seteuid root");
    }
    if (row == 12 || row == 13) {
        // Answer from a grandchild, after its fork.
        fflush(stdout);
        pid_t pid = fork();
        if (pid < 0) die("fork");
        if (pid != 0) {
            int status;
            if (waitpid(pid, &status, 0) < 0) die("waitpid");
            if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) printf("grandchild-died(%d)", status);
            return;
        }
    }
    switch (row) {
    case 0: r = rc(linkat(dirfd, "f", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 1: r = rc(linkat(dirfd, "f", AT_FDCWD, "n", 0)); break;
    case 2: r = rc(linkat(dirfd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 3: r = rc(linkat(dirfd, rooted, AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 4: r = rc(linkat(AT_FDCWD, "f", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 5: r = rc(linkat(AT_FDCWD, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 6: r = rc(linkat(filefd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 7: r = rc(linkat(filefd, "x", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 8: case 10: case 12: case 14: r = rc(linkat(dirfd, "f", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 9: case 11: case 13: case 15: r = rc(linkat(filefd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    }
    printf("%s", en(r));
    if (row == 12 || row == 13) {
        fflush(stdout);
        _exit(0);
    }
}

// --------------------------------------------------------------- UNLINKED
static const char *unlinkedlabel[] = {
    "\"n\"", "taken name \"f\"", "NULL", "\"\"", "PATH_MAX bytes", "newdirfd -1, \"n\"",
    "\"/dev/n\"", "unwritable \"u/n\"", "\"f/n\"", "\"n/\"", "\"nx/n\"",
};
#define NUNLINKED (int)(sizeof unlinkedlabel / sizeof unlinkedlabel[0])
static uid_t unlinked_caller;

static void unlinked_body(int row) {
    fixture(unlinked_caller);
    drop(unlinked_caller);
    int fd = open("g", O_RDONLY);
    if (fd < 0) die("open g");
    if (unlink("g") < 0) die("unlink g");
    int r = -1;
    switch (row) {
    case 0: r = rc(linkat(fd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 1: r = rc(linkat(fd, "", AT_FDCWD, "f", AT_EMPTY_PATH)); break;
    case 2: r = rc(linkat(fd, "", AT_FDCWD, null_path, AT_EMPTY_PATH)); break;
    case 3: r = rc(linkat(fd, "", AT_FDCWD, "", AT_EMPTY_PATH)); break;
    case 4: r = rc(linkat(fd, "", AT_FDCWD, overlong, AT_EMPTY_PATH)); break;
    case 5: r = rc(linkat(fd, "", -1, "n", AT_EMPTY_PATH)); break;
    case 6: r = rc(linkat(fd, "", AT_FDCWD, "/dev/n", AT_EMPTY_PATH)); unlink("/dev/n"); break;
    case 7: r = rc(linkat(fd, "", AT_FDCWD, "u/n", AT_EMPTY_PATH)); break;
    case 8: r = rc(linkat(fd, "", AT_FDCWD, "f/n", AT_EMPTY_PATH)); break;
    case 9: r = rc(linkat(fd, "", AT_FDCWD, "n/", AT_EMPTY_PATH)); break;
    case 10: r = rc(linkat(fd, "", AT_FDCWD, "nx/n", AT_EMPTY_PATH)); break;
    }
    printf("%s", en(r));
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
    // mktemp -d makes it 0700, which uid 1000 cannot search.
    if (chmod(base, 0755) < 0) die("chmod base");
    memset(overlong, 'a', PATH_MAX);
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\n", u.sysname, u.release, u.machine, (int)geteuid());
    for (int row = 0; row < NCRED; row++) {
        printf("CRED\t%s\t", credlabel[row]);
        in_child(cred_body, row);
        printf("\n");
    }
    const uid_t callers[] = {1000, 0};
    for (int c = 0; c < 2; c++) {
        unlinked_caller = callers[c];
        for (int row = 0; row < NUNLINKED; row++) {
            printf("UNLINKED\tcaller=%d\t%s\t", (int)callers[c], unlinkedlabel[row]);
            in_child(unlinked_body, row);
            printf("\n");
        }
    }
    return 0;
}
