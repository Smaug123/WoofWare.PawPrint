// Measures how each `*at` call treats its directory descriptor, and in what
// order that is checked against the pathname's own failures.
//
// Sections:
//   CONST   the AT_* numbers each <fcntl.h> defines.
//   AT      every call below, with the directory argument under test crossed
//           with every pathname below. Each cell is made in a forked child, in
//           a fresh fixture of its own, under alarm(5).
//   ORDER2  linkat and renameat with a bad argument on each side at once:
//           which side's failure is reported.
//   FLAGS   each flag-taking call with AT_FDCWD and an existing file, for
//           every single bit of its flag word: which bits it rejects.
//   FLAGORDER  a rejected flag bit against a NULL path, a bad dirfd and the
//           empty path: whether the flag word is screened first.
//
// Directory arguments (the dirfd kinds):
//   cwd       this flavour's AT_FDCWD
//   othercwd  the other flavour's AT_FDCWD (-2 on Linux, -100 on Darwin)
//   minus1    -1
//   closed    a descriptor number nothing holds (999)
//   dir       a directory holding f and f2
//   file      a regular file
//   pipe      the read end of a pipe
//   socket    one end of an AF_UNIX stream socketpair
//   orphan    a directory removed by rmdir after it was opened
//   moved     a directory renamed after it was opened (it holds f and f2)
//   locked    a directory chmod 0 after it was opened (it holds f and f2);
//             as root this reads as `dir`, so run the probe as a non-root
//             user too
//   devnull   /dev/null, a character device
//   eventq    an epoll instance on Linux, a kqueue on Darwin
// Pathnames:
//   f         an existing regular file in the starting directory
//   nx        a name that does not exist there
//   empty     ""
//   NULL      the null pointer
//   PROT_NONE a mapped page the caller may not read
//   overlong  PATH_MAX bytes of "a/a/..." with no NUL among them
//   abs       an absolute path to an existing regular file
//
// Every call goes through libc, as a client of the kernel sees it. glibc's
// fchmodat with a non-zero flag word calls fchmodat2, and its utimensat passes
// a NULL path through to the kernel, which then acts on the dirfd itself.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -O1 -o /tmp/p /probe/at-dirfd.c && d=$(mktemp -d) && /tmp/p "$d" && d=$(mktemp -d) && chown 1000:1000 "$d" && setpriv --reuid=1000 --regid=1000 --clear-groups /tmp/p "$d"'
// Darwin: nix develop -c clang -Wall -o at-dirfd at-dirfd.c && ./at-dirfd "$(mktemp -d /private/tmp/atdirfd.XXXXXX)"
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static const char *en(int e) {
    static char buf[32];
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
    case EXDEV: return "EXDEV";
    case ENOTEMPTY: return "ENOTEMPTY";
    case EBUSY: return "EBUSY";
    case ENOTSUP: return "ENOTSUP";
#if defined(EOPNOTSUPP) && EOPNOTSUPP != ENOTSUP
    case EOPNOTSUPP: return "EOPNOTSUPP";
#endif
    case EMLINK: return "EMLINK";
    case ENXIO: return "ENXIO";
    case ESPIPE: return "ESPIPE";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

enum { D_CWD, D_OTHERCWD, D_MINUS1, D_CLOSED, D_DIR, D_FILE, D_PIPE, D_SOCKET, D_ORPHAN, D_MOVED, D_LOCKED, D_DEVNULL, D_EVENTQ, D_COUNT };
static const char *dname[D_COUNT] = {"cwd", "othercwd", "minus1", "closed", "dir", "file", "pipe", "socket", "orphan", "moved", "locked", "devnull", "eventq"};
enum { P_F, P_NX, P_EMPTY, P_NULL, P_PROTNONE, P_OVERLONG, P_ABS, P_COUNT };
static const char *pname[P_COUNT] = {"f", "nx", "empty", "NULL", "PROT_NONE", "overlong", "abs"};

#ifdef __linux__
static const int other_at_fdcwd = -2;
#else
static const int other_at_fdcwd = -100;
#endif

static const char *base;
static char *protnone;
static char *overlong;
static int cellno;
static char celldir[PATH_MAX];
static char absf[PATH_MAX + 8];

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static void mkfile(const char *p) {
    int fd = open(p, O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die(p);
    close(fd);
}

// A fresh fixture in a directory of its own, entered as the cwd. Answers the
// descriptor `kind` names.
static int setup(int kind) {
    snprintf(celldir, sizeof celldir, "%s/c%05d", base, cellno);
    if (mkdir(celldir, 0755) < 0) die("mkdir cell");
    if (chdir(celldir) < 0) die("chdir cell");
    snprintf(absf, sizeof absf, "%s/f", celldir);
    mkfile("f");
    mkfile("f2");
    if (mkdir("d", 0755) < 0) die("mkdir d");
    mkfile("d/f");
    mkfile("d/f2");
    int fd, p[2];
    switch (kind) {
    case D_CWD: return AT_FDCWD;
    case D_OTHERCWD: return other_at_fdcwd;
    case D_MINUS1: return -1;
    case D_CLOSED: close(999); return 999;
    case D_DIR:
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d");
        return fd;
    case D_FILE:
        fd = open("f2", O_RDONLY);
        if (fd < 0) die("open f2");
        return fd;
    case D_PIPE:
        if (pipe(p) < 0) die("pipe");
        return p[0];
    case D_SOCKET:
        if (socketpair(AF_UNIX, SOCK_STREAM, 0, p) < 0) die("socketpair");
        return p[0];
    case D_ORPHAN:
        if (mkdir("gone", 0755) < 0) die("mkdir gone");
        fd = open("gone", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open gone");
        if (rmdir("gone") < 0) die("rmdir gone");
        return fd;
    case D_MOVED:
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d");
        if (rename("d", "d-moved") < 0) die("rename d");
        return fd;
    case D_LOCKED:
        fd = open("d", O_RDONLY | O_DIRECTORY);
        if (fd < 0) die("open d");
        if (chmod("d", 0) < 0) die("chmod d");
        return fd;
    case D_DEVNULL:
        fd = open("/dev/null", O_RDONLY);
        if (fd < 0) die("open /dev/null");
        return fd;
    case D_EVENTQ:
#ifdef __linux__
        fd = epoll_create1(0);
#else
        fd = kqueue();
#endif
        if (fd < 0) die("event queue");
        return fd;
    }
    abort();
}

static const char *path_of(int kind) {
    switch (kind) {
    case P_F: return "f";
    case P_NX: return "nx";
    case P_EMPTY: return "";
    case P_NULL: return NULL;
    case P_PROTNONE: return protnone;
    case P_OVERLONG: return overlong;
    case P_ABS: return absf;
    }
    abort();
}

static int rc(int r) { return r < 0 ? errno : 0; }

typedef int (*call_fn)(int dirfd, const char *path);

static int c_openat_rd(int d, const char *p) { return rc(openat(d, p, O_RDONLY)); }
static int c_openat_creat(int d, const char *p) { return rc(openat(d, p, O_RDONLY | O_CREAT, 0644)); }
static int c_fstatat(int d, const char *p) { struct stat s; return rc(fstatat(d, p, &s, 0)); }
static int c_fstatat_nofollow(int d, const char *p) { struct stat s; return rc(fstatat(d, p, &s, AT_SYMLINK_NOFOLLOW)); }
static int c_mkdirat(int d, const char *p) { return rc(mkdirat(d, p, 0755)); }
static int c_unlinkat(int d, const char *p) { return rc(unlinkat(d, p, 0)); }
static int c_unlinkat_rmdir(int d, const char *p) { return rc(unlinkat(d, p, AT_REMOVEDIR)); }
static int c_readlinkat(int d, const char *p) { char b[64]; return rc((int)readlinkat(d, p, b, sizeof b)); }
static int c_fchmodat(int d, const char *p) { return rc(fchmodat(d, p, 0644, 0)); }
static int c_fchownat(int d, const char *p) { return rc(fchownat(d, p, (uid_t)-1, (gid_t)-1, 0)); }
static int c_faccessat(int d, const char *p) { return rc(faccessat(d, p, F_OK, 0)); }
static int c_symlinkat(int d, const char *p) { return rc(symlinkat("target", d, p)); }
static int c_linkat_old(int d, const char *p) { return rc(linkat(d, p, AT_FDCWD, "newlink", 0)); }
static int c_linkat_new(int d, const char *p) { return rc(linkat(AT_FDCWD, "f2", d, p, 0)); }
static int c_renameat_old(int d, const char *p) { return rc(renameat(d, p, AT_FDCWD, "renamed")); }
static int c_renameat_new(int d, const char *p) { return rc(renameat(AT_FDCWD, "f2", d, p)); }
static int c_mkfifoat(int d, const char *p) { return rc(mkfifoat(d, p, 0644)); }
static int c_mknodat_reg(int d, const char *p) { return rc(mknodat(d, p, S_IFREG | 0644, 0)); }
static int c_utimensat(int d, const char *p) { return rc(utimensat(d, p, NULL, 0)); }

static struct {
    const char *name;
    call_fn fn;
} calls[] = {
    {"openat(O_RDONLY)", c_openat_rd},
    {"openat(O_CREAT)", c_openat_creat},
    {"fstatat", c_fstatat},
    {"fstatat(NOFOLLOW)", c_fstatat_nofollow},
    {"mkdirat", c_mkdirat},
    {"unlinkat", c_unlinkat},
    {"unlinkat(REMOVEDIR)", c_unlinkat_rmdir},
    {"readlinkat", c_readlinkat},
    {"fchmodat", c_fchmodat},
    {"fchownat", c_fchownat},
    {"faccessat", c_faccessat},
    {"symlinkat", c_symlinkat},
    {"linkat[old]", c_linkat_old},
    {"linkat[new]", c_linkat_new},
    {"renameat[old]", c_renameat_old},
    {"renameat[new]", c_renameat_new},
    {"mkfifoat", c_mkfifoat},
    {"mknodat(S_IFREG)", c_mknodat_reg},
    {"utimensat", c_utimensat},
};
#define NCALLS (sizeof calls / sizeof calls[0])

// Runs `body` in a forked child and prints what it printed, or how it died.
static void in_child(void (*body)(void *), void *arg) {
    fflush(stdout);
    cellno++;
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        exit(2);
    }
    if (pid == 0) {
        alarm(5);
        body(arg);
        fflush(stdout);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) {
        perror("waitpid");
        exit(2);
    }
    if (WIFSIGNALED(status)) printf("SIG%d", WTERMSIG(status));
    else if (WEXITSTATUS(status) != 0) printf("EXIT%d", WEXITSTATUS(status));
}

struct at_cell { int call, dir, path; };
static void at_body(void *v) {
    struct at_cell *c = v;
    int fd = setup(c->dir);
    printf("%s", en(calls[c->call].fn(fd, path_of(c->path))));
}

// ORDER2: two-path calls with a bad argument on each side.
enum { S_GOOD, S_BADFD, S_ABSENT, S_NULL, S_NODIR, S_COUNT };
static const char *sname[S_COUNT] = {"good", "badfd", "absent", "NULL", "nodir"};
struct order2_cell { int link; int old, new_; };
static void side(int kind, int is_old, int *fd, const char **p) {
    *fd = AT_FDCWD;
    switch (kind) {
    case S_GOOD: *p = is_old ? "f" : "new"; break;
    case S_BADFD: *fd = -1; *p = is_old ? "f" : "new"; break;
    case S_ABSENT: *p = is_old ? "nx" : "f2"; break; // a new side that exists
    case S_NULL: *p = NULL; break;
    case S_NODIR: *p = is_old ? "nxdir/f" : "nxdir/new"; break;
    }
}
static void order2_body(void *v) {
    struct order2_cell *c = v;
    setup(D_CWD);
    int ofd, nfd;
    const char *op = NULL, *np = NULL;
    side(c->old, 1, &ofd, &op);
    side(c->new_, 0, &nfd, &np);
    int r = c->link ? rc(linkat(ofd, op, nfd, np, 0)) : rc(renameat(ofd, op, nfd, np));
    printf("%s", en(r));
}

// FLAGS: one bit at a time, with AT_FDCWD and "f".
struct flag_cell { int which; unsigned flags; int dir; int path; };
static int flagged(int which, int d, const char *p, unsigned flags) {
    struct stat s;
    switch (which) {
    case 0: return rc(fstatat(d, p, &s, (int)flags));
    case 1: return rc(unlinkat(d, p, (int)flags));
    case 2: return rc(fchmodat(d, p, 0644, (int)flags));
    case 3: return rc(fchownat(d, p, (uid_t)-1, (gid_t)-1, (int)flags));
    case 4: return rc(linkat(d, p, AT_FDCWD, "newlink", (int)flags));
    case 5: return rc(utimensat(d, p, NULL, (int)flags));
    case 6: return rc(faccessat(d, p, F_OK, (int)flags));
    }
    abort();
}
static const char *flagname[] = {"fstatat", "unlinkat", "fchmodat", "fchownat", "linkat", "utimensat", "faccessat"};
#define NFLAGGED 7
static void flag_body(void *v) {
    struct flag_cell *c = v;
    int fd = setup(c->dir);
    printf("%s", en(flagged(c->which, fd, path_of(c->path), c->flags)));
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <empty scratch directory>\n", argv[0]);
        return 2;
    }
    base = argv[1];
    alarm(600);
    umask(022);

    struct utsname u;
    uname(&u);
    printf("UNAME\t%s %s %s\teuid=%d egid=%d\n", u.sysname, u.release, u.machine, (int)geteuid(), (int)getegid());
    printf("CONST\tAT_FDCWD=%d\tAT_SYMLINK_NOFOLLOW=0x%x\tAT_SYMLINK_FOLLOW=0x%x\tAT_REMOVEDIR=0x%x\tAT_EACCESS=0x%x", AT_FDCWD, AT_SYMLINK_NOFOLLOW, AT_SYMLINK_FOLLOW, AT_REMOVEDIR, AT_EACCESS);
#ifdef AT_EMPTY_PATH
    printf("\tAT_EMPTY_PATH=0x%x", AT_EMPTY_PATH);
#endif
#ifdef AT_NO_AUTOMOUNT
    printf("\tAT_NO_AUTOMOUNT=0x%x", AT_NO_AUTOMOUNT);
#endif
#ifdef AT_STATX_SYNC_TYPE
    printf("\tAT_STATX_SYNC_TYPE=0x%x", AT_STATX_SYNC_TYPE);
#endif
#ifdef AT_REALDEV
    printf("\tAT_REALDEV=0x%x", AT_REALDEV);
#endif
#ifdef AT_FDONLY
    printf("\tAT_FDONLY=0x%x", AT_FDONLY);
#endif
#ifdef AT_SYMLINK_NOFOLLOW_ANY
    printf("\tAT_SYMLINK_NOFOLLOW_ANY=0x%x", AT_SYMLINK_NOFOLLOW_ANY);
#endif
#ifdef AT_RESOLVE_BENEATH
    printf("\tAT_RESOLVE_BENEATH=0x%x", AT_RESOLVE_BENEATH);
#endif
#ifdef AT_NODELETEBUSY
    printf("\tAT_NODELETEBUSY=0x%x", AT_NODELETEBUSY);
#endif
#ifdef AT_UNIQUE
    printf("\tAT_UNIQUE=0x%x", AT_UNIQUE);
#endif
    printf("\tPATH_MAX=%d\n", PATH_MAX);

    long page = sysconf(_SC_PAGESIZE);
    protnone = mmap(NULL, (size_t)page, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (protnone == MAP_FAILED) {
        perror("mmap");
        return 2;
    }
    overlong = malloc(PATH_MAX + 1);
    for (int i = 0; i < PATH_MAX; i++) overlong[i] = (i % 2 == 0) ? 'a' : '/';
    overlong[PATH_MAX] = '\0';

    for (size_t c = 0; c < NCALLS; c++) {
        for (int d = 0; d < D_COUNT; d++) {
            printf("AT\t%s\t%s", calls[c].name, dname[d]);
            for (int p = 0; p < P_COUNT; p++) {
                struct at_cell cell = {(int)c, d, p};
                printf("\t%s=", pname[p]);
                in_child(at_body, &cell);
            }
            printf("\n");
        }
    }

    for (int link = 1; link >= 0; link--) {
        for (int o = 0; o < S_COUNT; o++) {
            printf("ORDER2\t%s\told=%s", link ? "linkat" : "renameat", sname[o]);
            for (int n = 0; n < S_COUNT; n++) {
                struct order2_cell cell = {link, o, n};
                printf("\tnew:%s=", sname[n]);
                in_child(order2_body, &cell);
            }
            printf("\n");
        }
    }

    for (int w = 0; w < NFLAGGED; w++) {
        printf("FLAGS\t%s\t0=", flagname[w]);
        struct flag_cell zero = {w, 0, D_CWD, P_F};
        in_child(flag_body, &zero);
        printf("\trejected:");
        for (int bit = 0; bit < 32; bit++) {
            unsigned f = 1u << bit;
            // Capture the child's answer through a pipe, so only the rejected
            // bits are printed.
            int pp[2];
            if (pipe(pp) < 0) {
                perror("pipe");
                return 2;
            }
            fflush(stdout);
            cellno++;
            pid_t pid = fork();
            if (pid == 0) {
                alarm(5);
                close(pp[0]);
                int fd = setup(D_CWD);
                int r = flagged(w, fd, "f", f);
                write(pp[1], &r, sizeof r);
                _exit(0);
            }
            close(pp[1]);
            int r = -1;
            ssize_t got = read(pp[0], &r, sizeof r);
            close(pp[0]);
            int status;
            waitpid(pid, &status, 0);
            if (got != sizeof r) printf(" 0x%x=died", f);
            else if (r == EINVAL) printf(" 0x%x", f);
            else if (r != 0) printf(" 0x%x=%s", f, en(r));
        }
        printf("\n");
    }

    // A flag bit both flavours reject on every call: bit 30.
    for (int w = 0; w < NFLAGGED; w++) {
        printf("FLAGORDER\t%s\tflags=0x40000000", flagname[w]);
        struct flag_cell cells[] = {
            {w, 0x40000000u, D_CWD, P_NULL},
            {w, 0x40000000u, D_MINUS1, P_F},
            {w, 0x40000000u, D_CWD, P_EMPTY},
            {w, 0x40000000u, D_CWD, P_NX},
        };
        const char *labels[] = {"NULL", "minus1+f", "empty", "nx"};
        for (int i = 0; i < 4; i++) {
            printf("\t%s=", labels[i]);
            in_child(flag_body, &cells[i]);
        }
        printf("\n");
    }
    return 0;
}
