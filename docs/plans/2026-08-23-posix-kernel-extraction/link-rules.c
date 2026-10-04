// Measures the rest of link(2) and linkat(2) that link-symlink.c did not: the
// order of its checks against one another, what fs.protected_hardlinks
// refuses beyond a plain file, a sticky directory on either side, the link
// count's ceiling, and Linux's AT_EMPTY_PATH.
//
// Sections:
//   ORDER     link(source, dest) for every source kind against every
//             destination kind, as each caller, under protected_hardlinks 0
//             and 1 (Linux), so that each pair of failures shows which wins.
//   DESTMORE  the destination's own refusals against an unwritable directory:
//             a free name with a trailing separator, a taken name, and (for
//             Darwin's sake) a name APFS will not bind.
//   STICKY    linking into, and out of, a sticky world-writable directory
//             another user owns.
//   EMLINK    how many names one file can be given before link fails, and
//             what it then answers; and st_nlink at the end.
//   EMPTY     linkat(fd, "", AT_FDCWD, name, AT_EMPTY_PATH) for each kind of
//             descriptor (Linux).
//   DESTORDER a source that will be refused late (a directory, or another
//             user's file under protected_hardlinks) against a destination
//             that cannot even be copied in or looked up: NULL, "", PATH_MAX
//             bytes, a dirfd naming nothing.
//
// Each cell runs in a forked child, in a fresh directory of its own, under
// alarm(5); EMLINK has its own budget.
//
// Linux, as root, which builds other users' files and then drops each call to
// the caller in a child:
//   container run --rm --cap-add ALL --shm-size 2G -v "$PWD":/probe gcc:14 sh -c 'mount -o remount,rw /proc/sys && gcc -Wall -O1 -o /tmp/p /probe/link-rules.c && /tmp/p "$(mktemp -d)" /dev/shm/emlink'
// Darwin, as an ordinary user (uid 501): "other's file" is a hard link to a
// root-owned file on the same volume, given as the third argument:
//   nix develop -c clang -Wall -o link-rules link-rules.c && ./link-rules "$(mktemp -d /private/tmp/lr.XXXXXX)" /private/tmp/lr-emlink /private/tmp/.AppleMiniSetupDidRun
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
    case EISDIR: return "EISDIR";
    case EEXIST: return "EEXIST";
    case ELOOP: return "ELOOP";
    case EINVAL: return "EINVAL";
    case EBADF: return "EBADF";
    case ENAMETOOLONG: return "ENAMETOOLONG";
    case EFAULT: return "EFAULT";
    case EXDEV: return "EXDEV";
    case EMLINK: return "EMLINK";
    case ENOSPC: return "ENOSPC";
    case EROFS: return "EROFS";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static const char *base;
static const char *rootfile; // Darwin: a root-owned file on this volume
static int cellno;
static int isroot;

static void die(const char *what) {
    perror(what);
    _exit(2);
}

static int rc(int r) { return r < 0 ? errno : 0; }

static void mkfile(const char *p, mode_t mode) {
    int fd = open(p, O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (fd < 0) die(p);
    close(fd);
    if (chmod(p, mode) < 0) die(p);
}

#ifdef __linux__
static void set_hardlinks(int value) {
    FILE *f = fopen("/proc/sys/fs/protected_hardlinks", "w");
    if (!f) die("protected_hardlinks");
    fprintf(f, "%d\n", value);
    if (fclose(f) != 0) die("protected_hardlinks");
}
#endif

// The cell, entered as the cwd, owned by the caller (1000 on Linux, the
// running user on Darwin):
//   f, g (0644)  d/  u/ (0555)  s/ (0600, holding sf)
//   o600 o644 o666 o4666 o2676 o2666 (another user's files, Linux; Darwin has
//   only o644, a link to `rootfile`) olnk (another user's link to f, Linux)
//   odir/ (another user's directory, Linux) st/ (01777, another user's,
//   holding so (another user's 0666 file), Linux)
static void fixture(uid_t caller) {
    char c[PATH_MAX];
    cellno++;
    snprintf(c, sizeof c, "%s/c%05d", base, cellno);
    if (mkdir(c, 0777) < 0 || chmod(c, 0777) < 0) die("mkdir cell");
    if (chdir(c) < 0) die("chdir cell");
    mkfile("f", 0644);
    mkfile("g", 0644);
    if (mkdir("d", 0755) < 0 || mkdir("u", 0755) < 0 || mkdir("s", 0755) < 0) die("mkdir");
    mkfile("s/sf", 0644);
    mkfile("u/ug", 0644);
    if (isroot) {
        const struct {
            const char *name;
            mode_t mode;
        } others[] = {{"o600", 0600}, {"o644", 0644}, {"o666", 0666}, {"o4666", 04666}, {"o2676", 02676}, {"o2666", 02666}};
        for (size_t i = 0; i < sizeof others / sizeof others[0]; i++) mkfile(others[i].name, others[i].mode);
        if (symlink("f", "olnk") < 0 || mkdir("odir", 0777) < 0) die("olnk/odir");
        if (mkdir("st", 0777) < 0 || chmod("st", 01777) < 0) die("st");
        mkfile("st/so", 0666);
        const char *mine[] = {".", "f", "g", "d", "u", "s", "s/sf"};
        for (int i = 0; i < 7; i++)
            if (chown(mine[i], caller, caller) < 0) die("chown");
    } else if (rootfile) {
        if (link(rootfile, "o644") < 0) die("link rootfile");
    }
    if (chmod("u", 0555) < 0 || chmod("s", 0600) < 0) die("chmod u s");
}

static void drop(uid_t caller) {
    if (isroot && caller != 0)
        if (setgroups(0, NULL) < 0 || setgid(caller) < 0 || setuid(caller) < 0) die("drop");
}

typedef void (*body_fn)(const void *);
static void in_child(body_fn body, const void *arg) {
    fflush(stdout);
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
    waitpid(pid, &status, 0);
    if (WIFSIGNALED(status)) printf("SIG%d", WTERMSIG(status));
    else if (WEXITSTATUS(status) != 0) printf("EXIT%d", WEXITSTATUS(status));
    cellno++;
}

// ------------------------------------------------------------------ ORDER
struct order { const char *source; const char *dest; int follow; uid_t caller; };
static void order_body(const void *v) {
    const struct order *a = v;
    fixture(a->caller);
    drop(a->caller);
    int r = rc(linkat(AT_FDCWD, a->source, AT_FDCWD, a->dest, a->follow ? AT_SYMLINK_FOLLOW : 0));
    printf("%s", en(r));
    if (r == 0) unlink(a->dest);
}

// ------------------------------------------------------------------ DESTORDER
static char overlong[PATH_MAX + 2];
struct destorder { const char *source; const char *dest; int destfd; uid_t caller; };
static void destorder_body(const void *v) {
    const struct destorder *a = v;
    fixture(a->caller);
    drop(a->caller);
    printf("%s", en(rc(linkat(AT_FDCWD, a->source, a->destfd, a->dest, 0))));
}

// ------------------------------------------------------------------ DESTMORE
struct destmore { const char *dest; uid_t caller; };
static void destmore_body(const void *v) {
    const struct destmore *a = v;
    fixture(a->caller);
    drop(a->caller);
    printf("%s", en(rc(link("f", a->dest))));
}

// ------------------------------------------------------------------ STICKY
struct sticky { int row; uid_t caller; };
static void sticky_body(const void *v) {
    const struct sticky *a = v;
    fixture(a->caller);
    drop(a->caller);
    char dest[PATH_MAX];
    int r;
    switch (a->row) {
    case 0:
        // An own file into another user's sticky world-writable directory.
        if (isroot) r = rc(link("f", "st/n"));
        else {
            snprintf(dest, sizeof dest, "/private/tmp/lr-sticky-%d", (int)getpid());
            r = rc(link("f", dest));
            if (r == 0) unlink(dest);
        }
        break;
    case 1:
        // Another user's 0666 file, out of that sticky directory.
        r = isroot ? rc(link("st/so", "n")) : rc(link(rootfile, "n"));
        break;
    default: r = -1;
    }
    printf("%s", en(r));
}

// ------------------------------------------------------------------ EMLINK
static void emlink(const char *dir) {
    if (mkdir(dir, 0755) < 0 && errno != EEXIST) die("mkdir emlink");
    char p[PATH_MAX];
    snprintf(p, sizeof p, "%s/f", dir);
    unlink(p);
    mkfile(p, 0644);
    int made = 0;
    int err = 0;
    alarm(600);
    for (int i = 0; i < 70000; i++) {
        char q[PATH_MAX];
        snprintf(q, sizeof q, "%s/l%d", dir, i);
        if (link(p, q) < 0) {
            err = errno;
            break;
        }
        made++;
    }
    struct stat s;
    stat(p, &s);
    printf("EMLINK\t%s\tlinks=%d\tthen=%s\tst_nlink=%ld\n", dir, made, err ? en(err) : "none", (long)s.st_nlink);
    for (int i = 0; i < made; i++) {
        char q[PATH_MAX];
        snprintf(q, sizeof q, "%s/l%d", dir, i);
        unlink(q);
    }
    unlink(p);
    rmdir(dir);
}

// ------------------------------------------------------------------ EMPTY
#ifdef AT_EMPTY_PATH
struct empty { int row; uid_t caller; };
static void empty_body(const void *v) {
    const struct empty *a = v;
    fixture(a->caller);
    drop(a->caller);
    int fd = -1;
    int r;
    switch (a->row) {
    case 0: fd = open("f", O_RDONLY); r = rc(linkat(fd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 1: fd = open("d", O_RDONLY | O_DIRECTORY); r = rc(linkat(fd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 2:
        fd = open("g", O_RDONLY);
        unlink("g");
        r = rc(linkat(fd, "", AT_FDCWD, "n", AT_EMPTY_PATH));
        break;
    case 3: fd = open("o644", O_RDONLY); r = rc(linkat(fd, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 4: fd = open("f", O_RDONLY); r = rc(linkat(fd, "", AT_FDCWD, "n", 0)); break;
    case 5: fd = open("d", O_RDONLY | O_DIRECTORY); r = rc(linkat(fd, "../f", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 6: r = rc(linkat(AT_FDCWD, "", AT_FDCWD, "n", AT_EMPTY_PATH)); break;
    case 7: fd = open("o644", O_RDONLY); r = rc(linkat(fd, "", AT_FDCWD, "n", AT_EMPTY_PATH | AT_SYMLINK_FOLLOW)); break;
    default: r = -1;
    }
    printf("%s", en(r));
}
static const char *emptylabel[] = {
    "own file", "own directory", "own file, unlinked", "another user's 0644 file",
    "own file, flags 0", "directory with ../f", "AT_FDCWD", "another user's 0644 file, with AT_SYMLINK_FOLLOW",
};
#endif

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: %s <scratch> <emlink dir> [darwin: root-owned file]\n", argv[0]);
        return 2;
    }
    base = argv[1];
    rootfile = argc > 3 ? argv[3] : NULL;
    isroot = geteuid() == 0;
    memset(overlong, 'a', PATH_MAX);
    overlong[PATH_MAX] = 0;
    alarm(1200);
    umask(022);
    struct utsname u;
    uname(&u);
    printf("UNAME\t%s %s %s\teuid=%d\n", u.sysname, u.release, u.machine, (int)geteuid());

    const char *sources[] = {"f", "d", "nx", "s/sf", "o600", "o644", "o666", "o4666", "o2676", "o2666", "olnk", "odir"};
    const char *dests[] = {"n", "g", "u/n", "n/", "nxdir/n", "/dev/n"};
#ifdef __linux__
    uid_t callers[] = {1000, 0};
    int ncallers = 2;
    int settings[] = {0, 1};
    int nsettings = 2;
#else
    uid_t callers[] = {getuid()};
    int ncallers = 1;
    int settings[] = {-1};
    int nsettings = 1;
#endif
    for (int s = 0; s < nsettings; s++) {
#ifdef __linux__
        set_hardlinks(settings[s]);
#endif
        for (int c = 0; c < ncallers; c++)
            for (size_t i = 0; i < sizeof sources / sizeof sources[0]; i++) {
                if (!isroot && strcmp(sources[i], "o644") != 0 && sources[i][0] == 'o') continue;
                printf("ORDER\tprotected_hardlinks=%d\tcaller=%d\tsource=%s", settings[s], (int)callers[c], sources[i]);
                for (size_t j = 0; j < sizeof dests / sizeof dests[0]; j++) {
                    struct order a = {sources[i], dests[j], 0, callers[c]};
                    printf("\t%s=", dests[j]);
                    in_child(order_body, &a);
                }
                printf("\n");
            }
        for (int c = 0; c < ncallers; c++) {
            const char *dests[] = {"u/n/", "u/ug", "\xff\xfe", "u/\xff\xfe"};
            const char *labels[] = {"unwritable: free name/", "unwritable: taken name", "writable: unbindable name", "unwritable: unbindable name"};
            for (int k = 0; k < 4; k++) {
                struct destmore a = {dests[k], callers[c]};
                printf("DESTMORE\tprotected_hardlinks=%d\tcaller=%d\t%s\t", settings[s], (int)callers[c], labels[k]);
                in_child(destmore_body, &a);
                printf("\n");
            }
        }
        for (int c = 0; c < ncallers; c++)
            for (int row = 0; row < 2; row++) {
                struct sticky a = {row, callers[c]};
                printf("STICKY\tprotected_hardlinks=%d\tcaller=%d\t%s\t", settings[s], (int)callers[c],
                       row == 0 ? "own file into another's sticky directory" : "another's file out of it (Linux 0666; Darwin the root-owned 0644 file)");
                in_child(sticky_body, &a);
                printf("\n");
            }
        for (int c = 0; c < ncallers; c++)
            for (size_t i = 0; i < sizeof sources / sizeof sources[0]; i++) {
                if (!isroot && strcmp(sources[i], "o644") != 0 && sources[i][0] == 'o') continue;
                printf("DESTORDER\tprotected_hardlinks=%d\tcaller=%d\tsource=%s", settings[s], (int)callers[c], sources[i]);
                struct destorder rows[] = {
                    {sources[i], NULL, AT_FDCWD, callers[c]},
                    {sources[i], "", AT_FDCWD, callers[c]},
                    {sources[i], overlong, AT_FDCWD, callers[c]},
                    {sources[i], "n", -1, callers[c]},
                };
                const char *labels[] = {"NULL", "empty", "overlong", "dirfd=-1"};
                for (int k = 0; k < 4; k++) {
                    printf("\t%s=", labels[k]);
                    in_child(destorder_body, &rows[k]);
                }
                printf("\n");
            }
#ifdef AT_EMPTY_PATH
        for (int c = 0; c < ncallers; c++)
            for (int row = 0; row < 8; row++) {
                struct empty a = {row, callers[c]};
                printf("EMPTY\tprotected_hardlinks=%d\tcaller=%d\t%s\t", settings[s], (int)callers[c], emptylabel[row]);
                in_child(empty_body, &a);
                printf("\n");
            }
#endif
    }
#ifdef __linux__
    set_hardlinks(0);
    emlink("/tmp/emlink");
#endif
    emlink(argv[2]);
    return 0;
}
