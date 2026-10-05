// Measures where openat(2)'s EMFILE falls against the answers its dirfd and
// pathname give, which fcntl-dup.c's LIMIT rows measured only for open(2).
//
// RLIMIT_NOFILE is set to each flavour's default (the bound
// WoofWare.PosixKernel assumes), and every descriptor below it is taken.
// Then each row calls openat once, with:
//   - a dirfd naming a directory, a file, a pipe, -1 or a closed number;
//   - a pathname that exists, one that does not, the empty one, NULL or a
//     rooted one;
//   - O_RDONLY or O_RDONLY | O_CREAT.
// The ONELEFT rows repeat a few cells with one descriptor free, as a check
// that the table, not the call, made the difference.
//
// Linux, as root:
//   container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Werror -O1 -o /tmp/p /probe/openat-limit.c && cd "$(mktemp -d)" && /tmp/p > /probe/openat-limit.linux-6.18.5-aarch64-root.txt'
// Darwin, as an ordinary user, in a fresh directory:
//   nix develop -c clang -Wall -Werror -o openat-limit openat-limit.c && (cd "$(mktemp -d /private/tmp/ol.XXXXXX)" && "$OLDPWD"/openat-limit) > openat-limit.darwin-27.0-uid501.txt
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>

#ifdef __APPLE__
#define BOUND 256
#else
#define BOUND 1024
#endif

static const char *en(int e) {
    static char buf[32];
    switch (e) {
    case EMFILE: return "EMFILE";
    case ENOENT: return "ENOENT";
    case ENOTDIR: return "ENOTDIR";
    case ENOTSUP: return "ENOTSUP";
    case EBADF: return "EBADF";
    case EFAULT: return "EFAULT";
    case EISDIR: return "EISDIR";
    }
    snprintf(buf, sizeof buf, "errno%d", e);
    return buf;
}

static void die(const char *what) {
    perror(what);
    exit(2);
}

static const char *volatile no_path = NULL;
static char rooted[PATH_MAX + 8];
static int dirfd_, filefd, pipefd;

static void row(const char *table, const char *dlabel, int d, const char *plabel, const char *p, const char *flabel, int flags) {
    int r = openat(d, p, flags, 0644);
    printf("%s\t%s\t%s\t%s\t", table, dlabel, plabel, flabel);
    if (r >= 0) {
        printf("ok\n");
        close(r);
        unlink("nx");
        unlink("d/nx");
    } else {
        printf("%s\n", en(errno));
    }
}

static void rows(const char *table) {
    struct { const char *label; int fd; } ds[] = {
        {"dir", dirfd_}, {"file", filefd}, {"pipe", pipefd}, {"minus1", -1}, {"closed", BOUND + 5},
    };
    struct { const char *label; const char *p; } ps[] = {
        {"f", "f"}, {"nx", "nx"}, {"empty", ""}, {"NULL", no_path}, {"rooted", rooted},
    };
    for (size_t i = 0; i < sizeof ds / sizeof ds[0]; i++)
        for (size_t j = 0; j < sizeof ps / sizeof ps[0]; j++) {
            row(table, ds[i].label, ds[i].fd, ps[j].label, ps[j].p, "O_RDONLY", O_RDONLY);
            row(table, ds[i].label, ds[i].fd, ps[j].label, ps[j].p, "O_CREAT", O_RDONLY | O_CREAT);
        }
}

int main(void) {
    struct utsname u;
    if (uname(&u) < 0) die("uname");
    printf("UNAME\t%s %s %s\teuid=%d\tbound=%d\n", u.sysname, u.release, u.machine, (int)geteuid(), BOUND);
    struct rlimit rl;
    if (getrlimit(RLIMIT_NOFILE, &rl) < 0) die("getrlimit");
    rl.rlim_cur = BOUND;
    if (setrlimit(RLIMIT_NOFILE, &rl) < 0) die("setrlimit");
    for (int i = 3; i < BOUND + 8; i++) close(i);
    // The cell: the cwd holds f and d/, and d holds f.
    int fd = open("f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("f");
    close(fd);
    if (mkdir("d", 0755) < 0) die("d");
    fd = open("d/f", O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) die("d/f");
    close(fd);
    char cwd[PATH_MAX];
    if (!getcwd(cwd, sizeof cwd)) die("getcwd");
    snprintf(rooted, sizeof rooted, "%s/f", cwd);
    // Descriptors: d, f, and a pipe's read end, then every other one taken.
    dirfd_ = open("d", O_RDONLY | O_DIRECTORY);
    filefd = open("f", O_RDONLY);
    int p[2];
    if (dirfd_ < 0 || filefd < 0 || pipe(p) < 0) die("descriptors");
    pipefd = p[0];
    while (fcntl(filefd, F_DUPFD, 0) >= 0) {}
    if (errno != EMFILE) die("fill");
    rows("FULL");
    close(BOUND - 1);
    rows("ONELEFT");
    return 0;
}
