// Measures who owns an inode that open(O_CREAT) or mkdir(2) creates: its uid,
// and whether its gid is the creator's effective group or the parent
// directory's.
//
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -o /tmp/p /probe/new-inode-owner.c && /tmp/p /tmp/owner-probe'
//         Run it as root: each row forks a child that takes the credentials
//         under test, inside parents that root has chowned to groups the child
//         is and is not a member of.
// Darwin: nix develop -c clang -Wall -o new-inode-owner new-inode-owner.c && ./new-inode-owner "$(mktemp -d /private/tmp/owner.XXXXXX)"
//         As an ordinary user. A fresh directory under /private/tmp is group
//         wheel, of which an ordinary user is not a member, which is the
//         non-member parent; the member parents are chgrp'd to the caller's
//         own groups.
//
// Measured on Linux 6.18.5 (aarch64, root in the container) and Darwin 27.0
// (arm64, uid 501, groups 20,12,61,100,701) on 2026-09-26. The rows are
// transcribed in WoofWare.PosixKernel.Test/TestInodeOwner.fs.
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
#include <sys/wait.h>
#include <unistd.h>

static char base[PATH_MAX];

static void create_in(const char *dir, const char *label) {
    umask(0);
    char f[PATH_MAX], d[PATH_MAX];
    snprintf(f, sizeof f, "%s/file", dir);
    snprintf(d, sizeof d, "%s/dir", dir);
    int fd = open(f, O_CREAT | O_EXCL | O_WRONLY, 0644);
    int fe = fd < 0 ? errno : 0;
    if (fd >= 0) close(fd);
    int de = mkdir(d, 0755) == 0 ? 0 : errno;
    struct stat ps, fs, ds;
    stat(dir, &ps);
    printf("%-44s euid=%-5u egid=%-5u parent gid=%-5u mode=%04o |", label, (unsigned)geteuid(), (unsigned)getegid(),
           (unsigned)ps.st_gid, (unsigned)(ps.st_mode & 07777));
    if (fe == 0 && stat(f, &fs) == 0) printf(" open: uid=%u gid=%u", (unsigned)fs.st_uid, (unsigned)fs.st_gid);
    else printf(" open: %s", strerror(fe));
    if (de == 0 && stat(d, &ds) == 0) printf(" | mkdir: uid=%u gid=%u", (unsigned)ds.st_uid, (unsigned)ds.st_gid);
    else printf(" | mkdir: %s", strerror(de));
    printf("\n");
    fflush(stdout);
}

static void parent(const char *name, gid_t gid, int mode, char *out) {
    snprintf(out, PATH_MAX, "%s/%s", base, name);
    if (mkdir(out, 0700) != 0) { perror(out); exit(2); }
    if (gid != (gid_t)-1 && chown(out, (uid_t)-1, gid) != 0) { perror("chown"); exit(2); }
    if (chmod(out, mode) != 0) { perror("chmod"); exit(2); }
}

#ifdef __linux__
static void as(uid_t ruid, uid_t euid, gid_t egid, int ngroups, const gid_t *groups, const char *dir, const char *label) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        if (setgroups(ngroups, groups) != 0 || setresgid(egid, egid, egid) != 0 || setresuid(ruid, euid, euid) != 0) {
            perror("credentials");
            _exit(2);
        }
        create_in(dir, label);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
}
#endif

int main(int argc, char **argv) {
    alarm(60);
    if (argc != 2) { fprintf(stderr, "usage: %s <empty directory>\n", argv[0]); return 2; }
    snprintf(base, sizeof base, "%s", argv[1]);
    mkdir(base, 0755);
    chmod(base, 0755);
    char p[PATH_MAX];
#ifdef __linux__
    // The creator: effective uid 1000 (real 1000) or effective 1001 (real 1000),
    // effective group 1000, supplementary groups {1000, 2000}. Group 3000 is one
    // it is not in.
    const gid_t groups[] = {1000, 2000};
    const struct { const char *name; gid_t gid; int mode; } parents[] = {
        {"plain-member", 2000, 0777}, {"sgid-member", 2000, 02777}, {"sgid-nonmember", 3000, 02777}, {"plain-nonmember", 3000, 0777},
    };
    for (size_t i = 0; i < 4; i++) {
        char name[64];
        snprintf(name, sizeof name, "%s-e1000", parents[i].name);
        parent(name, parents[i].gid, parents[i].mode, p);
        as(1000, 1000, 1000, 2, groups, p, name);
        snprintf(name, sizeof name, "%s-e1001", parents[i].name);
        parent(name, parents[i].gid, parents[i].mode, p);
        as(1000, 1001, 1000, 2, groups, p, name);
    }
#else
    // A parent in the caller's own group, in a supplementary group (12), in
    // that group and set-group-ID, and in wheel (0), which the caller is not in.
    parent("plain-egid", getegid(), 0777, p);
    create_in(p, "plain-egid");
    parent("plain-supplementary-12", 12, 0777, p);
    create_in(p, "plain-supplementary-12");
    parent("sgid-supplementary-12", 12, 02777, p);
    create_in(p, "sgid-supplementary-12");
    parent("plain-inherited-group", (gid_t)-1, 0777, p);
    create_in(p, "plain-inherited-group");
#endif
    return 0;
}
