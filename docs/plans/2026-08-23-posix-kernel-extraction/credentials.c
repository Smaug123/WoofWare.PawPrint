// Measures the facts `Credentials` rests on: which ids a process can hold,
// how many supplementary groups it can hold, and which of its ids decides
// whether it is privileged.
//
// Darwin: nix develop -c clang -Wall -o credentials credentials.c && ./credentials
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -o /tmp/p /probe/credentials.c && /tmp/p'
//
// On Linux run it as root: every row forks a child that takes the credentials
// under test before asking its question. On Darwin, as an ordinary user, only
// the rows that need no privilege say anything; the rest report EPERM, which
// is the answer to "may you change your ids" rather than to the question.
//
// Measured on Linux 6.18.5 (aarch64, root in the container) and Darwin 27.0
// (arm64, uid 501) on 2026-09-26. The rows are transcribed in
// WoofWare.PosixKernel.Test/TestCredentials.fs.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EINVAL: return "EINVAL";
    case EPERM: return "EPERM";
    case EACCES: return "EACCES";
    case EADDRINUSE: return "EADDRINUSE";
    default: {
        static char b[32];
        snprintf(b, sizeof b, "errno%d", e);
        return b;
    }
    }
}

// Run `what` in a child so that a successful id change cannot leak into the next row.
static void in_child(const char *label, int (*what)(void)) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        int e = what();
        printf("%-58s %s\n", label, en(e));
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
}

static int setuid_minus_one(void) { return setuid((uid_t)-1) == 0 ? 0 : errno; }
static int setgid_minus_one(void) { return setgid((gid_t)-1) == 0 ? 0 : errno; }
static int setuid_max_minus_one(void) { return setuid((uid_t)0xFFFFFFFE) == 0 ? 0 : errno; }
static int setgroups_with_minus_one(void) {
    gid_t g[2] = {1000, (gid_t)-1};
    return setgroups(2, g) == 0 ? 0 : errno;
}

static int setgroups_n(int n) {
    gid_t *g = calloc((size_t)n, sizeof(gid_t));
    for (int i = 0; i < n; i++) g[i] = (gid_t)(1000 + i);
    int r = setgroups(n, g) == 0 ? 0 : errno;
    free(g);
    return r;
}

static long ngroups_max;
static int setgroups_at_max(void) { return setgroups_n((int)ngroups_max); }
static int setgroups_above_max(void) { return setgroups_n((int)ngroups_max + 1); }

#ifdef __linux__
static int bind_port(uint16_t port) {
    int s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0) return errno;
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_port = htons(port);
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    int e = bind(s, (struct sockaddr *)&a, sizeof a) == 0 ? 0 : errno;
    close(s);
    return e;
}

static int real0_effective1000_bind(void) {
    if (setresuid(0, 1000, 0) != 0) return errno;
    return bind_port(80);
}

static int real1000_effective0_bind(void) {
    if (setresuid(1000, 0, 1000) != 0) return errno;
    return bind_port(80);
}

// A root-owned 0600 file, which only the superuser may open.
static const char *secret = "/tmp/credentials-probe-secret";

static int open_secret(void) {
    int fd = open(secret, O_RDONLY);
    if (fd < 0) return errno;
    close(fd);
    return 0;
}

static int real0_effective1000_open(void) {
    if (setresuid(0, 1000, 0) != 0) return errno;
    return open_secret();
}

static int real1000_effective0_open(void) {
    if (setresuid(1000, 0, 1000) != 0) return errno;
    return open_secret();
}

static int saved0_real_effective1000_bind(void) {
    if (setresuid(1000, 1000, 0) != 0) return errno;
    return bind_port(80);
}
#endif

int main(void) {
    alarm(60);
    ngroups_max = sysconf(_SC_NGROUPS_MAX);
    printf("uid=%u euid=%u gid=%u egid=%u NGROUPS_MAX=%ld\n", (unsigned)getuid(), (unsigned)geteuid(), (unsigned)getgid(),
           (unsigned)getegid(), ngroups_max);
    in_child("setuid((uid_t)-1)", setuid_minus_one);
    in_child("setgid((gid_t)-1)", setgid_minus_one);
    in_child("setuid(0xFFFFFFFE)", setuid_max_minus_one);
    in_child("setgroups({1000, (gid_t)-1})", setgroups_with_minus_one);
    in_child("setgroups(NGROUPS_MAX groups)", setgroups_at_max);
    in_child("setgroups(NGROUPS_MAX + 1 groups)", setgroups_above_max);
#ifdef __linux__
    in_child("real 0, effective 1000, saved 0: bind(127.0.0.1:80)", real0_effective1000_bind);
    in_child("real 1000, effective 0, saved 1000: bind(127.0.0.1:80)", real1000_effective0_bind);
    in_child("real 1000, effective 1000, saved 0: bind(127.0.0.1:80)", saved0_real_effective1000_bind);
    int fd = open(secret, O_CREAT | O_WRONLY | O_TRUNC, 0600);
    if (fd >= 0) close(fd);
    chmod(secret, 0600);
    in_child("real 0, effective 1000: open(root-owned 0600 file)", real0_effective1000_open);
    in_child("real 1000, effective 0: open(root-owned 0600 file)", real1000_effective0_open);
    unlink(secret);
#endif
    return 0;
}
