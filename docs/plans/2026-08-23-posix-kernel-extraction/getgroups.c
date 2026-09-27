// Measures getegid(2) and getgroups(2): what list getgroups reports for a
// given set of credentials, and how it screens its size and its buffer.
//
// Darwin: nix develop -c clang -Wall -o getgroups getgroups.c && ./getgroups
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Wno-stringop-overflow -o /tmp/p /probe/getgroups.c && /tmp/p'
// (glibc declares getgroups' buffer write-only for `size` elements, so gcc
// warns about the deliberately negative sizes.)
//
// On Linux run it as root: each list row forks a child that calls setgroups,
// setregid and setreuid, and only then asks. On Darwin, as an ordinary user,
// no row can change the credentials, so only the process's own list is asked
// about.
//
// Measured on Linux 6.18.5 (aarch64, root in the container, gcc:14) and Darwin
// 27.0 (arm64, uid 501) on 2026-09-27; the output is beside this file as
// getgroups.linux-6.18.5-aarch64.txt and getgroups.darwin-27.0-uid501.txt, and
// the rows are transcribed in WoofWare.PosixKernel.Test/TestGetGroups.fs.
#define _GNU_SOURCE
#include <errno.h>
#include <grp.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EINVAL: return "EINVAL";
    case EPERM: return "EPERM";
    case EFAULT: return "EFAULT";
    default: {
        static char b[32];
        snprintf(b, sizeof b, "errno%d", e);
        return b;
    }
    }
}

#define SENTINEL 0xA5A5A5A5u
#define CAP 256

static void show(const char *label, int size, gid_t *buf, int cap_shown) {
    for (int i = 0; i < CAP; i++) buf[i] = SENTINEL;
    errno = 0;
    int r = getgroups(size, buf);
    int e = errno;
    printf("  %-34s -> %d %s |", label, r, r < 0 ? en(e) : "");
    for (int i = 0; i < cap_shown; i++) {
        if (buf[i] == SENTINEL) printf(" .");
        else printf(" %u", (unsigned)buf[i]);
    }
    printf("\n");
}

static void ask_screens(void) {
    static gid_t buf[CAP];
    int count = getgroups(0, NULL);
    printf("  egid=%u rgid=%u euid=%u count(getgroups(0,NULL))=%d\n", (unsigned)getegid(), (unsigned)getgid(),
           (unsigned)geteuid(), count);
    int shown = count + 2 < CAP ? count + 2 : CAP;
    // Every size from 0 to two past the count, then a spread of negatives.
    for (int size = 0; size <= count + 2 && size <= CAP; size++) {
        char label[40];
        snprintf(label, sizeof label, "size %d", size);
        show(label, size, buf, shown);
    }
    show("size CAP", CAP, buf, shown);
    static const int negatives[] = {-1, -2, -16, -65536, INT_MIN + 1, INT_MIN};
    for (size_t i = 0; i < sizeof negatives / sizeof negatives[0]; i++) {
        char label[40];
        snprintf(label, sizeof label, "size %d", negatives[i]);
        show(label, negatives[i], buf, shown);
    }

    // NULL and unmapped buffers, each against a size that would be accepted.
    errno = 0;
    int r = getgroups(count, NULL);
    printf("  %-34s -> %d %s\n", "size count, NULL", r, r < 0 ? en(errno) : "");
    errno = 0;
    r = getgroups(count + 1, NULL);
    printf("  %-34s -> %d %s\n", "size count+1, NULL", r, r < 0 ? en(errno) : "");
    if (count > 0) {
        errno = 0;
        r = getgroups(count - 1, NULL);
        printf("  %-34s -> %d %s\n", "size count-1, NULL", r, r < 0 ? en(errno) : "");
    }
    errno = 0;
    r = getgroups(-1, NULL);
    printf("  %-34s -> %d %s\n", "size -1, NULL", r, r < 0 ? en(errno) : "");

    // A buffer that starts one element before an inaccessible page.
    long page = sysconf(_SC_PAGESIZE);
    char *two = mmap(NULL, (size_t)(2 * page), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (two == MAP_FAILED) { perror("mmap"); return; }
    if (mprotect(two + page, (size_t)page, PROT_NONE) != 0) { perror("mprotect"); return; }
    gid_t *edge = (gid_t *)(two + page) - 1;
    *edge = SENTINEL;
    if (count >= 2) {
        errno = 0;
        r = getgroups(count, edge);
        printf("  %-34s -> %d %s | first element %s\n", "size count, straddling PROT_NONE", r,
               r < 0 ? en(errno) : "", *edge == SENTINEL ? "untouched" : "written");
    }
    // A read-only buffer.
    if (mprotect(two, (size_t)page, PROT_READ) != 0) { perror("mprotect ro"); return; }
    errno = 0;
    r = getgroups(count, (gid_t *)two);
    printf("  %-34s -> %d %s\n", "size count, PROT_READ", r, r < 0 ? en(errno) : "");
    munmap(two, (size_t)(2 * page));
}

static void as(const char *label, gid_t egid, int n, const gid_t *list) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        if (setgroups((size_t)n, list) != 0) { printf("%s: setgroups %s\n", label, en(errno)); _exit(1); }
        // setregid/setreuid as root set the saved ids too, and exist on both.
        if (setregid(egid, egid) != 0) { printf("%s: setregid %s\n", label, en(errno)); _exit(1); }
        if (setreuid(1000, 1000) != 0) { printf("%s: setreuid %s\n", label, en(errno)); _exit(1); }
        printf("%s: setgroups(", label);
        for (int i = 0; i < n; i++) printf(i ? ",%u" : "%u", (unsigned)list[i]);
        printf(") egid %u\n", (unsigned)egid);
        ask_screens();
        fflush(stdout);
        _exit(0);
    }
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status)) printf("%s: child died, status %d\n", label, status);
}

int main(void) {
    alarm(60);
    printf("ngroups_max=%ld\n", sysconf(_SC_NGROUPS_MAX));
    printf("this process:\n");
    ask_screens();
    if (geteuid() != 0) {
        printf("not root: the setgroups rows need root\n");
        return 0;
    }
    static const gid_t unsorted[] = {30, 10, 20};
    static const gid_t dups[] = {7, 7, 3, 7};
    static const gid_t with_egid[] = {1000, 5, 1000};
    static const gid_t one[] = {5};
    static const gid_t descending[] = {20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
    static const gid_t big[] = {4294967294u, 0, 2147483648u, 65536};
    as("empty", 1000, 0, NULL);
    as("one", 1000, 1, one);
    as("unsorted", 1000, 3, unsorted);
    as("duplicates", 1000, 4, dups);
    as("with-egid", 1000, 3, with_egid);
    as("descending-20", 1000, 20, descending);
    as("large-ids", 1000, 4, big);
    as("egid-not-listed", 4242, 3, unsorted);
    return 0;
}
