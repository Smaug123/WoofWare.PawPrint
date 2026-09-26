// Shared raw-directory-read helpers for the directory-*.c probes.
// Linux: getdents64 via syscall(2). Darwin: __getdirentries64 from libsystem_kernel.
// Records are parsed from raw bytes at explicit offsets so that nothing
// depends on a libc's `struct dirent`.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#include <sys/vfs.h>
#define FLAVOUR "linux"
#else
#include <sys/mount.h>
#define FLAVOUR "darwin"
extern ssize_t __getdirentries64(int fd, void *buf, size_t bufsize, off_t *basep);
#endif

typedef struct {
    uint64_t ino;
    int64_t off;   // d_off (Linux) or d_seekoff (Darwin)
    uint16_t reclen;
    int namlen;    // d_namlen on Darwin; strlen on Linux
    uint8_t type;
    char name[1100];
    size_t at;     // byte offset of the record within the buffer
    int padNonZero; // count of non-zero bytes between the NUL and reclen
    int nameOffset;
} rec;

static __attribute__((unused)) long rawgetdents(int fd, void *buf, size_t size, long long *base) {
#ifdef __linux__
    if (base) *base = -999;
    return syscall(SYS_getdents64, fd, buf, size);
#else
    off_t b = -999;
    long r = __getdirentries64(fd, buf, size, &b);
    if (base) *base = b;
    return r;
#endif
}

static __attribute__((unused)) int parse(const unsigned char *buf, long n, rec *out, int max) {
    long pos = 0;
    int k = 0;
    while (pos < n && k < max) {
        rec *r = &out[k];
        memcpy(&r->ino, buf + pos, 8);
        memcpy(&r->off, buf + pos + 8, 8);
        memcpy(&r->reclen, buf + pos + 16, 2);
#ifdef __linux__
        r->type = buf[pos + 18];
        r->nameOffset = 19;
#else
        uint16_t nl;
        memcpy(&nl, buf + pos + 18, 2);
        r->namlen = nl;
        r->type = buf[pos + 20];
        r->nameOffset = 21;
#endif
        const char *nm = (const char *)buf + pos + r->nameOffset;
        size_t len = strnlen(nm, r->reclen - r->nameOffset);
        memcpy(r->name, nm, len);
        r->name[len] = 0;
#ifdef __linux__
        r->namlen = (int)len;
#endif
        r->padNonZero = 0;
        for (long i = r->nameOffset + (long)len + 1; i < r->reclen; i++)
            if (buf[pos + i] != 0) r->padNonZero++;
        r->at = pos;
        if (r->reclen == 0) { fprintf(stderr, "zero reclen\n"); exit(3); }
        pos += r->reclen;
        k++;
    }
    return k;
}

static __attribute__((unused)) const char *fsname(const char *path) {
    static char b[64];
#ifdef __linux__
    struct statfs s;
    if (statfs(path, &s) != 0) return "?";
    snprintf(b, sizeof b, "magic=0x%lx", (unsigned long)s.f_type);
#else
    struct statfs s;
    if (statfs(path, &s) != 0) return "?";
    snprintf(b, sizeof b, "%s", s.f_fstypename);
#endif
    return b;
}

static __attribute__((unused)) void rmrf(const char *path) {
    char cmd[4096];
    snprintf(cmd, sizeof cmd, "rm -rf '%s'", path);
    if (system(cmd) != 0) { fprintf(stderr, "rm failed\n"); }
}

static __attribute__((unused)) void touch(const char *path) {
    int fd = open(path, O_CREAT | O_WRONLY | O_EXCL, 0644);
    if (fd < 0) { perror(path); exit(2); }
    close(fd);
}
