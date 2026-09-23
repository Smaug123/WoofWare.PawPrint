// What the kernel's entropy syscall answers: `getrandom(2)` on Linux,
// `getentropy(2)` on Darwin (through libc and through the raw syscall, which
// agree).
//
// Prints one row per (buffer, length, flags): the return value, and the errno
// when it is -1. The buffers are a real one, NULL, and the last address there
// is; the large-count rows need about 2 GiB of memory, so a Linux container
// wants `-m 6G`.
//
// Measured on Linux 6.18.5 (arm64, 4 KiB pages) and Darwin 25.6.0 (arm64).
// `TestEntropyAgainstHost` asks the host the same questions.
//
// Build: cc -Wall -Wno-deprecated-declarations -o entropy-syscalls entropy-syscalls.c
#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/utsname.h>

#ifdef __linux__
#include <sys/random.h>
static long call(void *buf, size_t len, unsigned int flags) {
    return syscall(SYS_getrandom, buf, len, flags);
}
#else
#include <sys/random.h>
static long call(void *buf, size_t len, unsigned int flags) {
    (void)flags;
    return getentropy(buf, len);
}
static long rawcall(void *buf, size_t len) {
    return syscall(SYS_getentropy, buf, len);
}
#endif

static unsigned char small[4096];

static void row(const char *label, void *buf, size_t len, unsigned int flags) {
    errno = 0;
    long r = call(buf, len, flags);
    int e = errno;
    printf("%-40s len=%-12zu flags=0x%08x -> ret=%ld errno=%d (%s)\n", label, len, flags, r, r < 0 ? e : 0,
           r < 0 ? strerror(e) : "-");
#ifndef __linux__
    errno = 0;
    long r2 = rawcall(buf, len);
    int e2 = errno;
    printf("%-40s   raw syscall -> ret=%ld errno=%d\n", "", r2, r2 < 0 ? e2 : 0);
#endif
}

int main(void) {
    struct utsname u;
    uname(&u);
    printf("%s %s %s\n", u.sysname, u.release, u.machine);
    void *wild = (void *)(uintptr_t)-1;
    row("ok 16", small, 16, 0);
    row("ok 0", small, 0, 0);
    row("NULL 0", NULL, 0, 0);
    row("NULL 5", NULL, 5, 0);
    row("wild 0", wild, 0, 0);
    row("wild 5", wild, 5, 0);
#ifdef __linux__
    for (unsigned int f = 0; f <= 16; f++) row("ok 16 flags", small, 16, f);
    row("ok 16 flags", small, 16, 0x80000000u);
    row("ok 16 flags", small, 16, 0xFFFFFFFFu);
    row("ok 0 badflag 8", small, 0, 8);
    row("ok 0 RANDOM|INSECURE", small, 0, 6);
    row("NULL 5 badflag 8", NULL, 5, 8);
    row("wild 5 badflag 8", wild, 5, 8);
    row("wild 5 RANDOM|INSECURE", wild, 5, 6);
    row("wild 0 badflag 8", wild, 0, 8);
    row("NULL 0 badflag 8", NULL, 0, 8);
    // Range-end screen: last page of user space and one past.
    row("ok 4096", small, 4096, 0);
    row("ok 4096 NONBLOCK", small, 4096, 1);
    row("ok 4096 RANDOM", small, 4096, 2);
    row("ok 4096 INSECURE", small, 4096, 4);
    row("ok 4096 RANDOM|NONBLOCK", small, 4096, 3);
#else
    row("ok 255", small, 255, 0);
    row("ok 256", small, 256, 0);
    row("ok 257", small, 257, 0);
    row("ok 4096", small, 4096, 0);
    row("NULL 257", NULL, 257, 0);
    row("wild 257", wild, 257, 0);
    row("wild 256", wild, 256, 0);
    row("NULL 256", NULL, 256, 0);
    row("ok SIZE_MAX", small, SIZE_MAX, 0);
#endif

    // Large counts: how much does one call return?
    size_t sizes[] = {(size_t)1 << 20, (size_t)1 << 25, (size_t)1 << 28, (size_t)0x7FFFFFFF, (size_t)0x80000000,
                      (size_t)0x80001000};
    size_t mapLen = (size_t)0x80002000;
    unsigned char *big = mmap(NULL, mapLen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS
#ifdef MAP_NORESERVE
                                                                           | MAP_NORESERVE
#endif
                              , -1, 0);
    if (big == MAP_FAILED) {
        perror("mmap");
        return 1;
    }
    for (size_t i = 0; i < sizeof(sizes) / sizeof(sizes[0]); i++) {
        char label[64];
        snprintf(label, sizeof label, "big %zu", sizes[i]);
        row(label, big, sizes[i], 0);
    }
#ifdef __linux__
    // Beyond INT_MAX with a size_t that cannot possibly be mapped: clamped before the screen?
    row("big SIZE_MAX", big, SIZE_MAX, 0);
    row("big 1<<40", big, (size_t)1 << 40, 0);
#endif
    return 0;
}
