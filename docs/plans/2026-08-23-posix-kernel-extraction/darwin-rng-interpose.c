// Darwin: how libSystem's CCRandomGenerateBytes and arc4random_buf get their
// bytes. Interposes getentropy (and open/read, to see a /dev/random path),
// then calls each entry point with a sweep of sizes and reports, per call,
// how many getentropy calls were made and of what lengths.
//
// Build: clang -dynamiclib -o libinterpose.dylib darwin-rng-interpose.c -DINTERPOSE
//        clang -o rngcalls darwin-rng-interpose.c
// Run:   DYLD_INSERT_LIBRARIES=./libinterpose.dylib ./rngcalls
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/random.h>
#ifdef INTERPOSE
#include <stdarg.h>
#include <execinfo.h>
int interposed_calls;
size_t interposed_bytes;
char interposed_log[4096];
static int my_getentropy(void *buf, size_t n) {
    interposed_calls++;
    if (getenv("RNG_BACKTRACE")) { void *f[32]; int k = backtrace(f, 32); backtrace_symbols_fd(f, k, 2); write(2, "--\n", 3); }
    interposed_bytes += n;
    size_t l = strlen(interposed_log);
    if (l < sizeof interposed_log - 32) snprintf(interposed_log + l, sizeof interposed_log - l, " ge(%zu)", n);
    return getentropy(buf, n);
}
static int my_open(const char *p, int f, ...) {
    va_list ap; va_start(ap, f); int m = va_arg(ap, int); va_end(ap);
    size_t l = strlen(interposed_log);
    if (l < sizeof interposed_log - 64) snprintf(interposed_log + l, sizeof interposed_log - l, " open(%s)", p);
    return open(p, f, m);
}
__attribute__((used)) static struct { const void *r; const void *o; } interposers[] __attribute__((section("__DATA,__interpose"))) = {
    {(const void *)my_getentropy, (const void *)getentropy},
    {(const void *)my_open, (const void *)open},
};
#else
#include <dlfcn.h>
#include <CommonCrypto/CommonRandom.h>
int main(void) {
    alarm(30);
    int *calls = dlsym(RTLD_DEFAULT, "interposed_calls");
    char *log = dlsym(RTLD_DEFAULT, "interposed_log");
    if (!calls) { printf("not interposed\n"); return 1; }
    static unsigned char buf[1 << 20];
    printf("BEFORE-ANY calls=%d log=[%s]\n", *calls, log);
    size_t sizes[] = {1, 16, 32, 255, 256, 257, 4096, 65536, 1 << 20, 16};
    for (int which = 0; which < 2; which++)
        for (unsigned i = 0; i < sizeof sizes / sizeof *sizes; i++) {
            int before = *calls;
            log[0] = 0;
            if (which == 0) CCRandomGenerateBytes(buf, sizes[i]);
            else arc4random_buf(buf, sizes[i]);
            printf("%s %zu -> getentropy calls=%d log=[%s]\n", which == 0 ? "CCRandomGenerateBytes" : "arc4random_buf", sizes[i], *calls - before, log);
        }
    // Many small calls: is the generator reseeded after some volume?
    for (int which = 0; which < 2; which++) {
        int before = *calls;
        log[0] = 0;
        for (int i = 0; i < 200000; i++) {
            if (which == 0) CCRandomGenerateBytes(buf, 4096);
            else arc4random_buf(buf, 4096);
        }
        printf("%s 200000x4096 -> getentropy calls=%d log=[%.300s]\n", which == 0 ? "CCRandomGenerateBytes" : "arc4random_buf", *calls - before, log);
    }
    return 0;
}
#endif
