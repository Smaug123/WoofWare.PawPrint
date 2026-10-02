/* Whether the C library a .NET P/Invoke of "libc" binds exports gettid.
 *
 * Build with `clang -o gettid-symbol gettid-symbol.c`.
 *
 * Measured 2026-10-01 on Darwin 27.0.0 arm64: libc.dylib loads, and dlsym finds
 * pthread_threadid_np, syscall and getpid but not gettid (NULL), so on Darwin a
 * P/Invoke of libc's gettid fails to bind. glibc has exported gettid since 2.30.
 */
#include <dlfcn.h>
#include <stdio.h>
#include <unistd.h>

int main(void) {
    alarm(5);
    const char *names[] = {"gettid", "pthread_threadid_np", "syscall", "getpid"};
    void *libc = dlopen("libc.dylib", RTLD_NOW);
    for (int i = 0; i < 4; i++) {
        void *p = dlsym(libc ? libc : RTLD_DEFAULT, names[i]);
        printf("%s libc=%p sym=%p\n", names[i], libc, p);
    }
    return 0;
}
