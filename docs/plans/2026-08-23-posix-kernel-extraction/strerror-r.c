// Measures System.Native's SystemNative_StrErrorR, the shim CoreLib's
// Interop.Sys.StrError calls for every errno-built exception message and for
// Marshal.GetPInvokeErrorMessage: the text it answers for every number, what
// it returns (the caller's buffer, NULL, or a pointer of the C library's own),
// and what it leaves in the buffer at every interesting buffer size.
//
// The shim is dlopen'd from a real .NET install, so both halves are measured
// together: the shim's own arms (a negative size, the two pseudo-errnos it
// answers through gai_strerror and a literal) and the C library's strerror_r,
// GNU-shaped against glibc and XSI-shaped against Darwin's libc.
//
// Darwin: nix develop -c clang -Wall -o /tmp/strerror-r strerror-r.c
//         /tmp/strerror-r <dotnet>/shared/Microsoft.NETCore.App/<v>/libSystem.Native.dylib
// Linux:  copy libSystem.Native.so out of mcr.microsoft.com/dotnet/runtime:10.0
//         (add --arch amd64 for the x86-64 one) into $LIB, then
//         container run --rm -v "$PWD":/probe -v "$LIB":/lib-dotnet debian:trixie sh -c
//           'apt-get update -qq && apt-get install -y -qq gcc libc6-dev &&
//            gcc -Wall -o /tmp/p /probe/strerror-r.c -ldl &&
//            /tmp/p /lib-dotnet/libSystem.Native.so'
//         (That image's own apt mirror failed intermittently; the arm64
//         binary built on trixie was also run inside it, against its glibc
//         2.39, and printed the same bytes.)
// Linux, a glibc built against Linux 7.2's uapi headers (whose table names
// errno 134, EFTYPE): nixpkgs c9fe7d1 ships one, glibc 2.44, and links its
// dotnet's shim against it. In the nixos/nix image, with
// N="nix --extra-experimental-features 'nix-command flakes'" and
// P=github:NixOS/nixpkgs/c9fe7d12cd78d1adcd12dd15e24432dde5b155a0:
//           SDK=$($N build --no-link --print-out-paths $P#dotnetCorePackages.sdk_10_0 | grep -v -- -man$)
//           $N shell $P#gcc -c gcc -Wall -o /tmp/p strerror-r.c -ldl
//           /tmp/p $SDK/share/dotnet/shared/Microsoft.NETCore.App/10.0.12/libSystem.Native.so
//
// The probe never calls setlocale, so the C library answers in the "C" locale.
// So does a .NET process, whatever LANG and LC_ALL say: strerror-r-locale.cs
// beside this file measures that.
//
// Output rows (tab-separated):
//   TEXT  <n> <return> <changed> <errno> <text>
//         SystemNative_StrErrorR(n, buffer, 1024) for every n in [-300, 4096]
//         and a few extremes. <return> is BUFFER, NULL or OTHER (a pointer
//         that is neither); <changed> is how many of the buffer's 1024 bytes
//         differ from the fill byte afterwards; <errno> is "kept" if errno
//         still holds the value set just before the call, or its new value;
//         <text> is the string at the returned pointer, or at the buffer for
//         NULL.
//   IDENTITY <n> same|different
//         After an OTHER row: whether a second call, with another buffer,
//         returns the same pointer.
//   SIZE  <n> <size> <return> <beyond> <errno> <buffer> [<other>]
//         The same call at the sizes around the text's length and a few
//         fixed ones, for n in [-3, 140] and the extremes. <buffer> is the
//         first max(size, 0) bytes of the buffer, capped at 96, escaped (the
//         fill byte is \xaa); <beyond> says whether any byte past `size` (of
//         64 checked) changed; <other> is the string at an OTHER return.
// Each row is computed from a freshly-filled buffer.
//
// Measured: see the header of each output file beside this one.
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define FILL 0xAA
#define BUF 2048

typedef const char *(*StrErrorR)(int32_t, char *, int32_t);

static StrErrorR shim;
static char buffer[BUF];

static const char *kind(const char *ret) {
    if (ret == NULL) return "NULL";
    if (ret == buffer) return "BUFFER";
    return "OTHER";
}

// Escape bytes so the row stays on one line and the fill byte is visible.
static void escaped(const char *bytes, int count) {
    for (int i = 0; i < count; i++) {
        unsigned char c = (unsigned char)bytes[i];
        if (c >= 0x20 && c < 0x7f && c != '\\') putchar(c);
        else printf("\\x%02x", c);
    }
}

static void text_row(int32_t n) {
    memset(buffer, FILL, BUF);
    errno = 4242;
    const char *ret = shim(n, buffer, 1024);
    int after = errno;
    int changed = 0;
    for (int i = 0; i < 1024; i++)
        if ((unsigned char)buffer[i] != FILL) changed++;
    printf("TEXT\t%d\t%s\t%d\t", n, kind(ret), changed);
    if (after == 4242) printf("kept\t"); else printf("%d\t", after);
    // A NULL return leaves the message in the buffer, NUL-terminated.
    const char *at = ret == NULL ? buffer : ret;
    escaped(at, (int)strnlen(at, 1024));
    printf("\n");
    if (ret != NULL && ret != buffer) {
        // A pointer of the library's own: is it the same one every call?
        static char other[16];
        const char *again = shim(n, other, (int32_t)sizeof other);
        printf("IDENTITY\t%d\t%s\n", n, again == ret ? "same" : "different");
    }
}

static void size_row(int32_t n, int32_t size) {
    memset(buffer, FILL, BUF);
    errno = 4242;
    const char *ret = shim(n, buffer, size);
    int after = errno;
    int shown = size <= 0 ? 0 : (size > 96 ? 96 : size);
    int beyondFrom = size <= 0 ? 0 : size;
    int beyond = 0;
    for (int i = beyondFrom; i < beyondFrom + 64 && i < BUF; i++)
        if ((unsigned char)buffer[i] != FILL) beyond = 1;
    printf("SIZE\t%d\t%d\t%s\t%s\t", n, size, kind(ret), beyond ? "touched" : "untouched");
    if (after == 4242) printf("kept\t"); else printf("%d\t", after);
    escaped(buffer, shown);
    if (ret != NULL && ret != buffer) {
        printf("\t");
        escaped(ret, (int)strlen(ret));
    }
    printf("\n");
}

int main(int argc, char **argv) {
    alarm(60);
    if (argc < 2) { fprintf(stderr, "usage: %s <libSystem.Native>\n", argv[0]); return 2; }
    void *lib = dlopen(argv[1], RTLD_NOW);
    if (!lib) { printf("dlopen failed: %s\n", dlerror()); return 1; }
    shim = (StrErrorR)dlsym(lib, "SystemNative_StrErrorR");
    if (!shim) { printf("dlsym failed\n"); return 1; }

    static const int32_t extremes[] = {
        INT_MIN, INT_MIN + 1, -0x20003, -0x20002, -0x20001, -0x20000, -0x10000, INT_MAX - 1, INT_MAX
    };
    const int nExtremes = (int)(sizeof extremes / sizeof extremes[0]);

    for (int i = 0; i < nExtremes; i++) text_row(extremes[i]);
    for (int32_t n = -300; n <= 4096; n++) text_row(n);

    static const int32_t fixed[] = { INT_MIN, -2, -1, 0, 1, 2, 3, 4, 8, 16, 1024 };
    const int nFixed = (int)(sizeof fixed / sizeof fixed[0]);

    for (int i = -nExtremes; i <= 143; i++) {
        int32_t n = i < 0 ? extremes[nExtremes + i] : i - 3;
        // The text's length, from a call with room to spare.
        memset(buffer, FILL, BUF);
        const char *full = shim(n, buffer, 1024);
        int32_t len = (int32_t)strlen(full == NULL ? buffer : full);
        for (int j = 0; j < nFixed; j++) size_row(n, fixed[j]);
        for (int32_t s = len - 2; s <= len + 2; s++)
            if (s > 0) size_row(n, s);
    }
    return 0;
}
