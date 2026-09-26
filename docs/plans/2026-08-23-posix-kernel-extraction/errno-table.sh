#!/bin/sh
# errno-table.sh: every E* constant this platform's <errno.h> defines, as the
# C compiler sees it, beside the running libc's strerror() of that number; then
# a strerror() sweep for numbers the header names nothing for; then, if a
# libSystem.Native is named, what the .NET shim's
# SystemNative_ConvertErrorPlatformToPal answers for every number swept.
#
# The name list is not hand-picked: it is every macro matching ^E[A-Z0-9]+$
# that `$CC -dM -E` reports after `#include <errno.h>`, so an alias
# (EWOULDBLOCK, EDEADLOCK, glibc's ENOTSUP) prints under its own name with the
# number it resolves to.
#
# Usage: errno-table.sh [path-to-libSystem.Native]
#   CC defaults to cc; CFLAGS is passed when reading the header and compiling,
#   LDLIBS when linking. Output rows:
#     DEFINE <name> <value> <strerror text>
#     SWEEP  <n> <strerror text>      (only numbers no DEFINE row names)
#     PAL    <n> 0x<Interop.Error>   (only if a shim was given)
#
# Run, from this directory:
#   Linux (aarch64 userland, then x86-64 under Rosetta; the shim is copied out
#   of mcr.microsoft.com/dotnet/runtime:10.0 for the matching architecture):
#     container run --rm -v "$PWD:/probe" gcc:14 sh -c \
#       'cd /probe && LDLIBS=-ldl CC=gcc ./errno-table.sh lib-arm64/libSystem.Native.so'
#     container run --rm --arch amd64 -v "$PWD:/probe" gcc:14 sh -c \
#       'cd /probe && LDLIBS=-ldl CC=gcc ./errno-table.sh lib-amd64/libSystem.Native.so'
#   Darwin, against the macOS 26.4 SDK's headers (the Nix devshell's clang
#   links, but its 14.4 SDK predates ENOTCAPABLE):
#     nix develop -c sh -c "CC=clang CFLAGS='-isysroot \
#       /Library/Developer/CommandLineTools/SDKs/MacOSX26.4.sdk' \
#       ./errno-table.sh <dotnet>/shared/Microsoft.NETCore.App/<v>/libSystem.Native.dylib"
#
# Measured 2026-09-26:
#   Linux 6.18.5 (the container VM), glibc 2.41, linux-libc-dev 6.12.107
#   (Debian trixie), whose asm-generic/errno-base.h and asm-generic/errno.h
#   both architectures include unchanged; shims of .NET 10.0.11 (aarch64) and
#   10.0.12 (x86-64). The two userlands printed identical rows: the header
#   numbers are a fact of the userland ABI, so Rosetta does not qualify them.
#   Darwin 27.0.0 arm64, the macOS 26.4 SDK, the shim of .NET 10.0.7.
# Findings:
#   * Linux names 1..133 except 41 and 58; Darwin names 1..107. strerror knows
#     exactly those numbers in 1..4096 on each, and nothing the header lacks.
#   * They agree only on 1..34 except 11. No error above 34 shares a number.
#   * Aliases: EWOULDBLOCK = EAGAIN on both; EDEADLOCK = EDEADLK and glibc's
#     ENOTSUP = EOPNOTSUPP on Linux; ELAST is Darwin's bound, not an errno.
#   * The macOS 14.4 SDK differs from 26.4 only in lacking ENOTCAPABLE (107),
#     which Darwin 27's strerror knows ("Capabilities insufficient").
#   * The shim's PAL answer is, on both, the Interop.Error member named by the
#     number's error, ENONSTANDARD where the enum has no member for that error
#     or the number names none, and SUCCESS for 0: no exceptions in the sweep.
#     Its non-ENONSTANDARD rows are transcribed in TestUnixErrorPal, and the
#     header rows in WoofWare.PosixKernel.Test/ErrnoHeaders.fs.
set -eu
CC=${CC:-cc}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

printf '#include <errno.h>\n' | $CC ${CFLAGS:-} -dM -E -x c - \
    | sed -n 's/^#define \(E[A-Z0-9]*\) .*/\1/p' | sort -u > "$WORK/names"

{
    cat <<'C'
#include <dlfcn.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int named[4097];

static void row(const char *name, long value)
{
    printf("DEFINE %s %ld %s\n", name, value, strerror((int)value));
    if (value >= 0 && value <= 4096) named[value] = 1;
}

int main(int argc, char **argv)
{
    alarm(10);
C
    while read -r name; do
        printf '#ifdef %s\n    row("%s", (long)(%s));\n#endif\n' "$name" "$name" "$name"
    done < "$WORK/names"
    cat <<'C'
    /* Numbers the header names nothing for, from 0 to 4096: strerror's text
       says whether the running libc knows them anyway. */
    for (int n = 0; n <= 4096; n++)
        if (!named[n]) printf("SWEEP %d %s\n", n, strerror(n));

    if (argc > 1) {
        void *lib = dlopen(argv[1], RTLD_NOW);
        if (!lib) { printf("PAL dlopen failed: %s\n", dlerror()); return 1; }
        int32_t (*conv)(int32_t) =
            (int32_t (*)(int32_t))dlsym(lib, "SystemNative_ConvertErrorPlatformToPal");
        if (!conv) { printf("PAL dlsym failed\n"); return 1; }
        static const int32_t extra[] = { INT_MIN, -0x20002, -0x20001, INT_MAX };
        for (unsigned i = 0; i < sizeof extra / sizeof extra[0]; i++)
            printf("PAL %d 0x%x\n", extra[i], (unsigned)conv(extra[i]));
        for (int32_t n = -300; n <= 4096; n++)
            printf("PAL %d 0x%x\n", n, (unsigned)conv(n));
    }
    return 0;
}
C
} > "$WORK/probe.c"

# Compiled and linked separately, so CFLAGS can name headers (an -isysroot)
# that the linker need not also accept.
$CC ${CFLAGS:-} -Wall -c -o "$WORK/probe.o" "$WORK/probe.c"
$CC -o "$WORK/probe" "$WORK/probe.o" ${LDLIBS:-}
"$WORK/probe" "$@"
