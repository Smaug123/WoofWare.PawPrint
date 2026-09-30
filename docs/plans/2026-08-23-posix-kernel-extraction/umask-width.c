// Measures umask(2): which bits of its argument each kernel stores, what a
// later call returns, whether the mask is per process or per thread, and how
// the stored mask applies to open(O_CREAT) and mkdir.
//
// Darwin: nix develop -c clang -Wall -Wno-deprecated-declarations -o umask-width umask-width.c -lpthread && ./umask-width
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -o /tmp/p /probe/umask-width.c -lpthread && /tmp/p && setpriv --reuid=1000 --regid=1000 --clear-groups /tmp/p'
//
// Each sweep states its rule and prints the number of rows that disagree with
// it, so a zero is a claim over the whole sweep, not over the rows shown.
//
// Two routes set the mask: libc's umask(), whose argument is a mode_t (32 bits
// on Linux, 16 on Darwin, so Darwin's C wrapper truncates before the kernel
// sees anything), and syscall(SYS_umask, int), which hands the kernel all 32
// bits on both. The kernel's own rule is the second route's.
//
// Measured on Linux 6.18.5 (aarch64, gcc:14, as root and as uid 1000) and
// Darwin 27.0 (arm64, uid 501) on 2026-09-27; the output is beside this file
// as umask-width.linux-6.18.5-aarch64.txt and umask-width.darwin-27.0-uid501.txt,
// and the rules are transcribed in WoofWare.PosixKernel.Test/TestUmask.fs.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __APPLE__
#define FLAVOUR "darwin"
#define OPEN_MASK 0777u
#define MKDIR_MASK 0777u
#else
#define FLAVOUR "linux"
#define OPEN_MASK 07777u
#define MKDIR_MASK 01777u
#endif

static unsigned raw_umask(unsigned v) {
    // The kernel's argument is an int on both, and its answer is the old mask.
    return (unsigned)syscall(SYS_umask, (int)v);
}

static unsigned libc_umask(unsigned v) { return (unsigned)umask((mode_t)v); }

static void *thread_sets(void *arg) {
    (void)arg;
    umask(0077);
    return NULL;
}

int main(void) {
    alarm(120);
    printf("flavour %s uid %u gid %u\n", FLAVOUR, (unsigned)getuid(), (unsigned)getgid());

    unsigned inherited = libc_umask(0);
    printf("inherited %04o\n", inherited);

    // 1. Every 12-bit argument, both routes: what does the next call return?
    for (int route = 0; route < 2; route++) {
        const char *name = route == 0 ? "libc" : "syscall";
        long m777 = 0, m7777 = 0;
        for (unsigned u = 0; u <= 07777; u++) {
            if (route == 0) libc_umask(u); else raw_umask(u);
            unsigned back = route == 0 ? libc_umask(0) : raw_umask(0);
            if (back != (u & 0777u)) m777++;
            if (back != (u & 07777u)) m7777++;
        }
        printf("SWEEP12\t%s\trows=4096\tmismatches(stored = arg & 0777)=%ld\tmismatches(stored = arg & 07777)=%ld\n",
               name, m777, m7777);
    }

    // 2. Every bit above the permission word, alone and over 07777 and 022.
    unsigned lows[] = { 0u, 07777u, 0022u, 0777u };
    for (int route = 0; route < 2; route++) {
        const char *name = route == 0 ? "libc" : "syscall";
        for (unsigned li = 0; li < sizeof lows / sizeof lows[0]; li++) {
            printf("HIGH\t%s\tlow %04o:", name, lows[li]);
            for (int bit = 12; bit < 32; bit++) {
                unsigned v = (1u << bit) | lows[li];
                if (route == 0) libc_umask(v); else raw_umask(v);
                unsigned back = route == 0 ? libc_umask(0) : raw_umask(0);
                printf(" %d:%04o", bit, back);
            }
            printf("\n");
        }
        unsigned extremes[] = { 0xffffffffu, 0x80000000u, 0x7fffffffu, 0xfffff000u, 0x0000ffffu, 0x00010000u | 0022u };
        for (unsigned i = 0; i < sizeof extremes / sizeof extremes[0]; i++) {
            if (route == 0) libc_umask(extremes[i]); else raw_umask(extremes[i]);
            unsigned back = route == 0 ? libc_umask(0) : raw_umask(0);
            printf("EXTREME\t%s\targ %#010x\treads back %04o\n", name, extremes[i], back);
        }
    }

    // 3. A chain of calls: each returns exactly what the previous one stored,
    //    whatever high bits the previous argument carried. Bounded LCG, 20000 steps.
    for (int route = 0; route < 2; route++) {
        const char *name = route == 0 ? "libc" : "syscall";
        uint32_t x = 12345u;
        unsigned expected777 = 0, expected7777 = 0;
        long m777 = 0, m7777 = 0;
        if (route == 0) libc_umask(0); else raw_umask(0);
        for (int i = 0; i < 20000; i++) {
            x = x * 1664525u + 1013904223u;
            unsigned v = x;
            unsigned prev = route == 0 ? libc_umask(v) : raw_umask(v);
            if (prev != expected777) m777++;
            if (prev != expected7777) m7777++;
            // libc's Darwin wrapper sees only the low 16 bits; both masks sit inside them.
            expected777 = v & 0777u;
            expected7777 = v & 07777u;
        }
        printf("CHAIN\t%s\trows=20000\tmismatches(returns previous arg & 0777)=%ld\tmismatches(returns previous arg & 07777)=%ld\n",
               name, m777, m7777);
    }

    // The explicit rows the brief names.
    libc_umask(0);
    unsigned a = raw_umask(0xfffff000u | 0777u);
    unsigned b = raw_umask(0022u);
    unsigned c = raw_umask(0u);
    printf("ROW\tsyscall(0xfffff000 | 0777) returned %04o; then syscall(022) returned %04o; then syscall(0) returned %04o\n", a, b, c);
    libc_umask(0777u);
    unsigned d = libc_umask(07777u);
    unsigned e = libc_umask(0u);
    printf("ROW\tumask(0777) then umask(07777) returned %04o; then umask(0) returned %04o\n", d, e);

    // 4. Per process or per thread: a second thread sets 077; the main thread reads.
    libc_umask(0022u);
    pthread_t t;
    pthread_create(&t, NULL, thread_sets, NULL);
    pthread_join(t, NULL);
    printf("THREAD\tafter another thread's umask(077), this thread reads %04o\n", libc_umask(0022u));

    // 5. The stored mask applied: every 12-bit umask argument, mode 07777, to
    //    open(O_CREAT) and mkdir, against mode & SYSCALL_MASK & ~stored, where
    //    stored is this flavour's width. A second prediction applies only the
    //    mask's low nine bits, which is what the two agree on unless the
    //    stored high bits take part.
    char base[] = "/tmp/umask-width-XXXXXX";
    if (!mkdtemp(base)) { perror("mkdtemp"); return 1; }
    // Darwin's /tmp is group wheel; give the scratch directory the caller's own group.
    if (chown(base, (uid_t)-1, getegid()) != 0) perror("chown base");
    unsigned width = 0;
    { libc_umask(07777u); width = libc_umask(0); }
    printf("width %04o\n", width);
    unsigned modes[] = { 07777u, 0666u, 02775u, 04755u };
    for (int s = 0; s < 2; s++) {
        const char *sname = s == 0 ? "open" : "mkdir";
        unsigned smask = s == 0 ? OPEN_MASK : MKDIR_MASK;
        long rows = 0, mFull = 0, mLow = 0;
        for (unsigned u = 0; u <= 07777; u++) {
            for (unsigned mi = 0; mi < sizeof modes / sizeof modes[0]; mi++) {
                char p[256];
                snprintf(p, sizeof p, "%s/e", base);
                libc_umask(u);
                int rc;
                if (s == 0) {
                    int fd = open(p, O_CREAT | O_EXCL | O_WRONLY, modes[mi]);
                    rc = fd < 0 ? -1 : 0;
                    if (fd >= 0) close(fd);
                } else {
                    rc = mkdir(p, modes[mi]);
                }
                int err = errno;
                libc_umask(0);
                struct stat st;
                if (rc != 0 || lstat(p, &st) != 0) {
                    printf("APPLY-FAIL\t%s\tu=%04o\tmode=%04o\t%s\n", sname, u, modes[mi], strerror(err));
                    return 1;
                }
                unsigned got = st.st_mode & 07777u;
                unsigned stored = u & width;
                unsigned wantFull = modes[mi] & smask & ~stored;
                unsigned wantLow = modes[mi] & smask & ~(stored & 0777u);
                rows++;
                if (got != wantFull) { mFull++; if (mFull < 6) printf("MISMATCH-FULL\t%s\tu=%04o\tmode=%04o\tgot=%04o\twant=%04o\n", sname, u, modes[mi], got, wantFull); }
                if (got != wantLow) mLow++;
                if (s == 0) unlink(p); else rmdir(p);
            }
        }
        printf("APPLY\t%s\trows=%ld\tmismatches(mode & %04o & ~stored)=%ld\tmismatches(mode & %04o & ~(stored & 0777))=%ld\n",
               sname, rows, smask, mFull, smask, mLow);
    }
    rmdir(base);
    libc_umask(inherited);
    return 0;
}
