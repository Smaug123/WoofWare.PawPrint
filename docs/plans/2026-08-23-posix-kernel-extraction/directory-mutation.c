// Directory-read probe: enumeration order, and what a mutation part-way
// through a scan does, at the syscall level (one record per call, no libc
// buffer); deleting each entry as it is returned; and the Darwin position's
// high word.
//
// Usage: directory-mutation <base-dir> <seed> <trials>
// Build as directory-layout.c. directory-mutation-models.py checks the output
// against the per-filesystem models in docs/divergences.md.
//
// Measured 2026-09-26 on Darwin 27.0.0 (arm64, APFS) and Linux 6.18.5 (arm64,
// tmpfs on /dev/shm), seeds 1 to 8 with 40 trials each: 320 order trials and
// 1280 mutation scripts per filesystem.
#include "directory-common.h"

static unsigned long long rng;
static unsigned rnd(void) {
    rng ^= rng << 13; rng ^= rng >> 7; rng ^= rng << 17;
    return (unsigned)(rng >> 11);
}

static unsigned char big[1 << 20];
static rec recs[20000];
static char root[4096];

// Names whose record size makes one-entry-per-call possible:
// Linux reclen = align8(20 + namlen): namlen 1..4 -> 24, buffer 24 reads one.
// Darwin reclen = (namlen + 32) & ~7: namlen 8..15 -> 40, buffer 64 (the floor) reads one;
// the first call returns both dots (32 + 32).
#ifdef __linux__
#define MINLEN 1
#define MAXLEN 4
#define ONEBUF 24
#else
#define MINLEN 8
#define MAXLEN 15
#define ONEBUF 64
#endif

static void randname(char *out) {
    int len = MINLEN + rnd() % (MAXLEN - MINLEN + 1);
    for (int i = 0; i < len; i++) out[i] = "abcdefghijklmnopqrstuvwxyz0123456789"[rnd() % 36];
    out[len] = 0;
}

typedef struct { char n[64][32]; int count; } nameset;

static int inset(nameset *s, const char *n) {
    for (int i = 0; i < s->count; i++) if (strcmp(s->n[i], n) == 0) return 1;
    return 0;
}

static void freshset(nameset *s, int count) {
    s->count = 0;
    while (s->count < count) {
        char n[32]; randname(n);
        if (!inset(s, n)) { strcpy(s->n[s->count++], n); }
    }
}

static void shuffle(int *p, int n) {
    for (int i = 0; i < n; i++) p[i] = i;
    for (int i = n - 1; i > 0; i--) { int j = rnd() % (i + 1); int t = p[i]; p[i] = p[j]; p[j] = t; }
}

static void list(const char *d, char *out, size_t outsz) {
    int fd = open(d, O_RDONLY | O_DIRECTORY);
    out[0] = 0;
    for (;;) {
        long n = rawgetdents(fd, big, sizeof big, NULL);
        if (n <= 0) break;
        int k = parse(big, n, recs, 20000);
        for (int i = 0; i < k; i++) {
            if (!strcmp(recs[i].name, ".") || !strcmp(recs[i].name, "..")) continue;
            snprintf(out + strlen(out), outsz - strlen(out), "%s ", recs[i].name);
        }
    }
    close(fd);
}

static void pth(char *out, const char *d, const char *n) { snprintf(out, 4096, "%s/%s", d, n); }

int main(int argc, char **argv) {
    alarm(600);
    if (argc < 4) return 1;
    rng = strtoull(argv[2], NULL, 10) * 2654435761ULL + 1;
    int trials = atoi(argv[3]);
    snprintf(root, sizeof root, "%s/dirprobe-mutate", argv[1]);
    rmrf(root);
    mkdir(root, 0755);
    printf("FLAVOUR %s fs=%s seed=%s\n", FLAVOUR, fsname(root), argv[2]);
    char p[4096], q[4096];
    static char out[1 << 16], out2[1 << 16];

    // ORDER: the same name set created in two different orders.
    for (int t = 0; t < trials; t++) {
        nameset s; freshset(&s, 1 + rnd() % 30);
        char a[4096], b[4096];
        snprintf(a, sizeof a, "%s/oa%d", root, t); snprintf(b, sizeof b, "%s/ob%d", root, t);
        mkdir(a, 0755); mkdir(b, 0755);
        int pa[64], pb[64]; shuffle(pa, s.count); shuffle(pb, s.count);
        printf("ORDER t=%d created_a=", t);
        for (int i = 0; i < s.count; i++) { pth(p, a, s.n[pa[i]]); touch(p); printf("%s ", s.n[pa[i]]); }
        printf("\nORDER t=%d created_b=", t);
        for (int i = 0; i < s.count; i++) { pth(p, b, s.n[pb[i]]); touch(p); printf("%s ", s.n[pb[i]]); }
        list(a, out, sizeof out); list(b, out2, sizeof out2);
        printf("\nORDER t=%d listed_a=%s\nORDER t=%d listed_b=%s\n", t, out, t, out2);
        // Delete half at random from a, then recreate one deleted name and add a new one; list again.
        int del[64]; shuffle(del, s.count);
        printf("ORDER t=%d deleted_a=", t);
        for (int i = 0; i < s.count / 2; i++) { pth(p, a, s.n[del[i]]); unlink(p); printf("%s ", s.n[del[i]]); }
        list(a, out, sizeof out);
        printf("\nORDER t=%d after_delete_a=%s\n", t, out);
        if (s.count / 2 > 0) {
            pth(p, a, s.n[del[0]]); touch(p);
            printf("ORDER t=%d recreated_a=%s\n", t, s.n[del[0]]);
            list(a, out, sizeof out);
            printf("ORDER t=%d after_recreate_a=%s\n", t, out);
        }
        // Rename a surviving name to a fresh name, and one over another survivor.
        if (s.count - s.count / 2 >= 2) {
            const char *src = s.n[del[s.count / 2]];
            char fresh[32]; do { randname(fresh); } while (inset(&s, fresh));
            pth(p, a, src); pth(q, a, fresh);
            rename(p, q);
            printf("ORDER t=%d renamed_a=%s->%s\n", t, src, fresh);
            list(a, out, sizeof out);
            printf("ORDER t=%d after_rename_a=%s\n", t, out);
            const char *src2 = s.n[del[s.count / 2 + 1]];
            pth(p, a, fresh); pth(q, a, src2);
            rename(p, q);
            printf("ORDER t=%d renamed_over_a=%s->%s\n", t, fresh, src2);
            list(a, out, sizeof out);
            printf("ORDER t=%d after_rename_over_a=%s\n", t, out);
        }
        rmrf(a); rmrf(b);
    }

    // MUTATE: one entry per call; after k names, perform one operation, then drain.
    const char *opnames[] = {"create", "unlink_ahead", "unlink_behind", "rename_ahead", "rename_behind", "unlink_last_create",
                             "rename_ahead_over_behind", "rename_behind_over_ahead", "mkdir_create"};
    for (int t = 0; t < trials * 4; t++) {
        nameset s; freshset(&s, 2 + rnd() % 20);
        char d[4096]; snprintf(d, sizeof d, "%s/m%d", root, t); mkdir(d, 0755);
        int perm[64]; shuffle(perm, s.count);
        for (int i = 0; i < s.count; i++) { pth(p, d, s.n[perm[i]]); touch(p); }
        list(d, out, sizeof out);
        int op = rnd() % 9;
        int k = rnd() % (s.count + 1);
        printf("MUTATE t=%d op=%s k=%d before=%s\n", t, opnames[op], k, out);
        int fd = open(d, O_RDONLY | O_DIRECTORY);
        char seen[64][32]; int nseen = 0;
        printf("MUTATE t=%d stream=", t);
        long long cur = -1;
        int guard = 0;
        // read until k names have been returned
        while (nseen < k && guard++ < 1000) {
            long n = rawgetdents(fd, big, ONEBUF, NULL);
            if (n <= 0) { printf("[early %ld] ", n); break; }
            int c = parse(big, n, recs, 20000);
            for (int i = 0; i < c; i++) {
                printf("%s ", recs[i].name);
                if (strcmp(recs[i].name, ".") && strcmp(recs[i].name, "..")) strcpy(seen[nseen++], recs[i].name);
            }
        }
        cur = lseek(fd, 0, SEEK_CUR);
        // choose targets
        char ahead[64][32]; int nahead = 0;
        for (int i = 0; i < s.count; i++) {
            int was = 0;
            for (int j = 0; j < nseen; j++) if (!strcmp(seen[j], s.n[i])) was = 1;
            if (!was) strcpy(ahead[nahead++], s.n[i]);
        }
        char fresh[32]; do { randname(fresh); } while (inset(&s, fresh));
        char desc[256] = "";
        switch (op) {
        case 0: pth(p, d, fresh); touch(p); snprintf(desc, sizeof desc, "+%s", fresh); break;
        case 1: if (nahead) { const char *y = ahead[rnd() % nahead]; pth(p, d, y); unlink(p); snprintf(desc, sizeof desc, "-%s", y); } break;
        case 2: if (nseen) { const char *z = seen[rnd() % nseen]; pth(p, d, z); unlink(p); snprintf(desc, sizeof desc, "-%s", z); } break;
        case 3: if (nahead) { const char *w = ahead[rnd() % nahead]; pth(p, d, w); pth(q, d, fresh); rename(p, q); snprintf(desc, sizeof desc, "%s>%s", w, fresh); } break;
        case 4: if (nseen) { const char *u = seen[rnd() % nseen]; pth(p, d, u); pth(q, d, fresh); rename(p, q); snprintf(desc, sizeof desc, "%s>%s", u, fresh); } break;
        case 5: if (nseen) { const char *z = seen[nseen - 1]; pth(p, d, z); unlink(p); pth(p, d, fresh); touch(p); snprintf(desc, sizeof desc, "-%s +%s", z, fresh); } break;
        case 6: if (nahead && nseen) { const char *w = ahead[rnd() % nahead]; const char *u = seen[rnd() % nseen]; pth(p, d, w); pth(q, d, u); rename(p, q); snprintf(desc, sizeof desc, "%s>>%s", w, u); } break;
        case 7: if (nahead && nseen) { const char *w = ahead[rnd() % nahead]; const char *u = seen[rnd() % nseen]; pth(p, d, u); pth(q, d, w); rename(p, q); snprintf(desc, sizeof desc, "%s>>%s", u, w); } break;
        case 8: { pth(p, d, fresh); mkdir(p, 0755); snprintf(desc, sizeof desc, "+%s/", fresh); } break;
        }
        printf("| op %s | ", desc);
        guard = 0;
        for (;;) {
            long n = rawgetdents(fd, big, ONEBUF, NULL);
            if (n < 0) { printf("[err %d] ", errno); break; }
            if (n == 0 || guard++ > 1000) break;
            int c = parse(big, n, recs, 20000);
            for (int i = 0; i < c; i++) printf("%s ", recs[i].name);
        }
        list(d, out, sizeof out);
        printf("\nMUTATE t=%d cur_at_op=%lld after=%s\n", t, cur, out);
        close(fd);
        rmrf(d);
    }

    // DRAIN: delete everything each call returned, at several buffer sizes; count survivors.
    size_t bufs[] = {ONEBUF, 256, 4096, 65536};
    for (int bi = 0; bi < 4; bi++) {
        for (int rep = 0; rep < 3; rep++) {
            char d[4096]; snprintf(d, sizeof d, "%s/drain", root); rmrf(d); mkdir(d, 0755);
            int N = 3000;
            for (int i = 0; i < N; i++) {
                char n[32];
#ifdef __linux__
                snprintf(n, sizeof n, "%04d", i);
#else
                snprintf(n, sizeof n, "f%07d", i);
#endif
                pth(p, d, n); touch(p);
            }
            int fd = open(d, O_RDONLY | O_DIRECTORY);
            int returned = 0, calls = 0;
            for (;;) {
                long n = rawgetdents(fd, big, bufs[bi], NULL);
                if (n <= 0) break;
                calls++;
                int c = parse(big, n, recs, 20000);
                for (int i = 0; i < c; i++) {
                    if (!strcmp(recs[i].name, ".") || !strcmp(recs[i].name, "..")) continue;
                    returned++;
                    pth(p, d, recs[i].name); unlink(p);
                }
            }
            close(fd);
            list(d, out, sizeof out);
            int left = 0; for (char *c = out; *c; c++) if (*c == ' ') left++;
            printf("DRAIN buf=%zu rep=%d N=%d calls=%d returned=%d left=%d\n", bufs[bi], rep, N, calls, returned, left);
        }
    }

    // COOKIE: what SEEK_CUR reports after a partial read, over repeated opens of two directories.
    {
        char d1[4096], d2[4096];
        snprintf(d1, sizeof d1, "%s/c1", root); snprintf(d2, sizeof d2, "%s/c2", root);
        mkdir(d1, 0755); mkdir(d2, 0755);
        for (int i = 0; i < 10; i++) {
            char n[32]; snprintf(n, sizeof n, "cookie%04d", i);
            pth(p, d1, n); touch(p); pth(p, d2, n); touch(p);
        }
        for (int i = 0; i < 12; i++) {
            const char *d = (i % 3 == 2) ? d2 : d1;
            int fd = open(d, O_RDONLY | O_DIRECTORY);
            long long before = lseek(fd, 0, SEEK_CUR);
            rawgetdents(fd, big, ONEBUF, NULL);
            long long a1 = lseek(fd, 0, SEEK_CUR);
            rawgetdents(fd, big, ONEBUF, NULL);
            long long a2 = lseek(fd, 0, SEEK_CUR);
            printf("COOKIE i=%d dir=%s before=%lld after1=0x%llx after2=0x%llx\n", i, d == d1 ? "c1" : "c2", before, a1, a2);
            close(fd);
        }
        // Darwin: after an empty (EOF-by-size) answer, does the long entry survive?
        char dl[4096]; snprintf(dl, sizeof dl, "%s/lng", root); mkdir(dl, 0755);
        char nm[256]; memset(nm, 'L', 255); nm[255] = 0; pth(p, dl, nm); touch(p);
        int fd = open(dl, O_RDONLY | O_DIRECTORY);
        long r1 = rawgetdents(fd, big, 64, NULL);
        long long c1 = lseek(fd, 0, SEEK_CUR);
        errno = 0;
        long r2 = rawgetdents(fd, big, 64, NULL);
        int e2 = errno;
        long long c2 = lseek(fd, 0, SEEK_CUR);
        long r3 = rawgetdents(fd, big, 4096, NULL);
        int k3 = r3 > 0 ? parse(big, r3, recs, 20000) : 0;
        printf("TOOSMALL r1=%ld cur1=0x%llx r2=%ld errno=%d cur2=0x%llx r3=%ld k3=%d first3=%.4s\n", r1, c1, r2, r2 < 0 ? e2 : 0, c2, r3, k3, k3 ? recs[0].name : "-");
        close(fd);
    }
    rmrf(root);
    return 0;
}
