// Directory-read probe: the record layout getdents64 (Linux) and
// __getdirentries64 (Darwin) write, how many records fit a buffer and what a
// buffer too small answers, the position on the open file description (dup
// sharing, rewind, seeking to cookies and to arbitrary offsets), the error
// answers, and what libc's opendir/readdir/telldir do over the syscall.
//
// Usage: directory-layout <base-dir> (a fresh subdirectory is made and removed)
// Darwin: nix develop -c clang -Wall -o directory-layout directory-layout.c && ./directory-layout "$TMPDIR"
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -o /tmp/p /probe/directory-layout.c && /tmp/p /dev/shm'
//
// Measured 2026-09-26 on Darwin 27.0.0 (arm64, APFS) and Linux 6.18.5 (arm64,
// tmpfs on /dev/shm; also the container's ext4 root as a contrast). The facts
// are in docs/divergences.md and the emulated-posix-kernel skill's
// flavour-divergence table.
#include "directory-common.h"

static char dir[4096];
static unsigned char big[1 << 16];
static rec recs[4096];

static void p(const char *path) { snprintf(dir, sizeof dir, "%s", path); }

static int opendirfd(void) {
    int fd = open(dir, O_RDONLY | O_DIRECTORY);
    if (fd < 0) { perror("open dir"); exit(2); }
    return fd;
}

static void dumprec(const char *tag, rec *r) {
    printf("%s at=%zu ino=%llu off=%lld reclen=%u namlen=%d type=%u pad_nonzero=%d name=%s\n", tag, r->at,
           (unsigned long long)r->ino, (long long)r->off, r->reclen, r->namlen, r->type, r->padNonZero, r->name);
}

static void listnames(int fd, size_t bufsize, char *out, size_t outsz) {
    out[0] = 0;
    for (int guard = 0; guard < 10000; guard++) {
        long n = rawgetdents(fd, big, bufsize, NULL);
        if (n < 0) { snprintf(out + strlen(out), outsz - strlen(out), "[err=%d]", errno); return; }
        if (n == 0) return;
        int k = parse(big, n, recs, 4096);
        for (int i = 0; i < k; i++) snprintf(out + strlen(out), outsz - strlen(out), "%s ", recs[i].name);
    }
}

int main(int argc, char **argv) {
    alarm(60);
    if (argc < 2) return 1;
    char root[4096];
    snprintf(root, sizeof root, "%s/dirprobe-layout", argv[1]);
    rmrf(root);
    if (mkdir(root, 0755) != 0) { perror("mkdir root"); return 2; }
    printf("FLAVOUR %s fs=%s\n", FLAVOUR, fsname(root));
    char path[4096];
    snprintf(path, sizeof path, "%s/d", root);
    mkdir(path, 0755);
    p(path);

    // Entries of every kind the library models plus fifo and socket, and names 1..12 bytes long.
    char f[4096];
    snprintf(f, sizeof f, "%s/a", dir); touch(f);
    snprintf(f, sizeof f, "%s/bb", dir); mkdir(f, 0755);
    snprintf(f, sizeof f, "%s/ccc", dir); symlink("a", f);
    snprintf(f, sizeof f, "%s/dddd", dir); mkfifo(f, 0644);
    {
        if (chdir(dir) != 0) perror("chdir");
        int s = socket(AF_UNIX, SOCK_STREAM, 0);
        struct sockaddr_un sa; memset(&sa, 0, sizeof sa); sa.sun_family = AF_UNIX;
        snprintf(sa.sun_path, sizeof sa.sun_path, "%s", "eeeee");
        if (bind(s, (struct sockaddr *)&sa, sizeof sa) != 0) perror("bind");
        close(s);
    }
    { char a[4096]; snprintf(a, sizeof a, "%s/a", dir); snprintf(f, sizeof f, "%s/hardlink", dir); if (link(a, f) != 0) perror("link"); }
    for (int L = 1; L <= 12; L++) {
        char nm[32]; memset(nm, 'n', L); nm[L] = 0; nm[0] = 'm' + 0; // "mnnn.."
        snprintf(f, sizeof f, "%s/%s", dir, nm); touch(f);
    }

    // LAYOUT
    {
        int fd = opendirfd();
        long long base;
        long n = rawgetdents(fd, big, sizeof big, &base);
        printf("LAYOUT first_call ret=%ld base=%lld\n", n, base);
        int k = parse(big, n, recs, 4096);
        for (int i = 0; i < k; i++) {
            dumprec("LAYOUT rec", &recs[i]);
            struct stat st; char q[4096];
            if (strcmp(recs[i].name, ".") == 0) snprintf(q, sizeof q, "%s", dir);
            else if (strcmp(recs[i].name, "..") == 0) snprintf(q, sizeof q, "%s", root);
            else snprintf(q, sizeof q, "%s/%s", dir, recs[i].name);
            if (lstat(q, &st) == 0)
                printf("LAYOUT ino_check name=%s d_ino=%llu st_ino=%llu equal=%d\n", recs[i].name,
                       (unsigned long long)recs[i].ino, (unsigned long long)st.st_ino, recs[i].ino == (uint64_t)st.st_ino);
        }
        long long cur = lseek(fd, 0, SEEK_CUR);
        printf("LAYOUT after_first_call seek_cur=%lld\n", cur);
        n = rawgetdents(fd, big, sizeof big, &base);
        printf("LAYOUT second_call ret=%ld errno=%d base=%lld seek_cur=%lld\n", n, n < 0 ? errno : 0, base, (long long)lseek(fd, 0, SEEK_CUR));
        n = rawgetdents(fd, big, sizeof big, &base);
        printf("LAYOUT third_call ret=%ld errno=%d base=%lld\n", n, n < 0 ? errno : 0, base);
        close(fd);
    }

    // BUFSWEEP: what a single call answers for each buffer size.
    {
        size_t sizes[400]; int ns = 0;
        for (size_t s = 0; s <= 200; s++) sizes[ns++] = s;
        size_t extra[] = {255, 256, 511, 512, 1023, 1024, 1047, 1048, 1049, 1055, 1056, 2047, 2048, 4095, 4096, 8191, 8192};
        for (size_t i = 0; i < sizeof extra / sizeof extra[0]; i++) sizes[ns++] = extra[i];
        for (int i = 0; i < ns; i++) {
            int fd = opendirfd();
            errno = 0;
            long long base;
            long n = rawgetdents(fd, big, sizes[i], &base);
            int e = errno;
            char names[8192] = "";
            if (n > 0) {
                int k = parse(big, n, recs, 4096);
                for (int j = 0; j < k; j++) snprintf(names + strlen(names), sizeof names - strlen(names), "%s ", recs[j].name);
            }
            printf("BUFSWEEP size=%zu ret=%ld errno=%d seek_cur=%lld names=%s\n", sizes[i], n, n < 0 ? e : 0,
                   (long long)lseek(fd, 0, SEEK_CUR), names);
            close(fd);
        }
    }

    // BUFSWEEP2: a directory holding one 255-byte name; the smallest buffer that holds it, after the dots.
    {
        char d2[4096]; snprintf(d2, sizeof d2, "%s/long", root); mkdir(d2, 0755);
        char nm[256]; memset(nm, 'L', 255); nm[255] = 0;
        char q[4096]; snprintf(q, sizeof q, "%s/%s", d2, nm); touch(q);
        for (size_t s = 0; s <= 1100; s++) {
            int fd = open(d2, O_RDONLY | O_DIRECTORY);
            // Skip the dots with a big read first? No: read with this size repeatedly and report the first answer that is not the dots.
            char out[4096] = "";
            int calls = 0;
            for (;;) {
                errno = 0;
                long n = rawgetdents(fd, big, s, NULL);
                calls++;
                if (n < 0) { snprintf(out + strlen(out), sizeof out - strlen(out), "err=%d ", errno); break; }
                if (n == 0) { snprintf(out + strlen(out), sizeof out - strlen(out), "eof "); break; }
                int k = parse(big, n, recs, 4096);
                for (int j = 0; j < k; j++)
                    snprintf(out + strlen(out), sizeof out - strlen(out), "%s(%u) ", strlen(recs[j].name) > 3 ? "LONG" : recs[j].name, recs[j].reclen);
                if (calls > 5) break;
            }
            printf("BUFSWEEP_LONG size=%zu %s\n", s, out);
            close(fd);
        }
    }

    // CURSOR
    {
        // a) dup shares the cursor; b) a second open does not.
        int fd1 = opendirfd();
        size_t small = 0;
        // smallest size that returns >0 for a fresh descriptor
        for (size_t s = 1; s < 2000; s++) {
            int t = opendirfd();
            long n = rawgetdents(t, big, s, NULL);
            close(t);
            if (n > 0) { small = s; break; }
        }
        printf("CURSOR smallest_nonempty=%zu\n", small);
        long n = rawgetdents(fd1, big, small, NULL);
        int k = parse(big, n, recs, 4096);
        printf("CURSOR fd1_first names=%s count=%d off_last=%lld seek_cur_fd1=%lld\n", recs[0].name, k, (long long)recs[k - 1].off,
               (long long)lseek(fd1, 0, SEEK_CUR));
        int fd2 = dup(fd1);
        printf("CURSOR dup seek_cur_fd2=%lld\n", (long long)lseek(fd2, 0, SEEK_CUR));
        char out[8192];
        listnames(fd2, sizeof big, out, sizeof out);
        printf("CURSOR dup_continues names=%s\n", out);
        listnames(fd1, sizeof big, out, sizeof out);
        printf("CURSOR fd1_after_dup_drained names=[%s]\n", out);
        int fd3 = opendirfd();
        listnames(fd3, sizeof big, out, sizeof out);
        printf("CURSOR second_open names=%s\n", out);
        // c) rewind via SEEK_SET 0.
        long long r = lseek(fd1, 0, SEEK_SET);
        listnames(fd1, sizeof big, out, sizeof out);
        printf("CURSOR rewind lseek=%lld names=%s\n", r, out);
        close(fd1); close(fd2); close(fd3);

        // d) every cookie: seek to d_off[i] and read; the first name should be entry i+1.
        int fd = opendirfd();
        n = rawgetdents(fd, big, sizeof big, NULL);
        int total = parse(big, n, recs, 4096);
        rec all[64]; memcpy(all, recs, sizeof(rec) * total);
        for (int i = 0; i < total; i++) {
            errno = 0;
            long long s = lseek(fd, all[i].off, SEEK_SET);
            int e = errno;
            long long cur = lseek(fd, 0, SEEK_CUR);
            long m = rawgetdents(fd, big, sizeof big, NULL);
            int kk = m > 0 ? parse(big, m, recs, 4096) : 0;
            printf("CURSOR cookie i=%d name=%s off=%lld lseek=%lld errno=%d seek_cur=%lld next=%s expected=%s count=%d\n", i, all[i].name,
                   (long long)all[i].off, s, s < 0 ? e : 0, cur, kk ? recs[0].name : "(eof)", i + 1 < total ? all[i + 1].name : "(eof)", kk);
        }
        // e) SEEK_CUR arithmetic after a partial read.
        lseek(fd, 0, SEEK_SET);
        n = rawgetdents(fd, big, small, NULL);
        k = parse(big, n, recs, 4096);
        long long c0 = lseek(fd, 0, SEEK_CUR);
        errno = 0;
        long long c1 = lseek(fd, 1, SEEK_CUR);
        int e1 = errno;
        printf("CURSOR seek_cur after_partial last_off=%lld cur0=%lld cur_plus1=%lld errno=%d\n", (long long)recs[k - 1].off, c0, c1, c1 < 0 ? e1 : 0);
        listnames(fd, sizeof big, out, sizeof out);
        printf("CURSOR after_cur_plus1 names=%s\n", out);
        // f) arbitrary values.
        long long vals[] = {1, 2, 3, 4, 5, 7, 8, 16, 100, 4096, 0x7fffffffLL, 0x80000000LL, 0xffffffffLL, 0x100000000LL, 0x100000001LL,
                            1LL << 40, 1LL << 62, 0x7fffffffffffffffLL, -1, -2};
        for (size_t i = 0; i < sizeof vals / sizeof vals[0]; i++) {
            errno = 0;
            long long s = lseek(fd, vals[i], SEEK_SET);
            int e = errno;
            errno = 0;
            listnames(fd, sizeof big, out, sizeof out);
            printf("CURSOR arbitrary v=%lld lseek=%lld errno=%d names=%s\n", vals[i], s, s < 0 ? e : 0, out);
        }
        for (int wh = 2; wh <= 4; wh++) {
            errno = 0;
            long long s = lseek(fd, 0, wh);
            printf("CURSOR whence=%d off=0 lseek=%lld errno=%d\n", wh, s, s < 0 ? errno : 0);
        }
        close(fd);
    }

    // ERRORS
    {
        errno = 0;
        long n = rawgetdents(987, big, sizeof big, NULL);
        printf("ERRORS badfd ret=%ld errno=%d\n", n, errno);
        char q[4096]; snprintf(q, sizeof q, "%s/a", dir);
        int ff = open(q, O_RDONLY);
        errno = 0; n = rawgetdents(ff, big, sizeof big, NULL);
        printf("ERRORS regular_file ret=%ld errno=%d\n", n, errno);
        close(ff);
        // Before the pipe, which would otherwise reuse ff's number.
        errno = 0; n = rawgetdents(ff, big, 0, NULL);
        printf("ERRORS closed_fd_size0 ret=%ld errno=%d\n", n, errno);
        int pf[2]; pipe(pf);
        errno = 0; n = rawgetdents(pf[0], big, sizeof big, NULL);
        printf("ERRORS pipe ret=%ld errno=%d\n", n, errno);
        ff = open(q, O_RDONLY);
        errno = 0; n = rawgetdents(ff, big, 0, NULL);
        printf("ERRORS regular_file_size0 ret=%ld errno=%d\n", n, errno);
        close(ff);
        int fd = opendirfd();
        errno = 0; n = rawgetdents(fd, NULL, 4096, NULL);
        printf("ERRORS null_buffer ret=%ld errno=%d seek_cur=%lld\n", n, errno, (long long)lseek(fd, 0, SEEK_CUR));
        errno = 0; n = rawgetdents(fd, (void *)8, 4096, NULL);
        printf("ERRORS wild_buffer ret=%ld errno=%d\n", n, errno);
        close(fd);
        // a removed directory, before and after a partial read
        char rd[4096]; snprintf(rd, sizeof rd, "%s/gone", root); mkdir(rd, 0755);
        fd = open(rd, O_RDONLY | O_DIRECTORY);
        rmdir(rd);
        errno = 0; n = rawgetdents(fd, big, sizeof big, NULL);
        printf("ERRORS removed_before_read ret=%ld errno=%d\n", n, n < 0 ? errno : 0);
        close(fd);
        mkdir(rd, 0755);
        fd = open(rd, O_RDONLY | O_DIRECTORY);
        n = rawgetdents(fd, big, 64, NULL);
        printf("ERRORS removed_after_partial first ret=%ld\n", n);
        rmdir(rd);
        errno = 0; n = rawgetdents(fd, big, sizeof big, NULL);
        int kk = n > 0 ? parse(big, n, recs, 4096) : 0;
        printf("ERRORS removed_after_partial second ret=%ld errno=%d count=%d first=%s\n", n, n < 0 ? errno : 0, kk, kk ? recs[0].name : "-");
        long long s = lseek(fd, 0, SEEK_SET);
        errno = 0; n = rawgetdents(fd, big, sizeof big, NULL);
        printf("ERRORS removed_after_rewind lseek=%lld ret=%ld errno=%d\n", s, n, n < 0 ? errno : 0);
        close(fd);
        // O_WRONLY on a directory, and O_PATH/O_SEARCH-ish
        errno = 0;
        int w = open(dir, O_WRONLY);
        printf("ERRORS open_dir_wronly ret=%d errno=%d\n", w, errno);
#ifdef O_PATH
        int op = open(dir, O_PATH);
        errno = 0; n = rawgetdents(op, big, sizeof big, NULL);
        printf("ERRORS o_path ret=%ld errno=%d\n", n, errno);
        close(op);
#endif
        // Reading with read(2) on a directory fd
        fd = opendirfd();
        errno = 0; n = read(fd, big, sizeof big);
        printf("ERRORS read_on_dir ret=%ld errno=%d\n", n, errno);
        close(fd);
    }

    // LIBC: opendir's descriptor flags, and telldir/seekdir against the syscall cookies.
    {
        DIR *d = opendir(dir);
        int fd = dirfd(d);
        printf("LIBC dirfd=%d fd_flags=0x%x fl_flags=0x%x O_NONBLOCK=0x%x O_DIRECTORY=0x%x O_CLOEXEC=0x%x FD_CLOEXEC=%d\n", fd, fcntl(fd, F_GETFD),
               fcntl(fd, F_GETFL), O_NONBLOCK, O_DIRECTORY, O_CLOEXEC, FD_CLOEXEC);
        printf("LIBC before_readdir telldir=%ld seek_cur=%lld\n", telldir(d), (long long)lseek(fd, 0, SEEK_CUR));
        long tells[64]; char names[64][300]; int i = 0;
        struct dirent *e;
        while ((e = readdir(d)) != NULL && i < 64) {
#ifdef __linux__
            long long o = e->d_off;
#else
            long long o = e->d_seekoff;
#endif
            tells[i] = telldir(d);
            snprintf(names[i], 300, "%s", e->d_name);
            printf("LIBC readdir i=%d name=%s d_off=%lld telldir_after=%ld seek_cur=%lld\n", i, e->d_name, o, tells[i], (long long)lseek(fd, 0, SEEK_CUR));
            i++;
        }
        for (int j = 0; j < i; j++) {
            seekdir(d, tells[j]);
            e = readdir(d);
            printf("LIBC seekdir j=%d tell=%ld next=%s expected=%s\n", j, tells[j], e ? e->d_name : "(null)", j + 1 < i ? names[j + 1] : "(null)");
        }
        rewinddir(d);
        e = readdir(d);
        printf("LIBC rewinddir first=%s seek_cur=%lld\n", e ? e->d_name : "(null)", (long long)lseek(fd, 0, SEEK_CUR));
        closedir(d);
        errno = 0;
        printf("LIBC after_closedir fcntl=%d errno=%d\n", fcntl(fd, F_GETFD), errno);
        // readdir on an rmdir'd directory through libc
        char rd[4096]; snprintf(rd, sizeof rd, "%s/gone2", root); mkdir(rd, 0755);
        d = opendir(rd);
        rmdir(rd);
        errno = 0;
        e = readdir(d);
        printf("LIBC readdir_removed result=%s errno=%d\n", e ? e->d_name : "(null)", errno);
        closedir(d);
    }
    rmrf(root);
    return 0;
}
