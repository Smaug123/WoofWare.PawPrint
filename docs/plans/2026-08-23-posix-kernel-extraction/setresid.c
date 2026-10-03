// Measures how a process changes its own credentials: setresuid(2),
// setresgid(2) and setgroups(2), and reading them back with getresuid(2) and
// getresgid(2).
//
// Darwin: nix develop -c clang -Wall -o setresid setresid.c && ./setresid
// Linux:  container run --rm -v "$PWD":/probe gcc:14 sh -c 'gcc -Wall -Wno-stringop-overflow -o /tmp/p /probe/setresid.c && /tmp/p'
//
// On Linux run it as root. Every row forks a child, which as root takes the
// row's starting credentials, then makes the call under test and reports what
// it answered and what the process holds afterwards. Each sweep is the whole
// product of the sets it names:
//
//  - RESUID: every starting (real, effective, saved) user triple over
//    {0, 1000, 1001}, against every requested triple over
//    {-1, 0, 1000, 1001, 1002}: 27 * 125 rows. After the call the child
//    reports its triple, its filesystem uid (setfsuid(-1) answers it without
//    changing it), whether it is dumpable (PR_GET_DUMPABLE), and whether it
//    may open a root-owned 0600 file, which is what "privileged" means to
//    every permission check the kernel model makes.
//  - RESGID: every starting user triple of three shapes ((0,0,0), (0,1000,0)
//    and (1000,1000,1000)), times every starting group triple over
//    {0, 1000, 1001}, against every requested group triple over
//    {-1, 0, 1000, 1001, 1002}: 3 * 27 * 125 rows. The starting process holds
//    supplementary group 1002, so a row can show whether being in a group lets
//    a process take it.
//  - SETGROUPS: four starting user triples, against a fixed set of size and
//    buffer arguments chosen to order each pair of failures (privilege, size,
//    an unreadable buffer, a (gid_t)-1 in the list), including two lists
//    whose first two words are readable and whose third is not.
//  - GETRES: getresuid and getresgid with each of the three pointers in turn
//    NULL, reporting which of the IDs were written before the failure.
//  - FSOWNER: who owns a file a process creates after changing its effective
//    IDs.
//
// On Darwin, as uid 501, only setgroups exists of the three: libSystem
// declares no setresuid, setresgid, getresuid or getresgid. Its rows are the
// same size and buffer arguments as Linux's, from the process's own
// credentials, since uid 501 cannot take any other.
//
// Measured on Linux 6.18.5 (aarch64, root in the container, gcc:14) and Darwin
// 27.0 (arm64, uid 501) on 2026-10-03. The output is beside this file as
// setresid.linux-6.18.5-aarch64.txt and setresid.darwin-27.0-uid501.txt, which
// WoofWare.PosixKernel.Test/TestCredentialChange.fs replays row by row.
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/fsuid.h>
#include <sys/prctl.h>
#endif

static const char *en(int e) {
    switch (e) {
    case 0: return "OK";
    case EINVAL: return "EINVAL";
    case EPERM: return "EPERM";
    case EFAULT: return "EFAULT";
    case EACCES: return "EACCES";
    case ENOMEM: return "ENOMEM";
    default: {
        static char b[32];
        snprintf(b, sizeof b, "errno%d", e);
        return b;
    }
    }
}

#ifdef __linux__
static void id_text(char *out, size_t n, long id) {
    if (id == -1) snprintf(out, n, "-1");
    else snprintf(out, n, "%ld", id);
}

static void triple_text(char *out, size_t n, long a, long b, long c) {
    char x[16], y[16], z[16];
    id_text(x, sizeof x, a);
    id_text(y, sizeof y, b);
    id_text(z, sizeof z, c);
    snprintf(out, n, "%s,%s,%s", x, y, z);
}
#endif

// The process's groups as getgroups(2) reports them, or, for a list longer
// than a few, its length and its first and last entries.
static void groups_text(char *out, size_t n) {
    int count = getgroups(0, NULL);
    gid_t *g = count > 0 ? calloc((size_t)count, sizeof(gid_t)) : NULL;
    if (count < 0 || (count > 0 && getgroups(count, g) != count)) {
        snprintf(out, n, "getgroups:%s", en(errno));
        free(g);
        return;
    }
    if (count > 8) {
        snprintf(out, n, "[%d groups: %u..%u]", count, (unsigned)g[0], (unsigned)g[count - 1]);
        free(g);
        return;
    }
    size_t used = 0;
    out[0] = '\0';
    used += (size_t)snprintf(out + used, n - used, "[");
    for (int i = 0; i < count && used < n; i++)
        used += (size_t)snprintf(out + used, n - used, "%s%u", i == 0 ? "" : ",", (unsigned)g[i]);
    if (used < n) snprintf(out + used, n - used, "]");
    free(g);
}

static void wait_child(pid_t pid) {
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        printf("CHILD-FAILED\tstatus=%d\n", status);
        fflush(stdout);
    }
}

#ifdef __linux__
static const char *secret = "/tmp/setresid-probe-secret";

static const char *privileged(void) {
    int fd = open(secret, O_RDONLY);
    if (fd < 0) return errno == EACCES ? "no" : en(errno);
    close(fd);
    return "yes";
}

static const long user_starts[] = {0, 1000, 1001};
static const long requests[] = {-1, 0, 1000, 1001, 1002};

static void resuid_row(long r, long e, long s, long tr, long te, long ts) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        if (setresuid((uid_t)r, (uid_t)e, (uid_t)s) != 0) {
            printf("SETUP-FAILED\tsetresuid\t%s\n", en(errno));
            _exit(1);
        }
        // The setup's own change of IDs may have cleared the flag; set it so
        // that the call under test is what decides it.
        prctl(PR_SET_DUMPABLE, 1);
        int err = setresuid((uid_t)tr, (uid_t)te, (uid_t)ts) == 0 ? 0 : errno;
        uid_t ar, ae, as;
        getresuid(&ar, &ae, &as);
        int fsuid = setfsuid((uid_t)-1);
        int dumpable = prctl(PR_GET_DUMPABLE);
        char from[64], to[64], after[64];
        triple_text(from, sizeof from, r, e, s);
        triple_text(to, sizeof to, tr, te, ts);
        triple_text(after, sizeof after, (long)ar, (long)ae, (long)as);
        printf("RESUID\t%s\t%s\t%s\t%s\tfsuid=%d\tdumpable=1->%d\tprivileged=%s\n", from, to, en(err), after, fsuid, dumpable,
               privileged());
        fflush(stdout);
        _exit(0);
    }
    wait_child(pid);
}

static void resgid_row(long ur, long ue, long us, long r, long e, long s, long tr, long te, long ts) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        gid_t supplementary[1] = {1002};
        if (setgroups(1, supplementary) != 0 || setresgid((gid_t)r, (gid_t)e, (gid_t)s) != 0 ||
            setresuid((uid_t)ur, (uid_t)ue, (uid_t)us) != 0) {
            printf("SETUP-FAILED\tresgid\t%s\n", en(errno));
            _exit(1);
        }
        prctl(PR_SET_DUMPABLE, 1);
        int before = prctl(PR_GET_DUMPABLE);
        int err = setresgid((gid_t)tr, (gid_t)te, (gid_t)ts) == 0 ? 0 : errno;
        gid_t ar, ae, as;
        getresgid(&ar, &ae, &as);
        int fsgid = setfsgid((gid_t)-1);
        int dumpable = prctl(PR_GET_DUMPABLE);
        char users[64], from[64], to[64], after[64];
        triple_text(users, sizeof users, ur, ue, us);
        triple_text(from, sizeof from, r, e, s);
        triple_text(to, sizeof to, tr, te, ts);
        triple_text(after, sizeof after, (long)ar, (long)ae, (long)as);
        printf("RESGID\t%s\t%s\t%s\t%s\t%s\tfsgid=%d\tdumpable=%d->%d\n", users, from, to, en(err), after, fsgid, before,
               dumpable);
        fflush(stdout);
        _exit(0);
    }
    wait_child(pid);
}
#endif

// A setgroups argument: how many groups the call is told there are, and what
// the buffer holds (NULL when `words` is NULL).
struct groups_case {
    const char *label;
    int size;
    const gid_t *words;
};

static gid_t one_1000[] = {1000};
static gid_t three[] = {30, 10, 20};
static gid_t with_minus_one[] = {1000, (gid_t)-1};
static gid_t minus_one_first[] = {(gid_t)-1, 1000};
static gid_t duplicates[] = {7, 7, 3, 7};
static gid_t *many;

// Two words at the very end of a readable page whose successor is
// PROT_NONE: a list of groups starting here is readable for exactly its
// first two words.
static gid_t *make_straddle(gid_t first, gid_t second) {
    long page = sysconf(_SC_PAGESIZE);
    char *base = mmap(NULL, (size_t)page * 2, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == MAP_FAILED) return NULL;
    mprotect(base + page, (size_t)page, PROT_NONE);
    gid_t *words = (gid_t *)(base + page) - 2;
    words[0] = first;
    words[1] = second;
    return words;
}

static void setgroups_row(const char *who, const struct groups_case *c) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        char before[1024], after[1024];
        groups_text(before, sizeof before);
#ifdef __linux__
        int dumpable_before = prctl(PR_GET_DUMPABLE);
#endif
        int err = setgroups(c->size, c->words) == 0 ? 0 : errno;
        groups_text(after, sizeof after);
#ifdef __linux__
        printf("SETGROUPS\t%s\t%s\t%s\tbefore=%s\tafter=%s\tegid=%u\tdumpable=%d->%d\n", who, c->label, en(err), before, after,
               (unsigned)getegid(), dumpable_before, prctl(PR_GET_DUMPABLE));
#else
        printf("SETGROUPS\t%s\t%s\t%s\tbefore=%s\tafter=%s\tegid=%u\n", who, c->label, en(err), before, after,
               (unsigned)getegid());
#endif
        fflush(stdout);
        _exit(0);
    }
    wait_child(pid);
}

static void setgroups_rows(const char *who, long limit) {
    // `many` holds limit + 1 distinct groups, so a prefix of it is a list of
    // any size up to that.
    struct groups_case cases[] = {
        {"size=0 buffer=NULL", 0, NULL},
        {"size=0 buffer=[]", 0, one_1000},
        {"size=1 buffer=[1000]", 1, one_1000},
        {"size=3 buffer=[30,10,20]", 3, three},
        {"size=4 buffer=[7,7,3,7]", 4, duplicates},
        {"size=2 buffer=[1000,-1]", 2, with_minus_one},
        {"size=2 buffer=[-1,1000]", 2, minus_one_first},
        {"size=1 buffer=NULL", 1, NULL},
        {"size=2 buffer=NULL", 2, NULL},
        {"size=3 buffer=[1000,1001|fault]", 3, NULL},
        {"size=3 buffer=[1000,-1|fault]", 3, NULL},
        {"size=-1 buffer=NULL", -1, NULL},
        {"size=-1 buffer=[1000]", -1, one_1000},
        {"size=INT_MIN buffer=NULL", INT_MIN, NULL},
        {"size=INT_MAX buffer=NULL", INT_MAX, NULL},
        {"size=limit buffer=distinct", (int)limit, many},
        {"size=limit buffer=NULL", (int)limit, NULL},
        {"size=limit+1 buffer=distinct", (int)limit + 1, many},
        {"size=limit+1 buffer=NULL", (int)limit + 1, NULL},
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        if (strcmp(cases[i].label, "size=3 buffer=[1000,1001|fault]") == 0) cases[i].words = make_straddle(1000, 1001);
        if (strcmp(cases[i].label, "size=3 buffer=[1000,-1|fault]") == 0) cases[i].words = make_straddle(1000, (gid_t)-1);
        setgroups_row(who, &cases[i]);
    }
}

#ifdef __linux__
static void as_users_then_setgroups(long r, long e, long s) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(60);
        gid_t supplementary[1] = {2000};
        if (setgroups(1, supplementary) != 0 || setresgid(1000, 1000, 1000) != 0 ||
            setresuid((uid_t)r, (uid_t)e, (uid_t)s) != 0) {
            printf("SETUP-FAILED\tsetgroups\t%s\n", en(errno));
            _exit(1);
        }
        prctl(PR_SET_DUMPABLE, 1);
        char who[64];
        triple_text(who, sizeof who, r, e, s);
        setgroups_rows(who, sysconf(_SC_NGROUPS_MAX));
        _exit(0);
    }
    wait_child(pid);
}

static void getres_row(int is_user, int null_at) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        if (setgroups(0, NULL) != 0 || setresgid(10, 11, 12) != 0 || setresuid(20, 21, 22) != 0) {
            printf("SETUP-FAILED\tgetres\t%s\n", en(errno));
            _exit(1);
        }
        uint32_t ids[3] = {0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF};
        uint32_t *p[3] = {&ids[0], &ids[1], &ids[2]};
        if (null_at >= 0) p[null_at] = NULL;
        int err = is_user ? (getresuid(p[0], p[1], p[2]) == 0 ? 0 : errno)
                          : (getresgid(p[0], p[1], p[2]) == 0 ? 0 : errno);
        char written[3][16];
        for (int i = 0; i < 3; i++) {
            if (p[i] == NULL) snprintf(written[i], sizeof written[i], "null");
            else if (ids[i] == 0xDEADBEEF) snprintf(written[i], sizeof written[i], "unwritten");
            else snprintf(written[i], sizeof written[i], "%u", ids[i]);
        }
        printf("GETRES\t%s\tnull=%s\t%s\t%s,%s,%s\n", is_user ? "uid" : "gid",
               null_at < 0 ? "none" : null_at == 0 ? "real" : null_at == 1 ? "effective" : "saved", en(err), written[0],
               written[1], written[2]);
        fflush(stdout);
        _exit(0);
    }
    wait_child(pid);
}

static void fs_owner_row(void) {
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        alarm(10);
        const char *path = "/tmp/setresid-probe-owned";
        unlink(path);
        if (setresgid(1000, 1001, 1002) != 0 || setresuid(1003, 1004, 0) != 0) {
            printf("SETUP-FAILED\tfsowner\t%s\n", en(errno));
            _exit(1);
        }
        int fd = open(path, O_CREAT | O_WRONLY | O_EXCL, 0600);
        if (fd < 0) {
            printf("FSOWNER\tcreate\t%s\n", en(errno));
            _exit(0);
        }
        close(fd);
        struct stat st;
        stat(path, &st);
        printf("FSOWNER\tcredentials uid=1003,1004,0 gid=1000,1001,1002\towner=%u:%u\n", (unsigned)st.st_uid,
               (unsigned)st.st_gid);
        setresuid(-1, 0, -1);
        unlink(path);
        fflush(stdout);
        _exit(0);
    }
    wait_child(pid);
}
#endif

int main(void) {
    alarm(1800);
    long limit = sysconf(_SC_NGROUPS_MAX);
    many = calloc((size_t)limit + 1, sizeof(gid_t));
    for (long i = 0; i <= limit; i++) many[i] = (gid_t)(3000 + i);
    printf("RUN\tuid=%u euid=%u gid=%u egid=%u NGROUPS_MAX=%ld\n", (unsigned)getuid(), (unsigned)geteuid(),
           (unsigned)getgid(), (unsigned)getegid(), limit);
#ifdef __linux__
    FILE *f = fopen("/proc/sys/fs/suid_dumpable", "r");
    int suid_dumpable = -1;
    if (f) {
        if (fscanf(f, "%d", &suid_dumpable) != 1) suid_dumpable = -2;
        fclose(f);
    }
    printf("SUID_DUMPABLE\t%d\n", suid_dumpable);
    int fd = open(secret, O_CREAT | O_WRONLY | O_TRUNC, 0600);
    if (fd >= 0) close(fd);
    chmod(secret, 0600);

    for (int a = 0; a < 3; a++)
        for (int b = 0; b < 3; b++)
            for (int c = 0; c < 3; c++)
                for (int x = 0; x < 5; x++)
                    for (int y = 0; y < 5; y++)
                        for (int z = 0; z < 5; z++)
                            resuid_row(user_starts[a], user_starts[b], user_starts[c], requests[x], requests[y],
                                       requests[z]);

    const long user_shapes[3][3] = {{0, 0, 0}, {0, 1000, 0}, {1000, 1000, 1000}};
    for (int u = 0; u < 3; u++)
        for (int a = 0; a < 3; a++)
            for (int b = 0; b < 3; b++)
                for (int c = 0; c < 3; c++)
                    for (int x = 0; x < 5; x++)
                        for (int y = 0; y < 5; y++)
                            for (int z = 0; z < 5; z++)
                                resgid_row(user_shapes[u][0], user_shapes[u][1], user_shapes[u][2], user_starts[a],
                                           user_starts[b], user_starts[c], requests[x], requests[y], requests[z]);

    as_users_then_setgroups(0, 0, 0);
    as_users_then_setgroups(0, 1000, 0);
    as_users_then_setgroups(1000, 0, 1000);
    as_users_then_setgroups(1000, 1000, 1000);

    for (int is_user = 1; is_user >= 0; is_user--)
        for (int null_at = -1; null_at < 3; null_at++) getres_row(is_user, null_at);

    fs_owner_row();
    unlink(secret);
#else
    char who[64];
    snprintf(who, sizeof who, "%u,%u,%u", (unsigned)getuid(), (unsigned)geteuid(), (unsigned)geteuid());
    setgroups_rows(who, limit);
#endif
    printf("DONE\n");
    return 0;
}
