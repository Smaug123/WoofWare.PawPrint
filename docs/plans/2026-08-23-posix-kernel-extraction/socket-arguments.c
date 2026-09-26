// What socket(2) answers for every domain, type and protocol in a bounded
// sweep, and so the order in which a kernel checks the three.
//
// Unlike the socketMatrix sweep (which mirrors a runtime's own argument
// screens and then calls socket(2)), this passes raw platform numbers straight
// to the kernel.
//
// The sweep:
//   domain:   -2 .. 63, then 64, 128, 255, 256, 1000, 65535, 65536,
//             INT_MAX, INT_MIN
//   type:     base 0 .. 15 with no flag bits, each crossed with every domain
//             and protocol; and base 0 .. 15 with each of the modifiers below,
//             crossed with the smaller domain and protocol sets below
//   protocol: -2 .. 300, then 511, 512, 1000, 65535, 65536, INT_MAX, INT_MIN
//   modifiers: Linux's SOCK_NONBLOCK (0x800), SOCK_CLOEXEC (0x80000), both,
//             and the single bits 0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400,
//             0x1000, 0x4000, 0x40000, 0x100000, 0x40000000, 0x80000000.
//             The same numbers are passed on Darwin, which defines neither
//             flag, to see what it makes of them.
//   modifier domains:   -1, 0, 1, 2, 10, 30, 46, 99, 1000
//   modifier protocols: -1, 0, 1, 6, 17, 132, 255, 256, 300
//
// Each created socket is described by what the kernel reports back about it --
// SO_TYPE, SO_PROTOCOL where the platform has it, O_NONBLOCK and FD_CLOEXEC --
// and closed. Output is run-length encoded over both the type and the protocol
// axes. Each sweep opens with the two lists it walks, in order:
//
//   @types TAB <type words, hex, comma-separated>
//   @protocols TAB <protocols, comma-separated>
//
// and then has one line per domain and run of consecutive types whose answers
// were identical for every protocol:
//
//   <domain> TAB <first type> TAB <last type> TAB <run>;<run>;...
//
// where a run is `<first protocol>..<last protocol>=<answer>`, covering
// consecutive entries of the @protocols list (so its bounds are list entries,
// not a numeric range), and <answer> is `E <errno name>(<number>)` or
// `OK type=<n> protocol=<n|-> nonblock=<0|1> cloexec=<0|1>`.
//
// With `--drop-to 1000` (as root) the probe first drops to uid/gid 1000 with
// no supplementary groups, which also drops every capability.
//
// Darwin: nix develop -c clang -Wall -o /tmp/socket-arguments socket-arguments.c && /tmp/socket-arguments
// Linux:  container run --rm [--arch amd64] -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null && apt-get install -y -qq gcc libc6-dev >/dev/null && gcc -Wall -o /tmp/p /probe/socket-arguments.c && /tmp/p --drop-to 1000'
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

static const char *errnoName(int e) {
    switch (e) {
    case EPERM: return "EPERM";
    case EACCES: return "EACCES";
    case EINVAL: return "EINVAL";
    case EAFNOSUPPORT: return "EAFNOSUPPORT";
    case EPROTONOSUPPORT: return "EPROTONOSUPPORT";
    case EPROTOTYPE: return "EPROTOTYPE";
    case ESOCKTNOSUPPORT: return "ESOCKTNOSUPPORT";
    case EOPNOTSUPP: return "EOPNOTSUPP";
    case ENOENT: return "ENOENT";
    case ENOBUFS: return "ENOBUFS";
    case ENOMEM: return "ENOMEM";
    case EMFILE: return "EMFILE";
    case ENFILE: return "ENFILE";
    case EPFNOSUPPORT: return "EPFNOSUPPORT";
    case ENODEV: return "ENODEV";
    case ENXIO: return "ENXIO";
    case EBUSY: return "EBUSY";
#if ENOTSUP != EOPNOTSUPP
    case ENOTSUP: return "ENOTSUP";
#endif
    default: return "E?";
    }
}

static void answer(int domain, int type, int protocol, char *out, size_t outSize) {
    errno = 0;
    int fd = socket(domain, type, protocol);
    if (fd < 0) {
        int e = errno;
        snprintf(out, outSize, "E %s(%d)", errnoName(e), e);
        return;
    }

    int soType = -1;
    socklen_t len = sizeof soType;
    if (getsockopt(fd, SOL_SOCKET, SO_TYPE, &soType, &len) < 0) soType = -1000 - errno;

    char protocolText[32];
#ifdef SO_PROTOCOL
    int soProtocol = -1;
    len = sizeof soProtocol;
    if (getsockopt(fd, SOL_SOCKET, SO_PROTOCOL, &soProtocol, &len) < 0) soProtocol = -1000 - errno;
    snprintf(protocolText, sizeof protocolText, "%d", soProtocol);
#else
    snprintf(protocolText, sizeof protocolText, "-");
#endif

    int fl = fcntl(fd, F_GETFL, 0);
    int fdFlags = fcntl(fd, F_GETFD, 0);
    snprintf(
        out, outSize, "OK type=%d protocol=%s nonblock=%d cloexec=%d",
        soType, protocolText,
        fl < 0 ? -1 : ((fl & O_NONBLOCK) ? 1 : 0),
        fdFlags < 0 ? -1 : ((fdFlags & FD_CLOEXEC) ? 1 : 0));
    close(fd);
}

// The protocol runs for one (domain, type): consecutive entries of
// `protocols` that answered alike, as `<first>..<last>=<answer>` joined by `;`.
static void protocolRuns(int domain, int type, const int *protocols, int protocolCount, char *out, size_t outSize) {
    char previous[160] = "";
    int runStart = 0;
    size_t used = 0;
    out[0] = '\0';
    for (int i = 0; i <= protocolCount; i++) {
        char current[160] = "";
        if (i < protocolCount) answer(domain, type, protocols[i], current, sizeof current);
        if (i > 0 && (i == protocolCount || strcmp(current, previous) != 0)) {
            int n = snprintf(
                out + used, outSize - used, "%s%d..%d=%s",
                used == 0 ? "" : ";", protocols[runStart], protocols[i - 1], previous);
            if (n < 0 || (size_t)n >= outSize - used) { fprintf(stderr, "run buffer overflow\n"); exit(2); }
            used += (size_t)n;
        }
        if (i == 0 || strcmp(current, previous) != 0) {
            runStart = i;
            memcpy(previous, current, sizeof previous);
        }
    }
}

static void printList(const char *name, const int *values, int count, int hex) {
    printf("@%s\t", name);
    for (int i = 0; i < count; i++) printf(hex ? "%s0x%x" : "%s%d", i == 0 ? "" : ",", values[i]);
    printf("\n");
}

// One line per run of consecutive types (in `types` order) whose protocol runs
// are identical: `<domain> TAB <first type> TAB <last type> TAB <runs>`.
static void sweep(const int *domains, int domainCount, const int *types, int typeCount, const int *protocols, int protocolCount) {
    static char previous[1 << 16];
    static char current[1 << 16];
    printList("types", types, typeCount, 1);
    printList("protocols", protocols, protocolCount, 0);
    for (int di = 0; di < domainCount; di++) {
        int runStart = 0;
        for (int ti = 0; ti <= typeCount; ti++) {
            if (ti < typeCount) protocolRuns(domains[di], types[ti], protocols, protocolCount, current, sizeof current);
            if (ti > 0 && (ti == typeCount || strcmp(current, previous) != 0)) {
                printf("%d\t0x%x\t0x%x\t%s\n", domains[di], (unsigned)types[runStart], (unsigned)types[ti - 1], previous);
            }
            if (ti == 0 || (ti < typeCount && strcmp(current, previous) != 0)) {
                runStart = ti;
                memcpy(previous, current, sizeof previous);
            }
        }
        fflush(stdout);
    }
}

int main(int argc, char **argv) {
    alarm(3000);

    if (argc == 3 && strcmp(argv[1], "--drop-to") == 0) {
        int id = atoi(argv[2]);
        if (setgroups(0, NULL) != 0 || setgid(id) != 0 || setuid(id) != 0) {
            perror("dropping privileges");
            return 1;
        }
    }
    fprintf(stderr, "euid=%d egid=%d\n", (int)geteuid(), (int)getegid());

    int domains[128];
    int domainCount = 0;
    for (int d = -2; d <= 63; d++) domains[domainCount++] = d;
    const int extraDomains[] = { 64, 128, 255, 256, 1000, 65535, 65536, INT_MAX, INT_MIN };
    for (size_t i = 0; i < sizeof extraDomains / sizeof extraDomains[0]; i++) domains[domainCount++] = extraDomains[i];

    int protocols[400];
    int protocolCount = 0;
    for (int p = -2; p <= 300; p++) protocols[protocolCount++] = p;
    const int extraProtocols[] = { 511, 512, 1000, 65535, 65536, INT_MAX, INT_MIN };
    for (size_t i = 0; i < sizeof extraProtocols / sizeof extraProtocols[0]; i++) protocols[protocolCount++] = extraProtocols[i];

    int baseTypes[16];
    for (int t = 0; t <= 15; t++) baseTypes[t] = t;
    sweep(domains, domainCount, baseTypes, 16, protocols, protocolCount);

    const unsigned modifiers[] = {
        0x800, 0x80000, 0x80800,
        0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400, 0x1000, 0x4000, 0x40000, 0x100000, 0x40000000, 0x80000000u,
    };
    const int modifierDomains[] = { -1, 0, 1, 2, 10, 30, 46, 99, 1000 };
    const int modifierProtocols[] = { -1, 0, 1, 6, 17, 132, 255, 256, 300 };

    int modifiedTypes[16 * 16];
    int modifiedTypeCount = 0;
    for (size_t mi = 0; mi < sizeof modifiers / sizeof modifiers[0]; mi++)
        for (int base = 0; base <= 15; base++)
            modifiedTypes[modifiedTypeCount++] = (int)(modifiers[mi] | (unsigned)base);

    sweep(
        modifierDomains, sizeof modifierDomains / sizeof modifierDomains[0],
        modifiedTypes, modifiedTypeCount,
        modifierProtocols, sizeof modifierProtocols / sizeof modifierProtocols[0]);

    fprintf(stderr, "done\n");
    return 0;
}
