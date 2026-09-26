// epoll_ctl(2) on this kernel: which errno each (op, epfd, target, event
// pointer, events) input answers, and so the order of the checks; which bits
// epoll_wait then reports for every readiness state WoofWare.PosixKernel
// models; which registrations a data-ready wake queues; and the nesting and
// EPOLLWAKEUP facts the library refuses rather than models.
//
// Sections:
//   LADDER  every (epfd kind x target kind x op x event pointer x registered?)
//           cell, each tried with events = 0, each of the 32 single bits, and
//           two EPOLLEXCLUSIVE combinations. Every call runs against freshly
//           made descriptors, so no row sees another's state. A cell prints
//           its commonest answer and the exceptions, by events value.
//   REPORT  for each modelled readiness state: ADD with a request mask, one
//           epoll_wait(timeout 0), DEL. Masks swept: 0, each single bit,
//           every pair of bits, and 20000 pseudo-random 32-bit masks (fixed
//           seed). Checks the rule reported == full & (mask|ERR|HUP), where
//           `full` is the report for the mask 0x0FFFFFFF.
//   EXCL    the EPOLLEXCLUSIVE screen: 20000 random masks for each of ADD
//           (unregistered, registered, epoll target), MOD (registered,
//           unregistered) and op 0, against the predicted errno.
//   WAKE    which interest masks a listener's data-ready wake queues: listener
//           A registered with mask M|ET, listener B with IN|ET, connect to A,
//           connect to B, MOD A to IN|ET, wait. [A B] means the wake queued A
//           although M may not be reported; [B A] means it did not.
//   NEST    epoll instances inside epoll instances: a cycle, a chain, and
//           EPOLLEXCLUSIVE on an epoll target.
//   WAKEUP  whether EPOLLWAKEUP survives into the stored mask, read back
//           through /proc/self/fdinfo, beside this process's CapEff.
//
// Build and run, from this directory (both architectures, since
// `struct epoll_event` is packed on x86-64 only):
//   container run --rm -v "$PWD:/probe" gcc:14 sh -c
//     'gcc -Wall -O1 -o /tmp/ec /probe/epoll-ctl.c && /tmp/ec'
//   container run --rm --arch amd64 -v "$PWD:/probe" gcc:14 sh -c
//     'gcc -Wall -O1 -o /tmp/ec /probe/epoll-ctl.c && /tmp/ec'
//
// Measured 2026-09-26 on Linux 6.18.5 (the `container` VM) under both
// aarch64 and x86-64 (Rosetta) userlands; the results are summarised beside
// `UnixPoll.epollCtl` and in its tests.
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

static const char *en(int e)
{
    if (e == 0)
        return "ok";
    const char *n = strerrorname_np(e);
    return n ? n : "?";
}

// ---------------------------------------------------------------------------
// Descriptor kinds.
// ---------------------------------------------------------------------------

enum kind
{
    K_CLOSED,
    K_NEG,
    K_FILE,
    K_DIR,
    K_STDIN,
    K_STDOUT,
    K_TCP_IDLE,
    K_TCP_LISTEN,
    K_UDP,
    K_UNIX_STREAM,
    K_UNIX_DGRAM,
    K_EPOLL,
    K_SAME,
    K_DUP_OF_EPFD,
    K_COUNT
};

static const char *kind_name[K_COUNT] = {
    "closed", "-1", "file", "dir", "stdin-pipe", "stdout-pipe", "tcp-idle", "tcp-listen",
    "udp", "unix-stream", "unix-dgram", "epoll", "same-as-epfd", "dup-of-epfd",
};

static char file_path[64];

static int listener(struct sockaddr_in *out)
{
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    bind(fd, (struct sockaddr *)&a, sizeof a);
    listen(fd, 64);
    if (out)
    {
        socklen_t sl = sizeof *out;
        getsockname(fd, (struct sockaddr *)out, &sl);
    }
    return fd;
}

// Extra descriptors a made kind keeps alive (a pipe's other end), closed by
// `unmake`.
static int extra[4];
static int nextra;

static int make(enum kind k)
{
    int p[2];
    switch (k)
    {
    case K_CLOSED:
    {
        int fd = dup(0);
        close(fd);
        return fd;
    }
    case K_NEG:
        return -1;
    case K_FILE:
        return open(file_path, O_RDWR);
    case K_DIR:
        return open("/tmp", O_RDONLY | O_DIRECTORY);
    case K_STDIN:
        pipe(p);
        close(p[1]);
        return p[0];
    case K_STDOUT:
        pipe(p);
        extra[nextra++] = p[0];
        return p[1];
    case K_TCP_IDLE:
        return socket(AF_INET, SOCK_STREAM, 0);
    case K_TCP_LISTEN:
        return listener(NULL);
    case K_UDP:
        return socket(AF_INET, SOCK_DGRAM, 0);
    case K_UNIX_STREAM:
        return socket(AF_UNIX, SOCK_STREAM, 0);
    case K_UNIX_DGRAM:
        return socket(AF_UNIX, SOCK_DGRAM, 0);
    case K_EPOLL:
        return epoll_create1(0);
    default:
        fprintf(stderr, "make: kind %d is not standalone\n", k);
        exit(2);
    }
}

static void unmake(void)
{
    for (int i = 0; i < nextra; i++)
        close(extra[i]);
    nextra = 0;
}

// ---------------------------------------------------------------------------
// LADDER
// ---------------------------------------------------------------------------

#define NEV 36
static uint32_t ladder_events[NEV];

static int ladder_call(enum kind ek, enum kind tk, int op, int nullptr_, int registered, uint32_t events)
{
    int ep = make(ek);
    int t;
    int tclose = 1;
    if (tk == K_SAME)
    {
        t = ep;
        tclose = 0;
    }
    else if (tk == K_DUP_OF_EPFD)
        t = ep >= 0 ? dup(ep) : -1;
    else
        t = make(tk);

    if (registered)
    {
        struct epoll_event pre;
        memset(&pre, 0, sizeof pre);
        pre.events = EPOLLIN | EPOLLET;
        if (epoll_ctl(ep, EPOLL_CTL_ADD, t, &pre) != 0)
        {
            if (t >= 0 && tclose)
                close(t);
            if (ep >= 0)
                close(ep);
            unmake();
            return -1000; // not registrable: the cell is skipped
        }
    }

    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = events;
    ev.data.u64 = 42;
    errno = 0;
    int rc = epoll_ctl(ep, op, t, nullptr_ ? NULL : &ev);
    int e = rc == 0 ? 0 : errno;

    if (t >= 0 && tclose)
        close(t);
    if (ep >= 0)
        close(ep);
    unmake();
    return e;
}

static void ladder(void)
{
    printf("== LADDER ==\n");
    for (int b = 0; b < 32; b++)
        ladder_events[b] = 1u << b;
    ladder_events[32] = 0;
    ladder_events[33] = EPOLLEXCLUSIVE | EPOLLIN;
    ladder_events[34] = EPOLLEXCLUSIVE | EPOLLIN | EPOLLOUT | EPOLLERR | EPOLLHUP | EPOLLWAKEUP | EPOLLET;
    ladder_events[35] = EPOLLEXCLUSIVE | EPOLLIN | EPOLLRDHUP;

    enum kind epfds[] = {K_CLOSED, K_NEG, K_FILE, K_DIR, K_STDIN, K_TCP_IDLE, K_EPOLL};
    enum kind targets[] = {K_CLOSED, K_NEG, K_FILE, K_DIR, K_STDIN, K_STDOUT, K_TCP_IDLE, K_TCP_LISTEN,
                           K_UDP, K_UNIX_STREAM, K_UNIX_DGRAM, K_EPOLL, K_SAME, K_DUP_OF_EPFD};
    int ops[] = {EPOLL_CTL_ADD, EPOLL_CTL_DEL, EPOLL_CTL_MOD, 0, 4, 5, -1, INT_MAX, INT_MIN};

    for (unsigned ei = 0; ei < sizeof epfds / sizeof *epfds; ei++)
        for (unsigned ti = 0; ti < sizeof targets / sizeof *targets; ti++)
            for (unsigned oi = 0; oi < sizeof ops / sizeof *ops; oi++)
                for (int np = 0; np < 2; np++)
                    for (int reg = 0; reg < 2; reg++)
                    {
                        if (reg && epfds[ei] != K_EPOLL)
                            continue;
                        int results[NEV];
                        int skipped = 0;
                        for (int i = 0; i < NEV; i++)
                        {
                            results[i] = ladder_call(epfds[ei], targets[ti], ops[oi], np, reg, ladder_events[i]);
                            if (results[i] == -1000)
                                skipped = 1;
                        }
                        if (skipped)
                            continue;
                        // Commonest answer.
                        int best = results[0], bestCount = 0;
                        for (int i = 0; i < NEV; i++)
                        {
                            int c = 0;
                            for (int j = 0; j < NEV; j++)
                                c += results[j] == results[i];
                            if (c > bestCount)
                            {
                                best = results[i];
                                bestCount = c;
                            }
                        }
                        printf("epfd=%-11s target=%-12s op=%-11d ev=%-4s reg=%d : %s", kind_name[epfds[ei]],
                               kind_name[targets[ti]], ops[oi], np ? "NULL" : "ok", reg, en(best));
                        for (int i = 0; i < NEV; i++)
                            if (results[i] != best)
                                printf(" [0x%08x:%s]", ladder_events[i], en(results[i]));
                        printf("\n");
                    }

    // MOD and DEL of a registration made with EPOLLEXCLUSIVE.
    {
        int ep = epoll_create1(0);
        int s = socket(AF_INET, SOCK_DGRAM, 0);
        struct epoll_event ev;
        memset(&ev, 0, sizeof ev);
        ev.events = EPOLLEXCLUSIVE | EPOLLIN;
        int rc = epoll_ctl(ep, EPOLL_CTL_ADD, s, &ev);
        printf("exclusive registration: ADD EXCLUSIVE|IN -> %s\n", en(rc == 0 ? 0 : errno));
        for (int b = 0; b < 32; b++)
        {
            ev.events = 1u << b;
            rc = epoll_ctl(ep, EPOLL_CTL_MOD, s, &ev);
            if (rc != 0 || b == 0)
                printf("  MOD 0x%08x -> %s\n", ev.events, en(rc == 0 ? 0 : errno));
        }
        ev.events = 0;
        rc = epoll_ctl(ep, EPOLL_CTL_MOD, s, &ev);
        printf("  MOD 0 -> %s\n", en(rc == 0 ? 0 : errno));
        rc = epoll_ctl(ep, EPOLL_CTL_ADD, s, &ev);
        printf("  ADD 0 again -> %s\n", en(rc == 0 ? 0 : errno));
        rc = epoll_ctl(ep, EPOLL_CTL_DEL, s, &ev);
        printf("  DEL -> %s\n", en(rc == 0 ? 0 : errno));
        close(s);
        close(ep);
    }
}

// ---------------------------------------------------------------------------
// REPORT
// ---------------------------------------------------------------------------

static uint64_t rng_state = 0x243F6A8885A308D3ull;
static uint32_t rng(void)
{
    rng_state ^= rng_state << 13;
    rng_state ^= rng_state >> 7;
    rng_state ^= rng_state << 17;
    return (uint32_t)(rng_state >> 16);
}

// One ADD, one wait at timeout 0, one DEL. Returns the reported events, or
// sets *err on an ADD failure. *count receives the number of events.
static uint32_t report(int ep, int fd, uint32_t mask, int *err, int *count, uint32_t *second)
{
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = mask;
    ev.data.u64 = 7;
    *err = 0;
    *count = 0;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev) != 0)
    {
        *err = errno;
        return 0;
    }
    struct epoll_event out[4];
    int n = epoll_wait(ep, out, 4, 0);
    *count = n;
    uint32_t r = n > 0 ? out[0].events : 0;
    if (second)
    {
        int n2 = epoll_wait(ep, out, 4, 0);
        *second = n2 > 0 ? out[0].events : 0xFFFFFFFFu;
    }
    epoll_ctl(ep, EPOLL_CTL_DEL, fd, &ev);
    return r;
}

static void sweep_state(int fd, const char *label)
{
    int ep = epoll_create1(0);
    int err, n;
    uint32_t full = report(ep, fd, 0x0FFFFFFFu, &err, &n, NULL);
    printf("-- %s\n   full (mask 0x0fffffff) = 0x%08x%s\n", label, full, err ? " (ADD FAILED)" : "");
    if (err)
    {
        printf("   ADD errno %s\n", en(err));
        close(ep);
        return;
    }
    printf("   single bits (first wait, second wait):");
    for (int b = 0; b < 32; b++)
    {
        uint32_t second;
        uint32_t r = report(ep, fd, 1u << b, &err, &n, &second);
        if (err)
            printf(" b%d:%s", b, en(err));
        else if (r != 0 || second != 0xFFFFFFFFu)
            printf(" b%d:0x%x/%s", b, r, second == 0xFFFFFFFFu ? "-" : "again");
    }
    uint32_t second0;
    uint32_t r0 = report(ep, fd, 0, &err, &n, &second0);
    printf("\n   mask 0 -> 0x%x (second wait %s)\n", r0, second0 == 0xFFFFFFFFu ? "nothing" : "again");

    long tried = 0, misses = 0, addFailures = 0, countMisses = 0;
    int shown = 0;
    for (int pass = 0; pass < 2; pass++)
    {
        long limit = pass == 0 ? 32 * 32 : 20000;
        for (long i = 0; i < limit; i++)
        {
            uint32_t mask;
            if (pass == 0)
                mask = (1u << (i / 32)) | (1u << (i % 32));
            else
                mask = rng();
            uint32_t r = report(ep, fd, mask, &err, &n, NULL);
            tried++;
            if (err)
            {
                // Only EPOLLEXCLUSIVE combinations are expected to fail.
                if (!(mask & EPOLLEXCLUSIVE) && shown++ < 6)
                    printf("   unexpected ADD failure: mask 0x%08x %s\n", mask, en(err));
                addFailures++;
                continue;
            }
            uint32_t predicted = full & (mask | EPOLLERR | EPOLLHUP);
            if (predicted != r)
            {
                if (shown++ < 6)
                    printf("   RULE miss: mask 0x%08x reported 0x%08x predicted 0x%08x\n", mask, r, predicted);
                misses++;
            }
            if ((r != 0) != (n == 1))
                countMisses++;
        }
    }
    printf("   %ld masks: rule misses %ld, count misses %ld, ADD failures %ld (all EXCLUSIVE)\n", tried, misses,
           countMisses, addFailures);
    close(ep);
}

static void set_nonblock(int fd) { fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK); }

static void report_section(void)
{
    printf("== REPORT ==\n");
    struct sockaddr_in la;
    int l = listener(&la);

    int idle = socket(AF_INET, SOCK_STREAM, 0);
    sweep_state(idle, "TCP idle");
    int idle6 = socket(AF_INET6, SOCK_STREAM, 0);
    sweep_state(idle6, "TCP6 idle");
    sweep_state(l, "TCP listening, queue empty");

    int c = socket(AF_INET, SOCK_STREAM, 0);
    connect(c, (struct sockaddr *)&la, sizeof la);
    usleep(50000);
    sweep_state(l, "TCP listening, queue nonempty");
    sweep_state(c, "TCP client, connected (blocking), peer queued");
    int s = accept(l, NULL, NULL);
    sweep_state(s, "TCP accepted end, peer alive");
    sweep_state(c, "TCP client end, peer accepted");
    close(c);
    usleep(50000);
    sweep_state(s, "TCP accepted end, peer closed");
    close(s);

    int nb = socket(AF_INET, SOCK_STREAM, 0);
    set_nonblock(nb);
    connect(nb, (struct sockaddr *)&la, sizeof la);
    usleep(50000);
    sweep_state(nb, "TCP non-blocking connect completed, unreported");
    int nbs = accept(l, NULL, NULL);
    close(nbs);
    close(nb);

    struct sockaddr_in da;
    int dead = listener(&da);
    close(dead);
    int r = socket(AF_INET, SOCK_STREAM, 0);
    set_nonblock(r);
    connect(r, (struct sockaddr *)&da, sizeof da);
    usleep(50000);
    sweep_state(r, "TCP refused, pending delivery");
    connect(r, (struct sockaddr *)&da, sizeof da);
    sweep_state(r, "TCP refused, delivered");
    close(r);

    int u = socket(AF_INET, SOCK_DGRAM, 0);
    sweep_state(u, "UDP idle");
    connect(u, (struct sockaddr *)&la, sizeof la);
    sweep_state(u, "UDP with a peer");
    close(u);
    int u6 = socket(AF_INET6, SOCK_DGRAM, 0);
    sweep_state(u6, "UDP6 idle");

    int us = socket(AF_UNIX, SOCK_STREAM, 0);
    sweep_state(us, "UNIX stream idle");
    int ud = socket(AF_UNIX, SOCK_DGRAM, 0);
    sweep_state(ud, "UNIX dgram idle");

    int in[2];
    pipe(in);
    close(in[1]);
    sweep_state(in[0], "stdin shape: pipe read end, writer closed");
    int out[2];
    pipe(out);
    sweep_state(out[1], "stdout shape: pipe write end, reader alive");
    close(l);
}

// ---------------------------------------------------------------------------
// WAKE
// ---------------------------------------------------------------------------

static void connect_to(struct sockaddr_in *a)
{
    int c = socket(AF_INET, SOCK_STREAM, 0);
    connect(c, (struct sockaddr *)a, sizeof *a);
    // Leaked on purpose: closing would change the listener's queue.
}

static void wake_row(uint32_t mask)
{
    struct sockaddr_in aa, ab;
    int la = listener(&aa), lb = listener(&ab);
    int ep = epoll_create1(0);
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = mask | EPOLLET;
    ev.data.u64 = 'A';
    int rc = epoll_ctl(ep, EPOLL_CTL_ADD, la, &ev);
    if (rc != 0)
    {
        printf("   mask 0x%08x: ADD %s\n", mask, en(errno));
        close(la);
        close(lb);
        close(ep);
        return;
    }
    ev.events = EPOLLIN | EPOLLET;
    ev.data.u64 = 'B';
    epoll_ctl(ep, EPOLL_CTL_ADD, lb, &ev);
    struct epoll_event out[8];
    int drained = epoll_wait(ep, out, 8, 0);

    connect_to(&aa);
    usleep(20000);
    connect_to(&ab);
    usleep(20000);

    ev.events = EPOLLIN | EPOLLET;
    ev.data.u64 = 'A';
    epoll_ctl(ep, EPOLL_CTL_MOD, la, &ev);
    int n = epoll_wait(ep, out, 8, 0);
    printf("   mask 0x%08x: (pre-drain %d) ->", mask, drained);
    for (int i = 0; i < n; i++)
        printf(" %c", (char)out[i].data.u64);
    printf("\n");
    close(la);
    close(lb);
    close(ep);
}

static void wake_section(void)
{
    printf("== WAKE (A registered mask|ET, B IN|ET; connect A, connect B, MOD A to IN|ET) ==\n");
    wake_row(0);
    for (int b = 0; b < 28; b++)
        wake_row(1u << b);
    wake_row(EPOLLONESHOT);
    wake_row(EPOLLWAKEUP);
}

// ---------------------------------------------------------------------------
// EXCL: the EPOLLEXCLUSIVE screen over random masks, per op and table state.
// ---------------------------------------------------------------------------

#define OK_BITS (EPOLLIN | EPOLLOUT | EPOLLERR | EPOLLHUP | EPOLLWAKEUP | EPOLLET | EPOLLEXCLUSIVE)

static int excl_call(int op, int registered, int epollTarget, uint32_t mask)
{
    int ep = epoll_create1(0);
    int t = epollTarget ? epoll_create1(0) : socket(AF_INET, SOCK_DGRAM, 0);
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    if (registered)
    {
        ev.events = EPOLLIN | EPOLLET;
        epoll_ctl(ep, EPOLL_CTL_ADD, t, &ev);
    }
    ev.events = mask;
    int rc = epoll_ctl(ep, op, t, &ev);
    int e = rc == 0 ? 0 : errno;
    close(t);
    close(ep);
    return e;
}

static void excl_section(void)
{
    printf("== EXCL (20000 random masks per row; predicted from EXCLUSIVE and the OK bits 0x%08x) ==\n", OK_BITS);
    struct
    {
        const char *label;
        int op, registered, epollTarget;
    } rows[] = {
        {"ADD, unregistered socket", EPOLL_CTL_ADD, 0, 0},
        {"ADD, registered socket", EPOLL_CTL_ADD, 1, 0},
        {"MOD, registered socket", EPOLL_CTL_MOD, 1, 0},
        {"MOD, unregistered socket", EPOLL_CTL_MOD, 0, 0},
        {"op 0, registered socket", 0, 1, 0},
        {"ADD, unregistered epoll target", EPOLL_CTL_ADD, 0, 1},
    };
    for (unsigned r = 0; r < sizeof rows / sizeof *rows; r++)
    {
        long misses = 0, exclusive = 0;
        for (int i = 0; i < 20000; i++)
        {
            uint32_t mask = rng();
            int excl = (mask & EPOLLEXCLUSIVE) != 0;
            exclusive += excl;
            int predicted;
            if (rows[r].op == EPOLL_CTL_ADD)
            {
                if (excl && (rows[r].epollTarget || (mask & ~OK_BITS)))
                    predicted = EINVAL;
                else
                    predicted = rows[r].registered ? EEXIST : 0;
            }
            else if (rows[r].op == EPOLL_CTL_MOD)
                predicted = excl ? EINVAL : rows[r].registered ? 0 : ENOENT;
            else
                predicted = EINVAL;
            int got = excl_call(rows[r].op, rows[r].registered, rows[r].epollTarget, mask);
            if (got != predicted && misses++ < 4)
                printf("   miss: %s mask 0x%08x got %s predicted %s\n", rows[r].label, mask, en(got), en(predicted));
        }
        printf("-- %s: %ld misses (%ld masks carried EXCLUSIVE)\n", rows[r].label, misses, exclusive);
    }
}

// ---------------------------------------------------------------------------
// NEST
// ---------------------------------------------------------------------------

static void nest_section(void)
{
    printf("== NEST ==\n");
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN;
    int e1 = epoll_create1(0), e2 = epoll_create1(0);
    int rc = epoll_ctl(e1, EPOLL_CTL_ADD, e2, &ev);
    printf("e2 into e1 -> %s\n", en(rc == 0 ? 0 : errno));
    rc = epoll_ctl(e1, EPOLL_CTL_ADD, e2, &ev);
    printf("e2 into e1 again -> %s\n", en(rc == 0 ? 0 : errno));
    rc = epoll_ctl(e2, EPOLL_CTL_ADD, e1, &ev);
    printf("e1 into e2 (cycle) -> %s\n", en(rc == 0 ? 0 : errno));
    rc = epoll_ctl(e1, EPOLL_CTL_MOD, e2, &ev);
    printf("MOD e2 in e1 -> %s\n", en(rc == 0 ? 0 : errno));
    ev.events = EPOLLIN | EPOLLEXCLUSIVE;
    int e3 = epoll_create1(0);
    rc = epoll_ctl(e3, EPOLL_CTL_ADD, e1, &ev);
    printf("e1 into e3 with EXCLUSIVE|IN -> %s\n", en(rc == 0 ? 0 : errno));
    ev.events = EPOLLIN;
    rc = epoll_ctl(e1, EPOLL_CTL_DEL, e2, &ev);
    printf("DEL e2 from e1 -> %s\n", en(rc == 0 ? 0 : errno));

    int chain[10];
    for (int i = 0; i < 10; i++)
        chain[i] = epoll_create1(0);
    for (int i = 0; i + 1 < 10; i++)
    {
        rc = epoll_ctl(chain[i + 1], EPOLL_CTL_ADD, chain[i], &ev);
        printf("chain: e%d into e%d -> %s\n", i, i + 1, en(rc == 0 ? 0 : errno));
    }
}

// ---------------------------------------------------------------------------
// WAKEUP
// ---------------------------------------------------------------------------

static void wakeup_section(void)
{
    printf("== WAKEUP ==\n");
    FILE *st = fopen("/proc/self/status", "r");
    char line[256];
    while (st && fgets(line, sizeof line, st))
        if (strncmp(line, "CapEff:", 7) == 0 || strncmp(line, "Uid:", 4) == 0)
            printf("%s", line);
    if (st)
        fclose(st);
    int ep = epoll_create1(0);
    int s = socket(AF_INET, SOCK_DGRAM, 0);
    struct epoll_event ev;
    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN | EPOLLWAKEUP;
    int rc = epoll_ctl(ep, EPOLL_CTL_ADD, s, &ev);
    printf("ADD IN|WAKEUP -> %s\n", en(rc == 0 ? 0 : errno));
    char path[64];
    snprintf(path, sizeof path, "/proc/self/fdinfo/%d", ep);
    FILE *f = fopen(path, "r");
    while (f && fgets(line, sizeof line, f))
        if (strncmp(line, "tfd:", 4) == 0)
            printf("fdinfo: %s", line);
    if (f)
        fclose(f);
}

int main(void)
{
    alarm(600);
    snprintf(file_path, sizeof file_path, "/tmp/epollctlXXXXXX");
    int tf = mkstemp(file_path);
    close(tf);
    printf("EPOLLIN=0x%x PRI=0x%x OUT=0x%x ERR=0x%x HUP=0x%x RDNORM=0x%x RDBAND=0x%x WRNORM=0x%x "
           "WRBAND=0x%x MSG=0x%x RDHUP=0x%x EXCLUSIVE=0x%x WAKEUP=0x%x ONESHOT=0x%x ET=0x%x sizeof(epoll_event)=%zu\n",
           EPOLLIN, EPOLLPRI, EPOLLOUT, EPOLLERR, EPOLLHUP, EPOLLRDNORM, EPOLLRDBAND, EPOLLWRNORM,
           EPOLLWRBAND, EPOLLMSG, EPOLLRDHUP, EPOLLEXCLUSIVE, EPOLLWAKEUP, EPOLLONESHOT, (unsigned)EPOLLET,
           sizeof(struct epoll_event));
    ladder();
    report_section();
    wake_section();
    excl_section();
    nest_section();
    wakeup_section();
    unlink(file_path);
    return 0;
}
