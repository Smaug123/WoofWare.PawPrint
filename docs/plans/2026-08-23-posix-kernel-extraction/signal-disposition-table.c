// Measures what a signal's disposition does to instances of it that are
// pending, and what generating one signal does to another that is pending.
//
// Part "tr" (transitions). For every signal sigaction(2) accepts (the 29
// standard ones, plus Linux's SIGRTMIN..SIGRTMAX), generated process-directed
// (kill(getpid())) and thread-directed (pthread_kill(self)), under every
// disposition at generation (DFL, IGN, a handler H) and every disposition it
// is then changed to (DFL, IGN, H, a different handler H2), all while the
// signal is blocked: is it pending straight after generation, and after the
// change? Then a counting handler is installed and the signal unblocked: how
// many times did it arrive?
//
// Part "fl" (flush). For every ordered pair of distinct signals from SIGTSTP,
// SIGTTIN, SIGTTOU, SIGCONT and SIGUSR1 (the control), under every pair of
// dispositions at generation (DFL, IGN, H), each generated process- or
// thread-directed, both blocked: which of the two are pending after the second
// is generated? SIGSTOP is not in the sweep: it cannot be blocked, so it
// stops the child, and continuing it again means generating SIGCONT.
//
// Part "fu" (flush by an unblocked signal). A stop signal or SIGCONT with a
// handler, blocked and pending; then the opposite kind generated
// process-directed and *not* blocked, under IGN or a handler (and DFL for
// SIGCONT, which only continues a process that is not stopped): is the first
// still pending? A default stop signal is left out, because unblocked it
// would stop the child.
//
// Every trial runs in a fresh forked child, so no disposition, mask or
// pending signal crosses trials; each child has a 10 s alarm, the whole run a
// 300 s one, and every loop has a fixed bound. No signal leaves the child.
//
// Darwin: nix develop -c clang -Wall -pthread -o sdt signal-disposition-table.c && ./sdt
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq && apt-get install -y -qq gcc libc6-dev && gcc -Wall -pthread -o /tmp/p /probe/signal-disposition-table.c && /tmp/p'
//
// Measured 2026-09-26 on Darwin 25.6.0 (arm64, uid 501) and Linux 6.18.5
// (aarch64, root in the container, glibc 2.41). The rows are transcribed in
// WoofWare.PosixKernel.Test/TestSignalDispositions.fs.
#define _GNU_SOURCE
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __APPLE__
#define FLAVOUR "darwin"
#else
#define FLAVOUR "linux"
#endif

static volatile sig_atomic_t hits;
static void h1(int s) { (void)s; hits++; }
static void h2(int s) { (void)s; hits++; }

enum disp { DFL, IGN, H, H2 };
static const char *dispname[] = { "DFL", "IGN", "H", "H2" };

static void set(int s, enum disp d)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = d == DFL ? SIG_DFL : d == IGN ? SIG_IGN : d == H ? h1 : h2;
    if (sigaction(s, &sa, NULL) != 0) { perror("sigaction"); _exit(99); }
}

static int pending(int s) { sigset_t p; sigpending(&p); return sigismember(&p, s); }

static void generate(int s, int thread)
{
    if (thread) pthread_kill(pthread_self(), s);
    else kill(getpid(), s);
}

static const char *shape(int thread) { return thread ? "thread" : "proc"; }

// Runs `body` in a fresh child and prints what it wrote to the pipe,
// prefixed by `label`, or how the child died if it did not exit.
static void in_child(const char *label, void (*body)(int fd, const int *args), const int *args)
{
    int p[2];
    if (pipe(p) != 0) { perror("pipe"); _exit(99); }
    pid_t pid = fork();
    if (pid < 0) { perror("fork"); _exit(99); }
    if (pid == 0) {
        alarm(10);
        close(p[0]);
        body(p[1], args);
        _exit(0);
    }
    close(p[1]);
    char buf[256] = { 0 };
    ssize_t n = 0, r;
    while (n < (ssize_t)sizeof buf - 1 && (r = read(p[0], buf + n, sizeof buf - 1 - (size_t)n)) > 0) n += r;
    close(p[0]);
    int st;
    waitpid(pid, &st, 0);
    if (WIFEXITED(st) && WEXITSTATUS(st) == 0) printf("%s %s\n", label, buf);
    else if (WIFSIGNALED(st)) printf("%s CHILD-KILLED-BY %d\n", label, WTERMSIG(st));
    else printf("%s CHILD-STATUS 0x%x\n", label, st);
}

// args: signal, thread, from, to
static void transition(int fd, const int *a)
{
    int s = a[0];
    sigset_t set1;
    sigemptyset(&set1);
    sigaddset(&set1, s);
    sigprocmask(SIG_BLOCK, &set1, NULL);
    set(s, (enum disp)a[2]);
    generate(s, a[1]);
    int atGen = pending(s);
    set(s, (enum disp)a[3]);
    int after = pending(s);
    set(s, H);
    hits = 0;
    sigprocmask(SIG_UNBLOCK, &set1, NULL);
    char out[128];
    int o = snprintf(out, sizeof out, "pending_at_gen=%d after=%d delivered=%d", atGen, after, (int)hits);
    write(fd, out, (size_t)o);
}

// args: first, firstThread, firstDisp, second, secondThread, secondDisp
static void flush(int fd, const int *a)
{
    sigset_t both;
    sigemptyset(&both);
    sigaddset(&both, a[0]);
    sigaddset(&both, a[3]);
    sigprocmask(SIG_BLOCK, &both, NULL);
    set(a[0], (enum disp)a[2]);
    set(a[3], (enum disp)a[5]);
    generate(a[0], a[1]);
    int firstAtGen = pending(a[0]);
    generate(a[3], a[4]);
    char out[128];
    int o = snprintf(out, sizeof out, "first_at_gen=%d first=%d second=%d", firstAtGen, pending(a[0]), pending(a[3]));
    write(fd, out, (size_t)o);
}

// args: first, second, secondDisp
static void flush_unblocked(int fd, const int *a)
{
    sigset_t first;
    sigemptyset(&first);
    sigaddset(&first, a[0]);
    sigprocmask(SIG_BLOCK, &first, NULL);
    set(a[0], H);
    set(a[1], (enum disp)a[2]);
    generate(a[0], 0);
    int firstAtGen = pending(a[0]);
    hits = 0;
    generate(a[1], 0);
    char out[128];
    int o = snprintf(out, sizeof out, "first_at_gen=%d first=%d second_delivered=%d", firstAtGen, pending(a[0]), (int)hits);
    write(fd, out, (size_t)o);
}

static const char *name(int s)
{
    static char buf[16];
    switch (s) {
    case SIGTSTP: return "TSTP";
    case SIGTTIN: return "TTIN";
    case SIGTTOU: return "TTOU";
    case SIGCONT: return "CONT";
    case SIGUSR1: return "USR1";
    }
    snprintf(buf, sizeof buf, "%d", s);
    return buf;
}

int main(void)
{
    alarm(300);
    setvbuf(stdout, NULL, _IOLBF, 0);
    printf("# flavour %s\n", FLAVOUR);

    int sigs[64];
    int n = 0;
    for (int s = 1; s <= 31; s++)
        if (s != SIGKILL && s != SIGSTOP) sigs[n++] = s;
#ifndef __APPLE__
    for (int s = SIGRTMIN; s <= SIGRTMAX; s++) sigs[n++] = s;
#endif

    for (int i = 0; i < n; i++)
        for (int thread = 0; thread < 2; thread++)
            for (int from = DFL; from <= H; from++)
                for (int to = DFL; to <= H2; to++) {
                    int args[] = { sigs[i], thread, from, to };
                    char label[64];
                    snprintf(label, sizeof label, "tr %2d %-6s %-3s->%-3s", sigs[i], shape(thread), dispname[from], dispname[to]);
                    in_child(label, transition, args);
                }

    int fs[] = { SIGTSTP, SIGTTIN, SIGTTOU, SIGCONT, SIGUSR1 };
    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++) {
            if (i == j) continue;
            for (int fd = DFL; fd <= H; fd++)
                for (int sd = DFL; sd <= H; sd++)
                    for (int ft = 0; ft < 2; ft++)
                        for (int st = 0; st < 2; st++) {
                            int args[] = { fs[i], ft, fd, fs[j], st, sd };
                            char label[96];
                            snprintf(label, sizeof label, "fl %-4s %-6s %-3s then %-4s %-6s %-3s", name(fs[i]), shape(ft), dispname[fd], name(fs[j]), shape(st), dispname[sd]);
                            in_child(label, flush, args);
                        }
        }

    int stops[] = { SIGTSTP, SIGTTIN, SIGTTOU };
    for (int i = 0; i < 3; i++)
        for (int reverse = 0; reverse < 2; reverse++)
            for (int sd = DFL; sd <= H; sd++) {
                int first = reverse ? SIGCONT : stops[i];
                int second = reverse ? stops[i] : SIGCONT;
                if (sd == DFL && second != SIGCONT) continue;
                int args[] = { first, second, sd };
                char label[96];
                snprintf(label, sizeof label, "fu %-4s H   then %-4s unblocked %-3s", name(first), name(second), dispname[sd]);
                in_child(label, flush_unblocked, args);
            }
    return 0;
}
