// Which bits of a handler's sa_mask sigaction(2) stores, and which of them
// the thread's mask holds while the handler runs.
//
// For each sa_mask word below, in a fresh forked child: install a handler
// for USR1 with that sa_mask, query the handler back and print its sa_mask
// word, raise USR1, and print the mask inside the handler and after it
// returns. A word's bit n-1 is signo n, written straight into the sigset_t
// (Darwin's sigfillset is ~0, so it sets bit 31, signo 32, which Darwin does
// not have; glibc's sigaddset refuses 32 and 33, so those bits are written
// directly).
//
// Words: every bit of sigset_t's first word; bit 31 alone; on Linux bits 31
// and 32 (signals 32 and 33) alone. Linux installs through glibc's sigaction
// and through the raw rt_sigaction(2), which glibc does not screen.
//
// sigaction-sweep.c already found that a query hands back sa_mask without
// SIGKILL and SIGSTOP, and on Linux with 32 and 33, through both routes; this
// adds Darwin's bit 31 and the mask inside the handler.
//
// Bounded: GUARD_MAIN and GUARD_CHILD (alarm), and every loop over a fixed
// list. The only signal is raised by the thread at itself and caught.
//
// Darwin: clang -Wall -Wno-unused-function -Wno-deprecated-declarations -o /tmp/sigaction-mask-bits sigaction-mask-bits.c && /tmp/sigaction-mask-bits
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -Wno-unused-function -pthread -o /tmp/p /probe/sigaction-mask-bits.c && /tmp/p'
//
// Run on Darwin 27.0.0 (arm64, uid 501) and Linux 6.18.5 (aarch64, glibc
// 2.41, root in the container), 2026-10-08; the output is beside this file
// as sigaction-mask-bits.darwin-27.0-uid501.txt and
// sigaction-mask-bits.linux-6.18.5-aarch64.txt.
#include "signal-probe-common.h"
#include <stdarg.h>
#include <sys/syscall.h>
#include <sys/utsname.h>

#ifdef __APPLE__
typedef uint32_t raw_word;
#else
typedef uint64_t raw_word;
struct kernel_sigaction {
    void (*handler)(int);
    unsigned long flags;
    void (*restorer)(void);
    uint64_t mask;
};
#endif

static uint64_t bits_of(const sigset_t *set)
{
    raw_word w;
    memcpy(&w, set, sizeof w);
    return (uint64_t)w;
}

static void set_bits(sigset_t *set, uint64_t bits)
{
    memset(set, 0, sizeof *set);
    raw_word w = (raw_word)bits;
    memcpy(set, &w, sizeof w);
}

static uint64_t current(void)
{
    sigset_t old;
    memset(&old, 0, sizeof old);
#ifdef __APPLE__
    syscall(SYS___pthread_sigmask, SIG_BLOCK, NULL, &old);
#else
    syscall(SYS_rt_sigprocmask, SIG_BLOCK, NULL, &old, (size_t)8);
#endif
    return bits_of(&old);
}

static int g_fd;
static void out(int fd, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
static void out(int fd, const char *fmt, ...)
{
    char line[256];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(line, sizeof line, fmt, ap);
    va_end(ap);
    write(fd, line, strlen(line));
}

static void on_usr1(int s)
{
    (void)s;
    out(g_fd, " in-handler=%llx", (unsigned long long)current());
}

struct row { uint64_t word; int raw; };

static void body(int fd, struct row *row)
{
    g_fd = fd;
    sigset_t empty;
    sigemptyset(&empty);
    pthread_sigmask(SIG_SETMASK, &empty, NULL);
    if (!row->raw) {
        struct sigaction sa, back;
        memset(&sa, 0, sizeof sa);
        sa.sa_handler = on_usr1;
        set_bits(&sa.sa_mask, row->word);
        int ret = sigaction(SIGUSR1, &sa, NULL);
        memset(&back, 0, sizeof back);
        sigaction(SIGUSR1, NULL, &back);
        out(fd, " ret=%d stored=%llx", ret, (unsigned long long)bits_of(&back.sa_mask));
    } else {
#ifndef __APPLE__
        struct kernel_sigaction ka, back;
        memset(&ka, 0, sizeof ka);
        // A raw install needs glibc's restorer to return from the handler;
        // borrow the one glibc's own sigaction put in place.
        struct sigaction libc;
        memset(&libc, 0, sizeof libc);
        libc.sa_handler = on_usr1;
        sigaction(SIGUSR1, &libc, NULL);
        syscall(SYS_rt_sigaction, SIGUSR1, NULL, &back, (size_t)8);
        ka = back;
        ka.mask = row->word;
        int ret = (int)syscall(SYS_rt_sigaction, SIGUSR1, &ka, NULL, (size_t)8);
        memset(&back, 0, sizeof back);
        syscall(SYS_rt_sigaction, SIGUSR1, NULL, &back, (size_t)8);
        out(fd, " ret=%d stored=%llx", ret, (unsigned long long)back.mask);
#endif
    }
    raise(SIGUSR1);
    out(fd, " after=%llx", (unsigned long long)current());
}

int main(void)
{
    GUARD_MAIN();
    struct utsname u;
    uname(&u);
    printf("# %s %s %s, uid %d\n", u.sysname, u.release, u.machine, (int)getuid());
    printf("# flavour %s USR1 bit %llx\n", FLAVOUR, (unsigned long long)((uint64_t)1 << (SIGUSR1 - 1)));
    uint64_t words[] = {
#ifdef __APPLE__
        0xffffffffull, 0x80000000ull,
#else
        ~(uint64_t)0, 0x80000000ull, 0x180000000ull,
#endif
    };
    for (int raw = 0; raw <= 1; raw++) {
#ifdef __APPLE__
        if (raw) break;
#endif
        for (size_t i = 0; i < sizeof words / sizeof words[0]; i++) {
            struct row row = { words[i], raw };
            int fds[2];
            if (pipe(fds) != 0) die("pipe");
            fflush(stdout);
            pid_t child = fork();
            if (child < 0) die("fork");
            if (child == 0) {
                GUARD_CHILD();
                close(fds[0]);
                body(fds[1], &row);
                _exit(0);
            }
            close(fds[1]);
            char buf[1024];
            ssize_t total = 0, n;
            while (total < (ssize_t)sizeof buf - 1 && (n = read(fds[0], buf + total, sizeof buf - 1 - total)) > 0)
                total += n;
            buf[total] = 0;
            close(fds[0]);
            int status;
            waitpid(child, &status, 0);
            printf("%s sa_mask=%llx%s end=%s%d\n", raw ? "raw" : "libc", (unsigned long long)words[i], buf,
                   WIFEXITED(status) ? "exit" : "signal", WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
        }
    }
    return 0;
}
