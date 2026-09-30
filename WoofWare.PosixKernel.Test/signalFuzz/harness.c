// The signal fuzzer's real-kernel side: runs op sequences on the kernel it is
// built for, one forked child per sequence, and prints what happened.
//
// Input: one sequence per line. Scripts are separated by '|': the first is run
// at top level, and the n-th after it is the body of the n-th handler
// invocation (counting every handler that starts, in the order they start).
// Ops within a script are separated by ',':
//   a<S>.<h>.<mask>.<flags>  sigaction(S): h is d (SIG_DFL), i (SIG_IGN) or
//                            c (the recording handler); mask is sa_mask as
//                            hex, bit signo-1; flags 1 = SA_NODEFER,
//                            2 = SA_RESETHAND
//   k<S>                     kill(getpid(), S)
//   r<S>                     pthread_kill(pthread_self(), S)
//
// Output: one line per sequence, "= " then space-separated events:
//   p<hex>          sigpending(2) after an op (bit signo-1)
//   e<S>.<m>.<d>    a handler starts for S, with the thread's mask <m> (hex)
//                   and S's disposition then <d> (d, i or c)
//   x               that handler returns
//   f<errno>        an op failed
//   ok | died<S> | status<raw>   how the child ended
//
// Single-threaded, and every signal is sent by the process to itself, so each
// kernel delivers deterministically: a self-directed signal is taken on the
// return from the call that sent it.
#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAX_SCRIPTS 64
#define MAX_LINE 8192

#ifdef __linux__
#define HIGHEST 64
#else
#define HIGHEST 31
#endif

static int out_fd = -1;
static char *scripts[MAX_SCRIPTS];
static int script_count = 0;
static int next_script = 1;

static void emit(const char *s) {
    size_t n = strlen(s);
    while (n > 0) {
        ssize_t w = write(out_fd, s, n);
        if (w < 0) {
            if (errno == EINTR) continue;
            _exit(99);
        }
        s += w;
        n -= (size_t)w;
    }
}

static void emit_hex(uint64_t v) {
    char buf[20];
    int i = 19;
    buf[i] = 0;
    do {
        buf[--i] = "0123456789abcdef"[v & 0xf];
        v >>= 4;
    } while (v != 0);
    emit(buf + i);
}

static void emit_dec(long v) {
    char buf[24];
    int i = 23;
    int neg = v < 0;
    unsigned long u = neg ? (unsigned long)(-v) : (unsigned long)v;
    buf[i] = 0;
    do {
        buf[--i] = (char)('0' + u % 10);
        u /= 10;
    } while (u != 0);
    if (neg) buf[--i] = '-';
    emit(buf + i);
}

// A sigset_t's bits, read and written directly rather than through
// sigismember and sigaddset: glibc's refuse its reserved 32 and 33, which the
// kernel itself accepts in a mask. Bit signo-1 is glibc's layout (an array of
// unsigned long) and Darwin's (a uint32_t) alike, little-endian.
#ifdef __linux__
#define SET_BYTES 8
#else
#define SET_BYTES 4
#endif

static uint64_t set_bits(const sigset_t *set) {
    uint64_t bits = 0;
    memcpy(&bits, set, SET_BYTES);
    return bits;
}

static void bits_set(sigset_t *set, uint64_t bits) {
    sigemptyset(set);
    memcpy(set, &bits, SET_BYTES);
}

static void run_script(char *script);

static void handler(int sig) {
    int saved = errno;
    int mine = next_script++;
    sigset_t mask;
    sigprocmask(SIG_BLOCK, NULL, &mask);
    struct sigaction now;
    sigaction(sig, NULL, &now);
    emit(" e");
    emit_dec(sig);
    emit(".");
    emit_hex(set_bits(&mask));
    emit(".");
    emit(now.sa_handler == SIG_DFL ? "d" : now.sa_handler == SIG_IGN ? "i" : "c");
    if (mine < script_count) run_script(scripts[mine]);
    emit(" x");
    errno = saved;
}

static void pending(void) {
    sigset_t set;
    sigemptyset(&set);
    sigpending(&set);
    emit(" p");
    emit_hex(set_bits(&set));
}

static void run_op(char *op) {
    int rc = 0;
    int err = 0;
    switch (op[0]) {
    case 'a': {
        char *p = op + 1;
        int sig = (int)strtol(p, &p, 10);
        p++;
        char h = *p;
        p += 2;
        uint64_t mask = strtoull(p, &p, 16);
        p++;
        int flags = (int)strtol(p, &p, 10);
        struct sigaction act;
        memset(&act, 0, sizeof act);
        act.sa_handler = h == 'd' ? SIG_DFL : h == 'i' ? SIG_IGN : handler;
        bits_set(&act.sa_mask, mask);
        act.sa_flags = ((flags & 1) ? SA_NODEFER : 0) | ((flags & 2) ? SA_RESETHAND : 0);
        rc = sigaction(sig, &act, NULL);
        err = errno;
        break;
    }
    case 'k':
        rc = kill(getpid(), atoi(op + 1));
        err = errno;
        break;
    case 'r':
        rc = pthread_kill(pthread_self(), atoi(op + 1));
        err = rc;
        break;
    default:
        emit(" bad-op");
        _exit(98);
    }
    if (rc != 0) {
        emit(" f");
        emit_dec(err);
    }
    pending();
}

static void run_script(char *script) {
    char *copy = strdup(script);
    char *save = NULL;
    for (char *op = strtok_r(copy, ",", &save); op != NULL; op = strtok_r(NULL, ",", &save)) {
        if (*op != 0) run_op(op);
    }
    free(copy);
}

static void run_sequence(char *line) {
    int fds[2];
    if (pipe(fds) != 0) {
        perror("pipe");
        exit(1);
    }
    pid_t child = fork();
    if (child == 0) {
        close(fds[0]);
        out_fd = fds[1];
        alarm(10);
        struct rlimit none = {0, 0};
        setrlimit(RLIMIT_CORE, &none);
        for (int s = 1; s <= HIGHEST; s++) {
            if (s == SIGKILL || s == SIGSTOP || s == SIGALRM) continue;
            signal(s, SIG_DFL);
        }
        sigset_t empty;
        sigemptyset(&empty);
        sigprocmask(SIG_SETMASK, &empty, NULL);
        // strsep rather than strtok, which would merge the empty bodies that
        // separate two non-empty ones and so give a body to the wrong handler.
        script_count = 0;
        char *rest = line;
        for (char *s = strsep(&rest, "|"); s != NULL && script_count < MAX_SCRIPTS; s = strsep(&rest, "|")) {
            scripts[script_count++] = s;
        }
        if (script_count > 0) run_script(scripts[0]);
        _exit(0);
    }
    close(fds[1]);
    char buf[MAX_LINE * 4];
    size_t used = 0;
    for (;;) {
        ssize_t r = read(fds[0], buf + used, sizeof buf - 1 - used);
        if (r < 0 && errno == EINTR) continue;
        if (r <= 0) break;
        used += (size_t)r;
        if (used >= sizeof buf - 1) break;
    }
    buf[used] = 0;
    close(fds[0]);
    int status = 0;
    while (waitpid(child, &status, 0) < 0 && errno == EINTR) {
    }
    printf("=%s", buf);
    if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
        printf(" ok\n");
    } else if (WIFSIGNALED(status)) {
        printf(" died%d\n", WTERMSIG(status));
    } else {
        printf(" status%d\n", status);
    }
    fflush(stdout);
}

int main(void) {
    static char line[MAX_LINE];
    while (fgets(line, sizeof line, stdin) != NULL) {
        size_t n = strlen(line);
        while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r')) line[--n] = 0;
        if (n == 0) continue;
        run_sequence(line);
    }
    return 0;
}
