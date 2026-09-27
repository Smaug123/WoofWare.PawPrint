// What a parent's waitpid and waitid see for each way a child can end:
// exit(n) over a sweep of n, _exit(n), return from main, and death by every
// signal number with its default disposition (RLIMIT_CORE 0, and unlimited for
// a few). Stop signals are reported via WUNTRACED and then the child is killed.
//
// TSV: how, arg, raw_status_hex, decoded, waitid_code, waitid_status
#define _GNU_SOURCE
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static const char *code_name(int c) {
    switch (c) {
    case CLD_EXITED: return "CLD_EXITED";
    case CLD_KILLED: return "CLD_KILLED";
    case CLD_DUMPED: return "CLD_DUMPED";
    case CLD_STOPPED: return "CLD_STOPPED";
    case CLD_TRAPPED: return "CLD_TRAPPED";
    case CLD_CONTINUED: return "CLD_CONTINUED";
    default: return "?";
    }
}

static void decode(int st, char *buf, size_t n) {
    if (WIFEXITED(st)) snprintf(buf, n, "exited(%d)", WEXITSTATUS(st));
    else if (WIFSIGNALED(st)) snprintf(buf, n, "signaled(%d%s)", WTERMSIG(st), WCOREDUMP(st) ? ",core" : "");
    else if (WIFSTOPPED(st)) snprintf(buf, n, "stopped(%d)", WSTOPSIG(st));
    else snprintf(buf, n, "other");
}

// Runs `child` in a fork, twice: once reaped by waitid(WNOWAIT) then waitpid,
// so both views of one death are recorded.
static void observe(const char *how, long arg, void (*child)(long)) {
    pid_t pid = fork();
    if (pid < 0) { perror("fork"); exit(2); }
    if (pid == 0) { alarm(5); child(arg); _exit(200); }
    siginfo_t si;
    memset(&si, 0, sizeof si);
    int r = waitid(P_PID, (id_t)pid, &si, WEXITED | WSTOPPED | WNOWAIT);
    int wcode = r == 0 ? si.si_code : -errno;
    int wstatus = r == 0 ? si.si_status : 0;
    int st = 0;
    waitpid(pid, &st, WUNTRACED);
    char d[64];
    decode(st, d, sizeof d);
    if (WIFSTOPPED(st)) {
        kill(pid, SIGKILL);
        int st2; waitpid(pid, &st2, 0);
    }
    printf("%s\t%ld\t0x%x\t%s\t%s\t%d\n", how, arg, (unsigned)st, d, code_name(wcode), wstatus);
    fflush(stdout);
}

static void do_exit(long n) { exit((int)n); }
static void do__exit(long n) { _exit((int)n); }

static void die_by(long sig, int core) {
    struct rlimit rl = { core ? RLIM_INFINITY : 0, core ? RLIM_INFINITY : 0 };
    setrlimit(RLIMIT_CORE, &rl);
    signal((int)sig, SIG_DFL);
    sigset_t s; sigemptyset(&s); sigaddset(&s, (int)sig);
    sigprocmask(SIG_UNBLOCK, &s, NULL);
    kill(getpid(), (int)sig);
    // Survived: the default is to ignore (or continue).
    _exit(201);
}
static void do_signal(long sig) { die_by(sig, 0); }
static void do_signal_core(long sig) { die_by(sig, 1); }

int main(void) {
    alarm(120);
    printf("how\targ\traw_status\tdecoded\twaitid_code\twaitid_status\n");
    long ns[] = { 0, 1, 7, 127, 128, 255, 256, 257, 263, 511, 65535, 65536 + 7, -1, -256, INT_MAX, INT_MIN };
    for (size_t i = 0; i < sizeof ns / sizeof ns[0]; i++) observe("exit", ns[i], do_exit);
    for (size_t i = 0; i < sizeof ns / sizeof ns[0]; i++) observe("_exit", ns[i], do__exit);
#ifdef __linux__
    int maxsig = SIGRTMAX;
#else
    int maxsig = NSIG - 1;
#endif
    for (int s = 1; s <= maxsig; s++) {
#ifdef __linux__
        if (s == 32 || s == 33) continue; // glibc-reserved
#endif
        observe("signal", s, do_signal);
    }
    int cored[] = { SIGQUIT, SIGABRT, SIGSEGV };
    for (size_t i = 0; i < 3; i++) observe("signal_rlimit_core_inf", cored[i], do_signal_core);
    return 0;
}
