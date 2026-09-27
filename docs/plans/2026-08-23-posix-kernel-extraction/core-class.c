// Measures which signals' default action dumps core.
//
// For every signal number, a fresh child sets the signal to SIG_DFL, unblocks
// it and sends it to itself with kill(2). How the kernel reports the core
// class differs:
//
// Linux: the child raises RLIMIT_CORE to infinity and changes into a scratch
// directory first (the container's core_pattern is a plain file name), and
// the parent reads WCOREDUMP from the wait status.
//
// Darwin: a non-root user cannot write the dump itself, because /cores is
// root:wheel 0755, so WCOREDUMP is never set. What xnu does unconditionally
// for a death by a signal whose action dumps core is raise EXC_CRASH on the
// dying task's exception port (bsd/kern/kern_exit.c, which tests the same
// SA_CORE property the dump itself is gated on). Exception ports are
// inherited across fork, so the parent installs its own receive right as its
// task's EXC_CRASH port before forking, and each child's death either sends
// it a message within a second or does not. The parent answers each message
// with KERN_SUCCESS, so the crash goes no further (no crash report is filed).
//
// Signals whose default action stops the process leave a stopped child,
// which the parent kills with SIGKILL; those rows say "stopped".
//
// Every child has a 10 s alarm and the run a 300 s one; the loop is bounded.
//
// Darwin: nix develop -c clang -Wall -o core-class core-class.c && ./core-class
// Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq && apt-get install -y -qq gcc libc6-dev && gcc -Wall -o /tmp/p /probe/core-class.c && mkdir -p /tmp/cores && /tmp/p /tmp/cores'
//
// Measured 2026-09-26 on Darwin 25.6.0 (arm64, uid 501) and Linux 6.18.5
// (aarch64, root in the container, glibc 2.41). The rows are transcribed in
// WoofWare.PosixKernel.Test/TestSignal.fs.
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __APPLE__
#include <mach/mach.h>
#define HIGHEST 31
#else
#define HIGHEST 64
#endif

#ifdef __APPLE__
static mach_port_t port;

typedef struct {
    mach_msg_header_t head;
    mach_msg_body_t body;
    mach_msg_port_descriptor_t thread;
    mach_msg_port_descriptor_t task;
    NDR_record_t ndr;
    exception_type_t exception;
    mach_msg_type_number_t code_count;
    integer_t code[2];
    char trailer_space[256];
} request_t;

typedef struct {
    mach_msg_header_t head;
    NDR_record_t ndr;
    kern_return_t ret;
} reply_t;

// Waits up to a second for an exception message. Returns the exception type
// and its first code, or -1 if none came.
static int await_crash(int *code0)
{
    request_t req;
    memset(&req, 0, sizeof req);
    kern_return_t kr = mach_msg(&req.head, MACH_RCV_MSG | MACH_RCV_TIMEOUT, 0, sizeof req, port, 1000, MACH_PORT_NULL);
    if (kr != KERN_SUCCESS) return -1;
    *code0 = req.code[0];
    reply_t rep;
    memset(&rep, 0, sizeof rep);
    rep.head.msgh_bits = MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(req.head.msgh_bits), 0);
    rep.head.msgh_remote_port = req.head.msgh_remote_port;
    rep.head.msgh_local_port = MACH_PORT_NULL;
    rep.head.msgh_id = req.head.msgh_id + 100;
    rep.head.msgh_size = sizeof rep;
    rep.ndr = NDR_record;
    rep.ret = KERN_SUCCESS;
    mach_msg(&rep.head, MACH_SEND_MSG, sizeof rep, 0, MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    mach_port_deallocate(mach_task_self(), req.thread.name);
    mach_port_deallocate(mach_task_self(), req.task.name);
    return req.exception;
}
#endif

int main(int argc, char **argv)
{
    alarm(300);
    setvbuf(stdout, NULL, _IOLBF, 0);
#ifdef __APPLE__
    (void)argc;
    (void)argv;
    printf("# flavour darwin\n");
    if (mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &port) != KERN_SUCCESS) return 1;
    if (mach_port_insert_right(mach_task_self(), port, port, MACH_MSG_TYPE_MAKE_SEND) != KERN_SUCCESS) return 1;
    if (task_set_exception_ports(mach_task_self(), EXC_MASK_CRASH, port, EXCEPTION_DEFAULT, THREAD_STATE_NONE) != KERN_SUCCESS) return 1;
#else
    printf("# flavour linux\n");
    const char *dir = argc > 1 ? argv[1] : "/tmp";
#endif
    for (int s = 1; s <= HIGHEST; s++) {
        if (s == SIGKILL) continue;
        pid_t pid = fork();
        if (pid < 0) { perror("fork"); return 1; }
        if (pid == 0) {
            alarm(10);
#ifndef __APPLE__
            struct rlimit rl = { RLIM_INFINITY, RLIM_INFINITY };
            setrlimit(RLIMIT_CORE, &rl);
            if (chdir(dir) != 0) _exit(98);
#endif
            signal(s, SIG_DFL);
            sigset_t set;
            sigemptyset(&set);
            sigaddset(&set, s);
            sigprocmask(SIG_UNBLOCK, &set, NULL);
            kill(getpid(), s);
            _exit(77);
        }
#ifdef __APPLE__
        int code0 = 0;
        int exc = await_crash(&code0);
#endif
        int st;
        waitpid(pid, &st, WUNTRACED);
        if (WIFSTOPPED(st)) {
            kill(pid, SIGKILL);
            waitpid(pid, &st, 0);
            printf("%2d stopped\n", s);
            continue;
        }
        char desc[64];
        if (WIFEXITED(st)) snprintf(desc, sizeof desc, "exited(%d)", WEXITSTATUS(st));
        else snprintf(desc, sizeof desc, "signaled(%d)%s", WTERMSIG(st), WCOREDUMP(st) ? " core" : "");
#ifdef __APPLE__
        if (exc >= 0) printf("%2d %s EXC_CRASH exception=%d code0=0x%08x\n", s, desc, exc, (unsigned)code0);
        else printf("%2d %s no-exception\n", s, desc);
#else
        printf("%2d %s\n", s, desc);
#endif
    }
    return 0;
}
