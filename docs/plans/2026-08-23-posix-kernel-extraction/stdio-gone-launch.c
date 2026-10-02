// A launcher that starts a command with descriptor `fd` the write end of a pipe
// whose read end was closed before the command started (closed even before the
// fork, so no process ever holds it), with SIGPIPE at its default, and reports
// how the command ended. Usage: stdio-gone-launch <fd> <command> [args...]
//
// It measured what real .NET does when its standard output, or its standard
// error, has no reader: the guests `WoofWare.PawPrint.Test/sourcesPure/
// StandardOutputGone.cs` and `StandardErrorGone.cs`, built as net10.0
// executables and run as `stdio-gone-launch 1 dotnet StandardOutputGone.dll`
// and `stdio-gone-launch 2 dotnet StandardErrorGone.dll`, three times each.
//
// Results, 2026-10-02, every run: exited 0, with the other stream's line
// printed. So Console.Out and Console.Error swallow the EPIPE of a write longer
// than a pipe holds, SystemNative_Write reports it as EPIPE (32), and the
// SIGPIPE each write raises does not end the process, whose runtime ignores it.
//   Darwin 27.0.0 arm64, .NET 10.0.7 (the devshell's)
//   Linux 6.18.5 aarch64, .NET 10.0.11 (mcr.microsoft.com/dotnet/sdk:10.0 in
//   Apple's `container`, the launcher built static in gcc:14)
// The control, StandardOutputGone.dll with standard output on /dev/null
// instead, which never answers EPIPE, exited 1 on Darwin: its raw writes all
// went through.
//
// Build: `nix develop -c clang -O0 -o /tmp/l <this file>` on Darwin,
// `gcc -O0 -static -o /tmp/l <this file>` on Linux.
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc < 3) return 2;
    int fd = atoi(argv[1]);
    int p[2];
    if (pipe(p) != 0) return 3;
    close(p[0]);
    pid_t pid = fork();
    if (pid == 0) {
        signal(SIGPIPE, SIG_DFL);
        dup2(p[1], fd);
        close(p[1]);
        execvp(argv[2], argv + 2);
        _exit(127);
    }
    close(p[1]);
    int ws = 0;
    waitpid(pid, &ws, 0);
    if (WIFSIGNALED(ws)) fprintf(stderr, "[gone-launch] killed by signal %d\n", WTERMSIG(ws));
    else fprintf(stderr, "[gone-launch] exited %d\n", WEXITSTATUS(ws));
    return 0;
}
