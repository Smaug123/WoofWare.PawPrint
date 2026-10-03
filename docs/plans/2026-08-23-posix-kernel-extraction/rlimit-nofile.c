// The RLIMIT_NOFILE a process starts with, where nothing between the kernel
// (or the service manager) and the process has changed it: the bound
// WoofWare.PosixKernel assumes every process's soft limit reaches
// (SimulatedUnixPlatform.descriptorBound).
//
// Prints the soft and hard limit, and on Linux the fs.nr_open ceiling no
// limit may exceed. Run as the initramfs's /init it powers the machine off
// when done, since an /init that exits panics the kernel.
//
// Measured 2026-10-04: the first row by this probe, the rest by the shell's
// `ulimit -Sn` and `ulimit -Hn`, which read the same getrlimit.
//
//   where                                                  soft     hard
//   Linux 6.12 x86-64, as the kernel's first process        1024     4096
//     (QEMU, Debian trixie's linux-image-amd64, this
//     probe as /init; nr_open 1048576)
//   Linux 6.18.5 aarch64 container, root                1048576  1048576
//     (the container runtime raises it; nr_open 1048576)
//   ... uid 1000 through su or runuser (PAM)               1024  1048576
//   ... uid 1000 through setpriv (inherits root's)      1048576  1048576
//   Darwin 27.0.0, a job launchd starts (launchctl          256  unlimited
//     submit; `launchctl limit maxfiles` says the same)
//   Darwin 27.0.0, the shell this was run from          1048576  unlimited
//     (an application raised it)
//
// So the kernel's own default is 1024 on Linux (INR_OPEN_CUR, which PAM's
// login path keeps) and launchd's is 256 on Darwin. Container runtimes and
// applications raise the limit; only a parent that lowers it starts a process
// below those.
//
// Build:
//   Darwin:  nix develop -c clang -Wall -o /tmp/rl rlimit-nofile.c
//   Linux:   gcc -Wall -static -o init rlimit-nofile.c
#define _GNU_SOURCE
#include <stdio.h>
#include <sys/resource.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/stat.h>
#endif

int main(void)
{
    struct rlimit rl;
    getrlimit(RLIMIT_NOFILE, &rl);
    printf("RLIMIT_NOFILE soft %lld hard %lld pid %d uid %d\n", (long long)rl.rlim_cur, (long long)rl.rlim_max,
           (int)getpid(), (int)getuid());
#ifdef __linux__
    if (getpid() == 1) {
        mkdir("/proc", 0555);
        mount("proc", "/proc", "proc", 0, NULL);
    }
    FILE *f = fopen("/proc/sys/fs/nr_open", "r");
    if (f) {
        long n = -1;
        if (fscanf(f, "%ld", &n) == 1) printf("nr_open %ld\n", n);
        fclose(f);
    } else {
        printf("nr_open unreadable (no /proc)\n");
    }
    fflush(stdout);
    if (getpid() == 1) reboot(RB_POWER_OFF);
#endif
    return 0;
}
