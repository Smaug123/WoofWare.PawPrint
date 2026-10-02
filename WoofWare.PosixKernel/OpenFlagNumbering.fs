namespace WoofWare.PosixKernel

/// Each flavour's `<fcntl.h>` number for the `open(2)` flags another syscall's
/// flag word reuses: `pipe2` takes them as they are, and Linux numbers
/// `SOCK_NONBLOCK`, `SOCK_CLOEXEC` and `EPOLL_CLOEXEC` as `O_NONBLOCK` and
/// `O_CLOEXEC`.
[<RequireQualifiedAccess>]
module internal OpenFlagNumbering =

    // Linux's `<asm-generic/fcntl.h>`, which x86-64 and aarch64 share for these.

    /// Linux's `O_EXCL`, which is also `pipe2`'s `O_NOTIFICATION_PIPE`.
    [<Literal>]
    let LinuxExclusive : int = 0x80

    /// Linux's `O_NONBLOCK`, which is also `SOCK_NONBLOCK`.
    [<Literal>]
    let LinuxNonBlock : int = 0x800

    /// Linux's `O_CLOEXEC`, which is also `SOCK_CLOEXEC` and `EPOLL_CLOEXEC`.
    [<Literal>]
    let LinuxCloseOnExec : int = 0x80000

    /// `O_DIRECT`, whose number is the architecture's. aarch64's
    /// `<asm/fcntl.h>` moves it to 0x10000 (measured: `pipe2` with that bit
    /// makes a packet pipe on Linux 6.18.5 aarch64, and 0x4000, which is
    /// aarch64's `O_DIRECTORY`, is EINVAL). x86-64 keeps the generic 0x4000;
    /// that column is the uapi header's, and `TestPipeAgainstHost` holds it to
    /// the x86-64 kernel CI runs on.
    let linuxDirect (architecture : SimulatedUnixArchitecture) : int =
        match architecture with
        | SimulatedUnixArchitecture.Arm64 -> 0x10000
        | SimulatedUnixArchitecture.X64 -> 0x4000

    // Darwin's `<fcntl.h>`.

    /// Darwin's `O_NONBLOCK`.
    [<Literal>]
    let DarwinNonBlock : int = 0x4

    /// Darwin's `O_CLOEXEC`.
    [<Literal>]
    let DarwinCloseOnExec : int = 0x1000000

    /// Darwin's `O_CLOFORK`. Not in the SDK's headers, but the kernel sets
    /// `FD_CLOFORK` for it.
    [<Literal>]
    let DarwinCloseOnFork : int = 0x8000000
