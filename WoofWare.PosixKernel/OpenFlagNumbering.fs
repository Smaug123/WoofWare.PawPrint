namespace WoofWare.PosixKernel

/// Each flavour's `<fcntl.h>` number for the `open(2)` flags another syscall's
/// flag word reuses: `pipe2` takes them as they are, Linux numbers
/// `SOCK_NONBLOCK`, `SOCK_CLOEXEC` and `EPOLL_CLOEXEC` as `O_NONBLOCK` and
/// `O_CLOEXEC`, and `fcntl`'s `F_GETFL` and `F_SETFL` speak the status flags
/// in the same numbering.
[<RequireQualifiedAccess>]
module internal OpenFlagNumbering =

    // Linux's `<asm-generic/fcntl.h>`, which x86-64 and aarch64 share for these.

    /// Linux's `O_EXCL`, which is also `pipe2`'s `O_NOTIFICATION_PIPE`.
    [<Literal>]
    let LinuxExclusive : int = 0x80

    /// Linux's `O_APPEND`.
    [<Literal>]
    let LinuxAppend : int = 0x400

    /// Linux's `O_NONBLOCK`, which is also `SOCK_NONBLOCK`.
    [<Literal>]
    let LinuxNonBlock : int = 0x800

    /// Linux's `O_DSYNC`.
    [<Literal>]
    let LinuxDataSynchronous : int = 0x1000

    /// Linux's `FASYNC`, which `<fcntl.h>` spells `O_ASYNC`.
    [<Literal>]
    let LinuxAsynchronous : int = 0x2000

    /// Linux's `O_NOATIME`.
    [<Literal>]
    let LinuxNoAccessTime : int = 0x40000

    /// Linux's `__O_SYNC`, which with `O_DSYNC` beside it is `O_SYNC`.
    [<Literal>]
    let LinuxSynchronous : int = 0x100000

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

    /// `O_DIRECTORY`, whose number is the architecture's: aarch64's
    /// `<asm/fcntl.h>` moves it to 0x4000, and x86-64 keeps the generic
    /// 0x10000 (both measured by `open-flags.c`).
    let linuxDirectory (architecture : SimulatedUnixArchitecture) : int =
        match architecture with
        | SimulatedUnixArchitecture.Arm64 -> 0x4000
        | SimulatedUnixArchitecture.X64 -> 0x10000

    /// `O_NOFOLLOW`, whose number is the architecture's: 0x8000 on aarch64
    /// and 0x20000 on x86-64 (both measured by `open-flags.c`).
    let linuxNoFollow (architecture : SimulatedUnixArchitecture) : int =
        match architecture with
        | SimulatedUnixArchitecture.Arm64 -> 0x8000
        | SimulatedUnixArchitecture.X64 -> 0x20000

    /// `O_LARGEFILE`, whose number is the architecture's: 0x20000 on aarch64
    /// and 0x8000 on x86-64 (both measured by `open-flags.c`, whose `F_GETFL`
    /// shows it on every description `open(2)` makes).
    let linuxLargeFile (architecture : SimulatedUnixArchitecture) : int =
        match architecture with
        | SimulatedUnixArchitecture.Arm64 -> 0x20000
        | SimulatedUnixArchitecture.X64 -> 0x8000

    // Darwin's `<fcntl.h>`.

    /// Darwin's `O_NONBLOCK`.
    [<Literal>]
    let DarwinNonBlock : int = 0x4

    /// Darwin's `O_APPEND`.
    [<Literal>]
    let DarwinAppend : int = 0x8

    /// Darwin's `O_ASYNC`.
    [<Literal>]
    let DarwinAsynchronous : int = 0x40

    /// Darwin's `O_SYNC`.
    [<Literal>]
    let DarwinSynchronous : int = 0x80

    /// Darwin's `O_DSYNC`.
    [<Literal>]
    let DarwinDataSynchronous : int = 0x400000

    /// The bit Darwin's `F_GETFL` sets once an `flock(2)` lock has been
    /// granted to the description (the kernel's `FHASLOCK`).
    [<Literal>]
    let DarwinFlocked : int = 0x4000

    /// The bit Darwin's `F_GETFL` sets once something has been written through
    /// the description (the kernel's `FWASWRITTEN`).
    [<Literal>]
    let DarwinWritten : int = 0x10000

    /// Darwin's `O_CLOEXEC`.
    [<Literal>]
    let DarwinCloseOnExec : int = 0x1000000

    /// Darwin's `O_CLOFORK`. Not in the SDK's headers, but the kernel sets
    /// `FD_CLOFORK` for it.
    [<Literal>]
    let DarwinCloseOnFork : int = 0x8000000
