namespace WoofWare.PosixKernel

/// The instruction set the simulated process runs as, whose ABI decides the
/// layout of the structures a syscall reads and writes.
///
/// A kernel image is built for one instruction set, so this is a fact about the
/// kernel a process is running on, not about the machine underneath it.
[<RequireQualifiedAccess>]
type SimulatedUnixArchitecture =
    /// 64-bit x86. `uname -m` reports `x86_64` on Linux and Darwin alike.
    | X64
    /// 64-bit ARM. `uname -m` reports `aarch64` on Linux and `arm64` on Darwin.
    | Arm64

/// The size of a page of memory, which is the unit a kernel maps, and rounds
/// some transfer limits down to.
///
/// A kernel image is built for one page size. x86-64 has only 4 KiB base pages;
/// a Linux kernel for 64-bit ARM may be built for 4, 16 or 64 KiB; Darwin on
/// 64-bit ARM uses 16 KiB.
[<RequireQualifiedAccess>]
type SimulatedPageSize =
    /// 4096 bytes.
    | FourKiB
    /// 16384 bytes.
    | SixteenKiB
    /// 65536 bytes.
    | SixtyFourKiB

[<RequireQualifiedAccess>]
module SimulatedPageSize =
    /// The page size in bytes: what `sysconf(_SC_PAGESIZE)` reports.
    let bytes (pageSize : SimulatedPageSize) : int =
        match pageSize with
        | SimulatedPageSize.FourKiB -> 4096
        | SimulatedPageSize.SixteenKiB -> 16384
        | SimulatedPageSize.SixtyFourKiB -> 65536
