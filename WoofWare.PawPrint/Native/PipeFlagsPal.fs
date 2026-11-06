namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// What `SystemNative_Pipe` does for a flags argument its screen admits: `pipe2(2)` with
/// `Pipe2Flags`, in the platform's own `<fcntl.h>` numbering, then `O_NONBLOCK` set on each end
/// that asks for it.
type PipeRequest =
    {
        /// The flags the shim hands `pipe2`.
        Pipe2Flags : int
        /// Whether the shim then sets `O_NONBLOCK` on the read end (`pipeFds[0]`).
        NonBlockingRead : bool
        /// Whether the shim then sets `O_NONBLOCK` on the write end (`pipeFds[1]`), once the read
        /// end's, if asked for, has succeeded.
        NonBlockingWrite : bool
    }

/// The `PipeFlags` numbering `SystemNative_Pipe` takes its flags argument in
/// (`pal_io.h`), and the screen the shim applies to it, which differs between
/// runtimes.
///
/// The shim answers EINVAL for a word its screen refuses before it calls the
/// kernel, and translates `PAL_O_CLOEXEC` to the platform's `O_CLOEXEC` for
/// `pipe2(2)`. So this is PawPrint's half of the boundary, and the library gets
/// the flavour's raw `pipe2` flags.
[<RequireQualifiedAccess>]
module PipeFlagsPal =

    /// `PAL_O_CLOEXEC` (`pal_io.h`), which CoreLib's `Interop.Sys.PipeFlags`
    /// restates, and which System.Native's own signal initialisation passes.
    [<Literal>]
    let CloseOnExec = 0x0010

    /// `PAL_O_NONBLOCK_READ` (.NET 11's `pal_io.h`): set `O_NONBLOCK` on the read end.
    [<Literal>]
    let NonBlockingRead = 0x0400

    /// `PAL_O_NONBLOCK_WRITE` (.NET 11's `pal_io.h`): set `O_NONBLOCK` on the write end.
    [<Literal>]
    let NonBlockingWrite = 0x0800

    /// The platform's `O_CLOEXEC`, which the shim hands `pipe2` for `PAL_O_CLOEXEC`.
    ///
    /// Where the shim is built without `pipe2` (as it is on Darwin; see
    /// `atomicNonInheritableCreation`), it calls `pipe(2)` and then sets
    /// `FD_CLOEXEC` on each end with `fcntl`, which leaves the kernel in the
    /// state `pipe2` with `O_CLOEXEC` does.
    let closeOnExec (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 0x80000
        | SimulatedUnixFlavour.Darwin -> 0x1000000

    /// What `runtime`'s shim does for this flags argument on `platform`, or `None` where its
    /// screen answers EINVAL without reaching the kernel.
    ///
    /// .NET 10's shim admits 0 and `PAL_O_CLOEXEC` alone. .NET 11's admits any combination of
    /// `PAL_O_CLOEXEC`, `PAL_O_NONBLOCK_READ` and `PAL_O_NONBLOCK_WRITE`.
    let decode (runtime : EmulatedRuntime) (platform : SimulatedUnixPlatform) (flags : int) : PipeRequest option =
        let admitted =
            match runtime with
            // `switch (flags)` with `case 0` and `case PAL_O_CLOEXEC`.
            | EmulatedRuntime.Net10 -> flags = 0 || flags = CloseOnExec
            // `if ((flags & ~(PAL_O_CLOEXEC | PAL_O_NONBLOCK_READ | PAL_O_NONBLOCK_WRITE)) != 0)`.
            | EmulatedRuntime.Net11 -> flags &&& ~~~(CloseOnExec ||| NonBlockingRead ||| NonBlockingWrite) = 0

        if not admitted then
            None
        else
            Some
                {
                    Pipe2Flags =
                        if flags &&& CloseOnExec <> 0 then
                            closeOnExec platform
                        else
                            0
                    NonBlockingRead = flags &&& NonBlockingRead <> 0
                    NonBlockingWrite = flags &&& NonBlockingWrite <> 0
                }

    /// Whether the shim was built with `pipe2(2)`, and so creates a close-on-exec
    /// pipe in one system call. Reported to the guest by .NET 11's
    /// `SystemNative_IsAtomicNonInheritablePipeCreationSupported`, whose whole
    /// body is `#if HAVE_PIPE2 return 1; #else return 0;`.
    ///
    /// A compile-time property of the shim rather than of the running kernel.
    /// Measured in .NET 11 RC1's shipped shims: the linux-x64
    /// `libSystem.Native.so` imports `pipe2@GLIBC_2.9` and returns 1, and the
    /// osx-arm64 `libSystem.Native.dylib` imports only `pipe` and returns 0.
    let atomicNonInheritableCreation (platform : SimulatedUnixPlatform) : bool =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false
