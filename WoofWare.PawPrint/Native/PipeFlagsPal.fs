namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The `PipeFlags` numbering `SystemNative_Pipe` takes its flags argument in
/// (`pal_io.h`), and the screen the shim applies to it.
///
/// The shim's `switch` admits 0 and `PAL_O_CLOEXEC` alone, answering EINVAL for
/// anything else before it calls the kernel, and translates `PAL_O_CLOEXEC` to
/// the platform's `O_CLOEXEC` for `pipe2(2)`. So this is PawPrint's half of the
/// boundary, and the library gets the flavour's raw `pipe2` flags.
[<RequireQualifiedAccess>]
module PipeFlagsPal =

    /// `PAL_O_CLOEXEC` (`pal_io.h`), which CoreLib's `Interop.Sys.PipeFlags`
    /// restates, and which System.Native's own signal initialisation passes.
    [<Literal>]
    let CloseOnExec = 0x0010

    /// What the shim hands `pipe2` for this flags argument, in `platform`'s own
    /// `<fcntl.h>` numbering, or `None` for the `default` arm, which answers
    /// EINVAL without reaching the kernel.
    ///
    /// Where the shim is built without `pipe2` (as it is on Darwin; see
    /// `atomicNonInheritableCreation`), it calls `pipe(2)` and then sets
    /// `FD_CLOEXEC` on each end with `fcntl`, which leaves the kernel in the
    /// state `pipe2` with `O_CLOEXEC` does.
    let decode (platform : SimulatedUnixPlatform) (flags : int) : int option =
        let closeOnExec =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> 0x80000
            | SimulatedUnixFlavour.Darwin -> 0x1000000

        match flags with
        | 0 -> Some 0
        | CloseOnExec -> Some closeOnExec
        | _ -> None

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
