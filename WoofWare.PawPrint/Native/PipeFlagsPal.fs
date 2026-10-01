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
    /// Where the shim is built without `pipe2` (as it may be on Darwin), it calls
    /// `pipe(2)` and then sets `FD_CLOEXEC` on each end with `fcntl`. The
    /// kernel models no per-descriptor flag, so that and `pipe2` with
    /// `O_CLOEXEC` leave it in the same state.
    let decode (platform : SimulatedUnixPlatform) (flags : int) : int option =
        let closeOnExec =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> 0x80000
            | SimulatedUnixFlavour.Darwin -> 0x1000000

        match flags with
        | 0 -> Some 0
        | CloseOnExec -> Some closeOnExec
        | _ -> None
