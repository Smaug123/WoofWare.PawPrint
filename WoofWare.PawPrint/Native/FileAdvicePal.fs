namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The `FileAdvice` numbering `SystemNative_PosixFAdvise` takes its advice
/// argument in (`pal_io.h`), and the screen the shim applies to it.
///
/// The numbering is the shim's own: the six `PAL_POSIX_FADV_*` values are
/// translated to the platform's `POSIX_FADV_*` inside `SystemNative_PosixFAdvise`
/// precisely because the two need not agree. So this is PawPrint's half of the
/// boundary, and the library gets a <c>FileAccessAdvice</c>.
///
/// The screen is the shim's too, not the kernel's, and that shows: measured, the
/// kernel rejects an unknown advice value only *after* it has looked the
/// descriptor up, so a bad advice on a closed descriptor is EBADF through the
/// syscall and EINVAL through this shim.
[<RequireQualifiedAccess>]
module FileAdvicePal =

    /// `PAL_POSIX_FADV_*` (`pal_io.h`), which CoreLib's
    /// `Interop.Sys.FileAdvice` restates.
    [<RequireQualifiedAccess>]
    module private Pal =
        [<Literal>]
        let Normal = 0

        [<Literal>]
        let Random = 1

        [<Literal>]
        let Sequential = 2

        [<Literal>]
        let WillNeed = 3

        [<Literal>]
        let DontNeed = 4

        [<Literal>]
        let NoReuse = 5

    /// What the shim's `switch` makes of this advice argument: `None` for the
    /// `default` arm, which returns EINVAL without reaching `posix_fadvise` —
    /// and so without the descriptor being looked at.
    let decode (advice : int) : FileAccessAdvice option =
        match advice with
        | Pal.Normal -> Some FileAccessAdvice.Normal
        | Pal.Random -> Some FileAccessAdvice.Random
        | Pal.Sequential -> Some FileAccessAdvice.Sequential
        | Pal.WillNeed -> Some FileAccessAdvice.WillNeed
        | Pal.DontNeed -> Some FileAccessAdvice.DontNeed
        | Pal.NoReuse -> Some FileAccessAdvice.NoReuse
        | _ -> None
