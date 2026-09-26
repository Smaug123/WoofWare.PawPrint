namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The `FileAdvice` numbering `SystemNative_PosixFAdvise` takes its advice
/// argument in (`pal_io.h`), and the screen the shim applies to it.
///
/// The numbering is the shim's own: the six `PAL_POSIX_FADV_*` values are
/// translated to the platform's `POSIX_FADV_*` inside `SystemNative_PosixFAdvise`
/// precisely because the two need not agree. So this is PawPrint's half of the
/// boundary, and the library gets the platform's raw `POSIX_FADV_*` number. Only
/// Linux provides the call, and its numbers on x86-64 and aarch64 happen to equal
/// the PAL's; the translation is still stated row by row, because on other Linux
/// architectures (s390x numbers `DONTNEED` 6) they differ.
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

    /// What the shim's `switch` makes of this advice argument: Linux's
    /// `POSIX_FADV_*` number (x86-64 and aarch64 numbering), or `None` for the
    /// `default` arm, which returns EINVAL without reaching `posix_fadvise` —
    /// and so without the descriptor being looked at.
    let decode (advice : int) : int option =
        match advice with
        | Pal.Normal -> Some 0
        | Pal.Random -> Some 1
        | Pal.Sequential -> Some 2
        | Pal.WillNeed -> Some 3
        | Pal.DontNeed -> Some 4
        | Pal.NoReuse -> Some 5
        | _ -> None
