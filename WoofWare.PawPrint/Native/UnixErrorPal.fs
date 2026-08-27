namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `Interop.Error` numbering of the errors `WoofWare.PosixKernel`
/// speaks, and the conversions the `SystemNative_*` shims perform across it.
///
/// This is PawPrint's half of the errno boundary. The library states the raw
/// `<errno.h>` number, which is what a kernel states; the PAL numbering is
/// .NET's own, deliberately placed outside the errno range (`0x1xxxx`) so that
/// the two cannot be confused, and it is what CoreLib switches on after
/// `SystemNative_ConvertErrorPlatformToPal`.
///
/// The table below is a second exhaustive match over `UnixError`, so the
/// compiler keeps it complete but cannot keep it *correct*. Its oracle is not
/// the library but upstream: `TestUnixErrorPal` re-derives every value from the
/// pinned `Interop.Errors.cs` and fails if this disagrees.
[<RequireQualifiedAccess>]
module UnixErrorPal =

    /// `Interop.Error.SUCCESS`, which is also raw errno 0. Not a `UnixError`
    /// case: "no error" is not an error.
    [<Literal>]
    let palSuccess : int = 0

    /// `Interop.Error.ENONSTANDARD`, which upstream's
    /// `ConvertErrorPlatformToPal` returns for any errno it does not recognise.
    /// PawPrint returns it only where "unrecognised" is platform-independent —
    /// a raw errno inside the portable range that the PAL enum has no name for,
    /// today only `ENOTBLK`. Where "unrecognised" instead means "means
    /// different things on different Unixes", `ofRawErrno` fails rather than
    /// answering this; see there.
    [<Literal>]
    let palNonStandard : int = 0x1FFFF

    /// The `Interop.Error` value CoreLib switches on. Total: the PAL numbering
    /// is platform-independent, so it is always answerable.
    let toPal (error : UnixError) : int =
        match error with
        | UnixError.EPERM -> 0x10042
        | UnixError.ENOENT -> 0x1002D
        | UnixError.ESRCH -> 0x1004A
        | UnixError.EINTR -> 0x1001B
        | UnixError.EIO -> 0x1001D
        | UnixError.ENXIO -> 0x1003F
        | UnixError.E2BIG -> 0x10001
        | UnixError.ENOEXEC -> 0x1002E
        | UnixError.EBADF -> 0x10008
        | UnixError.ECHILD -> 0x1000C
        | UnixError.ENOMEM -> 0x10031
        | UnixError.EACCES -> 0x10002
        | UnixError.EFAULT -> 0x10015
        | UnixError.EBUSY -> 0x1000A
        | UnixError.EEXIST -> 0x10014
        | UnixError.EXDEV -> 0x1004F
        | UnixError.ENODEV -> 0x1002C
        | UnixError.ENOTDIR -> 0x10039
        | UnixError.EISDIR -> 0x1001F
        | UnixError.EINVAL -> 0x1001C
        | UnixError.ENFILE -> 0x10029
        | UnixError.EMFILE -> 0x10021
        | UnixError.ENOTTY -> 0x1003E
        | UnixError.ETXTBSY -> 0x1004E
        | UnixError.EFBIG -> 0x10016
        | UnixError.ENOSPC -> 0x10034
        | UnixError.ESPIPE -> 0x10049
        | UnixError.EROFS -> 0x10048
        | UnixError.EMLINK -> 0x10022
        | UnixError.EPIPE -> 0x10043
        | UnixError.EDOM -> 0x10012
        | UnixError.ERANGE -> 0x10047
        | UnixError.ELOOP -> 0x10020
        | UnixError.ENAMETOOLONG -> 0x10025
        | UnixError.ENOTEMPTY -> 0x1003A
        | UnixError.EAGAIN -> 0x10006
        | UnixError.EOVERFLOW -> 0x10040
        | UnixError.EAFNOSUPPORT -> 0x10005
        | UnixError.EPROTOTYPE -> 0x10046
        | UnixError.EPROTONOSUPPORT -> 0x10045
        | UnixError.EADDRINUSE -> 0x10003
        | UnixError.EADDRNOTAVAIL -> 0x10004
        | UnixError.EOPNOTSUPP -> 0x1003D
        | UnixError.ENOTSOCK -> 0x1003C
        | UnixError.EISCONN -> 0x1001E
        | UnixError.EINPROGRESS -> 0x1001A
        | UnixError.ECONNREFUSED -> 0x1000E

    /// PawPrint's `SystemNative_ConvertErrorPlatformToPal`: raw errno to PAL
    /// `Interop.Error`.
    ///
    ///   * **Portable and named** — answer its PAL value.
    ///   * **Portable but unnamed** — the number means the same thing on every
    ///     Unix we model, but the BCL's `Interop.Error` has no entry for it.
    ///     `ENOTBLK` (15) is the only such value. Upstream's switch has no case
    ///     for it either, so it falls through to `Error_ENONSTANDARD` — and that
    ///     answer is platform-independent, so we can give it too.
    ///   * **Negative** — POSIX errnos are positive, so every Unix we model
    ///     falls through to `ENONSTANDARD` for these; that is unambiguous and
    ///     needs no platform table, so we answer it. Reachable through
    ///     `Marshal.SetLastSystemError`, and through the synthetic
    ///     `EHOSTNOTFOUND` / `ESOCKETERROR` pseudo-errnos, which upstream
    ///     defines as the fixed negatives `-0x20001` / `-0x20002`.
    ///   * **Not portable** — 11, and everything from 35 up. Upstream answers
    ///     these from whichever platform's `<errno.h>` it was compiled against;
    ///     this converter has been given no platform, so it cannot. Answering
    ///     `ENONSTANDARD` for raw 39 would be silently wrong on Linux, where
    ///     upstream returns `Error_ENOTEMPTY`, so this fails loudly instead.
    ///
    /// Only the last case diverges from upstream; the others answer exactly what
    /// the C does.
    let ofRawErrno (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match UnixError.ofRawErrno raw with
        | Some error -> toPal error
        | None when UnixError.isPortableRawErrno raw ->
            // ENOTBLK, today the only member of this class.
            palNonStandard
        | None when UnixError.isUnambiguouslyNonStandardRawErrno raw -> palNonStandard
        | None ->

        failwith
            $"UnixErrorPal.ofRawErrno: cannot convert raw errno %d{raw} to a PAL Interop.Error value. PawPrint only maps the errnos that name the same error on every Unix it models (1-34 except 11); outside that set a raw number is platform-dependent — 39 is ENOTEMPTY on Linux but EDESTADDRREQ on Darwin, and 11 is EAGAIN on Linux but EDEADLK on Darwin. Upstream's ConvertErrorPlatformToPal answers ENONSTANDARD here because it was compiled against one platform's <errno.h> and this call site named no platform. If a guest legitimately needs this errno, use ofRawErrnoUnder, or decide the numbering (see issue #956); if it reached here via Marshal.SetLastSystemError, the guest is asserting a platform PawPrint does not model."

    /// `ofRawErrno` for a caller that knows which numbering the kernel reports —
    /// which is every caller inside the emulated kernel.
    ///
    /// A table entry whose raw number is platform-dependent becomes matchable,
    /// so raw 40 answers `ELOOP` under Linux and raw 39 answers `ENOTEMPTY`. A
    /// number the table does not contain at all still fails loudly rather than
    /// falling through to `ENONSTANDARD`: raw 35 under Linux really is
    /// `EDEADLK`, which PawPrint has not modelled, and `ENONSTANDARD` would
    /// silently take a guest down the wrong branch of an
    /// `if (errorInfo.Error == Interop.Error.EDEADLK)`. Knowing the platform
    /// does not conjure a table entry.
    let ofRawErrnoUnder (reporting : RawErrnoNumbering) (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match UnixError.ofRawErrnoUnder reporting raw with
        | Some error -> toPal error
        | None when UnixError.isPortableRawErrno raw -> palNonStandard
        | None when UnixError.isUnambiguouslyNonStandardRawErrno raw -> palNonStandard
        | None ->
            failwith
                $"UnixErrorPal.ofRawErrnoUnder: cannot convert raw errno %d{raw} to a PAL Interop.Error value under the %O{reporting} numbering. The number is outside the portable set (1-34 except 11) and the table has no entry for it on this platform — raw 35 under Linux is EDEADLK, which is not yet modelled. Answering ENONSTANDARD would silently take a guest down the wrong branch of an `if (errorInfo.Error == Interop.Error.EDEADLK)`, so this fails instead. Add the error to UnixError's table (with RawErrnoPortability.PlatformDependent if the two Unixes disagree); if it reached here via Marshal.SetLastSystemError, the guest is asserting an errno PawPrint does not model."
