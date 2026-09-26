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
/// pinned `Interop.Errors.cs`, and puts every raw number from -300 to 4096 to
/// the real shim, and fails if this disagrees.
[<RequireQualifiedAccess>]
module UnixErrorPal =

    /// `Interop.Error.SUCCESS`, which is also raw errno 0. Not a `UnixError`
    /// case: "no error" is not an error.
    [<Literal>]
    let palSuccess : int = 0

    /// `Interop.Error.ENONSTANDARD`, which upstream's
    /// `ConvertErrorPlatformToPal` returns for any errno its switch has no
    /// `case` for: a number the platform names no error for, a negative, or an
    /// error `Interop.Error` has no member for (`ENOTBLK`, `ENOKEY`, `EAUTH`
    /// and many more; `toPal` answers it for those).
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
        | UnixError.EILSEQ -> 0x10019
        | UnixError.EAFNOSUPPORT -> 0x10005
        | UnixError.EPROTOTYPE -> 0x10046
        | UnixError.EPROTONOSUPPORT -> 0x10045
        | UnixError.ESOCKTNOSUPPORT -> 0x1005E
        | UnixError.EADDRINUSE -> 0x10003
        | UnixError.EADDRNOTAVAIL -> 0x10004
        | UnixError.EOPNOTSUPP -> 0x1003D
        | UnixError.ENOTSOCK -> 0x1003C
        | UnixError.EISCONN -> 0x1001E
        | UnixError.EINPROGRESS -> 0x1001A
        | UnixError.ECONNREFUSED -> 0x1000E
        // The PAL, like Linux, gives ENOTSUP and EOPNOTSUPP one value.
        | UnixError.ENOTSUP -> 0x1003D
        | UnixError.ENOTCONN -> 0x10038
        | UnixError.ETIMEDOUT -> 0x1004D
        | UnixError.ECONNRESET -> 0x1000F
        | UnixError.EMSGSIZE -> 0x10023
        | UnixError.ENOSYS -> 0x10037
        | UnixError.EDEADLK -> 0x10010
        | UnixError.ENOLCK -> 0x1002F
        | UnixError.ENOMSG -> 0x10032
        | UnixError.EIDRM -> 0x10018
        | UnixError.ENODATA -> 0x10071
        | UnixError.ENOLINK -> 0x10030
        | UnixError.EPROTO -> 0x10044
        | UnixError.EMULTIHOP -> 0x10024
        | UnixError.EBADMSG -> 0x10009
        | UnixError.EDESTADDRREQ -> 0x10011
        | UnixError.ENOPROTOOPT -> 0x10033
        | UnixError.EPFNOSUPPORT -> 0x10060
        | UnixError.ENETDOWN -> 0x10026
        | UnixError.ENETUNREACH -> 0x10028
        | UnixError.ENETRESET -> 0x10027
        | UnixError.ECONNABORTED -> 0x1000D
        | UnixError.ENOBUFS -> 0x1002A
        | UnixError.ESHUTDOWN -> 0x1006C
        | UnixError.EHOSTDOWN -> 0x10070
        | UnixError.EHOSTUNREACH -> 0x10017
        | UnixError.EALREADY -> 0x10007
        | UnixError.ESTALE -> 0x1004B
        | UnixError.EDQUOT -> 0x10013
        | UnixError.ECANCELED -> 0x1000B
        | UnixError.EOWNERDEAD -> 0x10041
        | UnixError.ENOTRECOVERABLE -> 0x1003B
        // Errors the enum has no member for: the shim's switch has no `case`
        // for them, so it answers ENONSTANDARD.
        | UnixError.ENOTBLK
        | UnixError.ENOSTR
        | UnixError.ETIME
        | UnixError.ENOSR
        | UnixError.EREMOTE
        | UnixError.EUSERS
        | UnixError.ETOOMANYREFS
        | UnixError.ECHRNG
        | UnixError.EL2NSYNC
        | UnixError.EL3HLT
        | UnixError.EL3RST
        | UnixError.ELNRNG
        | UnixError.EUNATCH
        | UnixError.ENOCSI
        | UnixError.EL2HLT
        | UnixError.EBADE
        | UnixError.EBADR
        | UnixError.EXFULL
        | UnixError.ENOANO
        | UnixError.EBADRQC
        | UnixError.EBADSLT
        | UnixError.EBFONT
        | UnixError.ENONET
        | UnixError.ENOPKG
        | UnixError.EADV
        | UnixError.ESRMNT
        | UnixError.ECOMM
        | UnixError.EDOTDOT
        | UnixError.ENOTUNIQ
        | UnixError.EBADFD
        | UnixError.EREMCHG
        | UnixError.ELIBACC
        | UnixError.ELIBBAD
        | UnixError.ELIBSCN
        | UnixError.ELIBMAX
        | UnixError.ELIBEXEC
        | UnixError.ERESTART
        | UnixError.ESTRPIPE
        | UnixError.EUCLEAN
        | UnixError.ENOTNAM
        | UnixError.ENAVAIL
        | UnixError.EISNAM
        | UnixError.EREMOTEIO
        | UnixError.ENOMEDIUM
        | UnixError.EMEDIUMTYPE
        | UnixError.ENOKEY
        | UnixError.EKEYEXPIRED
        | UnixError.EKEYREVOKED
        | UnixError.EKEYREJECTED
        | UnixError.ERFKILL
        | UnixError.EHWPOISON
        | UnixError.EPROCLIM
        | UnixError.EBADRPC
        | UnixError.ERPCMISMATCH
        | UnixError.EPROGUNAVAIL
        | UnixError.EPROGMISMATCH
        | UnixError.EPROCUNAVAIL
        | UnixError.EFTYPE
        | UnixError.EAUTH
        | UnixError.ENEEDAUTH
        | UnixError.EPWROFF
        | UnixError.EDEVERR
        | UnixError.EBADEXEC
        | UnixError.EBADARCH
        | UnixError.ESHLIBVERS
        | UnixError.EBADMACHO
        | UnixError.ENOATTR
        | UnixError.ENOPOLICY
        | UnixError.EQFULL
        | UnixError.ENOTCAPABLE -> palNonStandard

    /// PawPrint's `SystemNative_ConvertErrorPlatformToPal`: raw errno to PAL
    /// `Interop.Error`, for a caller that names no platform.
    ///
    ///   * **Portable** — the number means the same error on every Unix we
    ///     model, so answer its PAL value (which is `ENONSTANDARD` for
    ///     `ENOTBLK`, the one such error `Interop.Error` has no member for).
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
    /// the C does. `ofRawErrnoUnder` answers the last case too.
    let ofRawErrno (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match UnixError.ofRawErrno raw with
        | Some error -> toPal error
        | None when UnixError.isUnambiguouslyNonStandardRawErrno raw -> palNonStandard
        | None ->

        failwith
            $"UnixErrorPal.ofRawErrno: cannot convert raw errno %d{raw} to a PAL Interop.Error value. Without a platform, PawPrint only maps the errnos that name the same error on every Unix it models (1-34 except 11); outside that set a raw number is platform-dependent — 39 is ENOTEMPTY on Linux but EDESTADDRREQ on Darwin, and 11 is EAGAIN on Linux but EDEADLK on Darwin. Upstream's ConvertErrorPlatformToPal answers from the one platform's <errno.h> it was compiled against, and this call site named no platform. Use ofRawErrnoUnder."

    /// `ofRawErrno` for a caller that knows which numbering the kernel reports —
    /// which is every caller inside the emulated kernel — and so total, exactly
    /// as the shim compiled for that platform is.
    ///
    /// A number the platform's `<errno.h>` names an error for answers that
    /// error's PAL value, so raw 40 is `ELOOP` under Linux and `EMSGSIZE` under
    /// Darwin. Any other number is one the shim's switch has no `case` for, so
    /// it answers `ENONSTANDARD`: `UnixError`'s table holds every error either
    /// header names, and every `Interop.Error` member names one of them.
    let ofRawErrnoUnder (reporting : RawErrnoNumbering) (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match UnixError.ofRawErrnoUnder reporting raw with
        | Some error -> toPal error
        | None -> palNonStandard
