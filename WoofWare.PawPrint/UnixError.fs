namespace WoofWare.PawPrint

/// How portable a raw `<errno.h>` number is across the Unixes PawPrint models.
///
/// The distinction is load-bearing rather than documentary. Raw errno *numbers*
/// are assigned per-platform, and Linux and Darwin agree only on the range they
/// both inherited from V7 Unix. Concretely, values 1-34 name the same error on
/// both — with exactly one exception, 11, where the two are transposed
/// (`EAGAIN` is 11 on Linux and 35 on Darwin; `EDEADLK` is 35 on Linux and 11 on
/// Darwin). Everything from 35 up was numbered independently, so e.g. 39 is
/// `ENOTEMPTY` on Linux and `EDESTADDRREQ` on Darwin.
///
/// PawPrint has not chosen which platform's numbering its emulated kernel
/// reports; see the `PlatformDependent` case for why that is deliberate.
[<RequireQualifiedAccess>]
type RawErrnoPortability =
    /// This error has the same raw number on every Unix PawPrint models, so the
    /// emulated kernel can report it without first deciding which platform it is
    /// impersonating.
    | Portable of value : int
    /// This error's raw number differs between the Unixes PawPrint models, and
    /// the project has not yet decided which numbering `EmulatedKernel.
    /// LastSystemError` reports. Carries both candidates so that the failure
    /// naming them is a one-liner, and so that the fact lives in the table
    /// rather than in a comment.
    ///
    /// No `UnixError` case uses this today. It exists so that adding one is a
    /// *compile-time* fork in the road: a future case for, say, `ENOTEMPTY`
    /// cannot be given a raw number without either picking a platform (and
    /// saying so) or landing here and inheriting the loud failure. Relying on
    /// whoever adds it to remember the transposition would be relying on
    /// discipline; this makes the machine ask.
    | PlatformDependent of linux : int * darwin : int

/// The errors PawPrint's emulated `SystemNative_*` shims can report, as a closed
/// vocabulary rather than bare integers.
///
/// Each case carries two numbers, which must not be confused (the BCL's own
/// comment in `Interop.Errors.cs` warns about exactly this):
///
///   * the **raw** `<errno.h>` number, which is what the host's `errno` would
///     hold and what a guest sees through `Marshal.GetLastSystemError` /
///     `Marshal.GetLastPInvokeError`; and
///   * the **PAL** value from the BCL's `Interop.Error` enum, deliberately
///     numbered outside the errno range (`0x1xxxx`) so the two cannot be mixed
///     up silently. This is what CoreLib actually switches on, after converting
///     via `SystemNative_ConvertErrorPlatformToPal`.
///
/// Membership is not arbitrary: this is exactly the set of errors whose raw
/// number is the same on every Unix we model *and* which the BCL's PAL enum has
/// a name for. `ENOTBLK` (raw 15) is the sole error in the portable range that
/// is excluded, because `Interop.Error` has no entry for it. Errors outside the
/// portable range — `ENOTEMPTY`, `ELOOP`, `EAGAIN`, `EOVERFLOW`, and every other
/// value from 35 up — are deliberately absent; see `RawErrnoPortability`.
[<RequireQualifiedAccess>]
type UnixError =
    /// `EPERM` — Operation not permitted.
    | EPERM
    /// `ENOENT` — No such file or directory.
    | ENOENT
    /// `ESRCH` — No such process.
    | ESRCH
    /// `EINTR` — Interrupted function.
    | EINTR
    /// `EIO` — I/O error.
    | EIO
    /// `ENXIO` — No such device or address.
    | ENXIO
    /// `E2BIG` — Argument list too long.
    | E2BIG
    /// `ENOEXEC` — Executable file format error.
    | ENOEXEC
    /// `EBADF` — Bad file descriptor.
    /// Returned by `dup`, `close`, `read`, `write`, etc. when the supplied
    /// fd is not currently open.
    | EBADF
    /// `ECHILD` — No child processes.
    | ECHILD
    /// `ENOMEM` — Not enough space.
    | ENOMEM
    /// `EACCES` — Permission denied.
    | EACCES
    /// `EFAULT` — Bad address.
    /// Returned by `read(2)` / `write(2)` and friends when the buffer
    /// pointer is outside the process's accessible address space — most
    /// notably when the caller passes `NULL` (or any other
    /// non-dereferenceable bit pattern) with a non-zero `bufferSize`. The
    /// kernel performs no I/O for such a call. PawPrint maps a null /
    /// non-managed buffer here rather than crashing the interpreter, so
    /// guests that issue a direct P/Invoke (skipping the BCL's null-guard in
    /// `Stream.Write`) see the same error as on the real runtime.
    | EFAULT
    /// `EBUSY` — Device or resource busy.
    | EBUSY
    /// `EEXIST` — File exists.
    | EEXIST
    /// `EXDEV` — Cross-device link.
    | EXDEV
    /// `ENODEV` — No such device.
    | ENODEV
    /// `ENOTDIR` — Not a directory or a symbolic link to a directory.
    | ENOTDIR
    /// `EISDIR` — Is a directory.
    | EISDIR
    /// `EINVAL` — Invalid argument.
    /// Returned by `sigaction(2)` when the caller asks to install a handler
    /// for an uncatchable signal (`SIGKILL` or `SIGSTOP`). PawPrint surfaces
    /// this through `SystemNative_EnablePosixSignalHandling`, which the BCL's
    /// `PosixSignalRegistration.Create` reads via `Marshal.GetLastSystemError`
    /// to throw a meaningful error to the guest.
    | EINVAL
    /// `ENFILE` — Too many files open in system.
    | ENFILE
    /// `EMFILE` — File descriptor value too large.
    | EMFILE
    /// `ENOTTY` — Inappropriate I/O control operation.
    /// Returned by `isatty(3)` when the supplied fd is open but does not
    /// refer to a terminal. PawPrint surfaces this through
    /// `SystemNative_IsATty`, which always reports "not a terminal" for live
    /// fds because the simulated process is headless.
    | ENOTTY
    /// `ETXTBSY` — Text file busy.
    | ETXTBSY
    /// `EFBIG` — File too large.
    | EFBIG
    /// `ENOSPC` — No space left on device.
    | ENOSPC
    /// `ESPIPE` — Invalid seek.
    | ESPIPE
    /// `EROFS` — Read-only file system.
    | EROFS
    /// `EMLINK` — Too many links.
    | EMLINK
    /// `EPIPE` — Broken pipe.
    | EPIPE
    /// `EDOM` — Mathematics argument out of domain of function.
    | EDOM
    /// `ERANGE` — Result too large.
    /// Used by PawPrint's `SystemNative_Write` / `SystemNative_Read` shims
    /// when the caller supplies a negative `bufferSize`, matching
    /// `Common_Write` / `Common_Read` in `pal_io_common.h`, which set
    /// `errno = ERANGE` and return -1 for negative sizes before the real
    /// `read(2)` / `write(2)` is invoked.
    | ERANGE

/// The raw and PAL numbering of one `UnixError`.
type UnixErrorNumbering =
    {
        /// Value from the BCL's `Interop.Error` enum. Platform-independent by
        /// construction — upstream chose `0x1xxxx` precisely so that this
        /// number means the same thing everywhere.
        Pal : int
        /// The `<errno.h>` number, together with whether PawPrint is in a
        /// position to state it.
        Raw : RawErrnoPortability
    }

[<RequireQualifiedAccess>]
module UnixError =
    /// `Interop.Error.SUCCESS`, which is also raw errno 0. Not a `UnixError`
    /// case: "no error" is not an error, and letting it into the DU would make
    /// every consumer handle a case that never denotes a failure.
    [<Literal>]
    let palSuccess : int = 0

    /// `Interop.Error.ENONSTANDARD`, which upstream's
    /// `ConvertErrorPlatformToPal` returns for any errno it does not recognise.
    /// PawPrint does not currently return this — see `palOfRawErrno` for why —
    /// but the value is here because that function's contract is stated in
    /// terms of it.
    [<Literal>]
    let palNonStandard : int = 0x1FFFF

    /// Every case of `UnixError`. The property tests fold over this to check the
    /// table against the pinned upstream sources, so it must stay exhaustive;
    /// `numbering` below is a total match, so the compiler catches a case added
    /// here and forgotten there, and `allCasesAreListed` catches the reverse.
    let all : UnixError list =
        [
            UnixError.EPERM
            UnixError.ENOENT
            UnixError.ESRCH
            UnixError.EINTR
            UnixError.EIO
            UnixError.ENXIO
            UnixError.E2BIG
            UnixError.ENOEXEC
            UnixError.EBADF
            UnixError.ECHILD
            UnixError.ENOMEM
            UnixError.EACCES
            UnixError.EFAULT
            UnixError.EBUSY
            UnixError.EEXIST
            UnixError.EXDEV
            UnixError.ENODEV
            UnixError.ENOTDIR
            UnixError.EISDIR
            UnixError.EINVAL
            UnixError.ENFILE
            UnixError.EMFILE
            UnixError.ENOTTY
            UnixError.ETXTBSY
            UnixError.EFBIG
            UnixError.ENOSPC
            UnixError.ESPIPE
            UnixError.EROFS
            UnixError.EMLINK
            UnixError.EPIPE
            UnixError.EDOM
            UnixError.ERANGE
        ]

    let private portable (pal : int) (raw : int) : UnixErrorNumbering =
        {
            Pal = pal
            Raw = RawErrnoPortability.Portable raw
        }

    /// The single table. Both conversion directions and both projections below
    /// are derived from it, so they cannot drift apart.
    ///
    /// PAL values are transcribed from `Interop.Errors.cs` and raw values from
    /// the kernel ABI headers; `TestUnixError` re-derives the PAL column from
    /// the pinned `$DOTNET_RUNTIME_SRC` and fails if this disagrees.
    let numbering (error : UnixError) : UnixErrorNumbering =
        match error with
        | UnixError.EPERM -> portable 0x10042 1
        | UnixError.ENOENT -> portable 0x1002D 2
        | UnixError.ESRCH -> portable 0x1004A 3
        | UnixError.EINTR -> portable 0x1001B 4
        | UnixError.EIO -> portable 0x1001D 5
        | UnixError.ENXIO -> portable 0x1003F 6
        | UnixError.E2BIG -> portable 0x10001 7
        | UnixError.ENOEXEC -> portable 0x1002E 8
        | UnixError.EBADF -> portable 0x10008 9
        | UnixError.ECHILD -> portable 0x1000C 10
        | UnixError.ENOMEM -> portable 0x10031 12
        | UnixError.EACCES -> portable 0x10002 13
        | UnixError.EFAULT -> portable 0x10015 14
        | UnixError.EBUSY -> portable 0x1000A 16
        | UnixError.EEXIST -> portable 0x10014 17
        | UnixError.EXDEV -> portable 0x1004F 18
        | UnixError.ENODEV -> portable 0x1002C 19
        | UnixError.ENOTDIR -> portable 0x10039 20
        | UnixError.EISDIR -> portable 0x1001F 21
        | UnixError.EINVAL -> portable 0x1001C 22
        | UnixError.ENFILE -> portable 0x10029 23
        | UnixError.EMFILE -> portable 0x10021 24
        | UnixError.ENOTTY -> portable 0x1003E 25
        | UnixError.ETXTBSY -> portable 0x1004E 26
        | UnixError.EFBIG -> portable 0x10016 27
        | UnixError.ENOSPC -> portable 0x10034 28
        | UnixError.ESPIPE -> portable 0x10049 29
        | UnixError.EROFS -> portable 0x10048 30
        | UnixError.EMLINK -> portable 0x10022 31
        | UnixError.EPIPE -> portable 0x10043 32
        | UnixError.EDOM -> portable 0x10012 33
        | UnixError.ERANGE -> portable 0x10047 34

    /// The `Interop.Error` value CoreLib switches on. Total: the PAL numbering is
    /// platform-independent, so it is always answerable.
    let toPal (error : UnixError) : int = (numbering error).Pal

    /// The raw `<errno.h>` number to store in `EmulatedKernel.LastSystemError`.
    ///
    /// Partial by design. Every case in the table today is `Portable`, so this
    /// cannot fail as things stand; it fails loudly the moment someone adds a
    /// case whose number PawPrint has not decided on, rather than letting a
    /// Linux number reach a guest that was told it is running on Darwin.
    let toRawErrno (error : UnixError) : int =
        match (numbering error).Raw with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwin) ->
            failwith
                $"UnixError.toRawErrno: %O{error} has no platform-independent errno number (Linux reports %d{linux}, Darwin reports %d{darwin}), and PawPrint has not chosen which numbering its emulated kernel reports. Reporting either would make a guest that read Marshal.GetLastPInvokeError() observe a number its configured SimulatedUnixPlatform contradicts. Decide the numbering (see issue #956) before routing this error to a guest."

    /// The error a raw `<errno.h>` number denotes, or `None` when this build
    /// cannot say. `None` means genuinely undecidable, not merely unmapped: see
    /// `palOfRawErrno`.
    let ofRawErrno (raw : int) : UnixError option =
        all
        |> List.tryFind (fun error ->
            match (numbering error).Raw with
            | RawErrnoPortability.Portable value -> value = raw
            | RawErrnoPortability.PlatformDependent _ -> false
        )

    /// PawPrint's `SystemNative_ConvertErrorPlatformToPal`: raw errno to PAL
    /// `Interop.Error`.
    ///
    /// **Diverges from upstream, deliberately.** `ConvertErrorPlatformToPal` in
    /// `pal_error_common.h` is total, falling back to `Error_ENONSTANDARD` for
    /// anything it does not recognise. PawPrint cannot honestly do that for an
    /// unrecognised value, because "unrecognised" here bundles together two very
    /// different situations that we cannot tell apart without embedding both
    /// platforms' complete errno tables:
    ///
    ///   * numbers that really are nonstandard on both platforms, where
    ///     `ENONSTANDARD` is the right answer; and
    ///   * numbers that name a perfectly ordinary error on each platform but a
    ///     *different* one on each — every value from 35 up, plus 11. Answering
    ///     `ENONSTANDARD` for raw 39 would be silently wrong on Linux, where
    ///     upstream returns `Error_ENOTEMPTY`.
    ///
    /// So this fails instead. A crash naming the value is recoverable; a guest
    /// that quietly took the wrong branch of `if (errorInfo.Error ==
    /// Interop.Error.ENOTEMPTY)` is not.
    let palOfRawErrno (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match ofRawErrno raw with
        | Some error -> toPal error
        | None ->
            failwith
                $"UnixError.palOfRawErrno: cannot convert raw errno %d{raw} to a PAL Interop.Error value. PawPrint only maps the errnos that name the same error on every Unix it models (1-34 except 11); outside that set a raw number is platform-dependent — 39 is ENOTEMPTY on Linux but EDESTADDRREQ on Darwin, and 11 is EAGAIN on Linux but EDEADLK on Darwin. Upstream's ConvertErrorPlatformToPal answers ENONSTANDARD here because it was compiled against one platform's <errno.h> and PawPrint has not chosen one. If a guest legitimately needs this errno, decide the numbering (see issue #956); if it reached here via Marshal.SetLastSystemError, the guest is asserting a platform PawPrint does not model."

    /// PawPrint's `SystemNative_ConvertErrorPalToPlatform`: PAL `Interop.Error`
    /// back to a raw errno.
    ///
    /// Upstream is explicit that this is *not* a round-trip inverse — its own
    /// comment says "we should not use this function to round-trip platform ->
    /// pal -> platform. It's here only to synthesize a platform number from the
    /// fixed set above" — and it returns -1 (after a debug assert) for
    /// `ENONSTANDARD` and for anything unrecognised. We match the -1, because
    /// unlike the raw direction there is no ambiguity to hide: a PAL value we
    /// do not map is one whose raw number we could not state anyway.
    let rawErrnoOfPal (pal : int) : int =
        if pal = palSuccess then
            0
        else

        match all |> List.tryFind (fun error -> toPal error = pal) with
        | Some error -> toRawErrno error
        | None -> -1
