namespace WoofWare.PawPrint

/// Whose `<errno.h>` numbering a run reports, for the errors where the Unixes
/// PawPrint models disagree.
///
/// Not `SimulatedUnixPlatform` itself: that lives in `EmulatedKernel`, which
/// compiles later. `SimulatedUnixPlatform.rawErrnoNumbering` maps platform to
/// numbering, and is total: every simulated platform states which Unix it is.
[<RequireQualifiedAccess>]
type RawErrnoNumbering =
    | Linux
    | Darwin

/// How portable a raw `<errno.h>` number is across the Unixes PawPrint models.
///
/// Raw errno *numbers* are assigned per-platform, and Linux and Darwin agree
/// only on the range they both inherited from V7 Unix: values 1-34 name the
/// same error on both, with exactly one exception, 11, where the two are
/// transposed (`EAGAIN` is 11 on Linux and 35 on Darwin; `EDEADLK` is 35 on
/// Linux and 11 on Darwin). Everything from 35 up was numbered independently,
/// so e.g. 39 is `ENOTEMPTY` on Linux and `EDESTADDRREQ` on Darwin.
///
/// A caller that knows which numbering its kernel reports gets an answer for
/// both classes (`toRawErrnoUnder`); one that does not is answered only for the
/// portable class, and told so loudly for the rest.
[<RequireQualifiedAccess>]
type RawErrnoPortability =
    /// This error has the same raw number on every Unix PawPrint models, so the
    /// emulated kernel can report it without first deciding which platform it is
    /// impersonating.
    | Portable of value : int
    /// This error's raw number differs between the Unixes PawPrint models, so
    /// it can only be reported once something says which one is being
    /// impersonated. Carries both candidates so the choice is a table lookup.
    ///
    /// E.g. `ELOOP`: raw 40 is `ELOOP` on Linux but `EMSGSIZE` on Darwin, and
    /// raw 62 is `ELOOP` on Darwin but `ETIME` on Linux, so either choice would
    /// silently rename a different error on the other platform. A case landing
    /// here keeps its PAL value (platform-independent, and what CoreLib
    /// actually switches on); its raw number is answerable only through
    /// `toRawErrnoUnder`, and `toRawErrno` refuses.
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
/// A case earns its place by being an error PawPrint's own emulation actually
/// raises, *and* by having a name in the BCL's PAL enum. `ENOTBLK` (raw 15) is
/// excluded despite being portable, because `Interop.Error` has no entry for
/// it, so there would be nothing for CoreLib to switch on.
///
/// Portability of the *raw* number is not a membership criterion — it is
/// recorded per case, in `RawErrnoPortability`.
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
    /// Used by PawPrint's `SystemNative_Write` shim when the caller supplies a
    /// negative `bufferSize`, matching `Common_Write` in `pal_io_common.h`,
    /// which sets `errno = ERANGE` and returns -1 before the real `write(2)` is
    /// invoked.
    ///
    /// **Not the answer for the reading half.** `Common_Read`, immediately
    /// above it in the same header, guards the same mistake with
    /// `errno = EINVAL` — so `SystemNative_Read` reports EINVAL where
    /// `SystemNative_Write` reports ERANGE. The asymmetry is upstream's.
    | ERANGE
    /// `ELOOP` — Too many levels of symbolic links.
    ///
    /// Reported by `VirtualFileSystem.resolve` when a path resolution traverses
    /// more symlinks than any Unix PawPrint models would allow. The raw number
    /// is not portable: Linux numbers it 40 and Darwin 62, and each of those
    /// numbers names a different error on the other platform. Its PAL value is
    /// unaffected by that, and is what CoreLib switches on, so this is fully
    /// usable everywhere except `toRawErrno` — which fails loudly rather than
    /// picking a platform. See `RawErrnoPortability`.
    | ELOOP
    /// `ENAMETOOLONG` — Filename too long.
    ///
    /// Reported for a pathname argument longer than the platform's `PATH_MAX`,
    /// and for any single component longer than its `NAME_MAX`. Both limits, and
    /// the *unit* the second is measured in, come from `PathLimits`: measured,
    /// APFS permits 255 UTF-16 code units where ext4 permits 255 bytes, so the
    /// same name can be legal on one and too long on the other.
    ///
    /// Platform-dependent like `ELOOP`, and for the same reason: raw 36 is
    /// `ENAMETOOLONG` on Linux but `EINPROGRESS` on Darwin, and raw 63 is
    /// `ENAMETOOLONG` on Darwin but `ENOSR` on Linux (both checked against the
    /// two `<errno.h>`s), so either choice would rename a different error on the
    /// other platform.
    | ENAMETOOLONG
    /// `ENOTEMPTY` — Directory not empty.
    ///
    /// Reported by `rmdir(2)` for a directory that still holds an entry, and —
    /// on both flavours — for a path whose final component was "..", which
    /// names a directory that necessarily contains the one the path came
    /// through. See `RmDirRules`.
    ///
    /// Platform-dependent like `ELOOP`: raw 39 is `ENOTEMPTY` on Linux but
    /// `EDESTADDRREQ` on Darwin, and raw 66 is `ENOTEMPTY` on Darwin but
    /// `EREMOTE` on Linux (measured on both).
    | ENOTEMPTY
    /// `EAGAIN` — Resource temporarily unavailable. `EWOULDBLOCK` is the *same*
    /// value, on both platforms and in the PAL enum
    /// (`Interop.Errors.cs:111` defines `EWOULDBLOCK = EAGAIN`), so there is one
    /// case here rather than two.
    ///
    /// Reported by `SystemNative_FLock` when a non-blocking lock request
    /// conflicts with a lock another open file description already holds. This
    /// is the one errno the BCL's `FileStream` treats as meaningful — every
    /// other failure to lock is swallowed, since the lock is advisory
    /// (`SafeFileHandle.Unix.cs:359`) — so it is what makes `FileShare` do
    /// anything at all on Unix.
    ///
    /// Platform-dependent: Linux numbers this 11 and Darwin 35, which are
    /// exactly the two numbers Darwin and Linux respectively give `EDEADLK`.
    /// Measured on both platforms rather than read off a header.
    | EAGAIN
    /// `EOVERFLOW` — Value too large to be stored in data type.
    ///
    /// Reported by `SystemNative_LSeek` under the Darwin flavour when the
    /// computed file offset does not fit in a signed 64-bit `off_t`. Linux
    /// answers `EINVAL` for the same input, which is the entire divergence
    /// between the two once the *filesystem* is held constant — measured on a
    /// tmpfs-backed file, since ext4's much lower `s_maxbytes` otherwise makes
    /// Linux look as though it rejects large offsets outright.
    ///
    /// Platform-dependent like `ELOOP`: raw 75 is `EOVERFLOW` on Linux but
    /// `EPROGMISMATCH` on Darwin, and raw 84 is `EOVERFLOW` on Darwin but
    /// `EILSEQ` on Linux (both read off the two `strerror` tables).
    | EOVERFLOW
    /// `EAFNOSUPPORT` — Address family not supported.
    ///
    /// Reported by `SystemNative_Socket` when the requested address family is
    /// not one the native shim knows how to translate. That screen is the C's
    /// own, ahead of any syscall, so this is the shim's answer rather than a
    /// kernel's.
    ///
    /// Platform-dependent: Linux numbers this 97 and Darwin 47, and neither
    /// number is free on the other platform — raw 97 is `ENOLINK` on Darwin and
    /// raw 47 is `EL3RST` on Linux. Measured from both `strerror` tables.
    | EAFNOSUPPORT
    /// `EPROTOTYPE` — Protocol wrong type for socket.
    ///
    /// Reported by `SystemNative_Socket` when the requested socket type is not
    /// one the native shim knows how to translate. Note that the C reports the
    /// *socket type* screen with this rather than with `ESOCKTNOSUPPORT`, which
    /// is what a kernel would say; that mismatch is upstream's, not PawPrint's.
    ///
    /// Platform-dependent: Linux numbers this 91 and Darwin 41. Raw 91 is
    /// `ENOMSG` on Darwin, and raw 41 has no name at all on Linux — `strerror`
    /// reports "Unknown error 41" — so neither number is safe to reuse.
    | EPROTOTYPE
    /// `EPROTONOSUPPORT` — Protocol not supported.
    ///
    /// Reported by `SystemNative_Socket` when the requested protocol is not one
    /// the native shim knows how to translate *for the requested address
    /// family*: the C's protocol conversion is a per-family table, so the same
    /// protocol can convert under `AF_INET` and be refused under `AF_UNIX`.
    ///
    /// Platform-dependent: Linux numbers this 93 and Darwin 43, and neither
    /// number is free on the other platform — raw 93 is `ENOATTR` on Darwin and
    /// raw 43 is `EIDRM` on Linux.
    | EPROTONOSUPPORT
    /// `EADDRINUSE` — Address already in use.
    ///
    /// Reported by `bind(2)` when another socket already holds a conflicting
    /// local address. Which pairs conflict is not a property of the address
    /// alone: it depends on both sockets' `SO_REUSEADDR`, on whether either is
    /// listening, and on the flavour, which relax the rule in opposite
    /// directions. See `SimulatedUnixPlatform.bindConflict`.
    ///
    /// Platform-dependent, and neither number is free on the other platform.
    /// Measured from both `strerror` tables: Linux numbers this 98, where Darwin
    /// reads 98 as "No STREAM resources"; Darwin numbers it 48, where Linux reads
    /// 48 as "Link number out of range".
    | EADDRINUSE
    /// `EADDRNOTAVAIL` — Cannot assign requested address.
    ///
    /// Reported by `bind(2)` for an address no local interface holds. What
    /// counts as local is a per-flavour rule over the configured prefixes: Linux
    /// accepts anything inside a local prefix (so the whole of `127.0.0.0/8`),
    /// Darwin only an address a prefix *is*.
    ///
    /// Platform-dependent, and neither number is free on the other platform.
    /// Measured: Linux numbers this 99, where Darwin reads 99 as "Not a STREAM";
    /// Darwin numbers it 49, where Linux reads 49 as "Protocol driver not
    /// attached".
    | EADDRNOTAVAIL
    /// `EOPNOTSUPP` — Operation not supported on socket.
    ///
    /// Reported by `listen(2)` on a socket whose type does not accept
    /// connections — a datagram socket, measured on both.
    ///
    /// The PAL folds this together with `ENOTSUP` (`Error_EOPNOTSUPP =
    /// Error_ENOTSUP`), so CoreLib's managed `Interop.Error` carries this name
    /// only as an alias and both conditions present the same value to a guest
    /// switching on it. The *raw* errnos are not folded, and that is measured:
    /// `listen(2)` on a datagram socket sets 95 on Linux and **102** on Darwin,
    /// while `flock` on a socket sets Darwin's `ENOTSUP`, 45. This case carries
    /// `EOPNOTSUPP`'s numbering; a caller meaning `ENOTSUP` needs its own case
    /// rather than this one, or a Darwin guest reading `errno` sees 102 where the
    /// kernel set 45.
    ///
    /// Neither number is free on the other platform: Darwin reads 95 as
    /// "EMULTIHOP (Reserved)" and Linux reads 102 as "Network dropped connection
    /// on reset".
    | EOPNOTSUPP
    /// `ENOTSOCK` — Socket operation on non-socket.
    ///
    /// Reported by the socket syscalls for a descriptor that is not a socket.
    /// Measured per entry point rather than generalised: `accept(2)`,
    /// `bind(2)`, `listen(2)` and `getsockname(2)` on a regular file, a socket
    /// event port and both ends of a pipe all answer this, on both kernels.
    ///
    /// Platform-dependent, and neither number is free on the other platform:
    /// Linux numbers this 88, which Darwin reads as `EBADMACHO`; Darwin numbers
    /// it 38, which Linux reads as `ENOSYS`.
    | ENOTSOCK
    /// `EISCONN` — Socket is connected.
    ///
    /// Reported by `connect(2)` on a socket that already is: measured on both
    /// kernels after a blocking loopback connect. Also Linux's answer for a
    /// connect issued on a *listening* socket, and Darwin's for a retry after
    /// an async establishment.
    ///
    /// Platform-dependent, and neither number is free on the other platform:
    /// Linux numbers this 106, which Darwin reads as unnamed; Darwin numbers
    /// it 56, which Linux reads as `EBADRQC`.
    | EISCONN
    /// `EINPROGRESS` — Operation now in progress.
    ///
    /// `connect(2)`'s answer on a non-blocking socket — measured on both
    /// kernels, *even on loopback*, and whether the destination is listening
    /// or refuses. The PAL's managed callers switch on it to pend
    /// (`SocketPal.TryStartConnect`), so answering the final outcome instead
    /// would change guest control flow.
    ///
    /// Platform-dependent: Linux numbers this 115, which Darwin has no name
    /// for; Darwin numbers it 36, which Linux reads as `ENAMETOOLONG`.
    | EINPROGRESS
    /// `ECONNREFUSED` — Connection refused.
    ///
    /// `connect(2)`'s answer for a local destination with no listening socket
    /// behind it, delivered inline by a blocking connect and by the first
    /// retry after a non-blocking one. Measured on both kernels for a closed
    /// port; on Linux a bound-but-not-listening port answers the same (the
    /// SYN gets RST either way), where Darwin drops the SYN instead.
    ///
    /// Platform-dependent, and neither number is free on the other platform:
    /// Linux numbers this 111, which Darwin reads as unnamed; Darwin numbers
    /// it 61, which Linux reads as `ENODATA`.
    | ECONNREFUSED

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
    /// case: "no error" is not an error.
    [<Literal>]
    let palSuccess : int = 0

    /// `Interop.Error.ENONSTANDARD`, which upstream's
    /// `ConvertErrorPlatformToPal` returns for any errno it does not recognise.
    /// PawPrint returns it only where "unrecognised" is platform-independent —
    /// a raw errno inside the portable range that the PAL enum has no name for,
    /// today only `ENOTBLK`. Where "unrecognised" instead means "means
    /// different things on different Unixes", `palOfRawErrno` fails rather than
    /// answering this; see there.
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
            UnixError.ELOOP
            UnixError.ENAMETOOLONG
            UnixError.ENOTEMPTY
            UnixError.EAGAIN
            UnixError.EOVERFLOW
            UnixError.EAFNOSUPPORT
            UnixError.EPROTOTYPE
            UnixError.EPROTONOSUPPORT
            UnixError.EADDRINUSE
            UnixError.EADDRNOTAVAIL
            UnixError.EOPNOTSUPP
            UnixError.ENOTSOCK
            UnixError.EISCONN
            UnixError.EINPROGRESS
            UnixError.ECONNREFUSED
        ]

    let private portable (pal : int) (raw : int) : UnixErrorNumbering =
        {
            Pal = pal
            Raw = RawErrnoPortability.Portable raw
        }

    let private platformDependent (pal : int) (linux : int) (darwin : int) : UnixErrorNumbering =
        {
            Pal = pal
            Raw = RawErrnoPortability.PlatformDependent (linux, darwin)
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
        // Raw 40 is EMSGSIZE on Darwin, and raw 62 is ETIME on Linux, so either
        // number would silently name a different error on the other platform.
        | UnixError.ELOOP -> platformDependent 0x10020 40 62
        // Likewise: raw 36 is EINPROGRESS on Darwin, and raw 63 is ENOSR on
        // Linux.
        | UnixError.ENAMETOOLONG -> platformDependent 0x10025 36 63
        // Measured on both: raw 39 is ENOTEMPTY on Linux and EDESTADDRREQ on
        // Darwin; raw 66 is ENOTEMPTY on Darwin and EREMOTE on Linux.
        | UnixError.ENOTEMPTY -> platformDependent 0x1003A 39 66
        // The V7 transposition itself: raw 11 is EAGAIN on Linux but EDEADLK on
        // Darwin, and raw 35 is EAGAIN on Darwin but EDEADLK on Linux.
        | UnixError.EAGAIN -> platformDependent 0x10006 11 35
        // Measured on both: raw 75 is EOVERFLOW on Linux and EPROGMISMATCH on
        // Darwin; raw 84 is EOVERFLOW on Darwin and EILSEQ on Linux.
        | UnixError.EOVERFLOW -> platformDependent 0x10040 75 84
        // Measured on both: raw 97 is EAFNOSUPPORT on Linux and ENOLINK on
        // Darwin; raw 47 is EAFNOSUPPORT on Darwin and EL3RST on Linux.
        | UnixError.EAFNOSUPPORT -> platformDependent 0x10005 97 47
        // Likewise: raw 91 is EPROTOTYPE on Linux and ENOMSG on Darwin, while
        // raw 41 is EPROTOTYPE on Darwin and unnamed on Linux.
        | UnixError.EPROTOTYPE -> platformDependent 0x10046 91 41
        // Likewise: raw 93 is EPROTONOSUPPORT on Linux and ENOATTR on Darwin,
        // while raw 43 is EPROTONOSUPPORT on Darwin and EIDRM on Linux.
        | UnixError.EPROTONOSUPPORT -> platformDependent 0x10045 93 43
        | UnixError.EADDRINUSE -> platformDependent 0x10003 98 48
        | UnixError.EADDRNOTAVAIL -> platformDependent 0x10004 99 49
        | UnixError.EOPNOTSUPP -> platformDependent 0x1003D 95 102
        // Measured on both: raw 88 is ENOTSOCK on Linux and EBADMACHO on
        // Darwin, while raw 38 is ENOTSOCK on Darwin and ENOSYS on Linux.
        | UnixError.ENOTSOCK -> platformDependent 0x1003C 88 38
        | UnixError.EISCONN -> platformDependent 0x1001E 106 56
        | UnixError.EINPROGRESS -> platformDependent 0x1001A 115 36
        | UnixError.ECONNREFUSED -> platformDependent 0x1000E 111 61

    /// The `Interop.Error` value CoreLib switches on. Total: the PAL numbering is
    /// platform-independent, so it is always answerable.
    let toPal (error : UnixError) : int = (numbering error).Pal

    /// The raw `<errno.h>` number to store in `EmulatedKernel.LastSystemError`.
    ///
    /// Partial: a `PlatformDependent` case (e.g. `ELOOP`) has no
    /// platform-independent number, so this fails for it rather than letting a
    /// Linux number reach a guest that was told it is running on Darwin.
    /// Callers that only need the value CoreLib switches on should use `toPal`,
    /// which is total.
    let toRawErrno (error : UnixError) : int =
        match (numbering error).Raw with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwin) ->
            failwith
                $"UnixError.toRawErrno: %O{error} has no platform-independent errno number (Linux reports %d{linux}, Darwin reports %d{darwin}), and PawPrint has not chosen which numbering its emulated kernel reports. Reporting either would make a guest that read Marshal.GetLastPInvokeError() observe a number its configured SimulatedUnixPlatform contradicts. Decide the numbering (see issue #956) before routing this error to a guest."

    /// The raw `<errno.h>` number under the chosen numbering. Unlike
    /// `toRawErrno`, this is total on the platform-dependent errors too,
    /// because the caller has said which Unix it impersonates.
    let toRawErrnoUnder (reporting : RawErrnoNumbering) (error : UnixError) : int =
        match (numbering error).Raw with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwin) ->
            match reporting with
            | RawErrnoNumbering.Linux -> linux
            | RawErrnoNumbering.Darwin -> darwin

    /// Is `raw` a number whose meaning PawPrint can state without first
    /// deciding which Unix it is impersonating?
    ///
    /// Every value in 1-34 is defined on both Linux and Darwin, and they agree
    /// on all of them except 11 (`EAGAIN` on Linux, `EDEADLK` on Darwin, and
    /// vice versa at 35). From 35 up the two numbered independently, so nothing
    /// there is answerable.
    ///
    /// Conservative at the top end: a number above *both* platforms' highest
    /// errno is in fact unambiguously nonstandard, but saying so would mean
    /// embedding both platforms' maxima, so such a value fails loudly instead.
    ///
    /// The bottom end does not fail: POSIX requires errno values to be
    /// positive, so no platform defines a negative one and both fall through to
    /// `ENONSTANDARD`, with no per-platform table needed. See
    /// `isUnambiguouslyNonStandardRawErrno`.
    let private isPortableRawErrno (raw : int) : bool = raw >= 1 && raw <= 34 && raw <> 11

    /// Raw values every Unix agrees are meaningless, and hence agrees convert to
    /// `ENONSTANDARD`. POSIX errnos are positive, so a negative number names an
    /// error on no platform we model and needs no platform choice to reject.
    let private isUnambiguouslyNonStandardRawErrno (raw : int) : bool = raw < 0

    /// The error a raw `<errno.h>` number denotes, or `None` when this build
    /// cannot say. `None` means undecidable, not merely unmapped: see
    /// `palOfRawErrno`.
    ///
    /// A `PlatformDependent` case is *not* matched, even though the table
    /// records both its candidate numbers: matching raw 40 to `ELOOP` would be
    /// right on Linux and wrong on Darwin, where 40 is `EMSGSIZE`, and matching
    /// 62 would be wrong the other way round. Answering "which error is this
    /// number" requires the platform choice PawPrint has not made, so both
    /// numbers stay unmapped and reach `palOfRawErrno`'s loud failure.
    let ofRawErrno (raw : int) : UnixError option =
        all
        |> List.tryFind (fun error ->
            match (numbering error).Raw with
            | RawErrnoPortability.Portable value -> value = raw
            | RawErrnoPortability.PlatformDependent _ -> false
        )

    /// The error a raw number denotes under a chosen numbering. Unlike
    /// `ofRawErrno`, this *can* match a platform-dependent entry — when the
    /// caller has supplied the fact that made it ambiguous.
    let ofRawErrnoUnder (reporting : RawErrnoNumbering) (raw : int) : UnixError option =
        all |> List.tryFind (fun error -> toRawErrnoUnder reporting error = raw)

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
    ///     PawPrint has chosen no platform, so it cannot. Answering
    ///     `ENONSTANDARD` for raw 39 would be silently wrong on Linux, where
    ///     upstream returns `Error_ENOTEMPTY`, so this fails loudly instead.
    ///
    /// Only the last case diverges from upstream; the others answer exactly what
    /// the C does.
    let palOfRawErrno (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match ofRawErrno raw with
        | Some error -> toPal error
        | None when isPortableRawErrno raw ->
            // ENOTBLK, today the only member of this class.
            palNonStandard
        | None when isUnambiguouslyNonStandardRawErrno raw -> palNonStandard
        | None ->

        failwith
            $"UnixError.palOfRawErrno: cannot convert raw errno %d{raw} to a PAL Interop.Error value. PawPrint only maps the errnos that name the same error on every Unix it models (1-34 except 11); outside that set a raw number is platform-dependent — 39 is ENOTEMPTY on Linux but EDESTADDRREQ on Darwin, and 11 is EAGAIN on Linux but EDEADLK on Darwin. Upstream's ConvertErrorPlatformToPal answers ENONSTANDARD here because it was compiled against one platform's <errno.h> and PawPrint has not chosen one. If a guest legitimately needs this errno, decide the numbering (see issue #956); if it reached here via Marshal.SetLastSystemError, the guest is asserting a platform PawPrint does not model."

    /// `palOfRawErrno` for a caller that knows which numbering the kernel
    /// reports — which is every caller inside the emulated kernel.
    ///
    /// A table entry whose raw number is platform-dependent becomes matchable,
    /// so raw 40 answers `ELOOP` under Linux and raw 39 answers `ENOTEMPTY`. A
    /// number the table does not contain at all still fails loudly rather than
    /// falling through to `ENONSTANDARD`: raw 35 under Linux really is
    /// `EDEADLK`, which PawPrint has not modelled, and `ENONSTANDARD` would
    /// silently take a guest down the wrong branch of an
    /// `if (errorInfo.Error == Interop.Error.EDEADLK)`. Knowing the platform
    /// does not conjure a table entry.
    let palOfRawErrnoUnder (reporting : RawErrnoNumbering) (raw : int) : int =
        if raw = 0 then
            palSuccess
        else

        match ofRawErrnoUnder reporting raw with
        | Some error -> toPal error
        | None when isPortableRawErrno raw -> palNonStandard
        | None when isUnambiguouslyNonStandardRawErrno raw -> palNonStandard
        | None ->
            failwith
                $"UnixError.palOfRawErrnoUnder: cannot convert raw errno %d{raw} to a PAL Interop.Error value under the %O{reporting} numbering. The number is outside the portable set (1-34 except 11) and PawPrint's table has no entry for it on this platform — raw 35 under Linux is EDEADLK, which is not yet modelled. Answering ENONSTANDARD would silently take a guest down the wrong branch of an `if (errorInfo.Error == Interop.Error.EDEADLK)`, so this fails instead. Add the error to UnixError's table (with RawErrnoPortability.PlatformDependent if the two Unixes disagree); if it reached here via Marshal.SetLastSystemError, the guest is asserting an errno PawPrint does not model."
