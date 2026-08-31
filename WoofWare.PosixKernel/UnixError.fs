namespace WoofWare.PosixKernel

/// <summary>
/// Describes which platform's errno convention the kernel is using.
/// </summary>
///
/// <remarks>
/// This exists because <c>&lt;errno.h&gt;</c> can be different on different platforms.
/// </remarks>
/// <remarks>
/// Interpreted by <c>SimulatedUnixPlatform.rawErrnoNumbering</c>.
/// </remarks>
[<RequireQualifiedAccess>]
type RawErrnoNumbering =
    | Linux
    | Darwin

/// <summary>
/// How portable a given raw <c>&lt;errno.h&gt;</c> number is, across the modelled Unixes.
/// </summary>
///
/// <remarks>
/// Raw errno numbers are assigned per-platform, and Linux and Darwin agree
/// only on the range they both inherited from V7 Unix.
/// For example, the values 1-34 name the same error on both (except 11, which is <c>EAGAIN</c> on Linux and <c>EDEADLK</c> on Darwin),
/// but everything from 35 up was numbered independently (so e.g. 39 is <c>ENOTEMPTY</c> on Linux and <c>EDESTADDRREQ</c> on Darwin).
/// </remarks>
[<RequireQualifiedAccess>]
type RawErrnoPortability =
    /// <summary>
    /// This error has the same raw number on every Unix we model, so the
    /// emulated kernel can report it without first deciding which platform it is
    /// impersonating.
    /// </summary>
    | Portable of value : int
    /// <summary>
    /// This error's raw number is not consistent among the modelled Unixes,
    /// so it can't be interpreted as an int without knowing what platform is being
    /// simulated.
    /// </summary>
    /// <example>
    /// E.g. <c>ELOOP</c>: raw 40 is <c>ELOOP</c> on Linux but <c>EMSGSIZE</c> on Darwin, and
    /// raw 62 is <c>ELOOP</c> on Darwin but <c>ETIME</c> on Linux.
    /// </example>
    | PlatformDependent of linux : int * darwin : int

/// <summary>
/// The conventional errors a syscall can report.
/// </summary>
/// <remarks>
/// The guest will experience these errors as integers, which on a real host would come from <c>&lt;errno.h&gt;</c>.
/// However, those ints are not portable across the simulated platforms, so we also provide this separate
/// semantic layer.
/// Call <c>UnixError.toRawErrnoUnder</c> to get an integer out directly for a given platform, or
/// <c>UnixError.rawNumbering</c> to get a representation which can be interpreted on any platform.
///
/// This vocabulary was chosen empirically during development of a .NET runtime emulation.
/// It may therefore lack some errors you expect to be there, because I didn't encounter them.
/// Let me know if you find something you need (e.g. <c>ENOTBLK</c> is a known example).
/// </remarks>
[<RequireQualifiedAccess>]
type UnixError =
    /// <summary>
    /// Operation not permitted.
    /// </summary>
    | EPERM
    /// <summary>
    /// No such file or directory.
    /// </summary>
    | ENOENT
    /// <summary>
    /// No such process.
    /// </summary>
    | ESRCH
    /// <summary>
    /// Interrupted function.
    /// </summary>
    | EINTR
    /// <summary>
    /// I/O error.
    /// </summary>
    | EIO
    /// <summary>
    /// No such device or address.
    /// </summary>
    | ENXIO
    /// <summary>
    /// Argument list too long.
    /// </summary>
    | E2BIG
    /// <summary>
    /// Executable file format error.
    /// </summary>
    | ENOEXEC
    /// <summary>Bad file descriptor.</summary>
    /// <example>
    /// Returned by <c>dup</c>, <c>close</c>, <c>read</c>, <c>write</c>, etc. when the supplied
    /// file descriptor is not currently open.
    /// </example>
    | EBADF
    /// <summary>
    /// No child processes.
    /// </summary>
    | ECHILD
    /// <summary>
    /// Not enough space.
    /// </summary>
    | ENOMEM
    /// <summary>
    /// Permission denied.
    /// </summary>
    | EACCES
    /// <summary>
    /// Bad address.
    /// </summary>
    /// <example>
    /// Returned by <c>read(2)</c> / <c>write(2)</c> and friends when the buffer
    /// pointer is outside the process's accessible address space - most
    /// notably when the caller passes <c>NULL</c> (or any other
    /// non-dereferenceable bit pattern) with a non-zero <c>bufferSize</c>.
    /// The kernel performs no I/O for such a call.
    /// </example>
    | EFAULT
    /// <summary>
    /// Device or resource busy.
    /// </summary>
    | EBUSY
    /// <summary>
    /// File exists.
    /// </summary>
    | EEXIST
    /// <summary>
    /// Cross-device link.
    /// </summary>
    | EXDEV
    /// <summary>
    /// No such device.
    /// </summary>
    | ENODEV
    /// <summary>
    /// Not a directory or a symbolic link to a directory.
    /// </summary>
    | ENOTDIR
    /// <summary>
    /// Is a directory.
    /// </summary>
    | EISDIR
    /// <summary>
    /// Invalid argument.
    /// </summary>
    | EINVAL
    /// <summary>
    /// Too many files open in system.
    /// </summary>
    | ENFILE
    /// <summary>
    /// File descriptor value too large.
    /// </summary>
    | EMFILE
    /// <summary>Inappropriate I/O control operation.</summary>
    /// <example>
    /// Returned by <c>isatty(3)</c> when the supplied file descriptor is open but does not
    /// refer to a terminal.
    /// </example>
    | ENOTTY
    /// <summary>
    /// Text file busy.
    /// </summary>
    | ETXTBSY
    /// <summary>
    /// File too large.
    /// </summary>
    | EFBIG
    /// <summary>
    /// No space left on device.
    /// </summary>
    | ENOSPC
    /// <summary>
    /// Invalid seek.
    /// </summary>
    | ESPIPE
    /// <summary>
    /// Read-only file system.
    /// </summary>
    | EROFS
    /// <summary>
    /// Too many links.
    /// </summary>
    | EMLINK
    /// <summary>
    /// Broken pipe.
    /// </summary>
    | EPIPE
    /// <summary>
    /// Mathematics argument out of domain of function.
    /// </summary>
    | EDOM
    /// <summary>
    /// Result too large.
    /// </summary>
    | ERANGE
    /// <summary>
    /// Too many levels of symbolic links.
    /// </summary>
    /// <example>
    /// Reported by <c>VirtualFileSystem.resolve</c> when a path resolution traverses
    /// more symlinks than any of our modelled Unixes would allow.
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 40; Darwin numbers it 62.
    /// </remarks>
    | ELOOP
    /// <summary>
    /// Filename too long.
    /// </summary>
    /// <example>
    /// Reported for a pathname argument longer than the platform's <c>PATH_MAX</c>,
    /// and for any single component longer than its <c>NAME_MAX</c>. Both those limits,
    /// and the unit components are measured in, come from <c>PathLimits</c> and are
    /// filesystem-dependent.
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 36; Darwin numbers it 63.
    /// </remarks>
    | ENAMETOOLONG
    /// <summary>
    /// Directory not empty.
    /// </summary>
    ///
    /// <example>
    /// Reported by <c>rmdir(2)</c> for a directory that still holds an entry,
    /// or for a path whose final component was ".." (see <c>RmDirRules</c>).
    /// </example>
    ///
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 39; Darwin numbers it 66.
    /// </remarks>
    | ENOTEMPTY
    /// <summary>
    /// Resource temporarily unavailable.
    /// </summary>
    /// <remarks>
    /// <c>EWOULDBLOCK</c> is the same value, on all platforms we model, so we
    /// don't supply a separate case for that.
    ///
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 11; Darwin numbers it 35.
    /// </remarks>
    | EAGAIN
    /// <summary>
    /// Value too large to be stored in data type.
    /// </summary>
    /// <example>
    /// <c>lseek</c> returns this on Darwin when the computed file offset does not
    /// fit in a signed 64-bit <c>off_t</c>. Linux returns <c>EINVAL</c> for the same
    /// input.
    /// (An offset which lands below zero is <c>EINVAL</c> on both.)
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 75; Darwin numbers it 84.
    /// </remarks>
    | EOVERFLOW
    /// <summary>
    /// Address family not supported.
    /// </summary>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 97; Darwin numbers it 47.
    /// </remarks>
    | EAFNOSUPPORT
    /// <summary>
    /// Protocol wrong type for socket.
    /// </summary>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 91; Darwin numbers it 41.
    /// </remarks>
    | EPROTOTYPE
    /// <summary>
    /// Protocol not supported.
    /// </summary>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 93; Darwin numbers it 43.
    /// </remarks>
    | EPROTONOSUPPORT
    /// <summary>
    /// Address already in use.
    /// </summary>
    /// <example>
    /// Reported by <c>bind(2)</c> when another socket already holds a conflicting
    /// local address. (Whether two sockets "conflict" depends on both sockets' <c>SO_REUSEADDR</c>,
    /// on whether they're listening, and on the Unix flavour.)
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 98; Darwin numbers it 48.
    /// </remarks>
    | EADDRINUSE
    /// <summary>
    /// Cannot assign requested address.
    /// </summary>
    /// <example>
    /// Reported by <c>bind(2)</c> for an address this machine does not hold.
    ///
    /// What counts as "held" differs per Unix flavour, as follows:
    /// the guest is configured with two lists of addresses, <c>LocalAddresses</c> (which configures interfaces),
    /// and <c>LocalRoutes</c> (the routing table).
    /// Darwin reads only <c>LocalAddresses</c> when binding, while Linux will additionally
    /// bind any address inside a prefix in <c>LocalRoutes</c>.
    ///
    /// So with the defaults - holding <c>127.0.0.1</c>, routing <c>127.0.0.0/8</c> -
    /// <c>127.0.0.9</c> binds on Linux but is <c>EADDRNOTAVAIL</c> on Darwin.
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 99; Darwin numbers it 49.
    /// </remarks>
    | EADDRNOTAVAIL
    /// <summary>
    /// Operation not supported on socket.
    /// </summary>
    /// <example>
    /// Reported by <c>listen(2)</c> on a socket whose type does not accept
    /// connections, such as a datagram socket.
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// </remarks>
    | EOPNOTSUPP
    /// <summary>
    /// Socket operation on non-socket.
    /// </summary>
    /// <example>
    /// For example, <c>accept(2)</c>, <c>bind(2)</c>, <c>listen(2)</c>, and <c>getsockname(2)</c>
    /// when their input isn't a socket but instead is any of
    /// a regular file, a socket event port, or either end of a pipe.
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 88; Darwin numbers it 38.
    /// </remarks>
    | ENOTSOCK
    /// <summary>
    /// Socket is connected.
    /// </summary>
    /// <example>
    /// Reported by <c>connect(2)</c> when a connect is issued on:
    ///
    /// <list type="bullet">
    /// <item>a socket that is already connected</item>
    /// <item>a socket that is listening (on Linux)</item>
    /// <item>a socket which has already completed an asynchronous connect (on Darwin).</item>
    /// </list>
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 106; Darwin numbers it 56.
    /// </remarks>
    | EISCONN
    /// <summary>
    /// Operation now in progress.
    /// </summary>
    /// <example>
    /// Reported by <c>connect(2)</c> when a connect is issued on a non-blocking socket.
    /// (This is true regardless of whether the destination is already listening or has refused the connection,
    /// and is true on loopback too, on all modelled platforms.)
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 115; Darwin numbers it 36.
    /// </remarks>
    | EINPROGRESS
    /// <summary>
    /// Connection refused.
    /// </summary>
    /// <example>
    /// Reported by <c>connect(2)</c> on Linux when another idle stream socket is already bound to
    /// the destination port but is not listening. (Not on Darwin, though; there, the SYN is dropped
    /// and the connect pends towards <c>ETIMEDOUT</c> instead.)
    /// Delivered inline if the connect was blocking, or on the first retry if the connect was non-blocking.
    /// </example>
    /// <remarks>
    /// This is one of the many errnos with an integer value that's not portable.
    /// Linux numbers it 111; Darwin numbers it 61.
    /// </remarks>
    | ECONNREFUSED

[<RequireQualifiedAccess>]
module UnixError =
    /// <summary>
    /// Every case of `UnixError`, exposed for property testing.
    /// </summary>
    let all : UnixError list =
        // TODO: write a Myriad source generator for this, it's ridiculous
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

    let private portable (raw : int) : RawErrnoPortability = RawErrnoPortability.Portable raw

    let private platformDependent (linux : int) (darwin : int) : RawErrnoPortability =
        RawErrnoPortability.PlatformDependent (linux, darwin)

    /// <summary>
    /// Describe the errno corresponding to a <c>UnixError</c>, in a way that is agnostic about the flavour of
    /// guest platform.
    /// </summary>
    /// <remarks>
    /// Use <c>toRawErrnoUnder</c> to get an actual integer.
    /// </remarks>
    let rawNumbering (error : UnixError) : RawErrnoPortability =
        // Transcribed from the kernel ABI headers, and checked against the host's
        // own errno.h by `TestUnixError`.
        match error with
        | UnixError.EPERM -> portable 1
        | UnixError.ENOENT -> portable 2
        | UnixError.ESRCH -> portable 3
        | UnixError.EINTR -> portable 4
        | UnixError.EIO -> portable 5
        | UnixError.ENXIO -> portable 6
        | UnixError.E2BIG -> portable 7
        | UnixError.ENOEXEC -> portable 8
        | UnixError.EBADF -> portable 9
        | UnixError.ECHILD -> portable 10
        | UnixError.ENOMEM -> portable 12
        | UnixError.EACCES -> portable 13
        | UnixError.EFAULT -> portable 14
        | UnixError.EBUSY -> portable 16
        | UnixError.EEXIST -> portable 17
        | UnixError.EXDEV -> portable 18
        | UnixError.ENODEV -> portable 19
        | UnixError.ENOTDIR -> portable 20
        | UnixError.EISDIR -> portable 21
        | UnixError.EINVAL -> portable 22
        | UnixError.ENFILE -> portable 23
        | UnixError.EMFILE -> portable 24
        | UnixError.ENOTTY -> portable 25
        | UnixError.ETXTBSY -> portable 26
        | UnixError.EFBIG -> portable 27
        | UnixError.ENOSPC -> portable 28
        | UnixError.ESPIPE -> portable 29
        | UnixError.EROFS -> portable 30
        | UnixError.EMLINK -> portable 31
        | UnixError.EPIPE -> portable 32
        | UnixError.EDOM -> portable 33
        | UnixError.ERANGE -> portable 34
        // Raw 40 is EMSGSIZE on Darwin, and raw 62 is ETIME on Linux.
        | UnixError.ELOOP -> platformDependent 40 62
        // Likewise: raw 36 is EINPROGRESS on Darwin, and raw 63 is ENOSR on
        // Linux.
        | UnixError.ENAMETOOLONG -> platformDependent 36 63
        // Raw 39 is ENOTEMPTY on Linux and EDESTADDRREQ on
        // Darwin; raw 66 is ENOTEMPTY on Darwin and EREMOTE on Linux.
        | UnixError.ENOTEMPTY -> platformDependent 39 66
        // The V7 transposition itself: raw 11 is EAGAIN on Linux but EDEADLK on
        // Darwin, and raw 35 is EAGAIN on Darwin but EDEADLK on Linux.
        | UnixError.EAGAIN -> platformDependent 11 35
        // Raw 75 is EOVERFLOW on Linux and EPROGMISMATCH on
        // Darwin; raw 84 is EOVERFLOW on Darwin and EILSEQ on Linux.
        | UnixError.EOVERFLOW -> platformDependent 75 84
        // Raw 97 is EAFNOSUPPORT on Linux and ENOLINK on
        // Darwin; raw 47 is EAFNOSUPPORT on Darwin and EL3RST on Linux.
        | UnixError.EAFNOSUPPORT -> platformDependent 97 47
        // Raw 91 is EPROTOTYPE on Linux and ENOMSG on Darwin, while
        // raw 41 is EPROTOTYPE on Darwin and unnamed on Linux.
        | UnixError.EPROTOTYPE -> platformDependent 91 41
        // Raw 93 is EPROTONOSUPPORT on Linux and ENOATTR on Darwin,
        // while raw 43 is EPROTONOSUPPORT on Darwin and EIDRM on Linux.
        | UnixError.EPROTONOSUPPORT -> platformDependent 93 43
        | UnixError.EADDRINUSE -> platformDependent 98 48
        | UnixError.EADDRNOTAVAIL -> platformDependent 99 49
        | UnixError.EOPNOTSUPP -> platformDependent 95 102
        // Raw 88 is ENOTSOCK on Linux and EBADMACHO on
        // Darwin, while raw 38 is ENOTSOCK on Darwin and ENOSYS on Linux.
        | UnixError.ENOTSOCK -> platformDependent 88 38
        | UnixError.EISCONN -> platformDependent 106 56
        | UnixError.EINPROGRESS -> platformDependent 115 36
        | UnixError.ECONNREFUSED -> platformDependent 111 61

    /// <summary>
    /// The raw <c>&lt;errno.h&gt;</c> integer for this error, if that number would be the same across all
    /// supported platforms.
    /// </summary>
    /// <remarks>
    /// You probably don't want to use this, because it throws if the input error is platform-dependent.
    /// <c>toRawErrnoUnder</c> is the safe version.
    /// </remarks>
    let toRawErrno (error : UnixError) : int =
        match rawNumbering error with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwin) ->
            failwith
                $"UnixError.toRawErrno: %O{error} has no platform-independent errno number (Linux reports %d{linux}, Darwin reports %d{darwin}), and PawPrint has not chosen which numbering its emulated kernel reports. Reporting either would make a guest that read Marshal.GetLastPInvokeError() observe a number its configured SimulatedUnixPlatform contradicts. Decide the numbering (see issue #956) before routing this error to a guest."

    /// <summary>
    /// The raw <c>&lt;errno.h&gt;</c> integer for this error on the chosen platform.
    /// </summary>
    let toRawErrnoUnder (reporting : RawErrnoNumbering) (error : UnixError) : int =
        match rawNumbering error with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwin) ->
            match reporting with
            | RawErrnoNumbering.Linux -> linux
            | RawErrnoNumbering.Darwin -> darwin

    /// <summary>
    /// Whether the integer is an errno which is portable across all WoofWare.PosixKernel's modelled Unixes.
    /// </summary>
    /// <example>
    /// The number 5 is <c>EIO</c> on all modelled platforms, so is portable.
    ///
    /// The number 11 is <c>EAGAIN</c> on Linux but <c>EDEADLK</c> on Darwin, so is not portable.
    /// </example>
    /// <remarks>
    /// This only indicates errnos which we <i>know</i> to be portable.
    /// There may coincidentally be portable errnos which <c>isPortableRawErrno</c> says are not portable.
    /// </remarks>
    let isPortableRawErrno (raw : int) : bool = raw >= 1 && raw <= 34 && raw <> 11

    /// <summary>
    /// A fast-path to detect whether the integer is known not to be a valid errno on any modelled Unix.
    /// </summary>
    /// <example>
    /// POSIX specifies that errnos are positive, so any negative int is unambiguously a nonstandard raw errno.
    /// </example>
    let isUnambiguouslyNonStandardRawErrno (raw : int) : bool = raw < 0

    /// <summary>
    /// Convert a portable raw integer errno to a structured error.
    /// </summary>
    /// <remarks>
    /// This returns <c>None</c> for non-portable errnos, as well as for errnos WoofWare.PosixKernel
    /// simply doesn't recognise.
    ///
    /// Use <c>ofRawErrnoUnder</c> to convert an errno if you already know the platform you're simulating.
    /// </remarks>
    let ofRawErrno (raw : int) : UnixError option =
        all
        |> List.tryFind (fun error ->
            match rawNumbering error with
            | RawErrnoPortability.Portable value -> value = raw
            | RawErrnoPortability.PlatformDependent _ -> false
        )

    /// <summary>
    /// Convert a raw integer errno to a structured error on the specified platform.
    /// </summary>
    /// <remarks>
    /// Unlike <c>ofRawErrno</c>, this returns values for platform-dependent entries too.
    /// It can still return <c>None</c> for errnos that are valid but which WoofWare.PosixKernel doesn't yet model.
    /// </remarks>
    let ofRawErrnoUnder (reporting : RawErrnoNumbering) (raw : int) : UnixError option =
        all |> List.tryFind (fun error -> toRawErrnoUnder reporting error = raw)
