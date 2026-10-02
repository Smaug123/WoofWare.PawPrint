namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The C library a process's native code runs against: whose `strerror_r` the
/// System.Native shim calls, and so whose words an errno-built exception
/// message is in.
///
/// Decided by the simulated platform's flavour (`ofPlatform`): a Linux process
/// gets glibc, which is what the linux-x64 runtime pack's shim is built
/// against, and a Darwin process Darwin's own libc. No other pairing is
/// modelled. A Linux process on musl (the linux-musl runtime packs) would make
/// this a choice of its own, alongside the platform rather than derived from
/// it, and the glibc facts PawPrint keys on the Linux flavour elsewhere (its
/// reserved signals 32 and 33, `StartupSignalDispositions`) would move onto it
/// with it.
[<RequireQualifiedAccess>]
type CLibrary =
    /// GNU libc. Its `strerror_r` is the GNU one, which returns a string of its
    /// own for an error it names and writes into the caller's buffer only for
    /// a number it does not.
    | Glibc
    /// Darwin's libc (libsystem_c). Its `strerror_r` is the XSI one, which
    /// always writes into the caller's buffer and reports ERANGE when the text
    /// did not fit.
    | DarwinLibc

[<RequireQualifiedAccess>]
module CLibrary =

    /// The C library a process on `platform` runs against.
    let ofPlatform (platform : SimulatedUnixPlatform) : CLibrary =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> CLibrary.Glibc
        | SimulatedUnixFlavour.Darwin -> CLibrary.DarwinLibc

    /// The `<errno.h>` numbering the library is built against.
    let errnoNumbering (library : CLibrary) : RawErrnoNumbering =
        match library with
        | CLibrary.Glibc -> RawErrnoNumbering.Linux
        | CLibrary.DarwinLibc -> RawErrnoNumbering.Darwin

    // The texts below are each library's `strerror_r` in the "C" locale, which
    // is the only one a .NET process runs it in: the runtime never calls
    // `setlocale`, so LANG and LC_ALL change nothing (measured,
    // docs/plans/2026-08-23-posix-kernel-extraction/strerror-r-locale.cs).
    // Transcribed from the measured output beside strerror-r.c, which
    // `TestStrErrorR` holds every row of them to: glibc 2.41 (and 2.39, the
    // same bytes) and Darwin 27.0.

    /// `strerror(0)`.
    let successText (library : CLibrary) : string =
        match library with
        | CLibrary.Glibc -> "Success"
        | CLibrary.DarwinLibc -> "Undefined error: 0"

    /// `gai_strerror(EAI_NONAME)`.
    let nameNotKnownText (library : CLibrary) : string =
        match library with
        | CLibrary.Glibc -> "Name or service not known"
        | CLibrary.DarwinLibc -> "nodename nor servname provided, or not known"

    /// `strerror_r`'s text for a number the library names no error for.
    let unknownErrorText (library : CLibrary) (number : int) : string =
        match library with
        | CLibrary.Glibc -> $"Unknown error %d{number}"
        | CLibrary.DarwinLibc -> $"Unknown error: %d{number}"

    let private glibcErrorText (error : UnixError) : string option =
        match error with
        | UnixError.EPERM -> Some "Operation not permitted"
        | UnixError.ENOENT -> Some "No such file or directory"
        | UnixError.ESRCH -> Some "No such process"
        | UnixError.EINTR -> Some "Interrupted system call"
        | UnixError.EIO -> Some "Input/output error"
        | UnixError.ENXIO -> Some "No such device or address"
        | UnixError.E2BIG -> Some "Argument list too long"
        | UnixError.ENOEXEC -> Some "Exec format error"
        | UnixError.EBADF -> Some "Bad file descriptor"
        | UnixError.ECHILD -> Some "No child processes"
        | UnixError.ENOMEM -> Some "Cannot allocate memory"
        | UnixError.EACCES -> Some "Permission denied"
        | UnixError.EFAULT -> Some "Bad address"
        | UnixError.EBUSY -> Some "Device or resource busy"
        | UnixError.EEXIST -> Some "File exists"
        | UnixError.EXDEV -> Some "Invalid cross-device link"
        | UnixError.ENODEV -> Some "No such device"
        | UnixError.ENOTDIR -> Some "Not a directory"
        | UnixError.EISDIR -> Some "Is a directory"
        | UnixError.EINVAL -> Some "Invalid argument"
        | UnixError.ENFILE -> Some "Too many open files in system"
        | UnixError.EMFILE -> Some "Too many open files"
        | UnixError.ENOTTY -> Some "Inappropriate ioctl for device"
        | UnixError.ETXTBSY -> Some "Text file busy"
        | UnixError.EFBIG -> Some "File too large"
        | UnixError.ENOSPC -> Some "No space left on device"
        | UnixError.ESPIPE -> Some "Illegal seek"
        | UnixError.EROFS -> Some "Read-only file system"
        | UnixError.EMLINK -> Some "Too many links"
        | UnixError.EPIPE -> Some "Broken pipe"
        | UnixError.EDOM -> Some "Numerical argument out of domain"
        | UnixError.ERANGE -> Some "Numerical result out of range"
        | UnixError.ELOOP -> Some "Too many levels of symbolic links"
        | UnixError.ENAMETOOLONG -> Some "File name too long"
        | UnixError.ENOTEMPTY -> Some "Directory not empty"
        | UnixError.EAGAIN -> Some "Resource temporarily unavailable"
        | UnixError.EOVERFLOW -> Some "Value too large for defined data type"
        | UnixError.EILSEQ -> Some "Invalid or incomplete multibyte or wide character"
        | UnixError.EAFNOSUPPORT -> Some "Address family not supported by protocol"
        | UnixError.EPROTOTYPE -> Some "Protocol wrong type for socket"
        | UnixError.EPROTONOSUPPORT -> Some "Protocol not supported"
        | UnixError.EADDRINUSE -> Some "Address already in use"
        | UnixError.EADDRNOTAVAIL -> Some "Cannot assign requested address"
        | UnixError.EOPNOTSUPP -> Some "Operation not supported"
        | UnixError.ENOTSOCK -> Some "Socket operation on non-socket"
        | UnixError.EISCONN -> Some "Transport endpoint is already connected"
        | UnixError.EINPROGRESS -> Some "Operation now in progress"
        | UnixError.ECONNREFUSED -> Some "Connection refused"
        | UnixError.ENOTSUP -> Some "Operation not supported"
        | UnixError.ENOTCONN -> Some "Transport endpoint is not connected"
        | UnixError.ETIMEDOUT -> Some "Connection timed out"
        | UnixError.ECONNRESET -> Some "Connection reset by peer"
        | UnixError.EMSGSIZE -> Some "Message too long"
        | UnixError.ENOSYS -> Some "Function not implemented"
        | UnixError.EDEADLK -> Some "Resource deadlock avoided"
        | UnixError.ENOLCK -> Some "No locks available"
        | UnixError.ENOTBLK -> Some "Block device required"
        | UnixError.ENOMSG -> Some "No message of desired type"
        | UnixError.EIDRM -> Some "Identifier removed"
        | UnixError.ENOSTR -> Some "Device not a stream"
        | UnixError.ENODATA -> Some "No data available"
        | UnixError.ETIME -> Some "Timer expired"
        | UnixError.ENOSR -> Some "Out of streams resources"
        | UnixError.EREMOTE -> Some "Object is remote"
        | UnixError.ENOLINK -> Some "Link has been severed"
        | UnixError.EPROTO -> Some "Protocol error"
        | UnixError.EMULTIHOP -> Some "Multihop attempted"
        | UnixError.EBADMSG -> Some "Bad message"
        | UnixError.EUSERS -> Some "Too many users"
        | UnixError.EDESTADDRREQ -> Some "Destination address required"
        | UnixError.ENOPROTOOPT -> Some "Protocol not available"
        | UnixError.ESOCKTNOSUPPORT -> Some "Socket type not supported"
        | UnixError.EPFNOSUPPORT -> Some "Protocol family not supported"
        | UnixError.ENETDOWN -> Some "Network is down"
        | UnixError.ENETUNREACH -> Some "Network is unreachable"
        | UnixError.ENETRESET -> Some "Network dropped connection on reset"
        | UnixError.ECONNABORTED -> Some "Software caused connection abort"
        | UnixError.ENOBUFS -> Some "No buffer space available"
        | UnixError.ESHUTDOWN -> Some "Cannot send after transport endpoint shutdown"
        | UnixError.ETOOMANYREFS -> Some "Too many references: cannot splice"
        | UnixError.EHOSTDOWN -> Some "Host is down"
        | UnixError.EHOSTUNREACH -> Some "No route to host"
        | UnixError.EALREADY -> Some "Operation already in progress"
        | UnixError.ESTALE -> Some "Stale file handle"
        | UnixError.EDQUOT -> Some "Disk quota exceeded"
        | UnixError.ECANCELED -> Some "Operation canceled"
        | UnixError.EOWNERDEAD -> Some "Owner died"
        | UnixError.ENOTRECOVERABLE -> Some "State not recoverable"
        | UnixError.ECHRNG -> Some "Channel number out of range"
        | UnixError.EL2NSYNC -> Some "Level 2 not synchronized"
        | UnixError.EL3HLT -> Some "Level 3 halted"
        | UnixError.EL3RST -> Some "Level 3 reset"
        | UnixError.ELNRNG -> Some "Link number out of range"
        | UnixError.EUNATCH -> Some "Protocol driver not attached"
        | UnixError.ENOCSI -> Some "No CSI structure available"
        | UnixError.EL2HLT -> Some "Level 2 halted"
        | UnixError.EBADE -> Some "Invalid exchange"
        | UnixError.EBADR -> Some "Invalid request descriptor"
        | UnixError.EXFULL -> Some "Exchange full"
        | UnixError.ENOANO -> Some "No anode"
        | UnixError.EBADRQC -> Some "Invalid request code"
        | UnixError.EBADSLT -> Some "Invalid slot"
        | UnixError.EBFONT -> Some "Bad font file format"
        | UnixError.ENONET -> Some "Machine is not on the network"
        | UnixError.ENOPKG -> Some "Package not installed"
        | UnixError.EADV -> Some "Advertise error"
        | UnixError.ESRMNT -> Some "Srmount error"
        | UnixError.ECOMM -> Some "Communication error on send"
        | UnixError.EDOTDOT -> Some "RFS specific error"
        | UnixError.ENOTUNIQ -> Some "Name not unique on network"
        | UnixError.EBADFD -> Some "File descriptor in bad state"
        | UnixError.EREMCHG -> Some "Remote address changed"
        | UnixError.ELIBACC -> Some "Can not access a needed shared library"
        | UnixError.ELIBBAD -> Some "Accessing a corrupted shared library"
        | UnixError.ELIBSCN -> Some ".lib section in a.out corrupted"
        | UnixError.ELIBMAX -> Some "Attempting to link in too many shared libraries"
        | UnixError.ELIBEXEC -> Some "Cannot exec a shared library directly"
        | UnixError.ERESTART -> Some "Interrupted system call should be restarted"
        | UnixError.ESTRPIPE -> Some "Streams pipe error"
        | UnixError.EUCLEAN -> Some "Structure needs cleaning"
        | UnixError.ENOTNAM -> Some "Not a XENIX named type file"
        | UnixError.ENAVAIL -> Some "No XENIX semaphores available"
        | UnixError.EISNAM -> Some "Is a named type file"
        | UnixError.EREMOTEIO -> Some "Remote I/O error"
        | UnixError.ENOMEDIUM -> Some "No medium found"
        | UnixError.EMEDIUMTYPE -> Some "Wrong medium type"
        | UnixError.ENOKEY -> Some "Required key not available"
        | UnixError.EKEYEXPIRED -> Some "Key has expired"
        | UnixError.EKEYREVOKED -> Some "Key has been revoked"
        | UnixError.EKEYREJECTED -> Some "Key was rejected by service"
        | UnixError.ERFKILL -> Some "Operation not possible due to RF-kill"
        | UnixError.EHWPOISON -> Some "Memory page has hardware error"
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
        | UnixError.ENOTCAPABLE -> None

    let private darwinErrorText (error : UnixError) : string option =
        match error with
        | UnixError.EPERM -> Some "Operation not permitted"
        | UnixError.ENOENT -> Some "No such file or directory"
        | UnixError.ESRCH -> Some "No such process"
        | UnixError.EINTR -> Some "Interrupted system call"
        | UnixError.EIO -> Some "Input/output error"
        | UnixError.ENXIO -> Some "Device not configured"
        | UnixError.E2BIG -> Some "Argument list too long"
        | UnixError.ENOEXEC -> Some "Exec format error"
        | UnixError.EBADF -> Some "Bad file descriptor"
        | UnixError.ECHILD -> Some "No child processes"
        | UnixError.ENOMEM -> Some "Cannot allocate memory"
        | UnixError.EACCES -> Some "Permission denied"
        | UnixError.EFAULT -> Some "Bad address"
        | UnixError.EBUSY -> Some "Resource busy"
        | UnixError.EEXIST -> Some "File exists"
        | UnixError.EXDEV -> Some "Cross-device link"
        | UnixError.ENODEV -> Some "Operation not supported by device"
        | UnixError.ENOTDIR -> Some "Not a directory"
        | UnixError.EISDIR -> Some "Is a directory"
        | UnixError.EINVAL -> Some "Invalid argument"
        | UnixError.ENFILE -> Some "Too many open files in system"
        | UnixError.EMFILE -> Some "Too many open files"
        | UnixError.ENOTTY -> Some "Inappropriate ioctl for device"
        | UnixError.ETXTBSY -> Some "Text file busy"
        | UnixError.EFBIG -> Some "File too large"
        | UnixError.ENOSPC -> Some "No space left on device"
        | UnixError.ESPIPE -> Some "Illegal seek"
        | UnixError.EROFS -> Some "Read-only file system"
        | UnixError.EMLINK -> Some "Too many links"
        | UnixError.EPIPE -> Some "Broken pipe"
        | UnixError.EDOM -> Some "Numerical argument out of domain"
        | UnixError.ERANGE -> Some "Result too large"
        | UnixError.ELOOP -> Some "Too many levels of symbolic links"
        | UnixError.ENAMETOOLONG -> Some "File name too long"
        | UnixError.ENOTEMPTY -> Some "Directory not empty"
        | UnixError.EAGAIN -> Some "Resource temporarily unavailable"
        | UnixError.EOVERFLOW -> Some "Value too large to be stored in data type"
        | UnixError.EILSEQ -> Some "Illegal byte sequence"
        | UnixError.EAFNOSUPPORT -> Some "Address family not supported by protocol family"
        | UnixError.EPROTOTYPE -> Some "Protocol wrong type for socket"
        | UnixError.EPROTONOSUPPORT -> Some "Protocol not supported"
        | UnixError.EADDRINUSE -> Some "Address already in use"
        | UnixError.EADDRNOTAVAIL -> Some "Can't assign requested address"
        | UnixError.EOPNOTSUPP -> Some "Operation not supported on socket"
        | UnixError.ENOTSOCK -> Some "Socket operation on non-socket"
        | UnixError.EISCONN -> Some "Socket is already connected"
        | UnixError.EINPROGRESS -> Some "Operation now in progress"
        | UnixError.ECONNREFUSED -> Some "Connection refused"
        | UnixError.ENOTSUP -> Some "Operation not supported"
        | UnixError.ENOTCONN -> Some "Socket is not connected"
        | UnixError.ETIMEDOUT -> Some "Operation timed out"
        | UnixError.ECONNRESET -> Some "Connection reset by peer"
        | UnixError.EMSGSIZE -> Some "Message too long"
        | UnixError.ENOSYS -> Some "Function not implemented"
        | UnixError.EDEADLK -> Some "Resource deadlock avoided"
        | UnixError.ENOLCK -> Some "No locks available"
        | UnixError.ENOTBLK -> Some "Block device required"
        | UnixError.ENOMSG -> Some "No message of desired type"
        | UnixError.EIDRM -> Some "Identifier removed"
        | UnixError.ENOSTR -> Some "Not a STREAM"
        | UnixError.ENODATA -> Some "No message available on STREAM"
        | UnixError.ETIME -> Some "STREAM ioctl timeout"
        | UnixError.ENOSR -> Some "No STREAM resources"
        | UnixError.EREMOTE -> Some "Too many levels of remote in path"
        | UnixError.ENOLINK -> Some "ENOLINK (Reserved)"
        | UnixError.EPROTO -> Some "Protocol error"
        | UnixError.EMULTIHOP -> Some "EMULTIHOP (Reserved)"
        | UnixError.EBADMSG -> Some "Bad message"
        | UnixError.EUSERS -> Some "Too many users"
        | UnixError.EDESTADDRREQ -> Some "Destination address required"
        | UnixError.ENOPROTOOPT -> Some "Protocol not available"
        | UnixError.ESOCKTNOSUPPORT -> Some "Socket type not supported"
        | UnixError.EPFNOSUPPORT -> Some "Protocol family not supported"
        | UnixError.ENETDOWN -> Some "Network is down"
        | UnixError.ENETUNREACH -> Some "Network is unreachable"
        | UnixError.ENETRESET -> Some "Network dropped connection on reset"
        | UnixError.ECONNABORTED -> Some "Software caused connection abort"
        | UnixError.ENOBUFS -> Some "No buffer space available"
        | UnixError.ESHUTDOWN -> Some "Can't send after socket shutdown"
        | UnixError.ETOOMANYREFS -> Some "Too many references: can't splice"
        | UnixError.EHOSTDOWN -> Some "Host is down"
        | UnixError.EHOSTUNREACH -> Some "No route to host"
        | UnixError.EALREADY -> Some "Operation already in progress"
        | UnixError.ESTALE -> Some "Stale NFS file handle"
        | UnixError.EDQUOT -> Some "Disc quota exceeded"
        | UnixError.ECANCELED -> Some "Operation canceled"
        | UnixError.EOWNERDEAD -> Some "Previous owner died"
        | UnixError.ENOTRECOVERABLE -> Some "State not recoverable"
        | UnixError.EPROCLIM -> Some "Too many processes"
        | UnixError.EBADRPC -> Some "RPC struct is bad"
        | UnixError.ERPCMISMATCH -> Some "RPC version wrong"
        | UnixError.EPROGUNAVAIL -> Some "RPC prog. not avail"
        | UnixError.EPROGMISMATCH -> Some "Program version wrong"
        | UnixError.EPROCUNAVAIL -> Some "Bad procedure for program"
        | UnixError.EFTYPE -> Some "Inappropriate file type or format"
        | UnixError.EAUTH -> Some "Authentication error"
        | UnixError.ENEEDAUTH -> Some "Need authenticator"
        | UnixError.EPWROFF -> Some "Device power is off"
        | UnixError.EDEVERR -> Some "Device error"
        | UnixError.EBADEXEC -> Some "Bad executable (or shared library)"
        | UnixError.EBADARCH -> Some "Bad CPU type in executable"
        | UnixError.ESHLIBVERS -> Some "Shared library version mismatch"
        | UnixError.EBADMACHO -> Some "Malformed Mach-o file"
        | UnixError.ENOATTR -> Some "Attribute not found"
        | UnixError.ENOPOLICY -> Some "Policy not found"
        | UnixError.EQFULL -> Some "Interface output queue is full"
        | UnixError.ENOTCAPABLE -> Some "Capabilities insufficient"
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
        | UnixError.EHWPOISON -> None

    /// `strerror`'s text for `error`, or `None` for an error the library's
    /// numbering does not have (Darwin's `EAUTH` under glibc, Linux's `ENOKEY`
    /// under Darwin's libc).
    let errorText (library : CLibrary) (error : UnixError) : string option =
        match library with
        | CLibrary.Glibc -> glibcErrorText error
        | CLibrary.DarwinLibc -> darwinErrorText error
