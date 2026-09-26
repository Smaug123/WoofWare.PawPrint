namespace WoofWare.PosixKernel.Test

/// Every `E*` macro each flavour's `<errno.h>` defines, with the number it
/// resolves to, exactly as the C compiler sees it: the `DEFINE` rows of
/// `docs/plans/2026-08-23-posix-kernel-extraction/errno-table.sh`, which reads
/// the names from `cc -dM -E` rather than from a hand-picked list. Aliases and
/// bounds are kept, because they are what the header says; which of them name
/// an error of their own is `TestUnixError`'s claim, not this file's.
///
/// This is the authority `UnixError`'s table is checked against on every host.
/// The host's own header and `strerror` then check this, one flavour per host.
[<RequireQualifiedAccess>]
module ErrnoHeaders =

    /// Linux's `<errno.h>`: the kernel's `asm-generic/errno-base.h` and
    /// `asm-generic/errno.h` (linux-libc-dev 6.12.107, Debian trixie), which
    /// x86-64 and aarch64 both include unchanged, plus glibc 2.41's
    /// `ENOTSUP`. Read identically from an aarch64 and an x86-64 userland.
    let linux : (string * int) list =
        [
            "EPERM", 1
            "ENOENT", 2
            "ESRCH", 3
            "EINTR", 4
            "EIO", 5
            "ENXIO", 6
            "E2BIG", 7
            "ENOEXEC", 8
            "EBADF", 9
            "ECHILD", 10
            "EAGAIN", 11
            "EWOULDBLOCK", 11
            "ENOMEM", 12
            "EACCES", 13
            "EFAULT", 14
            "ENOTBLK", 15
            "EBUSY", 16
            "EEXIST", 17
            "EXDEV", 18
            "ENODEV", 19
            "ENOTDIR", 20
            "EISDIR", 21
            "EINVAL", 22
            "ENFILE", 23
            "EMFILE", 24
            "ENOTTY", 25
            "ETXTBSY", 26
            "EFBIG", 27
            "ENOSPC", 28
            "ESPIPE", 29
            "EROFS", 30
            "EMLINK", 31
            "EPIPE", 32
            "EDOM", 33
            "ERANGE", 34
            "EDEADLK", 35
            "EDEADLOCK", 35
            "ENAMETOOLONG", 36
            "ENOLCK", 37
            "ENOSYS", 38
            "ENOTEMPTY", 39
            "ELOOP", 40
            "ENOMSG", 42
            "EIDRM", 43
            "ECHRNG", 44
            "EL2NSYNC", 45
            "EL3HLT", 46
            "EL3RST", 47
            "ELNRNG", 48
            "EUNATCH", 49
            "ENOCSI", 50
            "EL2HLT", 51
            "EBADE", 52
            "EBADR", 53
            "EXFULL", 54
            "ENOANO", 55
            "EBADRQC", 56
            "EBADSLT", 57
            "EBFONT", 59
            "ENOSTR", 60
            "ENODATA", 61
            "ETIME", 62
            "ENOSR", 63
            "ENONET", 64
            "ENOPKG", 65
            "EREMOTE", 66
            "ENOLINK", 67
            "EADV", 68
            "ESRMNT", 69
            "ECOMM", 70
            "EPROTO", 71
            "EMULTIHOP", 72
            "EDOTDOT", 73
            "EBADMSG", 74
            "EOVERFLOW", 75
            "ENOTUNIQ", 76
            "EBADFD", 77
            "EREMCHG", 78
            "ELIBACC", 79
            "ELIBBAD", 80
            "ELIBSCN", 81
            "ELIBMAX", 82
            "ELIBEXEC", 83
            "EILSEQ", 84
            "ERESTART", 85
            "ESTRPIPE", 86
            "EUSERS", 87
            "ENOTSOCK", 88
            "EDESTADDRREQ", 89
            "EMSGSIZE", 90
            "EPROTOTYPE", 91
            "ENOPROTOOPT", 92
            "EPROTONOSUPPORT", 93
            "ESOCKTNOSUPPORT", 94
            "ENOTSUP", 95
            "EOPNOTSUPP", 95
            "EPFNOSUPPORT", 96
            "EAFNOSUPPORT", 97
            "EADDRINUSE", 98
            "EADDRNOTAVAIL", 99
            "ENETDOWN", 100
            "ENETUNREACH", 101
            "ENETRESET", 102
            "ECONNABORTED", 103
            "ECONNRESET", 104
            "ENOBUFS", 105
            "EISCONN", 106
            "ENOTCONN", 107
            "ESHUTDOWN", 108
            "ETOOMANYREFS", 109
            "ETIMEDOUT", 110
            "ECONNREFUSED", 111
            "EHOSTDOWN", 112
            "EHOSTUNREACH", 113
            "EALREADY", 114
            "EINPROGRESS", 115
            "ESTALE", 116
            "EUCLEAN", 117
            "ENOTNAM", 118
            "ENAVAIL", 119
            "EISNAM", 120
            "EREMOTEIO", 121
            "EDQUOT", 122
            "ENOMEDIUM", 123
            "EMEDIUMTYPE", 124
            "ECANCELED", 125
            "ENOKEY", 126
            "EKEYEXPIRED", 127
            "EKEYREVOKED", 128
            "EKEYREJECTED", 129
            "EOWNERDEAD", 130
            "ENOTRECOVERABLE", 131
            "ERFKILL", 132
            "EHWPOISON", 133
        ]

    /// Darwin's `<sys/errno.h>` from the macOS 26.4 SDK, the newest on the
    /// machine that measured Darwin 27.0.0, whose libc also knows 107. The
    /// macOS 14.4 SDK differs only in lacking `ENOTCAPABLE` and so having
    /// `ELAST` 106.
    let darwin : (string * int) list =
        [
            "EPERM", 1
            "ENOENT", 2
            "ESRCH", 3
            "EINTR", 4
            "EIO", 5
            "ENXIO", 6
            "E2BIG", 7
            "ENOEXEC", 8
            "EBADF", 9
            "ECHILD", 10
            "EDEADLK", 11
            "ENOMEM", 12
            "EACCES", 13
            "EFAULT", 14
            "ENOTBLK", 15
            "EBUSY", 16
            "EEXIST", 17
            "EXDEV", 18
            "ENODEV", 19
            "ENOTDIR", 20
            "EISDIR", 21
            "EINVAL", 22
            "ENFILE", 23
            "EMFILE", 24
            "ENOTTY", 25
            "ETXTBSY", 26
            "EFBIG", 27
            "ENOSPC", 28
            "ESPIPE", 29
            "EROFS", 30
            "EMLINK", 31
            "EPIPE", 32
            "EDOM", 33
            "ERANGE", 34
            "EAGAIN", 35
            "EWOULDBLOCK", 35
            "EINPROGRESS", 36
            "EALREADY", 37
            "ENOTSOCK", 38
            "EDESTADDRREQ", 39
            "EMSGSIZE", 40
            "EPROTOTYPE", 41
            "ENOPROTOOPT", 42
            "EPROTONOSUPPORT", 43
            "ESOCKTNOSUPPORT", 44
            "ENOTSUP", 45
            "EPFNOSUPPORT", 46
            "EAFNOSUPPORT", 47
            "EADDRINUSE", 48
            "EADDRNOTAVAIL", 49
            "ENETDOWN", 50
            "ENETUNREACH", 51
            "ENETRESET", 52
            "ECONNABORTED", 53
            "ECONNRESET", 54
            "ENOBUFS", 55
            "EISCONN", 56
            "ENOTCONN", 57
            "ESHUTDOWN", 58
            "ETOOMANYREFS", 59
            "ETIMEDOUT", 60
            "ECONNREFUSED", 61
            "ELOOP", 62
            "ENAMETOOLONG", 63
            "EHOSTDOWN", 64
            "EHOSTUNREACH", 65
            "ENOTEMPTY", 66
            "EPROCLIM", 67
            "EUSERS", 68
            "EDQUOT", 69
            "ESTALE", 70
            "EREMOTE", 71
            "EBADRPC", 72
            "ERPCMISMATCH", 73
            "EPROGUNAVAIL", 74
            "EPROGMISMATCH", 75
            "EPROCUNAVAIL", 76
            "ENOLCK", 77
            "ENOSYS", 78
            "EFTYPE", 79
            "EAUTH", 80
            "ENEEDAUTH", 81
            "EPWROFF", 82
            "EDEVERR", 83
            "EOVERFLOW", 84
            "EBADEXEC", 85
            "EBADARCH", 86
            "ESHLIBVERS", 87
            "EBADMACHO", 88
            "ECANCELED", 89
            "EIDRM", 90
            "ENOMSG", 91
            "EILSEQ", 92
            "ENOATTR", 93
            "EBADMSG", 94
            "EMULTIHOP", 95
            "ENODATA", 96
            "ENOLINK", 97
            "ENOSR", 98
            "ENOSTR", 99
            "EPROTO", 100
            "ETIME", 101
            "EOPNOTSUPP", 102
            "ENOPOLICY", 103
            "ENOTRECOVERABLE", 104
            "EOWNERDEAD", 105
            "EQFULL", 106
            "ELAST", 107
            "ENOTCAPABLE", 107
        ]
