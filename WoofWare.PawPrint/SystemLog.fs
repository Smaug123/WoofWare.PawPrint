namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The severity of a message handed to syslog(3): the low three bits of its
/// `priority` argument, `LOG_EMERG` to `LOG_DEBUG` in `<syslog.h>`.
[<RequireQualifiedAccess>]
type SystemLogLevel =
    /// `LOG_EMERG` (0): the system is unusable.
    | Emergency
    /// `LOG_ALERT` (1): action must be taken immediately.
    | Alert
    /// `LOG_CRIT` (2): critical conditions.
    | Critical
    /// `LOG_ERR` (3): error conditions.
    | Error
    /// `LOG_WARNING` (4): warning conditions.
    | Warning
    /// `LOG_NOTICE` (5): normal but significant condition.
    | Notice
    /// `LOG_INFO` (6): informational.
    | Informational
    /// `LOG_DEBUG` (7): debug-level messages.
    | Debug

/// One entry in `EmulatedKernel.SystemLog`: a message the process handed to
/// syslog(3), as it handed it over.
///
/// This is what the process asked to be logged, not what any system logger
/// would have kept: no timestamp, ident or process ID is added, and nothing is
/// truncated. The facility is `LOG_USER` for every entry, the only one
/// System.Native's shim logs under.
type SystemLogEntry =
    {
        Level : SystemLogLevel
        /// The bytes of the formatted message, without its terminating NUL.
        /// Not necessarily UTF-8: syslog(3) takes bytes.
        Message : UnixByteString
    }
