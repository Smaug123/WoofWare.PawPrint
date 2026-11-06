namespace WoofWare.PawPrint

/// The `SysLogPriority` numbering `SystemNative_SysLog` takes its priority in
/// (`pal_process.h`), which CoreLib's `Interop.Sys.SysLogPriority` restates.
///
/// The PAL's eight numbers are `<syslog.h>`'s levels, which `pal_process.c`
/// static-asserts on every platform, and the shim ORs `LOG_USER` into its
/// argument before calling syslog(3). So each of the eight names one level of
/// the `LOG_USER` facility.
[<RequireQualifiedAccess>]
module SysLogPriorityPal =

    /// The level a `SysLogPriority` value names, or `None` for any number
    /// outside the enum. Such a number reaches syslog(3) with facility or
    /// unknown bits of its own, which no caller in the BCL passes.
    let toLevel (priority : int) : SystemLogLevel option =
        match priority with
        | 0 -> Some SystemLogLevel.Emergency
        | 1 -> Some SystemLogLevel.Alert
        | 2 -> Some SystemLogLevel.Critical
        | 3 -> Some SystemLogLevel.Error
        | 4 -> Some SystemLogLevel.Warning
        | 5 -> Some SystemLogLevel.Notice
        | 6 -> Some SystemLogLevel.Informational
        | 7 -> Some SystemLogLevel.Debug
        | _ -> None
