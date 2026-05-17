namespace WoofWare.PawPrint

/// A POSIX signal recognised by the simulator. The named cases cover the
/// signals the .NET BCL surfaces via the `PosixSignal` managed enum plus the
/// common ones the runtime emits at process-startup (SIGPIPE, SIGABRT,
/// SIGUSR1/2). Anything we don't have a case for is carried as `Other` along
/// with the raw managed `PosixSignal` value so callers retain identity
/// information across PawPrint even when we don't model the signal's
/// semantics.
///
/// Note: this type intentionally does NOT mention native (POSIX) signal
/// numbers, which are platform-specific. The native↔domain mapping lives at
/// the P/Invoke seam and is platform-aware; the domain type is the
/// platform-neutral identity that the rest of the simulator works with.
type Signal =
    | SIGHUP
    | SIGINT
    | SIGQUIT
    | SIGTERM
    | SIGCHLD
    | SIGCONT
    | SIGWINCH
    | SIGTSTP
    | SIGTTIN
    | SIGTTOU
    | SIGPIPE
    | SIGUSR1
    | SIGUSR2
    | SIGABRT
    /// Catch-all for signals the simulator doesn't model semantically yet.
    /// Carries the raw managed `PosixSignal` enum value (negative for
    /// cross-platform signals, positive when callers supply a native signo
    /// directly). Two `Other` values are equal iff their raw values match.
    | Other of rawSignal : int

/// How a delivered signal should be dispatched inside the simulator. Today
/// the only shape is "invoke a managed callback identified by an object
/// reference"; the discriminated-union shape leaves headroom for future
/// dispositions (ignore, default, terminate) without churning callers.
[<RequireQualifiedAccess>]
type SignalHandler =
    /// Dispatch by invoking a managed callback wrapped behind the supplied
    /// object reference. Concretely, this is the `PosixSignalRegistration`
    /// (or analogous) object the guest installed via the runtime; the
    /// signal-delivery glue will reach in and invoke its delegate.
    | PosixCallback of objectRef : ManagedHeapAddress
