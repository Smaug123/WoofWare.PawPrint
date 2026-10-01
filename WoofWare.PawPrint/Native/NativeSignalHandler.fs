namespace WoofWare.PawPrint

/// The disposition CoreCLR's PAL replaced when it installed its handler for a
/// hardware-fault signal before Main: whatever the launcher left, which can
/// only be the default or an ignore, because nothing runs before the runtime
/// to install a handler.
[<RequireQualifiedAccess>]
type PalReplacedDisposition =
    | Default
    | Ignore

/// The native code a caught signal's disposition names in a PawPrint process:
/// the handler value its kernel's `SignalState` stores and hands back on
/// delivery. Every handler a real .NET process has installed is one of these,
/// so each names the library the real handler lives in.
[<RequireQualifiedAccess>]
type NativeSignalHandler =
    /// The handler of CoreCLR's PAL (`pal/src/exception/signal.cpp`, in
    /// libcoreclr) for a hardware-fault signal: SIGILL, SIGABRT, SIGFPE,
    /// SIGBUS or SIGSEGV, installed before Main whatever the launcher left.
    /// `replaced` is the disposition it replaced, which the PAL saves.
    ///
    /// Run for a signal some process sent rather than a fault, it puts
    /// `replaced` back and returns if that was the default, so the process
    /// carries on with the default installed; if it was an ignore, it aborts
    /// the process, which dies of SIGABRT. See `StartupSignalDispositions`.
    | CoreClrPalFault of replaced : PalReplacedDisposition
    /// The PAL's handler for SIGTRAP, which it installs on Linux only. Whether
    /// a process survives a SIGTRAP sent to it depends on the CPU.
    | CoreClrPalTrap
    /// The PAL's handler for its thread-activation signal
    /// (`INJECT_ACTIVATION_SIGNAL`): SIGRTMIN, 34 under glibc, on Linux, and
    /// SIGUSR1 on Darwin.
    | CoreClrPalActivation
    /// glibc's handler for its reserved SIGSETXID, Linux's 33, which glibc
    /// installs for itself and which a real .NET process on Linux has by Main.
    | GlibcSetXid
    /// System.Native's `SignalHandler` (`pal_signal.c`), which
    /// `SystemNative_EnablePosixSignalHandling` installs. It hands the signal
    /// to the shim's dispatcher thread, having first run the handler it
    /// replaced unless the signal is SIGINT, SIGQUIT or SIGTERM; see
    /// `PosixSignalShim.original`.
    | SystemNative
