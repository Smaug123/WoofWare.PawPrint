namespace WoofWare.PawPrint

/// The native code a caught signal's disposition names in a PawPrint process:
/// the handler value its kernel's `SignalState` stores and hands back on
/// delivery. Every handler a real .NET process has installed is one of these,
/// so each names the library the real handler lives in.
[<RequireQualifiedAccess>]
type NativeSignalHandler =
    /// A handler of CoreCLR's PAL (`pal/src/exception/signal.cpp`, in
    /// libcoreclr), installed before Main for a signal the runtime takes
    /// over: the hardware-fault signals, and the runtime's thread-activation
    /// signal. See `StartupSignalDispositions`.
    | CoreClrPal
    /// glibc's handler for its reserved SIGSETXID, Linux's 33, which glibc
    /// installs for itself and which a real .NET process on Linux has by Main.
    | GlibcSetXid
    /// System.Native's `SignalHandler` (`pal_signal.c`), which
    /// `SystemNative_EnablePosixSignalHandling` installs. It hands the signal
    /// to the shim's dispatcher thread, having first run the handler it
    /// replaced unless the signal is SIGINT, SIGQUIT or SIGTERM; see
    /// `PosixSignalShim.original`.
    | SystemNative
