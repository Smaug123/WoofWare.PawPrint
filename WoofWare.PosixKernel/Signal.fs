namespace WoofWare.PosixKernel

/// <summary>
/// Whose <c>&lt;signal.h&gt;</c> a raw signal number is read under.
/// </summary>
/// <remarks>
/// A signo is meaningless until something says which Unix assigned it: 17 is
/// <c>SIGCHLD</c> on Linux and <c>SIGSTOP</c> on Darwin, and Darwin has no
/// signal 32 where Linux has real-time signals up to 64. This is the signal
/// counterpart of <c>RawErrnoNumbering</c>; <c>SimulatedUnixPlatform.signalNumbering</c>
/// says which one a platform uses.
/// </remarks>
[<RequireQualifiedAccess>]
type SignalNumbering =
    | Linux
    | Darwin

/// <summary>
/// A POSIX signal recognised by the simulator.
/// </summary>
/// <remarks>
/// The named cases cover some specific signals we've had reason to handle explicitly.
/// Use the <c>Other</c> case for anything we've missed.
///
/// This type intentionally does not model an assignment of signals to numbers, which are platform-specific.
/// Use <c>Signal.toRawSignoUnder</c> to render a number under a given <c>SignalNumbering</c>.
/// </remarks>
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
    | SIGURG
    /// Catch-all for signals the simulator doesn't model semantically yet.
    /// Carries a raw signo, read under the numbering of whichever platform the
    /// process simulates: `Other 17` is `SIGSTOP` in a Darwin process and
    /// `SIGCHLD` in a Linux one, so a value must not cross between processes
    /// of different flavours. `ofRawSignoUnder` is the only thing here that
    /// builds one, and it produces only signos in `(0, highestSignoUnder]`
    /// that name no other case — but the case is public and enforces nothing,
    /// so a client can put any number in it and `toRawSignoUnder` will hand
    /// that number straight back. Two `Other` values are equal iff their raw
    /// values match.
    | Other of rawSignal : int

/// <summary>
/// The kernel-level default action for a POSIX signal when no managed
/// handler claims it.
/// </summary>
/// <remarks>
/// Mirrors the POSIX 1003.1 categories.
/// </remarks>
[<RequireQualifiedAccess>]
type DefaultDisposition =
    /// <summary>
    /// Specify that the kernel default is to terminate the process.
    /// </summary>
    | Terminate
    /// <summary>
    /// Specify that the kernel default is to ignore the signal entirely. No state changes.
    /// </summary>
    | Ignore
    /// <summary>
    /// Specify that the kernel default is to suspend (stop) the process.
    /// </summary>
    | Stop
    /// <summary>
    /// Specify that the kernel default is to resume a stopped process.
    /// </summary>
    | Continue

[<RequireQualifiedAccess>]
module Signal =
    /// <summary>
    /// The highest signal number this kernel has: <c>kill(2)</c> and
    /// <c>sigaction(2)</c> refuse anything above it with <c>EINVAL</c>.
    /// </summary>
    /// <remarks>
    /// Linux: 64, which is glibc's <c>SIGRTMAX</c> and one less than its
    /// <c>NSIG</c> of 65. Darwin: 31, one less than its <c>NSIG</c> of 32; it
    /// has no real-time signals and so no <c>SIGRTMAX</c> at all. Both measured
    /// by installing <c>SIG_DFL</c> with <c>sigaction</c> for every number up
    /// to <c>NSIG + 1</c>, on Linux 6.18.5 / glibc 2.41 and Darwin 25.6.0.
    ///
    /// This is the kernel's ceiling, which is not the same number as the one
    /// CoreCLR's shim screens with: its <c>GetSignalMax()</c> is <c>SIGRTMAX</c>
    /// where that is defined and <c>NSIG</c> otherwise, so on Darwin it admits
    /// 32. A client that speaks to that shim states that rule itself.
    /// </remarks>
    let highestSignoUnder (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 64
        | SignalNumbering.Darwin -> 31

    /// <summary>
    /// The raw <c>&lt;signal.h&gt;</c> number for this signal on the chosen platform.
    /// </summary>
    /// <remarks>
    /// Nine of the named signals have the same number on both; <c>SIGUSR1</c>,
    /// <c>SIGUSR2</c>, <c>SIGCHLD</c>, <c>SIGCONT</c>, <c>SIGTSTP</c> and
    /// <c>SIGURG</c> do not.
    /// Both columns were measured with a C probe rather than transcribed, on
    /// Linux 6.18.5 / glibc 2.41 and Darwin 25.6.0.
    ///
    /// An <c>Other</c> value's payload is handed back unchanged: it was a raw
    /// signo under this numbering to begin with.
    /// </remarks>
    let toRawSignoUnder (numbering : SignalNumbering) (signal : Signal) : int =
        match signal with
        | Signal.SIGHUP -> 1
        | Signal.SIGINT -> 2
        | Signal.SIGQUIT -> 3
        | Signal.SIGABRT -> 6
        | Signal.SIGPIPE -> 13
        | Signal.SIGTERM -> 15
        | Signal.SIGTTIN -> 21
        | Signal.SIGTTOU -> 22
        | Signal.SIGWINCH -> 28
        | Signal.SIGUSR1 ->
            match numbering with
            | SignalNumbering.Linux -> 10
            | SignalNumbering.Darwin -> 30
        | Signal.SIGUSR2 ->
            match numbering with
            | SignalNumbering.Linux -> 12
            | SignalNumbering.Darwin -> 31
        | Signal.SIGCHLD ->
            match numbering with
            | SignalNumbering.Linux -> 17
            | SignalNumbering.Darwin -> 20
        | Signal.SIGCONT ->
            match numbering with
            | SignalNumbering.Linux -> 18
            | SignalNumbering.Darwin -> 19
        | Signal.SIGTSTP ->
            match numbering with
            | SignalNumbering.Linux -> 20
            | SignalNumbering.Darwin -> 18
        | Signal.SIGURG ->
            match numbering with
            | SignalNumbering.Linux -> 23
            | SignalNumbering.Darwin -> 16
        | Signal.Other rawSignal -> rawSignal

    /// Every case but `Other`, which is the search space for `ofRawSignoUnder`.
    let private named : Signal list =
        [
            Signal.SIGHUP
            Signal.SIGINT
            Signal.SIGQUIT
            Signal.SIGTERM
            Signal.SIGCHLD
            Signal.SIGCONT
            Signal.SIGWINCH
            Signal.SIGTSTP
            Signal.SIGTTIN
            Signal.SIGTTOU
            Signal.SIGPIPE
            Signal.SIGUSR1
            Signal.SIGUSR2
            Signal.SIGABRT
            Signal.SIGURG
        ]

    /// <summary>
    /// Convert a raw signo, read under the chosen platform's numbering, to a
    /// signal.
    /// </summary>
    /// <remarks>
    /// The inverse of <c>toRawSignoUnder</c>: a number naming one of the
    /// named cases produces that case, and any other number the kernel has —
    /// positive and at most <c>highestSignoUnder</c> — round-trips through
    /// <c>Signal.Other</c> so that its identity survives.
    /// </remarks>
    /// <returns>
    /// <c>ValueNone</c> for a number that is not a signal on this platform:
    /// zero, a negative, or anything above <c>highestSignoUnder</c>. Darwin's
    /// 32 is such a number, even though CoreCLR's shim admits it.
    /// </returns>
    let ofRawSignoUnder (numbering : SignalNumbering) (signo : int) : Signal voption =
        match named |> List.tryFind (fun signal -> toRawSignoUnder numbering signal = signo) with
        | Some signal -> ValueSome signal
        | None ->
            if signo > 0 && signo <= highestSignoUnder numbering then
                ValueSome (Signal.Other signo)
            else
                ValueNone

    /// <summary>
    /// The named spelling of a signal that may have arrived as <c>Other</c>
    /// carrying a named signal's number.
    /// </summary>
    /// <remarks>
    /// <c>Other</c> is public and enforces nothing, so a client can spell
    /// <c>SIGCHLD</c> as <c>Other 17</c> under Linux. The classifiers here
    /// answer for the signal a value <i>is</i> under the numbering, so they
    /// ask for this spelling first; a client keying its own tables on the
    /// case should do the same. A number naming no case, or none at all, is
    /// handed back unchanged.
    /// </remarks>
    let canonicalUnder (numbering : SignalNumbering) (signal : Signal) : Signal =
        match signal with
        | Signal.Other rawSignal ->
            match ofRawSignoUnder numbering rawSignal with
            | ValueSome named -> named
            | ValueNone -> signal
        | _ -> signal

    /// <summary>
    /// Whether <c>sigaction(2)</c> refuses to install a handler for this
    /// signal, with <c>EINVAL</c>.
    /// </summary>
    /// <remarks>
    /// <c>SIGKILL</c> and <c>SIGSTOP</c> on both, as POSIX requires: 9 and 19
    /// on Linux, 9 and 17 on Darwin. Linux additionally refuses 32 and 33,
    /// which are not the kernel's doing but glibc's: its <c>sigaction</c>
    /// wrapper screens out <c>SIGCANCEL</c> and <c>SIGSETXID</c>, which it
    /// reserves for its own thread machinery. Measured on Linux 6.18.5 /
    /// glibc 2.41 and Darwin 25.6.0 by installing <c>SIG_DFL</c> for every
    /// number up to <c>NSIG + 1</c>; these were the only refusals below the
    /// ceiling. Neither <c>SIGKILL</c> nor <c>SIGSTOP</c> is a named case, so
    /// they only ever arrive as <c>Signal.Other</c>, and every named case is
    /// catchable — including one spelled as <c>Other</c> carrying its number,
    /// which classifies as the signal it is.
    /// </remarks>
    let isUncatchableUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        match canonicalUnder numbering signal with
        | Signal.Other rawSignal ->
            match numbering with
            | SignalNumbering.Linux -> rawSignal = 9 || rawSignal = 19 || rawSignal = 32 || rawSignal = 33
            | SignalNumbering.Darwin -> rawSignal = 9 || rawSignal = 17
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGTERM
        | Signal.SIGCHLD
        | Signal.SIGCONT
        | Signal.SIGWINCH
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU
        | Signal.SIGPIPE
        | Signal.SIGUSR1
        | Signal.SIGUSR2
        | Signal.SIGABRT
        | Signal.SIGURG -> false

    /// <summary>
    /// The kernel-level default disposition for <c>signal</c>, read under the
    /// chosen platform's numbering.
    /// </summary>
    /// <remarks>
    /// The named cases have the same disposition everywhere. Only a
    /// <c>Signal.Other</c> needs the numbering, because its payload's identity
    /// does: Darwin's 29 is <c>SIGINFO</c> (discarded) where Linux's 29 is
    /// <c>SIGIO</c> (terminates), and <c>SIGIO</c> itself — Darwin's 23 — is
    /// discarded there where Linux's 23 is <c>SIGURG</c>. Measured on Linux
    /// 6.18.5 and Darwin 25.6.0 by having a forked child raise each signal on
    /// itself under <c>SIG_DFL</c>.
    ///
    /// Signals the measurement could not classify — <c>SIGTSTP</c>,
    /// <c>SIGTTIN</c> and <c>SIGTTOU</c>, which both kernels discard rather
    /// than stop on when the process group is orphaned, as the probe's was —
    /// take POSIX's <c>Stop</c>. Anything else terminates, which is the POSIX
    /// default for a signal not otherwise specified.
    ///
    /// An <c>Other</c> carrying a named signal's number classifies as that
    /// signal: <c>Other 17</c> under Linux is <c>SIGCHLD</c>, and is ignored.
    /// </remarks>
    let defaultDispositionUnder (numbering : SignalNumbering) (signal : Signal) : DefaultDisposition =
        match canonicalUnder numbering signal with
        | Signal.SIGCHLD
        | Signal.SIGWINCH
        | Signal.SIGURG -> DefaultDisposition.Ignore
        | Signal.SIGCONT -> DefaultDisposition.Continue
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU -> DefaultDisposition.Stop
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGTERM
        | Signal.SIGPIPE
        | Signal.SIGUSR1
        | Signal.SIGUSR2
        | Signal.SIGABRT -> DefaultDisposition.Terminate
        | Signal.Other rawSignal ->
            match numbering with
            | SignalNumbering.Linux ->
                match rawSignal with
                // SIGSTOP
                | 19 -> DefaultDisposition.Stop
                | _ -> DefaultDisposition.Terminate
            | SignalNumbering.Darwin ->
                match rawSignal with
                // SIGSTOP
                | 17 -> DefaultDisposition.Stop
                // SIGIO and SIGINFO
                | 23
                | 29 -> DefaultDisposition.Ignore
                | _ -> DefaultDisposition.Terminate
