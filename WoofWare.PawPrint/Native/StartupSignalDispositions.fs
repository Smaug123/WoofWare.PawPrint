namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The signals a real CoreCLR process catches or ignores from the moment it
/// starts, before any guest code runs, where the kernel's default would
/// terminate it.
///
/// PawPrint's kernel model starts every signal at its kernel default, so for
/// these signals it would terminate a process that a real runtime keeps
/// running. A generator of signals refuses them until the model holds the
/// runtime's startup dispositions. `TestStartupSignalDispositions` checks the
/// host's column against the real runtime the test host runs.
[<RequireQualifiedAccess>]
module StartupSignalDispositions =

    // Measured 2026-09-24 by sending each signal number, with nothing
    // registered through PosixSignalRegistration, from a .NET 10 program to
    // itself with libc's kill(2): .NET 10.0.7 on Darwin 25.6.0 arm64, and .NET
    // 10.0.12 on Linux 6.18.5 aarch64 (Debian trixie, glibc 2.41). Each number
    // here is one the process survived although its kernel default is to
    // terminate; every other terminating signal killed it, as the default says.
    //
    // Reading every disposition back with sigaction(2) at Main, in the same
    // programs, finds each of these but 33 caught or ignored already. That
    // matches pal/src/exception/signal.cpp: CoreCLR's PAL installs handlers
    // for SIGILL, SIGFPE, SIGBUS, SIGABRT and SIGSEGV everywhere, for SIGTRAP
    // on Linux only, and for its thread-activation signal
    // (INJECT_ACTIVATION_SIGNAL), which is SIGRTMIN, 34 under glibc, on Linux
    // and SIGUSR1 on Darwin; and it sets SIGPIPE to SIG_IGN. Linux's 33 is
    // glibc's SIGSETXID, whose disposition glibc's sigaction refuses even to
    // report, so the reading cannot say what catches it, only that the process
    // survives it.
    //
    // SIGINT, SIGQUIT and SIGTERM have PAL handlers too, and are not here:
    // each restores the previous disposition and sends the signal again, so
    // the process dies of it exactly as the default says.
    let private linuxSignos : Set<int> =
        Set.ofList [ 4 ; 5 ; 6 ; 7 ; 8 ; 11 ; 13 ; 33 ; 34 ]

    let private darwinSignos : Set<int> = Set.ofList [ 4 ; 6 ; 8 ; 10 ; 11 ; 13 ; 30 ]

    /// Whether a CoreCLR process that has registered no handler through
    /// `PosixSignalRegistration` survives being sent `signal`, although the
    /// kernel's default for it is to terminate the process.
    ///
    /// Answers for the signal the value *is* under the numbering, so an `Other`
    /// carrying SIGPIPE's number counts.
    let survivesDespiteTerminatingDefault (numbering : SignalNumbering) (signal : Signal) : bool =
        let signos =
            match numbering with
            | SignalNumbering.Linux -> linuxSignos
            | SignalNumbering.Darwin -> darwinSignos

        Set.contains (Signal.toRawSignoUnder numbering signal) signos
