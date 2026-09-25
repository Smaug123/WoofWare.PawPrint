namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The signals a real CoreCLR process catches or ignores from the moment it
/// starts, before any guest code runs, so that sending one does not do what
/// the kernel's default would: terminate the process with that signal.
///
/// PawPrint's kernel model starts every signal at its kernel default, so for
/// these signals its answer would be wrong. A generator of signals refuses
/// them until the model holds the runtime's startup dispositions. `TestStartupSignalDispositions` checks the
/// host's column against the real runtime the test host runs.
[<RequireQualifiedAccess>]
module StartupSignalDispositions =

    // Measured 2026-09-24 by sending each signal number, with nothing
    // registered through PosixSignalRegistration, from a .NET 10 program to
    // itself with libc's kill(2): .NET 10.0.7 on Darwin 25.6.0 arm64, and .NET
    // 10.0.12 on Linux 6.18.5 aarch64 (Debian trixie, glibc 2.41); and again on
    // Linux x86-64, both under Rosetta in the same container (`container run
    // --arch amd64`) and on GitHub's ubuntu x86-64 runner, which agree. Each
    // number here is one whose kernel default is to terminate, and which did
    // not kill the process with that signal; every other terminating signal
    // did.
    //
    // All but one of them the process survived, on every machine. The
    // exception is SIGTRAP on Linux, whose outcome depends on the CPU: the
    // process survives it on aarch64, and on x86-64 dies of SIGILL (exit 132).
    // The PAL maps a SIGTRAP whose si_code is SI_USER, as kill(2)'s is, to
    // EXCEPTION_BREAKPOINT (pal/src/thread/context.cpp,
    // CONTEXTGetExceptionCodeForSignal), and the runtime's breakpoint path
    // moves the interrupted program counter back by
    // CORDbg_BREAK_INSTRUCTION_SIZE (vm/exceptionhandling.cpp,
    // HandleHardwareException), on the grounds that x86's int3 leaves it after
    // the breakpoint. On arm64 the same path first moves it forward by the
    // same 4 bytes, so it is unchanged; on x86-64 it is left one byte into the
    // syscall instruction that sent the signal, which does not decode. That
    // reading is consistent with both outcomes, but the resumption itself was
    // not traced.
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
    // Darwin takes faults as Mach exceptions rather than signals, so its PAL
    // installs no SIGTRAP handler, and a Darwin process dies of SIGTRAP as the
    // default says.
    //
    // SIGINT, SIGQUIT and SIGTERM have PAL handlers too, and are not here:
    // each restores the previous disposition and sends the signal again, so
    // the process dies of it exactly as the default says.
    let private linuxSignos : Set<int> =
        Set.ofList [ 4 ; 5 ; 6 ; 7 ; 8 ; 11 ; 13 ; 33 ; 34 ]

    let private darwinSignos : Set<int> = Set.ofList [ 4 ; 6 ; 8 ; 10 ; 11 ; 13 ; 30 ]

    /// Whether the kernel's default for `signal` is to terminate the process,
    /// but a CoreCLR process that has registered no handler through
    /// `PosixSignalRegistration` is not terminated by it when sent it.
    ///
    /// What happens instead is not one answer: usually the process survives,
    /// but on x86-64 Linux, SIGTRAP kills it with SIGILL.
    ///
    /// Answers for the signal the value *is* under the numbering, so an `Other`
    /// carrying SIGPIPE's number counts.
    let overridesTerminatingDefault (numbering : SignalNumbering) (signal : Signal) : bool =
        let signos =
            match numbering with
            | SignalNumbering.Linux -> linuxSignos
            | SignalNumbering.Darwin -> darwinSignos

        Set.contains (Signal.toRawSignoUnder numbering signal) signos
