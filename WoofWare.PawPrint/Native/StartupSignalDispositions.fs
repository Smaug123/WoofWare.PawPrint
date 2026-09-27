namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The dispositions a real CoreCLR process has installed by the time any guest
/// code runs: the signals it catches or ignores from the moment it starts, so
/// that sending one does not do what the kernel's default would.
///
/// A PawPrint process starts with this table, over whatever its launcher left
/// ignored. `TestStartupSignalDispositions` checks the host's column against
/// the real runtime the test host runs.
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
    // report; the raw rt_sigaction(2) syscall, which glibc does not screen,
    // finds its handler in libc.so.6, glibc's own
    // (docs/plans/2026-08-23-posix-kernel-extraction/startup-signal-handler-owners.cs,
    // 2026-09-26, .NET 10.0.11 on Linux 6.18.5 aarch64).
    //
    // Darwin takes faults as Mach exceptions rather than signals, so its PAL
    // installs no SIGTRAP handler, and a Darwin process dies of SIGTRAP as the
    // default says.
    //
    // SIGINT, SIGQUIT and SIGTERM have PAL handlers too, and are not here:
    // each restores the previous disposition and sends the signal again, so
    // the process dies of it exactly as the default says.
    //
    // Inherited ignores, measured 2026-09-26 by starting both probes from
    // `bash -c "trap '' <signals>; exec ..."` (dash does not pass on an
    // ignored SIGCHLD, bash does), ignoring every standard signal the table
    // does not name as well as those it does: on Linux 6.18.5 (.NET 10.0.11)
    // and Darwin 27.0.0 (.NET 10.0.7) alike, the runtime's handlers replaced
    // the ignores of the signals below and of SIGTERM, and every other ignore
    // was still in place at Main. The PAL installs SIGINT's and SIGQUIT's
    // handlers only over a disposition that is not SIG_IGN; its others it
    // installs regardless.
    let private runtimeCaught (numbering : SignalNumbering) : int list =
        match numbering with
        | SignalNumbering.Linux -> [ 4 ; 5 ; 6 ; 7 ; 8 ; 11 ; 34 ]
        | SignalNumbering.Darwin -> [ 4 ; 6 ; 8 ; 10 ; 11 ; 30 ]

    /// Why a set of inherited ignores cannot start a PawPrint process, or
    /// `None` if it can.
    ///
    /// Refused: a number that is not a signal under `numbering`; any signal
    /// glibc's `sigaction` refuses (SIGKILL and SIGSTOP, and on Linux its
    /// reserved 32 and 33), which a launcher could not have left ignored; and
    /// SIGTERM. The runtime replaces an ignored SIGTERM with a handler of its
    /// own that restores the ignore and re-sends the signal, which PawPrint
    /// models as the ignore alone. That is exact until the guest registers a
    /// handler for SIGTERM: a real process then runs it, where the model
    /// would leave the signal ignored.
    let refusal (numbering : SignalNumbering) (inheritedIgnores : Set<Signal>) : string option =
        inheritedIgnores
        |> Seq.tryPick (fun signal ->
            match signal with
            | Signal.Other raw when (Signal.ofRawSignoUnder numbering raw).IsNone ->
                Some $"%d{raw} is not a signal under the %O{numbering} numbering"
            | _ ->

            let signal = Signal.canonicalUnder numbering signal

            if Signal.isUncatchableUnder numbering signal then
                Some $"%O{signal} cannot be ignored through sigaction under the %O{numbering} numbering"
            elif signal = Signal.SIGTERM then
                Some
                    "SIGTERM: the runtime replaces an ignored SIGTERM with a handler that restores the ignore and re-sends it, which PawPrint models as the ignore alone, and a guest's own SIGTERM handler would then never run where it runs on real .NET"
            else
                None
        )

    /// The disposition table of a real CoreCLR process at Main, whose launcher
    /// left `inheritedIgnores` ignored (read under `numbering`).
    ///
    /// The runtime ignores SIGPIPE, and catches the hardware-fault signals
    /// and its thread-activation signal with handlers of its own whatever
    /// they were before; on Linux, glibc's own handler catches its reserved
    /// 33. Every other signal is ignored if the launcher left it so, and at
    /// its default otherwise. The runtime's handlers for SIGINT, SIGQUIT and
    /// SIGTERM are not in the table: each restores the disposition it
    /// replaced and re-sends the signal, so the process does exactly what
    /// that disposition says.
    ///
    /// Fails loud on inherited ignores `refusal` refuses.
    let initial<'Task when 'Task : comparison>
        (numbering : SignalNumbering)
        (inheritedIgnores : Set<Signal>)
        : SignalState<'Task, NativeSignalHandler>
        =
        match refusal numbering inheritedIgnores with
        | Some reason -> failwith $"StartupSignalDispositions.initial: cannot start a process with %s{reason}."
        | None ->

        let caught =
            [
                for signo in runtimeCaught numbering do
                    signo, SignalDisposition.Catch NativeSignalHandler.CoreClrPal
                match numbering with
                | SignalNumbering.Linux -> 33, SignalDisposition.Catch NativeSignalHandler.GlibcSetXid
                | SignalNumbering.Darwin -> ()
                13, SignalDisposition.Ignore
            ]

        (SignalState.initial numbering inheritedIgnores, caught)
        ||> List.fold (fun state (signo, disposition) ->
            match Signal.ofRawSignoUnder numbering signo with
            | ValueSome signal -> SignalState.setDisposition signal disposition state
            | ValueNone -> failwith $"StartupSignalDispositions: %d{signo} is not a signal under %O{numbering}"
        )
