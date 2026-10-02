namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The dispositions a real CoreCLR process has installed by the time any guest
/// code runs: the signals it catches or ignores from the moment it starts, so
/// that sending one does not do what the kernel's default would.
///
/// Not every entry lasts: the runtime's handlers for the hardware-fault
/// signals restore the default the first time they are sent one, so a process
/// survives only the first; see `NativeSignalHandler.CoreClrPalFault`.
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
    //
    // Measured 2026-09-26 by sending each signal the process survives a second
    // time, reading the disposition back with sigaction(2) after each: .NET
    // 10.0.7 on Darwin 27.0.0 arm64, and .NET 10.0.12 on Linux 6.18.5 aarch64
    // (Ubuntu, glibc 2.39), natively and under Rosetta x86-64. SIGILL, SIGABRT,
    // SIGFPE, SIGBUS and SIGSEGV read SIG_DFL after the first, and the second
    // kills the process with that signal. Each of their handlers reaches
    // invoke_previous_action (pal/src/exception/signal.cpp): SIGABRT's at
    // once, the others once the runtime has declined to treat the signal as a
    // fault in managed code, which is inferred from the outcome rather than
    // traced. Given the SIG_DFL it replaced, that function runs the runtime's
    // one-shot shutdown notification (which cleans up the debugger transport),
    // restores SIG_DFL and returns, expecting the faulting instruction to run
    // again and raise the signal afresh. A signal sent by kill(2) is not
    // raised again, so the process carries on with the default installed.
    // Every other signal here reads the same after each as before (but
    // Linux's 33, which glibc will not report), and the process survives
    // both, except that x86-64 Linux dies of SIGILL at the first SIGTRAP, as
    // above. A handler registered through PosixSignalRegistration for one of
    // the five changes nothing: it runs for the first signal, and the second
    // still kills the process, on both flavours, because System.Native's
    // handler calls the runtime's handler it replaced (pal_signal.c,
    // SignalHandler), whose restore then overwrites System.Native's own.
    //
    // Over an inherited ignore the same five are fatal at the first: the
    // handler replaced SIG_IGN, and invoke_previous_action's branch for that
    // calls PROCAbort, so the process dies of SIGABRT (exit 134). Measured
    // 2026-09-26 on the same Darwin and Linux aarch64 machines, launching the
    // probe from `bash -c "trap '' <signo>; exec ..."`.

    /// The hardware-fault signals, which the runtime catches with
    /// `NativeSignalHandler.CoreClrPalFault`.
    let private runtimeFaultSignos (numbering : SignalNumbering) : int list =
        match numbering with
        | SignalNumbering.Linux -> [ 4 ; 6 ; 7 ; 8 ; 11 ]
        | SignalNumbering.Darwin -> [ 4 ; 6 ; 8 ; 10 ; 11 ]

    /// The runtime's thread-activation signal, which it catches with
    /// `NativeSignalHandler.CoreClrPalActivation`.
    let private runtimeActivationSigno (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 34
        | SignalNumbering.Darwin -> 30

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
            if not (Signal.existsUnder numbering signal) then
                Some $"%O{signal} is not a signal under the %O{numbering} numbering"
            elif Signal.isUncatchableUnder numbering signal then
                Some $"%O{signal} cannot be ignored through sigaction under the %O{numbering} numbering"
            elif signal = Signal.SIGTERM then
                Some
                    "SIGTERM: the runtime replaces an ignored SIGTERM with a handler that restores the ignore and re-sends it, which PawPrint models as the ignore alone, and a guest's own SIGTERM handler would then never run where it runs on real .NET"
            else
                None
        )

    /// `system`, a process its launcher started with `inheritedIgnores`
    /// ignored (read under `numbering`), as it stands once a real CoreCLR
    /// process has reached Main: with the dispositions the runtime and the C
    /// library install before then, each installed through `sigaction` as they
    /// install it. `system` must be one no code has run in yet.
    ///
    /// The runtime ignores SIGPIPE, and catches the hardware-fault signals
    /// and its thread-activation signal with handlers of its own whatever
    /// they were before; on Linux, glibc's own handler catches its reserved
    /// 33. Every other signal is ignored if the launcher left it so, and at
    /// its default otherwise. The runtime's handlers for SIGINT, SIGQUIT and
    /// SIGTERM are not installed: each restores the disposition it replaced
    /// and re-sends the signal, so the process does exactly what that
    /// disposition says.
    ///
    /// Fails loud, prefixed by `context`, on inherited ignores `refusal`
    /// refuses.
    let install<'Task when 'Task : comparison>
        (context : string)
        (numbering : SignalNumbering)
        (inheritedIgnores : Set<Signal>)
        (system : UnixSystem<'Task, NativeSignalHandler>)
        : UnixSystem<'Task, NativeSignalHandler>
        =
        match refusal numbering inheritedIgnores with
        | Some reason -> failwith $"%s{context}: cannot start a process with %s{reason}."
        | None ->

        let signal (signo : int) : Signal =
            match Signal.ofRawSignoUnder numbering signo with
            | ValueSome signal -> signal
            | ValueNone -> failwith $"StartupSignalDispositions: %d{signo} is not a signal under %O{numbering}"

        // `sigaction` as `call` makes it, which every signal installed here
        // accepts.
        let sigactionWith
            (call :
                int
                    -> SignalDisposition<NativeSignalHandler> option
                    -> UnixSystem<'Task, NativeSignalHandler>
                    -> Result<SignalDisposition<NativeSignalHandler> * UnixSystem<'Task, NativeSignalHandler>, UnixError>)
            (signo : int)
            (action : SignalDisposition<NativeSignalHandler> option)
            (system : UnixSystem<'Task, NativeSignalHandler>)
            : SignalDisposition<NativeSignalHandler> * UnixSystem<'Task, NativeSignalHandler>
            =
            match call signo action system with
            | Ok answer -> answer
            | Error (errno : UnixError) ->
                failwith
                    $"StartupSignalDispositions.install: sigaction(%d{signo}, %O{action}) under %O{numbering} was refused (%O{errno})"

        // A launcher such as `nohup` or a shell's `trap ''` ignores a signal
        // through the C library, and the ignore survives `execve`.
        let launched =
            (system, inheritedIgnores)
            ||> Set.fold (fun system ignored ->
                sigactionWith
                    UnixSignal.sigaction
                    (Signal.toRawSignoUnder numbering ignored)
                    (Some SignalDisposition.Ignore)
                    system
                |> snd
            )

        // The PAL's `handle_signal` installs every handler with `SA_RESTART`
        // and an empty `sa_mask`, except that on Linux SIGSEGV's runs on the
        // alternate stack and so masks the activation signal (34) too
        // (pal/src/exception/signal.cpp). glibc installs its SIGSETXID
        // handler with `SA_RESTART` (measured with the flags read back,
        // startup-signal-handler-owners.cs); its mask was not read, and
        // PawPrint refuses to send the signal it catches.
        let palCatch (handler : NativeSignalHandler) (signo : int) : SignalDisposition<NativeSignalHandler> =
            SignalDisposition.Catch
                {
                    Handler = handler
                    Mask =
                        match numbering, signo with
                        | SignalNumbering.Linux, 11 -> Set.singleton (signal 34)
                        | _ -> Set.empty
                    NoDefer = false
                    ResetHand = false
                    Restart = true
                }

        let installPal
            (signo : int)
            (handler : NativeSignalHandler)
            (system : UnixSystem<'Task, NativeSignalHandler>)
            : UnixSystem<'Task, NativeSignalHandler>
            =
            sigactionWith UnixSignal.sigaction signo (Some (palCatch handler signo)) system
            |> snd

        let withFaultHandlers =
            (launched, runtimeFaultSignos numbering)
            ||> List.fold (fun system signo ->
                // The PAL saves the disposition it replaces.
                let replaced =
                    match sigactionWith UnixSignal.sigaction signo None system |> fst with
                    | SignalDisposition.Default -> PalReplacedDisposition.Default
                    | SignalDisposition.Ignore -> PalReplacedDisposition.Ignore
                    | SignalDisposition.Catch action ->
                        failwith
                            $"StartupSignalDispositions.install: signal %d{signo} is caught by %O{action.Handler} before the runtime starts; a launcher can leave a signal only ignored or at its default."

                installPal signo (NativeSignalHandler.CoreClrPalFault replaced) system
            )

        let withActivation =
            installPal (runtimeActivationSigno numbering) NativeSignalHandler.CoreClrPalActivation withFaultHandlers

        let withPlatformHandlers =
            match numbering with
            | SignalNumbering.Linux ->
                withActivation
                |> installPal 5 NativeSignalHandler.CoreClrPalTrap
                // glibc installs its own handler below its `sigaction` wrapper,
                // which refuses 33 to everyone else.
                |> sigactionWith
                    UnixSignal.sigactionSyscall
                    33
                    (Some (
                        SignalDisposition.Catch
                            { SignalCatch.ofHandler NativeSignalHandler.GlibcSetXid with
                                Restart = true
                            }
                    ))
                |> snd
            | SignalNumbering.Darwin -> withActivation

        sigactionWith UnixSignal.sigaction 13 (Some SignalDisposition.Ignore) withPlatformHandlers
        |> snd
