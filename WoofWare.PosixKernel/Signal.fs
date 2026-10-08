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
/// The named cases cover every standard signal either platform's <c>&lt;signal.h&gt;</c> defines, and
/// <c>RealTime</c> covers Linux's real-time signals, so every signal number either kernel has names a case.
/// Some exist on one platform only (<c>SIGSTKFLT</c> and <c>SIGPWR</c> on Linux, <c>SIGEMT</c> and <c>SIGINFO</c>
/// on Darwin); <c>Signal.existsUnder</c> says which.
/// An alias is not a case of its own: <c>SIGIOT</c> is <c>SIGABRT</c>, <c>SIGCLD</c> is <c>SIGCHLD</c>, and Linux's
/// <c>SIGPOLL</c> is <c>SIGIO</c>.
///
/// This type intentionally does not model an assignment of signals to numbers, which are platform-specific.
/// Use <c>Signal.toRawSignoUnder</c> to render a number under a given <c>SignalNumbering</c>.
/// </remarks>
type Signal =
    | SIGHUP
    | SIGINT
    | SIGQUIT
    | SIGILL
    | SIGTRAP
    | SIGABRT
    | SIGBUS
    | SIGFPE
    | SIGKILL
    | SIGUSR1
    | SIGSEGV
    | SIGUSR2
    | SIGPIPE
    | SIGALRM
    | SIGTERM
    /// Linux only.
    | SIGSTKFLT
    | SIGCHLD
    | SIGCONT
    | SIGSTOP
    | SIGTSTP
    | SIGTTIN
    | SIGTTOU
    | SIGURG
    | SIGXCPU
    | SIGXFSZ
    | SIGVTALRM
    | SIGPROF
    | SIGWINCH
    /// Linux's `<signal.h>` also calls this `SIGPOLL`. Darwin's gives that
    /// name to `SIGEMT`'s number instead, and only under strict POSIX.
    | SIGIO
    /// Linux only.
    | SIGPWR
    | SIGSYS
    /// Darwin only.
    | SIGEMT
    /// Darwin only.
    | SIGINFO
    /// One of Linux's real-time signals, `offset` above the kernel's lowest,
    /// which is signal 32: `RealTime 0` is 32 and `RealTime 32` is 64. glibc
    /// keeps `RealTime 0` and `RealTime 1` for its own threads, so the
    /// `SIGRTMIN` a program reads from it is `RealTime 2`. Darwin has none.
    | RealTime of offset : int

/// <summary>
/// The kernel-level default action for a POSIX signal when no handler claims
/// it.
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
    /// </remarks>
    let highestSignoUnder (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 64
        | SignalNumbering.Darwin -> 31

    /// The kernel's lowest real-time signal on Linux, which is `RealTime 0`.
    let private lowestRealTimeSigno : int = 32

    /// The highest real-time signal's `offset`: Linux's 64 is `RealTime 32`.
    let private highestRealTimeOffset : int =
        highestSignoUnder SignalNumbering.Linux - lowestRealTimeSigno

    // Measured 2026-10-02 by
    // `docs/plans/2026-08-23-posix-kernel-extraction/signal-names.c`, which
    // prints every candidate name's macro and the C library's own name for
    // every number below NSIG: on Linux 6.18.5 with glibc 2.41, for aarch64
    // and for x86-64 (identical), and on Darwin 27.0.0 arm64. Its outputs are
    // committed beside it. Earlier columns were measured the same way on
    // Linux 6.18.5 / glibc 2.41 and Darwin 25.6.0.
    let private tryRawSignoUnder (numbering : SignalNumbering) (signal : Signal) : int voption =
        match numbering with
        | SignalNumbering.Linux ->
            match signal with
            | Signal.SIGHUP -> ValueSome 1
            | Signal.SIGINT -> ValueSome 2
            | Signal.SIGQUIT -> ValueSome 3
            | Signal.SIGILL -> ValueSome 4
            | Signal.SIGTRAP -> ValueSome 5
            | Signal.SIGABRT -> ValueSome 6
            | Signal.SIGBUS -> ValueSome 7
            | Signal.SIGFPE -> ValueSome 8
            | Signal.SIGKILL -> ValueSome 9
            | Signal.SIGUSR1 -> ValueSome 10
            | Signal.SIGSEGV -> ValueSome 11
            | Signal.SIGUSR2 -> ValueSome 12
            | Signal.SIGPIPE -> ValueSome 13
            | Signal.SIGALRM -> ValueSome 14
            | Signal.SIGTERM -> ValueSome 15
            | Signal.SIGSTKFLT -> ValueSome 16
            | Signal.SIGCHLD -> ValueSome 17
            | Signal.SIGCONT -> ValueSome 18
            | Signal.SIGSTOP -> ValueSome 19
            | Signal.SIGTSTP -> ValueSome 20
            | Signal.SIGTTIN -> ValueSome 21
            | Signal.SIGTTOU -> ValueSome 22
            | Signal.SIGURG -> ValueSome 23
            | Signal.SIGXCPU -> ValueSome 24
            | Signal.SIGXFSZ -> ValueSome 25
            | Signal.SIGVTALRM -> ValueSome 26
            | Signal.SIGPROF -> ValueSome 27
            | Signal.SIGWINCH -> ValueSome 28
            | Signal.SIGIO -> ValueSome 29
            | Signal.SIGPWR -> ValueSome 30
            | Signal.SIGSYS -> ValueSome 31
            | Signal.RealTime offset ->
                if offset >= 0 && offset <= highestRealTimeOffset then
                    ValueSome (lowestRealTimeSigno + offset)
                else
                    ValueNone
            | Signal.SIGEMT
            | Signal.SIGINFO -> ValueNone
        | SignalNumbering.Darwin ->
            match signal with
            | Signal.SIGHUP -> ValueSome 1
            | Signal.SIGINT -> ValueSome 2
            | Signal.SIGQUIT -> ValueSome 3
            | Signal.SIGILL -> ValueSome 4
            | Signal.SIGTRAP -> ValueSome 5
            | Signal.SIGABRT -> ValueSome 6
            | Signal.SIGEMT -> ValueSome 7
            | Signal.SIGFPE -> ValueSome 8
            | Signal.SIGKILL -> ValueSome 9
            | Signal.SIGBUS -> ValueSome 10
            | Signal.SIGSEGV -> ValueSome 11
            | Signal.SIGSYS -> ValueSome 12
            | Signal.SIGPIPE -> ValueSome 13
            | Signal.SIGALRM -> ValueSome 14
            | Signal.SIGTERM -> ValueSome 15
            | Signal.SIGURG -> ValueSome 16
            | Signal.SIGSTOP -> ValueSome 17
            | Signal.SIGTSTP -> ValueSome 18
            | Signal.SIGCONT -> ValueSome 19
            | Signal.SIGCHLD -> ValueSome 20
            | Signal.SIGTTIN -> ValueSome 21
            | Signal.SIGTTOU -> ValueSome 22
            | Signal.SIGIO -> ValueSome 23
            | Signal.SIGXCPU -> ValueSome 24
            | Signal.SIGXFSZ -> ValueSome 25
            | Signal.SIGVTALRM -> ValueSome 26
            | Signal.SIGPROF -> ValueSome 27
            | Signal.SIGWINCH -> ValueSome 28
            | Signal.SIGINFO -> ValueSome 29
            | Signal.SIGUSR1 -> ValueSome 30
            | Signal.SIGUSR2 -> ValueSome 31
            | Signal.SIGSTKFLT
            | Signal.SIGPWR
            | Signal.RealTime _ -> ValueNone

    /// Whether the chosen platform has this signal at all: `SIGPWR` is not a
    /// signal on Darwin, nor `SIGINFO` on Linux, nor a `RealTime` whose offset
    /// is outside 0 to 32 anywhere.
    let existsUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        (tryRawSignoUnder numbering signal).IsSome

    /// <summary>
    /// The raw <c>&lt;signal.h&gt;</c> number for this signal on the chosen platform.
    /// </summary>
    /// <remarks>
    /// Most signals have the same number on both; <c>SIGBUS</c>, <c>SIGUSR1</c>,
    /// <c>SIGUSR2</c>, <c>SIGCHLD</c>, <c>SIGCONT</c>, <c>SIGSTOP</c>, <c>SIGTSTP</c>,
    /// <c>SIGURG</c>, <c>SIGIO</c> and <c>SIGSYS</c> do not.
    /// Both columns were measured with a C probe rather than transcribed, on
    /// Linux 6.18.5 / glibc 2.41 and Darwin 27.0.0.
    ///
    /// Throws for a signal the platform does not have (see <c>existsUnder</c>),
    /// which no number under its numbering could stand for.
    /// </remarks>
    let toRawSignoUnder (numbering : SignalNumbering) (signal : Signal) : int =
        match tryRawSignoUnder numbering signal with
        | ValueSome signo -> signo
        | ValueNone ->
            failwith
                $"Signal.toRawSignoUnder: %O{signal} is not a signal under the %O{numbering} numbering, so it has no number there."

    /// <summary>
    /// Convert a raw signo, read under the chosen platform's numbering, to a
    /// signal.
    /// </summary>
    /// <remarks>
    /// The inverse of <c>toRawSignoUnder</c>: every number the kernel has —
    /// positive and at most <c>highestSignoUnder</c> — names a case.
    /// </remarks>
    /// <returns>
    /// <c>ValueNone</c> for a number that is not a signal on this platform:
    /// zero, a negative, or anything above <c>highestSignoUnder</c>. Darwin's
    /// 32 is such a number, although it is that platform's <c>NSIG</c>.
    /// </returns>
    let ofRawSignoUnder (numbering : SignalNumbering) (signo : int) : Signal voption =
        // Written out rather than searched for through `toRawSignoUnder`, so
        // that the round trip between the two is a check on each table rather
        // than true by construction.
        match numbering with
        | SignalNumbering.Linux ->
            match signo with
            | 1 -> ValueSome Signal.SIGHUP
            | 2 -> ValueSome Signal.SIGINT
            | 3 -> ValueSome Signal.SIGQUIT
            | 4 -> ValueSome Signal.SIGILL
            | 5 -> ValueSome Signal.SIGTRAP
            | 6 -> ValueSome Signal.SIGABRT
            | 7 -> ValueSome Signal.SIGBUS
            | 8 -> ValueSome Signal.SIGFPE
            | 9 -> ValueSome Signal.SIGKILL
            | 10 -> ValueSome Signal.SIGUSR1
            | 11 -> ValueSome Signal.SIGSEGV
            | 12 -> ValueSome Signal.SIGUSR2
            | 13 -> ValueSome Signal.SIGPIPE
            | 14 -> ValueSome Signal.SIGALRM
            | 15 -> ValueSome Signal.SIGTERM
            | 16 -> ValueSome Signal.SIGSTKFLT
            | 17 -> ValueSome Signal.SIGCHLD
            | 18 -> ValueSome Signal.SIGCONT
            | 19 -> ValueSome Signal.SIGSTOP
            | 20 -> ValueSome Signal.SIGTSTP
            | 21 -> ValueSome Signal.SIGTTIN
            | 22 -> ValueSome Signal.SIGTTOU
            | 23 -> ValueSome Signal.SIGURG
            | 24 -> ValueSome Signal.SIGXCPU
            | 25 -> ValueSome Signal.SIGXFSZ
            | 26 -> ValueSome Signal.SIGVTALRM
            | 27 -> ValueSome Signal.SIGPROF
            | 28 -> ValueSome Signal.SIGWINCH
            | 29 -> ValueSome Signal.SIGIO
            | 30 -> ValueSome Signal.SIGPWR
            | 31 -> ValueSome Signal.SIGSYS
            | _ ->
                if signo >= lowestRealTimeSigno && signo <= highestSignoUnder numbering then
                    ValueSome (Signal.RealTime (signo - lowestRealTimeSigno))
                else
                    ValueNone
        | SignalNumbering.Darwin ->
            match signo with
            | 1 -> ValueSome Signal.SIGHUP
            | 2 -> ValueSome Signal.SIGINT
            | 3 -> ValueSome Signal.SIGQUIT
            | 4 -> ValueSome Signal.SIGILL
            | 5 -> ValueSome Signal.SIGTRAP
            | 6 -> ValueSome Signal.SIGABRT
            | 7 -> ValueSome Signal.SIGEMT
            | 8 -> ValueSome Signal.SIGFPE
            | 9 -> ValueSome Signal.SIGKILL
            | 10 -> ValueSome Signal.SIGBUS
            | 11 -> ValueSome Signal.SIGSEGV
            | 12 -> ValueSome Signal.SIGSYS
            | 13 -> ValueSome Signal.SIGPIPE
            | 14 -> ValueSome Signal.SIGALRM
            | 15 -> ValueSome Signal.SIGTERM
            | 16 -> ValueSome Signal.SIGURG
            | 17 -> ValueSome Signal.SIGSTOP
            | 18 -> ValueSome Signal.SIGTSTP
            | 19 -> ValueSome Signal.SIGCONT
            | 20 -> ValueSome Signal.SIGCHLD
            | 21 -> ValueSome Signal.SIGTTIN
            | 22 -> ValueSome Signal.SIGTTOU
            | 23 -> ValueSome Signal.SIGIO
            | 24 -> ValueSome Signal.SIGXCPU
            | 25 -> ValueSome Signal.SIGXFSZ
            | 26 -> ValueSome Signal.SIGVTALRM
            | 27 -> ValueSome Signal.SIGPROF
            | 28 -> ValueSome Signal.SIGWINCH
            | 29 -> ValueSome Signal.SIGINFO
            | 30 -> ValueSome Signal.SIGUSR1
            | 31 -> ValueSome Signal.SIGUSR2
            | _ -> ValueNone

    /// <summary>
    /// Whether <c>sigaction(2)</c> refuses to install a handler for this
    /// signal, with <c>EINVAL</c>.
    /// </summary>
    /// <remarks>
    /// <c>SIGKILL</c> and <c>SIGSTOP</c> on both, as POSIX requires: 9 and 19
    /// on Linux, 9 and 17 on Darwin. Linux additionally refuses 32 and 33
    /// (<c>RealTime 0</c> and <c>RealTime 1</c>),
    /// which are not the kernel's doing but glibc's: its <c>sigaction</c>
    /// wrapper screens out <c>SIGCANCEL</c> and <c>SIGSETXID</c>, which it
    /// reserves for its own thread machinery. Measured on Linux 6.18.5 /
    /// glibc 2.41 and Darwin 25.6.0 by installing <c>SIG_DFL</c> for every
    /// number up to <c>NSIG + 1</c>; these were the only refusals below the
    /// ceiling.
    /// </remarks>
    let isUncatchableUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        match signal with
        | Signal.SIGKILL
        | Signal.SIGSTOP -> true
        | Signal.RealTime offset ->
            match numbering with
            | SignalNumbering.Linux -> offset = 0 || offset = 1
            | SignalNumbering.Darwin -> false
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGILL
        | Signal.SIGTRAP
        | Signal.SIGABRT
        | Signal.SIGBUS
        | Signal.SIGFPE
        | Signal.SIGUSR1
        | Signal.SIGSEGV
        | Signal.SIGUSR2
        | Signal.SIGPIPE
        | Signal.SIGALRM
        | Signal.SIGTERM
        | Signal.SIGSTKFLT
        | Signal.SIGCHLD
        | Signal.SIGCONT
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU
        | Signal.SIGURG
        | Signal.SIGXCPU
        | Signal.SIGXFSZ
        | Signal.SIGVTALRM
        | Signal.SIGPROF
        | Signal.SIGWINCH
        | Signal.SIGIO
        | Signal.SIGPWR
        | Signal.SIGSYS
        | Signal.SIGEMT
        | Signal.SIGINFO -> false

    /// Whether a thread's attempt to add this signal to its own signal mask is
    /// silently ignored: `sigprocmask(2)` and `pthread_sigmask(3)` succeed but
    /// leave the signal out of the resulting mask. Note the shape, which is not
    /// `sigaction`'s EINVAL refusal: the mask calls report success, so a model
    /// of `block` drops the signal silently rather than failing.
    ///
    /// SIGKILL and SIGSTOP on both flavours, as POSIX requires. On Linux,
    /// glibc's wrappers additionally screen 32 and 33 (SIGCANCEL and
    /// SIGSETXID, reserved for its thread machinery) out of every set they are
    /// handed.
    let isUnblockableUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        // Measured on Linux 6.18.5 / glibc 2.41 and Darwin 25.6.0 by setting
        // each signo's bit directly (bypassing sigaddset's own screening),
        // blocking, and reading the mask back, for every number up to past the
        // ceiling, through pthread_sigmask, sigprocmask and (Linux) the raw
        // rt_sigprocmask syscall. The raw syscall accepts 32 and 33 and
        // refuses only SIGKILL and SIGSTOP, so that pair is glibc's screening,
        // not the kernel's. The same sets as `isUncatchableUnder`, as it
        // happens — but those are different facts (what sigaction refuses
        // loudly, and what the mask calls drop silently), separately measured,
        // with no reason they must stay in step.
        match signal with
        | Signal.SIGKILL
        | Signal.SIGSTOP -> true
        | Signal.RealTime offset ->
            match numbering with
            | SignalNumbering.Linux -> offset = 0 || offset = 1
            | SignalNumbering.Darwin -> false
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGILL
        | Signal.SIGTRAP
        | Signal.SIGABRT
        | Signal.SIGBUS
        | Signal.SIGFPE
        | Signal.SIGUSR1
        | Signal.SIGSEGV
        | Signal.SIGUSR2
        | Signal.SIGPIPE
        | Signal.SIGALRM
        | Signal.SIGTERM
        | Signal.SIGSTKFLT
        | Signal.SIGCHLD
        | Signal.SIGCONT
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU
        | Signal.SIGURG
        | Signal.SIGXCPU
        | Signal.SIGXFSZ
        | Signal.SIGVTALRM
        | Signal.SIGPROF
        | Signal.SIGWINCH
        | Signal.SIGIO
        | Signal.SIGPWR
        | Signal.SIGSYS
        | Signal.SIGEMT
        | Signal.SIGINFO -> false

    /// Whether repeated generation of this signal queues multiple pending
    /// instances (a real-time signal), rather than coalescing into at most one
    /// per pending set (a standard signal).
    ///
    /// Linux's real-time signals are signos 32 to 64; Darwin has none at all.
    /// A signal the numbering does not have is not a real-time signal either.
    let isRealTimeUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        // Measured on Linux 6.18.5 / glibc 2.41: three generations of
        // SIGRTMIN+2 (glibc's 34+2 = 36) while blocked deliver three times,
        // via sigqueue and via kill alike, where three of SIGUSR1 deliver
        // once; and on Darwin 25.6.0, where SIGUSR1 likewise delivers once.
        // The range's lower end is the kernel's own threshold (its
        // legacy_queue test is `sig < SIGRTMIN` with the kernel's SIGRTMIN of
        // 32); its exact position is not observable through glibc, whose
        // wrappers screen 32 and 33 — its reserved pair — out of every mask
        // and sigaction, so those two rows follow the kernel's rule rather
        // than a measurement.
        match signal with
        | Signal.RealTime _ -> existsUnder numbering signal
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGILL
        | Signal.SIGTRAP
        | Signal.SIGABRT
        | Signal.SIGBUS
        | Signal.SIGFPE
        | Signal.SIGKILL
        | Signal.SIGUSR1
        | Signal.SIGSEGV
        | Signal.SIGUSR2
        | Signal.SIGPIPE
        | Signal.SIGALRM
        | Signal.SIGTERM
        | Signal.SIGSTKFLT
        | Signal.SIGCHLD
        | Signal.SIGCONT
        | Signal.SIGSTOP
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU
        | Signal.SIGURG
        | Signal.SIGXCPU
        | Signal.SIGXFSZ
        | Signal.SIGVTALRM
        | Signal.SIGPROF
        | Signal.SIGWINCH
        | Signal.SIGIO
        | Signal.SIGPWR
        | Signal.SIGSYS
        | Signal.SIGEMT
        | Signal.SIGINFO -> false

    /// Whether a signal generated while its disposition is "ignore" (SIG_IGN,
    /// or SIG_DFL with a default of Ignore) survives as pending when the
    /// receiving thread is blocking it: on Linux it stays pending, and is
    /// delivered if a handler is installed before the unblock, where Darwin
    /// discards it at generation despite the block, SIGCONT alone excepted.
    /// A signal that is ignored and *not* blocked is discarded on both.
    let blockedIgnoredSignalStaysPendingUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        // Measured 2026-09-16 on Linux 6.18.5 / glibc 2.41 and Darwin 25.6.0,
        // two runs each: SIG_IGN'd SIGUSR1 and default-ignored SIGWINCH,
        // generated both process-directed (kill) and thread-directed
        // (pthread_kill) while blocked. On Linux all four shapes read back
        // from sigpending and were delivered to a handler installed before
        // the unblock; on Darwin none was pending and none was delivered. A
        // SIG_DFL SIGUSR2 control stayed pending and delivered on both.
        //
        // Swept 2026-09-26 over every signal sigaction accepts, under SIG_IGN
        // and SIG_DFL at generation, both directions, on Linux 6.18.5 and
        // Darwin 27.0.0
        // (docs/plans/2026-08-23-posix-kernel-extraction/signal-disposition-table.c):
        // the same, except that Darwin keeps an ignored SIGCONT pending.
        //
        // Not measurable as a host-equality test: asking means installing
        // dispositions in the test host's own process, which is why the
        // sigaction facts in this file are probe-pinned too (see
        // TestSignalAgainstHost's header).
        match numbering with
        | SignalNumbering.Linux -> true
        | SignalNumbering.Darwin -> signal = Signal.SIGCONT

    /// Whether the kernel's default action for `signal` dumps core as well as
    /// terminating the process. The dump is written only if the process's
    /// settings allow it (see `CoreDumps`); either way the process dies.
    ///
    /// Linux: SIGQUIT, SIGILL, SIGTRAP, SIGABRT, SIGBUS, SIGFPE, SIGSEGV,
    /// SIGXCPU, SIGXFSZ and SIGSYS. Darwin: SIGQUIT, SIGILL, SIGTRAP,
    /// SIGABRT, SIGEMT, SIGFPE, SIGBUS, SIGSEGV and SIGSYS, which is to say
    /// not SIGXCPU or SIGXFSZ.
    let dumpsCoreUnder (numbering : SignalNumbering) (signal : Signal) : bool =
        // Measured 2026-09-26 by sending every signal number to a child under
        // SIG_DFL (docs/plans/2026-08-23-posix-kernel-extraction/core-class.c).
        // Linux 6.18.5, with RLIMIT_CORE raised to infinity: exactly these
        // deaths carried the wait status's core flag, as they did on
        // 2026-09-23 in the signals research's own sweep. Darwin 27.0.0, as
        // an unprivileged user who cannot write the dump into /cores: exactly
        // these deaths raised EXC_CRASH on the child's task exception port,
        // which xnu does for a death by a signal whose properties include
        // SA_CORE, the property its core dump is gated on.
        match signal with
        | Signal.SIGQUIT
        | Signal.SIGILL
        | Signal.SIGTRAP
        | Signal.SIGABRT
        | Signal.SIGBUS
        | Signal.SIGFPE
        | Signal.SIGSEGV
        | Signal.SIGSYS
        | Signal.SIGEMT -> true
        | Signal.SIGXCPU
        | Signal.SIGXFSZ ->
            match numbering with
            | SignalNumbering.Linux -> true
            | SignalNumbering.Darwin -> false
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGKILL
        | Signal.SIGUSR1
        | Signal.SIGUSR2
        | Signal.SIGPIPE
        | Signal.SIGALRM
        | Signal.SIGTERM
        | Signal.SIGSTKFLT
        | Signal.SIGCHLD
        | Signal.SIGCONT
        | Signal.SIGSTOP
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU
        | Signal.SIGURG
        | Signal.SIGVTALRM
        | Signal.SIGPROF
        | Signal.SIGWINCH
        | Signal.SIGIO
        | Signal.SIGPWR
        | Signal.SIGINFO
        | Signal.RealTime _ -> false

    /// <summary>
    /// The kernel-level default disposition for <c>signal</c>, read under the
    /// chosen platform's numbering.
    /// </summary>
    /// <remarks>
    /// Only <c>SIGIO</c>'s differs between the platforms: Linux's terminates,
    /// and Darwin's is discarded. Measured on Linux
    /// 6.18.5 and Darwin 25.6.0 by having a forked child raise each signal on
    /// itself under <c>SIG_DFL</c>.
    ///
    /// Signals the measurement could not classify — <c>SIGTSTP</c>,
    /// <c>SIGTTIN</c> and <c>SIGTTOU</c>, which both kernels discard rather
    /// than stop on when the process group is orphaned, as the probe's was —
    /// take POSIX's <c>Stop</c>. Anything else terminates, which is the POSIX
    /// default for a signal not otherwise specified.
    /// </remarks>
    let defaultDispositionUnder (numbering : SignalNumbering) (signal : Signal) : DefaultDisposition =
        match signal with
        | Signal.SIGCHLD
        | Signal.SIGWINCH
        | Signal.SIGURG
        | Signal.SIGINFO -> DefaultDisposition.Ignore
        | Signal.SIGIO ->
            match numbering with
            | SignalNumbering.Linux -> DefaultDisposition.Terminate
            | SignalNumbering.Darwin -> DefaultDisposition.Ignore
        | Signal.SIGCONT -> DefaultDisposition.Continue
        | Signal.SIGSTOP
        | Signal.SIGTSTP
        | Signal.SIGTTIN
        | Signal.SIGTTOU -> DefaultDisposition.Stop
        | Signal.SIGHUP
        | Signal.SIGINT
        | Signal.SIGQUIT
        | Signal.SIGILL
        | Signal.SIGTRAP
        | Signal.SIGABRT
        | Signal.SIGBUS
        | Signal.SIGFPE
        | Signal.SIGKILL
        | Signal.SIGUSR1
        | Signal.SIGSEGV
        | Signal.SIGUSR2
        | Signal.SIGPIPE
        | Signal.SIGALRM
        | Signal.SIGTERM
        | Signal.SIGSTKFLT
        | Signal.SIGXCPU
        | Signal.SIGXFSZ
        | Signal.SIGVTALRM
        | Signal.SIGPROF
        | Signal.SIGPWR
        | Signal.SIGSYS
        | Signal.SIGEMT
        | Signal.RealTime _ -> DefaultDisposition.Terminate

/// Why `SignalMask.ofWord` refuses a word.
[<RequireQualifiedAccess>]
type SignalMaskRefusal =
    /// Contradictory. `word` sets a bit at or above `sigsetBits`, the width of
    /// the numbering's `sigset_t`, so no signal set of that flavour can hold
    /// it: Darwin's `sigset_t` is 32 bits, where Linux's kernel set is 64.
    | WiderThanSigset of word : uint64 * sigsetBits : int

[<RequireQualifiedAccess>]
module SignalMaskRefusal =
    /// A human-readable account of why the word was refused.
    let describe (refusal : SignalMaskRefusal) : string =
        match refusal with
        | SignalMaskRefusal.WiderThanSigset (word, sigsetBits) ->
            $"the signal set 0x%x{word} sets a bit at or above bit %d{sigsetBits}, and this flavour's sigset_t is only %d{sigsetBits} bits wide"

/// <summary>
/// A signal set as a kernel holds it: the bits of a <c>sigset_t</c>, in which
/// bit <c>n - 1</c> is signal number <c>n</c>, read under one
/// <c>SignalNumbering</c>.
/// </summary>
/// <remarks>
/// A set of <c>Signal</c>s is not enough, because a kernel holds a bit that
/// names no signal: Darwin's <c>sigset_t</c> is 32 bits and its highest signal
/// is 31, and Darwin 27.0.0 stores bit 31 (signal number 32) in a thread's mask
/// and in a handler's <c>sa_mask</c>, and hands it back
/// (<c>docs/plans/2026-08-23-posix-kernel-extraction/sigprocmask-ops.c</c> and
/// <c>sigaction-mask-bits.c</c>). Darwin's <c>sigfillset</c> sets every bit,
/// so an ordinary <c>sigfillset</c> and <c>SIG_SETMASK</c> sets it. Every bit
/// of Linux's 64 names a signal.
///
/// Opaque: made by <c>SignalMask.ofWord</c> or <c>SignalMask.ofSignals</c>,
/// and read by <c>SignalMask.signals</c> and <c>SignalMask.toWord</c>. The empty
/// mask is the same under every numbering, so <c>SignalMask.empty</c> needs
/// none; any other mask remembers the numbering it was made under, and the
/// operations that combine two masks fail loudly if theirs differ.
///
/// A mask may name SIGKILL and SIGSTOP. A kernel drops both when it stores a
/// mask, which <c>SignalState</c> does; this type does not.
/// </remarks>
[<RequireQualifiedAccess>]
[<StructuredFormatDisplay("{Display}")>]
type SignalMask =
    internal
    | Empty
    /// `word` is not zero, and sets no bit at or above the numbering's
    /// `sigset_t` width.
    | Under of numbering : SignalNumbering * word : uint64

    /// The signals the mask names, and the bits it sets that name none.
    member this.Display : string =
        match this with
        | SignalMask.Empty -> "{}"
        | SignalMask.Under (numbering, word) ->
            let named =
                [
                    for bit in 0..63 do
                        if word &&& (1UL <<< bit) <> 0UL then
                            match Signal.ofRawSignoUnder numbering (bit + 1) with
                            | ValueSome signal -> yield Choice1Of2 signal
                            | ValueNone -> yield Choice2Of2 (bit + 1)
                ]

            let parts =
                named
                |> List.map (fun part ->
                    match part with
                    | Choice1Of2 signal -> string<Signal> signal
                    | Choice2Of2 signo -> $"unnamed signal number %d{signo}"
                )

            "{" + String.concat ", " parts + "} under " + string<SignalNumbering> numbering

    override this.ToString () = this.Display

[<RequireQualifiedAccess>]
module SignalMask =

    /// How many bits the numbering's `sigset_t` holds: Linux's kernel set is
    /// 64 bits (`rt_sigprocmask`'s `sigsetsize` is 8), Darwin's `sigset_t` 32.
    let private sigsetBits (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 64
        | SignalNumbering.Darwin -> 32

    /// The mask that blocks nothing, under every numbering.
    let empty : SignalMask = SignalMask.Empty

    /// The mask whose bits are `word`, read under `numbering`: bit `n - 1` is
    /// signal number `n`. A bit that names no signal is kept (Darwin's bit 31,
    /// see `SignalMask`).
    ///
    /// Refuses a word wider than the numbering's `sigset_t`: on Darwin, any
    /// bit from 32 up.
    let ofWord (numbering : SignalNumbering) (word : uint64) : Result<SignalMask, SignalMaskRefusal> =
        let bits = sigsetBits numbering

        if bits < 64 && word >>> bits <> 0UL then
            Error (SignalMaskRefusal.WiderThanSigset (word, bits))
        elif word = 0UL then
            Ok SignalMask.Empty
        else
            Ok (SignalMask.Under (numbering, word))

    /// The mask naming exactly `signals`, under `numbering`.
    ///
    /// Fails loudly on a signal the numbering does not have (see
    /// `Signal.existsUnder`), which a client can build only by hand: one read
    /// from a signal number went through `Signal.ofRawSignoUnder`, which
    /// refuses those.
    let ofSignals (numbering : SignalNumbering) (signals : Set<Signal>) : SignalMask =
        let word =
            (0UL, signals)
            ||> Set.fold (fun word signal ->
                if not (Signal.existsUnder numbering signal) then
                    failwith
                        $"SignalMask.ofSignals: %O{signal} is not a signal under the %O{numbering} numbering, so no mask there can name it."

                word ||| (1UL <<< (Signal.toRawSignoUnder numbering signal - 1))
            )

        if word = 0UL then
            SignalMask.Empty
        else
            SignalMask.Under (numbering, word)

    /// The mask's bits as its numbering's `sigset_t` holds them: bit `n - 1`
    /// is signal number `n`. Zero for the empty mask.
    let toWord (mask : SignalMask) : uint64 =
        match mask with
        | SignalMask.Empty -> 0UL
        | SignalMask.Under (_, word) -> word

    /// The signals the mask names. A bit that names no signal (Darwin's bit 31)
    /// is left out: only `toWord` shows it.
    let signals (mask : SignalMask) : Set<Signal> =
        match mask with
        | SignalMask.Empty -> Set.empty
        | SignalMask.Under (numbering, word) ->
            seq {
                for bit in 0..63 do
                    if word &&& (1UL <<< bit) <> 0UL then
                        match Signal.ofRawSignoUnder numbering (bit + 1) with
                        | ValueSome signal -> yield signal
                        | ValueNone -> ()
            }
            |> Set.ofSeq

    /// Whether the mask names `signal`. False for a signal its numbering does
    /// not have.
    let contains (signal : Signal) (mask : SignalMask) : bool =
        match mask with
        | SignalMask.Empty -> false
        | SignalMask.Under (numbering, word) ->
            Signal.existsUnder numbering signal
            && word &&& (1UL <<< (Signal.toRawSignoUnder numbering signal - 1)) <> 0UL

    /// Whether the mask sets no bit.
    let isEmpty (mask : SignalMask) : bool =
        match mask with
        | SignalMask.Empty -> true
        | SignalMask.Under _ -> false

    /// The numbering a non-empty mask was made under.
    let internal numbering (mask : SignalMask) : SignalNumbering voption =
        match mask with
        | SignalMask.Empty -> ValueNone
        | SignalMask.Under (numbering, _) -> ValueSome numbering

    let private ofWordUnder (numbering : SignalNumbering) (word : uint64) : SignalMask =
        if word = 0UL then
            SignalMask.Empty
        else
            SignalMask.Under (numbering, word)

    /// The numbering two masks share, failing loudly if they disagree: the
    /// same bit is a different signal under each.
    let private shared (operation : string) (a : SignalMask) (b : SignalMask) : SignalNumbering voption =
        match a, b with
        | SignalMask.Empty, SignalMask.Empty -> ValueNone
        | SignalMask.Under (numbering, _), SignalMask.Empty
        | SignalMask.Empty, SignalMask.Under (numbering, _) -> ValueSome numbering
        | SignalMask.Under (left, _), SignalMask.Under (right, _) ->
            if left <> right then
                failwith
                    $"SignalMask.%s{operation}: a mask under the %O{left} numbering cannot be combined with one under %O{right}."

            ValueSome left

    /// Every bit either mask sets.
    let internal union (a : SignalMask) (b : SignalMask) : SignalMask =
        match shared "union" a b with
        | ValueNone -> SignalMask.Empty
        | ValueSome numbering -> ofWordUnder numbering (toWord a ||| toWord b)

    /// The bits of `a` that `b` does not set.
    let internal difference (a : SignalMask) (b : SignalMask) : SignalMask =
        match shared "difference" a b with
        | ValueNone -> SignalMask.Empty
        | ValueSome numbering -> ofWordUnder numbering (toWord a &&& ~~~(toWord b))

    /// `mask` with `signal`'s bit set, under `numbering`, which must be the
    /// mask's own if it is not empty.
    let internal add (numbering : SignalNumbering) (signal : Signal) (mask : SignalMask) : SignalMask =
        union mask (ofSignals numbering (Set.singleton signal))

    /// `mask` without the bit of any of `signals` its numbering has.
    let internal without (signals : Set<Signal>) (mask : SignalMask) : SignalMask =
        match mask with
        | SignalMask.Empty -> SignalMask.Empty
        | SignalMask.Under (numbering, _) ->
            let present = signals |> Set.filter (Signal.existsUnder numbering)
            difference mask (ofSignals numbering present)
