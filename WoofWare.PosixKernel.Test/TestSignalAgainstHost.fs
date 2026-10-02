namespace WoofWare.PosixKernel.Test

open System.Diagnostics
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The signo table for the flavour this test process runs on, checked against
/// the machine itself rather than against a literal: `TestSignal` restates both
/// columns, and this is what stops the restatement and the table agreeing with
/// each other while both are wrong.
///
/// The oracle is the shell's `kill -l N`, which prints the name of signal `N`
/// and fails for a number that is not a signal. It is the one thing that is
/// callable from a .NET test host on both platforms and names *every* signal:
/// `SystemNative_GetPlatformSignalNumber` names only the ten with a
/// `PosixSignal` member (and is measured that way in
/// `WoofWare.PawPrint.Test.TestPosixSignalPal`), `strsignal(3)` words its
/// descriptions differently on the two libcs, and `sigabbrev_np(3)` is glibc's
/// alone. macOS's `/bin/sh` is bash and Debian's is dash; both answer `kill -l`
/// the same way for a number below `NSIG`, and both refuse one at or above it.
///
/// Whether `sigaction(2)` refuses a signal is not measured here: the only way
/// to ask is to install a disposition in the test host's own process, and for
/// the signals the runtime handles itself that would remove its handler.
/// `TestSignal` pins that set from a probe instead.
[<TestFixture>]
module TestSignalAgainstHost =

    /// `Some name` when `kill -l signo` names a signal, `None` when the shell
    /// refuses the number. The name is the abbreviation without its `SIG`
    /// prefix, which is how both shells print it.
    let private hostSignalName (signo : int) : string option =
        let info = ProcessStartInfo ("/bin/sh", [| "-c" ; $"kill -l %d{signo}" |])
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.UseShellExecute <- false
        use proc = Process.Start info
        let output = proc.StandardOutput.ReadToEnd().Trim ()
        proc.StandardError.ReadToEnd () |> ignore
        proc.WaitForExit ()
        if proc.ExitCode = 0 then Some output else None

    /// What `kill -l` calls each named signal, or `None` for a real-time
    /// signal, which the shells name relative to the C library's `SIGRTMIN`
    /// (`RTMIN+1`, `RTMAX-2`) or not at all.
    let private abbreviation (signal : Signal) : string option =
        match signal with
        | Signal.SIGHUP -> Some "HUP"
        | Signal.SIGINT -> Some "INT"
        | Signal.SIGQUIT -> Some "QUIT"
        | Signal.SIGILL -> Some "ILL"
        | Signal.SIGTRAP -> Some "TRAP"
        | Signal.SIGABRT -> Some "ABRT"
        | Signal.SIGBUS -> Some "BUS"
        | Signal.SIGFPE -> Some "FPE"
        | Signal.SIGKILL -> Some "KILL"
        | Signal.SIGUSR1 -> Some "USR1"
        | Signal.SIGSEGV -> Some "SEGV"
        | Signal.SIGUSR2 -> Some "USR2"
        | Signal.SIGPIPE -> Some "PIPE"
        | Signal.SIGALRM -> Some "ALRM"
        | Signal.SIGTERM -> Some "TERM"
        | Signal.SIGSTKFLT -> Some "STKFLT"
        | Signal.SIGCHLD -> Some "CHLD"
        | Signal.SIGCONT -> Some "CONT"
        | Signal.SIGSTOP -> Some "STOP"
        | Signal.SIGTSTP -> Some "TSTP"
        | Signal.SIGTTIN -> Some "TTIN"
        | Signal.SIGTTOU -> Some "TTOU"
        | Signal.SIGURG -> Some "URG"
        | Signal.SIGXCPU -> Some "XCPU"
        | Signal.SIGXFSZ -> Some "XFSZ"
        | Signal.SIGVTALRM -> Some "VTALRM"
        | Signal.SIGPROF -> Some "PROF"
        | Signal.SIGWINCH -> Some "WINCH"
        | Signal.SIGIO -> Some "IO"
        | Signal.SIGPWR -> Some "PWR"
        | Signal.SIGSYS -> Some "SYS"
        | Signal.SIGEMT -> Some "EMT"
        | Signal.SIGINFO -> Some "INFO"
        | Signal.RealTime _ -> None

    /// What a shell prints for a number it knows as a signal but has no name
    /// for: dash prints the number itself (its table lacks `STKFLT`, and
    /// glibc's reserved 32 and 33), and bash prints nothing for those two.
    let private unnamedByShell (printed : string) : bool =
        printed = "" || printed |> Seq.forall System.Char.IsDigit

    /// Well past either platform's ceiling, so the sweep sees the shell refuse.
    [<Literal>]
    let private sweepLimit : int = 80

    [<Test>]
    let ``toRawSignoUnder agrees with this host's kill -l about every signal`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            // Both directions: the host's name for each number must be the one
            // this library gives the signal it parses that number as, and the
            // number the host gives each name must be the one this library
            // renders it as. The first catches a wrong or missing row; the
            // second catches a name the host puts somewhere else.
            let hostTable : Map<string, int> =
                [ 1..sweepLimit ]
                |> List.choose (fun signo -> hostSignalName signo |> Option.map (fun name -> name, signo))
                |> List.filter (fun (name, _) -> not (unnamedByShell name))
                |> Map.ofList

            for signo in 1 .. Signal.highestSignoUnder numbering do
                let signal =
                    match Signal.ofRawSignoUnder numbering signo with
                    | ValueSome signal -> signal
                    | ValueNone -> failwith $"%O{numbering}: signo %d{signo} is within range"

                match hostSignalName signo, abbreviation signal with
                | None, _ ->
                    failwith
                        $"%O{numbering}: this host's kill -l refuses signo %d{signo}, which this library calls %O{signal}"
                | Some printed, _ when unnamedByShell printed -> ()
                | Some printed, Some name ->
                    if printed <> name then
                        failwith
                            $"%O{numbering}: this library says signo %d{signo} is %O{signal}, but this host's kill -l calls it %s{printed}"
                | Some printed, None ->
                    if not (printed.StartsWith "RTMIN" || printed.StartsWith "RTMAX") then
                        failwith
                            $"%O{numbering}: this library says signo %d{signo} is %O{signal}, but this host's kill -l calls it %s{printed}"

            for KeyValue (name, host) in hostTable do
                match Signal.ofRawSignoUnder numbering host with
                | ValueSome signal when abbreviation signal = Some name -> ()
                | ValueSome signal when name.StartsWith "RTM" && abbreviation signal = None -> ()
                | other ->
                    failwith
                        $"%O{numbering}: this host's kill -l says SIG%s{name} is %d{host}, which this library parses as %A{other}"
        )

    [<Test>]
    let ``highestSignoUnder is the last number this host's kill -l accepts`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            let modelled = Signal.highestSignoUnder numbering

            let accepted : int list =
                [ 1..sweepLimit ] |> List.filter (fun signo -> (hostSignalName signo).IsSome)

            let hostHighest = List.max accepted

            if hostHighest <> modelled then
                failwith
                    $"%O{numbering}: this library says the highest signo is %d{modelled}, but this host's kill -l accepts up to %d{hostHighest}"

            // And every number below it is a signal, which is what
            // `ofRawSignoUnder`'s range check assumes.
            accepted |> shouldEqual [ 1..hostHighest ]
        )
