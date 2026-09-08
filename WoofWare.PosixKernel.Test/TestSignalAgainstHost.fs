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

    /// What `kill -l` calls each modelled signal.
    let private abbreviation (signal : Signal) : string =
        match signal with
        | Signal.SIGHUP -> "HUP"
        | Signal.SIGINT -> "INT"
        | Signal.SIGQUIT -> "QUIT"
        | Signal.SIGTERM -> "TERM"
        | Signal.SIGCHLD -> "CHLD"
        | Signal.SIGCONT -> "CONT"
        | Signal.SIGWINCH -> "WINCH"
        | Signal.SIGTSTP -> "TSTP"
        | Signal.SIGTTIN -> "TTIN"
        | Signal.SIGTTOU -> "TTOU"
        | Signal.SIGPIPE -> "PIPE"
        | Signal.SIGUSR1 -> "USR1"
        | Signal.SIGUSR2 -> "USR2"
        | Signal.SIGABRT -> "ABRT"
        | Signal.SIGURG -> "URG"
        | Signal.Other raw -> failwith $"Signal.Other %d{raw} has no name to look up"

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

    /// Well past either platform's ceiling, so the sweep sees the shell refuse.
    [<Literal>]
    let private sweepLimit : int = 80

    [<Test>]
    let ``toRawSignoUnder agrees with this host's kill -l about every named signal`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            // Both directions: the number this library gives a name must be
            // what the host calls that name, and the host's name for that
            // number must be the one this library expects. The first catches
            // a wrong row; the second catches two rows swapped.
            let hostTable : Map<string, int> =
                [ 1..sweepLimit ]
                |> List.choose (fun signo -> hostSignalName signo |> Option.map (fun name -> name, signo))
                |> Map.ofList

            for signal in named do
                let modelled = Signal.toRawSignoUnder numbering signal
                let name = abbreviation signal

                match Map.tryFind name hostTable with
                | None -> failwith $"this host's kill -l never printed %s{name} for any signo in 1..%d{sweepLimit}"
                | Some host ->
                    if host <> modelled then
                        failwith
                            $"%O{numbering}: this library says %O{signal} is signo %d{modelled}, but this host's kill -l says SIG%s{name} is %d{host}"

                match hostSignalName modelled with
                | Some hostName when hostName = name -> ()
                | other ->
                    failwith
                        $"%O{numbering}: this library says signo %d{modelled} is %O{signal}, but this host's kill -l calls it %A{other}"
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
