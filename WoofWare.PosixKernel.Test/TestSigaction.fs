namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSignal.sigaction` and `UnixSignal.sigactionSyscall`, held to the rows of
/// `docs/plans/2026-08-23-posix-kernel-extraction/sigaction-sweep.c` (measured on
/// Linux 6.18.5 aarch64 with glibc 2.41, and Darwin 27.0.0 arm64) and of
/// `signal-disposition-table.c`'s part "tr", replayed through the entry point.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSigaction =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private leader : int = 0

    let private systemOn (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour) UnixSystem.pipedStandardStreams leader (CpuId 0)
        |> UnixBootImage.boot

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

    /// Which way a call reaches the kernel: the probe's `libc` and `raw` rows.
    type private Entry =
        | CLibrary
        | Syscall

    let private entries : Entry list = [ Entry.CLibrary ; Entry.Syscall ]

    let private call
        (entry : Entry)
        (signo : int)
        (action : SignalDisposition<string> option)
        (system : UnixSystem<int, string>)
        : Result<SignalDisposition<string> * UnixSystem<int, string>, UnixError>
        =
        match entry with
        | Entry.CLibrary -> UnixSignal.sigaction signo action system
        | Entry.Syscall -> UnixSignal.sigactionSyscall signo action system

    /// The highest signal number, written out rather than taken from
    /// `Signal.highestSignoUnder`, so that this oracle and the implementation
    /// share nothing but the measurement.
    let private highestSigno (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 64
        | SimulatedUnixFlavour.Darwin -> 31

    /// SIGSTOP's number.
    let private sigstop (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 19
        | SimulatedUnixFlavour.Darwin -> 17

    /// How a number answers in the sweep.
    type private Row =
        /// Every call is EINVAL.
        | Refused
        /// A query reports SIG_DFL, and every attempt to install anything is
        /// EINVAL: Linux's SIGKILL and SIGSTOP.
        | DefaultOnly
        /// Every call succeeds.
        | Accepted

    /// The sweep's rows, as the probe printed them.
    let private measured (flavour : SimulatedUnixFlavour) (entry : Entry) (signo : int) : Row =
        if signo < 1 || signo > highestSigno flavour then
            Row.Refused
        else
            match flavour, entry, signo with
            | SimulatedUnixFlavour.Linux, _, (9 | 19) -> Row.DefaultOnly
            | SimulatedUnixFlavour.Linux, Entry.CLibrary, (32 | 33) -> Row.Refused
            | SimulatedUnixFlavour.Linux, _, _ -> Row.Accepted
            | SimulatedUnixFlavour.Darwin, _, (9 | 17) -> Row.Refused
            | SimulatedUnixFlavour.Darwin, _, _ -> Row.Accepted

    let private signoGen : Gen<int> =
        Gen.oneof
            [
                Gen.choose (-3, 70)
                Gen.elements [ Int32.MinValue ; Int32.MinValue + 1 ; Int32.MaxValue ; 128 ; 255 ; 1000 ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    /// A handler action as the probe installs it: `sa_mask` holding every
    /// number from 1 to the highest signal.
    let private probeHandler (flavour : SimulatedUnixFlavour) : SignalDisposition<string> =
        let numbering = numberingOf flavour

        SignalDisposition.Catch
            { SignalCatch.ofHandler "H" with
                Mask =
                    [ 1 .. highestSigno flavour ]
                    |> List.map (fun signo -> Signal.ofRawSignoUnder numbering signo |> ValueOption.get)
                    |> Set.ofList
            }

    /// One call of the probe's sequence, as the probe prints it: `EINVAL`, or
    /// the old action reported.
    let private describe (answer : Result<SignalDisposition<string> * UnixSystem<int, string>, UnixError>) : string =
        match answer with
        | Error errno -> $"%O{errno}"
        | Ok (SignalDisposition.Default, _) -> "DFL"
        | Ok (SignalDisposition.Ignore, _) -> "IGN"
        | Ok (SignalDisposition.Catch action, _) -> action.Handler

    [<Test>]
    let ``sigaction answers the measured rows`` () : unit =
        let extras = [ 65 ; 66 ; 128 ; 1000 ; Int32.MinValue ; Int32.MaxValue ]

        for flavour in flavours do
            for entry in entries do
                for signo in [ -1 .. highestSigno flavour + 2 ] @ extras do
                    let system = systemOn flavour

                    // The probe's sequence: q0, h, q1, i, d, q2. Each call is
                    // made on whatever the previous one left.
                    let steps : (string * SignalDisposition<string> option) list =
                        [
                            "q0", None
                            "h", Some (probeHandler flavour)
                            "q1", None
                            "i", Some SignalDisposition.Ignore
                            "d", Some SignalDisposition.Default
                            "q2", None
                        ]

                    let _, answers =
                        ((system, []), steps)
                        ||> List.fold (fun (system, answers) (label, action) ->
                            let answer = call entry signo action system

                            let system =
                                match answer with
                                | Ok (_, system) -> system
                                | Error _ -> system

                            system, answers @ [ $"%s{label}=%s{describe answer}" ]
                        )

                    let expected =
                        match measured flavour entry signo with
                        | Row.Refused -> [ "q0" ; "h" ; "q1" ; "i" ; "d" ; "q2" ] |> List.map (fun l -> $"%s{l}=EINVAL")
                        | Row.DefaultOnly -> [ "q0=DFL" ; "h=EINVAL" ; "q1=DFL" ; "i=EINVAL" ; "d=EINVAL" ; "q2=DFL" ]
                        | Row.Accepted -> [ "q0=DFL" ; "h=DFL" ; "q1=H" ; "i=H" ; "d=IGN" ; "q2=DFL" ]

                    (flavour, entry, signo, answers)
                    |> shouldEqual (flavour, entry, signo, expected)

    [<Test>]
    let ``the old action's mask is the one installed, less SIGKILL and SIGSTOP`` () : unit =
        // The probe's q1 rows: every number from 1 to the highest signal was
        // installed, and SIGKILL and SIGSTOP were missing on the way back;
        // Linux's 32 and 33 were not.
        for flavour in flavours do
            let numbering = numberingOf flavour

            for entry in entries do
                for signo in [ 1 .. highestSigno flavour ] do
                    if measured flavour entry signo = Row.Accepted then
                        let installed =
                            match call entry signo (Some (probeHandler flavour)) (systemOn flavour) with
                            | Ok (_, system) -> system
                            | Error errno -> failwith $"%O{flavour} %A{entry} %d{signo}: %O{errno}"

                        match call entry signo None installed with
                        | Ok (SignalDisposition.Catch action, after) ->
                            after |> shouldEqual installed

                            let expected =
                                [ 1 .. highestSigno flavour ]
                                |> List.filter (fun s -> s <> 9 && s <> sigstop flavour)
                                |> Set.ofList

                            (flavour, entry, signo, action.Mask |> Set.map (Signal.toRawSignoUnder numbering))
                            |> shouldEqual (flavour, entry, signo, expected)
                        | other -> failwith $"%O{flavour} %A{entry} %d{signo}: expected the handler back, got %A{other}"

    /// What the oracle stores for a number: the disposition installed, with
    /// its mask as raw numbers.
    type private Installed =
        | Dfl
        | Ign
        | Handler of name : string * mask : Set<int>

    let private installedOf (numbering : SignalNumbering) (disposition : SignalDisposition<string>) : Installed =
        match disposition with
        | SignalDisposition.Default -> Installed.Dfl
        | SignalDisposition.Ignore -> Installed.Ign
        | SignalDisposition.Catch action ->
            Installed.Handler (action.Handler, action.Mask |> Set.map (Signal.toRawSignoUnder numbering))

    [<Test>]
    let ``sigaction behaves as a table of dispositions with the measured refusals`` () : unit =
        let actionGen (flavour : SimulatedUnixFlavour) : Gen<SignalDisposition<string> option> =
            let numbering = numberingOf flavour

            let catchGen =
                gen {
                    let! handler = Gen.elements [ "a" ; "b" ]
                    let! mask = Gen.subListOf [ 1 ; 2 ; 9 ; sigstop flavour ; 15 ; 28 ; 31 ]
                    let! noDefer = ArbMap.defaults |> ArbMap.generate<bool>
                    let! resetHand = ArbMap.defaults |> ArbMap.generate<bool>
                    let! restart = ArbMap.defaults |> ArbMap.generate<bool>

                    return
                        SignalDisposition.Catch
                            {
                                Handler = handler
                                Mask =
                                    mask
                                    |> List.map (fun signo -> Signal.ofRawSignoUnder numbering signo |> ValueOption.get)
                                    |> Set.ofList
                                NoDefer = noDefer
                                ResetHand = resetHand
                                Restart = restart
                            }
                }

            Gen.oneof
                [
                    Gen.constant None
                    Gen.constant (Some SignalDisposition.Default)
                    Gen.constant (Some SignalDisposition.Ignore)
                    catchGen |> Gen.map Some
                ]

        let gen =
            gen {
                let! flavour = Gen.elements flavours

                let callGen =
                    gen {
                        let! entry = Gen.elements entries
                        let! signo = Gen.oneof [ signoGen ; Gen.choose (1, highestSigno flavour) ]
                        let! action = actionGen flavour
                        return entry, signo, action
                    }

                let! calls = Gen.listOf callGen
                return flavour, List.truncate 30 calls
            }

        let property (flavour : SimulatedUnixFlavour, calls : (Entry * int * SignalDisposition<string> option) list) =
            let numbering = numberingOf flavour

            let installedNow (oracle : Map<int, Installed>) (signo : int) : Installed =
                Map.tryFind signo oracle |> Option.defaultValue Installed.Dfl

            let finalSystem, finalOracle =
                ((systemOn flavour, Map.empty), calls)
                ||> List.fold (fun (system, oracle) (entry, signo, action) ->
                    let row = measured flavour entry signo
                    let answer = call entry signo action system

                    match row, action, answer with
                    | Row.Refused, _, Error errno
                    | Row.DefaultOnly, Some _, Error errno ->
                        errno |> shouldEqual UnixError.EINVAL
                        system, oracle
                    | Row.DefaultOnly, None, Ok (old, after) ->
                        old |> shouldEqual SignalDisposition.Default
                        after |> shouldEqual system
                        system, oracle
                    | Row.Accepted, None, Ok (old, after) ->
                        installedOf numbering old |> shouldEqual (installedNow oracle signo)
                        after |> shouldEqual system
                        system, oracle
                    | Row.Accepted, Some installed, Ok (old, after) ->
                        installedOf numbering old |> shouldEqual (installedNow oracle signo)

                        let stored =
                            match installedOf numbering installed with
                            | Installed.Handler (name, mask) ->
                                Installed.Handler (name, mask |> Set.remove 9 |> Set.remove (sigstop flavour))
                            | other -> other

                        let oracle =
                            match stored with
                            | Installed.Dfl -> Map.remove signo oracle
                            | _ -> Map.add signo stored oracle

                        after, oracle
                    | _ ->
                        failwith
                            $"%O{flavour} %A{entry} sigaction(%d{signo}, %A{action}): the row is %A{row}, but the answer was %A{answer}"
                )

            // Every number now reads back as the oracle says, through the
            // syscall, which answers for every signal Linux has.
            for signo in [ 1 .. highestSigno flavour ] do
                match UnixSignal.sigactionSyscall signo None finalSystem with
                | Ok (now, _) ->
                    (signo, installedOf numbering now)
                    |> shouldEqual (signo, installedNow finalOracle signo)
                | Error errno ->
                    (signo, measured flavour Entry.Syscall signo)
                    |> shouldEqual (signo, Row.Refused)

                    errno |> shouldEqual UnixError.EINVAL

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    /// A disposition as the "tr" probe names it.
    type private Disposition =
        | DflTr
        | IgnTr
        | HTr
        | H2Tr

    let private toDisposition (d : Disposition) : SignalDisposition<string> =
        match d with
        | Disposition.DflTr -> SignalDisposition.Default
        | Disposition.IgnTr -> SignalDisposition.Ignore
        | Disposition.HTr -> SignalDisposition.Catch (SignalCatch.ofHandler "H")
        | Disposition.H2Tr -> SignalDisposition.Catch (SignalCatch.ofHandler "H2")

    /// Signals whose default is to discard them: Linux CHLD URG WINCH; Darwin
    /// URG CHLD IO WINCH INFO.
    let private defaultIgnored (flavour : SimulatedUnixFlavour) : Set<int> =
        match flavour with
        | SimulatedUnixFlavour.Linux -> Set.ofList [ 17 ; 23 ; 28 ]
        | SimulatedUnixFlavour.Darwin -> Set.ofList [ 16 ; 20 ; 23 ; 28 ; 29 ]

    let private sigcont (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 18
        | SimulatedUnixFlavour.Darwin -> 19

    /// Part "tr", the `after` column: the changes that discarded a pending
    /// instance.
    let private discards (flavour : SimulatedUnixFlavour) (toward : Disposition) (signo : int) : bool =
        match toward with
        | Disposition.IgnTr -> true
        | Disposition.DflTr -> signo = sigcont flavour || Set.contains signo (defaultIgnored flavour)
        | Disposition.HTr
        | Disposition.H2Tr -> false

    [<Test>]
    let ``installing a disposition through sigaction discards a pending instance exactly as measured`` () : unit =
        // The "tr" rows whose signal was pending at generation under a
        // handler: a signal generated while caught is pending on both
        // flavours, so the change alone decides whether it stays.
        for flavour in flavours do
            let numbering = numberingOf flavour

            for signo in [ 1 .. highestSigno flavour ] do
                if measured flavour Entry.CLibrary signo = Row.Accepted && signo <> 27 then
                    let signal = Signal.ofRawSignoUnder numbering signo |> ValueOption.get

                    for threadDirected in [ false ; true ] do
                        for toward in [ Disposition.DflTr ; Disposition.IgnTr ; Disposition.HTr ; Disposition.H2Tr ] do
                            let caught =
                                match
                                    systemOn flavour
                                    |> HandlerFrames.enterIn "carrier" leader (Set.singleton signal)
                                    |> UnixSignal.sigaction signo (Some (toDisposition Disposition.HTr))
                                with
                                | Ok (_, system) -> system
                                | Error errno -> failwith $"%O{flavour} %d{signo}: %O{errno}"

                            let generated =
                                let answer =
                                    if threadDirected then
                                        UnixSignal.pthreadKill leader signo caught |> Result.mapError string
                                    else
                                        UnixSignal.kill (ProcessId.toInt32 (UnixSystem.processId caught)) signo caught
                                        |> Result.mapError string

                                match answer with
                                | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
                                | other -> failwith $"%O{flavour} %d{signo}: generating it answered %A{other}"

                            let isPending (system : UnixSystem<int, string>) : bool =
                                SignalState.pending system.Process.Signals
                                |> List.exists (fun entry -> entry.Signal = signal)

                            isPending generated |> shouldEqual true

                            let changed =
                                match UnixSignal.sigaction signo (Some (toDisposition toward)) generated with
                                | Ok (old, system) ->
                                    old |> shouldEqual (toDisposition Disposition.HTr)
                                    system
                                | Error errno -> failwith $"%O{flavour} %d{signo}: %O{errno}"

                            (flavour, signo, threadDirected, toward, isPending changed)
                            |> shouldEqual (flavour, signo, threadDirected, toward, not (discards flavour toward signo))

    [<Test>]
    let ``a query never changes the system`` () : unit =
        let gen =
            gen {
                let! flavour = Gen.elements flavours
                let! entry = Gen.elements entries
                let! signo = signoGen
                let! pendingSigno = Gen.choose (1, 31)
                return flavour, entry, signo, pendingSigno
            }

        let property (flavour : SimulatedUnixFlavour, entry : Entry, signo : int, pendingSigno : int) =
            let numbering = numberingOf flavour

            // Something pending, so that a query that discarded would show.
            let system =
                match Signal.ofRawSignoUnder numbering pendingSigno with
                | ValueSome signal when pendingSigno <> 9 && pendingSigno <> sigstop flavour && pendingSigno <> 27 ->
                    match
                        systemOn flavour
                        |> HandlerFrames.enterIn "carrier" leader (Set.singleton signal)
                        |> UnixSignal.pthreadKill leader pendingSigno
                    with
                    | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
                    | other -> failwith $"generating %d{pendingSigno}: %A{other}"
                | _ -> systemOn flavour

            match call entry signo None system with
            | Ok (_, after) -> after |> shouldEqual system
            | Error _ -> ()

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)
