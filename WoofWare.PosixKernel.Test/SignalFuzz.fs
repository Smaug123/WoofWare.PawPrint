namespace WoofWare.PosixKernel.Test

open System
open WoofWare.PosixKernel

/// One operation of the signal fuzzer's op language, which `signalFuzz/harness.c`
/// runs on a real kernel and `SignalFuzz.executeEmulated` runs on this library.
/// Every signal is a raw signo under the flavour's numbering.
[<RequireQualifiedAccess>]
type SignalFuzzOp =
    /// `sigaction(signo)`: to `SIG_DFL`, `SIG_IGN`, or the recording handler with
    /// `sa_mask` holding the signos in `mask` and the two flags.
    | Act of signo : int * disposition : SignalFuzzDisposition * mask : Set<int> * noDefer : bool * resetHand : bool
    /// `kill(getpid(), signo)`.
    | Kill of signo : int
    /// `pthread_kill(pthread_self(), signo)`: aimed at the only thread, which is
    /// the leader.
    | Raise of signo : int

/// What an `Act` installs.
and [<RequireQualifiedAccess>] SignalFuzzDisposition =
    | Default
    | Ignore
    | Catch

/// A sequence: the ops run at top level, then the body of each handler
/// invocation in the order the invocations start. An invocation beyond the last
/// body runs no ops.
type SignalFuzzSequence =
    {
        Top : SignalFuzzOp list
        Bodies : SignalFuzzOp list list
    }

/// How the emulated side answered one sequence.
[<RequireQualifiedAccess>]
type SignalFuzzRun =
    /// Every op answered; the transcript is comparable with the harness's.
    | Transcript of string
    /// The library refused with one of its typed refusals: the sequence is
    /// outside the modelled envelope, and the comparison skips it.
    | Refused of message : string

[<RequireQualifiedAccess>]
module SignalFuzz =

    let private flags (noDefer : bool) (resetHand : bool) : int =
        (if noDefer then 1 else 0) ||| (if resetHand then 2 else 0)

    let private maskBits (mask : Set<int>) : uint64 =
        (0UL, mask) ||> Set.fold (fun bits signo -> bits ||| (1UL <<< (signo - 1)))

    let private opText (op : SignalFuzzOp) : string =
        match op with
        | SignalFuzzOp.Act (signo, disposition, mask, noDefer, resetHand) ->
            let h =
                match disposition with
                | SignalFuzzDisposition.Default -> "d"
                | SignalFuzzDisposition.Ignore -> "i"
                | SignalFuzzDisposition.Catch -> "c"

            $"a%d{signo}.%s{h}.%x{maskBits mask}.%d{flags noDefer resetHand}"
        | SignalFuzzOp.Kill signo -> $"k%d{signo}"
        | SignalFuzzOp.Raise signo -> $"r%d{signo}"

    /// The line `harness.c` reads.
    let serialise (sequence : SignalFuzzSequence) : string =
        sequence.Top :: sequence.Bodies
        |> List.map (fun script -> script |> List.map opText |> String.concat ",")
        |> String.concat "|"

    let private parseOp (text : string) : SignalFuzzOp =
        match text.[0] with
        | 'a' ->
            match text.Substring(1).Split '.' with
            | [| signo ; h ; mask ; fl |] ->
                let bits = Convert.ToUInt64 (mask, 16)
                let fl = Int32.Parse fl

                let disposition =
                    match h with
                    | "d" -> SignalFuzzDisposition.Default
                    | "i" -> SignalFuzzDisposition.Ignore
                    | "c" -> SignalFuzzDisposition.Catch
                    | other -> failwith $"no such disposition: %s{other}"

                SignalFuzzOp.Act (
                    Int32.Parse signo,
                    disposition,
                    [ 1..64 ]
                    |> List.filter (fun s -> bits &&& (1UL <<< (s - 1)) <> 0UL)
                    |> Set.ofList,
                    fl &&& 1 <> 0,
                    fl &&& 2 <> 0
                )
            | _ -> failwith $"not an act: %s{text}"
        | 'k' -> SignalFuzzOp.Kill (Int32.Parse (text.Substring 1))
        | 'r' -> SignalFuzzOp.Raise (Int32.Parse (text.Substring 1))
        | _ -> failwith $"no such op: %s{text}"

    /// The inverse of `serialise`.
    let parse (line : string) : SignalFuzzSequence =
        let scripts =
            line.Split '|'
            |> Array.toList
            |> List.map (fun script ->
                script.Split (',', StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList
                |> List.map parseOp
            )

        match scripts with
        | top :: bodies ->
            {
                Top = top
                Bodies = bodies
            }
        | [] -> failwith "an empty sequence"

    /// The signos a sequence may send or catch under `numbering`: catchable
    /// standard signals, the synchronous ones Linux takes first among them, and
    /// on Linux some real-time signals. Left out: SIGALRM, which the harness
    /// arms as its own timeout, and SIGCONT and the stop signals, whose defaults
    /// would stop or continue the child.
    let alphabet (numbering : SignalNumbering) : int list =
        match numbering with
        | SignalNumbering.Linux ->
            [
                1
                2
                3
                4
                5
                6
                7
                8
                10
                11
                12
                13
                15
                17
                23
                28
                31
                34
                35
                40
            ]
        | SignalNumbering.Darwin ->
            [
                1
                2
                3
                4
                5
                6
                7
                8
                10
                11
                12
                13
                15
                16
                20
                23
                28
                29
                30
                31
            ]

    /// Signos a mask may name besides the alphabet: SIGKILL and SIGSTOP, which
    /// the kernel drops from it, and on Linux glibc's reserved 32 and 33, which
    /// the kernel keeps.
    let private maskExtras (numbering : SignalNumbering) : int list =
        match numbering with
        | SignalNumbering.Linux -> [ 9 ; 19 ; 32 ; 33 ]
        | SignalNumbering.Darwin -> [ 9 ; 17 ]

    /// A random sequence.
    let generate (numbering : SignalNumbering) (rng : Random) : SignalFuzzSequence =
        let letters = alphabet numbering
        let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]

        let randomMask () : Set<int> =
            List.init
                (rng.Next 6)
                (fun _ ->
                    if rng.Next 8 = 0 then
                        pick (maskExtras numbering)
                    else
                        pick letters
                )
            |> Set.ofList

        // Most signals are caught up front, and most sends are of a caught
        // one, so that sends reach handlers, pend behind their masks and nest,
        // rather than killing the child at once.
        let caught = letters |> List.filter (fun _ -> rng.Next 5 <> 0)

        let setup =
            caught
            |> List.map (fun signo ->
                SignalFuzzOp.Act (signo, SignalFuzzDisposition.Catch, randomMask (), rng.Next 4 = 0, rng.Next 7 = 0)
            )

        let sent () : int =
            if not caught.IsEmpty && rng.Next 10 <> 0 then
                pick caught
            else
                pick letters

        let op () : SignalFuzzOp =
            match rng.Next 20 with
            | n when n < 5 ->
                let disposition =
                    match rng.Next 10 with
                    | 0 -> SignalFuzzDisposition.Default
                    | 1 -> SignalFuzzDisposition.Ignore
                    | _ -> SignalFuzzDisposition.Catch

                SignalFuzzOp.Act (pick letters, disposition, randomMask (), rng.Next 4 = 0, rng.Next 5 = 0)
            | n when n < 17 -> SignalFuzzOp.Kill (sent ())
            | _ -> SignalFuzzOp.Raise (sent ())

        {
            Top = setup @ List.init (1 + rng.Next 5) (fun _ -> op ())
            Bodies = List.init (rng.Next 12) (fun _ -> List.init (rng.Next 6) (fun _ -> op ()))
        }

    exception private Died of signo : int
    exception private Refusal of string

    /// Run `sequence` on this library, as a one-task process of `platform`, and
    /// render what happened as the harness would.
    let executeEmulated (platform : SimulatedUnixPlatform) (sequence : SignalFuzzSequence) : SignalFuzzRun =
        let numbering = SimulatedUnixPlatform.signalNumbering platform
        let leader = 0
        let events = Collections.Generic.List<string> ()
        let mutable bodies = sequence.Bodies

        let mutable system : UnixSystem<int, unit> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams leader (CpuId 0)

        let self = ProcessId.toInt32 (UnixSystem.processId system)

        let signal (signo : int) : Signal =
            match Signal.ofRawSignoUnder numbering signo with
            | ValueSome signal -> signal
            | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

        let signo (signal : Signal) : int = Signal.toRawSignoUnder numbering signal

        let bits (signals : Set<Signal>) : uint64 =
            (0UL, signals) ||> Set.fold (fun acc s -> acc ||| (1UL <<< (signo s - 1)))

        let signals () = system.Process.Signals

        let withSignals (signals : SignalState<int, unit>) : unit =
            system <-
                { system with
                    Process =
                        { system.Process with
                            Signals = signals
                        }
                }

        let rec returnToUser () : unit =
            match UnixSignal.onReturnToUser leader system with
            | Error refusal -> raise (Refusal $"onReturnToUser refused: %O{refusal}")
            | Ok (None, after) -> system <- after
            | Ok (Some (SignalDelivery.RunHandlers frames), after) ->
                system <- after
                runFrames frames
            | Ok (Some (SignalDelivery.DefaultTerminate (killedBy, _)), _) -> raise (Died (signo killedBy))
            | Ok (Some other, _) -> raise (Refusal $"onReturnToUser answered %A{other}")

        and runFrames (frames : HandlerFrame<int, unit> list) : unit =
            match frames with
            | [] -> ()
            | frame :: outer ->
                let disposition =
                    match SignalState.disposition frame.Entry.Signal (signals ()) with
                    | SignalDisposition.Default -> "d"
                    | SignalDisposition.Ignore -> "i"
                    | SignalDisposition.Catch _ -> "c"

                events.Add $"e%d{signo frame.Entry.Signal}.%x{bits frame.Mask}.%s{disposition}"

                match bodies with
                | body :: rest ->
                    bodies <- rest
                    runScript body
                | [] -> ()

                events.Add "x"
                system <- UnixSignal.sigreturn leader frame.Id system
                returnToUser ()
                runFrames outer

        and runOp (op : SignalFuzzOp) : unit =
            match op with
            | SignalFuzzOp.Act (s, disposition, mask, noDefer, resetHand) ->
                let disposition =
                    match disposition with
                    | SignalFuzzDisposition.Default -> SignalDisposition.Default
                    | SignalFuzzDisposition.Ignore -> SignalDisposition.Ignore
                    | SignalFuzzDisposition.Catch ->
                        SignalDisposition.Catch
                            {
                                Handler = ()
                                Mask = mask |> Set.map signal
                                NoDefer = noDefer
                                ResetHand = resetHand
                                Restart = false
                            }

                withSignals (SignalState.setDisposition (signal s) disposition (signals ()))
            | SignalFuzzOp.Kill s ->
                match UnixSignal.kill self s system with
                | Error refusal -> raise (Refusal $"kill refused: %O{refusal}")
                | Ok (Error errno) ->
                    events.Add
                        $"f%d{UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno}"
                | Ok (Ok (KillOutcome.ProcessContinues after)) -> system <- after
                | Ok (Ok (KillOutcome.ProcessEnded ended)) ->
                    match ended.Termination with
                    | ProcessTermination.Signaled (killedBy, _) -> raise (Died (signo killedBy))
                    | other -> failwith $"kill ended the process with %O{other}"
                | Ok (Ok (KillOutcome.ProcessStopped _)) -> raise (Refusal "a stop signal was sent")
            | SignalFuzzOp.Raise s ->
                match UnixSignal.pthreadKill leader s system with
                | Error refusal -> raise (Refusal $"raise refused: %O{refusal}")
                | Ok (Error errno) ->
                    events.Add
                        $"f%d{UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno}"
                | Ok (Ok (KillOutcome.ProcessContinues after)) -> system <- after
                | Ok (Ok (KillOutcome.ProcessEnded ended)) ->
                    match ended.Termination with
                    | ProcessTermination.Signaled (killedBy, _) -> raise (Died (signo killedBy))
                    | other -> failwith $"raise ended the process with %O{other}"
                | Ok (Ok (KillOutcome.ProcessStopped _)) -> raise (Refusal "a stop signal was raised")

            returnToUser ()

            let pending =
                SignalState.pending (signals ()) |> List.map (fun e -> e.Signal) |> Set.ofList

            events.Add $"p%x{bits pending}"

        and runScript (script : SignalFuzzOp list) : unit =
            for op in script do
                runOp op

        try
            runScript sequence.Top
            events.Add "ok"
            SignalFuzzRun.Transcript (String.Join (" ", events))
        with
        | Died killedBy ->
            events.Add $"died%d{killedBy}"
            SignalFuzzRun.Transcript (String.Join (" ", events))
        | Refusal message -> SignalFuzzRun.Refused message
