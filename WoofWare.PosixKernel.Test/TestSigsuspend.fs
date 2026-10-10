namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSignal.sigsuspend`, `pause`, `rtSigsuspend` and `finishSigsuspend`:
/// every row of `docs/plans/2026-08-23-posix-kernel-extraction/sigsuspend-mask.c`
/// (measured 2026-10-08 on Linux 6.18.5 aarch64 with glibc 2.41, and Darwin
/// 27.0.0 arm64), replayed from its embedded outputs; a reference model of the
/// masks a task in the call has, over random signal sequences; and the
/// invariants a mask to restore keeps.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSigsuspend =

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

    let private boot (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// The probe's main thread, the process's leader.
    let private main : int = 0

    /// The probe's helper thread, which blocks every signal.
    let private helper : int = 1

    /// `SIG_BLOCK`, `SIG_UNBLOCK` and `SIG_SETMASK` as each `<signal.h>`
    /// numbers them.
    let private how (flavour : SimulatedUnixFlavour) (change : SignalMaskChange) : int =
        let block =
            match flavour with
            | SimulatedUnixFlavour.Linux -> 0
            | SimulatedUnixFlavour.Darwin -> 1

        match change with
        | SignalMaskChange.Block -> block
        | SignalMaskChange.Unblock -> block + 1
        | SignalMaskChange.SetMask -> block + 2

    /// Every bit a `sigset_t` holds.
    let private everyBitWord (flavour : SimulatedUnixFlavour) : uint64 =
        match flavour with
        | SimulatedUnixFlavour.Linux -> UInt64.MaxValue
        | SimulatedUnixFlavour.Darwin -> 0xffffffffUL

    let private maskOfWord (flavour : SimulatedUnixFlavour) (word : uint64) : SignalMask =
        match SignalMask.ofWord (numberingOf flavour) word with
        | Ok mask -> mask
        | Error refusal -> failwith (SignalMaskRefusal.describe refusal)

    // ----------------------- Replaying the probe ----------------------- //

    /// The probe's names for the signals its rows name.
    let private probeName (signal : Signal) : string =
        match signal with
        | Signal.SIGHUP -> "HUP"
        | Signal.SIGINT -> "INT"
        | Signal.SIGQUIT -> "QUIT"
        | Signal.SIGILL -> "ILL"
        | Signal.SIGUSR1 -> "USR1"
        | Signal.SIGUSR2 -> "USR2"
        | Signal.SIGTERM -> "TERM"
        | Signal.SIGURG -> "URG"
        | Signal.SIGCONT -> "CONT"
        | other -> failwith $"the probe names no row after %O{other}"

    /// One replay of a probe row against the model.
    type private Replay =
        {
            Flavour : SimulatedUnixFlavour
            System : UnixSystem<int, string>
            /// What the row's child has printed, latest first.
            Printed : string list
            /// What the row's parent prints ahead of the child's output, latest
            /// first: only the stopcont rows have any.
            Parent : string list
            /// Whether the main thread's call was answered as it was made,
            /// without parking: the probe's `took=<50ms`.
            AnsweredAtOnce : bool
            /// Whether the main thread's call has returned.
            Returned : bool
        }

    /// Why a replay stopped before the row's child exited.
    [<RequireQualifiedAccess>]
    type private Stop =
        /// The process died of this signal, as the replay stood: the row ends
        /// `end=signal<signo>`.
        | Killed of Signal * Replay
        /// The kernel would not answer.
        | Refused of SigsuspendRefusal

    type private Step = Replay -> Result<Replay, Stop>

    let private (>=>) (first : Step) (second : Step) : Step = fun r -> Result.bind second (first r)

    let private nothing : Step = Ok

    let private numbering (r : Replay) : SignalNumbering = numberingOf r.Flavour

    let private signo (r : Replay) (signal : Signal) : int32 =
        Signal.toRawSignoUnder (numbering r) signal

    let private hex (mask : SignalMask) : string = $"%x{SignalMask.toWord mask}"

    let private maskIn (task : int) (system : UnixSystem<int, string>) : SignalMask =
        SignalState.maskOf task system.Process.Signals

    let private print (text : string) : Step =
        fun r ->
            Ok
                { r with
                    Printed = text :: r.Printed
                }

    let private printWith (text : Replay -> string) : Step = fun r -> print (text r) r

    let private signals (list : Signal list) (r : Replay) : SignalMask =
        SignalMask.ofSignals (numbering r) (Set.ofList list)

    /// The probe's `set_current`: the raw per-thread call, which on Linux
    /// keeps 32 and 33.
    let private setMask (task : int) (mask : Replay -> SignalMask) : Step =
        fun r ->
            let set = Some (mask r)
            let setmask = how r.Flavour SignalMaskChange.SetMask

            let result =
                match r.Flavour with
                | SimulatedUnixFlavour.Linux -> UnixSignal.rtSigprocmask task setmask set 8UL r.System
                | SimulatedUnixFlavour.Darwin -> UnixSignal.pthreadSigmask task setmask set r.System

            match result with
            | Ok (_, system) ->
                Ok
                    { r with
                        System = system
                    }
            | Error errno -> failwith $"set_current failed with %O{errno}"

    let private install (signal : Signal) (disposition : SignalDisposition<string>) : Step =
        fun r ->
            match UnixSignal.sigaction (signo r signal) (Some disposition) r.System with
            | Ok (_, system) ->
                Ok
                    { r with
                        System = system
                    }
            | Error errno -> failwith $"sigaction of %O{signal} failed with %O{errno}"

    /// The probe's `catch_with`: a handler with `saMask` and the flags.
    let private catchWith (signal : Signal) (saMask : Signal list) (noDefer : bool) (restart : bool) : Step =
        fun r ->
            let action =
                {
                    Handler = "record"
                    Mask = signals saMask r
                    NoDefer = noDefer
                    ResetHand = false
                    Restart = restart
                }

            install signal (SignalDisposition.Catch action) r

    let private catch (signal : Signal) : Step = catchWith signal [] false false

    /// The main thread's return to user mode from a call that has ended with
    /// `error`: every handler runs, each recorded as the probe's handler
    /// records itself, and then the main thread prints what the probe's does.
    let private mainReturns (error : UnixError) : Step =
        fun r ->
            let rec returnThen
                (continuation :
                    UnixSystem<int, string>
                        -> (Signal * SignalMask * SignalMask) list
                        -> Result<UnixSystem<int, string> * (Signal * SignalMask * SignalMask) list, Stop>)
                (system : UnixSystem<int, string>)
                (ran : (Signal * SignalMask * SignalMask) list)
                : Result<UnixSystem<int, string> * (Signal * SignalMask * SignalMask) list, Stop>
                =
                match UnixSignal.onReturnToUser main system with
                | Error refusal -> failwith $"onReturnToUser refused: %A{refusal}"
                | Ok (ReturnToUserOutcome.Resumes system) -> continuation system ran
                | Ok (ReturnToUserOutcome.RunHandlers (frames, system)) ->
                    runFrames frames system ran
                    |> Result.bind (fun (system, ran) -> continuation system ran)
                | Ok (ReturnToUserOutcome.ProcessEnded ended) ->
                    match EndedProcess.termination ended with
                    | ProcessTermination.Signaled (signal, _) -> Error (Stop.Killed (signal, r))
                    | other -> failwith $"the main thread's return ended the process with %O{other}"
                | Ok (ReturnToUserOutcome.ProcessStopped _ as other)
                | Ok (ReturnToUserOutcome.ContinueDiscarded _ as other) ->
                    failwith $"the main thread's return took %A{other}"

            and runFrames
                (frames : HandlerFrame<int, string> list)
                (system : UnixSystem<int, string>)
                (ran : (Signal * SignalMask * SignalMask) list)
                : Result<UnixSystem<int, string> * (Signal * SignalMask * SignalMask) list, Stop>
                =
                match frames with
                | [] -> Ok (system, ran)
                | frame :: outer ->
                    let seen = frame.Entry.Signal, frame.SavedMask, maskIn main system
                    let system = UnixSignal.sigreturn main frame.Id system
                    returnThen (runFrames outer) system (seen :: ran)

            returnThen (fun system ran -> Ok (system, ran)) r.System []
            |> Result.map (fun (system, ran) ->
                let ran = List.rev ran

                let ranText =
                    match ran with
                    | [] -> "none"
                    | _ ->
                        ran
                        |> List.map (fun (signal, saved, inside) ->
                            $"%s{probeName signal}(saved=%s{hex saved},in=%s{hex inside})"
                        )
                        |> String.concat ","

                let errno =
                    UnixError.toRawErrnoUnder
                        (SimulatedUnixPlatform.rawErrnoNumbering system.Machine.UnixPlatform)
                        error

                let took = if r.AnsweredAtOnce then "<50ms" else ">=50ms"

                let report =
                    $" ret=-1 errno=%d{errno} after=%s{hex (maskIn main system)} at-return=%d{List.length ran} ran=%s{ranText} took=%s{took}"

                { r with
                    System = system
                    Returned = true
                    Printed = report :: r.Printed
                }
            )

    /// Which call the main thread makes.
    [<RequireQualifiedAccess>]
    type private Call =
        /// `sigsuspend` with this mask.
        | Sigsuspend of (Replay -> SignalMask)
        | Pause

    let private suspend (call : Call) : Step =
        fun r ->
            let outcome =
                match call with
                | Call.Sigsuspend mask -> UnixSignal.sigsuspend main (mask r) r.System
                | Call.Pause -> UnixSignal.pause main r.System

            match outcome with
            | Error refusal -> Error (Stop.Refused refusal)
            | Ok (SigsuspendOutcome.Failed error, system) ->
                mainReturns
                    error
                    { r with
                        System = system
                        AnsweredAtOnce = true
                    }
            | Ok (SigsuspendOutcome.WouldBlock _, system) ->
                Ok
                    { r with
                        System = system
                    }

    /// If the main thread is asleep in its call and the system wakes it, it
    /// finishes the call, and returns if the call ends.
    let private wakeIfDue : Step =
        fun r ->
            match UnixTaskTable.parkedFor main r.System.Tasks with
            | None -> Ok r
            | Some ParkedSyscall.SigSuspend ->
                match UnixWait.wakes (Set.singleton main) r.System with
                | [] -> Ok r
                | _ ->
                    match UnixSignal.finishSigsuspend main r.System with
                    | Error refusal -> Error (Stop.Refused refusal)
                    | Ok (SigsuspendOutcome.Failed error, system) ->
                        mainReturns
                            error
                            { r with
                                System = system
                            }
                    | Ok (SigsuspendOutcome.WouldBlock _, system) ->
                        Ok
                            { r with
                                System = system
                            }
            | Some other -> failwith $"the main thread is parked in %A{other}"

    let private afterKill (outcome : Result<KillOutcome<int, string>, UnixError>) : Step =
        fun r ->
            match outcome with
            | Error errno -> failwith $"the send failed with %O{errno}"
            | Ok (KillOutcome.ProcessContinues system)
            | Ok (KillOutcome.ProcessStopped (_, system)) ->
                wakeIfDue
                    { r with
                        System = system
                    }
            | Ok (KillOutcome.ProcessEnded ended) ->
                match ended.Termination with
                | ProcessTermination.Signaled (signal, _) -> Error (Stop.Killed (signal, r))
                | other -> failwith $"the process ended by %O{other}"

    let private kill (r : Replay) (signal : Signal) : Result<KillOutcome<int, string>, UnixError> =
        let pid = ProcessId.toInt32 (UnixSystem.processId r.System)

        match UnixSignal.kill pid (signo r signal) r.System with
        | Ok outcome -> outcome
        | Error refusal -> failwith $"kill was refused: %A{refusal}"

    /// The probe's `send`: `pthread_kill` of the main thread, or `kill` of the
    /// process.
    let private send (toProcess : bool) (signal : Signal) : Step =
        fun r ->
            let outcome =
                if toProcess then
                    kill r signal
                else
                    match UnixSignal.pthreadKill main (signo r signal) r.System with
                    | Ok outcome -> outcome
                    | Error refusal -> failwith $"pthread_kill was refused: %A{refusal}"

            afterKill outcome r

    let private woke (label : string) : Step =
        printWith (fun r -> $" %s{label}=%d{if r.Returned then 1 else 0}")

    let private probeSend (toProcess : bool) (signal : Signal) : Step =
        send toProcess signal >=> woke $"woke-after-%s{probeName signal}"

    let private printMaskOf (label : string) (task : int) : Step =
        printWith (fun r -> $" %s{label}=%s{hex (maskIn task r.System)}")

    let private printPending (label : string) : Step =
        printWith (fun r -> $" %s{label}=%s{hex (UnixSignal.sigpending main r.System)}")

    /// The probe's `with_helper`: a second thread, which blocks every signal.
    let private spawnHelper : Step =
        fun r ->
            match UnixTaskLifecycle.spawn main helper (CpuId 0) r.System with
            | Ok (SpawnAnswer.Spawned _, system) ->
                setMask
                    helper
                    (fun r -> maskOfWord r.Flavour (everyBitWord r.Flavour))
                    { r with
                        System = system
                    }
            | other -> failwith $"spawn: %A{other}"

    /// The probe's kinds of call: `sigsuspend` with the row's mask, or `pause`.
    [<RequireQualifiedAccess>]
    type private Kind =
        | Suspend
        | Pause

    let private call (kind : Kind) (mask : Replay -> SignalMask) : Call =
        match kind with
        | Kind.Suspend -> Call.Sigsuspend mask
        | Kind.Pause -> Call.Pause

    let private during (toProcess : bool) (kind : Kind) : Step =
        catch Signal.SIGUSR1
        >=> catch Signal.SIGUSR2
        >=> catch Signal.SIGHUP
        >=> catch Signal.SIGTERM
        >=> match kind with
            | Kind.Suspend ->
                setMask main (signals [ Signal.SIGUSR1 ; Signal.SIGUSR2 ; Signal.SIGHUP ])
                >=> spawnHelper
                >=> suspend (Call.Sigsuspend (signals [ Signal.SIGUSR2 ; Signal.SIGTERM ]))
                >=> probeSend toProcess Signal.SIGTERM
                >=> probeSend toProcess Signal.SIGUSR2
            | Kind.Pause ->
                setMask main (signals [ Signal.SIGUSR2 ; Signal.SIGHUP ])
                >=> spawnHelper
                >=> suspend Call.Pause
                >=> probeSend toProcess Signal.SIGUSR2
        >=> printMaskOf "helper-mask" helper
        >=> send toProcess Signal.SIGUSR1

    let private handler (kind : Kind) (noDefer : bool) (restart : bool) : Step =
        catchWith Signal.SIGUSR1 [ Signal.SIGQUIT ] noDefer restart
        >=> setMask main (signals [ Signal.SIGUSR2 ; Signal.SIGHUP ])
        >=> spawnHelper
        >=> suspend (call kind (signals [ Signal.SIGINT ]))
        >=> send false Signal.SIGUSR1

    let private pending (toProcess : bool) : Step =
        catch Signal.SIGUSR1
        >=> setMask main (signals [ Signal.SIGUSR1 ; Signal.SIGHUP ])
        >=> send toProcess Signal.SIGUSR1
        >=> printPending "pending-before"
        >=> suspend (Call.Sigsuspend (signals [ Signal.SIGINT ]))

    let private two (toProcess : bool) : Step =
        let sent = [ Signal.SIGTERM ; Signal.SIGUSR1 ; Signal.SIGILL ]

        (nothing, sent) ||> List.fold (fun step signal -> step >=> catch signal)
        >=> setMask main (signals (Signal.SIGHUP :: sent))
        >=> ((nothing, sent)
             ||> List.fold (fun step signal -> step >=> send toProcess signal))
        >=> printPending "pending-before"
        >=> suspend (Call.Sigsuspend (signals [ Signal.SIGINT ]))

    let private ignored (toProcess : bool) (kind : Kind) : Step =
        catch Signal.SIGUSR1
        >=> install Signal.SIGUSR2 SignalDisposition.Ignore
        >=> install Signal.SIGURG SignalDisposition.Default
        >=> setMask main (signals [ Signal.SIGHUP ])
        >=> spawnHelper
        >=> suspend (call kind (signals []))
        >=> probeSend toProcess Signal.SIGUSR2
        >=> probeSend toProcess Signal.SIGURG
        >=> send toProcess Signal.SIGUSR1

    let private stale (signal : Signal) (disposition : SignalDisposition<string>) : Step =
        catch Signal.SIGUSR1
        >=> install signal disposition
        >=> setMask main (signals [ signal ; Signal.SIGHUP ])
        >=> send true signal
        >=> printPending "pending-before"
        >=> spawnHelper
        >=> suspend (Call.Sigsuspend (signals []))
        >=> wakeIfDue
        >=> woke "woke-before-handler"
        >=> catch signal
        >=> wakeIfDue
        >=> woke "woke-after-handler"
        >=> send false Signal.SIGUSR1
        >=> printPending "pending-after"

    let private termDuring (toProcess : bool) : Step =
        catch Signal.SIGUSR1
        >=> setMask main (signals [ Signal.SIGHUP ])
        >=> spawnHelper
        >=> suspend (Call.Sigsuspend (signals []))
        >=> send toProcess Signal.SIGTERM
        >=> print " survived-TERM"
        >=> send toProcess Signal.SIGUSR1

    let private termPendingUnblocked : Step =
        catch Signal.SIGUSR1
        >=> setMask main (signals [ Signal.SIGTERM ])
        >=> send true Signal.SIGTERM
        >=> printPending "pending-before"
        >=> suspend (Call.Sigsuspend (signals []))

    let private killEveryBit : Step =
        catch Signal.SIGUSR1
        >=> setMask main (signals [])
        >=> spawnHelper
        >=> suspend (Call.Sigsuspend (fun r -> maskOfWord r.Flavour (everyBitWord r.Flavour)))
        >=> send false Signal.SIGKILL
        >=> print " survived-KILL"
        >=> send false Signal.SIGUSR1

    let private killstop : Step =
        let everyBitButUsr1 (r : Replay) : SignalMask =
            let usr1 = SignalMask.toWord (signals [ Signal.SIGUSR1 ] r)
            maskOfWord r.Flavour (everyBitWord r.Flavour &&& ~~~usr1)

        catch Signal.SIGUSR1
        >=> setMask main (signals [])
        >=> spawnHelper
        >=> suspend (Call.Sigsuspend everyBitButUsr1)
        >=> send false Signal.SIGUSR1

    let private parent (text : Replay -> string) : Step =
        fun r ->
            Ok
                { r with
                    Parent = text r :: r.Parent
                }

    /// The probe's `stopcont`: the parent stops the child and continues it,
    /// and sends USR1 if the call has not returned.
    let private stopcont (kind : Kind) (contHandler : bool) : Step =
        let stop : Step =
            fun r ->
                match kill r Signal.SIGSTOP with
                | Ok (KillOutcome.ProcessStopped (_, system)) ->
                    parent
                        (fun _ -> " stopped=1")
                        { r with
                            System = system
                        }
                | other -> failwith $"SIGSTOP did not stop the process: %A{other}"

        catch Signal.SIGUSR1
        >=> (if contHandler then catch Signal.SIGCONT else nothing)
        >=> setMask main (signals [ Signal.SIGHUP ])
        >=> suspend (call kind (signals [ Signal.SIGINT ]))
        >=> stop
        >=> send true Signal.SIGCONT
        >=> parent (fun r -> $" returned-after-cont=%d{if r.Returned then 1 else 0}")
        >=> fun r -> if r.Returned then Ok r else send true Signal.SIGUSR1 r

    let private spread (kind : Kind) (change : SignalMaskChange) (set : Signal list) (label : string) : Step =
        let helperCall : Step =
            fun r ->
                match UnixSignal.sigprocmask helper (how r.Flavour change) (Some (signals set r)) r.System with
                | Error refusal ->
                    failwith $"the helper's sigprocmask was refused: %s{SigprocmaskRefusal.describe refusal}"
                | Ok (Ok (_, system)) ->
                    let r =
                        { r with
                            System = system
                        }

                    (printWith (fun r -> $" helper-%s{label}-ret=0 helper-mask=%s{hex (maskIn helper r.System)}")
                     >=> wakeIfDue)
                        r
                | Ok (Error errno) -> failwith $"the helper's sigprocmask failed with %O{errno}"

        catch Signal.SIGUSR1
        >=> setMask main (signals [ Signal.SIGHUP ])
        >=> spawnHelper
        >=> suspend (call kind (signals [ Signal.SIGINT ]))
        >=> helperCall
        >=> send false Signal.SIGUSR1

    let private rtsize (size : uint64) : Step =
        catch Signal.SIGUSR1
        >=> setMask main (signals [ Signal.SIGHUP ])
        >=> fun r ->
            match UnixSignal.rtSigsuspend main (signals [] r) size r.System with
            | Ok (SigsuspendOutcome.Failed error, system) ->
                let errno =
                    UnixError.toRawErrnoUnder
                        (SimulatedUnixPlatform.rawErrnoNumbering system.Machine.UnixPlatform)
                        error

                print
                    $" ret=-1 errno=%d{errno} after=%s{hex (maskIn main system)}"
                    { r with
                        System = system
                    }
            | other -> failwith $"rt_sigsuspend with sigsetsize %d{size}: %A{other}"

    /// Every row the probe prints, by its tag.
    let private rows : (string * Step) list =
        [
            for toProcess, target in [ false, "thread" ; true, "process" ] do
                $"during to=%s{target}", during toProcess Kind.Suspend
                $"pause-during to=%s{target}", during toProcess Kind.Pause
                $"pending to=%s{target}", pending toProcess
                $"two to=%s{target}", two toProcess
                $"ignored to=%s{target}", ignored toProcess Kind.Suspend
                $"term during to=%s{target}", termDuring toProcess
            "pause-ignored to=thread", ignored false Kind.Pause
            for flags, noDefer, restart in [ "0", false, false ; "NODEFER", true, false ; "RESTART", false, true ] do
                $"handler flags=%s{flags}", handler Kind.Suspend noDefer restart
                $"pause-handler flags=%s{flags}", handler Kind.Pause noDefer restart
            "stale URG SIG_DFL", stale Signal.SIGURG SignalDisposition.Default
            "stale USR2 SIG_IGN", stale Signal.SIGUSR2 SignalDisposition.Ignore
            "stale CONT SIG_DFL", stale Signal.SIGCONT SignalDisposition.Default
            "stale CONT SIG_IGN", stale Signal.SIGCONT SignalDisposition.Ignore
            "term pending-unblocked", termPendingUnblocked
            "kill temp=all", killEveryBit
            "killstop temp=all-but-USR1", killstop
            for contHandler in [ 0 ; 1 ] do
                $"stopcont STOP cont-handler=%d{contHandler}", stopcont Kind.Suspend (contHandler = 1)
                $"pause-stopcont STOP cont-handler=%d{contHandler}", stopcont Kind.Pause (contHandler = 1)
            for kind, prefix in [ Kind.Suspend, "spread" ; Kind.Pause, "pause-spread" ] do
                $"%s{prefix} SIG_BLOCK TERM", spread kind SignalMaskChange.Block [ Signal.SIGTERM ] "block"
                $"%s{prefix} SIG_UNBLOCK HUP", spread kind SignalMaskChange.Unblock [ Signal.SIGHUP ] "unblock"
            for size in [ 0UL ; 1UL ; 4UL ; 7UL ; 9UL ; 16UL ; 128UL ] do
                $"rtsize %d{size}", rtsize size
        ]

    /// The rows this library refuses rather than answers, by flavour: Darwin
    /// leaves a pending SIGCONT that the temporary mask unblocks pending
    /// without ending the call, which no wake pulled from the state can say.
    /// One sent during the call, as the stopcont rows' is, is discarded as it
    /// is sent, and answered.
    let private refused : Map<SimulatedUnixFlavour * string, SigsuspendRefusal> =
        [ "stale CONT SIG_DFL" ; "stale CONT SIG_IGN" ]
        |> List.map (fun tag -> (SimulatedUnixFlavour.Darwin, tag), SigsuspendRefusal.DarwinPendingContinue)
        |> Map.ofList

    /// The probe's output on `flavour`, embedded from beside the probe.
    let private probeLines (flavour : SimulatedUnixFlavour) : string list =
        let name =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "sigsuspend-mask.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "sigsuspend-mask.darwin.txt"

        let assembly = Reflection.Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream $"WoofWare.PosixKernel.Test.%s{name}" with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new IO.StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.TrimEnd '\r')
        |> Array.filter (fun line -> not (line.StartsWith '#'))
        |> List.ofArray

    /// The line `step` prints for the row `tag` on `flavour`, or the kernel's
    /// refusal.
    let private replay
        (flavour : SimulatedUnixFlavour)
        (tag : string)
        (step : Step)
        : Result<string, SigsuspendRefusal>
        =
        let start =
            {
                Flavour = flavour
                System = boot flavour
                Printed = []
                Parent = []
                AnsweredAtOnce = false
                Returned = false
            }

        let line (r : Replay) (ending : string) : string =
            tag
            + String.concat "" (List.rev r.Parent)
            + String.concat "" (List.rev r.Printed)
            + ending

        match step start with
        | Ok r ->
            UnixSystem.checkInvariants r.System |> shouldEqual []
            Ok (line r " end=exit0")
        | Error (Stop.Killed (signal, r)) ->
            Ok (line r $" end=signal%d{Signal.toRawSignoUnder (numberingOf flavour) signal}")
        | Error (Stop.Refused refusal) -> Error refusal

    let private replaysExactly (flavour : SimulatedUnixFlavour) : unit =
        let lines = probeLines flavour

        let tagOf (line : string) : string =
            rows
            |> List.map fst
            |> List.filter (fun tag -> line = tag || line.StartsWith (tag + " "))
            |> List.sortByDescending String.length
            |> List.tryHead
            |> Option.defaultWith (fun () -> failwith $"no replay for the probe's row: %s{line}")

        let replayed = lines |> List.map tagOf |> Set.ofList

        // Every replay has its row on this flavour, except the raw call's,
        // which only Linux has.
        let expectedTags =
            rows
            |> List.map fst
            |> List.filter (fun tag -> flavour = SimulatedUnixFlavour.Linux || not (tag.StartsWith "rtsize"))
            |> Set.ofList

        replayed |> shouldEqual expectedTags

        for measured in lines do
            let tag = tagOf measured
            let step = rows |> List.find (fun (t, _) -> t = tag) |> snd

            match replay flavour tag step, Map.tryFind (flavour, tag) refused with
            | Ok line, None -> line |> shouldEqual measured
            | Error refusal, Some expected -> (tag, refusal) |> shouldEqual (tag, expected)
            | other, expected ->
                failwith $"%O{flavour} %s{tag}: replayed %A{other}, expected %A{expected}; measured %s{measured}"

    [<Test>]
    let ``every row of the probe replays exactly, on Linux`` () : unit =
        replaysExactly SimulatedUnixFlavour.Linux

    [<Test>]
    let ``every row of the probe replays exactly, on Darwin`` () : unit =
        replaysExactly SimulatedUnixFlavour.Darwin

    // ----------------------- Reference model ----------------------- //

    /// What a signal in the model's pool does when delivered.
    [<RequireQualifiedAccess>]
    type private ModelDisposition =
        /// Caught, with this `sa_mask` and `SA_NODEFER`.
        | Catch of saMask : Set<Signal> * noDefer : bool
        /// Discarded: `SIG_IGN`, or SIGWINCH's default.
        | Ignore

    /// The main thread as the reference model sees it: its masks as sets of
    /// signals, and the signals pending on it alone.
    type private Model =
        {
            Mask : Set<Signal>
            /// The mask a `sigsuspend` will restore.
            Restore : Set<Signal> option
            Parked : bool
            /// Innermost first: each frame's signal and the mask it saved.
            Frames : (Signal * Set<Signal>) list
            /// A standard signal is pending at most once.
            Pending : Set<Signal>
            Dispositions : Map<Signal, ModelDisposition>
        }

    /// The signals the model sends and installs. None terminates the process:
    /// each is caught or ignored. SIGSEGV is one Linux takes first.
    let private pool : Signal list =
        [
            Signal.SIGHUP
            Signal.SIGINT
            Signal.SIGUSR1
            Signal.SIGUSR2
            Signal.SIGTERM
            Signal.SIGSEGV
            Signal.SIGWINCH
        ]

    let private unmaskable (signal : Signal) : bool =
        signal = Signal.SIGKILL || signal = Signal.SIGSTOP

    /// The measured pick order, written out: Linux takes SIGSEGV (of the pool)
    /// first, then the lowest number; Darwin the lowest number.
    let private pickKey (numbering : SignalNumbering) (signal : Signal) : int * int =
        let signo = Signal.toRawSignoUnder numbering signal

        match numbering with
        | SignalNumbering.Linux when signal = Signal.SIGSEGV -> 0, signo
        | SignalNumbering.Linux -> 1, signo
        | SignalNumbering.Darwin -> 0, signo

    let private modelIgnores (m : Model) (signal : Signal) : bool =
        Map.find signal m.Dispositions = ModelDisposition.Ignore

    /// The model's return to user mode: take the first signal the mask lets
    /// through, again and again, rather than walking the pending signals once.
    /// Answers the frames pushed, innermost first.
    let private modelReturn (numbering : SignalNumbering) (m : Model) : Model * (Signal * Set<Signal>) list =
        let rec go (m : Model) (pushed : (Signal * Set<Signal>) list) : Model * (Signal * Set<Signal>) list =
            let next =
                m.Pending
                |> Set.toList
                |> List.filter (fun signal -> not (Set.contains signal m.Mask))
                |> List.sortBy (pickKey numbering)
                |> List.tryHead

            match next with
            | None -> m, pushed
            | Some signal ->
                let m =
                    { m with
                        Pending = Set.remove signal m.Pending
                    }

                match Map.find signal m.Dispositions with
                | ModelDisposition.Ignore -> go m pushed
                | ModelDisposition.Catch (saMask, noDefer) ->
                    let saved =
                        match pushed, m.Restore with
                        | [], Some restore -> restore
                        | _, _ -> m.Mask

                    let mask =
                        Set.unionMany [ m.Mask ; saMask ; (if noDefer then Set.empty else Set.singleton signal) ]
                        |> Set.filter (unmaskable >> not)

                    let frame = signal, saved

                    go
                        { m with
                            Mask = mask
                            Frames = frame :: m.Frames
                        }
                        (frame :: pushed)

        let after, pushed = go m []

        match after.Restore with
        | None -> after, pushed
        | Some restore when pushed.IsEmpty ->
            { after with
                Mask = restore
                Restore = None
            },
            pushed
        | Some _ ->
            { after with
                Restore = None
            },
            pushed

    /// Whether the main thread, under its mask, could take a caught signal now.
    let private modelHandlerDue (m : Model) : bool =
        m.Pending
        |> Set.exists (fun signal -> not (Set.contains signal m.Mask) && not (modelIgnores m signal))

    let private changed (change : SignalMaskChange) (set : Set<Signal>) (mask : Set<Signal>) : Set<Signal> =
        let set = set |> Set.filter (unmaskable >> not)

        match change with
        | SignalMaskChange.Block -> Set.union mask set
        | SignalMaskChange.Unblock -> Set.difference mask set
        | SignalMaskChange.SetMask -> set

    [<RequireQualifiedAccess>]
    type private ModelOp =
        /// The main thread calls `sigsuspend` with this mask.
        | Suspend of Set<Signal>
        /// The main thread calls `pause`.
        | Pause
        /// The main thread changes its own mask with `pthread_sigmask`.
        | ChangeMask of SignalMaskChange * Set<Signal>
        /// The main thread's innermost handler returns.
        | Return
        /// The helper sends this signal to the main thread.
        | Send of Signal
        /// The helper sets a signal's disposition.
        | SetDisposition of Signal * ModelDisposition
        /// The helper calls the C library's `sigprocmask`, which on Darwin
        /// changes every task's mask, and then blocks every signal again.
        | Spread of SignalMaskChange * Set<Signal>

    /// The paths of the model that its walk must reach.
    [<RequireQualifiedAccess>]
    type private ModelLabel =
        /// A `sigsuspend` or `pause` answered `EINTR` at once.
        | AnsweredAtOnce
        /// A `sigsuspend` or `pause` that slept.
        | Park
        /// A sleeping main thread that a helper's call woke.
        | Wake
        | Pause
        /// A return to user mode under a suspend whose last frame saved a mask
        /// other than the thread's own.
        | RestoredByFrame
        /// A return to user mode under a suspend that pushed several frames.
        | SeveralFramesFromSuspend
        /// A Darwin `sigprocmask` while the main thread slept.
        | SpreadWhileParked
        /// A Darwin `sigprocmask` refused because it would unblock a signal
        /// pending for the main thread.
        | RefusedSpread
        /// A Linux sleep that discarded an ignored signal the temporary mask
        /// let through.
        | StaleDiscard

    let private checkAgainstModel (flavour : SimulatedUnixFlavour) : unit =
        let numbering = numberingOf flavour

        let property (cover : ModelLabel -> unit) (seed : int) : unit =
            let rng = Random seed
            let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]

            let someSignals () : Set<Signal> =
                List.init (rng.Next 4) (fun _ -> pick (Signal.SIGKILL :: Signal.SIGSTOP :: pool))
                |> Set.ofList

            let maskOf (signals : Set<Signal>) : SignalMask = SignalMask.ofSignals numbering signals

            let pickDisposition (signal : Signal) : ModelDisposition =
                if rng.Next 3 = 0 then
                    ModelDisposition.Ignore
                else
                    ModelDisposition.Catch (someSignals () |> Set.filter (unmaskable >> not), rng.Next 3 = 0)

            let install (signal : Signal) (disposition : ModelDisposition) (system : UnixSystem<int, string>) =
                let installed =
                    match disposition with
                    | ModelDisposition.Ignore when signal = Signal.SIGWINCH && rng.Next 2 = 0 ->
                        SignalDisposition.Default
                    | ModelDisposition.Ignore -> SignalDisposition.Ignore
                    | ModelDisposition.Catch (saMask, noDefer) ->
                        SignalDisposition.Catch
                            {
                                Handler = "h"
                                Mask = maskOf saMask
                                NoDefer = noDefer
                                ResetHand = false
                                Restart = rng.Next 2 = 0
                            }

                match UnixSignal.sigaction (Signal.toRawSignoUnder numbering signal) (Some installed) system with
                | Ok (_, system) -> system
                | Error errno -> failwith $"sigaction: %O{errno}"

            let returnToUser (system : UnixSystem<int, string>) : UnixSystem<int, string> =
                match UnixSignal.onReturnToUser main system with
                | Ok (ReturnToUserOutcome.Resumes system)
                | Ok (ReturnToUserOutcome.RunHandlers (_, system)) -> system
                | other -> failwith $"the main thread's return took %A{other}"

            let setmask = how flavour SignalMaskChange.SetMask
            let everyBit = maskOfWord flavour (everyBitWord flavour)

            let withMask (task : int) (mask : SignalMask) (system : UnixSystem<int, string>) =
                match UnixSignal.pthreadSigmask task setmask (Some mask) system with
                | Ok (_, system) -> system
                | Error errno -> failwith $"pthread_sigmask: %O{errno}"

            let dispositions = pool |> List.map (fun signal -> signal, pickDisposition signal)
            let initialMask = someSignals () |> Set.filter (unmaskable >> not)

            let mutable system =
                let booted = boot flavour

                let spawned =
                    match UnixTaskLifecycle.spawn main helper (CpuId 0) booted with
                    | Ok (SpawnAnswer.Spawned _, system) -> system
                    | other -> failwith $"spawn: %A{other}"

                (spawned, dispositions)
                ||> List.fold (fun system (signal, disposition) -> install signal disposition system)
                |> withMask helper everyBit
                |> withMask main (maskOf initialMask)

            let mutable model =
                {
                    Mask = initialMask
                    Restore = None
                    Parked = false
                    Frames = []
                    Pending = Set.empty
                    Dispositions = Map.ofList dispositions
                }

            let agree (step : string) : unit =
                let signals = system.Process.Signals
                let words (mask : Set<Signal>) = SignalMask.toWord (maskOf mask)

                (step, SignalMask.toWord (SignalState.maskOf main signals))
                |> shouldEqual (step, words model.Mask)

                (step, SignalState.maskToRestore main signals |> Option.map SignalMask.toWord)
                |> shouldEqual (step, model.Restore |> Option.map words)

                (step, UnixTaskTable.parkedFor main system.Tasks)
                |> shouldEqual (step, (if model.Parked then Some ParkedSyscall.SigSuspend else None))

                (step,
                 SignalState.framesOf main signals
                 |> List.map (fun frame -> frame.Entry.Signal, SignalMask.signals frame.SavedMask))
                |> shouldEqual (step, model.Frames)

                (step,
                 SignalState.pending signals
                 |> List.map (fun entry -> entry.Target, entry.Signal)
                 |> Set.ofList)
                |> shouldEqual (step, model.Pending |> Set.map (fun signal -> ValueSome main, signal))

                (step, UnixSystem.checkInvariants system) |> shouldEqual (step, [])

            /// The main thread returns to user mode, in both.
            let deliver () : unit =
                let next, pushed = modelReturn numbering model

                if model.Restore.IsSome && List.length pushed > 1 then
                    cover ModelLabel.SeveralFramesFromSuspend

                match List.tryLast pushed, model.Restore with
                | Some (_, saved), Some _ when saved <> model.Mask -> cover ModelLabel.RestoredByFrame
                | _ -> ()

                model <- next
                system <- returnToUser system

            /// After the helper's call: the main thread wakes if it is asleep
            /// and the system wakes it, and otherwise takes what it can at
            /// once, as a kernel delivers at its next return to user mode.
            let afterHelper () : unit =
                if model.Parked then
                    if
                        model.Pending
                        |> Set.exists (fun signal -> not (Set.contains signal model.Mask) && modelIgnores model signal)
                    then
                        failwith "the model holds an ignored signal its sleeping main thread could take"

                    let due = modelHandlerDue model
                    let woken = not (List.isEmpty (UnixWait.wakes (Set.singleton main) system))
                    woken |> shouldEqual due

                    if due then
                        cover ModelLabel.Wake

                        match UnixSignal.finishSigsuspend main system with
                        | Ok (SigsuspendOutcome.Failed UnixError.EINTR, after) ->
                            system <- after

                            model <-
                                { model with
                                    Parked = false
                                }

                            deliver ()
                        | other -> failwith $"finishSigsuspend answered %A{other}"
                else
                    deliver ()

            let suspendWith
                (temporary : Set<Signal>)
                (outcome : Result<SigsuspendOutcome * UnixSystem<int, string>, SigsuspendRefusal>)
                =
                model <-
                    { model with
                        Restore = Some model.Mask
                        Mask = temporary |> Set.filter (unmaskable >> not)
                    }

                match outcome with
                | Ok (SigsuspendOutcome.Failed UnixError.EINTR, after) ->
                    // The return to user mode takes everything, ignored signals
                    // included, in order: a frame pushed first may block an
                    // ignored signal, which then stays pending.
                    modelHandlerDue model |> shouldEqual true
                    cover ModelLabel.AnsweredAtOnce
                    system <- after
                    deliver ()
                | Ok (SigsuspendOutcome.WouldBlock _, after) ->
                    modelHandlerDue model |> shouldEqual false
                    cover ModelLabel.Park
                    system <- after

                    // With no handler due, Linux takes every ignored signal the
                    // temporary mask lets through, discarding it, and restarts
                    // the call. Darwin holds no ignored signal pending.
                    let discarded =
                        model.Pending
                        |> Set.filter (fun signal -> not (Set.contains signal model.Mask) && modelIgnores model signal)

                    if not discarded.IsEmpty then
                        cover ModelLabel.StaleDiscard

                    model <-
                        { model with
                            Parked = true
                            Pending = Set.difference model.Pending discarded
                        }
                | other -> failwith $"sigsuspend answered %A{other}"

            agree "start"

            for step in 1 .. rng.Next (10, 60) do
                let op =
                    let helperOp () : ModelOp =
                        match rng.Next 10 with
                        | 0
                        | 1 ->
                            let signal = pick pool
                            ModelOp.SetDisposition (signal, pickDisposition signal)
                        | 2
                        | 3 ->
                            ModelOp.Spread (
                                pick [ SignalMaskChange.Block ; SignalMaskChange.Unblock ; SignalMaskChange.SetMask ],
                                someSignals ()
                            )
                        | _ -> ModelOp.Send (pick pool)

                    if model.Parked then
                        helperOp ()
                    else
                        match rng.Next 20 with
                        | 0
                        | 1
                        | 2
                        | 3 -> ModelOp.Suspend (someSignals ())
                        | 4 -> ModelOp.Pause
                        | 5
                        | 6 ->
                            ModelOp.ChangeMask (
                                pick [ SignalMaskChange.Block ; SignalMaskChange.Unblock ; SignalMaskChange.SetMask ],
                                someSignals ()
                            )
                        | 7
                        | 8
                        | 9
                        | 10 when not model.Frames.IsEmpty -> ModelOp.Return
                        | _ -> helperOp ()

                match op with
                | ModelOp.Suspend temporary ->
                    suspendWith temporary (UnixSignal.sigsuspend main (maskOf temporary) system)
                | ModelOp.Pause ->
                    cover ModelLabel.Pause
                    suspendWith model.Mask (UnixSignal.pause main system)
                | ModelOp.ChangeMask (change, set) ->
                    match UnixSignal.pthreadSigmask main (how flavour change) (Some (maskOf set)) system with
                    | Ok (_, after) -> system <- after
                    | Error errno -> failwith $"pthread_sigmask: %O{errno}"

                    model <-
                        { model with
                            Mask = changed change set model.Mask
                        }

                    deliver ()
                | ModelOp.Return ->
                    match model.Frames, SignalState.framesOf main system.Process.Signals with
                    | (_, saved) :: outer, innermost :: _ ->
                        system <- UnixSignal.sigreturn main innermost.Id system

                        model <-
                            { model with
                                Mask = saved
                                Frames = outer
                            }

                        deliver ()
                    | modelFrames, frames -> failwith $"a return with frames %A{modelFrames} and %A{frames}"
                | ModelOp.Send signal ->
                    let blocked = Set.contains signal model.Mask

                    let keeps =
                        not (modelIgnores model signal)
                        || (numbering = SignalNumbering.Linux && blocked)

                    match UnixSignal.pthreadKill main (Signal.toRawSignoUnder numbering signal) system with
                    | Ok (Ok (KillOutcome.ProcessContinues after)) -> system <- after
                    | other -> failwith $"pthread_kill answered %A{other}"

                    if keeps then
                        model <-
                            { model with
                                Pending = Set.add signal model.Pending
                            }

                    afterHelper ()
                | ModelOp.SetDisposition (signal, disposition) ->
                    system <- install signal disposition system

                    model <-
                        { model with
                            Dispositions = Map.add signal disposition model.Dispositions
                            Pending =
                                match disposition with
                                | ModelDisposition.Ignore -> Set.remove signal model.Pending
                                | ModelDisposition.Catch _ -> model.Pending
                        }

                    afterHelper ()
                | ModelOp.Spread (change, set) ->
                    if model.Parked && numbering = SignalNumbering.Darwin then
                        cover ModelLabel.SpreadWhileParked

                    // Darwin's spread reaches the main thread's mask, and is
                    // refused where it would unblock a signal pending there
                    // (`SigprocmaskRefusal.DarwinUnblockedForAnotherTask`).
                    let unblockedForMain =
                        match numbering with
                        | SignalNumbering.Darwin ->
                            let after = changed change set model.Mask

                            model.Pending
                            |> Set.filter (fun signal ->
                                Set.contains signal model.Mask && not (Set.contains signal after)
                            )
                        | SignalNumbering.Linux -> Set.empty

                    match UnixSignal.sigprocmask helper (how flavour change) (Some (maskOf set)) system with
                    | Error (SigprocmaskRefusal.DarwinUnblockedForAnotherTask (task, signal)) ->
                        (task, Set.contains signal unblockedForMain) |> shouldEqual (main, true)
                        cover ModelLabel.RefusedSpread
                    | Ok (Ok (_, after)) ->
                        unblockedForMain |> shouldEqual Set.empty
                        system <- withMask helper everyBit after

                        match numbering with
                        | SignalNumbering.Darwin ->
                            model <-
                                { model with
                                    Mask = changed change set model.Mask
                                }
                        | SignalNumbering.Linux -> ()

                        afterHelper ()
                    | Ok (Error errno) -> failwith $"sigprocmask: %O{errno}"

                agree $"step %d{step}, %A{op}"

        // The seed is drawn from the whole range, so that each fresh case walks
        // a fresh sequence; a seed has no meaningful shrink.
        let coverage =
            CoverageSample.check
                (Config.QuickThrowOnFailure.WithMaxTest 500)
                (Arb.fromGen (Gen.choose (0, Int32.MaxValue)))
                property

        // The fixed sample must reach each path often enough for a regression
        // there to surface.
        coverage.Count ModelLabel.AnsweredAtOnce |> shouldBeGreaterThan 50
        coverage.Count ModelLabel.Park |> shouldBeGreaterThan 50
        coverage.Count ModelLabel.Wake |> shouldBeGreaterThan 50
        coverage.Count ModelLabel.Pause |> shouldBeGreaterThan 50
        coverage.Count ModelLabel.RestoredByFrame |> shouldBeGreaterThan 50
        coverage.Count ModelLabel.SeveralFramesFromSuspend |> shouldBeGreaterThan 20

        match numbering with
        | SignalNumbering.Linux -> coverage.Count ModelLabel.StaleDiscard |> shouldBeGreaterThan 30
        | SignalNumbering.Darwin ->
            coverage.Count ModelLabel.SpreadWhileParked |> shouldBeGreaterThan 20
            coverage.Count ModelLabel.RefusedSpread |> shouldBeGreaterThan 20

    [<Test>]
    let ``random signal sequences agree with a reference model of the masks, on Linux`` () : unit =
        checkAgainstModel SimulatedUnixFlavour.Linux

    [<Test>]
    let ``random signal sequences agree with a reference model of the masks, on Darwin`` () : unit =
        checkAgainstModel SimulatedUnixFlavour.Darwin

    // ----------------------- Invariants and lifecycle ----------------------- //

    let private withSignals
        (signals : SignalState<int, string>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    Signals = signals
                }
        }

    let private suspended (task : int) (signals : SignalState<int, string>) : SignalState<int, string> =
        SignalState.suspend task SignalMask.empty signals

    [<Test>]
    let ``a mask to restore for a task the table does not hold is a defect`` () : unit =
        let system = boot SimulatedUnixFlavour.Linux

        system
        |> withSignals (suspended 42 system.Process.Signals)
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.MaskToRestoreWithoutTask 42 ]

        // The control: a task between its call's answer and its return to user
        // mode holds one, parked in nothing.
        system
        |> withSignals (suspended main system.Process.Signals)
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``a mask to restore beside a park in another call is a defect`` () : unit =
        let polling =
            match UnixPoll.poll main [] -1 (boot SimulatedUnixFlavour.Linux) with
            | Ok (PollOutcome.WouldBlock _, system) -> system
            | other -> failwith $"poll: %A{other}"

        let parked =
            UnixTaskTable.parkedFor main polling.Tasks
            |> Option.defaultWith (fun () -> failwith "the poll did not park")

        polling
        |> withSignals (suspended main polling.Process.Signals)
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.MaskToRestoreOutsideSigsuspend (main, parked) ]

        polling |> UnixSystem.checkInvariants |> shouldEqual []

    [<Test>]
    let ``a sigsuspend with no mask to restore is a defect`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let asleep =
                match UnixSignal.sigsuspend main SignalMask.empty (boot flavour) with
                | Ok (SigsuspendOutcome.WouldBlock _, system) -> system
                | other -> failwith $"sigsuspend: %A{other}"

            asleep |> UnixSystem.checkInvariants |> shouldEqual []

            asleep
            |> withSignals (SignalState.initial (numberingOf flavour) Set.empty)
            |> UnixSystem.checkInvariants
            |> shouldEqual [ UnixSystemDefect.SigsuspendWithoutMaskToRestore main ]

    [<Test>]
    let ``a thread's exit and the process's end leave no mask to restore`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let numbering = numberingOf flavour
            let hup = SignalMask.ofSignals numbering (Set.singleton Signal.SIGHUP)
            let usr1 = SignalMask.ofSignals numbering (Set.singleton Signal.SIGUSR1)

            let spawned =
                match UnixTaskLifecycle.spawn main helper (CpuId 0) (boot flavour) with
                | Ok (SpawnAnswer.Spawned _, system) -> system
                | other -> failwith $"spawn: %A{other}"

            let blocking =
                match UnixSignal.pthreadSigmask helper (how flavour SignalMaskChange.SetMask) (Some hup) spawned with
                | Ok (_, system) -> system
                | Error errno -> failwith $"pthread_sigmask: %O{errno}"

            let asleep =
                match UnixSignal.sigsuspend helper usr1 blocking with
                | Ok (SigsuspendOutcome.WouldBlock _, system) -> system
                | other -> failwith $"sigsuspend: %A{other}"

            SignalState.maskToRestore helper asleep.Process.Signals
            |> shouldEqual (Some hup)

            match UnixTaskLifecycle.exitThread helper 0 asleep with
            | Error (ThreadExitRefusal.Parked (task, _)) -> task |> shouldEqual helper
            | other -> failwith $"a parked task's exit: %A{other}"

            let ended = UnixTaskLifecycle.exitGroup main 0 asleep

            SignalState.tasksWithMasksToRestore ended.FinalProcess.Signals
            |> shouldEqual Set.empty

            SignalState.tasksWithMasks ended.FinalProcess.Signals |> shouldEqual Set.empty

            // And `forgetTask` alone, on a task whose call has ended and which has
            // not yet returned to user mode.
            let forgotten =
                SignalState.suspend helper usr1 blocking.Process.Signals
                |> SignalState.forgetTask helper

            SignalState.maskToRestore helper forgotten |> shouldEqual None
            SignalState.tasksWithMasksToRestore forgotten |> shouldEqual Set.empty
            SignalState.maskOf helper forgotten |> shouldEqual SignalMask.empty

    [<Test>]
    let ``SyscallInterruption says a sigsuspend fails with EINTR whatever SA_RESTART says`` () : unit =
        // The probe's "handler flags=RESTART" and "pause-handler flags=RESTART"
        // rows: EINTR under SA_RESTART, on both flavours. The calls' own
        // finishing answers that without asking; this is the classification
        // any other reader of a park gets.
        SyscallInterruption.ruleOf ParkedSyscall.SigSuspend
        |> shouldEqual SignalRestartRule.FailsWithEintr

    [<Test>]
    let ``Darwin: another thread's sigprocmask that would let a pending SIGCONT through to a sigsuspend is refused``
        ()
        : unit
        =
        // Darwin leaves such a SIGCONT pending without ending the call, even
        // once a handler is installed for it (found by review, and reproduced
        // natively), and wakes no sleeper for a signal another thread's
        // sigprocmask unblocks (`unblock-wakes-sleeper.c`). Answered, the
        // unblock would leave a signal this library delivers at the next return
        // to user mode, and wakes the sleeper for, where Darwin does neither.
        for disposition in [ SignalDisposition.Ignore ; SignalDisposition.Default ] do
            let flavour = SimulatedUnixFlavour.Darwin
            let numbering = numberingOf flavour
            let cont = SignalMask.ofSignals numbering (Set.singleton Signal.SIGCONT)
            let contSigno = Signal.toRawSignoUnder numbering Signal.SIGCONT

            let orFail (what : string) (result : Result<'a * UnixSystem<int, string>, 'e>) =
                match result with
                | Ok (_, system) -> system
                | Error e -> failwith $"%s{what}: %A{e}"

            let spawned =
                match UnixTaskLifecycle.spawn main helper (CpuId 0) (boot flavour) with
                | Ok (SpawnAnswer.Spawned _, system) -> system
                | other -> failwith $"spawn: %A{other}"

            let blocking =
                spawned
                |> UnixSignal.sigaction contSigno (Some disposition)
                |> orFail "sigaction"
                |> UnixSignal.pthreadSigmask main (how flavour SignalMaskChange.SetMask) (Some cont)
                |> orFail "pthread_sigmask"
                |> UnixSignal.pthreadSigmask helper (how flavour SignalMaskChange.SetMask) (Some cont)
                |> orFail "pthread_sigmask"

            let sent =
                match UnixSignal.kill (ProcessId.toInt32 (UnixSystem.processId blocking)) contSigno blocking with
                | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
                | other -> failwith $"kill: %A{other}"

            let asleep =
                match UnixSignal.sigsuspend main cont sent with
                | Ok (SigsuspendOutcome.WouldBlock _, system) -> system
                | other -> failwith $"sigsuspend: %A{other}"

            UnixWait.wakes (Set.singleton main) asleep |> shouldEqual []

            match UnixSignal.sigprocmask helper (how flavour SignalMaskChange.Unblock) (Some cont) asleep with
            | Error refusal ->
                refusal
                |> shouldEqual (SigprocmaskRefusal.DarwinUnblockedForAnotherTask (main, Signal.SIGCONT))
            | other -> failwith $"%A{disposition}: sigprocmask answered %A{other}"
