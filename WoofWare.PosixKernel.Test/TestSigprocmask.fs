namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSignal.sigprocmask`, `pthreadSigmask`, `rtSigprocmask` and
/// `sigpending`, held to the rows of
/// `docs/plans/2026-08-23-posix-kernel-extraction/sigprocmask-ops.c` and
/// `sigpending-scope.c` (measured 2026-10-08 on Linux 6.18.5 aarch64 with
/// glibc 2.41, and Darwin 27.0.0 arm64), and to a reference model of the mask
/// arithmetic written over raw words.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSigprocmask =

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private systemOn (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// `UnixSignal.sigprocmask`, failing the test on a refusal.
    let private sigprocmask
        (task : int)
        (how : int)
        (set : SignalMask option)
        (system : UnixSystem<int, string>)
        : Result<SignalMask * UnixSystem<int, string>, UnixError>
        =
        match UnixSignal.sigprocmask task how set system with
        | Ok answered -> answered
        | Error refusal -> failwith $"sigprocmask was refused: %s{SigprocmaskRefusal.describe refusal}"

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

    /// `SIG_BLOCK`, `SIG_UNBLOCK` and `SIG_SETMASK`, as each `<signal.h>`
    /// numbers them (the probe's header line).
    let private block (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0
        | SimulatedUnixFlavour.Darwin -> 1

    let private unblock (flavour : SimulatedUnixFlavour) : int = block flavour + 1
    let private setmask (flavour : SimulatedUnixFlavour) : int = block flavour + 2

    /// Which call a row makes: the probe's routes.
    type private Route =
        | Libc
        | Pthread
        /// Linux's `rt_sigprocmask` with a `sigsetsize` of 8.
        | Raw

    let private routesOf (flavour : SimulatedUnixFlavour) : Route list =
        match flavour with
        | SimulatedUnixFlavour.Linux -> [ Route.Libc ; Route.Pthread ; Route.Raw ]
        // Darwin's raw `sigprocmask` and `__pthread_sigmask` answered as the C
        // library's two on every row.
        | SimulatedUnixFlavour.Darwin -> [ Route.Libc ; Route.Pthread ]

    let private call
        (route : Route)
        (task : int)
        (how : int)
        (set : SignalMask option)
        (system : UnixSystem<int, string>)
        : Result<SignalMask * UnixSystem<int, string>, UnixError>
        =
        match route with
        | Route.Libc -> sigprocmask task how set system
        | Route.Pthread -> UnixSignal.pthreadSigmask task how set system
        | Route.Raw -> UnixSignal.rtSigprocmask task how set 8UL system

    let private maskOfWord (flavour : SimulatedUnixFlavour) (word : uint64) : SignalMask =
        match SignalMask.ofWord (numberingOf flavour) word with
        | Ok mask -> mask
        | Error refusal -> failwith (SignalMaskRefusal.describe refusal)

    let private wordOf (task : int) (system : UnixSystem<int, string>) : uint64 =
        SignalState.maskOf task system.Process.Signals |> SignalMask.toWord

    let private orFail (result : Result<SignalMask * UnixSystem<int, string>, UnixError>) : UnixSystem<int, string> =
        match result with
        | Ok (_, system) -> system
        | Error errno -> failwith $"the mask call failed with %O{errno}"

    /// `task` with exactly `word` blocked, set through `pthread_sigmask`.
    let private withMask
        (flavour : SimulatedUnixFlavour)
        (task : int)
        (word : uint64)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        UnixSignal.pthreadSigmask task (setmask flavour) (Some (maskOfWord flavour word)) system
        |> orFail

    // HUP, INT and TERM are 1, 2 and 15 under both numberings.
    let private hup : uint64 = 0x1UL
    let private int' : uint64 = 0x2UL
    let private term : uint64 = 0x4000UL

    [<Test>]
    let ``every how answers as measured, with a set and with a NULL set, through every route`` () : unit =
        // The probe's "how" rows: the thread starts with HUP|INT (3) blocked,
        // the set is INT|TERM (4002). With a NULL set every how answered
        // old=3 and left 3; with the set, BLOCK left 4003, UNBLOCK 1, SETMASK
        // 4002, and every other how was EINVAL, left 3 and wrote no old mask.
        let unnamed = [ -1 ; 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 100 ; Int32.MaxValue ; Int32.MinValue ]

        for flavour in flavours do
            let start = systemOn flavour |> withMask flavour 0 (hup ||| int')

            for route in routesOf flavour do
                let named =
                    [ block flavour, 0x4003UL ; unblock flavour, 0x1UL ; setmask flavour, 0x4002UL ]

                let rows =
                    named
                    @ (unnamed
                       |> List.filter (fun how -> not (List.exists (fun (n, _) -> n = how) named))
                       |> List.map (fun how -> how, 0UL))

                for how, after in rows do
                    let isNamed = after <> 0UL

                    match call route 0 how None start with
                    | Ok (old, system) ->
                        (flavour, route, how, SignalMask.toWord old, wordOf 0 system)
                        |> shouldEqual (flavour, route, how, 0x3UL, 0x3UL)
                    | Error errno -> failwith $"%O{flavour} %A{route} how=%d{how} NULL: %O{errno}"

                    match call route 0 how (Some (maskOfWord flavour (int' ||| term))) start with
                    | Ok (old, system) ->
                        isNamed |> shouldEqual true

                        (flavour, route, how, SignalMask.toWord old, wordOf 0 system)
                        |> shouldEqual (flavour, route, how, 0x3UL, after)
                    | Error errno ->
                        (flavour, route, how, isNamed, errno)
                        |> shouldEqual (flavour, route, how, false, UnixError.EINVAL)

    [<Test>]
    let ``blocking every bit leaves the mask measured, through every route`` () : unit =
        // The probe's "full" rows, for SIG_BLOCK and SIG_SETMASK alike.
        let measured (flavour : SimulatedUnixFlavour) (route : Route) : uint64 =
            match flavour, route with
            | SimulatedUnixFlavour.Linux, Route.Raw -> 0xfffffffffffbfeffUL
            | SimulatedUnixFlavour.Linux, _ -> 0xfffffffe7ffbfeffUL
            | SimulatedUnixFlavour.Darwin, _ -> 0xfffefeffUL

        for flavour in flavours do
            let full =
                match flavour with
                | SimulatedUnixFlavour.Linux -> UInt64.MaxValue
                | SimulatedUnixFlavour.Darwin -> 0xffffffffUL

            for route in routesOf flavour do
                for how in [ block flavour ; setmask flavour ] do
                    match call route 0 how (Some (maskOfWord flavour full)) (systemOn flavour) with
                    | Ok (old, system) ->
                        (flavour, route, how, SignalMask.toWord old, wordOf 0 system)
                        |> shouldEqual (flavour, route, how, 0UL, measured flavour route)

                        UnixSystem.checkInvariants system |> shouldEqual []
                    | Error errno -> failwith $"%O{flavour} %A{route}: %O{errno}"

    [<Test>]
    let ``Linux: the raw call blocks 32 and 33, which glibc reads back and cannot set`` () : unit =
        // The probe's "rt" row: the raw call blocking 32, 33 and HUP left
        // 180000001, which glibc's query read back; glibc's SIG_SETMASK of HUP
        // then left 1; and from 180000001, glibc's SIG_UNBLOCK of 32 and 33
        // left 180000001.
        let flavour = SimulatedUnixFlavour.Linux
        let rt = 0x180000000UL

        let blocked =
            UnixSignal.rtSigprocmask 0 (block flavour) (Some (maskOfWord flavour (rt ||| hup))) 8UL (systemOn flavour)
            |> orFail

        match sigprocmask 0 (block flavour) None blocked with
        | Ok (old, _) -> SignalMask.toWord old |> shouldEqual 0x180000001UL
        | Error errno -> failwith $"%O{errno}"

        match UnixSignal.pthreadSigmask 0 (block flavour) None blocked with
        | Ok (old, _) -> SignalMask.toWord old |> shouldEqual 0x180000001UL
        | Error errno -> failwith $"%O{errno}"

        sigprocmask 0 (setmask flavour) (Some (maskOfWord flavour hup)) blocked
        |> orFail
        |> wordOf 0
        |> shouldEqual 0x1UL

        sigprocmask 0 (unblock flavour) (Some (maskOfWord flavour rt)) blocked
        |> orFail
        |> wordOf 0
        |> shouldEqual 0x180000001UL

    [<Test>]
    let ``Linux: rt_sigprocmask answers EINVAL for every sigsetsize but 8, before anything else`` () : unit =
        // The probe's "size" rows: every size but 8 was EINVAL with a set,
        // with a NULL set, and with a NULL set and an unnamed how, and left
        // the mask as it was.
        let flavour = SimulatedUnixFlavour.Linux
        let start = systemOn flavour |> withMask flavour 0 (hup ||| int')
        let set = Some (maskOfWord flavour (int' ||| term))

        for size in [ 0UL ; 1UL ; 4UL ; 7UL ; 9UL ; 16UL ; 128UL ] do
            for how, set in [ block flavour, set ; block flavour, None ; 100, None ; 100, set ] do
                UnixSignal.rtSigprocmask 0 how set size start
                |> Result.map fst
                |> shouldEqual (Error UnixError.EINVAL)

        // At 8, the rows answered as every route does.
        UnixSignal.rtSigprocmask 0 100 None 8UL start
        |> Result.map (fun (old, system) -> SignalMask.toWord old, wordOf 0 system)
        |> shouldEqual (Ok (0x3UL, 0x3UL))

        UnixSignal.rtSigprocmask 0 100 set 8UL start
        |> Result.map fst
        |> shouldEqual (Error UnixError.EINVAL)

    [<Test>]
    let ``Darwin: rt_sigprocmask is not a system call there`` () : unit =
        Assert.Throws<exn> (fun () ->
            UnixSignal.rtSigprocmask 0 1 None 8UL (systemOn SimulatedUnixFlavour.Darwin)
            |> ignore<Result<SignalMask * UnixSystem<int, string>, UnixError>>
        )
        |> ignore

    [<Test>]
    let ``Darwin's sigprocmask changes every task's mask, and every other call the caller's alone`` () : unit =
        // The probe's "spread" rows: the main thread blocks HUP (1) and a
        // second thread INT (2), then the main thread calls.
        let measured (flavour : SimulatedUnixFlavour) (route : Route) (how : int) : uint64 * uint64 =
            let everyTask = flavour = SimulatedUnixFlavour.Darwin && route = Route.Libc

            if how = block flavour then
                0x4001UL, (if everyTask then 0x4002UL else 0x2UL)
            elif how = unblock flavour then
                0x0UL, (if everyTask then 0x0UL else 0x2UL)
            else
                0x4000UL, (if everyTask then 0x4000UL else 0x2UL)

        for flavour in flavours do
            let start =
                systemOn flavour
                |> Tasks.spawn 1
                |> withMask flavour 0 hup
                |> withMask flavour 1 int'

            let sets =
                [ block flavour, term ; unblock flavour, hup ||| int' ; setmask flavour, term ]

            for route in routesOf flavour do
                for how, set in sets do
                    match call route 0 how (Some (maskOfWord flavour set)) start with
                    | Ok (old, system) ->
                        (flavour, route, how, SignalMask.toWord old, (wordOf 0 system, wordOf 1 system))
                        |> shouldEqual (flavour, route, how, hup, measured flavour route how)

                        UnixSystem.checkInvariants system |> shouldEqual []
                    | Error errno -> failwith $"%O{flavour} %A{route}: %O{errno}"

    let private signo (flavour : SimulatedUnixFlavour) (signal : Signal) : int =
        Signal.toRawSignoUnder (numberingOf flavour) signal

    let private catching (flavour : SimulatedUnixFlavour) (signal : Signal) (system : UnixSystem<int, string>) =
        match
            UnixSignal.sigaction
                (signo flavour signal)
                (Some (SignalDisposition.Catch (SignalCatch.ofHandler (string<Signal> signal))))
                system
        with
        | Ok (_, system) -> system
        | Error errno -> failwith $"sigaction failed with %O{errno}"

    let private sent
        (result : Result<Result<KillOutcome<int, string>, UnixError>, 'refusal>)
        : UnixSystem<int, string>
        =
        match result with
        | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
        | other -> failwith $"expected the process to carry on, got %A{other}"

    let private self (system : UnixSystem<int, string>) : int =
        ProcessId.toInt32 (UnixSystem.processId system)

    [<Test>]
    let ``unblocking three pending caught signals runs every handler before the call returns, last taken first``
        ()
        : unit
        =
        // The probe's "unblock" rows: handlers for ILL, USR1 and TERM, all
        // three blocked and sent (TERM, USR1, ILL), then one SIG_UNBLOCK of
        // the three. All three handlers had run when it returned, in the order
        // Linux TERM, USR1, ILL and Darwin USR1, TERM, ILL, sent to the
        // process or to the thread alike.
        let measured (flavour : SimulatedUnixFlavour) : Signal list =
            match flavour with
            | SimulatedUnixFlavour.Linux -> [ Signal.SIGTERM ; Signal.SIGUSR1 ; Signal.SIGILL ]
            | SimulatedUnixFlavour.Darwin -> [ Signal.SIGUSR1 ; Signal.SIGTERM ; Signal.SIGILL ]

        for flavour in flavours do
            let three = [ Signal.SIGTERM ; Signal.SIGUSR1 ; Signal.SIGILL ]
            let mask = SignalMask.ofSignals (numberingOf flavour) (Set.ofList three)

            for threadDirected in [ false ; true ] do
                let system =
                    (systemOn flavour, three)
                    ||> List.fold (fun system signal -> catching flavour signal system)
                    |> UnixSignal.pthreadSigmask 0 (setmask flavour) (Some mask)
                    |> orFail

                let system =
                    (system, three)
                    ||> List.fold (fun system signal ->
                        if threadDirected then
                            UnixSignal.pthreadKill 0 (signo flavour signal) system |> sent
                        else
                            UnixSignal.kill (self system) (signo flavour signal) system |> sent
                    )

                UnixSignal.onReturnToUser 0 system |> Result.map fst |> shouldEqual (Ok None)

                UnixSignal.sigpending 0 system |> shouldEqual mask

                let unblocked = sigprocmask 0 (unblock flavour) (Some mask) system |> orFail

                match UnixSignal.onReturnToUser 0 unblocked with
                | Ok (Some (SignalDelivery.RunHandlers frames), after) ->
                    (flavour, threadDirected, frames |> List.map (fun frame -> frame.Entry.Signal))
                    |> shouldEqual (flavour, threadDirected, measured flavour)

                    UnixSignal.sigpending 0 after |> shouldEqual SignalMask.empty
                | other -> failwith $"%O{flavour}: expected three frames, got %A{other}"

    [<Test>]
    let ``sigpending sees the process's signals from every task on Linux, and from the main thread alone on Darwin``
        ()
        : unit
        =
        // `sigpending-scope.c`, rows "proc" and "thread": the main thread and
        // a second both block the signal.
        for flavour in flavours do
            let numbering = numberingOf flavour

            let both (signal : Signal) =
                let mask = SignalMask.ofSignals numbering (Set.singleton signal)

                systemOn flavour
                |> Tasks.spawn 1
                |> withMask flavour 0 (SignalMask.toWord mask)
                |> withMask flavour 1 (SignalMask.toWord mask)

            let usr1 = SignalMask.ofSignals numbering (Set.singleton Signal.SIGUSR1)
            let usr2 = SignalMask.ofSignals numbering (Set.singleton Signal.SIGUSR2)

            let proc =
                let system = both Signal.SIGUSR1
                UnixSignal.kill (self system) (signo flavour Signal.SIGUSR1) system |> sent

            UnixSignal.sigpending 0 proc |> shouldEqual usr1

            UnixSignal.sigpending 1 proc
            |> shouldEqual (
                match flavour with
                | SimulatedUnixFlavour.Linux -> usr1
                | SimulatedUnixFlavour.Darwin -> SignalMask.empty
            )

            for target, other in [ 1, 0 ; 0, 1 ] do
                let thread =
                    UnixSignal.pthreadKill target (signo flavour Signal.SIGUSR2) (both Signal.SIGUSR2)
                    |> sent

                UnixSignal.sigpending target thread |> shouldEqual usr2
                UnixSignal.sigpending other thread |> shouldEqual SignalMask.empty

    [<Test>]
    let ``an ignored signal sent while blocked is pending as measured, and gone once unblocked`` () : unit =
        // `sigpending-scope.c`, rows "ignored" and "cont": Linux kept every
        // one pending; Darwin kept only SIGCONT. Unblocked and blocked again,
        // each was gone on both. SIGCONT at its default, blocked, was pending
        // on both.
        let rows =
            [
                Signal.SIGUSR1, SignalDisposition.Ignore, (true, false)
                Signal.SIGWINCH, SignalDisposition.Default, (true, false)
                Signal.SIGURG, SignalDisposition.Default, (true, false)
                Signal.SIGCONT, SignalDisposition.Ignore, (true, true)
                Signal.SIGCONT, SignalDisposition.Default, (true, true)
            ]

        for flavour in flavours do
            let numbering = numberingOf flavour

            for signal, disposition, (onLinux, onDarwin) in rows do
                let mask = SignalMask.ofSignals numbering (Set.singleton signal)

                let system =
                    match UnixSignal.sigaction (signo flavour signal) (Some disposition) (systemOn flavour) with
                    | Ok (_, system) -> system
                    | Error errno -> failwith $"%O{errno}"
                    |> withMask flavour 0 (SignalMask.toWord mask)

                let system = UnixSignal.kill (self system) (signo flavour signal) system |> sent

                let system =
                    match UnixSignal.onReturnToUser 0 system with
                    | Ok (None, system) -> system
                    | other -> failwith $"%O{flavour} %O{signal}: expected nothing taken, got %A{other}"

                let expected =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> onLinux
                    | SimulatedUnixFlavour.Darwin -> onDarwin

                (flavour, signal, UnixSignal.sigpending 0 system)
                |> shouldEqual (flavour, signal, (if expected then mask else SignalMask.empty))

                if disposition = SignalDisposition.Ignore then
                    let unblocked =
                        UnixSignal.pthreadSigmask 0 (unblock flavour) (Some mask) system |> orFail

                    let returned =
                        match UnixSignal.onReturnToUser 0 unblocked with
                        | Ok (None, system) -> system
                        | other -> failwith $"%O{flavour} %O{signal}: expected nothing taken, got %A{other}"

                    let reblocked = withMask flavour 0 (SignalMask.toWord mask) returned

                    (flavour, signal, UnixSignal.sigpending 0 reblocked)
                    |> shouldEqual (flavour, signal, SignalMask.empty)

    [<Test>]
    let ``inside a handler, sigpending shows the signals its mask holds back`` () : unit =
        // `sigpending-scope.c`, row "handler": a handler for USR1 with sa_mask
        // TERM raises USR1 and kills the process with TERM. sigpending inside
        // it, and its mask, were both {USR1, TERM}; once it returned, USR1's
        // handler ran again and then TERM's, on both.
        for flavour in flavours do
            let numbering = numberingOf flavour

            let system =
                match
                    UnixSignal.sigaction
                        (signo flavour Signal.SIGUSR1)
                        (Some (
                            SignalDisposition.Catch
                                { SignalCatch.ofHandler "USR1" with
                                    Mask = SignalMask.ofSignals numbering (Set.singleton Signal.SIGTERM)
                                }
                        ))
                        (systemOn flavour |> catching flavour Signal.SIGTERM)
                with
                | Ok (_, system) -> system
                | Error errno -> failwith $"%O{errno}"

            let raised = UnixSignal.pthreadKill 0 (signo flavour Signal.SIGUSR1) system |> sent

            let frame, inHandler =
                match UnixSignal.onReturnToUser 0 raised with
                | Ok (Some (SignalDelivery.RunHandlers [ frame ]), system) -> frame, system
                | other -> failwith $"expected USR1's handler, got %A{other}"

            let inHandler =
                UnixSignal.pthreadKill 0 (signo flavour Signal.SIGUSR1) inHandler |> sent

            let inHandler =
                UnixSignal.kill (self inHandler) (signo flavour Signal.SIGTERM) inHandler
                |> sent

            let both =
                SignalMask.ofSignals numbering (Set.ofList [ Signal.SIGUSR1 ; Signal.SIGTERM ])

            UnixSignal.sigpending 0 inHandler |> shouldEqual both
            SignalState.maskOf 0 inHandler.Process.Signals |> shouldEqual both

            let rec run
                (ran : Signal list)
                (frames : HandlerFrame<int, string> list)
                (system : UnixSystem<int, string>)
                =
                match frames with
                | [] -> ran, system
                | frame :: outer ->
                    let ran = ran @ [ frame.Entry.Signal ]
                    let system = UnixSignal.sigreturn 0 frame.Id system

                    let ran, system =
                        match UnixSignal.onReturnToUser 0 system with
                        | Ok (None, system) -> ran, system
                        | Ok (Some (SignalDelivery.RunHandlers frames), system) -> run ran frames system
                        | other -> failwith $"expected handlers, got %A{other}"

                    run ran outer system

            let ran, after = run [] [ frame ] inHandler

            (flavour, ran)
            |> shouldEqual (flavour, [ Signal.SIGUSR1 ; Signal.SIGUSR1 ; Signal.SIGTERM ])

            SignalState.maskOf 0 after.Process.Signals |> shouldEqual SignalMask.empty
            UnixSignal.sigpending 0 after |> shouldEqual SignalMask.empty

    [<Test>]
    let ``a caught signal the leader blocks and another task does not is refused, with no handler involved`` () : unit =
        // Which task takes it is the receiver rule this library does not
        // model (`SignalReceiverRefusal.LeaderBlocks`).
        for flavour in flavours do
            let usr1 =
                SignalMask.ofSignals (numberingOf flavour) (Set.singleton Signal.SIGUSR1)
                |> SignalMask.toWord

            let system =
                systemOn flavour
                |> catching flavour Signal.SIGUSR1
                |> Tasks.spawn 1
                |> withMask flavour 0 usr1

            SignalState.framesOf 0 system.Process.Signals |> shouldEqual []

            UnixSignal.kill (self system) (signo flavour Signal.SIGUSR1) system
            |> Result.map (fun _ -> ())
            |> shouldEqual (Error (KillRefusal.Receiver (SignalReceiverRefusal.LeaderBlocks Signal.SIGUSR1)))

            // Once the other task blocks it too, it is simply pending.
            let both = system |> withMask flavour 1 usr1

            let pending =
                UnixSignal.kill (self both) (signo flavour Signal.SIGUSR1) both |> sent

            UnixSignal.sigpending 0 pending |> SignalMask.toWord |> shouldEqual usr1

    [<Test>]
    let ``a sigprocmask that unblocks a signal pending for a task asleep in poll is refused on Darwin, and wakes nothing on Linux``
        ()
        : unit
        =
        // `unblock-wakes-sleeper.c`: on Darwin the sleeper slept on until its
        // own condition ended the call, which this library cannot express
        // (`TestUnblockForAnotherTask` searches every call and signal).
        for flavour in flavours do
            let usr1 = SignalMask.ofSignals (numberingOf flavour) (Set.singleton Signal.SIGUSR1)

            let system =
                systemOn flavour
                |> catching flavour Signal.SIGUSR1
                |> Tasks.spawn 1
                |> withMask flavour 1 (SignalMask.toWord usr1)

            let system = UnixSignal.pthreadKill 1 (signo flavour Signal.SIGUSR1) system |> sent

            // A poll of nothing, with no deadline, sleeps until a signal ends
            // it.
            let parked =
                match UnixPoll.poll 1 [] -1 system with
                | Ok (_, system) when UnixTaskTable.parkOf 1 system.Tasks |> Option.isSome -> system
                | other -> failwith $"expected the poll to park, got %A{other}"

            UnixWait.wakes (Set.singleton 1) parked |> shouldEqual []

            match flavour, UnixSignal.sigprocmask 0 (unblock flavour) (Some usr1) parked with
            | SimulatedUnixFlavour.Darwin, Error refusal ->
                refusal
                |> shouldEqual (SigprocmaskRefusal.DarwinUnblockedForAnotherTask (1, Signal.SIGUSR1))
            | SimulatedUnixFlavour.Linux, Ok (Ok (_, unblocked)) ->
                UnixWait.wakes (Set.singleton 1) unblocked |> shouldEqual []
            | _, other -> failwith $"%O{flavour}: the sigprocmask answered %A{other}"

    /// One mask call of the property's sequences.
    type private MaskCall =
        {
            Route : Route
            Task : int
            How : int
            Set : uint64 option
            /// `rt_sigprocmask`'s `sigsetsize`; read for `Route.Raw` alone.
            Size : uint64
        }

    /// The reference: each task's mask as a word, and the mask calls' rules
    /// written out from the probe's rows rather than read from the library.
    let private referenceCall
        (flavour : SimulatedUnixFlavour)
        (masks : Map<int, uint64>)
        (call : MaskCall)
        : Result<uint64 * Map<int, uint64>, UnixError>
        =
        let old = Map.find call.Task masks

        let killStop =
            match flavour with
            | SimulatedUnixFlavour.Linux -> (1UL <<< 8) ||| (1UL <<< 18)
            | SimulatedUnixFlavour.Darwin -> (1UL <<< 8) ||| (1UL <<< 16)

        if call.Route = Route.Raw && call.Size <> 8UL then
            Error UnixError.EINVAL
        else

        match call.Set with
        | None -> Ok (old, masks)
        | Some set ->
            if
                call.How <> block flavour
                && call.How <> unblock flavour
                && call.How <> setmask flavour
            then
                Error UnixError.EINVAL
            else
                // glibc takes 32 and 33 out of the set; the raw call does not.
                let screened =
                    match flavour, call.Route with
                    | SimulatedUnixFlavour.Linux, (Route.Libc | Route.Pthread) -> 0x180000000UL
                    | _ -> 0UL

                let set = set &&& ~~~screened

                let change (m : uint64) : uint64 =
                    let changed =
                        if call.How = setmask flavour then set
                        elif call.How = block flavour then m ||| set
                        else m &&& ~~~set

                    changed &&& ~~~killStop

                let targets =
                    if flavour = SimulatedUnixFlavour.Darwin && call.Route = Route.Libc then
                        masks |> Map.keys |> List.ofSeq
                    else
                        [ call.Task ]

                let masks =
                    (masks, targets)
                    ||> List.fold (fun masks task -> Map.add task (change (Map.find task masks)) masks)

                Ok (old, masks)

    [<Test>]
    let ``the mask calls agree with a reference over raw words, and store no SIGKILL, SIGSTOP or empty mask``
        ()
        : unit
        =
        let gen =
            gen {
                let! flavour = Gen.elements flavours

                let bits =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> 64
                    | SimulatedUnixFlavour.Darwin -> 32

                let word =
                    Gen.oneof
                        [
                            ArbMap.defaults |> ArbMap.generate<uint64>
                            // A few low signals, SIGKILL's and SIGSTOP's bits, and
                            // the bits only one route can set: Linux's 32 and 33,
                            // Darwin's bit 31.
                            Gen.subListOf [ 0 ; 1 ; 8 ; 9 ; 14 ; 16 ; 18 ; 29 ; 30 ; 31 ; 32 ]
                            |> Gen.map (List.fold (fun w bit -> w ||| (1UL <<< bit)) 0UL)
                        ]
                    |> Gen.map (fun w -> if bits = 64 then w else w &&& 0xffffffffUL)

                let callGen =
                    gen {
                        let! route = Gen.elements (routesOf flavour)
                        let! task = Gen.elements [ 0 ; 1 ]

                        let! how =
                            Gen.frequency
                                [
                                    6, Gen.elements [ block flavour ; unblock flavour ; setmask flavour ]
                                    1, Gen.elements [ -1 ; 0 ; 3 ; 4 ; 100 ; Int32.MinValue ]
                                ]

                        let! set = Gen.frequency [ 5, word |> Gen.map Some ; 1, Gen.constant None ]
                        let! size = Gen.frequency [ 8, Gen.constant 8UL ; 1, Gen.elements [ 0UL ; 4UL ; 128UL ] ]

                        return
                            {
                                Route = route
                                Task = task
                                How = how
                                Set = set
                                Size = size
                            }
                    }

                let! calls = Gen.listOf callGen
                return flavour, List.truncate 40 calls
            }

        let property (flavour : SimulatedUnixFlavour, calls : MaskCall list) : unit =
            let start = systemOn flavour |> Tasks.spawn 1

            ((start, Map.ofList [ 0, 0UL ; 1, 0UL ]), calls)
            ||> List.fold (fun (system, masks) c ->
                let set = c.Set |> Option.map (maskOfWord flavour)

                let actual =
                    match c.Route with
                    | Route.Raw -> UnixSignal.rtSigprocmask c.Task c.How set c.Size system
                    | route -> call route c.Task c.How set system

                let expected = referenceCall flavour masks c

                match actual, expected with
                | Ok (old, system'), Ok (expectedOld, masks') ->
                    (c, SignalMask.toWord old) |> shouldEqual (c, expectedOld)

                    for task in [ 0 ; 1 ] do
                        (c, task, wordOf task system') |> shouldEqual (c, task, Map.find task masks')

                    SignalState.tasksWithMasks system'.Process.Signals
                    |> shouldEqual (masks' |> Map.filter (fun _ w -> w <> 0UL) |> Map.keys |> Set.ofSeq)

                    UnixSystem.checkInvariants system' |> shouldEqual []
                    system', masks'
                | Error a, Error b ->
                    (c, a) |> shouldEqual (c, b)
                    system, masks
                | _ -> failwith $"%A{c}: the library answered %A{actual}, the reference %A{expected}"
            )
            |> ignore

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``sigreturn restores the mask at delivery, whatever the handler did to the mask`` () : unit =
        // The signals research's `sigaction_flags.c`: a handler that unblocked
        // SIGHUP and blocked SIGTERM returned to exactly the mask at delivery.
        let gen =
            gen {
                let! flavour = Gen.elements flavours

                let signals =
                    [
                        Signal.SIGHUP
                        Signal.SIGINT
                        Signal.SIGTERM
                        Signal.SIGUSR2
                        Signal.SIGWINCH
                    ]

                let! before = Gen.subListOf signals
                let! saMask = Gen.subListOf signals
                let! noDefer = ArbMap.defaults |> ArbMap.generate<bool>

                let! inside =
                    Gen.listOf (
                        Gen.zip
                            (Gen.elements [ block flavour ; unblock flavour ; setmask flavour ])
                            (Gen.subListOf (Signal.SIGUSR1 :: signals))
                    )

                return flavour, before, saMask, noDefer, List.truncate 10 inside
            }

        let property
            (flavour : SimulatedUnixFlavour, before : Signal list, saMask : Signal list, noDefer : bool, inside)
            : unit
            =
            let numbering = numberingOf flavour

            let ofList (signals : Signal list) =
                SignalMask.ofSignals numbering (Set.ofList signals)

            let system =
                match
                    UnixSignal.sigaction
                        (signo flavour Signal.SIGUSR1)
                        (Some (
                            SignalDisposition.Catch
                                { SignalCatch.ofHandler "h" with
                                    Mask = ofList saMask
                                    NoDefer = noDefer
                                }
                        ))
                        (systemOn flavour)
                with
                | Ok (_, system) -> system
                | Error errno -> failwith $"%O{errno}"
                |> withMask flavour 0 (SignalMask.toWord (ofList before))

            let system = UnixSignal.pthreadKill 0 (signo flavour Signal.SIGUSR1) system |> sent

            let frame, inHandler =
                match UnixSignal.onReturnToUser 0 system with
                | Ok (Some (SignalDelivery.RunHandlers [ frame ]), system) -> frame, system
                | other -> failwith $"expected one frame, got %A{other}"

            frame.SavedMask |> shouldEqual (ofList before)

            SignalState.maskOf 0 inHandler.Process.Signals
            |> SignalMask.signals
            |> shouldEqual (
                Set.unionMany
                    [
                        Set.ofList before
                        Set.ofList saMask
                        (if noDefer then Set.empty else Set.singleton Signal.SIGUSR1)
                    ]
            )

            let changed =
                (inHandler, inside)
                ||> List.fold (fun system (how, set) ->
                    UnixSignal.pthreadSigmask 0 how (Some (ofList set)) system |> orFail
                )

            let returned = UnixSignal.sigreturn 0 frame.Id changed
            SignalState.maskOf 0 returned.Process.Signals |> shouldEqual (ofList before)
            UnixSystem.checkInvariants returned |> shouldEqual []

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)
