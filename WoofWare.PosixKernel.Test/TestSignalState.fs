namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Property-based and unit tests for the deterministic `SignalState` data
/// model, exercised in isolation from the dispatcher.
///
/// The property test runs a random sequence of operations through both the
/// production module and a structurally-different reference oracle, then
/// asserts agreement on every observable accessor after each step. The
/// oracle uses index-based scanning over an array; the production module
/// uses a recursive accumulator-threaded walk. A regression in either side
/// surfaces as a divergence the property catches.
///
/// Everything runs under both numberings, because the state's contract is
/// stated *under a numbering*: a signal one platform lacks, such as `SIGPWR`
/// in a Darwin process, is refused by every operation.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalState =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// Stand-in for whatever a client uses to identify a task. Deliberately a
    /// nominal type of this test's own rather than `int`: the point of these
    /// tests is that `SignalState` is generic in its task identity, and an
    /// `int` could satisfy the signature through some numeric path without the
    /// parameter being genuinely opaque.
    type TestTask = | TestTask of int

    /// Stand-in for a client's signal-handler identity, which `SignalState` is
    /// generic in and constrains only to equality.
    type TestHandler = | TestHandler of string

    /// `initial` at this test's instantiation. Named because the
    /// generic `initial` constrains neither parameter, so every bare use would
    /// infer `obj` for the handler -- which `FS3559` rejects, correctly: an
    /// `obj` handler would make the equality constraint vacuous and the tests
    /// below would stop saying anything about it.
    let private initial (numbering : SignalNumbering) : SignalState<TestTask, TestHandler> =
        SignalState.initial numbering Set.empty

    /// Most of the operations' behaviour does not depend on the numbering at
    /// all; those tests run on this instance and rely on the property test to
    /// cover the other numbering.
    let private empty : SignalState<TestTask, TestHandler> =
        initial SignalNumbering.Linux

    /// The signals a process that `generation` left running has.
    let private continuesWith
        (generation : SignalGeneration<TestTask, TestHandler>)
        : SignalState<TestTask, TestHandler>
        =
        match generation with
        | SignalGeneration.ProcessContinues state -> state
        | SignalGeneration.ProcessTerminated _
        | SignalGeneration.ProcessStopped _ -> failwith $"expected the process to carry on, got %A{generation}"

    let private t0 : TestTask = TestTask 0
    let private t1 : TestTask = TestTask 1
    let private t2 : TestTask = TestTask 2

    let private allThreads : TestTask list = [ t0 ; t1 ; t2 ]

    let private namedSignals : Signal list =
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

    /// SIGKILL and SIGSTOP, plus (on Linux) the two real-time signals glibc
    /// reserves for itself: what glibc's mask calls refuse.
    let private unblockableSignals (numbering : SignalNumbering) : Signal list =
        match numbering with
        | SignalNumbering.Linux -> [ Signal.SIGKILL ; Signal.SIGSTOP ; Signal.RealTime 0 ; Signal.RealTime 1 ]
        | SignalNumbering.Darwin -> [ Signal.SIGKILL ; Signal.SIGSTOP ]

    /// SIGKILL and SIGSTOP: what a kernel drops from any mask, and for which
    /// it holds no disposition but the default.
    let private kernelUncatchable : Signal list = [ Signal.SIGKILL ; Signal.SIGSTOP ]

    /// Further signals, blockable and catchable, which flow through every
    /// operation like those above.
    let private furtherSignals (numbering : SignalNumbering) : Signal list =
        match numbering with
        // A real-time signal, which Darwin does not have.
        | SignalNumbering.Linux -> [ Signal.SIGTRAP ; Signal.RealTime 8 ]
        | SignalNumbering.Darwin -> [ Signal.SIGTRAP ]

    /// Every signal the property test hands the state under this numbering:
    /// the named ones above, the further ones, and the unblockable ones.
    let private allSignals (numbering : SignalNumbering) : Signal list =
        namedSignals @ furtherSignals numbering @ unblockableSignals numbering

    /// The subset of `allSignals` that `enable` accepts: everything
    /// `sigaction` would install a handler for.
    let private enableableSignals (numbering : SignalNumbering) : Signal list =
        allSignals numbering
        |> List.filter (fun signal -> not (Signal.isUncatchableUnder numbering signal))

    /// What the leader, `t0`, takes as it returns to user mode, in a process
    /// whose tasks are `tasks`. Fails the test on a refusal.
    let private leaderDelivery
        (tasks : TestTask list)
        (s : SignalState<TestTask, TestHandler>)
        : SignalDelivery<TestTask, TestHandler> option * SignalState<TestTask, TestHandler>
        =
        match SignalState.onReturnToUser CoreDumps.Suppressed t0 (Set.ofList tasks) t0 s with
        | Ok answer -> answer
        | Error refusal -> failwith $"onReturnToUser refused: %O{refusal}"

    /// The one entry a delivery ran a handler for, failing the test on any
    /// other delivery.
    let private oneHandler (delivery : SignalDelivery<TestTask, TestHandler> option) : PendingSignal<TestTask> =
        match delivery with
        | Some (SignalDelivery.RunHandlers [ frame ]) -> frame.Entry
        | other -> failwith $"expected one handler frame, got %A{other}"

    /// `SignalState.generate` in a process whose tasks are `tasks`, led by `t0`.
    /// Fails the test on a refusal.
    let private generateAmong
        (coreDumps : CoreDumps)
        (tasks : TestTask list)
        (entry : PendingSignal<TestTask>)
        (s : SignalState<TestTask, TestHandler>)
        : SignalGeneration<TestTask, TestHandler>
        =
        match SignalState.generate coreDumps t0 (Set.ofList tasks) entry s with
        | Ok generation -> generation
        | Error refusal -> failwith $"generate refused: %O{refusal}"

    let private handler : TestHandler = TestHandler "h"

    let private catch : SignalDisposition<TestHandler> =
        SignalDisposition.Catch (SignalCatch.ofHandler handler)

    /// `task` inside a handler that blocks `signal` (see `HandlerFrames.enter`),
    /// in a process whose tasks are `allThreads`, led by `t0`.
    let private block
        (task : TestTask)
        (signal : Signal)
        (s : SignalState<TestTask, TestHandler>)
        : SignalState<TestTask, TestHandler>
        =
        HandlerFrames.enter (TestHandler "carrier") t0 (Set.ofList allThreads) task (Set.singleton signal) s

    /// Whether `task`'s mask holds `signal`.
    let private isBlocked (task : TestTask) (signal : Signal) (s : SignalState<TestTask, TestHandler>) : bool =
        SignalMask.contains signal (SignalState.maskOf task s)

    /// Install `handler` for `signal`, as `sigaction` with a handler does.
    let private enable (signal : Signal) (s : SignalState<TestTask, TestHandler>) : SignalState<TestTask, TestHandler> =
        SignalState.setDisposition signal catch s

    /// Restore `signal`'s default, as `sigaction` with `SIG_DFL` does.
    let private restoreDefault
        (signal : Signal)
        (s : SignalState<TestTask, TestHandler>)
        : SignalState<TestTask, TestHandler>
        =
        SignalState.setDisposition signal SignalDisposition.Default s

    // ------------------------- Unit tests ------------------------- //

    [<Test>]
    let ``initial has every signal at its default, nothing blocked, nothing pending`` () : unit =
        for numbering in everyNumbering do
            let s = initial numbering
            SignalState.numbering s |> shouldEqual numbering
            SignalState.disposition Signal.SIGINT s |> shouldEqual SignalDisposition.Default
            SignalState.maskOf t0 s |> shouldEqual SignalMask.empty
            SignalState.framesOf t0 s |> shouldEqual []
            SignalState.tasksWithFrames s |> shouldEqual Set.empty
            SignalState.pending s |> Seq.toList |> shouldEqual []
            SignalState.dispositions s |> shouldEqual Map.empty

    [<Test>]
    let ``initial ignores exactly the inherited ignores`` () : unit =
        for numbering in everyNumbering do
            let s : SignalState<TestTask, TestHandler> =
                SignalState.initial numbering (Set.ofList [ Signal.SIGHUP ; Signal.SIGUSR2 ])

            SignalState.dispositions s
            |> shouldEqual (
                Map.ofList
                    [
                        Signal.SIGHUP, SignalDisposition.Ignore
                        Signal.SIGUSR2, SignalDisposition.Ignore
                    ]
            )

            SignalState.disposition Signal.SIGHUP s |> shouldEqual SignalDisposition.Ignore
            SignalState.disposition Signal.SIGINT s |> shouldEqual SignalDisposition.Default

    [<Test>]
    let ``initial refuses an inherited ignore of SIGKILL or SIGSTOP`` () : unit =
        for numbering in everyNumbering do
            for signal in kernelUncatchable do
                Assert.Throws (fun () ->
                    SignalState.initial numbering (Set.singleton signal)
                    |> ignore<SignalState<TestTask, TestHandler>>
                )
                |> ignore<exn>

    [<Test>]
    let ``setDisposition installs a handler`` () : unit =
        let s = empty |> enable Signal.SIGINT
        SignalState.disposition Signal.SIGINT s |> shouldEqual catch
        SignalState.disposition Signal.SIGHUP s |> shouldEqual SignalDisposition.Default
        SignalState.dispositions s |> shouldEqual (Map.ofList [ Signal.SIGINT, catch ])

    [<Test>]
    let ``setDisposition is idempotent`` () : unit =
        let once = empty |> enable Signal.SIGINT
        let twice = once |> enable Signal.SIGINT
        twice |> shouldEqual once

    [<Test>]
    let ``a second handler replaces the first`` () : unit =
        let other = SignalDisposition.Catch (SignalCatch.ofHandler (TestHandler "other"))

        let s =
            empty |> enable Signal.SIGINT |> SignalState.setDisposition Signal.SIGINT other

        SignalState.disposition Signal.SIGINT s |> shouldEqual other

    [<Test>]
    let ``restoring the default of a signal at its default is a no-op`` () : unit =
        let s = empty |> restoreDefault Signal.SIGINT
        s |> shouldEqual empty

    [<Test>]
    let ``a handler then the default collapses to the initial state`` () : unit =
        // Mirrors the unblock/empty-mask collapse: installing a handler and
        // then restoring the default must be structurally identical to never
        // having installed one. Without this, state dedup in the debugger
        // would distinguish two semantically-equivalent states.
        let caughtThenRestored =
            empty |> enable Signal.SIGINT |> restoreDefault Signal.SIGINT

        caughtThenRestored |> shouldEqual empty

    [<Test>]
    let ``restoring a terminating default leaves a pending entry queued, and the default then applies`` () : unit =
        // Removing the handler while a signal is pending doesn't drop the
        // signal — it remains queued, and delivery applies whatever the
        // disposition is at that moment: SIGINT's kernel default is to
        // terminate the process.
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue entry
            |> restoreDefault Signal.SIGINT

        SignalState.pending s |> Seq.toList |> shouldEqual [ entry ]

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery
        |> shouldEqual (Some (SignalDelivery.DefaultTerminate (Signal.SIGINT, false)))


        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``enable after enqueue makes a queued signal deliverable`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s = empty |> SignalState.enqueue entry |> enable Signal.SIGINT

        let delivery, s' = leaderDelivery [ t0 ] s
        oneHandler delivery |> shouldEqual entry
        SignalState.pending s' |> Seq.toList |> shouldEqual []

    [<Test>]
    let ``a handler frame blocks what its sa_mask names, for its task alone`` () : unit =
        let s = empty |> block t0 Signal.SIGINT
        isBlocked t0 Signal.SIGINT s |> shouldEqual true
        isBlocked t0 Signal.SIGHUP s |> shouldEqual false
        isBlocked t1 Signal.SIGINT s |> shouldEqual false
        SignalState.tasksWithFrames s |> shouldEqual (Set.singleton t0)

    [<Test>]
    let ``nested frames block the union of their masks, and sigreturn restores each mask exactly`` () : unit =
        let one = empty |> block t0 Signal.SIGINT
        let two = one |> block t0 Signal.SIGHUP

        SignalState.maskOf t0 two
        |> SignalMask.signals
        |> shouldEqual (Set.ofList [ Signal.SIGINT ; Signal.SIGHUP ])

        let back = two |> HandlerFrames.leave t0
        SignalState.maskOf t0 back |> shouldEqual (SignalState.maskOf t0 one)
        SignalState.framesOf t0 back |> shouldEqual (SignalState.framesOf t0 one)

        let none = back |> HandlerFrames.leave t0
        SignalState.maskOf t0 none |> shouldEqual SignalMask.empty
        SignalState.framesOf t0 none |> shouldEqual []
        // No empty stack is stored.
        SignalState.tasksWithFrames none |> shouldEqual Set.empty

    [<Test>]
    let ``sigreturn refuses any frame but the task's innermost`` () : unit =
        let two = empty |> block t0 Signal.SIGINT |> block t0 Signal.SIGHUP

        let outer =
            match SignalState.framesOf t0 two with
            | [ _ ; outer ] -> outer
            | frames -> failwith $"expected two frames, got %A{frames}"

        Assert.Throws (fun () -> SignalState.sigreturn t0 outer.Id two |> ignore<SignalState<_, _>>)
        |> ignore<exn>

        Assert.Throws (fun () -> SignalState.sigreturn t1 outer.Id two |> ignore<SignalState<_, _>>)
        |> ignore<exn>

    [<Test>]
    let ``every frame gets an id no other frame has had`` () : unit =
        let s =
            empty
            |> block t0 Signal.SIGINT
            |> HandlerFrames.leave t0
            |> block t0 Signal.SIGINT

        let before = empty |> block t0 Signal.SIGINT

        (SignalState.framesOf t0 s |> List.head).Id
        |> shouldNotEqual (SignalState.framesOf t0 before |> List.head).Id

    // ------------------- Unblockable and uncatchable signals ------------------- //

    [<Test>]
    let ``SIGSTOP is dropped from a mask under each numbering, and the signals sharing its numbers are not`` () : unit =
        // 17 is SIGSTOP on Darwin and SIGCHLD on Linux; 19 is SIGSTOP on
        // Linux and SIGCONT on Darwin.
        for numbering in everyNumbering do
            initial numbering
            |> block t0 Signal.SIGSTOP
            |> SignalState.maskOf t0
            |> shouldEqual SignalMask.empty

            initial numbering
            |> block t0 Signal.SIGCHLD
            |> SignalState.maskOf t0
            |> SignalMask.signals
            |> shouldEqual (Set.singleton Signal.SIGCHLD)

            initial numbering
            |> block t0 Signal.SIGCONT
            |> SignalState.maskOf t0
            |> SignalMask.signals
            |> shouldEqual (Set.singleton Signal.SIGCONT)

    [<Test>]
    let ``a handler's sa_mask holds every signal but SIGKILL and SIGSTOP`` () : unit =
        // Measured by the signal fuzzer's harness on Linux 6.18.5 and Darwin
        // 27.0.0: a handler whose sa_mask named every signal ran with every
        // one blocked but SIGKILL and SIGSTOP -- on Linux glibc's reserved 32
        // and 33 included, which only glibc's own set operations refuse.
        for numbering in everyNumbering do
            let caught =
                initial numbering
                |> SignalState.setDisposition
                    Signal.SIGHUP
                    (SignalDisposition.Catch
                        { SignalCatch.ofHandler handler with
                            Mask = SignalMask.ofSignals numbering (Set.ofList (allSignals numbering))
                        })

            let expected =
                allSignals numbering
                |> List.filter (fun signal -> not (List.contains signal kernelUncatchable))
                |> Set.ofList

            match SignalState.disposition Signal.SIGHUP caught with
            | SignalDisposition.Catch action -> SignalMask.signals action.Mask |> shouldEqual expected
            | other -> failwith $"expected a handler, got %A{other}"

            // And a frame for it blocks exactly that, and SIGHUP.
            let delivered =
                caught
                |> SignalState.enqueue
                    {
                        Signal = Signal.SIGHUP
                        Target = ValueNone
                    }
                |> leaderDelivery [ t0 ]
                |> snd

            SignalState.maskOf t0 delivered
            |> SignalMask.signals
            |> shouldEqual (Set.add Signal.SIGHUP expected)

    [<Test>]
    let ``setDisposition refuses every disposition for SIGKILL and SIGSTOP`` () : unit =
        for numbering in everyNumbering do
            for signal in kernelUncatchable do
                for disposition in [ SignalDisposition.Default ; SignalDisposition.Ignore ; catch ] do
                    Assert.Throws (fun () ->
                        initial numbering
                        |> SignalState.setDisposition signal disposition
                        |> ignore<SignalState<_, _>>
                    )
                    |> ignore<exn>

    [<Test>]
    let ``setDisposition accepts glibc's reserved 32 and 33, which only glibc's sigaction refuses`` () : unit =
        // The kernel holds a handler for 33 in every process glibc's
        // setxid machinery has run in; it is glibc's own sigaction wrapper
        // that refuses the pair, and a client modelling that wrapper screens
        // with Signal.isUncatchableUnder first.
        for signal in [ Signal.RealTime 0 ; Signal.RealTime 1 ] do
            Signal.isUncatchableUnder SignalNumbering.Linux signal |> shouldEqual true

            let s = initial SignalNumbering.Linux |> enable signal
            SignalState.disposition signal s |> shouldEqual catch

    [<Test>]
    let ``every operation refuses a signal the numbering does not have`` () : unit =
        let otherNumbering (numbering : SignalNumbering) : SignalNumbering =
            match numbering with
            | SignalNumbering.Linux -> SignalNumbering.Darwin
            | SignalNumbering.Darwin -> SignalNumbering.Linux

        let notASignal (numbering : SignalNumbering) : Signal list =
            match numbering with
            | SignalNumbering.Linux -> [ Signal.SIGEMT ; Signal.SIGINFO ; Signal.RealTime -1 ; Signal.RealTime 33 ]
            // A real-time signal on Linux, and nothing at all on Darwin.
            | SignalNumbering.Darwin -> [ Signal.SIGSTKFLT ; Signal.SIGPWR ; Signal.RealTime 0 ; Signal.RealTime 8 ]

        for numbering in everyNumbering do
            for signal in notASignal numbering do
                let s = initial numbering

                Assert.Throws (fun () -> enable signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> restoreDefault signal s |> ignore<SignalState<_, _>>)
                |> ignore<exn>

                Assert.Throws (fun () -> SignalState.disposition signal s |> ignore<SignalDisposition<_>>)
                |> ignore<exn>

                Assert.Throws (fun () ->
                    SignalState.initial numbering (Set.singleton signal)
                    |> ignore<SignalState<TestTask, TestHandler>>
                )
                |> ignore<exn>

                Assert.Throws (fun () ->
                    SignalState.setDisposition
                        Signal.SIGHUP
                        (SignalDisposition.Catch
                            { SignalCatch.ofHandler handler with
                                Mask = SignalMask.ofSignals (otherNumbering numbering) (Set.singleton signal)
                            })
                        s
                    |> ignore<SignalState<_, _>>
                )
                |> ignore<exn>

                Assert.Throws (fun () ->
                    SignalState.enqueue
                        {
                            Signal = signal
                            Target = ValueNone
                        }
                        s
                    |> ignore<SignalState<_, _>>
                )
                |> ignore<exn>

    // ------------------- Queue and delivery ------------------- //

    [<Test>]
    let ``enqueue keeps each set in the order it is taken in, whatever order it was generated in`` () : unit =
        // No kernel delivers in generation order (`TestSignalPickOrder`), so two
        // generation orders of the same signals are one state.
        let a =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let b =
            {
                Signal = Signal.SIGHUP
                Target = ValueNone
            }

        let s = empty |> SignalState.enqueue a |> SignalState.enqueue b

        SignalState.pending s |> Seq.toList |> shouldEqual [ b ; a ]
        s |> shouldEqual (empty |> SignalState.enqueue b |> SignalState.enqueue a)

    [<Test>]
    let ``enqueue coalesces a standard signal already pending in the same set`` () : unit =
        // A kernel holds at most one pending instance of a standard signal
        // per pending set: measured, three process-directed SIGUSR1 while
        // blocked deliver once, on both platforms. The discarded duplicate
        // leaves the state structurally identical, not merely equivalent.
        for target in [ ValueNone ; ValueSome t1 ] do
            let e =
                {
                    Signal = Signal.SIGINT
                    Target = target
                }

            let once = empty |> SignalState.enqueue e
            let twice = once |> SignalState.enqueue e

            twice |> shouldEqual once
            SignalState.pending twice |> shouldEqual [ e ]

    [<Test>]
    let ``enqueue keeps separate pending sets separate`` () : unit =
        // Measured: a process-directed plus a thread-directed instance of one
        // standard signal deliver twice (Linux, and a two-thread Darwin
        // process), so ValueNone and each ValueSome are distinct sets — as
        // are two different threads' own sets.
        let processDirected =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let atT0 =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t0
            }

        let atT1 =
            {
                Signal = Signal.SIGINT
                Target = ValueSome t1
            }

        let s =
            empty
            |> SignalState.enqueue processDirected
            |> SignalState.enqueue atT0
            |> SignalState.enqueue atT1
            // And a redundant round: each is already pending in its own set.
            |> SignalState.enqueue processDirected
            |> SignalState.enqueue atT0

        SignalState.pending s |> shouldEqual [ processDirected ; atT0 ; atT1 ]

    [<Test>]
    let ``a real-time signal queues without coalescing under Linux numbering`` () : unit =
        // Measured on Linux 6.18.5: three generations of signo 36 while
        // blocked deliver three times, via sigqueue and via kill alike.
        let rt =
            {
                Signal = Signal.RealTime 4
                Target = ValueNone
            }

        let s =
            initial SignalNumbering.Linux
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt

        SignalState.pending s |> shouldEqual [ rt ; rt ; rt ]

        // And each queued instance delivers separately.
        let s = s |> enable (Signal.RealTime 4)

        // The first instance's handler blocks the signal while it runs, so the
        // other two wait for its sigreturn.
        let delivery, s = leaderDelivery [ t0 ] s
        oneHandler delivery |> shouldEqual rt
        SignalState.pending s |> shouldEqual [ rt ; rt ]

        let delivery, s = s |> HandlerFrames.leave t0 |> leaderDelivery [ t0 ]
        oneHandler delivery |> shouldEqual rt
        SignalState.pending s |> shouldEqual [ rt ]

    [<Test>]
    let ``structural equality survives a non-empty pending queue`` () : unit =
        // `ImmutableQueue<T>` compares by reference, so storing `Pending` in
        // one would make two independently-built states with identical
        // contents compare unequal once the queue was non-empty.
        // A client may compare a `UnixSystem` (whose process embeds
        // `SignalState`) structurally for deterministic state dedup; this
        // test pins down that the contract holds across every operation
        // that touches the queue.
        let entryA =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let entryB =
            {
                Signal = Signal.SIGHUP
                Target = ValueSome t1
            }

        let buildA () =
            empty
            |> enable Signal.SIGINT
            |> block t0 Signal.SIGTERM
            |> SignalState.enqueue entryA
            |> SignalState.enqueue entryB

        let buildB () =
            empty
            |> enable Signal.SIGINT
            |> block t0 Signal.SIGTERM
            |> SignalState.enqueue entryA
            |> SignalState.enqueue entryB

        let a = buildA ()
        let b = buildB ()
        a |> shouldEqual b
        hash a |> shouldEqual (hash b)

        // The state after delivery must also compare equal to an
        // independently-rebuilt equivalent — exercises the path where
        // onReturnToUser rebuilds the pending list from a skipped/tail
        // split.
        let drainedFromA =
            match leaderDelivery [ t0 ; t1 ] a with
            | Some (SignalDelivery.RunHandlers _), s' -> s'
            | other, _ -> failwith $"expected a handler delivery from buildA, got %A{other}"

        let drainedFromB =
            match leaderDelivery [ t0 ; t1 ] b with
            | Some (SignalDelivery.RunHandlers _), s' -> s'
            | other, _ -> failwith $"expected a handler delivery from buildB, got %A{other}"

        drainedFromA |> shouldEqual drainedFromB
        hash drainedFromA |> shouldEqual (hash drainedFromB)

    [<Test>]
    let ``onReturnToUser surfaces the kernel default for a pending signal nobody enabled`` () : unit =
        // The scenario #1380 called out: a pending, non-enabled SIGTERM must
        // not sit queued forever — a kernel applies SIG_DFL and terminates.
        for signal in [ Signal.SIGTERM ; Signal.SIGINT ; Signal.SIGHUP ] do
            let s =
                empty
                |> SignalState.enqueue
                    {
                        Signal = signal
                        Target = ValueNone
                    }

            let delivery, s' = leaderDelivery [ t0 ] s

            delivery |> shouldEqual (Some (SignalDelivery.DefaultTerminate (signal, false)))
            SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``onReturnToUser surfaces Stop and Continue defaults`` () : unit =
        let deliveryFor (signal : Signal) : SignalDelivery<TestTask, TestHandler> option =
            empty
            |> SignalState.enqueue
                {
                    Signal = signal
                    Target = ValueNone
                }
            |> leaderDelivery [ t0 ]
            |> fst

        deliveryFor Signal.SIGTSTP
        |> shouldEqual (Some (SignalDelivery.DefaultStop Signal.SIGTSTP))

        deliveryFor Signal.SIGCONT
        |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))

    [<Test>]
    let ``a blocked default SIGCONT stays pending, and is discarded once unblocked`` () : unit =
        // Measured on Linux 6.18.5 and Darwin 27.0.0 by `sigpending-scope.c`:
        // SIGCONT at its default, blocked and sent with kill, was pending
        // after the kill had returned. This library has no stopped process
        // for it to resume, so what is left is the pending signal, gated by
        // the mask like any other.
        for numbering in everyNumbering do
            let cont = SignalMask.ofSignals numbering (Set.singleton Signal.SIGCONT)

            let entry =
                {
                    Signal = Signal.SIGCONT
                    Target = ValueNone
                }

            let s =
                initial numbering
                |> SignalState.changeMask SignalMaskChange.Block cont t0
                |> SignalState.enqueue entry

            let delivery, s = leaderDelivery [ t0 ] s
            delivery |> shouldEqual None
            SignalState.pending s |> shouldEqual [ entry ]
            SignalState.pendingBlocked t0 t0 s |> shouldEqual cont

            let delivery, s =
                s
                |> SignalState.changeMask SignalMaskChange.Unblock cont t0
                |> leaderDelivery [ t0 ]

            delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
            SignalState.pending s |> shouldEqual []

    [<Test>]
    let ``a return from sigsuspend that takes a default SIGCONT goes on under the call's temporary mask`` () : unit =
        // Linux takes SIGCONT (18) before SIGWINCH (28). Its `get_signal`
        // passes over a signal whose default is to do nothing and goes on
        // taking signals under the call's temporary mask, so SIGWINCH, which
        // the mask from before the call blocks, is delivered, and its frame
        // saves that mask for its `sigreturn` to restore. A return that then
        // takes nothing restores the mask at once.
        let before =
            SignalMask.ofSignals SignalNumbering.Linux (Set.ofList [ Signal.SIGCONT ; Signal.SIGWINCH ])

        let toProcess (signal : Signal) : PendingSignal<TestTask> =
            {
                Signal = signal
                Target = ValueNone
            }

        let suspended (pending : Signal list) : SignalState<TestTask, TestHandler> =
            pending
            |> List.fold
                (fun s signal -> SignalState.enqueue (toProcess signal) s)
                (empty
                 |> enable Signal.SIGWINCH
                 |> SignalState.changeMask SignalMaskChange.Block before t0)
            |> SignalState.suspend t0 SignalMask.empty

        let delivery, s =
            leaderDelivery [ t0 ] (suspended [ Signal.SIGCONT ; Signal.SIGWINCH ])

        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.maskToRestore t0 s |> shouldEqual (Some before)
        SignalState.maskOf t0 s |> shouldEqual SignalMask.empty

        let delivery, s = leaderDelivery [ t0 ] s

        match delivery with
        | Some (SignalDelivery.RunHandlers [ frame ]) ->
            frame.Entry |> shouldEqual (toProcess Signal.SIGWINCH)
            frame.SavedMask |> shouldEqual before
        | other -> failwith $"expected SIGWINCH's handler, got %A{other}"

        SignalState.maskToRestore t0 s |> shouldEqual None

        let delivery, s = leaderDelivery [ t0 ] (suspended [ Signal.SIGCONT ])
        delivery |> shouldEqual (Some (SignalDelivery.DefaultContinue Signal.SIGCONT))
        SignalState.maskToRestore t0 s |> shouldEqual (Some before)

        let delivery, s = leaderDelivery [ t0 ] s
        delivery |> shouldEqual None
        SignalState.maskToRestore t0 s |> shouldEqual None
        SignalState.maskOf t0 s |> shouldEqual before

    [<Test>]
    let ``generating an unclaimed fatal signal terminates the process at once`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        generateAmong CoreDumps.Suppressed [ t0 ] entry empty
        |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.SIGTERM, false))

        // SIGKILL cannot be blocked, so a mask naming it changes nothing.
        let masked = empty |> block t0 Signal.SIGKILL

        generateAmong
            CoreDumps.Suppressed
            [ t0 ]
            {
                Signal = Signal.SIGKILL
                Target = ValueNone
            }
            masked
        |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.SIGKILL, false))

    [<Test>]
    let ``generating an unclaimed stop signal stops the process at once`` () : unit =
        generateAmong
            CoreDumps.Suppressed
            [ t0 ]
            {
                Signal = Signal.SIGSTOP
                Target = ValueNone
            }
            empty
        |> shouldEqual (SignalGeneration.ProcessStopped (Signal.SIGSTOP, empty))

    [<Test>]
    let ``a fatal signal nobody can receive yet is queued, not fatal`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        // Every task blocks it.
        let blocked = empty |> block t0 Signal.SIGTERM |> block t1 Signal.SIGTERM

        let s' =
            generateAmong CoreDumps.Suppressed [ t0 ; t1 ] entry blocked |> continuesWith

        SignalState.pending s' |> shouldEqual [ entry ]

        // A thread-directed one whose target blocks it waits even though
        // another task would not block it.
        let directed =
            { entry with
                Target = ValueSome t0
            }

        let s' =
            generateAmong CoreDumps.Suppressed [ t0 ; t1 ] directed blocked |> continuesWith

        SignalState.pending s' |> shouldEqual [ directed ]

    [<Test>]
    let ``a signal a handler claims is queued for it`` () : unit =
        let entry =
            {
                Signal = Signal.SIGTERM
                Target = ValueNone
            }

        let claimed = empty |> enable Signal.SIGTERM

        let s' = generateAmong CoreDumps.Suppressed [ t0 ] entry claimed |> continuesWith

        SignalState.pending s' |> shouldEqual [ entry ]

    [<Test>]
    let ``an ignored signal some thread could receive is discarded at generation`` () : unit =
        for numbering in everyNumbering do
            let entry =
                {
                    Signal = Signal.SIGCHLD
                    Target = ValueNone
                }

            let s = initial numbering

            generateAmong CoreDumps.Suppressed [ t0 ] entry s
            |> shouldEqual (SignalGeneration.ProcessContinues s)

            // Nothing is left for a later handler to claim.
            let claimedLater =
                generateAmong CoreDumps.Suppressed [ t0 ] entry s
                |> continuesWith
                |> enable Signal.SIGCHLD

            leaderDelivery [ t0 ] claimedLater |> fst |> shouldEqual None

    [<Test>]
    let ``an ignored signal every thread blocks is left pending at generation under Linux numbering`` () : unit =
        let entry =
            {
                Signal = Signal.SIGCHLD
                Target = ValueNone
            }

        let blocked = empty |> block t0 Signal.SIGCHLD

        let s' = generateAmong CoreDumps.Suppressed [ t0 ] entry blocked |> continuesWith

        SignalState.pending s' |> shouldEqual [ entry ]

    [<Test>]
    let ``a default action still requires a receiver`` () : unit =
        // A pending terminate-default signal blocked by every live thread
        // stays pending, exactly as a handler delivery would.
        let s =
            empty
            |> block t0 Signal.SIGTERM
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGTERM
                    Target = ValueNone
                }

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``a receivable ignored signal is discarded with no action`` () : unit =
        // Linux keeps an ignored signal pending only while nothing could
        // receive it; the delivery scan is what discards it. The scan must
        // report the state change even though there is no action.
        let s =
            empty
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCHLD
                    Target = ValueNone
                }

        SignalState.pending s |> List.length |> shouldEqual 1

        let delivery, s' = leaderDelivery [ t0 ] s

        delivery |> shouldEqual None
        SignalState.pending s' |> shouldEqual []
        s' |> shouldEqual empty

    [<Test>]
    let ``an ignored signal blocked everywhere stays pending under Linux numbering`` () : unit =
        // The measured Linux rule: blocked-and-ignored stays pending, and a
        // handler enabled before the unblock receives it — or, still ignored
        // when a receiver appears, it is discarded.
        let entry =
            {
                Signal = Signal.SIGCHLD
                Target = ValueNone
            }

        let held = empty |> block t0 Signal.SIGCHLD |> SignalState.enqueue entry

        let delivery, afterScan = leaderDelivery [ t0 ] held

        delivery |> shouldEqual None
        SignalState.pending afterScan |> shouldEqual [ entry ]

        // Handler claimed before the unblock: delivered.
        let claimed = afterScan |> enable Signal.SIGCHLD |> HandlerFrames.leave t0

        let delivery, s' = leaderDelivery [ t0 ] claimed
        oneHandler delivery |> shouldEqual entry
        SignalState.pending s' |> shouldEqual []

        // Still ignored at the unblock: discarded.
        let discarded = afterScan |> HandlerFrames.leave t0

        let delivery, s' = leaderDelivery [ t0 ] discarded

        delivery |> shouldEqual None
        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``Darwin numbering discards an ignored signal at generation even when blocked`` () : unit =
        // The measured Darwin rule: the block does not preserve it, so the
        // handler-before-unblock rescue that works on Linux has nothing to
        // rescue.
        let blocked = initial SignalNumbering.Darwin |> block t0 Signal.SIGCHLD

        let s =
            blocked
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCHLD
                    Target = ValueNone
                }

        s |> shouldEqual blocked
        SignalState.pending s |> shouldEqual []

    [<Test>]
    let ``a discarded ignored entry does not stop the scan`` () : unit =
        // An ignored entry taken first (SIGHUP is 1) is dropped and the walk
        // carries on to deliver the caught entry behind it, all in one call.
        let ignored =
            {
                Signal = Signal.SIGHUP
                Target = ValueNone
            }

        let handled =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s =
            empty
            |> SignalState.setDisposition Signal.SIGHUP SignalDisposition.Ignore
            |> enable Signal.SIGINT
            |> SignalState.enqueue ignored
            |> SignalState.enqueue handled

        SignalState.pending s |> shouldEqual [ ignored ; handled ]

        let delivery, s' = leaderDelivery [ t0 ] s
        oneHandler delivery |> shouldEqual handled
        SignalState.pending s' |> shouldEqual []

    [<Test>]
    let ``onReturnToUser returns nothing for an empty queue`` () : unit =
        let delivery, s' = leaderDelivery [ t0 ] empty

        delivery |> shouldEqual None
        s' |> shouldEqual empty

    [<Test>]
    let ``onReturnToUser gives a signal pending on the process to the leader alone`` () : unit =
        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let s = empty |> enable Signal.SIGINT |> SignalState.enqueue entry
        let tasks = Set.ofList [ t2 ; t0 ; t1 ]

        for task in [ t1 ; t2 ] do
            SignalState.onReturnToUser CoreDumps.Suppressed t0 tasks task s
            |> shouldEqual (Ok (None, s))

        match SignalState.onReturnToUser CoreDumps.Suppressed t0 tasks t0 s with
        | Ok (Some (SignalDelivery.RunHandlers [ frame ]), s') ->
            frame.Entry |> shouldEqual entry
            frame.Action.Handler |> shouldEqual handler
            SignalState.pending s' |> shouldEqual []
            SignalState.framesOf t0 s' |> shouldEqual [ frame ]
        | other -> failwith $"expected one handler frame for the leader, got %A{other}"

    [<Test>]
    let ``onReturnToUser refuses every task while a caught signal pending on the process could reach only a non-leader``
        ()
        : unit
        =
        let s =
            empty
            |> enable Signal.SIGINT
            |> block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        for task in [ t0 ; t1 ; t2 ] do
            SignalState.onReturnToUser CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ; t2 ]) task s
            |> shouldEqual (Error (SignalReceiverRefusal.LeaderBlocks Signal.SIGINT))

    [<Test>]
    let ``generate refuses a caught signal for the process that only a non-leader could receive`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> block t0 Signal.SIGINT
            |> block t1 Signal.SIGINT

        let entry =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        SignalState.generate CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ; t2 ]) entry s
        |> shouldEqual (Error (SignalReceiverRefusal.LeaderBlocks Signal.SIGINT))

        // Once every task blocks it, it is simply pending.
        SignalState.generate CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) entry s
        |> Result.map continuesWith
        |> Result.map SignalState.pending
        |> shouldEqual (Ok [ entry ])

    [<Test>]
    let ``a default SIGCONT sent to the process that only a task but the leader could take is refused`` () : unit =
        // Whichever task takes it discards it as it returns to user mode, and
        // only the leader is asked.
        let entry =
            {
                Signal = Signal.SIGCONT
                Target = ValueNone
            }

        let s =
            empty
            |> SignalState.changeMask
                SignalMaskChange.Block
                (SignalMask.ofSignals SignalNumbering.Linux (Set.singleton Signal.SIGCONT))
                t0

        SignalState.generate CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) entry s
        |> shouldEqual (Error (SignalReceiverRefusal.LeaderBlocks Signal.SIGCONT))

        SignalState.onReturnToUser CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) t0 (SignalState.enqueue entry s)
        |> shouldEqual (Error (SignalReceiverRefusal.LeaderBlocks Signal.SIGCONT))

        // With every task blocking it, it is simply pending.
        SignalState.generate CoreDumps.Suppressed t0 (Set.singleton t0) entry s
        |> Result.map continuesWith
        |> Result.map SignalState.pending
        |> shouldEqual (Ok [ entry ])

    [<Test>]
    let ``onReturnToUser holds a signal every task blocks`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> block t0 Signal.SIGINT
            |> block t1 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }

        let delivery, s' = leaderDelivery [ t0 ; t1 ] s

        delivery |> shouldEqual None
        s' |> shouldEqual s

    [<Test>]
    let ``onReturnToUser does not redirect a targeted signal to another thread`` () : unit =
        // pthread_kill is pinned: even though t1 is unblocked, a signal
        // targeted at t0 must stay queued, not get delivered to t1.
        let s =
            empty
            |> enable Signal.SIGINT
            |> block t0 Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t0
                }

        for task in [ t0 ; t1 ] do
            SignalState.onReturnToUser CoreDumps.Suppressed t0 (Set.ofList [ t0 ; t1 ]) task s
            |> shouldEqual (Ok (None, s))

    [<Test>]
    let ``a signal pending on something that is not a task fails loudly`` () : unit =
        let s =
            empty
            |> enable Signal.SIGINT
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGINT
                    Target = ValueSome t2
                }

        let exn = Assert.Throws<exn> (fun () -> leaderDelivery [ t0 ; t1 ] s |> ignore)

        exn.Message |> shouldContainText "not one of the process's tasks"

    [<Test>]
    let ``asking with a leader or task that is not a task fails loudly`` () : unit =
        let tasks = Set.ofList [ t0 ; t1 ]

        (Assert.Throws<exn> (fun () -> SignalState.onReturnToUser CoreDumps.Suppressed t2 tasks t0 empty |> ignore))
            .Message
        |> shouldContainText "leader"

        (Assert.Throws<exn> (fun () -> SignalState.onReturnToUser CoreDumps.Suppressed t0 tasks t2 empty |> ignore))
            .Message
        |> shouldContainText "task asked"

        (Assert.Throws<exn> (fun () ->
            SignalState.generate
                CoreDumps.Suppressed
                t2
                tasks
                {
                    Signal = Signal.SIGINT
                    Target = ValueNone
                }
                empty
            |> ignore
        ))
            .Message
        |> shouldContainText "leader"

    [<Test>]
    let ``onReturnToUser walks past what the task blocks and what is not its own, leaving both`` () : unit =
        // SIGHUP (1) is pending on t1 alone, SIGINT (2) on the process, and
        // SIGQUIT (3) on the process but blocked everywhere: the leader skips the
        // first as not its own and the last as blocked, and takes SIGINT.
        let atT1 =
            {
                Signal = Signal.SIGHUP
                Target = ValueSome t1
            }

        let deliverable =
            {
                Signal = Signal.SIGINT
                Target = ValueNone
            }

        let held =
            {
                Signal = Signal.SIGQUIT
                Target = ValueNone
            }

        let s =
            empty
            |> enable Signal.SIGHUP
            |> enable Signal.SIGINT
            |> enable Signal.SIGQUIT
            |> block t0 Signal.SIGQUIT
            |> block t1 Signal.SIGQUIT
            |> SignalState.enqueue held
            |> SignalState.enqueue atT1
            |> SignalState.enqueue deliverable

        let delivery, s' = leaderDelivery [ t0 ; t1 ] s
        oneHandler delivery |> shouldEqual deliverable
        SignalState.pending s' |> Seq.toList |> shouldEqual [ held ; atT1 ]

    // ----------------------- Handler frames ----------------------- //

    let private processSignal (signal : Signal) : PendingSignal<TestTask> =
        {
            Signal = signal
            Target = ValueNone
        }

    let private catchWith
        (signal : Signal)
        (f : SignalCatch<TestHandler> -> SignalCatch<TestHandler>)
        (s : SignalState<TestTask, TestHandler>)
        : SignalState<TestTask, TestHandler>
        =
        SignalState.setDisposition signal (SignalDisposition.Catch (f (SignalCatch.ofHandler handler))) s

    [<Test>]
    let ``every caught signal taken at one return gets a frame, the last taken innermost`` () : unit =
        // Measured by `signal-pick-order.c` (84 of 84 runs on each flavour)
        // and the signal fuzzer: with empty sa_masks, every pending caught
        // signal is given a frame before any handler runs, so the handlers
        // run in the reverse of the order the signals are taken in. Each
        // frame's mask adds its signal to the one before.
        for numbering in everyNumbering do
            let s =
                initial numbering
                |> enable Signal.SIGHUP
                |> enable Signal.SIGINT
                |> enable Signal.SIGTERM
                |> SignalState.enqueue (processSignal Signal.SIGTERM)
                |> SignalState.enqueue (processSignal Signal.SIGHUP)
                |> SignalState.enqueue (processSignal Signal.SIGINT)

            match leaderDelivery [ t0 ] s with
            | Some (SignalDelivery.RunHandlers frames), s' ->
                // Each frame saves the mask before it, which its sigreturn
                // restores: the mask each outer handler then runs under.
                frames
                |> List.map (fun frame -> frame.Entry.Signal, SignalMask.signals frame.SavedMask)
                |> shouldEqual
                    [
                        Signal.SIGTERM, Set.ofList [ Signal.SIGHUP ; Signal.SIGINT ]
                        Signal.SIGINT, Set.singleton Signal.SIGHUP
                        Signal.SIGHUP, Set.empty
                    ]

                SignalState.maskOf t0 s'
                |> SignalMask.signals
                |> shouldEqual (Set.ofList [ Signal.SIGHUP ; Signal.SIGINT ; Signal.SIGTERM ])

                SignalState.framesOf t0 s' |> shouldEqual frames
                SignalState.pending s' |> shouldEqual []
            | other, _ -> failwith $"expected three frames, got %A{other}"

    [<Test>]
    let ``a frame's sa_mask holds back a signal until that frame's sigreturn`` () : unit =
        // SIGINT's handler blocks SIGHUP, so SIGHUP generated while it runs
        // waits for its return, and is then taken at the next return to user
        // mode.
        let s =
            empty
            |> catchWith
                Signal.SIGINT
                (fun c ->
                    { c with
                        Mask = SignalMask.ofSignals SignalNumbering.Linux (Set.singleton Signal.SIGHUP)
                    }
                )
            |> enable Signal.SIGHUP
            |> SignalState.enqueue (processSignal Signal.SIGINT)

        let delivery, s = leaderDelivery [ t0 ] s
        oneHandler delivery |> shouldEqual (processSignal Signal.SIGINT)

        let s = s |> SignalState.enqueue (processSignal Signal.SIGHUP)
        let delivery, s = leaderDelivery [ t0 ] s
        delivery |> shouldEqual None
        SignalState.pending s |> shouldEqual [ processSignal Signal.SIGHUP ]

        let delivery, s = s |> HandlerFrames.leave t0 |> leaderDelivery [ t0 ]
        oneHandler delivery |> shouldEqual (processSignal Signal.SIGHUP)

        SignalState.maskOf t0 s
        |> SignalMask.signals
        |> shouldEqual (Set.singleton Signal.SIGHUP)

    [<Test>]
    let ``SA_NODEFER leaves the signal unblocked, so its instances nest`` () : unit =
        // Measured by `signal-sigaction-flags.c`: re-raised inside its handler, a
        // signal waits for the return without SA_NODEFER and nests with it.
        // Three queued instances of a real-time signal all get frames at once.
        let rt = processSignal (Signal.RealTime 4)

        let s =
            initial SignalNumbering.Linux
            |> catchWith
                (Signal.RealTime 4)
                (fun c ->
                    { c with
                        NoDefer = true
                    }
                )
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt

        match leaderDelivery [ t0 ] s with
        | Some (SignalDelivery.RunHandlers frames), s' ->
            frames |> List.map (fun frame -> frame.Entry) |> shouldEqual [ rt ; rt ; rt ]

            frames
            |> List.map (fun frame -> frame.SavedMask)
            |> shouldEqual [ SignalMask.empty ; SignalMask.empty ; SignalMask.empty ]

            SignalState.maskOf t0 s' |> shouldEqual SignalMask.empty

            SignalState.pending s' |> shouldEqual []
        | other, _ -> failwith $"expected three nested frames, got %A{other}"

    [<Test>]
    let ``SA_RESETHAND restores the default at delivery, but for Darwin's SIGILL and SIGTRAP`` () : unit =
        // Measured by `signal-sigaction-flags.c` (29 of 29 signals on Linux, 27 of 29
        // on Darwin) and the signal fuzzer. It does not imply SA_NODEFER.
        for numbering in everyNumbering do
            for signo in [ 1 ; 2 ; 4 ; 5 ; 15 ] do
                let signal =
                    match Signal.ofRawSignoUnder numbering signo with
                    | ValueSome signal -> signal
                    | ValueNone -> failwith $"%O{numbering}: %d{signo} is a signal"

                let s =
                    initial numbering
                    |> catchWith
                        signal
                        (fun c ->
                            { c with
                                ResetHand = true
                            }
                        )
                    |> SignalState.enqueue (processSignal signal)

                let delivery, s = leaderDelivery [ t0 ] s
                oneHandler delivery |> shouldEqual (processSignal signal)

                SignalState.maskOf t0 s
                |> SignalMask.signals
                |> shouldEqual (Set.singleton signal)

                let keeps = numbering = SignalNumbering.Darwin && (signo = 4 || signo = 5)

                match SignalState.disposition signal s with
                | SignalDisposition.Default -> keeps |> shouldEqual false
                | SignalDisposition.Catch _ -> keeps |> shouldEqual true
                | SignalDisposition.Ignore -> failwith "SA_RESETHAND left the signal ignored"

    [<Test>]
    let ``a second instance reset to its default by SA_RESETHAND takes that default at the same return`` () : unit =
        // Two real-time instances, the handler with SA_NODEFER and
        // SA_RESETHAND: the first gets a frame, which resets the disposition,
        // and the second then terminates the process.
        let rt = processSignal (Signal.RealTime 4)

        let s =
            initial SignalNumbering.Linux
            |> catchWith
                (Signal.RealTime 4)
                (fun c ->
                    { c with
                        NoDefer = true
                        ResetHand = true
                    }
                )
            |> SignalState.enqueue rt
            |> SignalState.enqueue rt

        leaderDelivery [ t0 ] s
        |> fst
        |> shouldEqual (Some (SignalDelivery.DefaultTerminate (Signal.RealTime 4, false)))

    [<Test>]
    let ``a fatal default behind a caught signal kills the process, and no handler runs`` () : unit =
        // Linux takes SIGHUP (1) before SIGUSR1 (10): SIGHUP gets a frame, and
        // SIGUSR1 at its default kills the process before any handler runs.
        // Measured by the signal fuzzer.
        let s =
            empty
            |> enable Signal.SIGHUP
            |> SignalState.enqueue (processSignal Signal.SIGUSR1)
            |> SignalState.enqueue (processSignal Signal.SIGHUP)

        leaderDelivery [ t0 ] s
        |> fst
        |> shouldEqual (Some (SignalDelivery.DefaultTerminate (Signal.SIGUSR1, false)))

    [<Test>]
    let ``a stop or continue default behind a caught signal is refused`` () : unit =
        // SIGHUP (1) is taken before SIGTSTP (20) and SIGCONT (18) on Linux.
        for stopOrContinue in [ Signal.SIGTSTP ; Signal.SIGCONT ] do
            let s =
                empty
                |> enable Signal.SIGHUP
                |> SignalState.enqueue (processSignal stopOrContinue)
                |> SignalState.enqueue (processSignal Signal.SIGHUP)

            SignalState.onReturnToUser CoreDumps.Suppressed t0 (Set.singleton t0) t0 s
            |> shouldEqual (Error (SignalReceiverRefusal.DefaultBehindHandlers stopOrContinue))

    // ----------------------- Property tests ----------------------- //

    /// Operation language for the random property test. Each constructor
    /// maps to exactly one public method on the API, except `Spawn`, which is
    /// a task joining the process: it has no entries yet, so only the task set
    /// the operations are given changes.
    type private Op =
        | SetDisposition of signal : Signal * disposition : SignalDisposition<TestHandler>
        /// `changeMask`, as `sigprocmask(2)` calls it, with a set of signals.
        | ChangeMask of task : TestTask * change : SignalMaskChange * set : Set<Signal>
        | Enqueue of entry : PendingSignal<TestTask>
        | Generate of coreDumps : CoreDumps * entry : PendingSignal<TestTask>
        | Deliver of coreDumps : CoreDumps * task : TestTask
        /// `sigreturn` of the task's innermost frame, which it has.
        | Sigreturn of task : TestTask
        /// `suspend`, as `sigsuspend(2)` calls it, by a task with no mask to
        /// restore already.
        | Suspend of task : TestTask * temporary : Set<Signal>
        | Spawn of task : TestTask
        | Exit of task : TestTask

    /// The leader of every process the property test runs. It never exits.
    let private referenceLeader : TestTask = t0

    /// Every task the property test's processes can have.
    let private taskPool : TestTask list = [ t0 ; t1 ; t2 ; TestTask 3 ]

    /// Reference implementation: simple lists / sets / maps, completely
    /// independent of the production module's internal representation.
    ///
    /// It stores every disposition it is given, `Default` included: the
    /// production module must store none, and `assertEquivalent` checks that
    /// it answers every read the same way *and* holds no stored default.
    ///
    /// It keeps pending signals in the order they were generated, and sorts
    /// them only when asked which comes first, where the production module
    /// keeps them sorted as they arrive. It keeps each task's frames as a
    /// list that may be empty, where the production module drops an empty
    /// stack.
    type private ReferenceState =
        {
            Dispositions : Map<Signal, SignalDisposition<TestHandler>>
            /// Each task's mask; may hold an empty one.
            Blocked : Map<TestTask, Set<Signal>>
            /// The mask each task in `sigsuspend` gets back.
            Restore : Map<TestTask, Set<Signal>>
            Frames : Map<TestTask, HandlerFrame<TestTask, TestHandler> list>
            NextFrame : int64
            Pending : PendingSignal<TestTask> list
        }

    let private referenceEmpty : ReferenceState =
        {
            Dispositions = Map.empty
            Blocked = Map.empty
            Restore = Map.empty
            Frames = Map.empty
            NextFrame = 0L
            Pending = []
        }

    let private referenceDisposition (r : ReferenceState) (signal : Signal) : SignalDisposition<TestHandler> =
        Map.tryFind signal r.Dispositions
        |> Option.defaultValue SignalDisposition.Default

    let private referenceFrames (r : ReferenceState) (task : TestTask) : HandlerFrame<TestTask, TestHandler> list =
        Map.tryFind task r.Frames |> Option.defaultValue []

    let private referenceMask (r : ReferenceState) (task : TestTask) : Set<Signal> =
        Map.tryFind task r.Blocked |> Option.defaultValue Set.empty

    let private referenceBlocks (r : ReferenceState) (task : TestTask) (signal : Signal) : bool =
        Set.contains signal (referenceMask r task)

    /// SIGKILL and SIGSTOP, written as numbers: what a mask never holds.
    let private referenceUnmaskable (numbering : SignalNumbering) (signal : Signal) : bool =
        match numbering, Signal.toRawSignoUnder numbering signal with
        | _, 9
        | SignalNumbering.Linux, 19
        | SignalNumbering.Darwin, 17 -> true
        | _, _ -> false

    /// A disposition as the kernel stores it: its mask without SIGKILL and
    /// SIGSTOP.
    let private referenceStored
        (numbering : SignalNumbering)
        (disposition : SignalDisposition<TestHandler>)
        : SignalDisposition<TestHandler>
        =
        match disposition with
        | SignalDisposition.Catch action ->
            SignalDisposition.Catch
                { action with
                    Mask =
                        action.Mask
                        |> SignalMask.signals
                        |> Set.filter (referenceUnmaskable numbering >> not)
                        |> SignalMask.ofSignals numbering
                }
        | other -> other

    /// The measured table, stated per disposition rather than through the
    /// production module's helpers: which dispositions ignore a signal as it
    /// is generated.
    let private referenceIgnoredAtGeneration
        (numbering : SignalNumbering)
        (r : ReferenceState)
        (signal : Signal)
        : bool
        =
        match referenceDisposition r signal, Signal.defaultDispositionUnder numbering signal with
        | SignalDisposition.Ignore, _ -> true
        | SignalDisposition.Default, DefaultDisposition.Ignore -> true
        | _, _ -> false

    /// Which dispositions discard a signal's pending instances as they are
    /// set: the generation-time ones, plus a default SIGCONT.
    let private referenceDiscardsWhenSet
        (numbering : SignalNumbering)
        (disposition : SignalDisposition<TestHandler>)
        (signal : Signal)
        : bool
        =
        match disposition, Signal.defaultDispositionUnder numbering signal with
        | SignalDisposition.Ignore, _ -> true
        | SignalDisposition.Default, DefaultDisposition.Ignore -> true
        | SignalDisposition.Default, DefaultDisposition.Continue -> true
        | _, _ -> false

    /// Whether `signal` is at its default and its default continues the process.
    let private referenceContinuesAtDefault
        (numbering : SignalNumbering)
        (r : ReferenceState)
        (signal : Signal)
        : bool
        =
        referenceDisposition r signal = SignalDisposition.Default
        && Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Continue

    [<RequireQualifiedAccess>]
    type private ReferenceReceiver =
        | Task of TestTask
        | BeyondLeader
        | Nobody

    /// Who would take `e` now: its target, if not blocking; for the process's
    /// own, the leader if not blocking, and otherwise "some other task", which
    /// the model does not name.
    let private referenceReceiver
        (tasks : Set<TestTask>)
        (r : ReferenceState)
        (e : PendingSignal<TestTask>)
        : ReferenceReceiver
        =
        match e.Target with
        | ValueSome task ->
            if referenceBlocks r task e.Signal then
                ReferenceReceiver.Nobody
            else
                ReferenceReceiver.Task task
        | ValueNone ->
            if not (referenceBlocks r referenceLeader e.Signal) then
                ReferenceReceiver.Task referenceLeader
            elif
                tasks
                |> Set.toList
                |> List.exists (fun task -> not (referenceBlocks r task e.Signal))
            then
                ReferenceReceiver.BeyondLeader
            else
                ReferenceReceiver.Nobody

    /// The order of the measured pick rules, written out from the probe's rows
    /// rather than taken from the module: Linux takes ILL, TRAP, BUS, FPE, SEGV
    /// and SYS (4, 5, 7, 8, 11, 31) first, then the lowest number; Darwin the
    /// lowest number.
    let private referencePickKey (numbering : SignalNumbering) (signal : Signal) : int * int =
        let signo = Signal.toRawSignoUnder numbering signal

        match numbering with
        | SignalNumbering.Linux when List.contains signo [ 4 ; 5 ; 7 ; 8 ; 11 ; 31 ] -> 0, signo
        | SignalNumbering.Linux -> 1, signo
        | SignalNumbering.Darwin -> 0, signo

    /// What `task` could take, in the order it takes them: a naive stable sort
    /// of the entries in generation order. Linux takes its own set before the
    /// process's; Darwin takes them as one, its own first where they tie.
    let private referenceCandidates
        (numbering : SignalNumbering)
        (task : TestTask)
        (r : ReferenceState)
        : PendingSignal<TestTask> list
        =
        let own = r.Pending |> List.filter (fun e -> e.Target = ValueSome task)

        let shared =
            if task = referenceLeader then
                r.Pending |> List.filter (fun e -> e.Target = ValueNone)
            else
                []

        let byKey (entries : PendingSignal<TestTask> list) : PendingSignal<TestTask> list =
            entries |> List.sortBy (fun e -> referencePickKey numbering e.Signal)

        match numbering with
        | SignalNumbering.Linux -> byKey own @ byKey shared
        | SignalNumbering.Darwin -> byKey (own @ shared)

    /// Every pending entry grouped by set, the process's first, each set in the
    /// order it is taken in: what `SignalState.pending` promises.
    let private referencePendingView (numbering : SignalNumbering) (r : ReferenceState) : PendingSignal<TestTask> list =
        r.Pending
        |> List.sortBy (fun e -> e.Target, referencePickKey numbering e.Signal)

    /// `pending` without its first entry equal to `e`: the earliest generated.
    let rec private referenceRemoveFirst
        (e : PendingSignal<TestTask>)
        (pending : PendingSignal<TestTask> list)
        : PendingSignal<TestTask> list
        =
        match pending with
        | [] -> failwith $"reference: %A{e} is not pending"
        | head :: tail when head = e -> tail
        | head :: tail -> head :: referenceRemoveFirst e tail

    /// The first step of generating `signal`: `None` if Darwin
    /// drops it as ignored before anything else (SIGCONT excepted), and
    /// otherwise the state after the opposite kind's pending instances are
    /// flushed.
    let private referenceBeginGeneration
        (numbering : SignalNumbering)
        (signal : Signal)
        (r : ReferenceState)
        : ReferenceState option
        =
        let droppedFirst =
            numbering = SignalNumbering.Darwin
            && signal <> Signal.SIGCONT
            && referenceIgnoredAtGeneration numbering r signal

        if droppedFirst then
            None
        else
            let isStop (x : Signal) : bool =
                Signal.defaultDispositionUnder numbering x = DefaultDisposition.Stop

            let flushes (pendingSignal : Signal) : bool =
                (signal = Signal.SIGCONT && isStop pendingSignal)
                || (isStop signal && pendingSignal = Signal.SIGCONT)

            Some
                { r with
                    Pending = r.Pending |> List.filter (fun p -> not (flushes p.Signal))
                }

    /// Coalescing of standard signals, on an entry already admitted.
    let private referenceAdmit
        (numbering : SignalNumbering)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : ReferenceState
        =
        let alreadyPendingInSet =
            r.Pending
            |> List.exists (fun p -> p.Signal = entry.Signal && p.Target = entry.Target)

        if alreadyPendingInSet && not (Signal.isRealTimeUnder numbering entry.Signal) then
            r
        else
            { r with
                Pending = r.Pending @ [ entry ]
            }

    let private referenceEnqueue
        (numbering : SignalNumbering)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : ReferenceState
        =
        match referenceBeginGeneration numbering entry.Signal r with
        | None -> r
        | Some r -> referenceAdmit numbering entry r

    let private referenceCore (numbering : SignalNumbering) (coreDumps : CoreDumps) (signal : Signal) : bool =
        coreDumps = CoreDumps.Written && Signal.dumpsCoreUnder numbering signal

    /// The reference's `SignalGeneration`: what generating a signal does, with
    /// the state the process carries on with where it carries on.
    [<RequireQualifiedAccess>]
    type private ReferenceGeneration =
        | Continues of ReferenceState
        | Terminated of Signal * coreDumped : bool
        | Stopped of Signal * ReferenceState
        | Refused of Signal
        /// Darwin's numbering, and the signal would be left pending beside an
        /// instance in the set Darwin holds as the same one.
        | RefusedAsMerged of Signal

    /// What generating `entry` does at once, and the state after.
    /// A default-disposition signal some task could receive takes its default
    /// here: terminate, stop, or be discarded if the default ignores it; an
    /// ignored one some task could receive is discarded. A caught one for the
    /// process that only a non-leader could receive is refused, and so, under
    /// Darwin's numbering, is one left pending beside an instance in the set
    /// Darwin holds as the same: it keeps a signal sent to the process in the
    /// leader's own set.
    let private referenceGenerate
        (numbering : SignalNumbering)
        (coreDumps : CoreDumps)
        (tasks : Set<TestTask>)
        (entry : PendingSignal<TestTask>)
        (r : ReferenceState)
        : ReferenceGeneration
        =
        match referenceBeginGeneration numbering entry.Signal r with
        | None -> ReferenceGeneration.Continues r
        | Some r ->
            let receiver = referenceReceiver tasks r entry

            let darwinSet (target : TestTask voption) : TestTask =
                match target with
                | ValueNone -> referenceLeader
                | ValueSome task -> task

            let pend () : ReferenceGeneration =
                let mergedOnDarwin =
                    numbering = SignalNumbering.Darwin
                    && r.Pending
                       |> List.exists (fun p ->
                           p.Signal = entry.Signal
                           && p.Target <> entry.Target
                           && darwinSet p.Target = darwinSet entry.Target
                       )

                if mergedOnDarwin then
                    ReferenceGeneration.RefusedAsMerged entry.Signal
                else
                    ReferenceGeneration.Continues (referenceAdmit numbering entry r)

            match
                receiver, referenceDisposition r entry.Signal, Signal.defaultDispositionUnder numbering entry.Signal
            with
            | ReferenceReceiver.Nobody, _, _ -> pend ()
            | ReferenceReceiver.BeyondLeader, SignalDisposition.Catch _, _ -> ReferenceGeneration.Refused entry.Signal
            | _, SignalDisposition.Catch _, _ -> pend ()
            | _, SignalDisposition.Ignore, _ -> ReferenceGeneration.Continues r
            | _, SignalDisposition.Default, DefaultDisposition.Terminate ->
                ReferenceGeneration.Terminated (entry.Signal, referenceCore numbering coreDumps entry.Signal)
            | _, SignalDisposition.Default, DefaultDisposition.Stop -> ReferenceGeneration.Stopped (entry.Signal, r)
            | _, SignalDisposition.Default, DefaultDisposition.Ignore -> ReferenceGeneration.Continues r
            | ReferenceReceiver.BeyondLeader, SignalDisposition.Default, DefaultDisposition.Continue ->
                ReferenceGeneration.Refused entry.Signal
            | _, SignalDisposition.Default, DefaultDisposition.Continue -> pend ()

    /// The reference's return to user mode: after every action, start again
    /// from the head of the task's candidates and take the first it can, where
    /// the production module walks them once. A distinct algorithm, so a
    /// regression in either side surfaces as a divergence.
    let private referenceOnReturnToUser
        (numbering : SignalNumbering)
        (coreDumps : CoreDumps)
        (tasks : Set<TestTask>)
        (task : TestTask)
        (r : ReferenceState)
        : Result<SignalDelivery<TestTask, TestHandler> option * ReferenceState, SignalReceiverRefusal>
        =
        let refused =
            referencePendingView numbering r
            |> List.tryFind (fun e -> referenceReceiver tasks r e = ReferenceReceiver.BeyondLeader)

        match refused with
        | Some e -> Error (SignalReceiverRefusal.LeaderBlocks e.Signal)
        | None ->

        let rec go
            (pushed : HandlerFrame<TestTask, TestHandler> list)
            (r : ReferenceState)
            : Result<SignalDelivery<TestTask, TestHandler> option * ReferenceState, SignalReceiverRefusal>
            =
            let next =
                referenceCandidates numbering task r
                |> List.tryFind (fun e -> not (referenceBlocks r task e.Signal))

            let removed (e : PendingSignal<TestTask>) : ReferenceState =
                { r with
                    Pending = referenceRemoveFirst e r.Pending
                }

            let alone (e : PendingSignal<TestTask>) (delivery : SignalDelivery<TestTask, TestHandler>) =
                if pushed.IsEmpty then
                    Ok (Some delivery, removed e)
                else
                    Error (SignalReceiverRefusal.DefaultBehindHandlers e.Signal)

            match next with
            | None when pushed.IsEmpty -> Ok (None, r)
            | None -> Ok (Some (SignalDelivery.RunHandlers pushed), r)
            | Some e when referenceContinuesAtDefault numbering r e.Signal ->
                alone e (SignalDelivery.DefaultContinue e.Signal)
            | Some e ->

            match referenceDisposition r e.Signal with
            | SignalDisposition.Catch action ->
                let current = referenceMask r task

                // The first frame of a return from `sigsuspend` saves the mask
                // the call replaced.
                let saved =
                    match pushed, Map.tryFind task r.Restore with
                    | [], Some restore -> restore
                    | _, _ -> current

                let mask =
                    Set.unionMany
                        [
                            current
                            SignalMask.signals action.Mask
                            (if action.NoDefer then Set.empty else Set.singleton e.Signal)
                        ]
                    |> Set.filter (referenceUnmaskable numbering >> not)

                let frame =
                    {
                        Id = HandlerFrameId r.NextFrame
                        Entry = e
                        Action = action
                        SavedMask = SignalMask.ofSignals numbering saved
                    }

                let signo = Signal.toRawSignoUnder numbering e.Signal

                let keepsHandler = numbering = SignalNumbering.Darwin && (signo = 4 || signo = 5)

                let r = removed e

                let r =
                    { r with
                        Dispositions =
                            if action.ResetHand && not keepsHandler then
                                Map.add e.Signal SignalDisposition.Default r.Dispositions
                            else
                                r.Dispositions
                        Blocked = Map.add task mask r.Blocked
                        Frames = Map.add task (frame :: referenceFrames r task) r.Frames
                        NextFrame = r.NextFrame + 1L
                    }

                go (frame :: pushed) r
            | SignalDisposition.Ignore -> go pushed (removed e)
            | SignalDisposition.Default ->
                match Signal.defaultDispositionUnder numbering e.Signal with
                | DefaultDisposition.Ignore -> go pushed (removed e)
                | DefaultDisposition.Terminate ->
                    Ok (
                        Some (SignalDelivery.DefaultTerminate (e.Signal, referenceCore numbering coreDumps e.Signal)),
                        removed e
                    )
                | DefaultDisposition.Stop -> alone e (SignalDelivery.DefaultStop e.Signal)
                | DefaultDisposition.Continue -> failwith "unreachable: handled above"

        // The return gives a task in `sigsuspend` its mask back: through the
        // outermost frame when one was pushed, and at once otherwise, unless
        // it took a default SIGCONT, after which the return goes on.
        go [] r
        |> Result.map (fun (delivery, r') ->
            match Map.tryFind task r'.Restore, delivery with
            | None, _ -> delivery, r'
            | Some _, Some (SignalDelivery.DefaultContinue _) -> delivery, r'
            | Some restore, _ ->
                let cleared =
                    { r' with
                        Restore = Map.remove task r'.Restore
                    }

                if r'.NextFrame > r.NextFrame then
                    delivery, cleared
                else
                    delivery,
                    { cleared with
                        Blocked = Map.add task restore cleared.Blocked
                    }
        )

    /// Advance both implementations by one op, asserting agreement on
    /// `onReturnToUser`'s full returned action (since the next step's
    /// observable state alone cannot always distinguish a divergence in
    /// which entry was consumed). Answers the task set after the op too.
    let private stepBoth
        (numbering : SignalNumbering)
        (tasks : Set<TestTask>)
        (op : Op)
        (s : SignalState<TestTask, TestHandler>)
        (r : ReferenceState)
        : SignalState<TestTask, TestHandler> * ReferenceState * Set<TestTask>
        =
        match op with
        | Op.SetDisposition (signal, disposition) ->
            let pending =
                if referenceDiscardsWhenSet numbering disposition signal then
                    r.Pending |> List.filter (fun p -> p.Signal <> signal)
                else
                    r.Pending

            SignalState.setDisposition signal disposition s,
            { r with
                Dispositions = Map.add signal (referenceStored numbering disposition) r.Dispositions
                Pending = pending
            },
            tasks
        | Op.ChangeMask (task, change, set) ->
            let set = set |> Set.filter (referenceUnmaskable numbering >> not)
            let current = referenceMask r task

            let changed =
                match change with
                | SignalMaskChange.Block -> Set.union current set
                | SignalMaskChange.Unblock -> Set.difference current set
                | SignalMaskChange.SetMask -> set

            SignalState.changeMask change (SignalMask.ofSignals numbering set) task s,
            { r with
                Blocked = Map.add task changed r.Blocked
            },
            tasks
        | Op.Sigreturn task ->
            match referenceFrames r task with
            | innermost :: outer ->
                SignalState.sigreturn task innermost.Id s,
                { r with
                    Frames = Map.add task outer r.Frames
                    Blocked = Map.add task (SignalMask.signals innermost.SavedMask) r.Blocked
                },
                tasks
            | [] -> failwith $"generated a sigreturn for %O{task}, which has no frame"
        | Op.Suspend (task, temporary) ->
            SignalState.suspend task (SignalMask.ofSignals numbering temporary) s,
            { r with
                Restore = Map.add task (referenceMask r task) r.Restore
                Blocked = Map.add task (temporary |> Set.filter (referenceUnmaskable numbering >> not)) r.Blocked
            },
            tasks
        | Op.Enqueue e -> SignalState.enqueue e s, referenceEnqueue numbering e r, tasks
        | Op.Generate (coreDumps, e) ->
            let actual = SignalState.generate coreDumps referenceLeader tasks e s

            let expected = referenceGenerate numbering coreDumps tasks e r

            // A terminated process has no state to carry on with, and a refused
            // generation did not happen, so in both the run goes on from the
            // state the signal was generated in.
            match actual, expected with
            | Ok (SignalGeneration.ProcessContinues s'), ReferenceGeneration.Continues r' -> s', r', tasks
            | Ok (SignalGeneration.ProcessStopped (a, s')), ReferenceGeneration.Stopped (b, r') when a = b ->
                s', r', tasks
            | Ok (SignalGeneration.ProcessTerminated (a, aCore)), ReferenceGeneration.Terminated (b, bCore) when
                a = b && aCore = bCore
                ->
                s, r, tasks
            | Error (SignalReceiverRefusal.LeaderBlocks a), ReferenceGeneration.Refused b when a = b -> s, r, tasks
            | Error (SignalReceiverRefusal.PendingForProcessAndLeader a), ReferenceGeneration.RefusedAsMerged b when
                a = b
                ->
                s, r, tasks
            | _ -> failwith $"generate disagreed: actual=%A{actual}, reference=%A{expected}"
        | Op.Deliver (coreDumps, task) ->
            let actual = SignalState.onReturnToUser coreDumps referenceLeader tasks task s
            let expected = referenceOnReturnToUser numbering coreDumps tasks task r

            match actual, expected with
            | Ok (actualDelivery, s'), Ok (expectedDelivery, r') ->
                if actualDelivery <> expectedDelivery then
                    failwith $"onReturnToUser disagreed: actual=%A{actualDelivery}, reference=%A{expectedDelivery}"

                s', r', tasks
            | Error a, Error b when a = b -> s, r, tasks
            | _ -> failwith $"onReturnToUser disagreed: actual=%A{actual}, reference=%A{expected}"
        | Op.Spawn task -> s, r, Set.add task tasks
        | Op.Exit task ->
            SignalState.forgetTask task s,
            { r with
                Blocked = Map.remove task r.Blocked
                Restore = Map.remove task r.Restore
                Frames = Map.remove task r.Frames
                Pending = r.Pending |> List.filter (fun e -> e.Target <> ValueSome task)
            },
            Set.remove task tasks

    /// Compare every observable accessor; the accessors are the contract.
    let private assertEquivalent
        (numbering : SignalNumbering)
        (tasks : Set<TestTask>)
        (s : SignalState<TestTask, TestHandler>)
        (r : ReferenceState)
        : unit
        =
        // Equal as maps, so a stored `Default` on the production side is a
        // failure even though every read below would agree with it.
        SignalState.dispositions s
        |> shouldEqual (r.Dispositions |> Map.filter (fun _ d -> d <> SignalDisposition.Default))

        SignalState.pending s
        |> Seq.toList
        |> shouldEqual (referencePendingView numbering r)

        for task in tasks do
            SignalState.pendingFor referenceLeader task s
            |> shouldEqual (referenceCandidates numbering task r)

        for sig0 in allSignals numbering do
            SignalState.disposition sig0 s |> shouldEqual (referenceDisposition r sig0)

        for tid in taskPool do
            SignalState.framesOf tid s |> shouldEqual (referenceFrames r tid)

            SignalState.maskOf tid s
            |> SignalMask.signals
            |> shouldEqual (referenceMask r tid)

            SignalState.maskToRestore tid s
            |> Option.map SignalMask.signals
            |> shouldEqual (Map.tryFind tid r.Restore)

        SignalState.tasksWithFrames s
        |> shouldEqual (
            r.Frames
            |> Map.filter (fun _ frames -> not frames.IsEmpty)
            |> Map.keys
            |> Set.ofSeq
        )

        SignalState.tasksWithMasksToRestore s
        |> shouldEqual (r.Restore |> Map.keys |> Set.ofSeq)

        // Equal as sets, so a stored empty mask on the production side is a
        // failure even though every `maskOf` would agree with it.
        SignalState.tasksWithMasks s
        |> shouldEqual (
            r.Blocked
            |> Map.filter (fun _ mask -> not mask.IsEmpty)
            |> Map.keys
            |> Set.ofSeq
        )

        for tid in tasks do
            let seen (e : PendingSignal<TestTask>) : bool =
                match e.Target with
                | ValueSome target -> target = tid
                | ValueNone -> numbering = SignalNumbering.Linux || tid = referenceLeader

            SignalState.pendingBlocked referenceLeader tid s
            |> SignalMask.signals
            |> shouldEqual (
                r.Pending
                |> List.filter (fun e -> seen e && referenceBlocks r tid e.Signal)
                |> List.map (fun e -> e.Signal)
                |> Set.ofList
            )

    /// Every signal `setDisposition` accepts: everything but SIGKILL and
    /// SIGSTOP, glibc's reserved pair included.
    let private settableSignals (numbering : SignalNumbering) : Signal list =
        allSignals numbering
        |> List.filter (fun signal -> not (List.contains signal (kernelUncatchable)))

    /// The signals the stop/continue flush acts between, over-weighted in the
    /// walk: a flush needs one pending and the other generated after it.
    let private jobControl (numbering : SignalNumbering) : Signal list =
        [ Signal.SIGTSTP ; Signal.SIGTTIN ; Signal.SIGTTOU ; Signal.SIGCONT ]

    let private randomOp
        (numbering : SignalNumbering)
        (tasks : Set<TestTask>)
        (r : ReferenceState)
        (rng : System.Random)
        : Op
        =
        let pick (xs : 'a list) : 'a = xs.[rng.Next xs.Length]
        let current = Set.toList tasks

        let pickDisposition () : SignalDisposition<TestHandler> =
            let pickCatch (h : TestHandler) : SignalDisposition<TestHandler> =
                // A handful of signals in the mask, SIGKILL and SIGSTOP
                // included, which the kernel drops; and each flag half the
                // time, so that frames block, nest and reset.
                let mask =
                    List.init (rng.Next 4) (fun _ -> pick (allSignals numbering)) |> Set.ofList

                SignalDisposition.Catch
                    {
                        Handler = h
                        Mask = SignalMask.ofSignals numbering mask
                        NoDefer = rng.Next 3 = 0
                        ResetHand = rng.Next 4 = 0
                        Restart = rng.Next 2 = 0
                    }

            match rng.Next 5 with
            | 0 -> SignalDisposition.Default
            | 1 -> SignalDisposition.Ignore
            | 2 -> catch
            | 3 -> pickCatch handler
            | _ -> pickCatch (TestHandler "other")

        let pickCoreDumps () : CoreDumps =
            if rng.Next 2 = 0 then
                CoreDumps.Suppressed
            else
                CoreDumps.Written

        let pickGenerated () : Signal =
            // The real-time signal is over-weighted: observing
            // queue-not-coalesce needs the *same* signal generated twice
            // before a delivery consumes it. So are the job-control signals,
            // for the flush between them, and a few signals Linux takes early
            // or late, so that the two pick rules part company often, and
            // signals the leader blocks, which another task may not.
            //
            // A signal is blocked only while a frame's mask holds it, so
            // signals some frame blocks are over-weighted too: that is how a
            // signal comes to pend, to coalesce, and to wait for a sigreturn.
            let leaderMask = referenceMask r referenceLeader |> Set.toList

            let anyMask = current |> List.map (referenceMask r) |> Set.unionMany |> Set.toList

            match rng.Next 12 with
            | 0 when numbering = SignalNumbering.Linux -> Signal.RealTime 8
            | 1
            | 2 -> pick (jobControl numbering)
            | 3 -> pick [ Signal.SIGILL ; Signal.SIGSEGV ; Signal.SIGHUP ; Signal.SIGTERM ]
            | 4
            | 5
            | 6 when not leaderMask.IsEmpty -> pick leaderMask
            | 7
            | 8 when not anyMask.IsEmpty -> pick anyMask
            | _ -> pick (allSignals numbering)

        let pickTarget () : TestTask voption =
            if rng.Next 2 = 0 then
                ValueNone
            else
                ValueSome (pick current)

        // Darwin holds a signal pending on the process and one pending on the
        // leader as one, which `generate` refuses to leave pending; so under
        // Darwin's numbering, generating at the partner of an entry already
        // pending is over-weighted; otherwise about two walks in a hundred
        // reach the refusal.
        let pickGeneratedEntry () : PendingSignal<TestTask> =
            let partners =
                match numbering with
                | SignalNumbering.Linux -> []
                | SignalNumbering.Darwin ->
                    r.Pending
                    |> List.choose (fun p ->
                        match p.Target with
                        | ValueNone ->
                            Some
                                { p with
                                    Target = ValueSome referenceLeader
                                }
                        | ValueSome task when task = referenceLeader ->
                            Some
                                { p with
                                    Target = ValueNone
                                }
                        | ValueSome _ -> None
                    )

            match partners with
            | _ :: _ when rng.Next 3 = 0 -> pick partners
            | _ ->
                {
                    Signal = pickGenerated ()
                    Target = pickTarget ()
                }

        // Darwin takes a task's candidates lowest number first, the process's
        // and its own as one, so an action passes a candidate the task blocks
        // only when a higher-numbered signal the task does not block is
        // pending beside it. The blocked one must be directed at the task: one
        // directed at the process, which the leader blocks, is refused at
        // delivery while another task could take it. So under Darwin's
        // numbering, a task some frame masks is over-weighted as a target: it
        // is sent a signal its mask holds, or, once one is pending for it, a
        // signal above it that it would take; otherwise about one walk in a
        // hundred lines the two up.
        let pickEnqueuedEntry () : PendingSignal<TestTask> =
            let masked =
                match numbering with
                | SignalNumbering.Linux -> []
                | SignalNumbering.Darwin ->
                    current
                    |> List.choose (fun task ->
                        match referenceMask r task |> Set.toList with
                        | [] -> None
                        | mask -> Some (task, mask)
                    )

            match masked with
            | _ :: _ when rng.Next 2 = 0 ->
                let task, mask = pick masked

                let heldHere =
                    r.Pending
                    |> List.filter (fun p -> p.Target = ValueSome task && referenceBlocks r task p.Signal)

                let deliverableAbove (held : Signal) : Signal list =
                    allSignals numbering
                    |> List.filter (fun signal ->
                        referencePickKey numbering signal > referencePickKey numbering held
                        && not (referenceBlocks r task signal)
                        && not (referenceIgnoredAtGeneration numbering r signal)
                    )

                let signal =
                    match heldHere |> List.map (fun p -> p.Signal) with
                    | [] -> pick mask
                    | held ->
                        match deliverableAbove (List.minBy (referencePickKey numbering) held) with
                        | [] -> pick mask
                        | above -> pick above

                {
                    Signal = signal
                    Target = ValueSome task
                }
            | _ ->
                {
                    Signal = pickGenerated ()
                    Target = pickTarget ()
                }

        let kind = rng.Next 120

        if kind >= 112 then
            match current |> List.filter (fun task -> not (Map.containsKey task r.Restore)) with
            | [] -> Op.Deliver (pickCoreDumps (), referenceLeader)
            | free ->
                // Over-weights what is pending, so that the temporary mask
                // lets a signal through often, and blocks one often.
                let pendingSignals = r.Pending |> List.map (fun p -> p.Signal)

                let temporary =
                    List.init
                        (rng.Next 4)
                        (fun _ ->
                            if not pendingSignals.IsEmpty && rng.Next 2 = 0 then
                                pick pendingSignals
                            else
                                pick (allSignals numbering)
                        )
                    |> Set.ofList

                Op.Suspend (pick free, temporary)
        elif kind >= 100 then
            // A few signals, SIGKILL and SIGSTOP among them sometimes; an
            // unblock over-weights what is pending, so that it delivers.
            let change =
                pick [ SignalMaskChange.Block ; SignalMaskChange.Unblock ; SignalMaskChange.SetMask ]

            let pendingSignals = r.Pending |> List.map (fun p -> p.Signal)

            let set =
                List.init
                    (rng.Next 4)
                    (fun _ ->
                        if not pendingSignals.IsEmpty && rng.Next 2 = 0 then
                            pick pendingSignals
                        else
                            pick (allSignals numbering)
                    )
                |> Set.ofList

            Op.ChangeMask (pick current, change, set)
        elif kind < 25 then
            // Only what the kernel accepts: `setDisposition` fails loud on
            // SIGKILL and SIGSTOP, and the refusal has its own unit test.
            Op.SetDisposition (pick (settableSignals numbering), pickDisposition ())
        elif kind < 42 then
            match current |> List.filter (fun task -> not (referenceFrames r task).IsEmpty) with
            | [] -> Op.Deliver (pickCoreDumps (), referenceLeader)
            | framed -> Op.Sigreturn (pick framed)
        elif kind < 60 then
            Op.Enqueue (pickEnqueuedEntry ())
        elif kind < 70 then
            Op.Generate (pickCoreDumps (), pickGeneratedEntry ())
        elif kind < 94 then
            // The leader is asked half the time: it is the only task that takes
            // the process's own signals.
            let task = if rng.Next 2 = 0 then referenceLeader else pick current

            Op.Deliver (pickCoreDumps (), task)
        else
            match taskPool |> List.filter (fun task -> not (Set.contains task tasks)) with
            | absent when not absent.IsEmpty && rng.Next 2 = 0 -> Op.Spawn (pick absent)
            | _ ->
                match current |> List.filter (fun task -> task <> referenceLeader) with
                | [] -> Op.Spawn (pick (taskPool |> List.filter (fun task -> not (Set.contains task tasks))))
                | others -> Op.Exit (pick others)

    /// The paths of the reference that the oracle walk must reach, each counted
    /// once per step that reaches it (an ignored-signal discard, once per entry
    /// discarded).
    [<RequireQualifiedAccess>]
    type private OracleLabel =
        | HandlerDeliveries
        | NonLeaderDeliveries
        | DefaultTerminates
        | CoreDumps
        | DefaultStopsAndContinues
        | IgnoredDiscards
        | ActionAfterSkip
        | DrainOfEmpty
        | DrainNoneNonEmpty
        | UnmaskableInMasks
        | NestedFrames
        | Sigreturns
        | ResetHands
        | HeldByFrame
        | GenerationDrops
        | MergeRefusals
        | CoalescedEnqueues
        | QueuedRealTimeDuplicates
        | GeneratedTerminations
        | GeneratedStops
        | GeneratedQueued
        | GeneratedIgnoredDiscards
        | GenerationRefusals
        | DeliveryRefusals
        | DiscardsWhenSet
        | Flushes
        | DefaultsStored
        | OwnAndSharedCandidates
        | OutOfGenerationOrder
        | Exits
        | MaskChanges
        | Suspends
        | RestoresThroughFrames
        | RestoresWithoutFrames

    let private checkAgainstOracle (numbering : SignalNumbering) : unit =
        let property (cover : OracleLabel -> unit) (seed : int) : unit =
            let rng = System.Random seed
            let steps = rng.Next (10, 80)

            let mutable s = initial numbering
            let mutable r = referenceEmpty
            let mutable tasks = Set.ofList [ t0 ; t1 ; t2 ]
            assertEquivalent numbering tasks s r

            for _ in 1..steps do
                let op = randomOp numbering tasks r rng

                // Distribution telemetry collected before the step so we can
                // see what shape the random walk drove the model into.
                match op with
                | Op.Deliver (coreDumps, task) ->
                    let candidates = referenceCandidates numbering task r

                    if
                        candidates |> List.exists (fun e -> e.Target.IsSome)
                        && candidates |> List.exists (fun e -> e.Target.IsNone)
                    then
                        cover OracleLabel.OwnAndSharedCandidates

                    if candidates <> (r.Pending |> List.filter (fun e -> List.contains e candidates)) then
                        cover OracleLabel.OutOfGenerationOrder

                    match referenceOnReturnToUser numbering coreDumps tasks task r with
                    | Error _ -> cover OracleLabel.DeliveryRefusals
                    | Ok (expected, r') ->

                    if Map.containsKey task r.Restore then
                        if r'.NextFrame > r.NextFrame then
                            cover OracleLabel.RestoresThroughFrames
                        else
                            cover OracleLabel.RestoresWithoutFrames

                    match expected with
                    | Some (SignalDelivery.RunHandlers frames) ->
                        cover OracleLabel.HandlerDeliveries

                        if frames.Length > 1 then
                            cover OracleLabel.NestedFrames

                        if
                            frames
                            |> List.exists (fun frame ->
                                frame.Action.ResetHand
                                && referenceDisposition r' frame.Entry.Signal = SignalDisposition.Default
                            )
                        then
                            cover OracleLabel.ResetHands

                        if task <> referenceLeader then
                            cover OracleLabel.NonLeaderDeliveries
                    | Some (SignalDelivery.DefaultTerminate (_, cored)) ->
                        cover OracleLabel.DefaultTerminates

                        if cored then
                            cover OracleLabel.CoreDumps
                    | Some (SignalDelivery.DefaultStop _)
                    | Some (SignalDelivery.DefaultContinue _) -> cover OracleLabel.DefaultStopsAndContinues
                    | None when r.Pending.IsEmpty -> cover OracleLabel.DrainOfEmpty
                    | None -> cover OracleLabel.DrainNoneNonEmpty

                    // Entries the walk removed beyond the ones the action
                    // consumed are ignored-signal discards.
                    let consumed =
                        match expected with
                        | Some (SignalDelivery.RunHandlers frames) -> frames.Length
                        | Some _ -> 1
                        | None -> 0

                    for _ in 1 .. List.length r.Pending - List.length r'.Pending - consumed do
                        cover OracleLabel.IgnoredDiscards

                    // An action fired past a candidate the task blocks.
                    match expected, candidates with
                    | Some _, head :: _ when List.contains head r'.Pending -> cover OracleLabel.ActionAfterSkip
                    | _ -> ()

                    // A caught signal held back by a frame's mask.
                    if
                        candidates
                        |> List.exists (fun e ->
                            referenceBlocks r task e.Signal
                            && (
                                match referenceDisposition r e.Signal with
                                | SignalDisposition.Catch _ -> true
                                | _ -> false
                            )
                        )
                    then
                        cover OracleLabel.HeldByFrame
                | Op.Generate (coreDumps, e) ->
                    match referenceGenerate numbering coreDumps tasks e r with
                    | ReferenceGeneration.Terminated (_, cored) ->
                        cover OracleLabel.GeneratedTerminations

                        if cored then
                            cover OracleLabel.CoreDumps
                    | ReferenceGeneration.Stopped _ -> cover OracleLabel.GeneratedStops
                    | ReferenceGeneration.Refused _ -> cover OracleLabel.GenerationRefusals
                    | ReferenceGeneration.RefusedAsMerged _ -> cover OracleLabel.MergeRefusals
                    | ReferenceGeneration.Continues r' ->
                        cover OracleLabel.GeneratedQueued

                        if r' = r && referenceIgnoredAtGeneration numbering r e.Signal then
                            cover OracleLabel.GeneratedIgnoredDiscards
                | Op.SetDisposition (signal, disposition) ->
                    if disposition = SignalDisposition.Default then
                        cover OracleLabel.DefaultsStored

                    match disposition with
                    | SignalDisposition.Catch action when
                        action.Mask |> SignalMask.signals |> Set.exists (referenceUnmaskable numbering)
                        ->
                        cover OracleLabel.UnmaskableInMasks
                    | _ -> ()

                    if
                        referenceDiscardsWhenSet numbering disposition signal
                        && r.Pending |> List.exists (fun p -> p.Signal = signal)
                    then
                        cover OracleLabel.DiscardsWhenSet
                | Op.Enqueue _ -> ()
                | Op.ChangeMask _ -> cover OracleLabel.MaskChanges
                | Op.Exit _ -> cover OracleLabel.Exits
                | Op.Sigreturn _ -> cover OracleLabel.Sigreturns
                | Op.Suspend _ -> cover OracleLabel.Suspends
                | Op.Spawn _ -> ()

                match op with
                | Op.Enqueue e
                | Op.Generate (_, e) ->

                    match referenceBeginGeneration numbering e.Signal r with
                    | None -> cover OracleLabel.GenerationDrops
                    | Some flushed ->
                        if List.length flushed.Pending < List.length r.Pending then
                            cover OracleLabel.Flushes

                        match op with
                        | Op.Enqueue _ ->
                            let alreadyPendingInSet =
                                flushed.Pending
                                |> List.exists (fun p -> p.Signal = e.Signal && p.Target = e.Target)

                            if alreadyPendingInSet then
                                if Signal.isRealTimeUnder numbering e.Signal then
                                    cover OracleLabel.QueuedRealTimeDuplicates
                                else
                                    cover OracleLabel.CoalescedEnqueues
                        | _ -> ()
                | _ -> ()

                let s', r', tasks' = stepBoth numbering tasks op s r
                s <- s'
                r <- r'
                tasks <- tasks'
                assertEquivalent numbering tasks s r

        // The seed is drawn from the whole range, so that each fresh case walks
        // a fresh sequence: FsCheck draws a size-bounded integer from 0 to 100
        // only. A seed has no meaningful shrink, so it is given no shrinker.
        let coverage =
            CoverageSample.check
                (Config.QuickThrowOnFailure.WithMaxTest 1000)
                (Arb.fromGen (Gen.choose (0, System.Int32.MaxValue)))
                property

        // Distribution checks, counted over the fixed sample: the random walk
        // must hit each of these paths often enough that a regression would
        // actually surface. Each floor sits at least four standard deviations
        // below what 1000 walks reach, except for the rare paths, which are
        // only required to be reached: a frame holding back a caught signal, an
        // action past a skipped candidate, a queued real-time duplicate, and a
        // merge refusal. Nested frames, the nearest their floor of 15, reached a
        // mean of about 20 in 500 walks with a standard deviation of 5 (measured
        // over 60 runs per numbering), so 1000 walks clear it with room to spare.
        coverage.Count OracleLabel.HandlerDeliveries |> shouldBeGreaterThan 30
        coverage.Count OracleLabel.NonLeaderDeliveries |> shouldBeGreaterThan 10
        coverage.Count OracleLabel.DefaultTerminates |> shouldBeGreaterThan 50
        coverage.Count OracleLabel.CoreDumps |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.DefaultStopsAndContinues |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.ActionAfterSkip |> shouldBeGreaterThan 0
        coverage.Count OracleLabel.DrainOfEmpty |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.DrainNoneNonEmpty |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.UnmaskableInMasks |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.NestedFrames |> shouldBeGreaterThan 15
        coverage.Count OracleLabel.Sigreturns |> shouldBeGreaterThan 50
        coverage.Count OracleLabel.ResetHands |> shouldBeGreaterThan 10
        coverage.Count OracleLabel.HeldByFrame |> shouldBeGreaterThan 0
        coverage.Count OracleLabel.CoalescedEnqueues |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.GeneratedTerminations |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.GeneratedStops |> shouldBeGreaterThan 5
        coverage.Count OracleLabel.GeneratedQueued |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.GeneratedIgnoredDiscards |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.GenerationRefusals |> shouldBeGreaterThan 3
        coverage.Count OracleLabel.DeliveryRefusals |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.DiscardsWhenSet |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.Flushes |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.DefaultsStored |> shouldBeGreaterThan 100
        coverage.Count OracleLabel.OwnAndSharedCandidates |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.OutOfGenerationOrder |> shouldBeGreaterThan 50
        coverage.Count OracleLabel.Exits |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.MaskChanges |> shouldBeGreaterThan 100
        coverage.Count OracleLabel.Suspends |> shouldBeGreaterThan 100
        coverage.Count OracleLabel.RestoresThroughFrames |> shouldBeGreaterThan 20
        coverage.Count OracleLabel.RestoresWithoutFrames |> shouldBeGreaterThan 20

        // The generation-versus-delivery halves of the ignore rule are
        // flavour-divergent, so their counters are too: only Darwin drops at
        // generation, and only Linux lets an ignored signal reach the
        // delivery walk's discard. (A Darwin discard is still reachable by
        // catching, enqueueing and then ignoring, but the walk is not
        // guaranteed to line those up, so its floor stays at zero.)
        match numbering with
        | SignalNumbering.Linux ->
            coverage.Count OracleLabel.IgnoredDiscards |> shouldBeGreaterThan 20
            coverage.Count OracleLabel.GenerationDrops |> shouldEqual 0
        | SignalNumbering.Darwin -> coverage.Count OracleLabel.GenerationDrops |> shouldBeGreaterThan 20

        // Only Darwin holds a signal pending on the process and one pending on
        // the leader as one, which this library refuses to leave pending.
        match numbering with
        | SignalNumbering.Linux -> coverage.Count OracleLabel.MergeRefusals |> shouldEqual 0
        | SignalNumbering.Darwin -> coverage.Count OracleLabel.MergeRefusals |> shouldBeGreaterThan 0

        // Only Linux numbering has real-time signals in the pool (`RealTime 8`),
        // so only there can the walk exercise the queue-not-coalesce arm.
        match numbering with
        | SignalNumbering.Linux -> coverage.Count OracleLabel.QueuedRealTimeDuplicates |> shouldBeGreaterThan 0
        | SignalNumbering.Darwin -> coverage.Count OracleLabel.QueuedRealTimeDuplicates |> shouldEqual 0

    [<Test>]
    let ``random op sequences agree with the reference oracle on every observable, under Linux numbering`` () : unit =
        checkAgainstOracle SignalNumbering.Linux

    [<Test>]
    let ``random op sequences agree with the reference oracle on every observable, under Darwin numbering`` () : unit =
        checkAgainstOracle SignalNumbering.Darwin
