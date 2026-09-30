namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The order a task takes its pending signals in, held to the rows of
/// `docs/plans/2026-08-23-posix-kernel-extraction/signal-pick-order.c`, which are
/// checked in as `signalOrder/linux.txt` (Linux 6.18.5, aarch64) and
/// `signalOrder/darwin.txt` (Darwin 27.0.0; the same rows as on 25.6.0).
///
/// Each trial there ran in a fresh child that blocked every signal it was about
/// to generate, generated them, and then took them: with `sigwait` (so in the
/// order they are picked, whatever their dispositions), or by unblocking them
/// under handlers. Each is replayed here on a fresh state holding them pending,
/// whose only task then returns to user mode and runs the handlers its frames
/// say, the expected order being the probe's output rather than anything
/// derived from the model.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalPickOrder =

    let private leader : int = 0

    let private tasks : Set<int> = Set.singleton leader

    let private handler : string = "h"

    let private numberingOf (flavour : string) : SignalNumbering =
        match flavour with
        | "linux" -> SignalNumbering.Linux
        | "darwin" -> SignalNumbering.Darwin
        | other -> failwith $"no such flavour: %s{other}"

    let private rows (flavour : string) : string list =
        let assembly = Assembly.GetExecutingAssembly ()
        let name = $"WoofWare.PosixKernel.Test.signalOrder.%s{flavour}.txt"

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))

    let private numbers (text : string) : int list =
        text.Split (' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse
        |> Array.toList

    let private signal (numbering : SignalNumbering) (signo : int) : Signal =
        match Signal.ofRawSignoUnder numbering signo with
        | ValueSome signal -> signal
        | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

    let private signo (numbering : SignalNumbering) (signal : Signal) : int = Signal.toRawSignoUnder numbering signal

    /// How the probe's consumer took the signals: `sigwait`, with every
    /// disposition the default; or handlers whose `sa_mask` holds every signal
    /// generated, so that one runs at a time; or handlers with an empty
    /// `sa_mask`.
    [<RequireQualifiedAccess>]
    type private Consumer =
        | SigWait
        | FullMask
        | NoMask

    /// Each of `generated` pending in a fresh state, as the probe left them by
    /// generating them while every one was blocked: `(signo, directed at the
    /// leader?)`. Blocked, a generated signal is pending with nothing else done
    /// to it, which is what `SignalState.enqueue` does.
    let private generateBlocked
        (numbering : SignalNumbering)
        (consumer : Consumer)
        (generated : (int * bool) list)
        : SignalState<int, string>
        =
        let everyGenerated =
            generated |> List.map (fun (signo, _) -> signal numbering signo) |> Set.ofList

        let disposition : SignalDisposition<string> =
            match consumer with
            | Consumer.SigWait -> SignalDisposition.Default
            | Consumer.FullMask ->
                SignalDisposition.Catch
                    { SignalCatch.ofHandler handler with
                        Mask = everyGenerated
                    }
            | Consumer.NoMask -> SignalDisposition.Catch (SignalCatch.ofHandler handler)

        let withDispositions =
            match disposition with
            | SignalDisposition.Default -> SignalState.initial numbering Set.empty
            | disposition ->
                (SignalState.initial numbering Set.empty, everyGenerated)
                ||> Set.fold (fun state signal -> SignalState.setDisposition signal disposition state)

        (withDispositions, generated)
        ||> List.fold (fun state (signo, directed) ->
            SignalState.enqueue
                {
                    Signal = signal numbering signo
                    Target = if directed then ValueSome leader else ValueNone
                }
                state
        )

    /// What `sigwait` on every signal takes, in order: the leader's pending
    /// signals in pick order, whatever their dispositions.
    let private sigwaitOrder (numbering : SignalNumbering) (state : SignalState<int, string>) : int list =
        SignalState.pendingFor leader leader state
        |> List.map (fun entry -> signo numbering entry.Signal)

    /// The handler bodies the leader runs, in the order they run, once it
    /// returns to user mode: each frame's handler, innermost first, its
    /// `sigreturn`, and the next return to user mode, which may push frames
    /// that run before the rest.
    let private handlerOrder (numbering : SignalNumbering) (state : SignalState<int, string>) : int list =
        let returnToUser
            (state : SignalState<int, string>)
            : HandlerFrame<int, string> list * SignalState<int, string>
            =
            match SignalState.onReturnToUser CoreDumps.Suppressed leader tasks leader state with
            | Ok (None, state) -> [], state
            | Ok (Some (SignalDelivery.RunHandlers frames), state) -> frames, state
            | other -> failwith $"expected handlers to run, got %A{other}"

        let rec run
            (ran : int list)
            (frames : HandlerFrame<int, string> list)
            (state : SignalState<int, string>)
            (fuel : int)
            : int list * SignalState<int, string>
            =
            if fuel = 0 then
                failwith "the leader ran more handlers than signals were ever pending"

            match frames with
            | [] -> ran, state
            | frame :: outer ->
                let ran = signo numbering frame.Entry.Signal :: ran
                let pushed, state = returnToUser (SignalState.sigreturn leader frame.Id state)
                let ran, state = run ran pushed state (fuel - 1)
                run ran outer state (fuel - 1)

        let frames, state = returnToUser state
        let ran, state = run [] frames state 1000
        SignalState.framesOf leader state |> shouldEqual []
        List.rev ran

    [<Literal>]
    let private RowPattern =
        @"^(std|rt) (sigwait|fullmask|nomask) (proc|thread) perm\d+ generated: ([\d ]+) delivered: ([\d ]*?)(?: \| exit: ([\d ]*))?$"

    [<Literal>]
    let private PairPattern = @"^pair thread=(\d+) proc=(\d+) delivered=([\d,]*)$"

    [<Literal>]
    let private FifoPattern =
        @"^rtfifo queued \(signo:value\) ([\d: ]+) -> delivered values: ([\d ]+)$"

    /// Replay every row of one flavour's file, answering how many rows of each
    /// kind were replayed.
    let private replay (flavour : string) : Map<string, int> =
        let numbering = numberingOf flavour
        let mutable counts : Map<string, int> = Map.empty

        let count (kind : string) : unit =
            counts <- Map.change kind (fun n -> Some (1 + Option.defaultValue 0 n)) counts

        for line in rows flavour do
            let row = Text.RegularExpressions.Regex.Match (line, RowPattern)
            let pair = Text.RegularExpressions.Regex.Match (line, PairPattern)
            let fifo = Text.RegularExpressions.Regex.Match (line, FifoPattern)

            if row.Success then
                let consumer = row.Groups.[2].Value
                let directed = row.Groups.[3].Value = "thread"

                let generated =
                    numbers row.Groups.[4].Value |> List.map (fun signo -> signo, directed)

                let delivered = numbers row.Groups.[5].Value

                let actual =
                    match consumer with
                    | "sigwait" -> generateBlocked numbering Consumer.SigWait generated |> sigwaitOrder numbering
                    | "fullmask" -> generateBlocked numbering Consumer.FullMask generated |> handlerOrder numbering
                    // Each handler here leaves every other signal unblocked, so
                    // every pending signal's frame is pushed before any handler
                    // body runs, and the bodies run last-picked first: which the
                    // model's frames say without help.
                    | "nomask" -> generateBlocked numbering Consumer.NoMask generated |> handlerOrder numbering
                    | other -> failwith $"no such consumer: %s{other}"

                if actual <> delivered then
                    failwith $"%s{flavour}: the model took %A{actual} where the kernel took %A{delivered}, in: %s{line}"

                count $"%s{row.Groups.[1].Value} %s{consumer} %s{row.Groups.[3].Value}"
            elif pair.Success then
                let directedAtLeader = Int32.Parse pair.Groups.[1].Value
                let toProcess = Int32.Parse pair.Groups.[2].Value

                let delivered =
                    pair.Groups.[3].Value.Split (',', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map Int32.Parse
                    |> Array.toList

                let actual =
                    generateBlocked numbering Consumer.FullMask [ directedAtLeader, true ; toProcess, false ]
                    |> handlerOrder numbering

                if actual <> delivered then
                    failwith $"%s{flavour}: the model took %A{actual} where the kernel took %A{delivered}, in: %s{line}"

                count "pair"
            elif fifo.Success then
                // The model holds no `sigqueue` values, so only the signal each
                // value came with is compared.
                let queued =
                    fifo.Groups.[1].Value.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun item ->
                        match item.Split ':' with
                        | [| signo ; value |] -> Int32.Parse value, Int32.Parse signo
                        | _ -> failwith $"not signo:value: %s{item}"
                    )
                    |> Array.toList

                let delivered =
                    numbers fifo.Groups.[2].Value
                    |> List.map (fun value -> List.find (fun (v, _) -> v = value) queued |> snd)

                let actual =
                    generateBlocked numbering Consumer.FullMask (queued |> List.map (fun (_, s) -> s, false))
                    |> handlerOrder numbering

                if actual <> delivered then
                    failwith $"%s{flavour}: the model took %A{actual} where the kernel took %A{delivered}, in: %s{line}"

                count "rtfifo"
            else
                failwith $"%s{flavour}: unrecognised row: %s{line}"

        counts

    [<Test>]
    let ``every Linux row of the pick-order probe`` () : unit =
        replay "linux"
        |> shouldEqual (
            Map.ofList
                [
                    "std sigwait proc", 42
                    "std sigwait thread", 42
                    "std fullmask proc", 42
                    "std fullmask thread", 42
                    "std nomask proc", 42
                    "std nomask thread", 42
                    "pair", 812
                    "rt sigwait proc", 20
                    "rt fullmask proc", 20
                    "rtfifo", 1
                ]
        )

    [<Test>]
    let ``every Darwin row of the pick-order probe`` () : unit =
        replay "darwin"
        |> shouldEqual (
            Map.ofList
                [
                    "std sigwait proc", 42
                    "std sigwait thread", 42
                    "std fullmask proc", 42
                    "std fullmask thread", 42
                    "std nomask proc", 42
                    "std nomask thread", 42
                    "pair", 812
                ]
        )

    // A few of the rows again, as literals.

    [<Test>]
    let ``Linux takes the synchronous signals first, then the lowest number`` () : unit =
        // std sigwait proc perm00: 1..31 but KILL and STOP, ascending. The
        // stop signals discarded the SIGCONT generated before them.
        let generated =
            [ 1..31 ]
            |> List.filter (fun s -> s <> 9 && s <> 19)
            |> List.map (fun s -> s, false)

        generateBlocked SignalNumbering.Linux Consumer.SigWait generated
        |> sigwaitOrder SignalNumbering.Linux
        |> shouldEqual
            [
                4
                5
                7
                8
                11
                31
                1
                2
                3
                6
                10
                12
                13
                14
                15
                16
                17
                20
                21
                22
                23
                24
                25
                26
                27
                28
                29
                30
            ]

    [<Test>]
    let ``Darwin takes the lowest number first, having dropped the ignored ones`` () : unit =
        // std sigwait proc perm00: 1..31 but KILL and STOP, ascending, at their
        // defaults: URG, CHLD, IO, WINCH and INFO were discarded as ignored, and
        // the stop signals discarded SIGCONT.
        let generated =
            [ 1..31 ]
            |> List.filter (fun s -> s <> 9 && s <> 17)
            |> List.map (fun s -> s, false)

        generateBlocked SignalNumbering.Darwin Consumer.SigWait generated
        |> sigwaitOrder SignalNumbering.Darwin
        |> shouldEqual
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
                14
                15
                21
                22
                24
                25
                26
                27
                30
                31
            ]

    [<Test>]
    let ``Linux takes a task's own signals before the process's`` () : unit =
        // pair thread=15 proc=4, and thread=4 proc=15.
        generateBlocked SignalNumbering.Linux Consumer.FullMask [ 15, true ; 4, false ]
        |> handlerOrder SignalNumbering.Linux
        |> shouldEqual [ 15 ; 4 ]

        generateBlocked SignalNumbering.Linux Consumer.FullMask [ 4, true ; 15, false ]
        |> handlerOrder SignalNumbering.Linux
        |> shouldEqual [ 4 ; 15 ]

    [<Test>]
    let ``Darwin takes a task's own signals and the process's as one set`` () : unit =
        // pair thread=15 proc=4, and thread=4 proc=15.
        generateBlocked SignalNumbering.Darwin Consumer.FullMask [ 15, true ; 4, false ]
        |> handlerOrder SignalNumbering.Darwin
        |> shouldEqual [ 4 ; 15 ]

        generateBlocked SignalNumbering.Darwin Consumer.FullMask [ 4, true ; 15, false ]
        |> handlerOrder SignalNumbering.Darwin
        |> shouldEqual [ 4 ; 15 ]

    [<Test>]
    let ``Linux takes a real-time signal's instances in order, lower numbers first`` () : unit =
        // rtfifo: 37, 35, 37, 35, 37, 35 queued; the three 35s were taken first.
        generateBlocked
            SignalNumbering.Linux
            Consumer.FullMask
            [ 37, false ; 35, false ; 37, false ; 35, false ; 37, false ; 35, false ]
        |> handlerOrder SignalNumbering.Linux
        |> shouldEqual [ 35 ; 35 ; 35 ; 37 ; 37 ; 37 ]
