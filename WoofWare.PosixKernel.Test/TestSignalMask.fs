namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `SignalMask`: the codec between a `sigset_t`'s bits and the signals they
/// name, held to a reference that reads each bit through
/// `Signal.ofRawSignoUnder`, and to the rows measured on Darwin's bit 31.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalMask =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// How wide each flavour's `sigset_t` is, written out: glibc's kernel set
    /// is 64 bits (`rt_sigprocmask` refuses any `sigsetsize` but 8,
    /// `sigprocmask-ops.c`), Darwin's `sigset_t` is a `uint32_t`.
    let private width (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 64
        | SignalNumbering.Darwin -> 32

    /// Words over every bit, weighted towards the edges: the empty word, the
    /// full word, the bits either side of each width, and arbitrary ones.
    let private wordGen : Gen<uint64> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<uint64>
                Gen.choose (0, 63) |> Gen.map (fun bit -> 1UL <<< bit)
                Gen.elements
                    [
                        0UL
                        UInt64.MaxValue
                        0xffffffffUL
                        0x80000000UL
                        0x1_0000_0000UL
                        0x7fffffffUL
                    ]
                ArbMap.defaults |> ArbMap.generate<uint32> |> Gen.map uint64
            ]

    let private numberingGen : Gen<SignalNumbering> = Gen.elements everyNumbering

    /// The signals a word names, read bit by bit.
    let private referenceSignals (numbering : SignalNumbering) (word : uint64) : Set<Signal> =
        [ 1..64 ]
        |> List.filter (fun signo -> word &&& (1UL <<< (signo - 1)) <> 0UL)
        |> List.choose (fun signo -> Signal.ofRawSignoUnder numbering signo |> ValueOption.toOption)
        |> Set.ofList

    [<Test>]
    let ``ofWord accepts exactly the words its sigset_t can hold, and toWord gives each back`` () : unit =
        let property (numbering : SignalNumbering, word : uint64) : unit =
            let fits = width numbering = 64 || word >>> width numbering = 0UL

            match SignalMask.ofWord numbering word with
            | Ok mask ->
                fits |> shouldEqual true
                SignalMask.toWord mask |> shouldEqual word
                SignalMask.signals mask |> shouldEqual (referenceSignals numbering word)
                SignalMask.isEmpty mask |> shouldEqual (word = 0UL)
            | Error (SignalMaskRefusal.WiderThanSigset (refused, bits)) ->
                fits |> shouldEqual false
                refused |> shouldEqual word
                bits |> shouldEqual (width numbering)

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip numberingGen wordGen)) property)

    [<Test>]
    let ``ofSignals names exactly its signals, and ofWord reads its word back to the same mask`` () : unit =
        let property (numbering : SignalNumbering, word : uint64) : unit =
            let signals = referenceSignals numbering word
            let mask = SignalMask.ofSignals numbering signals

            SignalMask.signals mask |> shouldEqual signals

            for signal in signals do
                SignalMask.contains signal mask |> shouldEqual true

            SignalMask.ofWord numbering (SignalMask.toWord mask) |> shouldEqual (Ok mask)

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip numberingGen wordGen)) property)

    [<Test>]
    let ``contains agrees with signals for every signal`` () : unit =
        let everySignal =
            [ 1..64 ]
            |> List.collect (fun signo ->
                everyNumbering
                |> List.choose (fun n -> Signal.ofRawSignoUnder n signo |> ValueOption.toOption)
            )
            |> List.distinct

        let property (numbering : SignalNumbering, word : uint64) : unit =
            match SignalMask.ofWord numbering word with
            | Error _ -> ()
            | Ok mask ->
                for signal in everySignal do
                    SignalMask.contains signal mask
                    |> shouldEqual (Set.contains signal (SignalMask.signals mask))

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip numberingGen wordGen)) property)

    [<Test>]
    let ``the empty mask is the same under every numbering`` () : unit =
        for numbering in everyNumbering do
            SignalMask.ofWord numbering 0UL |> shouldEqual (Ok SignalMask.empty)
            SignalMask.ofSignals numbering Set.empty |> shouldEqual SignalMask.empty

        SignalMask.toWord SignalMask.empty |> shouldEqual 0UL
        SignalMask.signals SignalMask.empty |> shouldEqual Set.empty

    [<Test>]
    let ``Darwin's bit 31 names no signal and is kept`` () : unit =
        // `sigprocmask-ops.c` and `sigaction-mask-bits.c` on Darwin 27.0.0:
        // bit 31 alone, blocked or set as a handler's sa_mask, read back as
        // 0x80000000; and sigfillset's ~0 read back as 0xfffefeff.
        let bit31 =
            SignalMask.ofWord SignalNumbering.Darwin 0x80000000UL
            |> Result.defaultWith (fun refusal -> failwith (SignalMaskRefusal.describe refusal))

        SignalMask.signals bit31 |> shouldEqual Set.empty
        SignalMask.isEmpty bit31 |> shouldEqual false
        SignalMask.toWord bit31 |> shouldEqual 0x80000000UL

        let filled =
            SignalMask.ofWord SignalNumbering.Darwin 0xffffffffUL
            |> Result.defaultWith (fun refusal -> failwith (SignalMaskRefusal.describe refusal))

        SignalMask.signals filled
        |> Set.map (Signal.toRawSignoUnder SignalNumbering.Darwin)
        |> shouldEqual (Set.ofList [ 1..31 ])

        // Under Linux the same bit is signal 32, glibc's SIGCANCEL.
        SignalMask.ofWord SignalNumbering.Linux 0x80000000UL
        |> Result.map SignalMask.signals
        |> shouldEqual (Ok (Set.singleton (Signal.RealTime 0)))

        SignalMask.ofWord SignalNumbering.Darwin 0x1_0000_0000UL
        |> shouldEqual (Error (SignalMaskRefusal.WiderThanSigset (0x1_0000_0000UL, 32)))

    [<Test>]
    let ``ofSignals refuses a signal its numbering does not have`` () : unit =
        Assert.Throws<exn> (fun () ->
            SignalMask.ofSignals SignalNumbering.Darwin (Set.singleton Signal.SIGPWR)
            |> ignore<SignalMask>
        )
        |> ignore

        Assert.Throws<exn> (fun () ->
            SignalMask.ofSignals SignalNumbering.Linux (Set.singleton Signal.SIGINFO)
            |> ignore<SignalMask>
        )
        |> ignore

    [<Test>]
    let ``union, difference and without are the word's or, and-not and cleared bits`` () : unit =
        let property (numbering : SignalNumbering, a : uint64, b : uint64) : unit =
            let fit (word : uint64) : uint64 =
                if width numbering = 64 then
                    word
                else
                    word &&& ((1UL <<< width numbering) - 1UL)

            let mask (word : uint64) : SignalMask =
                SignalMask.ofWord numbering (fit word)
                |> Result.defaultWith (fun refusal -> failwith (SignalMaskRefusal.describe refusal))

            let a = fit a
            let b = fit b

            SignalMask.union (mask a) (mask b) |> shouldEqual (mask (a ||| b))
            SignalMask.difference (mask a) (mask b) |> shouldEqual (mask (a &&& ~~~b))

            let signals = SignalMask.signals (mask b)

            SignalMask.without signals (mask a)
            |> shouldEqual (mask (a &&& ~~~(SignalMask.toWord (SignalMask.ofSignals numbering signals))))

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip3 numberingGen wordGen wordGen)) property)

    [<Test>]
    let ``masks under different numberings do not combine`` () : unit =
        let linux =
            SignalMask.ofSignals SignalNumbering.Linux (Set.singleton Signal.SIGUSR1)

        let darwin =
            SignalMask.ofSignals SignalNumbering.Darwin (Set.singleton Signal.SIGUSR1)

        Assert.Throws<exn> (fun () -> SignalMask.union linux darwin |> ignore<SignalMask>)
        |> ignore

        // The empty mask combines with either.
        SignalMask.union linux SignalMask.empty |> shouldEqual linux
        SignalMask.union SignalMask.empty darwin |> shouldEqual darwin
