namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `getrandom(2)` and `getentropy(2)` against the model's own rules: what each
/// answer does to the pool, and the orderings between the flags, the length, the
/// buffer screen and the copy. `TestEntropyAgainstHost` checks the answers
/// themselves against a real kernel.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixEntropy =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private linux () : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64

    let private darwin () : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.macOsArm64

    /// Every kind of buffer, with an unmapped address at each end of the space.
    let private bufferGen : Gen<UserBuffer> =
        Gen.oneof
            [
                Gen.constant UserBuffer.Mapped
                Gen.constant UserBuffer.Opaque
                Gen.constant UserBuffer.Addressless
                Gen.constant (UserBuffer.Unmapped 0UL)
                Gen.constant (UserBuffer.Unmapped UInt64.MaxValue)
                (ArbMap.defaults |> ArbMap.generate<uint64>) |> Gen.map UserBuffer.Unmapped
            ]

    /// Small counts, which is where the zero-length shortcut and the eight-byte
    /// output boundary are, plus arbitrary and huge ones. Only for calls that
    /// never reach a draw: a mapped buffer with the largest of these would
    /// allocate the whole answer.
    let private countGen : Gen<uint64> =
        Gen.oneof
            [
                Gen.choose (0, 40) |> Gen.map uint64
                (ArbMap.defaults |> ArbMap.generate<uint64>)
                Gen.elements [ 256UL ; 257UL ; 0x7FFF_F000UL ; UInt64.MaxValue ]
            ]

    let private validFlags : uint32 list =
        [
            0u
            GetRandomFlags.NonBlock
            GetRandomFlags.Random
            GetRandomFlags.Insecure
        ]
        @ [ GetRandomFlags.NonBlock ||| GetRandomFlags.Random ]
        @ [ GetRandomFlags.NonBlock ||| GetRandomFlags.Insecure ]

    /// `system` with its pool replaced by `reference`'s, so that "nothing but
    /// the pool changed" is one equality.
    let private withPoolOf
        (reference : UnixSystem<int, string>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Machine =
                { system.Machine with
                    EntropyPool = reference.Machine.EntropyPool
                }
        }

    // ------------------------------------------------------------------
    // getrandom
    // ------------------------------------------------------------------

    /// Unknown bits, and `GRND_RANDOM` with `GRND_INSECURE`, are EINVAL ahead of
    /// everything else: the buffer is not screened, the length is not looked at,
    /// and nothing is drawn.
    [<Test>]
    let ``getrandom answers malformed flags before anything else`` () : unit =
        let malformedFlags : Gen<uint32> =
            Gen.oneof
                [
                    Gen.elements [ 6u ; 7u ; 8u ; 0x8000_0000u ; UInt32.MaxValue ]
                    (ArbMap.defaults |> ArbMap.generate<uint32>)
                    |> Gen.filter (fun flags -> not (List.contains flags validFlags))
                ]

        let property (flags : uint32, buffer : UserBuffer, count : uint64) : unit =
            let system = linux ()

            match UnixEntropy.getRandom buffer count flags system with
            | Ok (GetRandomAnswer.Failed error, after) ->
                error |> shouldEqual UnixError.EINVAL
                after |> shouldEqual system
            | other -> failwith $"flags 0x%x{flags}: expected EINVAL, got %O{other}"

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip3 malformedFlags bufferGen countGen)) property)

    /// There is one pool, so every accepted combination of flags draws the same
    /// bytes from it, and moves it the same distance.
    [<Test>]
    let ``every valid flag combination draws from the one pool`` () : unit =
        let property (count : byte) : unit =
            let system = linux ()
            let expected, pool = EntropyPool.draw (int count) system.Machine.EntropyPool

            for flags in validFlags do
                match UnixEntropy.getRandom UserBuffer.Mapped (uint64 count) flags system with
                | Ok (GetRandomAnswer.Completed draw, after) ->
                    Seq.toArray (EntropyDraw.bytes draw) |> shouldEqual (Seq.toArray expected)
                    after.Machine.EntropyPool |> shouldEqual pool
                    withPoolOf system after |> shouldEqual system
                | other -> failwith $"flags 0x%x{flags}, count %d{count}: expected bytes, got %O{other}"

        Check.One (config, property)

    /// A zero-length call returns 0 without consulting the buffer — but only
    /// once the screen has passed it, which a buffer ending past the address
    /// limit does not.
    [<Test>]
    let ``getrandom of nothing is screened, then touches nothing`` () : unit =
        let system = linux ()

        for buffer in [ UserBuffer.Mapped ; UserBuffer.Opaque ; UserBuffer.Unmapped 0UL ] do
            match UnixEntropy.getRandom buffer 0UL 0u system with
            | Ok (GetRandomAnswer.Completed draw, after) ->
                EntropyDraw.count draw |> shouldEqual 0
                after |> shouldEqual system
            | other -> failwith $"%O{buffer}: expected nothing to move, got %O{other}"

        UnixEntropy.getRandom (UserBuffer.Unmapped UInt64.MaxValue) 0UL 0u system
        |> shouldEqual (Ok (GetRandomAnswer.Failed UnixError.EFAULT, system))

        UnixEntropy.getRandom UserBuffer.Addressless 0UL 0u system
        |> shouldEqual (Error (GetRandomRefusal.Buffer BufferRefusal.AddresslessAtScreen))

    /// An address the screen passes but the copy cannot write through is EFAULT,
    /// and the pool does not move.
    [<Test>]
    let ``getrandom into an unmapped buffer is EFAULT`` () : unit =
        let property (count : byte) : unit =
            let count = uint64 count + 1UL
            let system = linux ()

            UnixEntropy.getRandom (UserBuffer.Unmapped 0UL) count 0u system
            |> shouldEqual (Ok (GetRandomAnswer.Failed UnixError.EFAULT, system))

        Check.One (config, property)

    [<Test>]
    let ``getrandom into an opaque buffer is refused at the copy`` () : unit =
        UnixEntropy.getRandom UserBuffer.Opaque 5UL 0u (linux ())
        |> shouldEqual (Error (GetRandomRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

    /// One call moves at most `UnixEntropy.getRandomMaxTransfer` bytes, however
    /// many were asked for, and moves the pool past exactly those. Nothing here
    /// produces more than a few bytes: `TestEntropyPool` shows that a draw's
    /// pieces are the draw, so its last bytes stand in for the rest.
    [<Test>]
    let ``getrandom transfers at most the most one call can`` () : unit =
        let maxTransfer = UnixEntropy.getRandomMaxTransfer SimulatedUnixPlatform.linuxX64
        let limit = int maxTransfer

        for count in [ maxTransfer + 1UL ; 1UL <<< 31 ; 1UL <<< 40 ; UInt64.MaxValue ] do
            let system = linux ()
            let pool = system.Machine.EntropyPool

            match UnixEntropy.getRandom UserBuffer.Mapped count 0u system with
            | Ok (GetRandomAnswer.Completed draw, after) ->
                EntropyDraw.count draw |> shouldEqual limit
                after.Machine.EntropyPool |> shouldEqual (snd (EntropyPool.take limit pool))

                // The draw's last bytes are the stream's bytes at that point.
                let tailStart = limit - 16
                let tail, _ = EntropyPool.draw 16 (snd (EntropyPool.take tailStart pool))

                Seq.toArray (EntropyDraw.range tailStart 16 draw)
                |> shouldEqual (Seq.toArray tail)
            | other -> failwith $"%d{count} bytes: expected a short transfer, got %O{other}"

        // At the limit itself nothing is cut short.
        match UnixEntropy.getRandom UserBuffer.Mapped maxTransfer 0u (linux ()) with
        | Ok (GetRandomAnswer.Completed draw, _) -> EntropyDraw.count draw |> shouldEqual limit
        | other -> failwith $"expected the whole request, got %O{other}"

    [<Test>]
    let ``Darwin has no getrandom`` () : unit =
        let property (flags : uint32, buffer : UserBuffer, count : uint64) : unit =
            UnixEntropy.getRandom buffer count flags (darwin ())
            |> shouldEqual (Error (GetRandomRefusal.NoSuchSyscall SimulatedUnixFlavour.Darwin))

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 ((ArbMap.defaults |> ArbMap.generate<uint32>)) bufferGen countGen))
                property
        )

    // ------------------------------------------------------------------
    // getentropy
    // ------------------------------------------------------------------

    [<Test>]
    let ``getentropy fills exactly the buffer asked for`` () : unit =
        let property (length : byte) : unit =
            let system = darwin ()
            let expected, pool = EntropyPool.draw (int length) system.Machine.EntropyPool

            match UnixEntropy.getEntropy UserBuffer.Mapped (uint64 length) system with
            | Ok (GetEntropyAnswer.Completed draw, after) ->
                Seq.toArray (EntropyDraw.bytes draw) |> shouldEqual (Seq.toArray expected)
                after.Machine.EntropyPool |> shouldEqual pool
                withPoolOf system after |> shouldEqual system
            | other -> failwith $"length %d{length}: expected bytes, got %O{other}"

        Check.One (config, property)

    /// Above the limit is EINVAL whatever the buffer, including one that could
    /// not be screened or copied through.
    [<Test>]
    let ``getentropy refuses a length above the limit before the buffer`` () : unit =
        let tooLong : Gen<uint64> =
            (ArbMap.defaults |> ArbMap.generate<uint64>)
            |> Gen.filter (fun length -> length > UnixEntropy.getEntropyMaxLength)

        let property (buffer : UserBuffer, length : uint64) : unit =
            let system = darwin ()

            UnixEntropy.getEntropy buffer length system
            |> shouldEqual (Ok (GetEntropyAnswer.Failed UnixError.EINVAL, system))

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip bufferGen tooLong)) property)

    [<Test>]
    let ``getentropy at the limit is answered`` () : unit =
        match UnixEntropy.getEntropy UserBuffer.Mapped UnixEntropy.getEntropyMaxLength (darwin ()) with
        | Ok (GetEntropyAnswer.Completed draw, _) ->
            uint64 (EntropyDraw.count draw) |> shouldEqual UnixEntropy.getEntropyMaxLength
        | other -> failwith $"expected bytes, got %O{other}"

    /// Darwin screens nothing up front, so a zero-length call returns 0 at any
    /// address at all.
    [<Test>]
    let ``getentropy of nothing touches nothing`` () : unit =
        let system = darwin ()

        for buffer in
            [
                UserBuffer.Mapped
                UserBuffer.Opaque
                UserBuffer.Addressless
                UserBuffer.Unmapped 0UL
                UserBuffer.Unmapped UInt64.MaxValue
            ] do
            match UnixEntropy.getEntropy buffer 0UL system with
            | Ok (GetEntropyAnswer.Completed draw, after) ->
                EntropyDraw.count draw |> shouldEqual 0
                after |> shouldEqual system
            | other -> failwith $"%O{buffer}: expected nothing to move, got %O{other}"

    [<Test>]
    let ``getentropy into an unmapped buffer is EFAULT`` () : unit =
        let system = darwin ()

        for address in [ 0UL ; UInt64.MaxValue ] do
            for length in [ 1UL ; 5UL ; UnixEntropy.getEntropyMaxLength ] do
                UnixEntropy.getEntropy (UserBuffer.Unmapped address) length system
                |> shouldEqual (Ok (GetEntropyAnswer.Failed UnixError.EFAULT, system))

    [<Test>]
    let ``getentropy into a buffer it cannot write is refused at the copy`` () : unit =
        UnixEntropy.getEntropy UserBuffer.Opaque 5UL (darwin ())
        |> shouldEqual (Error (GetEntropyRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        UnixEntropy.getEntropy UserBuffer.Addressless 5UL (darwin ())
        |> shouldEqual (Error (GetEntropyRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

    [<Test>]
    let ``Linux has no getentropy`` () : unit =
        let property (buffer : UserBuffer, length : uint64) : unit =
            UnixEntropy.getEntropy buffer length (linux ())
            |> shouldEqual (Error (GetEntropyRefusal.NoSuchSyscall SimulatedUnixFlavour.Linux))

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip bufferGen countGen)) property)

    // ------------------------------------------------------------------
    // Both
    // ------------------------------------------------------------------

    /// A call resumes the stream where the previous one left it, less the
    /// unused tail of the previous call's last output.
    [<Test>]
    let ``consecutive draws continue the stream`` () : unit =
        let property (first : byte, second : byte) : unit =
            let system = linux ()
            let whole, _ = EntropyPool.draw 1000 system.Machine.EntropyPool
            let firstRounded = (int first + 7) / 8 * 8

            match UnixEntropy.getRandom UserBuffer.Mapped (uint64 first) 0u system with
            | Ok (GetRandomAnswer.Completed a, afterFirst) ->
                match UnixEntropy.getRandom UserBuffer.Mapped (uint64 second) 0u afterFirst with
                | Ok (GetRandomAnswer.Completed b, _) ->
                    Seq.toArray (EntropyDraw.bytes a)
                    |> shouldEqual (whole |> Seq.take (int first) |> Seq.toArray)

                    Seq.toArray (EntropyDraw.bytes b)
                    |> shouldEqual (whole |> Seq.skip firstRounded |> Seq.take (int second) |> Seq.toArray)
                | other -> failwith $"second draw: %O{other}"
            | other -> failwith $"first draw: %O{other}"

        Check.One (config, property)
