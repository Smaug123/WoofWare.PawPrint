namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMinipalRandom =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    /// Every platform preset, so that both flavours' syscalls are exercised.
    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// A fresh kernel on `platform` whose pool starts at `seed`.
    let private kernelAt (platform : SimulatedUnixPlatform) (seed : uint64) : EmulatedKernel =
        let kernel = EmulatedKernel.create platform StandardStreamsConfig.piped

        { kernel with
            Machine =
                { kernel.Machine with
                    EntropyPool = EntropyPool.ofSeed seed
                }
        }

    /// Request lengths either side of `getentropy`'s 256-byte limit, and of
    /// every multiple of eight, so that a request spans several calls on Darwin
    /// and leaves part of a generator output unused.
    let private genLength : Gen<int> =
        Gen.oneof [ Gen.choose (0, 24) ; Gen.choose (250, 270) ; Gen.choose (0, 1100) ]

    [<Test>]
    let ``every request gets the pool's next bytes, as a direct draw from the pool did`` () : unit =
        // The replay contract: the bytes a guest's `Guid.NewGuid` sees must not
        // change with how PawPrint asks the kernel for them. A direct draw from
        // the pool (`EntropyPool.draw`, which the library's `TestEntropyPool`
        // pins against a plain splitmix64) gave splitmix64 from the pool's
        // seed, a fresh output per eight bytes and a fresh one for each draw.
        // That is exactly `NonCryptoRandom.drawBytes`, PawPrint's own
        // splitmix64, chained from the same seed.
        let property (platform : SimulatedUnixPlatform) (seed : uint64) (lengths : int list) : unit =
            let actual, _ =
                lengths
                |> List.mapFold
                    (fun kernel length ->
                        let bytes, kernel = MinipalRandom.secureRandomBytes "test" length kernel
                        Seq.toArray bytes, kernel
                    )
                    (kernelAt platform seed)

            let expected, _ =
                lengths
                |> List.mapFold (fun state length -> NonCryptoRandom.drawBytes length state) seed

            actual |> shouldEqual expected

        Check.One (
            propertyConfig,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3
                        (Gen.elements platforms)
                        (ArbMap.defaults |> ArbMap.generate<uint64>)
                        (Gen.listOf genLength)
                ))
                (fun (platform, seed, lengths) -> property platform seed lengths)
        )

    [<Test>]
    let ``the kernel afterwards is the one its own entropy syscalls leave`` () : unit =
        // What the kernel answers a process that asks for `length` bytes: one
        // `getrandom` on Linux, whose limit no length here reaches, and
        // `getentropy` 256 bytes at a time on Darwin. The whole kernel is
        // compared, so a request that touched anything besides the pool fails.
        let viaSyscalls
            (length : int)
            (system : UnixSystem<ThreadId, NativeSignalHandler>)
            : byte list * UnixSystem<ThreadId, NativeSignalHandler>
            =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux ->
                if length = 0 then
                    [], system
                else

                match UnixEntropy.getRandom UserBuffer.Mapped (uint64 length) GetRandomFlags.Insecure system with
                | Ok (GetRandomAnswer.Completed draw, system) -> List.ofSeq (EntropyDraw.bytes draw), system
                | other -> failwith $"getrandom of %d{length} bytes answered %A{other}"
            | SimulatedUnixFlavour.Darwin ->
                let rec go (remaining : int) (acc : byte list) system =
                    if remaining = 0 then
                        acc, system
                    else

                    let chunk = min remaining 256

                    match UnixEntropy.getEntropy UserBuffer.Mapped (uint64 chunk) system with
                    | Ok (GetEntropyAnswer.Completed draw, system) ->
                        go (remaining - chunk) (acc @ List.ofSeq (EntropyDraw.bytes draw)) system
                    | other -> failwith $"getentropy of %d{chunk} bytes answered %A{other}"

                go length [] system

        let property (platform : SimulatedUnixPlatform) (seed : uint64) (length : int) : unit =
            let kernel = kernelAt platform seed
            let bytes, after = MinipalRandom.secureRandomBytes "test" length kernel
            let expectedBytes, expectedSystem = viaSyscalls length (EmulatedKernel.unix kernel)

            List.ofSeq bytes |> shouldEqual expectedBytes
            after |> shouldEqual (EmulatedKernel.withUnix expectedSystem kernel)

        Check.One (
            propertyConfig,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 (Gen.elements platforms) (ArbMap.defaults |> ArbMap.generate<uint64>) genLength))
                (fun (platform, seed, length) -> property platform seed length)
        )

    [<Test>]
    let ``a request for no bytes leaves the kernel alone`` () : unit =
        for platform in platforms do
            let kernel = kernelAt platform 0x1234UL
            let bytes, after = MinipalRandom.secureRandomBytes "test" 0 kernel
            bytes.Length |> shouldEqual 0
            after |> shouldEqual kernel

    [<Test>]
    let ``a negative length is refused`` () : unit =
        (fun () -> MinipalRandom.secureRandomBytes "test" -1 EmulatedKernel.initial |> ignore)
        |> shouldFail<exn>
