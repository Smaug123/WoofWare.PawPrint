namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Where CoreCLR's two copies of minipal get random bytes, per flavour.
///
/// On Linux, System.Native's copy reads a `/dev/urandom` descriptor it opens on
/// first use, and XORs glibc's `lrand48` over those bytes for its non-secure
/// entry point; CoreCLR's own copy is answered by `getrandom`. On Darwin, every
/// entry point draws from one generator libSystem seeds with `getentropy(32)`
/// before `main`. Measured by `minipal-random-descriptors.cs`,
/// `darwin-rng-interpose.c` and `lrand48.c` in
/// `docs/plans/2026-08-23-posix-kernel-extraction/`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMinipalRandom =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private thread : ThreadId = ThreadId 0

    let private linuxPlatforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.linuxArm64 ]

    /// A process booted from `image`, whose entropy pool is seeded with
    /// `seed`.
    let private bootedWithPool (image : UnixBootImage<ThreadId, NativeSignalHandler>) (seed : uint64) : EmulatedKernel =
        image |> UnixBootImage.withEntropySeed seed |> EmulatedKernel.boot

    /// A fresh kernel on `platform` whose pool starts at `seed`.
    let private linuxAt (platform : SimulatedUnixPlatform) (seed : uint64) : EmulatedKernel =
        bootedWithPool (EmulatedKernel.image platform StandardStreamsConfig.piped) seed

    let private genLength : Gen<int> =
        Gen.oneof [ Gen.choose (0, 24) ; Gen.choose (250, 270) ; Gen.choose (0, 1100) ]

    let private secure (length : int) (kernel : EmulatedKernel) : SecureRandomFill * EmulatedKernel =
        MinipalRandom.systemNativeSecureRandomBytes "test" thread length kernel

    let private filled (fill : SecureRandomFill) : byte[] =
        match fill with
        | SecureRandomFill.Filled bytes -> Seq.toArray bytes
        | SecureRandomFill.Failed (written, error) -> failwith $"failed with %O{error} after %d{written.Length} bytes"

    let private descriptorOf (kernel : EmulatedKernel) : int option =
        match kernel.ProcessRandom with
        | ProcessRandom.Minipal (MinipalUrandom.Open fd, _) -> Some fd
        | ProcessRandom.Minipal (MinipalUrandom.Unopened, _) -> None
        | ProcessRandom.LibSystem _ -> failwith "a Linux kernel holds libSystem's generator"

    let private openFds (kernel : EmulatedKernel) : int list =
        FileDescriptorRegistry.fds (UnixSystem.fileDescriptors kernel.System)
        |> Map.keys
        |> List.ofSeq

    // ------------------------------------------------------------------ lrand48

    [<Test>]
    let ``lrand48 reproduces glibc's first outputs for every measured seed`` () : unit =
        // `lrand48.c`, identical on Linux 6.18.5 aarch64 and x86-64 (glibc 2.41).
        let measured =
            [
                0L, [ 366850414L ; 1610402240L ; 206956554L ; 1869309841L ]
                1L, [ 89400484L ; 976015093L ; 1792756325L ; 721524505L ]
                13070L, [ 1196153321L ; 1604749343L ; 1245270141L ; 488256136L ]
                1790000000L, [ 1465080878L ; 1602157952L ; 105148618L ; 970465617L ]
                4294967295L, [ 644300343L ; 97305740L ; 768640432L ; 869611528L ]
                4294967296L, [ 366850414L ; 1610402240L ; 206956554L ; 1869309841L ]
                1250999896491L, [ 2031243605L ; 259578962L ; 133859582L ; 110670910L ]
                -1L, [ 644300343L ; 97305740L ; 768640432L ; 869611528L ]
            ]

        for seed, expected in measured do
            let outputs, _ =
                (Lrand48.seed seed, [ 1..4 ])
                ||> List.mapFold (fun state _ ->
                    let output, state = Lrand48.next state
                    output, state
                )
                |> fun (outputs, state) -> outputs, state

            outputs |> shouldEqual expected

    // ------------------------------------------------------------------ Linux, secure

    [<Test>]
    let ``Linux's secure bytes are the pool's, read through one descriptor kept from the first call`` () : unit =
        // The bytes a guest's `Guid.NewGuid` sees must not change with how
        // PawPrint asks the kernel for them: a read of /dev/urandom takes from
        // the pool as `getrandom` does, and that is splitmix64 from the pool's
        // seed, a fresh output per eight bytes and a fresh one for each draw.
        let property (platform : SimulatedUnixPlatform) (seed : uint64) (lengths : int list) : unit =
            let start = linuxAt platform seed

            let actual, kernel =
                lengths
                |> List.mapFold
                    (fun kernel length ->
                        let fill, kernel = secure length kernel
                        filled fill, kernel
                    )
                    start

            let expected, _ =
                lengths
                |> List.mapFold (fun state length -> NonCryptoRandom.drawBytes length state) seed

            actual |> shouldEqual expected

            match descriptorOf kernel with
            | None -> lengths |> shouldEqual []
            | Some fd ->
                // The lowest free descriptor at the first call, and one more
                // than the process held before.
                fd |> shouldEqual (List.max (openFds start) + 1)
                openFds kernel |> shouldEqual (openFds start @ [ fd ])

                match UnixPathResolution.fstat fd kernel.System with
                | Ok (FileStatusAnswer.Reported status) ->
                    status.Mode |> shouldEqual 0o020666
                    status.SpecialFileDevice |> shouldEqual 265L
                | other -> failwith $"fstat of minipal's descriptor: %A{other}"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3
                        (Gen.elements linuxPlatforms)
                        (ArbMap.defaults |> ArbMap.generate<uint64>)
                        (Gen.nonEmptyListOf genLength)
                ))
                (fun (platform, seed, lengths) -> property platform seed lengths)
        )

    [<Test>]
    let ``a guest that closes minipal's descriptor makes later secure calls fail with EBADF`` () : unit =
        let _, kernel = secure 16 (linuxAt SimulatedUnixPlatform.linuxX64 7UL)
        let fd = (descriptorOf kernel).Value

        let kernel =
            match UnixDescriptor.close fd kernel.System with
            | Ok (SyscallAnswer.Completed _, system) -> EmulatedKernel.withUnix system kernel
            | other -> failwith $"close: %A{other}"

        let fill, after = secure 16 kernel

        fill
        |> shouldEqual (SecureRandomFill.Failed (ImmutableArray.Empty, UnixError.EBADF))
        // minipal keeps the number, and the pool did not move.
        descriptorOf after |> shouldEqual (Some fd)

        (UnixSystem.entropyPool after.System)
        |> shouldEqual (UnixSystem.entropyPool kernel.System)

    [<Test>]
    let ``a request for no bytes still opens the descriptor and reads it once`` () : unit =
        // minipal's loop is a do-while: one read even of nothing.
        let fill, kernel = secure 0 (linuxAt SimulatedUnixPlatform.linuxX64 7UL)
        fill |> shouldEqual (SecureRandomFill.Filled ImmutableArray.Empty)
        let fd = (descriptorOf kernel).Value

        let kernel =
            match UnixDescriptor.close fd kernel.System with
            | Ok (SyscallAnswer.Completed _, system) -> EmulatedKernel.withUnix system kernel
            | other -> failwith $"close: %A{other}"

        secure 0 kernel
        |> fst
        |> shouldEqual (SecureRandomFill.Failed (ImmutableArray.Empty, UnixError.EBADF))

    [<Test>]
    let ``a descriptor that reads end-of-file is refused, as minipal would read it for ever`` () : unit =
        let _, kernel = secure 16 (linuxAt SimulatedUnixPlatform.linuxX64 7UL)
        let fd = (descriptorOf kernel).Value

        // Close it, and let the next open take its number: /dev/null.
        let system =
            match UnixDescriptor.close fd kernel.System with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"close: %A{other}"

        let system =
            match
                UnixNamespace.openPath
                    0
                    (PathArgumentBytes.Bytes (Result.toOption (UnixByteString.ofString "/dev/null")).Value)
                    0
                    system
            with
            | Ok (SyscallAnswer.Completed reopened, system) ->
                int reopened |> shouldEqual fd
                system
            | other -> failwith $"open: %A{other}"

        let thrown =
            Assert.Throws<exn> (fun () -> secure 16 (EmulatedKernel.withUnix system kernel) |> ignore)

        thrown.Message |> shouldContainText "answered 0"

    // ------------------------------------------------------------------ Linux, non-secure

    [<Test>]
    let ``Linux's non-secure bytes are the secure ones XORed with lrand48, seeded once from the coarse clock``
        ()
        : unit
        =
        let property (seed : uint64) (epochSeconds : int64) (lengths : int list) : unit =
            let start =
                bootedWithPool
                    (EmulatedKernel.image SimulatedUnixPlatform.linuxX64 StandardStreamsConfig.piped
                     |> EmulatedKernel.withWallClockEpochMs (epochSeconds * 1000L + 999L))
                    seed

            let actual, _ =
                lengths
                |> List.mapFold
                    (fun kernel length ->
                        match MinipalRandom.systemNativeNonSecureRandomBytes "test" thread length kernel with
                        | NonSecureRandomFill.Filled bytes, kernel -> Seq.toArray bytes, kernel
                        | other, _ -> failwith $"%A{other}"
                    )
                    start

            // glibc's generator written out again here: X ← (0x5DEECE66D·X +
            // 0xB) mod 2^48 from (seed << 16) | 0x330E, each output X >> 17.
            let lcg (x : uint64) =
                (0x5DEECE66DUL * x + 0xBUL) &&& 0xFFFF_FFFF_FFFFUL

            let mutable x = ((uint64 epochSeconds &&& 0xFFFF_FFFFUL) <<< 16) ||| 0x330EUL

            let pool, _ =
                lengths
                |> List.mapFold (fun state length -> NonCryptoRandom.drawBytes length state) seed

            let expected =
                pool
                |> List.map (fun (bytes : byte[]) ->
                    let mutable output = 0UL

                    bytes
                    |> Array.mapi (fun i b ->
                        if i % 4 = 0 then
                            x <- lcg x
                            output <- x >>> 17

                        let masked = b ^^^ byte output
                        output <- output >>> 8
                        masked
                    )
                )

            actual |> shouldEqual expected

        Check.One (
            propertyConfig,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3
                        (ArbMap.defaults |> ArbMap.generate<uint64>)
                        (Gen.choose (0, 2_000_000_000) |> Gen.map int64)
                        (Gen.nonEmptyListOf (Gen.choose (1, 40)))
                ))
                (fun (seed, epoch, lengths) -> property seed epoch lengths)
        )

    [<Test>]
    let ``Linux's non-secure call reports a failed secure read's errno and still applies its mask`` () : unit =
        let _, kernel = secure 16 (linuxAt SimulatedUnixPlatform.linuxX64 7UL)
        let fd = (descriptorOf kernel).Value

        let kernel =
            match UnixDescriptor.close fd kernel.System with
            | Ok (SyscallAnswer.Completed _, system) -> EmulatedKernel.withUnix system kernel
            | other -> failwith $"close: %A{other}"

        match MinipalRandom.systemNativeNonSecureRandomBytes "test" thread 6 kernel with
        | NonSecureRandomFill.OverExisting (written, mask, error), _ ->
            written.IsEmpty |> shouldEqual true
            error |> shouldEqual UnixError.EBADF
            mask.Length |> shouldEqual 6
        | other -> failwith $"%A{other}"

    // ------------------------------------------------------------------ CoreCLR's copy

    [<Test>]
    let ``CoreCLR's copy on Linux is getrandom's bytes, and opens no descriptor`` () : unit =
        let property (platform : SimulatedUnixPlatform) (seed : uint64) (length : int) : unit =
            let kernel = linuxAt platform seed

            let bytes, after =
                MinipalRandom.coreClrSecureRandomBytes "test" thread length kernel

            let expected, system =
                if length = 0 then
                    [||], kernel.System
                else

                match
                    UnixEntropy.getRandom thread UserBuffer.Mapped (uint64 length) GetRandomFlags.Insecure kernel.System
                with
                | Ok (GetRandomAnswer.Completed draw, system) -> Seq.toArray (EntropyDraw.bytes draw), system
                | other -> failwith $"%A{other}"

            Seq.toArray bytes |> shouldEqual expected
            after |> shouldEqual (EmulatedKernel.withUnix system kernel)

        Check.One (
            propertyConfig,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3 (Gen.elements linuxPlatforms) (ArbMap.defaults |> ArbMap.generate<uint64>) genLength
                ))
                (fun (platform, seed, length) -> property platform seed length)
        )

    // ------------------------------------------------------------------ Darwin

    [<Test>]
    let ``Darwin's libSystem takes 32 bytes before main, and every entry point draws from that one generator``
        ()
        : unit
        =
        let platform = SimulatedUnixPlatform.macOsArm64
        let kernel = EmulatedKernel.create platform StandardStreamsConfig.piped

        // What `create` asked of the kernel: one getentropy(32), and nothing
        // else.
        let seedBytes, booted: ImmutableArray<byte> * UnixSystem<ThreadId, NativeSignalHandler> =
            match
                UnixEntropy.getEntropy
                    UserBuffer.Mapped
                    32UL
                    (UnixSystem.initial platform UnixSystem.pipedStandardStreams (ThreadId 0) (CpuId 0)
                     |> UnixBootImage.boot)
            with
            | Ok (GetEntropyAnswer.Completed draw, system) -> EntropyDraw.bytes draw, system
            | other -> failwith $"%A{other}"

        (UnixSystem.entropyPool kernel.System)
        |> shouldEqual (UnixSystem.entropyPool booted)

        let property (draws : (int * int) list) : unit =
            let actual, after =
                draws
                |> List.mapFold
                    (fun kernel (entry, length) ->
                        match entry % 3 with
                        | 0 ->
                            match MinipalRandom.systemNativeSecureRandomBytes "test" thread length kernel with
                            | SecureRandomFill.Filled bytes, kernel -> Seq.toArray bytes, kernel
                            | other, _ -> failwith $"%A{other}"
                        | 1 ->
                            match MinipalRandom.systemNativeNonSecureRandomBytes "test" thread length kernel with
                            | NonSecureRandomFill.Filled bytes, kernel -> Seq.toArray bytes, kernel
                            | other, _ -> failwith $"%A{other}"
                        | _ ->
                            let bytes, kernel =
                                MinipalRandom.coreClrSecureRandomBytes "test" thread length kernel

                            Seq.toArray bytes, kernel
                    )
                    kernel

            let expected, _ =
                draws
                |> List.mapFold
                    (fun generator (_, length) ->
                        let bytes, generator = LibSystemRandom.draw length generator
                        Seq.toArray bytes, generator
                    )
                    (LibSystemRandom.ofSeed seedBytes)

            actual |> shouldEqual expected
            // No syscall, so no descriptor and no pool movement.
            after.System |> shouldEqual kernel.System

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.listOf (Gen.zip (Gen.choose (0, 2)) (Gen.choose (0, 300))))) property
        )

    [<Test>]
    let ``CoreCLR's copy on Linux, asked for more than a page with a signal pending, stops the run, naming why``
        ()
        : unit
        =
        // A default SIGCONT stays pending for the thread it was sent to.
        let kernel = linuxAt SimulatedUnixPlatform.linuxX64 7UL

        let system =
            match UnixSignal.pthreadKill thread 18 kernel.System with
            | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
            | other -> failwith $"pthread_kill: %A{other}"

        let kernel = EmulatedKernel.withUnix system kernel

        let thrown =
            Assert.Throws<exn> (fun () -> MinipalRandom.coreClrSecureRandomBytes "test" thread 4097 kernel |> ignore)

        thrown.Message |> shouldContainText "signal pending"

        // A page or less is answered whatever is pending.
        MinipalRandom.coreClrSecureRandomBytes "test" thread 4096 kernel
        |> fst
        |> fun bytes -> bytes.Length |> shouldEqual 4096

    [<Test>]
    let ``a negative length is refused`` () : unit =
        (fun () ->
            MinipalRandom.coreClrSecureRandomBytes "test" thread -1 EmulatedKernel.initial
            |> ignore
        )
        |> shouldFail<exn>

        (fun () ->
            MinipalRandom.systemNativeSecureRandomBytes "test" thread -1 EmulatedKernel.initial
            |> ignore
        )
        |> shouldFail<exn>
