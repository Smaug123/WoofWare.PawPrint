namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLocalMemoryPool =

    type private WriteCase =
        {
            BlockLength : int
            Offset : int
            Bytes : byte[]
        }

    type private TwoWriteCase =
        {
            BlockLength : int
            FirstOffset : int
            FirstBytes : byte[]
            SecondOffset : int
            SecondBytes : byte[]
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private allocateZeroInitialized (byteCount : int) (pool : LocalMemoryPool) : LocallocBlockId * LocalMemoryPool =
        LocalMemoryPool.allocate LocalMemoryInitialization.ZeroInitialized byteCount pool

    let private allocateUninitialized (byteCount : int) (pool : LocalMemoryPool) : LocallocBlockId * LocalMemoryPool =
        LocalMemoryPool.allocate LocalMemoryInitialization.Uninitialized byteCount pool

    let private genWriteCase : Gen<WriteCase> =
        gen {
            let! blockLength = Gen.choose (1, 128)

            let! offset =
                if blockLength = 1 then
                    Gen.constant 0
                else
                    Gen.frequency [ 1, Gen.constant 0 ; 3, Gen.choose (1, blockLength - 1) ]

            let! byteCount = Gen.choose (1, blockLength - offset)
            let! bytes = Gen.arrayOfLength byteCount (ArbMap.defaults |> ArbMap.generate<byte>)

            return
                {
                    BlockLength = blockLength
                    Offset = offset
                    Bytes = bytes
                }
        }

    let private genUninitializedWriteCase : Gen<WriteCase> =
        let genWholeBlockCase =
            gen {
                let! blockLength = Gen.choose (1, 128)
                let! bytes = Gen.arrayOfLength blockLength (ArbMap.defaults |> ArbMap.generate<byte>)

                return
                    {
                        BlockLength = blockLength
                        Offset = 0
                        Bytes = bytes
                    }
            }

        let genPrefixGapCase =
            gen {
                let! blockLength = Gen.choose (2, 128)
                let! offset = Gen.choose (1, blockLength - 1)
                let! byteCount = Gen.choose (1, blockLength - offset)
                let! bytes = Gen.arrayOfLength byteCount (ArbMap.defaults |> ArbMap.generate<byte>)

                return
                    {
                        BlockLength = blockLength
                        Offset = offset
                        Bytes = bytes
                    }
            }

        let genSuffixGapCase =
            gen {
                let! blockLength = Gen.choose (2, 128)
                let! offset = Gen.choose (0, blockLength - 2)
                let! byteCount = Gen.choose (1, blockLength - offset - 1)
                let! bytes = Gen.arrayOfLength byteCount (ArbMap.defaults |> ArbMap.generate<byte>)

                return
                    {
                        BlockLength = blockLength
                        Offset = offset
                        Bytes = bytes
                    }
            }

        Gen.frequency
            [
                1, genWholeBlockCase
                3, genPrefixGapCase
                3, genSuffixGapCase
                3, genWriteCase
            ]

    let private genTwoWriteCase : Gen<TwoWriteCase> =
        gen {
            let! blockLength = Gen.choose (1, 128)
            let! firstOffset = Gen.choose (0, blockLength - 1)
            let! firstCount = Gen.choose (1, blockLength - firstOffset)
            let! firstBytes = Gen.arrayOfLength firstCount (ArbMap.defaults |> ArbMap.generate<byte>)
            let! secondOffset = Gen.choose (0, blockLength - 1)
            let! secondCount = Gen.choose (1, blockLength - secondOffset)
            let! secondBytes = Gen.arrayOfLength secondCount (ArbMap.defaults |> ArbMap.generate<byte>)

            return
                {
                    BlockLength = blockLength
                    FirstOffset = firstOffset
                    FirstBytes = firstBytes
                    SecondOffset = secondOffset
                    SecondBytes = secondBytes
                }
        }

    [<Test>]
    let ``Allocated local memory block ids are unique`` () : unit =
        let block1, pool = allocateZeroInitialized 0 LocalMemoryPool.empty
        let block2, pool = allocateZeroInitialized 0 pool
        let block3, _ = allocateZeroInitialized 0 pool

        (block1 = block2) |> shouldEqual false
        (block1 = block3) |> shouldEqual false
        (block2 = block3) |> shouldEqual false

    [<Test>]
    let ``Allocated local memory is zero initialized`` () : unit =
        let property (NonNegativeInt blockLength) : unit =
            let blockLength = blockLength % 129
            let block, pool = allocateZeroInitialized blockLength LocalMemoryPool.empty
            let actual = LocalMemoryPool.readBytes block 0 blockLength pool
            actual |> shouldEqual (Array.zeroCreate<byte> blockLength)

        Check.One (config, property)

    [<Test>]
    let ``Uninitialized local memory cannot be read before write`` () : unit =
        let property (PositiveInt blockLength) : unit =
            let blockLength = (blockLength % 128) + 1
            let block, pool = allocateUninitialized blockLength LocalMemoryPool.empty

            Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.readBytes block 0 blockLength pool |> ignore)
            |> ignore

        Check.One (config, property)

    [<Test>]
    let ``Uninitialized local memory reads only succeed after covering writes`` () : unit =
        let mutable hasPrefixGap = 0
        let mutable hasSuffixGap = 0
        let mutable coversWholeBlock = 0

        let property (case : WriteCase) : unit =
            let block, pool = allocateUninitialized case.BlockLength LocalMemoryPool.empty
            let pool = LocalMemoryPool.writeBytes block case.Offset case.Bytes pool
            let writtenEnd = case.Offset + case.Bytes.Length

            LocalMemoryPool.readBytes block case.Offset case.Bytes.Length pool
            |> shouldEqual case.Bytes

            if case.Offset = 0 && writtenEnd = case.BlockLength then
                coversWholeBlock <- coversWholeBlock + 1

                LocalMemoryPool.readBytes block 0 case.BlockLength pool
                |> shouldEqual case.Bytes

            if case.Offset > 0 then
                hasPrefixGap <- hasPrefixGap + 1

                Assert.Throws<System.Exception> (fun () ->
                    LocalMemoryPool.readBytes block (case.Offset - 1) 1 pool |> ignore
                )
                |> ignore

            if writtenEnd < case.BlockLength then
                hasSuffixGap <- hasSuffixGap + 1

                Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.readBytes block writtenEnd 1 pool |> ignore)
                |> ignore

        Check.One (config, Prop.forAll (Arb.fromGen genUninitializedWriteCase) property)

        if hasPrefixGap < 150 || hasSuffixGap < 150 || coversWholeBlock < 40 then
            failwith
                $"Uninitialized-memory write generator was unbalanced: prefix gaps %d{hasPrefixGap}, suffix gaps %d{hasSuffixGap}, whole-block writes %d{coversWholeBlock}"

    [<Test>]
    let ``Write then read returns the written bytes`` () : unit =
        let mutable zeroOffsets = 0
        let mutable nonZeroOffsets = 0
        let mutable multiByteWrites = 0

        let property (case : WriteCase) : unit =
            if case.Offset = 0 then
                zeroOffsets <- zeroOffsets + 1
            else
                nonZeroOffsets <- nonZeroOffsets + 1

            if case.Bytes.Length > 1 then
                multiByteWrites <- multiByteWrites + 1

            let block, pool = allocateZeroInitialized case.BlockLength LocalMemoryPool.empty
            let pool = LocalMemoryPool.writeBytes block case.Offset case.Bytes pool
            let actual = LocalMemoryPool.readBytes block case.Offset case.Bytes.Length pool

            actual |> shouldEqual case.Bytes

        Check.One (config, Prop.forAll (Arb.fromGen genWriteCase) property)

        if zeroOffsets < 50 || nonZeroOffsets < 200 || multiByteWrites < 200 then
            failwith
                $"Local-memory write generator was unbalanced: zero offsets %d{zeroOffsets}, non-zero offsets %d{nonZeroOffsets}, multi-byte writes %d{multiByteWrites}"

    [<Test>]
    let ``Multiple writes preserve exactly the bytes they overwrite`` () : unit =
        let mutable overlappingWrites = 0
        let mutable nonOverlappingWrites = 0

        let property (case : TwoWriteCase) : unit =
            let firstEnd = case.FirstOffset + case.FirstBytes.Length
            let secondEnd = case.SecondOffset + case.SecondBytes.Length

            if case.FirstOffset < secondEnd && case.SecondOffset < firstEnd then
                overlappingWrites <- overlappingWrites + 1
            else
                nonOverlappingWrites <- nonOverlappingWrites + 1

            let block, pool = allocateZeroInitialized case.BlockLength LocalMemoryPool.empty

            let pool = LocalMemoryPool.writeBytes block case.FirstOffset case.FirstBytes pool
            let pool = LocalMemoryPool.writeBytes block case.SecondOffset case.SecondBytes pool

            let expected = Array.zeroCreate<byte> case.BlockLength
            Array.blit case.FirstBytes 0 expected case.FirstOffset case.FirstBytes.Length
            Array.blit case.SecondBytes 0 expected case.SecondOffset case.SecondBytes.Length

            LocalMemoryPool.readBytes block 0 case.BlockLength pool |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen genTwoWriteCase) property)

        if overlappingWrites < 100 || nonOverlappingWrites < 100 then
            failwith
                $"Local-memory two-write generator was unbalanced: overlapping writes %d{overlappingWrites}, non-overlapping writes %d{nonOverlappingWrites}"

    [<Test>]
    let ``Blocks in the same pool are isolated`` () : unit =
        let block1, pool = allocateZeroInitialized 4 LocalMemoryPool.empty
        let block2, pool = allocateZeroInitialized 4 pool

        let pool = LocalMemoryPool.writeBytes block1 1 [| 1uy ; 2uy |] pool

        LocalMemoryPool.readBytes block1 0 4 pool
        |> shouldEqual [| 0uy ; 1uy ; 2uy ; 0uy |]

        LocalMemoryPool.readBytes block2 0 4 pool
        |> shouldEqual [| 0uy ; 0uy ; 0uy ; 0uy |]

    [<Test>]
    let ``Out of range reads and writes fail visibly`` () : unit =
        let block, pool = allocateZeroInitialized 4 LocalMemoryPool.empty

        Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.readBytes block -1 1 pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.readBytes block 4 1 pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.writeBytes block 3 [| 1uy ; 2uy |] pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.readBytes block 0 -1 pool |> ignore)
        |> ignore

    [<Test>]
    let ``Zero sized blocks support zero byte reads`` () : unit =
        let block, pool = allocateUninitialized 0 LocalMemoryPool.empty
        LocalMemoryPool.readBytes block 0 0 pool |> shouldEqual [||]

        Assert.Throws<System.Exception> (fun () -> LocalMemoryPool.writeBytes block 0 [| 1uy |] pool |> ignore)
        |> ignore

    [<Test>]
    let ``Negative sized allocations fail visibly`` () : unit =
        Assert.Throws<System.Exception> (fun () -> allocateZeroInitialized -1 LocalMemoryPool.empty |> ignore)
        |> ignore
