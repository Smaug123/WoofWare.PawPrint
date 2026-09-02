namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStackMemoryPool =

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

    let private allocateZeroInitialized
        (byteCount : int)
        (pool : StackMemoryPool)
        : StackMemoryBlockId * StackMemoryPool
        =
        StackMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized byteCount pool

    let private allocateUninitialized
        (byteCount : int)
        (pool : StackMemoryPool)
        : StackMemoryBlockId * StackMemoryPool
        =
        StackMemoryPool.allocate MemoryBlockInitialization.Uninitialized byteCount pool

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
        let block1, pool = allocateZeroInitialized 0 StackMemoryPool.empty
        let block2, pool = allocateZeroInitialized 0 pool
        let block3, _ = allocateZeroInitialized 0 pool

        (block1 = block2) |> shouldEqual false
        (block1 = block3) |> shouldEqual false
        (block2 = block3) |> shouldEqual false

    [<Test>]
    let ``Allocated local memory is zero initialized`` () : unit =
        let property (NonNegativeInt blockLength) : unit =
            let blockLength = blockLength % 129
            let block, pool = allocateZeroInitialized blockLength StackMemoryPool.empty
            let actual = StackMemoryPool.readBytes block 0 blockLength pool
            actual |> shouldEqual (Array.zeroCreate<byte> blockLength)

        Check.One (config, property)

    [<Test>]
    let ``Uninitialized local memory cannot be read before write`` () : unit =
        let property (PositiveInt blockLength) : unit =
            let blockLength = (blockLength % 128) + 1
            let block, pool = allocateUninitialized blockLength StackMemoryPool.empty

            Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block 0 blockLength pool |> ignore)
            |> ignore

        Check.One (config, property)

    [<Test>]
    let ``Uninitialized local memory reads only succeed after covering writes`` () : unit =
        let mutable hasPrefixGap = 0
        let mutable hasSuffixGap = 0
        let mutable coversWholeBlock = 0

        let property (case : WriteCase) : unit =
            let block, pool = allocateUninitialized case.BlockLength StackMemoryPool.empty
            let pool = StackMemoryPool.writeBytes block case.Offset case.Bytes pool
            let writtenEnd = case.Offset + case.Bytes.Length

            StackMemoryPool.readBytes block case.Offset case.Bytes.Length pool
            |> shouldEqual case.Bytes

            if case.Offset = 0 && writtenEnd = case.BlockLength then
                coversWholeBlock <- coversWholeBlock + 1

                StackMemoryPool.readBytes block 0 case.BlockLength pool
                |> shouldEqual case.Bytes

            if case.Offset > 0 then
                hasPrefixGap <- hasPrefixGap + 1

                Assert.Throws<System.Exception> (fun () ->
                    StackMemoryPool.readBytes block (case.Offset - 1) 1 pool |> ignore
                )
                |> ignore

            if writtenEnd < case.BlockLength then
                hasSuffixGap <- hasSuffixGap + 1

                Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block writtenEnd 1 pool |> ignore)
                |> ignore

        Check.One (config, Prop.forAll (Arb.fromGen genUninitializedWriteCase) property)

        // For 500 cases, this generator has expected counts of roughly 405
        // prefix gaps, 416 suffix gaps, and 52 whole-block writes. These lower
        // bounds give a binomial lower-tail union bound below 1e-11 for a false
        // balance failure while still catching a broken generator branch.
        if hasPrefixGap < 341 || hasSuffixGap < 354 || coversWholeBlock < 13 then
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

            let block, pool = allocateZeroInitialized case.BlockLength StackMemoryPool.empty
            let pool = StackMemoryPool.writeBytes block case.Offset case.Bytes pool
            let actual = StackMemoryPool.readBytes block case.Offset case.Bytes.Length pool

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

            let block, pool = allocateZeroInitialized case.BlockLength StackMemoryPool.empty

            let pool = StackMemoryPool.writeBytes block case.FirstOffset case.FirstBytes pool
            let pool = StackMemoryPool.writeBytes block case.SecondOffset case.SecondBytes pool

            let expected = Array.zeroCreate<byte> case.BlockLength
            Array.blit case.FirstBytes 0 expected case.FirstOffset case.FirstBytes.Length
            Array.blit case.SecondBytes 0 expected case.SecondOffset case.SecondBytes.Length

            StackMemoryPool.readBytes block 0 case.BlockLength pool |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen genTwoWriteCase) property)

        if overlappingWrites < 100 || nonOverlappingWrites < 100 then
            failwith
                $"Local-memory two-write generator was unbalanced: overlapping writes %d{overlappingWrites}, non-overlapping writes %d{nonOverlappingWrites}"

    [<Test>]
    let ``Blocks in the same pool are isolated`` () : unit =
        let block1, pool = allocateZeroInitialized 4 StackMemoryPool.empty
        let block2, pool = allocateZeroInitialized 4 pool

        let pool = StackMemoryPool.writeBytes block1 1 [| 1uy ; 2uy |] pool

        StackMemoryPool.readBytes block1 0 4 pool
        |> shouldEqual [| 0uy ; 1uy ; 2uy ; 0uy |]

        StackMemoryPool.readBytes block2 0 4 pool
        |> shouldEqual [| 0uy ; 0uy ; 0uy ; 0uy |]

    [<Test>]
    let ``Out of range reads and writes fail visibly`` () : unit =
        let block, pool = allocateZeroInitialized 4 StackMemoryPool.empty

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block -1 1 pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block 4 1 pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.writeBytes block 3 [| 1uy ; 2uy |] pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block 0 -1 pool |> ignore)
        |> ignore

    [<Test>]
    let ``Zero sized blocks support zero byte reads`` () : unit =
        let block, pool = allocateUninitialized 0 StackMemoryPool.empty
        StackMemoryPool.readBytes block 0 0 pool |> shouldEqual [||]

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.writeBytes block 0 [| 1uy |] pool |> ignore)
        |> ignore

    [<Test>]
    let ``Negative sized allocations fail visibly`` () : unit =
        Assert.Throws<System.Exception> (fun () -> allocateZeroInitialized -1 StackMemoryPool.empty |> ignore)
        |> ignore

    [<Test>]
    let ``writeCell makes the cell readable via tryReadCell`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty
        let cell = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let pool = StackMemoryPool.writeCell block 4 cell pool

        StackMemoryPool.tryReadCell block 4 pool |> shouldEqual (Some cell)
        StackMemoryPool.tryReadCell block 0 pool |> shouldEqual None
        StackMemoryPool.tryReadCell block 8 pool |> shouldEqual None

    [<Test>]
    let ``tryFindCellCovering returns the cell covering the offset`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty
        let cell = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let pool = StackMemoryPool.writeCell block 4 cell pool

        StackMemoryPool.tryFindCellCovering block 4 pool |> shouldEqual (Some (4, cell))

        StackMemoryPool.tryFindCellCovering block 5 pool |> shouldEqual (Some (4, cell))

        StackMemoryPool.tryFindCellCovering block 7 pool |> shouldEqual (Some (4, cell))

        StackMemoryPool.tryFindCellCovering block 3 pool |> shouldEqual None
        StackMemoryPool.tryFindCellCovering block 8 pool |> shouldEqual None

    [<Test>]
    let ``tryFindCellCovering returns None for out-of-range offsets`` () : unit =
        let block, pool = allocateZeroInitialized 4 StackMemoryPool.empty

        StackMemoryPool.tryFindCellCovering block -1 pool |> shouldEqual None
        StackMemoryPool.tryFindCellCovering block 4 pool |> shouldEqual None
        StackMemoryPool.tryFindCellCovering block 100 pool |> shouldEqual None

    [<Test>]
    let ``writeCell evicts overlapping cells and bytes`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty
        let firstCell = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let pool = StackMemoryPool.writeCell block 0 firstCell pool

        let pool = StackMemoryPool.writeBytes block 4 [| 0xAAuy |] pool

        let secondCell =
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x55667788_99AABBCCL))

        let pool = StackMemoryPool.writeCell block 0 secondCell pool

        StackMemoryPool.tryReadCell block 0 pool |> shouldEqual (Some secondCell)
        StackMemoryPool.tryReadCell block 4 pool |> shouldEqual None

    [<Test>]
    let ``writeCell preserves provenance for tagged native-int sources`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty

        let cell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let pool = StackMemoryPool.writeCell block 0 cell pool

        StackMemoryPool.tryReadCell block 0 pool |> shouldEqual (Some cell)

    [<Test>]
    let ``readBytes refuses to render tagged pointer cells`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty

        let cell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let pool = StackMemoryPool.writeCell block 0 cell pool

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block 0 8 pool |> ignore)
        |> ignore

    [<Test>]
    let ``writeCell accepts replacing an existing cell at the same offset`` () : unit =
        let block, pool = allocateZeroInitialized 8 StackMemoryPool.empty
        let firstCell = CliType.Numeric (CliNumericType.Int32 0x11111111)
        let pool = StackMemoryPool.writeCell block 0 firstCell pool

        let secondCell = CliType.Numeric (CliNumericType.Int32 0x22222222)
        let pool = StackMemoryPool.writeCell block 0 secondCell pool

        StackMemoryPool.tryReadCell block 0 pool |> shouldEqual (Some secondCell)

    [<Test>]
    let ``Byte writes covering a primitive cell update the cell payload`` () : unit =
        let block, pool = allocateZeroInitialized 8 StackMemoryPool.empty
        let cell = CliType.Numeric (CliNumericType.Int32 0)
        let pool = StackMemoryPool.writeCell block 0 cell pool

        // 0x40490FDB = bit pattern for ~3.14159f. After writing those bytes
        // into the Int32 cell, the cell should hold that bit pattern as Int32.
        let bytes = [| 0xDBuy ; 0x0Fuy ; 0x49uy ; 0x40uy |]
        let pool = StackMemoryPool.writeBytes block 0 bytes pool

        StackMemoryPool.tryReadCell block 0 pool
        |> shouldEqual (Some (CliType.Numeric (CliNumericType.Int32 0x40490FDB)))

    [<Test>]
    let ``writeBytes refuses to scatter through tagged pointer cells`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty

        let cell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let pool = StackMemoryPool.writeCell block 0 cell pool

        Assert.Throws<System.Exception> (fun () ->
            StackMemoryPool.writeBytes block 0 [| 1uy ; 2uy ; 3uy ; 4uy |] pool |> ignore
        )
        |> ignore

    [<Test>]
    let ``writeCell evicts the overlay bytes it covers`` () : unit =
        // Eviction is not observable through reads: a cell shadows any overlay byte beneath it,
        // and when the cell is itself evicted, the bytes that become visible are the cell's own
        // (see `writeCell keeps the bytes of a cell it overlaps at its head` below), not
        // whatever lay underneath. So this asserts the representation invariant directly.
        let block, pool = allocateZeroInitialized 8 StackMemoryPool.empty

        let pool = StackMemoryPool.writeBytes block 1 [| 0xAAuy |] pool

        let pool =
            StackMemoryPool.writeCell block 0 (CliType.Numeric (CliNumericType.Int32 0x11223344)) pool

        StackMemoryPool.checkInvariants block pool

    let private int64Cell (value : int64) : CliType =
        CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value))

    [<Test>]
    let ``writeCell keeps the bytes of a cell it overlaps at its tail`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty
        let pool = StackMemoryPool.writeCell block 4 (int64Cell 0x1122334455667788L) pool
        let pool = StackMemoryPool.writeCell block 0 (int64Cell 0x0102030405060708L) pool

        StackMemoryPool.readBytes block 0 16 pool
        |> shouldEqual
            [|
                0x08uy
                0x07uy
                0x06uy
                0x05uy
                0x04uy
                0x03uy
                0x02uy
                0x01uy
                0x44uy
                0x33uy
                0x22uy
                0x11uy
                0uy
                0uy
                0uy
                0uy
            |]

        StackMemoryPool.tryReadCell block 4 pool |> shouldEqual None
        StackMemoryPool.checkInvariants block pool

    [<Test>]
    let ``writeCell keeps the bytes of a cell it overlaps at its head`` () : unit =
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty
        let pool = StackMemoryPool.writeCell block 0 (int64Cell 0x1122334455667788L) pool
        let pool = StackMemoryPool.writeCell block 4 (int64Cell 0x0102030405060708L) pool

        StackMemoryPool.readBytes block 0 16 pool
        |> shouldEqual
            [|
                0x88uy
                0x77uy
                0x66uy
                0x55uy
                0x08uy
                0x07uy
                0x06uy
                0x05uy
                0x04uy
                0x03uy
                0x02uy
                0x01uy
                0uy
                0uy
                0uy
                0uy
            |]

        StackMemoryPool.tryReadCell block 0 pool |> shouldEqual None
        StackMemoryPool.checkInvariants block pool

    [<Test>]
    let ``The bytes kept from an overlapped cell are the cell's, not the block's default`` () : unit =
        // In an uninitialised block the only bytes that can be read are ones that were written,
        // so a read of the kept tail succeeding at all shows the cell's bytes survived, while
        // the byte after them is still unreadable.
        let block, pool = allocateUninitialized 16 StackMemoryPool.empty
        let pool = StackMemoryPool.writeCell block 4 (int64Cell 0x1122334455667788L) pool
        let pool = StackMemoryPool.writeCell block 0 (int64Cell 0x0102030405060708L) pool

        StackMemoryPool.readBytes block 8 4 pool
        |> shouldEqual [| 0x44uy ; 0x33uy ; 0x22uy ; 0x11uy |]

        Assert.Throws<System.Exception> (fun () -> StackMemoryPool.readBytes block 12 1 pool |> ignore)
        |> ignore

    [<Test>]
    let ``writeCell refuses to overlap part of a tagged pointer cell`` () : unit =
        // A tagged pointer has no byte image, so the bytes of it that the new cell does not
        // cover cannot be kept; discarding them silently is the one thing that must not happen.
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty

        let tagged =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let pool = StackMemoryPool.writeCell block 4 tagged pool

        Assert.Throws<System.Exception> (fun () ->
            StackMemoryPool.writeCell block 0 (int64Cell 0x0102030405060708L) pool |> ignore
        )
        |> ignore

        Assert.Throws<System.Exception> (fun () ->
            StackMemoryPool.writeCell block 8 (int64Cell 0x0102030405060708L) pool |> ignore
        )
        |> ignore

    [<Test>]
    let ``writeCell replaces a tagged pointer cell it covers entirely`` () : unit =
        // Nothing of a fully covered cell survives, so its lack of a byte image is no obstacle,
        // whether the new cell is the same width or wider.
        let block, pool = allocateZeroInitialized 16 StackMemoryPool.empty

        let taggedInt =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let pool = StackMemoryPool.writeCell block 4 taggedInt pool
        let replacement = int64Cell 0x0102030405060708L
        let pool = StackMemoryPool.writeCell block 4 replacement pool
        StackMemoryPool.tryReadCell block 4 pool |> shouldEqual (Some replacement)

        let taggedByte =
            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.NativeIntByte (NativeIntSource.FieldHandlePtr 1234L, 0)))

        let pool = StackMemoryPool.writeCell block 2 taggedByte pool
        let wider = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let pool = StackMemoryPool.writeCell block 0 wider pool
        StackMemoryPool.tryReadCell block 0 pool |> shouldEqual (Some wider)
        StackMemoryPool.tryReadCell block 2 pool |> shouldEqual None
        StackMemoryPool.checkInvariants block pool

    /// Cells of every primitive width at any offset, so that a new cell can replace an existing
    /// one exactly, cover several, or overlap the head or the tail of one. A cell written over
    /// part of an existing cell keeps the bytes of the old cell it does not cover, so the whole
    /// history reads back as a last-write-wins byte array.
    type private MemOp =
        | WriteCell of offset : int * bytes : byte[]
        | WriteRawBytes of offset : int * bytes : byte[]

    let private blockLength : int = 16

    /// Every byte value equally likely. The default `byte` generator is size-bounded and so
    /// mostly produces small values, whose upper bytes are zero — exactly what a lost byte in a
    /// zero-initialised block also reads as.
    let private genFullRangeBytes (count : int) : Gen<byte[]> =
        Gen.arrayOfLength count (Gen.choose (0, 255) |> Gen.map byte)

    let private genMemOp : Gen<MemOp> =
        let genCell =
            gen {
                let! width = Gen.elements [ 1 ; 2 ; 4 ; 8 ]
                let! offset = Gen.choose (0, blockLength - width)
                let! bytes = genFullRangeBytes width
                return MemOp.WriteCell (offset, bytes)
            }

        let genBytes =
            gen {
                let! offset = Gen.choose (0, blockLength - 1)
                let! count = Gen.choose (1, blockLength - offset)
                let! bytes = genFullRangeBytes count
                return MemOp.WriteRawBytes (offset, bytes)
            }

        Gen.oneof [ genCell ; genBytes ]

    /// The primitive cell whose little-endian image is `bytes`. Decoded with `BitConverter`
    /// rather than with the interpreter's own `CliType` byte helpers, so the model is an
    /// independent oracle for the layout and not merely a restatement of it.
    let private cellOfBytes (bytes : byte[]) : CliType =
        match bytes.Length with
        | 1 -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim bytes.[0]))
        | 2 -> CliType.Numeric (CliNumericType.Int16 (System.BitConverter.ToInt16 (bytes, 0)))
        | 4 -> CliType.Numeric (CliNumericType.Int32 (System.BitConverter.ToInt32 (bytes, 0)))
        | 8 -> int64Cell (System.BitConverter.ToInt64 (bytes, 0))
        | other -> failwith $"no primitive cell is %d{other} bytes wide"

    [<Test>]
    let ``Mixed cell and byte writes read back as a last-write-wins byte array`` () : unit =
        // How often a cell write overlapped only part of an existing cell, at that cell's head
        // or at its tail. Those are the shapes a wholesale eviction gets wrong, so the run must
        // be seen to reach both.
        let headOverlaps = ref 0
        let tailOverlaps = ref 0
        let block = StackMemoryBlockId 0

        let property (ops : MemOp list) : unit =
            let pool =
                (snd (allocateZeroInitialized blockLength StackMemoryPool.empty), ops)
                ||> List.fold (fun pool op ->
                    let pool =
                        match op with
                        | MemOp.WriteCell (offset, bytes) ->
                            match StackMemoryPool.tryFindCellCovering block offset pool with
                            | Some (cellOffset, _) when cellOffset < offset ->
                                headOverlaps.Value <- headOverlaps.Value + 1
                            | _ -> ()

                            let last = offset + bytes.Length - 1

                            match StackMemoryPool.tryFindCellCovering block last pool with
                            | Some (cellOffset, cell) when cellOffset + CliType.sizeOf cell > last + 1 ->
                                tailOverlaps.Value <- tailOverlaps.Value + 1
                            | _ -> ()

                            StackMemoryPool.writeCell block offset (cellOfBytes bytes) pool
                        | MemOp.WriteRawBytes (offset, bytes) -> StackMemoryPool.writeBytes block offset bytes pool

                    StackMemoryPool.checkInvariants block pool
                    pool
                )

            let expected : byte[] = Array.zeroCreate blockLength

            for op in ops do
                match op with
                | MemOp.WriteCell (offset, bytes)
                | MemOp.WriteRawBytes (offset, bytes) -> System.Array.Copy (bytes, 0, expected, offset, bytes.Length)

            StackMemoryPool.readBytes block 0 blockLength pool |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.listOf genMemOp)) property)

        // Four runs of 500 cases each observed between 1700 and 1800 of each; a run that reached
        // fewer than 400 is exploring some other space than this one.
        headOverlaps.Value |> shouldBeGreaterThan 400
        tailOverlaps.Value |> shouldBeGreaterThan 400
