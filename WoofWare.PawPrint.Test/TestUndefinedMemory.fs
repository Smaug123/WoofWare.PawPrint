namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Undefined bytes in a `localloc` block, against a model that is simply the block's bytes, each
/// a number or undefined with the never-written byte it descends from.
///
/// The properties are the memory half of the undefined-value design: a typed read is undefined
/// exactly when a byte in its range is, an undefined value written to memory reads back byte for
/// byte, and a byte-by-byte copy carries each byte's definedness, origin included, across.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUndefinedMemory =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private thread : ThreadId = ThreadId 0
    let private frame : FrameId = FrameId 0

    let private memoryOf (block : StackMemoryBlockId) : UninitialisedMemory =
        UninitialisedMemory.Stack (thread, frame, block)

    /// A block elsewhere, which the undefined values these tests write descend from, so that a
    /// byte copied in is distinguishable from a byte of the block itself that nothing wrote.
    let private foreign : UninitialisedMemory =
        UninitialisedMemory.Native (NativeMemoryBlockId 99)

    let private primitiveKinds : UndefinedPrimitive list =
        [
            UndefinedPrimitive.Int8
            UndefinedPrimitive.UInt8
            UndefinedPrimitive.Int16
            UndefinedPrimitive.UInt16
            UndefinedPrimitive.Int32
            UndefinedPrimitive.Int64
            UndefinedPrimitive.NativeInt
            UndefinedPrimitive.Float32
            UndefinedPrimitive.Float64
            UndefinedPrimitive.Bool
            UndefinedPrimitive.Char
        ]

    [<RequireQualifiedAccess>]
    type private Op =
        | WriteBytes of offset : int * bytes : byte[]
        /// A defined primitive, written as a typed cell.
        | WriteCell of offset : int * value : CliType
        /// An undefined primitive, whose image has at least one undefined byte.
        | WriteUndefined of offset : int * value : UndefinedValue

    let private genValueByte : Gen<ValueByte> =
        Gen.frequency
            [
                2, ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map ValueByte.Defined
                1,
                Gen.choose (0, 1000)
                |> Gen.map (fun offset ->
                    ValueByte.Undefined
                        {
                            Memory = foreign
                            Offset = offset
                        }
                )
            ]

    let private genUndefined (kind : UndefinedPrimitive) : Gen<UndefinedValue> =
        Gen.arrayOfLength (UndefinedPrimitive.size kind) genValueByte
        |> Gen.map List.ofArray
        |> Gen.filter (
            List.exists (fun b ->
                match b with
                | ValueByte.Undefined _ -> true
                | ValueByte.Defined _ -> false
            )
        )
        |> Gen.map (fun bytes ->
            match UndefinedValue.tryOfBytes kind bytes with
            | ValueSome u -> u
            | ValueNone -> failwith "unreachable: the image has an undefined byte"
        )

    let private genOp (blockLength : int) : Gen<Op> =
        gen {
            let! kind = Gen.elements primitiveKinds
            let size = UndefinedPrimitive.size kind

            if size > blockLength then
                let! offset = Gen.choose (0, blockLength - 1)
                let! count = Gen.choose (1, blockLength - offset)
                let! bytes = Gen.arrayOfLength count (ArbMap.defaults |> ArbMap.generate<byte>)
                return Op.WriteBytes (offset, bytes)
            else
                let! offset = Gen.choose (0, blockLength - size)

                let! which = Gen.choose (0, 2)

                match which with
                | 0 ->
                    let! count = Gen.choose (1, blockLength - offset)
                    let! bytes = Gen.arrayOfLength count (ArbMap.defaults |> ArbMap.generate<byte>)
                    return Op.WriteBytes (offset, bytes)
                | 1 ->
                    let! bytes = Gen.arrayOfLength size (ArbMap.defaults |> ArbMap.generate<byte>)
                    return Op.WriteCell (offset, CliType.OfBytesLike (CliType.ZeroOfPrimitive kind) bytes)
                | _ ->
                    let! value = genUndefined kind
                    return Op.WriteUndefined (offset, value)
        }

    type private Case =
        {
            BlockLength : int
            Ops : Op list
            Reads : (int * UndefinedPrimitive) list
        }

    let private genCase : Gen<Case> =
        gen {
            let! blockLength = Gen.choose (1, 24)
            let! ops = Gen.listOf (genOp blockLength)

            let genRead =
                gen {
                    let! kind =
                        primitiveKinds
                        |> List.filter (fun k -> UndefinedPrimitive.size k <= blockLength)
                        |> Gen.elements

                    let! offset = Gen.choose (0, blockLength - UndefinedPrimitive.size kind)
                    return offset, kind
                }

            let! reads = Gen.nonEmptyListOf genRead

            return
                {
                    BlockLength = blockLength
                    Ops = ops
                    Reads = reads
                }
        }

    /// The block's bytes before anything is written: each undefined, descending from itself.
    let private initialModel (memory : UninitialisedMemory) (length : int) : ValueByte[] =
        Array.init
            length
            (fun i ->
                ValueByte.Undefined
                    {
                        Memory = memory
                        Offset = i
                    }
            )

    let private applyToModel (model : ValueByte[]) (op : Op) : unit =
        match op with
        | Op.WriteBytes (offset, bytes) -> bytes |> Array.iteri (fun i b -> model.[offset + i] <- ValueByte.Defined b)
        | Op.WriteCell (offset, value) ->
            CliType.ToBytes value
            |> Array.iteri (fun i b -> model.[offset + i] <- ValueByte.Defined b)
        | Op.WriteUndefined (offset, value) -> value.Bytes |> List.iteri (fun i b -> model.[offset + i] <- b)

    let private applyToPool (block : StackMemoryBlockId) (pool : StackMemoryPool) (op : Op) : StackMemoryPool =
        match op with
        | Op.WriteBytes (offset, bytes) -> StackMemoryPool.writeBytes block offset bytes pool
        | Op.WriteCell (offset, value) -> StackMemoryPool.writeCell block offset value pool
        | Op.WriteUndefined (offset, value) -> StackMemoryPool.writeCell block offset (CliType.Undefined value) pool

    let private definedByte (b : ValueByte) : byte =
        match b with
        | ValueByte.Defined b -> b
        | ValueByte.Undefined origin -> failwith $"test bug: expected a defined byte, got one from %O{origin}"

    let private isUndefined (b : ValueByte) : bool =
        match b with
        | ValueByte.Undefined _ -> true
        | ValueByte.Defined _ -> false

    /// What a read of `kind` at `offset` must give against `model`: an undefined value whose image
    /// is the model's bytes when any of them is undefined, and otherwise the number those bytes
    /// spell.
    let private checkRead
        (memory : UninitialisedMemory)
        (pool : StackMemoryPool)
        (model : ValueByte[])
        (offset : int, kind : UndefinedPrimitive)
        : unit
        =
        let size = UndefinedPrimitive.size kind
        let expected = Array.sub model offset size
        let read = StackMemoryPool.readValueBytes memory offset size pool

        let value =
            match read with
            | BlockBytes.Defined bytes ->
                Array.exists isUndefined expected |> shouldEqual false

                bytes
                |> shouldEqual (expected |> Array.map (definedByte >> UInt8Source.Verbatim))

                CliType.OfSymbolicBytesLike (CliType.ZeroOfPrimitive kind) bytes
            | BlockBytes.SomeUndefined bytes ->
                bytes |> shouldEqual expected
                CliType.OfValueBytesLike (CliType.ZeroOfPrimitive kind) bytes

        match value with
        | CliType.Undefined u ->
            Array.exists isUndefined expected |> shouldEqual true
            u.Kind |> shouldEqual kind
            u.Bytes |> shouldEqual (List.ofArray expected)
        | defined ->
            Array.exists isUndefined expected |> shouldEqual false
            CliType.TryPrimitiveShape defined |> shouldEqual (Some kind)

            CliType.ToBytes defined |> shouldEqual (expected |> Array.map definedByte)

    [<Test>]
    let ``A read is undefined exactly when a byte in its range is, and an undefined value written reads back``
        ()
        : unit
        =
        let mutable undefinedReads = 0
        let mutable definedReads = 0
        let mutable readsOverWrittenUndefined = 0

        let property (case : Case) : unit =
            let block, pool =
                StackMemoryPool.allocate MemoryBlockInitialization.Uninitialized case.BlockLength StackMemoryPool.empty

            let memory = memoryOf block
            let model = initialModel memory case.BlockLength

            let pool =
                (pool, case.Ops)
                ||> List.fold (fun pool op ->
                    applyToModel model op
                    let pool = applyToPool block pool op
                    StackMemoryPool.checkInvariants block pool
                    pool
                )

            for offset, kind in case.Reads do
                checkRead memory pool model (offset, kind)

                let slice = Array.sub model offset (UndefinedPrimitive.size kind)

                if Array.exists isUndefined slice then
                    undefinedReads <- undefinedReads + 1

                    if
                        slice
                        |> Array.exists (fun b ->
                            match b with
                            | ValueByte.Undefined origin -> origin.Memory = foreign
                            | ValueByte.Defined _ -> false
                        )
                    then
                        readsOverWrittenUndefined <- readsOverWrittenUndefined + 1
                else
                    definedReads <- definedReads + 1

        Check.One (config, Prop.forAll (Arb.fromGen genCase) property)

        if undefinedReads < 100 || definedReads < 100 || readsOverWrittenUndefined < 50 then
            failwith
                $"generator was unbalanced: %d{undefinedReads} undefined reads, %d{definedReads} defined, %d{readsOverWrittenUndefined} over a written undefined value"

    [<Test>]
    let ``A byte-by-byte copy carries each byte's definedness and origin across`` () : unit =
        let mutable copiedUndefined = 0

        let property (case : Case, destinationLength : int, sourceStart : int, count : int, destinationStart : int) =
            let pool = StackMemoryPool.empty

            let source, pool =
                StackMemoryPool.allocate MemoryBlockInitialization.Uninitialized case.BlockLength pool

            let destination, pool =
                StackMemoryPool.allocate MemoryBlockInitialization.Uninitialized destinationLength pool

            let sourceModel = initialModel (memoryOf source) case.BlockLength
            let destinationModel = initialModel (memoryOf destination) destinationLength

            let pool =
                (pool, case.Ops)
                ||> List.fold (fun pool op ->
                    applyToModel sourceModel op
                    applyToPool source pool op
                )

            // One byte at a time, as `CellAwareMemOps.copy` walks a raw block: read the byte as a
            // byte-typed value, then store that value.
            let byteTemplate = CliType.ZeroOfPrimitive UndefinedPrimitive.UInt8

            let pool =
                (pool, [ 0 .. count - 1 ])
                ||> List.fold (fun pool i ->
                    let value =
                        match StackMemoryPool.readValueBytes (memoryOf source) (sourceStart + i) 1 pool with
                        | BlockBytes.Defined bytes -> CliType.OfSymbolicBytesLike byteTemplate bytes
                        | BlockBytes.SomeUndefined bytes -> CliType.OfValueBytesLike byteTemplate bytes

                    destinationModel.[destinationStart + i] <- sourceModel.[sourceStart + i]
                    StackMemoryPool.writeCell destination (destinationStart + i) value pool
                )

            StackMemoryPool.checkInvariants destination pool

            for offset in 0 .. destinationLength - 1 do
                checkRead (memoryOf destination) pool destinationModel (offset, UndefinedPrimitive.UInt8)

            for offset in 0 .. destinationLength - 4 do
                checkRead (memoryOf destination) pool destinationModel (offset, UndefinedPrimitive.Int32)

            if Array.sub sourceModel sourceStart count |> Array.exists isUndefined then
                copiedUndefined <- copiedUndefined + 1

        let gen =
            gen {
                let! case = genCase
                let! destinationLength = Gen.choose (1, 24)
                let! sourceStart = Gen.choose (0, case.BlockLength - 1)
                let! count = Gen.choose (0, min (case.BlockLength - sourceStart) destinationLength)
                let! destinationStart = Gen.choose (0, destinationLength - count)
                return case, destinationLength, sourceStart, count, destinationStart
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

        if copiedUndefined < 100 then
            failwith $"generator was unbalanced: only %d{copiedUndefined} copies moved an undefined byte"

    [<Test>]
    let ``Moving an undefined value into a narrower or wider slot keeps exactly the bytes a move keeps`` () : unit =
        let origin (i : int) : ValueByte =
            ValueByte.Undefined
                {
                    Memory = foreign
                    Offset = i
                }

        let value (kind : UndefinedPrimitive) (bytes : ValueByte list) : UndefinedValue =
            match UndefinedValue.tryOfBytes kind bytes with
            | ValueSome u -> u
            | ValueNone -> failwith "test bug: image has no undefined byte"

        // A signed byte sign-extends: every byte of the wider slot copies its (undefined) sign.
        UndefinedValue.moveInto UndefinedPrimitive.Int32 (value UndefinedPrimitive.Int8 [ origin 0 ])
        |> shouldEqual (List.replicate 4 (origin 0))

        // An unsigned byte zero-extends, so the extension is defined.
        UndefinedValue.moveInto UndefinedPrimitive.Int32 (value UndefinedPrimitive.UInt8 [ origin 0 ])
        |> shouldEqual
            [
                origin 0
                ValueByte.Defined 0uy
                ValueByte.Defined 0uy
                ValueByte.Defined 0uy
            ]

        // A signed 16-bit value whose sign byte is defined extends with defined bytes.
        UndefinedValue.moveInto
            UndefinedPrimitive.Int32
            (value UndefinedPrimitive.Int16 [ origin 0 ; ValueByte.Defined 0x80uy ])
        |> shouldEqual
            [
                origin 0
                ValueByte.Defined 0x80uy
                ValueByte.Defined 0xFFuy
                ValueByte.Defined 0xFFuy
            ]

        // Truncation can drop every undefined byte.
        UndefinedValue.moveInto
            UndefinedPrimitive.UInt8
            (value UndefinedPrimitive.Int32 [ ValueByte.Defined 7uy ; origin 1 ; origin 2 ; origin 3 ])
        |> shouldEqual [ ValueByte.Defined 7uy ]

        // An int32 widens into a 64-bit slot by sign extension, per CoreCLR's importer.
        UndefinedValue.moveInto
            UndefinedPrimitive.NativeInt
            (value
                UndefinedPrimitive.Int32
                [
                    origin 0
                    ValueByte.Defined 0uy
                    ValueByte.Defined 0uy
                    ValueByte.Defined 0uy
                ])
        |> shouldEqual (
            [
                origin 0
                ValueByte.Defined 0uy
                ValueByte.Defined 0uy
                ValueByte.Defined 0uy
            ]
            @ List.replicate 4 (ValueByte.Defined 0uy)
        )

        // A float converted to another width is undefined in every byte.
        UndefinedValue.moveInto
            UndefinedPrimitive.Float32
            (value UndefinedPrimitive.Float64 (origin 5 :: List.replicate 7 (ValueByte.Defined 0uy)))
        |> shouldEqual (List.replicate 4 (origin 5))
