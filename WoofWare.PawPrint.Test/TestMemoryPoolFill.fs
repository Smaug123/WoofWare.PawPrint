namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMemoryPoolFill =

    /// One thing laid down in a block before the fill, at a fixed offset.
    [<RequireQualifiedAccess>]
    type private Piece =
        /// A typed cell. A tagged native int has no byte image; the others do.
        | Cell of CliType
        /// A byte written through the overlay.
        | Byte of byte
        /// A byte nothing writes.
        | Untouched

    /// What a byte of the block holds, as the oracle sees it.
    [<RequireQualifiedAccess>]
    type private Slot =
        | Known of byte
        /// Neither written nor defaulted: the block is uninitialised there.
        | Unset
        /// Inside the cell starting at this offset, which has no byte image.
        | Opaque of cellOffset : int

    /// The two pools share their block representation, and each case runs against one of them.
    [<RequireQualifiedAccess>]
    type private PoolKind =
        | Stack
        | Native

    /// A pool holding the one block a case fills.
    [<RequireQualifiedAccess>]
    type private Pool =
        | Stack of StackMemoryBlockId * StackMemoryPool
        | Native of NativeMemoryBlockId * NativeMemoryPool

    type private FillCase =
        {
            PoolKind : PoolKind
            Initialization : MemoryBlockInitialization
            /// Each piece with the offset it starts at, in ascending order and disjoint.
            Pieces : (int * Piece) list
            Size : int
            Offset : int
            Count : int
            Value : byte
        }

    [<RequireQualifiedAccess>]
    type private FillShape =
        | CoversOpaqueCellWhole
        | CutsOpaqueCell
        | CutsCellWithBytes
        | NonZeroValue
        | LeavesUnsetBytes

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private sizeOf (piece : Piece) : int =
        match piece with
        | Piece.Cell cell -> CliType.sizeOf cell
        | Piece.Byte _
        | Piece.Untouched -> 1

    let private genPiece : Gen<Piece> =
        Gen.frequency
            [
                3, Gen.choose (0, 255) |> Gen.map (byte >> Piece.Byte)
                2, Gen.constant Piece.Untouched
                1,
                Gen.choose (0, 255)
                |> Gen.map (fun n -> Piece.Cell (CliType.Numeric (CliNumericType.Int32 n)))
                1,
                Gen.choose (0, 255)
                |> Gen.map (fun n ->
                    Piece.Cell (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 n))))
                )
                3,
                Gen.choose (1, 1000)
                |> Gen.map (fun n ->
                    Piece.Cell (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr (int64 n))))
                )
            ]

    let private genFillCase : Gen<FillCase> =
        gen {
            let! poolKind = Gen.elements [ PoolKind.Stack ; PoolKind.Native ]

            let! initialization =
                Gen.elements
                    [
                        MemoryBlockInitialization.ZeroInitialized
                        MemoryBlockInitialization.Uninitialized
                    ]

            let! pieces = Gen.listOfLength 8 genPiece

            let placed, size =
                pieces |> List.mapFold (fun at piece -> (at, piece), at + sizeOf piece) 0
            // A quarter of the cases fill the whole block, so that a cell lying wholly inside the
            // range is common.
            let! wholeBlock = Gen.frequency [ 1, Gen.constant true ; 3, Gen.constant false ]
            let! offset = if wholeBlock then Gen.constant 0 else Gen.choose (0, size)

            let! count =
                if wholeBlock then
                    Gen.constant size
                else
                    Gen.choose (0, size - offset)

            let! value = Gen.frequency [ 1, Gen.constant 0uy ; 1, Gen.choose (1, 255) |> Gen.map byte ]

            return
                {
                    PoolKind = poolKind
                    Initialization = initialization
                    Pieces = placed
                    Size = size
                    Offset = offset
                    Count = count
                    Value = value
                }
        }

    let private allocate (case : FillCase) : Pool =
        match case.PoolKind with
        | PoolKind.Stack ->
            StackMemoryPool.allocate case.Initialization case.Size StackMemoryPool.empty
            |> Pool.Stack
        | PoolKind.Native ->
            NativeMemoryPool.allocate case.Initialization NativeBlockAlignment.malloc case.Size NativeMemoryPool.empty
            |> Pool.Native

    let private writeCell (offset : int) (cell : CliType) (pool : Pool) : Pool =
        match pool with
        | Pool.Stack (id, p) -> Pool.Stack (id, StackMemoryPool.writeCell id offset cell p)
        | Pool.Native (id, p) -> Pool.Native (id, NativeMemoryPool.writeCell id offset cell p)

    let private writeBytes (offset : int) (bytes : byte[]) (pool : Pool) : Pool =
        match pool with
        | Pool.Stack (id, p) -> Pool.Stack (id, StackMemoryPool.writeBytes id offset bytes p)
        | Pool.Native (id, p) -> Pool.Native (id, NativeMemoryPool.writeBytes id offset bytes p)

    let private fill (offset : int) (count : int) (value : byte) (pool : Pool) : Pool =
        match pool with
        | Pool.Stack (id, p) -> Pool.Stack (id, StackMemoryPool.fill id offset count value p)
        | Pool.Native (id, p) -> Pool.Native (id, NativeMemoryPool.fill id offset count value p)

    let private tryReadBytes (offset : int) (count : int) (pool : Pool) : byte[] voption =
        match pool with
        | Pool.Stack (id, p) -> StackMemoryPool.tryReadBytes id offset count p
        | Pool.Native (id, p) -> NativeMemoryPool.tryReadBytes id offset count p

    let private tryReadCell (offset : int) (pool : Pool) : CliType option =
        match pool with
        | Pool.Stack (id, p) -> StackMemoryPool.tryReadCell id offset p
        | Pool.Native (id, p) -> NativeMemoryPool.tryReadCell id offset p

    let private checkInvariants (pool : Pool) : unit =
        match pool with
        | Pool.Stack (id, p) -> StackMemoryPool.checkInvariants id p
        | Pool.Native (id, p) -> NativeMemoryPool.checkInvariants id p

    let private poolOf (case : FillCase) : Pool =
        case.Pieces
        |> List.fold
            (fun pool (at, piece) ->
                match piece with
                | Piece.Cell cell -> writeCell at cell pool
                | Piece.Byte b -> writeBytes at [| b |] pool
                | Piece.Untouched -> pool
            )
            (allocate case)

    let private slotsOf (case : FillCase) : Slot[] =
        let unwritten =
            match case.Initialization with
            | MemoryBlockInitialization.ZeroInitialized -> Slot.Known 0uy
            | MemoryBlockInitialization.Uninitialized -> Slot.Unset

        case.Pieces
        |> List.collect (fun (at, piece) ->
            match piece with
            | Piece.Byte b -> [ Slot.Known b ]
            | Piece.Untouched -> [ unwritten ]
            | Piece.Cell cell ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.ByteAddressable -> CliType.ToBytes cell |> Array.map Slot.Known |> List.ofArray
                | CliByteAddressability.SymbolicallyAddressable _
                | CliByteAddressability.Rejected _ -> List.replicate (CliType.sizeOf cell) (Slot.Opaque at)
        )
        |> Array.ofList

    let private inRange (case : FillCase) (offset : int) : bool =
        offset >= case.Offset && offset < case.Offset + case.Count

    [<Test>]
    let ``Filling a range sets its bytes, keeps the rest, and refuses to cut a cell with no byte image`` () : unit =
        let property (cover : FillShape -> unit) (case : FillCase) : unit =
            let pool = poolOf case
            let before = slotsOf case

            let cells =
                case.Pieces
                |> List.choose (fun (at, piece) ->
                    match piece with
                    | Piece.Cell cell -> Some (at, cell)
                    | Piece.Byte _
                    | Piece.Untouched -> None
                )

            let cut (at : int, cell : CliType) : bool =
                let size = CliType.sizeOf cell
                // An empty range intersects nothing, even one starting inside a cell.
                let intersects =
                    case.Count > 0 && at < case.Offset + case.Count && case.Offset < at + size

                let inside = case.Offset <= at && at + size <= case.Offset + case.Count
                intersects && not inside

            let hasNoByteImage (_ : int, cell : CliType) : bool =
                CliType.ByteAddressability cell <> CliByteAddressability.ByteAddressable

            if case.Value <> 0uy && case.Count > 0 then
                cover FillShape.NonZeroValue

            if cells |> List.exists (fun c -> cut c && not (hasNoByteImage c)) then
                cover FillShape.CutsCellWithBytes

            if cells |> List.exists (fun c -> cut c && hasNoByteImage c) then
                cover FillShape.CutsOpaqueCell

                let thrown =
                    Assert.Throws<System.Exception> (fun () -> fill case.Offset case.Count case.Value pool |> ignore)

                thrown.Message |> shouldContainText "no byte image to keep"
            else
                if
                    cells
                    |> List.exists (fun c -> hasNoByteImage c && not (cut c) && inRange case (fst c))
                then
                    cover FillShape.CoversOpaqueCellWhole

                let filled = fill case.Offset case.Count case.Value pool
                checkInvariants filled

                for offset in 0 .. case.Size - 1 do
                    let actual = tryReadBytes offset 1 filled

                    if inRange case offset then
                        actual |> shouldEqual (ValueSome [| case.Value |])
                    else
                        match before.[offset] with
                        | Slot.Known b -> actual |> shouldEqual (ValueSome [| b |])
                        | Slot.Unset ->
                            cover FillShape.LeavesUnsetBytes
                            actual |> shouldEqual ValueNone
                        | Slot.Opaque cellOffset ->
                            let original = cells |> List.find (fun (at, _) -> at = cellOffset) |> snd
                            tryReadCell cellOffset filled |> shouldEqual (Some original)

        let coverage = CoverageSample.check config (Arb.fromGen genFillCase) property

        let counts =
            [
                FillShape.CoversOpaqueCellWhole
                FillShape.CutsOpaqueCell
                FillShape.CutsCellWithBytes
                FillShape.NonZeroValue
                FillShape.LeavesUnsetBytes
            ]
            |> List.map (fun shape -> shape, coverage.Count shape)

        // Counted over a fixed sample, so a run cannot fall short by chance. The sample reaches each
        // shape between 130 and 215 times; the floor catches only a broken generator branch.
        for shape, count in counts do
            if count < 60 then
                failwith $"Fill generator reached %A{shape} only %d{count} times in 500: %A{counts}"

    [<Test>]
    let ``Filling no bytes inside a cell with no byte image leaves the cell`` () : unit =
        let cell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let block, pool =
            StackMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized 16 StackMemoryPool.empty

        let pool =
            StackMemoryPool.writeCell block 4 cell pool
            |> StackMemoryPool.fill block 8 0 0xFFuy

        StackMemoryPool.tryReadCell block 4 pool |> shouldEqual (Some cell)
