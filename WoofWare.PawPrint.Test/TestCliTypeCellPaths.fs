namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `CliType.CellPathsExactlyCovering` answers "which storage cells does this byte range name?",
/// descending through nested value types rather than stopping at the top level. It is the primitive
/// the byref layer needs when storage cannot be rendered as bytes at all — a value type containing
/// object references — because then naming the cell is the only way to serve the access.
///
/// It is deliberately *structural*: it reports every cell whose extent is exactly the range,
/// outermost first, and says nothing about whether any of them is type-compatible with what the
/// caller wants to read or write. Callers apply their own rule to the returned contents. More than
/// one answer is normal and not an ambiguity — a transparent wrapper and the field it wraps occupy
/// the same bytes, and which one a caller wants depends on the type it is reinterpreting to.
[<TestFixture>]
module TestCliTypeCellPaths =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private declaredHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.TypedReference

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int32

    let private int64Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int64

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Byte

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Object

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    let private cliField
        (name : string)
        (contents : CliType)
        (offset : int option)
        (fieldType : ConcreteTypeHandle)
        : CliField
        =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = offset
            Type = fieldType
            MarshallingDescriptor = None
        }

    let private ofFields (fields : CliField list) : CliType =
        CliValueType.OfFields bct allCt declaredHandle Layout.Default CharSet.Ansi fields
        |> CliType.ValueType

    let private ofFieldsSized (size : int) (fields : CliField list) : CliType =
        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = size, packingSize = 0))
            CharSet.Ansi
            fields
        |> CliType.ValueType

    // ----------------------------------------------------------------------------------------
    // Worked examples
    // ----------------------------------------------------------------------------------------

    /// The shape that motivated the primitive: `[InlineArray(2)] struct { Elem _item; }` where
    /// `Elem = { byte Tag; Box Payload }`. The reference is laid out first (GC auto-layout promotes
    /// it), so `Tag` sits at offset 8 *inside* each slot — depth 2 from the buffer's point of view,
    /// which no top-level cover can reach.
    let private elem () : CliType =
        ofFields
            [
                cliField "Payload" (CliType.ObjectRef None) None objectHandle
                cliField "Tag" (CliType.Numeric (CliNumericType.UInt8 0uy)) None byteHandle
            ]

    let private buffer () : CliType =
        let e = elem ()

        ofFields
            [
                cliField "_item" e None declaredHandle
                cliField "_item[1]" e None declaredHandle
            ]

    [<Test>]
    let ``inline array of reference-containing structs: slot is found at depth 1`` () : unit =
        let value = buffer ()
        CliType.SizeOf(value).Size |> shouldEqual 32

        match CliType.CellPathsExactlyCovering 16 16 value with
        | [ (path, contents) ] ->
            path |> shouldEqual [ FieldId.named "_item[1]" ]
            contents |> shouldEqual (elem ())
        | other -> failwith $"expected exactly one cover of slot 1, got %O{other}"

    [<Test>]
    let ``inline array of reference-containing structs: primitive leaf is found at depth 2`` () : unit =
        let value = buffer ()

        // `Tag` of slot 0 lives at offset 8; only the depth-2 descent can name it.
        match CliType.CellPathsExactlyCovering 8 1 value with
        | [ (path, contents) ] ->
            path |> shouldEqual [ FieldId.named "_item" ; FieldId.named "Tag" ]
            contents |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0uy))
        | other -> failwith $"expected exactly one cover of slot 0's Tag, got %O{other}"

    [<Test>]
    let ``inline array of reference-containing structs: reference leaf is found at depth 2`` () : unit =
        let value = buffer ()

        match CliType.CellPathsExactlyCovering 16 8 value with
        | [ (path, contents) ] ->
            path |> shouldEqual [ FieldId.named "_item[1]" ; FieldId.named "Payload" ]
            contents |> shouldEqual (CliType.ObjectRef None)
        | other -> failwith $"expected exactly one cover of slot 1's Payload, got %O{other}"

    /// A transparent wrapper and the field it wraps occupy the same bytes, so both are legitimate
    /// answers. The caller decides which it wants by looking at the contents; the resolver must not
    /// pick for it, and must report the outer one first so a caller that stops at the first
    /// compatible answer gets the shallowest.
    [<Test>]
    let ``nested transparent wrappers all report, outermost first`` () : unit =
        let inner = ofFields [ cliField "_item" (CliType.ObjectRef None) None objectHandle ]
        let middle = ofFields [ cliField "wrapped" inner None declaredHandle ]
        let value = ofFields [ cliField "outer" middle None declaredHandle ]

        CliType.CellPathsExactlyCovering 0 8 value
        |> List.map fst
        |> shouldEqual
            [
                [ FieldId.named "outer" ]
                [ FieldId.named "outer" ; FieldId.named "wrapped" ]
                [ FieldId.named "outer" ; FieldId.named "wrapped" ; FieldId.named "_item" ]
            ]

    [<Test>]
    let ``a range straddling two fields names nothing`` () : unit =
        let value =
            ofFields
                [
                    cliField "A" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                    cliField "B" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                ]

        // [2, 6) straddles A and B.
        CliType.CellPathsExactlyCovering 2 4 value |> shouldEqual []

    [<Test>]
    let ``a range covering part of one field names nothing`` () : unit =
        let value =
            ofFields
                [
                    cliField "A" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
                ]

        CliType.CellPathsExactlyCovering 0 4 value |> shouldEqual []
        CliType.CellPathsExactlyCovering 4 4 value |> shouldEqual []

    /// Under explicit layout two fields can alias. Naming either would let a write leave the other
    /// stale, so an aliased range names nothing even though one field covers it exactly.
    [<Test>]
    let ``an aliased range names nothing`` () : unit =
        let value =
            ofFieldsSized
                8
                [
                    cliField
                        "AsLong"
                        (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                        (Some 0)
                        int64Handle
                    cliField "Upper" (CliType.Numeric (CliNumericType.Int32 0)) (Some 4) int32Handle
                ]

        CliType.CellPathsExactlyCovering 0 8 value |> shouldEqual []
        CliType.CellPathsExactlyCovering 4 4 value |> shouldEqual []

    /// An *abutting* sibling must not block: otherwise no field past the first could ever be named.
    /// This is the boundary case of the aliasing rule.
    [<Test>]
    let ``an adjacent sibling does not block`` () : unit =
        let value =
            ofFields
                [
                    cliField "A" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                    cliField "B" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                ]

        CliType.CellPathsExactlyCovering 4 4 value
        |> List.map fst
        |> shouldEqual [ [ FieldId.named "B" ] ]

    [<Test>]
    let ``a degenerate range names nothing`` () : unit =
        let value =
            ofFields [ cliField "A" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle ]

        CliType.CellPathsExactlyCovering 0 0 value |> shouldEqual []
        CliType.CellPathsExactlyCovering 0 -4 value |> shouldEqual []

    /// The offset is guest-controlled: it accumulates `Unsafe.Add`/`Unsafe.AddByteOffset`
    /// arithmetic, which the guest may drive to either end of the range. Such a range names nothing,
    /// and must *say* so — this file compiles under `open Checked`, so computing the range's end
    /// point would raise `OverflowException` out of a lookup documented to return `[]`.
    [<Test>]
    let ``a range whose end point does not fit in an int names nothing`` () : unit =
        let value =
            ofFields [ cliField "A" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle ]

        CliType.CellPathsExactlyCovering System.Int32.MaxValue 4 value |> shouldEqual []

        CliType.CellPathsExactlyCovering (System.Int32.MaxValue - 1) 8 value
        |> shouldEqual []

        CliType.CellPathsExactlyCovering System.Int32.MinValue 4 value |> shouldEqual []

        CliType.CellPathsExactlyCovering System.Int32.MinValue System.Int32.MaxValue value
        |> shouldEqual []

    /// `f.Size` is set from `SizeOf(f.Contents)` when a value is built, but `WithFieldSetById`
    /// replaces `Contents` without recomputing it — so a field record can be made inconsistent, and
    /// a cell whose recorded extent disagrees with what it now holds must not be named. Reached
    /// through the public API rather than asserted as unreachable, because mutation-testing the
    /// guard showed nothing else covers it.
    [<Test>]
    let ``a cell whose recorded size disagrees with its contents names nothing`` () : unit =
        let value =
            ofFields
                [
                    cliField "A" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                    cliField "B" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                ]

        CliType.CellPathsExactlyCovering 0 4 value
        |> List.map fst
        |> shouldEqual [ [ FieldId.named "A" ] ]

        // Widen A's contents to 8 bytes without its recorded 4-byte extent following.
        let inconsistent =
            CliType.withFieldSetById
                (FieldId.named "A")
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                value

        CliType.CellPathsExactlyCovering 0 4 inconsistent |> shouldEqual []
        CliType.CellPathsExactlyCovering 0 8 inconsistent |> shouldEqual []

    [<Test>]
    let ``a non-value-type names nothing`` () : unit =
        CliType.CellPathsExactlyCovering 0 8 (CliType.ObjectRef None) |> shouldEqual []

    // ----------------------------------------------------------------------------------------
    // Reading and writing through a path
    // ----------------------------------------------------------------------------------------

    [<Test>]
    let ``getting and setting a depth-2 cell touches only that cell`` () : unit =
        let value = buffer ()
        let path = [ FieldId.named "_item[1]" ; FieldId.named "Tag" ]

        CliType.getCellAtPath path value
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0uy))

        let updated =
            CliType.withCellAtPathSet path (CliType.Numeric (CliNumericType.UInt8 9uy)) value

        CliType.getCellAtPath path updated
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 9uy))

        // The sibling slot, and the sibling field within the same slot, are untouched.
        CliType.getCellAtPath [ FieldId.named "_item" ] updated
        |> shouldEqual (CliType.getCellAtPath [ FieldId.named "_item" ] value)

        CliType.getCellAtPath [ FieldId.named "_item[1]" ; FieldId.named "Payload" ] updated
        |> shouldEqual (CliType.ObjectRef None)

    [<Test>]
    let ``an empty path is the value itself`` () : unit =
        let value = buffer ()
        CliType.getCellAtPath [] value |> shouldEqual value

        let replacement = elem ()
        CliType.withCellAtPathSet [] replacement value |> shouldEqual replacement

    // ----------------------------------------------------------------------------------------
    // Properties
    // ----------------------------------------------------------------------------------------

    /// A random nested value type, built only from alias-free sequential layout so that every cell
    /// has a well-defined extent.
    type private Shape =
        | Prim of width : int
        | Ref
        | Struct of Shape list

    let private shapeGen : Gen<Shape> =
        let leaf =
            Gen.oneof
                [
                    Gen.elements [ 1 ; 2 ; 4 ; 8 ] |> Gen.map Shape.Prim
                    Gen.constant Shape.Ref
                ]

        let rec go (depth : int) : Gen<Shape> =
            if depth <= 0 then
                leaf
            else
                Gen.frequency
                    [
                        2, leaf
                        1,
                        gen {
                            let! n = Gen.choose (1, 3)
                            let! children = Gen.listOfLength n (go (depth - 1))
                            return Shape.Struct children
                        }
                    ]

        go 3

    let rec private buildShape (path : string) (shape : Shape) : CliType * ConcreteTypeHandle =
        match shape with
        | Shape.Prim 1 -> CliType.Numeric (CliNumericType.UInt8 0uy), byteHandle
        | Shape.Prim 2 -> CliType.Numeric (CliNumericType.UInt16 0us), byteHandle
        | Shape.Prim 4 -> CliType.Numeric (CliNumericType.Int32 0), int32Handle
        | Shape.Prim 8 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)), int64Handle
        | Shape.Prim w -> failwith $"unexpected generated primitive width %d{w}"
        | Shape.Ref -> CliType.ObjectRef None, objectHandle
        | Shape.Struct children ->
            let fields =
                children
                |> List.mapi (fun i child ->
                    let name = $"%s{path}_f%d{i}"
                    let contents, handle = buildShape name child
                    cliField name contents None handle
                )

            ofFields fields, declaredHandle

    /// Independent recursive walk of the constructed value: every cell at every depth, with its
    /// absolute offset. This is the oracle — the resolver *searches* for a cell given a range, and
    /// this *enumerates* what is there, so agreement between them is a real check rather than the
    /// same code twice.
    let rec private enumerateCells
        (prefix : FieldId list)
        (baseOffset : int)
        (value : CliType)
        : (FieldId list * int * CliType) list
        =
        match value with
        | CliType.ValueType vt ->
            CliValueType.TryAllFields vt
            |> List.map CliConcreteField.ToCliField
            |> List.collect (fun f ->
                // Deliberately via the public `getFieldLayoutById` rather than the concrete field's
                // own offset: the resolver reads the latter, so going through a different accessor
                // keeps this an independent check rather than the same lookup twice.
                let off, _ = CliType.getFieldLayoutById f.Id value
                let path = prefix @ [ f.Id ]
                let abs = baseOffset + off
                (path, abs, f.Contents) :: enumerateCells path abs f.Contents
            )
        | _ -> []

    [<Test>]
    let ``every cell of an alias-free tree is named by its own extent`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])

            enumerateCells [] 0 value
            |> List.forall (fun (path, abs, contents) ->
                let size = CliType.SizeOf(contents).Size
                let found = CliType.CellPathsExactlyCovering abs size value

                // The cell must be among the answers, and every answer must genuinely be a cell of
                // that extent — so this pins both directions, not just "finds at least something".
                List.exists (fun (p, c) -> p = path && c = contents) found
                && found |> List.forall (fun (_, c) -> CliType.SizeOf(c).Size = size)
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// Answers are ordered outermost-first, i.e. by path length. A caller that takes the first
    /// compatible answer therefore gets the shallowest cell that will do, which is the one that
    /// disturbs least on write.
    [<Test>]
    let ``answers are ordered outermost first`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])

            enumerateCells [] 0 value
            |> List.forall (fun (_, abs, contents) ->
                let size = CliType.SizeOf(contents).Size

                CliType.CellPathsExactlyCovering abs size value
                |> List.map (fst >> List.length)
                |> fun lengths -> lengths = List.sort lengths
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// Every answer is a prefix-extension of the one before it: they are nested cells sharing the
    /// same bytes, not unrelated cells that happen to have the same extent.
    [<Test>]
    let ``answers form a single nesting chain`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])

            enumerateCells [] 0 value
            |> List.forall (fun (_, abs, contents) ->
                let size = CliType.SizeOf(contents).Size

                CliType.CellPathsExactlyCovering abs size value
                |> List.map fst
                |> List.pairwise
                |> List.forall (fun (shorter, longer) ->
                    List.length longer > List.length shorter
                    && List.truncate (List.length shorter) longer = shorter
                )
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// The load-bearing equivalence: wherever the storage *can* be rendered as bytes, naming a cell
    /// and reading its bytes must give the same answer as the byte path would. This is what makes
    /// the descent a faithful substitute for the byte view rather than a second, divergent way to
    /// read memory — and it is checkable on exactly the values the incumbent path already handles.
    [<Test>]
    let ``naming a cell agrees with the byte view wherever bytes are defined`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])

            match CliType.ByteAddressability value with
            | CliByteAddressability.Rejected _ -> true
            | CliByteAddressability.ByteAddressable ->

            enumerateCells [] 0 value
            |> List.forall (fun (_, abs, contents) ->
                let size = CliType.SizeOf(contents).Size

                match CliType.CellPathsExactlyCovering abs size value with
                | [] -> true
                | answers ->
                    answers
                    |> List.forall (fun (_, cell) ->
                        // Reading the cell's own bytes out of the whole value must reproduce the
                        // cell.
                        let viaBytes = CliType.ofBytesLike cell (CliType.BytesAt abs size value)
                        CliType.ToBytes viaBytes = CliType.ToBytes cell
                    )
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// The *leaves* of a value — the cells that hold data rather than further cells. These carry
    /// the observable content; an enclosing value type is bookkeeping around them.
    let private leavesOf (value : CliType) : (FieldId list * CliType) list =
        enumerateCells [] 0 value
        |> List.choose (fun (path, _, contents) ->
            match contents with
            | CliType.ValueType _ -> None
            | leaf -> Some (path, leaf)
        )

    /// Setting a cell to what it already holds leaves every leaf reading back as it did.
    ///
    /// Deliberately stated over leaves rather than over the whole value, for two reasons that both
    /// bite: `withFieldSetById` stamps a write timestamp on the field it touches, so even a
    /// semantically-null write yields a structurally different value; and writing a nested cell
    /// necessarily rebuilds every value that *contains* it, so ancestors differ by construction.
    /// Neither is observable to a guest. Over-reporting a change is safe for the write path — it
    /// costs one redundant store — so the law worth pinning is about content, not representation.
    [<Test>]
    let ``setting a cell to its current contents preserves every leaf`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let before = leavesOf value

            enumerateCells [] 0 value
            |> List.forall (fun (path, _, contents) ->
                CliType.getCellAtPath path value = contents
                && leavesOf (CliType.withCellAtPathSet path contents value) = before
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// Writing a cell changes that cell and leaves every other cell alone — the property the write
    /// path depends on when it elides a reinterpret onto a named cell.
    [<Test>]
    let ``writing a cell disturbs no other cell`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let cells = enumerateCells [] 0 value

            cells
            |> List.forall (fun (path, _, contents) ->
                // A value distinguishable from the zero the tree was built with.
                let replacement =
                    match contents with
                    | CliType.Numeric (CliNumericType.UInt8 _) -> Some (CliType.Numeric (CliNumericType.UInt8 3uy))
                    | CliType.Numeric (CliNumericType.UInt16 _) -> Some (CliType.Numeric (CliNumericType.UInt16 3us))
                    | CliType.Numeric (CliNumericType.Int32 _) -> Some (CliType.Numeric (CliNumericType.Int32 3))
                    | CliType.Numeric (CliNumericType.Int64 _) ->
                        Some (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 3L)))
                    | _ -> None

                match replacement with
                | None -> true
                | Some replacement ->

                let updated = CliType.withCellAtPathSet path replacement value

                CliType.getCellAtPath path updated = replacement
                && cells
                   |> List.forall (fun (otherPath, _, otherContents) ->
                       // Cells on the path to the written one legitimately change (they contain it);
                       // everything else must not.
                       let isAncestorOrSelf =
                           List.length otherPath <= List.length path
                           && List.truncate (List.length otherPath) path = otherPath

                       let isDescendant =
                           List.length otherPath > List.length path
                           && List.truncate (List.length path) otherPath = path

                       isAncestorOrSelf
                       || isDescendant
                       || CliType.getCellAtPath otherPath updated = otherContents
                   )
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)
