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

    /// Two fields declared at the *same* offset are each the other's alias, so neither may be
    /// named. Distinct from the case above, where one field strictly contains the range.
    [<Test>]
    let ``a union of two same-width fields names nothing`` () : unit =
        let value =
            ofFieldsSized
                4
                [
                    cliField "AsInt" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle
                    cliField "AlsoInt" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle
                ]

        CliType.CellPathsExactlyCovering 0 4 value |> shouldEqual []

    /// A narrower field overlapping the start of a wider one: the wider field contains the range
    /// exactly, but the narrow sibling aliases part of it, so naming the wider one would leave the
    /// narrow one stale on write.
    [<Test>]
    let ``a narrower overlapping sibling blocks the field that covers the range`` () : unit =
        let value =
            ofFieldsSized
                4
                [
                    cliField "AsInt" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle
                    cliField "Byte0" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 0) byteHandle
                ]

        CliType.CellPathsExactlyCovering 0 4 value |> shouldEqual []

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

    // ----------------------------------------------------------------------------------------
    // `CandidateCellExtentsContainingByte`
    // ----------------------------------------------------------------------------------------
    //
    // The subordinate half of the pair. `CellAwareMemOps` steps a copy cursor through a byte range
    // and must decide how wide the next move should be *before* it can ask whether that width names
    // a cell; this generator proposes the widths and `CellPathsExactlyCovering` disposes of them.
    // Because the validator has the last word, the only thing that can actually go wrong here is
    // under-reporting — a width the validator would have accepted but which is never offered to it,
    // which silently costs the caller a route it should have had. That is what the completeness
    // property below is for, and it is stated against a brute-force enumeration of every width
    // rather than against a second hand-written walk.

    [<Test>]
    let ``candidate extents are reported outermost first and rebased through nesting`` () : unit =
        let value = buffer ()

        // Byte 8 is `Tag` inside slot 0: the whole buffer, then the slot, then the byte itself.
        CliType.CandidateCellExtentsContainingByte 8 value
        |> shouldEqual [ 0, 32 ; 0, 16 ; 8, 1 ]

        // Byte 16 opens slot 1, whose own `Payload` starts there too — so both are rebased by 16.
        CliType.CandidateCellExtentsContainingByte 16 value
        |> shouldEqual [ 0, 32 ; 16, 16 ; 16, 8 ]

    [<Test>]
    let ``a byte in padding stops the descent at the enclosing cell`` () : unit =
        let value = buffer ()

        // `Elem` is `{ Box Payload@0; byte Tag@8 }` padded to 16, so bytes 9..15 belong to no
        // field. The slot is still a cell and is still reported; there is nothing below it.
        CliType.CandidateCellExtentsContainingByte 9 value
        |> shouldEqual [ 0, 32 ; 0, 16 ]

    [<Test>]
    let ``a byte outside the value proposes nothing`` () : unit =
        let value = buffer ()
        CliType.CandidateCellExtentsContainingByte -1 value |> shouldEqual []
        CliType.CandidateCellExtentsContainingByte 32 value |> shouldEqual []

    [<Test>]
    let ``overlapping fields stop the descent`` () : unit =
        // Explicit layout, two fields sharing byte 0: there is no single field to descend into.
        let value =
            ofFieldsSized
                8
                [
                    cliField "a" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle
                    cliField "b" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle
                ]

        CliType.CandidateCellExtentsContainingByte 0 value |> shouldEqual [ 0, 8 ]

    /// Every extent proposed genuinely lies within the value and contains the byte asked about.
    /// Weak on its own — the validator would catch a violation — but it keeps the generator from
    /// drifting into nonsense that happens to be harmless.
    [<Test>]
    let ``every proposed extent contains the byte it was asked about`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size

            [ 0 .. size - 1 ]
            |> List.forall (fun byteOffset ->
                CliType.CandidateCellExtentsContainingByte byteOffset value
                |> List.forall (fun (offset, width) ->
                    offset >= 0
                    && width > 0
                    && offset + width <= size
                    && offset <= byteOffset
                    && byteOffset < offset + width
                )
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// The property the design rests on: **the generator is complete**. `CellAwareMemOps` proposes
    /// widths from one endpoint only, so a width the validator would have accepted but which was
    /// never proposed is a move silently lost.
    ///
    /// The oracle is brute force — every width from 1 to the value's size, asked of
    /// `CellPathsExactlyCovering` directly — which is the implementation the caller would have if
    /// it did not have this generator. Both anchorings are checked, since the copy loop runs
    /// forwards (the cursor is the move's first byte) and backwards (its last).
    [<Test>]
    let ``every width the validator accepts at an anchor is proposed`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size

            // Mirrors `CellAwareMemOps.namedCells`: the validator reports *fields*, so the range
            // being the whole value is the caller's own base case rather than something it returns.
            let namesACell (start : int) (width : int) : bool =
                (start = 0 && width = size)
                || not (List.isEmpty (CliType.CellPathsExactlyCovering start width value))

            [ 0 .. size - 1 ]
            |> List.forall (fun byteOffset ->
                [ false ; true ]
                |> List.forall (fun backwards ->
                    let proposed =
                        CliType.CandidateCellExtentsContainingByte byteOffset value
                        |> List.filter (fun (offset, width) ->
                            if backwards then
                                offset + width = byteOffset + 1
                            else
                                offset = byteOffset
                        )
                        |> List.map snd
                        |> Set.ofList

                    let accepted =
                        [ 1..size ]
                        |> List.filter (fun width ->
                            let start = if backwards then byteOffset - width + 1 else byteOffset

                            start >= 0 && start + width <= size && namesACell start width
                        )
                        |> Set.ofList

                    Set.isSubset accepted proposed
                )
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    // ----------------------------------------------------------------------------------------
    // Padding runs
    // ----------------------------------------------------------------------------------------

    // The complement of cell naming. `CellPathsExactlyCovering` names the bytes that *are* a cell;
    // `TryPaddingRunAt` names the bytes that are no cell at all, which alignment filler between
    // fields and trailing filler at the end of a struct both are. A bulk move whose range starts or
    // ends inside a reference-containing struct puts its cursor on exactly those bytes, and there
    // neither route works: no cell begins there, and the struct around them has no byte image, so
    // `BytesAt` refuses the whole of it.
    //
    // The oracle below is deliberately built the other way round from the implementation. The
    // implementation *descends*, stepping through the single field containing the byte;
    // `enumerateCells` *enumerates* what is there. A byte covered by a leaf cell — a primitive or a
    // reference, the things that hold content — is data, and every other byte is padding. Agreement
    // between a search and an enumeration is a real check rather than the same code twice.

    /// Every byte covered by a leaf cell, i.e. every byte that holds content rather than filler.
    let private oracleDataBytes (value : CliType) : Set<int> =
        enumerateCells [] 0 value
        |> List.collect (fun (_, abs, contents) ->
            match contents with
            | CliType.ValueType _ -> []
            | leaf -> [ abs .. abs + CliType.SizeOf(leaf).Size - 1 ]
        )
        |> Set.ofList

    /// The deepest value type containing the byte — the struct whose filler this byte is. `None`
    /// for a byte that only the whole value contains. A run cannot cross from one owner to another,
    /// because the two live in different values' preserved byte images.
    let private oracleOwner (value : CliType) (b : int) : FieldId list option =
        enumerateCells [] 0 value
        |> List.filter (fun (_, abs, contents) ->
            match contents with
            | CliType.ValueType _ -> abs <= b && b < abs + CliType.SizeOf(contents).Size
            | _ -> false
        )
        |> List.sortByDescending (fun (path, _, _) -> List.length path)
        |> List.tryHead
        |> Option.map (fun (path, _, _) -> path)

    let private oracleRunAt (value : CliType) (b : int) : (int * int) option =
        let size = CliType.SizeOf(value).Size
        let data = oracleDataBytes value

        if b < 0 || b >= size || data.Contains b then
            None
        else

        let owner = oracleOwner value b

        let inRun (b' : int) : bool =
            b' >= 0 && b' < size && not (data.Contains b') && oracleOwner value b' = owner

        let mutable start = b

        while inRun (start - 1) do
            start <- start - 1

        let mutable endExclusive = b + 1

        while inRun endExclusive do
            endExclusive <- endExclusive + 1

        Some (start, endExclusive - start)

    /// Padding and data partition the value: every byte is one or the other, never both and never
    /// neither. This is what makes the padding step and the cell step jointly total over a struct's
    /// bytes, which is what the copy driver needs of them.
    [<Test>]
    let ``padding is exactly the bytes no leaf cell covers`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size
            let data = oracleDataBytes value

            [ 0 .. size - 1 ]
            |> List.forall (fun b -> (CliType.TryPaddingRunAt b value |> Option.isSome) = not (data.Contains b))

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// Each run is the maximal stretch of padding around the byte that belongs to the same struct.
    /// Maximality is not needed for correctness — a caller taking one byte at a time would still
    /// move the right bytes — but a run that were merely *some* padding around the byte could
    /// silently have the wrong extent, so pinning it exactly is what makes the accessors'
    /// preconditions mean anything.
    [<Test>]
    let ``a padding run is the maximal stretch of its owner's filler`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size

            [ 0 .. size - 1 ]
            |> List.forall (fun b -> CliType.TryPaddingRunAt b value = oracleRunAt value b)

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// Every byte of a run reports that same run, so a cursor anywhere inside it sees one answer.
    [<Test>]
    let ``every byte of a run reports the same run`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size

            [ 0 .. size - 1 ]
            |> List.forall (fun b ->
                match CliType.TryPaddingRunAt b value with
                | None -> true
                | Some (start, length) ->
                    [ start .. start + length - 1 ]
                    |> List.forall (fun b' -> CliType.TryPaddingRunAt b' value = Some (start, length))
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    let private paddingPayload (length : int) : byte[] =
        Array.init length (fun i -> byte ((0xA5 + i) % 256))

    /// Writing padding round-trips, and touches nothing that holds content. The second half is the
    /// load-bearing one: the copy driver writes the *whole* enclosing cell back afterwards, so a
    /// padding write that disturbed a field would corrupt live data rather than filler.
    [<Test>]
    let ``writing a padding run round-trips and disturbs no leaf`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size
            let before = leavesOf value

            [ 0 .. size - 1 ]
            |> List.forall (fun b ->
                match CliType.TryPaddingRunAt b value with
                | None -> true
                | Some (start, length) ->
                    let payload = paddingPayload length

                    let updated =
                        CliType.WithPaddingBytesAtIfChanged start payload value
                        |> Option.defaultValue value

                    CliType.PaddingBytesAt start length updated = payload
                    && leavesOf updated = before
                    && CliType.TryPaddingRunAt b updated = Some (start, length)
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// The outside oracle. Wherever the value *can* be rendered as bytes, the structural padding
    /// accessors and the byte path must be the same operation — otherwise the copy driver would
    /// have two ways to move the same bytes that disagree, with only one of them tested. Checkable
    /// on exactly the values the incumbent byte path already handles, which is why the shapes
    /// carrying a reference are skipped rather than worked around.
    [<Test>]
    let ``padding access agrees with the byte path wherever bytes are defined`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])

            match CliType.ByteAddressability value with
            | CliByteAddressability.Rejected _ -> true
            | CliByteAddressability.ByteAddressable ->

            let size = CliType.SizeOf(value).Size

            [ 0 .. size - 1 ]
            |> List.forall (fun b ->
                match CliType.TryPaddingRunAt b value with
                | None -> true
                | Some (start, length) ->
                    let payload = paddingPayload length

                    let viaPadding =
                        CliType.WithPaddingBytesAtIfChanged start payload value
                        |> Option.defaultValue value

                    let viaBytes =
                        CliType.WithBytesAtIfChanged start payload value |> Option.defaultValue value

                    CliType.PaddingBytesAt start length value = CliType.BytesAt start length value
                    && CliType.ToBytes viaPadding = CliType.ToBytes viaBytes
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    /// Rewriting a run with what it already holds reports no change, so the copy driver's
    /// "`None` means nothing to store" contract does not quietly rewrite storage on every step.
    [<Test>]
    let ``rewriting a padding run with its current bytes reports no change`` () : unit =
        let property (shape : Shape) : bool =
            let value, _ = buildShape "r" (Shape.Struct [ shape ])
            let size = CliType.SizeOf(value).Size

            [ 0 .. size - 1 ]
            |> List.forall (fun b ->
                match CliType.TryPaddingRunAt b value with
                | None -> true
                | Some (start, length) ->
                    let current = CliType.PaddingBytesAt start length value
                    CliType.WithPaddingBytesAtIfChanged start current value = None
            )

        Check.One (config, Prop.forAll (Arb.fromGen shapeGen) property)

    // ----------------------------------------------------------------------------------------
    // Padding: shapes the generator cannot reach
    //
    // `shapeGen` builds only alias-free sequential layout whose fields exactly fill their recorded
    // extents, so the three ways `TryPaddingRunAt` declines other than "the byte holds content"
    // have no generated coverage at all. Each is reachable through the public API, and each is
    // pinned by hand below.
    // ----------------------------------------------------------------------------------------

    /// Explicit layout can put two fields over one byte. There is then no single field to descend
    /// through, so the byte cannot be classified — the same refusal `CellPathsExactlyCovering`
    /// makes for an aliased range. Crucially it must not be *mistaken* for padding: those bytes
    /// hold two fields' worth of live content.
    [<Test>]
    let ``an aliased byte is not padding`` () : unit =
        let value =
            ofFieldsSized
                16
                [
                    cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle
                    cliField
                        "Alias"
                        (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                        (Some 0)
                        int64Handle
                    cliField "Tail" (CliType.Numeric (CliNumericType.Int32 0)) (Some 8) int32Handle
                ]

        // Bytes 0..7 are covered by two fields at once.
        for b in 0..7 do
            CliType.TryPaddingRunAt b value |> shouldEqual None

        // Bytes 8..11 hold `Tail`.
        for b in 8..11 do
            CliType.TryPaddingRunAt b value |> shouldEqual None

        // Bytes 12..15 are covered by nothing, so they are filler.
        for b in 12..15 do
            CliType.TryPaddingRunAt b value |> shouldEqual (Some (12, 4))

    /// Explicit layout can also place a reference at an offset real CoreCLR would reject at type
    /// load. PawPrint takes `[FieldOffset(n)]` verbatim, so the shape exists here and the padding
    /// rule has to cope with it rather than assume pointer alignment.
    [<Test>]
    let ``filler either side of a misaligned reference is padding`` () : unit =
        let value =
            ofFieldsSized 16 [ cliField "Obj" (CliType.ObjectRef None) (Some 3) objectHandle ]

        for b in 0..2 do
            CliType.TryPaddingRunAt b value |> shouldEqual (Some (0, 3))

        for b in 3..10 do
            CliType.TryPaddingRunAt b value |> shouldEqual None

        for b in 11..15 do
            CliType.TryPaddingRunAt b value |> shouldEqual (Some (11, 5))

        // The runs are readable and writable even though the value as a whole has no byte image,
        // which is the entire reason the primitive exists.
        match CliType.ByteAddressability value with
        | CliByteAddressability.ByteAddressable -> failwith "expected a reference-containing value to be rejected"
        | CliByteAddressability.Rejected _ -> ()

        let updated =
            CliType.WithPaddingBytesAtIfChanged 11 [| 1uy ; 2uy ; 3uy ; 4uy ; 5uy |] value
            |> Option.get

        CliType.PaddingBytesAt 11 5 updated
        |> shouldEqual [| 1uy ; 2uy ; 3uy ; 4uy ; 5uy |]

        CliType.getCellAtPath [ FieldId.named "Obj" ] updated
        |> shouldEqual (CliType.ObjectRef None)

    /// A nested field's filler is only *its* filler where nothing else claims those bytes. Explicit
    /// layout can put a sibling over part of the nested field's extent, and `ToBytes` awards the
    /// overlap to whichever field wrote last — so a run reported across the sibling would have a
    /// caller read and write the nested field's preserved image for bytes the sibling owns. A bulk
    /// copy would then leave the destination sibling untouched while believing it had moved them.
    ///
    /// Found by review with a differential repro, not by the properties above: `shapeGen` builds
    /// only alias-free layout, so it cannot pose the question at all.
    [<Test>]
    let ``a nested run stops where an overlapping sibling begins`` () : unit =
        // `Inner` is 16 bytes with filler at [1, 8): `Tag` at 0, `V` needing 8-byte alignment.
        let inner =
            ofFields
                [
                    cliField "Tag" (CliType.Numeric (CliNumericType.UInt8 0uy)) None byteHandle
                    cliField "V" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
                ]

        CliType.SizeOf(inner).Size |> shouldEqual 16
        CliType.TryPaddingRunAt 1 inner |> shouldEqual (Some (1, 7))

        // `Alias` sits over [4, 8) of the same bytes `Inner`'s filler occupies.
        let value =
            ofFieldsSized
                16
                [
                    cliField "A" inner (Some 0) declaredHandle
                    cliField "Alias" (CliType.Numeric (CliNumericType.Int32 0)) (Some 4) int32Handle
                ]

        // Bytes [1, 4) really are `A`'s filler and nothing else's.
        for b in 1..3 do
            CliType.TryPaddingRunAt b value |> shouldEqual (Some (1, 3))

        // Bytes [4, 8) belong to `Alias`, so they are not filler at all — two fields contain them,
        // and the byte path is what serves those.
        for b in 4..7 do
            CliType.TryPaddingRunAt b value |> shouldEqual None

    /// `withFieldSetById` replaces a field's `Contents` without recomputing its recorded `Size`, so
    /// a field can be left claiming an extent that disagrees with what it now holds. Descending
    /// through such a field is what the guard in `TryDescendableFieldAt` refuses, and this is why
    /// it has to: the padding run comes back in the *contents'* coordinates, so a field whose
    /// contents are larger than its extent can report a run that runs off the end of the field and
    /// across a sibling. A caller would then write the sibling's bytes believing they were filler.
    ///
    /// Reached through the public API rather than asserted unreachable, because that is the only
    /// way the state arises — and mutation-testing the guard showed nothing else covers it.
    [<Test>]
    let ``a field whose extent disagrees with its contents yields no padding run`` () : unit =
        // Two 4-byte fields exactly filling 8 bytes, so there is no padding to begin with.
        let value =
            ofFields
                [
                    cliField "A" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                    cliField "B" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                ]

        CliType.SizeOf(value).Size |> shouldEqual 8

        for b in 0..7 do
            CliType.TryPaddingRunAt b value |> shouldEqual None

        // A 16-byte struct whose own filler is bytes [1, 8): far wider than the 4-byte extent the
        // field it is about to be dropped into records.
        let oversized =
            ofFields
                [
                    cliField "Tag" (CliType.Numeric (CliNumericType.UInt8 0uy)) None byteHandle
                    cliField "Wide" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
                ]

        CliType.SizeOf(oversized).Size |> shouldEqual 16
        CliType.TryPaddingRunAt 1 oversized |> shouldEqual (Some (1, 7))

        // A now claims four bytes but holds sixteen. Descending into it would report the run
        // `(1, 7)` in the *outer* value's coordinates, which covers bytes [4, 8) — all of B.
        let inconsistent = CliType.withFieldSetById (FieldId.named "A") oversized value

        CliType.SizeOf(inconsistent).Size |> shouldEqual 8

        for b in 0..7 do
            CliType.TryPaddingRunAt b inconsistent |> shouldEqual None

        // The other direction — contents narrower than the recorded extent — is refused too. There
        // the value is unrenderable rather than mis-attributed: `ToBytes` overlays the field's own
        // image across its full `Size` and runs off the end of it.
        let narrowed =
            CliType.withFieldSetById (FieldId.named "A") (CliType.Numeric (CliNumericType.UInt8 0uy)) value

        for b in 0..7 do
            CliType.TryPaddingRunAt b narrowed |> shouldEqual None

    /// A raw-bytes value type is all bytes and no fields, so there is nothing for filler to sit
    /// between; the ordinary byte path already serves every offset of it.
    [<Test>]
    let ``a raw-bytes value type has no padding`` () : unit =
        let value = ofFieldsSized 8 []

        for b in 0..7 do
            CliType.TryPaddingRunAt b value |> shouldEqual None

    [<Test>]
    let ``a non-value-type has no padding`` () : unit =
        CliType.TryPaddingRunAt 0 (CliType.ObjectRef None) |> shouldEqual None

        CliType.TryPaddingRunAt 0 (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual None

        CliType.TryPaddingRunAt 0 (CliType.Bool 0uy) |> shouldEqual None

    [<Test>]
    let ``a byte outside the value is not padding`` () : unit =
        let value =
            ofFieldsSized 16 [ cliField "Obj" (CliType.ObjectRef None) (Some 3) objectHandle ]

        CliType.TryPaddingRunAt -1 value |> shouldEqual None
        CliType.TryPaddingRunAt 16 value |> shouldEqual None

    /// The accessors are partial in the same way, and say so rather than answering about bytes the
    /// caller never established were filler.
    [<Test>]
    let ``the padding accessors refuse a range that is not wholly one run`` () : unit =
        let value =
            ofFieldsSized 16 [ cliField "Obj" (CliType.ObjectRef None) (Some 3) objectHandle ]

        // Offset 3 holds the reference, not filler.
        let notPadding =
            Assert.Throws<exn> (fun () -> CliType.PaddingBytesAt 3 1 value |> ignore<byte[]>)

        notPadding.Message |> shouldContainText "is not padding"

        // The run at 0 is only three bytes long.
        let overrun =
            Assert.Throws<exn> (fun () -> CliType.PaddingBytesAt 0 4 value |> ignore<byte[]>)

        overrun.Message |> shouldContainText "leaves the padding run [0, 3)"

        let overrunWrite =
            Assert.Throws<exn> (fun () ->
                CliType.WithPaddingBytesAtIfChanged 0 [| 0uy ; 0uy ; 0uy ; 0uy |] value
                |> ignore<CliType option>
            )

        overrunWrite.Message |> shouldContainText "leaves the padding run [0, 3)"
