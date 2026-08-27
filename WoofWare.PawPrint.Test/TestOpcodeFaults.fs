namespace WoofWare.PawPrint.Test

open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the `OpcodeFaults` table.
///
/// Totality of each per-opcode match is enforced by the compiler, so these tests are aimed at the
/// two things it cannot see: that the escape hatch stays closed, and that entries mean what their
/// names say.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpcodeFaults =

    /// Enumerate a discriminated union's nullary cases. Every case of `NullaryIlOp` is nullary, so
    /// this is exhaustive for it; `UnaryMetadataTokenIlOp` and `UnaryStringTokenIlOp` are the same.
    let private allCasesOf<'a> () : 'a list =
        FSharpType.GetUnionCases typeof<'a>
        |> Array.toList
        |> List.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> 'a)

    let private nameOf (x : 'a) : string =
        let case, _ = FSharpValue.GetUnionFields (x, typeof<'a>)
        case.Name

    // ---------- The escape hatch stays closed ----------

    /// `Unmodelled` means "may raise anything", so every entry that carries it costs an analysis
    /// all precision at that instruction, and an entry added to it silently is a table that
    /// quietly stopped saying anything. There is exactly one, and it is `rethrow`, whose answer
    /// genuinely is not a fact about the opcode.
    ///
    /// Written as an equality against the whole set rather than as a per-case assertion, so that
    /// *adding* an unmodelled entry fails here as loudly as removing one — which is the direction
    /// that matters, since the way to make an awkward opcode compile is to reach for `Unmodelled`.
    [<Test>]
    let ``exactly one nullary opcode is unmodelled`` () : unit =
        allCasesOf<NullaryIlOp> ()
        |> List.filter (fun op -> OpcodeFaults.ofNullary op = OpcodeFaults.Unmodelled)
        |> List.map nameOf
        |> shouldEqual [ "Rethrow" ]

    [<Test>]
    let ``no unary-metadata opcode is unmodelled`` () : unit =
        allCasesOf<UnaryMetadataTokenIlOp> ()
        |> List.filter (fun op -> OpcodeFaults.ofUnaryMetadata op = OpcodeFaults.Unmodelled)
        |> List.map nameOf
        |> shouldEqual []

    [<Test>]
    let ``no string-token opcode is unmodelled`` () : unit =
        allCasesOf<UnaryStringTokenIlOp> ()
        |> List.filter (fun op -> OpcodeFaults.ofUnaryStringToken op = OpcodeFaults.Unmodelled)
        |> List.map nameOf
        |> shouldEqual []

    /// A duplicate entry would make `Raises` a bag where the type says it is a set, and would make
    /// two tables that mean the same thing compare unequal.
    [<Test>]
    let ``no nullary entry lists a fault twice`` () : unit =
        for op in allCasesOf<NullaryIlOp> () do
            match OpcodeFaults.ofNullary op with
            | OpcodeFaults.Unmodelled -> ()
            | OpcodeFaults.Raises xs -> xs |> List.distinct |> shouldEqual xs

    [<Test>]
    let ``no unary-metadata entry lists a fault twice`` () : unit =
        for op in allCasesOf<UnaryMetadataTokenIlOp> () do
            match OpcodeFaults.ofUnaryMetadata op with
            | OpcodeFaults.Unmodelled -> ()
            | OpcodeFaults.Raises xs -> xs |> List.distinct |> shouldEqual xs

    // ---------- `mayRaise` ----------

    /// The safety property the whole table rests on: an unclassified instruction must never be
    /// readable as harmless, whichever fault is asked about.
    [<Test>]
    let ``Unmodelled may raise every fault`` () : unit =
        for fault in allCasesOf<OpcodeFault> () do
            OpcodeFaults.mayRaise fault OpcodeFaults.Unmodelled |> shouldEqual true

    [<Test>]
    let ``an empty Raises may raise nothing`` () : unit =
        for fault in allCasesOf<OpcodeFault> () do
            OpcodeFaults.mayRaise fault (OpcodeFaults.Raises []) |> shouldEqual false

    [<Test>]
    let ``mayRaise finds a listed fault and not an unlisted one`` () : unit =
        let faults = OpcodeFaults.ofNullary NullaryIlOp.LdLen
        OpcodeFaults.mayRaise OpcodeFault.NullReference faults |> shouldEqual true
        OpcodeFaults.mayRaise OpcodeFault.IndexOutOfRange faults |> shouldEqual false

    // ---------- Entries mean what their names say ----------

    /// The signed forms have an unrepresentable quotient at MinValue / -1; the unsigned forms do
    /// not. Asserted as a pair, because the two differing in exactly one fault is the whole claim
    /// and testing either alone would not show it.
    [<Test>]
    let ``signed division overflows where unsigned division does not`` () : unit =
        OpcodeFaults.ofNullary NullaryIlOp.Div
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ; OpcodeFault.Overflow ])

        OpcodeFaults.ofNullary NullaryIlOp.Div_un
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ])

        OpcodeFaults.ofNullary NullaryIlOp.Rem
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ; OpcodeFault.Overflow ])

        OpcodeFaults.ofNullary NullaryIlOp.Rem_un
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ])

    /// Every checked conversion can overflow, and no unchecked one can. The two families are named
    /// alike and are easy to move between by accident.
    [<Test>]
    let ``checked conversions overflow and unchecked ones do not`` () : unit =
        let overflows, rest =
            allCasesOf<NullaryIlOp> ()
            |> List.filter (fun op -> (nameOf op).StartsWith ("Conv_", System.StringComparison.Ordinal))
            |> List.partition (fun op -> (nameOf op).StartsWith ("Conv_ovf", System.StringComparison.Ordinal))

        overflows |> List.isEmpty |> shouldEqual false
        rest |> List.isEmpty |> shouldEqual false

        for op in overflows do
            OpcodeFaults.ofNullary op
            |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.Overflow ])

        for op in rest do
            OpcodeFaults.ofNullary op |> shouldEqual (OpcodeFaults.Raises [])

    /// A store into an array takes the covariance check that a load does not, and `ldelema` takes
    /// it too: it hands out a writable address, so letting it through would defeat the check
    /// `stelem` makes.
    [<Test>]
    let ``array stores take the covariance check and loads do not`` () : unit =
        OpcodeFaults.ofNullary NullaryIlOp.Ldelem_ref
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual false

        OpcodeFaults.ofNullary NullaryIlOp.Stelem_ref
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual true

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Ldelem
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual false

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Ldelema
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual true

    /// A static-field access can surface a failed `.cctor`; an instance-field access cannot,
    /// having no `.cctor` to run.
    [<Test>]
    let ``static field access can surface a failed cctor and instance access cannot`` () : unit =
        for op in
            [
                UnaryMetadataTokenIlOp.Ldsfld
                UnaryMetadataTokenIlOp.Ldsflda
                UnaryMetadataTokenIlOp.Stsfld
            ] do
            OpcodeFaults.ofUnaryMetadata op
            |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.TypeInitialization ])

        for op in
            [
                UnaryMetadataTokenIlOp.Ldfld
                UnaryMetadataTokenIlOp.Ldflda
                UnaryMetadataTokenIlOp.Stfld
            ] do
            OpcodeFaults.ofUnaryMetadata op
            |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.NullReference ])

    /// The control-transfer instructions fault on nothing of their own beyond a null receiver:
    /// what the target raises is the call graph's business, not this table's. `callvirt` has the
    /// receiver and so is the one that differs.
    [<Test>]
    let ``control transfers carry only their own faults`` () : unit =
        for op in
            [
                UnaryMetadataTokenIlOp.Call
                UnaryMetadataTokenIlOp.Calli
                UnaryMetadataTokenIlOp.Jmp
            ] do
            OpcodeFaults.ofUnaryMetadata op |> shouldEqual (OpcodeFaults.Raises [])

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Callvirt
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.NullReference ])

    /// `ofIlOp` must agree with the per-shape functions rather than being a second opinion.
    [<Test>]
    let ``ofIlOp agrees with the per-shape tables`` () : unit =
        for op in allCasesOf<NullaryIlOp> () do
            OpcodeFaults.ofIlOp (IlOp.Nullary op) |> shouldEqual (OpcodeFaults.ofNullary op)

        let dummy =
            SourcedMetadataToken.ofInt (System.Reflection.AssemblyName "dummy-for-opcode-fault-tests") 0x02000001

        for op in allCasesOf<UnaryMetadataTokenIlOp> () do
            OpcodeFaults.ofIlOp (IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata dummy))
            |> shouldEqual (OpcodeFaults.ofUnaryMetadata op)

    /// `switch` falls through when the index is out of range rather than faulting, which is the
    /// one thing about it a reader is likely to get wrong.
    [<Test>]
    let ``switch does not fault on an out-of-range index`` () : unit =
        OpcodeFaults.ofIlOp (IlOp.Switch (System.Collections.Immutable.ImmutableArray.Create<int32> 0))
        |> shouldEqual (OpcodeFaults.Raises [])
