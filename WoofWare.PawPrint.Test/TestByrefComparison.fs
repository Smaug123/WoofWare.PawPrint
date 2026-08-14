namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ceqNormalised` is the single place three boundaries — `ceq` on byrefs,
/// `Unsafe.AreSame`, and pointer comparison in `NativeIntSource` — decide whether two
/// managed pointers name the same address. What it must never do is answer when it
/// cannot tell, so these tests pin both halves: the shapes it decides, and the several
/// distinct shapes it refuses.
///
/// The refusals have no end-to-end coverage by construction. Every guest that reaches one
/// is parked, and a parked guest is only ever run against real .NET — so nothing but this
/// fixture asserts that PawPrint refuses rather than guessing.
///
/// That makes the *decidable* half at least as important to pin, and harder: a rule that
/// refuses too much still passes every guest, because a guest that no longer runs simply
/// moves to the parked list. `Utf8LiteralSpanEquality.cs` is the end-to-end guard against
/// exactly that, and it exists because over-refusal did ship here once already.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByrefComparison =

    /// Parsed once for all tests; DumpedAssembly is immutable, so sharing it under
    /// ParallelScope.All is safe. Only needed to name a real type for `ReinterpretAs`.
    let private corelib =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

    let private concreteType
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ConcreteType<ConcreteTypeHandle>
        =
        ConcreteType.makeFromIdentity
            typeInfo.Identity
            typeInfo.Namespace
            typeInfo.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

    let private byteType : ConcreteType<ConcreteTypeHandle> =
        concreteType (Corelib.getBaseTypes corelib).Byte

    /// A second type, so that a test can vary the *view* a chain takes without varying the
    /// address it names.
    let private int32Type : ConcreteType<ConcreteTypeHandle> =
        concreteType (Corelib.getBaseTypes corelib).Int32

    let private thread = ThreadId 0
    let private frame = FrameId 0

    let private local (index : uint16) : ByrefRoot =
        ByrefRoot.LocalVariable (thread, frame, index)

    let private byref (root : ByrefRoot) (projs : ByrefProjection list) : NormalisedManagedPointerSource =
        ManagedPointerSource.Byref (root, projs)
        |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

    let private ceq (p1 : NormalisedManagedPointerSource) (p2 : NormalisedManagedPointerSource) : bool =
        ManagedPointerSource.ceqNormalised "test" p1 p2

    let private fieldX = ByrefProjection.Field (FieldId.Named "X")
    let private fieldY = ByrefProjection.Field (FieldId.Named "Y")

    [<Test>]
    let ``identical chains are equal`` () =
        ceq (byref (local 0us) [ fieldX ]) (byref (local 0us) [ fieldX ])
        |> shouldEqual true

    [<Test>]
    let ``distinct roots are unequal`` () =
        ceq (byref (local 0us) []) (byref (local 1us) []) |> shouldEqual false

    /// Even two *different* fields of one value are refused, which is not obvious: the
    /// tempting argument is that distinct fields occupy disjoint extents, so the divergence
    /// alone proves the addresses differ.
    ///
    /// That argument is false under explicit layout. `[FieldOffset(0)] int A;` and
    /// `[FieldOffset(0)] int B;` are distinct fields at one address, and such values stay
    /// field-backed rather than collapsing to a byte range — measured, by running
    /// `Unsafe.AreSame(ref u.A, ref u.B)` on both runtimes: real .NET says `true`, and this
    /// comparison used to say `false`. `AreSameExplicitLayoutOverlappingFields.cs` is the
    /// parked guest for it.
    ///
    /// So the field-offset table is the only thing that separates that from an ordinary
    /// sequential struct, and comparison does not carry it.
    [<Test>]
    let ``distinct fields of the same root are refused`` () =
        let exn =
            Assert.Throws (fun () -> ceq (byref (local 0us) [ fieldX ]) (byref (local 0us) [ fieldY ]) |> ignore)

        exn.Message |> shouldContainText "field offsets"

    /// Two fields of one heap object are *different roots*, but that is a fact about how
    /// each byref was built, not about where it points: `[StructLayout(LayoutKind.Explicit)]`
    /// on a class can put two fields on one address. Measured the same way as the struct
    /// case — real .NET says `true`, this comparison used to say `false`. Parked as
    /// `AreSameHeapFieldsOverlappingExplicitLayout.cs`.
    [<Test>]
    let ``two fields of one heap object are refused`` () =
        let obj = ManagedHeapAddress 1

        let field (name : string) =
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (obj, FieldId.Named name), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        let exn = Assert.Throws (fun () -> ceq (field "A") (field "B") |> ignore)

        exn.Message |> shouldContainText "one heap object"

    /// Fields of *different* objects are genuinely different storage, so those still decide.
    [<Test>]
    let ``fields of different heap objects are unequal`` () =
        let field (addr : int) (name : string) =
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (ManagedHeapAddress addr, FieldId.Named name), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        ceq (field 1 "A") (field 2 "B") |> shouldEqual false

    /// The same field of the same object is the same address, and must not be swept into
    /// the refusal by an over-eager same-object rule.
    [<Test>]
    let ``the same field of one heap object is equal`` () =
        let field () =
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (ManagedHeapAddress 1, FieldId.Named "A"), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        ceq (field ()) (field ()) |> shouldEqual true

    /// Distinct array elements are disjoint, so two *undisplaced* byrefs to different
    /// elements still decide — the refusals must not generalise to every same-container
    /// root pair.
    [<Test>]
    let ``distinct undisplaced elements of one array are unequal`` () =
        let element (index : int) =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 1, index), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        ceq (element 0) (element 1) |> shouldEqual false

    /// ... but once a projection displaces one of them, element disjointness stops settling
    /// anything: it can walk out of its own element and into the next. For
    /// `struct Pair { int X; int Y }`, a byte view of `a[0].Y` advanced 4 bytes *is* `a[1]`.
    /// Parked as `AreSameProjectionCrossesArrayElement.cs`.
    [<Test>]
    let ``a displaced element byref against another element is refused`` () =
        let displaced =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (ManagedHeapAddress 1, 0),
                [
                    fieldY
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 4
                ]
            )
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        let bare =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 1, 1), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        let exn = Assert.Throws (fun () -> ceq displaced bare |> ignore)

        exn.Message |> shouldContainText "root's extent"

    /// Two field-free cursors on one root are pure arithmetic: the addresses differ exactly
    /// when the displacements do. Both of these pin that arm, and each kills a different
    /// wrong implementation of it that the rest of the suite tolerates.
    ///
    /// This one kills `Some true` — answering "same root, no fields, therefore same
    /// address". Every other decidable test either has empty residuals, where that mutant
    /// happens to agree, or takes a different arm entirely.
    [<Test>]
    let ``unequal cursors on one root are unequal`` () =
        let at (n : int) =
            byref (local 0us) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset n ]

        ceq (at 1) (at 5) |> shouldEqual false

    /// And this one kills `Some (rest1 = rest2)` — deciding by structural equality of the
    /// residuals rather than by their arithmetic. The two chains reach one byte by different
    /// routes, differing only in a type view, which does not move the address.
    [<Test>]
    let ``equal cursors under different type views are equal`` () =
        let viaByte =
            byref (local 0us) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 4 ]

        let viaInt =
            byref (local 0us) [ ByrefProjection.ReinterpretAs int32Type ; ByrefProjection.ByteOffset 4 ]

        ceq viaByte viaInt |> shouldEqual true

    /// Selecting a field navigates *into* a value, so it cannot carry a byref out of the
    /// root it started from. `Unsafe.AreSame(ref left.X, ref right.X)` for two distinct
    /// local structs must therefore still decide — this is the ordinary comparison of one
    /// field across two values, and refusing it would be a plain regression.
    [<Test>]
    let ``the same field of two distinct roots is unequal`` () =
        ceq (byref (local 0us) [ fieldX ]) (byref (local 1us) [ fieldX ])
        |> shouldEqual false

    /// Likewise for different fields of distinct roots: neither byref has left its own
    /// local, so the roots settle it.
    [<Test>]
    let ``different fields of two distinct roots are unequal`` () =
        ceq (byref (local 0us) [ fieldX ]) (byref (local 1us) [ fieldY ])
        |> shouldEqual false

    /// A cursor with no field before it has been folded by `normaliseTrailingByteOffset`
    /// down to a residual within one element, so it cannot have reached the next element
    /// and the comparison still decides. Only a cursor that follows a `Field` is unbounded,
    /// because that folding cannot happen through a field.
    [<Test>]
    let ``a field-free in-cell cursor still decides against another element`` () =
        let inCell =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (ManagedHeapAddress 1, 0),
                [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 1 ]
            )
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        let other =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 1, 1), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        ceq inCell other |> shouldEqual false

    /// A local has no stride for `normaliseTrailingByteOffset` to fold against, so a cursor
    /// on a local root is unbounded even with no `Field` before it — unlike the array case
    /// above, where folding leaves a residual inside one element.
    ///
    /// This is the shape that was answering `false` while the predicate justified itself by
    /// a folding that had never happened for this root. ECMA-335 promises no relative address
    /// between two independently declared locals, so there is no fact to answer with.
    [<Test>]
    let ``an unbounded cursor on a local root is refused`` () =
        let displaced =
            byref (local 0us) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 1000 ]

        let exn = Assert.Throws (fun () -> ceq displaced (byref (local 1us) []) |> ignore)

        exn.Message |> shouldContainText "root's extent"

    /// A cursor on an *array element* root is bounded by folding, so it still decides where
    /// the identical projection list on a local root is refused. Paired with the test above,
    /// this is what pins the rule to the root kind rather than to the projection list alone.
    ///
    /// The residual is deliberately small. `unsafeAssumeNormalisedForComparison` does not
    /// validate it, so a residual of 1000 would be an input the real pipeline can never
    /// produce — `normaliseTrailingByteOffset` would have folded whole strides of it into
    /// the index — and asserting an answer on it would enshrine an impossible shape. Worse,
    /// the asserted answer would be *wrong* in the only world where the input were canonical:
    /// an element size above 1000 is what it would take, and then the address genuinely is
    /// `a[1]`.
    [<Test>]
    let ``the same cursor on a fold-eligible root still decides`` () =
        let displaced =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (ManagedHeapAddress 1, 0),
                [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 2 ]
            )
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        let other =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 1, 1), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        ceq displaced other |> shouldEqual false

    /// The displacement rule keys on displacement, not on merely having projections: a
    /// byref that only changes type view has not moved, so it still decides against a
    /// different root.
    [<Test>]
    let ``a reinterpreted-but-undisplaced byref still decides against another root`` () =
        let reinterpreted =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (ManagedHeapAddress 1, 0),
                [ ByrefProjection.ReinterpretAs byteType ]
            )
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        let other =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 1, 1), [])
            |> ManagedPointerSource.unsafeAssumeNormalisedForComparison

        ceq reinterpreted other |> shouldEqual false

    /// A chain differing only by a trailing `ReinterpretAs` is address-preserving, so it
    /// still decides — this is what makes `Unsafe.As` round-trips compare equal, and it is
    /// deliberately *not* what the field refusal catches.
    [<Test>]
    let ``a trailing reinterpret does not stop the comparison deciding`` () =
        ceq (byref (local 0us) [ ByrefProjection.ReinterpretAs byteType ]) (byref (local 0us) [])
        |> shouldEqual true

    /// The measured divergence: `ref a.X` against `ref a` reinterpreted. These are the same
    /// address exactly when `X` sits at offset 0, which comparison cannot see, so it must
    /// refuse rather than answer `false`.
    [<Test>]
    let ``a field run against a bare root is refused`` () =
        let exn =
            Assert.Throws (fun () -> ceq (byref (local 0us) [ fieldX ]) (byref (local 0us) []) |> ignore)

        exn.Message |> shouldContainText "field offsets"

    [<Test>]
    let ``the refusal is symmetric`` () =
        let exn =
            Assert.Throws (fun () -> ceq (byref (local 0us) []) (byref (local 0us) [ fieldX ]) |> ignore)

        exn.Message |> shouldContainText "field offsets"

    /// The prefix need not be empty: the extra run is what matters, not where it starts.
    [<Test>]
    let ``a deeper field run against a shorter shared prefix is refused`` () =
        let exn =
            Assert.Throws (fun () ->
                ceq (byref (local 0us) [ fieldX ; fieldY ]) (byref (local 0us) [ fieldX ])
                |> ignore
            )

        exn.Message |> shouldContainText "field offsets"

    /// A run that also advances the cursor by a strictly positive number of bytes IS
    /// decidable, and must not be swept into the refusal: a field offset is non-negative
    /// and `ReinterpretAs` preserves the address, so the run moves strictly forward
    /// whatever the layout turns out to be. This is the shape reached by comparing
    /// `Unsafe.As<S, byte>(ref s)` with
    /// `Unsafe.AddByteOffset(ref Unsafe.As<int, byte>(ref s.X), 1)`.
    [<Test>]
    let ``a field run carrying a positive byte offset still decides`` () =
        let extra =
            [
                fieldX
                ByrefProjection.ReinterpretAs byteType
                ByrefProjection.ByteOffset 1
            ]

        ceq (byref (local 0us) []) (byref (local 0us) extra) |> shouldEqual false

    /// The known parts are compared against each other, not merely against zero: a field
    /// walk displaced 5 bytes cannot land on the same byte as a bare 1-byte displacement,
    /// because the field offsets it also picks up can only push it further forward. The
    /// other residual being non-empty is irrelevant.
    [<Test>]
    let ``a field run outruns a smaller known displacement`` () =
        let withField =
            byref
                (local 0us)
                [
                    fieldX
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 5
                ]

        let bare =
            byref (local 0us) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 1 ]

        ceq withField bare |> shouldEqual false

    /// ... but when the field walk's known part does not already exceed the other's, the
    /// unknown field offsets could make up exactly the difference, so it is still refused.
    [<Test>]
    let ``a field run that does not outrun the other side is refused`` () =
        let withField =
            byref
                (local 0us)
                [
                    fieldX
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 1
                ]

        let bare =
            byref (local 0us) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 5 ]

        let exn = Assert.Throws (fun () -> ceq withField bare |> ignore)

        exn.Message |> shouldContainText "field offsets"

    /// A *negative* cursor is a different matter: it can cancel an unknown field offset
    /// exactly, so the pair is undecidable again and must still be refused.
    [<Test>]
    let ``a field run carrying a negative byte offset is refused`` () =
        let extra =
            [
                fieldX
                ByrefProjection.ReinterpretAs byteType
                ByrefProjection.ByteOffset -4
            ]

        let exn =
            Assert.Throws (fun () -> ceq (byref (local 0us) []) (byref (local 0us) extra) |> ignore)

        exn.Message |> shouldContainText "field offsets"

    /// Divergent field chains can alias too, once a cursor lets one run walk out of its
    /// own field: in a sequential `{ int X; int Y }`, `ref s.X` advanced by 4 bytes and
    /// `ref s.Y` are one address, though the chains diverge at the very first step. The
    /// refusal must therefore not be limited to prefix pairs.
    [<Test>]
    let ``divergent field chains joined by a cursor are refused`` () =
        let viaCursor =
            [
                fieldX
                ByrefProjection.ReinterpretAs byteType
                ByrefProjection.ByteOffset 4
            ]

        let exn =
            Assert.Throws (fun () -> ceq (byref (local 0us) viaCursor) (byref (local 0us) [ fieldY ]) |> ignore)

        exn.Message |> shouldContainText "field offsets"

    /// For any shared prefix and any non-empty run of extra fields, extending one side is
    /// undecidable — the extra fields may all sit at offset 0 and denote no bytes at all.
    /// Generated rather than enumerated, so the rule is pinned at every depth rather than
    /// at the two the examples above happen to use.
    [<Test>]
    let ``extending a chain by any run of fields is always refused`` () =
        let fieldNames =
            Gen.elements [ "A" ; "B" ; "C" ]
            |> Gen.map (FieldId.Named >> ByrefProjection.Field)

        let property =
            gen {
                let! prefix = Gen.listOfLength 3 fieldNames |> Gen.map (List.truncate 3)
                let! prefixLen = Gen.choose (0, 3)
                let! extra = Gen.nonEmptyListOf fieldNames
                return List.truncate prefixLen prefix, extra
            }
            |> Arb.fromGen
            |> Prop.forAll
            <| fun (prefix : ByrefProjection list, extra : ByrefProjection list) ->
                let shorter = byref (local 0us) prefix
                let longer = byref (local 0us) (prefix @ extra)

                // Specifically the field-offset refusal, not merely *some* exception: a
                // `MatchFailureException`, or a failure inside normalisation, would
                // otherwise count as the rule working correctly.
                let refused =
                    try
                        ceq shorter longer |> ignore
                        false
                    with e ->
                        e.Message.Contains "field offsets"

                refused

        Check.QuickThrowOnFailure property

    /// A PE byte range knows its own size, so a cursor on one is not the open question that
    /// a cursor on a local is: it either lands inside the range or it does not. This is the
    /// shape a `u8` literal takes — `Utf8LiteralSpanEquality.cs` is the end-to-end guest —
    /// and grouping it with the roots that carry no size was a measured regression, in which
    /// `"abc"u8.Slice(1) == "xy"u8` began failing outright where both runtimes had agreed
    /// on `false`.
    let private peRange (rva : int) (size : int) : ByrefRoot =
        ByrefRoot.PeByteRange
            {
                AssemblyFullName = "Test, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
                Source = PeByteRangePointerSource.ManagedResource $"r%d{rva}"
                RelativeVirtualAddress = rva
                Size = size
            }

    [<Test>]
    let ``a cursor inside a sized range still decides against another range`` () =
        let sliced =
            byref (peRange 16 4) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 1 ]

        ceq sliced (byref (peRange 32 3) []) |> shouldEqual false

    /// One past the end is the case that must still refuse: that address may well be the
    /// base of whatever the linker laid down next, so "inside my own range" is exactly the
    /// claim that fails here.
    [<Test>]
    let ``a cursor at the end of a sized range is refused`` () =
        let past =
            byref (peRange 16 4) [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 4 ]

        let exn = Assert.Throws (fun () -> ceq past (byref (peRange 32 3) []) |> ignore)

        exn.Message |> shouldContainText "root's extent"

    /// And a cursor applied after a `Field` is unplaceable whatever the root's size, because
    /// the field's own offset is the part that is unknown.
    [<Test>]
    let ``a cursor after a field on a sized range is refused`` () =
        let viaField =
            byref
                (peRange 16 4)
                [
                    fieldX
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 1
                ]

        let exn = Assert.Throws (fun () -> ceq viaField (byref (peRange 32 3) []) |> ignore)

        exn.Message |> shouldContainText "root's extent"
