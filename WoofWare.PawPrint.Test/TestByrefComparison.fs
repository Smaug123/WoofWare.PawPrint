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
/// cannot tell, so these tests pin both halves: the shapes it decides, and the one
/// shape it refuses.
///
/// The refusal has no end-to-end coverage by construction. The guest that reaches it,
/// `AreSameFirstFieldVersusReinterpretedWhole.cs`, is parked, and a parked guest is only
/// ever run against real .NET — so nothing but this fixture asserts that PawPrint refuses
/// rather than guessing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByrefComparison =

    /// Parsed once for all tests; DumpedAssembly is immutable, so sharing it under
    /// ParallelScope.All is safe. Only needed to name a real type for `ReinterpretAs`.
    let private byteType : ConcreteType<ConcreteTypeHandle> =
        let corelib =
            let _, loggerFactory = LoggerFactory.makeTest ()
            Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

        let typeInfo = (Corelib.getBaseTypes corelib).Byte

        ConcreteType.makeFromIdentity
            typeInfo.Identity
            typeInfo.Namespace
            typeInfo.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

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

                let threw =
                    try
                        ceq shorter longer |> ignore
                        false
                    with _ ->
                        true

                threw

        Check.QuickThrowOnFailure property
