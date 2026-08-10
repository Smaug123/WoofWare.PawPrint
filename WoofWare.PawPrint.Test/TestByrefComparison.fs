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

    /// Two *different* fields of one value occupy different offsets in any layout that can
    /// be field-addressed at all, so structural inequality is sound here and must not be
    /// swept into the refusal. Overlapping explicit layouts are stored byte-backed and so
    /// never carry `Field` projections in the first place.
    [<Test>]
    let ``distinct fields of the same root are unequal`` () =
        ceq (byref (local 0us) [ fieldX ]) (byref (local 0us) [ fieldY ])
        |> shouldEqual false

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

        exn.Message |> shouldContainText "field projections"
        exn.Message |> shouldContainText "offset 0"

    [<Test>]
    let ``the refusal is symmetric`` () =
        let exn =
            Assert.Throws (fun () -> ceq (byref (local 0us) []) (byref (local 0us) [ fieldX ]) |> ignore)

        exn.Message |> shouldContainText "field projections"

    /// The prefix need not be empty: the extra run is what matters, not where it starts.
    [<Test>]
    let ``a deeper field run against a shorter shared prefix is refused`` () =
        let exn =
            Assert.Throws (fun () ->
                ceq (byref (local 0us) [ fieldX ; fieldY ]) (byref (local 0us) [ fieldX ])
                |> ignore
            )

        exn.Message |> shouldContainText "field projections"

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

        exn.Message |> shouldContainText "field projections"

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
