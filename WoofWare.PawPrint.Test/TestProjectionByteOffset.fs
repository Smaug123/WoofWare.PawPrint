namespace WoofWare.PawPrint.Test

open System
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `IlMachineManagedByref.walkProjectionByteOffset` folds a `ByrefProjection` chain to a byte
/// offset. `Field` consults the current type cursor; `ReinterpretAs` re-anchors it; `ByteOffset`
/// advances the offset and leaves the cursor alone.
///
/// A `Field` *after* a `ByteOffset` means exactly what
/// `ldflda` on a `ref T` sitting `n` bytes along means in the real runtime: `base + n +
/// offsetof(field, T)`, with no check on `n`. The cursor is still `T`-typed because
/// `ManagedPointerSource.appendProjection` only ever appends a `ByteOffset` to a chain already
/// ending in a `ReinterpretAs` — so the byte cursor always qualifies a reinterpret, and the
/// reinterpret target is the anchor a following `Field` resolves against.
///
/// The violation is the mirror image: a `ByteOffset` hung off a `Field` navigation, with no
/// reinterpret to say what type the raw bytes are being viewed as. `appendProjection` refuses to
/// construct that, and the walk refuses to fold it.
[<TestFixture>]
module TestProjectionByteOffset =

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

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Byte

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Object

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int32

    /// Some `ConcreteType<ConcreteTypeHandle>` to hang a `ReinterpretAs` on. Which type it names is
    /// irrelevant to these tests: `templateFor` is a parameter of the function under test, so the
    /// tests supply the template the reinterpret resolves to directly.
    let private someConcreteType : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup int32Handle allCt |> Option.get

    let private cliField (name : string) (contents : CliType) (fieldType : ConcreteTypeHandle) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = None
            Type = fieldType
            MarshallingDescriptor = None
        }

    let private ofFields (fields : CliField list) : CliType =
        SynthesisedLayoutKind.ofFields bct allCt declaredHandle Layout.Default CharSet.Ansi fields
        |> CliType.ValueType

    /// `struct Elem { byte Tag; Box Payload }`. GC auto-layout promotes the reference, so the real
    /// layout is `Payload@0, Tag@8` — which makes `Tag`'s offset a fact worth asserting rather than
    /// the 0 a declaration-order reading would give.
    let private elem () : CliType =
        ofFields
            [
                cliField "Payload" (CliType.ObjectRef None) objectHandle
                cliField "Tag" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) byteHandle
            ]

    /// `struct Nested { Elem I; byte Outer }` — one level deeper, so a chain can navigate two
    /// `Field`s past a `ByteOffset` and the second's layout depends on the first.
    let private nested () : CliType =
        ofFields
            [
                cliField "I" (elem ()) declaredHandle
                cliField "Outer" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) byteHandle
            ]

    let private tagField : FieldId = FieldId.named "Tag"
    let private innerField : FieldId = FieldId.named "I"

    let private elemSize : int = CliType.sizeOf (elem ())

    let private tagOffset : int = CliType.getFieldLayoutById tagField (elem ()) |> fst

    let private innerOffset : int =
        CliType.getFieldLayoutById innerField (nested ()) |> fst

    /// The walk never needs to resolve a `ReinterpretAs` in most of these chains, so the default
    /// `templateFor` is a trap: a test that unexpectedly consults it fails loudly rather than
    /// silently agreeing for the wrong reason.
    let private noTemplate (_ : ConcreteType<ConcreteTypeHandle>) : CliType =
        failwith "templateFor should not have been consulted"

    let private walk
        (templateFor : ConcreteType<ConcreteTypeHandle> -> CliType)
        (root : CliType)
        (projs : ByrefProjection list)
        : int64
        =
        IlMachineManagedByref.walkProjectionByteOffset templateFor (fun () -> root) projs

    // ----------------------------------------------------------------------------------------
    // Field after ByteOffset: the accepted shapes
    // ----------------------------------------------------------------------------------------

    /// `buffer[k].Tag` for `k > 0`. `peelTrailingByteView` strips the leading `ReinterpretAs Elem`
    /// and hands the walk the suffix `[ByteOffset k*sizeof(Elem); Field Tag]`, anchored by the
    /// `rootTemplate` it passes. This is the write side of `InlineArrayFieldWriteAtLaterSlot.cs`.
    [<Test>]
    let ``a Field after a ByteOffset resolves against the carried cursor`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset elemSize ; ByrefProjection.Field tagField ]
        |> shouldEqual (int64<int> (elemSize + tagOffset))

    /// Slot 0 is the degenerate case; asserting it alongside pins the stride.
    [<Test>]
    let ``slot zero and slot one differ by exactly one element`` () : unit =
        let atSlot (k : int) : int64 =
            walk noTemplate (elem ()) [ ByrefProjection.ByteOffset (k * elemSize) ; ByrefProjection.Field tagField ]

        atSlot 1 - atSlot 0 |> shouldEqual (int64<int> elemSize)

    /// The offset is not required to be a whole multiple of the cursor's size.
    /// `Unsafe.AddByteOffset(ref elem, 3)` followed by a field access is legal and well-defined in
    /// the real runtime — it adds the field offset to whatever byte the ref points at — so refusing
    /// it (or rounding it) would turn a legal program into a host crash.
    [<Test>]
    let ``a Field after an unaligned ByteOffset is still just addition`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset 3 ; ByrefProjection.Field tagField ]
        |> shouldEqual (int64<int> (3 + tagOffset))

    /// Two `Field`s past a `ByteOffset`: the second's layout depends on the type the first selected,
    /// so this only works if the cursor is threaded through rather than reset.
    [<Test>]
    let ``a ByteOffset followed by nested Field navigation threads the cursor`` () : unit =
        let projs =
            [
                ByrefProjection.ByteOffset 16
                ByrefProjection.Field innerField
                ByrefProjection.Field tagField
            ]

        walk noTemplate (nested ()) projs
        |> shouldEqual (int64<int> (16 + innerOffset + tagOffset))

    /// The same shape reached through an explicit `ReinterpretAs`, which is what an unpeeled chain
    /// looks like. The reinterpret re-anchors the cursor, so the root template is irrelevant here.
    [<Test>]
    let ``a ReinterpretAs re-anchors the cursor for a later Field`` () : unit =
        let projs =
            [
                ByrefProjection.ReinterpretAs someConcreteType
                ByrefProjection.ByteOffset elemSize
                ByrefProjection.Field tagField
            ]

        walk (fun _ -> elem ()) (CliType.Numeric (CliNumericType.Int32 0)) projs
        |> shouldEqual (int64<int> (elemSize + tagOffset))

    // ----------------------------------------------------------------------------------------
    // The shape that is a violation
    // ----------------------------------------------------------------------------------------

    /// A `ByteOffset` hung off a `Field` navigation has no reinterpret saying what type the raw
    /// bytes are being viewed as. `ManagedPointerSource.appendProjection` refuses to construct this,
    /// so reaching the walk with it means something built a chain directly and got it wrong.
    [<Test>]
    let ``a ByteOffset directly after a Field is refused`` () : unit =
        let projs = [ ByrefProjection.Field innerField ; ByrefProjection.ByteOffset 4 ]

        let exc =
            Assert.Throws<exn> (fun () -> walk noTemplate (nested ()) projs |> ignore<int64>)

        exc.Message |> shouldContainText "Field navigation followed by ByteOffset"

    /// The violation is refused wherever it appears, not only at the head of the chain.
    [<Test>]
    let ``a ByteOffset after a Field is refused deeper in the chain`` () : unit =
        let projs =
            [
                ByrefProjection.ByteOffset 16
                ByrefProjection.Field innerField
                ByrefProjection.ByteOffset 4
            ]

        let exc =
            Assert.Throws<exn> (fun () -> walk noTemplate (nested ()) projs |> ignore<int64>)

        exc.Message |> shouldContainText "Field navigation followed by ByteOffset"

    // ----------------------------------------------------------------------------------------
    // Simple chains
    // ----------------------------------------------------------------------------------------

    [<Test>]
    let ``an empty chain is offset zero`` () : unit =
        walk noTemplate (elem ()) [] |> shouldEqual 0L

    [<Test>]
    let ``a bare Field is its own offset`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.Field tagField ]
        |> shouldEqual (int64<int> tagOffset)

    [<Test>]
    let ``a trailing ByteOffset terminates the walk`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset 24 ] |> shouldEqual 24L

    [<Test>]
    let ``consecutive ByteOffsets accumulate`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset 8 ; ByrefProjection.ByteOffset -3 ]
        |> shouldEqual 5L

    /// A `ReinterpretAs` is address-preserving: it moves the cursor's type, never its offset.
    [<Test>]
    let ``a ReinterpretAs does not move the offset`` () : unit =
        let projs =
            [
                ByrefProjection.ByteOffset 12
                ByrefProjection.ReinterpretAs someConcreteType
            ]

        walk (fun _ -> elem ()) (elem ()) projs |> shouldEqual 12L

    // ----------------------------------------------------------------------------------------
    // The accumulation must not wrap (issue #993)
    // ----------------------------------------------------------------------------------------

    /// The chain `Unsafe.AddByteOffset (ref s.B, Int32.MaxValue)` builds, over a template whose
    /// second field sits at offset 1. Its true coordinate is 2147483648, which an `int`
    /// accumulator wraps onto `Int32.MinValue` — the value the *first* field displaced by
    /// `Int32.MinValue` resolves to, so two different addresses land on one number and
    /// `Unsafe.ByteOffset` reports their distance as zero (issue #993).
    [<Test>]
    let ``a Field offset plus a maximal ByteOffset does not wrap`` () : unit =
        let twoBytes =
            ofFields
                [
                    cliField "A" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) byteHandle
                    cliField "B" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) byteHandle
                ]

        let fieldA = FieldId.named "A"
        let fieldB = FieldId.named "B"

        // Premise: the two fields are one byte apart. If layout ever changes so that they are
        // not, the collision below stops being the one the issue describes and this test must
        // fail here rather than silently asserting something weaker.
        CliType.getFieldLayoutById fieldA twoBytes |> fst |> shouldEqual 0
        CliType.getFieldLayoutById fieldB twoBytes |> fst |> shouldEqual 1

        let atB =
            walk
                (fun _ -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                twoBytes
                [
                    ByrefProjection.Field fieldB
                    ByrefProjection.ReinterpretAs someConcreteType
                    ByrefProjection.ByteOffset Int32.MaxValue
                ]

        let atA =
            walk
                (fun _ -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                twoBytes
                [
                    ByrefProjection.Field fieldA
                    ByrefProjection.ReinterpretAs someConcreteType
                    ByrefProjection.ByteOffset Int32.MinValue
                ]

        atB |> shouldEqual 2147483648L
        atA |> shouldEqual -2147483648L

        // The point of the two together: an `int` fold makes these equal.
        (atA = atB) |> shouldEqual false

    /// Chains whose steps sum past `int32` in the *middle* as well as at the end. The
    /// accumulator must be wide throughout, not merely widened once at the end.
    [<Test>]
    let ``several maximal ByteOffsets accumulate without wrapping`` () : unit =
        let projs =
            [
                ByrefProjection.ByteOffset Int32.MaxValue
                ByrefProjection.ReinterpretAs someConcreteType
                ByrefProjection.ByteOffset Int32.MaxValue
                ByrefProjection.ReinterpretAs someConcreteType
                ByrefProjection.ByteOffset Int32.MaxValue
            ]

        walk (fun _ -> elem ()) (elem ()) projs
        |> shouldEqual (3L * int64<int> Int32.MaxValue)

    /// Offsets are drawn from an explicit distribution, not from FsCheck's default `int`: under
    /// `Quick` that is size-bounded to roughly [-100, 100], so a property driven by it would
    /// never once reach the boundary this whole fixture is about. The extremes are drawn as
    /// named constants rather than hoped for from a uniform range, since even a full-range
    /// uniform draw hits `Int32.MaxValue` itself with probability 2^-32.
    let private genOffset : Gen<int> =
        Gen.frequency
            [
                3,
                Gen.elements
                    [
                        Int32.MinValue
                        Int32.MinValue + 1
                        Int32.MaxValue
                        Int32.MaxValue - 1
                        -1
                        0
                        1
                    ]
                2, Gen.choose (Int32.MinValue, Int32.MaxValue)
            ]

    /// Which of the two composite templates the generator's type cursor currently sits on, so it
    /// can offer only `Field` steps that exist there. Deliberately a hand-written model of the
    /// two templates rather than a read-back of their `CliType`s: the generator's notion of what
    /// is navigable is then independent of the layout code the folds both consult, so a change
    /// there cannot quietly stop generating `Field` steps.
    [<RequireQualifiedAccess>]
    type private Cursor =
        | Nested
        | Elem
        /// A `byte` or an object reference: reached by a `Field` step, and declaring nothing.
        | Leaf

    let private navigableFields (cursor : Cursor) : (FieldId * Cursor) list =
        match cursor with
        | Cursor.Nested -> [ innerField, Cursor.Elem ; FieldId.named "Outer", Cursor.Leaf ]
        | Cursor.Elem -> [ FieldId.named "Payload", Cursor.Leaf ; tagField, Cursor.Leaf ]
        | Cursor.Leaf -> []

    /// The reinterpret target every generated `ReinterpretAs` resolves to. One target is enough:
    /// what the property tests is the arithmetic, and a second target would only re-anchor the
    /// cursor to another layout the reference fold reads the same way.
    let private reinterpretTemplate () : CliType = nested ()

    let private genChain : Gen<ByrefProjection list> =
        // `lastWasField` is what decides whether a `ByteOffset` may follow: the walk refuses a
        // `Field` immediately followed by a `ByteOffset`, since nothing then says what type the
        // raw bytes are being viewed as.
        let rec go (cursor : Cursor) (lastWasField : bool) (fuel : int) : Gen<ByrefProjection list> =
            if fuel = 0 then
                Gen.constant []
            else

            let steps =
                [
                    yield 1, Gen.constant (ByrefProjection.ReinterpretAs someConcreteType, Cursor.Nested, false)
                    if not lastWasField then
                        yield
                            3,
                            gen {
                                let! n = genOffset
                                return ByrefProjection.ByteOffset n, cursor, false
                            }
                    match navigableFields cursor with
                    | [] -> ()
                    | fields ->
                        yield
                            2,
                            gen {
                                let! f, next = Gen.elements fields
                                return ByrefProjection.Field f, next, true
                            }
                ]

            gen {
                let! step, nextCursor, nextWasField = Gen.frequency steps
                let! rest = go nextCursor nextWasField (fuel - 1)
                return step :: rest
            }

        gen {
            let! fuel = Gen.choose (0, 6)
            return! go Cursor.Nested false fuel
        }

    /// The oracle: the same fold in unbounded arithmetic. That is deliberately a restatement of
    /// the walk *in a different numeric type*, because the specification being tested is exactly
    /// "the coordinate is the unbounded sum of the steps". It says nothing about whether any
    /// individual field offset is right — both sides ask `getFieldLayoutById` — which is what
    /// the worked examples above are for.
    let private referenceCoordinate (root : CliType) (projs : ByrefProjection list) : bigint =
        let rec go (template : CliType) (acc : bigint) (remaining : ByrefProjection list) : bigint =
            match remaining with
            | [] -> acc
            | ByrefProjection.Field f :: rest ->
                let offset, _ = CliType.getFieldLayoutById f template
                go (CliType.getFieldById f template) (acc + bigint offset) rest
            | ByrefProjection.ReinterpretAs _ :: rest -> go (reinterpretTemplate ()) acc rest
            | ByrefProjection.ByteOffset n :: rest -> go template (acc + bigint n) rest

        go root 0I projs

    /// What an `int` accumulator computes, so the distribution check below can assert that
    /// the generator reaches inputs on which the two disagree. Plain `+` on `int` in F#
    /// is unchecked.
    let private wrappingCoordinate (root : CliType) (projs : ByrefProjection list) : int =
        let rec go (template : CliType) (acc : int) (remaining : ByrefProjection list) : int =
            match remaining with
            | [] -> acc
            | ByrefProjection.Field f :: rest ->
                let offset, _ = CliType.getFieldLayoutById f template
                go (CliType.getFieldById f template) (acc + offset) rest
            | ByrefProjection.ReinterpretAs _ :: rest -> go (reinterpretTemplate ()) acc rest
            | ByrefProjection.ByteOffset n :: rest -> go template (acc + n) rest

        go root 0 projs

    [<Test>]
    let ``the walk agrees with unbounded arithmetic on every chain`` () : unit =
        let mutable outsideInt32 = 0
        let mutable wouldHaveWrapped = 0

        let property =
            Prop.forAll
                (Arb.fromGen genChain)
                (fun projs ->
                    let root = nested ()
                    let expected = referenceCoordinate root projs

                    walk (fun _ -> reinterpretTemplate ()) root projs
                    |> bigint
                    |> shouldEqual expected

                    if expected < bigint Int32.MinValue || expected > bigint Int32.MaxValue then
                        outsideInt32 <- outsideInt32 + 1

                    if bigint (wrappingCoordinate root projs) <> expected then
                        wouldHaveWrapped <- wouldHaveWrapped + 1
                )

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 5000, property)

        // Without these the property is vacuous: a generator that never leaves `int32` range
        // would pass against the very accumulator this fixture exists to reject.
        if outsideInt32 = 0 then
            failwith "property never generated a chain whose coordinate leaves int32 range"

        if wouldHaveWrapped = 0 then
            failwith "property never generated a chain on which an int32 accumulator would wrap"
