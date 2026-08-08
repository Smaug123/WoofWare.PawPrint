namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `IlMachineManagedByref.walkProjectionByteOffset` folds a `ByrefProjection` chain to a byte
/// offset. `Field` consults the current type cursor; `ReinterpretAs` re-anchors it; `ByteOffset`
/// advances the offset and leaves the cursor alone.
///
/// The interesting question is what a `Field` *after* a `ByteOffset` means. It is exactly what
/// `ldflda` on a `ref T` sitting `n` bytes along means in the real runtime: `base + n +
/// offsetof(field, T)`, with no check on `n`. The cursor is still `T`-typed because
/// `ManagedPointerSource.appendProjection` only ever appends a `ByteOffset` to a chain already
/// ending in a `ReinterpretAs` — so the byte cursor always qualifies a reinterpret, and the
/// reinterpret target is the anchor a following `Field` resolves against.
///
/// The genuine violation is the mirror image: a `ByteOffset` hung off a `Field` navigation, with no
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
        CliValueType.OfFields bct allCt declaredHandle Layout.Default CharSet.Ansi fields
        |> CliType.ValueType

    /// `struct Elem { byte Tag; Box Payload }`. GC auto-layout promotes the reference, so the real
    /// layout is `Payload@0, Tag@8` — which makes `Tag`'s offset a fact worth asserting rather than
    /// the 0 a declaration-order reading would give.
    let private elem () : CliType =
        ofFields
            [
                cliField "Payload" (CliType.ObjectRef None) objectHandle
                cliField "Tag" (CliType.Numeric (CliNumericType.UInt8 0uy)) byteHandle
            ]

    /// `struct Nested { Elem I; byte Outer }` — one level deeper, so a chain can navigate two
    /// `Field`s past a `ByteOffset` and the second's layout depends on the first.
    let private nested () : CliType =
        ofFields
            [
                cliField "I" (elem ()) declaredHandle
                cliField "Outer" (CliType.Numeric (CliNumericType.UInt8 0uy)) byteHandle
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
        : int
        =
        IlMachineManagedByref.walkProjectionByteOffset templateFor (fun () -> root) projs

    // ----------------------------------------------------------------------------------------
    // The shape the guard used to refuse
    // ----------------------------------------------------------------------------------------

    /// `buffer[k].Tag` for `k > 0`. `peelTrailingByteView` strips the leading `ReinterpretAs Elem`
    /// and hands the walk the suffix `[ByteOffset k*sizeof(Elem); Field Tag]`, anchored by the
    /// `rootTemplate` it passes. This is the write side of `InlineArrayFieldWriteAtLaterSlot.cs`.
    [<Test>]
    let ``a Field after a ByteOffset resolves against the carried cursor`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset elemSize ; ByrefProjection.Field tagField ]
        |> shouldEqual (elemSize + tagOffset)

    /// Slot 0 is the degenerate case and already worked; asserting it alongside pins the stride.
    [<Test>]
    let ``slot zero and slot one differ by exactly one element`` () : unit =
        let atSlot (k : int) : int =
            walk noTemplate (elem ()) [ ByrefProjection.ByteOffset (k * elemSize) ; ByrefProjection.Field tagField ]

        atSlot 1 - atSlot 0 |> shouldEqual elemSize

    /// The offset is not required to be a whole multiple of the cursor's size.
    /// `Unsafe.AddByteOffset(ref elem, 3)` followed by a field access is legal and well-defined in
    /// the real runtime — it adds the field offset to whatever byte the ref points at — so refusing
    /// it (or rounding it) would turn a legal program into a host crash.
    [<Test>]
    let ``a Field after an unaligned ByteOffset is still just addition`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset 3 ; ByrefProjection.Field tagField ]
        |> shouldEqual (3 + tagOffset)

    /// Two `Field`s past a `ByteOffset`: the second's layout depends on the type the first selected,
    /// so this only works if the cursor is genuinely threaded through rather than reset.
    [<Test>]
    let ``a ByteOffset followed by nested Field navigation threads the cursor`` () : unit =
        let projs =
            [
                ByrefProjection.ByteOffset 16
                ByrefProjection.Field innerField
                ByrefProjection.Field tagField
            ]

        walk noTemplate (nested ()) projs |> shouldEqual (16 + innerOffset + tagOffset)

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
        |> shouldEqual (elemSize + tagOffset)

    // ----------------------------------------------------------------------------------------
    // The shape that is a genuine violation
    // ----------------------------------------------------------------------------------------

    /// A `ByteOffset` hung off a `Field` navigation has no reinterpret saying what type the raw
    /// bytes are being viewed as. `ManagedPointerSource.appendProjection` refuses to construct this,
    /// so reaching the walk with it means something built a chain directly and got it wrong.
    [<Test>]
    let ``a ByteOffset directly after a Field is refused`` () : unit =
        let projs = [ ByrefProjection.Field innerField ; ByrefProjection.ByteOffset 4 ]

        let exc =
            Assert.Throws<exn> (fun () -> walk noTemplate (nested ()) projs |> ignore<int>)

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
            Assert.Throws<exn> (fun () -> walk noTemplate (nested ()) projs |> ignore<int>)

        exc.Message |> shouldContainText "Field navigation followed by ByteOffset"

    // ----------------------------------------------------------------------------------------
    // Shapes that already worked, pinned so the relaxation doesn't disturb them
    // ----------------------------------------------------------------------------------------

    [<Test>]
    let ``an empty chain is offset zero`` () : unit =
        walk noTemplate (elem ()) [] |> shouldEqual 0

    [<Test>]
    let ``a bare Field is its own offset`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.Field tagField ]
        |> shouldEqual tagOffset

    [<Test>]
    let ``a trailing ByteOffset terminates the walk`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset 24 ] |> shouldEqual 24

    [<Test>]
    let ``consecutive ByteOffsets accumulate`` () : unit =
        walk noTemplate (elem ()) [ ByrefProjection.ByteOffset 8 ; ByrefProjection.ByteOffset -3 ]
        |> shouldEqual 5

    /// A `ReinterpretAs` is address-preserving: it moves the cursor's type, never its offset.
    [<Test>]
    let ``a ReinterpretAs does not move the offset`` () : unit =
        let projs =
            [
                ByrefProjection.ByteOffset 12
                ByrefProjection.ReinterpretAs someConcreteType
            ]

        walk (fun _ -> elem ()) (elem ()) projs |> shouldEqual 12
