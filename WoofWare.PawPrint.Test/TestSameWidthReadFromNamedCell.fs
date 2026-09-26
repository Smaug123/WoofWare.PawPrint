namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PawPrint

/// The read mirror of `TestSameWidthStoreIntoNamedCell`. `Unsafe.As<float, int>(ref s.F32)` reads a
/// `System.Single` field as an `int32`, and what the guest sees is the field's bit pattern read as
/// that type -- so the oracle here is bytes: take the cell's little-endian bytes and decode them as
/// the requested type.
///
/// Storage holding a reference has no byte image, so a read reaches such a cell by naming it. The
/// properties run over storage with and without a reference, in a boxed value and in an array
/// element, and over byrefs that reach the cell through a plain `Field` step, through a view of
/// that field, and through a byte view of the whole storage, so every route is held to the same
/// oracle.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSameWidthReadFromNamedCell =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private preparedState : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
        }

    type private CellKind =
        {
            Name : string
            Handle : ConcreteTypeHandle
            Type : ConcreteType<ConcreteTypeHandle>
            Zero : CliType
        }

        override this.ToString () : string = this.Name

    let private cellKinds : CellKind list =
        [
            "Boolean", bct.Boolean
            "Byte", bct.Byte
            "SByte", bct.SByte
            "Char", bct.Char
            "Int16", bct.Int16
            "UInt16", bct.UInt16
            "Int32", bct.Int32
            "UInt32", bct.UInt32
            "Single", bct.Single
            "Int64", bct.Int64
            "UInt64", bct.UInt64
            "Double", bct.Double
            "IntPtr", bct.IntPtr
            "UIntPtr", bct.UIntPtr
        ]
        |> List.map (fun (name, typeInfo) ->
            let handle =
                AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes typeInfo

            let zero, _ = IlMachineState.cliTypeZeroOfHandle preparedState bct handle

            let ty =
                AllConcreteTypes.lookup handle preparedState.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"%s{name} has no registry entry")

            {
                Name = name
                Handle = handle
                Type = ty
                Zero = zero
            }
        )

    let private kindNamed (name : string) : CellKind =
        cellKinds |> List.find (fun k -> k.Name = name)

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.Object

    let private storageDeclared : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.TypedReference

    /// The templates the primitive `ldind` opcodes read with: `ldind.i1`, `.u1`, `.i2`, `.u2`,
    /// `.i4` (and `.u4`), `.i8` (and `.u8`), `.i`, `.r4` and `.r8`.
    let private ldindTemplates : CliType list =
        [
            CliType.Numeric (CliNumericType.Int8 0y)
            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
            CliType.Numeric (CliNumericType.Int16 0s)
            CliType.Numeric (CliNumericType.UInt16 0us)
            CliType.Numeric (CliNumericType.Int32 0)
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
            CliType.Numeric (CliNumericType.Float32 0.0f)
            CliType.Numeric (CliNumericType.Float64 0.0)
        ]

    let private cellId : FieldId = FieldId.named "Cell"
    let private refId : FieldId = FieldId.named "Ref"
    let private leadId : FieldId = FieldId.named "Lead"
    let private innerId : FieldId = FieldId.named "Inner"

    let private field (id : FieldId) (handle : ConcreteTypeHandle) (contents : CliType) : CliField =
        {
            Id = id
            Name = id.Name
            Contents = contents
            Offset = None
            Type = handle
            MarshallingDescriptor = None
        }

    /// `{ byte Lead; T Cell; object Ref }`, or the same without `Ref`. `Lead` puts the cell at a
    /// non-zero offset, so a byref reaching it through a byte view needs a `ByteOffset`.
    let private storageOf
        (cellHandle : ConcreteTypeHandle)
        (withReference : bool)
        (cellValue : CliType)
        : CliValueType
        =
        [
            yield
                field
                    leadId
                    (kindNamed "Byte").Handle
                    (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0xA5uy)))
            yield field cellId cellHandle cellValue
            if withReference then
                yield field refId objectHandle (CliType.ObjectRef (Some (ManagedHeapAddress.ManagedHeapAddress 7)))
        ]
        |> SynthesisedLayoutKind.ofFields bct preparedState.ConcreteTypes storageDeclared Layout.Default CharSet.Ansi

    type private Root =
        /// The storage is a boxed value: `ByrefRoot.HeapValue`.
        | Boxed
        /// The storage is element 1 of a two-element array: `ByrefRoot.ArrayElement`.
        | ArrayElement

    type private Route =
        /// `[Field Cell]`: `ldflda` of the cell, then a read at the cell's own address.
        | PlainField
        /// `[Field Cell; ReinterpretAs V]`: `Unsafe.As<T, V>(ref s.Cell)`.
        | FieldView
        /// `[ReinterpretAs V; ByteOffset n]`: `Unsafe.Add(ref Unsafe.As<S, V>(ref s), ...)`-style
        /// pointer arithmetic landing on the cell.
        | ByteView

    type private Shape =
        /// `ldind` through a byte view or a `conv`'d pointer, which reads with the opcode's own
        /// template whatever the pointer's view is.
        | Ldind of CliType
        /// `ldobj V`, which reads with `V`'s template.
        | Ldobj

    type private Case =
        {
            Kind : CellKind
            View : CellKind
            WithReference : bool
            Root : Root
            Nested : bool
            Route : Route
            Shape : Shape
            Initial : byte[]
        }

        member this.Template : CliType =
            match this.Shape with
            | Shape.Ldind template -> template
            | Shape.Ldobj -> this.View.Zero

        override this.ToString () : string =
            $"%s{this.Kind.Name} cell (reference: %b{this.WithReference}, %A{this.Root}, nested: %b{this.Nested}, %A{this.Route} viewed as %s{this.View.Name}) holding %A{this.Initial}, read by %A{this.Shape}"

    let private bytesGen (count : int) : Gen<byte[]> =
        Gen.arrayOfLength count (Gen.choose (0, 255) |> Gen.map byte<int>)

    let private caseGen : Gen<Case> =
        gen {
            let! kind = Gen.elements cellKinds
            let width = CliType.sizeOf kind.Zero

            let! view =
                cellKinds
                |> List.filter (fun k -> CliType.sizeOf k.Zero = width)
                |> Gen.elements

            let! shape =
                Gen.oneof
                    [
                        Gen.constant Shape.Ldobj
                        ldindTemplates
                        |> List.filter (fun t -> CliType.sizeOf t = width)
                        |> Gen.elements
                        |> Gen.map Shape.Ldind
                    ]

            let! initial = bytesGen width
            let! withReference = Gen.elements [ true ; false ]
            let! root = Gen.elements [ Root.Boxed ; Root.ArrayElement ]
            let! nested = Gen.elements [ true ; false ]
            let! route = Gen.elements [ Route.PlainField ; Route.FieldView ; Route.ByteView ]

            return
                {
                    Kind = kind
                    View = view
                    WithReference = withReference
                    Root = root
                    Nested = nested
                    Route = route
                    Shape = shape
                    Initial = initial
                }
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    /// Same kind of value holding the same bits. Structural equality would call two NaNs different
    /// and two zeros of opposite sign the same.
    let private sameValue (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Numeric x, CliType.Numeric y -> CliNumericType.SameKind x y && CliType.ToBytes a = CliType.ToBytes b
        | CliType.Bool _, CliType.Bool _
        | CliType.Char _, CliType.Char _ -> CliType.ToBytes a = CliType.ToBytes b
        | CliType.ValueType x, CliType.ValueType y -> x.Declared = y.Declared && CliType.ToBytes a = CliType.ToBytes b
        | _ -> false

    /// A byref to the storage, rooted as `root` says, with `projections` applied. A `nested`
    /// storage sits in the root as the only field of an enclosing struct, so the byref steps into
    /// it with a `Field` before `projections`: a byte view then starts below the root, as it does
    /// for `ref local.Buffer[k]` or `ref obj.Buffer[k]`.
    let private rootedAt
        (root : Root)
        (nested : bool)
        (storage : CliValueType)
        (projections : ByrefProjection list)
        : IlMachineState * ManagedPointerSource
        =
        let storage, projections =
            if nested then
                [ field innerId storageDeclared (CliType.ValueType storage) ]
                |> SynthesisedLayoutKind.ofFields
                    bct
                    preparedState.ConcreteTypes
                    storageDeclared
                    Layout.Default
                    CharSet.Ansi,
                ByrefProjection.Field innerId :: projections
            else
                storage, projections

        let byrefRoot, state =
            match root with
            | Root.Boxed ->
                let addr, state =
                    IlMachineState.allocateManagedObject storageDeclared storage preparedState

                ByrefRoot.HeapValue addr, state
            | Root.ArrayElement ->
                let arr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero storageDeclared)
                        (fun () -> CliType.ValueType storage)
                        2
                        preparedState

                ByrefRoot.ArrayElement (arr, 1), state

        let src =
            (ManagedPointerSource.Byref
                {
                    Root = byrefRoot
                    Projections = []
                },
             projections)
            ||> List.fold (fun src proj -> ManagedPointerSource.appendProjection proj src)

        state, src

    let private rooted (case : Case) : IlMachineState * ManagedPointerSource * CliType =
        let cellValue = CliType.OfBytesLike case.Kind.Zero case.Initial
        let storage = storageOf case.Kind.Handle case.WithReference cellValue

        let projections =
            match case.Route with
            | Route.PlainField -> [ ByrefProjection.Field cellId ]
            | Route.FieldView -> [ ByrefProjection.Field cellId ; ByrefProjection.ReinterpretAs case.View.Type ]
            | Route.ByteView ->
                let offset, _ = CliValueType.GetFieldLayoutById cellId storage

                [
                    ByrefProjection.ReinterpretAs case.View.Type
                    ByrefProjection.ByteOffset offset
                ]

        let state, src = rootedAt case.Root case.Nested storage projections
        state, src, cellValue

    let private read
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (shape : Shape)
        (ldobjTemplate : CliType)
        : CliType
        =
        match shape with
        | Shape.Ldind template -> IlMachineState.readManagedByrefBytesAs bct state src template
        | Shape.Ldobj -> IlMachineState.readManagedByrefAs bct state ldobjTemplate src

    /// The read yields the cell's bits decoded as the requested type, whatever route reaches the
    /// cell and whether or not the storage has a byte image.
    [<Test>]
    let ``a same-width read decodes the cell's bits as the requested type`` () : unit =
        let property (case : Case) : bool =
            let state, src, cellValue = rooted case
            let template = case.Template
            let got = read state src case.Shape template
            sameValue got (CliType.OfBytesLike template (CliType.ToBytes cellValue))

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// Naming a cell for a byte read does not make a reference cell byte-readable: an eight-byte
    /// primitive read over it through a byte view is refused rather than inventing bits for the
    /// reference.
    [<Test>]
    let ``a primitive read over a reference cell in reference-holding storage is refused`` () : unit =
        let byteKind = kindNamed "Byte"
        let int64Kind = kindNamed "Int64"

        let storage =
            storageOf byteKind.Handle true (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))

        let refOffset, _ = CliValueType.GetFieldLayoutById refId storage

        for root in [ Root.Boxed ; Root.ArrayElement ] do
            for nested in [ true ; false ] do
                for shape in [ Shape.Ldobj ; Shape.Ldind int64Kind.Zero ] do
                    let state, src =
                        rootedAt
                            root
                            nested
                            storage
                            [
                                ByrefProjection.ReinterpretAs int64Kind.Type
                                ByrefProjection.ByteOffset refOffset
                            ]

                    Assert.Throws<System.Exception> (fun () -> read state src shape int64Kind.Zero |> ignore<CliType>)
                    |> ignore<System.Exception>

    /// A native-int cell whose bytes are a pointer's identity rather than a number has no bits to
    /// reinterpret. Read as a native int it comes back verbatim, provenance and all; read as an
    /// `int64` or a `double` it is refused.
    [<Test>]
    let ``a provenance-bearing native int in reference-holding storage is never reinterpreted`` () : unit =
        let intPtrKind = kindNamed "IntPtr"
        let int64Kind = kindNamed "Int64"
        let doubleKind = kindNamed "Double"

        let pointer =
            CliType.Numeric (
                CliNumericType.NativeInt (
                    NativeIntSource.ManagedPointer (
                        ManagedPointerSource.Byref
                            {
                                Root = ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 9)
                                Projections = []
                            }
                    )
                )
            )

        let storage = storageOf intPtrKind.Handle true pointer
        let cellOffset, _ = CliValueType.GetFieldLayoutById cellId storage

        let nativeIntTemplate =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

        for root in [ Root.Boxed ; Root.ArrayElement ] do
            for nested in [ true ; false ] do
                let viewAs (view : CellKind) : IlMachineState * ManagedPointerSource =
                    rootedAt
                        root
                        nested
                        storage
                        [
                            ByrefProjection.ReinterpretAs view.Type
                            ByrefProjection.ByteOffset cellOffset
                        ]

                let state, src = viewAs intPtrKind

                let got = IlMachineState.readManagedByrefBytesAs bct state src nativeIntTemplate
                Assert.That (got, Is.EqualTo pointer)

                for view, template in [ int64Kind, int64Kind.Zero ; doubleKind, doubleKind.Zero ] do
                    let state, src = viewAs view

                    for shape in [ Shape.Ldobj ; Shape.Ldind template ] do
                        Assert.Throws<System.Exception> (fun () -> read state src shape template |> ignore<CliType>)
                        |> ignore<System.Exception>
