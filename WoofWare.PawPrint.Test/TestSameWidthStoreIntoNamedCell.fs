namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A primitive `stind` does not know the type of the cell it stores into: `stind.i1` hands over a
/// signed byte whether the cell is a `System.Byte`, a `System.SByte` or a `System.Boolean`, and
/// `stind.i4`/`stind.r4` hand over an `int32` or a `float32` whatever four-byte cell they land on.
/// What the guest then reads back through the cell's own type is the payload's bit pattern, read as
/// that type — so the oracle here is bytes: splice the payload's little-endian bytes over the cell,
/// and decode them as the cell's type.
///
/// Storage holding a reference has no byte image, so a store reaches such a cell by naming it. The
/// properties run over storage with and without a reference, and over byrefs that reach the cell
/// through a byte view and through a plain `Field` step, so every route is held to the same oracle.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSameWidthStoreIntoNamedCell =

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

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.Object

    let private storageDeclared : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.TypedReference

    /// The payloads the primitive `stind` opcodes store, as `EvalStackValue.toCliTypeCoerced`
    /// shapes them: `stind.i1`, `.i2`, `.i4`, `.i8`, `.i`, `.r4` and `.r8`.
    let private payloadZeros : CliType list =
        [
            CliType.Numeric (CliNumericType.Int8 0y)
            CliType.Numeric (CliNumericType.Int16 0s)
            CliType.Numeric (CliNumericType.Int32 0)
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
            CliType.Numeric (CliNumericType.Float32 0.0f)
            CliType.Numeric (CliNumericType.Float64 0.0)
        ]

    let private cellId : FieldId = FieldId.named "Cell"
    let private refId : FieldId = FieldId.named "Ref"
    let private leadId : FieldId = FieldId.named "Lead"

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
    let private storageOf (kind : CellKind) (withReference : bool) (cellValue : CliType) : CliValueType =
        [
            yield
                field
                    leadId
                    (cellKinds |> List.find (fun k -> k.Name = "Byte")).Handle
                    (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0xA5uy)))
            yield field cellId kind.Handle cellValue
            if withReference then
                yield field refId objectHandle (CliType.ObjectRef (Some (ManagedHeapAddress.ManagedHeapAddress 7)))
        ]
        |> SynthesisedLayoutKind.ofFields bct preparedState.ConcreteTypes storageDeclared Layout.Default CharSet.Ansi

    type private Route =
        /// `[Field Cell]`: `ldflda` of the cell, then `stind`.
        | PlainField
        /// `[ReinterpretAs T; ByteOffset n]`: `Unsafe.Add(ref Unsafe.As<S, T>(ref s), ...)`-style
        /// pointer arithmetic landing on the cell.
        | ByteView

    type private Case =
        {
            Kind : CellKind
            WithReference : bool
            Route : Route
            Initial : byte[]
            Payload : CliType
        }

        override this.ToString () : string =
            $"%s{this.Kind.Name} cell (reference: %b{this.WithReference}, %A{this.Route}) initially %A{this.Initial}, storing %O{this.Payload}"

    let private bytesGen (count : int) : Gen<byte[]> =
        Gen.arrayOfLength count (Gen.choose (0, 255) |> Gen.map byte<int>)

    let private caseGen : Gen<Case> =
        gen {
            let! kind = Gen.elements cellKinds
            let width = CliType.sizeOf kind.Zero

            let! payloadZero = payloadZeros |> List.filter (fun p -> CliType.sizeOf p = width) |> Gen.elements

            let! payloadBytes = bytesGen width
            let! initial = bytesGen width
            let! withReference = Gen.elements [ true ; false ]
            let! route = Gen.elements [ Route.PlainField ; Route.ByteView ]

            return
                {
                    Kind = kind
                    WithReference = withReference
                    Route = route
                    Initial = initial
                    Payload = CliType.OfBytesLike payloadZero payloadBytes
                }
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private fieldsOf (value : CliType) : CliField list =
        match value with
        | CliType.ValueType cvt -> CliValueType.TryAllFields cvt |> List.map CliConcreteField.ToCliField
        | _ -> []

    let rec private leaves (path : FieldId list) (value : CliType) : (FieldId list * CliType) list =
        match value with
        | CliType.ValueType _ -> fieldsOf value |> List.collect (fun f -> leaves (path @ [ f.Id ]) f.Contents)
        | other -> [ path, other ]

    /// Same kind of cell holding the same bits. Structural equality would call two NaNs different
    /// and two zeros of opposite sign the same.
    let private sameLeaf (a : CliType) (b : CliType) : bool =
        let sameKind =
            match a, b with
            | CliType.Numeric x, CliType.Numeric y -> CliNumericType.SameKind x y
            | CliType.Bool _, CliType.Bool _
            | CliType.Char _, CliType.Char _ -> true
            | CliType.ObjectRef x, CliType.ObjectRef y -> x = y
            | _ -> false

        match a, b with
        | CliType.ObjectRef _, _ -> sameKind
        | _ -> sameKind && CliType.ToBytes a = CliType.ToBytes b

    let private sameStorage (a : CliType) (b : CliType) : bool =
        let a = leaves [] a
        let b = leaves [] b

        List.length a = List.length b
        && List.forall2 (fun (pa, va) (pb, vb) -> pa = pb && sameLeaf va vb) a b

    let private rooted (case : Case) : IlMachineState * ManagedHeapAddress * ManagedPointerSource * CliValueType =
        let storage =
            storageOf case.Kind case.WithReference (CliType.OfBytesLike case.Kind.Zero case.Initial)

        let addr, state =
            IlMachineState.allocateManagedObject storageDeclared storage preparedState

        let projections =
            match case.Route with
            | Route.PlainField -> [ ByrefProjection.Field cellId ]
            | Route.ByteView ->
                let offset, _ = CliValueType.GetFieldLayoutById cellId storage

                [
                    ByrefProjection.ReinterpretAs case.Kind.Type
                    ByrefProjection.ByteOffset offset
                ]

        let src =
            (ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.HeapValue addr
                    Projections = []
                },
             projections)
            ||> List.fold (fun src proj -> ManagedPointerSource.appendProjection proj src)

        state, addr, src, storage

    /// The cell keeps its own type and holds the payload's bits; nothing else in the storage moves.
    [<Test>]
    let ``a same-width stind leaves the cell its own type holding the payload's bits`` () : unit =
        let property (case : Case) : bool =
            let state, addr, src, storage = rooted case

            let state = IlMachineState.writeIndirectPrimitiveStore bct state src case.Payload

            let expected =
                CliType.withCellAtPathSet
                    [ cellId ]
                    (CliType.OfBytesLike case.Kind.Zero (CliType.ToBytes case.Payload))
                    (CliType.ValueType storage)

            sameStorage (CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents) expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// Reading the cell back through the byref that wrote it, as the cell's own type, yields the
    /// payload's bits.
    [<Test>]
    let ``a same-width stind reads back through the cell's own type as the payload's bits`` () : unit =
        let property (case : Case) : bool =
            let state, _, src, _ = rooted case
            let state = IlMachineState.writeIndirectPrimitiveStore bct state src case.Payload
            let read = IlMachineState.readManagedByref bct state src

            sameStorage read (CliType.OfBytesLike case.Kind.Zero (CliType.ToBytes case.Payload))

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// Naming a cell for a byte write does not make a reference cell byte-writable: an eight-byte
    /// primitive stored over it through a byte view is refused rather than replacing the reference.
    [<Test>]
    let ``a primitive store over a reference cell in reference-holding storage is refused`` () : unit =
        let byteKind = cellKinds |> List.find (fun k -> k.Name = "Byte")

        let storage =
            storageOf byteKind true (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))

        let addr, state =
            IlMachineState.allocateManagedObject storageDeclared storage preparedState

        let int64Kind = cellKinds |> List.find (fun k -> k.Name = "Int64")
        let refOffset, _ = CliValueType.GetFieldLayoutById refId storage

        let src =
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.HeapValue addr
                    Projections = []
                }
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs int64Kind.Type)
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset refOffset)

        let payload = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x1234L))

        Assert.Throws<System.Exception> (fun () ->
            IlMachineState.writeIndirectPrimitiveStore bct state src payload
            |> ignore<IlMachineState>
        )
        |> ignore<System.Exception>
