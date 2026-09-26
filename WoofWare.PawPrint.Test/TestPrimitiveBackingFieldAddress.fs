namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The address of a primitive's own backing field (`System.Int32::m_value`) through a pointer to
/// that primitive, as `ldflda` computes it in `UnaryMetadataFieldOps.instanceFieldAddress`.
///
/// PawPrint stores the primitive in two shapes, and the answer differs between them. Everywhere
/// but a box it is a bare cell, which *is* the field, so the address is the pointer itself. Inside
/// a box it is a single-field wrapper holding that very field, so the address is an ordinary field
/// projection. The guest cases `UnsafeAccessorPrimitiveBackingField.cs` and
/// `DynamicMethodPrimitiveBackingField.cs` reach only the bare cell, because no guest can yet form
/// a pointer into a boxed primitive other than the `this` of CoreLib's own methods; this fixture
/// covers both.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPrimitiveBackingFieldAddress =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = allCt
        }

    let private generate<'a> (f : 'a -> CliType) : Gen<CliType> =
        ArbMap.defaults |> ArbMap.generate<'a> |> Gen.map f

    /// Every primitive PawPrint stores as a bare cell, with a generator for its value.
    /// `System.IntPtr`/`System.UIntPtr` are absent: their storage is a field map everywhere.
    let private barePrimitiveCases : (string * TypeInfo<GenericParamFromMetadata, TypeDefn> * Gen<CliType>) list =
        [
            "Boolean", bct.Boolean, generate<bool> CliType.ofBool
            "Char", bct.Char, generate<char> CliType.ofChar
            "SByte", bct.SByte, generate<sbyte> (CliNumericType.Int8 >> CliType.Numeric)
            "Byte", bct.Byte, generate<byte> (UInt8Source.Verbatim >> CliNumericType.UInt8 >> CliType.Numeric)
            "Int16", bct.Int16, generate<int16> (CliNumericType.Int16 >> CliType.Numeric)
            "UInt16", bct.UInt16, generate<uint16> (CliNumericType.UInt16 >> CliType.Numeric)
            "Int32", bct.Int32, generate<int32> (CliNumericType.Int32 >> CliType.Numeric)
            "UInt32", bct.UInt32, generate<uint32> (int32 >> CliNumericType.Int32 >> CliType.Numeric)
            "Int64", bct.Int64, generate<int64> (Int64Source.Verbatim >> CliNumericType.Int64 >> CliType.Numeric)
            "UInt64",
            bct.UInt64,
            generate<uint64> (int64 >> Int64Source.Verbatim >> CliNumericType.Int64 >> CliType.Numeric)
            "Single", bct.Single, generate<float32> (CliNumericType.Float32 >> CliType.Numeric)
            "Double", bct.Double, generate<float> (CliNumericType.Float64 >> CliType.Numeric)
        ]

    /// Float equality by bit pattern; plain `=` says `NaN <> NaN`.
    let private cliTypesBitEqual (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Numeric (CliNumericType.Float32 x), CliType.Numeric (CliNumericType.Float32 y) ->
            System.BitConverter.SingleToInt32Bits x = System.BitConverter.SingleToInt32Bits y
        | CliType.Numeric (CliNumericType.Float64 x), CliType.Numeric (CliNumericType.Float64 y) ->
            System.BitConverter.DoubleToInt64Bits x = System.BitConverter.DoubleToInt64Bits y
        | _ -> a = b

    let private backingField
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : FieldInfo<GenericParamFromMetadata, TypeDefn> * FieldId
        =
        let field = ti.Fields |> List.filter (fun f -> not f.IsStatic) |> List.exactlyOne
        let handle = AllConcreteTypes.getRequiredNonGenericHandle allCt ti
        field, FieldId.metadata handle field.Handle field.Name

    let private fieldAddress
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (src : ManagedPointerSource)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field, fieldId = backingField ti

        let state, pointer =
            UnaryMetadataFieldOps.instanceFieldAddress
                loggerFactory
                bct
                "test"
                field
                fieldId
                (EvalStackValue.ManagedPointer src)
                state

        pointer, state

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 50

    [<Test>]
    let ``through a bare cell the backing field is the cell itself`` () : unit =
        for name, ti, genStored in barePrimitiveCases do
            let property =
                genStored |> Arb.fromGen |> Prop.forAll
                <| fun stored ->
                    let state = state ()
                    let elementHandle = AllConcreteTypes.getRequiredNonGenericHandle allCt ti

                    let arr, state =
                        IlMachineState.allocateArray
                            (ConcreteTypeHandle.OneDimArrayZero elementHandle)
                            (fun () -> stored)
                            2
                            state

                    let src =
                        ManagedPointerSource.Byref
                            {
                                Root = ByrefRoot.ArrayElement (arr, 1)
                                Projections = []
                            }

                    let pointer, state = fieldAddress ti src state

                    if pointer <> src then
                        failwithf "%s: expected the element's own address %O, got %O" name src pointer

                    let read =
                        IlMachineState.readManagedByref bct state (ManagedPointerSource.requireAddressed pointer)

                    if not (cliTypesBitEqual read stored) then
                        failwithf "%s: stored %O but read %O through the field's address" name stored read

                    true

            Check.One (config, property)

    [<Test>]
    let ``inside a box the backing field is a projection of the wrapper`` () : unit =
        for name, ti, genStored in barePrimitiveCases do
            let property =
                genStored |> Arb.fromGen |> Prop.forAll
                <| fun stored ->
                    let _, loggerFactory = LoggerFactory.makeTest ()
                    let state = state ()
                    let handle = AllConcreteTypes.getRequiredNonGenericHandle allCt ti

                    let boxed, state =
                        Boxing.boxValueType loggerFactory bct handle (EvalStackValue.ofCliType stored) state

                    let src =
                        ManagedPointerSource.Byref
                            {
                                Root = ByrefRoot.HeapValue boxed
                                Projections = []
                            }

                    let pointer, state = fieldAddress ti src state
                    let _, fieldId = backingField ti

                    let expected =
                        ManagedPointerSource.Byref
                            {
                                Root = ByrefRoot.HeapValue boxed
                                Projections = [ ByrefProjection.Field fieldId ]
                            }

                    if pointer <> expected then
                        failwithf "%s: expected the projection %O, got %O" name expected pointer

                    let read =
                        IlMachineState.readManagedByref bct state (ManagedPointerSource.requireAddressed pointer)

                    if not (cliTypesBitEqual read stored) then
                        failwithf "%s: boxed %O but read %O through the field's address" name stored read

                    true

            Check.One (config, property)

    [<Test>]
    let ``through untyped bytes the backing field is the bytes' own address`` () : unit =
        // A `ref int` over `stackalloc` or native memory addresses bytes with no typed cell, and
        // uninitialised ones cannot be read at all, so taking the field's address must not read.
        for name, ti, _ in barePrimitiveCases do
            let src, state =
                IlMachineState.allocateNativeMemory MemoryBlockInitialization.Uninitialized 8 (state ())

            let pointer, _ = fieldAddress ti src state

            if pointer <> src then
                failwithf "%s: expected the bytes' own address %O, got %O" name src pointer

    [<Test>]
    let ``a native int's backing field is an ordinary projection`` () : unit =
        // `System.IntPtr` is stored as a field map even outside a box, so the near miss beside the
        // bare primitives keeps the projection.
        let state = state ()
        let handle = AllConcreteTypes.getRequiredNonGenericHandle allCt bct.IntPtr
        let zero, state = IlMachineState.cliTypeZeroOfHandle state bct handle

        let arr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero handle) (fun () -> zero) 1 state

        let src =
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.ArrayElement (arr, 0)
                    Projections = []
                }

        let pointer, _ = fieldAddress bct.IntPtr src state
        let _, fieldId = backingField bct.IntPtr

        pointer
        |> shouldEqual (
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.ArrayElement (arr, 0)
                    Projections = [ ByrefProjection.Field fieldId ]
                }
        )
