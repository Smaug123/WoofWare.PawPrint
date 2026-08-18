namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Property-based oracle for `CliType.ToBytes` / `CliType.OfBytesLike`:
/// for every primitive CliType, serialising to bytes and back must be the
/// identity. Catches size-mismatch bugs (e.g. overload resolution picking
/// `BitConverter.GetBytes(Half)` for sbyte, producing 2 bytes instead of 1)
/// and endian mistakes in either direction.
[<TestFixture>]
module TestCliTypeBytes =

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

    let private boolHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Boolean

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Object

    let private intPtrHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.IntPtr

    let private doubleHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Double

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

    let private genPrimitiveNumeric : Gen<CliNumericType> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<sbyte> |> Gen.map CliNumericType.Int8
                ArbMap.defaults
                |> ArbMap.generate<byte>
                |> Gen.map (UInt8Source.Verbatim >> CliNumericType.UInt8)
                ArbMap.defaults |> ArbMap.generate<int16> |> Gen.map CliNumericType.Int16
                ArbMap.defaults |> ArbMap.generate<uint16> |> Gen.map CliNumericType.UInt16
                ArbMap.defaults |> ArbMap.generate<int32> |> Gen.map CliNumericType.Int32
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (Int64Source.Verbatim >> CliNumericType.Int64)
                ArbMap.defaults |> ArbMap.generate<float32> |> Gen.map CliNumericType.Float32
                ArbMap.defaults |> ArbMap.generate<float> |> Gen.map CliNumericType.Float64
            ]

    let private genPrimitiveCliType : Gen<CliType> =
        Gen.oneof
            [
                genPrimitiveNumeric |> Gen.map CliType.Numeric
                ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map CliType.Bool
                gen {
                    let! hi = ArbMap.defaults |> ArbMap.generate<byte>
                    let! lo = ArbMap.defaults |> ArbMap.generate<byte>
                    return CliType.Char (hi, lo)
                }
            ]

    let private toBytesSizeAgreesWithSizeOf (v : CliType) : bool =
        let bytes = CliType.ToBytes v
        bytes.Length = CliType.SizeOf(v).Size

    /// Float equality under the `NaN = NaN` bit-pattern view the
    /// round-trip preserves. Regular `=` treats NaN as unequal, which would
    /// fail the property spuriously for NaN-generating seeds.
    let private cliTypesBitEqual (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Numeric (CliNumericType.Float32 x), CliType.Numeric (CliNumericType.Float32 y) ->
            System.BitConverter.SingleToInt32Bits x = System.BitConverter.SingleToInt32Bits y
        | CliType.Numeric (CliNumericType.Float64 x), CliType.Numeric (CliNumericType.Float64 y) ->
            System.BitConverter.DoubleToInt64Bits x = System.BitConverter.DoubleToInt64Bits y
        | _ -> a = b

    let private roundTripIsIdentity (v : CliType) : bool =
        let bytes = CliType.ToBytes v
        let recovered = CliType.OfBytesLike v bytes
        cliTypesBitEqual v recovered

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private genBytes (length : int) : Gen<byte[]> =
        ArbMap.defaults |> ArbMap.generate<byte> |> Gen.arrayOfLength length

    let private genSliceRange (size : int) : Gen<int * int> =
        gen {
            let! offset = Gen.choose (0, size)
            let! count = Gen.choose (0, size - offset)
            return offset, count
        }

    let private rawSizedValueType (size : int) : CliType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = size, packingSize = 0))
            CharSet.Ansi
            []
        |> CliType.ValueType

    let private fieldBackedValueType (value : int32) : CliValueType =
        let field =
            cliField "Value" (CliType.Numeric (CliNumericType.Int32 value)) None int32Handle

        SynthesisedLayoutKind.ofFields bct allCt declaredHandle Layout.Default CharSet.Ansi [ field ]

    let private explicitUnionValueType (asInt : int32) : CliValueType =
        let asIntField =
            cliField "AsInt" (CliType.Numeric (CliNumericType.Int32 asInt)) (Some 0) int32Handle

        let byte0 =
            cliField "Byte0" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) (Some 0) byteHandle

        let byte1 =
            cliField "Byte1" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) (Some 1) byteHandle

        let byte2 =
            cliField "Byte2" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) (Some 2) byteHandle

        let byte3 =
            cliField "Byte3" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) (Some 3) byteHandle

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 4, packingSize = 0))
            CharSet.Ansi
            [ asIntField ; byte0 ; byte1 ; byte2 ; byte3 ]

    let private paddedValueType () : CliValueType =
        let byteField =
            cliField "Byte" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) None byteHandle

        let intField =
            cliField "Int" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle

        SynthesisedLayoutKind.ofFields bct allCt declaredHandle Layout.Default CharSet.Ansi [ byteField ; intField ]

    let private nestedUnionValueType () : CliValueType =
        let inner =
            cliField "Inner" (explicitUnionValueType 0 |> CliType.ValueType) (Some 0) declaredHandle

        let asLong =
            cliField "AsLong" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (Some 0) int64Handle

        let upper =
            cliField "UpperInt" (CliType.Numeric (CliNumericType.Int32 0)) (Some 4) int32Handle

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ inner ; asLong ; upper ]

    let private outerOverInnerPaddingValueType () : CliValueType =
        let inner =
            cliField "Inner" (paddedValueType () |> CliType.ValueType) (Some 0) declaredHandle

        let other =
            cliField "Other" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) (Some 1) byteHandle

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ inner ; other ]

    let private trailingStorageValueType () : CliValueType =
        let prefix =
            cliField "Prefix" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ prefix ]

    let private explicitOverlapWithTailValueType () : CliValueType =
        let whole =
            cliField "Whole" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (Some 0) int64Handle

        let low =
            cliField "Low" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ whole ; low ]

    let private fieldBackedBoolValueType () : CliType =
        let field = cliField "Flag" (CliType.Bool 0uy) None boolHandle

        SynthesisedLayoutKind.ofFields bct allCt declaredHandle Layout.Default CharSet.Ansi [ field ]
        |> CliType.ValueType

    let private objectReferenceValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle ]

    let private runtimePointerValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [
                cliField
                    "Ptr"
                    (CliType.RuntimePointer (
                        CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle)
                    ))
                    (Some 0)
                    intPtrHandle
            ]

    let private taggedNativeIntValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [
                cliField
                    "Handle"
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L)))
                    (Some 0)
                    intPtrHandle
            ]

    let private nestedTaggedNativeIntValueType () : CliValueType =
        let inner = taggedNativeIntValueType ()

        let innerField =
            cliField "Inner" (inner |> CliType.ValueType) (Some 0) inner.Declared

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ innerField ]

    let private nestedObjectReferenceValueType () : CliValueType =
        let inner =
            let innerValueType = objectReferenceValueType ()
            cliField "Inner" (innerValueType |> CliType.ValueType) (Some 0) innerValueType.Declared

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            int64Handle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [ inner ]

    let private objectAndRuntimePointerValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 16, packingSize = 0))
            CharSet.Ansi
            [
                cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle
                cliField
                    "Ptr"
                    (CliType.RuntimePointer (
                        CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle)
                    ))
                    (Some 8)
                    intPtrHandle
            ]

    /// `struct { int N; Box B; }`. A value type holding a reference gets CoreCLR's auto layout,
    /// which hoists references to the front of the pointer-sized class, so this is `B` at [0,8)
    /// and `N` at [8,12) — the reference lands in the *low* slot whatever the declaration order.
    /// This is the shape of `Dictionary<K,V>.Entry`, and the reason a pointer-slot walk over such
    /// an array has slots that contain no reference at all.
    let private mixedReferenceValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            Layout.Default
            CharSet.Ansi
            [
                cliField "N" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                cliField "B" (CliType.ObjectRef None) None objectHandle
            ]

    /// A single-field wrapper around a reference. As a *field* this is a value class rather than
    /// a primitive, so auto layout places it after every size-class bucket instead of hoisting it.
    let private referenceWrapperValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            Layout.Default
            CharSet.Ansi
            [ cliField "B" (CliType.ObjectRef None) None objectHandle ]

    /// `struct { long L; struct { Box B } W; }`: `L` at [0,8) and the wrapper — so the reference —
    /// at [8,16). Distinct from `mixedReferenceValueType` because it puts the reference in the
    /// *high* slot, so a walk that writes the low slot first leaves a live reference in place
    /// while the low slot is processed. Declaration order alone cannot produce this shape: auto
    /// layout always hoists a directly-held reference to the front, so the reference has to be
    /// buried in a by-value field to keep it out of the low slots.
    let private referenceHighSlotValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            Layout.Default
            CharSet.Ansi
            [
                cliField "L" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
                cliField "W" (referenceWrapperValueType () |> CliType.ValueType) None declaredHandle
            ]

    /// A struct whose reference is buried inside a nested struct, so a range covering the
    /// nested field has to recurse rather than treat it as an opaque cell.
    let private nestedMixedValueType () : CliValueType =
        let inner = mixedReferenceValueType ()

        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            Layout.Default
            CharSet.Ansi
            [
                cliField "Inner" (inner |> CliType.ValueType) None inner.Declared
                cliField "Tail" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
            ]

    /// Explicit layout in which a reference field *overlaps* a sibling. Classification is per
    /// field and purely interval-based, so a range covering the union must zero both members;
    /// this pins that rather than leaving it to argument.
    let private overlappingReferenceUnionValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 16, packingSize = 0))
            CharSet.Ansi
            [
                cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle
                cliField "Alias" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (Some 0) int64Handle
                cliField "Tail" (CliType.Numeric (CliNumericType.Int32 0)) (Some 8) int32Handle
            ]

    /// Explicit layout placing an object reference at a misaligned offset. Real CoreCLR rejects
    /// this at type load; PawPrint's `ComputeConcreteFields` takes `[FieldOffset(n)]` verbatim
    /// and does not, so the shape is representable here and any range rule has to cope with it
    /// rather than assume pointer alignment.
    let private misalignedReferenceValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 16, packingSize = 0))
            CharSet.Ansi
            [ cliField "Obj" (CliType.ObjectRef None) (Some 3) objectHandle ]

    let private syntheticCrossStorageNativeIntSource () : NativeIntSource =
        NativeIntSource.syntheticCrossStorageByteOffset
            (ByteStorageIdentity.StackMemory (ThreadId 0, FrameId 0, StackMemoryBlockId 0))
            0L
            (ByteStorageIdentity.StackLocal (ThreadId 0, FrameId 0, 0us))
            8L

    let private syntheticCrossStorageOffset () : SyntheticCrossArrayOffset =
        match syntheticCrossStorageNativeIntSource () with
        | NativeIntSource.SyntheticCrossArrayOffset offset -> offset
        | other -> failwith $"Expected synthetic cross-storage offset, got %O{other}"

    /// Native ints that are *handles*: an identity PawPrint carries in place of an address, so
    /// each byte of one is a position within that identity and can be named.
    let private namedByteNativeIntSources () : NativeIntSource list =
        [
            NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed int32Handle)
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle)
            NativeIntSource.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed int32Handle)
            NativeIntSource.MethodHandlePtr 1234L
            NativeIntSource.FieldHandlePtr 1234L
            NativeIntSource.AssemblyHandle "assembly"
            NativeIntSource.ModuleHandle "module"
            NativeIntSource.MetadataImportHandle "metadata"
            NativeIntSource.GcHandlePtr (GcHandleAddress 0, 0L)
        ]

    /// Native ints with no byte image at all: a byref is a storage location rather than an
    /// identity, and the synthetic cross-storage offset is a sentinel standing in for a distance
    /// that does not exist.
    let private refusedNativeIntSources () : NativeIntSource list =
        [
            NativeIntSource.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.StackMemoryByte (ThreadId 0, FrameId 0, StackMemoryBlockId 0, 0),
                    []
                )
            )
            syntheticCrossStorageNativeIntSource ()
        ]

    let private nonByteRenderableNativeIntSources () : NativeIntSource list =
        namedByteNativeIntSources () @ refusedNativeIntSources ()

    let private genByteAddressabilityCliType : Gen<CliType> =
        Gen.oneof
            [
                genPrimitiveCliType
                ArbMap.defaults
                |> ArbMap.generate<int32>
                |> Gen.map (fieldBackedValueType >> CliType.ValueType)
                Gen.constant (rawSizedValueType 8)
                Gen.constant (fieldBackedBoolValueType ())
                Gen.constant (CliType.ObjectRef None)
                Gen.constant (
                    CliType.RuntimePointer (
                        CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle)
                    )
                )
                Gen.constant (objectReferenceValueType () |> CliType.ValueType)
                Gen.constant (runtimePointerValueType () |> CliType.ValueType)
                nonByteRenderableNativeIntSources ()
                |> List.map (CliNumericType.NativeInt >> CliType.Numeric)
                |> Gen.elements
                Gen.constant (
                    CliType.Numeric (
                        CliNumericType.Int64 (Int64Source.widenedNativeInt (NativeIntSource.FieldHandlePtr 1234L) true)
                    )
                )
                Gen.constant (
                    CliType.Numeric (
                        CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset (syntheticCrossStorageOffset ()))
                    )
                )
                Gen.constant (taggedNativeIntValueType () |> CliType.ValueType)
                Gen.constant (nestedTaggedNativeIntValueType () |> CliType.ValueType)
                Gen.constant (nestedObjectReferenceValueType () |> CliType.ValueType)
                Gen.constant (objectAndRuntimePointerValueType () |> CliType.ValueType)
                Gen.constant (mixedReferenceValueType () |> CliType.ValueType)
                Gen.constant (referenceHighSlotValueType () |> CliType.ValueType)
                Gen.constant (nestedMixedValueType () |> CliType.ValueType)
                Gen.constant (misalignedReferenceValueType () |> CliType.ValueType)
                Gen.constant (overlappingReferenceUnionValueType () |> CliType.ValueType)
            ]

    [<Test>]
    let ``ToBytes output size matches SizeOf for primitive CliType values`` () : unit =
        Check.One (config, Prop.forAll (Arb.fromGen genPrimitiveCliType) toBytesSizeAgreesWithSizeOf)

    [<Test>]
    let ``OfBytesLike inverts ToBytes for primitive CliType values`` () : unit =
        Check.One (config, Prop.forAll (Arb.fromGen genPrimitiveCliType) roundTripIsIdentity)

    [<Test>]
    let ``CliType byte slices read and write primitive values`` () : unit =
        let value = CliType.Numeric (CliNumericType.Int32 0x11223344)

        CliType.BytesAt 1 2 value |> shouldEqual [| 0x33uy ; 0x22uy |]

        let updated = CliType.WithBytesAt 1 [| 0xAAuy ; 0xBBuy |] value

        CliType.ToBytes updated |> shouldEqual [| 0x44uy ; 0xAAuy ; 0xBBuy ; 0x11uy |]

        let originalBytes = CliType.BytesAt 0 4 value

        CliType.WithBytesAtIfChanged 0 originalBytes value |> shouldEqual None

        CliType.WithBytesAtIfChanged 1 [| 0xAAuy ; 0xBBuy |] value
        |> shouldEqual (Some updated)

    [<Test>]
    let ``CliType byte slices reject byte-unaddressable values and bad ranges`` () : unit =
        let assertFailsWith (message : string) (action : unit -> unit) : unit =
            let ex = Assert.Throws<System.Exception> (fun () -> action ())
            ex.Message |> shouldContainText message

        assertFailsWith
            "byte count -1 is negative"
            (fun () -> CliType.BytesAt 0 -1 (CliType.Numeric (CliNumericType.Int32 0)) |> ignore)

        assertFailsWith
            "byte range [3, 5) exceeds 4-byte CLI value"
            (fun () ->
                CliType.WithBytesAt 3 [| 0uy ; 1uy |] (CliType.Numeric (CliNumericType.Int32 0))
                |> ignore
            )

        assertFailsWith
            "refusing byte slice over object reference"
            (fun () -> CliType.BytesAt 0 1 (CliType.ObjectRef None) |> ignore)

    [<Test>]
    let ``ByteAddressability classifies direct and nested reference-like storage`` () : unit =
        CliType.ByteAddressability (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual CliByteAddressability.ByteAddressable

        CliType.ByteAddressability (CliType.Bool 1uy)
        |> shouldEqual CliByteAddressability.ByteAddressable

        CliType.ByteAddressability (CliType.Char (0uy, byte 'a'))
        |> shouldEqual CliByteAddressability.ByteAddressable

        CliType.ByteAddressability (rawSizedValueType 8)
        |> shouldEqual CliByteAddressability.ByteAddressable

        CliType.ByteAddressability (fieldBackedValueType 3 |> CliType.ValueType)
        |> shouldEqual CliByteAddressability.ByteAddressable

        CliType.ByteAddressability (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1L)))
        |> shouldEqual CliByteAddressability.ByteAddressable

        CliType.ByteAddressability (
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        )
        |> shouldEqual CliByteAddressability.ByteAddressable

        match
            CliType.ByteAddressability (
                CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))
            )
        with
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            obstruction.Description |> shouldContainText "native int"
        | other -> failwith $"Expected FieldHandlePtr native int to be addressable only as named bytes, got %O{other}"

        for source in namedByteNativeIntSources () do
            match CliType.ByteAddressability (CliType.Numeric (CliNumericType.NativeInt source)) with
            | CliByteAddressability.SymbolicallyAddressable obstruction ->
                obstruction.Description |> shouldContainText "native int"
            | other ->
                failwith $"Expected native int source %O{source} to be addressable only as named bytes, got %O{other}"

        // The control that keeps the line above meaningful: not every native int PawPrint refuses
        // to render becomes nameable. A byref is a storage location rather than an identity, so
        // there is nothing for a byte to be a position in.
        for source in refusedNativeIntSources () do
            match CliType.ByteAddressability (CliType.Numeric (CliNumericType.NativeInt source)) with
            | CliByteAddressability.Rejected rejection -> rejection.Description |> shouldContainText "native int"
            | other -> failwith $"Expected native int source %O{source} to stay refused outright, got %O{other}"

        match
            CliType.ByteAddressability (
                CliType.Numeric (
                    CliNumericType.Int64 (Int64Source.widenedNativeInt (NativeIntSource.FieldHandlePtr 1234L) true)
                )
            )
        with
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            failwith $"int64 provenance has no per-byte naming rule, so it must stay refused outright: %O{obstruction}"
        | CliByteAddressability.Rejected rejection -> rejection.Description |> shouldContainText "int64"
        | CliByteAddressability.ByteAddressable ->
            failwith "Expected widened FieldHandlePtr int64 to be rejected as byte-unaddressable"

        match
            CliType.ByteAddressability (
                CliType.Numeric (
                    CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset (syntheticCrossStorageOffset ()))
                )
            )
        with
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            failwith $"int64 provenance has no per-byte naming rule, so it must stay refused outright: %O{obstruction}"
        | CliByteAddressability.Rejected rejection -> rejection.Description |> shouldContainText "int64"
        | CliByteAddressability.ByteAddressable ->
            failwith "Expected synthetic cross-storage int64 to be rejected as byte-unaddressable"

        match
            CliType.ByteAddressability (
                CliType.Numeric (
                    CliNumericType.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.Verbatim 0L, true))
                )
            )
        with
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            failwith $"int64 provenance has no per-byte naming rule, so it must stay refused outright: %O{obstruction}"
        | CliByteAddressability.Rejected rejection -> rejection.Description |> shouldContainText "int64"
        | CliByteAddressability.ByteAddressable ->
            failwith "Expected non-canonical widened verbatim int64 to be rejected as byte-unaddressable"

        CliType.ByteAddressability (CliType.ObjectRef None)
        |> shouldEqual (CliByteAddressability.Rejected CliByteAddressabilityRejection.ObjectReference)

        CliType.ByteAddressability (
            CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle))
        )
        |> shouldEqual (CliByteAddressability.Rejected CliByteAddressabilityRejection.RuntimePointer)

        let objectValueType = objectReferenceValueType ()

        CliValueType.ByteAddressability objectValueType
        |> shouldEqual (
            CliByteAddressability.Rejected (
                CliByteAddressabilityRejection.ValueTypeContainsObjectReferences objectValueType.Declared
            )
        )

        let pointerValueType = runtimePointerValueType ()

        CliType.ByteAddressability (CliType.ValueType pointerValueType)
        |> shouldEqual (
            CliByteAddressability.Rejected (
                CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers pointerValueType.Declared
            )
        )

        let taggedNativeIntValueType = taggedNativeIntValueType ()

        match CliType.ByteAddressability (CliType.ValueType taggedNativeIntValueType) with
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            obstruction.Description
            |> shouldContainText "value type containing non-byte-addressable field"
        | other ->
            failwith
                $"Expected value type containing tagged native int to be addressable only as named bytes, got %O{other}"

        let nestedTaggedNativeIntValueType = nestedTaggedNativeIntValueType ()

        match CliType.ByteAddressability (CliType.ValueType nestedTaggedNativeIntValueType) with
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            obstruction.Description
            |> shouldContainText "value type containing non-byte-addressable field"

            obstruction.Description |> shouldContainText "native int"
        | other ->
            failwith
                $"Expected nested value type containing tagged native int to be addressable only as named bytes, got %O{other}"

        let nestedObjectValueType = nestedObjectReferenceValueType ()

        CliType.ByteAddressability (CliType.ValueType nestedObjectValueType)
        |> shouldEqual (
            CliByteAddressability.Rejected (
                CliByteAddressabilityRejection.ValueTypeContainsObjectReferences nestedObjectValueType.Declared
            )
        )

        nestedObjectValueType.Declared |> shouldNotEqual declaredHandle

        let mixedValueType = objectAndRuntimePointerValueType ()

        CliValueType.ByteAddressability mixedValueType
        |> shouldEqual (
            CliByteAddressability.Rejected (
                CliByteAddressabilityRejection.ValueTypeContainsObjectReferences mixedValueType.Declared
            )
        )

    [<Test>]
    let ``ByteAddressability accepted values render as bytes`` () : unit =
        // `ByteAddressability` is stricter than "contains references": tagged
        // numeric provenance can also make a value unsafe to render. The
        // invariant we need from accepted values is that byte helpers can
        // actually materialise their byte image.
        let mutable byteAddressableCount = 0
        let mutable symbolicCount = 0
        let mutable rejectedCount = 0

        let property (value : CliType) : unit =
            match CliType.ByteAddressability value with
            | CliByteAddressability.ByteAddressable ->
                byteAddressableCount <- byteAddressableCount + 1

                let bytes = CliType.ToBytes value
                bytes.Length |> shouldEqual (CliType.SizeOf(value).Size)

                CliType.BytesAt 0 bytes.Length value |> shouldEqual bytes
            | CliByteAddressability.SymbolicallyAddressable _ ->
                symbolicCount <- symbolicCount + 1

                // The corresponding invariant for a value whose bytes are only nameable: the
                // named image exists and is the declared width, and the `byte[]` helpers still
                // refuse it.
                let size = CliType.SizeOf(value).Size
                CliType.SymbolicBytesAt 0 size value |> Array.length |> shouldEqual size

                (fun () -> CliType.BytesAt 0 size value |> ignore) |> shouldFail<exn>
            | CliByteAddressability.Rejected _ -> rejectedCount <- rejectedCount + 1

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)
        byteAddressableCount > 0 |> shouldEqual true
        symbolicCount > 0 |> shouldEqual true
        rejectedCount > 0 |> shouldEqual true

    [<Test>]
    let ``byteAtOffset rejects byte-unaddressable values with clear diagnostics`` () : unit =
        let cases =
            [
                CliType.ObjectRef None, "object reference"
                CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle)),
                "runtime pointer"
                objectReferenceValueType () |> CliType.ValueType, "value type containing object references"
                runtimePointerValueType () |> CliType.ValueType, "value type containing runtime pointers"
            ]

        for value, description in cases do
            let ex =
                Assert.Throws<System.Exception> (fun () ->
                    IntrinsicHelpers.byteAtOffset "test byte compare" ManagedPointerSource.Null 0 value
                    |> ignore
                )

            ex.Message |> shouldContainText "test byte compare"

            ex.Message |> shouldContainText "CliType.BytesAt: refusing byte slice over"

            ex.Message |> shouldContainText description

    [<Test>]
    let ``DescribeByteLayout renders field-backed storage diagnostics`` () : unit =
        let template = paddedValueType ()
        let bytes : byte[] = Array.zeroCreate (CliValueType.SizeOf(template).Size)
        bytes.[0] <- 10uy
        bytes.[1] <- 0xAAuy
        bytes.[2] <- 0xBBuy
        bytes.[3] <- 0xCCuy

        let recovered = CliValueType.OfBytesLike template bytes
        let diagnostic = CliValueType.DescribeByteLayout (Some allCt) recovered

        diagnostic |> shouldContainText "value type byte layout:"
        diagnostic |> shouldContainText "declared type:"
        diagnostic |> shouldContainText "storage: field-backed"
        diagnostic |> shouldContainText "preserved byte image: 8 bytes"
        diagnostic |> shouldContainText "byte-addressability: byte-addressable"
        diagnostic |> shouldContainText "Byte: range=[0, 1), size=1"
        diagnostic |> shouldContainText "Int: range=[4, 8), size=4"
        diagnostic |> shouldContainText "value=Numeric"
        diagnostic |> shouldContainText "unrepresented byte ranges:"
        diagnostic |> shouldContainText "[1, 4): AA BB CC"

    [<Test>]
    let ``DescribeByteLayout renders raw storage and rejection reasons`` () : unit =
        let rawPayload = [| 0x01uy ; 0x02uy ; 0xFEuy ; 0xFFuy |]
        let rawTemplate = rawSizedValueType rawPayload.Length

        let rawRecovered =
            match CliType.OfBytesLike rawTemplate rawPayload with
            | CliType.ValueType vt -> vt
            | other -> failwith $"expected value type, got %O{other}"

        let rawDiagnostic = CliValueType.DescribeByteLayout (Some allCt) rawRecovered

        rawDiagnostic |> shouldContainText "storage: raw bytes"
        rawDiagnostic |> shouldContainText "fields: none"
        rawDiagnostic |> shouldContainText "[0, 4): 01 02 FE FF"

        let rejectedDiagnostic =
            runtimePointerValueType () |> CliValueType.DescribeByteLayout (Some allCt)

        rejectedDiagnostic
        |> shouldContainText "byte-addressability: rejected: value type containing runtime pointers"

        rejectedDiagnostic |> shouldContainText "Ptr: range=[0, 8), size=8"

    [<Test>]
    let ``ToBytes output size matches SizeOf for raw-backed fieldless value types`` () : unit =
        for size in [ 16 ; 64 ] do
            let value = rawSizedValueType size
            let bytes = CliType.ToBytes value
            bytes.Length |> shouldEqual (CliType.SizeOf(value).Size)
            bytes.Length |> shouldEqual size

    [<Test>]
    let ``OfBytesLike round-trips raw-backed fieldless value types`` () : unit =
        for size in [ 16 ; 64 ] do
            let template = rawSizedValueType size

            let payload : byte[] = Array.init size (fun i -> byte ((i * 37 + 11) &&& 0xFF))

            let recovered = CliType.OfBytesLike template payload
            CliType.ToBytes recovered |> shouldEqual payload

    [<Test>]
    let ``BytesAt returns independent slices from raw-backed fieldless value types`` () : unit =
        let property (payload : byte[]) ((offset, count) : int * int) : unit =
            let template = rawSizedValueType payload.Length
            let recovered = CliType.OfBytesLike template payload
            let expected = Array.zeroCreate<byte> count
            Array.blit payload offset expected 0 count

            let actual =
                match recovered with
                | CliType.ValueType vt -> CliValueType.BytesAt offset count vt
                | other -> failwith $"Expected value type, got %O{other}"

            actual |> shouldEqual expected

            if actual.Length > 0 then
                actual.[0] <- actual.[0] ^^^ 0xFFuy

            CliType.ToBytes recovered |> shouldEqual payload

        Check.One (
            config,
            Prop.forAll
                (genBytes 16 |> Arb.fromGen)
                (fun payload ->
                    Prop.forAll (genSliceRange payload.Length |> Arb.fromGen) (fun range -> property payload range)
                )
        )

    [<Test>]
    let ``WithBytesAt updates raw-backed fieldless value types`` () : unit =
        let property (payload : byte[]) (replacement : byte[]) ((offset, count) : int * int) : unit =
            let template = rawSizedValueType payload.Length
            let recovered = CliType.OfBytesLike template payload
            replacement.Length |> shouldEqual count
            let expected = Array.copy payload
            Array.blit replacement 0 expected offset replacement.Length

            let updated =
                match recovered with
                | CliType.ValueType vt -> CliValueType.WithBytesAt offset replacement vt
                | other -> failwith $"Expected value type, got %O{other}"

            CliValueType.ToBytes updated |> shouldEqual expected
            CliType.ToBytes recovered |> shouldEqual payload

        Check.One (
            config,
            Prop.forAll
                (genBytes 16 |> Arb.fromGen)
                (fun payload ->
                    Prop.forAll
                        (genSliceRange payload.Length |> Arb.fromGen)
                        (fun range ->
                            let _, count = range

                            Prop.forAll
                                (genBytes count |> Arb.fromGen)
                                (fun replacement -> property payload replacement range)
                        )
                )
        )

    [<Test>]
    let ``OfBytesLike round-trips field-backed value types`` () : unit =
        let property (value : int32) : unit =
            let template = fieldBackedValueType 0
            let source = fieldBackedValueType value
            let expectedBytes = CliValueType.ToBytes source

            let recovered = CliType.OfBytesLike (CliType.ValueType template) expectedBytes

            CliType.ToBytes recovered |> shouldEqual expectedBytes

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen) property)

    [<Test>]
    let ``BytesAt returns slices from field-backed value types including preserved padding`` () : unit =
        let property (payload : byte[]) ((offset, count) : int * int) : unit =
            let template = trailingStorageValueType ()
            let recovered = CliValueType.OfBytesLike template payload
            let expected = Array.zeroCreate<byte> count
            Array.blit payload offset expected 0 count

            let actual = CliValueType.BytesAt offset count recovered
            actual |> shouldEqual expected

            if actual.Length > 0 then
                actual.[0] <- actual.[0] ^^^ 0xFFuy

            CliValueType.ToBytes recovered |> shouldEqual payload

        Check.One (
            config,
            Prop.forAll
                (genBytes 8 |> Arb.fromGen)
                (fun payload ->
                    Prop.forAll (genSliceRange payload.Length |> Arb.fromGen) (fun range -> property payload range)
                )
        )

    [<Test>]
    let ``WithBytesAt updates field-backed value types and preserves adjacent storage`` () : unit =
        let property (initialPrefix : int32) (trailing : int32) (replacement : int32) : unit =
            let template = trailingStorageValueType ()
            let bytes : byte[] = Array.zeroCreate 8

            let initialBytes = System.BitConverter.GetBytes initialPrefix
            Array.blit initialBytes 0 bytes 0 initialBytes.Length

            let trailingBytes = System.BitConverter.GetBytes trailing
            Array.blit trailingBytes 0 bytes 4 trailingBytes.Length

            let recovered = CliValueType.OfBytesLike template bytes
            let replacementBytes = System.BitConverter.GetBytes replacement
            let updated = CliValueType.WithBytesAt 0 replacementBytes recovered

            let expected = Array.copy bytes
            Array.blit replacementBytes 0 expected 0 replacementBytes.Length

            CliValueType.ToBytes updated |> shouldEqual expected

            CliValueType.DereferenceField "Prefix" updated
            |> shouldEqual (CliType.Numeric (CliNumericType.Int32 replacement))

            let updatedAgain =
                CliValueType.WithFieldSet "Prefix" (CliType.Numeric (CliNumericType.Int32 initialPrefix)) updated

            let expectedAgain = Array.copy expected
            Array.blit initialBytes 0 expectedAgain 0 initialBytes.Length

            CliValueType.ToBytes updatedAgain |> shouldEqual expectedAgain

        Check.One (
            config,
            Prop.forAll
                (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                (fun initialPrefix ->
                    Prop.forAll
                        (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                        (fun trailing ->
                            Prop.forAll
                                (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                                (fun replacement -> property initialPrefix trailing replacement)
                        )
                )
        )

    [<Test>]
    let ``WithBytesAt updates padded field-backed value types including padding slices`` () : unit =
        let property (initialBytes : byte[]) (replacementSource : byte[]) : unit =
            let template = paddedValueType ()
            let recovered = CliValueType.OfBytesLike template initialBytes

            let ranges =
                [
                    for offset = 0 to initialBytes.Length - 1 do
                        for count = 1 to initialBytes.Length - offset do
                            offset, count
                ]

            for offset, count in ranges do
                let replacement = Array.zeroCreate<byte> count
                Array.blit replacementSource offset replacement 0 count

                let expected = Array.copy initialBytes
                Array.blit replacement 0 expected offset replacement.Length

                let updated = CliValueType.WithBytesAt offset replacement recovered

                CliValueType.ToBytes updated |> shouldEqual expected
                CliValueType.BytesAt 1 3 updated |> shouldEqual expected.[1..3]

                CliValueType.DereferenceField "Byte" updated
                |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim expected.[0])))

                CliValueType.DereferenceField "Int" updated
                |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (System.BitConverter.ToInt32 (expected, 4))))

        Check.One (
            config,
            Prop.forAll
                (genBytes 8 |> Arb.fromGen)
                (fun initialBytes ->
                    Prop.forAll
                        (genBytes 8 |> Arb.fromGen)
                        (fun replacementSource -> property initialBytes replacementSource)
                )
        )

    [<Test>]
    let ``byte slice operations reject invalid ranges`` () : unit =
        let template = trailingStorageValueType ()
        let value = CliValueType.OfBytesLike template (Array.zeroCreate 8)

        let assertFailsWith (message : string) (action : unit -> unit) : unit =
            let ex = Assert.Throws<System.Exception> (fun () -> action ())
            ex.Message |> shouldContainText message

        assertFailsWith "byte offset -1 is negative" (fun () -> CliValueType.BytesAt -1 1 value |> ignore)
        assertFailsWith "byte count -1 is negative" (fun () -> CliValueType.BytesAt 0 -1 value |> ignore)

        assertFailsWith
            "byte range [7, 9) exceeds 8-byte value type"
            (fun () -> CliValueType.BytesAt 7 2 value |> ignore)

        assertFailsWith "byte offset -1 is negative" (fun () -> CliValueType.WithBytesAt -1 [| 0uy |] value |> ignore)

        assertFailsWith
            "byte range [0, 9) exceeds 8-byte value type"
            (fun () -> CliValueType.WithBytesAt 0 (Array.zeroCreate 9) value |> ignore)

        assertFailsWith
            "byte range [9, 9) exceeds 8-byte value type"
            (fun () -> CliValueType.WithBytesAt 9 Array.empty value |> ignore)

    [<Test>]
    let ``WithBytesAt preserves field provenance for byte-identical writes`` () : unit =
        let template = explicitOverlapWithTailValueType ()

        let afterWhole =
            CliValueType.WithFieldSet
                "Whole"
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x0102030405060708L)))
                template

        let value =
            CliValueType.WithFieldSet "Low" (CliType.Numeric (CliNumericType.Int32 0x11223344)) afterWhole

        let lowBytes = CliValueType.BytesAt 0 4 value

        CliValueType.WithBytesAtIfChanged 0 lowBytes value |> shouldEqual None

        CliValueType.WithBytesAtIfChanged 0 Array.empty value |> shouldEqual None

        let changed = CliValueType.WithBytesAt 0 [| 0xFEuy ; 0xDCuy |] value

        CliValueType.WithBytesAtIfChanged 0 [| 0xFEuy ; 0xDCuy |] value
        |> shouldEqual (Some changed)

    [<Test>]
    let ``WithBytesAt updates explicit-layout overlapping fields consistently`` () : unit =
        let property (initialWhole : int64) (replacementSource : byte[]) : unit =
            let template = explicitOverlapWithTailValueType ()
            let initialBytes = System.BitConverter.GetBytes initialWhole
            let recovered = CliValueType.OfBytesLike template initialBytes

            for offset = 0 to initialBytes.Length - 1 do
                for count = 1 to initialBytes.Length - offset do
                    let replacement = Array.zeroCreate<byte> count
                    Array.blit replacementSource offset replacement 0 count

                    let expected = Array.copy initialBytes
                    Array.blit replacement 0 expected offset replacement.Length

                    let updated = CliValueType.WithBytesAt offset replacement recovered

                    CliValueType.ToBytes updated |> shouldEqual expected

                    CliValueType.DereferenceField "Whole" updated
                    |> shouldEqual (
                        CliType.Numeric (
                            CliNumericType.Int64 (Int64Source.Verbatim (System.BitConverter.ToInt64 (expected, 0)))
                        )
                    )

                    CliValueType.DereferenceField "Low" updated
                    |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (System.BitConverter.ToInt32 (expected, 0))))

        Check.One (
            config,
            Prop.forAll
                (ArbMap.defaults |> ArbMap.generate<int64> |> Arb.fromGen)
                (fun initialWhole ->
                    Prop.forAll
                        (genBytes 8 |> Arb.fromGen)
                        (fun replacementSource -> property initialWhole replacementSource)
                )
        )

    [<Test>]
    let ``OfBytesLike round-trips overlapping field-backed value types`` () : unit =
        let property (value : int32) : unit =
            let template = explicitUnionValueType 0
            let expectedBytes = System.BitConverter.GetBytes value

            let recovered = CliValueType.OfBytesLike template expectedBytes

            CliValueType.ToBytes recovered |> shouldEqual expectedBytes

            CliValueType.DereferenceField "AsInt" recovered
            |> shouldEqual (CliType.Numeric (CliNumericType.Int32 value))

            for i = 0 to expectedBytes.Length - 1 do
                CliValueType.DereferenceField $"Byte%i{i}" recovered
                |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim expectedBytes.[i])))

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen) property)

    [<Test>]
    let ``OfBytesLike reconstructs nested overlapping value-type fields`` () : unit =
        let property (lowInt : int32) (upperInt : int32) : unit =
            let template = nestedUnionValueType ()
            let expectedBytes : byte[] = Array.zeroCreate 8

            System.BitConverter.GetBytes lowInt
            |> fun low -> Array.blit low 0 expectedBytes 0 low.Length

            System.BitConverter.GetBytes upperInt
            |> fun upper -> Array.blit upper 0 expectedBytes 4 upper.Length

            let recovered = CliValueType.OfBytesLike template expectedBytes

            CliValueType.ToBytes recovered |> shouldEqual expectedBytes

            match CliValueType.DereferenceField "Inner" recovered with
            | CliType.ValueType inner ->
                CliValueType.DereferenceField "AsInt" inner
                |> shouldEqual (CliType.Numeric (CliNumericType.Int32 lowInt))
            | other -> failwith $"Expected nested value type, got %O{other}"

            CliValueType.DereferenceField "UpperInt" recovered
            |> shouldEqual (CliType.Numeric (CliNumericType.Int32 upperInt))

            CliValueType.DereferenceField "AsLong" recovered
            |> shouldEqual (
                CliType.Numeric (
                    CliNumericType.Int64 (Int64Source.Verbatim (System.BitConverter.ToInt64 (expectedBytes, 0)))
                )
            )

        Check.One (
            config,
            Prop.forAll
                (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                (fun lowInt ->
                    Prop.forAll
                        (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                        (fun upperInt -> property lowInt upperInt)
                )
        )

    [<Test>]
    let ``OfBytesLike preserves non-zero padding bytes for field-backed value types`` () : unit =
        let template = paddedValueType ()
        let bytes : byte[] = Array.zeroCreate (CliValueType.SizeOf(template).Size)
        bytes.[0] <- 3uy
        bytes.[1] <- 1uy

        let recovered = CliValueType.OfBytesLike template bytes

        CliValueType.ToBytes recovered |> shouldEqual bytes

        let updated =
            CliValueType.WithFieldSet
                "Byte"
                (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 9uy)))
                recovered

        let expected = Array.copy bytes
        expected.[0] <- 9uy

        CliValueType.ToBytes updated |> shouldEqual expected

        let updatedInt =
            CliValueType.WithFieldSet "Int" (CliType.Numeric (CliNumericType.Int32 0x11223344)) recovered

        let expectedInt = Array.copy bytes
        let intBytes = System.BitConverter.GetBytes 0x11223344
        Array.blit intBytes 0 expectedInt 4 intBytes.Length

        CliValueType.ToBytes updatedInt |> shouldEqual expectedInt

    [<Test>]
    let ``OfBytesLike allows inner padding bytes when outer fields preserve them`` () : unit =
        let template = outerOverInnerPaddingValueType ()
        let bytes : byte[] = Array.zeroCreate (CliValueType.SizeOf(template).Size)
        bytes.[0] <- 10uy
        bytes.[1] <- 5uy

        let recovered = CliValueType.OfBytesLike template bytes

        CliValueType.ToBytes recovered |> shouldEqual bytes

        CliValueType.DereferenceField "Other" recovered
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 5uy)))

    [<Test>]
    let ``field writes preserve trailing storage recovered from bytes`` () : unit =
        let property (initialPrefix : int32) (trailing : int32) (updatedPrefix : int32) : unit =
            let template = trailingStorageValueType ()
            let bytes : byte[] = Array.zeroCreate 8

            let initialBytes = System.BitConverter.GetBytes initialPrefix
            Array.blit initialBytes 0 bytes 0 initialBytes.Length

            let trailingBytes = System.BitConverter.GetBytes trailing
            Array.blit trailingBytes 0 bytes 4 trailingBytes.Length

            let recovered = CliValueType.OfBytesLike template bytes

            CliValueType.ToBytes recovered |> shouldEqual bytes

            let updated =
                CliValueType.WithFieldSet "Prefix" (CliType.Numeric (CliNumericType.Int32 updatedPrefix)) recovered

            let expected = Array.copy bytes
            let updatedBytes = System.BitConverter.GetBytes updatedPrefix
            Array.blit updatedBytes 0 expected 0 updatedBytes.Length

            CliValueType.ToBytes updated |> shouldEqual expected

        Check.One (
            config,
            Prop.forAll
                (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                (fun initialPrefix ->
                    Prop.forAll
                        (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                        (fun trailing ->
                            Prop.forAll
                                (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                                (fun updatedPrefix -> property initialPrefix trailing updatedPrefix)
                        )
                )
        )

    [<Test>]
    let ``explicit-layout union updates after byte recovery preserve untouched overlap bytes`` () : unit =
        let property (initialWhole : int64) (updatedLow : int32) : unit =
            let template = explicitOverlapWithTailValueType ()
            let bytes = System.BitConverter.GetBytes initialWhole

            let recovered = CliValueType.OfBytesLike template bytes

            CliValueType.ToBytes recovered |> shouldEqual bytes

            // `OfBytesLike` recovers declaration-order edit timestamps; writing `Low` must make it
            // win over `Whole` while preserving the untouched high bytes from the preserved image.
            let updated =
                CliValueType.WithFieldSet "Low" (CliType.Numeric (CliNumericType.Int32 updatedLow)) recovered

            let expected = Array.copy bytes
            let lowBytes = System.BitConverter.GetBytes updatedLow
            Array.blit lowBytes 0 expected 0 lowBytes.Length

            CliValueType.ToBytes updated |> shouldEqual expected

            CliValueType.DereferenceField "Whole" updated
            |> shouldEqual (
                CliType.Numeric (
                    CliNumericType.Int64 (Int64Source.Verbatim (System.BitConverter.ToInt64 (expected, 0)))
                )
            )

            CliValueType.DereferenceField "Low" updated
            |> shouldEqual (CliType.Numeric (CliNumericType.Int32 updatedLow))

        Check.One (
            config,
            Prop.forAll
                (ArbMap.defaults |> ArbMap.generate<int64> |> Arb.fromGen)
                (fun initialWhole ->
                    Prop.forAll
                        (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen)
                        (fun updatedLow -> property initialWhole updatedLow)
                )
        )

    [<Test>]
    let ``Marshal size guard detects shapes whose unmanaged size may differ`` () : unit =
        CliType.TryFindMarshalSizeDifference (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual None

        CliType.TryFindMarshalSizeDifference (CliType.Bool 0uy)
        |> Option.isSome
        |> shouldEqual true

        CliType.TryFindMarshalSizeDifference (CliType.Char (0uy, 0uy))
        |> Option.isSome
        |> shouldEqual true

        CliType.TryFindMarshalSizeDifference (CliType.ObjectRef None)
        |> Option.isSome
        |> shouldEqual true

        CliType.TryFindMarshalSizeDifference (fieldBackedBoolValueType ())
        |> Option.isSome
        |> shouldEqual true

    /// Structural shape equality: same CLI storage form, ignoring the values held. This is the
    /// invariant `CliType.ZeroLike` exists to guarantee — its result is written back into the
    /// cell it was derived from, through typed write paths that overwrite wholesale.
    let rec private sameShape (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Bool _, CliType.Bool _
        | CliType.Char _, CliType.Char _
        | CliType.ObjectRef _, CliType.ObjectRef _
        | CliType.RuntimePointer _, CliType.RuntimePointer _ -> true
        | CliType.Numeric a, CliType.Numeric b ->
            match a, b with
            | CliNumericType.Int8 _, CliNumericType.Int8 _
            | CliNumericType.UInt8 _, CliNumericType.UInt8 _
            | CliNumericType.Int16 _, CliNumericType.Int16 _
            | CliNumericType.UInt16 _, CliNumericType.UInt16 _
            | CliNumericType.Int32 _, CliNumericType.Int32 _
            | CliNumericType.Int64 _, CliNumericType.Int64 _
            | CliNumericType.NativeInt _, CliNumericType.NativeInt _
            | CliNumericType.NativeFloat _, CliNumericType.NativeFloat _
            | CliNumericType.Float32 _, CliNumericType.Float32 _
            | CliNumericType.Float64 _, CliNumericType.Float64 _ -> true
            | _ -> false
        // The declared handle is the canonical identifier for a struct's layout, so equality
        // means same fields at same offsets -- the same criterion the production copy path
        // (`CellAwareMemOps.cellsHaveCompatibleShape`) uses to decide a whole-cell write is
        // shape-preserving.
        | CliType.ValueType a, CliType.ValueType b ->
            a.Declared = b.Declared && a.PrimitiveLikeKind = b.PrimitiveLikeKind
        | _ -> false

    [<Test>]
    let ``ZeroLike preserves CLI shape and size`` () : unit =
        let property (template : CliType) : bool =
            let zero = CliType.ZeroLike template

            CliType.SizeOf zero = CliType.SizeOf template && sameShape zero template

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)

    [<Test>]
    let ``ZeroLike is idempotent`` () : unit =
        let property (template : CliType) : bool =
            let zero = CliType.ZeroLike template
            CliType.ZeroLike zero = zero

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)

    [<Test>]
    let ``ZeroLike renders as all-zero bytes whenever it is byte-addressable`` () : unit =
        // Not every zero is byte-renderable: the zero of a reference cell is the null
        // reference and the zero of a pointer cell is the null pointer, neither of which
        // `ToBytes` models. Where a byte rendering does exist, it must be all zeros --
        // otherwise a byte-level consumer would disagree with the typed one about what
        // "cleared" means.
        let property (template : CliType) : bool =
            let zero = CliType.ZeroLike template

            match CliType.ByteAddressability zero with
            | CliByteAddressability.SymbolicallyAddressable _
            | CliByteAddressability.Rejected _ -> true
            | CliByteAddressability.ByteAddressable -> CliType.ToBytes zero |> Array.forall (fun b -> b = 0uy)

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)

    [<Test>]
    let ``ZeroLike agrees with OfBytesLike over zero bytes wherever that is defined`` () : unit =
        // The oracle property. `OfBytesLike template (all zeros)` is the same value by
        // construction wherever it is defined at all; `ZeroLike` exists because it is *not*
        // defined for object references, runtime pointers, or any struct transitively
        // containing one -- shapes that do reach a bulk clear (raw pointers are not
        // GC-tracked, so `struct S { int N; int* P; }[]` reports no GC pointers and clears
        // through the byte-count path).
        let property (template : CliType) : bool =
            let zeroBytes : byte[] = Array.zeroCreate (CliType.SizeOf template).Size

            let viaBytes =
                try
                    Some (CliType.OfBytesLike template zeroBytes)
                with _ ->
                    None

            match viaBytes with
            | None -> true
            | Some viaBytes -> viaBytes = CliType.ZeroLike template

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)

    [<Test>]
    let ``ZeroLike is total over shapes that have no byte rendering`` () : unit =
        // These are exactly the shapes `OfBytesLike` cannot reconstruct, and the reason
        // `ZeroLike` is structural rather than byte-driven. A struct holding an unmanaged
        // pointer is not hypothetical: raw pointers are not GC-tracked, so `S[]` for
        // `struct S { int N; int* P; }` reports `ContainsGCPointers = false` and `Array.Clear`
        // sends it down the byte-count path that ends in a bulk zeroing.
        let cases : CliType list =
            [
                CliType.ObjectRef None
                CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle))
                runtimePointerValueType () |> CliType.ValueType
                objectReferenceValueType () |> CliType.ValueType
                objectAndRuntimePointerValueType () |> CliType.ValueType
                nestedObjectReferenceValueType () |> CliType.ValueType
            ]

        for case in cases do
            // Sanity: each of these really is beyond `OfBytesLike`, so the property test's
            // oracle arm is vacuous for them and this test is carrying the weight.
            let zeroBytes : byte[] = Array.zeroCreate (CliType.SizeOf case).Size

            let viaBytesFailed =
                try
                    CliType.OfBytesLike case zeroBytes |> ignore
                    false
                with _ ->
                    true

            if not viaBytesFailed then
                failwith $"expected OfBytesLike to have no byte rendering for %O{case}"

            let zero = CliType.ZeroLike case
            CliType.SizeOf zero |> shouldEqual (CliType.SizeOf case)
            sameShape zero case |> shouldEqual true

        CliType.ZeroLike (CliType.ObjectRef (Some (ManagedHeapAddress 7)))
        |> shouldEqual (CliType.ObjectRef None)

        CliType.ZeroLike (
            CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle))
        )
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))

    // ---- CliType.WithZeroedRangeIfChanged ----
    //
    // The operation `Array.Clear` needs for reference-containing element types: zero a byte
    // range of a cell that may have no byte rendering at all.

    /// Ranges within a value, biased towards the whole-value range and towards
    /// pointer-slot-sized/aligned windows, which are the ones `SpanHelpers.ClearWithReferences`
    /// actually produces.
    let private genRangeWithin (size : int) : Gen<int * int> =
        let slots =
            [ 0..8 .. max 0 (size - 8) ]
            |> List.map (fun o -> o, 8)
            |> List.filter (fun (o, c) -> o + c <= size)

        Gen.oneof
            [
                Gen.constant (0, size)
                (if List.isEmpty slots then
                     Gen.constant (0, size)
                 else
                     Gen.elements slots)
                gen {
                    let! offset = Gen.choose (0, size)
                    let! count = Gen.choose (0, size - offset)
                    return offset, count
                }
            ]

    let private genValueAndRange : Gen<CliType * int * int> =
        gen {
            let! value = genByteAddressabilityCliType
            let! offset, count = genRangeWithin (CliType.SizeOf value).Size
            return value, offset, count
        }

    /// `WithZeroedRangeIfChanged` is partial by design (it refuses to half-zero a reference), so
    /// properties are stated over the calls that succeed.
    let private tryZero (offset : int) (count : int) (value : CliType) : CliType option option =
        try
            Some (CliType.WithZeroedRangeIfChanged offset count value)
        with _ ->
            None

    [<Test>]
    let ``Zeroing the whole range agrees with ZeroLike`` () : unit =
        let property (value : CliType) : bool =
            let size = (CliType.SizeOf value).Size

            match CliType.WithZeroedRangeIfChanged 0 size value with
            | Some zeroed -> zeroed = CliType.ZeroLike value
            | None ->
                // `None` means "nothing observable changed". Structural equality against
                // `ZeroLike` is too strong a reading of that: it also compares field
                // write-timestamp bookkeeping, which only orders overlapping replays and is
                // not observable. Where a byte rendering exists it is the honest check, and it
                // still catches `-0.0`. The unrenderable case is pinned by the explicit
                // `-0.0`-inside-an-unrenderable-struct test below, which this generator cannot
                // introspect well enough to assert.
                match CliType.ByteAddressability value with
                | CliByteAddressability.ByteAddressable ->
                    CliType.ToBytes value = CliType.ToBytes (CliType.ZeroLike value)
                | CliByteAddressability.SymbolicallyAddressable _
                | CliByteAddressability.Rejected _ -> true

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)

    [<Test>]
    let ``Zeroing a range preserves CLI shape and size`` () : unit =
        let property (value : CliType, offset : int, count : int) : bool =
            match tryZero offset count value with
            | None
            | Some None -> true
            | Some (Some zeroed) -> CliType.SizeOf zeroed = CliType.SizeOf value && sameShape zeroed value

        Check.One (config, Prop.forAll (Arb.fromGen genValueAndRange) property)

    [<Test>]
    let ``Zeroing a range is idempotent`` () : unit =
        let property (value : CliType, offset : int, count : int) : bool =
            match tryZero offset count value with
            | None
            | Some None -> true
            | Some (Some zeroed) ->
                // A second identical zeroing must be a no-op, i.e. report "unchanged".
                match tryZero offset count zeroed with
                | Some None -> true
                | _ -> false

        Check.One (config, Prop.forAll (Arb.fromGen genValueAndRange) property)

    [<Test>]
    let ``Zeroing a range agrees with a byte-level oracle where bytes are defined`` () : unit =
        // For values that DO have a byte rendering, the existing (independently property-tested)
        // byte-write path is the oracle: zeroing a range must be writing that many zero bytes.
        let property (value : CliType, offset : int, count : int) : bool =
            match CliType.ByteAddressability value with
            | CliByteAddressability.SymbolicallyAddressable _
            | CliByteAddressability.Rejected _ -> true
            | CliByteAddressability.ByteAddressable ->
                let viaBytes = CliType.WithBytesAtIfChanged offset (Array.zeroCreate count) value

                match tryZero offset count value with
                | None -> false // must not fail on byte-addressable storage
                | Some result ->
                    match result, viaBytes with
                    | None, None -> true
                    | Some a, Some b -> CliType.ToBytes a = CliType.ToBytes b
                    // One reported "changed" and the other didn't: only consistent if the
                    // rendered bytes agree anyway.
                    | Some a, None -> CliType.ToBytes a = CliType.ToBytes value
                    | None, Some b -> CliType.ToBytes b = CliType.ToBytes value

        Check.One (config, Prop.forAll (Arb.fromGen genValueAndRange) property)

    [<Test>]
    let ``Zeroing a pointer slot of a reference-containing struct`` () : unit =
        // The concrete shapes that motivated the operation, at the exact ranges
        // ClearWithReferences produces for them.
        let heapRef = CliType.ObjectRef (Some (ManagedHeapAddress 11))

        let field (name : string) (value : CliType) : CliType =
            match value with
            | CliType.ValueType vt -> CliValueType.DereferenceFieldById (FieldId.named name) vt
            | other -> failwith $"expected a value type, got %O{other}"

        // struct { int N; Box B; } -- auto layout hoists the reference, so slot [0,8) is the ref
        // and the int sits at [8,12) with padding out to 16.
        let mixed =
            mixedReferenceValueType ()
            |> CliValueType.WithFieldSet "N" (CliType.Numeric (CliNumericType.Int32 7))
            |> CliValueType.WithFieldSet "B" heapRef
            |> CliType.ValueType

        match CliType.WithZeroedRangeIfChanged 0 8 mixed with
        | None -> failwith "expected the low slot of a populated mixed struct to change"
        | Some result ->
            // The reference is cleared; the int in the *other* slot is untouched.
            field "B" result |> shouldEqual (CliType.ObjectRef None)
            field "N" result |> shouldEqual (CliType.Numeric (CliNumericType.Int32 7))

        match CliType.WithZeroedRangeIfChanged 8 8 mixed with
        | None -> failwith "expected the high slot of a populated mixed struct to change"
        | Some result ->
            field "B" result |> shouldEqual heapRef
            field "N" result |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

        // A reference buried in a by-value field, so that it lands in the HIGH slot.
        let nestedRef (value : CliType) : CliType =
            match value with
            | CliType.ValueType w -> CliValueType.DereferenceFieldById (FieldId.named "B") w
            | other -> failwith $"expected a value type, got %O{other}"

        let refHigh =
            referenceHighSlotValueType ()
            |> CliValueType.WithFieldSet "L" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 9L)))
            |> CliValueType.WithFieldSet
                "W"
                (referenceWrapperValueType ()
                 |> CliValueType.WithFieldSet "B" heapRef
                 |> CliType.ValueType)
            |> CliType.ValueType

        match CliType.WithZeroedRangeIfChanged 0 8 refHigh with
        | None -> failwith "expected the low slot of a populated high-reference struct to change"
        | Some result ->
            field "L" result
            |> shouldEqual (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))

            nestedRef (field "W" result) |> shouldEqual heapRef

        match CliType.WithZeroedRangeIfChanged 8 8 refHigh with
        | None -> failwith "expected the high slot of a populated high-reference struct to change"
        | Some result ->
            field "L" result
            |> shouldEqual (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 9L)))

            nestedRef (field "W" result) |> shouldEqual (CliType.ObjectRef None)

    [<Test>]
    let ``Zeroing refuses to half-clear a reference`` () : unit =
        let heapRef = CliType.ObjectRef (Some (ManagedHeapAddress 11))

        // A bare reference cell, half covered.
        (fun () -> CliType.WithZeroedRangeIfChanged 0 4 heapRef |> ignore)
        |> shouldFail<exn>

        // A misaligned explicit-layout reference field, straddled by a pointer slot. This is the
        // shape PawPrint accepts but CoreCLR rejects at type load, so it must fail loudly rather
        // than silently drop or keep the reference.
        let misaligned =
            misalignedReferenceValueType ()
            |> CliValueType.WithFieldSet "Obj" heapRef
            |> CliType.ValueType

        (fun () -> CliType.WithZeroedRangeIfChanged 0 8 misaligned |> ignore)
        |> shouldFail<exn>

        // Zeroing a range that misses it entirely is still fine.
        CliType.WithZeroedRangeIfChanged 11 5 misaligned |> ignore

    [<Test>]
    let ``Zeroing a negative zero is a change`` () : unit =
        // Regression: structural equality on floats is IEEE, so `-0.0 = 0.0` is true. Deciding
        // "did zeroing change anything?" with `=` therefore reports a cell holding `-0.0` as
        // already-zero and leaves the sign bit set, even though every byte that matters
        // differs. Caught by the signed-zero property in TestMethodTableProjection,
        // which writes `+0.0` over `-0.0` through the array byte-write path this operation
        // serves; pinned here too, at the operation itself.
        let negativeZero = CliType.Numeric (CliNumericType.Float64 -0.0)

        match CliType.WithZeroedRangeIfChanged 0 8 negativeZero with
        | None -> failwith "zeroing -0.0 must report a change: its sign bit is set"
        | Some zeroed ->
            CliType.ToBytes zeroed
            |> shouldEqual (CliType.ToBytes (CliType.Numeric (CliNumericType.Float64 0.0)))

        // Same story one level down, for a float field inside a struct.
        let structWithNegativeZero =
            SynthesisedLayoutKind.ofFields
                bct
                allCt
                declaredHandle
                Layout.Default
                CharSet.Ansi
                [ cliField "D" negativeZero None doubleHandle ]
            |> CliType.ValueType

        match
            CliType.WithZeroedRangeIfChanged 0 (CliType.SizeOf structWithNegativeZero).Size structWithNegativeZero
        with
        | None -> failwith "zeroing a struct holding -0.0 must report a change"
        | Some zeroed -> CliType.ToBytes zeroed |> Array.forall (fun b -> b = 0uy) |> shouldEqual true

        // And a positive zero really is unchanged, so the check hasn't just been made vacuous.
        CliType.WithZeroedRangeIfChanged 0 8 (CliType.Numeric (CliNumericType.Float64 0.0))
        |> shouldEqual None

    [<Test>]
    let ``Zeroing a union whose members overlap a reference clears every member`` () : unit =
        // Explicit layout lets a reference share bytes with a sibling. Fields are classified one
        // at a time against the range, independently of each other, so every member of the union
        // must be zeroed when the range covers those bytes -- otherwise the "no untouched field
        // can overlap the zeroed region" invariant that makes replay order irrelevant would not
        // hold, and the stale member would win the `ToBytes` overlay.
        //
        // Asserted through the rendered bytes rather than by reading members back: projecting
        // any member of a union that overlaps a live reference reconstructs it from the union's
        // bytes, and a reference has no byte rendering. That is a limit of the
        // value model, unrelated to zeroing.
        let heapRef = CliType.ObjectRef (Some (ManagedHeapAddress 13))

        let populated =
            overlappingReferenceUnionValueType ()
            |> CliValueType.WithFieldSet "Obj" heapRef
            |> CliValueType.WithFieldSet "Tail" (CliType.Numeric (CliNumericType.Int32 5))
            |> CliType.ValueType

        // Zeroing the union's own 8 bytes clears the reference AND its aliasing sibling, so the
        // whole value renders again; the disjoint tail keeps its 5.
        match CliType.WithZeroedRangeIfChanged 0 8 populated with
        | None -> failwith "expected zeroing a populated union to change it"
        | Some result ->
            let bytes = CliType.ToBytes result
            bytes.[0..7] |> Array.forall (fun b -> b = 0uy) |> shouldEqual true
            bytes.[8] |> shouldEqual 5uy

        // Zeroing the whole thing is the same as ZeroLike, tail included.
        match CliType.WithZeroedRangeIfChanged 0 16 populated with
        | None -> failwith "expected zeroing the whole union to change it"
        | Some result ->
            result |> shouldEqual (CliType.ZeroLike populated)
            CliType.ToBytes result |> Array.forall (fun b -> b = 0uy) |> shouldEqual true

    [<Test>]
    let ``Zeroing part of an overlapping field leaves bytes outside the range alone`` () : unit =
        // `ToBytes` replays overlapping fields in `EditedAtTime` order. A field that only
        // *partially* overlaps the requested range extends outside it, so promoting it to
        // "newest" would change who wins on bytes the call was never asked to touch.
        //
        // Layout: a 16-byte nested struct at [0,16), aliased over its upper half by an 8-byte
        // field at [8,16) that was written later. Zeroing [0,8) must not let the nested
        // struct's stale upper half overwrite the alias.
        let nested =
            SynthesisedLayoutKind.ofFields
                bct
                allCt
                declaredHandle
                Layout.Default
                CharSet.Ansi
                [
                    cliField "Lo" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
                    cliField "Hi" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) None int64Handle
                ]

        let outer =
            SynthesisedLayoutKind.ofFields
                bct
                allCt
                declaredHandle
                (Layout.Custom (size = 16, packingSize = 0))
                CharSet.Ansi
                [
                    cliField "Nested" (nested |> CliType.ValueType) (Some 0) nested.Declared
                    cliField
                        "Alias"
                        (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                        (Some 8)
                        int64Handle
                ]

        let populated =
            outer
            |> CliValueType.WithFieldSet
                "Nested"
                (nested
                 |> CliValueType.WithFieldSet
                     "Lo"
                     (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x1111111111111111L)))
                 |> CliValueType.WithFieldSet
                     "Hi"
                     (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x2222222222222222L)))
                 |> CliType.ValueType)
            // Written last, so the alias owns [8,16) in the replay.
            |> CliValueType.WithFieldSet
                "Alias"
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x3333333333333333L)))
            |> CliType.ValueType

        let before = CliType.ToBytes populated
        before.[8..15] |> shouldEqual (Array.create 8 0x33uy)

        match CliType.WithZeroedRangeIfChanged 0 8 populated with
        | None -> failwith "expected zeroing the low half to change the value"
        | Some result ->
            let after = CliType.ToBytes result
            // Requested range cleared...
            after.[0..7] |> Array.forall (fun b -> b = 0uy) |> shouldEqual true
            // ...and everything outside it byte-for-byte as it was.
            after.[8..15] |> shouldEqual before.[8..15]

    [<Test>]
    let ``Zeroing a negative zero inside an unrenderable struct is a change`` () : unit =
        // The struct has no byte rendering of its own because of the pointer field, so the
        // "compare rendered bytes" arm does not apply and the comparison has to descend. If it
        // compared the aggregates structurally instead, IEEE equality would call the `-0.0`
        // field already-zero and the whole-cell clear would report "unchanged", leaving the
        // sign bit set.
        let pointerField =
            CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed int32Handle))

        let vt =
            SynthesisedLayoutKind.ofFields
                bct
                allCt
                declaredHandle
                Layout.Default
                CharSet.Ansi
                [
                    cliField "P" pointerField None intPtrHandle
                    cliField "D" (CliType.Numeric (CliNumericType.Float64 -0.0)) None doubleHandle
                ]
            |> CliType.ValueType

        match CliType.ByteAddressability vt with
        | CliByteAddressability.Rejected _ -> ()
        | other -> failwith $"test premise broken: this struct is supposed to have no byte rendering, got %O{other}"

        match CliType.WithZeroedRangeIfChanged 0 (CliType.SizeOf vt).Size vt with
        | None ->
            failwith "zeroing a struct holding -0.0 must report a change even when the struct has no byte rendering"
        | Some zeroed -> zeroed |> shouldEqual (CliType.ZeroLike vt)

    [<Test>]
    let ``Zeroing rejects ranges whose end would overflow`` () : unit =
        // `offset + count` wraps negative for large inputs, so a guard phrased in terms of the
        // computed end silently accepts an out-of-range request instead of rejecting it.
        let value = CliType.Numeric (CliNumericType.Int32 0x11223344)

        (fun () -> CliType.WithZeroedRangeIfChanged System.Int32.MaxValue 2 value |> ignore)
        |> shouldFail<exn>

        (fun () -> CliType.WithZeroedRangeIfChanged 2 System.Int32.MaxValue value |> ignore)
        |> shouldFail<exn>

        (fun () -> CliType.WithZeroedRangeIfChanged 0 5 value |> ignore)
        |> shouldFail<exn>

        (fun () -> CliType.WithZeroedRangeIfChanged -1 1 value |> ignore)
        |> shouldFail<exn>

        (fun () -> CliType.WithZeroedRangeIfChanged 0 -1 value |> ignore)
        |> shouldFail<exn>

        // The whole 4-byte range is still legal.
        CliType.WithZeroedRangeIfChanged 0 4 value |> Option.isSome |> shouldEqual true

    // ------------------------------------------------------------------
    // Named bytes over a native int PawPrint models as an identity.
    //
    // `SignatureHelper.InternalAddRuntimeType` copies the eight bytes of `type.TypeHandle.Value`
    // into a `Reflection.Emit` signature blob one at a time, so the shape below -- an `IntPtr`
    // whose single field is a type handle -- has to answer a byte view without any bits being
    // invented for it.
    // ------------------------------------------------------------------

    let private typeHandleTarget : RuntimeTypeHandleTarget =
        RuntimeTypeHandleTarget.Closed int32Handle

    let private typeHandleSource : NativeIntSource =
        NativeIntSource.TypeHandlePtr typeHandleTarget

    /// An `IntPtr`-shaped struct whose single `_value` field holds a type handle.
    let private typeHandleIntPtr () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi
            [
                cliField "_value" (CliType.Numeric (CliNumericType.NativeInt typeHandleSource)) (Some 0) intPtrHandle
            ]

    /// Eight bytes of handle at offset 0, then four ordinary bytes at offset 8, so that a slice
    /// can miss the handle entirely and a slice that straddles the boundary can be checked.
    let private handleThenIntValueType () : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 12, packingSize = 0))
            CharSet.Ansi
            [
                cliField "_value" (CliType.Numeric (CliNumericType.NativeInt typeHandleSource)) (Some 0) intPtrHandle
                cliField "Tail" (CliType.Numeric (CliNumericType.Int32 0x11223344)) (Some 8) int32Handle
            ]

    [<Test>]
    let ``A native int carrying an identity is symbolically addressable, not rejected`` () : unit =
        CliType.ByteAddressability (CliType.Numeric (CliNumericType.NativeInt typeHandleSource))
        |> shouldEqual (
            CliByteAddressability.SymbolicallyAddressable (
                CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable typeHandleSource
            )
        )

    [<Test>]
    let ``A struct whose only obstruction is such a native int is symbolically addressable`` () : unit =
        match CliType.ByteAddressability (typeHandleIntPtr () |> CliType.ValueType) with
        | CliByteAddressability.SymbolicallyAddressable (CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField (_,
                                                                                                                                  field,
                                                                                                                                  inner)) ->
            field |> shouldEqual (FieldId.named "_value")

            inner
            |> shouldEqual (CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable typeHandleSource)
        | other -> failwith $"expected a symbolically-addressable struct, got %O{other}"

    [<Test>]
    let ``An object reference is still rejected outright, not named`` () : unit =
        // The discriminating control for the classifier: a reference has no byte image *at all*,
        // so widening "name it instead of refusing" to cover every non-addressable value would
        // promise a byte for something that has none.
        match CliType.ByteAddressability (CliType.ObjectRef None) with
        | CliByteAddressability.Rejected CliByteAddressabilityRejection.ObjectReference -> ()
        | other -> failwith $"expected an object reference to stay rejected, got %O{other}"

        match CliType.ByteAddressability (objectReferenceValueType () |> CliType.ValueType) with
        | CliByteAddressability.Rejected (CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _) -> ()
        | other -> failwith $"expected a reference-containing struct to stay rejected, got %O{other}"

    [<Test>]
    let ``A rejected field dominates a nameable one`` () : unit =
        // Order matters here: the nameable field comes first, so an implementation that reports
        // whichever obstruction it met first would call this whole struct nameable and then owe a
        // `UInt8Source` for the reference's bytes, which do not exist.
        let mixed =
            SynthesisedLayoutKind.ofFields
                bct
                allCt
                declaredHandle
                (Layout.Custom (size = 16, packingSize = 0))
                CharSet.Ansi
                [
                    cliField
                        "_value"
                        (CliType.Numeric (CliNumericType.NativeInt typeHandleSource))
                        (Some 0)
                        intPtrHandle
                    cliField "Obj" (CliType.ObjectRef None) (Some 8) objectHandle
                ]

        match CliType.ByteAddressability (mixed |> CliType.ValueType) with
        | CliByteAddressability.Rejected (CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _) -> ()
        | other -> failwith $"expected the object reference to dominate, got %O{other}"

    [<Test>]
    let ``SymbolicBytesAt names every byte of a type handle, in ascending order`` () : unit =
        // The primary anchor. Both the source and the *index* are asserted, because an
        // implementation that named all eight bytes identically would round-trip through a
        // decoder that only checked the source, and would then be unable to tell a handle from
        // its own bytes reversed.
        let bytes = CliType.SymbolicBytesAt 0 8 (typeHandleIntPtr () |> CliType.ValueType)

        bytes
        |> shouldEqual (Array.init 8 (fun i -> UInt8Source.NativeIntByte (typeHandleSource, i)))

    [<Test>]
    let ``SymbolicBytesAt reports the index within the native int, not within the slice`` () : unit =
        // A slice starting part-way into the handle must still say which byte of the *handle* each
        // one is: the decoder reassembles by index, so an index rebased on the slice would name
        // byte 0 of a handle whose byte 0 is elsewhere.
        CliType.SymbolicBytesAt 3 4 (typeHandleIntPtr () |> CliType.ValueType)
        |> shouldEqual (Array.init 4 (fun i -> UInt8Source.NativeIntByte (typeHandleSource, i + 3)))

    [<Test>]
    let ``SymbolicBytesAt over a bare native int indexes from the start of the handle`` () : unit =
        // The value here *is* the native int, with no struct around it, which is the shape a byref
        // to a bare `nint` local names. The offset is the caller's, so the index has to be rebased
        // onto the handle: an index counted from the start of the slice would call this byte 0.
        let bare = CliType.Numeric (CliNumericType.NativeInt typeHandleSource)

        CliType.SymbolicBytesAt 5 3 bare
        |> shouldEqual (Array.init 3 (fun i -> UInt8Source.NativeIntByte (typeHandleSource, i + 5)))

        CliType.SymbolicBytesAt 0 8 bare
        |> shouldEqual (Array.init 8 (fun i -> UInt8Source.NativeIntByte (typeHandleSource, i)))

    [<Test>]
    let ``SymbolicBytesAt indexes a handle from the field's own start, not the struct's`` () : unit =
        // A handle at a non-zero field offset: the byte at struct offset 9 is byte *1* of the
        // handle, not byte 9 of it. With the handle at offset 0 the two are indistinguishable,
        // which is why this struct puts an int in front of it.
        let intThenHandle =
            SynthesisedLayoutKind.ofFields
                bct
                allCt
                declaredHandle
                (Layout.Custom (size = 16, packingSize = 0))
                CharSet.Ansi
                [
                    cliField "Head" (CliType.Numeric (CliNumericType.Int32 0x11223344)) (Some 0) int32Handle
                    cliField
                        "_value"
                        (CliType.Numeric (CliNumericType.NativeInt typeHandleSource))
                        (Some 8)
                        intPtrHandle
                ]
            |> CliType.ValueType

        CliType.SymbolicBytesAt 8 8 intThenHandle
        |> shouldEqual (Array.init 8 (fun i -> UInt8Source.NativeIntByte (typeHandleSource, i)))

        CliType.SymbolicBytesAt 9 2 intThenHandle
        |> shouldEqual
            [|
                UInt8Source.NativeIntByte (typeHandleSource, 1)
                UInt8Source.NativeIntByte (typeHandleSource, 2)
            |]

    [<Test>]
    let ``SymbolicBytesAt is verbatim where the slice misses the handle`` () : unit =
        let value = handleThenIntValueType () |> CliType.ValueType

        CliType.SymbolicBytesAt 8 4 value
        |> shouldEqual (System.BitConverter.GetBytes 0x11223344 |> Array.map UInt8Source.Verbatim)

    [<Test>]
    let ``SymbolicBytesAt spans the boundary between named and verbatim bytes`` () : unit =
        let value = handleThenIntValueType () |> CliType.ValueType
        let tail = System.BitConverter.GetBytes 0x11223344

        CliType.SymbolicBytesAt 6 4 value
        |> shouldEqual
            [|
                UInt8Source.NativeIntByte (typeHandleSource, 6)
                UInt8Source.NativeIntByte (typeHandleSource, 7)
                UInt8Source.Verbatim tail.[0]
                UInt8Source.Verbatim tail.[1]
            |]

    [<Test>]
    let ``SymbolicBytesAt refuses a value with no byte image at all`` () : unit =
        (fun () -> CliType.SymbolicBytesAt 0 8 (CliType.ObjectRef None) |> ignore)
        |> shouldFail<exn>

        (fun () ->
            CliType.SymbolicBytesAt 0 8 (objectReferenceValueType () |> CliType.ValueType)
            |> ignore
        )
        |> shouldFail<exn>

    [<Test>]
    let ``BytesAt still refuses a value whose bytes are only nameable`` () : unit =
        // The currency of every existing caller is `byte[]`, and a named byte is not a number, so
        // nothing that was refused before starts succeeding.
        (fun () -> CliType.BytesAt 0 8 (typeHandleIntPtr () |> CliType.ValueType) |> ignore)
        |> shouldFail<exn>

        (fun () ->
            CliType.BytesAt 0 8 (CliType.Numeric (CliNumericType.NativeInt typeHandleSource))
            |> ignore
        )
        |> shouldFail<exn>

    [<Test>]
    let ``SymbolicBytesAt agrees with BytesAt wherever BytesAt succeeds`` () : unit =
        // The oracle: for anything that does have a byte image, naming must not change what the
        // bytes are, only how they are spelled.
        let property (value : CliType) (offset : int) (count : int) : bool =
            let size = CliType.sizeOf value
            let offset = if size = 0 then 0 else abs offset % size

            let count =
                if size - offset <= 0 then
                    0
                else
                    abs count % (size - offset) + 1

            let expected = CliType.BytesAt offset count value |> Array.map UInt8Source.Verbatim
            CliType.SymbolicBytesAt offset count value = expected

        let config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

        let gen =
            gen {
                let! value = genPrimitiveCliType
                let! offset = Gen.choose (0, 64)
                let! count = Gen.choose (0, 64)
                return value, offset, count
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) (fun (v, o, c) -> property v o c))
