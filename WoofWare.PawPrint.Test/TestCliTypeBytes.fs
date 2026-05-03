namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
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

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

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
        }

    let private genPrimitiveNumeric : Gen<CliNumericType> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<sbyte> |> Gen.map CliNumericType.Int8
                ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map CliNumericType.UInt8
                ArbMap.defaults |> ArbMap.generate<int16> |> Gen.map CliNumericType.Int16
                ArbMap.defaults |> ArbMap.generate<uint16> |> Gen.map CliNumericType.UInt16
                ArbMap.defaults |> ArbMap.generate<int32> |> Gen.map CliNumericType.Int32
                ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CliNumericType.Int64
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
        CliValueType.OfFields bct allCt declaredHandle (Layout.Custom (size = size, packingSize = 0)) []
        |> CliType.ValueType

    let private fieldBackedValueType (value : int32) : CliValueType =
        let field =
            cliField "Value" (CliType.Numeric (CliNumericType.Int32 value)) None int32Handle

        CliValueType.OfFields bct allCt declaredHandle Layout.Default [ field ]

    let private explicitUnionValueType (asInt : int32) : CliValueType =
        let asIntField =
            cliField "AsInt" (CliType.Numeric (CliNumericType.Int32 asInt)) (Some 0) int32Handle

        let byte0 =
            cliField "Byte0" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 0) byteHandle

        let byte1 =
            cliField "Byte1" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 1) byteHandle

        let byte2 =
            cliField "Byte2" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 2) byteHandle

        let byte3 =
            cliField "Byte3" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 3) byteHandle

        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 4, packingSize = 0))
            [ asIntField ; byte0 ; byte1 ; byte2 ; byte3 ]

    let private paddedValueType () : CliValueType =
        let byteField =
            cliField "Byte" (CliType.Numeric (CliNumericType.UInt8 0uy)) None byteHandle

        let intField =
            cliField "Int" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle

        CliValueType.OfFields bct allCt declaredHandle Layout.Default [ byteField ; intField ]

    let private nestedUnionValueType () : CliValueType =
        let inner =
            cliField "Inner" (explicitUnionValueType 0 |> CliType.ValueType) (Some 0) declaredHandle

        let asLong =
            cliField "AsLong" (CliType.Numeric (CliNumericType.Int64 0L)) (Some 0) int64Handle

        let upper =
            cliField "UpperInt" (CliType.Numeric (CliNumericType.Int32 0)) (Some 4) int32Handle

        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            [ inner ; asLong ; upper ]

    let private outerOverInnerPaddingValueType () : CliValueType =
        let inner =
            cliField "Inner" (paddedValueType () |> CliType.ValueType) (Some 0) declaredHandle

        let other =
            cliField "Other" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 1) byteHandle

        CliValueType.OfFields bct allCt declaredHandle (Layout.Custom (size = 8, packingSize = 0)) [ inner ; other ]

    let private trailingStorageValueType () : CliValueType =
        let prefix =
            cliField "Prefix" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle

        CliValueType.OfFields bct allCt declaredHandle (Layout.Custom (size = 8, packingSize = 0)) [ prefix ]

    let private explicitOverlapWithTailValueType () : CliValueType =
        let whole =
            cliField "Whole" (CliType.Numeric (CliNumericType.Int64 0L)) (Some 0) int64Handle

        let low =
            cliField "Low" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle

        CliValueType.OfFields bct allCt declaredHandle (Layout.Custom (size = 8, packingSize = 0)) [ whole ; low ]

    let private fieldBackedBoolValueType () : CliType =
        let field = cliField "Flag" (CliType.Bool 0uy) None boolHandle

        CliValueType.OfFields bct allCt declaredHandle Layout.Default [ field ]
        |> CliType.ValueType

    let private objectReferenceValueType () : CliValueType =
        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            [ cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle ]

    let private runtimePointerValueType () : CliValueType =
        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 8, packingSize = 0))
            [
                cliField
                    "Ptr"
                    (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr int32Handle))
                    (Some 0)
                    intPtrHandle
            ]

    let private nestedObjectReferenceValueType () : CliValueType =
        let inner =
            let innerValueType = objectReferenceValueType ()
            cliField "Inner" (innerValueType |> CliType.ValueType) (Some 0) innerValueType.Declared

        CliValueType.OfFields bct allCt int64Handle (Layout.Custom (size = 8, packingSize = 0)) [ inner ]

    let private objectAndRuntimePointerValueType () : CliValueType =
        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = 16, packingSize = 0))
            [
                cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle
                cliField
                    "Ptr"
                    (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr int32Handle))
                    (Some 8)
                    intPtrHandle
            ]

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
                Gen.constant (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr int32Handle))
                Gen.constant (objectReferenceValueType () |> CliType.ValueType)
                Gen.constant (runtimePointerValueType () |> CliType.ValueType)
                Gen.constant (nestedObjectReferenceValueType () |> CliType.ValueType)
                Gen.constant (objectAndRuntimePointerValueType () |> CliType.ValueType)
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

        System.Object.ReferenceEquals (CliType.WithBytesAt 0 originalBytes value, value)
        |> shouldEqual true

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

        CliType.ByteAddressability (CliType.ObjectRef None)
        |> shouldEqual (CliByteAddressability.Rejected CliByteAddressabilityRejection.ObjectReference)

        CliType.ByteAddressability (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr int32Handle))
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
    let ``ByteAddressability agrees with reference-like containment`` () : unit =
        let mutable byteAddressableCount = 0
        let mutable rejectedCount = 0

        let property (value : CliType) : unit =
            let expected =
                not (CliType.ContainsObjectReferences value)
                && not (CliType.ContainsRuntimePointers value)

            match CliType.ByteAddressability value with
            | CliByteAddressability.ByteAddressable ->
                byteAddressableCount <- byteAddressableCount + 1
                expected |> shouldEqual true
            | CliByteAddressability.Rejected _ ->
                rejectedCount <- rejectedCount + 1
                expected |> shouldEqual false

        Check.One (config, Prop.forAll (Arb.fromGen genByteAddressabilityCliType) property)
        byteAddressableCount > 0 |> shouldEqual true
        rejectedCount > 0 |> shouldEqual true

    [<Test>]
    let ``byteAtOffset rejects byte-unaddressable values with clear diagnostics`` () : unit =
        let cases =
            [
                CliType.ObjectRef None, "object reference"
                CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr int32Handle), "runtime pointer"
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

            ex.Message
            |> shouldContainText "refusing to byte-compare byte-unaddressable value"

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
    let ``WithBytesAt updates padded field-backed value types and preserves padding`` () : unit =
        let property (initialBytes : byte[]) (replacementSource : byte[]) : unit =
            let template = paddedValueType ()
            let recovered = CliValueType.OfBytesLike template initialBytes

            let representedRanges =
                [
                    0, 1

                    for offset = 4 to initialBytes.Length - 1 do
                        for count = 1 to initialBytes.Length - offset do
                            offset, count
                ]

            for offset, count in representedRanges do
                let replacement = Array.zeroCreate<byte> count
                Array.blit replacementSource offset replacement 0 count

                let expected = Array.copy initialBytes
                Array.blit replacement 0 expected offset replacement.Length

                let updated = CliValueType.WithBytesAt offset replacement recovered

                CliValueType.ToBytes updated |> shouldEqual expected
                CliValueType.BytesAt 1 3 updated |> shouldEqual initialBytes.[1..3]

                CliValueType.DereferenceField "Byte" updated
                |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 expected.[0]))

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
            CliValueType.WithFieldSet "Whole" (CliType.Numeric (CliNumericType.Int64 0x0102030405060708L)) template

        let value =
            CliValueType.WithFieldSet "Low" (CliType.Numeric (CliNumericType.Int32 0x11223344)) afterWhole

        let lowBytes = CliValueType.BytesAt 0 4 value

        System.Object.ReferenceEquals (CliValueType.WithBytesAt 0 lowBytes value, value)
        |> shouldEqual true

        System.Object.ReferenceEquals (CliValueType.WithBytesAt 0 Array.empty value, value)
        |> shouldEqual true

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
                    |> shouldEqual (CliType.Numeric (CliNumericType.Int64 (System.BitConverter.ToInt64 (expected, 0))))

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
                |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 expectedBytes.[i]))

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
            |> shouldEqual (CliType.Numeric (CliNumericType.Int64 (System.BitConverter.ToInt64 (expectedBytes, 0))))

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
            CliValueType.WithFieldSet "Byte" (CliType.Numeric (CliNumericType.UInt8 9uy)) recovered

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
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 5uy))

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
            |> shouldEqual (CliType.Numeric (CliNumericType.Int64 (System.BitConverter.ToInt64 (expected, 0))))

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
