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

    [<Test>]
    let ``ToBytes output size matches SizeOf for primitive CliType values`` () : unit =
        Check.One (config, Prop.forAll (Arb.fromGen genPrimitiveCliType) toBytesSizeAgreesWithSizeOf)

    [<Test>]
    let ``OfBytesLike inverts ToBytes for primitive CliType values`` () : unit =
        Check.One (config, Prop.forAll (Arb.fromGen genPrimitiveCliType) roundTripIsIdentity)

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
    let ``OfBytesLike round-trips field-backed value types`` () : unit =
        let property (value : int32) : unit =
            let template = fieldBackedValueType 0
            let source = fieldBackedValueType value
            let expectedBytes = CliValueType.ToBytes source

            let recovered = CliType.OfBytesLike (CliType.ValueType template) expectedBytes

            CliType.ToBytes recovered |> shouldEqual expectedBytes

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.generate<int32> |> Arb.fromGen) property)

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
            // win over `Whole` while preserving the untouched high bytes from the byte snapshot.
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
