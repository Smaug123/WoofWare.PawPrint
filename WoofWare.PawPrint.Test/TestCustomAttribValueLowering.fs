namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCustomAttribValueLowering =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes
    // over its sinks, and disposing while the assembly is still live would silently drop
    // events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    /// The plan every `SerString`-valued argument below is decoded and lowered with.
    let private stringPlan : CustomAttribArgPlan =
        CustomAttribArgPlan.Primitive PrimitiveType.String

    let private handleFor (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ty

    let private byteHandle : ConcreteTypeHandle = handleFor bct.Byte
    let private int32Handle : ConcreteTypeHandle = handleFor bct.Int32
    let private stringHandle : ConcreteTypeHandle = handleFor bct.String

    let private bytePlan : CustomAttribArgPlan =
        CustomAttribArgPlan.SzArray (byteHandle, CustomAttribArgPlan.Primitive PrimitiveType.Byte)

    let private int32ArrayPlan : CustomAttribArgPlan =
        CustomAttribArgPlan.SzArray (int32Handle, CustomAttribArgPlan.Primitive PrimitiveType.Int32)

    let private stringArrayPlan : CustomAttribArgPlan =
        CustomAttribArgPlan.SzArray (stringHandle, CustomAttribArgPlan.Primitive PrimitiveType.String)

    /// `System.Security.SecurityRuleSet` is byte-underlying, so a lowering that used the enum's
    /// underlying *width* wrongly, or its underlying *type* as the array's element type, is
    /// visible: `byte` and `int32` differ in both stride and identity.
    let private securityRuleSetPlan
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        : ConcreteTypeHandle * CustomAttribArgPlan * IlMachineState
        =
        let typeInfo =
            corelib.TryGetTopLevelTypeDef "System.Security" "SecurityRuleSet"
            |> Option.defaultWith (fun () -> failwith "System.Security.SecurityRuleSet not found in corelib")

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                bct
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, System.Reflection.Metadata.SignatureTypeKind.ValueType))

        handle, CustomAttribArgPlan.SzArray (handle, CustomAttribArgPlan.Enum EnumUnderlyingType.Byte), state

    let private arrayAddress (result : CliType) : ManagedHeapAddress =
        match result with
        | CliType.ObjectRef (Some addr) -> addr
        | other -> failwithf "expected ObjectRef (Some _), got %A" other

    let private cells (addr : ManagedHeapAddress) (state : IlMachineState) : CliType list =
        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

        [ 0 .. shape.Length - 1 ]
        |> List.map (fun i -> ManagedHeap.getArrayValue addr i state.ManagedHeap)

    let private freshState () : Microsoft.Extensions.Logging.ILoggerFactory * IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = concreteTypes
            }

        loggerFactory, state

    [<Test>]
    let ``primitive: Bool false`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.Bool false)
        |> shouldEqual (Ok (CliType.Bool 0uy))

    [<Test>]
    let ``primitive: Bool true`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.Bool true)
        |> shouldEqual (Ok (CliType.Bool 1uy))

    [<Test>]
    let ``primitive: Char ASCII`` () : unit =
        // ASCII 'A' is U+0041, so high byte = 0x00, low byte = 0x41.
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.Char 'A')
        |> shouldEqual (Ok (CliType.Char (0x00uy, 0x41uy)))

    [<Test>]
    let ``primitive: Char above BMP byte`` () : unit =
        // U+4E2D ('中') has high byte 0x4E, low byte 0x2D.
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.Char '中')
        |> shouldEqual (Ok (CliType.Char (0x4Euy, 0x2Duy)))

    [<Test>]
    let ``primitive: I1 wraps -1 correctly`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.I1 -1y)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Int8 -1y)))

    [<Test>]
    let ``primitive: U1 max`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.U1 255uy)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 255uy))))

    [<Test>]
    let ``primitive: I4 round-trips negative`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.I4 -2147483648)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Int32 -2147483648)))

    [<Test>]
    let ``primitive: U4 max stored as Int32 -1 (two's complement wraparound)`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.U4 4294967295u)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Int32 -1)))

    [<Test>]
    let ``primitive: I8 round-trips`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.I8 -9223372036854775808L)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim -9223372036854775808L))))

    [<Test>]
    let ``primitive: U8 max stored as Int64 -1`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.U8 18446744073709551615UL)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim -1L))))

    [<Test>]
    let ``primitive: R4 round-trips`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.R4 3.14159f)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Float32 3.14159f)))

    [<Test>]
    let ``primitive: R8 round-trips`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.R8 2.718281828459045)
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.Float64 2.718281828459045)))

    [<Test>]
    let ``String None lowers to null object ref`` () : unit =
        CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.String None)
        |> shouldEqual (Ok (CliType.ObjectRef None))

    [<Test>]
    let ``tryToPureCliType rejects String (Some _)`` () : unit =
        match CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.String (Some "hello")) with
        | Error msg -> msg |> shouldContainText "allocation"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``tryToPureCliType rejects Array (None)`` () : unit =
        match CustomAttribValueLowering.tryToPureCliType (CustomAttribFixedArg.Array None) with
        | Error msg -> msg |> shouldContainText "Array"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``tryToPureCliType rejects Array (Some _)`` () : unit =
        let arr =
            CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.I4 1 ; CustomAttribFixedArg.I4 2 ])

        match CustomAttribValueLowering.tryToPureCliType arr with
        | Error msg -> msg |> shouldContainText "Array"
        | Ok r -> failwithf "expected Error, got Ok %A" r

    [<Test>]
    let ``toCliType: String None routes through pure path`` () : unit =
        let loggerFactory, state = freshState ()

        let result, stateAfter =
            CustomAttribValueLowering.toCliType loggerFactory bct stringPlan (CustomAttribFixedArg.String None) state

        result |> shouldEqual (CliType.ObjectRef None)
        System.Object.ReferenceEquals (stateAfter, state) |> shouldEqual true

    [<Test>]
    let ``toCliType: String (Some _) allocates a managed string with the right contents`` () : unit =
        let loggerFactory, state = freshState ()

        let result, stateAfter =
            CustomAttribValueLowering.toCliType
                loggerFactory
                bct
                stringPlan
                (CustomAttribFixedArg.String (Some "hello"))
                state

        match result with
        | CliType.ObjectRef (Some addr) ->
            ManagedHeap.getStringContents addr stateAfter.ManagedHeap
            |> shouldEqual (Some "hello")
        | other -> failwithf "expected ObjectRef (Some _), got %A" other

    [<Test>]
    let ``toCliType: String (Some "") routes through canonical empty string`` () : unit =
        // CoreCLR's StringObject::NewString(0) returns GetEmptyString(); we mirror that
        // so attribute consumers comparing against `string.Empty` / `ldstr ""` get the
        // expected reference identity. See CustomAttribValueLowering.toCliType docs.
        let loggerFactory, state = freshState ()

        let result, stateAfter =
            CustomAttribValueLowering.toCliType
                loggerFactory
                bct
                stringPlan
                (CustomAttribFixedArg.String (Some ""))
                state

        let canonicalAddr, _ =
            IlMachineState.internCanonicalEmptyString loggerFactory bct stateAfter

        match result with
        | CliType.ObjectRef (Some addr) ->
            ManagedHeap.getStringContents addr stateAfter.ManagedHeap
            |> shouldEqual (Some "")

            addr |> shouldEqual canonicalAddr
        | other -> failwithf "expected ObjectRef (Some _), got %A" other

    [<Test>]
    let ``toCliType: String (Some "") is identity-stable across repeated calls`` () : unit =
        let loggerFactory, state = freshState ()

        let r1, state =
            CustomAttribValueLowering.toCliType
                loggerFactory
                bct
                stringPlan
                (CustomAttribFixedArg.String (Some ""))
                state

        let r2, _ =
            CustomAttribValueLowering.toCliType
                loggerFactory
                bct
                stringPlan
                (CustomAttribFixedArg.String (Some ""))
                state

        match r1, r2 with
        | CliType.ObjectRef (Some a1), CliType.ObjectRef (Some a2) -> a1 |> shouldEqual a2
        | _ -> failwithf "expected two ObjectRef (Some _), got %A and %A" r1 r2

    [<Test>]
    let ``toCliType: String (Some non-empty) allocates distinct objects on repeated calls`` () : unit =
        // Non-empty SerString values are not interned by CoreCLR's GetDataFromBlob;
        // each call to StringObject::NewString(psz, cBytes) produces a fresh instance.
        let loggerFactory, state = freshState ()

        let r1, state =
            CustomAttribValueLowering.toCliType
                loggerFactory
                bct
                stringPlan
                (CustomAttribFixedArg.String (Some "dup"))
                state

        let r2, _ =
            CustomAttribValueLowering.toCliType
                loggerFactory
                bct
                stringPlan
                (CustomAttribFixedArg.String (Some "dup"))
                state

        match r1, r2 with
        | CliType.ObjectRef (Some a1), CliType.ObjectRef (Some a2) -> a1 |> shouldNotEqual a2
        | _ -> failwithf "expected two ObjectRef (Some _), got %A and %A" r1 r2

    // --- property-based primitive round-trip --------------------------------

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private genPrimitiveArg : Gen<CustomAttribFixedArg> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map CustomAttribFixedArg.Bool
                ArbMap.defaults |> ArbMap.generate<char> |> Gen.map CustomAttribFixedArg.Char
                ArbMap.defaults |> ArbMap.generate<sbyte> |> Gen.map CustomAttribFixedArg.I1
                ArbMap.defaults |> ArbMap.generate<byte> |> Gen.map CustomAttribFixedArg.U1
                ArbMap.defaults |> ArbMap.generate<int16> |> Gen.map CustomAttribFixedArg.I2
                ArbMap.defaults |> ArbMap.generate<uint16> |> Gen.map CustomAttribFixedArg.U2
                ArbMap.defaults |> ArbMap.generate<int32> |> Gen.map CustomAttribFixedArg.I4
                ArbMap.defaults |> ArbMap.generate<uint32> |> Gen.map CustomAttribFixedArg.U4
                ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CustomAttribFixedArg.I8
                ArbMap.defaults |> ArbMap.generate<uint64> |> Gen.map CustomAttribFixedArg.U8
                ArbMap.defaults
                |> ArbMap.generate<NormalFloat>
                |> Gen.map (fun (NormalFloat f) -> CustomAttribFixedArg.R4 (float32 f))
                ArbMap.defaults
                |> ArbMap.generate<NormalFloat>
                |> Gen.map (fun (NormalFloat f) -> CustomAttribFixedArg.R8 f)
                Gen.constant (CustomAttribFixedArg.String None)
                // An enum lowers to its bare underlying integer — no enum wrapper is built here —
                // so it must land in exactly the slot its underlying type would.
                ArbMap.defaults
                |> ArbMap.generate<byte>
                |> Gen.map (CustomAttribFixedArg.U1 >> CustomAttribFixedArg.Enum)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (CustomAttribFixedArg.I8 >> CustomAttribFixedArg.Enum)
            ]

    /// SizeOf agrees with the canonical mapping in `CliType.zeroOfPrimitive`,
    /// so the lowering preserves the expected slot width.
    let rec private expectedSize (arg : CustomAttribFixedArg) : int =
        match arg with
        // Deliberately the underlying type's width, not a wrapper's: the ctor's parameter slot is
        // built from the declared enum type by `callMethod`, and the value pushed here is the bare
        // integer that `toCliTypeCoerced` rewraps into it.
        | CustomAttribFixedArg.Enum underlying -> expectedSize underlying
        | CustomAttribFixedArg.Bool _ -> 1
        | CustomAttribFixedArg.Char _ -> 2
        | CustomAttribFixedArg.I1 _
        | CustomAttribFixedArg.U1 _ -> 1
        | CustomAttribFixedArg.I2 _
        | CustomAttribFixedArg.U2 _ -> 2
        | CustomAttribFixedArg.I4 _
        | CustomAttribFixedArg.U4 _ -> 4
        | CustomAttribFixedArg.I8 _
        | CustomAttribFixedArg.U8 _ -> 8
        | CustomAttribFixedArg.R4 _ -> 4
        | CustomAttribFixedArg.R8 _ -> 8
        | CustomAttribFixedArg.String _ -> 8
        | CustomAttribFixedArg.Array _ ->
            failwith "expectedSize: Array is outside genPrimitiveArg's range and has no defined slot size here"

    [<Test>]
    let ``tryToPureCliType: every non-allocating arg yields the expected slot size`` () : unit =
        let property (arg : CustomAttribFixedArg) : bool =
            match CustomAttribValueLowering.tryToPureCliType arg with
            | Ok cli -> (CliType.SizeOf cli).Size = expectedSize arg
            | Error _ -> false

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPrimitiveArg) property)

    // --- SZARRAY ------------------------------------------------------------

    [<Test>]
    let ``toCliType: the null-array sentinel lowers to a null object ref`` () : unit =
        // ECMA-335 II.23.3's NumElem = 0xFFFFFFFF, which CoreCLR leaves as a null argument rather
        // than allocating an empty array.
        let loggerFactory, state = freshState ()

        let result, stateAfter =
            CustomAttribValueLowering.toCliType loggerFactory bct bytePlan (CustomAttribFixedArg.Array None) state

        result |> shouldEqual (CliType.ObjectRef None)
        System.Object.ReferenceEquals (stateAfter, state) |> shouldEqual true

    [<Test>]
    let ``toCliType: byte array allocates a byte[] with the decoded cells`` () : unit =
        let loggerFactory, state = freshState ()

        let arg =
            CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.U1 2uy ; CustomAttribFixedArg.U1 1uy ])

        let result, state =
            CustomAttribValueLowering.toCliType loggerFactory bct bytePlan arg state

        let addr = arrayAddress result

        cells addr state
        |> shouldEqual
            [
                CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 2uy))
                CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 1uy))
            ]

        // The array is a byte[], not an int[] that happens to hold small numbers.
        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

        shape.ConcreteType
        |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero byteHandle)

        ManagedHeap.getArrayElementStride addr state.ManagedHeap |> shouldEqual 1

    [<Test>]
    let ``toCliType: empty array is distinct from the null sentinel and still knows its element type`` () : unit =
        // An empty array has no cell to sample, so its element type and stride are the only things
        // recording what it is an array *of*.
        let loggerFactory, state = freshState ()

        let result, state =
            CustomAttribValueLowering.toCliType loggerFactory bct bytePlan (CustomAttribFixedArg.Array (Some [])) state

        let addr = arrayAddress result
        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap
        shape.Length |> shouldEqual 0

        shape.ConcreteType
        |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero byteHandle)

        ManagedHeap.getArrayElementStride addr state.ManagedHeap |> shouldEqual 1

        ManagedHeap.getArrayElementZero addr state.ManagedHeap
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))

    [<Test>]
    let ``toCliType: string array lowers each element, including both null and empty`` () : unit =
        let loggerFactory, state = freshState ()

        let arg =
            CustomAttribFixedArg.Array (
                Some
                    [
                        CustomAttribFixedArg.String (Some "a")
                        CustomAttribFixedArg.String None
                        CustomAttribFixedArg.String (Some "")
                    ]
            )

        let result, state =
            CustomAttribValueLowering.toCliType loggerFactory bct stringArrayPlan arg state

        let addr = arrayAddress result

        let canonicalEmpty, _ =
            IlMachineState.internCanonicalEmptyString loggerFactory bct state

        match cells addr state with
        | [ CliType.ObjectRef (Some first) ; CliType.ObjectRef None ; CliType.ObjectRef (Some third) ] ->
            ManagedHeap.getStringContents first state.ManagedHeap |> shouldEqual (Some "a")
            // The empty SerString is the canonical interned instance, as it is for a scalar arg.
            third |> shouldEqual canonicalEmpty
        | other -> failwithf "expected [non-null; null; non-null] object refs, got %A" other

    [<Test>]
    let ``toCliType: enum array has the enum as its element type, not the underlying integer`` () : unit =
        let loggerFactory, state = freshState ()
        let enumHandle, plan, state = securityRuleSetPlan loggerFactory state

        let arg =
            CustomAttribFixedArg.Array (
                Some
                    [
                        CustomAttribFixedArg.Enum (CustomAttribFixedArg.U1 1uy)
                        CustomAttribFixedArg.Enum (CustomAttribFixedArg.U1 2uy)
                    ]
            )

        let result, state =
            CustomAttribValueLowering.toCliType loggerFactory bct plan arg state

        let addr = arrayAddress result

        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

        shape.ConcreteType
        |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero enumHandle)

        // The cells hold the enum wrapper the array's stride was measured from, not the bare
        // integer the decoder produced: an enum lowers to its underlying value only where the
        // evaluation stack is going to rewrap it.
        cells addr state
        |> List.map (fun cell ->
            match cell with
            | CliType.ValueType vt ->
                vt.PrimitiveLikeKind |> shouldEqual (Some PrimitiveLikeKind.EnumLike)
                (CliValueType.PrimitiveLikeField vt).Contents
            | other -> failwithf "expected an enum value type, got %A" other
        )
        |> shouldEqual
            [
                CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 1uy))
                CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 2uy))
            ]

    [<Test>]
    let ``toCliType: nested array allocates an array of arrays`` () : unit =
        // ECMA-335's grammar admits `byte[][]` and CoreCLR's ReadArray handles it, though C#
        // forbids it in an attribute argument, so only hand-written IL gets here.
        let loggerFactory, state = freshState ()

        let plan =
            CustomAttribArgPlan.SzArray (ConcreteTypeHandle.OneDimArrayZero byteHandle, bytePlan)

        let arg =
            CustomAttribFixedArg.Array (
                Some
                    [
                        CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.U1 7uy ])
                        CustomAttribFixedArg.Array None
                    ]
            )

        let result, state =
            CustomAttribValueLowering.toCliType loggerFactory bct plan arg state

        let addr = arrayAddress result

        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

        shape.ConcreteType
        |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.OneDimArrayZero byteHandle))

        match cells addr state with
        | [ CliType.ObjectRef (Some inner) ; CliType.ObjectRef None ] ->
            (ManagedHeap.getArrayShape inner state.ManagedHeap).ConcreteType
            |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero byteHandle)

            cells inner state
            |> shouldEqual [ CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 7uy)) ]
        | other -> failwithf "expected [array; null], got %A" other

    [<Test>]
    let ``toCliType: an array value under a scalar plan fails loudly`` () : unit =
        let loggerFactory, state = freshState ()

        let arg = CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.U1 1uy ])

        let exn =
            Assert.Throws (fun () ->
                CustomAttribValueLowering.toCliType loggerFactory bct stringPlan arg state
                |> ignore
            )

        exn.Message |> shouldContainText "scalar plan"

    [<Test>]
    let ``toCliType: a scalar value under an array plan fails loudly`` () : unit =
        let loggerFactory, state = freshState ()

        let exn =
            Assert.Throws (fun () ->
                CustomAttribValueLowering.toCliType loggerFactory bct bytePlan (CustomAttribFixedArg.U1 1uy) state
                |> ignore
            )

        exn.Message |> shouldContainText "SZARRAY plan"

    [<Test>]
    let ``toCliType: array cells agree element-by-element with lowering each element alone`` () : unit =
        // Order-sensitive by construction: a reversed or off-by-one fill disagrees with the
        // per-element lowering on any list whose entries are not all equal.
        let property (values : int list) : bool =
            let loggerFactory, state = freshState ()
            use _loggerFactory = loggerFactory

            let elements = values |> List.map CustomAttribFixedArg.I4

            let result, state =
                CustomAttribValueLowering.toCliType
                    loggerFactory
                    bct
                    int32ArrayPlan
                    (CustomAttribFixedArg.Array (Some elements))
                    state

            let expected =
                elements
                |> List.map (fun element ->
                    match CustomAttribValueLowering.tryToPureCliType element with
                    | Ok cli -> cli
                    | Error msg -> failwithf "expected a pure lowering, got Error %s" msg
                )

            cells (arrayAddress result) state = expected

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int list>) property)
