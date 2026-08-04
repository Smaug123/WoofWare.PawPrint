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
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.UInt8 255uy)))

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
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String None) state

        result |> shouldEqual (CliType.ObjectRef None)
        System.Object.ReferenceEquals (stateAfter, state) |> shouldEqual true

    [<Test>]
    let ``toCliType: String (Some _) allocates a managed string with the right contents`` () : unit =
        let loggerFactory, state = freshState ()

        let result, stateAfter =
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String (Some "hello")) state

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
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String (Some "")) state

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
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String (Some "")) state

        let r2, _ =
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String (Some "")) state

        match r1, r2 with
        | CliType.ObjectRef (Some a1), CliType.ObjectRef (Some a2) -> a1 |> shouldEqual a2
        | _ -> failwithf "expected two ObjectRef (Some _), got %A and %A" r1 r2

    [<Test>]
    let ``toCliType: String (Some non-empty) allocates distinct objects on repeated calls`` () : unit =
        // Non-empty SerString values are not interned by CoreCLR's GetDataFromBlob;
        // each call to StringObject::NewString(psz, cBytes) produces a fresh instance.
        let loggerFactory, state = freshState ()

        let r1, state =
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String (Some "dup")) state

        let r2, _ =
            CustomAttribValueLowering.toCliType loggerFactory bct (CustomAttribFixedArg.String (Some "dup")) state

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
            ]

    /// SizeOf agrees with the canonical mapping in `CliType.zeroOfPrimitive`,
    /// so the lowering preserves the expected slot width.
    let private expectedSize (arg : CustomAttribFixedArg) : int =
        match arg with
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
