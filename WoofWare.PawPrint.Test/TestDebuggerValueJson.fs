namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.Json
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The debugger's structured encoding of numbers must be exact: a client reading it back gets
/// the value the guest holds, including 64-bit integers beyond JSON's 2^53 and every float bit
/// pattern (NaN payloads, signed zeros, subnormals, infinities).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDebuggerValueJson =

    /// A number as the encoding promises to preserve it: width, signedness or nativeness, and
    /// the exact bits.
    [<RequireQualifiedAccess>]
    type private Decoded =
        | Int of bits : int * signedness : string * value : int64
        | NativeNumber of value : int64
        | Float of bits : int * native : bool * rawBits : int64

    /// Numbers are context-free, so an empty machine is enough to render them.
    let private context : DebuggerValueContext =
        {
            Assemblies = LoadedAssemblies.empty
            ConcreteTypes = AllConcreteTypes.Empty
            Heap = ManagedHeap.empty
        }

    let private render (write : Utf8JsonWriter -> unit) : JsonElement =
        use stream = new MemoryStream ()

        do
            use writer = new Utf8JsonWriter (stream)
            write writer

        use document = JsonDocument.Parse (stream.ToArray ())
        document.RootElement.Clone ()

    let private decode (json : JsonElement) : Decoded =
        match json.GetProperty("kind").GetString () with
        | "int" ->
            let bits = json.GetProperty("bits").GetInt32 ()
            let value = json.GetProperty "value"

            let value =
                if bits = 64 then
                    Int64.Parse (value.GetString (), Globalization.CultureInfo.InvariantCulture)
                else
                    value.GetInt64 ()

            Decoded.Int (bits, json.GetProperty("signedness").GetString (), value)
        | "nativeInt" ->
            let source = json.GetProperty "source"
            source.GetProperty("kind").GetString () |> shouldEqual "number"

            Decoded.NativeNumber (
                Int64.Parse (source.GetProperty("value").GetString (), Globalization.CultureInfo.InvariantCulture)
            )
        | "float" ->
            let bits = json.GetProperty("bits").GetInt32 ()
            let rawBits = Convert.ToInt64 (json.GetProperty("rawBits").GetString (), 16)
            let value = json.GetProperty "value"

            // `value` must agree with `rawBits` wherever it can express the number at all.
            let fromBits =
                if bits = 32 then
                    float (BitConverter.Int32BitsToSingle (int rawBits))
                else
                    BitConverter.Int64BitsToDouble rawBits

            match value.ValueKind with
            | JsonValueKind.Number ->
                if bits = 32 then
                    BitConverter.SingleToInt32Bits (value.GetSingle ())
                    |> shouldEqual (BitConverter.SingleToInt32Bits (float32 fromBits))
                else
                    BitConverter.DoubleToInt64Bits (value.GetDouble ())
                    |> shouldEqual (BitConverter.DoubleToInt64Bits fromBits)
            | JsonValueKind.String ->
                let expected =
                    if Double.IsNaN fromBits then
                        "NaN"
                    elif Double.IsPositiveInfinity fromBits then
                        "Infinity"
                    elif Double.IsNegativeInfinity fromBits then
                        "-Infinity"
                    else
                        failwith $"finite value %f{fromBits} was written as a string"

                value.GetString () |> shouldEqual expected
            | other -> failwith $"float value of kind %O{other}"

            Decoded.Float (bits, json.GetProperty("native").GetBoolean (), rawBits)
        | other -> failwith $"a number rendered as kind %s{other}"

    /// Full-range 64 bits: FsCheck's default integer generators stay small, and the interesting
    /// 64-bit values are the ones beyond 2^53.
    let private genInt64 : Gen<int64> =
        gen {
            let! high = Gen.choose (Int32.MinValue, Int32.MaxValue)
            let! low = Gen.choose (Int32.MinValue, Int32.MaxValue)
            return (int64 high <<< 32) ||| (int64 (uint32 low))
        }

    let private genInt32 : Gen<int32> = Gen.choose (Int32.MinValue, Int32.MaxValue)

    /// A value to render, and what reading it back must give.
    let private genCliNumeric : Gen<CliType * Decoded> =
        Gen.oneof
            [
                genInt32
                |> Gen.map (fun i ->
                    CliType.Numeric (CliNumericType.Int8 (int8 i)), Decoded.Int (8, "signed", int64 (int8 i))
                )
                genInt32
                |> Gen.map (fun i ->
                    CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim (uint8 i))),
                    Decoded.Int (8, "unsigned", int64 (uint8 i))
                )
                genInt32
                |> Gen.map (fun i ->
                    CliType.Numeric (CliNumericType.Int16 (int16 i)), Decoded.Int (16, "signed", int64 (int16 i))
                )
                genInt32
                |> Gen.map (fun i ->
                    CliType.Numeric (CliNumericType.UInt16 (uint16 i)), Decoded.Int (16, "unsigned", int64 (uint16 i))
                )
                genInt32
                |> Gen.map (fun i -> CliType.Numeric (CliNumericType.Int32 i), Decoded.Int (32, "unspecified", int64 i))
                genInt64
                |> Gen.map (fun i ->
                    CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim i)), Decoded.Int (64, "unspecified", i)
                )
                genInt64
                |> Gen.map (fun i ->
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i)), Decoded.NativeNumber i
                )
                genInt32
                |> Gen.map (fun bits ->
                    CliType.Numeric (CliNumericType.Float32 (BitConverter.Int32BitsToSingle bits)),
                    Decoded.Float (32, false, int64 (uint32 bits))
                )
                genInt64
                |> Gen.map (fun bits ->
                    CliType.Numeric (CliNumericType.Float64 (BitConverter.Int64BitsToDouble bits)),
                    Decoded.Float (64, false, bits)
                )
                genInt64
                |> Gen.map (fun bits ->
                    CliType.Numeric (CliNumericType.NativeFloat (BitConverter.Int64BitsToDouble bits)),
                    Decoded.Float (64, true, bits)
                )
            ]

    let private genEvalStackNumeric : Gen<EvalStackValue * Decoded> =
        Gen.oneof
            [
                genInt32
                |> Gen.map (fun i ->
                    EvalStackValue.Int32 (Int32Source.Verbatim i), Decoded.Int (32, "unspecified", int64 i)
                )
                genInt64
                |> Gen.map (fun i -> EvalStackValue.Int64 (Int64Source.Verbatim i), Decoded.Int (64, "unspecified", i))
                genInt64
                |> Gen.map (fun i -> EvalStackValue.NativeInt (NativeIntSource.Verbatim i), Decoded.NativeNumber i)
                genInt32
                |> Gen.map (fun bits ->
                    EvalStackValue.Float (EvalStackFloat.Single (BitConverter.Int32BitsToSingle bits)),
                    Decoded.Float (32, false, int64 (uint32 bits))
                )
                genInt64
                |> Gen.map (fun bits ->
                    EvalStackValue.Float (EvalStackFloat.Double (BitConverter.Int64BitsToDouble bits)),
                    Decoded.Float (64, false, bits)
                )
            ]

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    [<Test>]
    let ``a stored number reads back exactly from its structured encoding`` () : unit =
        let property (value : CliType, expected : Decoded) : unit =
            render (fun writer -> DebuggerValueJson.writeCliType writer context value)
            |> decode
            |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen genCliNumeric) property)

    [<Test>]
    let ``an evaluation-stack number reads back exactly from its structured encoding`` () : unit =
        let property (value : EvalStackValue, expected : Decoded) : unit =
            render (fun writer -> DebuggerValueJson.writeEvalStackValue writer context value)
            |> decode
            |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen genEvalStackNumeric) property)

    /// The float32 bit patterns for which `value` is a string rather than a number, pinned, so
    /// that a generator that happened never to produce them could not hide a regression.
    [<TestCase(0x7FC00000)>]
    [<TestCase(0x7F800001)>]
    [<TestCase(0x7F800000)>]
    [<TestCase(0xFF800000)>]
    [<TestCase(0x80000000)>]
    let ``special float32 values read back exactly`` (bits : int) : unit =
        let value = BitConverter.Int32BitsToSingle bits

        render (fun writer ->
            DebuggerValueJson.writeCliType writer context (CliType.Numeric (CliNumericType.Float32 value))
        )
        |> decode
        |> shouldEqual (Decoded.Float (32, false, int64 (uint32 bits)))
