namespace WoofWare.PawPrint.Test

open System
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The `OpcodeFaults` table checked against the host runtime, for the instructions whose operands
/// are all numbers: arithmetic, comparison, shifts, conversions and `ckfinite`.
///
/// Each instruction runs as a `DynamicMethod` whose body is `ldarg`s, the instruction, `ret`. The
/// result is returned rather than popped, because the JIT is free to drop a computation whose value
/// nothing reads. On a grid of boundary operands, what the host raises must be exactly what the
/// table lists: nothing it leaves out, which would make an analysis reading the table unsound, and
/// nothing it lists needlessly. Beyond the grid, a property checks that random operands raise only
/// what the table lists.
///
/// The host is the oracle here, so a run checks the code the JIT emits for the host's own
/// architecture.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpcodeFaultsOnHost =

    /// An operand as the host passes it. Each is one of the stack types of ECMA-335 III.1.5, except
    /// that `Float32` and `Float64` are both its F.
    [<RequireQualifiedAccess>]
    type Operand =
        | Int32
        | Int64
        | NativeInt
        | Float32
        | Float64

    /// Which operand types an instruction accepts, and what it pushes, per the tables of ECMA-335
    /// III.1.5. Only those are ever emitted: ill-typed IL can kill the host outright rather than
    /// raising `InvalidProgramException`, as `ckfinite` on an int32 does on .NET 10 arm64.
    [<RequireQualifiedAccess>]
    type Typing =
        /// Table III.2: `add`, `sub`, `mul`, `div`, `rem`.
        | BinaryNumeric
        /// Table III.4: `ceq`, `cgt`, `cgt.un`, `clt`, `clt.un`, which push an int32.
        | Comparison
        /// Tables III.5 and III.7: `and`, `or`, `xor`, `div.un`, `rem.un`, and the `*.ovf*` arithmetic.
        | BinaryInteger
        /// Table III.6: `shl`, `shr`, `shr.un`.
        | Shift
        /// Table III.3: `neg`.
        | UnaryNumeric
        /// `not`, from Table III.5.
        | UnaryInteger
        /// Table III.8, pushing this.
        | Conversion of Operand
        /// `ckfinite`, which takes F alone (III.3.19).
        | FloatCheck

    let private integers : Operand list =
        [ Operand.Int32 ; Operand.Int64 ; Operand.NativeInt ]

    let private floats : Operand list = [ Operand.Float32 ; Operand.Float64 ]

    /// The integer operand pairs that Tables III.2, III.4, III.5 and III.7 all accept, with what
    /// they push.
    let private integerPairs : (Operand list * Operand) list =
        [
            [ Operand.Int32 ; Operand.Int32 ], Operand.Int32
            [ Operand.Int32 ; Operand.NativeInt ], Operand.NativeInt
            [ Operand.Int64 ; Operand.Int64 ], Operand.Int64
            [ Operand.NativeInt ; Operand.Int32 ], Operand.NativeInt
            [ Operand.NativeInt ; Operand.NativeInt ], Operand.NativeInt
        ]

    let private floatPairs : (Operand list * Operand) list =
        [
            for a in floats do
                for b in floats do
                    [ a ; b ], Operand.Float64
        ]

    /// Every operand list the typing accepts, with what the instruction pushes. An F result is
    /// returned as a float64.
    let private signatures (typing : Typing) : (Operand list * Operand) list =
        let floatAsF (operand : Operand) : Operand =
            match operand with
            | Operand.Float32 -> Operand.Float64
            | other -> other

        match typing with
        | Typing.BinaryNumeric -> integerPairs @ floatPairs
        | Typing.Comparison ->
            integerPairs @ floatPairs
            |> List.map (fun (operands, _) -> operands, Operand.Int32)
        | Typing.BinaryInteger -> integerPairs
        | Typing.Shift ->
            [
                for value in integers do
                    for amount in [ Operand.Int32 ; Operand.NativeInt ] do
                        [ value ; amount ], value
            ]
        | Typing.UnaryNumeric -> integers @ floats |> List.map (fun o -> [ o ], floatAsF o)
        | Typing.UnaryInteger -> integers |> List.map (fun o -> [ o ], o)
        | Typing.Conversion result -> integers @ floats |> List.map (fun o -> [ o ], floatAsF result)
        | Typing.FloatCheck -> floats |> List.map (fun o -> [ o ], Operand.Float64)

    /// The instructions this fixture checks, each with the host's encoding of it and its typing.
    let private checkedHere : (NullaryIlOp * OpCode * Typing) list =
        [
            NullaryIlOp.Add, OpCodes.Add, Typing.BinaryNumeric
            NullaryIlOp.Sub, OpCodes.Sub, Typing.BinaryNumeric
            NullaryIlOp.Mul, OpCodes.Mul, Typing.BinaryNumeric
            NullaryIlOp.Div, OpCodes.Div, Typing.BinaryNumeric
            NullaryIlOp.Rem, OpCodes.Rem, Typing.BinaryNumeric
            NullaryIlOp.Ceq, OpCodes.Ceq, Typing.Comparison
            NullaryIlOp.Cgt, OpCodes.Cgt, Typing.Comparison
            NullaryIlOp.Cgt_un, OpCodes.Cgt_Un, Typing.Comparison
            NullaryIlOp.Clt, OpCodes.Clt, Typing.Comparison
            NullaryIlOp.Clt_un, OpCodes.Clt_Un, Typing.Comparison
            NullaryIlOp.Div_un, OpCodes.Div_Un, Typing.BinaryInteger
            NullaryIlOp.Rem_un, OpCodes.Rem_Un, Typing.BinaryInteger
            NullaryIlOp.And, OpCodes.And, Typing.BinaryInteger
            NullaryIlOp.Or, OpCodes.Or, Typing.BinaryInteger
            NullaryIlOp.Xor, OpCodes.Xor, Typing.BinaryInteger
            NullaryIlOp.Add_ovf, OpCodes.Add_Ovf, Typing.BinaryInteger
            NullaryIlOp.Add_ovf_un, OpCodes.Add_Ovf_Un, Typing.BinaryInteger
            NullaryIlOp.Sub_ovf, OpCodes.Sub_Ovf, Typing.BinaryInteger
            NullaryIlOp.Sub_ovf_un, OpCodes.Sub_Ovf_Un, Typing.BinaryInteger
            NullaryIlOp.Mul_ovf, OpCodes.Mul_Ovf, Typing.BinaryInteger
            NullaryIlOp.Mul_ovf_un, OpCodes.Mul_Ovf_Un, Typing.BinaryInteger
            NullaryIlOp.Shl, OpCodes.Shl, Typing.Shift
            NullaryIlOp.Shr, OpCodes.Shr, Typing.Shift
            NullaryIlOp.Shr_un, OpCodes.Shr_Un, Typing.Shift
            NullaryIlOp.Neg, OpCodes.Neg, Typing.UnaryNumeric
            NullaryIlOp.Not, OpCodes.Not, Typing.UnaryInteger
            NullaryIlOp.Ckfinite, OpCodes.Ckfinite, Typing.FloatCheck
            NullaryIlOp.Conv_I, OpCodes.Conv_I, Typing.Conversion Operand.NativeInt
            NullaryIlOp.Conv_I1, OpCodes.Conv_I1, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_I2, OpCodes.Conv_I2, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_I4, OpCodes.Conv_I4, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_I8, OpCodes.Conv_I8, Typing.Conversion Operand.Int64
            NullaryIlOp.Conv_U, OpCodes.Conv_U, Typing.Conversion Operand.NativeInt
            NullaryIlOp.Conv_U1, OpCodes.Conv_U1, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_U2, OpCodes.Conv_U2, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_U4, OpCodes.Conv_U4, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_U8, OpCodes.Conv_U8, Typing.Conversion Operand.Int64
            NullaryIlOp.Conv_R4, OpCodes.Conv_R4, Typing.Conversion Operand.Float64
            NullaryIlOp.Conv_R8, OpCodes.Conv_R8, Typing.Conversion Operand.Float64
            NullaryIlOp.Conv_r_un, OpCodes.Conv_R_Un, Typing.Conversion Operand.Float64
            NullaryIlOp.Conv_ovf_i, OpCodes.Conv_Ovf_I, Typing.Conversion Operand.NativeInt
            NullaryIlOp.Conv_ovf_i1, OpCodes.Conv_Ovf_I1, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_i2, OpCodes.Conv_Ovf_I2, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_i4, OpCodes.Conv_Ovf_I4, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_i8, OpCodes.Conv_Ovf_I8, Typing.Conversion Operand.Int64
            NullaryIlOp.Conv_ovf_u, OpCodes.Conv_Ovf_U, Typing.Conversion Operand.NativeInt
            NullaryIlOp.Conv_ovf_u1, OpCodes.Conv_Ovf_U1, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_u2, OpCodes.Conv_Ovf_U2, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_u4, OpCodes.Conv_Ovf_U4, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_u8, OpCodes.Conv_Ovf_U8, Typing.Conversion Operand.Int64
            NullaryIlOp.Conv_ovf_i_un, OpCodes.Conv_Ovf_I_Un, Typing.Conversion Operand.NativeInt
            NullaryIlOp.Conv_ovf_i1_un, OpCodes.Conv_Ovf_I1_Un, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_i2_un, OpCodes.Conv_Ovf_I2_Un, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_i4_un, OpCodes.Conv_Ovf_I4_Un, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_i8_un, OpCodes.Conv_Ovf_I8_Un, Typing.Conversion Operand.Int64
            NullaryIlOp.Conv_ovf_u_un, OpCodes.Conv_Ovf_U_Un, Typing.Conversion Operand.NativeInt
            NullaryIlOp.Conv_ovf_u1_un, OpCodes.Conv_Ovf_U1_Un, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_u2_un, OpCodes.Conv_Ovf_U2_Un, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_u4_un, OpCodes.Conv_Ovf_U4_Un, Typing.Conversion Operand.Int32
            NullaryIlOp.Conv_ovf_u8_un, OpCodes.Conv_Ovf_U8_Un, Typing.Conversion Operand.Int64
        ]

    let private hostType (operand : Operand) : Type =
        match operand with
        | Operand.Int32 -> typeof<int32>
        | Operand.Int64 -> typeof<int64>
        | Operand.NativeInt -> typeof<nativeint>
        | Operand.Float32 -> typeof<float32>
        | Operand.Float64 -> typeof<float>

    /// The values at which some instruction here changes behaviour: each type's extremes, and
    /// the edges of every narrower type a conversion targets, from either side.
    let private boundaries (operand : Operand) : obj list =
        let wide : int64 list =
            [
                0L
                1L
                -1L
                2L
                -2L
                127L
                128L
                -128L
                -129L
                255L
                256L
                32767L
                32768L
                -32768L
                -32769L
                65535L
                65536L
                int64 Int32.MaxValue
                int64 Int32.MaxValue + 1L
                int64 Int32.MinValue
                int64 Int32.MinValue - 1L
                int64 UInt32.MaxValue
                int64 UInt32.MaxValue + 1L
                Int64.MaxValue
                Int64.MinValue
                Int64.MinValue + 1L
            ]

        let edges : float list =
            [
                0.0
                -0.0
                0.5
                -0.5
                1.0
                -1.0
                -0.99
                255.5
                256.0
                -128.5
                -129.0
                65535.5
                2147483647.5
                2147483648.0
                -2147483648.5
                -2147483649.0
                4294967295.5
                4294967296.0
                9223372036854774784.0
                9223372036854775808.0
                -9223372036854775808.0
                -9223372036854777856.0
                18446744073709549568.0
                18446744073709551616.0
                Double.MaxValue
                Double.MinValue
                Double.Epsilon
                nan
                infinity
                -infinity
            ]

        match operand with
        | Operand.Int32 ->
            wide
            |> List.filter (fun x -> x >= int64 Int32.MinValue && x <= int64 Int32.MaxValue)
            |> List.map (int32 >> box)
        | Operand.Int64 -> wide |> List.map box
        | Operand.NativeInt -> wide |> List.map (nativeint >> box)
        | Operand.Float64 -> edges |> List.map box
        // Each float64 rounded to the nearest float32, which keeps the large edges just past
        // and just short of each integer range and adds float32's own extremes.
        | Operand.Float32 ->
            [ Single.MaxValue ; Single.MinValue ; Single.Epsilon ]
            @ (edges |> List.map float32)
            |> List.distinctBy BitConverter.SingleToInt32Bits
            |> List.map box

    let private methods = ConcurrentDictionary<OpCode * Operand list, DynamicMethod> ()

    /// `ldarg`s for `operands`, then `op`, then `ret` of `result`.
    let private probe (op : OpCode) (operands : Operand list) (result : Operand) : DynamicMethod =
        methods.GetOrAdd (
            (op, operands),
            fun _ ->
                let method =
                    DynamicMethod (
                        $"Probe_%s{op.Name}",
                        hostType result,
                        operands |> List.map hostType |> Array.ofList,
                        typeof<OpcodeFault>.Module
                    )

                let il = method.GetILGenerator ()

                for i in 0 .. operands.Length - 1 do
                    il.Emit (OpCodes.Ldarg, int16 i)

                il.Emit op
                il.Emit OpCodes.Ret
                method
        )

    /// The full name of what the host raises running `method` on `arguments`, if it raises.
    let private raised (method : DynamicMethod) (arguments : obj list) : string option =
        try
            method.Invoke ((null : obj), Array.ofList arguments) |> ignore<obj>
            None
        with :? TargetInvocationException as e ->
            Some (e.InnerException.GetType().FullName)

    let private listed (op : NullaryIlOp) : Set<string> =
        match OpcodeFaults.ofNullary op with
        | OpcodeFaults.Unmodelled -> failwith $"%O{op} is unmodelled, so the host cannot contradict it"
        | OpcodeFaults.Raises faults -> faults |> List.map OpcodeFault.typeName |> Set.ofList

    let rec private cartesian (lists : 'a list list) : 'a list list =
        match lists with
        | [] -> [ [] ]
        | first :: rest ->
            let tails = cartesian rest

            [
                for x in first do
                    for tail in tails -> x :: tail
            ]

    /// A pairing of an instruction with the wrong `OpCodes` field would check one instruction's
    /// entry against another's behaviour.
    [<Test>]
    let ``each instruction is paired with its own encoding`` () : unit =
        let spelling (name : string) : string =
            name.Replace("_", "").Replace(".", "").ToLowerInvariant ()

        for op, opcode, _ in checkedHere do
            spelling opcode.Name |> shouldEqual (spelling $"%O{op}")

        checkedHere
        |> List.map (fun (op, _, _) -> op)
        |> List.distinct
        |> List.length
        |> shouldEqual checkedHere.Length

    [<Test>]
    let ``on boundary operands, each instruction raises exactly what the table lists`` () : unit =
        let mismatches =
            [
                for op, opcode, typing in checkedHere do
                    let observed =
                        [
                            for operands, result in signatures typing do
                                let method = probe opcode operands result

                                for arguments in cartesian (List.map boundaries operands) do
                                    match raised method arguments with
                                    | Some name -> name, (operands, arguments)
                                    | None -> ()
                        ]
                        |> List.groupBy fst
                        |> List.map (fun (name, witnesses) -> name, snd (List.head witnesses))
                        |> Map.ofList

                    let expected = listed op

                    for KeyValue (name, (operands, arguments)) in observed do
                        if not (expected.Contains name) then
                            $"%O{op} raised %s{name}, which the table omits, on %A{operands} %A{arguments}"

                    for name in expected do
                        if not (observed.ContainsKey name) then
                            $"%O{op} never raised %s{name}, which the table lists"
            ]

        mismatches |> shouldEqual []

    /// Any value of the operand's host type, weighted towards the boundaries.
    let private operandValue (operand : Operand) : Gen<obj> =
        let anyInt64 : Gen<int64> =
            Gen.map2
                (fun (high : int) (low : int) -> (int64 high <<< 32) ||| int64 (uint32 low))
                (Gen.choose (Int32.MinValue, Int32.MaxValue))
                (Gen.choose (Int32.MinValue, Int32.MaxValue))

        // Every bit pattern is a float64, which reaches NaN payloads, subnormals and huge
        // magnitudes; a random integer plus a fraction reaches the ranges conversions target.
        let anyFloat : Gen<float> =
            Gen.oneof
                [
                    anyInt64 |> Gen.map BitConverter.Int64BitsToDouble
                    Gen.map2
                        (fun (n : int64) (fraction : int) -> float n + float fraction / 4.0)
                        anyInt64
                        (Gen.choose (-3, 3))
                ]

        let random : Gen<obj> =
            match operand with
            | Operand.Int32 -> Gen.choose (Int32.MinValue, Int32.MaxValue) |> Gen.map box
            | Operand.Int64 -> anyInt64 |> Gen.map box
            | Operand.NativeInt -> anyInt64 |> Gen.map (nativeint >> box)
            | Operand.Float64 -> anyFloat |> Gen.map box
            | Operand.Float32 -> anyFloat |> Gen.map (float32 >> box)

        Gen.frequency [ 1, Gen.elements (boundaries operand) ; 3, random ]

    [<Test>]
    let ``on any operands, each instruction raises only what the table lists`` () : unit =
        let case =
            gen {
                let! op, opcode, typing = Gen.elements checkedHere
                let! operands, result = Gen.elements (signatures typing)
                let! arguments = operands |> List.map operandValue |> Gen.sequenceToList
                return op, opcode, operands, result, arguments
            }

        let property
            (op : NullaryIlOp, opcode : OpCode, operands : Operand list, result : Operand, arguments : obj list)
            : bool
            =
            match raised (probe opcode operands result) arguments with
            | None -> true
            | Some name -> (listed op).Contains name

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 20000, Prop.forAll (Arb.fromGen case) property)
