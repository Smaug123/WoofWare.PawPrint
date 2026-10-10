namespace WoofWare.PawPrint.Test

open System
open System.Collections.Concurrent
open System.Reflection.Emit
open WoofWare.PawPrint

/// The instructions whose operands are all numbers, the operand types each accepts, and the
/// boundary operands at which their behaviour changes: the inputs that `TestOpcodeFaultsOnHost`
/// runs on the host and `TestOpcodeFaultsOnPawPrint` runs under PawPrint.
[<RequireQualifiedAccess>]
module NumericOpcodeGrid =

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
    let signatures (typing : Typing) : (Operand list * Operand) list =
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

    /// The instructions with only numeric operands, each with the host's encoding of it and its
    /// typing.
    let instructions : (NullaryIlOp * OpCode * Typing) list =
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

    let hostType (operand : Operand) : Type =
        match operand with
        | Operand.Int32 -> typeof<int32>
        | Operand.Int64 -> typeof<int64>
        | Operand.NativeInt -> typeof<nativeint>
        | Operand.Float32 -> typeof<float32>
        | Operand.Float64 -> typeof<float>

    /// The values at which some instruction here changes behaviour: each type's extremes, and
    /// the edges of every narrower type a conversion targets, from either side.
    let boundaries (operand : Operand) : obj list =
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

    /// A host `DynamicMethod` that is `ldarg`s for `operands`, then `op`, then `ret` of `result`.
    let probe (op : OpCode) (operands : Operand list) (result : Operand) : DynamicMethod =
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
