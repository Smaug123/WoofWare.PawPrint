namespace WoofWare.PawPrint

type TransientPointerSource = | LocalVariable

/// See I.12.3.2.1 for definition
type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | Float of float
    /// allowed to be null
    | ManagedPointer of ManagedHeapAddress option
    | ObjectRef of ManagedHeapAddress
    | TransientPointer of TransientPointerSource
    | UserDefinedValueType

type EvalStack =
    {
        Values : EvalStackValue list
    }

    static member Empty : EvalStack =
        {
            Values = []
        }

    static member Pop (stack : EvalStack) : EvalStackValue * EvalStack =
        match stack.Values with
        | [] -> failwith "eval stack was empty on pop instruction"
        | v :: rest ->
            let stack =
                {
                    Values = rest
                }

            v, stack

    static member Peek (stack : EvalStack) : EvalStackValue option = stack.Values |> List.tryHead

    static member Push' (v : EvalStackValue) (stack : EvalStack) : EvalStack =
        {
            Values = v :: stack.Values
        }

    static member Push (v : CliType) (stack : EvalStack) : EvalStack =
        let v =
            match v with
            | CliType.Numeric numeric ->
                match numeric with
                | CliNumericType.Int32 i -> EvalStackValue.Int32 i
                | CliNumericType.Int64 i -> EvalStackValue.Int64 i
                | CliNumericType.NativeInt i -> failwith "TODO"
                // Sign-extend types int8 and int16
                // Zero-extend unsigned int8/unsigned int16
                | CliNumericType.Int8 b -> int32 b |> EvalStackValue.Int32
                | CliNumericType.UInt8 b -> int32 b |> EvalStackValue.Int32
                | CliNumericType.Int16 s -> int32 s |> EvalStackValue.Int32
                | CliNumericType.UInt16 s -> int32 s |> EvalStackValue.Int32
                | CliNumericType.Float32 f -> failwith "todo"
                | CliNumericType.Float64 f -> failwith "todo"
                | CliNumericType.NativeFloat f -> failwith "todo"
            | CliType.ObjectRef i -> EvalStackValue.ManagedPointer i
            // Zero-extend bool/char
            | CliType.Bool b -> int32 b |> EvalStackValue.Int32
            | CliType.Char (high, low) -> int32 high * 256 + int32 low |> EvalStackValue.Int32
            | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"

        EvalStack.Push' v stack
