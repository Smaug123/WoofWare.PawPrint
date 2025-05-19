namespace WoofWare.PawPrint

open Microsoft.FSharp.Core

type ManagedPointerSource =
    | LocalVariable
    | Heap of ManagedHeapAddress
    | Null

/// See I.12.3.2.1 for definition
type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | Float of float
    | ManagedPointer of ManagedPointerSource
    | ObjectRef of ManagedHeapAddress
    // Fraser thinks this isn't really a thing in CoreCLR
    // | TransientPointer of TransientPointerSource
    | UserDefinedValueType

[<RequireQualifiedAccess>]
module EvalStackValue =
    /// The conversion performed by Conv_u.
    let toUnsignedNativeInt (value : EvalStackValue) : uint64 option =
        // Table III.8
        match value with
        | EvalStackValue.Int32 i ->
            if i >= 0 then
                Some (uint64 i)
            else
            // Zero-extend.
            failwith "todo"
        | EvalStackValue.Int64 i -> if i >= 0L then Some (uint64 i) else failwith "todo"
        | EvalStackValue.NativeInt i -> if i >= 0L then Some (uint64 i) else failwith "todo"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType -> failwith "todo"

    let toCliTypeCoerced (target : CliType) (popped : EvalStackValue) : CliType =
        match target with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int32 i)
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.Int64 int64 -> failwith "todo"
            | CliNumericType.NativeInt int64 -> failwith "todo"
            | CliNumericType.NativeFloat f -> failwith "todo"
            | CliNumericType.Int8 b -> failwith "todo"
            | CliNumericType.Int16 s -> failwith "todo"
            | CliNumericType.UInt8 b -> failwith "todo"
            | CliNumericType.UInt16 s -> failwith "todo"
            | CliNumericType.Float32 f -> failwith "todo"
            | CliNumericType.Float64 f -> failwith "todo"
        | CliType.ObjectRef _ ->
            match popped with
            | EvalStackValue.ManagedPointer ptrSource ->
                match ptrSource with
                | ManagedPointerSource.LocalVariable ->
                    failwith "TODO: trying to fit a local variable address into an ObjectRef"
                | ManagedPointerSource.Heap managedHeapAddress -> CliType.ObjectRef (Some managedHeapAddress)
                | ManagedPointerSource.Null -> CliType.ObjectRef None
            | i -> failwith $"TODO: %O{i}"
        | CliType.Bool _ ->
            match popped with
            | EvalStackValue.Int32 i ->
                // Bools are zero-extended
                CliType.Bool (i % 256 |> byte)
            | EvalStackValue.ManagedPointer src ->
                failwith $"unexpectedly tried to convert a managed pointer (%O{src}) into a bool"
            | i -> failwith $"TODO: %O{i}"
        | i -> failwith $"TODO: %O{i}"

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
            | CliType.ObjectRef i ->
                match i with
                | None -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | Some i -> EvalStackValue.ManagedPointer (ManagedPointerSource.Heap i)
            // Zero-extend bool/char
            | CliType.Bool b -> int32 b |> EvalStackValue.Int32
            | CliType.Char (high, low) -> int32 high * 256 + int32 low |> EvalStackValue.Int32
            | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"

        EvalStack.Push' v stack
