namespace WoofWare.PawPrint

open Microsoft.FSharp.Core

type ManagedPointerSource =
    | LocalVariable of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Heap of ManagedHeapAddress
    | Null

[<RequireQualifiedAccess>]
type NativeIntSource = | Verbatim of int64

[<RequireQualifiedAccess>]
module NativeIntSource =
    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource = | Verbatim of uint64

/// See I.12.3.2.1 for definition
type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of NativeIntSource
    | Float of float
    | ManagedPointer of ManagedPointerSource
    | ObjectRef of ManagedHeapAddress
    // Fraser thinks this isn't really a thing in CoreCLR
    // | TransientPointer of TransientPointerSource
    | UserDefinedValueType

[<RequireQualifiedAccess>]
module EvalStackValue =
    /// The conversion performed by Conv_u.
    let toUnsignedNativeInt (value : EvalStackValue) : UnsignedNativeIntSource option =
        // Table III.8
        match value with
        | EvalStackValue.Int32 i ->
            if i >= 0 then
                Some (uint64 i |> UnsignedNativeIntSource.Verbatim)
            else
            // Zero-extend.
            failwith "todo"
        | EvalStackValue.Int64 i ->
            if i >= 0L then
                Some (uint64 i |> UnsignedNativeIntSource.Verbatim)
            else
                failwith "todo"
        | EvalStackValue.NativeInt i ->
            match i with
            | NativeIntSource.Verbatim i ->
                if i >= 0L then
                    uint64 i |> UnsignedNativeIntSource.Verbatim |> Some
                else
                    failwith "todo"
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
            | CliNumericType.Float64 _ ->
                match popped with
                | EvalStackValue.Float f -> CliType.Numeric (CliNumericType.Float64 f)
                | _ -> failwith "todo"
        | CliType.ObjectRef _ ->
            match popped with
            | EvalStackValue.ManagedPointer ptrSource ->
                match ptrSource with
                | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                    CliType.RuntimePointer (
                        CliRuntimePointer.Managed (
                            CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, whichVar)
                        )
                    )
                // failwith "TODO: trying to fit a local variable address into an ObjectRef"
                | ManagedPointerSource.Heap managedHeapAddress -> CliType.ObjectRef (Some managedHeapAddress)
                | ManagedPointerSource.Null -> CliType.ObjectRef None
            | EvalStackValue.NativeInt nativeIntSource ->
                match nativeIntSource with
                | NativeIntSource.Verbatim 0L -> CliType.ObjectRef None
                | NativeIntSource.Verbatim i -> failwith $"refusing to interpret verbatim native int {i} as a pointer"
            | i -> failwith $"TODO: %O{i}"
        | CliType.Bool _ ->
            match popped with
            | EvalStackValue.Int32 i ->
                // Bools are zero-extended
                CliType.Bool (i % 256 |> byte)
            | EvalStackValue.ManagedPointer src ->
                failwith $"unexpectedly tried to convert a managed pointer (%O{src}) into a bool"
            | i -> failwith $"TODO: %O{i}"
        | CliType.RuntimePointer _ ->
            match popped with
            | EvalStackValue.ManagedPointer src ->
                match src with
                | ManagedPointerSource.Heap addr -> CliType.OfManagedObject addr
                | ManagedPointerSource.Null -> CliType.ObjectRef None
                | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, var) ->
                    CliType.RuntimePointer (
                        CliRuntimePointer.Managed (
                            CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, var)
                        )
                    )
            | _ -> failwith $"TODO: %O{popped}"
        | CliType.Char _ -> failwith "TODO: char"

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
                | CliNumericType.Float64 f -> EvalStackValue.Float f
                | CliNumericType.NativeFloat f -> EvalStackValue.Float f
            | CliType.ObjectRef i ->
                match i with
                | None -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | Some i -> EvalStackValue.ManagedPointer (ManagedPointerSource.Heap i)
            // Zero-extend bool/char
            | CliType.Bool b -> int32 b |> EvalStackValue.Int32
            | CliType.Char (high, low) -> int32 high * 256 + int32 low |> EvalStackValue.Int32
            | CliType.RuntimePointer ptr ->
                match ptr with
                | CliRuntimePointer.Unmanaged () -> failwith "todo: unmanaged"
                | CliRuntimePointer.Managed ptr ->
                    match ptr with
                    | CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, var) ->
                        EvalStackValue.ManagedPointer (
                            ManagedPointerSource.LocalVariable (sourceThread, methodFrame, var)
                        )

        EvalStack.Push' v stack
