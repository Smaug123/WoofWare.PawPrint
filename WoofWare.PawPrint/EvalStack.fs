namespace WoofWare.PawPrint

open Microsoft.FSharp.Core

type ManagedPointerSource =
    | LocalVariable of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Argument of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Heap of ManagedHeapAddress
    | Null

    override this.ToString () =
        match this with
        | ManagedPointerSource.Null -> "<null pointer>"
        | ManagedPointerSource.Heap addr -> $"%O{addr}"
        | ManagedPointerSource.LocalVariable (source, method, var) ->
            $"<variable %i{var} in method frame %i{method} of thread %O{source}>"
        | ManagedPointerSource.Argument (source, method, var) ->
            $"<argument %i{var} in method frame %i{method} of thread %O{source}>"

[<RequireQualifiedAccess>]
type NativeIntSource =
    | Verbatim of int64
    | FunctionPointer of MethodInfo<FakeUnit, GenericParameter>
    | TypeHandlePtr of int64<typeHandle>

    override this.ToString () : string =
        match this with
        | NativeIntSource.Verbatim int64 -> $"%i{int64}"
        | NativeIntSource.FunctionPointer methodDefinition ->
            $"<pointer to {methodDefinition.Name} in {methodDefinition.DeclaringType.Assembly.Name}>"
        | NativeIntSource.TypeHandlePtr ptr -> $"<type ID %i{ptr}>"

[<RequireQualifiedAccess>]
module NativeIntSource =
    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.TypeHandlePtr _ -> false
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.TypeHandlePtr _ -> true

    /// True if a < b.
    let isLess (a : NativeIntSource) (b : NativeIntSource) : bool =
        match a, b with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> a < b
        | _, _ -> failwith "TODO"

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
    | UserDefinedValueType of EvalStackValue list

    override this.ToString () =
        match this with
        | EvalStackValue.Int32 i -> $"Int32(%i{i})"
        | EvalStackValue.Int64 i -> $"Int64(%i{i})"
        | EvalStackValue.NativeInt src -> $"NativeInt(%O{src})"
        | EvalStackValue.Float f -> $"Float(%f{f})"
        | EvalStackValue.ManagedPointer managedPointerSource -> $"Pointer(%O{managedPointerSource})"
        | EvalStackValue.ObjectRef managedHeapAddress -> $"ObjectRef(%O{managedHeapAddress})"
        | EvalStackValue.UserDefinedValueType evalStackValues ->
            let desc = evalStackValues |> List.map string<EvalStackValue> |> String.concat " | "
            $"Struct(%s{desc})"

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
            | NativeIntSource.FunctionPointer _ -> failwith "TODO"
            | NativeIntSource.TypeHandlePtr _ -> failwith "TODO"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType _ -> failwith "todo"

    let convToInt32 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> Some i
        | EvalStackValue.Int64 int64 -> failwith "todo"
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    let convToInt64 (value : EvalStackValue) : int64 option =
        match value with
        | EvalStackValue.Int32 i -> Some (int64<int> i)
        | EvalStackValue.Int64 i -> Some i
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    /// Then truncates to int64.
    let convToUInt64 (value : EvalStackValue) : int64 option =
        match value with
        | EvalStackValue.Int32 i -> if i >= 0 then Some (int64 i) else failwith "TODO"
        | EvalStackValue.Int64 int64 -> failwith "todo"
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    let rec toCliTypeCoerced (target : CliType) (popped : EvalStackValue) : CliType =
        match target with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int32 i)
                | EvalStackValue.UserDefinedValueType [ popped ] -> toCliTypeCoerced target popped
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.TypeHandlePtr _
            | CliNumericType.ProvenanceTrackedNativeInt64 _
            | CliNumericType.Int64 _ ->
                match popped with
                | EvalStackValue.Int64 i -> CliType.Numeric (CliNumericType.Int64 i)
                | EvalStackValue.NativeInt src ->
                    match src with
                    | NativeIntSource.Verbatim i -> CliType.Numeric (CliNumericType.Int64 i)
                    | NativeIntSource.FunctionPointer f ->
                        CliType.Numeric (CliNumericType.ProvenanceTrackedNativeInt64 f)
                    | NativeIntSource.TypeHandlePtr f -> CliType.Numeric (CliNumericType.TypeHandlePtr f)
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.NativeInt int64 -> failwith "todo"
            | CliNumericType.NativeFloat f -> failwith "todo"
            | CliNumericType.Int8 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int8 (i % 256 |> int8))
                | i -> failwith $"TODO: %O{i}"
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
                    CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, whichVar)
                    |> CliRuntimePointer.Managed
                    |> CliType.RuntimePointer
                | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
                    CliRuntimePointerSource.Argument (sourceThread, methodFrame, whichVar)
                    |> CliRuntimePointer.Managed
                    |> CliType.RuntimePointer
                | ManagedPointerSource.Heap managedHeapAddress -> CliType.ObjectRef (Some managedHeapAddress)
                | ManagedPointerSource.Null -> CliType.ObjectRef None
            | EvalStackValue.NativeInt nativeIntSource ->
                match nativeIntSource with
                | NativeIntSource.Verbatim 0L -> CliType.ObjectRef None
                | NativeIntSource.Verbatim i -> failwith $"refusing to interpret verbatim native int {i} as a pointer"
                | NativeIntSource.FunctionPointer _ -> failwith "TODO"
                | NativeIntSource.TypeHandlePtr _ -> failwith "refusing to interpret type handle ID as an object ref"
            | EvalStackValue.UserDefinedValueType fields ->
                match fields with
                | [ esv ] -> toCliTypeCoerced target esv
                | fields -> failwith $"TODO: don't know how to coerce struct of {fields} to a pointer"
            | _ -> failwith $"TODO: {popped}"
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
                    CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, var)
                    |> CliRuntimePointer.Managed
                    |> CliType.RuntimePointer
                | ManagedPointerSource.Argument (sourceThread, methodFrame, var) ->
                    CliRuntimePointerSource.Argument (sourceThread, methodFrame, var)
                    |> CliRuntimePointer.Managed
                    |> CliType.RuntimePointer
            | _ -> failwith $"TODO: %O{popped}"
        | CliType.Char _ ->
            match popped with
            | EvalStackValue.Int32 i ->
                let high = i / 256
                let low = i % 256
                CliType.Char (byte<int> high, byte<int> low)
            | popped -> failwith $"Unexpectedly wanted a char from {popped}"
        | CliType.ValueType fields ->
            match popped with
            | EvalStackValue.UserDefinedValueType popped ->
                if fields.Length <> popped.Length then
                    failwith "mismatch"

                List.map2 toCliTypeCoerced fields popped |> CliType.ValueType
            | popped ->
                match fields with
                | [ target ] -> toCliTypeCoerced target popped
                | _ -> failwith "TODO"

    let rec ofCliType (v : CliType) : EvalStackValue =
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
            | CliNumericType.ProvenanceTrackedNativeInt64 f ->
                EvalStackValue.NativeInt (NativeIntSource.FunctionPointer f)
            | CliNumericType.TypeHandlePtr f -> EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr f)
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
                    ManagedPointerSource.LocalVariable (sourceThread, methodFrame, var)
                    |> EvalStackValue.ManagedPointer
                | CliRuntimePointerSource.Argument (sourceThread, methodFrame, var) ->
                    ManagedPointerSource.Argument (sourceThread, methodFrame, var)
                    |> EvalStackValue.ManagedPointer
                | CliRuntimePointerSource.HeapAddress addr ->
                    ManagedPointerSource.Heap addr |> EvalStackValue.ManagedPointer
        | CliType.ValueType fields -> fields |> List.map ofCliType |> EvalStackValue.UserDefinedValueType

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
        let v = EvalStackValue.ofCliType v

        EvalStack.Push' v stack
