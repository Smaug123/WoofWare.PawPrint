namespace WoofWare.PawPrint

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
    | ManagedPointer of ManagedPointerSource
    | FunctionPointer of MethodInfo<FakeUnit, GenericParameter>

    override this.ToString () : string =
        match this with
        | NativeIntSource.Verbatim int64 -> $"%i{int64}"
        | NativeIntSource.ManagedPointer ptr -> $"<managed pointer {ptr}"
        | NativeIntSource.FunctionPointer methodDefinition ->
            $"<pointer to {methodDefinition.Name} in {methodDefinition.DeclaringType.Assembly.Name}>"

[<RequireQualifiedAccess>]
module NativeIntSource =
    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> true
            | _ -> false

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.ManagedPointer _ -> true

    /// True if a < b.
    let isLess (a : NativeIntSource) (b : NativeIntSource) : bool =
        match a, b with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> a < b
        | _, _ -> failwith "TODO"

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource =
    | Verbatim of uint64
    | FromManagedPointer of ManagedPointerSource

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
            | NativeIntSource.ManagedPointer _ -> failwith "TODO"
            | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource ->
            UnsignedNativeIntSource.FromManagedPointer managedPointerSource |> Some
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType _ -> failwith "todo"

    /// The conversion performed by Conv_i.
    let toNativeInt (value : EvalStackValue) : NativeIntSource option =
        match value with
        | EvalStackValue.Int64 i -> Some (NativeIntSource.Verbatim i)
        | EvalStackValue.Int32 i -> Some (NativeIntSource.Verbatim (int64<int> i))
        | value -> failwith $"{value}"

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
            | CliNumericType.ProvenanceTrackedNativeInt64 _
            | CliNumericType.Int64 _ ->
                match popped with
                | EvalStackValue.Int64 i -> CliType.Numeric (CliNumericType.Int64 i)
                | EvalStackValue.NativeInt src ->
                    match src with
                    | NativeIntSource.Verbatim i -> CliType.Numeric (CliNumericType.Int64 i)
                    | NativeIntSource.ManagedPointer ptr -> failwith "TODO"
                    | NativeIntSource.FunctionPointer f ->
                        CliType.Numeric (CliNumericType.ProvenanceTrackedNativeInt64 f)
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.NativeInt int64 -> failwith "todo"
            | CliNumericType.NativeFloat f -> failwith "todo"
            | CliNumericType.Int8 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int8 (i % 256 |> int8))
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.Int16 _ ->
                match popped with
                | EvalStackValue.Int32 popped -> CliType.Numeric (CliNumericType.Int32 (popped % 65536))
                | _ -> failwith $"TODO: {popped}"
            | CliNumericType.UInt8 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.UInt8 (i % 256 |> uint8))
                | i -> failwith $"todo: {i} to uint8"
            | CliNumericType.UInt16 _ ->
                match popped with
                | EvalStackValue.Int32 popped -> CliType.Numeric (CliNumericType.UInt16 (uint16<int32> popped))
                | i -> failwith $"todo: {i} to uint16"
            | CliNumericType.Float32 _ ->
                match popped with
                | EvalStackValue.Float f -> CliType.Numeric (CliNumericType.Float32 (float32<float> f))
                | i -> failwith $"todo: {i} to float32"
            | CliNumericType.Float64 _ ->
                match popped with
                | EvalStackValue.Float f -> CliType.Numeric (CliNumericType.Float64 f)
                | _ -> failwith $"todo: {popped} to float64"
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
                | NativeIntSource.ManagedPointer ptr ->
                    match ptr with
                    | ManagedPointerSource.Null -> CliType.ObjectRef None
                    | ManagedPointerSource.Heap s -> CliType.ObjectRef (Some s)
                    | _ -> failwith "TODO"
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
            | EvalStackValue.NativeInt intSrc ->
                match intSrc with
                | NativeIntSource.Verbatim i -> CliType.RuntimePointer (CliRuntimePointer.Unmanaged i)
                | NativeIntSource.ManagedPointer src ->
                    match src with
                    | ManagedPointerSource.Heap src ->
                        CliType.RuntimePointer (CliRuntimePointer.Managed (CliRuntimePointerSource.Heap src))
                    | ManagedPointerSource.Null -> failwith "TODO"
                    | ManagedPointerSource.LocalVariable (a, b, c) ->
                        CliType.RuntimePointer (
                            CliRuntimePointer.Managed (CliRuntimePointerSource.LocalVariable (a, b, c))
                        )
                    | ManagedPointerSource.Argument (a, b, c) ->
                        CliType.RuntimePointer (CliRuntimePointer.Managed (CliRuntimePointerSource.Argument (a, b, c)))
                | NativeIntSource.FunctionPointer methodInfo ->
                    CliType.Numeric (CliNumericType.ProvenanceTrackedNativeInt64 methodInfo)
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
                | _ -> failwith $"TODO: {popped} into value type {target}"

    let rec ofCliType (v : CliType) : EvalStackValue =
        match v with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 i -> EvalStackValue.Int32 i
            | CliNumericType.Int64 i -> EvalStackValue.Int64 i
            | CliNumericType.NativeInt i -> failwith "TODO"
            // Sign-extend types int8 and int16
            // Zero-extend unsigned int8/unsigned int16
            | CliNumericType.Int8 b -> int32<int8> b |> EvalStackValue.Int32
            | CliNumericType.UInt8 b -> int32<uint8> b |> EvalStackValue.Int32
            | CliNumericType.Int16 s -> int32<int16> s |> EvalStackValue.Int32
            | CliNumericType.UInt16 s -> int32<uint16> s |> EvalStackValue.Int32
            | CliNumericType.Float32 f -> EvalStackValue.Float (float<float32> f)
            | CliNumericType.Float64 f -> EvalStackValue.Float f
            | CliNumericType.NativeFloat f -> EvalStackValue.Float f
            | CliNumericType.ProvenanceTrackedNativeInt64 f ->
                EvalStackValue.NativeInt (NativeIntSource.FunctionPointer f)
        | CliType.ObjectRef i ->
            match i with
            | None -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
            | Some i -> EvalStackValue.ManagedPointer (ManagedPointerSource.Heap i)
        // Zero-extend bool/char
        | CliType.Bool b -> int32 b |> EvalStackValue.Int32
        | CliType.Char (high, low) -> int32 high * 256 + int32 low |> EvalStackValue.Int32
        | CliType.RuntimePointer ptr ->
            match ptr with
            | CliRuntimePointer.Unmanaged _ -> failwith "todo: unmanaged"
            | CliRuntimePointer.Managed ptr ->
                match ptr with
                | CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, var) ->
                    ManagedPointerSource.LocalVariable (sourceThread, methodFrame, var)
                    |> EvalStackValue.ManagedPointer
                | CliRuntimePointerSource.Argument (sourceThread, methodFrame, var) ->
                    ManagedPointerSource.Argument (sourceThread, methodFrame, var)
                    |> EvalStackValue.ManagedPointer
                | CliRuntimePointerSource.Heap addr -> EvalStackValue.ObjectRef addr
                | CliRuntimePointerSource.Null -> failwith "TODO"
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
