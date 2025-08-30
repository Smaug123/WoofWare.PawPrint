namespace WoofWare.PawPrint

#nowarn "42"

/// See I.12.3.2.1 for definition
type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of NativeIntSource
    | Float of float
    | ManagedPointer of ManagedPointerSource
    | ObjectRef of ManagedHeapAddress
    /// This doesn't match what the CLR does in reality, but we can work out whatever we need from it.
    | UserDefinedValueType of CliValueType

    override this.ToString () =
        match this with
        | EvalStackValue.Int32 i -> $"Int32(%i{i})"
        | EvalStackValue.Int64 i -> $"Int64(%i{i})"
        | EvalStackValue.NativeInt src -> $"NativeInt(%O{src})"
        | EvalStackValue.Float f -> $"Float(%f{f})"
        | EvalStackValue.ManagedPointer managedPointerSource -> $"Pointer(%O{managedPointerSource})"
        | EvalStackValue.ObjectRef managedHeapAddress -> $"ObjectRef(%O{managedHeapAddress})"
        | EvalStackValue.UserDefinedValueType evalStackValues -> $"Struct(%O{evalStackValues})"

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
            | NativeIntSource.FieldHandlePtr _ -> failwith "TODO"
            | NativeIntSource.TypeHandlePtr _ -> failwith "TODO"
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
        | EvalStackValue.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim int64 -> Some int64
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> Some 0L
            | NativeIntSource.ManagedPointer _
            | NativeIntSource.FunctionPointer _
            | NativeIntSource.TypeHandlePtr _
            | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to convert pointer to int64"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    /// Then truncates to int64.
    let convToUInt64 (value : EvalStackValue) : int64 option =
        match value with
        | EvalStackValue.Int32 i -> Some (int64 (uint32 i))
        | EvalStackValue.Int64 int64 -> Some int64
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    /// Then truncates to int32.
    let convToUInt8 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 (i : int32) ->
            let v = (# "conv.u1" i : uint8 #)
            Some (int32<uint8> v)
        | EvalStackValue.Int64 int64 ->
            let v = (# "conv.u1" int64 : uint8 #)
            Some (int32<uint8> v)
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    let rec ofCliType (v : CliType) : EvalStackValue =
        match v with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 i -> EvalStackValue.Int32 i
            | CliNumericType.Int64 i -> EvalStackValue.Int64 i
            | CliNumericType.NativeInt i -> EvalStackValue.NativeInt i
            // Sign-extend types int8 and int16
            // Zero-extend unsigned int8/unsigned int16
            | CliNumericType.Int8 b -> int32<int8> b |> EvalStackValue.Int32
            | CliNumericType.UInt8 b -> int32<uint8> b |> EvalStackValue.Int32
            | CliNumericType.Int16 s -> int32<int16> s |> EvalStackValue.Int32
            | CliNumericType.UInt16 s -> int32<uint16> s |> EvalStackValue.Int32
            | CliNumericType.Float32 f -> EvalStackValue.Float (float<float32> f)
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
            | CliRuntimePointer.Verbatim ptrInt -> NativeIntSource.Verbatim ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.FieldRegistryHandle ptrInt ->
                NativeIntSource.FieldHandlePtr ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.Managed ptr -> ptr |> EvalStackValue.ManagedPointer
        | CliType.ValueType fields -> EvalStackValue.UserDefinedValueType fields

    let rec toCliTypeCoerced (target : CliType) (popped : EvalStackValue) : CliType =
        match target with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Int32 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int32 i)
                | EvalStackValue.UserDefinedValueType popped ->
                    let popped = CliValueType.DereferenceFieldAt 0 4 popped
                    // TODO: when we have a general mechanism to coerce CliTypes to each other,
                    // do that
                    match popped with
                    | CliType.Numeric (CliNumericType.Int32 i) -> CliType.Numeric (CliNumericType.Int32 i)
                    | _ -> failwith "TODO"
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.Int64 _ ->
                match popped with
                | EvalStackValue.Int64 i -> CliType.Numeric (CliNumericType.Int64 i)
                | EvalStackValue.NativeInt src ->
                    match src with
                    | NativeIntSource.Verbatim i -> CliType.Numeric (CliNumericType.Int64 i)
                    | NativeIntSource.ManagedPointer ptr -> failwith "TODO"
                    | NativeIntSource.FunctionPointer f -> failwith $"TODO: {f}"
                    | NativeIntSource.FieldHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.TypeHandlePtr f -> failwith $"TODO: {f}"
                // CliType.Numeric (CliNumericType.TypeHandlePtr f)
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.NativeInt _ ->
                match popped with
                | EvalStackValue.NativeInt s -> CliNumericType.NativeInt s |> CliType.Numeric
                | EvalStackValue.ManagedPointer ptrSrc ->
                    CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptrSrc)
                    |> CliType.Numeric
                | EvalStackValue.UserDefinedValueType vt ->
                    let popped = CliValueType.DereferenceFieldAt 0 NATIVE_INT_SIZE vt
                    // TODO: when we have a general mechanism to coerce CliTypes to each other,
                    // do that
                    match popped with
                    | CliType.Numeric (CliNumericType.NativeInt i) -> CliType.Numeric (CliNumericType.NativeInt i)
                    | CliType.Numeric (CliNumericType.Int64 i) ->
                        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i))
                    | CliType.RuntimePointer ptr ->
                        match ptr with
                        | CliRuntimePointer.Verbatim i ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i))
                        | CliRuntimePointer.FieldRegistryHandle ptr ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr ptr))
                        | CliRuntimePointer.Managed src ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src))
                    | _ -> failwith $"TODO: {popped}"
                | _ -> failwith $"TODO: {popped}"
            | CliNumericType.NativeFloat f -> failwith "todo"
            | CliNumericType.Int8 _ ->
                match popped with
                | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int8 (i % 256 |> int8))
                | i -> failwith $"TODO: %O{i}"
            | CliNumericType.Int16 _ ->
                match popped with
                | EvalStackValue.Int32 popped -> CliType.Numeric (CliNumericType.Int16 (popped % 65536 |> int16<int>))
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
                ptrSource |> CliRuntimePointer.Managed |> CliType.RuntimePointer
            | EvalStackValue.ObjectRef ptr ->
                ManagedPointerSource.Heap ptr
                |> CliRuntimePointer.Managed
                |> CliType.RuntimePointer
            | EvalStackValue.NativeInt nativeIntSource ->
                match nativeIntSource with
                | NativeIntSource.Verbatim 0L -> CliType.ObjectRef None
                | NativeIntSource.Verbatim i -> failwith $"refusing to interpret verbatim native int {i} as a pointer"
                | NativeIntSource.FunctionPointer _ -> failwith "TODO"
                | NativeIntSource.TypeHandlePtr _ -> failwith "refusing to interpret type handle ID as an object ref"
                | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to interpret field handle ID as an object ref"
                | NativeIntSource.ManagedPointer ptr ->
                    match ptr with
                    | ManagedPointerSource.Null -> CliType.ObjectRef None
                    | ManagedPointerSource.Heap s -> CliType.ObjectRef (Some s)
                    | _ -> failwith "TODO"
            | EvalStackValue.UserDefinedValueType obj ->
                let popped = CliValueType.DereferenceFieldAt 0 NATIVE_INT_SIZE obj

                match popped with
                | CliType.ObjectRef r -> CliType.ObjectRef r
                | _ -> failwith "TODO"
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
            | EvalStackValue.ManagedPointer src -> src |> CliRuntimePointer.Managed |> CliType.RuntimePointer
            | EvalStackValue.NativeInt intSrc ->
                match intSrc with
                | NativeIntSource.Verbatim i -> CliType.RuntimePointer (CliRuntimePointer.Verbatim i)
                | NativeIntSource.ManagedPointer src -> src |> CliRuntimePointer.Managed |> CliType.RuntimePointer
                | NativeIntSource.FunctionPointer methodInfo ->
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer methodInfo))
                | NativeIntSource.TypeHandlePtr int64 -> failwith "todo"
                | NativeIntSource.FieldHandlePtr int64 -> failwith "todo"
            | EvalStackValue.ObjectRef addr ->
                ManagedPointerSource.Heap addr
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
        | CliType.ValueType vt ->
            match popped with
            | EvalStackValue.UserDefinedValueType popped' ->
                match CliValueType.TrySequentialFields vt, CliValueType.TrySequentialFields popped' with
                | Some vt, Some popped ->
                    if vt.Length <> popped.Length then
                        failwith
                            $"mismatch: popped value type {popped} (length %i{popped.Length}) into {vt} (length %i{vt.Length})"

                    (vt, popped)
                    ||> List.map2 (fun field1 popped ->
                        if field1.Name <> popped.Name then
                            failwith $"TODO: name mismatch, {field1.Name} vs {popped.Name}"

                        if field1.Offset <> popped.Offset then
                            failwith $"TODO: offset mismatch for {field1.Name}, {field1.Offset} vs {popped.Offset}"

                        let contents = toCliTypeCoerced field1.Contents (ofCliType popped.Contents)

                        {
                            CliField.Name = field1.Name
                            Contents = contents
                            Offset = field1.Offset
                            Type = field1.Type
                        }
                    )
                    |> CliValueType.OfFields popped'.Layout
                    |> CliType.ValueType
                | _, _ -> failwith "TODO: overlapping fields going onto eval stack"
            | popped ->
                match CliValueType.TryExactlyOneField vt with
                | Some field -> toCliTypeCoerced field.Contents popped
                | _ -> failwith $"TODO: {popped} into value type {target}"

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

    static member PeekNthFromTop (n : int) (stack : EvalStack) : EvalStackValue option = stack.Values |> List.tryItem n
