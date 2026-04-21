namespace WoofWare.PawPrint

#nowarn "42"

/// See I.12.3.2.1 for definition
type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of NativeIntSource
    | Float of float
    | ManagedPointer of ManagedPointerSource
    | NullObjectRef
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
        | EvalStackValue.NullObjectRef -> "NullObjectRef"
        | EvalStackValue.ObjectRef managedHeapAddress -> $"ObjectRef(%O{managedHeapAddress})"
        | EvalStackValue.UserDefinedValueType evalStackValues -> $"Struct(%O{evalStackValues})"

[<RequireQualifiedAccess>]
module EvalStackValue =
    /// The conversion performed by Conv_u.
    let toUnsignedNativeInt (value : EvalStackValue) : UnsignedNativeIntSource option =
        // Table III.8. Negative inputs are bit-reinterpreted (zero-extended
        // for Int32, same bits for Int64/NativeInt); the F# `uint32`/`uint64`
        // conversions from signed already do this.
        match value with
        | EvalStackValue.Int32 i -> Some (uint64 (uint32 i) |> UnsignedNativeIntSource.Verbatim)
        | EvalStackValue.Int64 i -> Some (uint64 i |> UnsignedNativeIntSource.Verbatim)
        | EvalStackValue.NativeInt i ->
            match i with
            | NativeIntSource.Verbatim i -> uint64 i |> UnsignedNativeIntSource.Verbatim |> Some
            // `SyntheticCrossArrayOffset` intentionally flows through the same
            // Conv.U path as `Verbatim`: the overlap check in Memmove is
            // exactly `(nuint)ByteOffset(...) < len`, and the synthetic's
            // large signed-negative sentinel becomes a very large unsigned
            // value after this bit-reinterpret — which is the answer we want.
            | NativeIntSource.SyntheticCrossArrayOffset i -> uint64 i |> UnsignedNativeIntSource.Verbatim |> Some
            | NativeIntSource.ManagedPointer _ -> failwith "TODO"
            | NativeIntSource.FunctionPointer _ -> failwith "TODO"
            | NativeIntSource.FieldHandlePtr _ -> failwith "TODO"
            | NativeIntSource.TypeHandlePtr _ -> failwith "TODO"
            | NativeIntSource.AssemblyHandle _ -> failwith "TODO"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource ->
            UnsignedNativeIntSource.FromManagedPointer managedPointerSource |> Some
        | EvalStackValue.NullObjectRef -> failwith "todo"
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
        | EvalStackValue.NullObjectRef -> failwith "todo"
        | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
        | EvalStackValue.UserDefinedValueType evalStackValues -> failwith "todo"

    let convToInt64 (value : EvalStackValue) : int64 option =
        match value with
        | EvalStackValue.Int32 i -> Some (int64<int> i)
        | EvalStackValue.Int64 i -> Some i
        | EvalStackValue.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim int64 -> Some int64
            // `SyntheticCrossArrayOffset` converts to Int64 bit-identically:
            // casting an IntPtr to long (`(long)ptr`) lowers to Conv.I8, and
            // the cross-array ByteOffset use sites need to see the same bits
            // they'd get back from a subsequent Conv.U.
            | NativeIntSource.SyntheticCrossArrayOffset int64 -> Some int64
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> Some 0L
            | NativeIntSource.ManagedPointer _
            | NativeIntSource.FunctionPointer _
            | NativeIntSource.TypeHandlePtr _
            | NativeIntSource.FieldHandlePtr _
            | NativeIntSource.AssemblyHandle _ -> failwith "refusing to convert pointer to int64"
        | EvalStackValue.Float f -> failwith "todo"
        | EvalStackValue.ManagedPointer managedPointerSource -> failwith "todo"
        | EvalStackValue.NullObjectRef -> failwith "todo"
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
        | EvalStackValue.NullObjectRef -> failwith "todo"
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
        | EvalStackValue.NullObjectRef -> failwith "todo"
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
        | CliType.ObjectRef None -> EvalStackValue.NullObjectRef
        | CliType.ObjectRef (Some addr) -> EvalStackValue.ObjectRef addr
        // Zero-extend bool/char
        | CliType.Bool b -> int32 b |> EvalStackValue.Int32
        | CliType.Char (high, low) -> int32 high * 256 + int32 low |> EvalStackValue.Int32
        | CliType.RuntimePointer ptr ->
            match ptr with
            | CliRuntimePointer.Verbatim ptrInt -> NativeIntSource.Verbatim ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.FieldRegistryHandle ptrInt ->
                NativeIntSource.FieldHandlePtr ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.Managed ptr -> ptr |> EvalStackValue.ManagedPointer
        | CliType.ValueType vt ->
            // Primitive-like BCL wrappers (IntPtr, RuntimeTypeHandle, ...) and enums both get
            // flattened to their underlying primitive on the stack. ECMA III.1.8 treats enums
            // as their underlying integer for every numeric/comparison opcode; flattening here
            // means cgt.un/clt.un/add/etc. don't need enum-specific arms. Storage stays wrapped;
            // `toCliTypeCoerced` re-wraps on the pop side when the target slot is enum- or
            // primitive-like.
            if vt.PrimitiveLikeKind.IsSome || vt.IsEnumLike then
                match CliValueType.TryExactlyOneField vt with
                | Some field -> ofCliType field.Contents
                | None ->
                    failwith
                        $"primitive-like or enum-like struct %O{vt.Declared} did not have a single field at offset 0 during eval-stack flatten"
            else
                EvalStackValue.UserDefinedValueType vt

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
                    | NativeIntSource.SyntheticCrossArrayOffset i -> CliType.Numeric (CliNumericType.Int64 i)
                    | NativeIntSource.ManagedPointer ptr -> failwith "TODO"
                    | NativeIntSource.FunctionPointer f -> failwith $"TODO: {f}"
                    | NativeIntSource.FieldHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.TypeHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.AssemblyHandle f -> failwith $"TODO: {f}"
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
            | EvalStackValue.NullObjectRef -> CliType.ObjectRef None
            | EvalStackValue.ObjectRef addr -> CliType.ObjectRef (Some addr)
            | EvalStackValue.NativeInt nativeIntSource ->
                match nativeIntSource with
                | NativeIntSource.Verbatim 0L -> CliType.ObjectRef None
                | NativeIntSource.Verbatim i -> failwith $"refusing to interpret verbatim native int {i} as a pointer"
                | NativeIntSource.SyntheticCrossArrayOffset i ->
                    failwith $"refusing to interpret synthetic cross-array byte offset {i} as a pointer"
                | NativeIntSource.FunctionPointer _ -> failwith "TODO"
                | NativeIntSource.TypeHandlePtr _ -> failwith "refusing to interpret type handle ID as an object ref"
                | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to interpret field handle ID as an object ref"
                | NativeIntSource.AssemblyHandle _ -> failwith "refusing to interpret assembly handle as an object ref"
                | NativeIntSource.ManagedPointer ptr ->
                    match ptr with
                    | ManagedPointerSource.Null -> CliType.ObjectRef None
                    | _ -> failwith "TODO: non-null managed pointer in NativeIntSource coerced to ObjectRef"
            | EvalStackValue.UserDefinedValueType obj ->
                let popped = CliValueType.DereferenceFieldAt 0 NATIVE_INT_SIZE obj

                match popped with
                | CliType.ObjectRef r -> CliType.ObjectRef r
                | _ -> failwith "TODO"
            | EvalStackValue.ManagedPointer _ -> failwith "cannot coerce managed pointer to object reference"
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
                | NativeIntSource.SyntheticCrossArrayOffset i ->
                    failwith
                        $"refusing to interpret synthetic cross-array byte offset {i} as a runtime pointer: the value is a deterministic sentinel, not a real address"
                | NativeIntSource.ManagedPointer src -> src |> CliRuntimePointer.Managed |> CliType.RuntimePointer
                | NativeIntSource.FunctionPointer methodInfo ->
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer methodInfo))
                | NativeIntSource.TypeHandlePtr int64 -> failwith "todo: TypeHandlePtr into CliType.RuntimePointer"
                | NativeIntSource.FieldHandlePtr ptr ->
                    CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle ptr)
                | NativeIntSource.AssemblyHandle _ -> failwith "todo: AssemblyHandle into CliType.RuntimePointer"
            | EvalStackValue.NullObjectRef -> failwith "cannot coerce null object reference to runtime pointer"
            | EvalStackValue.ObjectRef addr -> failwith $"cannot coerce object reference %O{addr} to runtime pointer"
            | _ -> failwith $"TODO: %O{popped}"
        | CliType.Char _ ->
            match popped with
            | EvalStackValue.Int32 i ->
                // Char is a 16-bit unsigned slot. The int32 on the stack may
                // carry a sign-extended negative value (e.g. from coercing a
                // negative Int16 through a `Unsafe.As<ushort, short>` write);
                // narrow via `uint16` so the reinterpret preserves the low
                // 16 bits bit-for-bit instead of splitting signed/ arithmetic
                // into the wrong high byte.
                let truncated = uint16<int> i
                let high = byte<uint16> (truncated >>> 8)
                let low = byte<uint16> (truncated &&& 0xFFus)
                CliType.Char (high, low)
            | popped -> failwith $"Unexpectedly wanted a char from {popped}"
        | CliType.ValueType vt ->
            match popped with
            | EvalStackValue.UserDefinedValueType popped' ->
                match CliValueType.TrySequentialFields vt, CliValueType.TrySequentialFields popped' with
                | Some vtFields, Some popped ->
                    if vtFields.Length <> popped.Length then
                        failwith
                            $"mismatch: popped value type {popped} (length %i{popped.Length}) into {vtFields} (length %i{vtFields.Length})"

                    (vtFields, popped)
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
                    |> CliValueType.OfFieldsLike vt popped'.Layout
                    |> CliType.ValueType
                | _, _ -> failwith "TODO: overlapping fields going onto eval stack"
            | popped ->
                // A bare primitive popped into a ValueType slot is only legal for (a) the
                // primitive-like BCL wrappers (IntPtr, RuntimeTypeHandle, ...), which are
                // flattened on push and rewrapped here, and (b) enums, where CIL freely
                // coerces between the underlying integer on the stack and the enum slot.
                // Both cases share the same rewrap: clone the target's single-field skeleton
                // and store the coerced primitive into `value__`/`_value`. A single-field
                // user-defined struct receiving a bare primitive is invalid IL; fail loud
                // so the misfire surfaces instead of silently degrading the storage shape.
                if vt.PrimitiveLikeKind.IsSome || vt.IsEnumLike then
                    match CliValueType.TryExactlyOneField vt with
                    | Some field ->
                        let newContents = toCliTypeCoerced field.Contents popped

                        let newField =
                            { field with
                                Contents = newContents
                            }

                        [ newField ] |> CliValueType.OfFieldsLike vt vt.Layout |> CliType.ValueType
                    | None ->
                        failwith
                            $"invariant: primitive-like or enum-like struct {vt.Declared} must have exactly one field at offset 0"
                else
                    failwith $"TODO: {popped} into value type {target}"

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
        // Invariant: primitive-like wrapper structs (IntPtr, RuntimeTypeHandle, ...) and enums
        // must never appear on the eval stack as UserDefinedValueType; EvalStackValue.ofCliType
        // flattens them on push. A caller using Push' directly must respect this too.
        match v with
        | EvalStackValue.UserDefinedValueType vt when vt.PrimitiveLikeKind.IsSome ->
            failwith
                $"eval-stack invariant violated: primitive-like struct %O{vt.Declared} pushed as UserDefinedValueType (kind = %O{vt.PrimitiveLikeKind})"
        | EvalStackValue.UserDefinedValueType vt when vt.IsEnumLike ->
            failwith $"eval-stack invariant violated: enum-like struct %O{vt.Declared} pushed as UserDefinedValueType"
        | _ -> ()

        {
            Values = v :: stack.Values
        }

    static member Push (v : CliType) (stack : EvalStack) : EvalStack =
        let v = EvalStackValue.ofCliType v

        EvalStack.Push' v stack

    static member PeekNthFromTop (n : int) (stack : EvalStack) : EvalStackValue option = stack.Values |> List.tryItem n
