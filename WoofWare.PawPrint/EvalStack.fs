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
    let private nativeIntBitsForIntegerConversion (operation : string) (src : NativeIntSource) : int64 =
        match src with
        | NativeIntSource.Verbatim i
        | NativeIntSource.SyntheticCrossArrayOffset i -> i
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L
        | NativeIntSource.ManagedPointer ptr ->
            failwith $"%s{operation}: refusing to convert managed pointer %O{ptr} to an integer"
        | NativeIntSource.FunctionPointer methodInfo ->
            failwith $"%s{operation}: refusing to convert function pointer %O{methodInfo} to an integer"
        | NativeIntSource.TypeHandlePtr typeHandle ->
            failwith $"%s{operation}: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to an integer"
        | NativeIntSource.MethodTablePtr typeHandle ->
            failwith $"%s{operation}: refusing to convert MethodTable pointer %O{typeHandle} to an integer"
        | NativeIntSource.FieldHandlePtr handle ->
            failwith $"%s{operation}: refusing to convert RuntimeFieldHandle pointer %d{handle} to an integer"
        | NativeIntSource.MethodHandlePtr handle ->
            failwith $"%s{operation}: refusing to convert RuntimeMethodHandle pointer %d{handle} to an integer"
        | NativeIntSource.GcHandlePtr handle ->
            failwith $"%s{operation}: refusing to convert GC handle pointer %O{handle} to an integer"
        | NativeIntSource.AssemblyHandle assemblyName ->
            failwith $"%s{operation}: refusing to convert assembly handle %s{assemblyName} to an integer"

    let private failReferenceConversion (operation : string) (value : EvalStackValue) : 'a =
        match value with
        | EvalStackValue.ManagedPointer ptr -> failwith $"%s{operation}: refusing to convert managed pointer %O{ptr}"
        | EvalStackValue.NullObjectRef -> failwith $"%s{operation}: refusing to convert null object reference"
        | EvalStackValue.ObjectRef addr -> failwith $"%s{operation}: refusing to convert object reference %O{addr}"
        | EvalStackValue.UserDefinedValueType valueType ->
            failwith $"%s{operation}: refusing to convert user-defined value type %O{valueType}"
        | _ -> failwith $"%s{operation}: unexpected non-reference value %O{value}"

    let private convIFromInt32 (value : int32) : int64 =
        let converted = (# "conv.i" value : nativeint #)
        int64<nativeint> converted

    let private convIFromInt64 (value : int64) : int64 =
        let converted = (# "conv.i" value : nativeint #)
        int64<nativeint> converted

    let private convIFromFloat (value : float) : int64 =
        let converted = (# "conv.i" value : nativeint #)
        int64<nativeint> converted

    let private convUFromFloat (value : float) : uint64 =
        let converted = (# "conv.u" value : unativeint #)
        uint64<unativeint> converted

    let private convI1FromInt64 (value : int64) : int32 =
        let converted = (# "conv.i1" value : int8 #)
        int32<int8> converted

    let private convI1FromInt32 (value : int32) : int32 =
        let converted = (# "conv.i1" value : int8 #)
        int32<int8> converted

    let private convI1FromFloat (value : float) : int32 =
        let converted = (# "conv.i1" value : int8 #)
        int32<int8> converted

    let private convI2FromInt64 (value : int64) : int32 =
        let converted = (# "conv.i2" value : int16 #)
        int32<int16> converted

    let private convI2FromInt32 (value : int32) : int32 =
        let converted = (# "conv.i2" value : int16 #)
        int32<int16> converted

    let private convI2FromFloat (value : float) : int32 =
        let converted = (# "conv.i2" value : int16 #)
        int32<int16> converted

    let private convI4FromInt64 (value : int64) : int32 = (# "conv.i4" value : int32 #)

    let private convI4FromFloat (value : float) : int32 = (# "conv.i4" value : int32 #)

    let private convI8FromFloat (value : float) : int64 = (# "conv.i8" value : int64 #)

    let private convU1FromInt64 (value : int64) : int32 =
        let converted = (# "conv.u1" value : uint8 #)
        int32<uint8> converted

    let private convU1FromInt32 (value : int32) : int32 =
        let converted = (# "conv.u1" value : uint8 #)
        int32<uint8> converted

    let private convU1FromFloat (value : float) : int32 =
        let converted = (# "conv.u1" value : uint8 #)
        int32<uint8> converted

    let private convU2FromInt64 (value : int64) : int32 =
        let converted = (# "conv.u2" value : uint16 #)
        int32<uint16> converted

    let private convU2FromInt32 (value : int32) : int32 =
        let converted = (# "conv.u2" value : uint16 #)
        int32<uint16> converted

    let private convU2FromFloat (value : float) : int32 =
        let converted = (# "conv.u2" value : uint16 #)
        int32<uint16> converted

    let private convU4FromInt64 (value : int64) : int32 =
        let converted = (# "conv.u4" value : uint32 #)
        int32<uint32> converted

    let private convU4FromInt32 (value : int32) : int32 =
        let converted = (# "conv.u4" value : uint32 #)
        int32<uint32> converted

    let private convU4FromFloat (value : float) : int32 =
        let converted = (# "conv.u4" value : uint32 #)
        int32<uint32> converted

    let private convU8FromFloat (value : float) : int64 =
        let converted = (# "conv.u8" value : uint64 #)
        int64<uint64> converted

    let private convR4FromInt32 (value : int32) : float =
        let converted = (# "conv.r4" value : float32 #)
        float<float32> converted

    let private convR4FromInt64 (value : int64) : float =
        let converted = (# "conv.r4" value : float32 #)
        float<float32> converted

    let private convR4FromFloat (value : float) : float =
        let converted = (# "conv.r4" value : float32 #)
        float<float32> converted

    let private convR8FromInt32 (value : int32) : float = (# "conv.r8" value : float #)

    let private convR8FromInt64 (value : int64) : float = (# "conv.r8" value : float #)

    let private convR8FromFloat (value : float) : float = (# "conv.r8" value : float #)

    let private convRUnFromInt32 (value : int32) : float = (# "conv.r.un" value : float #)

    let private convRUnFromInt64 (value : int64) : float = (# "conv.r.un" value : float #)

    /// The conversion performed by Conv_u.
    let toUnsignedNativeInt (value : EvalStackValue) : UnsignedNativeIntSource option =
        // Table III.8. Negative inputs are bit-reinterpreted (zero-extended
        // for Int32, same bits for Int64/NativeInt); the F# `uint32`/`uint64`
        // conversions from signed already do this.
        let managedPointerToUnsignedNativeInt (ptr : ManagedPointerSource) : UnsignedNativeIntSource =
            match ManagedPointerSource.tryStableAddressBits ptr with
            | Some bits -> uint64 bits |> UnsignedNativeIntSource.Verbatim
            | None -> UnsignedNativeIntSource.FromManagedPointer ptr

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
            | NativeIntSource.ManagedPointer ptr -> managedPointerToUnsignedNativeInt ptr |> Some
            | NativeIntSource.FunctionPointer methodInfo ->
                failwith $"Conv_U: refusing to convert function pointer %O{methodInfo} to unsigned native int"
            | NativeIntSource.FieldHandlePtr handle ->
                failwith $"Conv_U: refusing to convert RuntimeFieldHandle pointer %d{handle} to unsigned native int"
            | NativeIntSource.MethodHandlePtr handle ->
                failwith $"Conv_U: refusing to convert RuntimeMethodHandle pointer %d{handle} to unsigned native int"
            | NativeIntSource.TypeHandlePtr typeHandle ->
                failwith $"Conv_U: refusing to convert RuntimeTypeHandle pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.MethodTablePtr typeHandle ->
                failwith $"Conv_U: refusing to convert MethodTable pointer %O{typeHandle} to unsigned native int"
            | NativeIntSource.GcHandlePtr handle ->
                failwith $"Conv_U: refusing to convert GC handle pointer %O{handle} to unsigned native int"
            | NativeIntSource.AssemblyHandle assemblyName ->
                failwith $"Conv_U: refusing to convert assembly handle %s{assemblyName} to unsigned native int"
        | EvalStackValue.Float f -> convUFromFloat f |> UnsignedNativeIntSource.Verbatim |> Some
        | EvalStackValue.ManagedPointer managedPointerSource ->
            managedPointerToUnsignedNativeInt managedPointerSource |> Some
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null |> managedPointerToUnsignedNativeInt |> Some
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U" value

    /// The conversion performed by Conv_i.
    let toNativeInt (value : EvalStackValue) : NativeIntSource option =
        match value with
        | EvalStackValue.Int64 i -> i |> convIFromInt64 |> NativeIntSource.Verbatim |> Some
        | EvalStackValue.Int32 i -> i |> convIFromInt32 |> NativeIntSource.Verbatim |> Some
        | EvalStackValue.NativeInt src -> Some src
        | EvalStackValue.Float f -> f |> convIFromFloat |> NativeIntSource.Verbatim |> Some
        | EvalStackValue.ManagedPointer ptr -> NativeIntSource.ManagedPointer ptr |> Some
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null |> NativeIntSource.ManagedPointer |> Some
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I" value

    let convToInt8 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> convI1FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convI1FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_I1" src |> convI1FromInt64 |> Some
        | EvalStackValue.Float f -> convI1FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I1" value

    let convToInt16 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> convI2FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convI2FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_I2" src |> convI2FromInt64 |> Some
        | EvalStackValue.Float f -> convI2FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I2" value

    let convToInt32 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> Some i
        | EvalStackValue.Int64 i -> convI4FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_I4" src |> convI4FromInt64 |> Some
        | EvalStackValue.Float f -> convI4FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I4" value

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
            | NativeIntSource.MethodTablePtr _
            | NativeIntSource.MethodHandlePtr _
            | NativeIntSource.FieldHandlePtr _
            | NativeIntSource.GcHandlePtr _
            | NativeIntSource.AssemblyHandle _ -> failwith "refusing to convert pointer to int64"
        | EvalStackValue.Float f -> convI8FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_I8" value

    /// Then truncates to int64.
    let convToUInt64 (value : EvalStackValue) : int64 option =
        match value with
        | EvalStackValue.Int32 i -> Some (int64 (uint32 i))
        | EvalStackValue.Int64 int64 -> Some int64
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_U8" src |> Some
        | EvalStackValue.Float f -> convU8FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U8" value

    /// Then truncates to int32.
    let convToUInt8 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> convU1FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convU1FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_U1" src |> convU1FromInt64 |> Some
        | EvalStackValue.Float f -> convU1FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U1" value

    /// Then truncates to int32.
    let convToUInt16 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> convU2FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convU2FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_U2" src |> convU2FromInt64 |> Some
        | EvalStackValue.Float f -> convU2FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U2" value

    /// Then truncates to int32.
    let convToUInt32 (value : EvalStackValue) : int32 option =
        match value with
        | EvalStackValue.Int32 i -> convU4FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convU4FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_U4" src |> convU4FromInt64 |> Some
        | EvalStackValue.Float f -> convU4FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_U4" value

    let convToFloat32 (value : EvalStackValue) : float option =
        match value with
        | EvalStackValue.Int32 i -> convR4FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convR4FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_R4" src |> convR4FromInt64 |> Some
        | EvalStackValue.Float f -> convR4FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_R4" value

    let convToFloat64 (value : EvalStackValue) : float option =
        match value with
        | EvalStackValue.Int32 i -> convR8FromInt32 i |> Some
        | EvalStackValue.Int64 i -> convR8FromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_R8" src |> convR8FromInt64 |> Some
        | EvalStackValue.Float f -> convR8FromFloat f |> Some
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_R8" value

    let convUnsignedToFloat (value : EvalStackValue) : float option =
        match value with
        | EvalStackValue.Int32 i -> convRUnFromInt32 i |> Some
        | EvalStackValue.Int64 i -> convRUnFromInt64 i |> Some
        | EvalStackValue.NativeInt src -> nativeIntBitsForIntegerConversion "Conv_R_Un" src |> convRUnFromInt64 |> Some
        | EvalStackValue.Float _ -> failwith "Conv_R_Un: refusing to convert an existing float as unsigned integer"
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> failReferenceConversion "Conv_R_Un" value

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
            | CliRuntimePointer.TypeHandlePtr typeHandle ->
                NativeIntSource.TypeHandlePtr typeHandle |> EvalStackValue.NativeInt
            | CliRuntimePointer.FieldRegistryHandle ptrInt ->
                NativeIntSource.FieldHandlePtr ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.MethodRegistryHandle ptrInt ->
                NativeIntSource.MethodHandlePtr ptrInt |> EvalStackValue.NativeInt
            | CliRuntimePointer.MethodTablePtr typeHandle ->
                NativeIntSource.MethodTablePtr typeHandle |> EvalStackValue.NativeInt
            | CliRuntimePointer.Managed ptr -> ptr |> EvalStackValue.ManagedPointer
        | CliType.ValueType vt ->
            // Primitive-like single-field wrappers (IntPtr, RuntimeTypeHandle, enums, ...) all get
            // flattened to their underlying primitive on the stack. ECMA III.1.8 treats enums as
            // their underlying integer for every numeric/comparison opcode; flattening here means
            // cgt.un/clt.un/add/etc. don't need enum-specific arms. Storage stays wrapped;
            // `toCliTypeCoerced` re-wraps on the pop side when the target slot is primitive-like.
            if vt.PrimitiveLikeKind.IsSome then
                ofCliType (CliValueType.PrimitiveLikeField vt).Contents
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
                    | NativeIntSource.MethodHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.TypeHandlePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.MethodTablePtr f -> failwith $"TODO: {f}"
                    | NativeIntSource.GcHandlePtr f -> failwith $"TODO: {f}"
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
                        | CliRuntimePointer.TypeHandlePtr typeHandle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr typeHandle))
                        | CliRuntimePointer.FieldRegistryHandle ptr ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr ptr))
                        | CliRuntimePointer.MethodRegistryHandle ptr ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodHandlePtr ptr))
                        | CliRuntimePointer.MethodTablePtr typeHandle ->
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodTablePtr typeHandle))
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
                | NativeIntSource.MethodTablePtr _ ->
                    failwith "refusing to interpret method table pointer as an object ref"
                | NativeIntSource.MethodHandlePtr _ ->
                    failwith "refusing to interpret method handle ID as an object ref"
                | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to interpret field handle ID as an object ref"
                | NativeIntSource.GcHandlePtr _ -> failwith "refusing to interpret GC handle ID as an object ref"
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
                | NativeIntSource.TypeHandlePtr typeHandle ->
                    CliType.RuntimePointer (CliRuntimePointer.TypeHandlePtr typeHandle)
                | NativeIntSource.MethodTablePtr typeHandle ->
                    CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr typeHandle)
                | NativeIntSource.FieldHandlePtr ptr ->
                    CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle ptr)
                | NativeIntSource.MethodHandlePtr ptr ->
                    CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle ptr)
                | NativeIntSource.GcHandlePtr _ ->
                    failwith "refusing to coerce a GC handle pointer to a runtime pointer"
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
                let coerceContents (targetContents : CliType) (sourceContents : CliType) : CliType =
                    toCliTypeCoerced targetContents (ofCliType sourceContents)

                CliValueType.CoerceFrom coerceContents vt popped' |> CliType.ValueType
            | popped ->
                // A bare primitive popped into a ValueType slot is only legal for primitive-like
                // wrappers: the BCL handles (IntPtr, RuntimeTypeHandle, ...) flattened on push,
                // and enums, where CIL freely coerces between the underlying integer on the stack
                // and the enum slot. Both cases share the same rewrap: clone the target's single-
                // field skeleton and store the coerced primitive into `value__`/`_value`. A
                // single-field user-defined struct receiving a bare primitive is invalid IL; fail
                // loud so the misfire surfaces instead of silently degrading the storage shape.
                if vt.PrimitiveLikeKind.IsSome then
                    let field = CliValueType.PrimitiveLikeField vt
                    let newContents = toCliTypeCoerced field.Contents popped

                    let newField =
                        { field with
                            Contents = newContents
                        }

                    [ newField ] |> CliValueType.OfFieldsLike vt vt.Layout |> CliType.ValueType
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
        // Invariant: primitive-like wrapper structs (IntPtr, RuntimeTypeHandle, enums, ...) must
        // never appear on the eval stack as UserDefinedValueType; EvalStackValue.ofCliType flattens
        // them on push. A caller using Push' directly must respect this too.
        match v with
        | EvalStackValue.UserDefinedValueType vt when vt.PrimitiveLikeKind.IsSome ->
            failwith
                $"eval-stack invariant violated: primitive-like struct %O{vt.Declared} pushed as UserDefinedValueType (kind = %O{vt.PrimitiveLikeKind})"
        | _ -> ()

        {
            Values = v :: stack.Values
        }

    static member Push (v : CliType) (stack : EvalStack) : EvalStack =
        let v = EvalStackValue.ofCliType v

        EvalStack.Push' v stack

    static member PeekNthFromTop (n : int) (stack : EvalStack) : EvalStackValue option = stack.Values |> List.tryItem n
