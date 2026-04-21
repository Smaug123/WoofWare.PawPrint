namespace WoofWare.PawPrint

#nowarn "42"

type private FieldContainer =
    | HeapObject of ManagedHeapAddress
    | ByrefContainer of ManagedPointerSource

type private ArithmeticTarget =
    | NullTarget
    | ArrayTarget of ManagedHeapAddress * int
    | FieldTarget of FieldContainer * string
    /// A byref ending in `ReinterpretAs T [; ByteOffset n]`. Pointer arithmetic
    /// walks the byte cursor rather than the underlying storage. `prefixProjs`
    /// is whatever came before the reinterpret; the caller rebuilds the byref
    /// by appending `[ReinterpretAs reinterpretTy; ByteOffset <new>]` (or
    /// dropping the ByteOffset when it returns to zero).
    | ByteViewTarget of
        root : ByrefRoot *
        prefixProjs : ByrefProjection list *
        reinterpretTy : ConcreteType<ConcreteTypeHandle> *
        byteOffset : int

[<RequireQualifiedAccess>]
module private ArithmeticTarget =

    let decompose (ptr : ManagedPointerSource) : ArithmeticTarget =
        match ptr with
        | ManagedPointerSource.Null -> ArithmeticTarget.NullTarget
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            ArithmeticTarget.ArrayTarget (arr, index)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, fieldName), []) ->
            ArithmeticTarget.FieldTarget (FieldContainer.HeapObject addr, fieldName)
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.Field fieldName :: revRest ->
                let parentPtr = ManagedPointerSource.Byref (root, List.rev revRest)
                ArithmeticTarget.FieldTarget (FieldContainer.ByrefContainer parentPtr, fieldName)
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs ty :: revRest ->
                ArithmeticTarget.ByteViewTarget (root, List.rev revRest, ty, n)
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: {ptr} (this is an interpreter bug)"
            | ByrefProjection.ReinterpretAs ty :: revRest ->
                ArithmeticTarget.ByteViewTarget (root, List.rev revRest, ty, 0)
            | [] -> failwith $"refusing to do pointer arithmetic on a bare stack slot address: {ptr}"

    let getFieldContainerValue (state : IlMachineState) (container : FieldContainer) : CliType =
        match container with
        | FieldContainer.HeapObject addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | FieldContainer.ByrefContainer ptr -> IlMachineState.readManagedByref state ptr

type IArithmeticOperation =
    abstract Int32Int32 : int32 -> int32 -> int32
    abstract Int32NativeInt : int32 -> nativeint -> nativeint
    abstract NativeIntInt32 : nativeint -> int32 -> nativeint
    abstract Int64Int64 : int64 -> int64 -> int64
    abstract FloatFloat : float -> float -> float
    abstract NativeIntNativeInt : nativeint -> nativeint -> nativeint
    abstract Int32ManagedPtr : IlMachineState -> int32 -> ManagedPointerSource -> Choice<ManagedPointerSource, int>
    abstract ManagedPtrInt32 : IlMachineState -> ManagedPointerSource -> int32 -> Choice<ManagedPointerSource, int>

    abstract ManagedPtrManagedPtr :
        IlMachineState -> ManagedPointerSource -> ManagedPointerSource -> Choice<ManagedPointerSource, nativeint>

    abstract Name : string

[<RequireQualifiedAccess>]
module ArithmeticOperation =
    let private addInt32ManagedPtr
        (state : IlMachineState)
        (v : int32)
        (ptr : ManagedPointerSource)
        : Choice<ManagedPointerSource, int>
        =
        match ArithmeticTarget.decompose ptr with
        | ArithmeticTarget.NullTarget -> Choice2Of2 v
        | ArithmeticTarget.ArrayTarget (_arr, _index) -> failwith "TODO: arrays"
        | ArithmeticTarget.FieldTarget (container, fieldName) ->
            let obj = ArithmeticTarget.getFieldContainerValue state container

            let offset, _ = CliType.getFieldLayout fieldName obj

            match CliType.getFieldAt (offset + v) obj with
            | None -> failwith "TODO: couldn't identify field at offset"
            | Some field ->
                let newFieldName = CliConcreteField.ToCliField(field).Name

                let newPtr =
                    match container with
                    | FieldContainer.HeapObject addr ->
                        ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, newFieldName), [])
                    | FieldContainer.ByrefContainer parentPtr ->
                        ManagedPointerSource.appendProjection (ByrefProjection.Field newFieldName) parentPtr

                Choice1Of2 newPtr
        | ArithmeticTarget.ByteViewTarget (root, prefixProjs, reinterpretTy, byteOffset) ->
            // Walk the byte cursor under the trailing reinterpret. The reinterpret
            // stays (it's the type view the caller set up); the byte offset
            // accumulates. A zero result drops the ByteOffset so stripping
            // behaviour and byref equality continue to normalise.
            let newOffset = byteOffset + v

            let tailProjs =
                if newOffset = 0 then
                    [ ByrefProjection.ReinterpretAs reinterpretTy ]
                else
                    [
                        ByrefProjection.ReinterpretAs reinterpretTy
                        ByrefProjection.ByteOffset newOffset
                    ]

            // Fold whole cells into the array index when the root is an array:
            // two byrefs denoting the same byte location must share one
            // structural form, else equality (Unsafe.AreSame, ceq) spuriously
            // returns false when the cursor lands on another cell boundary.
            let cellSizeOf (addr : ManagedHeapAddress) : int =
                let obj = state.ManagedHeap.Arrays.[addr]

                if obj.Length = 0 then
                    0
                else
                    CliType.sizeOf obj.Elements.[0]

            ManagedPointerSource.Byref (root, prefixProjs @ tailProjs)
            |> ManagedPointerSource.normaliseArrayByteOffset cellSizeOf
            |> Choice1Of2

    let private mulInt32ManagedPtr
        (state : IlMachineState)
        (v : int32)
        (ptr : ManagedPointerSource)
        : Choice<ManagedPointerSource, int>
        =
        if v = 0 then
            Choice2Of2 0
        elif v = 1 then
            Choice1Of2 ptr
        else

        match ptr with
        | ManagedPointerSource.Null -> Choice2Of2 0
        | _ -> failwith "refusing to multiply pointers"

    let add =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "add" a b : int32 #)
            member _.Int64Int64 a b = (# "add" a b : int64 #)
            member _.FloatFloat a b = (# "add" a b : float #)
            member _.NativeIntNativeInt a b = (# "add" a b : nativeint #)
            member _.Int32NativeInt a b = (# "add" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "add" a b : nativeint #)

            member _.ManagedPtrManagedPtr _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice1Of2 ptr2
                | _, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | _, _ -> failwith "refusing to add two managed pointers"

            member _.Int32ManagedPtr state val1 ptr2 = addInt32ManagedPtr state val1 ptr2
            member _.ManagedPtrInt32 state ptr1 val2 = addInt32ManagedPtr state val2 ptr1

            member _.Name = "add"
        }

    let addOvf =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "add.ovf" a b : int32 #)
            member _.Int64Int64 a b = (# "add.ovf" a b : int64 #)
            member _.FloatFloat a b = (# "add.ovf" a b : float #)
            member _.NativeIntNativeInt a b = (# "add.ovf" a b : nativeint #)
            member _.Int32NativeInt a b = (# "add.ovf" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "add.ovf" a b : nativeint #)

            member _.ManagedPtrManagedPtr _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice1Of2 ptr2
                | _, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | _, _ -> failwith "refusing to add two managed pointers"

            member _.Int32ManagedPtr state val1 ptr2 = addInt32ManagedPtr state val1 ptr2
            member _.ManagedPtrInt32 state ptr1 val2 = addInt32ManagedPtr state val2 ptr1

            member _.Name = "add.ovf"
        }

    let sub =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "sub" a b : int32 #)
            member _.Int64Int64 a b = (# "sub" a b : int64 #)
            member _.FloatFloat a b = (# "sub" a b : float #)
            member _.NativeIntNativeInt a b = (# "sub" a b : nativeint #)
            member _.Int32NativeInt a b = (# "sub" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "sub" a b : nativeint #)

            member _.ManagedPtrManagedPtr state ptr1 ptr2 =
                match ptr1, ptr2 with
                | ptr1, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | ManagedPointerSource.Null, _ -> failwith "refusing to create negative pointer"
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr1, index1), []),
                  ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr2, index2), []) ->
                    if arr1 <> arr2 then
                        failwith "refusing to operate on pointers to different arrays"

                    (index1 - index2) |> nativeint |> Choice2Of2
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement _, _), _ ->
                    failwith $"refusing to operate on array index ptr vs %O{ptr2}"
                | ManagedPointerSource.Byref (ByrefRoot.Argument _, _), _
                | _, ManagedPointerSource.Byref (ByrefRoot.Argument _, _) ->
                    failwith $"refusing to operate on pointers to arguments: %O{ptr1} and %O{ptr2}"
                | ManagedPointerSource.Byref _, ManagedPointerSource.Byref _ ->
                    match ArithmeticTarget.decompose ptr1, ArithmeticTarget.decompose ptr2 with
                    | ArithmeticTarget.FieldTarget (container1, fieldName1),
                      ArithmeticTarget.FieldTarget (container2, fieldName2) ->
                        if container1 <> container2 then
                            failwith
                                $"refusing to subtract pointers to fields of different containers: %O{container1} vs %O{container2}"

                        let obj1 = ArithmeticTarget.getFieldContainerValue state container1
                        let obj2 = ArithmeticTarget.getFieldContainerValue state container2

                        let offset1, _ = CliType.getFieldLayout fieldName1 obj1
                        let offset2, _ = CliType.getFieldLayout fieldName2 obj2

                        (offset1 - offset2) |> nativeint |> Choice2Of2
                    | ArithmeticTarget.ByteViewTarget (root1, prefix1, _, off1),
                      ArithmeticTarget.ByteViewTarget (root2, prefix2, _, off2) when root1 = root2 && prefix1 = prefix2 ->
                        // Same underlying storage; subtraction is the byte-offset
                        // delta regardless of which `ReinterpretAs` type was used
                        // on each side (the view is address-preserving).
                        (off1 - off2) |> nativeint |> Choice2Of2
                    | _, _ -> failwith "TODO"

            member _.Int32ManagedPtr state val1 ptr2 =
                match ptr2 with
                | ManagedPointerSource.Null -> Choice2Of2 val1
                | _ -> failwith "refusing to subtract a pointer"

            member _.ManagedPtrInt32 state ptr1 val2 = failwith "TODO: subtract from pointer"

            member _.Name = "sub"
        }

    let mul =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul" a b : int32 #)
            member _.Int64Int64 a b = (# "mul" a b : int64 #)
            member _.FloatFloat a b = (# "mul" a b : float #)
            member _.NativeIntNativeInt a b = (# "mul" a b : nativeint #)
            member _.Int32NativeInt a b = (# "mul" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "mul" a b : nativeint #)

            member _.ManagedPtrManagedPtr _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (nativeint 0)
                | _, ManagedPointerSource.Null -> Choice2Of2 (nativeint 0)
                | _, _ -> failwith "refusing to multiply two managed pointers"

            member _.Int32ManagedPtr state a ptr = mulInt32ManagedPtr state a ptr
            member _.ManagedPtrInt32 state ptr a = mulInt32ManagedPtr state a ptr

            member _.Name = "mul"
        }

    let rem =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "rem" a b : int32 #)
            member _.Int64Int64 a b = (# "rem" a b : int64 #)
            member _.FloatFloat a b = (# "rem" a b : float #)
            member _.NativeIntNativeInt a b = (# "rem" a b : nativeint #)
            member _.Int32NativeInt a b = (# "rem" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "rem" a b : nativeint #)

            member _.ManagedPtrManagedPtr _ ptr1 ptr2 = failwith "refusing to rem pointers"

            member _.Int32ManagedPtr _ a ptr = failwith "refusing to rem pointer"

            member _.ManagedPtrInt32 _ ptr a = failwith "refusing to rem pointer"

            member _.Name = "rem"
        }

    let mulOvf =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul.ovf" a b : int32 #)
            member _.Int64Int64 a b = (# "mul.ovf" a b : int64 #)
            member _.FloatFloat a b = (# "mul.ovf" a b : float #)
            member _.NativeIntNativeInt a b = (# "mul.ovf" a b : nativeint #)
            member _.Int32NativeInt a b = (# "mul.ovf" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "mul.ovf" a b : nativeint #)

            member _.ManagedPtrManagedPtr _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (nativeint 0)
                | _, ManagedPointerSource.Null -> Choice2Of2 (nativeint 0)
                | _, _ -> failwith "refusing to multiply two managed pointers"

            member _.Int32ManagedPtr state a ptr = mulInt32ManagedPtr state a ptr
            member _.ManagedPtrInt32 state a ptr = mulInt32ManagedPtr state ptr a

            member _.Name = "mul_ovf"
        }

    let div =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "div" a b : int32 #)
            member _.Int64Int64 a b = (# "div" a b : int64 #)
            member _.FloatFloat a b = (# "div" a b : float #)
            member _.NativeIntNativeInt a b = (# "div" a b : nativeint #)
            member _.Int32NativeInt a b = (# "div" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "div" a b : nativeint #)

            member _.ManagedPtrManagedPtr _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (nativeint 0)
                | _, _ -> failwith "refusing to divide two managed pointers"

            member _.Int32ManagedPtr _ a ptr =
                if a = 0 then
                    Choice2Of2 0
                else
                    failwith "refusing to divide pointers"

            member _.ManagedPtrInt32 _ ptr a =
                if a = 1 then
                    Choice1Of2 ptr
                else
                    failwith "refusing to divide a pointer"

            member _.Name = "div"
        }

[<RequireQualifiedAccess>]
module BinaryArithmetic =
    let execute
        (op : IArithmeticOperation)
        (state : IlMachineState)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : EvalStackValue
        =
        // see table at https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes.add?view=net-9.0
        match val1, val2 with
        | EvalStackValue.Int32 val1, EvalStackValue.Int32 val2 -> op.Int32Int32 val1 val2 |> EvalStackValue.Int32
        | EvalStackValue.Int32 val1, EvalStackValue.NativeInt val2 ->
            let val2 =
                match val2 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to operate on non-verbatim native int %O{v}"

            op.Int32NativeInt val1 val2
            |> int64<nativeint>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | EvalStackValue.Int32 val1, EvalStackValue.ManagedPointer val2 ->
            match op.Int32ManagedPtr state val1 val2 with
            | Choice1Of2 v -> EvalStackValue.ManagedPointer v
            | Choice2Of2 i -> EvalStackValue.Int32 i
        | EvalStackValue.Int32 val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.Int32 _, EvalStackValue.NullObjectRef -> failwith ""
        | EvalStackValue.Int64 val1, EvalStackValue.Int64 val2 -> op.Int64Int64 val1 val2 |> EvalStackValue.Int64
        | EvalStackValue.NativeInt val1, EvalStackValue.Int32 val2 ->
            let val1 =
                match val1 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to operate on non-verbatim native int %O{v}"

            op.NativeIntInt32 val1 val2
            |> int64<nativeint>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | EvalStackValue.NativeInt val1, EvalStackValue.NativeInt val2 ->
            let val1 =
                match val1 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to operate on non-verbatim native int %O{v}"

            let val2 =
                match val2 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to operate on non-verbatim native int %O{v}"

            op.NativeIntNativeInt val1 val2
            |> int64<nativeint>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
        | EvalStackValue.NativeInt val1, EvalStackValue.ManagedPointer val2 ->
            let val1 =
                match val1 with
                | NativeIntSource.Verbatim n ->
                    if n > int64<int32> System.Int32.MaxValue || n < int64<int32> System.Int32.MinValue then
                        failwith
                            $"managed pointer arithmetic (%s{op.Name}): nativeint offset does not fit in int32: %d{n}"

                    int32<int64> n
                | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0
                | v ->
                    failwith
                        $"managed pointer arithmetic (%s{op.Name}): refusing to use non-verbatim native int %O{v} as pointer offset"

            match op.Int32ManagedPtr state val1 val2 with
            | Choice1Of2 v -> EvalStackValue.ManagedPointer v
            | Choice2Of2 i -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<int32> i))
        | EvalStackValue.NativeInt val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.NativeInt _, EvalStackValue.NullObjectRef -> failwith ""
        | EvalStackValue.Float val1, EvalStackValue.Float val2 -> op.FloatFloat val1 val2 |> EvalStackValue.Float
        | EvalStackValue.ManagedPointer val1, EvalStackValue.NativeInt val2 ->
            let val2 =
                match val2 with
                | NativeIntSource.Verbatim n ->
                    if n > int64<int32> System.Int32.MaxValue || n < int64<int32> System.Int32.MinValue then
                        failwith
                            $"managed pointer arithmetic (%s{op.Name}): nativeint offset does not fit in int32: %d{n}"

                    int32<int64> n
                | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0
                | v ->
                    failwith
                        $"managed pointer arithmetic (%s{op.Name}): refusing to use non-verbatim native int %O{v} as pointer offset"

            match op.ManagedPtrInt32 state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<int32> result))
        | EvalStackValue.ObjectRef val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.NullObjectRef, EvalStackValue.NativeInt _ -> failwith ""
        | EvalStackValue.ManagedPointer val1, EvalStackValue.Int32 val2 ->
            match op.ManagedPtrInt32 state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<int32> result))
        | EvalStackValue.ObjectRef val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.NullObjectRef, EvalStackValue.Int32 _ -> failwith ""
        | EvalStackValue.ManagedPointer val1, EvalStackValue.ManagedPointer val2 ->
            match op.ManagedPtrManagedPtr state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<nativeint> result))
        | val1, val2 -> failwith $"invalid %s{op.Name} operation: {val1} and {val2}"
