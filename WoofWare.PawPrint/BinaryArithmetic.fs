namespace WoofWare.PawPrint

#nowarn "42"

type private FieldContainer =
    | HeapObject of ManagedHeapAddress
    | ByrefContainer of ManagedPointerSource

type private ArithmeticTarget =
    | NullTarget
    | ArrayTarget of ManagedHeapAddress * int
    | StringTarget of ManagedHeapAddress * int
    | FieldTarget of FieldContainer * FieldId
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
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            ArithmeticTarget.StringTarget (str, charIndex)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, field), []) ->
            ArithmeticTarget.FieldTarget (FieldContainer.HeapObject addr, field)
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.Field field :: revRest ->
                let parentPtr = ManagedPointerSource.Byref (root, List.rev revRest)
                ArithmeticTarget.FieldTarget (FieldContainer.ByrefContainer parentPtr, field)
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

    abstract Int32ManagedPtr :
        BaseClassTypes<DumpedAssembly> ->
        IlMachineState ->
        int32 ->
        ManagedPointerSource ->
            Choice<ManagedPointerSource, int>

    abstract ManagedPtrInt32 :
        BaseClassTypes<DumpedAssembly> ->
        IlMachineState ->
        ManagedPointerSource ->
        int32 ->
            Choice<ManagedPointerSource, int>

    abstract ManagedPtrManagedPtr :
        BaseClassTypes<DumpedAssembly> ->
        IlMachineState ->
        ManagedPointerSource ->
        ManagedPointerSource ->
            Choice<ManagedPointerSource, NativeIntSource>

    abstract Name : string

[<RequireQualifiedAccess>]
module ArithmeticOperation =
    let private verbatimInt64 (value : int64) : NativeIntSource = NativeIntSource.Verbatim value

    let private checkedAddInt32 (context : string) (a : int) (b : int) : int =
        let result = int64 a + int64 b

        if result > int64 System.Int32.MaxValue || result < int64 System.Int32.MinValue then
            failwith $"managed pointer arithmetic (%s{context}) overflowed int32 offset model: %d{a} + %d{b}"

        int result

    let private arrayElementHandle (arrObj : AllocatedArray) : ConcreteTypeHandle =
        match arrObj.ConcreteType with
        | ConcreteTypeHandle.OneDimArrayZero element -> element
        | ConcreteTypeHandle.Array (element, _) -> element
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> failwith $"array object has non-array concrete type: %O{arrObj.ConcreteType}"

    let private arrayElementSize
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        : int
        =
        let obj = state.ManagedHeap.Arrays.[arr]

        if obj.Length > 0 then
            CliType.sizeOf obj.Elements.[0]
        else
            let zero, _ =
                CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes (arrayElementHandle obj)

            CliType.sizeOf zero

    let private arrayBytePosition
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        : int64
        =
        int64 index * int64 (arrayElementSize baseClassTypes state arr)
        + int64 byteOffset

    let private crossArrayPointerDelta
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr1 : ManagedHeapAddress)
        (index1 : int)
        (byteOffset1 : int)
        (arr2 : ManagedHeapAddress)
        (index2 : int)
        (byteOffset2 : int)
        : NativeIntSource
        =
        if arr1 = arr2 then
            failwith "crossArrayPointerDelta called for two byrefs into the same array"

        let position1 = arrayBytePosition baseClassTypes state arr1 index1 byteOffset1
        let position2 = arrayBytePosition baseClassTypes state arr2 index2 byteOffset2

        NativeIntSource.syntheticCrossArrayByteOffset arr2 position2 arr1 position1

    let private subtractArrayByteLocations
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr1 : ManagedHeapAddress)
        (index1 : int)
        (offset1 : int)
        (arr2 : ManagedHeapAddress)
        (index2 : int)
        (offset2 : int)
        : NativeIntSource
        =
        if arr1 <> arr2 then
            // Distinct PawPrint arrays have no real byte distance. Keep the
            // result tagged so later arithmetic cannot silently compose it.
            crossArrayPointerDelta baseClassTypes state arr1 index1 offset1 arr2 index2 offset2
        else
            let elementSize = arrayElementSize baseClassTypes state arr1

            let cellDelta = (int64 index1 - int64 index2) * int64 elementSize
            let byteDelta = cellDelta + int64 (offset1 - offset2)

            verbatimInt64 byteDelta

    let private addInt32ManagedPtr
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (v : int32)
        (ptr : ManagedPointerSource)
        : Choice<ManagedPointerSource, int>
        =
        match ArithmeticTarget.decompose ptr with
        | ArithmeticTarget.NullTarget -> Choice2Of2 v
        | ArithmeticTarget.ArrayTarget (arr, index) ->
            let index = checkedAddInt32 "array index" index v

            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), [])
            |> Choice1Of2
        | ArithmeticTarget.StringTarget (str, charIndex) ->
            let charIndex = checkedAddInt32 "string character index" charIndex v

            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), [])
            |> Choice1Of2
        | ArithmeticTarget.FieldTarget (container, field) ->
            let obj = ArithmeticTarget.getFieldContainerValue state container

            let offset, _ = CliType.getFieldLayoutById field obj
            let offset = checkedAddInt32 "field byte offset" offset v

            match CliType.getFieldAt offset obj with
            | None -> failwith "TODO: couldn't identify field at offset"
            | Some field ->
                let newField = CliConcreteField.ToCliField(field).Id

                let newPtr =
                    match container with
                    | FieldContainer.HeapObject addr ->
                        ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, newField), [])
                    | FieldContainer.ByrefContainer parentPtr ->
                        ManagedPointerSource.appendProjection (ByrefProjection.Field newField) parentPtr

                Choice1Of2 newPtr
        | ArithmeticTarget.ByteViewTarget (root, prefixProjs, reinterpretTy, byteOffset) ->
            // Walk the byte cursor under the trailing reinterpret. The reinterpret
            // stays (it's the type view the caller set up); the byte offset
            // accumulates. A zero result drops the ByteOffset so stripping
            // behaviour and byref equality continue to normalise.
            let newOffset = checkedAddInt32 "byte-view offset" byteOffset v

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
            ManagedPointerSource.Byref (root, prefixProjs @ tailProjs)
            |> ManagedPointerSource.normaliseArrayByteOffset (arrayElementSize baseClassTypes state)
            |> ManagedPointerSource.normaliseStringByteOffset
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

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice1Of2 ptr2
                | _, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | _, _ -> failwith "refusing to add two managed pointers"

            member _.Int32ManagedPtr baseClassTypes state val1 ptr2 =
                addInt32ManagedPtr baseClassTypes state val1 ptr2

            member _.ManagedPtrInt32 baseClassTypes state ptr1 val2 =
                addInt32ManagedPtr baseClassTypes state val2 ptr1

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

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice1Of2 ptr2
                | _, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | _, _ -> failwith "refusing to add two managed pointers"

            member _.Int32ManagedPtr baseClassTypes state val1 ptr2 =
                addInt32ManagedPtr baseClassTypes state val1 ptr2

            member _.ManagedPtrInt32 baseClassTypes state ptr1 val2 =
                addInt32ManagedPtr baseClassTypes state val2 ptr1

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

            member _.ManagedPtrManagedPtr baseClassTypes state ptr1 ptr2 =
                match ptr1, ptr2 with
                | ptr1, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | ManagedPointerSource.Null, _ -> failwith "refusing to create negative pointer"
                | ManagedPointerSource.Byref (ByrefRoot.Argument _, _), _
                | _, ManagedPointerSource.Byref (ByrefRoot.Argument _, _) ->
                    failwith $"refusing to operate on pointers to arguments: %O{ptr1} and %O{ptr2}"
                | ManagedPointerSource.Byref _, ManagedPointerSource.Byref _ ->
                    match ArithmeticTarget.decompose ptr1, ArithmeticTarget.decompose ptr2 with
                    | ArithmeticTarget.ArrayTarget (arr1, index1), ArithmeticTarget.ArrayTarget (arr2, index2) ->
                        subtractArrayByteLocations baseClassTypes state arr1 index1 0 arr2 index2 0
                        |> Choice2Of2
                    | ArithmeticTarget.StringTarget (str1, index1), ArithmeticTarget.StringTarget (str2, index2) ->
                        if str1 <> str2 then
                            failwith
                                $"refusing to subtract character pointers into different strings: %O{str1} vs %O{str2}"

                        (int64 index1 - int64 index2) * 2L |> verbatimInt64 |> Choice2Of2
                    | ArithmeticTarget.ByteViewTarget (ByrefRoot.ArrayElement (arr1, index1), prefix1, _, offset1),
                      ArithmeticTarget.ByteViewTarget (ByrefRoot.ArrayElement (arr2, index2), prefix2, _, offset2) when
                        prefix1 = prefix2
                        ->
                        subtractArrayByteLocations baseClassTypes state arr1 index1 offset1 arr2 index2 offset2
                        |> Choice2Of2
                    | ArithmeticTarget.FieldTarget (container1, field1),
                      ArithmeticTarget.FieldTarget (container2, field2) ->
                        if container1 <> container2 then
                            failwith
                                $"refusing to subtract pointers to fields of different containers: %O{container1} vs %O{container2}"

                        let obj1 = ArithmeticTarget.getFieldContainerValue state container1
                        let obj2 = ArithmeticTarget.getFieldContainerValue state container2

                        let offset1, _ = CliType.getFieldLayoutById field1 obj1
                        let offset2, _ = CliType.getFieldLayoutById field2 obj2

                        int64 offset1 - int64 offset2 |> verbatimInt64 |> Choice2Of2
                    | ArithmeticTarget.ByteViewTarget (root1, prefix1, _, off1),
                      ArithmeticTarget.ByteViewTarget (root2, prefix2, _, off2) when root1 = root2 && prefix1 = prefix2 ->
                        // Same underlying storage; subtraction is the byte-offset
                        // delta regardless of which `ReinterpretAs` type was used
                        // on each side (the view is address-preserving).
                        int64 off1 - int64 off2 |> verbatimInt64 |> Choice2Of2
                    | ArithmeticTarget.ArrayTarget _, _
                    | _, ArithmeticTarget.ArrayTarget _ ->
                        failwith
                            $"refusing to subtract array element pointer from incompatible pointer: %O{ptr1} vs %O{ptr2}"
                    | ArithmeticTarget.StringTarget _, _
                    | _, ArithmeticTarget.StringTarget _ ->
                        failwith
                            $"refusing to subtract string character pointer from incompatible pointer: %O{ptr1} vs %O{ptr2}"
                    | _, _ -> failwith "TODO"

            member _.Int32ManagedPtr _ state val1 ptr2 =
                match ptr2 with
                | ManagedPointerSource.Null -> Choice2Of2 val1
                | _ -> failwith "refusing to subtract a pointer"

            member _.ManagedPtrInt32 baseClassTypes state ptr1 val2 =
                if val2 = System.Int32.MinValue then
                    failwith
                        "managed pointer subtraction by Int32.MinValue would overflow the interpreter's int32 offset model"

                addInt32ManagedPtr baseClassTypes state (-val2) ptr1

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

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, ManagedPointerSource.Null -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, _ -> failwith "refusing to multiply two managed pointers"

            member _.Int32ManagedPtr _ state a ptr = mulInt32ManagedPtr state a ptr
            member _.ManagedPtrInt32 _ state ptr a = mulInt32ManagedPtr state a ptr

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

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 = failwith "refusing to rem pointers"

            member _.Int32ManagedPtr _ _ a ptr = failwith "refusing to rem pointer"

            member _.ManagedPtrInt32 _ _ ptr a = failwith "refusing to rem pointer"

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

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, ManagedPointerSource.Null -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, _ -> failwith "refusing to multiply two managed pointers"

            member _.Int32ManagedPtr _ state a ptr = mulInt32ManagedPtr state a ptr
            member _.ManagedPtrInt32 _ state a ptr = mulInt32ManagedPtr state ptr a

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

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, _ -> failwith "refusing to divide two managed pointers"

            member _.Int32ManagedPtr _ _ a ptr =
                if a = 0 then
                    Choice2Of2 0
                else
                    failwith "refusing to divide pointers"

            member _.ManagedPtrInt32 _ _ ptr a =
                if a = 1 then
                    Choice1Of2 ptr
                else
                    failwith "refusing to divide a pointer"

            member _.Name = "div"
        }

[<RequireQualifiedAccess>]
module BinaryArithmetic =
    let execute
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
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
            match op.Int32ManagedPtr baseClassTypes state val1 val2 with
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

            match op.Int32ManagedPtr baseClassTypes state val1 val2 with
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

            match op.ManagedPtrInt32 baseClassTypes state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<int32> result))
        | EvalStackValue.ObjectRef val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.NullObjectRef, EvalStackValue.NativeInt _ -> failwith ""
        | EvalStackValue.ManagedPointer val1, EvalStackValue.Int32 val2 ->
            match op.ManagedPtrInt32 baseClassTypes state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<int32> result))
        | EvalStackValue.ObjectRef val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.NullObjectRef, EvalStackValue.Int32 _ -> failwith ""
        | EvalStackValue.ManagedPointer val1, EvalStackValue.ManagedPointer val2 ->
            match op.ManagedPtrManagedPtr baseClassTypes state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt result
        | val1, val2 -> failwith $"invalid %s{op.Name} operation: {val1} and {val2}"
