namespace WoofWare.PawPrint

#nowarn "42"

type IArithmeticOperation =
    abstract Int32Int32 : int32 -> int32 -> int32
    abstract Int32NativeInt : int32 -> nativeint -> nativeint
    abstract NativeIntInt32 : nativeint -> int32 -> nativeint
    abstract Int64Int64 : int64 -> int64 -> int64
    abstract FloatFloat : float -> float -> float
    abstract NativeIntNativeInt : nativeint -> nativeint -> nativeint
    abstract Int32ManagedPtr : IlMachineState -> int32 -> ManagedPointerSource -> Choice<ManagedPointerSource, int>

    abstract ManagedPtrManagedPtr :
        IlMachineState -> ManagedPointerSource -> ManagedPointerSource -> Choice<ManagedPointerSource, nativeint>

    abstract Name : string

[<RequireQualifiedAccess>]
module ArithmeticOperation =
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

            member _.Int32ManagedPtr state val1 ptr2 =
                match ptr2 with
                | LocalVariable (sourceThread, methodFrame, whichVar) ->
                    failwith "refusing to add to a local variable address"
                | Argument (sourceThread, methodFrame, whichVar) -> failwith "refusing to add to an argument address"
                | Heap managedHeapAddress -> failwith "refusing to add to a heap address"
                | ArrayIndex (arr, index) -> failwith "TODO: arrays"
                | Field (src, fieldName) ->
                    let obj = IlMachineState.dereferencePointer state src
                    let offset, _ = CliType.getFieldLayout fieldName obj

                    match CliType.getFieldAt (offset + val1) obj with
                    | None -> failwith "TODO: couldn't identify field at offset"
                    | Some field ->
                        ManagedPointerSource.Field (src, CliConcreteField.ToCliField(field).Name)
                        |> Choice1Of2
                | Null -> Choice2Of2 val1
                | InterpretedAsType (managedPointerSource, concreteType) -> failwith "todo"

            member _.Name = "add"
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
                | ManagedPointerSource.ArrayIndex (arr1, index1), ManagedPointerSource.ArrayIndex (arr2, index2) ->
                    if arr1 <> arr2 then
                        failwith "refusing to operate on pointers to different arrays"

                    (index1 - index2) |> nativeint |> Choice2Of2
                | ManagedPointerSource.ArrayIndex _, _ -> failwith $"refusing to operate on array index ptr vs %O{ptr2}"
                | ManagedPointerSource.Argument _, _
                | _, ManagedPointerSource.Argument _ ->
                    failwith $"refusing to operate on pointers to arguments: %O{ptr1} and %O{ptr2}"
                | ManagedPointerSource.Field (obj1, fieldName1), ManagedPointerSource.Field (obj2, fieldName2) ->
                    if obj1 <> obj2 then
                        failwith "refusing to operate on field pointers in different objects"

                    let obj = IlMachineState.dereferencePointer state obj1

                    let offset1, _ = CliType.getFieldLayout fieldName1 obj
                    let offset2, _ = CliType.getFieldLayout fieldName2 obj

                    (offset1 - offset2) |> nativeint |> Choice2Of2
                | _, _ -> failwith "TODO"

            member _.Int32ManagedPtr state val1 ptr2 =
                match ptr2 with
                | ManagedPointerSource.Null -> Choice2Of2 val1
                | _ -> failwith "refusing to subtract a pointer"

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

            member _.Int32ManagedPtr _ a ptr =
                if a = 0 then
                    Choice2Of2 0
                elif a = 1 then
                    Choice1Of2 ptr
                else

                match ptr with
                | ManagedPointerSource.Null -> Choice2Of2 0
                | _ -> failwith "refusing to multiply pointers"

            member _.Name = "mul"
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

            member _.Int32ManagedPtr _ a ptr =
                if a = 0 then
                    Choice2Of2 0
                elif a = 1 then
                    Choice1Of2 ptr
                else

                match ptr with
                | ManagedPointerSource.Null -> Choice2Of2 0
                | _ -> failwith "refusing to multiply pointers"

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
            failwith "" |> EvalStackValue.ManagedPointer
        | EvalStackValue.NativeInt val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.Float val1, EvalStackValue.Float val2 -> op.FloatFloat val1 val2 |> EvalStackValue.Float
        | EvalStackValue.ManagedPointer val1, EvalStackValue.NativeInt val2 ->
            failwith "" |> EvalStackValue.ManagedPointer
        | EvalStackValue.ObjectRef val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.ManagedPointer val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.ManagedPointer
        | EvalStackValue.ObjectRef val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.ManagedPointer val1, EvalStackValue.ManagedPointer val2 ->
            match op.ManagedPtrManagedPtr state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64<nativeint> result))
        | val1, val2 -> failwith $"invalid %s{op.Name} operation: {val1} and {val2}"
