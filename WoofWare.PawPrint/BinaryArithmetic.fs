namespace WoofWare.PawPrint

#nowarn "42"

type IArithmeticOperation =
    abstract Int32Int32 : int32 -> int32 -> int32
    abstract Int64Int64 : int64 -> int64 -> int64
    abstract FloatFloat : float -> float -> float
    abstract NativeIntNativeInt : nativeint -> nativeint -> nativeint
    abstract Name : string

[<RequireQualifiedAccess>]
module ArithmeticOperation =
    let add =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "add" a b : int32 #)
            member _.Int64Int64 a b = (# "add" a b : int64 #)
            member _.FloatFloat a b = (# "add" a b : float #)
            member _.NativeIntNativeInt a b = (# "add" a b : nativeint #)
            member _.Name = "add"
        }

    let sub =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "sub" a b : int32 #)
            member _.Int64Int64 a b = (# "sub" a b : int64 #)
            member _.FloatFloat a b = (# "sub" a b : float #)
            member _.NativeIntNativeInt a b = (# "sub" a b : nativeint #)
            member _.Name = "sub"
        }

    let mul =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul" a b : int32 #)
            member _.Int64Int64 a b = (# "mul" a b : int64 #)
            member _.FloatFloat a b = (# "mul" a b : float #)
            member _.NativeIntNativeInt a b = (# "mul" a b : nativeint #)
            member _.Name = "mul"
        }

    let mulOvf =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul.ovf" a b : int32 #)
            member _.Int64Int64 a b = (# "mul.ovf" a b : int64 #)
            member _.FloatFloat a b = (# "mul.ovf" a b : float #)
            member _.NativeIntNativeInt a b = (# "mul.ovf" a b : nativeint #)
            member _.Name = "mul_ovf"
        }

[<RequireQualifiedAccess>]
module BinaryArithmetic =
    let execute (op : IArithmeticOperation) (val1 : EvalStackValue) (val2 : EvalStackValue) : EvalStackValue =
        // see table at https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes.add?view=net-9.0
        match val1, val2 with
        | EvalStackValue.Int32 val1, EvalStackValue.Int32 val2 -> op.Int32Int32 val1 val2 |> EvalStackValue.Int32
        | EvalStackValue.Int32 val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.NativeInt
        | EvalStackValue.Int32 val1, EvalStackValue.ManagedPointer val2 -> failwith "" |> EvalStackValue.ManagedPointer
        | EvalStackValue.Int32 val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
        | EvalStackValue.Int64 val1, EvalStackValue.Int64 val2 -> op.Int64Int64 val1 val2 |> EvalStackValue.Int64
        | EvalStackValue.NativeInt val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.NativeInt
        | EvalStackValue.NativeInt val1, EvalStackValue.NativeInt val2 ->
            let val1 =
                match val1 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to multiply non-verbatim native int %O{v}"

            let val2 =
                match val2 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to multiply non-verbatim native int %O{v}"

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
        | val1, val2 -> failwith $"invalid %s{op.Name} operation: {val1} and {val2}"
