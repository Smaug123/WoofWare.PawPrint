namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module EvalStackValueComparisons =

    let clt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> var1 < var2
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 < var2
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            failwith $"Clt instruction invalid for comparing object refs, {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, other -> failwith $"invalid comparison, ref %O{var1} vs %O{other}"
        | other, EvalStackValue.ObjectRef var2 -> failwith $"invalid comparison, %O{other} vs ref %O{var2}"
        | EvalStackValue.Float i, other -> failwith $"invalid comparison, float %f{i} vs %O{other}"
        | other, EvalStackValue.Float i -> failwith $"invalid comparison, %O{other} vs float %f{i}"
        | EvalStackValue.Int64 i, other -> failwith $"invalid comparison, int64 %i{i} vs %O{other}"
        | other, EvalStackValue.Int64 i -> failwith $"invalid comparison, %O{other} vs int64 %i{i}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 < var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: Clt Int32 vs NativeInt comparison unimplemented"
        | EvalStackValue.Int32 i, other -> failwith $"invalid comparison, int32 %i{i} vs %O{other}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: Clt NativeInt vs Int32 comparison unimplemented"
        | other, EvalStackValue.Int32 var2 -> failwith $"invalid comparison, {other} vs int32 {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> NativeIntSource.isLess var1 var2
        | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
        | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
            failwith "TODO: Clt ManagedPointer vs NativeInt comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
            failwith "TODO: Clt ManagedPointer vs ManagedPointer comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType _ ->
            failwith "TODO: Clt ManagedPointer vs UserDefinedValueType comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, NativeInt int64 ->
            failwith "TODO: Clt UserDefinedValueType vs NativeInt comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, ManagedPointer managedPointerSource ->
            failwith "TODO: Clt UserDefinedValueType vs ManagedPointer comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, UserDefinedValueType _ ->
            failwith "TODO: Clt UserDefinedValueType vs UserDefinedValueType comparison unimplemented"

    let cgt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> var1 > var2
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 > var2
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            failwith $"Cgt instruction invalid for comparing object refs, {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, other -> failwith $"invalid comparison, ref %O{var1} vs %O{other}"
        | other, EvalStackValue.ObjectRef var2 -> failwith $"invalid comparison, %O{other} vs ref %O{var2}"
        | EvalStackValue.Float i, other -> failwith $"invalid comparison, float %f{i} vs %O{other}"
        | other, EvalStackValue.Float i -> failwith $"invalid comparison, %O{other} vs float %f{i}"
        | EvalStackValue.Int64 i, other -> failwith $"invalid comparison, int64 %i{i} vs %O{other}"
        | other, EvalStackValue.Int64 i -> failwith $"invalid comparison, %O{other} vs int64 %i{i}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 > var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: Cgt Int32 vs NativeInt comparison unimplemented"
        | EvalStackValue.Int32 i, other -> failwith $"invalid comparison, int32 %i{i} vs %O{other}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: Cgt NativeInt vs Int32 comparison unimplemented"
        | other, EvalStackValue.Int32 var2 -> failwith $"invalid comparison, {other} vs int32 {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> NativeIntSource.isLess var1 var2
        | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
        | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
            failwith "TODO: Cgt ManagedPointer vs NativeInt comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
            failwith "TODO: Cgt ManagedPointer vs ManagedPointer comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType _ ->
            failwith "TODO: Cgt ManagedPointer vs UserDefinedValueType comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, NativeInt int64 ->
            failwith "TODO: Cgt UserDefinedValueType vs NativeInt comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, ManagedPointer managedPointerSource ->
            failwith "TODO: Cgt UserDefinedValueType vs ManagedPointer comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, UserDefinedValueType _ ->
            failwith "TODO: Cgt UserDefinedValueType vs UserDefinedValueType comparison unimplemented"

    let cgtUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> uint32 var1 > uint32 var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> uint64 var1 > uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned nativeints"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: comparison of unsigned nativeint with int32"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 <= var2)
        | EvalStackValue.Float _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 ->
            // I'm going to be stricter than the spec and simply ban every pointer comparison except those with null,
            // pending a strong argument to fully support this.
            match var1, var2 with
            | ManagedPointerSource.Null, ManagedPointerSource.Null -> false
            | ManagedPointerSource.Null, _ -> true
            | _, ManagedPointerSource.Null -> true
            | _, _ -> failwith $"I've banned this case: {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            // According to the spec, cgt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct comparison between two object refs is not specified, so we treat it as a pointer comparison.
            failwith "TODO"
        | other1, other2 -> failwith $"Cgt.un instruction invalid for comparing {other1} vs {other2}"

    let cltUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> uint32 var1 < uint32 var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> uint64 var1 < uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned nativeints"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: comparison of unsigned nativeint with int32"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 >= var2)
        | EvalStackValue.Float _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 -> failwith "TODO"
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            // According to the spec, cgt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct comparison between two object refs is not specified, so we treat it as a pointer comparison.
            failwith "TODO"
        | other1, other2 -> failwith $"Cgt.un instruction invalid for comparing {other1} vs {other2}"

    let rec ceq (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        // Table III.4
        match var1, var2 with
        | EvalStackValue.UserDefinedValueType {
                                                  Fields = [ f ]
                                              },
          v -> ceq f.ContentsEval v
        | u,
          EvalStackValue.UserDefinedValueType {
                                                  Fields = [ f ]
                                              } -> ceq u f.ContentsEval
        | EvalStackValue.UserDefinedValueType {
                                                  Fields = []
                                              },
          EvalStackValue.UserDefinedValueType {
                                                  Fields = []
                                              } ->
            // hmm, surely this can't happen, but :shrug:
            true
        | EvalStackValue.UserDefinedValueType _, _
        | _, EvalStackValue.UserDefinedValueType _ -> failwith $"TODO: ceq {var1} vs {var2}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 = var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 -> failwith "TODO: int32 CEQ nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"bad ceq: Int32 vs {var2}"
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> var1 = var2
        | EvalStackValue.Int64 _, _ -> failwith $"bad ceq: Int64 vs {var2}"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 = var2
        | EvalStackValue.Float _, _ -> failwith $"bad ceq: Float vs {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.FunctionPointer f1, NativeIntSource.FunctionPointer f2 ->
                if f1 = f2 then
                    true
                else
                    failwith $"TODO(CEQ): nativeint vs nativeint, {f1} vs {f2}"
            | NativeIntSource.TypeHandlePtr f1, NativeIntSource.TypeHandlePtr f2 -> f1 = f2
            | NativeIntSource.Verbatim f1, NativeIntSource.Verbatim f2 -> f1 = f2
            | NativeIntSource.ManagedPointer f1, NativeIntSource.ManagedPointer f2 -> f1 = f2
            | _, _ -> failwith $"TODO (CEQ): nativeint vs nativeint, {var1} vs {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 -> failwith $"TODO (CEQ): nativeint vs int32"
        | EvalStackValue.NativeInt var1, EvalStackValue.ManagedPointer var2 ->
            failwith $"TODO (CEQ): nativeint vs managed pointer"
        | EvalStackValue.NativeInt _, _ -> failwith $"bad ceq: NativeInt vs {var2}"
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 -> var1 = var2
        | EvalStackValue.ManagedPointer src, EvalStackValue.ObjectRef var1
        | EvalStackValue.ObjectRef var1, EvalStackValue.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Heap src -> src = var1
            | ManagedPointerSource.Null -> false
            | ManagedPointerSource.LocalVariable _
            | ManagedPointerSource.Argument _ -> false
            | ManagedPointerSource.ArrayIndex (arr, index) -> failwith "todo"
        | EvalStackValue.ObjectRef _, _ -> failwith $"bad ceq: ObjectRef vs {var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 -> var1 = var2
        | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt var2 ->
            failwith $"TODO (CEQ): managed pointer vs nativeint"
        | EvalStackValue.ManagedPointer _, _ -> failwith $"bad ceq: ManagedPointer vs {var2}"
