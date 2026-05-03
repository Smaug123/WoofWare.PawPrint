namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module EvalStackValueComparisons =
    let private compareVerbatimUInt64Bits (left : int64) (right : int64) : int =
        // Compare raw UInt64 bits. Within one storage identity, a negative
        // byte offset has wrapped bits and therefore sorts above positive offsets.
        compare (uint64 left) (uint64 right)

    let private compareManagedAddresses (left : ManagedAddress) (right : ManagedAddress) : int =
        match left.Storage, right.Storage with
        | None, None -> compareVerbatimUInt64Bits left.Offset right.Offset
        // Public UInt64 comparison normalises storage-free addresses first; keep
        // these null-vs-storage arms for any future caller that compares
        // ManagedAddress values directly.
        | None, Some _ when left.Offset = 0L -> -1
        | Some _, None when right.Offset = 0L -> 1
        | None, Some _
        | Some _, None ->
            failwith
                $"unsigned managed address comparison between storage-free non-null address and storage-backed address: %O{left} vs %O{right}"
        | Some leftStorage, Some rightStorage when leftStorage = rightStorage ->
            compareVerbatimUInt64Bits left.Offset right.Offset
        | Some _, Some _ ->
            failwith $"unsigned managed address comparison between different storage: %O{left} vs %O{right}"

    let private compareUInt64Sources (left : UInt64Source) (right : UInt64Source) : int =
        let left = TaggedUInt64.normaliseStorageFreeAddress left
        let right = TaggedUInt64.normaliseStorageFreeAddress right

        match left, right with
        | UInt64Source.Verbatim left, UInt64Source.Verbatim right -> compareVerbatimUInt64Bits left right
        | UInt64Source.ManagedAddress left, UInt64Source.ManagedAddress right -> compareManagedAddresses left right
        | UInt64Source.ManagedAddress _, UInt64Source.Verbatim 0L -> 1
        | UInt64Source.Verbatim 0L, UInt64Source.ManagedAddress _ -> -1
        | UInt64Source.ManagedAddress left, UInt64Source.Verbatim right ->
            let right = sprintf "0x%016X" (uint64 right)
            failwith $"unsigned comparison between managed address %O{left} and verbatim %s{right}"
        | UInt64Source.Verbatim left, UInt64Source.ManagedAddress right ->
            let left = sprintf "0x%016X" (uint64 left)
            failwith $"unsigned comparison between verbatim %s{left} and managed address %O{right}"

    let private ceqManagedAddresses (left : ManagedAddress) (right : ManagedAddress) : bool =
        left.Storage = right.Storage && left.Offset = right.Offset

    let private ceqUInt64Sources (left : UInt64Source) (right : UInt64Source) : bool =
        let left = TaggedUInt64.normaliseStorageFreeAddress left
        let right = TaggedUInt64.normaliseStorageFreeAddress right

        match left, right with
        | UInt64Source.Verbatim left, UInt64Source.Verbatim right -> left = right
        | UInt64Source.ManagedAddress left, UInt64Source.ManagedAddress right -> ceqManagedAddresses left right
        // Storage-free addresses were normalised above, so any ManagedAddress
        // remaining here carries storage identity and is never literal zero.
        | UInt64Source.ManagedAddress left, UInt64Source.Verbatim right
        | UInt64Source.Verbatim right, UInt64Source.ManagedAddress left when right = 0L -> false
        | UInt64Source.ManagedAddress left, UInt64Source.Verbatim right ->
            failwith $"ceq between managed address %O{left} and non-zero verbatim UInt64 0x%016X{uint64 right}"
        | UInt64Source.Verbatim left, UInt64Source.ManagedAddress right ->
            failwith $"ceq between non-zero verbatim UInt64 0x%016X{uint64 left} and managed address %O{right}"

    let private nullOnlyManagedPointerProjection (ptr : ManagedPointerSource) : ManagedAddress option =
        match ptr with
        | ManagedPointerSource.Null ->
            Some
                {
                    Storage = None
                    Offset = 0L
                }
        | ManagedPointerSource.Byref _ -> None

    let private nativeIntToUInt64Source
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (source : NativeIntSource)
        : Result<UInt64Source, string>
        =
        match source with
        | NativeIntSource.Verbatim bits -> UInt64Source.Verbatim bits |> Ok
        | NativeIntSource.SyntheticCrossArrayOffset _ ->
            Error $"refusing to flatten synthetic cross-storage offset %O{source} into tagged UInt64 comparison"
        | NativeIntSource.ManagedPointer ptr ->
            match tryProjectManagedPointer ptr with
            | Some address ->
                address
                |> UInt64Source.ManagedAddress
                |> TaggedUInt64.normaliseStorageFreeAddress
                |> Ok
            | None -> Error $"unprojectable managed pointer %O{ptr}"
        | _ -> Error $"opaque nativeint %O{source}"

    let private nativeIntToUInt64SourceOrFail
        (operation : string)
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (source : NativeIntSource)
        : UInt64Source
        =
        match nativeIntToUInt64Source tryProjectManagedPointer source with
        | Ok source -> source
        | Error reason -> failwith $"%s{operation}: %s{reason}"

    let clt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> var1 < var2
        | EvalStackValue.UInt64 _, _
        | _, EvalStackValue.UInt64 _ ->
            failwith $"Clt instruction invalid for tagged UInt64 comparison, {var1} vs {var2}"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 < var2
        | EvalStackValue.NullObjectRef, _
        | _, EvalStackValue.NullObjectRef ->
            failwith $"Clt instruction invalid for comparing object refs, {var1} vs {var2}"
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
        | EvalStackValue.UInt64 _, _
        | _, EvalStackValue.UInt64 _ ->
            failwith $"Cgt instruction invalid for tagged UInt64 comparison, {var1} vs {var2}"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 > var2
        | EvalStackValue.NullObjectRef, _
        | _, EvalStackValue.NullObjectRef ->
            failwith $"Cgt instruction invalid for comparing object refs, {var1} vs {var2}"
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
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> NativeIntSource.isLess var2 var1
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

    let private cgtUnCore
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        match var1, var2 with
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> uint32 var1 > uint32 var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 var1, EvalStackValue.UInt64 var2 ->
            compareUInt64Sources (UInt64Source.Verbatim (int64 (uint32 var1))) var2 > 0
        | EvalStackValue.Int32 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.UInt64 var1, EvalStackValue.Int32 var2 ->
            compareUInt64Sources var1 (UInt64Source.Verbatim (int64 (uint32 var2))) > 0
        | EvalStackValue.UInt64 var1, EvalStackValue.Int64 var2 ->
            compareUInt64Sources var1 (UInt64Source.Verbatim var2) > 0
        | EvalStackValue.Int64 var1, EvalStackValue.UInt64 var2 ->
            compareUInt64Sources (UInt64Source.Verbatim var1) var2 > 0
        | EvalStackValue.UInt64 var1, EvalStackValue.UInt64 var2 -> compareUInt64Sources var1 var2 > 0
        | EvalStackValue.UInt64 var1, EvalStackValue.NativeInt var2 ->
            let var2 =
                nativeIntToUInt64SourceOrFail "cgt.un tagged UInt64/nativeint" tryProjectManagedPointer var2

            compareUInt64Sources var1 var2 > 0
        | EvalStackValue.UInt64 var1, EvalStackValue.ManagedPointer var2 ->
            match tryProjectManagedPointer var2 with
            | Some var2 -> compareUInt64Sources var1 (UInt64Source.ManagedAddress var2) > 0
            | None -> failwith $"TODO: cgt.un on tagged UInt64 and unprojectable managed pointer: %O{var1} vs %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.UInt64 var2 ->
            match tryProjectManagedPointer var1 with
            | Some var1 -> compareUInt64Sources (UInt64Source.ManagedAddress var1) var2 > 0
            | None -> failwith $"TODO: cgt.un on unprojectable managed pointer and tagged UInt64: %O{var1} vs %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.UInt64 var2 ->
            let var1 =
                nativeIntToUInt64SourceOrFail "cgt.un nativeint/tagged UInt64" tryProjectManagedPointer var1

            compareUInt64Sources var1 var2 > 0
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> uint64 var1 > uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            let asInt64 (src : NativeIntSource) : int64 option =
                match src with
                | NativeIntSource.Verbatim v
                | NativeIntSource.SyntheticCrossArrayOffset v -> Some v
                | _ -> None

            match asInt64 var1, asInt64 var2 with
            | Some v1, Some v2 -> uint64 v1 > uint64 v2
            | _ -> failwith $"TODO: cgt.un on non-Verbatim nativeints: %O{var1} vs %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: comparison of unsigned nativeint with int32"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 <= var2)
        | EvalStackValue.Float _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 ->
            // I'm going to be stricter than the spec and simply ban every pointer comparison except those with null,
            // pending a strong argument to fully support this.
            match var1, var2 with
            | ManagedPointerSource.Null, ManagedPointerSource.Null -> false
            | ManagedPointerSource.Null, _ -> false
            | _, ManagedPointerSource.Null -> true
            | _, _ -> failwith $"I've banned this case: {var1} vs {var2}"
        | EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _ -> false
        | EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> true
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            // According to the spec, cgt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct comparison between two object refs is not specified, so we treat it as a pointer comparison.
            failwith "TODO"
        | EvalStackValue.NullObjectRef, other -> failwith $"Cgt.un invalid for comparing NullObjectRef with {other}"
        | EvalStackValue.ObjectRef _, other -> failwith $"Cgt.un invalid for comparing ObjectRef with {other}"
        | other1, other2 -> failwith $"Cgt.un instruction invalid for comparing {other1} vs {other2}"

    let cgtUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        cgtUnCore nullOnlyManagedPointerProjection var1 var2

    let cgtUnWithManagedPointerProjection
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        cgtUnCore tryProjectManagedPointer var1 var2

    let private cltUnCore
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        match var1, var2 with
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> uint32 var1 < uint32 var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 var1, EvalStackValue.UInt64 var2 ->
            compareUInt64Sources (UInt64Source.Verbatim (int64 (uint32 var1))) var2 < 0
        | EvalStackValue.Int32 _, _ -> failwith $"Clt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.UInt64 var1, EvalStackValue.Int32 var2 ->
            compareUInt64Sources var1 (UInt64Source.Verbatim (int64 (uint32 var2))) < 0
        | EvalStackValue.UInt64 var1, EvalStackValue.Int64 var2 ->
            compareUInt64Sources var1 (UInt64Source.Verbatim var2) < 0
        | EvalStackValue.Int64 var1, EvalStackValue.UInt64 var2 ->
            compareUInt64Sources (UInt64Source.Verbatim var1) var2 < 0
        | EvalStackValue.UInt64 var1, EvalStackValue.UInt64 var2 -> compareUInt64Sources var1 var2 < 0
        | EvalStackValue.UInt64 var1, EvalStackValue.NativeInt var2 ->
            let var2 =
                nativeIntToUInt64SourceOrFail "clt.un tagged UInt64/nativeint" tryProjectManagedPointer var2

            compareUInt64Sources var1 var2 < 0
        | EvalStackValue.UInt64 var1, EvalStackValue.ManagedPointer var2 ->
            match tryProjectManagedPointer var2 with
            | Some var2 -> compareUInt64Sources var1 (UInt64Source.ManagedAddress var2) < 0
            | None -> failwith $"TODO: clt.un on tagged UInt64 and unprojectable managed pointer: %O{var1} vs %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.UInt64 var2 ->
            match tryProjectManagedPointer var1 with
            | Some var1 -> compareUInt64Sources (UInt64Source.ManagedAddress var1) var2 < 0
            | None -> failwith $"TODO: clt.un on unprojectable managed pointer and tagged UInt64: %O{var1} vs %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.UInt64 var2 ->
            let var1 =
                nativeIntToUInt64SourceOrFail "clt.un nativeint/tagged UInt64" tryProjectManagedPointer var1

            compareUInt64Sources var1 var2 < 0
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> uint64 var1 < uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Clt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            let asInt64 (src : NativeIntSource) : int64 option =
                match src with
                | NativeIntSource.Verbatim v
                | NativeIntSource.SyntheticCrossArrayOffset v -> Some v
                | _ -> None

            match asInt64 var1, asInt64 var2 with
            | Some v1, Some v2 -> uint64 v1 < uint64 v2
            | _ -> failwith $"TODO: clt.un on non-Verbatim nativeints: %O{var1} vs %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: comparison of unsigned nativeint with int32"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 >= var2)
        | EvalStackValue.Float _, _ -> failwith $"Clt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 -> failwith "TODO"
        | EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _ -> true
        | EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            // According to the spec, cgt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct comparison between two object refs is not specified, so we treat it as a pointer comparison.
            failwith "TODO"
        | EvalStackValue.NullObjectRef, other -> failwith $"Clt.un invalid for comparing NullObjectRef with {other}"
        | EvalStackValue.ObjectRef _, other -> failwith $"Clt.un invalid for comparing ObjectRef with {other}"
        | other1, other2 -> failwith $"Clt.un instruction invalid for comparing {other1} vs {other2}"

    let cltUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        cltUnCore nullOnlyManagedPointerProjection var1 var2

    let cltUnWithManagedPointerProjection
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        cltUnCore tryProjectManagedPointer var1 var2

    let cgeUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 < var2)
        | EvalStackValue.Float _, _ -> failwith $"Bge.un invalid for comparing %O{var1} with %O{var2}"
        | _, EvalStackValue.Float _ -> failwith $"Bge.un invalid for comparing %O{var1} with %O{var2}"
        | _ -> not (cltUn var1 var2)

    let cgeUnWithManagedPointerProjection
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        match var1, var2 with
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 < var2)
        | EvalStackValue.Float _, _ -> failwith $"Bge.un invalid for comparing %O{var1} with %O{var2}"
        | _, EvalStackValue.Float _ -> failwith $"Bge.un invalid for comparing %O{var1} with %O{var2}"
        | _ -> not (cltUnWithManagedPointerProjection tryProjectManagedPointer var1 var2)

    let cleUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 > var2)
        | EvalStackValue.Float _, _ -> failwith $"Ble.un invalid for comparing %O{var1} with %O{var2}"
        | _, EvalStackValue.Float _ -> failwith $"Ble.un invalid for comparing %O{var1} with %O{var2}"
        | _ -> not (cgtUn var1 var2)

    let cleUnWithManagedPointerProjection
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        match var1, var2 with
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 > var2)
        | EvalStackValue.Float _, _ -> failwith $"Ble.un invalid for comparing %O{var1} with %O{var2}"
        | _, EvalStackValue.Float _ -> failwith $"Ble.un invalid for comparing %O{var1} with %O{var2}"
        | _ -> not (cgtUnWithManagedPointerProjection tryProjectManagedPointer var1 var2)

    let private ceqNormalisedManagedPointers
        (context : string)
        (p1 : NormalisedManagedPointerSource)
        (p2 : NormalisedManagedPointerSource)
        : bool
        =
        if
            ManagedPointerSource.hasNonTrailingReinterpret p1
            || ManagedPointerSource.hasNonTrailingReinterpret p2
        then
            failwith
                $"TODO (CEQ): %s{context} with `ReinterpretAs` followed by `Field` needs a bytewise layout comparison; got %O{NormalisedManagedPointerSource.value p1} vs %O{NormalisedManagedPointerSource.value p2}"

        ManagedPointerSource.stripTrailingReinterprets p1 = ManagedPointerSource.stripTrailingReinterprets p2

    let rec private ceqCore
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        // Table III.4
        // Primitive-like wrappers AND enums are flattened on push (see EvalStackValue.ofCliType),
        // so UserDefinedValueType here is always a genuine user struct. ECMA leaves ceq between
        // user-defined value types unspecified, so we fail loud.
        match var1, var2 with
        | EvalStackValue.UserDefinedValueType var1, v ->
            failwith $"ceq is not specified for UserDefinedValueType: %O{var1} vs %O{v}"
        | u, EvalStackValue.UserDefinedValueType var2 ->
            failwith $"ceq is not specified for UserDefinedValueType: %O{u} vs %O{var2}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 = var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 -> failwith "TODO: int32 CEQ nativeint"
        | EvalStackValue.Int32 var1, EvalStackValue.UInt64 var2 ->
            ceqUInt64Sources (UInt64Source.Verbatim (int64 (uint32 var1))) var2
        | EvalStackValue.Int32 _, _ -> failwith $"bad ceq: Int32 vs {var2}"
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> var1 = var2
        | EvalStackValue.Int64 var1, EvalStackValue.UInt64 var2 -> ceqUInt64Sources (UInt64Source.Verbatim var1) var2
        | EvalStackValue.Int64 _, _ -> failwith $"bad ceq: Int64 vs {var2}"
        | EvalStackValue.UInt64 var1, EvalStackValue.UInt64 var2 -> ceqUInt64Sources var1 var2
        | EvalStackValue.UInt64 var1, EvalStackValue.Int32 var2 ->
            ceqUInt64Sources var1 (UInt64Source.Verbatim (int64 (uint32 var2)))
        | EvalStackValue.UInt64 var1, EvalStackValue.Int64 var2 -> ceqUInt64Sources var1 (UInt64Source.Verbatim var2)
        | EvalStackValue.UInt64 var1, EvalStackValue.NativeInt var2 ->
            let var2 =
                nativeIntToUInt64SourceOrFail "ceq tagged UInt64/nativeint" tryProjectManagedPointer var2

            ceqUInt64Sources var1 var2
        | EvalStackValue.UInt64 var1, EvalStackValue.ManagedPointer var2 ->
            match tryProjectManagedPointer var2 with
            | Some var2 -> ceqUInt64Sources var1 (UInt64Source.ManagedAddress var2)
            | None -> failwith $"bad ceq: UInt64 vs unprojectable ManagedPointer {var2}"
        | EvalStackValue.UInt64 _, _ -> failwith $"bad ceq: UInt64 vs {var2}"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 = var2
        | EvalStackValue.Float _, _ -> failwith $"bad ceq: Float vs {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.FunctionPointer f1, NativeIntSource.FunctionPointer f2 -> MethodInfo.NominallyEqual f1 f2
            | NativeIntSource.TypeHandlePtr f1, NativeIntSource.TypeHandlePtr f2 -> f1 = f2
            | NativeIntSource.MethodTablePtr f1, NativeIntSource.MethodTablePtr f2 -> f1 = f2
            | NativeIntSource.MethodTableAuxiliaryDataPtr f1, NativeIntSource.MethodTableAuxiliaryDataPtr f2 -> f1 = f2
            | NativeIntSource.MethodHandlePtr f1, NativeIntSource.MethodHandlePtr f2 -> f1 = f2
            | NativeIntSource.FieldHandlePtr f1, NativeIntSource.FieldHandlePtr f2 -> f1 = f2
            | NativeIntSource.AssemblyHandle f1, NativeIntSource.AssemblyHandle f2 -> f1 = f2
            | NativeIntSource.ModuleHandle f1, NativeIntSource.ModuleHandle f2 -> f1 = f2
            | NativeIntSource.MetadataImportHandle f1, NativeIntSource.MetadataImportHandle f2 -> f1 = f2
            | NativeIntSource.GcHandlePtr f1, NativeIntSource.GcHandlePtr f2 -> f1 = f2
            | NativeIntSource.Verbatim f1, NativeIntSource.Verbatim f2 -> f1 = f2
            // `SyntheticCrossArrayOffset` and `Verbatim` share an int64
            // payload and the same bit-level ceq semantics.
            | NativeIntSource.SyntheticCrossArrayOffset f1, NativeIntSource.SyntheticCrossArrayOffset f2
            | NativeIntSource.Verbatim f1, NativeIntSource.SyntheticCrossArrayOffset f2
            | NativeIntSource.SyntheticCrossArrayOffset f1, NativeIntSource.Verbatim f2 -> f1 = f2
            | NativeIntSource.ManagedPointer f1, NativeIntSource.ManagedPointer f2 ->
                // Match the `EvalStackValue.ManagedPointer` vs `ManagedPointer`
                // arm below: trailing `ReinterpretAs` projections are address-
                // preserving, so a byref converted to a native int via
                // `conv.u` / `Unsafe.AsPointer` must compare equal to the same
                // byref whose type view was changed by an `Unsafe.As`. Refuse
                // the comparison on non-trailing `ReinterpretAs` for the same
                // reason as the direct byref-ceq arm.
                ceqNormalisedManagedPointers
                    "native-int-wrapped byref"
                    (ManagedPointerSource.unsafeAssumeNormalisedForComparison f1)
                    (ManagedPointerSource.unsafeAssumeNormalisedForComparison f2)
            | NativeIntSource.Verbatim _, NativeIntSource.ManagedPointer _
            | NativeIntSource.ManagedPointer _, NativeIntSource.Verbatim _
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.ManagedPointer _
            | NativeIntSource.ManagedPointer _, NativeIntSource.SyntheticCrossArrayOffset _ ->
                let z1 = NativeIntSource.isZero var1
                let z2 = NativeIntSource.isZero var2

                if z1 && z2 then
                    true
                elif z1 <> z2 then
                    false
                else
                    failwith $"TODO (CEQ): mixed nativeint representations, {var1} vs {var2}"
            // Distinct opaque handle kinds have distinct non-null bit patterns, so never alias.
            | NativeIntSource.FunctionPointer _, _
            | _, NativeIntSource.FunctionPointer _
            | NativeIntSource.TypeHandlePtr _, _
            | _, NativeIntSource.TypeHandlePtr _
            | NativeIntSource.MethodTablePtr _, _
            | _, NativeIntSource.MethodTablePtr _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _, _
            | _, NativeIntSource.MethodTableAuxiliaryDataPtr _
            | NativeIntSource.MethodHandlePtr _, _
            | _, NativeIntSource.MethodHandlePtr _
            | NativeIntSource.FieldHandlePtr _, _
            | _, NativeIntSource.FieldHandlePtr _
            | NativeIntSource.AssemblyHandle _, _
            | _, NativeIntSource.AssemblyHandle _
            | NativeIntSource.ModuleHandle _, _
            | _, NativeIntSource.ModuleHandle _
            | NativeIntSource.MetadataImportHandle _, _
            | _, NativeIntSource.MetadataImportHandle _
            | NativeIntSource.GcHandlePtr _, _
            | _, NativeIntSource.GcHandlePtr _ -> false
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 -> failwith $"TODO (CEQ): nativeint vs int32"
        | EvalStackValue.NativeInt var1, EvalStackValue.UInt64 var2 ->
            let var1 =
                nativeIntToUInt64SourceOrFail "ceq nativeint/tagged UInt64" tryProjectManagedPointer var1

            ceqUInt64Sources var1 var2
        | EvalStackValue.NativeInt var1, EvalStackValue.ManagedPointer var2 ->
            ceqCore
                tryProjectManagedPointer
                (EvalStackValue.NativeInt var1)
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
        | EvalStackValue.NativeInt _, _ -> failwith $"bad ceq: NativeInt vs {var2}"
        | EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> true
        | EvalStackValue.ObjectRef addr1, EvalStackValue.ObjectRef addr2 -> addr1 = addr2
        | EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _
        | EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.ManagedPointer p1, EvalStackValue.ManagedPointer p2 ->
            // `ceq` on byrefs is address equality; trailing `ReinterpretAs`
            // projections are address-preserving type-view changes, so strip
            // them from both sides before comparison. A `ReinterpretAs`
            // followed by a `Field` would need a bytewise layout comparison
            // (fields at the same offset under different type views still
            // alias); we don't model that yet, so refuse rather than risk a
            // silent false negative.
            ceqNormalisedManagedPointers
                "byref"
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison p1)
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison p2)
        | EvalStackValue.ManagedPointer _, EvalStackValue.NullObjectRef
        | EvalStackValue.NullObjectRef, EvalStackValue.ManagedPointer _
        | EvalStackValue.ManagedPointer _, EvalStackValue.ObjectRef _
        | EvalStackValue.ObjectRef _, EvalStackValue.ManagedPointer _ ->
            // In CLI, ceq between O and & types is unspecified.
            // If this fires, investigate the upstream IL.
            failwith "ceq between managed pointer and object reference"
        | EvalStackValue.NullObjectRef, _ -> failwith $"bad ceq: NullObjectRef vs {var2}"
        | EvalStackValue.ObjectRef _, _ -> failwith $"bad ceq: ObjectRef vs {var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt var2 ->
            ceqCore
                tryProjectManagedPointer
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1))
                (EvalStackValue.NativeInt var2)
        | EvalStackValue.ManagedPointer var1, EvalStackValue.UInt64 var2 ->
            match tryProjectManagedPointer var1 with
            | Some var1 -> ceqUInt64Sources (UInt64Source.ManagedAddress var1) var2
            | None -> failwith $"bad ceq: unprojectable ManagedPointer {var1} vs UInt64"
        | EvalStackValue.ManagedPointer _, _ -> failwith $"bad ceq: ManagedPointer vs {var2}"

    let ceq (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        ceqCore nullOnlyManagedPointerProjection var1 var2

    let ceqWithManagedPointerProjection
        (tryProjectManagedPointer : ManagedPointerSource -> ManagedAddress option)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool
        =
        ceqCore tryProjectManagedPointer var1 var2
