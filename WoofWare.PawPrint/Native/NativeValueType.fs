namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

open NativeRuntimeTypeHelpers

/// QCalls declared on `System.ValueType`: the runtime half of the default structural
/// `GetHashCode` for value types.
[<RequireQualifiedAccess>]
module NativeValueType =
    /// Which single field of a value type `ValueType.GetHashCode` should hash, and how. The
    /// numbering of CoreLib's `ValueType.ValueTypeHashCodeStrategy` (`System/ValueType.cs:153`,
    /// mirrored by `vm/comutilnative.cpp:1687`) is the QCall's return value.
    type private HashCodeStrategy =
        | NoField
        | ReferenceField of offset : int
        | DoubleField of offset : int
        | SingleField of offset : int
        | FastGetHashCode of offset : int * size : int
        | ValueTypeOverride of offset : int * fieldMethodTable : ConcreteTypeHandle

    let private strategyCode (strategy : HashCodeStrategy) : int32 =
        match strategy with
        | HashCodeStrategy.NoField -> 0
        | HashCodeStrategy.ReferenceField _ -> 1
        | HashCodeStrategy.DoubleField _ -> 2
        | HashCodeStrategy.SingleField _ -> 3
        | HashCodeStrategy.FastGetHashCode _ -> 4
        | HashCodeStrategy.ValueTypeOverride _ -> 5

    /// The distinctions CoreCLR's walk draws between fields. It reads `FieldDesc::GetFieldType()`,
    /// the field's *normalised* CorElementType, so this classifies the field's declared type and
    /// not the shape PawPrint happens to store it in: `CliType.unwrapPrimitiveLikeDeep` would
    /// collapse `RuntimeTypeHandle` to the object reference inside it, where CoreCLR keeps seeing
    /// a value type that overrides `GetHashCode`.
    type private FieldClass =
        /// `FieldDesc::IsObjRef()`.
        | ObjectReference
        | R8
        | R4
        /// Anything that is neither an object reference, a float, nor `ELEMENT_TYPE_VALUETYPE`:
        /// the integers, `bool`, `char`, pointers and function pointers. Hashed as raw bytes.
        | Bits
        | NestedValueType

    /// `[InlineArray(N)]` expands to N storage slots in PawPrint (`InlineArrayStorage.expand`)
    /// where CoreCLR's `ApproxFieldDescIterator` still sees the one declared field, so the walk
    /// below would examine slot 1 after skipping a null slot 0 where CoreCLR would stop. The
    /// deeper problem is that the predicate the walk consults for a nested value type,
    /// `canCompareBitsOrUseFastGetHashCode`, models neither of CoreCLR's inline-array answers —
    /// `FALSE` from the internal recursion (`vm/comutilnative.cpp:1614`) nor the
    /// `NotSupportedException` the sibling `MethodTable_CanCompareBitsOrUseFastGetHashCode` QCall
    /// throws (`:1678`) — so an answer derived from it would be silently wrong rather than
    /// merely absent. Refuse instead.
    let private refuseInlineArray
        (operation : string)
        (state : IlMachineState)
        (methodTable : ConcreteTypeHandle)
        : unit
        =
        match IlMachineState.tryGetConcreteTypeInfo state methodTable with
        | None -> ()
        | Some (_, typeInfo) ->
            // `isValueType = true`: both callers hand over a value type — the receiver's own
            // MethodTable, or a field the classifier has already put in `NestedValueType`.
            match InlineArrayStorage.effectiveLength true typeInfo.InlineArrayLength with
            | None -> ()
            | Some length ->
                failwith
                    $"TODO: %s{operation} reached the `[InlineArray(%d{length})]` type %s{typeInfo.Namespace}.%s{typeInfo.Name}. CoreCLR reports such a type as never bit-comparable and throws NotSupportedException (NotSupported_InlineArrayEqualsGetHashCode) from MethodTable_CanCompareBitsOrUseFastGetHashCode; PawPrint's canCompareBitsOrUseFastGetHashCode models neither, so there is no trustworthy answer to derive here"

    let private classifyField
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (fieldType : ConcreteTypeHandle)
        : FieldClass
        =
        let ofPrimitive (primitive : PrimitiveType) : FieldClass =
            match primitive with
            | PrimitiveType.Double -> FieldClass.R8
            | PrimitiveType.Single -> FieldClass.R4
            | PrimitiveType.String
            | PrimitiveType.Object -> FieldClass.ObjectReference
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> FieldClass.Bits
            | PrimitiveType.TypedReference ->
                // CoreCLR reports `TYPEDBYREF` as `ELEMENT_TYPE_VALUETYPE`
                // (`vm/methodtablebuilder.cpp:4215`), but a `TypedReference` field makes its
                // container ByRefLike, and a ByRefLike type cannot be boxed — so
                // `ValueType.GetHashCode`, which reaches this walk only through a boxed receiver,
                // cannot be reached for one.
                failwith
                    $"%s{operation}: field of type System.TypedReference; only a ByRefLike type may declare one, and such a type cannot be boxed, so ValueType.GetHashCode is unreachable for it"

        match fieldType with
        | ConcretePrimitive state.ConcreteTypes primitive -> ofPrimitive primitive
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> FieldClass.ObjectReference
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> FieldClass.Bits
        | ConcreteTypeHandle.Byref _ ->
            // A byref field exists only in a `ByRefLike` type, which cannot be boxed, so
            // `ValueType.GetHashCode` — which boxes its receiver on the way in — cannot be
            // reached for one.
            failwith
                $"%s{operation}: field of byref type %O{fieldType}; only a ByRefLike type may declare one, and such a type cannot be boxed, so ValueType.GetHashCode is unreachable for it"
        | ConcreteTypeHandle.Concrete _ ->
            if IlMachineState.isReferenceTypeHandle baseClassTypes operation state fieldType then
                FieldClass.ObjectReference
            else

            let _, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state fieldType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: field type handle was not registered: %O{fieldType}"
                )

            // `MethodTableBuilder` strips an enum-typed field down to its underlying primitive
            // (#FieldDescTypeMorph, `vm/methodtablebuilder.cpp:4318`), so an enum field reaches
            // the walk as that primitive and never as a value type. Nothing else is morphed.
            //
            // No guest can currently tell this arm from its absence. Routed as a value type
            // instead, an enum field would ask `canCompareBitsOrUseFastGetHashCode` about the enum
            // type, and PawPrint answers `true` — CoreCLR answers `false`, because
            // `HasOverriddenMethod` sees `System.Enum`'s own `Equals`/`GetHashCode` in the enum's
            // vtable slots and `overridesValueTypeMethod` does not — which yields
            // `FastGetHashCode` at the same offset and size this arm produces. Measured: mutating
            // this arm away, with and without swapping the two checks in the value-type arm below,
            // leaves every test green. Kept because it is what CoreCLR does, and because the
            // coincidence rests on that second divergence rather than on anything structural.
            match enumUnderlyingPrimitive operation typeInfo with
            | Some primitive -> ofPrimitive primitive
            | None -> FieldClass.NestedValueType

    /// CoreCLR's `GetHashCodeStrategy` (`vm/comutilnative.cpp:1696`). Walks the instance fields of
    /// `methodTable` in *metadata declaration* order — which auto layout reorders, so it is not
    /// the order `CliValueType` stores them in — and commits to the first field it sees, except
    /// that a null object reference is skipped in favour of the next declared field.
    ///
    /// `contents` is the receiver's storage as reached so far, and `accumulatedOffset` where that
    /// storage begins inside the boxed receiver; the two advance together as the walk descends
    /// into a nested value type.
    let rec private walk
        (operation : string)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (valueTypeGetHashCode :
            WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (methodTable : ConcreteTypeHandle)
        (contents : CliValueType)
        (accumulatedOffset : int)
        (state : IlMachineState)
        : IlMachineState * HashCodeStrategy
        =
        refuseInlineArray operation state methodTable

        let state, fields =
            IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state methodTable

        let rec step (fields : CliField list) (state : IlMachineState) : IlMachineState * HashCodeStrategy =
            match fields with
            | [] ->
                // Every field was a null object reference, or the type declares none at all.
                state, HashCodeStrategy.NoField
            | field :: rest ->
                let fieldOffset, fieldSize = CliValueType.GetFieldLayoutById field.Id contents
                let offset = accumulatedOffset + fieldOffset

                match classifyField operation baseClassTypes state field.Type with
                | FieldClass.ObjectReference ->
                    match CliValueType.DereferenceFieldById field.Id contents with
                    | CliType.ObjectRef None -> step rest state
                    | CliType.ObjectRef (Some _) -> state, HashCodeStrategy.ReferenceField offset
                    | other ->
                        failwith
                            $"%s{operation}: field '%s{field.Name}' of type %O{field.Type} classified as an object reference, but its storage holds %O{other}"
                | FieldClass.R8 -> state, HashCodeStrategy.DoubleField offset
                | FieldClass.R4 -> state, HashCodeStrategy.SingleField offset
                | FieldClass.Bits -> state, HashCodeStrategy.FastGetHashCode (offset, fieldSize)
                | FieldClass.NestedValueType ->
                    refuseInlineArray operation state field.Type

                    let state, canCompare =
                        canCompareBitsOrUseFastGetHashCode loggerFactory baseClassTypes thread field.Type state

                    if canCompare then
                        state, HashCodeStrategy.FastGetHashCode (offset, fieldSize)
                    else

                    let state, overridesGetHashCode =
                        overridesValueTypeMethod
                            loggerFactory
                            baseClassTypes
                            thread
                            field.Type
                            valueTypeGetHashCode
                            state

                    if overridesGetHashCode then
                        state, HashCodeStrategy.ValueTypeOverride (offset, field.Type)
                    else

                    match CliValueType.DereferenceFieldById field.Id contents with
                    | CliType.ValueType nested ->
                        walk
                            operation
                            loggerFactory
                            baseClassTypes
                            thread
                            valueTypeGetHashCode
                            field.Type
                            nested
                            offset
                            state
                    | other ->
                        failwith
                            $"%s{operation}: field '%s{field.Name}' of type %O{field.Type} classified as a nested value type, but its storage holds %O{other}"

        step fields state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        // The method name is deliberately not matched: the `[LibraryImport]` stub Roslyn generates
        // carries a mangled local-function name (`<GetHashCodeStrategy>g____PInvoke|5_0`). Entry
        // point, declaring type and signature already disambiguate it.
        | "ValueType_GetHashCodeStrategy",
          "System.Private.CoreLib",
          "System",
          "ValueType",
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics))
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32)
            ConcretePointer (ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                                               "MethodTable",
                                                                               fieldMethodTableGenerics))) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "ValueTypeHashCodeStrategy", strategyGenerics)) when
            methodTableGenerics.IsEmpty
            && objectHandleGenerics.IsEmpty
            && fieldMethodTableGenerics.IsEmpty
            && strategyGenerics.IsEmpty
            ->
            let operation = "ValueType.GetHashCodeStrategy"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let methodTable =
                NativeCall.methodTableOfEvalStackValue operation (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let receiverPointer =
                NativeCall.objectHandleOnStackTarget operation state "objHandle" instruction.Arguments.[1]

            let outFieldOffset =
                NativeCall.managedPointerOfPointerArgument operation "fieldOffset" instruction.Arguments.[2]

            let outFieldSize =
                NativeCall.managedPointerOfPointerArgument operation "fieldSize" instruction.Arguments.[3]

            let outFieldMethodTable =
                NativeCall.managedPointerOfPointerArgument operation "fieldMT" instruction.Arguments.[4]

            // The managed caller writes `object thisRef = this` and hands us a handle to that
            // local, so the receiver is always a live box of the very struct `methodTable`
            // describes. CoreCLR reads it only to decide whether an object-reference field is
            // null; PawPrint reads its `CliValueType` structurally instead of byte-poking it,
            // which answers the same question without needing a byte view of an object reference.
            let receiver =
                match IlMachineState.readManagedByref ctx.BaseClassTypes state receiverPointer with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    failwith
                        $"%s{operation}: objHandle addressed a null reference; the managed caller boxes its receiver into that local before the call"
                | other -> failwith $"%s{operation}: expected objHandle to address an object reference, got %O{other}"

            let contents =
                match ManagedHeap.tryGet receiver state.ManagedHeap with
                | Some allocated ->
                    // `ValueType.GetHashCode`'s `this` is already a reference to the box, so
                    // `object thisRef = this` copies that reference rather than boxing again, and
                    // `pMT` is that same box's MethodTable. The walk reads its field *order* from
                    // `methodTable` and its field *values* from this storage, so the two
                    // disagreeing would silently pair one type's fields with another's cells.
                    if allocated.ConcreteType <> methodTable then
                        failwith
                            $"%s{operation}: pMT names %O{methodTable} but objHandle addresses a box of %O{allocated.ConcreteType}; the managed caller passes the MethodTable of the very object it hands over"

                    allocated.Contents
                | None ->
                    failwith
                        $"%s{operation}: objHandle addressed %O{receiver}, which is not a live boxed value type on the non-array heap"

            let valueTypeGetHashCode =
                requiredValueTypeMethod operation ctx.BaseClassTypes "GetHashCode" 0

            let state, strategy =
                walk
                    operation
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    ctx.Thread
                    valueTypeGetHashCode
                    methodTable
                    contents
                    0
                    state

            // PawPrint carries a CLI `uint32` as an `Int32` preserving the low 32 bits; see
            // `PrimitiveType.UInt32`.
            let uint32Value (value : int) : CliType =
                CliType.Numeric (CliNumericType.Int32 value)

            let offsetValue, sizeValue, methodTableValue =
                match strategy with
                | HashCodeStrategy.NoField -> 0, 0, None
                | HashCodeStrategy.ReferenceField offset
                | HashCodeStrategy.DoubleField offset
                | HashCodeStrategy.SingleField offset -> offset, 0, None
                | HashCodeStrategy.FastGetHashCode (offset, size) -> offset, size, None
                | HashCodeStrategy.ValueTypeOverride (offset, fieldMethodTable) -> offset, 0, Some fieldMethodTable

            let methodTableCell =
                match methodTableValue with
                | None ->
                    // The guest asserts `fieldMT != null` only on the `ValueTypeOverride` arm, but
                    // it must still compare equal to zero on the others, which a
                    // `MethodTablePtr` never would.
                    CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
                | Some handle ->
                    CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle))

            // The QCall wrapper zeroes all three out-params before the walk runs
            // (`vm/comutilnative.cpp:1789`), so every strategy — `None` included — leaves them
            // defined.
            let state =
                let write ptr value state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state ptr value

                state
                |> write outFieldOffset (uint32Value offsetValue)
                |> write outFieldSize (uint32Value sizeValue)
                |> write outFieldMethodTable methodTableCell

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (strategyCode strategy)))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | _ -> None
