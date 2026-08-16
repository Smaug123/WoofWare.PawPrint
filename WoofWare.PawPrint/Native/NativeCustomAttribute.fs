namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeCustomAttribute =
    /// Decode an `ObjectHandleOnStack` argument whose target slot holds an object reference,
    /// and return that referenced address. Mirrors CoreCLR's `pHandle.Get()` on a `pCaType`-
    /// or `pCtor`-shaped argument: the QCall caller hands us a byref to a managed slot, and
    /// the slot's current value is the heap object we want to operate on.
    let private dereferenceObjectHandle
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (argName : string)
        (arg : CliType)
        : ManagedHeapAddress
        =
        let ptr = NativeCall.objectHandleOnStackTarget operation state argName arg

        match IlMachineState.readManagedByref baseClassTypes state ptr with
        | CliType.ObjectRef (Some addr) -> addr
        | CliType.ObjectRef None ->
            failwith $"%s{operation}: ObjectHandleOnStack(%s{argName}) pointed at a null reference"
        | other -> failwith $"%s{operation}: expected ObjectRef behind ObjectHandleOnStack(%s{argName}), got %O{other}"

    /// Materialise the byte slice `[startIdx, endIdx)` of `arr` into an
    /// `ImmutableArray<byte>`. Each cell must already be a UInt8 (the byte[] shape produced
    /// by callers that allocate a fresh blob buffer); a different cell shape indicates the
    /// caller wired a non-byte-array into a byte pointer slot and is a contract violation.
    let private materialiseBytes
        (operation : string)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (startIdx : int)
        (endIdx : int)
        : ImmutableArray<byte>
        =
        if endIdx < startIdx then
            failwith $"%s{operation}: blob end index %d{endIdx} is before start index %d{startIdx} (same array %O{arr})"

        let builder = ImmutableArray.CreateBuilder<byte> (endIdx - startIdx)

        for i in startIdx .. endIdx - 1 do
            match ManagedHeap.getArrayValue arr i state.ManagedHeap with
            | CliType.Numeric (CliNumericType.UInt8 b) -> builder.Add b
            | other ->
                failwith $"%s{operation}: expected UInt8 cell at byte offset %d{i} of array %O{arr}, got %O{other}"

        builder.MoveToImmutable ()

    /// Extract `(arrayAddr, byteIndex)` from a blob byte pointer. PawPrint currently models
    /// the `byte*` cursor as a managed byref into a `byte[]` cell; ECMA-style raw native
    /// pointers haven't yet been threaded through the BCL's `CustomAttributeRecord` path,
    /// so we require both `ppBlob` and `pEndBlob` to be plain `ArrayElement` byrefs (no
    /// projections) into the same byte array. Generalising to other shapes is upstream
    /// work tracked alongside the metadata-blob reader.
    let private blobPointerBounds
        (operation : string)
        (label : string)
        (ptr : ManagedPointerSource)
        : ManagedHeapAddress * int
        =
        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, idx), []) -> arr, idx
        | ManagedPointerSource.Null ->
            failwith
                $"TODO: %s{operation} %s{label} pointer is null; CoreCLR allows a null *ppBlob to skip the entire blob parse but PawPrint hasn't modelled that path yet"
        | other ->
            failwith
                $"TODO: %s{operation} %s{label} pointer must be a plain ArrayElement byref (no projections); other shapes (e.g. raw native pointers, byte-view projections) are not yet supported, got %O{other}"

    /// <summary>
    /// Resolve one constructor parameter type to the shape the blob decoder needs.
    /// </summary>
    /// <remarks>
    /// The fixed-args section of a <c>CustomAttrib</c> blob is not self-describing (ECMA-335
    /// II.23.3): an enum argument is written as a bare value of its underlying type. So the
    /// width has to come from the metadata, which is why this lives here — beside the machine
    /// state — rather than in the parser.
    ///
    /// Only parameter types that <c>tryShapeWithoutResolution</c> cannot handle reach the
    /// resolution path, so an attribute whose ctor takes only primitives costs no type loads.
    /// </remarks>
    let private resolveArgShape
        (operation : string)
        (ctx : NativeCallContext)
        (ctorAssembly : DumpedAssembly)
        (declaringTypeGenerics : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        (paramType : TypeDefn)
        : IlMachineState * CustomAttribArgShape
        =
        match CustomAttribute.tryShapeWithoutResolution paramType with
        | Some shape -> state, shape
        | None ->

        let state, handle =
            IlMachineTypeResolution.concretizeType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                ctorAssembly.Name
                declaringTypeGenerics
                // An attribute ctor cannot be a generic method: ECMA-335 II.22.10 names it by a
                // MethodDef/MemberRef with no generic arguments, and the BCL's
                // `FilterCustomAttributeRecord` never surfaces one.
                ImmutableArray.Empty
                paramType

        let state, isEnum =
            IlMachineRuntimeMetadata.isEnumValueType ctx.LoggerFactory ctx.BaseClassTypes state handle

        if not isEnum then
            failwith
                $"TODO: %s{operation}: ctor parameter of type %O{paramType} is neither a primitive, an SZARRAY of primitives, nor an enum; TYPE (0x50) and TAGGED_OBJECT (0x51) fixed args are not yet decoded"

        let state, underlyingHandle =
            IlMachineRuntimeMetadata.enumUnderlyingHandle ctx.LoggerFactory ctx.BaseClassTypes state handle
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: ctor parameter of type %O{paramType} is an enum, but its underlying type could not be read; an enum's sole instance field must be `value__`"
            )

        match underlyingHandle with
        | ConcretePrimitive state.ConcreteTypes underlying ->
            match EnumUnderlyingType.ofPrimitive underlying with
            | Some underlying -> state, CustomAttribArgShape.Enum underlying
            | None ->
                // ECMA-335 II.14.3 requires a built-in integer type here, and the CLR type loader
                // enforces it, so only hand-crafted metadata can get us here.
                failwith
                    $"%s{operation}: ctor parameter of type %O{paramType} is an enum whose underlying type %O{underlying} is not a legal enum underlying type"
        | other ->
            failwith
                $"%s{operation}: ctor parameter of type %O{paramType} is an enum whose `value__` did not concretize to a primitive, got %O{other}"

    /// <summary>
    /// Resolve a named argument's blob-declared <c>FieldOrPropType</c> to the shape the value
    /// decoder needs.
    /// </summary>
    /// <remarks>
    /// This is the sole refusal point for every named-arg serialization type PawPrint does not yet
    /// support, so that each one produces its own diagnostic naming the construct. In particular
    /// <c>SzArray</c> is refused *here* rather than being decoded and left to fail inside
    /// <c>CustomAttribValueLowering</c>: that failure would name neither named args nor this
    /// entry point, and the null-array sentinel would trip it too.
    ///
    /// The counterpart for fixed args is <c>resolveArgShape</c> above, which resolves a ctor
    /// parameter's <c>TypeDefn</c> instead. Both exist because an enum's value width lives in the
    /// enum's metadata rather than in the blob.
    /// </remarks>
    let private resolveNamedArgShape
        (operation : string)
        (elemType : CustomAttribFieldOrPropType)
        : CustomAttribArgShape
        =
        match elemType with
        | CustomAttribFieldOrPropType.Primitive pt -> CustomAttribArgShape.Primitive pt
        | CustomAttribFieldOrPropType.SzArray elt ->
            failwith
                $"TODO: %s{operation}: named argument has SZARRAY type (element %O{elt}); lowering a decoded array to a managed array is not implemented in CustomAttribValueLowering, which blocks the fixed-arg path identically, so both should be fixed together"
        | CustomAttribFieldOrPropType.Enum typeName ->
            let described =
                match typeName with
                | Some name -> $"\"%s{name}\""
                | None -> "the SerString null sentinel"

            failwith
                $"TODO: %s{operation}: named argument has ENUM type named by %s{described}; resolving a custom-attribute type name needs the reflection type-name parser (CoreCLR's TypeName::GetTypeReferencedByCustomAttribute), which PawPrint does not have"
        | CustomAttribFieldOrPropType.Type ->
            failwith
                $"TODO: %s{operation}: named argument has TYPE (0x50) type; its value is a reflection type name, so it needs the same type-name parser as the ENUM case"
        | CustomAttribFieldOrPropType.TaggedObject ->
            failwith
                $"TODO: %s{operation}: named argument has TAGGED_OBJECT (0x51) type; its value carries its own FieldOrPropType recursively, so it depends on the SZARRAY and TYPE cases first"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        // `instruction.ExecutingMethod.Name` is deliberately omitted from the match.
        // For `CustomAttribute_CreateCustomAttributeInstance` the actual PInvoke stub
        // carries a Roslyn-generated mangled name (`<CreateCustomAttributeInstance>g____PInvoke|30_0`),
        // whereas other QCalls (e.g. `RuntimeMethodHandle::IsCAVisibleFromDecoratedType`)
        // are unwrapped P/Invoke methods with the natural name. The entry-point name and
        // the parameter/return signature together already disambiguate this QCall.
        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "CustomAttribute_CreateCustomAttributeInstance",
          "System.Private.CoreLib",
          "System.Reflection",
          "CustomAttribute",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              moduleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              typeHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              ctorHandleGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              instanceHandleGenerics) ],
          MethodReturnType.Void when
            moduleGenerics.IsEmpty
            && typeHandleGenerics.IsEmpty
            && ctorHandleGenerics.IsEmpty
            && instanceHandleGenerics.IsEmpty
            ->
            let operation = "CustomAttribute.CreateCustomAttributeInstance"

            if instruction.Arguments.Length <> 7 then
                failwith $"%s{operation}: expected seven native arguments, got %d{instruction.Arguments.Length}"

            // The handler runs in two phases, sharing the marker discipline used by
            // `RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter` (NativeRuntimeType.fs:
            // around line 2347): phase 1 allocates the attribute instance, writes the cursor /
            // named-arg-count out-slots, pushes the allocated address as a re-entry marker, then
            // hands control to the ctor; phase 2 sees the marker on re-entry and copies it into
            // the `instance` ObjectHandleOnStack. We defer the `result.Set` write to phase 2
            // rather than match CoreCLR's pre-ctor placement: in CoreCLR the caller GC-protects
            // through `pInstance`, but PawPrint's caller (RuntimeCustomAttributeData) treats a
            // ctor-thrown exception as fatal anyway, so the observable difference is nil.
            let resultHandle =
                NativeCall.objectHandleOnStackTarget operation state "instance" instruction.Arguments.[6]

            match instruction.EvaluationStack.Values with
            | [ marker ] ->
                let addr =
                    match marker with
                    | EvalStackValue.ObjectRef a -> a
                    | other ->
                        failwith
                            $"%s{operation}: expected re-entry marker (object ref to allocated attribute instance) on eval stack, got %O{other}"

                let _, state = IlMachineState.popEvalStack ctx.Thread state

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        resultHandle
                        (CliType.ObjectRef (Some addr))

                NativeHandlerResult.completed state |> Some
            | [] ->
                // QCallModule is not consulted on the success path; CoreCLR threads it through
                // `GetDataFromBlob` for SERIALIZATION_TYPE_TYPE / TAGGED_OBJECT, which the
                // Phase A blob reader does not yet emit. Decode and ignore for now so that
                // refactoring the wiring later doesn't require reshuffling argument positions.
                let _moduleAssemblyFullName =
                    NativeCall.qCallModuleToAssemblyFullName
                        operation
                        state
                        (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

                let attrTypeAddr =
                    dereferenceObjectHandle operation ctx.BaseClassTypes state "pCaType" instruction.Arguments.[1]

                let ctorStubAddr =
                    dereferenceObjectHandle operation ctx.BaseClassTypes state "pCtor" instruction.Arguments.[2]

                // `ref IntPtr ppBlob` is a managed byref to an IntPtr cell. The byref itself
                // lets us write the new cursor back via `*ppBlob = updated`; the *current*
                // cursor is the IntPtr value stored in that cell, so we have to read through
                // the byref once more before decoding the underlying managed pointer. Don't
                // collapse this into a single `managedPointerOfPointerArgument` call — that
                // would treat the outer byref as if it were the IntPtr itself.
                let blobCursorSlot =
                    NativeCall.managedPointerOfPointerArgument operation "ppBlob" instruction.Arguments.[3]

                let blobCursorIntPtr =
                    IlMachineState.readManagedByref ctx.BaseClassTypes state blobCursorSlot

                let blobCursorPtr =
                    NativeCall.managedPointerOfPointerArgument operation "*ppBlob" blobCursorIntPtr

                // `pEndBlob` is `IntPtr` by value, so it already *is* the cursor.
                let blobEndPtr =
                    NativeCall.managedPointerOfPointerArgument operation "pEndBlob" instruction.Arguments.[4]

                let namedArgsSlot =
                    NativeCall.managedPointerOfPointerArgument operation "pcNamedArgs" instruction.Arguments.[5]

                // ECMA-335 attribute ctors are reference-typed by construction (custom-attribute
                // classes derive from `System.Attribute`, a reference type). CoreCLR's QCall path
                // explicitly handles value-typed ctor declaring types by unboxing the allocated
                // instance for `args[0]`; that path is unreachable from the BCL filter
                // (`FilterCustomAttributeRecord` only surfaces attribute types), and PawPrint's
                // `concretizeMethodWithAllGenerics` produces a ConcreteTypeHandle which the
                // caller would have to unbox before passing in. Until a real value-type
                // attribute case appears, refuse it explicitly here rather than silently
                // allocating a boxed instance.
                let typeHandleTarget =
                    NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                        operation
                        state
                        (EvalStackValue.ObjectRef attrTypeAddr)

                let instantiatedHandle =
                    match typeHandleTarget with
                    | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                        RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                    | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                        failwith
                            $"TODO: open constructed types are not handled at Native/NativeCustomAttribute.fs:%s{__LINE__}; got %O{openConstructed}"
                    | RuntimeTypeHandleTarget.Closed handle -> handle
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                        failwith
                            $"TODO: %s{operation}: attribute type is an open generic type definition (%s{identity.AssemblyFullName} / %O{identity.TypeDefinition.Get}); attribute decoration is restricted to closed types"
                    | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                        failwith
                            $"%s{operation}: attribute type was a generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; the BCL filter should never surface this"
                    | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                        failwith
                            $"%s{operation}: attribute type was a method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; the BCL filter should never surface this"

                let concreteType =
                    AllConcreteTypes.lookup instantiatedHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: attribute type handle %O{instantiatedHandle} was not registered in ConcreteTypes"
                    )

                let attrAssembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: attribute type's assembly %s{concreteType.Assembly.FullName} is not loaded"
                    )

                let attrTypeInfo = attrAssembly.TypeDefs.[concreteType.Definition.Get]

                if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies attrTypeInfo then
                    failwith
                        $"TODO: %s{operation}: value-typed attribute %s{attrTypeInfo.Namespace}.%s{attrTypeInfo.Name} would need unboxing for `this` slot; CoreCLR's value-type branch is unreachable from the BCL filter and is not yet modelled here"

                // Resolve the ctor metadata via the method-handle registry. The stub's `m_value`
                // field (a `RuntimeMethodHandleInternal`) carries a registry id minted by whichever
                // QCall produced the handle, and that's the canonical identity available regardless
                // of whether the stub was allocated F#-side (via `IlMachineState.getOrAllocateMethod`)
                // or by the BCL itself (via `ModuleHandle.ResolveMethodHandle(...).GetMethodInfo()`).
                let ctorStubObj = ManagedHeap.get ctorStubAddr state.ManagedHeap

                let stubValueField =
                    IlMachineState.requiredOwnInstanceFieldId state ctorStubObj.ConcreteType "m_value"

                let stubMValue =
                    AllocatedNonArrayObject.DereferenceFieldById stubValueField ctorStubObj

                let methodRegistryId =
                    NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation stubMValue
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: RuntimeMethodInfoStub at %O{ctorStubAddr} carried a null RuntimeMethodHandleInternal"
                    )

                // An attribute's constructor is always a metadata method: it is named by a
                // MethodDef/MemberRef token in the custom-attribute blob we are decoding, so a
                // no-metadata (`DynamicMethod`) handle cannot reach here.
                let identity =
                    match MethodHandleRegistry.resolveMethodFromId methodRegistryId state.MethodHandles with
                    | Some (MethodHandle.FromMetadata identity) -> identity
                    | Some (MethodHandle.FromDynamic dynamicHandle) ->
                        failwith
                            $"%s{operation}: RuntimeMethodHandleInternal id %d{methodRegistryId} (from stub at %O{ctorStubAddr}) names %O{dynamicHandle}, a Reflection.Emit method; an attribute constructor is named by a token in the blob being decoded, so this is a bug in whatever produced the stub"
                    | None ->
                        failwith
                            $"%s{operation}: RuntimeMethodHandleInternal id %d{methodRegistryId} (from stub at %O{ctorStubAddr}) did not resolve to a registered MethodHandle"

                let ctorAssemblyName = identity.GetAssemblyFullName ()

                let ctorAssembly =
                    state.LoadedAssembly' ctorAssemblyName
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: ctor's declaring assembly %s{ctorAssemblyName} is not loaded"
                    )

                let methodDefHandle = identity.GetMethodDefinitionHandle().Get

                let ctorMetadata =
                    let mutable found = Unchecked.defaultof<_>

                    if not (ctorAssembly.Methods.TryGetValue (methodDefHandle, &found)) then
                        failwith
                            $"%s{operation}: ctor MethodDef %O{methodDefHandle} not found in assembly %s{ctorAssemblyName}"

                    found

                // ECMA-335 II.23.3 attribute ctors must be instance methods; CoreCLR likewise
                // rejects vararg ctors before this QCall (FilterCustomAttributeRecord). A
                // mismatch indicates a corrupted BCL filter rather than user error, so we surface
                // the precise condition.
                if ctorMetadata.IsStatic then
                    failwith
                        $"%s{operation}: ctor %s{attrTypeInfo.Namespace}.%s{attrTypeInfo.Name}::.ctor is static; attribute ctors must be instance methods"

                let blobStartArr, blobStartIdx =
                    blobPointerBounds operation "*ppBlob (start)" blobCursorPtr

                let blobEndArr, blobEndIdx = blobPointerBounds operation "pEndBlob" blobEndPtr

                if blobStartArr <> blobEndArr then
                    failwith
                        $"%s{operation}: ppBlob (array %O{blobStartArr}) and pEndBlob (array %O{blobEndArr}) point into different arrays; the bounds must straddle a single contiguous byte buffer"

                let blobBytes =
                    materialiseBytes operation state blobStartArr blobStartIdx blobEndIdx

                // Resolving the ctor's parameter types can load assemblies, so it threads state;
                // it cannot suspend, and it happens before the cursor write-back below, so a
                // re-entered handler simply redoes it.
                let state, paramShapes =
                    ((state, []), ctorMetadata.Signature.ParameterTypes)
                    ||> List.fold (fun (state, acc) paramType ->
                        let state, shape =
                            resolveArgShape operation ctx ctorAssembly concreteType.Generics state paramType

                        state, shape :: acc
                    )
                    |> fun (state, acc) -> state, List.rev acc

                let fixedArgs, fixedArgsConsumed =
                    match CustomAttribute.readFixedArgs paramShapes blobBytes with
                    | Ok (args, next) -> args, next
                    | Error msg -> failwith $"%s{operation}: failed to parse fixed args from CustomAttrib blob: %s{msg}"

                // ECMA-335 II.23.3: the named-arg count is a uint16 that follows the fixed args.
                // If the blob is exhausted at this point the BCL convention is "zero named args";
                // CoreCLR's `pBlob != pEndBlob` check guards the read. Mirror that here.
                let remainingBytes = blobBytes.Length - fixedArgsConsumed

                let namedArgCount, cursorAfterNamedArgs =
                    if remainingBytes = 0 then
                        0, fixedArgsConsumed
                    elif remainingBytes < 2 then
                        failwith
                            $"%s{operation}: CustomAttrib blob has %d{remainingBytes} byte(s) after fixed args, but the named-arg count requires 2 bytes"
                    else
                        let lo = blobBytes.[fixedArgsConsumed]
                        let hi = blobBytes.[fixedArgsConsumed + 1]
                        let n = int (uint16 lo ||| (uint16 hi <<< 8))
                        n, fixedArgsConsumed + 2

                if namedArgCount = 0 && cursorAfterNamedArgs <> blobBytes.Length then
                    failwith
                        $"%s{operation}: CustomAttrib blob declares zero named args but %d{blobBytes.Length - cursorAfterNamedArgs} byte(s) remain at offset %d{cursorAfterNamedArgs} (total %d{blobBytes.Length})"

                // Run type initialisation *before* writing back the blob cursor and named-arg
                // count. If `ensureTypeInitialised` suspends for a static ctor (or a chain of
                // them), the QCall frame is re-entered with an empty eval stack and the handler
                // has to re-parse the blob from `*ppBlob`. Advancing the cursor first would mean
                // the re-entered handler reads zero bytes (or the wrong bytes), so the writes
                // must wait until we've committed to allocating the instance and calling the
                // ctor.
                let state, typeInit =
                    IlMachineStateExecution.ensureTypeInitialised
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.Thread
                        instantiatedHandle
                        state

                match typeInit with
                | WhatWeDid.SuspendedForClassInit -> NativeHandlerResult.suspendedForClassInit state |> Some
                | WhatWeDid.BlockedOnClassInit blockedBy ->
                    NativeHandlerResult.blockedOnClassInit blockedBy state |> Some
                | WhatWeDid.ThrowingTypeInitializationException ->
                    NativeHandlerResult.throwingTypeInitializationException state |> Some
                | WhatWeDid.SuspendedForManagedCall ->
                    failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
                | WhatWeDid.VoluntaryYield _ ->
                    failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
                | WhatWeDid.Executed ->

                let updatedCursor =
                    ManagedPointerSource.Byref (
                        ByrefRoot.ArrayElement (blobStartArr, blobStartIdx + cursorAfterNamedArgs),
                        []
                    )

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        blobCursorSlot
                        (CliType.RuntimePointer (CliRuntimePointer.Managed updatedCursor))

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        namedArgsSlot
                        (CliType.Numeric (CliNumericType.Int32 namedArgCount))

                let state, instanceFields =
                    IlMachineState.buildInstanceStorage ctx.LoggerFactory ctx.BaseClassTypes state instantiatedHandle

                let instanceAddr, state =
                    IlMachineState.allocateManagedObject instantiatedHandle instanceFields state

                let state, concretizedCtor, _declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        concreteType.Generics
                        ctorMetadata
                        (identity.GetMethodGenerics () |> ImmutableArray.CreateRange)
                        state

                if MethodInfo.arity concretizedCtor <> List.length fixedArgs then
                    failwith
                        $"%s{operation}: ctor expects %d{MethodInfo.arity concretizedCtor} fixed argument(s) but the blob produced %d{List.length fixedArgs}"

                // Push the re-entry marker first so it survives the ctor call below: `callMethod`
                // pops `this` plus the ctor args, leaving exactly one ObjectRef beneath the new
                // frame's locals. The phase-2 branch above pops that marker to recover the
                // allocated instance address.
                let state =
                    IlMachineState.pushToEvalStack (CliType.ObjectRef (Some instanceAddr)) ctx.Thread state

                let state =
                    IlMachineState.pushToEvalStack (CliType.ObjectRef (Some instanceAddr)) ctx.Thread state

                let state =
                    (state, fixedArgs)
                    ||> List.fold (fun state arg ->
                        let cliValue, state =
                            CustomAttribValueLowering.toCliType ctx.LoggerFactory ctx.BaseClassTypes arg state

                        IlMachineState.pushToEvalStack cliValue ctx.Thread state
                    )

                let threadState = state.ThreadState.[ctx.Thread]

                // wasConstructing = None: we drive the ctor as a regular instance call, since
                // the constructed instance is already on the eval stack as the re-entry marker.
                // ReturnValueDisposition.PushToCaller / advanceProgramCounterOfCaller = false:
                // the native QCall frame has no IL to advance.
                let state =
                    IlMachineStateExecution.callMethod
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        None
                        ConstructionState.NotConstructing
                        false
                        false
                        false
                        concretizedCtor.Generics
                        concretizedCtor
                        ctx.Thread
                        threadState
                        None
                        ReturnValueDisposition.PushToCaller
                        false // wrapExceptionInTargetInvocation
                        state

                NativeHandlerResult.pushedManagedCallee state |> Some
            | other ->
                failwith
                    $"%s{operation}: expected at most one re-entry marker on the eval stack, got %d{other.Length} value(s): %A{other}"
        | "CustomAttribute_CreatePropertyOrFieldData",
          "System.Private.CoreLib",
          "System.Reflection",
          "CustomAttribute",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              moduleGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "StringHandleOnStack",
                                              nameGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              typeGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              valueGenerics) ],
          MethodReturnType.Void when
            moduleGenerics.IsEmpty
            && nameGenerics.IsEmpty
            && typeGenerics.IsEmpty
            && valueGenerics.IsEmpty
            ->
            let operation = "CustomAttribute.CreatePropertyOrFieldData"

            if instruction.Arguments.Length <> 7 then
                failwith $"%s{operation}: expected seven native arguments, got %d{instruction.Arguments.Length}"

            // Single-phase, unlike the sibling handler above: nothing in the supported set runs
            // guest code or triggers class initialisation. `boxValueType` allocates without running
            // a cctor (matching CoreCLR's `MethodTable::Box`), `concretizeType` on a primitive is a
            // pure lookup, and `getOrAllocateType` has no suspension path. So there is no re-entry
            // marker, and `*ppBlobStart` is written last, as CoreCLR does. Adding ENUM support
            // reintroduces a type load and may reintroduce the two-phase structure.

            // QCallModule is not consulted while only primitive named args are supported; CoreCLR
            // threads it into `GetDataFromBlob` for the ENUM / TYPE / TAGGED_OBJECT cases, all of
            // which `resolveNamedArgShape` refuses. Decode and ignore, so that wiring it up later
            // doesn't reshuffle argument positions.
            let _moduleAssemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            // `BYTE** ppBlobStart` is a managed byref to an IntPtr cell: the byref lets us write
            // the advanced cursor back, and the cell's *current* value is the cursor itself, so we
            // read through it once more before decoding. Same shape as `ppBlob` in the sibling
            // handler; don't collapse the two reads into one.
            let blobCursorSlot =
                NativeCall.managedPointerOfPointerArgument operation "ppBlobStart" instruction.Arguments.[1]

            let blobCursorIntPtr =
                IlMachineState.readManagedByref ctx.BaseClassTypes state blobCursorSlot

            let blobCursorPtr =
                NativeCall.managedPointerOfPointerArgument operation "*ppBlobStart" blobCursorIntPtr

            // `pBlobEnd` is `BYTE*` by value, so it already *is* the cursor.
            let blobEndPtr =
                NativeCall.managedPointerOfPointerArgument operation "pBlobEnd" instruction.Arguments.[2]

            let nameHandle =
                NativeCall.stringHandleOnStackTarget operation state "pName" instruction.Arguments.[3]

            // `[MarshalAs(UnmanagedType.Bool)] out bool` lowers to `int32*`: the managed wrapper
            // passes `&` of an int32 local and does `cgt.un` against 0 after the call.
            let isPropertySlot =
                NativeCall.managedPointerOfPointerArgument operation "pbIsProperty" instruction.Arguments.[4]

            let typeSlot =
                NativeCall.objectHandleOnStackTarget operation state "pType" instruction.Arguments.[5]

            let valueSlot =
                NativeCall.objectHandleOnStackTarget operation state "pValue" instruction.Arguments.[6]

            let blobStartArr, blobStartIdx =
                blobPointerBounds operation "*ppBlobStart" blobCursorPtr

            let blobEndArr, blobEndIdx = blobPointerBounds operation "pBlobEnd" blobEndPtr

            if blobStartArr <> blobEndArr then
                failwith
                    $"%s{operation}: ppBlobStart (array %O{blobStartArr}) and pBlobEnd (array %O{blobEndArr}) point into different arrays; the bounds must straddle a single contiguous byte buffer"

            let blobBytes =
                materialiseBytes operation state blobStartArr blobStartIdx blobEndIdx

            let header, valueOffset =
                match CustomAttribute.readNamedArgHeader blobBytes 0 with
                | Ok (header, next) -> header, next
                | Error msg -> failwith $"%s{operation}: failed to parse named arg from CustomAttrib blob: %s{msg}"

            let shape = resolveNamedArgShape operation header.ElemType

            let decoded, consumed =
                match CustomAttribute.readElem shape blobBytes valueOffset with
                | Ok (value, next) -> value, next
                | Error msg ->
                    failwith $"%s{operation}: failed to parse named arg value from CustomAttrib blob: %s{msg}"

            let isProperty =
                match header.Kind with
                | CustomAttribNamedArgKind.Field -> 0
                | CustomAttribNamedArgKind.Property -> 1

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    isPropertySlot
                    (CliType.Numeric (CliNumericType.Int32 isProperty))

            // CoreCLR passes a null name straight through (`pName.Set(NULL)`), and the managed
            // caller then throws out of `GetProperty(null)` into a CustomAttributeFormatException.
            // Reproduce the null rather than inventing a name.
            let state, nameValue =
                match header.Name with
                | None -> state, CliType.ObjectRef None
                | Some "" ->
                    let addr, state =
                        IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state

                    state, CliType.ObjectRef (Some addr)
                | Some name ->
                    let addr, state =
                        IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes name state

                    state, CliType.ObjectRef (Some addr)

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state nameHandle nameValue

            // `pType` is written only where CoreCLR writes it. For a non-null string or a boxed
            // primitive it stays null, and the managed caller infers the member's type from
            // `value.GetType()`; the distinction picks `GetProperty(name)` over the type-filtered
            // `GetProperty(name, type, Type.EmptyTypes)` overload, so it is observable. We write
            // the null explicitly rather than relying on the caller having pre-nulled its local.
            let state, valueCli, typeCli =
                match decoded with
                | CustomAttribFixedArg.String None ->
                    // The one in-scope case where CoreCLR sets a real type: a null SerString value
                    // leaves pValue null, so it hands back typeof(string) to disambiguate.
                    let state, stringHandle =
                        IlMachineTypeResolution.concretizeType
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            state
                            ctx.BaseClassTypes.Corelib.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            (TypeDefn.PrimitiveType PrimitiveType.String)

                    let typeAddr, state =
                        IlMachineState.getOrAllocateType
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            (RuntimeTypeHandleTarget.Closed stringHandle)
                            state

                    state, CliType.ObjectRef None, CliType.ObjectRef (Some typeAddr)
                | CustomAttribFixedArg.String (Some _) ->
                    let value, state =
                        CustomAttribValueLowering.toCliType ctx.LoggerFactory ctx.BaseClassTypes decoded state

                    state, value, CliType.ObjectRef None
                | _ ->

                // A primitive boxes as the corelib type named by the *blob's* type byte, not by the
                // decoded value's F# shape: `U4` boxes as System.UInt32 even though CLI eval-stack
                // rules normalise its value to Int32. Mirrors CoreCLR's
                // `CoreLibBinder::GetElementType((CorElementType)fieldType)`.
                let boxPrimitive =
                    match shape with
                    | CustomAttribArgShape.Primitive pt -> pt
                    | other ->
                        failwith
                            $"%s{operation}: logic error: decoded value %O{decoded} came from non-primitive shape %O{other}, which resolveNamedArgShape should have refused"

                let state, boxHandle =
                    IlMachineTypeResolution.concretizeType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        ctx.BaseClassTypes.Corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        (TypeDefn.PrimitiveType boxPrimitive)

                let payload, state =
                    CustomAttribValueLowering.toCliType ctx.LoggerFactory ctx.BaseClassTypes decoded state

                let boxAddr, state =
                    UnaryMetadataObjectOps.boxValueType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        boxHandle
                        (EvalStackValue.ofCliType payload)
                        state

                state, CliType.ObjectRef (Some boxAddr), CliType.ObjectRef None

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state valueSlot valueCli

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state typeSlot typeCli

            // Written last, as CoreCLR does. The managed caller checks `blobStart != blobEnd` after
            // the whole named-arg loop, so an error here surfaces to the guest as a
            // CustomAttributeFormatException rather than silently mis-decoding the next arg.
            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    blobCursorSlot
                    (CliType.RuntimePointer (
                        CliRuntimePointer.Managed (
                            ManagedPointerSource.Byref (
                                ByrefRoot.ArrayElement (blobStartArr, blobStartIdx + consumed),
                                []
                            )
                        )
                    ))

            NativeHandlerResult.completed state |> Some
        | _ -> None
