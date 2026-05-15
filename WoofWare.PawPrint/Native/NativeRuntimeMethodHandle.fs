namespace WoofWare.PawPrint

open System.Reflection

[<RequireQualifiedAccess>]
module NativeRuntimeMethodHandle =
    let private resolveMethodInfoFromHandleArg
        (operation : string)
        (state : IlMachineState)
        (arg : CliType)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        // CoreCLR's RuntimeMethodHandle FCalls dereference the MethodDesc* directly and
        // assert non-null; PawPrint's existing callers never yield a null handle, so we
        // surface a contract violation rather than silently producing a default value.
        let methodHandleId =
            NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation arg
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: null RuntimeMethodHandleInternal")

        let methodHandle =
            MethodHandleRegistry.resolveMethodFromId methodHandleId state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: registry id %d{methodHandleId} did not resolve to a known MethodHandle"
            )

        let assemblyFullName = methodHandle.GetAssemblyFullName ()

        let assembly =
            state.LoadedAssembly' assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

        let methodDefHandle = methodHandle.GetMethodDefinitionHandle().Get

        let mutable methodInfo =
            Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

        if not (assembly.Methods.TryGetValue (methodDefHandle, &methodInfo)) then
            failwith $"%s{operation}: MethodDef %O{methodDefHandle} not found in assembly %s{assemblyFullName}"

        methodInfo

    /// Resolve a <c>QCallTypeHandle</c>-encoded type to its <c>TypeInfo</c>
    /// definition and concrete-handle, refusing the non-closed cases that
    /// <c>RuntimeMethodHandle_IsCAVisibleFromDecoratedType</c>'s CoreCLR sibling
    /// rejects with <c>Arg_InvalidHandle</c> (open generic definitions and
    /// generic parameters correspond to CoreCLR <c>TypeDesc</c>s).
    let private closedTypeFromTarget
        (operation : string)
        (label : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn> * ConcreteTypeHandle
        =
        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | None -> failwith $"%s{operation}: %s{label} concrete handle %O{handle} not found in AllConcreteTypes"
                | Some concreteType ->
                    let assembly =
                        state.LoadedAssembly concreteType.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly %s{concreteType.Assembly.FullName} for %s{label} is not loaded"
                        )

                    let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                    assembly, typeInfo, handle
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // CoreCLR treats arrays/byrefs/pointers/fnptrs as TypeDescs; its
                // RuntimeMethodHandle_IsCAVisibleFromDecoratedType throws
                // Arg_InvalidHandle (kArgumentNullException) when sourceHandle or
                // targetHandle is a TypeDesc. PawPrint doesn't yet have a host
                // helper to raise that exception object, so surface the precise
                // condition for the caller to fix at the source.
                failwith
                    $"TODO: %s{operation}: %s{label} is a structural type (%O{handle}); CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            failwith
                $"TODO: %s{operation}: %s{label} is an open generic definition (%O{identity}); CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"TODO: %s{operation}: %s{label} is a generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: %s{operation}: %s{label} is a method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"

    /// Build a type's enclosing-type chain (innermost first, outermost last),
    /// where each entry projects only the bits <c>AccessCheck.canAccessClass</c>
    /// inspects. The walk terminates at the outermost top-level type whose
    /// <c>DeclaringType</c> handle is nil.
    let private buildAccessLevelChain
        (operation : string)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : AccessLevelInfo list
        =
        let mutable current = typeInfo
        let acc = ResizeArray<AccessLevelInfo> ()

        let toLevel (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : AccessLevelInfo =
            {
                Visibility = ti.TypeAttributes
                Name = ti.Name
            }

        acc.Add (toLevel current)

        while not current.DeclaringType.IsNil do
            match assembly.TypeDefs.TryGetValue current.DeclaringType with
            | true, parent ->
                acc.Add (toLevel parent)
                current <- parent
            | false, _ ->
                failwith
                    $"%s{operation}: nested type %s{current.Namespace}.%s{current.Name} has DeclaringType handle %O{current.DeclaringType} that is not present in assembly %s{assembly.Name.Name}"

        List.ofSeq acc

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "RuntimeMethodHandle_IsCAVisibleFromDecoratedType",
          "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "IsCAVisibleFromDecoratedType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              attrGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              ctorGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              sourceGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              moduleGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "BOOL", boolGenerics)) when
            attrGenerics.IsEmpty
            && ctorGenerics.IsEmpty
            && sourceGenerics.IsEmpty
            && moduleGenerics.IsEmpty
            && boolGenerics.IsEmpty
            ->
            // Mirrors CoreCLR's RuntimeMethodHandle_IsCAVisibleFromDecoratedType
            // (runtimehandles.cpp). Decides whether a custom-attribute type's
            // constructor is visible from a decorated type when reflecting custom
            // attributes; reflection filters CA instances using this check.
            let operation = "RuntimeMethodHandle.IsCAVisibleFromDecoratedType"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let attrTypeArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let attrCtorArg = instruction.Arguments.[1]
            let sourceTypeArg = instruction.Arguments.[2] |> EvalStackValue.ofCliType
            let sourceModuleArg = instruction.Arguments.[3] |> EvalStackValue.ofCliType

            // Target: the custom-attribute type and (optionally) its constructor.
            let attrTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state attrTypeArg

            let attrAssembly, attrTypeInfo, _ =
                closedTypeFromTarget operation "attribute type" state attrTarget

            // CoreCLR: if pCACtor is NULL, look up the default ctor of the target
            // type. If that lookup fails and the target is not a value type, throw
            // MissingMethodException; if it is a value type, fall back to mdPublic.
            let attrCtorId : int64 option =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation attrCtorArg

            let attrCtorMethodOpt : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option =
                match attrCtorId with
                | Some _ ->
                    // The caller supplied a non-null RuntimeMethodHandleInternal;
                    // resolve it through the registry the same way the other arms do.
                    Some (resolveMethodInfoFromHandleArg operation state attrCtorArg)
                | None ->
                    // Look up the default (parameterless instance) ctor on the
                    // attribute type. CoreCLR's MethodTable::GetDefaultConstructor
                    // walks the type's vtable looking for an instance ctor with no
                    // parameters; we approximate that with the same "name = .ctor,
                    // not static, no parameters" predicate used elsewhere
                    // (IlMachineStateExecution.fs activator paths).
                    attrTypeInfo.Methods
                    |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Parameters.IsEmpty)

            let attrCtorAttrs : MethodAttributes =
                match attrCtorMethodOpt with
                | Some m -> m.MethodAttributes
                | None ->
                    // No constructor was supplied or found.
                    if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies attrTypeInfo then
                        // CoreCLR: value types fall through with dwAttr = mdPublic, so
                        // canAccessMethod only checks class visibility.
                        MethodAttributes.Public
                    else
                        // CoreCLR throws MissingMethodException(COR_CTOR_METHOD_NAME_W).
                        // PawPrint doesn't yet have a host helper to raise that from a
                        // QCall, so surface the precise condition the same way the
                        // Activator paths do.
                        failwith
                            $"TODO: %s{operation}: attribute type %s{attrTypeInfo.Namespace}.%s{attrTypeInfo.Name} has no default constructor; CoreCLR throws MissingMethodException"

            let targetChain = buildAccessLevelChain operation attrAssembly attrTypeInfo

            // Source / accessor: the decorated type (which may be null, in which
            // case CoreCLR builds an AccessCheckContext with a NULL pDecoratedMT
            // and only the assembly is consulted) plus the assembly carried by the
            // QCallModule.
            let sourceTargetOpt =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTargetOption operation state sourceTypeArg

            let sourceModuleAssemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName operation state sourceModuleArg

            let sourceAssembly =
                state.LoadedAssembly' sourceModuleAssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: source module's assembly %s{sourceModuleAssemblyFullName} is not loaded"
                )

            let sourceChain =
                match sourceTargetOpt with
                | None ->
                    // CoreCLR: AccessCheckContext(NULL, pDecoratedMT=NULL, sourceAsm).
                    // AccessCheck.canAccessClass only iterates target.TypeChain, so the
                    // accessor's chain is unused in this slice. An empty list reflects
                    // "no decorated type", and any future widening that does consume
                    // it will fail loudly rather than silently using a default.
                    []
                | Some target ->
                    let _, sourceTypeInfo, _ =
                        closedTypeFromTarget operation "decorated type" state target

                    buildAccessLevelChain operation sourceAssembly sourceTypeInfo

            let accessor : AccessParty =
                {
                    TypeChain = sourceChain
                    Assembly = sourceAssembly.Name
                    Friends = sourceAssembly.Friends
                }

            let target : AccessParty =
                {
                    TypeChain = targetChain
                    Assembly = attrAssembly.Name
                    Friends = attrAssembly.Friends
                }

            let sameAssembly = accessor.Assembly.FullName = target.Assembly.FullName

            let visible = AccessCheck.canAccessMethod sameAssembly accessor target attrCtorAttrs

            // Interop.BOOL is int-backed with FALSE=0, TRUE=1.
            let state =
                let ret = if visible then 1 else 0
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetUtf8NameInternal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              generics) ],
          MethodReturnType.Returns (ConcretePointer (ConcreteVoid state.ConcreteTypes)) when generics.IsEmpty ->
            // CoreCLR's RuntimeMethodHandle.GetUtf8NameInternal returns a raw pointer into
            // metadata; the managed wrapper RuntimeMethodHandle.GetUtf8Name(...) wraps the
            // result in MdUtf8String, which calls string.strlen on the pointer to discover
            // the byte length. PawPrint materialises the method's metadata name as a
            // freshly-allocated null-terminated UTF-8 byte[] and returns a byref to it; the
            // managed strlen path then walks the array as expected.
            let operation = "RuntimeMethodHandle.GetUtf8NameInternal"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let namePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes methodInfo.Name state

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer namePtr) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetAttributes",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              generics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "MethodAttributes",
                                                                      retGenerics)) when
            generics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR (runtimehandles.cpp): asserts non-null and returns
            // (INT32)pMethod->GetAttrs(). The managed wrapper exposes this as the
            // MethodAttributes flags backing MethodBase.Attributes / RuntimeMethodInfo's
            // candidate filter.
            let operation = "RuntimeMethodHandle.GetAttributes"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (int32 methodInfo.MethodAttributes)))
                    ctx.Thread
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetStubIfNeededInternal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              handleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeMethodHandleInternal",
                                                                      retGenerics)) when
            handleGenerics.IsEmpty && runtimeTypeGenerics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR runtimehandles.cpp:1886-1911. Fast path that returns the same MethodDesc*
            // when no instantiating/unboxing stub is needed. Returning NULL hands off to the slow
            // QCall RuntimeMethodHandle_GetStubIfNeededSlow, which materialises an
            // InstantiatedMethodDesc via FindOrCreateAssociatedMethodDescForReflection.
            //
            // CoreCLR predicate (skipping the IsAsyncVariantMethod short-circuit since async
            // variants aren't yet modelled in PawPrint):
            //   pMethod->HasMethodInstantiation()
            //   || (!instType.IsValueType()
            //       && (!instType.HasInstantiation() || instType.IsGenericTypeDefinition()))
            let operation = "RuntimeMethodHandle.GetStubIfNeededInternal"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let methodHandleId =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null RuntimeMethodHandleInternal")

            let hasMethodInstantiation = not methodInfo.Generics.IsEmpty

            let state = IlMachineState.loadArgument ctx.Thread 1 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let returnsOriginalHandle =
                if hasMethodInstantiation then
                    true
                else
                    match target with
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                        // An open generic type definition has HasInstantiation = true and
                        // IsGenericTypeDefinition = true, so the inner disjunction is true; the
                        // overall predicate reduces to !IsValueType. (Roslyn does emit value-typed
                        // generic definitions, e.g. Nullable<>, so we cannot assume false here.)
                        let assembly =
                            state.LoadedAssembly identity.Assembly
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                            )

                        let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]

                        not (DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies typeInfo)
                    | RuntimeTypeHandleTarget.Closed handle ->
                        // A `Closed` handle is a fully-bound type. IsGenericTypeDefinition is
                        // therefore false; HasInstantiation matches whether the concrete type has
                        // generic arguments. Structural shapes (array/byref/pointer/fnptr) have no
                        // nominal definition and are reference-shaped from this predicate's
                        // perspective: !IsValueType && (!HasInstantiation || false) = true.
                        match handle with
                        | ConcreteTypeHandle.Concrete _ ->
                            match AllConcreteTypes.lookup handle state.ConcreteTypes with
                            | None ->
                                failwith
                                    $"%s{operation}: closed RuntimeTypeHandle %O{handle} not found in ConcreteTypes"
                            | Some concreteType ->
                                let assembly =
                                    state.LoadedAssembly concreteType.Assembly
                                    |> Option.defaultWith (fun () ->
                                        failwith
                                            $"%s{operation}: assembly %s{concreteType.Assembly.FullName} for closed RuntimeTypeHandle is not loaded"
                                    )

                                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

                                let isValueType =
                                    DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies typeInfo

                                let hasInstantiation = not concreteType.Generics.IsEmpty

                                not isValueType && not hasInstantiation
                        | ConcreteTypeHandle.Byref _
                        | ConcreteTypeHandle.Pointer _
                        | ConcreteTypeHandle.FunctionPointer _
                        | ConcreteTypeHandle.OneDimArrayZero _
                        | ConcreteTypeHandle.Array _ -> true
                    | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                        failwith
                            $"%s{operation}: TODO: not implemented for type-generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; reflecting MethodInfo through a TypeVar declaring type is uncommon and would need IsValueType modelling against the parameter's struct constraint"
                    | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                        failwith
                            $"%s{operation}: TODO: not implemented for method-generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            let returnValue =
                if returnsOriginalHandle then
                    MethodHandleRegistry.internalHandleFromId ctx.BaseClassTypes state.ConcreteTypes methodHandleId
                else
                    MethodHandleRegistry.zeroInternalHandle ctx.BaseClassTypes state.ConcreteTypes

            let state =
                IlMachineState.pushToEvalStack (CliType.ValueType returnValue) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetLoaderAllocatorInternal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              handleGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "LoaderAllocator",
                                                                      retGenerics)) when
            handleGenerics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR runtimehandles.cpp:2148 returns
            //   pMethod->GetLoaderAllocator()->GetExposedObject()
            // and `GetExposedObject` (loaderallocator.inl:11) reads
            // `m_hLoaderAllocatorObjectHandle`, which is only populated by
            // `LoaderAllocator::SetupManagedTracking`. That function is only invoked
            // from `Assembly::Create` and `AssemblyNative::CreateAssemblyLoadContext`
            // for *collectible* loader allocators (assembly.cpp:468). Non-collectible
            // assemblies — i.e. everything PawPrint currently loads — leave the handle
            // null, so the FCall returns null and the BCL takes the static-cache path
            // (e.g. `RuntimeType.RuntimeTypeCache.GetGenericMethodInfo` switches to
            // `s_methodInstantiations`). Allocating a fresh `LoaderAllocator` here would
            // route those caches into a per-call object and silently break
            // canonicalization of reflected generic methods.
            //
            // When collectible AssemblyLoadContexts get modelled, this arm should look
            // up the method's LoaderAllocator identity and return the corresponding
            // exposed object.
            let operation = "RuntimeMethodHandle.GetLoaderAllocatorInternal"

            // CoreCLR asserts non-null on the FCall entry; surface the same precondition.
            let _ : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let state = IlMachineState.pushToEvalStack (CliType.ObjectRef None) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None
