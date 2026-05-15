namespace WoofWare.PawPrint

open System.Collections.Immutable

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

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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
            // CoreCLR runtimehandles.cpp:2148: returns
            //   pMethod->GetLoaderAllocator()->GetExposedObject()
            // The managed `LoaderAllocator` object exists solely as a GC keepalive: the
            // sole consumer is `RuntimeMethodInfoStub.m_keepalive`, a write-only field.
            // No managed code ever reads the object's state back through that reference,
            // so per-call allocation of a fresh `LoaderAllocator` preserves the contract.
            // We skip running the type's parameterless constructor since the side
            // effects (allocating `LoaderAllocatorScout` and an `object[5]`) only matter
            // for the unmanaged-finalizer dance, which we don't model. If we ever add
            // AssemblyLoadContext modelling, this allocation moves to per-ALC identity.
            let operation = "RuntimeMethodHandle.GetLoaderAllocatorInternal"

            // CoreCLR asserts non-null on the FCall entry; surface the same precondition.
            let _ : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let typeInfo = ctx.BaseClassTypes.LoaderAllocator

            let stk =
                DumpedAssembly.signatureTypeKind ctx.BaseClassTypes state._LoadedAssemblies typeInfo

            let state, typeHandle =
                IlMachineState.concretizeType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    ctx.BaseClassTypes.Corelib.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (typeInfo.Identity, stk))

            let state, allFields =
                IlMachineState.collectAllInstanceFields ctx.LoggerFactory ctx.BaseClassTypes state typeHandle

            let fields =
                CliValueType.OfFields
                    ctx.BaseClassTypes
                    state.ConcreteTypes
                    typeHandle
                    typeInfo.Layout
                    (CharSetMetadata.ofTypeAttributes typeInfo.TypeAttributes)
                    allFields

            let addr, state = IlMachineState.allocateManagedObject typeHandle fields state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
