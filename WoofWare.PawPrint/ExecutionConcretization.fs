namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module ExecutionConcretization =

    /// <summary>
    /// The method a <c>DynamicMethodHandle</c> names, in the form a frame can be pushed for.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Built here rather than read out of a table because there is nothing to read: what
    /// <c>ModuleHandle_GetDynamicMethod</c> recorded is a name, a signature *blob*, and an IL body
    /// whose local types are <see cref="TypeDefn"/>s. Turning those into the
    /// <c>ConcreteTypeHandle</c>-flavoured <see cref="MethodInfo"/> the interpreter runs on is
    /// this function's whole job, and it is the same decode-then-concretise pair
    /// <c>NativeDelegate</c> performs when it decides whether a delegate may bind at all.
    /// </para>
    /// <para>
    /// Built at *invocation* rather than at bind time, deliberately. CoreCLR reads a dynamic
    /// method's <c>initLocals</c> once, at first JIT, and latches it; PawPrint records it when the
    /// method is minted, which is earlier still, and <c>DynamicMethodBody</c> refuses the one
    /// instruction that could observe the difference. Building here keeps the shape that a future
    /// first-execution latch would need, where a bind-time build would have to be undone first.
    /// </para>
    /// <para>
    /// No generics, and no `this`: a <c>DynamicMethod</c> is always static and never generic
    /// (its constructors set <c>mdStatic</c> unconditionally and offer no way to declare a type
    /// parameter), so both instantiations are empty and the signature is concretised in an empty
    /// generic context.
    /// </para>
    /// </remarks>
    let concretizeDynamicMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (handle : DynamicMethodHandle)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let definition =
            MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: %O{handle} is not registered in the method-handle registry"
            )

        let scopeAssemblyFullName = definition.GetScopeAssemblyFullName ()

        let scopeAssembly =
            state.LoadedAssembly' scopeAssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: the scope assembly %s{scopeAssemblyFullName} is not loaded"
            )

        let concretise (state : IlMachineState) (typeDefn : TypeDefn) : IlMachineState * ConcreteTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                scopeAssembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                typeDefn

        let state, signature =
            MethodSignatureDecoding.decode
                scopeAssembly.Name
                (scopeAssembly.PeReader.GetMetadataReader ())
                (definition.GetSignature () |> Seq.toArray)
            |> TypeMethodSignature.make (fun ty ->
                match ty with
                | TypeDefn.Void -> MethodReturnType.Void
                | ret -> MethodReturnType.Returns ret
            )
            |> TypeMethodSignature.map state concretise

        let body = definition.GetBody ()

        // The locals were decoded at mint time by `LocalSignatureDecoding`, in the same token
        // universe as the signature; concretise them the same way. `None` means the method
        // declared none, which is distinct from declaring zero of them only in that the frame has
        // no locals array to build.
        let state, localVars =
            match body.LocalVars with
            | None -> state, None
            | Some vars ->
                let mutable state = state
                let handles = ImmutableArray.CreateBuilder vars.Length

                for var in vars do
                    let newState, handle = concretise state var
                    state <- newState
                    handles.Add handle

                state, Some (handles.ToImmutable ())

        let core =
            {
                Owner = MethodOwner.DynamicMethodsClass scopeAssembly.Name
                Name = definition.GetName ()
                Body = MethodBody.Il (MethodInstructions.setLocalVars localVars body)
                Generics = ImmutableArray.Empty
                Signature = signature
                IsStatic = true
            }

        state, MethodInfo.Synthesised (core, SynthesisedMethod.DynamicMethod handle)

    let concretizeMethodWithAllGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        // Now concretize the entire method
        let concretizedMethod, newConcreteTypes, newAssemblies =
            Concretization.concretizeMethod
                state.ConcreteTypes
                (IlMachineState.loader loggerFactory state)
                state._LoadedAssemblies
                baseClassTypes
                methodToCall
                typeGenerics
                methodGenerics

        let state =
            { state with
                ConcreteTypes = newConcreteTypes
                _LoadedAssemblies = newAssemblies
            }

        // Get the handle for the declaring type
        let declaringTypeHandle =
            match
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    concretizedMethod.RequiredDeclaringType.Identity
                    concretizedMethod.DeclaringTypeGenerics
            with
            | Some handle -> handle
            | None -> failwith "Concretized method's declaring type not found in ConcreteTypes"

        state, concretizedMethod, declaringTypeHandle

    let concretizeMethodWithTypeGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : TypeDefn ImmutableArray option)
        (callingAssembly : AssemblyName)
        (currentExecutingMethodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =

        // Concretize method generics if any
        let state, concretizedMethodGenerics =
            match methodGenerics with
            | None -> state, ImmutableArray.Empty
            | Some generics ->
                let handles = ImmutableArray.CreateBuilder ()
                let mutable state = state

                for i = 0 to generics.Length - 1 do
                    let state2, handle =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            callingAssembly
                            typeGenerics
                            currentExecutingMethodGenerics
                            generics.[i]

                    state <- state2
                    handles.Add handle

                state, handles.ToImmutable ()

        // Now concretize the entire method
        concretizeMethodWithAllGenerics
            loggerFactory
            baseClassTypes
            typeGenerics
            methodToCall
            concretizedMethodGenerics
            state

    /// Resolve the target method's declaring-type generics from the IL metadata, falling back to
    /// the current frame when none were supplied. Returned handles are already concretized.
    let private resolveTargetTypeGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (typeArgsFromMetadata : TypeDefn ImmutableArray option)
        (state : IlMachineState)
        : ImmutableArray<ConcreteTypeHandle> * IlMachineState
        =
        match typeArgsFromMetadata with
        | Some args when not args.IsEmpty ->
            // We have concrete type arguments from the IL metadata
            // Need to concretize them to ConcreteTypeHandle first
            let handles = ImmutableArray.CreateBuilder args.Length
            let mutable state = state

            for i = 0 to args.Length - 1 do
                let ctx =
                    {
                        TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                        TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                        TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
                    }

                let handle, newCtx =
                    TypeConcretization.concretizeType
                        ctx
                        (IlMachineState.loader loggerFactory state)
                        (state.ActiveAssembly thread).Name
                        ImmutableArray.Empty // No type generics for the concretization context
                        ImmutableArray.Empty // No method generics for the concretization context
                        args.[i]

                handles.Add handle

                state <-
                    { state with
                        ConcreteTypes = newCtx.ConcreteTypes
                        _LoadedAssemblies = newCtx.LoadedAssemblies
                    }

            handles.ToImmutable (), state
        | _ ->
            // Fall back to current execution context
            let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod
            currentMethod.DeclaringTypeGenerics, state

    /// Returns also the declaring type.
    let concretizeMethodForExecution
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : TypeDefn ImmutableArray option)
        (typeArgsFromMetadata : TypeDefn ImmutableArray option)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        let typeGenerics, state =
            resolveTargetTypeGenerics loggerFactory baseClassTypes thread typeArgsFromMetadata state

        let callingAssembly = (state.ActiveAssembly thread).Name
        let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

        concretizeMethodWithTypeGenerics
            loggerFactory
            baseClassTypes
            typeGenerics
            methodToCall
            methodGenerics
            callingAssembly
            currentMethod.Generics
            state

    /// Variant of `concretizeMethodForExecution` for callers that have already concretized the
    /// method's generic args against the current frame's context (e.g. MethodSpec dispatch where
    /// `spec.Signature` has been substituted to pick the right MemberReference overload). Avoids
    /// re-substituting those args against the target type's generics, which would be the wrong
    /// context whenever `spec.Signature` references the caller's class or method generics.
    let concretizeMethodForExecutionWithConcreteMethodGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (typeArgsFromMetadata : TypeDefn ImmutableArray option)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        let typeGenerics, state =
            resolveTargetTypeGenerics loggerFactory baseClassTypes thread typeArgsFromMetadata state

        concretizeMethodWithAllGenerics loggerFactory baseClassTypes typeGenerics methodToCall methodGenerics state

    /// Concretize the declaring type of a resolved field token, substituting any generic
    /// parameters it still mentions against the supplied context.
    ///
    /// This is the handle that keys every `FieldId` an `ldfld`/`stfld`/`ldflda` site builds, and
    /// it must equal the handle `IlMachineRuntimeMetadata.collectAllInstanceFields` used when the
    /// receiver's storage was laid out -- otherwise the field cannot be found. It is factored out
    /// of `concretizeFieldForExecution` so that agreement can be tested directly against the
    /// production code rather than against a re-implementation of it, which would be free to drift
    /// away from this exactly when it mattered. See `TestFieldIdAgreement`.
    ///
    /// Returns the declaring type's handle and its concretized generic arguments.
    let concretizeFieldDeclaringType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (contextMethodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (field : WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle * ImmutableArray<ConcreteTypeHandle>
        =
        let loadedAssemblies =
            Concretization.ensureTypeDefinitionBaseAssembliesLoaded
                (IlMachineState.loader loggerFactory state)
                state._LoadedAssemblies
                state._LoadedAssemblies.[field.DeclaringType.Assembly]
                field.DeclaringType.Definition.Get

        let state =
            { state with
                _LoadedAssemblies = loadedAssemblies
            }

        // Create a concretization context
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        // Create a TypeDefn for the field's declaring type
        let declaringTypeDefn =
            if field.DeclaringType.Generics.IsEmpty then
                // Non-generic type - determine the SignatureTypeKind
                let assy = state._LoadedAssemblies.[field.DeclaringType.Assembly]
                let typeDef = assy.TypeDefs.[field.DeclaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeDef

                TypeDefn.FromDefinition (field.DeclaringType.Identity, signatureTypeKind)
            else
                // Generic type - the field's declaring type already has the generic arguments
                let assy = state._LoadedAssemblies.[field.DeclaringType.Assembly]
                let typeDef = assy.TypeDefs.[field.DeclaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeDef

                let baseType =
                    TypeDefn.FromDefinition (field.DeclaringType.Identity, signatureTypeKind)

                // Use the actual type arguments from the field's declaring type
                // These should already be correctly instantiated (e.g., GenericMethodParameter 0 for Array.Empty<T>)
                let genericArgs = field.DeclaringType.Generics

                TypeDefn.GenericInstantiation (baseType, genericArgs)

        // Concretize the declaring type
        let declaringHandle, newCtx =
            TypeConcretization.concretizeType
                ctx
                (IlMachineState.loader loggerFactory state)
                field.DeclaringType.Assembly
                contextTypeGenerics
                contextMethodGenerics
                declaringTypeDefn

        let state =
            { state with
                ConcreteTypes = newCtx.ConcreteTypes
                _LoadedAssemblies = newCtx.LoadedAssemblies
            }

        // Get the concretized type's generics
        let concretizedType =
            AllConcreteTypes.lookup declaringHandle state.ConcreteTypes |> Option.get

        let typeGenerics = concretizedType.Generics

        state, declaringHandle, typeGenerics

    let concretizeFieldForExecution
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (field : WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle * ImmutableArray<ConcreteTypeHandle>
        =
        // Get type and method generics from current execution context
        let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

        let contextTypeGenerics = currentMethod.DeclaringTypeGenerics

        let contextMethodGenerics = currentMethod.Generics |> ImmutableArray.CreateRange

        concretizeFieldDeclaringType loggerFactory baseClassTypes contextTypeGenerics contextMethodGenerics field state
