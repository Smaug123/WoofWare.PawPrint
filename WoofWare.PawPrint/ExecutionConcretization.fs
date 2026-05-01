namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module ExecutionConcretization =

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
                (IlMachineTypeResolution.loader loggerFactory state)
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
                    concretizedMethod.DeclaringType.Identity
                    concretizedMethod.DeclaringType.Generics
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
                        IlMachineTypeResolution.concretizeType
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
        // Use type generics from metadata if available, otherwise fall back to current execution context
        let typeGenerics =
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
                            (IlMachineTypeResolution.loader loggerFactory state)
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
                currentMethod.DeclaringType.Generics, state

        let typeGenerics, state = typeGenerics

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

        let contextTypeGenerics = currentMethod.DeclaringType.Generics

        let contextMethodGenerics = currentMethod.Generics |> ImmutableArray.CreateRange

        let loadedAssemblies =
            Concretization.ensureTypeDefinitionBaseAssembliesLoaded
                (IlMachineTypeResolution.loader loggerFactory state)
                state._LoadedAssemblies
                field.DeclaringType.Assembly
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
                let assy = state._LoadedAssemblies.[field.DeclaringType.Assembly.FullName]
                let typeDef = assy.TypeDefs.[field.DeclaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeDef

                TypeDefn.FromDefinition (field.DeclaringType.Identity, signatureTypeKind)
            else
                // Generic type - the field's declaring type already has the generic arguments
                let assy = state._LoadedAssemblies.[field.DeclaringType.Assembly.FullName]
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
                (IlMachineTypeResolution.loader loggerFactory state)
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
