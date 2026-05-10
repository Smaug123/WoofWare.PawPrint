namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataTokenOps =
    let executeLdftn (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread
        let logger = ctx.Logger

        // Resolution mirrors `UnaryMetadataCallOps.executeCall`: in-assembly methods arrive as
        // MethodDef (optionally wrapped in a MethodSpec for generic methods), cross-assembly
        // methods arrive as MemberReference (optionally MethodSpec-wrapped). MemberReference
        // resolution must thread the extracted declaring-type generics back to
        // `concretizeMethodForExecution`, otherwise generic types defined in another assembly
        // would lose their instantiation when projected onto the eval-stack function pointer.
        let state, method, methodGenerics, typeArgsFromMetadata =
            match metadataToken with
            | MetadataToken.MethodDef handle ->
                let method =
                    activeAssy.Methods.[handle]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                state, method, None, None
            | MetadataToken.MemberReference h ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember
                        loggerFactory
                        baseClassTypes
                        thread
                        activeAssy
                        ImmutableArray.Empty
                        h
                        state

                match method with
                | Choice2Of2 _field -> failwith "tried to Ldftn a field"
                | Choice1Of2 method -> state, method, None, Some extractedTypeArgs
            | MetadataToken.MethodSpecification h ->
                let spec = activeAssy.MethodSpecs.[h]

                match spec.Method with
                | MetadataToken.MethodDef token ->
                    let method =
                        activeAssy.Methods.[token]
                        |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                    state, method, Some spec.Signature, None
                | MetadataToken.MemberReference ref ->
                    // Concretize the spec's generic method args against the current frame's
                    // generic context so `resolveMember` can pick the right overload — the
                    // member signature may reference these method type parameters by index.
                    let state, methodGenerics =
                        ((state, []), spec.Signature)
                        ||> Seq.fold (fun (state, acc) typeDefn ->
                            let state, concreteType =
                                IlMachineState.concretizeType
                                    loggerFactory
                                    baseClassTypes
                                    state
                                    activeAssy.Name
                                    currentMethod.DeclaringType.Generics
                                    currentMethod.Generics
                                    typeDefn

                            state, concreteType :: acc
                        )

                    let methodGenerics = List.rev methodGenerics |> ImmutableArray.CreateRange

                    let state, _, method, extractedTypeArgs =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            methodGenerics
                            ref
                            state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Ldftn a field"
                    | Choice1Of2 method -> state, method, Some spec.Signature, Some extractedTypeArgs
                | k -> failwith $"Unrecognised MethodSpecification kind for Ldftn: %O{k}"
            | t -> failwith $"Unexpectedly asked to Ldftn a non-method: {t}"

        let state, concretizedMethod, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                method
                methodGenerics
                typeArgsFromMetadata
                state

        logger.LogDebug (
            "Pushed pointer to function {LdFtnAssembly}.{LdFtnType}.{LdFtnMethodName}",
            method.DeclaringType.Assembly.Name,
            method.DeclaringType.Name,
            method.Name
        )

        state
        |> IlMachineState.pushToEvalStack'
            (EvalStackValue.NativeInt (NativeIntSource.FunctionPointer concretizedMethod))
            thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeLdtoken (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Helper function to handle type tokens and create RuntimeTypeHandle
        let handleTypeToken
            (declaringAssembly : DumpedAssembly)
            (allowOpenGenericDefinition : bool)
            (typeDefn : TypeDefn)
            (state : IlMachineState)
            : IlMachineState
            =
            let ty = baseClassTypes.RuntimeTypeHandle
            let field = ty.Fields |> List.exactlyOne

            if field.Name <> "m_type" then
                failwith $"unexpected field name ${field.Name} for BCL type RuntimeTypeHandle"

            let methodGenerics = currentMethod.Generics
            let typeGenerics = currentMethod.DeclaringType.Generics

            let state, target =
                IlMachineState.runtimeTypeHandleTargetForTypeToken
                    loggerFactory
                    baseClassTypes
                    declaringAssembly
                    allowOpenGenericDefinition
                    typeGenerics
                    methodGenerics
                    typeDefn
                    state

            let alloc, state =
                IlMachineState.getOrAllocateType loggerFactory baseClassTypes target state

            let state, runtimeTypeHandleHandle =
                DumpedAssembly.typeInfoToTypeDefn'
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.RuntimeTypeHandle
                |> IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            let vt =
                // https://github.com/dotnet/runtime/blob/2b21c73fa2c32fa0195e4a411a435dda185efd08/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L92
                let mTypeField =
                    FieldIdentity.requiredOwnInstanceField baseClassTypes.RuntimeTypeHandle "m_type"

                FieldIdentity.cliField
                    runtimeTypeHandleHandle
                    mTypeField
                    (CliType.ObjectRef (Some alloc))
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.RuntimeType)
                |> List.singleton
                |> CliValueType.OfFields
                    baseClassTypes
                    state.ConcreteTypes
                    runtimeTypeHandleHandle
                    Layout.Default
                    (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeTypeHandle.TypeAttributes)

            IlMachineState.pushToEvalStack (CliType.ValueType vt) thread state

        let state =
            match metadataToken with
            | MetadataToken.FieldDefinition h ->
                // TODO: how do we know what concrete type this is a field on?
                let runtimeFieldHandle, state =
                    IlMachineState.getOrAllocateField loggerFactory baseClassTypes activeAssy.Name h state

                IlMachineState.pushToEvalStack runtimeFieldHandle thread state
            | MetadataToken.MethodDef h ->
                let method =
                    activeAssy.Methods.[h]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                if not method.DeclaringType.Generics.IsEmpty then
                    failwith
                        $"TODO: ldtoken MethodDef for methods on generic declaring types requires open generic RuntimeMethodHandle support; got %O{method}"

                if not method.Generics.IsEmpty then
                    failwith
                        $"TODO: ldtoken MethodDef for generic methods requires open generic RuntimeMethodHandle support; got %O{method}"

                let state, concretizedMethod, _declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodForExecution
                        loggerFactory
                        baseClassTypes
                        thread
                        method
                        None
                        None
                        state

                let runtimeMethodHandle, state =
                    IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretizedMethod state

                IlMachineState.pushToEvalStack runtimeMethodHandle thread state
            | MetadataToken.TypeSpecification h ->
                // Use the raw TypeSpec signature directly, bypassing the lossy
                // resolveTypeFromDefn → TypeInfo → typeInfoToTypeDefn round-trip.
                // TypeInfo cannot represent array/pointer/byref wrappers, so the
                // round-trip would collapse e.g. typeof(X[]) to typeof(X).
                let sign = activeAssy.TypeSpecs.[h].Signature
                handleTypeToken activeAssy false sign state
            | MetadataToken.TypeReference h ->
                let typeGenerics = currentMethod.DeclaringType.Generics

                let state, typeDefn, assy =
                    IlMachineState.lookupTypeRef loggerFactory baseClassTypes state activeAssy typeGenerics h

                handleTypeToken assy true typeDefn state
            | MetadataToken.TypeDefinition h ->
                let state, typeDefn =
                    IlMachineState.lookupTypeDefn baseClassTypes state activeAssy h

                handleTypeToken activeAssy true typeDefn state
            | _ -> failwith $"Unexpected metadata token %O{metadataToken} in LdToken"

        state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed
