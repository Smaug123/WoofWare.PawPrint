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
        let thread = ctx.Thread
        let logger = ctx.Logger

        let method, methodGenerics =
            match metadataToken with
            | MetadataToken.MethodDef handle ->
                let method =
                    activeAssy.Methods.[handle]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                method, None
            | MetadataToken.MethodSpecification h ->
                let spec = activeAssy.MethodSpecs.[h]

                match spec.Method with
                | MetadataToken.MethodDef token ->
                    let method =
                        activeAssy.Methods.[token]
                        |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                    method, Some spec.Signature
                | k -> failwith $"Unrecognised MethodSpecification kind: %O{k}"
            | t -> failwith $"Unexpectedly asked to Ldftn a non-method: {t}"

        let state, concretizedMethod, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                method
                methodGenerics
                None
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
                |> CliValueType.OfFields baseClassTypes state.ConcreteTypes runtimeTypeHandleHandle Layout.Default

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
