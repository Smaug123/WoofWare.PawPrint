namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineMemberResolution =
    let resolveMemberWithGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (typeGenerics : ImmutableArray<TypeDefn>)
        (methodGenerics : ImmutableArray<TypeDefn>)
        (genericMethodTypeArgs : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]
        let sourceAssembly = assy

        let memberName : string = assy.Strings mem.Name

        let state, assy, targetType, extractedTypeArgs =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                // TODO: generics here?
                let state, assy, targetType =
                    IlMachineTypeResolution.resolveType loggerFactory parent ImmutableArray.Empty assy state

                state, assy, targetType, ImmutableArray.Empty // No type args from TypeReference
            | MetadataToken.TypeSpecification parent ->
                let state, assy, targetType =
                    IlMachineTypeResolution.resolveTypeFromSpec
                        loggerFactory
                        baseClassTypes
                        parent
                        assy
                        typeGenerics
                        methodGenerics
                        state

                // Extract type arguments from the resolved type
                let extractedTypeArgs = targetType.Generics

                state, assy, targetType, extractedTypeArgs
            | parent -> failwith $"Unexpected: {parent}"

        let state, concreteExtractedTypeArgs =
            ((state, ImmutableArray.CreateBuilder ()), extractedTypeArgs)
            ||> Seq.fold (fun (state, acc) ty ->
                // TODO: generics?
                let state, t =
                    IlMachineTypeResolution.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        targetType.Assembly
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty

                acc.Add t
                state, acc
            )
            |> Tuple.rmap (fun x -> x.ToImmutable ())

        match mem.Signature with
        | MemberSignature.Field fieldSig ->
            // Concretize the field signature from the member reference
            let state, concreteFieldSig =
                IlMachineTypeResolution.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    sourceAssembly.Name
                    concreteExtractedTypeArgs
                    ImmutableArray.Empty
                    fieldSig

            // Find matching fields by comparing concretized signatures
            let state, availableFields =
                ((state, []), targetType.Fields)
                ||> List.fold (fun (state, acc) fi ->
                    if fi.Name <> memberName then
                        state, acc
                    else
                        // Concretize the field's signature for comparison
                        let state, fieldSigConcrete =
                            IlMachineTypeResolution.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                assy.Name
                                concreteExtractedTypeArgs
                                ImmutableArray.Empty
                                fi.Signature

                        if fieldSigConcrete = concreteFieldSig then
                            state, fi :: acc
                        else
                            state, acc
                )

            let field =
                match availableFields with
                | [] ->
                    failwith
                        $"Could not find field member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> FieldInfo.mapTypeGenerics (fun _ (par, md) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for {targetType.Namespace}.{targetType.Name}'s field {memberName}!"

            state, assy.Name, Choice2Of2 field, extractedTypeArgs

        | MemberSignature.Method memberSig ->
            let availableMethods =
                targetType.Methods |> List.filter (fun mi -> mi.Name = memberName)

            let state, memberSig =
                memberSig
                |> TypeMethodSignature.map
                    state
                    (fun state ty ->
                        IlMachineTypeResolution.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            sourceAssembly.Name
                            concreteExtractedTypeArgs
                            genericMethodTypeArgs
                            ty
                    )

            let state, availableMethods =
                ((state, []), availableMethods)
                ||> List.fold (fun (state, acc) meth ->
                    // A candidate overload whose generic arity doesn't match the call site
                    // cannot be the target. Reject it up front: concretising its signature
                    // would otherwise index past the end of `genericMethodTypeArgs` (which
                    // was sized for `memberSig`) whenever the candidate signature mentions
                    // a `GenericMethodParameter`. See e.g. Interlocked.CompareExchange,
                    // where the generic `<T>` overload sits alongside type-specific ones.
                    if meth.Signature.GenericParameterCount <> memberSig.GenericParameterCount then
                        state, acc
                    else
                        let state, methSig =
                            meth.Signature
                            |> TypeMethodSignature.map
                                state
                                (fun state ty ->
                                    IlMachineTypeResolution.concretizeType
                                        loggerFactory
                                        baseClassTypes
                                        state
                                        assy.Name
                                        concreteExtractedTypeArgs
                                        genericMethodTypeArgs
                                        ty
                                )

                        if methSig = memberSig then
                            state, meth :: acc
                        else
                            state, acc
                )

            let method =
                match availableMethods with
                | [] ->
                    failwith
                        $"Could not find member {memberName} with the right signature {memberSig} on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

            state, assy.Name, Choice1Of2 method, extractedTypeArgs

    let resolveMember
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (genericMethodTypeArgs : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod

        let toTypeDefn (handle : ConcreteTypeHandle) : TypeDefn =
            Concretization.concreteHandleToTypeDefn baseClassTypes handle state.ConcreteTypes state._LoadedAssemblies

        let typeGenerics =
            executing.DeclaringType.Generics
            |> Seq.map toTypeDefn
            |> ImmutableArray.CreateRange

        let methodGenerics =
            executing.Generics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        resolveMemberWithGenerics
            loggerFactory
            baseClassTypes
            currentThread
            assy
            typeGenerics
            methodGenerics
            genericMethodTypeArgs
            m
            state
