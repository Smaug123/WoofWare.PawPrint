namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineStateExecution =
    let getTypeOfObj
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (esv : EvalStackValue)
        : IlMachineState * ConcreteTypeHandle
        =
        match esv with
        | EvalStackValue.Int32 _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int32
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.Int64 _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int64
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Double
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.ManagedPointer _ -> failwith "cannot get type of managed pointer target"
        | EvalStackValue.ObjectRef addr ->
            let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
            state, concreteType
        | EvalStackValue.NullObjectRef -> failwith "TODO: throw NullReferenceException"
        | EvalStackValue.UserDefinedValueType tuples -> failwith "todo"

    let isAssignableFrom
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (objToCast : ConcreteTypeHandle)
        (possibleTargetType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state objToCast possibleTargetType

    /// An SZ array implicitly implements five generic interfaces (see
    /// `BaseClassTypes.IsImplicitInterfaceOfSzArray`), but nothing in the metadata says so:
    /// `System.Array` does not list them among its implemented interfaces, and `T[]` has no
    /// TypeDef row of its own to carry a MethodImpl. The runtime supplies the bodies instead,
    /// from the corelib-internal shim `System.SZArrayHelper`, whose methods take the array
    /// itself as `this` and immediately re-view it via `Unsafe.As<T[]>(this)`. CoreCLR does
    /// this in `MethodTable::FindDispatchImpl` (`src/coreclr/vm/methodtable.cpp`) →
    /// `GetActualImplementationForArrayGenericIListOrIReadOnlyListMethod`
    /// (`src/coreclr/vm/array.cpp`).
    ///
    /// Returns `None` when the (receiver, interface) pair is not in the carve-out, leaving
    /// ordinary resolution to run.
    let private tryResolveSzArrayImplicitInterfaceMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (state : IlMachineState)
        : (IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) option
        =
        match dispatchTypeHandle with
        // Multi-dimensional arrays deliberately do *not* participate: CoreCLR's
        // `IsImplicitInterfaceOfSZArray` is reached only for SZ arrays, and
        // `isConcreteTypeAssignableTo` already refuses the corresponding cast.
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None
        | ConcreteTypeHandle.OneDimArrayZero _ ->

        if not (baseClassTypes.IsImplicitInterfaceOfSzArray methodToCall.RequiredDeclaringType.Identity) then
            None
        else

        // `theT` is the *interface's* type argument, not the array's element type
        // (`methodtable.cpp`: `TypeHandle theT = pIfcMT->GetInstantiation()[0];`). Under
        // covariance those differ — `((ICollection<object>) new string[3])` dispatches to
        // `get_Count<object>` over a `string[]`.
        //
        // That is safe even for the mutating slots, because the store check does not consult
        // this `T`: `SZArrayHelper.set_Item<T>` bottoms out in a `stelem`, and
        // `UnaryMetadataArrayOps.executeStelem` uses the token-resolved element type only to
        // pick a coercion target, delegating the ArrayTypeMismatchException decision to
        // `checkArrayStoreVariance`, which reads the array's real allocation-time element type
        // and the stored value's real runtime type. So
        // `((IList<object>) new string[3])[0] = new object()` still throws.
        //
        let theT =
            match Seq.toList methodToCall.DeclaringTypeGenerics with
            | [ t ] -> t
            | generics ->
                failwith
                    $"SZ-array implicit interface %s{MethodOwner.describe methodToCall.Owner} should have exactly one generic argument, got %i{List.length generics}"

        // CoreCLR maps interface slot → shim method by slot arithmetic, but asserts the result
        // equals `MemberLoader::FindMethodByName(g_pSZArrayHelperClass, pItfcMeth->GetName())`.
        // `SZArrayHelper`'s method names are pairwise distinct, so name lookup is equivalent.
        let implementation =
            baseClassTypes.SZArrayHelper.Methods
            |> List.filter (fun meth -> meth.Name = methodToCall.Name)

        let implementation =
            match implementation with
            | [ impl ] -> impl
            | [] ->
                failwith
                    $"System.SZArrayHelper has no method named %s{methodToCall.Name}, needed to dispatch %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} on an SZ-array receiver"
            | _ ->
                failwith
                    $"System.SZArrayHelper has multiple methods named %s{methodToCall.Name}; the SZ-array dispatch carve-out relies on shim method names being unique"

        // Every shim method is a one-generic-parameter instance method whose parameters are the
        // interface method's with `T` substituted, so these must line up. If a future corelib
        // breaks that, fail here rather than silently building a mis-shaped frame.
        if implementation.Signature.GenericParameterCount <> 1 then
            failwith
                $"System.SZArrayHelper::%s{implementation.Name} should take exactly one generic parameter, got %i{implementation.Signature.GenericParameterCount}"

        if
            implementation.Signature.RequiredParameterCount
            <> methodToCall.Signature.RequiredParameterCount
        then
            failwith
                $"System.SZArrayHelper::%s{implementation.Name} takes %i{implementation.Signature.RequiredParameterCount} parameters but the interface slot %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} takes %i{methodToCall.Signature.RequiredParameterCount}"

        if implementation.IsStatic then
            failwith
                $"System.SZArrayHelper::%s{implementation.Name} should be an instance method; the SZ-array receiver is passed as its `this`"

        // CoreCLR canonicalises a reference-type `theT` to `System.Object` on every slot except
        // `GetEnumerator` (`array.cpp`, gated on `startingMethod[inheritanceDepth]`, i.e. on the
        // interface rather than the individual method — `GetEnumerator` is always reached
        // through `IEnumerable`1`, so preserving it there and canonicalising the other four
        // interfaces is the same rule).
        //
        // Its comment calls this an optimisation ("causes fewer methods to be instantiated"),
        // but it is *observable*, so we must reproduce it rather than keep the more precise
        // instantiation. `Contains`/`IndexOf` bottom out in `EqualityComparer<T>.Default`:
        // `EqualityComparer<object>` dispatches through the virtual `object.Equals(object)`,
        // whereas `EqualityComparer<B>` for a `B : IEquatable<B>` dispatches through
        // `IEquatable<B>.Equals(B)`. A type implementing the two inconsistently therefore gives
        // different answers depending on the instantiation; see
        // `sourcesPure/ArrayInterfaceEqualityComparer.cs`, which fails against the real runtime
        // without this.
        //
        // `GetEnumerator` is the exception because the enumerator it returns is itself typed:
        // `IEnumerable<B>.GetEnumerator()` must yield an `IEnumerator<B>`, not an
        // `IEnumerator<object>`.
        let isReferenceType (handle : ConcreteTypeHandle) : bool =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> true
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> false
            | ConcreteTypeHandle.Concrete _ ->
                match IlMachineState.tryGetConcreteTypeInfo state handle with
                | Some (_, typeInfo) -> DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
                | None ->
                    failwith
                        $"SZ-array interface dispatch: type argument %O{handle} of %s{MethodOwner.describe methodToCall.Owner} has no TypeDef row"

        let dispatchThroughEnumerable =
            methodToCall.RequiredDeclaringType.Identity = baseClassTypes.IEnumerableGeneric.Identity

        let state, instantiation =
            if dispatchThroughEnumerable || not (isReferenceType theT) then
                state, theT
            else
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Object
                |> IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

        // `this` is the array, not an SZArrayHelper — exactly the lie CoreCLR tells (see the
        // "! Warning: \"this\" is an array, not an SZArrayHelper" comments in
        // `Array.CoreCLR.cs`). It survives our calling convention because `SZArrayHelper` is a
        // reference type, so `callMethod`'s `thisArgCoercionTarget` yields `CliType.ObjectRef`,
        // whose coercion passes the object reference through without a type check.
        let state, meth, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                implementation
                (ImmutableArray.Create instantiation)
                state

        Some (state, meth)

    /// Resolve one entry of `ownerTy`'s `ImplementedInterfaces` list to the concrete interface
    /// it names, registering that instantiation in the ConcreteTypes registry if it is not
    /// already there.
    let private resolveImplementedInterface
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ownerTy : ConcreteType<ConcreteTypeHandle>)
        (impl : WoofWare.PawPrint.InterfaceImplementation)
        (state : IlMachineState)
        : IlMachineState *
          ConcreteTypeHandle *
          ConcreteType<ConcreteTypeHandle> *
          TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let ownerAssy =
            state._LoadedAssemblies.ByDefinitionName ownerTy.Identity.AssemblyFullName

        let implAssy =
            match state.LoadedAssembly impl.RelativeToAssembly.FullName with
            | Some assy -> assy
            | None -> ownerAssy

        let state, implTypeDefn, implResolvedAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                implAssy
                ownerTy.Generics
                impl.InterfaceHandle

        let state, implHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                implResolvedAssy.DefinitionFullName
                ownerTy.Generics
                ImmutableArray.Empty
                implTypeDefn

        match IlMachineState.tryGetConcreteTypeInfo state implHandle with
        | Some (implTy, typeInfo) -> state, implHandle, implTy, typeInfo
        | None -> failwith $"Interface implementation handle %O{implHandle} was not registered or has no TypeDef row"

    let private tryResolveVirtualImplementationForSlot
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        let logger = loggerFactory.CreateLogger "CallMethod"

        logger.LogDebug (
            "Identifying target of virtual call for {TypeName}.{MethodName}",
            methodToCall.RequiredDeclaringType.Name,
            methodToCall.Name
        )

        // The SZ-array carve-out runs *before* the ordinary walks, unlike CoreCLR, which reaches
        // it only after its dispatch map misses. CoreCLR can afford that ordering because its
        // lookup is exact-slot; ours matches on name and signature, which is fuzzier. Running
        // first is safe and total: when the receiver is an SZ array and the target is one of the
        // five interfaces, the answer is always SZArrayHelper. Nothing on the receiver's fixed
        // class chain can shadow it either — an array's
        // only ancestors are `System.Array` and `System.Object`, and every collection member of
        // `System.Array` is an *explicit* implementation of the corresponding **non-generic**
        // interface, so its metadata name is `System.Collections.IList.Contains` and can match
        // neither the plain name nor the `System.Collections.Generic.ICollection`1.Contains`
        // form that `interfaceExplicitNamedMethod` below constructs.
        //
        // Gated on `walkBaseTypes` because `false` means "exact-type, non-virtual dispatch" (the
        // `constrained.` value-type probe), and this redirect is inherently a synthetic *virtual*
        // substitute with no exact-type reading. Array receivers cannot reach those call sites
        // today — `constrained.` on an array takes ECMA case 1 in `executeCallvirt` and re-enters
        // ordinary virtual dispatch — but gating keeps that invariant checkable from here alone.
        match
            if walkBaseTypes then
                tryResolveSzArrayImplicitInterfaceMethod
                    loggerFactory
                    baseClassTypes
                    methodToCall
                    dispatchTypeHandle
                    state
            else
                None
        with
        | Some (state, impl) ->
            logger.LogDebug (
                "Dispatching SZ-array implicit interface method {MethodName} to System.SZArrayHelper",
                methodToCall.Name
            )

            state, Some impl
        | None ->

        let declaringAssy =
            state.LoadedAssembly(methodToCall.DeclaringAssemblyFullName).Value

        let methodDeclaringType =
            declaringAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        let interfaceExplicitNamedMethod =
            if methodDeclaringType.IsInterface then
                Some
                    $"{TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) methodDeclaringType}.{methodToCall.Name}"
            else
                None

        let signatureMatchesTarget
            (candidateAssemblyFullName : string)
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (candidateSignature : TypeMethodSignature<TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            // The target's own signature as its blob spells it. `methodToCall.Signature` has been
            // concretised, which has already discarded the custom modifiers and the choice of
            // encoding that this comparison turns on.
            let targetSignature =
                match methodToCall.TryMetadata with
                | Some metadata -> declaringAssy.Methods.[metadata.Handle].Signature
                | None ->
                    // Every dispatch target reached from a metadata token has a MethodDef row. A
                    // synthesised method has no blob to compare against, so refuse rather than fall
                    // back to a comparison that would answer a different question.
                    failwith
                        $"TODO: virtual dispatch to synthesised method %s{methodToCall.Name} on %O{methodToCall.RequiredDeclaringType.Name}, which has no MethodDef row and so no signature blob to match candidates against"

            let candidateComparand : TypeConcretization.SignatureComparand =
                {
                    Signature = candidateSignature
                    AssemblyFullName = candidateAssemblyFullName
                    DeclaringTypeGenerics = TypeConcretization.SubstitutionContext.ofClosed candidateTypeGenerics
                }

            let targetComparand : TypeConcretization.SignatureComparand =
                {
                    Signature = targetSignature
                    AssemblyFullName = methodToCall.DeclaringAssemblyFullName
                    DeclaringTypeGenerics =
                        TypeConcretization.SubstitutionContext.ofClosed methodToCall.DeclaringTypeGenerics
                }

            // The return column is compared separately, because PawPrint's *dispatch* rule is
            // deliberately looser than CoreCLR's *layout* rule: it accepts an assignable return so
            // that a covariant-return override can be found, where
            // `VirtualSlotLayout.candidateFillsSlot` requires the exact signature CoreCLR
            // requires. `skipReturnType` is how `MethodSignature::SignaturesEquivalent` expresses
            // the same latitude.
            let state, signatureMatches =
                IlMachineTypeResolution.signaturesEquivalent
                    loggerFactory
                    baseClassTypes
                    state
                    true
                    candidateComparand
                    targetComparand

            if not signatureMatches then
                state, false
            else

            let state, candidateReturn =
                candidateSignature.ReturnType
                |> IlMachineState.concretizeReturnColumn
                    loggerFactory
                    baseClassTypes
                    state
                    candidateAssemblyFullName
                    candidateTypeGenerics
                    methodToCall.Generics

            match candidateReturn, methodToCall.Signature.ReturnType with
            | MethodReturnType.Void, MethodReturnType.Void -> state, true
            | MethodReturnType.Returns retType, MethodReturnType.Returns targetType ->
                isAssignableFrom loggerFactory baseClassTypes retType targetType state
            | MethodReturnType.Void, MethodReturnType.Returns _
            | MethodReturnType.Returns _, MethodReturnType.Void -> state, false

        // When dispatching through a variant interface (ECMA-335 §I.8.7), the MethodImpl's
        // declaration may name a variance-compatible — not identical — instantiation of the
        // call target's interface. The candidate's signature has been substituted with the
        // declaration's view (e.g. `IContravariant<object>.Set(object)`) while methodToCall
        // holds the dispatch view (`IContravariant<string>.Set(string)`), so a literal
        // parameter-type comparison would wrongly reject the override.
        //
        // Instead of relaxing the signature comparison — which can match the wrong overload
        // when an interface has overloads with assignable parameters (e.g. both `M(object)`
        // and `M(string)`) — identify the slot by its underlying MethodDefinitionHandle.
        // Both `meth.Handle` and `methodToCall.Handle` resolve to the same MethodDef in the
        // interface's assembly when they name the same virtual slot under variance
        // substitution, regardless of how the surrounding type generics differ.
        let methodReferenceMatchesTarget
            (varianceInPlay : bool)
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (meth : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            if meth.Name <> methodToCall.Name then
                state, false
            elif varianceInPlay then
                state, MethodInfo.sameDeclaredMethod meth methodToCall
            else
                signatureMatchesTarget meth.DeclaringAssemblyFullName candidateTypeGenerics meth.Signature state

        let methodMatches
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (allowImplicitInterfaceImplementation : bool)
            (meth : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : (WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * bool) option *
              IlMachineState
            =
            if
                meth.Signature.GenericParameterCount
                <> methodToCall.Signature.GenericParameterCount
                || meth.Signature.RequiredParameterCount
                   <> methodToCall.Signature.RequiredParameterCount
            then
                None, state
            elif
                meth.Name <> methodToCall.Name
                && (not allowImplicitInterfaceImplementation
                    || Some meth.Name <> interfaceExplicitNamedMethod)
            then
                None, state
            elif
                not allowImplicitInterfaceImplementation
                && (not meth.IsVirtual
                    || (meth.IsNewSlot && not (MethodInfo.sameDeclaredMethod meth methodToCall)))
            then
                None, state
            elif
                // A static method can never stand in for an instance slot, nor an instance
                // method for a static one. Without this, a same-signature `static` shadow of an
                // interface method is dispatched as though it were the implementation, and the
                // missing `this` desynchronises the evaluation stack — the failure surfaces far
                // away, as "method returned with more than one evaluation stack value".
                meth.IsStatic <> methodToCall.IsStatic
            then
                None, state
            elif
                // Implicit implementation of an interface slot requires a *public* method
                // (ECMA-335 II.12.2): a private same-signature method is an ordinary member that
                // happens to collide, and leaves the slot to a default body or a base. Matching
                // by the explicit `Namespace.IFoo.Method` name is exempt, because that *is* the
                // explicit-implementation form and is private by construction.
                allowImplicitInterfaceImplementation
                && Some meth.Name <> interfaceExplicitNamedMethod
                && not meth.IsPublic
            then
                None, state
            else

            let state, matches =
                signatureMatchesTarget meth.DeclaringAssemblyFullName candidateTypeGenerics meth.Signature state

            if matches then
                Some (meth, Some meth.Name = interfaceExplicitNamedMethod), state
            else
                None, state

        let concretizeTypeArgs
            (declaringAssemblyFullName : string)
            (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (args : TypeDefn ImmutableArray)
            (state : IlMachineState)
            : IlMachineState * ImmutableArray<ConcreteTypeHandle>
            =
            ((state, ImmutableArray.CreateBuilder<ConcreteTypeHandle> ()), args)
            ||> Seq.fold (fun (state, acc) ty ->
                let state, handle =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        declaringAssemblyFullName
                        contextTypeGenerics
                        methodGenerics
                        ty

                acc.Add handle
                state, acc
            )
            |> Tuple.rmap (fun builder -> builder.ToImmutable ())

        let concreteTypeHandlesToTypeDefns
            (state : IlMachineState)
            (handles : ImmutableArray<ConcreteTypeHandle>)
            : ImmutableArray<TypeDefn>
            =
            handles
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let resolveMethodReference
            (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (relativeAssembly : DumpedAssembly)
            (token : MetadataToken)
            (state : IlMachineState)
            : IlMachineState *
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn> *
              TypeDefn ImmutableArray option
            =
            match token with
            | MetadataToken.MethodDef h ->
                let method =
                    relativeAssembly.Methods.[h]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                state, method, None
            | MetadataToken.MemberReference h ->
                let contextTypeGenerics = concreteTypeHandlesToTypeDefns state contextTypeGenerics
                let contextMethodGenerics = concreteTypeHandlesToTypeDefns state methodGenerics

                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMemberWithGenerics
                        loggerFactory
                        baseClassTypes
                        thread
                        relativeAssembly
                        contextTypeGenerics
                        contextMethodGenerics
                        h
                        state

                match method with
                | Choice1Of2 method -> state, method, Some extractedTypeArgs
                | Choice2Of2 _field -> failwith "MethodImpl referenced a field where a method was expected"
            | other ->
                // ECMA-335 permits MethodSpec here for generic method implementations; resolve it when
                // MethodImpl dispatch reaches such metadata.
                failwith $"MethodImpl referenced unexpected metadata token %O{other}"

        let methodImplDeclarationCouldMatch (relativeAssembly : DumpedAssembly) (token : MetadataToken) : bool =
            match token with
            | MetadataToken.MethodDef h ->
                let method = relativeAssembly.Methods.[h]

                method.Name = methodToCall.Name
                && method.Signature.GenericParameterCount = methodToCall.Signature.GenericParameterCount
                && method.Signature.RequiredParameterCount = methodToCall.Signature.RequiredParameterCount
            | MetadataToken.MemberReference h ->
                let memberRef = relativeAssembly.Members.[h]

                match memberRef.Signature with
                | MemberSignature.Method signature ->
                    memberRef.PrettyName = methodToCall.Name
                    && signature.GenericParameterCount = methodToCall.Signature.GenericParameterCount
                    && signature.RequiredParameterCount = methodToCall.Signature.RequiredParameterCount
                | MemberSignature.Field _ -> false
            | _ -> false

        let findMatchingMethodImplBodies
            (currentTy : ConcreteType<ConcreteTypeHandle>)
            (currentTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list
            =
            let currentAssy =
                state._LoadedAssemblies.ByDefinitionName currentTy.Identity.AssemblyFullName

            ((state, []), currentTypeInfo.MethodImpls.Values)
            ||> Seq.fold (fun (state, acc) impl ->
                if not (methodImplDeclarationCouldMatch currentAssy impl.Declaration) then
                    state, acc
                else
                    let state, declaration, declarationTypeArgs =
                        resolveMethodReference currentTy.Generics currentAssy impl.Declaration state

                    let state, declarationTypeGenerics =
                        match declarationTypeArgs with
                        | Some typeArgs ->
                            concretizeTypeArgs declaration.DeclaringAssemblyFullName currentTy.Generics typeArgs state
                        | None when declaration.DeclaringTypeGenerics.IsEmpty -> state, ImmutableArray.Empty
                        | None when declaration.RequiredDeclaringType.Identity = currentTy.Identity ->
                            state, currentTy.Generics
                        | None ->
                            failwith
                                $"MethodImpl declaration for %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} referenced generic MethodDef %s{declaration.Name} without concrete type arguments"

                    // A MethodImpl binds a Body to the specific virtual slot identified by its
                    // Declaration: ECMA-335 II.22.27 keys the slot on (declaring type, member).
                    // Name + signature alone is not enough — two unrelated interfaces can share
                    // a shape (e.g. `IReader.Read()` and `IScanner.Read()`), so we also require
                    // the declaration's declaring type to match the dispatch target.
                    //
                    // For variant interfaces (ECMA-335 §I.8.7) a MethodImpl on `IFoo<X>` also
                    // satisfies dispatch through `IFoo<Y>` when `IFoo<X>` is variance-assignable
                    // to `IFoo<Y>` (e.g. `ICovariant<string>` satisfies `ICovariant<object>`),
                    // so we defer same-TypeDef generic comparisons to the assignability walk
                    // rather than insisting on exact-argument equality.
                    //
                    // The declaration's declaring-type instantiation may not yet be in the
                    // ConcreteTypes registry (e.g. `ICovariant<object> obj = new CovariantImpl();
                    // obj.Get()` only concretizes the call target `ICovariant<object>`, not the
                    // body's declared interface `ICovariant<string>`). Register it on demand so
                    // the variance check is not silently skipped.
                    let ensureRegistered
                        (state : IlMachineState)
                        (identity : ResolvedTypeIdentity)
                        (ns : string)
                        (name : string)
                        (generics : ImmutableArray<ConcreteTypeHandle>)
                        : IlMachineState * ConcreteTypeHandle
                        =
                        match AllConcreteTypes.findExistingConcreteType state.ConcreteTypes identity generics with
                        | Some handle -> state, handle
                        | None ->
                            let ct = ConcreteType.makeFromIdentity identity ns name generics
                            let handle, newConcreteTypes = AllConcreteTypes.add ct state.ConcreteTypes

                            { state with
                                ConcreteTypes = newConcreteTypes
                            },
                            handle

                    // declarationTypeMatches is true when the MethodImpl's declared interface
                    // matches the dispatch target; varianceInPlay tracks whether the match
                    // relied on generic variance (vs identical instantiations), so we know to
                    // relax the parameter check accordingly.
                    let state, declarationTypeMatches, varianceInPlay =
                        if
                            declaration.RequiredDeclaringType.Identity
                            <> methodToCall.RequiredDeclaringType.Identity
                        then
                            state, false, false
                        elif declarationTypeGenerics = methodToCall.DeclaringTypeGenerics then
                            state, true, false
                        else
                            let state, fromH =
                                ensureRegistered
                                    state
                                    declaration.RequiredDeclaringType.Identity
                                    declaration.RequiredDeclaringType.Namespace
                                    declaration.RequiredDeclaringType.Name
                                    declarationTypeGenerics

                            let state, toH =
                                ensureRegistered
                                    state
                                    methodToCall.RequiredDeclaringType.Identity
                                    methodToCall.RequiredDeclaringType.Namespace
                                    methodToCall.RequiredDeclaringType.Name
                                    methodToCall.DeclaringTypeGenerics

                            let state, matches = isAssignableFrom loggerFactory baseClassTypes fromH toH state

                            state, matches, matches

                    if not declarationTypeMatches then
                        state, acc
                    else

                    let matches, state =
                        let state, matches =
                            methodReferenceMatchesTarget varianceInPlay declarationTypeGenerics declaration state

                        matches, state

                    if not matches then
                        state, acc
                    else
                        match impl.Body with
                        | MetadataToken.MethodDef body -> state, currentAssy.Methods.[body] :: acc
                        | other ->
                            failwith
                                $"MethodImpl body for %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} was not a MethodDef: %O{other}"
            )

        let concretizeImplementation
            (implementationTypeHandle : ConcreteTypeHandle)
            (implementation : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            =
            let typeGenerics =
                AllConcreteTypes.lookup implementationTypeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Implementation declaring type handle %O{implementationTypeHandle} was not registered while concretizing %s{MethodOwner.describe implementation.Owner}::%s{implementation.Name}"
                )
                |> _.Generics

            let state, meth, _ =
                ExecutionConcretization.concretizeMethodWithAllGenerics
                    loggerFactory
                    baseClassTypes
                    typeGenerics
                    implementation
                    methodGenerics
                    state

            state, meth

        /// The receiver's class chain, most-derived first, as `(handle, identity)`.
        ///
        /// Needed because the slot table names its occupant's declaring type by
        /// `ResolvedTypeIdentity`, while concretising a method needs that type's `ConcreteTypeHandle`
        /// -- the instantiation the receiver actually supplies. `None` means some link is not a
        /// registered nominal type, which is the signal to fall back: a structural receiver has no
        /// class chain to walk.
        let concreteChainOfReceiver
            (state : IlMachineState)
            : IlMachineState * (ConcreteTypeHandle * ResolvedTypeIdentity) list option
            =
            let rec go state handle acc =
                match IlMachineState.tryGetConcreteTypeInfo state handle with
                | None -> state, None
                | Some (ty, _) ->
                    let acc = (handle, ty.Identity) :: acc

                    let state, baseHandle =
                        IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state handle

                    match baseHandle with
                    | None -> state, Some (List.rev acc)
                    | Some baseHandle -> go state baseHandle acc

            go state dispatchTypeHandle []

        /// Answer the call the way CoreCLR does: find the slot the target declaration owns, then read
        /// that slot of the receiver's method table.
        ///
        /// `None` means the shape is outside what this serves and the caller should fall back: an
        /// interface target, whose dispatch goes through the interface map rather than a vtable index;
        /// a non-virtual or static target; a target with no MethodDef row; a receiver with no class
        /// chain; `walkBaseTypes = false`, which is the `constrained.` exact-type probe; or a
        /// declaration owning no slot of its own declaring type.
        let tryResolveBySlotTable
            (state : IlMachineState)
            : IlMachineState *
              (ConcreteTypeHandle *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
              string) option
            =
            if
                not walkBaseTypes
                || methodDeclaringType.IsInterface
                || not methodToCall.IsVirtual
                || methodToCall.IsStatic
                || methodToCall.TryMetadata.IsNone
            then
                state, None
            else

            // One walk gives both halves: which slot every declaration in the receiver's chain owns,
            // and what each slot of the receiver holds. Asking the declaring type separately for the
            // first would build a second table for no gain -- slot numbers are prefix-stable, so the
            // receiver's own list already names the target's declaration at the index its declaring
            // type gave it.
            let state, table =
                VirtualSlotLayout.dispatchTableOfClosed loggerFactory baseClassTypes "callvirt" state dispatchTypeHandle

            match table with
            | None ->
                // A receiver with no method table: a byref, pointer or function pointer.
                state, None
            | Some table ->

            let target = methodToCall.DeclaringAssemblyFullName, methodToCall.IdentityKey

            match
                (match table.SlotOfDeclaration.TryGetValue target with
                 | true, slot -> Some slot
                 | false, _ -> None)
            with
            | None ->
                // The target owns no vtable slot anywhere on the receiver's chain -- either it holds
                // none of its own declaring type's slots, or the receiver does not derive from that
                // type at all. Valid IL gives neither, so hand the question back rather than guess.
                state, None
            | Some slot ->

            match
                (if slot >= 0 && slot < table.Occupants.Length then
                     Some table.Occupants.[slot]
                 else
                     None)
            with
            | None ->
                // Prefix stability means a slot named by an ancestor is always within the receiver's
                // table, so this is unreachable for a chain the walk built consistently. Falling back
                // beats reading past the end.
                state, None
            | Some occupant ->

            // The table says *which MethodDef*. Concretising it needs the instantiation the receiver
            // supplies for the type that declares it, which is that type's handle on the receiver's own
            // chain.
            let state, chain = concreteChainOfReceiver state

            match chain with
            | None -> state, None
            | Some chain ->

            match
                chain
                |> List.tryFind (fun (_, identity) -> identity = occupant.DeclaredBy.Identity)
            with
            | None ->
                // The occupant is declared by a type that is not on the receiver's chain, which would
                // mean the content table and the chain disagree about the receiver's ancestry.
                state, None
            | Some (implementationHandle, _) ->

            state,
            Some (implementationHandle, occupant.Method, "Found concrete implementation by reading the receiver's slot")

        let findClassImplementation (state : IlMachineState) : IlMachineState * _ option =
            // Resolution precedence: explicit MethodImpl entries, then method name/signature
            // matches on the current type, then the base type walk when enabled.
            let rec walkBase (state : IlMachineState) (currentTypeHandle : ConcreteTypeHandle) =
                if not walkBaseTypes then
                    state, None
                else
                    match currentTypeHandle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ -> state, None
                    | ConcreteTypeHandle.Concrete _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        let state, baseType =
                            IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state currentTypeHandle

                        match baseType with
                        | None -> state, None
                        | Some baseType -> walk state baseType

            and walk (state : IlMachineState) (currentTypeHandle : ConcreteTypeHandle) =
                match IlMachineState.tryGetConcreteTypeInfo state currentTypeHandle with
                | None -> walkBase state currentTypeHandle
                | Some (currentTy, currentTypeInfo) ->
                    let state, matchingMethodImplBodies =
                        findMatchingMethodImplBodies currentTy currentTypeInfo state

                    match matchingMethodImplBodies with
                    | [ impl ] -> state, Some (currentTypeHandle, impl, "Found concrete implementation from MethodImpl")
                    | _ :: _ ->
                        matchingMethodImplBodies
                        |> List.map (fun m -> m.Name)
                        |> String.concat ", "
                        // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
                        |> failwithf
                            "multiple MethodImpl bodies matched this virtual slot; overload/interface disambiguation is not implemented: %s"
                    | [] ->
                        let implementation, state =
                            (state, currentTypeInfo.Methods)
                            ||> List.mapFold (fun state meth ->
                                methodMatches currentTy.Generics methodDeclaringType.IsInterface meth state
                            )

                        let implementation =
                            implementation
                            |> List.choose id
                            |> List.sortBy (fun (_, isInterface) -> if isInterface then -1 else 0)

                        match implementation with
                        | (impl, true) :: l when (l |> List.forall (fun (_, b) -> not b)) ->
                            state, Some (currentTypeHandle, impl, "Found concrete implementation from an interface")
                        | [ impl, false ] -> state, Some (currentTypeHandle, impl, "Found concrete implementation")
                        | _ :: _ ->
                            implementation
                            |> List.map (fun (m, _) -> m.Name)
                            |> String.concat ", "
                            |> failwithf "multiple options: %s"
                        | [] -> walkBase state currentTypeHandle

            walk state dispatchTypeHandle

        let state, bySlotTable = tryResolveBySlotTable state

        let state, classImplementation =
            match bySlotTable with
            | Some result -> state, Some result
            | None -> findClassImplementation state

        match classImplementation with
        | Some (implementationTypeHandle, impl, logMessage) ->
            logger.LogDebug logMessage
            let state, impl = concretizeImplementation implementationTypeHandle impl state
            state, Some impl
        | None when not walkBaseTypes -> state, None
        | None ->

        logger.LogDebug "No concrete implementation found; scanning interfaces"

        let resolveImplementedInterface =
            resolveImplementedInterface loggerFactory baseClassTypes

        let hasCallableBody
            (meth : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            : bool
            =
            match meth.Body with
            | MethodBody.Il _ -> true
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _
            | MethodBody.Abstract -> false

        let findInterfaceImplementationOnType
            (currentTypeHandle : ConcreteTypeHandle)
            (currentTy : ConcreteType<ConcreteTypeHandle>)
            (currentTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option
            =
            let state, matchingMethodImplBodies =
                findMatchingMethodImplBodies currentTy currentTypeInfo state

            let matchingMethodImplBodies =
                matchingMethodImplBodies |> List.filter hasCallableBody

            match matchingMethodImplBodies with
            | [ impl ] -> state, Some impl
            | _ :: _ ->
                matchingMethodImplBodies
                |> List.map (fun m -> m.Name)
                |> String.concat ", "
                // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
                |> failwithf
                    "multiple interface MethodImpl bodies matched this virtual slot on %O; overload/interface disambiguation is not implemented: %s"
                    currentTypeHandle
            | [] ->
                let implementation, state =
                    (state, currentTypeInfo.Methods)
                    ||> List.mapFold (fun state meth -> methodMatches currentTy.Generics true meth state)

                let implementation =
                    implementation |> List.choose id |> List.map fst |> List.filter hasCallableBody

                match implementation with
                | [ impl ] -> state, Some impl
                | _ :: _ ->
                    implementation
                    |> List.map (fun m -> m.Name)
                    |> String.concat ", "
                    // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
                    |> failwithf
                        "multiple default interface methods matched this virtual slot on %O; overload/interface disambiguation is not implemented: %s"
                        currentTypeHandle
                | [] -> state, None

        let rec collectInterfaceCandidates
            (state : IlMachineState)
            (visited : Set<ConcreteTypeHandle>)
            (currentTypeHandle : ConcreteTypeHandle)
            (currentTy : ConcreteType<ConcreteTypeHandle>)
            (currentTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : IlMachineState *
              (ConcreteTypeHandle *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) list
            =
            if visited.Contains currentTypeHandle then
                state, []
            else
                let visited = visited.Add currentTypeHandle

                logger.LogDebug (
                    "Interface {InterfaceName} (generics: {InterfaceGenerics})",
                    currentTypeInfo.Name,
                    currentTy.Generics
                )

                let state, ownCandidate =
                    findInterfaceImplementationOnType currentTypeHandle currentTy currentTypeInfo state

                let ownCandidates =
                    match ownCandidate with
                    | Some impl -> [ currentTypeHandle, impl ]
                    | None -> []

                ((state, ownCandidates), currentTypeInfo.ImplementedInterfaces)
                ||> Seq.fold (fun (state, acc) impl ->
                    let state, parentHandle, parentTy, parentTypeInfo =
                        resolveImplementedInterface currentTy impl state

                    let state, parentCandidates =
                        collectInterfaceCandidates state visited parentHandle parentTy parentTypeInfo

                    state, parentCandidates @ acc
                )

        let collectDirectInterfaceCandidates
            (ownerTy : ConcreteType<ConcreteTypeHandle>)
            (ownerTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState *
              (ConcreteTypeHandle *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) list
            =
            ((state, []), ownerTypeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun (state, acc) impl ->
                let state, interfaceHandle, interfaceTy, interfaceTypeInfo =
                    resolveImplementedInterface ownerTy impl state

                let state, candidates =
                    // Each direct interface gets an independent visited set; diamond duplicates
                    // are intentionally collapsed by the distinctBy after collection.
                    collectInterfaceCandidates state Set.empty interfaceHandle interfaceTy interfaceTypeInfo

                state, candidates @ acc
            )

        let rec collectTypeAndBaseInterfaceCandidates
            (state : IlMachineState)
            (visited : Set<ConcreteTypeHandle>)
            (currentTypeHandle : ConcreteTypeHandle)
            : IlMachineState *
              (ConcreteTypeHandle *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) list
            =
            if visited.Contains currentTypeHandle then
                state, []
            else
                let visited = visited.Add currentTypeHandle

                let state, ownCandidates =
                    match IlMachineState.tryGetConcreteTypeInfo state currentTypeHandle with
                    | Some (currentTy, currentTypeInfo) ->
                        collectDirectInterfaceCandidates currentTy currentTypeInfo state
                    | None ->
                        match currentTypeHandle with
                        | ConcreteTypeHandle.Byref _
                        | ConcreteTypeHandle.Pointer _
                        | ConcreteTypeHandle.FunctionPointer _ ->
                            failwith $"No metadata dispatch type available for virtual receiver %O{currentTypeHandle}"
                        | ConcreteTypeHandle.Concrete _
                        | ConcreteTypeHandle.OneDimArrayZero _
                        | ConcreteTypeHandle.Array _ -> state, []

                let state, baseCandidates =
                    if not walkBaseTypes then
                        state, []
                    else
                        match currentTypeHandle with
                        | ConcreteTypeHandle.Byref _
                        | ConcreteTypeHandle.Pointer _
                        | ConcreteTypeHandle.FunctionPointer _ -> state, []
                        | ConcreteTypeHandle.Concrete _
                        | ConcreteTypeHandle.OneDimArrayZero _
                        | ConcreteTypeHandle.Array _ ->
                            let state, baseType =
                                IlMachineState.resolveBaseConcreteType
                                    loggerFactory
                                    baseClassTypes
                                    state
                                    currentTypeHandle

                            match baseType with
                            | None -> state, []
                            | Some baseType -> collectTypeAndBaseInterfaceCandidates state visited baseType

                state, ownCandidates @ baseCandidates

        let state, possibleInterfaceMethods =
            collectTypeAndBaseInterfaceCandidates state Set.empty dispatchTypeHandle

        let possibleInterfaceMethods =
            possibleInterfaceMethods
            |> List.distinctBy (fun (interfaceHandle, meth) -> interfaceHandle, meth.TryMetadata |> Option.map _.Handle)

        let rec hasMoreSpecificInterfaceImplementation
            (state : IlMachineState)
            (interfaceHandle : ConcreteTypeHandle)
            (candidates :
                (ConcreteTypeHandle *
                WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) list)
            : IlMachineState * bool
            =
            match candidates with
            | [] -> state, false
            | (otherInterfaceHandle, _) :: remaining ->
                if otherInterfaceHandle = interfaceHandle then
                    hasMoreSpecificInterfaceImplementation state interfaceHandle remaining
                else
                    let state, otherIsMoreSpecific =
                        IlMachineState.isConcreteTypeAssignableTo
                            loggerFactory
                            baseClassTypes
                            state
                            otherInterfaceHandle
                            interfaceHandle

                    if otherIsMoreSpecific then
                        state, true
                    else
                        hasMoreSpecificInterfaceImplementation state interfaceHandle remaining

        let state, mostSpecificInterfaceMethods =
            ((state, []), possibleInterfaceMethods)
            ||> List.fold (fun (state, acc) (interfaceHandle, meth) ->
                let state, hasMoreSpecificImplementation =
                    hasMoreSpecificInterfaceImplementation state interfaceHandle possibleInterfaceMethods

                if hasMoreSpecificImplementation then
                    state, acc
                else
                    state, (interfaceHandle, meth) :: acc
            )
            |> Tuple.rmap List.rev

        match mostSpecificInterfaceMethods with
        | [] ->
            logger.LogDebug "No interface implementation found either"
            state, None
        | [ implementationTypeHandle, meth ] ->
            logger.LogDebug (
                "Exactly one interface implementation found {DeclaringTypeNamespace}.{DeclaringTypeName}.{MethodName} ({MethodGenerics})",
                meth.RequiredDeclaringType.Namespace,
                meth.RequiredDeclaringType.Name,
                meth.Name,
                meth.Generics
            )

            let state, meth = concretizeImplementation implementationTypeHandle meth state
            state, Some meth
        | _ ->
            mostSpecificInterfaceMethods
            |> List.map (fun (_, m) -> $"%s{MethodOwner.describe m.Owner}::%s{m.Name}")
            |> String.concat ", "
            // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
            |> failwithf "multiple most-specific default interface implementations matched this virtual slot: %s"

    /// One entry of a receiver's interface map: the interface, and the type whose level of the
    /// map contributed it. The owner is what a slot's implicit implementation may come from —
    /// dispatch for `I<X>` declared by a base is answered by that base's methods, not by an
    /// unrelated same-signature method a derived type happens to introduce.
    type private InterfaceMapEntry =
        {
            Handle : ConcreteTypeHandle
            Type : ConcreteType<ConcreteTypeHandle>
            Owner : ConcreteTypeHandle
        }

    /// One interface, followed by its transitive parents, depth-first. `visited` collapses
    /// diamonds at the *first* occurrence; `variantInterfaceMapRetargets` depends on the
    /// resulting order.
    let rec private expandInterfaceEntry
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (owner : ConcreteTypeHandle)
        (state : IlMachineState)
        (visited : Set<ConcreteTypeHandle>)
        (ifaceHandle : ConcreteTypeHandle)
        (ifaceTy : ConcreteType<ConcreteTypeHandle>)
        (ifaceTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * Set<ConcreteTypeHandle> * InterfaceMapEntry list
        =
        if visited.Contains ifaceHandle then
            state, visited, []
        else

        let visited = visited.Add ifaceHandle

        let state, visited, parents =
            ((state, visited, []), ifaceTypeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun (state, visited, acc) impl ->
                let state, parentHandle, parentTy, parentTypeInfo =
                    resolveImplementedInterface loggerFactory baseClassTypes ifaceTy impl state

                let state, visited, expanded =
                    expandInterfaceEntry
                        loggerFactory
                        baseClassTypes
                        owner
                        state
                        visited
                        parentHandle
                        parentTy
                        parentTypeInfo

                state, visited, acc @ expanded
            )

        state,
        visited,
        {
            Handle = ifaceHandle
            Type = ifaceTy
            Owner = owner
        }
        :: parents

    /// The receiver's interface map, in the order variance-compatible entries are *searched*:
    /// the interfaces the type itself declares, in metadata order and each expanded through its
    /// own parents, and only then the base class's map.
    ///
    /// This is not the order of CoreCLR's interface-map array, which is built the
    /// other way round — `MethodTableBuilder::ExpandApproxInheritedInterfaces` lays the parent's
    /// entries down first and `ExpandApproxDeclaredInterfaces` appends the freshly-declared ones
    /// (`methodtablebuilder.cpp`). The search order is what matters, and it inverts that:
    /// `MethodTable::FindDefaultInterfaceImplementation` walks from the receiver up through
    /// `GetParentMethodTable`, scanning at each level only `IterateInterfaceMapFrom
    /// (dwParentInterfaces)` — i.e. only the entries that level newly declares, skipping the
    /// inherited prefix. `sourcesPure/VariantInterfaceMapOrder.cs` pins the resulting order
    /// against the real runtime.
    ///
    /// Variant interface dispatch resolves to the *first* compatible entry (see
    /// `variantInterfaceMapRetargets`), so this must not be reordered or set-ified.
    let rec private collectInterfaceMap
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (walkBaseTypes : bool)
        (state : IlMachineState)
        (visited : Set<ConcreteTypeHandle>)
        (typeHandle : ConcreteTypeHandle)
        : IlMachineState * Set<ConcreteTypeHandle> * InterfaceMapEntry list
        =
        if visited.Contains typeHandle then
            state, visited, []
        else

        // Class handles and interface handles share one `visited` set: a handle is one or the
        // other, never both, so they cannot shadow each other. Valid metadata has no cycle in a
        // base-type chain, but guarding here means malformed metadata fails as a missing entry
        // rather than as a hang.
        let visited = visited.Add typeHandle

        // The inherited prefix is computed *first*, even though it is emitted last, so that this
        // level only contributes entries the base map does not already supply: `visited` carries
        // the inherited set into the expansion below. That is what
        // `IterateInterfaceMapFrom(dwParentInterfaces)` buys CoreCLR for free.
        //
        // It matters when a type declares a child interface whose parent instantiation its base
        // already supplies — `class D : B, IChild<object>, I<Exception>` over `class B :
        // I<object>`, where `IChild<T> : I<T>`. Expanding `IChild<object>` reaches `I<object>`,
        // but that entry belongs at B's position, not D's, so `I<Exception>` is the first
        // `I`-identity entry and D's own body is what a call through `I<ArgumentException>` must
        // reach. Emitting the expanded `I<object>` at D's level instead would put B's body first.
        //
        // The `walkBaseTypes` gate is the same one the ordinary walks use: `false` means
        // "exact-type dispatch" (the `constrained.` value-type probe), where only the type's own
        // interface list is in scope. A value type's base chain is `ValueType`/`Enum`/`Object`,
        // none of which contributes a generic interface.
        let state, visited, baseEntries =
            if not walkBaseTypes then
                state, visited, []
            else

            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, visited, []
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->

            let state, baseType =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state typeHandle

            match baseType with
            | None -> state, visited, []
            | Some baseType -> collectInterfaceMap loggerFactory baseClassTypes walkBaseTypes state visited baseType

        let state, visited, ownEntries =
            match IlMachineState.tryGetConcreteTypeInfo state typeHandle with
            | None -> state, visited, []
            | Some (ty, typeInfo) ->
                ((state, visited, []), typeInfo.ImplementedInterfaces)
                ||> Seq.fold (fun (state, visited, acc) impl ->
                    let state, ifaceHandle, ifaceTy, ifaceTypeInfo =
                        resolveImplementedInterface loggerFactory baseClassTypes ty impl state

                    let state, visited, expanded =
                        expandInterfaceEntry
                            loggerFactory
                            baseClassTypes
                            typeHandle
                            state
                            visited
                            ifaceHandle
                            ifaceTy
                            ifaceTypeInfo

                    state, visited, acc @ expanded
                )

        state, visited, ownEntries @ baseEntries

    /// ECMA-335 §I.8.7 lets a call site name a variance-compatible instantiation of an interface
    /// the receiver never declares: `ISink<in T>` implemented at `ISink<object>` is dispatched
    /// through `ISink<string>`. The receiver's *own* entry is what supplies the body, so that is
    /// what dispatch must resolve against — not the call site's view.
    ///
    /// Dispatch is therefore retargeted rather than any comparison loosened: an implicit
    /// implementation `ObjectSink.Accept(object, ...)` matches `ISink<object>::Accept` exactly,
    /// and only fails against `ISink<string>::Accept` because the call site substituted a
    /// different `T`. (The explicit-MethodImpl form of the same shape already works, because a
    /// MethodImpl row identifies its slot by declaration rather than by signature; an implicit
    /// implementation has no such row.)
    ///
    /// When several entries are variance-compatible the *first* in interface-map order wins,
    /// with no ambiguity exception — swapping the declaration order swaps which body runs.
    /// CoreCLR resolves ordinary instance calls through a dispatch map built at type load, so
    /// there is no single line of it to cite for that rule, but its two adjacent variance passes
    /// both spell out the same one: `MethodTable::FindDefaultInterfaceImplementation` takes the
    /// first candidate and "[doesn't] look for a conflict for instance methods" once
    /// `allowVariance` is set, and `TryResolveVirtualStaticMethodOnThisType`'s second pass
    /// iterates the interface map in order, skipping "the exact matches as they were handled
    /// above", requiring `HasSameTypeDefAs`, and re-resolving on `pItfInMap` — the interface as
    /// the type declares it. `sourcesPure/VariantInterfaceMapOrder.cs` pins the observable rule
    /// against the real runtime, which is the authority here.
    ///
    /// "First wins" is a tie-break among entries that are *equally good*, and it does not
    /// override the CLR's precedence between a real implementation and a default interface body:
    /// CoreCLR only reaches `FindDefaultInterfaceImplementation` after its dispatch map — which
    /// records class implementations — has missed for every variance-compatible entry. So this
    /// returns *all* compatible entries in order and leaves the caller to prefer a real
    /// implementation from a later entry over a default body from an earlier one; see
    /// `sourcesPure/VariantInterfaceDefaultBodyPrecedence.cs`.
    ///
    /// Each returned entry is paired with the type that owns it, which is where its slot's
    /// implementation must be looked for. Resolving from the *receiver* instead would let an
    /// unrelated same-signature method introduced by a derived type answer for a slot its base
    /// declared — see `sourcesPure/VariantInterfaceSlotOwnership.cs`. That is as far as the
    /// interface map alone can go; `sourcesPure/InterfaceSlotHiddenByDerivedMethod.cs` records
    /// the cases that need a real slot-to-implementation dispatch map, which PawPrint lacks.
    ///
    /// The rule is deliberately restricted to *instance* methods; see the `methodToCall.IsStatic`
    /// guard below for why static interface members neither need nor may use this path.
    ///
    /// Returns `[]` when no such entry exists, leaving the caller's answer unchanged.
    let private variantInterfaceMapRetargets
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : IlMachineState)
        : IlMachineState *
          (WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> * ConcreteTypeHandle) list
        =
        // A non-generic interface has nothing to vary, so it can never reach here.
        if methodToCall.DeclaringTypeGenerics.IsEmpty then
            state, []
        elif
            // Static interface members do not reach the retarget: a static virtual slot has no
            // name-based matching to fall back on, so implementing one requires an explicit
            // MethodImpl row, and the MethodImpl path in `tryResolveVirtualImplementationForSlot`
            // is already variance-aware (`sourcesPure/StaticAbstractVariantInterfaceDispatch.cs`
            // exercises exactly that route). Declining here rather than assuming it holds matters
            // because the first-wins tie-break below would be *wrong* for a static member:
            // CoreCLR guards its equivalent shortcut on `!pInterfaceMD->IsStatic()`, so a static
            // one keeps scanning for a conflict and can throw AmbiguousResolutionException. If
            // this ever does become reachable, returning nothing leaves the caller's existing
            // loud failure in place instead of silently diverging.
            methodToCall.IsStatic
        then
            state, []
        else

        // The caller has already resolved this assembly on the path that led here, so a miss is
        // a broken invariant rather than a reason to decline.
        let declaringAssy =
            state.LoadedAssembly(methodToCall.DeclaringAssemblyFullName).Value

        let declaringTypeIsInterface =
            declaringAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get].IsInterface

        if not declaringTypeIsInterface then
            state, []
        else

        let state, _, interfaceMap =
            collectInterfaceMap loggerFactory baseClassTypes walkBaseTypes state Set.empty dispatchTypeHandle

        // Entries at the *same* instantiation are exactly what the caller already searched, so
        // excluding them keeps this a strict fallback: it can only ever try an instantiation
        // that has not been tried.
        let candidates =
            interfaceMap
            |> List.filter (fun entry ->
                entry.Type.Identity = methodToCall.RequiredDeclaringType.Identity
                && entry.Type.Generics <> methodToCall.DeclaringTypeGenerics
            )

        if candidates.IsEmpty then
            state, []
        else

        let state, targetHandle =
            match
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    methodToCall.RequiredDeclaringType.Identity
                    methodToCall.DeclaringTypeGenerics
            with
            | Some handle -> state, handle
            | None ->
                let handle, newConcreteTypes =
                    AllConcreteTypes.add methodToCall.RequiredDeclaringType state.ConcreteTypes

                { state with
                    ConcreteTypes = newConcreteTypes
                },
                handle

        let state, compatible =
            ((state, []), candidates)
            ||> List.fold (fun (state, acc) entry ->
                let state, isCompatible =
                    isAssignableFrom loggerFactory baseClassTypes entry.Handle targetHandle state

                if isCompatible then state, acc @ [ entry ] else state, acc
            )

        ((state, []), compatible)
        ||> List.fold (fun (state, acc) entry ->
            let chosenTy = entry.Type

            match IlMachineState.tryGetConcreteTypeInfo state entry.Handle with
            | None ->
                // Unreachable: every entry here came from `resolveImplementedInterface`, which
                // already `failwith`s unless this same lookup succeeds. Loud rather than silent,
                // so that an upstream change breaking that invariant shows up here.
                failwith
                    $"variant interface dispatch: interface-map entry %s{chosenTy.Namespace}.%s{chosenTy.Name} (%O{entry.Handle}) is no longer registered"
            | Some (_, chosenTypeInfo) ->

            // Both instantiations share a TypeDef, so they share a method list: the slot is
            // identified by its MethodDef handle, exactly as the variance MethodImpl path does.
            match
                chosenTypeInfo.Methods
                |> List.tryFind (fun m -> MethodInfo.sameDeclaredMethod m methodToCall)
            with
            | None ->
                failwith
                    $"variant interface dispatch: %s{chosenTy.Namespace}.%s{chosenTy.Name} has no method with handle matching %s{methodToCall.Name}, though it shares a TypeDef with the call target"
            | Some slot ->
                let state, retargeted, _ =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        loggerFactory
                        baseClassTypes
                        chosenTy.Generics
                        slot
                        methodGenerics
                        state

                state, acc @ [ retargeted, entry.Owner ]
        )

    /// Identify the body a virtual or interface call lands on, given the receiver's runtime type.
    ///
    /// `walkBaseTypes` false means "exact-type dispatch": the `constrained.` value-type probe,
    /// which asks whether `T` itself supplies the method rather than inheriting it.
    ///
    /// Returns `None` when no override exists, which for a `callvirt` means the call site's own
    /// method is the answer.
    let tryResolveVirtualImplementation
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        let state, primary =
            tryResolveVirtualImplementationForSlot
                loggerFactory
                baseClassTypes
                thread
                methodGenerics
                methodToCall
                dispatchTypeHandle
                walkBaseTypes
                state

        // A resolved method whose declaring type is itself an interface came from a default
        // interface body; anything else is a real implementation.
        let isDefaultInterfaceBody
            (state : IlMachineState)
            (meth : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            : bool
            =
            state
                .LoadedAssembly(meth.DeclaringAssemblyFullName)
                .Value.TypeDefs.[meth.RequiredDeclaringType.Definition.Get].IsInterface

        match primary with
        // A real implementation from the call site's own instantiation is final: it is the
        // highest-precedence answer there is, so nothing further need be looked at. Almost every
        // call takes this branch.
        | Some resolved when not (isDefaultInterfaceBody state resolved) -> state, Some resolved
        | _ ->
            // Either nothing resolved, or the exact instantiation only offered a default body.
            // Both still lose to a real implementation reached through a variance-compatible
            // entry — CoreCLR consults its dispatch map, which records class implementations, for
            // *every* compatible entry before `FindDefaultInterfaceImplementation` runs at all —
            // so the scan below runs in both cases. See
            // `sourcesPure/VariantInterfaceDefaultBodyPrecedence.cs`.
            let state, retargets =
                variantInterfaceMapRetargets
                    loggerFactory
                    baseClassTypes
                    methodGenerics
                    methodToCall
                    dispatchTypeHandle
                    walkBaseTypes
                    state

            // Resolve every compatible entry rather than only the first: interface-map order is
            // the tie-break between equally good entries, not the whole precedence rule.
            // Every entry is resolved even once a winner is known: entries are few, and
            // stopping early would make the answer depend on evaluation order.
            let state, resolvedRetargets =
                ((state, []), retargets)
                ||> List.fold (fun (state, acc) (retargeted, owner) ->
                    // Two questions, and they have different answers.
                    //
                    // *Which method implements this slot* is settled at the entry's owner — the
                    // type at whose level of the interface map the entry sits. Only its own
                    // methods and its bases' are eligible; walking from the receiver would let a
                    // same-signature method introduced by a more-derived type answer for a slot
                    // it never re-declared.
                    //
                    // *Which body that method lands on* is then ordinary virtual dispatch from
                    // the receiver's runtime type, because an implementing method may be
                    // `virtual` (or `abstract`) and overridden further down. Re-resolving the
                    // owner's method against the receiver is exactly that, and it is safe to
                    // reuse here: the method is declared on a class, so `methodMatches` applies
                    // its `newslot`/non-virtual guard and accepts an `override` while rejecting
                    // an unrelated `new` method. A non-virtual implementation matches nothing and
                    // falls back to itself.
                    //
                    // One retry per entry: a retargeted call target *is* an interface-map entry,
                    // so a second scan could not find a not-yet-tried instantiation even if it
                    // ran. These call the inner resolution, so there is no recursion at all.
                    let state, atOwner =
                        tryResolveVirtualImplementationForSlot
                            loggerFactory
                            baseClassTypes
                            thread
                            methodGenerics
                            retargeted
                            owner
                            walkBaseTypes
                            state

                    match atOwner with
                    | None -> state, acc
                    | Some atOwner ->

                    // A default interface body has no class slot to override, and re-resolving
                    // one would re-enter the implicit-interface matching that owner-scoping just
                    // ruled out.
                    if owner = dispatchTypeHandle || isDefaultInterfaceBody state atOwner then
                        state, acc @ [ retargeted, atOwner ]
                    else

                    let state, overridden =
                        tryResolveVirtualImplementationForSlot
                            loggerFactory
                            baseClassTypes
                            thread
                            methodGenerics
                            atOwner
                            dispatchTypeHandle
                            walkBaseTypes
                            state

                    state, acc @ [ retargeted, Option.defaultValue atOwner overridden ]
                )

            // Precedence, highest first: a real implementation from any compatible entry; then
            // the exact instantiation's own default body; then a compatible entry's default body.
            // The last two are `FindDefaultInterfaceImplementation`'s own ordering, which tries
            // the exact match before allowing variance.
            let chosen =
                resolvedRetargets
                |> List.tryFind (fun (_, resolved) -> not (isDefaultInterfaceBody state resolved))

            match chosen, primary with
            | None, Some _ -> state, primary
            | None, None ->
                match List.tryHead resolvedRetargets with
                | None -> state, None
                | Some (_, resolved) -> state, Some resolved
            | Some (retargeted, resolved), _ ->
                let logger = loggerFactory.CreateLogger "CallMethod"

                logger.LogDebug (
                    "Retargeting variant interface call {DeclaringTypeName}::{MethodName} to the receiver's own instantiation {Generics}",
                    methodToCall.RequiredDeclaringType.Name,
                    methodToCall.Name,
                    retargeted.DeclaringTypeGenerics
                )

                state, Some resolved

    /// What `callMethodWithCommitment` actually did, for callers that must distinguish the cases.
    ///
    /// Initialising the callee's declaring type is the callee's own prologue, which runs after
    /// this function has pushed its frame — so every call commits, and the only question is
    /// whether it committed by running or by raising.
    [<RequireQualifiedAccess>]
    type CallCommitment =
        /// The call happened: a frame was pushed for the callee, or an intrinsic serviced it
        /// inline.
        ///
        /// This says the *call* took effect, not that the calling instruction is finished.
        /// Whether that instruction re-executes is the caller's own choice, made through
        /// `advanceProgramCounterOfCaller`.
        | Committed
        /// The callee raised instead of running: an exception constructor is now the active frame
        /// and dispatch follows. The calling instruction will not re-execute, and its arguments
        /// have already been consumed.
        | Raised
        /// The call was refused in a way the guest cannot catch, and the process is going down.
        /// No frame was pushed and nothing else will run on any thread.
        ///
        /// Distinct from `Raised` because there is no handler search to follow and no state the
        /// caller could usefully continue from: every caller must propagate rather than carry on.
        | Aborted of FatalError

    /// What a call site does to the thread on the way into its callee: whether it is still in
    /// cooperative mode when the callee's prologue runs.
    ///
    /// Read from the call site's signature; PawPrint does not model GC mode. This is the property
    /// CoreCLR's reverse-P/Invoke prologue actually tests, and so the thing that decides whether
    /// entering a `[UnmanagedCallersOnly]` method is the legal native transition or a fatal one.
    ///
    /// Keyed on the call site rather than the callee because the same method admits both:
    /// `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs` and
    /// `sourcesImpure/UnmanagedCallersOnlyManagedCalli.cs` are exactly that pair.
    [<RequireQualifiedAccess>]
    type CallSiteTransition =
        /// The thread leaves cooperative mode before the callee runs. Only a `calli` through a
        /// `delegate* unmanaged&lt;...&gt;` does this, and only when it does not suppress the
        /// transition. This is the entry a `[UnmanagedCallersOnly]` method admits.
        | EntersPreemptive
        /// The thread is still cooperative when the callee's prologue runs. Every managed call site
        /// — `call`, `callvirt`, a delegate's `Invoke`, reflection — and *also* an unmanaged one
        /// carrying `CallConvSuppressGCTransition`, which is an unmanaged calling convention that
        /// nevertheless skips the transition.
        ///
        /// That last case is why this is not simply "is the calling convention managed": measured,
        /// real .NET refuses a `delegate* unmanaged[SuppressGCTransition]&lt;int, int&gt;` entry into
        /// such a method with the same fatal error it gives a managed one, because the caller never
        /// left cooperative mode for the callee's prologue to find it in.
        | StaysCooperative

    [<RequireQualifiedAccess>]
    module CallSiteTransition =
        /// The namespace every calling-convention modifier lives in; CoreCLR's
        /// `CMOD_CALLCONV_NAMESPACE`, and the first thing it compares (callconvbuilder.cpp).
        let private callConvNamespace = "System.Runtime.CompilerServices"

        /// Is this custom modifier `CallConvSuppressGCTransition`?
        ///
        /// `resolveTypeDefName` names a modifier the signature gives as a TypeDef, which needs the
        /// owning module's tables. CoreCLR resolves both forms —
        /// `GetNameOfTypeRefOrDef(pModule, tk, ...)` — and *ignores* a modifier it cannot name
        /// rather than failing, so `None` lands on the same "not this one" as an unrelated
        /// modifier. Erroring here instead would crash on a legal call that merely carries a
        /// modifier we do not recognise.
        ///
        /// Same accepted risk as the rest of PawPrint's well-known-type matching: this compares
        /// namespace and name without checking that the type resolves to corelib's.
        let private isSuppressGcTransition
            (resolveTypeDefName : ResolvedTypeIdentity -> (string * string) option)
            (modifier : TypeDefn)
            : bool
            =
            let named =
                match modifier with
                | TypeDefn.FromReference (typeRef, _) -> Some (typeRef.Namespace, typeRef.Name)
                | TypeDefn.FromDefinition (identity, _) -> resolveTypeDefName identity
                | _ -> None

            match named with
            | Some (ns, name) -> ns = callConvNamespace && name = "CallConvSuppressGCTransition"
            | None -> false

        /// The whole signature, not just its header: `delegate* unmanaged[SuppressGCTransition]<...>`
        /// carries the *same* `Unmanaged` header as a plain `delegate* unmanaged<...>` and differs
        /// only by a `modopt` on the return type (measured: `09 01 08 08` against
        /// `09 01 20 49 08 08`), so a classifier reading the header alone would call the two the
        /// same thing.
        ///
        /// This follows CoreCLR's own algorithm rather than an approximation of it, because each
        /// place the two could differ is a call PawPrint would refuse and .NET would run:
        ///
        ///  * only the `Unmanaged` (0x09) header consults modifiers at all. A legacy header names
        ///    its convention outright, and `getUnmanagedCallConv` (jitinterface.cpp) returns it
        ///    without ever calling `TryGetUnmanagedCallingConventionFromModOpt`;
        ///  * only *optional* modifiers count. The parser skips required ones outright
        ///    (`if (!fIsOptional) continue;`, callconvbuilder.cpp), so a
        ///    `modreq(CallConvSuppressGCTransition)` suppresses nothing.
        let ofCallSiteSignature
            (resolveTypeDefName : ResolvedTypeIdentity -> (string * string) option)
            (signature : TypeMethodSignature<TypeDefn>)
            : CallSiteTransition
            =
            match signature.Header.Get.CallingConvention with
            | SignatureCallingConvention.Default
            | SignatureCallingConvention.VarArgs -> CallSiteTransition.StaysCooperative
            | SignatureCallingConvention.CDecl
            | SignatureCallingConvention.StdCall
            | SignatureCallingConvention.ThisCall
            | SignatureCallingConvention.FastCall -> CallSiteTransition.EntersPreemptive
            | SignatureCallingConvention.Unmanaged ->
                // Only the outermost run of modifiers describes the call site; one nested inside
                // the return type (`int32 modopt(X)[]`) is about that type, not the transition.
                let rec suppresses (ty : TypeDefn) : bool =
                    match ty with
                    | TypeDefn.Modified modified ->
                        (not modified.IsRequired
                         && isSuppressGcTransition resolveTypeDefName modified.Modifier)
                        || suppresses modified.Unmodified
                    | _ -> false

                match signature.ReturnType with
                // An unmodified void return carries no modifiers to inspect. A *modified* one
                // lands in `Returns` even when what it modifies is void, which is where the walk
                // above finds it.
                | MethodReturnType.Void -> CallSiteTransition.EntersPreemptive
                | MethodReturnType.Returns returnType ->
                    if suppresses returnType then
                        CallSiteTransition.StaysCooperative
                    else
                        CallSiteTransition.EntersPreemptive
            | other ->
                failwith
                    $"call site declares calling convention %O{other}, which is not one ECMA-335 II.23.2.3 admits for a method signature; refusing to guess whether the thread leaves cooperative mode"

    /// The fatal error that entering <paramref name="method"/> raises, or `None` when the entry is
    /// legal.
    ///
    /// A `[UnmanagedCallersOnly]` method may be entered only from native code. CoreCLR compiles one
    /// with `CORJIT_FLAG_REVERSE_PINVOKE`, whose prologue performs a reverse-P/Invoke transition
    /// asserting *preemptive* GC mode; a thread that is still cooperative trips
    /// `ReversePInvokeBadTransition` (dllimportcallback.cpp) and the process goes down uncatchably.
    ///
    /// One-directional deliberately: a *transitioning* entry into a method that is not
    /// `[UnmanagedCallersOnly]` is undefined behaviour in real .NET rather than a diagnosed error,
    /// so there is no answer to be faithful to and none is invented.
    ///
    /// This is a rule about *entering a method*, not about calling one, which is why it lives here
    /// rather than inside `callMethodWithCommitment`: a thread's entry point is entered without any
    /// call instruction, and `sourcesImpure/UnmanagedCallersOnlyThreadStart.cs` is that route.
    /// Every caller must apply it before anything the callee could observe — real .NET refuses the
    /// entry *without* running the declaring type's static constructor, which
    /// `sourcesImpure/UnmanagedCallersOnlyCctorNotRun.cs` pins.
    ///
    /// The places a method gets entered, and what each does with this:
    /// <list type="bullet">
    /// <item><c>callMethodWithCommitment</c>, which every call instruction, delegate dispatch and
    /// reflective invoke passes through — applies it;</item>
    /// <item><c>Thread.StartInternal</c>, which builds a worker's bottom frame directly — applies
    /// it;</item>
    /// <item>the guest's entry point, installed by <c>Program</c> — does not. Roslyn refuses to
    /// attribute one (CS8899), so no guest compiled from C# can present the shape; an image handed
    /// to PawPrint directly could, and what CoreCLR does with it is unmeasured. See
    /// docs/divergences.md;</item>
    /// <item><c>AppContextSeed</c> and <c>SignalDispatch</c>, which likewise build frames directly
    /// — do not, because neither lets the guest choose the method: the first names BCL methods, and
    /// a signal handler takes a <c>PosixSignalContext</c>, whose non-blittability makes the
    /// attribute illegal on it (CS8894).</item>
    /// </list>
    let unmanagedCallersOnlyRefusal
        (transition : CallSiteTransition)
        (method : WoofWare.PawPrint.MethodInfo<'a, 'b, 'c>)
        : FatalError option
        =
        match transition with
        | CallSiteTransition.EntersPreemptive -> None
        | CallSiteTransition.StaysCooperative ->
            if MethodInfo.isUnmanagedCallersOnly method then
                {
                    Code = FatalErrorCode.ExecutionEngine
                    // CoreCLR's own wording, extended with the method it refused: the guest cannot
                    // observe either way, and a run that ends this way should say what ended it.
                    Message =
                        Some
                            $"Invalid Program: attempted to call a UnmanagedCallersOnly method from managed code. (%s{MethodOwner.describe method.Owner}::%s{method.Name})"
                }
                |> Some
            else
                None

    let rec callMethodWithCommitment
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (wasInitialising : ConcreteTypeHandle option)
        (wasConstructing : ConstructionState)
        (performInterfaceResolution : bool)
        (wasClassConstructor : bool)
        (advanceProgramCounterOfCaller : bool)
        (callSiteTransition : CallSiteTransition)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (thread : ThreadId)
        (threadState : ThreadState)
        (callSiteIlOpIndexOverride : int option)
        (constructedObjectDisposition : ReturnValueDisposition)
        (wrapExceptionInTargetInvocation : bool)
        (state : IlMachineState)
        : IlMachineState * CallCommitment
        =
        let logger = loggerFactory.CreateLogger "CallMethod"

        let activeMethodState = threadState.MethodState

        // The method named at the call site, before any virtual/interface resolution. Retained
        // because the *type-level* `[Intrinsic]` check below is keyed on it; see there.
        let callSiteMethod = methodToCall

        // Virtual/interface resolution runs before the `[Intrinsic]` classification below, so
        // that `isIntrinsic` and `intrinsicKey` describe the method we are actually about to
        // execute.
        let shouldPerformVirtualResolution =
            performInterfaceResolution && methodToCall.DispatchesVirtually

        let state, methodToCall =
            if shouldPerformVirtualResolution then
                let callingObj =
                    match
                        activeMethodState.EvaluationStack
                        |> EvalStack.PeekNthFromTop (MethodInfo.arity methodToCall)
                    with
                    | None -> failwith "unexpectedly no `this` on the eval stack of instance method"
                    | Some this -> this

                let state, callingObjTyHandle =
                    getTypeOfObj loggerFactory baseClassTypes state callingObj

                let state, resolved =
                    tryResolveVirtualImplementation
                        loggerFactory
                        baseClassTypes
                        thread
                        methodGenerics
                        methodToCall
                        callingObjTyHandle
                        true
                        state

                state, resolved |> Option.defaultValue methodToCall
            else
                state, methodToCall

        // Keyed on the call site, not on the target alone -- the target is perfectly legal to
        // enter. `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs` calls this very method
        // through a `delegate* unmanaged<int, int>` and must keep working, and
        // `sourcesImpure/UnmanagedCallersOnlyManagedCalli.cs` is the same method reached by a
        // *managed* `calli`, which must not. When `Marshal.GetDelegateForFunctionPointer` lands, a
        // delegate wrapping such a method's native pointer is likewise a host-initiated entry and
        // must be given a transitioning call site rather than arriving here as cooperative.
        //
        // Before the callee's class initialiser is armed, and before anything else the callee could
        // observe; see `unmanagedCallersOnlyRefusal`.
        match unmanagedCallersOnlyRefusal callSiteTransition methodToCall with
        | Some fatal -> state, CallCommitment.Aborted fatal
        | None ->

        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringAssemblyFullName with
            | Some assy -> assy
            | None ->
                failwith
                    $"CallMethod: declaring assembly for %O{methodToCall} is not loaded: %O{methodToCall.DeclaringAssemblyFullName}"

        let getMemberRefParentType (handle : MemberReferenceHandle) : TypeRef =
            match declaringAssy.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> declaringAssy.TypeRefs.[r]
            | x -> failwith $"{x}"

        // Check for intrinsics first
        let methodHasIntrinsicAttribute =
            MethodInfo.isJITIntrinsic getMemberRefParentType declaringAssy.Methods methodToCall

        // The two `[Intrinsic]` checks deliberately use different methods as their basis.
        //
        //  * Method-level `[Intrinsic]` (above) is a property of the body we are about to run, so
        //    it is keyed on the post-resolution method. For example,
        //    `callvirt ICloneable::Clone()` must be recognised as `Array::Clone`.
        //
        //  * Type-level `[Intrinsic]` is a property of the call site's static type. It marks a
        //    type whose own API surface the JIT knows (`Int128`, `Vector128<T>`, ...); it says
        //    nothing about that type's `System.Object` overrides. `Int128.GetHashCode` is plain
        //    `HashCode.Combine(_lower, _upper)` and carries no method-level attribute, so
        //    `callvirt Object::GetHashCode()` on a boxed `Int128` must interpret it as normal.
        //
        // When no resolution happened the two coincide, so this only diverges for `callvirt`.
        let callSiteDeclaringAssy =
            match state.LoadedAssembly callSiteMethod.DeclaringAssemblyFullName with
            | Some assy -> assy
            | None ->
                failwith
                    $"CallMethod: declaring assembly for call-site method %O{callSiteMethod} is not loaded: %O{callSiteMethod.DeclaringAssemblyFullName}"

        let callSiteGetMemberRefParentType (handle : MemberReferenceHandle) : TypeRef =
            match callSiteDeclaringAssy.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> callSiteDeclaringAssy.TypeRefs.[r]
            | x -> failwith $"{x}"

        // An abstract call-site declaration has no IL of its own, so a type-level `[Intrinsic]`
        // inherited from it is a hint about the interface, not about the override we resolved
        // to. `IEnumerator<T>` carries a type-level `[Intrinsic]`, so without this suppression
        // every `callvirt IEnumerator<T>::get_Current()` would be rejected even though it
        // resolves to an ordinary `SZGenericArrayEnumerator<T>` body.
        let callSiteBodyIsAbstract =
            match callSiteMethod.Body with
            | MethodBody.Abstract -> true
            | _ -> false

        // A method the runtime synthesised is never an intrinsic, and asking whether it is would
        // crash: both of the remaining questions -- the type-level `[Intrinsic]` and the method
        // key -- read a TypeDef row, and a `Reflection.Emit` method has none. `isJITIntrinsic`
        // already answers `false` for a synthesised method on the same reasoning
        // (Domain/MethodInfo.fs), so this keys on the same thing rather than on whether the owner
        // happens to be a type.
        //
        // Keyed on the *kind* rather than on `TryDeclaringType`: CoreCLR never
        // intrinsic-classifies synthesised code, and this also covers
        // the struct-marshal stub, whose owner is the type being *marshalled*: without this, a
        // `[Intrinsic]`-attributed struct being marshalled would divert its stub into
        // `Intrinsics.call` and fail with a TODO naming the subject type.
        //
        // `callSiteMethod` need not be tested separately: a synthesised method has
        // `DispatchesVirtually = false` and this path is reached with
        // `performInterfaceResolution = false`, so resolution can never make one of the pair
        // synthesised and the other not.
        let isSynthesised =
            match methodToCall with
            | MethodInfo.Synthesised _ -> true
            | MethodInfo.Metadata _ -> false

        let declaringTypeHasIntrinsicAttribute =
            not isSynthesised
            && not callSiteBodyIsAbstract
            && MethodInfo.hasIntrinsicAttribute
                callSiteGetMemberRefParentType
                callSiteDeclaringAssy.Methods
                callSiteDeclaringAssy.TypeDefs.[callSiteMethod.RequiredDeclaringType.Definition.Get].Attributes

        // `[Intrinsic]` on an abstract/interface method is a JIT inlining hint for the
        // call site only — there is no IL to interpret. Virtual resolution has already run
        // above, so `methodToCall` is normally the concrete override and this guard rarely
        // triggers. It only matters when resolution was skipped (`performInterfaceResolution = false`)
        // or found no implementation.
        let isAbstractBody =
            match methodToCall.Body with
            | MethodBody.Abstract -> true
            | _ -> false

        let isIntrinsic =
            (methodHasIntrinsicAttribute || declaringTypeHasIntrinsicAttribute)
            && not isAbstractBody

        // `None` exactly when there is no metadata to key on; see `isSynthesised` above. Every
        // consumer below therefore has to say what it does for a synthesised method, and each says
        // the same thing: it is not that intrinsic.
        let intrinsicKey : IntrinsicMethodKeys.IntrinsicMethodKey option =
            if isSynthesised then
                None
            else
                Some (Intrinsics.methodKey state methodToCall)

        // `static T Activator.CreateInstance<T>()` is marked `[Intrinsic]` because the JIT inlines it
        // to an allocate+ctor sequence. The managed IL bottoms out in InternalCalls
        // (`RuntimeType.CreateInstanceOfT`, `CallDefaultStructConstructor`) we don't model, so we
        // implement the high-level intrinsic semantics directly: for a value type T, push `default(T)`
        // (skipping any explicit parameterless struct ctor for now — see TODO); for a reference type T,
        // allocate the object and run its parameterless ctor by recursing through `callMethod`.
        // See https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Activator.RuntimeType.cs#L137-L160
        // (`CreateInstanceOfT` and `CallDefaultStructConstructor` are RuntimeType.CoreCLR.cs#L4028 and #L4056.)
        //
        // Exception wrapping:
        //  - CoreCLR's `CreateInstanceOfT` wraps any exception thrown by the recursed ctor in a
        //    `TargetInvocationException`. We can't observe that in a separate Activator frame
        //    because we inline the intrinsic, so the recursive `callMethod` for the ctor sets
        //    `WrapExceptionInTargetInvocation = true` on the ctor frame's `ReturnState`. Exception
        //    dispatch treats that flag as a boundary its first pass cannot see past — the wrap
        //    changes the exception's *type*, so outer frames must be searched against the wrapper
        //    — and its second pass, on reaching the ctor frame, pops it, synthesises a fresh
        //    `TargetInvocationException` with the original exception as `_innerException`, and
        //    starts a new search from the caller. A try/catch *inside* the ctor that handles the
        //    exception is unaffected, matching CoreCLR.
        //
        // Intentional divergence (see docs/divergences.md):
        //  - For `BeforeFieldInit` reference types, CoreCLR defers the type initializer past the
        //    Activator allocation/ctor pair. PawPrint's `newobj` (UnaryMetadataObjectOps.fs:240)
        //    runs cctor eagerly on every instance creation regardless of the flag, so this
        //    intrinsic follows the same convention. ECMA-335 II.10.5.3.2 permits eager schedules.
        let tryHandleActivatorCreateInstance () : (IlMachineState * CallCommitment) option =
            // A synthesised method has no key, and is not `Activator.CreateInstance` whatever else
            // it is.
            match intrinsicKey with
            | None -> None
            | Some intrinsicKey ->

            if
                AssemblyDefinitionName.isNamed "System.Private.CoreLib" intrinsicKey.DeclaringAssemblyFullName
                && intrinsicKey.DeclaringTypeFullName = "System.Activator"
                && intrinsicKey.MethodName = "CreateInstance"
                && List.isEmpty intrinsicKey.ParameterShapes
                && methodToCall.Generics.Length = 1
            then
                let tHandle = methodToCall.Generics.[0]

                // Determine whether T is a value type BEFORE running its cctor: CoreCLR's
                // `Activator.CreateInstance<T>()` for a value type without an explicit parameterless
                // ctor returns `default(T)` and does NOT trigger T's static constructor. We must not
                // observe cctor side effects on that path. The ref-type path picks up cctor naturally
                // via the recursive `callMethod` for the .ctor.
                let isValueType, typeDefOpt =
                    match tHandle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ ->
                        failwith
                            $"Activator.CreateInstance<T>() requires T to satisfy `new()`, but T has handle %O{tHandle}"
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Arrays are reference types but their construction is special; defer.
                        false, None
                    | ConcreteTypeHandle.Concrete _ ->
                        match IlMachineState.tryGetConcreteTypeInfo state tHandle with
                        | Some (_, typeInfo) ->
                            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo, Some typeInfo
                        | None ->
                            failwith
                                $"Activator.CreateInstance<T>(): concrete type handle %O{tHandle} has no TypeDef row"

                if isValueType then
                    match typeDefOpt with
                    | Some typeDef ->
                        let hasExplicitParameterlessCtor =
                            typeDef.Methods
                            |> List.exists (fun m -> m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0)

                        if hasExplicitParameterlessCtor then
                            failwith
                                $"TODO: Activator.CreateInstance<T>() for value type %s{typeDef.Namespace}.%s{typeDef.Name} with an explicit parameterless ctor is not yet implemented (CoreCLR runs it via CallDefaultStructConstructor, including running the cctor)"
                    | None -> failwith "Activator.CreateInstance<T>(): value-type branch without typeDef"

                    let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes tHandle

                    let state = state |> IlMachineState.pushToEvalStack zero thread

                    let state =
                        if advanceProgramCounterOfCaller then
                            IlMachineState.advanceProgramCounter thread state
                        else
                            state

                    // Serviced inline: the zero value is already on the caller's stack.
                    Some (state, CallCommitment.Committed)
                else

                match tHandle with
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    failwith $"TODO: Activator.CreateInstance<T>() for array type %O{tHandle} is not yet implemented"
                | _ -> ()

                let typeDef =
                    match typeDefOpt with
                    | Some typeDef -> typeDef
                    | None -> failwith "Activator.CreateInstance<T>(): reference-type branch without typeDef"

                // Validate T BEFORE running its cctor. CoreCLR rejects abstract types and types
                // without a public parameterless ctor in `RuntimeType.CreateInstanceOfT` /
                // ActivatorCache construction, before any class-init side effects are observable.
                // Running `ensureTypeInitialised` first would let a throwing `.cctor` mask the
                // `MissingMethodException` users actually expect — empirically verified against
                // .NET 10.
                if typeDef.TypeAttributes.HasFlag TypeAttributes.Abstract then
                    // CoreCLR's MissingMethodException carries the message
                    // "Cannot dynamically create an instance of type 'X'. Reason: Cannot create
                    // an abstract class." (verified against .NET 10).
                    failwith
                        $"TODO: Activator.CreateInstance<T>() should throw MissingMethodException because T = %s{typeDef.Namespace}.%s{typeDef.Name} is abstract"

                // CoreCLR's `CreateInstanceOfT` consults `ActivatorCache.CtorIsPublic` and throws
                // `MissingMethodException` if the parameterless ctor is non-public — see
                // RuntimeType.CoreCLR.cs:4034. Filter accordingly so an internal/private ctor is
                // not silently invoked.
                let isPublic (m : MethodInfo<_, _, _>) : bool = m.IsPublic

                let ctor =
                    typeDef.Methods
                    |> List.tryFind (fun m ->
                        m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0 && isPublic m
                    )

                match ctor with
                | None ->
                    // CoreCLR throws MissingMethodException here. We don't yet have a host helper
                    // to raise that, so fail loudly with the precise condition.
                    let hasNonPublicParameterless =
                        typeDef.Methods
                        |> List.exists (fun m ->
                            m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0 && not (isPublic m)
                        )

                    let reason =
                        if hasNonPublicParameterless then
                            "its parameterless instance constructor is non-public"
                        else
                            "it has no parameterless instance constructor"

                    failwith
                        $"TODO: Activator.CreateInstance<T>() should throw MissingMethodException because T = %s{typeDef.Namespace}.%s{typeDef.Name} %s{reason}"
                | Some ctor ->

                let ct =
                    AllConcreteTypes.lookup tHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"Activator.CreateInstance<T>(): concrete type handle %O{tHandle} not found in AllConcreteTypes"
                    )

                // CoreCLR's `CreateInstanceOfT` catches *every* exception escaping the
                // cache.CallRefConstructor path — including a `TypeInitializationException`
                // raised by T's `.cctor` — and rethrows it wrapped in `TargetInvocationException`.
                // Setting `wrapExceptionInTargetInvocation` on T's ctor frame below is the whole
                // of that: T's initialisation happens in that frame's prologue, so a `.cctor`
                // failure — running for the first time or cached from an earlier one — unwinds
                // through the ctor frame and meets the wrap on its way out, and so does anything
                // the ctor body itself throws.
                let state, concretizedCtor, declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        loggerFactory
                        baseClassTypes
                        ct.Generics
                        ctor
                        ImmutableArray.Empty
                        state

                let state, fields =
                    IlMachineState.buildInstanceStorage loggerFactory baseClassTypes state declaringTypeHandle

                let allocatedAddr, state =
                    IlMachineState.allocateManagedObject declaringTypeHandle fields state

                let state =
                    state
                    |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocatedAddr)) thread

                let threadState = state.ThreadState.[thread]

                callMethod
                    loggerFactory
                    baseClassTypes
                    None
                    (ConstructionState.Constructing allocatedAddr)
                    false
                    false
                    advanceProgramCounterOfCaller
                    concretizedCtor.Generics
                    concretizedCtor
                    thread
                    threadState
                    None
                    ReturnValueDisposition.PushToCaller
                    true // wrapExceptionInTargetInvocation: mirror CreateInstanceOfT
                    state
                // T's ctor frame is pushed; the activator call itself is done.
                |> fun state -> Some (state, CallCommitment.Committed)
            else
                None

        match
            // `isIntrinsic` is false whenever the key is absent, so the `Option.get` shape here is
            // discharged by the conjunction rather than assumed.
            if isIntrinsic && not (Intrinsics.isSafeIntrinsic (Option.get intrinsicKey)) then
                match tryHandleActivatorCreateInstance () with
                | Some result -> Some result
                | None ->

                match Intrinsics.call loggerFactory baseClassTypes wasConstructing methodToCall thread state with
                | IntrinsicResult.Completed result -> Some (result, CallCommitment.Committed)
                | IntrinsicResult.RaiseException (state, exnType, message) ->
                    // The intrinsic described an exception rather than raising it, because it
                    // cannot see `raiseRuntimeException` (compile order) and because raising it
                    // here is what makes an *unhandled* one expressible: `raiseRuntimeException`
                    // defers dispatch to the ctor's `Ret`, which can report
                    // `ExecutionResult.UnhandledException`. The intrinsic has deliberately not
                    // advanced the PC, so dispatch sees the faulting instruction's offset.
                    // `WhatWeDid` is always `Executed` here — the ctor frame is now the active
                    // frame, exactly as for an opcode-manufactured exception.
                    raiseRuntimeExceptionWithMessage loggerFactory baseClassTypes exnType message thread state
                    |> fst
                    |> fun state -> Some (state, CallCommitment.Raised)
                | IntrinsicResult.Unrecognised ->
                    failwith
                        $"TODO: implement JIT intrinsic %s{Intrinsics.formatMethodKey (Option.get intrinsicKey)}, or add it to safeIntrinsics after reviewing its IL"
            else
                None
        with
        | Some result -> result
        | None ->

        // Get zero values for all parameters.
        //
        // These are the coercion targets for the popped arguments below, and they are
        // deliberately derived from the `methodToCall` post-resolution — i.e. the body we are
        // about to execute, not the declaration named at the call site. The two
        // differ under `in`-variance: dispatching `IContravariant<string>::Set(string)` selects
        // a body declaring `Set(object)`, and the argument must be coerced to the body's
        // parameter type. `thisArgCoercionTarget` and `createNewFrame` below share that basis.
        let state, argZeroObjects =
            ((state, []), methodToCall.Signature.ParameterTypes)
            ||> List.fold (fun (state, zeros) tyHandle ->
                let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes tyHandle
                state, zero :: zeros
            )

        let argZeroObjects = List.rev argZeroObjects

        // Helper to pop and coerce a single argument
        let popAndCoerceArg zeroType methodState =
            let value, newState = MethodState.popFromStack methodState
            EvalStackValue.toCliTypeCoerced zeroType value, newState

        let thisArgCoercionTarget
            (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            : CliType
            =
            let declaringAssembly =
                state.LoadedAssembly (methodToCall.DeclaringAssemblyFullName) |> Option.get

            let declaringType =
                declaringAssembly.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies declaringType then
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
            else
                CliType.ObjectRef None

        // Pop exactly the method's declared parameters, leaving no `this` slot in the
        // resulting `Arguments` array.
        let popDeclaredParametersOnly () =
            let args = ImmutableArray.CreateBuilder (MethodInfo.arity methodToCall)
            let mutable currentState = activeMethodState

            for i = MethodInfo.arity methodToCall - 1 downto 0 do
                let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                args.Add arg
                currentState <- newState

            args.Reverse ()
            args.ToImmutable (), currentState

        // Collect arguments based on calling convention
        let args, afterPop =
            if methodToCall.IsStatic then
                popDeclaredParametersOnly ()
            else

            match wasConstructing with
            | ConstructionState.Constructing _ ->
                // Instance method: handle `this` pointer
                let argCount = MethodInfo.arity methodToCall
                let args = ImmutableArray.CreateBuilder (argCount + 1)
                let mutable currentState = activeMethodState
                let thisArgTarget = thisArgCoercionTarget methodToCall

                // Constructor: `this` is on top of stack, by our own odd little calling convention
                // where Newobj puts the object pointer on top
                let thisArg, newState = popAndCoerceArg thisArgTarget currentState

                currentState <- newState

                // Pop remaining args in reverse
                for i = argCount - 1 downto 0 do
                    let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                    args.Add arg
                    currentState <- newState

                args.Add thisArg
                args.Reverse ()
                args.ToImmutable (), currentState
            | ConstructionState.NotConstructing ->
                // Instance method: handle `this` pointer
                let argCount = MethodInfo.arity methodToCall
                let args = ImmutableArray.CreateBuilder (argCount + 1)
                let mutable currentState = activeMethodState
                let thisArgTarget = thisArgCoercionTarget methodToCall

                // Regular instance method: args then `this`
                for i = argCount - 1 downto 0 do
                    let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                    args.Add arg
                    currentState <- newState

                let thisArg, newState =
                    let rawThis, newState = MethodState.popFromStack currentState

                    let coerced =
                        match thisArgTarget, rawThis with
                        | CliType.RuntimePointer _, EvalStackValue.ObjectRef addr ->
                            // Boxed value type receiver: implicit unbox to managed pointer
                            // into the heap object's value data.
                            CliType.RuntimePointer (
                                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []))
                            )
                        | _ -> EvalStackValue.toCliTypeCoerced thisArgTarget rawThis

                    coerced, newState

                args.Add thisArg
                currentState <- newState

                args.Reverse ()
                args.ToImmutable (), currentState

        // Helper to create new frame with assembly loading
        let rec createNewFrame state =
            let returnInfo =
                Some
                    {
                        JumpTo = threadState.ActiveMethodState
                        WasInitialisingType = wasInitialising
                        Constructing = wasConstructing
                        CallSiteIlOpIndex = callSiteIlOpIndexOverride |> Option.defaultValue afterPop.IlOpIndex
                        ReturnValueDisposition = constructedObjectDisposition
                        WrapExceptionInTargetInvocation = wrapExceptionInTargetInvocation
                    }

            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    declaringAssy
                    methodToCall
                    methodGenerics
                    args
                    returnInfo
            with
            | Ok frame -> state, frame
            | Error toLoad ->
                let state' =
                    (state, toLoad)
                    ||> List.fold (fun s (asmRef : WoofWare.PawPrint.AssemblyReference) ->
                        let s, _, _ =
                            IlMachineState.loadAssembly
                                loggerFactory
                                (state.LoadedAssembly methodToCall.DeclaringAssemblyFullName |> Option.get)
                                (fst asmRef.Handle)
                                s

                        s
                    )

                createNewFrame state'

        let state, newFrame = createNewFrame state

        // The callee's prologue. Recorded on the frame rather than run here, and asked *after*
        // virtual resolution, so it names the type whose method actually runs: measured on
        // .NET 10, `callvirt IFace::M` resolving to `Impl.M` never runs `IFace`'s own
        // initialiser. A `.cctor` reached from here therefore unwinds through this frame and its
        // `TypeInitializationException` names this method, which is what the CLR reports.
        //
        // Only the calls ECMA-335 II.10.5.3.1 names as triggers arm one: a static method, an
        // instance constructor, or any instance method of a value type. An instance method call on
        // a reference-type object that already exists is not a trigger, and the difference is
        // observable — measured on .NET 10, an instance published by a `.cctor` that then threw
        // still answers a virtual call, while constructing another of the same type throws
        // `TypeInitializationException`. Arming every metadata method fails the first;
        // arming only statics and constructors fails the value-type clause.
        //
        // A `.cctor` frame is exempt too: it *is* the initialisation, and asking again would see
        // its own type in progress. `loadClass` answers `NothingToDo` for that, so this is an
        // optimisation rather than a correctness guard — but it keeps the invariant "a frame with
        // a pending init has not started" true of every frame that has one.
        let newFrame =
            if wasClassConstructor then
                newFrame
            else

            // The synthesised arm comes first, and asks its question *without* looking the
            // declaring type up: a method minted by `Reflection.Emit` is owned by a class with
            // no TypeDef row, so running the lookup below first would crash on every
            // dynamic-method call.
            match methodToCall with
            | MethodInfo.Synthesised (_, kind) ->
                if SynthesisedMethod.initialisesDeclaringType kind then
                    // No synthesised kind answers `true` today. When one does, it will need a
                    // declaring type to initialise, and this is where to look it up — separately
                    // from the metadata arm, because "which type does this synthesised method
                    // initialise" is a question about its semantics rather than about its owner.
                    failwith
                        $"TODO: %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} is a synthesised method whose kind claims to initialise its declaring type, but no path yet resolves which type that is"
                else
                    newFrame
            | MethodInfo.Metadata _ ->

            let handle =
                match
                    AllConcreteTypes.findExistingConcreteType
                        state.ConcreteTypes
                        methodToCall.RequiredDeclaringType.Identity
                        methodToCall.DeclaringTypeGenerics
                with
                | Some handle -> handle
                | None ->
                    failwith
                        $"calling %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name}: the resolved method's declaring type is not registered in AllConcreteTypes, so its initialiser cannot be scheduled"

            let initialises =
                if methodToCall.IsStatic then
                    true
                elif methodToCall.Name = ".ctor" then
                    // Identified by name, as elsewhere in the codebase. `wasConstructing` would be
                    // the wrong question: a derived constructor chaining to `base..ctor()` is not
                    // constructing a fresh object and yet does trigger the base type's
                    // initialiser — measured on .NET 10, `new Derived()` runs `Derived..cctor` and
                    // then `Base..cctor`, the latter from that chained call's own prologue.
                    true
                else
                    // An instance method of a *value type* is a trigger in its own right, and the
                    // only instance-method shape where that is observable: a class instance
                    // implies its constructor chain ran, and construction is itself a trigger,
                    // whereas `default(S)` runs nothing.
                    // `DelegateToValueTypeInstanceMethodRunsCctor.cs` is the case.
                    AllConcreteTypes.tryIsValueType baseClassTypes state._LoadedAssemblies state.ConcreteTypes handle
                    |> Option.defaultValue false

            if initialises then
                newFrame |> MethodState.withPendingTypeInit handle
            else
                newFrame

        let oldFrame =
            if wasClassConstructor || not advanceProgramCounterOfCaller then
                afterPop
            else
                afterPop |> MethodState.advanceProgramCounter

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState oldFrame threadState

        let calleeFrameId, threadState = ThreadState.appendFrame newFrame threadState
        let newThreadState = ThreadState.setActiveFrame calleeFrameId threadState

        // The callee's frame is now active and the caller's PC has been advanced (unless the
        // caller asked otherwise): the call has happened.
        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        },
        CallCommitment.Committed

    /// `callMethodWithCommitment` for the callers that do not need to distinguish whether the call
    /// committed by running or by raising: in both cases the returned state already reflects what
    /// happened, so there is nothing left to decide.
    ///
    /// Only for a call site whose target cannot be *refused*. An abort has nowhere to go in this
    /// return type, so it is a loud failure rather than a silently dropped outcome; a call site
    /// that can name a refusable target must use `callMethodWithCommitment` and propagate
    /// `CallCommitment.Aborted`, as `call`, `callvirt` and `calli` do.
    and callMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (wasInitialising : ConcreteTypeHandle option)
        (wasConstructing : ConstructionState)
        (performInterfaceResolution : bool)
        (wasClassConstructor : bool)
        (advanceProgramCounterOfCaller : bool)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (thread : ThreadId)
        (threadState : ThreadState)
        (callSiteIlOpIndexOverride : int option)
        (constructedObjectDisposition : ReturnValueDisposition)
        (wrapExceptionInTargetInvocation : bool)
        (state : IlMachineState)
        : IlMachineState
        =
        callMethodWithCommitment
            loggerFactory
            baseClassTypes
            wasInitialising
            wasConstructing
            performInterfaceResolution
            wasClassConstructor
            advanceProgramCounterOfCaller
            // Every caller of this wrapper is interpreter-internal machinery entering a method the
            // ordinary managed way: a class constructor, a delegate's constructor, a helper the
            // interpreter itself decided to run. None of them leaves cooperative mode, so a target
            // carrying `[UnmanagedCallersOnly]` is refused here just as it would be at a `call` --
            // and the wrapper's own guard below then fails loudly, because such a caller has no way
            // to propagate the abort.
            CallSiteTransition.StaysCooperative
            methodGenerics
            methodToCall
            thread
            threadState
            callSiteIlOpIndexOverride
            constructedObjectDisposition
            wrapExceptionInTargetInvocation
            state
        |> function
            | state, CallCommitment.Committed
            | state, CallCommitment.Raised -> state
            | _, CallCommitment.Aborted fatal ->
                // This wrapper's return type has nowhere to put an abort, and dropping one would
                // let the caller carry on against a state whose process has already died. Its
                // callers all name a constructor, a class initialiser, or a specific BCL method,
                // and no guest that compiles can point any of those at a `[UnmanagedCallersOnly]`
                // method: C# admits the attribute only on ordinary method declarations (CS0592
                // rejects it on a static constructor), and the BCL targets are ours to choose.
                //
                // So reaching here means metadata PawPrint has no answer for, not a wrong call-site
                // choice — and CoreCLR's behaviour when its *own* machinery enters such a method is
                // not something we have been able to measure. Refuse rather than guess; see
                // docs/divergences.md, "`[UnmanagedCallersOnly]` declarations and unmanaged call
                // sites are not validated".
                let message = fatal.Message |> Option.defaultValue "<no message>"

                failwith
                    $"a call made through `callMethod` aborted the process (%O{fatal.Code}: %s{message}). PawPrint cannot say what should happen here: this wrapper serves the interpreter's own entries — class initialisers, constructors, chosen BCL helpers — and it is unmeasured whether CoreCLR applies the reverse-P/Invoke transition to those at all. A guest cannot produce this; hand-authored metadata can"

    and loadClass
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : ConcreteTypeHandle)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : StateLoadResult
        =
        let logger = loggerFactory.CreateLogger "LoadClass"

        match TypeInitTable.tryGet ty state.TypeInitTable with
        | Some TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.Failed (tieAddr, tieType)) ->
            // The .cctor previously threw. Per ECMA-335, subsequent access should throw
            // TypeInitializationException. We rethrow the *same* cached instance to match
            // CLR identity semantics (ReferenceEquals across repeated accesses).
            match
                ExceptionDispatching.throwExceptionObject
                    loggerFactory
                    baseClassTypes
                    state
                    currentThread
                    tieAddr
                    tieType
            with
            | ExceptionDispatchResult.Dispatched state -> StateLoadResult.ThrowingTypeInitializationException state
            | ExceptionDispatchResult.ExceptionUnhandled _ ->
                failwith $"Unhandled TypeInitializationException during class loading for type with cached TIE"
        | Some (TypeInitState.InProgress tid) when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress blocker) ->
            // Another thread owns this type's .cctor lock. Surface the blocker so the caller can
            // translate to `WhatWeDid.BlockedOnClassInit blocker`; the scheduler then parks this
            // thread until `blocker` makes progress or its cctor fails. We deliberately do not
            // touch `state` (no WithTypeBeginInit, no PC advance): on wake-up the caller retries
            // the same opcode and re-enters loadClass to observe the new TypeInitTable entry.
            StateLoadResult.Blocked (state, blocker)
        | None ->
            // We have work to do!

            // Look up the concrete type from the handle
            let concreteType =
                match AllConcreteTypes.lookup ty state.ConcreteTypes with
                | Some ct -> ct
                | None -> failwith $"ConcreteTypeHandle {ty} not found in ConcreteTypes mapping"

            let sourceAssembly =
                state.LoadedAssembly concreteType.AssemblyFullName |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue concreteType.Definition.Get with
                | false, _ ->
                    failwith
                        $"Failed to find type definition {concreteType.Definition.Get} in {concreteType.AssemblyFullName}"
                | true, v -> v

            logger.LogDebug ("Resolving type {TypeDefNamespace}.{TypeDefName}", typeDef.Namespace, typeDef.Name)

            // The CLR does not eagerly run base type initializers before the current type's .cctor.
            // Base types get initialized later when their own constructors or static members are touched.
            // TODO: also need to initialise any prerequisites that the CLI genuinely requires here;
            // if so, do them *before* WithTypeBeginInit, otherwise a suspended prerequisite causes
            // retries to see "in-progress" and skip this type's own .cctor.
            let state = state.WithTypeBeginInit currentThread ty

            // Find the class constructor (.cctor) if it exists
            let cctor =
                typeDef.Methods
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && MethodInfo.arity method = 0)

            match cctor with
            | Some cctorMethod ->
                // Call the class constructor! We concretize manually and call `callMethod` directly,
                // because we're already in the middle of loading this class.
                let currentThreadState = state.ThreadState.[currentThread]

                // Convert the method's type generics from TypeDefn to ConcreteTypeHandle
                let cctorMethodWithTypeGenerics =
                    cctorMethod
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> concreteType.Generics.[par.SequenceNumber])

                // Convert method generics (should be empty for cctor)
                let cctorMethodWithMethodGenerics =
                    cctorMethodWithTypeGenerics
                    |> MethodInfo.mapMethodGenerics (fun _ -> failwith "cctor cannot be generic")

                // Convert method signature from TypeDefn to ConcreteTypeHandle using concretization
                let state, convertedSignature =
                    cctorMethodWithMethodGenerics.Signature
                    |> IlMachineState.concretizeMethodSignature
                        loggerFactory
                        baseClassTypes
                        state
                        concreteType.AssemblyFullName
                        concreteType.Generics
                        // no method generics for cctor
                        ImmutableArray.Empty

                // Convert method instructions (local variables)
                let state, convertedBody =
                    match cctorMethodWithMethodGenerics.Body with
                    | MethodBody.Il methodInstr ->
                        let state, convertedLocalVars =
                            match methodInstr.LocalVars with
                            | None -> state, None
                            | Some localVars ->
                                // Concretize each local variable type. The result is indexed by
                                // local-variable slot, so it must preserve the declaration order
                                // of `localVars`.
                                let state, convertedVars =
                                    ((state, ImmutableArray.CreateBuilder<ConcreteTypeHandle> ()), localVars)
                                    ||> Seq.fold (fun (state, acc) typeDefn ->
                                        let state, handle =
                                            IlMachineState.concretizeType
                                                loggerFactory
                                                baseClassTypes
                                                state
                                                concreteType.AssemblyFullName
                                                concreteType.Generics
                                                ImmutableArray.Empty // no method generics for cctor
                                                typeDefn

                                        acc.Add handle
                                        state, acc
                                    )
                                    |> Tuple.rmap (fun builder -> builder.ToImmutable ())

                                state, Some convertedVars

                        state, MethodBody.Il (MethodInstructions.setLocalVars convertedLocalVars methodInstr)
                    | MethodBody.InternalCall -> state, MethodBody.InternalCall
                    | MethodBody.PInvoke -> state, MethodBody.PInvoke
                    | MethodBody.RuntimeProvided rb -> state, MethodBody.RuntimeProvided rb
                    | MethodBody.Abstract -> state, MethodBody.Abstract

                let fullyConvertedMethod =
                    MethodInfo.setMethodVars convertedBody convertedSignature cctorMethodWithMethodGenerics

                callMethod
                    loggerFactory
                    baseClassTypes
                    (Some ty)
                    ConstructionState.NotConstructing
                    true
                    true
                    false
                    // constructor is surely not generic
                    ImmutableArray.Empty
                    fullyConvertedMethod
                    currentThread
                    currentThreadState
                    None
                    ReturnValueDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state = state.WithTypeEndInit currentThread ty

                NothingToDo state

    and ensureTypeInitialised
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (ty : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        match TypeInitTable.tryGet ty state.TypeInitTable with
        | None ->
            match loadClass loggerFactory baseClassTypes ty thread state with
            | NothingToDo state -> state, WhatWeDid.Executed
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
            | Blocked _ ->
                // Unreachable: we just observed `None` in the TypeInitTable, so no thread
                // (including another one) can hold the in-progress lock. The state isn't
                // mutated between the lookup and the loadClass call (single-threaded F# code).
                failwith "logic error: loadClass returned Blocked after tryGet observed no TypeInitTable entry"
        | Some TypeInitState.Initialized -> state, WhatWeDid.Executed
        | Some (TypeInitState.Failed (tieAddr, tieType)) ->
            // The .cctor for this type threw. Per ECMA-335, subsequent access should throw
            // TypeInitializationException. Rethrow the cached instance for CLR identity semantics.
            match
                ExceptionDispatching.throwExceptionObject loggerFactory baseClassTypes state thread tieAddr tieType
            with
            | ExceptionDispatchResult.Dispatched state -> state, WhatWeDid.ThrowingTypeInitializationException
            | ExceptionDispatchResult.ExceptionUnhandled _ ->
                failwith
                    "Unhandled TypeInitializationException during ensureTypeInitialised; should have been caught by a handler"
        | Some (TypeInitState.InProgress threadId) ->
            if threadId = thread then
                // II.10.5.3.2: avoid the deadlock by simply proceeding.
                state, WhatWeDid.Executed
            else
                state, WhatWeDid.BlockedOnClassInit threadId

    /// Synthesise an exception from inside the runtime itself (the host emulating the CLR),
    /// as opposed to a `throw` opcode executed by guest IL. Allocates the exception without
    /// running the exception type's .cctor, pushes its default instance constructor frame,
    /// and returns to the dispatch loop. When the ctor completes (Ret), returnStackFrame
    /// will signal DispatchException so the Ret handler can dispatch the exception.
    ///
    /// Use this for opcode-manufactured exceptions like `NullReferenceException` from a null
    /// dereference or `InvalidCastException` from a failed `castclass`. Do NOT use it for
    /// dispatching exceptions that the guest itself constructs and throws via `newobj` + `throw`
    /// — those go through `ExceptionDispatching.throwExceptionObject` and the cctor will already
    /// have run during the guest's `newobj`.
    ///
    /// All current call sites pass a non-generic BCL exception type from `BaseClassTypes`. The
    /// cctor-skip is safe for those (their cctors are trivial or empty); it would not be safe
    /// for an arbitrary guest-defined exception type, which is why this entry point is
    /// reserved for runtime use.
    ///
    /// This is a runtime boundary, not guest `newobj` semantics. It mirrors the CLR's
    /// EEException::CreateThrowable path: allocate the object directly, call the default
    /// instance ctor, then overwrite HResult.
    /// See: https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
    ///
    /// `message` overrides `_message` once the ctor has run, for the cases where the CLR
    /// would have used a message-taking ctor overload. Most callers want `None` — the CLR
    /// throws the great majority of these with no argument — and should use
    /// `raiseRuntimeException` below, which is this function with `None` supplied.
    and raiseRuntimeExceptionWithMessage
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (message : string option)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        // This is part of the `callMethod` recursion group only because it needs to call
        // `callMethod` to run the ctor, and `callMethod` needs to call it to service
        // `IntrinsicResult.RaiseException`.
        //
        // 1. Allocate the zero-initialised exception with _HResult pre-set.  This deliberately
        //    bypasses ensureTypeInitialised: opcode-manufactured exceptions are produced by the
        //    runtime rather than by guest `newobj` class-initialisation semantics.
        let addr, _exnHandle, state =
            ExceptionDispatching.allocateRuntimeException loggerFactory baseClassTypes exceptionTypeInfo state

        // 2. Find the parameterless .ctor on the exception type.
        let assy =
            state._LoadedAssemblies.ByDefinitionName exceptionTypeInfo.AssemblyFullName

        let typeDef = assy.TypeDefs.[exceptionTypeInfo.Identity.TypeDefinition.Get]

        if not typeDef.Generics.IsEmpty then
            failwith
                $"raiseRuntimeException: expected non-generic exception type, but %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} has %i{typeDef.Generics.Length} generic parameter(s)"

        let ctor =
            typeDef.Methods
            |> List.tryFind (fun method -> method.Name = ".ctor" && not method.IsStatic && MethodInfo.arity method = 0)
            |> Option.defaultWith (fun () ->
                failwith
                    $"raiseRuntimeException: no parameterless .ctor found on %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name}"
            )
            // The type has no generic parameters (guarded above), so any GenericParamFromMetadata
            // in the ctor's type-generic positions is unreachable. Map them to TypeDefn to satisfy
            // concretizeMethodForExecution's signature.
            |> MethodInfo.mapTypeGenerics (fun _ ->
                failwith<TypeDefn> "raiseRuntimeException: exception type was unexpectedly generic"
            )

        // 3. Push the allocated object ref as `this` for the ctor.
        let state =
            IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) currentThread state

        // 4. Call the ctor, marking the return state so that returnStackFrame dispatches
        //    the exception instead of pushing the object onto the caller's eval stack.
        //    Do NOT advance the caller's PC: when the ctor returns and exception dispatch
        //    begins, handler lookup and the stack-trace frame must see the faulting
        //    instruction's PC, not the next instruction.  (Same class of bug as call-site
        //    vs resumed-PC for cross-frame unwinding, which CallSiteIlOpIndex solves.)
        let state, concretizedCtor, ctorDeclaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                currentThread
                ctor
                None
                None
                state

        let threadState = state.ThreadState.[currentThread]

        let state =
            callMethod
                loggerFactory
                baseClassTypes
                None
                (ConstructionState.Constructing addr) // weAreConstructingObj
                false // no interface resolution
                false // wasClassConstructor
                false // do NOT advance caller PC — dispatch needs the faulting instruction's offset
                concretizedCtor.Generics
                concretizedCtor
                currentThread
                threadState
                None
                (ReturnValueDisposition.DispatchAsException message)
                false // wrapExceptionInTargetInvocation
                state

        // 5. Discharge the ctor frame's prologue without running it, holding step 1's bypass.
        //    `callMethod` arms a type-initialisation check on every metadata callee it pushes,
        //    which for this one would run the exception type's own `.cctor` — guest code, in the
        //    middle of manufacturing a runtime exception, able to replace it with a
        //    `TypeInitializationException`.
        //
        //    Latent as it stands: no exception type this path manufactures has a `.cctor` in the
        //    CoreLib we resolve. CoreCLR reaches these through `EEException::CreateThrowable`
        //    rather than through a JIT'd prologue.
        //
        //    Checked rather than assumed, because clearing the flag off the wrong frame would
        //    silently let a `.cctor` run somewhere else instead: the frame `callMethod` just
        //    pushed must be active, and must be awaiting this very exception type.
        let threadState = state.ThreadState.[currentThread]
        let ctorFrameId = threadState.ActiveMethodState

        match threadState.MethodState.PendingTypeInit with
        | Some pending when pending = ctorDeclaringTypeHandle ->
            state
            |> IlMachineState.mapFrame currentThread ctorFrameId MethodState.clearPendingTypeInit,
            WhatWeDid.Executed
        | other ->
            failwith
                $"logic error: manufacturing %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} pushed a constructor frame whose pending type initialisation is %O{other}, not the exception's own type %O{ctorDeclaringTypeHandle}; the class-initialisation bypass cannot be applied to it"

    /// `raiseRuntimeExceptionWithMessage` with no message override, i.e. the exception is
    /// constructed exactly as `new SomeException()` would construct it. This is the right
    /// entry point wherever the CLR throws the exception with no argument, which is almost
    /// everywhere the runtime manufactures one.
    and raiseRuntimeException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        raiseRuntimeExceptionWithMessage loggerFactory baseClassTypes exceptionTypeInfo None currentThread state

    /// Result of the ECMA-335 III.4.x runtime array-store variance gate.
    [<RequireQualifiedAccess>]
    type ArrayStoreVarianceCheck =
        /// The store may proceed. The state may have been updated as a side effect of
        /// the assignability walk (which may concretize additional metadata), so the
        /// caller must use the state carried here, not its pre-check state.
        | Allowed of state : IlMachineState
        /// The store was rejected as covariance-incompatible; `ArrayTypeMismatchException`
        /// has been raised on the current thread. The caller must return
        /// `(state, WhatWeDid.Executed)` immediately without advancing PC: exception
        /// dispatch needs the faulting instruction's offset.
        | Raised of state : IlMachineState

    /// ECMA-335 III.4.x runtime-assignment-compatibility gate for `stelem` /
    /// runtime-synthesized `T[<rank>]::Set`. For reference-typed array elements, the
    /// value's runtime type must be assignment-compatible with the array's stored
    /// element type; otherwise raise `ArrayTypeMismatchException`. Null is always
    /// storable. Value-typed-element arrays bypass the gate: there is no covariance
    /// for value types, the verifier rejects mismatching value-store opcodes at
    /// load time, and primitive coercion is handled by `EvalStackValue.toCliTypeCoerced`.
    let checkArrayStoreVariance
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (arrayAddress : ManagedHeapAddress)
        (value : EvalStackValue)
        (state : IlMachineState)
        : ArrayStoreVarianceCheck
        =
        let arrayObj =
            match ManagedHeap.tryGetArrayShape arrayAddress state.ManagedHeap with
            | Some v -> v
            | None ->
                failwith
                    $"checkArrayStoreVariance: no array allocation at %O{arrayAddress}; helper called with a non-array heap address"

        let storedElement =
            match arrayObj.ConcreteType with
            | ConcreteTypeHandle.OneDimArrayZero elt -> elt
            | ConcreteTypeHandle.Array (elt, _) -> elt
            | other ->
                failwith
                    $"checkArrayStoreVariance: array allocation at %O{arrayAddress} has non-array ConcreteType %O{other}"

        let storedElementIsReference =
            IlMachineState.isReferenceTypeHandle baseClassTypes "checkArrayStoreVariance" state storedElement

        if not storedElementIsReference then
            // Value-type element store: variance does not apply. Numeric coercion
            // happens in toCliTypeCoerced; the verifier guards value-type identity.
            ArrayStoreVarianceCheck.Allowed state
        else

        match value with
        | EvalStackValue.NullObjectRef ->
            // Null is always storable into a reference-typed array slot.
            ArrayStoreVarianceCheck.Allowed state
        | EvalStackValue.ObjectRef addr ->
            let valueRuntimeType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            let state, isAssignable =
                IlMachineState.isConcreteTypeAssignableTo
                    loggerFactory
                    baseClassTypes
                    state
                    valueRuntimeType
                    storedElement

            if isAssignable then
                ArrayStoreVarianceCheck.Allowed state
            else
                let state, _whatWeDid =
                    raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.ArrayTypeMismatchException
                        currentThread
                        state

                ArrayStoreVarianceCheck.Raised state
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.NativeInt _
        | EvalStackValue.Float _
        | EvalStackValue.UserDefinedValueType _ ->
            // Reference-typed-element arrays only accept ObjectRef / NullObjectRef stack
            // values. The verifier rejects other shapes at load time, so reaching this
            // arm means either the verifier was skipped or the interpreter produced a
            // value of the wrong shape. Surface the gap explicitly rather than letting
            // the store silently mis-coerce.
            failwith
                $"TODO: array-store variance check for reference-typed-element array with stack value form %O{value}; expected ObjectRef or NullObjectRef"
