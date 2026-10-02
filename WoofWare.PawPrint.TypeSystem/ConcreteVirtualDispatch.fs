namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

/// Which method a virtual or interface call runs on a receiver of a known concrete type, as
/// CoreCLR's `MethodTable::FindDispatchImpl` decides it: the receiver's dispatch table and
/// MethodImpls, its dispatch map, default interface bodies, variance, and the SZ-array carve-out;
/// and which implementation of a static abstract interface member a `constrained.` type supplies.
/// Whatever loads an assembly or registers a concrete type on the way returns the state it leaves
/// behind; `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module ConcreteVirtualDispatch =
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
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (state : TypeSystemState)
        : (TypeSystemState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) option
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
                match TypeSystemState.tryGetConcreteTypeInfo state handle with
                | Some (_, typeInfo) -> LoadedTypeInfo.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
                | None ->
                    failwith
                        $"SZ-array interface dispatch: type argument %O{handle} of %s{MethodOwner.describe methodToCall.Owner} has no TypeDef row"

        let dispatchThroughEnumerable =
            methodToCall.RequiredDeclaringType.Identity = baseClassTypes.IEnumerableGeneric.Identity

        let state, instantiation =
            if dispatchThroughEnumerable || not (isReferenceType theT) then
                state, theT
            else
                LoadedTypeInfo.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Object
                |> TypeSystemState.concretizeType
                    loggerFactory
                    dotnetRuntimeDirs
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
            MethodConcretisation.concretizeMethodWithAllGenerics
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                ImmutableArray.Empty
                implementation
                (ImmutableArray.Create instantiation)
                state

        Some (state, meth)

    let private tryResolveVirtualImplementationForSlot
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        : TypeSystemState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        let logger = loggerFactory.CreateLogger "CallMethod"

        logger.LogDebug (
            "Identifying target of virtual call for {TypeName}.{MethodName}",
            methodToCall.RequiredDeclaringType.Name,
            methodToCall.Name
        )

        // The SZ-array carve-out runs *before* the ordinary walks, unlike CoreCLR, which reaches
        // it only after its dispatch map misses. Running first is safe and total: when the
        // receiver is an SZ array and the target is one of the five interfaces, the answer is
        // always SZArrayHelper. Nothing on the receiver's fixed class chain can supply it instead:
        // an array's only ancestors are `System.Array` and `System.Object`, neither of which lists
        // any of the five generic interfaces, so no dispatch map on the chain has an entry for them.
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
                    dotnetRuntimeDirs
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
            (state : TypeSystemState)
            : TypeSystemState * bool
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
            // `MethodTableLayout.candidateFillsSlot` requires the exact signature CoreCLR
            // requires. `skipReturnType` is how `MethodSignature::SignaturesEquivalent` expresses
            // the same latitude.
            let state, signatureMatches =
                TypeSystemState.signaturesEquivalent
                    loggerFactory
                    dotnetRuntimeDirs
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
                |> TypeSystemState.concretizeReturnColumn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    candidateAssemblyFullName
                    candidateTypeGenerics
                    methodToCall.Generics

            match candidateReturn, methodToCall.Signature.ReturnType with
            | MethodReturnType.Void, MethodReturnType.Void -> state, true
            | MethodReturnType.Returns retType, MethodReturnType.Returns targetType ->
                TypeAssignability.isConcreteTypeAssignableTo
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    retType
                    targetType
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
            (state : TypeSystemState)
            : TypeSystemState * bool
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
            (state : TypeSystemState)
            : (WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * bool) option *
              TypeSystemState
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
            (state : TypeSystemState)
            : TypeSystemState * ImmutableArray<ConcreteTypeHandle>
            =
            ((state, ImmutableArray.CreateBuilder<ConcreteTypeHandle> ()), args)
            ||> Seq.fold (fun (state, acc) ty ->
                let state, handle =
                    TypeSystemState.concretizeType
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        declaringAssemblyFullName
                        contextTypeGenerics
                        methodGenerics
                        ty

                acc.Add handle
                state, acc
            )
            |> fun (state, builder) -> state, builder.ToImmutable ()

        let concreteTypeHandlesToTypeDefns
            (state : TypeSystemState)
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
            (state : TypeSystemState)
            : TypeSystemState *
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
                    MemberReferenceInstantiation.resolveMemberWithGenerics
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
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
            (state : TypeSystemState)
            : TypeSystemState *
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
                        (state : TypeSystemState)
                        (identity : ResolvedTypeIdentity)
                        (ns : string)
                        (name : string)
                        (generics : ImmutableArray<ConcreteTypeHandle>)
                        : TypeSystemState * ConcreteTypeHandle
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

                            let state, matches =
                                TypeAssignability.isConcreteTypeAssignableTo
                                    loggerFactory
                                    dotnetRuntimeDirs
                                    baseClassTypes
                                    state
                                    fromH
                                    toH

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
            (state : TypeSystemState)
            : TypeSystemState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            =
            let typeGenerics =
                AllConcreteTypes.lookup implementationTypeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Implementation declaring type handle %O{implementationTypeHandle} was not registered while concretizing %s{MethodOwner.describe implementation.Owner}::%s{implementation.Name}"
                )
                |> _.Generics

            let state, meth, _ =
                MethodConcretisation.concretizeMethodWithAllGenerics
                    loggerFactory
                    dotnetRuntimeDirs
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
            (state : TypeSystemState)
            : TypeSystemState * (ConcreteTypeHandle * ResolvedTypeIdentity) list option
            =
            let rec go state handle acc =
                match TypeSystemState.tryGetConcreteTypeInfo state handle with
                | None -> state, None
                | Some (ty, _) ->
                    let acc = (handle, ty.Identity) :: acc

                    let state, baseHandle =
                        TypeSystemState.resolveBaseConcreteType
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            handle

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
            (state : TypeSystemState)
            : TypeSystemState *
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
                ConcreteMethodTable.dispatchTableOfClosed
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    "callvirt"
                    state
                    dispatchTypeHandle

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

        /// Answer an instance interface call from the dispatch map of the receiver's chain, then read
        /// the slot it names from the receiver's own method table.
        ///
        /// `None` means no type on the chain maps the method -- only a default interface body can
        /// answer -- or, when `walkBaseTypes` is false, that the receiver does not supply the body
        /// itself.
        let tryResolveByInterfaceDispatchMap
            (state : TypeSystemState)
            : TypeSystemState *
              (ConcreteTypeHandle *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
              string) option
            =
            let interfaceMethod =
                match methodToCall.TryMetadata with
                | Some metadata -> methodToCall.DeclaringAssemblyFullName, (Some metadata.Handle, None)
                | None ->
                    failwith
                        $"TODO: interface dispatch to synthesised method %s{methodToCall.Name} on %O{methodToCall.RequiredDeclaringType.Name}, which has no MethodDef row to key the dispatch map on"

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

            let state, slot =
                ConcreteInterfaceDispatch.tryFindImplementationSlot
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    "callvirt"
                    state
                    dispatchTypeHandle
                    walkBaseTypes
                    targetHandle
                    interfaceMethod

            match slot with
            | None -> state, None
            | Some slot ->

            let state, table =
                ConcreteMethodTable.dispatchTableOfClosed
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    "callvirt"
                    state
                    dispatchTypeHandle

            let occupant =
                match table with
                | Some table when slot >= 0 && slot < table.Occupants.Length -> table.Occupants.[slot]
                | _ ->
                    // A slot some type on the chain mapped is within the receiver's table, because slot
                    // numbers are prefix-stable down the chain.
                    failwith
                        $"interface dispatch of %s{methodToCall.Name}: the dispatch map names slot %i{slot}, which the receiver %O{dispatchTypeHandle}'s method table does not have"

            // The instantiation the receiver supplies for the occupant's declaring type. A synthesised
            // array has no TypeDef row of its own, so it is stepped over to `System.Array`.
            let rec declaringHandleOnChain (state : TypeSystemState) (level : ConcreteTypeHandle) =
                let matches =
                    match TypeSystemState.tryGetConcreteTypeInfo state level with
                    | Some (levelTy, _) -> levelTy.Identity = occupant.DeclaredBy.Identity
                    | None -> false

                if matches then
                    state, level
                else
                    let state, baseType =
                        TypeSystemState.resolveBaseConcreteType
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            level

                    match baseType with
                    | Some baseType -> declaringHandleOnChain state baseType
                    | None ->
                        failwith
                            $"interface dispatch of %s{methodToCall.Name}: slot %i{slot} of %O{dispatchTypeHandle} holds a method of %s{occupant.DeclaredBy.Description}, which is not on the receiver's chain"

            let state, implementationHandle = declaringHandleOnChain state dispatchTypeHandle

            if not walkBaseTypes && implementationHandle <> dispatchTypeHandle then
                // The exact-type probe asks whether the receiver supplies the body itself; an
                // inherited one means it does not.
                state, None
            else
                state,
                Some (implementationHandle, occupant.Method, "Found interface implementation through the dispatch map")

        let findClassImplementation (state : TypeSystemState) : TypeSystemState * _ option =
            // Resolution precedence: explicit MethodImpl entries, then method name/signature
            // matches on the current type, then the base type walk when enabled.
            let rec walkBase (state : TypeSystemState) (currentTypeHandle : ConcreteTypeHandle) =
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
                            TypeSystemState.resolveBaseConcreteType
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                currentTypeHandle

                        match baseType with
                        | None -> state, None
                        | Some baseType -> walk state baseType

            and walk (state : TypeSystemState) (currentTypeHandle : ConcreteTypeHandle) =
                match TypeSystemState.tryGetConcreteTypeInfo state currentTypeHandle with
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
            | None when methodDeclaringType.IsInterface && not methodToCall.IsStatic ->
                tryResolveByInterfaceDispatchMap state
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
            ConcreteInterfaceDispatch.resolveImplementedInterface loggerFactory dotnetRuntimeDirs baseClassTypes

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
            (state : TypeSystemState)
            : TypeSystemState *
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
            (state : TypeSystemState)
            (visited : Set<ConcreteTypeHandle>)
            (currentTypeHandle : ConcreteTypeHandle)
            (currentTy : ConcreteType<ConcreteTypeHandle>)
            (currentTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : TypeSystemState *
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
            (state : TypeSystemState)
            : TypeSystemState *
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
            (state : TypeSystemState)
            (visited : Set<ConcreteTypeHandle>)
            (currentTypeHandle : ConcreteTypeHandle)
            : TypeSystemState *
              (ConcreteTypeHandle *
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) list
            =
            if visited.Contains currentTypeHandle then
                state, []
            else
                let visited = visited.Add currentTypeHandle

                let state, ownCandidates =
                    match TypeSystemState.tryGetConcreteTypeInfo state currentTypeHandle with
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
                                TypeSystemState.resolveBaseConcreteType
                                    loggerFactory
                                    dotnetRuntimeDirs
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
            (state : TypeSystemState)
            (interfaceHandle : ConcreteTypeHandle)
            (candidates :
                (ConcreteTypeHandle *
                WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) list)
            : TypeSystemState * bool
            =
            match candidates with
            | [] -> state, false
            | (otherInterfaceHandle, _) :: remaining ->
                if otherInterfaceHandle = interfaceHandle then
                    hasMoreSpecificInterfaceImplementation state interfaceHandle remaining
                else
                    let state, otherIsMoreSpecific =
                        TypeAssignability.isConcreteTypeAssignableTo
                            loggerFactory
                            dotnetRuntimeDirs
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
            |> fun (state, acc) -> state, List.rev acc

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

    /// One entry of a receiver's interface map, as the search for a variance-compatible default
    /// body visits it.
    type private InterfaceSearchEntry =
        {
            Handle : ConcreteTypeHandle
            Type : ConcreteType<ConcreteTypeHandle>
        }

    /// One interface, followed by its transitive parents, depth-first. `visited` collapses
    /// diamonds at the *first* occurrence; `variantInterfaceMapRetargets` depends on the
    /// resulting order.
    let rec private expandInterfaceEntry
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (visited : Set<ConcreteTypeHandle>)
        (ifaceHandle : ConcreteTypeHandle)
        (ifaceTy : ConcreteType<ConcreteTypeHandle>)
        (ifaceTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * Set<ConcreteTypeHandle> * InterfaceSearchEntry list
        =
        if visited.Contains ifaceHandle then
            state, visited, []
        else

        let visited = visited.Add ifaceHandle

        let state, visited, parents =
            ((state, visited, []), ifaceTypeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun (state, visited, acc) impl ->
                let state, parentHandle, parentTy, parentTypeInfo =
                    ConcreteInterfaceDispatch.resolveImplementedInterface
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        ifaceTy
                        impl
                        state

                let state, visited, expanded =
                    expandInterfaceEntry
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
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
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        (visited : Set<ConcreteTypeHandle>)
        (typeHandle : ConcreteTypeHandle)
        : TypeSystemState * Set<ConcreteTypeHandle> * InterfaceSearchEntry list
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
                TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state typeHandle

            match baseType with
            | None -> state, visited, []
            | Some baseType ->
                collectInterfaceMap loggerFactory dotnetRuntimeDirs baseClassTypes walkBaseTypes state visited baseType

        let state, visited, ownEntries =
            match TypeSystemState.tryGetConcreteTypeInfo state typeHandle with
            | None -> state, visited, []
            | Some (ty, typeInfo) ->
                ((state, visited, []), typeInfo.ImplementedInterfaces)
                ||> Seq.fold (fun (state, visited, acc) impl ->
                    let state, ifaceHandle, ifaceTy, ifaceTypeInfo =
                        ConcreteInterfaceDispatch.resolveImplementedInterface
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            ty
                            impl
                            state

                    let state, visited, expanded =
                        expandInterfaceEntry
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            visited
                            ifaceHandle
                            ifaceTy
                            ifaceTypeInfo

                    state, visited, acc @ expanded
                )

        state, visited, ownEntries @ baseEntries

    /// The interfaces in `receiverType`'s interface map, its base types' included, in the order
    /// CoreCLR's interface map lists them.
    let interfaceMapHandles
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (receiverType : ConcreteTypeHandle)
        : TypeSystemState * ConcreteTypeHandle list
        =
        let state, _, entries =
            collectInterfaceMap loggerFactory dotnetRuntimeDirs baseClassTypes true state Set.empty receiverType

        state, entries |> List.map _.Handle

    /// ECMA-335 §I.8.7 lets a call site name a variance-compatible instantiation of an interface
    /// the receiver never declares: `ISink<in T>` implemented at `ISink<object>` is dispatched
    /// through `ISink<string>`. A class implementation reached that way is found by the dispatch
    /// map, which is variance-aware itself; this serves the fallback after it, where only a
    /// default interface body can answer, and returns the call target retargeted onto each of the
    /// receiver's own variance-compatible entries, so that the default-body search can run against
    /// each in turn.
    ///
    /// The entries come in the order `MethodTable::FindDefaultInterfaceImplementation` searches
    /// them, and the *first* that yields a body wins with no ambiguity exception: that function
    /// takes the first candidate and "[doesn't] look for a conflict for instance methods" once
    /// `allowVariance` is set. `sourcesPure/VariantInterfaceDefaultBodyPrecedence.cs` pins that
    /// against the real runtime, and it runs only after the exact instantiation's own default body
    /// has been looked for, which is `FindDispatchImpl`'s order.
    ///
    /// The rule is deliberately restricted to *instance* methods; see the `methodToCall.IsStatic`
    /// guard below for why static interface members neither need nor may use this path.
    ///
    /// Returns `[]` when no such entry exists, leaving the caller's answer unchanged.
    let private variantInterfaceMapRetargets
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        : TypeSystemState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
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
            collectInterfaceMap
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                walkBaseTypes
                state
                Set.empty
                dispatchTypeHandle

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
                    TypeAssignability.isConcreteTypeAssignableTo
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        entry.Handle
                        targetHandle

                if isCompatible then state, acc @ [ entry ] else state, acc
            )

        ((state, []), compatible)
        ||> List.fold (fun (state, acc) entry ->
            let chosenTy = entry.Type

            match TypeSystemState.tryGetConcreteTypeInfo state entry.Handle with
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
                    MethodConcretisation.concretizeMethodWithAllGenerics
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        chosenTy.Generics
                        slot
                        methodGenerics
                        state

                state, acc @ [ retargeted ]
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
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        : TypeSystemState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        let state, primary =
            tryResolveVirtualImplementationForSlot
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                methodGenerics
                methodToCall
                dispatchTypeHandle
                walkBaseTypes
                state

        match primary with
        | Some _ -> state, primary
        | None ->

        // Nothing implements the call site's own instantiation, not even a default body. A
        // variance-compatible entry's default body is what is left: `FindDispatchImpl` tries the
        // exact default before allowing variance.
        let state, retargets =
            variantInterfaceMapRetargets
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                methodGenerics
                methodToCall
                dispatchTypeHandle
                walkBaseTypes
                state

        let isDefaultInterfaceBody
            (state : TypeSystemState)
            (meth : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            : bool
            =
            state
                .LoadedAssembly(meth.DeclaringAssemblyFullName)
                .Value.TypeDefs.[meth.RequiredDeclaringType.Definition.Get].IsInterface

        let rec firstResolved
            (state : TypeSystemState)
            (retargets : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
            =
            match retargets with
            | [] -> state, None
            | retargeted :: rest ->
                let state, resolved =
                    tryResolveVirtualImplementationForSlot
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        methodGenerics
                        retargeted
                        dispatchTypeHandle
                        walkBaseTypes
                        state

                match resolved with
                | None -> firstResolved state rest
                | Some resolved when isDefaultInterfaceBody state resolved ->
                    let logger = loggerFactory.CreateLogger "CallMethod"

                    logger.LogDebug (
                        "Retargeting variant interface call {DeclaringTypeName}::{MethodName} to the default body for the receiver's own instantiation {Generics}",
                        methodToCall.RequiredDeclaringType.Name,
                        methodToCall.Name,
                        retargeted.DeclaringTypeGenerics
                    )

                    state, Some resolved
                | Some resolved ->
                    // The dispatch map's variance pass already considered every entry this could
                    // have come from, so a class implementation here means the two disagree.
                    failwith
                        $"variant interface dispatch of %s{methodToCall.Name}: the dispatch map found no class implementation through any variance-compatible entry, but retargeting onto %O{retargeted.DeclaringTypeGenerics} found %s{MethodOwner.describe resolved.Owner}::%s{resolved.Name}"

        firstResolved state retargets

    /// Resolve a `constrained.`-prefixed reference to a static abstract interface member down to
    /// the implementation the constrained type supplies, returning it alongside its declaring
    /// type's handle.
    ///
    /// Shared by `constrained. call` and `constrained. ldftn`, which pick their target the same
    /// way: CoreCLR routes both through `getCallInfo` with the constrained token, and the switch
    /// there is `pConstrainedResolvedToken != NULL && pMD->IsInterface() && pMD->IsStatic()`
    /// (`jitinterface.cpp`, `getCallInfo`). That test is computed before anything branches on
    /// `CORINFO_CALLINFO_LDFTN`, so the *method chosen* cannot differ between the two opcodes;
    /// what differs afterwards is only what the caller does with it.
    ///
    /// `opName` names the prefixed instruction (`constrained.call` / `Ldftn`), so a failure says
    /// which one hit it rather than always blaming `call`.
    ///
    /// The instance-receiver forms of the prefix (`CORINFO_DEREF_THIS` / `CORINFO_BOX_THIS`) are
    /// not implemented: Roslyn emits `constrained.` before `ldftn` only for static
    /// abstract interface members, and before `call`/`callvirt` the instance cases are handled by
    /// `executeCallvirt`'s own transformation. Anything else fails loudly here rather than being
    /// guessed at.
    let resolveConstrainedStaticInterfaceMethod
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (constrainedTypeHandle : ConcreteTypeHandle)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        let methodDeclAssy =
            state._LoadedAssemblies.ByDefinitionName methodToCall.DeclaringAssemblyFullName

        let methodDeclType =
            methodDeclAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        if not methodToCall.IsStatic || not methodDeclType.IsInterface then
            failwith
                $"%s{opName}: expected a static interface method, got %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name}"

        match constrainedTypeHandle with
        | ConcreteTypeHandle.Concrete _ ->
            // Registration is checked eagerly, and separately from rendering: an unregistered
            // handle would otherwise surface as a confusing resolution failure below rather than
            // as the bookkeeping error it is.
            if (AllConcreteTypes.lookup constrainedTypeHandle state.ConcreteTypes).IsNone then
                failwith $"%s{opName}: constrained type handle %O{constrainedTypeHandle} is not registered"
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"%s{opName}: static interface dispatch for non-concrete constrained type %O{constrainedTypeHandle} is not implemented"

        let state, implementation =
            tryResolveVirtualImplementation
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                concretizedMethod.Generics
                concretizedMethod
                constrainedTypeHandle
                true
                state

        match implementation with
        | None ->
            let constrained =
                AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes constrainedTypeHandle

            failwith $"%s{opName}: could not find static implementation of %s{methodToCall.Name} on %s{constrained}"
        | Some implementation when not implementation.IsStatic ->
            failwith
                $"%s{opName}: resolved non-static implementation %s{MethodOwner.describe implementation.Owner}::%s{implementation.Name}"
        | Some implementation ->
            let declaringTypeHandle =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    implementation.RequiredDeclaringType.Identity
                    implementation.DeclaringTypeGenerics
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{opName}: resolved implementation declaring type %s{MethodOwner.describe implementation.Owner} is not registered"
                )

            state, implementation, declaringTypeHandle
