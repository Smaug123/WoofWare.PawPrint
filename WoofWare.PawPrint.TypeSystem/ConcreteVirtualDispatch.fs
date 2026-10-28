namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

/// A method a call runs, as its definition and the generic arguments it runs with, before
/// `MethodConcretisation.concretizeMethodWithAllGenerics` instantiates it.
type DispatchedMethod =
    {
        /// The method's definition.
        Definition : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        /// Its declaring type's generic arguments.
        TypeGenerics : ImmutableArray<ConcreteTypeHandle>
        /// Its own generic arguments.
        MethodGenerics : ImmutableArray<ConcreteTypeHandle>
    }

/// The body a virtual or interface call lands on, given the receiver's runtime type.
[<RequireQualifiedAccess>]
type VirtualImplementation =
    /// This method, not yet instantiated: CoreCLR reads a method's locals only when it compiles
    /// the method, so choosing it does not.
    | Found of DispatchedMethod
    /// Nothing overrides the method the call names, which for a `callvirt` means that method runs.
    /// Never the answer for an interface method on a type that does not implement it: that is
    /// `NotImplemented`.
    | NotOverridden
    /// Nothing implements the interface method on the type: no type on its chain maps it, no
    /// default body for it is found among the interfaces the type implements, and, for a static
    /// virtual, the method has no body of its own. This is the answer where the type does not
    /// implement the interface at all, which no verifiable IL reaches. An instance call throws
    /// `EntryPointNotFoundException`, as `DispatchFailure.NotImplemented` says, and a static one
    /// `AmbiguousImplementationException`, as `DispatchFailure.StaticNotImplemented` says.
    | NotImplemented
    /// More than one default interface body is most specific for the method, so the call throws
    /// `AmbiguousImplementationException` (`MethodTable::FindDefaultInterfaceImplementation`,
    /// methodtable.cpp, through `ThrowAmbiguousResolutionException`). These are the candidates.
    | Ambiguous of WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list
    /// The most specific default interface body is this abstract MethodImpl: an interface more
    /// specific than the one declaring the method *reabstracts* it, as `IBar : IFoo` declaring
    /// `abstract int IFoo.Frob();` over `IFoo`'s default body does. The call throws
    /// `EntryPointNotFoundException` (`MethodTable::FindDispatchImpl`, methodtable.cpp, through
    /// `ThrowEntryPointNotFoundException`).
    | Reabstracted of WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
    /// The receiver's default interface bodies decide the call in a way this does not model, for
    /// the reason given: a conflict at a variant interface's exact instantiation, where whether
    /// CoreCLR throws depends on how the JIT compiled the call.
    | Unmodelled of reason : string

/// Why a virtual call has no method to run, and throws instead of calling anything.
[<RequireQualifiedAccess>]
type DispatchFailure =
    /// The most specific default body is this reabstraction, as for
    /// `VirtualImplementation.Reabstracted`: the call throws `EntryPointNotFoundException`.
    | Reabstracted of WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
    /// These default bodies are equally specific, as for `VirtualImplementation.Ambiguous`: the call
    /// throws `AmbiguousImplementationException`.
    | Ambiguous of WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list
    /// Nothing implements this instance interface method on the receiver's type, as for
    /// `VirtualImplementation.NotImplemented`: virtual stub dispatch throws
    /// `EntryPointNotFoundException` (`VirtualCallStubManager::Resolver`, virtualcallstub.cpp),
    /// even where the method has a body of its own, an interface the receiver does not implement
    /// being none that `MethodTable::FindDispatchImpl` searches for one.
    | NotImplemented of WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    /// Nothing implements this static abstract interface method on the type a `constrained.`
    /// prefix names, as for `VirtualImplementation.NotImplemented`: `CEEInfo::getCallInfo`
    /// (jitinterface.cpp) compiles the call, or the `ldftn`, to throw
    /// `AmbiguousImplementationException`, as it does for every static virtual it cannot resolve
    /// to a body but a reabstraction, and in shared code the stub the generic dictionary holds
    /// throws the same when called.
    | StaticNotImplemented of WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

[<RequireQualifiedAccess>]
module DispatchFailure =
    /// The exception the call throws.
    let exceptionType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (failure : DispatchFailure)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match failure with
        | DispatchFailure.Reabstracted _
        | DispatchFailure.NotImplemented _ -> baseClassTypes.EntryPointNotFoundException
        | DispatchFailure.Ambiguous _
        | DispatchFailure.StaticNotImplemented _ -> baseClassTypes.AmbiguousImplementationException

    /// The failure in words, for a refusal that names it.
    let describe (failure : DispatchFailure) : string =
        let name (m : WoofWare.PawPrint.MethodInfo<_, _, _>) =
            $"%s{MethodOwner.describe m.Owner}::%s{m.Name}"

        match failure with
        | DispatchFailure.Reabstracted reabstraction -> $"the reabstraction %s{name reabstraction}"
        | DispatchFailure.Ambiguous candidates ->
            let described = candidates |> List.map name |> String.concat ", "
            $"the ambiguous default bodies %s{described}"
        | DispatchFailure.NotImplemented method -> $"no implementation of %s{name method}"
        | DispatchFailure.StaticNotImplemented method -> $"no implementation of the static %s{name method}"

/// Which method a virtual or interface call runs on a receiver of a known concrete type, as
/// CoreCLR's `MethodTable::FindDispatchImpl` decides it: the receiver's dispatch table and
/// MethodImpls, its dispatch map, default interface bodies, variance, and the SZ-array carve-out.
/// A static virtual is dispatched by `StaticVirtualDispatch` instead.
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
        : (TypeSystemState * Result<DispatchedMethod, TypeBindingFailure>) option
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
                state, Ok theT
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
                |> fun (state, objectHandle) ->
                    match objectHandle with
                    | Error failure -> state, Error failure
                    | Ok objectHandle -> state, Ok objectHandle

        // `this` is the array, not an SZArrayHelper — exactly the lie CoreCLR tells (see the
        // "! Warning: \"this\" is an array, not an SZArrayHelper" comments in
        // `Array.CoreCLR.cs`). It survives our calling convention because `SZArrayHelper` is a
        // reference type, so `callMethod`'s `thisArgCoercionTarget` yields `CliType.ObjectRef`,
        // whose coercion passes the object reference through without a type check.
        Some (
            state,
            instantiation
            |> Result.map (fun instantiation ->
                {
                    Definition = implementation
                    TypeGenerics = ImmutableArray.Empty
                    MethodGenerics = ImmutableArray.Create instantiation
                }
            )
        )

    let private tryResolveVirtualImplementationForSlot
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        : TypeSystemState * Result<VirtualImplementation, TypeBindingFailure>
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

            state, impl |> Result.map VirtualImplementation.Found
        | None ->

        let declaringAssy =
            state.LoadedAssembly(methodToCall.DeclaringAssemblyFullName).Value

        let methodDeclaringType =
            declaringAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        let signatureMatchesTarget
            (candidateAssemblyFullName : string)
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (candidateSignature : TypeMethodSignature<TypeDefn>)
            (state : TypeSystemState)
            : TypeSystemState * Result<bool, TypeBindingFailure>
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
            match
                TypeSystemState.signaturesEquivalent
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    true
                    candidateComparand
                    targetComparand
            with
            | state, Error failure -> state, Error failure
            | state, Ok false -> state, Ok false
            | state, Ok true ->

            match
                candidateSignature.ReturnType
                |> TypeSystemState.concretizeReturnColumn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    candidateAssemblyFullName
                    candidateTypeGenerics
                    methodToCall.Generics
            with
            | state, Error failure -> state, Error failure
            | state, Ok candidateReturn ->

            match candidateReturn, methodToCall.Signature.ReturnType with
            | MethodReturnType.Void, MethodReturnType.Void -> state, Ok true
            | MethodReturnType.Returns retType, MethodReturnType.Returns targetType ->
                TypeAssignability.isConcreteTypeAssignableTo
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    retType
                    targetType
            | MethodReturnType.Void, MethodReturnType.Returns _
            | MethodReturnType.Returns _, MethodReturnType.Void -> state, Ok false

        /// Whether `meth`, a method of a class on the receiver's chain, overrides the target, a class's
        /// virtual method, by name and signature.
        let methodMatches
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (meth : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            (state : TypeSystemState)
            : TypeSystemState *
              Result<
                  WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option,
                  TypeBindingFailure
               >
            =
            if
                meth.Signature.GenericParameterCount
                <> methodToCall.Signature.GenericParameterCount
                || meth.Signature.RequiredParameterCount
                   <> methodToCall.Signature.RequiredParameterCount
            then
                state, Ok None
            elif meth.Name <> methodToCall.Name then
                state, Ok None
            elif
                not meth.IsVirtual
                || (meth.IsNewSlot && not (MethodInfo.sameDeclaredMethod meth methodToCall))
            then
                state, Ok None
            else

            let state, matches =
                signatureMatchesTarget meth.DeclaringAssemblyFullName candidateTypeGenerics meth.Signature state

            state, matches |> Result.map (fun matches -> if matches then Some meth else None)

        let concretizeTypeArgs
            (declaringAssemblyFullName : string)
            (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (args : TypeDefn ImmutableArray)
            (state : TypeSystemState)
            : TypeSystemState * Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure>
            =
            (state, [], args)
            |||> StateResult.fold (fun state acc ty ->
                match
                    TypeSystemState.concretizeType
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        declaringAssemblyFullName
                        contextTypeGenerics
                        methodGenerics
                        ty
                with
                | state, Error failure -> state, Error failure
                | state, Ok handle -> state, Ok (handle :: acc)
            )
            |> fun (state, handles) -> state, handles |> Result.map (List.rev >> ImmutableArray.CreateRange)

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

        /// The method a MethodImpl row of `owner` declares an override of, with the type arguments
        /// a MemberRef declaration spells for its parent.
        let resolveMethodImplDeclaration
            (owner : ResolvedTypeIdentity)
            (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (relativeAssembly : DumpedAssembly)
            (token : MetadataToken)
            (state : TypeSystemState)
            : TypeSystemState *
              Result<
                  WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn> *
                  TypeDefn ImmutableArray option,
                  TypeBindingFailure
               >
            =
            match token with
            | MetadataToken.MethodDef h ->
                let method =
                    relativeAssembly.Methods.[h]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                state, Ok (method, None)
            | MetadataToken.MemberReference h ->
                let contextTypeGenerics = concreteTypeHandlesToTypeDefns state contextTypeGenerics
                let contextMethodGenerics = concreteTypeHandlesToTypeDefns state methodGenerics

                let state, resolved =
                    MemberReferenceInstantiation.resolveMemberWithGenerics
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        relativeAssembly
                        contextTypeGenerics
                        contextMethodGenerics
                        h
                        state

                match resolved with
                | Error MemberBindingFailure.NoSuchMethod ->
                    let miss : MethodImplDeclarationMiss =
                        {
                            Type = owner
                            Declaration = h
                            Description = MemberReferenceInstantiation.rowDescription relativeAssembly h
                        }

                    state, Error (TypeBindingFailure.MethodImplDeclarationAbsent miss)
                | Error (MemberBindingFailure.TypeUnbound failure) -> state, Error failure
                | Error (MemberBindingFailure.Unmodelled unmodelled) ->
                    state, Error (TypeBindingFailure.Unmodelled unmodelled)
                | Ok resolved ->

                match resolved.Member with
                | Choice1Of2 method -> state, Ok (method, Some resolved.TargetTypeGenerics)
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

        /// The bodies of `currentTy`'s MethodImpls whose declaration is the target, at the target's own
        /// instantiation.
        let findMatchingMethodImplBodies
            (currentTy : ConcreteType<ConcreteTypeHandle>)
            (currentTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (state : TypeSystemState)
            : TypeSystemState *
              Result<
                  WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list,
                  TypeBindingFailure
               >
            =
            let currentAssy =
                state._LoadedAssemblies.ByDefinitionName currentTy.Identity.AssemblyFullName

            (state, [], currentTypeInfo.MethodImpls.Values)
            |||> StateResult.fold (fun state acc impl ->
                if not (methodImplDeclarationCouldMatch currentAssy impl.Declaration) then
                    state, Ok acc
                else
                    match
                        resolveMethodImplDeclaration
                            currentTy.Identity
                            currentTy.Generics
                            currentAssy
                            impl.Declaration
                            state
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok (declaration, declarationTypeArgs) ->

                    let state, declarationTypeGenerics =
                        match declarationTypeArgs with
                        | Some typeArgs ->
                            concretizeTypeArgs declaration.DeclaringAssemblyFullName currentTy.Generics typeArgs state
                        | None when declaration.DeclaringTypeGenerics.IsEmpty -> state, Ok ImmutableArray.Empty
                        | None when declaration.RequiredDeclaringType.Identity = currentTy.Identity ->
                            state, Ok currentTy.Generics
                        | None ->
                            failwith
                                $"MethodImpl declaration for %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} referenced generic MethodDef %s{declaration.Name} without concrete type arguments"

                    match declarationTypeGenerics with
                    | Error failure -> state, Error failure
                    | Ok declarationTypeGenerics ->

                    // A MethodImpl binds a Body to the specific virtual slot identified by its
                    // Declaration: ECMA-335 II.22.27 keys the slot on (declaring type, member).
                    // Name + signature alone is not enough — two unrelated interfaces can share
                    // a shape (e.g. `IReader.Read()` and `IScanner.Read()`), so we also require
                    // the declaration's declaring type to be the dispatch target's, at the same
                    // instantiation. Only the class walk asks, and only of a target declared on a
                    // class, to which no variance applies.
                    if
                        declaration.RequiredDeclaringType.Identity
                        <> methodToCall.RequiredDeclaringType.Identity
                        || declarationTypeGenerics <> methodToCall.DeclaringTypeGenerics
                        || declaration.Name <> methodToCall.Name
                    then
                        state, Ok acc
                    else

                    match
                        signatureMatchesTarget
                            declaration.DeclaringAssemblyFullName
                            declarationTypeGenerics
                            declaration.Signature
                            state
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok false -> state, Ok acc
                    | state, Ok true ->
                        match impl.Body with
                        | MetadataToken.MethodDef body -> state, Ok (currentAssy.Methods.[body] :: acc)
                        | other ->
                            failwith
                                $"MethodImpl body for %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} was not a MethodDef: %O{other}"
            )

        let dispatchedOn
            (implementationTypeHandle : ConcreteTypeHandle)
            (implementation : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            (state : TypeSystemState)
            : DispatchedMethod
            =
            let typeGenerics =
                AllConcreteTypes.lookup implementationTypeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Implementation declaring type handle %O{implementationTypeHandle} was not registered while dispatching to %s{MethodOwner.describe implementation.Owner}::%s{implementation.Name}"
                )
                |> _.Generics

            {
                Definition = implementation
                TypeGenerics = typeGenerics
                MethodGenerics = methodGenerics
            }

        /// The receiver's class chain, most-derived first, as `(handle, identity)`.
        ///
        /// Needed because the slot table names its occupant's declaring type by
        /// `ResolvedTypeIdentity`, while concretising a method needs that type's `ConcreteTypeHandle`
        /// -- the instantiation the receiver actually supplies. `None` means some link is not a
        /// registered nominal type, which is the signal to fall back: a structural receiver has no
        /// class chain to walk. A base type on the chain that does not bind is the failure.
        let concreteChainOfReceiver
            (state : TypeSystemState)
            : TypeSystemState * Result<(ConcreteTypeHandle * ResolvedTypeIdentity) list option, TypeBindingFailure>
            =
            let rec go state handle acc =
                match TypeSystemState.tryGetConcreteTypeInfo state handle with
                | None -> state, Ok None
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
                    | Error failure -> state, Error failure
                    | Ok baseHandle ->

                    match baseHandle with
                    | None -> state, Ok (Some (List.rev acc))
                    | Some baseHandle -> go state baseHandle acc

            go state dispatchTypeHandle []

        /// Answer the call the way CoreCLR does: find the slot the target declaration owns, then read
        /// that slot of the receiver's method table.
        ///
        /// `None` means the shape is outside what this serves and the caller should fall back: an
        /// interface target, whose dispatch goes through the interface map rather than a vtable index;
        /// a non-virtual target; a target with no MethodDef row; a receiver with no class
        /// chain; `walkBaseTypes = false`, which is the `constrained.` exact-type probe; a
        /// declaration owning no slot of its own declaring type; or a receiver that does not derive
        /// from the target's declaring type. A type that building the receiver's method table reaches
        /// and that does not bind is the failure.
        let tryResolveBySlotTable
            (state : TypeSystemState)
            : TypeSystemState *
              Result<
                  (ConcreteTypeHandle *
                  WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
                  string) option,
                  TypeBindingFailure
               >
            =
            if
                not walkBaseTypes
                || methodDeclaringType.IsInterface
                || not methodToCall.IsVirtual
                || methodToCall.TryMetadata.IsNone
            then
                state, Ok None
            else

            // One walk gives both halves: which slot every declaration in the receiver's chain owns,
            // and what each slot of the receiver holds. Asking the declaring type separately for the
            // first would build a second table for no gain -- slot numbers are prefix-stable, so the
            // receiver's own list already names the target's declaration at the index its declaring
            // type gave it.
            match
                ConcreteMethodTable.dispatchTableOfClosed
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    "callvirt"
                    state
                    dispatchTypeHandle
            with
            | state, Error failure -> state, Error failure
            | state, Ok None ->
                // A receiver with no method table: a byref, pointer or function pointer.
                state, Ok None
            | state, Ok (Some table) ->

            let target = methodToCall.DeclaringAssemblyFullName, methodToCall.IdentityKey

            match
                (match table.SlotOfDeclaration.TryGetValue target with
                 | true, slot -> Some slot
                 | false, _ -> None)
            with
            | None ->
                // The target owns no vtable slot anywhere on the receiver's chain -- either it holds
                // none of its own declaring type's slots, or the receiver does not derive from that
                // type at all. A `constrained.` call reaches the second: on a value type that inherits
                // the slot of an overridable method of a class it does not derive from, it boxes the
                // value type and dispatches on the box (`ConstrainedType.resolveClassMethodOnValueType`,
                // `ClassMethodOnValueType.Boxes`). The only class a value type and that class both
                // derive from is `Object`, and a slot beyond `Object`'s vtable is `Undecided` there,
                // so the target overrides one of `Object`'s virtuals by name and signature, and the
                // walk the caller falls back to finds what the box's table holds in that slot. Hand
                // the question back rather than guess.
                state, Ok None
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
                state, Ok None
            | Some occupant ->

            // The table says *which MethodDef*. Concretising it needs the instantiation the receiver
            // supplies for the type that declares it, which is that type's handle on the receiver's own
            // chain.
            match concreteChainOfReceiver state with
            | state, Error failure -> state, Error failure
            | state, Ok None -> state, Ok None
            | state, Ok (Some chain) ->

            match
                chain
                |> List.tryFind (fun (_, identity) -> identity = occupant.DeclaredBy.Identity)
            with
            | None ->
                // The occupant is declared by a type that is not on the receiver's chain, which would
                // mean the content table and the chain disagree about the receiver's ancestry.
                state, Ok None
            | Some (implementationHandle, _) ->

            state,
            Ok (
                Some (
                    implementationHandle,
                    occupant.Method,
                    "Found concrete implementation by reading the receiver's slot"
                )
            )

        /// Answer an instance interface call from the dispatch map of the receiver's chain, then read
        /// the slot it names from the receiver's own method table.
        ///
        /// `None` means no type on the chain maps the method -- only a default interface body can
        /// answer -- or, when `walkBaseTypes` is false, that the receiver does not supply the body
        /// itself.
        let tryResolveByInterfaceDispatchMap
            (state : TypeSystemState)
            : TypeSystemState *
              Result<
                  (ConcreteTypeHandle *
                  WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
                  string) option,
                  TypeBindingFailure
               >
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

            match
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
            with
            | state, Error failure -> state, Error failure
            | state, Ok None -> state, Ok None
            | state, Ok (Some slot) ->

            match
                ConcreteMethodTable.dispatchTableOfClosed
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    "callvirt"
                    state
                    dispatchTypeHandle
            with
            | state, Error failure -> state, Error failure
            | state, Ok table ->

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
                    state, Ok level
                else
                    let state, baseType =
                        TypeSystemState.resolveBaseConcreteType
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            level

                    match baseType with
                    | Error failure -> state, Error failure
                    | Ok baseType ->

                    match baseType with
                    | Some baseType -> declaringHandleOnChain state baseType
                    | None ->
                        failwith
                            $"interface dispatch of %s{methodToCall.Name}: slot %i{slot} of %O{dispatchTypeHandle} holds a method of %s{occupant.DeclaredBy.Description}, which is not on the receiver's chain"

            match declaringHandleOnChain state dispatchTypeHandle with
            | state, Error failure -> state, Error failure
            | state, Ok implementationHandle ->

            if not walkBaseTypes && implementationHandle <> dispatchTypeHandle then
                // The exact-type probe asks whether the receiver supplies the body itself; an
                // inherited one means it does not.
                state, Ok None
            else
                state,
                Ok (
                    Some (
                        implementationHandle,
                        occupant.Method,
                        "Found interface implementation through the dispatch map"
                    )
                )

        let findClassImplementation
            (state : TypeSystemState)
            : TypeSystemState *
              Result<
                  (ConcreteTypeHandle *
                  WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
                  string) option,
                  TypeBindingFailure
               >
            =
            // Resolution precedence: explicit MethodImpl entries, then method name/signature
            // matches on the current type, then the base type walk when enabled.
            let rec walkBase (state : TypeSystemState) (currentTypeHandle : ConcreteTypeHandle) =
                if not walkBaseTypes then
                    state, Ok None
                else
                    match currentTypeHandle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ -> state, Ok None
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
                        | Error failure -> state, Error failure
                        | Ok baseType ->

                        match baseType with
                        | None -> state, Ok None
                        | Some baseType -> walk state baseType

            and walk
                (state : TypeSystemState)
                (currentTypeHandle : ConcreteTypeHandle)
                : TypeSystemState *
                  Result<
                      (ConcreteTypeHandle *
                      WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
                      string) option,
                      TypeBindingFailure
                   >
                =
                match TypeSystemState.tryGetConcreteTypeInfo state currentTypeHandle with
                | None -> walkBase state currentTypeHandle
                | Some (currentTy, currentTypeInfo) ->
                    match findMatchingMethodImplBodies currentTy currentTypeInfo state with
                    | state, Error failure -> state, Error failure
                    | state, Ok matchingMethodImplBodies ->

                    // Rows naming the same body are one override. Different bodies for one slot make a
                    // type CoreCLR refuses to load (`AddMethodImplDispatchMapping`, methodtablebuilder.cpp),
                    // so no call ever reaches it.
                    match matchingMethodImplBodies |> List.distinctBy _.IdentityKey with
                    | [ impl ] ->
                        state, Ok (Some (currentTypeHandle, impl, "Found concrete implementation from MethodImpl"))
                    | _ :: _ :: _ as bodies ->
                        let names = bodies |> List.map _.Name |> String.concat ", "

                        failwith
                            $"virtual dispatch of %s{methodToCall.Name}: %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} carries MethodImpls that override it with different bodies (%s{names}); CoreCLR rejects this type at load time with a TypeLoadException (IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES)"
                    | [] when methodDeclaringType.IsInterface ->
                        failwith
                            $"virtual dispatch of %s{methodToCall.Name}: an instance interface method reached the class walk, though its implementation is found through the dispatch map"
                    | [] ->
                        let state, implementation =
                            (state, [], currentTypeInfo.Methods)
                            |||> StateResult.fold (fun state acc meth ->
                                match methodMatches currentTy.Generics meth state with
                                | state, Ok (Some matched) -> state, Ok (matched :: acc)
                                | state, Ok None -> state, Ok acc
                                | state, Error failure -> state, Error failure
                            )

                        match implementation with
                        | Error failure -> state, Error failure
                        | Ok implementation ->

                        match List.rev implementation with
                        | [ impl ] -> state, Ok (Some (currentTypeHandle, impl, "Found concrete implementation"))
                        | _ :: _ as implementation ->
                            implementation
                            |> List.map (fun m -> m.Name)
                            |> String.concat ", "
                            |> failwithf "multiple options: %s"
                        | [] -> walkBase state currentTypeHandle

            walk state dispatchTypeHandle

        let state, classImplementation =
            match tryResolveBySlotTable state with
            | state, Error failure -> state, Error failure
            | state, Ok (Some result) -> state, Ok (Some result)
            | state, Ok None when methodDeclaringType.IsInterface -> tryResolveByInterfaceDispatchMap state
            | state, Ok None -> findClassImplementation state

        match classImplementation with
        | Error failure -> state, Error failure
        | Ok (Some (implementationTypeHandle, impl, logMessage)) ->
            logger.LogDebug logMessage
            state, Ok (VirtualImplementation.Found (dispatchedOn implementationTypeHandle impl state))
        | Ok None when not walkBaseTypes -> state, Ok VirtualImplementation.NotOverridden
        | Ok None ->

        logger.LogDebug "No concrete implementation found; searching for a default interface body"

        let search (allowVariance : bool) (state : TypeSystemState) =
            DefaultInterfaceImplementation.search
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                allowVariance
                dispatchTypeHandle
                methodToCall
                state

        let foundDefault (state : TypeSystemState) (candidate : DefaultInterfaceImplementation.Candidate) =
            match candidate.Body.Body with
            | MethodBody.Abstract ->
                logger.LogDebug (
                    "The most specific default interface body is the reabstraction {DeclaringTypeName}.{MethodName}",
                    candidate.Body.RequiredDeclaringType.Name,
                    candidate.Body.Name
                )

                state, Ok (VirtualImplementation.Reabstracted candidate.Body)
            | MethodBody.Il _
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _ ->
                logger.LogDebug (
                    "Found default interface body {DeclaringTypeName}.{MethodName}",
                    candidate.Body.RequiredDeclaringType.Name,
                    candidate.Body.Name
                )

                state, Ok (VirtualImplementation.Found (dispatchedOn candidate.Interface candidate.Body state))

        let throughVariantInterface =
            methodDeclaringType.Generics
            |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

        // `FindDispatchImpl` found nothing. For an interface method, virtual stub dispatch asks a
        // class implementing `IDynamicInterfaceCastable` for an implementation, and throws for any
        // other receiver; for a class's method the receiver is not derived from the class, and the
        // method itself runs.
        let nothingFound
            (state : TypeSystemState)
            : TypeSystemState * Result<VirtualImplementation, TypeBindingFailure>
            =
            if not methodDeclaringType.IsInterface then
                state, Ok VirtualImplementation.NotOverridden
            else

            match dispatchTypeHandle with
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.OneDimArrayZero _ when not (Seq.isEmpty methodDeclaringType.Generics) ->
                // `FindDispatchImpl` assumes any generic interface an array is dispatched on is one
                // SZ arrays implement implicitly, and runs SZArrayHelper's body for it, a
                // multi-dimensional array's included: measured, `ICollection<int>.Count` on an
                // `int[2,3]` is 6.
                state,
                Ok (
                    VirtualImplementation.Unmodelled
                        $"%s{methodToCall.Name} of a generic interface is not implemented by the array %O{dispatchTypeHandle}, which CoreCLR dispatches to System.SZArrayHelper as though it were an SZ array implementing one of the five; PawPrint does not model that"
                )
            | _ ->

            // The runtime marks only a class, never a value type, as `IDynamicInterfaceCastable`
            // (`MethodTableBuilder`, methodtablebuilder.cpp). An array has no TypeDef row and
            // implements no such interface.
            let state, dynamicallyCastable =
                match TypeSystemState.tryGetConcreteTypeInfo state dispatchTypeHandle with
                | None -> state, Ok false
                | Some (_, receiverInfo) when
                    LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies receiverInfo
                    ->
                    state, Ok false
                | Some _ ->
                    let state, map =
                        ConcreteInterfaceDispatch.interfaceMapOf
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            "callvirt"
                            state
                            dispatchTypeHandle

                    state,
                    map
                    |> Result.map (
                        List.exists (fun (entry : ConcreteInterfaceDispatch.InterfaceMapEntry) ->
                            entry.Type.Identity = baseClassTypes.IDynamicInterfaceCastable.Identity
                        )
                    )

            match dynamicallyCastable with
            | Error failure -> state, Error failure
            | Ok true ->
                state,
                Ok (
                    VirtualImplementation.Unmodelled
                        $"%s{methodToCall.Name} is not implemented by %O{dispatchTypeHandle}, which implements IDynamicInterfaceCastable, so CoreCLR asks the receiver's GetInterfaceImplementation for the interface that implements it; PawPrint does not call back into the guest for that"
                )
            | Ok false -> state, Ok VirtualImplementation.NotImplemented

        // `FindDispatchImpl` searches at the call's exact instantiation, and only if that finds
        // nothing, and the interface is variant, allowing variance.
        match search false state with
        | state, Error failure -> state, Error failure
        | state, Ok [ only ] -> foundDefault state only
        | state, Ok (_ :: _ :: _ as candidates) ->
            let candidates = candidates |> List.map _.Body

            if throughVariantInterface then
                // A conflict at the call's exact instantiation, where CoreCLR's search reports
                // one. Through a variant interface, though, whether the call throws depends on the
                // JIT: measured on an instance method, unoptimised code throws
                // AmbiguousImplementationException from the stub resolver, while optimised code
                // devirtualises with `throwOnConflict` false, so the conflict falls through to the
                // variant search, which runs the first candidate.
                let described =
                    candidates
                    |> List.map (fun m -> $"%s{MethodOwner.describe m.Owner}::%s{m.Name}")
                    |> String.concat ", "

                state,
                Ok (
                    VirtualImplementation.Unmodelled
                        $"more than one most-specific default body of %s{methodToCall.Name} at a variant interface's exact instantiation, where CoreCLR throws or runs the first depending on how the JIT compiled the call: %s{described}"
                )
            else
                state, Ok (VirtualImplementation.Ambiguous candidates)
        | state, Ok [] when not throughVariantInterface -> nothingFound state
        | state, Ok [] ->

        // The variant search "[doesn't] look for a conflict for instance methods": it runs the first
        // survivor. `sourcesPure/VariantInterfaceDefaultBodyPrecedence.cs` and
        // `VariantInterfaceMapOrder.cs` pin its order against the real runtime.
        match search true state with
        | state, Error failure -> state, Error failure
        | state, Ok [] -> nothingFound state
        | state, Ok (first :: _) -> foundDefault state first

    /// One entry of a receiver's interface map, as `collectInterfaceMap` visits it.
    type private InterfaceSearchEntry =
        {
            Handle : ConcreteTypeHandle
            Type : ConcreteType<ConcreteTypeHandle>
        }

    /// One interface, followed by its transitive parents, depth-first. `visited` collapses
    /// diamonds at the *first* occurrence; `interfaceMapHandles` reports the resulting order.
    let rec private expandInterfaceEntry
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (visited : Set<ConcreteTypeHandle>)
        (ifaceHandle : ConcreteTypeHandle)
        (ifaceTy : ConcreteType<ConcreteTypeHandle>)
        (ifaceTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * Result<Set<ConcreteTypeHandle> * InterfaceSearchEntry list, TypeBindingFailure>
        =
        if visited.Contains ifaceHandle then
            state, Ok (visited, [])
        else

        let visited = visited.Add ifaceHandle

        let state, parents =
            (state, (visited, []), ifaceTypeInfo.ImplementedInterfaces)
            |||> StateResult.fold (fun state (visited, acc) impl ->
                match
                    ConcreteInterfaceDispatch.resolveImplementedInterface
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        ifaceTy
                        impl
                        state
                with
                | state, Error failure -> state, Error failure
                | state, Ok (parentHandle, parentTy, parentTypeInfo) ->

                match
                    expandInterfaceEntry
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        visited
                        parentHandle
                        parentTy
                        parentTypeInfo
                with
                | state, Error failure -> state, Error failure
                | state, Ok (visited, expanded) -> state, Ok (visited, acc @ expanded)
            )

        state,
        parents
        |> Result.map (fun (visited, parents) ->
            visited,
            {
                Handle = ifaceHandle
                Type = ifaceTy
            }
            :: parents
        )

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
    /// `interfaceMapHandles` reports this order to callers that pick the first compatible entry, so
    /// it must not be reordered or set-ified.
    let rec private collectInterfaceMap
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        (visited : Set<ConcreteTypeHandle>)
        (typeHandle : ConcreteTypeHandle)
        : TypeSystemState * Result<Set<ConcreteTypeHandle> * InterfaceSearchEntry list, TypeBindingFailure>
        =
        if visited.Contains typeHandle then
            state, Ok (visited, [])
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
        let state, inherited =
            if not walkBaseTypes then
                state, Ok (visited, [])
            else

            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, Ok (visited, [])
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->

            let state, baseType =
                TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state typeHandle

            match baseType with
            | Error failure -> state, Error failure
            | Ok baseType ->

            match baseType with
            | None -> state, Ok (visited, [])
            | Some baseType ->
                collectInterfaceMap loggerFactory dotnetRuntimeDirs baseClassTypes walkBaseTypes state visited baseType

        match inherited with
        | Error failure -> state, Error failure
        | Ok (visited, baseEntries) ->

        let state, own =
            match TypeSystemState.tryGetConcreteTypeInfo state typeHandle with
            | None -> state, Ok (visited, [])
            | Some (ty, typeInfo) ->
                (state, (visited, []), typeInfo.ImplementedInterfaces)
                |||> StateResult.fold (fun state (visited, acc) impl ->
                    match
                        ConcreteInterfaceDispatch.resolveImplementedInterface
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            ty
                            impl
                            state
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok (ifaceHandle, ifaceTy, ifaceTypeInfo) ->

                    match
                        expandInterfaceEntry
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            visited
                            ifaceHandle
                            ifaceTy
                            ifaceTypeInfo
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok (visited, expanded) -> state, Ok (visited, acc @ expanded)
                )

        state,
        own
        |> Result.map (fun (visited, ownEntries) -> visited, ownEntries @ baseEntries)

    /// The interfaces in `receiverType`'s interface map, its base types' included, in the order
    /// `MethodTable::FindDefaultInterfaceImplementation` searches them (see `collectInterfaceMap`).
    /// An interface on the way that does not bind is the failure, returned with what loaded before it.
    let interfaceMapHandles
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (receiverType : ConcreteTypeHandle)
        : TypeSystemState * Result<ConcreteTypeHandle list, TypeBindingFailure>
        =
        let state, entries =
            collectInterfaceMap loggerFactory dotnetRuntimeDirs baseClassTypes true state Set.empty receiverType

        state, entries |> Result.map (fun (_, entries) -> entries |> List.map _.Handle)

    /// Identify the body a virtual or interface call lands on, given the receiver's runtime type.
    ///
    /// `walkBaseTypes` false means "exact-type dispatch": the `constrained.` value-type probe,
    /// which asks whether `T` itself supplies the method rather than inheriting it.
    ///
    /// `Unmodelled` for the default-body outcomes `VirtualImplementation.Unmodelled` lists.
    ///
    /// `Reabstracted`, and never `NotOverridden`, where the most specific default body is a
    /// reabstraction, so a caller must not fall back to the method the call names.
    ///
    /// `NotImplemented` where `walkBaseTypes` is true and nothing implements an interface method on
    /// the receiver's type; `Unmodelled` where such a receiver implements
    /// `IDynamicInterfaceCastable`, or is an array and the interface is generic.
    ///
    /// `methodToCall` must be an instance method: a static virtual is `StaticVirtualDispatch`'s.
    ///
    /// A type the search reaches that does not bind is the failure, returned with what loaded before
    /// it: CoreCLR would have failed to load that type before any call could dispatch on it.
    let tryResolveVirtualImplementation
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : TypeSystemState)
        : TypeSystemState * Result<VirtualImplementation, TypeBindingFailure>
        =
        if methodToCall.IsStatic then
            failwith
                $"virtual dispatch of %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name}: a static virtual is resolved by StaticVirtualDispatch.resolve, not by instance dispatch"

        tryResolveVirtualImplementationForSlot
            loggerFactory
            dotnetRuntimeDirs
            baseClassTypes
            methodGenerics
            methodToCall
            dispatchTypeHandle
            walkBaseTypes
            state
