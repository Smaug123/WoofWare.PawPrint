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
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.Int64 _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int64
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Double
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
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

        if not (baseClassTypes.IsImplicitInterfaceOfSzArray methodToCall.DeclaringType.Identity) then
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
            match Seq.toList methodToCall.DeclaringType.Generics with
            | [ t ] -> t
            | generics ->
                failwith
                    $"SZ-array implicit interface %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name} should have exactly one generic argument, got %i{List.length generics}"

        // CoreCLR maps interface slot → shim method by slot arithmetic, but asserts the result
        // equals `MemberLoader::FindMethodByName(g_pSZArrayHelperClass, pItfcMeth->GetName())`.
        // Name lookup is therefore the specified-equivalent, and `SZArrayHelper`'s method names
        // are pairwise distinct, so `exactlyOne` is the honest reading rather than a heuristic.
        let implementation =
            baseClassTypes.SZArrayHelper.Methods
            |> List.filter (fun meth -> meth.Name = methodToCall.Name)

        let implementation =
            match implementation with
            | [ impl ] -> impl
            | [] ->
                failwith
                    $"System.SZArrayHelper has no method named %s{methodToCall.Name}, needed to dispatch %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name}::%s{methodToCall.Name} on an SZ-array receiver"
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
                $"System.SZArrayHelper::%s{implementation.Name} takes %i{implementation.Signature.RequiredParameterCount} parameters but the interface slot %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name}::%s{methodToCall.Name} takes %i{methodToCall.Signature.RequiredParameterCount}"

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
                        $"SZ-array interface dispatch: type argument %O{handle} of %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name} has no TypeDef row"

        let dispatchThroughEnumerable =
            methodToCall.DeclaringType.Identity = baseClassTypes.IEnumerableGeneric.Identity

        let state, instantiation =
            if dispatchThroughEnumerable || not (isReferenceType theT) then
                state, theT
            else
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Object
                |> IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.Name
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
        let logger = loggerFactory.CreateLogger "CallMethod"

        logger.LogDebug (
            "Identifying target of virtual call for {TypeName}.{MethodName}",
            methodToCall.DeclaringType.Name,
            methodToCall.Name
        )

        // The SZ-array carve-out runs *before* the ordinary walks, unlike CoreCLR, which reaches
        // it only after its dispatch map misses. CoreCLR can afford that ordering because its
        // lookup is exact-slot; ours matches on name and signature, which is fuzzier. Running
        // first is safe and total: when the receiver is an SZ array and the target is one of the
        // five interfaces, the answer is always SZArrayHelper. Nothing on the receiver's fixed
        // class chain can shadow it either, structurally rather than by coincidence — an array's
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

        let declaringAssy = state.LoadedAssembly(methodToCall.DeclaringType.Assembly).Value

        let methodDeclaringType =
            declaringAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

        let interfaceExplicitNamedMethod =
            if methodDeclaringType.IsInterface then
                Some
                    $"{TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) methodDeclaringType}.{methodToCall.Name}"
            else
                None

        let signatureMatchesTarget
            (candidateAssembly : AssemblyName)
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (candidateSignature : TypeMethodSignature<TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            if
                candidateSignature.GenericParameterCount
                <> methodToCall.Signature.GenericParameterCount
                || candidateSignature.RequiredParameterCount
                   <> methodToCall.Signature.RequiredParameterCount
            then
                state, false
            else

            let state, candidateSignature =
                candidateSignature
                |> TypeMethodSignature.map
                    state
                    (fun state ty ->
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            candidateAssembly
                            candidateTypeGenerics
                            methodToCall.Generics
                            ty
                    )

            let state, retAssignable =
                match candidateSignature.ReturnType, methodToCall.Signature.ReturnType with
                | MethodReturnType.Void, MethodReturnType.Void -> state, true
                | MethodReturnType.Returns retType, MethodReturnType.Returns targetType ->
                    isAssignableFrom loggerFactory baseClassTypes retType targetType state
                | MethodReturnType.Void, MethodReturnType.Returns _
                | MethodReturnType.Returns _, MethodReturnType.Void -> state, false

            state,
            retAssignable
            && candidateSignature.ParameterTypes = methodToCall.Signature.ParameterTypes

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
                state, meth.Handle = methodToCall.Handle
            else
                signatureMatchesTarget meth.DeclaringType.Assembly candidateTypeGenerics meth.Signature state

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
                && (not (meth.MethodAttributes.HasFlag MethodAttributes.Virtual)
                    || (meth.MethodAttributes.HasFlag MethodAttributes.NewSlot
                        && meth.Handle <> methodToCall.Handle))
            then
                None, state
            else

            let state, matches =
                signatureMatchesTarget meth.DeclaringType.Assembly candidateTypeGenerics meth.Signature state

            if matches then
                Some (meth, Some meth.Name = interfaceExplicitNamedMethod), state
            else
                None, state

        let concretizeTypeArgs
            (declaringAssembly : AssemblyName)
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
                        declaringAssembly
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
                        methodGenerics
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
                            concretizeTypeArgs declaration.DeclaringType.Assembly currentTy.Generics typeArgs state
                        | None when declaration.DeclaringType.Generics.IsEmpty -> state, ImmutableArray.Empty
                        | None when declaration.DeclaringType.Identity = currentTy.Identity ->
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
                        if declaration.DeclaringType.Identity <> methodToCall.DeclaringType.Identity then
                            state, false, false
                        elif declarationTypeGenerics = methodToCall.DeclaringType.Generics then
                            state, true, false
                        else
                            let state, fromH =
                                ensureRegistered
                                    state
                                    declaration.DeclaringType.Identity
                                    declaration.DeclaringType.Namespace
                                    declaration.DeclaringType.Name
                                    declarationTypeGenerics

                            let state, toH =
                                ensureRegistered
                                    state
                                    methodToCall.DeclaringType.Identity
                                    methodToCall.DeclaringType.Namespace
                                    methodToCall.DeclaringType.Name
                                    methodToCall.DeclaringType.Generics

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
                        $"Implementation declaring type handle %O{implementationTypeHandle} was not registered while concretizing %s{implementation.DeclaringType.Namespace}.%s{implementation.DeclaringType.Name}::%s{implementation.Name}"
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

        let state, classImplementation = findClassImplementation state

        match classImplementation with
        | Some (implementationTypeHandle, impl, logMessage) ->
            logger.LogDebug logMessage
            let state, impl = concretizeImplementation implementationTypeHandle impl state
            state, Some impl
        | None when not walkBaseTypes -> state, None
        | None ->

        logger.LogDebug "No concrete implementation found; scanning interfaces"

        let resolveImplementedInterface
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
                match state.LoadedAssembly impl.RelativeToAssembly with
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
                    implResolvedAssy.Name
                    ownerTy.Generics
                    ImmutableArray.Empty
                    implTypeDefn

            match IlMachineState.tryGetConcreteTypeInfo state implHandle with
            | Some (implTy, typeInfo) -> state, implHandle, implTy, typeInfo
            | None ->
                failwith $"Interface implementation handle %O{implHandle} was not registered or has no TypeDef row"

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
            |> List.distinctBy (fun (interfaceHandle, meth) -> interfaceHandle, meth.Handle)

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
                meth.DeclaringType.Namespace,
                meth.DeclaringType.Name,
                meth.Name,
                meth.Generics
            )

            let state, meth = concretizeImplementation implementationTypeHandle meth state
            state, Some meth
        | _ ->
            mostSpecificInterfaceMethods
            |> List.map (fun (_, m) -> $"%s{m.DeclaringType.Namespace}.%s{m.DeclaringType.Name}::%s{m.Name}")
            |> String.concat ", "
            // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
            |> failwithf "multiple most-specific default interface implementations matched this virtual slot: %s"

    let rec callMethod
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
        (dispatchAsExceptionOnReturn : bool)
        (wrapExceptionInTargetInvocation : bool)
        (state : IlMachineState)
        : IlMachineState
        =
        let logger = loggerFactory.CreateLogger "CallMethod"

        let activeMethodState = threadState.MethodState

        // The method named at the call site, before any virtual/interface resolution. Retained
        // because the *type-level* `[Intrinsic]` check below is keyed on it; see there.
        let callSiteMethod = methodToCall

        // Virtual/interface resolution runs BEFORE the `[Intrinsic]` classification below, so
        // that `isIntrinsic` and `intrinsicKey` describe the method we are actually about to
        // execute. Classifying the pre-resolution method instead lets a `callvirt` of a
        // non-intrinsic abstract declaration (e.g. `ICloneable::Clone`) resolve to an
        // `[Intrinsic]` override (`Array::Clone`) whose IL body would then be interpreted —
        // which is exactly what `[Intrinsic]` means we must not do.
        let shouldPerformVirtualResolution =
            performInterfaceResolution
            && not methodToCall.IsStatic
            && methodToCall.MethodAttributes.HasFlag MethodAttributes.Virtual
            && not (methodToCall.MethodAttributes.HasFlag MethodAttributes.Final)

        let state, methodToCall =
            if shouldPerformVirtualResolution then
                let callingObj =
                    match
                        activeMethodState.EvaluationStack
                        |> EvalStack.PeekNthFromTop methodToCall.Parameters.Length
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

        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringType.Assembly with
            | Some assy -> assy
            | None ->
                failwith
                    $"CallMethod: declaring assembly for %O{methodToCall} is not loaded: %O{methodToCall.DeclaringType.Assembly}"

        let getMemberRefParentType (handle : MemberReferenceHandle) : TypeRef =
            match declaringAssy.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> declaringAssy.TypeRefs.[r]
            | x -> failwith $"{x}"

        // Check for intrinsics first
        let methodHasIntrinsicAttribute =
            MethodInfo.isJITIntrinsic getMemberRefParentType declaringAssy.Methods methodToCall

        let declaringType =
            declaringAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

        // The two `[Intrinsic]` checks deliberately use different methods as their basis, and the
        // difference is load-bearing:
        //
        //  * Method-level `[Intrinsic]` (above) is a property of the body we are about to run, so
        //    it is keyed on the POST-resolution method. That is the whole point of hoisting
        //    resolution: `callvirt ICloneable::Clone()` must be recognised as `Array::Clone`.
        //
        //  * Type-level `[Intrinsic]` is a property of the CALL SITE's static type. It marks a
        //    type whose own API surface the JIT knows (`Int128`, `Vector128<T>`, ...); it says
        //    nothing about that type's `System.Object` overrides. `Int128.GetHashCode` is plain
        //    `HashCode.Combine(_lower, _upper)` and carries no method-level attribute, so
        //    `callvirt Object::GetHashCode()` on a boxed `Int128` must interpret it as normal.
        //    Keying this check on the resolved override would instead reject every virtual call
        //    that happens to land on one of those types. `BoxedIntrinsicTypeVirtualCall.cs`
        //    pins the behaviour.
        //
        // When no resolution happened the two coincide, so this only diverges for `callvirt`.
        let callSiteDeclaringAssy =
            match state.LoadedAssembly callSiteMethod.DeclaringType.Assembly with
            | Some assy -> assy
            | None ->
                failwith
                    $"CallMethod: declaring assembly for call-site method %O{callSiteMethod} is not loaded: %O{callSiteMethod.DeclaringType.Assembly}"

        let callSiteGetMemberRefParentType (handle : MemberReferenceHandle) : TypeRef =
            match callSiteDeclaringAssy.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> callSiteDeclaringAssy.TypeRefs.[r]
            | x -> failwith $"{x}"

        let callSiteDeclaringType =
            callSiteDeclaringAssy.TypeDefs.[callSiteMethod.DeclaringType.Definition.Get]

        // An abstract call-site declaration has no IL of its own, so a type-level `[Intrinsic]`
        // inherited from it is a hint about the *interface*, not about the override we resolved
        // to. `IEnumerator<T>` carries a type-level `[Intrinsic]`, so without this suppression
        // every `callvirt IEnumerator<T>::get_Current()` would be rejected even though it
        // resolves to an ordinary `SZGenericArrayEnumerator<T>` body. This mirrors the
        // `isAbstractBody` suppression below, which on the pre-hoist ordering covered this case
        // because the classification saw the abstract declaration.
        let callSiteBodyIsAbstract =
            match callSiteMethod.Body with
            | MethodBody.Abstract -> true
            | _ -> false

        let declaringTypeHasIntrinsicAttribute =
            not callSiteBodyIsAbstract
            && MethodInfo.hasIntrinsicAttribute
                callSiteGetMemberRefParentType
                callSiteDeclaringAssy.Methods
                callSiteDeclaringType.Attributes

        // `[Intrinsic]` on an abstract/interface method is a JIT inlining hint for the
        // call site only — there is no IL to interpret. Virtual resolution has already run
        // above, so `methodToCall` is normally the concrete override and this guard is
        // rarely load-bearing; it still matters when resolution was skipped
        // (`performInterfaceResolution = false`) or found no implementation, where we'd
        // otherwise fail any callvirt of an abstract `[Intrinsic]` method
        // (e.g. IEnumerable`1::GetEnumerator) that has no body to run.
        let isAbstractBody =
            match methodToCall.Body with
            | MethodBody.Abstract -> true
            | _ -> false

        let isIntrinsic =
            (methodHasIntrinsicAttribute || declaringTypeHasIntrinsicAttribute)
            && not isAbstractBody

        let intrinsicKey = Intrinsics.methodKey state methodToCall

        // `static T Activator.CreateInstance<T>()` is marked `[Intrinsic]` because the JIT inlines it
        // to an allocate+ctor sequence. The managed IL bottoms out in InternalCalls
        // (`RuntimeType.CreateInstanceOfT`, `CallDefaultStructConstructor`) we don't model, so we
        // implement the high-level intrinsic semantics directly: for a value type T, push `default(T)`
        // (skipping any explicit parameterless struct ctor for now — see TODO); for a reference type T,
        // allocate the object and run its parameterless ctor by recursing through `callMethod`.
        // See https://github.com/dotnet/runtime/blob/HEAD/src/coreclr/System.Private.CoreLib/src/System/Activator.RuntimeType.cs#L138
        //
        // Exception wrapping:
        //  - CoreCLR's `CreateInstanceOfT` wraps any exception thrown by the recursed ctor in a
        //    `TargetInvocationException`. We can't observe that in a separate Activator frame
        //    because we inline the intrinsic, so the recursive `callMethod` for the ctor sets
        //    `WrapExceptionInTargetInvocation = true` on the ctor frame's `ReturnState`. When
        //    `ExceptionDispatching.unwindToCallerAndSearch` pops the ctor frame, it synthesises
        //    a fresh `TargetInvocationException` with the original exception as `_innerException`
        //    and continues the search with the wrapped exception. A try/catch *inside* the ctor
        //    that handles the exception is unaffected, matching CoreCLR.
        //
        // Intentional divergence (see docs/divergences.md):
        //  - For `BeforeFieldInit` reference types, CoreCLR defers the type initializer past the
        //    Activator allocation/ctor pair. PawPrint's `newobj` (UnaryMetadataObjectOps.fs:240)
        //    runs cctor eagerly on every instance creation regardless of the flag, so this
        //    intrinsic follows the same convention. ECMA-335 II.10.5.3.2 permits eager schedules.
        let tryHandleActivatorCreateInstance () : IlMachineState option =
            if
                intrinsicKey.AssemblyName = "System.Private.CoreLib"
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
                            |> List.exists (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Parameters.IsEmpty)

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

                    Some state
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
                let isPublic (m : MethodInfo<_, _, _>) : bool =
                    (m.MethodAttributes &&& MethodAttributes.MemberAccessMask) = MethodAttributes.Public

                let ctor =
                    typeDef.Methods
                    |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Parameters.IsEmpty && isPublic m)

                match ctor with
                | None ->
                    // CoreCLR throws MissingMethodException here. We don't yet have a host helper
                    // to raise that, so fail loudly with the precise condition.
                    let hasNonPublicParameterless =
                        typeDef.Methods
                        |> List.exists (fun m ->
                            m.Name = ".ctor" && not m.IsStatic && m.Parameters.IsEmpty && not (isPublic m)
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
                // We mirror that on three sub-paths:
                //
                //   (a) T's cctor was previously cached as Failed. We synthesise a fresh
                //       `TargetInvocationException` whose `_innerException` is the cached TIE
                //       and dispatch it ourselves. Per CoreCLR the cctor is NOT re-run, but a
                //       fresh wrap is produced each time (verified against .NET 10).
                //   (b) T's cctor is about to run for the first time. We let `ensureTypeInitialised`
                //       push the cctor frame, then flip its `WrapExceptionInTargetInvocation`
                //       flag so that if the cctor unwinds with a TIE (after the existing
                //       `WasInitialisingType` wrap), the dispatcher additionally wraps it in
                //       `TargetInvocationException` on the way out of the cctor frame.
                //   (c) T's cctor has already run successfully; we just call the instance ctor
                //       with the wrap flag set on the ctor's frame.
                match TypeInitTable.tryGet tHandle state.TypeInitTable with
                | Some (TypeInitState.Failed (cachedTieAddr, _cachedTieType)) ->
                    let state =
                        IlMachineState.setExceptionStackTraceString loggerFactory baseClassTypes cachedTieAddr [] state

                    let tieAddr, tieType, state =
                        IlMachineState.synthesizeTargetInvocationException
                            loggerFactory
                            baseClassTypes
                            cachedTieAddr
                            state

                    match
                        ExceptionDispatching.throwExceptionObject
                            loggerFactory
                            baseClassTypes
                            state
                            thread
                            tieAddr
                            tieType
                    with
                    | ExceptionDispatchResult.HandlerFound state -> Some state
                    | ExceptionDispatchResult.ExceptionUnhandled _ ->
                        failwith
                            "Unhandled TargetInvocationException wrapping a cached TypeInitializationException during Activator.CreateInstance<T>(); should have been caught by a handler"
                | _ ->

                let state, init =
                    ensureTypeInitialised loggerFactory baseClassTypes thread tHandle state

                match init with
                | WhatWeDid.Executed ->
                    let state, concretizedCtor, declaringTypeHandle =
                        ExecutionConcretization.concretizeMethodWithAllGenerics
                            loggerFactory
                            baseClassTypes
                            ct.Generics
                            ctor
                            ImmutableArray.Empty
                            state

                    let state, allFields =
                        IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state declaringTypeHandle

                    let fields =
                        CliValueType.OfFields
                            baseClassTypes
                            state.ConcreteTypes
                            declaringTypeHandle
                            typeDef.Layout
                            (CharSetMetadata.ofTypeAttributes typeDef.TypeAttributes)
                            allFields

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
                        false
                        true // wrapExceptionInTargetInvocation: mirror CreateInstanceOfT
                        state
                    |> Some
                | WhatWeDid.SuspendedForClassInit ->
                    // T's cctor was kicked off and is now running on top of the current frame.
                    // We need the activator call to be retried after the cctor returns. The
                    // simplest signal to the engine for that today is: leave the state with the
                    // cctor frame pushed, but the activator caller's PC must not have advanced,
                    // because when control returns to it, we want it to re-execute the call
                    // opcode and re-enter Activator.CreateInstance<T>().
                    //
                    // Caller-PC advancement happens later in `callMethod` (line ~961); by short-
                    // circuiting here we never reach it, so the caller's PC stays put. Good.
                    //
                    // The cctor frame is now the active frame on this thread. Mark it so that if
                    // the cctor throws, the resulting TIE is rewrapped in TargetInvocationException
                    // when the cctor frame unwinds — see comment block (a)/(b)/(c) above.
                    let state = IlMachineState.markActiveFrameWrapInTargetInvocation thread state

                    Some state
                | WhatWeDid.BlockedOnClassInit _ ->
                    failwith
                        "TODO: cross-thread class init blocking inside Activator.CreateInstance<T>() is not yet handled"
                | WhatWeDid.SuspendedForManagedCall ->
                    failwith
                        "logic error: ensureTypeInitialised inside Activator.CreateInstance<T>() cannot suspend for an arbitrary managed call"
                | WhatWeDid.ThrowingTypeInitializationException ->
                    // Unreachable: the only way `ensureTypeInitialised` returns this is via the
                    // `TypeInitState.Failed` cached-cctor path, which we pre-handle above.
                    failwith
                        "logic error: ensureTypeInitialised should not reach the cached-failure path inside Activator.CreateInstance<T>() (handled separately above)"
                | WhatWeDid.VoluntaryYield ->
                    failwith
                        "logic error: ensureTypeInitialised inside Activator.CreateInstance<T>() cannot produce a VoluntaryYield (cctor execution has no path to a yield primitive)"
            else
                None

        match
            if isIntrinsic && not (Intrinsics.isSafeIntrinsic intrinsicKey) then
                match tryHandleActivatorCreateInstance () with
                | Some result -> Some result
                | None ->

                match Intrinsics.call loggerFactory baseClassTypes wasConstructing methodToCall thread state with
                | Some result -> Some result
                | None ->
                    failwith
                        $"TODO: implement JIT intrinsic %s{Intrinsics.formatMethodKey intrinsicKey}, or add it to safeIntrinsics after reviewing its IL"
            else
                None
        with
        | Some result -> result
        | None ->

        // Get zero values for all parameters.
        //
        // These are the coercion targets for the popped arguments below, and they are
        // deliberately derived from the POST-resolution `methodToCall` — i.e. the body we are
        // about to execute, not the declaration named at the call site. The two genuinely
        // differ under `in`-variance: dispatching `IContravariant<string>::Set(string)` selects
        // a body declaring `Set(object)`, and the argument must be coerced to the body's
        // parameter type. `thisArgCoercionTarget` and `createNewFrame` below share that basis.
        // See `ContravariantExplicitMethodImpl.cs` and `VirtualOverrideArgumentCoercion.cs`.
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
                state.LoadedAssembly (methodToCall.DeclaringType.Assembly) |> Option.get

            let declaringType =
                declaringAssembly.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies declaringType then
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
            else
                CliType.ObjectRef None

        // Pop exactly the method's declared parameters, leaving no `this` slot in the
        // resulting `Arguments` array. Shared by genuinely static methods and by
        // variable-size constructors, which CoreCLR calls with no `this` at all (see
        // `ConstructionState.ConstructingVariableSize`).
        let popDeclaredParametersOnly () =
            let args = ImmutableArray.CreateBuilder methodToCall.Parameters.Length
            let mutable currentState = activeMethodState

            for i = methodToCall.Parameters.Length - 1 downto 0 do
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
            | ConstructionState.ConstructingVariableSize ->
                // Variable-size constructor: `executeNewobj` pushed no `this`, so the eval
                // stack holds only the declared arguments. The constructor's `Arguments`
                // array is correspondingly `this`-less, and the object it allocates is
                // handed back via `withSuppliedConstructedObject`.
                popDeclaredParametersOnly ()
            | ConstructionState.Constructing _ ->
                // Instance method: handle `this` pointer
                let argCount = methodToCall.Parameters.Length
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
                let argCount = methodToCall.Parameters.Length
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
                        DispatchAsExceptionOnReturn = dispatchAsExceptionOnReturn
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
                                (state.LoadedAssembly methodToCall.DeclaringType.Assembly |> Option.get)
                                (fst asmRef.Handle)
                                s

                        s
                    )

                createNewFrame state'

        let state, newFrame = createNewFrame state

        let oldFrame =
            if wasClassConstructor || not advanceProgramCounterOfCaller then
                afterPop
            else
                afterPop |> MethodState.advanceProgramCounter

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState oldFrame threadState

        let calleeFrameId, threadState = ThreadState.appendFrame newFrame threadState
        let newThreadState = ThreadState.setActiveFrame calleeFrameId threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

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
            | ExceptionDispatchResult.HandlerFound state -> StateLoadResult.ThrowingTypeInitializationException state
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

            let sourceAssembly = state.LoadedAssembly concreteType.Assembly |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue concreteType.Definition.Get with
                | false, _ ->
                    failwith
                        $"Failed to find type definition {concreteType.Definition.Get} in {concreteType.Assembly.FullName}"
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
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && method.Parameters.IsEmpty)

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
                    |> TypeMethodSignature.map
                        state
                        (fun state typeDefn ->
                            IlMachineState.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                concreteType.Assembly
                                concreteType.Generics
                                // no method generics for cctor
                                ImmutableArray.Empty
                                typeDefn
                        )

                // Convert method instructions (local variables)
                let state, convertedBody =
                    match cctorMethodWithMethodGenerics.Body with
                    | MethodBody.Il methodInstr ->
                        let state, convertedLocalVars =
                            match methodInstr.LocalVars with
                            | None -> state, None
                            | Some localVars ->
                                // Concretize each local variable type
                                let state, convertedVars =
                                    ((state, []), localVars)
                                    ||> Seq.fold (fun (state, acc) typeDefn ->
                                        let state, handle =
                                            IlMachineState.concretizeType
                                                loggerFactory
                                                baseClassTypes
                                                state
                                                concreteType.Assembly
                                                concreteType.Generics
                                                ImmutableArray.Empty // no method generics for cctor
                                                typeDefn

                                        state, handle :: acc
                                    )
                                    |> Tuple.rmap ImmutableArray.CreateRange

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
                    false
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
            | ExceptionDispatchResult.HandlerFound state -> state, WhatWeDid.ThrowingTypeInitializationException
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
    let raiseRuntimeException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        // 1. Allocate the zero-initialised exception with _HResult pre-set.  This deliberately
        //    bypasses ensureTypeInitialised: opcode-manufactured exceptions are produced by the
        //    runtime rather than by guest `newobj` class-initialisation semantics.
        let addr, _exnHandle, state =
            ExceptionDispatching.allocateRuntimeException loggerFactory baseClassTypes exceptionTypeInfo state

        // 2. Find the parameterless .ctor on the exception type.
        let assy = state._LoadedAssemblies.[exceptionTypeInfo.Assembly]
        let typeDef = assy.TypeDefs.[exceptionTypeInfo.Identity.TypeDefinition.Get]

        if not typeDef.Generics.IsEmpty then
            failwith
                $"raiseRuntimeException: expected non-generic exception type, but %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} has %i{typeDef.Generics.Length} generic parameter(s)"

        let ctor =
            typeDef.Methods
            |> List.tryFind (fun method -> method.Name = ".ctor" && not method.IsStatic && method.Parameters.IsEmpty)
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
        let state, concretizedCtor, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                currentThread
                ctor
                None
                None
                state

        let threadState = state.ThreadState.[currentThread]

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
            true // dispatchAsExceptionOnReturn
            false // wrapExceptionInTargetInvocation
            state,
        WhatWeDid.Executed

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
            match state.ManagedHeap.Arrays.TryGetValue arrayAddress with
            | true, v -> v
            | false, _ ->
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
