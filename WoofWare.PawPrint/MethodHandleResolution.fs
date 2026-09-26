namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// <summary>
/// From a guest's method handle to the method it names: the registry id inside a
/// <c>RuntimeMethodHandleInternal</c>, the reflection object behind an <c>IRuntimeMethodInfo</c>,
/// and a metadata identity concretised into a method a frame can be pushed for.
/// </summary>
/// <remarks>
/// Compiled before the IL ops rather than beside the natives that use most of it, because a dynamic
/// method's <c>call</c> and <c>callvirt</c> can name a reflected method and need the same answers.
/// </remarks>
[<RequireQualifiedAccess>]
module MethodHandleResolution =

    /// Extract the registry id from the m_handle of a `RuntimeMethodHandleInternal`. Accepts both
    /// the canonical `RuntimePointer (MethodRegistryHandle id)` form and the `NativeInt
    /// (MethodHandlePtr id)` form that primitive-like rewrapping produces when the value is
    /// stored through an `IntPtr`-shaped byref (see EvalStack rewrap rules). `Verbatim 0L` in
    /// either tag means "null sentinel" — the BCL writes that when iteration is exhausted.
    let methodHandleIdOfRuntimeMethodHandleInternal (operation : string) (arg : CliType) : int64 option =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> Some id
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> None
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodHandlePtr id)) -> Some id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other ->
            failwith
                $"%s{operation}: expected RuntimeMethodHandleInternal containing a method-registry handle, got %O{other}"

    /// Resolve a `RuntimeMethodHandleInternal` argument to the `MethodHandle` it denotes.
    let resolveMethodHandleFromArg (operation : string) (state : IlMachineState) (arg : CliType) : MethodHandle =
        // CoreCLR's RuntimeMethodHandle FCalls dereference the MethodDesc* directly and
        // assert non-null; PawPrint's existing callers never yield a null handle, so we
        // surface a contract violation rather than silently producing a default value.
        let methodHandleId =
            methodHandleIdOfRuntimeMethodHandleInternal operation arg
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: null RuntimeMethodHandleInternal")

        MethodHandleRegistry.resolveMethodFromId methodHandleId state.MethodHandles
        |> Option.defaultWith (fun () ->
            failwith $"%s{operation}: registry id %d{methodHandleId} did not resolve to a known MethodHandle"
        )

    /// Resolve an <c>IRuntimeMethodInfo</c> object-reference argument to the <c>MethodHandle</c> it
    /// names. The sibling of <c>resolveMethodHandleFromArg</c> for the handful of natives CoreCLR
    /// declares over the reflection object rather than over a <c>RuntimeMethodHandleInternal</c>.
    ///
    /// Accepts each of the three CoreLib types that implement the interface, and refuses anything
    /// else by name; a null reference is a contract violation rather than an answer, as it is in
    /// CoreCLR.
    let resolveMethodHandleFromMethodInfoObject
        (operation : string)
        (state : IlMachineState)
        (arg : CliType)
        : MethodHandle
        =
        let address =
            match arg with
            | CliType.ObjectRef (Some address) -> address
            | CliType.ObjectRef None ->
                // CoreCLR asserts the argument non-null, and both of its managed callers pass
                // `this`, so a null here is a contract violation rather than a case to answer.
                failwith $"%s{operation}: null IRuntimeMethodInfo"
            | other -> failwith $"%s{operation}: expected an IRuntimeMethodInfo object reference, got %O{other}"

        let object' = ManagedHeap.get address state.ManagedHeap

        // CoreCLR reads the `MethodDesc*` at a fixed offset (`ReflectMethodObject::m_pMD`,
        // object.h:1120); the three implementers are laid out so that this is legal, which is why
        // `RuntimeMethodInfoStub` carries eight unused `object?` fields whose comment says they are
        // there "to ensure that this class has the same layout as RuntimeMethodInfo"
        // (RuntimeHandles.cs:930-940). Reading by name instead means naming the three, and means
        // that the two spellings CoreLib gives that one slot both have to be handled:
        // `RuntimeMethodInfo` and `RuntimeConstructorInfo` call it `m_handle` and declare it
        // `IntPtr`; the stub calls it `m_value` and declares it `RuntimeMethodHandleInternal`.
        //
        // Matched against CoreLib's own types rather than against the namespace and name alone, so
        // that a guest which declares a type of the same name is not mistaken for one of these.
        // Nothing reachable exercises that: `IRuntimeMethodInfo` is internal to CoreLib, so no
        // guest-authored object can arrive typed as this parameter.
        let fieldName =
            match object'.ConcreteType with
            | CorelibType state.ConcreteTypes ("System.Reflection", "RuntimeMethodInfo", generics) when generics.IsEmpty ->
                "m_handle"
            | CorelibType state.ConcreteTypes ("System.Reflection", "RuntimeConstructorInfo", generics) when
                generics.IsEmpty
                ->
                "m_handle"
            | CorelibType state.ConcreteTypes ("System", "RuntimeMethodInfoStub", generics) when generics.IsEmpty ->
                "m_value"
            | other ->
                let described =
                    match AllConcreteTypes.lookup other state.ConcreteTypes with
                    | Some concrete -> $"%s{concrete.Namespace}.%s{concrete.Name} in %O{concrete.AssemblyFullName}"
                    | None -> string other

                failwith
                    $"%s{operation}: object at %O{address} is a %s{described}, which is not one of CoreLib's three IRuntimeMethodInfo implementers (System.Reflection.RuntimeMethodInfo, System.Reflection.RuntimeConstructorInfo, System.RuntimeMethodInfoStub); a fourth implementer needs its handle field naming here"

        // Both declared types reach the registry id through the same reader.
        resolveMethodHandleFromArg operation state (AllocatedNonArrayObject.DereferenceField fieldName object')

    /// The declaring type of a metadata method handle, narrowed to a closed instantiation.
    ///
    /// For consumers that can only work under a concrete instantiation. An open generic type
    /// definition, or an open construction over one's variables, is refused rather than
    /// approximated: binding an invocation under `G&lt;&gt;` needs a formal type context — the
    /// definition's own type variables — and `ConcreteTypeHandle` cannot express one. Consumers that need only the *layout* of a
    /// definition should ask `VirtualSlotLayout.slotTableOfDefinition`, and those that need the
    /// types a definition's signature *reflects as* should ask
    /// `ReflectedTypeTarget.reflectedTypeTarget`; both carry a formal context of their own.
    let requireClosedDeclaringType (operation : string) (identity : MetadataMethodIdentity) : ConcreteTypeHandle =
        match identity.GetDeclaringType () with
        | RuntimeTypeHandleTarget.Closed handle -> handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringIdentity ->
            failwith
                $"TODO: %s{operation} on a method declared by open generic type definition %O{declaringIdentity}; this needs the definition's own type variables as a substitution context, which ConcreteTypeHandle cannot express -- reflection over such a definition names them with RuntimeTypeHandleTarget.GenericParameter instead, which no runtime type can stand in for here"
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: %s{operation} on a method declared by %O{openConstructed}; at least one of its arguments is a type variable, which ConcreteTypeHandle cannot express as a substitution context"
        | other ->
            // `MethodHandleRegistry` admits only `Closed`, `OpenGenericTypeDefinition` and
            // `OpenConstructed` when minting, so any other shape here means a handle was built
            // outside that chokepoint.
            failwith
                $"%s{operation}: declaring type %O{other} cannot declare a metadata-backed method; MethodHandleRegistry refuses to mint such a handle, so this identity did not come from it"

    /// The metadata `MethodInfo` the given identity's MethodDef token names.
    let methodInfoOfMetadataIdentity
        (operation : string)
        (state : IlMachineState)
        (identity : MetadataMethodIdentity)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let assemblyFullName = identity.GetAssemblyFullName ()

        let assembly =
            state.LoadedAssembly assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

        let methodDefHandle = identity.GetMethodDefinitionHandle().Get

        let mutable methodInfo =
            Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

        if not (assembly.Methods.TryGetValue (methodDefHandle, &methodInfo)) then
            failwith $"%s{operation}: MethodDef %O{methodDefHandle} not found in assembly %s{assemblyFullName}"

        methodInfo

    /// The method a metadata handle names, concretized under its declaring type's instantiation and
    /// the handle's own method instantiation, as a frame for it would be pushed. Returns the
    /// declaring type alongside.
    ///
    /// Refuses a handle whose declaring type is not closed (see `requireClosedDeclaringType`), and one
    /// on a generic method that binds none of its type arguments; a caller for which CoreCLR has a
    /// defined answer on such a handle must give it before asking for this.
    let concretizeClosedMetadataIdentity
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (identity : MetadataMethodIdentity)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> * ConcreteTypeHandle
        =
        let methodInfo = methodInfoOfMetadataIdentity operation state identity

        let declaringTypeHandle = requireClosedDeclaringType operation identity

        let typeGenerics =
            match declaringTypeHandle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                | Some declaringType -> declaringType.Generics
                | None ->
                    failwith
                        $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so the target method cannot be resolved"
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // The runtime-generated array methods (Get/Set/Address/.ctor) are the only members
                // of a structural type. CoreCLR resolves their signatures against
                // `GetClassOrArrayInstantiation`, which PawPrint does not model — it stores array
                // element types structurally in the handle rather than as a generic argument
                // vector. `Array_CreateInstance` is the supported route to those.
                failwith
                    $"TODO: %s{operation} on a method whose declaring type is the structural type %O{declaringTypeHandle}; CoreCLR resolves such a signature against GetClassOrArrayInstantiation, which PawPrint does not model"

        let methodGenerics = identity.GetMethodGenerics () |> ImmutableArray.CreateRange

        if methodInfo.Generics.Length <> methodGenerics.Length then
            failwith
                $"TODO: %s{operation} on generic method definition %s{methodInfo.Name}: it declares %d{methodInfo.Generics.Length} generic parameter(s) but the handle carries %d{methodGenerics.Length} generic argument(s); the managed reflection layer is expected to reject an uninstantiated generic method before the QCall"

        let state, concretized, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                typeGenerics
                methodInfo
                methodGenerics
                state

        state, concretized, declaringTypeHandle
