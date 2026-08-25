namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

/// The properties of a MethodTable-backed declaring type that CoreCLR's
/// `MethodDesc::FindOrCreateAssociatedMethodDescForReflection` (genmeth.cpp:1233) and its
/// duplicated fast-path predicate in `RuntimeMethodHandle::GetStubIfNeededInternal`
/// (runtimehandles.cpp:1901-1906) consult when deciding whether reflection needs an
/// instantiating stub.
type MethodTableStubFacts =
    {
        IsValueType : bool
        /// CoreCLR's `TypeHandle::HasInstantiation()`: the type has generic parameters, whether or
        /// not they are bound. True for both `Foo<int>` and the typical `Foo<>`.
        HasInstantiation : bool
        IsGenericTypeDefinition : bool
        IsInterface : bool
    }

/// A declaring type as the `GetStubIfNeeded` decision sees it. CoreCLR splits on
/// `TypeHandle::IsTypeDesc()` first, and a TypeDesc carries none of the MethodTable properties, so
/// the split is modelled as a DU rather than a record with meaningless fields.
[<RequireQualifiedAccess>]
type StubDeclaringType =
    /// A `TypeDesc`: byrefs and pointers (`ParamTypeDesc`), function pointers (`FnPtrTypeDesc`),
    /// and generic variables (`TypeVarTypeDesc`). Note arrays are *not* TypeDescs -- modern
    /// CoreCLR gives them MethodTables (typedesc.h:112 lists ParamTypeDesc as BYREF/PTR only).
    | TypeDesc
    | MethodTable of MethodTableStubFacts

/// What `RuntimeMethodHandle_GetStubIfNeededSlow` should do, as a description rather than an
/// action, so the decision can be pinned independently of the QCall plumbing.
[<RequireQualifiedAccess>]
type StubOutcome =
    /// Hand back the caller's own handle. CoreCLR returns the same `MethodDesc*`.
    | Original
    /// Hand back a handle for the same MethodDef, rebound onto the QCall's declaring type and the
    /// supplied method instantiation.
    | Rebind
    /// The supplied instantiation's length disagrees with the method's declared generic arity;
    /// CoreCLR throws `ArgumentException` (genmeth.cpp:1261-1262).
    | ArityMismatch

[<RequireQualifiedAccess>]
module NativeRuntimeMethodHandle =
    /// The predicate behind CoreCLR's `MethodDesc::IsGenericMethodDefinition`
    /// (method.hpp:3804: `GetClassification() == mcInstantiated &&
    /// AsInstantiatedMethodDesc()->IMD_IsGenericMethodDefinition()`), expressed over PawPrint's
    /// representation so it can be pinned independently of the reflection machinery that
    /// resolves a `RuntimeMethodHandleInternal` down to these two counts:
    ///  - `methodGenericParamCount` is the method's own declared generic-parameter count
    ///    (`MethodInfo.Generics.Length`, from metadata, independent of any instantiation) --
    ///    non-zero exactly when this method declares type parameters, which is the
    ///    method-vs-class distinction real CoreCLR draws via `mcInstantiated` classification
    ///    (only method-level generics get an `InstantiatedMethodDesc`; a non-generic method on a
    ///    generic type never does, however many class type parameters its declaring type has).
    ///  - `handleInstantiationCount` is the number of concrete type arguments bound to *this*
    ///    handle (`MethodHandle.MethodGenerics.Length`) -- zero means the handle denotes the
    ///    open/typical form (what `makeOpenMethodHandle` and `getOrAllocateInternalHandle` in
    ///    MethodHandleRegistry.fs call "the method definition"); non-zero means the handle has
    ///    been instantiated with concrete type arguments (e.g. `Foo<int>`'s IMD kind is
    ///    SharedMethodInstantiation/UnsharedMethodInstantiation, not GenericMethodDefinition).
    let isGenericMethodDefinition (methodGenericParamCount : int) (handleInstantiationCount : int) : bool =
        methodGenericParamCount > 0 && handleInstantiationCount = 0

    /// CoreCLR's `MethodDesc::HasMethodInstantiation` (method.hpp:3812), which
    /// `RuntimeMethodHandle::HasMethodInstantiation` (runtimehandles.cpp:1722) returns verbatim:
    ///
    ///     mcInstantiated == GetClassification() && AsInstantiatedMethodDesc()->IMD_HasMethodInstantiation()
    ///
    /// Despite the name this asks whether the method *declares* type parameters, not whether any are
    /// bound to the handle in hand. `mcInstantiated` is the classification given to methods with
    /// method-level generics (method.hpp:172), and `IMD_HasMethodInstantiation` (method.hpp:3520)
    /// returns TRUE outright for a generic method *definition*, falling back to
    /// `m_pPerInstInfo != NULL` otherwise -- so both the typical form and every instantiation of it
    /// answer true. `RuntimeMethodInfo.IsGenericMethod` is this predicate verbatim
    /// (RuntimeMethodInfo.CoreCLR.cs:471), and an open generic method is `IsGenericMethod = true`.
    ///
    /// So the sole input is the method's declared generic-parameter count -- the same
    /// `methodGenericParamCount` that `isGenericMethodDefinition` above takes, and deliberately not
    /// the handle's `MethodGenerics.Length`. The two predicates are related but distinct: a generic
    /// method definition has a method instantiation *and* is a generic method definition, which is
    /// why `RuntimeType.GetMethodBase` (RuntimeType.CoreCLR.cs:1940) needs both to decide between
    /// `Cache.GetGenericMethodInfo` and `Cache.GetMethod`.
    let hasMethodInstantiation (methodGenericParamCount : int) : bool = methodGenericParamCount > 0

    /// The predicate behind CoreCLR's `MethodDesc::IsNoMetadata` (method.hpp:1932), which
    /// `RuntimeMethodHandle::IsDynamicMethod` (runtimehandles.cpp:1746) returns verbatim:
    /// `FC_RETURN_BOOL(pMethod->IsNoMetadata())`.
    ///
    /// "No metadata" is CoreCLR's name for a `MethodDesc` that no MethodDef token names --
    /// `DynamicMethod`/LCG stubs, built at runtime by `Reflection.Emit` rather than read from an
    /// assembly. `RuntimeType.GetMethodBase` (RuntimeType.CoreCLR.cs:1825) branches on this
    /// *first*, because for such a method there is no declaring assembly to look a token up in;
    /// it instead recovers the `DynamicMethod` from the handle's `Resolver`. Every other reflection
    /// native in this file assumes the metadata branch was taken.
    ///
    /// PawPrint mints a no-metadata handle in exactly one place -- `ModuleHandle_GetDynamicMethod`,
    /// the QCall behind `DynamicMethod.GetMethodDescriptor()` -- so this is `true` for precisely
    /// those and `false` for everything read out of metadata.
    let isDynamicMethod (handle : MethodHandle) : bool =
        match handle with
        | MethodHandle.FromMetadata _ -> false
        | MethodHandle.FromDynamic _ -> true

    /// The nil MethodDef token, `mdMethodDefNil` (corhdr.h:1525, `(mdMethodDef)mdtMethodDef`), which
    /// CoreCLR reports for a method that no MethodDef row names. Note it is 0x06000000, not zero:
    /// `MdToken.IsNullToken` (MdImport.cs:149) recognises it by masking the table byte off, so both
    /// spellings satisfy every BCL caller and only the exact value is right.
    ///
    /// Sibling of `NativeRuntimeTypeHelpers.mdTypeDefNil`.
    let mdMethodDefNil : int32 = 0x06000000

    /// The token CoreCLR's `RuntimeMethodHandle::GetMethodDef` FCall (runtimehandles.cpp:1577)
    /// returns: `pMethod->GetMemberDef()` (method.hpp:3703).
    ///
    /// This is a function of the MethodDef row identity alone, so it needs no assembly, no
    /// `MethodInfo` and no concretization; in particular the instantiations bound to the handle do
    /// not affect it, and every instantiation of a method reports the generic definition's token.
    /// Measured on real .NET: `Gen&lt;int&gt;.Id`, `Gen&lt;string&gt;.Id` and `Gen&lt;&gt;.Id` all
    /// report one token, as do `Map` and `Map&lt;string&gt;`, and `List&lt;int&gt;.Add` and
    /// `List&lt;&gt;.Add`.
    ///
    /// A method with no metadata row reports `mdMethodDefNil`. That is an answer rather than a
    /// refusal, and callers rely on it: `RuntimeParameterInfo.GetParameters`
    /// (RuntimeParameterInfo.cs:49) tests `MdToken.IsNullToken` and skips enumerating ParamDef rows,
    /// which is how such a method gets the empty parameter metadata it should have.
    let methodDefToken (handle : MethodHandle) : int32 =
        match handle with
        | MethodHandle.FromMetadata identity ->
            // `GetMemberDef` reads a value stored on the MethodDesc at construction rather than
            // looking anything up, and `InstantiatedMethodDesc::CreateMethodDesc`
            // (genmeth.cpp:85,134) copies the generic definition's token onto every instantiation,
            // unboxing stub and instantiating stub it builds -- hence the row identity alone.
            let definitionHandle : System.Reflection.Metadata.EntityHandle =
                System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit (identity.GetMethodDefinitionHandle().Get)

            System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken definitionHandle
        | MethodHandle.FromDynamic _ ->
            // CoreCLR calls `SetMemberDef(0)` for every MethodDesc that no MethodDef row names: LCG
            // methods (dynamicmethod.cpp:178), array `Get`/`Set`/`Address` stubs (array.cpp:194),
            // and IL stubs (ilstubcache.cpp:172); `MergeToken` (method.hpp:148) then ORs
            // `mdtMethodDef` back in, so what comes out is `mdMethodDefNil` rather than 0. Dynamic
            // methods are merely the only one of those three PawPrint can be holding here:
            // `methodTableOfDeclaringType` above refuses to mint a handle for an array method, and
            // PawPrint has no IL stubs.
            mdMethodDefNil

    /// The `MethodTable*` CoreCLR's `RuntimeMethodHandle::GetMethodTable` FCall
    /// (runtimehandles.cpp:1344) returns: `pMethod->GetMethodTable()`, i.e. the MethodTable of the
    /// chunk the MethodDesc lives in (method.hpp:3687). `Error` carries the reason this declaring
    /// type cannot name a MethodTable, for the caller to prefix with its operation name.
    ///
    /// The instantiation is preserved verbatim rather than canonicalised. In CoreCLR the chunk's
    /// MethodTable is the *canonical* one, which for a shared (reference-type) instantiation is
    /// `Foo&lt;__Canon&gt;` -- but for a value-type instantiation `Foo&lt;int&gt;` is its own canonical
    /// MethodTable, so returning the exact type is what CoreCLR itself does whenever the
    /// instantiation is unshared. PawPrint has no code sharing at all: every handle its registry can
    /// mint records an exact declaring type (`MethodHandleRegistry.makeMethodHandle` /
    /// `makeOpenMethodHandle`), so the unshared regime is the only one there is, and this is the
    /// faithful projection of it rather than an approximation of `__Canon`.
    ///
    /// Note this is not `RuntimeTypeHandleTarget.OpenGenericTypeDefinition`: that models
    /// `typeof(Foo&lt;&gt;)`, whose MethodTable answers `IsGenericTypeDefinition = true`, which
    /// `Foo&lt;__Canon&gt;` does not. The two are deliberately distinct MethodTables here (see the
    /// `MethodTablePtr`/`TypeHandlePtr` CEQ arms in NativeIntSource.fs) as they are in CoreCLR.
    ///
    /// The one guest-observable difference this leaves is handle identity across reference-type
    /// instantiations: real .NET shares one MethodDesc between `List&lt;string&gt;.Add` and
    /// `List&lt;object&gt;.Add`, so their `MethodHandle.Value`s compare equal, while PawPrint mints
    /// two registry ids. That difference is created by the registry recording exact declaring types,
    /// not by this function, and would exist however this FCall answered.
    let methodTableOfDeclaringType (declaringType : ConcreteTypeHandle) : Result<RuntimeTypeHandleTarget, string> =
        match declaringType with
        | ConcreteTypeHandle.Concrete _ -> Ok (RuntimeTypeHandleTarget.Closed declaringType)
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // TypeDescs own no MethodDescs in CoreCLR, so no MethodDesc's MethodTable can be one:
            // `TypeHandleTag.forTarget` classifies exactly these shapes as TypeDesc-tagged, and
            // `NativeIntSource.MethodTablePtr` requires producer sites to reject them.
            Error
                $"declaring type %O{declaringType} is a byref/pointer/function-pointer, which is a TypeDesc and owns no MethodTable; a MethodDesc's declaring type is never a TypeDesc"
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Arrays do have MethodTables (they are not TypeDescs), and in CoreCLR their `Get`/`Set`/
            // `Address` MethodDescs really do live on them -- so passing an array handle straight
            // through would be the CoreCLR-honest answer. It is refused only because nothing in
            // PawPrint can mint such a handle today: both `MethodHandle` constructors resolve a
            // nominal type identity plus generics, which always yields `Concrete`. Whoever teaches
            // the registry about array intrinsic methods should delete this arm rather than work
            // around it.
            Error
                $"declaring type %O{declaringType} is an array; array intrinsic methods (Get/Set/Address) are not represented in the method-handle registry, so no handle should name one"

    /// CoreCLR's `MethodDesc::IsClassConstructorOrCtor` (method.hpp:491), which
    /// `RuntimeMethodHandle::IsConstructor` (runtimehandles.cpp:2135) returns verbatim:
    ///
    ///     DWORD dwAttrs = GetAttrs();
    ///     if (IsMdRTSpecialName(dwAttrs))
    ///     {
    ///         LPCUTF8 name = GetName();
    ///         return IsMdInstanceInitializer(dwAttrs, name) || IsMdClassConstructor(dwAttrs, name);
    ///     }
    ///     return FALSE;
    ///
    /// Both inner macros re-test `mdRTSpecialName` and compare the name with `strcmp`
    /// (corhdr.h:433-435), so the predicate is exactly: the `RTSpecialName` flag is set, and the
    /// name is `.ctor` or `.cctor` (case-sensitively). Neither macro consults `mdStatic`, so a
    /// `.cctor` is recognised by flag and name alone; and the flag alone does not suffice, since
    /// `RTSpecialName` is also set on other runtime-special members.
    ///
    /// Despite the FCall's name this covers *class* constructors too, and `RuntimeType.GetMethodBase`
    /// (RuntimeType.CoreCLR.cs:1932) relies on that: the answer selects whether the guest is handed a
    /// `RuntimeConstructorInfo` or a `RuntimeMethodInfo`.
    ///
    /// This is deliberately not shared with the `.ctor` searches in `IlMachineStateExecution`
    /// (fs:1728, 1783, 1793), which answer a different question -- whether a type has a
    /// parameterless *instance* constructor to invoke -- and so need arity and staticness while
    /// excluding `.cctor`. CoreCLR draws the same distinction, between `IsCtor` (method.hpp:967) and
    /// this predicate.
    let isConstructorOrClassConstructor (attributes : MethodAttributes) (name : string) : bool =
        attributes.HasFlag MethodAttributes.RTSpecialName
        && (name = ".ctor" || name = ".cctor")

    /// The instantiation CoreCLR's `MethodDesc::LoadMethodInstantiation` (method.cpp:793) reports
    /// for a method, expressed over PawPrint's representation so it can be pinned independently of
    /// the QCall plumbing. The two counts are exactly the ones `isGenericMethodDefinition` above
    /// consumes, and the three arms line up with `MethodDesc::GetMethodInstantiation`
    /// (method.hpp:3787):
    ///  - a non-generic method is never `mcInstantiated`, so its instantiation is empty;
    ///  - a *generic method definition* (the typical form: the method declares type parameters but
    ///    this handle binds none) reports its own type variables, i.e. `[T]` for `void Foo&lt;T&gt;()`.
    ///    This is not an empty list: `IMD_GetMethodInstantiation` (method.hpp:3531) returns the
    ///    typical instantiation's `TypeVarTypeDesc`s;
    ///  - an instantiated generic method reports the type arguments bound to the handle.
    ///
    /// <c>declaringType</c> is the *uninstantiated* metadata identity of the type that declares the
    /// method, which is what <c>RuntimeTypeHandleTarget.MethodGenericParameter</c> carries (a
    /// <c>ResolvedTypeIdentity</c> has no room for an instantiation). That is not a lossy shortcut:
    /// CoreCLR canonicalises the same way, redirecting any non-typical generic method definition to
    /// <c>LoadTypicalMethodDefinition()->GetMethodInstantiation()</c> (method.cpp:803-806), so a
    /// method's type variables are reported against the typical declaring type however the handle
    /// was reached.
    let methodInstantiationTargets
        (operation : string)
        (declaringType : ResolvedTypeIdentity)
        (methodDefinition : ComparableMethodDefinitionHandle)
        (methodGenericParamCount : int)
        (handleInstantiation : ConcreteTypeHandle list)
        : RuntimeTypeHandleTarget list
        =
        if methodGenericParamCount < 0 then
            failwith
                $"%s{operation}: method %O{methodDefinition.Get} reported a negative generic-parameter count %d{methodGenericParamCount}"

        match handleInstantiation with
        | [] ->
            if isGenericMethodDefinition methodGenericParamCount 0 then
                List.init
                    methodGenericParamCount
                    (fun position ->
                        RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, methodDefinition, position)
                    )
            else
                []
        | _ ->
            // A bound handle must bind exactly as many arguments as the method declares. A
            // mismatch means the registry and the metadata disagree about the same method, which
            // would silently produce a wrong-length `RuntimeType[]`; refuse instead.
            if List.length handleInstantiation <> methodGenericParamCount then
                failwith
                    $"%s{operation}: method %O{methodDefinition.Get} on %O{declaringType.TypeDefinition.Get} declares %d{methodGenericParamCount} generic parameters but its handle binds %d{List.length handleInstantiation} type arguments"

            handleInstantiation |> List.map RuntimeTypeHandleTarget.Closed

    /// CoreCLR's `RuntimeMethodHandle::GetStubIfNeededInternal` FCall predicate
    /// (runtimehandles.cpp:1901-1906):
    ///
    ///     pMethod->HasMethodInstantiation()
    ///     || (!instType.IsValueType()
    ///         && (!instType.HasInstantiation() || instType.IsGenericTypeDefinition()))
    ///
    /// When true, the fast path hands back the original `MethodDesc*` and the slow QCall is never
    /// reached. CoreCLR notes this logic is "duplicated from
    /// FindOrCreateAssociatedMethodDescForReflection" (runtimehandles.cpp:1899-1900), which is what
    /// makes the cross-check property in TestNativeRuntimeMethodHandle.fs meaningful: whenever this
    /// says "original", `stubOutcome` must agree.
    ///
    /// A TypeDesc answers `false` to both `IsValueType()` and `HasInstantiation()`, so it satisfies
    /// the second disjunct and returns the original -- consistent with `stubOutcome`'s TypeDesc arm.
    let fastPathReturnsOriginal (methodHasInstantiation : bool) (declaringType : StubDeclaringType) : bool =
        if methodHasInstantiation then
            true
        else
            match declaringType with
            | StubDeclaringType.TypeDesc -> true
            | StubDeclaringType.MethodTable facts ->
                not facts.IsValueType
                && (not facts.HasInstantiation || facts.IsGenericTypeDefinition)

    /// CoreCLR's `MethodDesc::FindOrCreateAssociatedMethodDescForReflection` (genmeth.cpp:1233),
    /// as reached through `RuntimeMethodHandle_GetStubIfNeededSlow`.
    ///
    /// `methodGenericParamCount` is the method's declared generic arity
    /// (`pMethod->GetNumGenericMethodArgs()`, and `HasMethodInstantiation()` is that being
    /// non-zero); `methodInstantiationCount` is the length of the decoded `RuntimeType[]`, where
    /// CoreCLR treats a null array and an empty one alike (runtimehandles.cpp:1936 guards on
    /// non-null, and an empty array yields `ntypars = 0`).
    ///
    /// Only the instantiation's *length* bears on the decision, and taking it that way keeps
    /// callers honest about ordering: CoreCLR returns for a TypeDesc declaring type before the
    /// instantiation is inspected at all, so a caller must not do work that can fail on the
    /// instantiation's *contents* until this has answered `Rebind`.
    let stubOutcome
        (declaringType : StubDeclaringType)
        (methodIsStatic : bool)
        (methodGenericParamCount : int)
        (methodInstantiationCount : int)
        : StubOutcome
        =
        if methodInstantiationCount < 0 then
            failwith
                $"RuntimeMethodHandle.GetStubIfNeededSlow: method instantiation count must be non-negative, got %d{methodInstantiationCount}"

        match declaringType with
        | StubDeclaringType.TypeDesc ->
            // genmeth.cpp:1247-1249: "no stubs for TypeDesc". This runs *before* the instantiation
            // is examined, so even a non-empty (or wrongly-sized) instantiation returns the
            // original here rather than being validated.
            StubOutcome.Original
        | StubDeclaringType.MethodTable facts ->

        if methodInstantiationCount > 0 then
            // genmeth.cpp:1256-1270: BindGenericParameters() was called, so an instantiating stub
            // is always wanted. CoreCLR asserts `pMethod->HasMethodInstantiation()` here; in a
            // release build that assert is absent and the arity check below rejects the same
            // condition, since a non-generic method has arity 0 and the instantiation is non-empty.
            if methodInstantiationCount <> methodGenericParamCount then
                StubOutcome.ArityMismatch
            else
                StubOutcome.Rebind
        else
            // genmeth.cpp:1272-1277. Needs an instantiating stub if the method is non-generic and
            // it is a non-generic static method on a generic class, a non-generic method on a
            // struct, or a non-generic method on a generic interface.
            let needsStub =
                methodGenericParamCount = 0
                && (facts.IsValueType
                    || (facts.HasInstantiation
                        && not facts.IsGenericTypeDefinition
                        && (facts.IsInterface || methodIsStatic)))

            if needsStub then
                StubOutcome.Rebind
            else
                StubOutcome.Original

    /// Project a `RuntimeTypeHandleTarget` onto the facts `stubOutcome` consumes, i.e. classify it
    /// the way CoreCLR's `TypeHandle` would.
    let private stubDeclaringTypeOfTarget
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : StubDeclaringType
        =
        let factsOfTypeInfo
            (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (hasInstantiation : bool)
            (isGenericTypeDefinition : bool)
            : StubDeclaringType
            =
            StubDeclaringType.MethodTable
                {
                    IsValueType = DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo
                    HasInstantiation = hasInstantiation
                    IsGenericTypeDefinition = isGenericTypeDefinition
                    IsInterface = typeInfo.TypeAttributes.HasFlag TypeAttributes.Interface
                }

        let typeInfoOf (identity : ResolvedTypeIdentity) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            let assembly =
                state.LoadedAssembly identity.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: assembly %s{identity.AssemblyFullName} is not loaded"
                )

            assembly.TypeDefs.[identity.TypeDefinition.Get]

        match target with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeMethodHandle.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            let concreteType =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle %O{handle} is not registered in ConcreteTypes"
                )

            // A `Closed` handle is fully bound, so it is never a generic type *definition*; it has
            // an instantiation exactly when it was built with generic arguments.
            factsOfTypeInfo (typeInfoOf concreteType.Identity) (not concreteType.Generics.IsEmpty) false
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _) ->
            // Arrays carry MethodTables in modern CoreCLR (only byrefs/pointers, function pointers
            // and generic variables are TypeDescs). An array is a reference type with no
            // instantiation of its own, so no stub is ever needed for a method on one.
            StubDeclaringType.MethodTable
                {
                    IsValueType = false
                    HasInstantiation = false
                    IsGenericTypeDefinition = false
                    IsInterface = false
                }
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _) ->
            // ParamTypeDesc (BYREF, PTR) and FnPtrTypeDesc.
            StubDeclaringType.TypeDesc
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            // `typeof(G<>)` is a MethodTable in CoreCLR -- the typical instantiation -- with
            // HasInstantiation and IsGenericTypeDefinition both true.
            factsOfTypeInfo (typeInfoOf identity) true true
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // TypeVarTypeDesc.
            StubDeclaringType.TypeDesc

    /// Resolve a `RuntimeMethodHandleInternal` argument to the `MethodHandle` it denotes.
    let private resolveMethodHandleFromArg
        (operation : string)
        (state : IlMachineState)
        (arg : CliType)
        : MethodHandle
        =
        // CoreCLR's RuntimeMethodHandle FCalls dereference the MethodDesc* directly and
        // assert non-null; PawPrint's existing callers never yield a null handle, so we
        // surface a contract violation rather than silently producing a default value.
        let methodHandleId =
            NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation arg
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: null RuntimeMethodHandleInternal")

        MethodHandleRegistry.resolveMethodFromId methodHandleId state.MethodHandles
        |> Option.defaultWith (fun () ->
            failwith $"%s{operation}: registry id %d{methodHandleId} did not resolve to a known MethodHandle"
        )

    /// The declaring type of a metadata method handle, narrowed to a closed instantiation.
    ///
    /// For consumers that can only work under a concrete instantiation. An open generic type
    /// definition is refused rather than approximated: binding an invocation under `G&lt;&gt;`
    /// needs a formal type context — the definition's own type variables — and
    /// `ConcreteTypeHandle` cannot express one. Consumers that need only the *layout* of a
    /// definition should ask `VirtualSlotLayout.slotTableOfDefinition`, and those that need the
    /// types a definition's signature *reflects as* should ask
    /// `NativeRuntimeTypeHelpers.reflectedTypeTarget`; both carry a formal context of their own.
    let requireClosedDeclaringType (operation : string) (identity : MetadataMethodIdentity) : ConcreteTypeHandle =
        match identity.GetDeclaringType () with
        | RuntimeTypeHandleTarget.Closed handle -> handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringIdentity ->
            failwith
                $"TODO: %s{operation} on a method declared by open generic type definition %O{declaringIdentity}; this needs the definition's own type variables as a substitution context, which ConcreteTypeHandle cannot express -- reflection over such a definition names them with RuntimeTypeHandleTarget.GenericParameter instead, which no runtime type can stand in for here"
        | other ->
            // `MethodHandleRegistry` admits only `Closed` and `OpenGenericTypeDefinition` when
            // minting, so any other shape here means a handle was built outside that chokepoint.
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

    /// Resolve a `RuntimeMethodHandleInternal` argument to the metadata identity it denotes.
    /// Every native that reads a MethodDef token, a declaring assembly, or a method instantiation
    /// needs one of these, and none of them has an answer for a no-metadata (`DynamicMethod`)
    /// handle: there is no token to read.
    ///
    /// Several of the operations funnelled through here are perfectly legal on a dynamic method in
    /// CoreCLR -- `GetMethodTable`/`GetDeclaringType` answer with the `DynamicMethodTable`'s
    /// synthetic type, which is how `Signature`'s constructor (RuntimeHandles.cs:2051) works on an
    /// LCG method -- so this is a "not implemented yet" boundary rather than a caller bug, and it
    /// says so. It is where the next increment of `Reflection.Emit` support will start.
    let resolveMetadataIdentityFromArg
        (operation : string)
        (state : IlMachineState)
        (arg : CliType)
        : MetadataMethodIdentity
        =
        match resolveMethodHandleFromArg operation state arg with
        | MethodHandle.FromMetadata identity -> identity
        | MethodHandle.FromDynamic dynamicHandle ->
            let name =
                MethodHandleRegistry.resolveDynamicMethod dynamicHandle state.MethodHandles
                |> Option.map (fun definition -> definition.GetName ())
                |> Option.defaultValue "<unregistered>"

            failwith
                $"TODO: %s{operation} was given %O{dynamicHandle} (%s{name}), a Reflection.Emit method with no MethodDef token to read; PawPrint mints these in ModuleHandle_GetDynamicMethod but cannot yet answer metadata queries about them"

    let private resolveMethodInfoFromHandleArg
        (operation : string)
        (state : IlMachineState)
        (arg : CliType)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        resolveMetadataIdentityFromArg operation state arg
        |> methodInfoOfMetadataIdentity operation state

    /// Resolve a <c>QCallTypeHandle</c>-encoded type to its
    /// <c>(DumpedAssembly, TypeInfo)</c>, accepting the MethodTable-backed
    /// cases (closed concrete instantiations and open generic type
    /// definitions) and refusing the TypeDesc-backed cases that
    /// <c>RuntimeMethodHandle_IsCAVisibleFromDecoratedType</c>'s CoreCLR
    /// sibling rejects with <c>Arg_InvalidHandle</c>
    /// (arrays/byrefs/pointers/fnptrs and generic parameters).
    /// <c>typeof(G&lt;&gt;)</c> reaches this QCall as the decorated source
    /// when reflection walks custom attributes on a generic type definition,
    /// so <c>OpenGenericTypeDefinition</c> must be accepted or ordinary CA
    /// filtering on open generics breaks.
    let private resolveMethodTableType
        (operation : string)
        (label : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let fromIdentity (identity : ResolvedTypeIdentity) =
            let assembly =
                state.LoadedAssembly identity.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: assembly %s{identity.AssemblyFullName} for %s{label} is not loaded"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            assembly, typeInfo

        match target with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeMethodHandle.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.Closed handle ->
            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | None -> failwith $"%s{operation}: %s{label} concrete handle %O{handle} not found in AllConcreteTypes"
                | Some concreteType ->
                    let assembly =
                        state.LoadedAssembly concreteType.AssemblyFullName
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly %s{concreteType.AssemblyFullName} for %s{label} is not loaded"
                        )

                    let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                    assembly, typeInfo
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // CoreCLR treats arrays/byrefs/pointers/fnptrs as TypeDescs; its
                // RuntimeMethodHandle_IsCAVisibleFromDecoratedType throws
                // Arg_InvalidHandle (kArgumentNullException) when sourceHandle or
                // targetHandle is a TypeDesc. PawPrint doesn't yet have a host
                // helper to raise that exception object, so surface the precise
                // condition for the caller to fix at the source.
                failwith
                    $"TODO: %s{operation}: %s{label} is a structural type (%O{handle}); CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            // typeof(G<>) is a MethodTable in CoreCLR (the "typical
            // instantiation"); reflection passes it here when filtering CAs on
            // a generic type definition. The MethodTable carries the same
            // TypeAttributes and nesting chain as any other instantiation, so
            // an access check using the TypeDef's own attributes is correct.
            fromIdentity identity
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"TODO: %s{operation}: %s{label} is a generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: %s{operation}: %s{label} is a method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; CoreCLR throws ArgumentNullException(\"Arg_InvalidHandle\") for TypeDesc handles here"

    /// Build a type's enclosing-type chain (innermost first, outermost last),
    /// where each entry projects only the bits <c>AccessCheck.canAccessClass</c>
    /// inspects. The walk terminates at the outermost top-level type whose
    /// <c>DeclaringType</c> handle is nil.
    let private buildAccessLevelChain
        (operation : string)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : AccessLevelInfo list
        =
        let mutable current = typeInfo
        let acc = ResizeArray<AccessLevelInfo> ()

        let toLevel (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : AccessLevelInfo =
            {
                Visibility = ti.TypeAttributes
                Name = ti.Name
            }

        acc.Add (toLevel current)

        while not current.DeclaringType.IsNil do
            match assembly.TypeDefs.TryGetValue current.DeclaringType with
            | true, parent ->
                acc.Add (toLevel parent)
                current <- parent
            | false, _ ->
                failwith
                    $"%s{operation}: nested type %s{current.Namespace}.%s{current.Name} has DeclaringType handle %O{current.DeclaringType} that is not present in assembly %s{assembly.Name.Name}"

        List.ofSeq acc

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "RuntimeMethodHandle_IsCAVisibleFromDecoratedType",
          "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "IsCAVisibleFromDecoratedType",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", attrGenerics)
            CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", ctorGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", sourceGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
            attrGenerics.IsEmpty
            && ctorGenerics.IsEmpty
            && sourceGenerics.IsEmpty
            && moduleGenerics.IsEmpty
            && boolGenerics.IsEmpty
            ->
            // Mirrors CoreCLR's RuntimeMethodHandle_IsCAVisibleFromDecoratedType
            // (runtimehandles.cpp). Decides whether a custom-attribute type's
            // constructor is visible from a decorated type when reflecting custom
            // attributes; reflection filters CA instances using this check.
            let operation = "RuntimeMethodHandle.IsCAVisibleFromDecoratedType"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let attrTypeArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let attrCtorArg = instruction.Arguments.[1]
            let sourceTypeArg = instruction.Arguments.[2] |> EvalStackValue.ofCliType
            let sourceModuleArg = instruction.Arguments.[3] |> EvalStackValue.ofCliType

            // Target: the custom-attribute type and (optionally) its constructor.
            let attrTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state attrTypeArg

            let attrAssembly, attrTypeInfo =
                resolveMethodTableType operation "attribute type" state attrTarget

            // CoreCLR: if pCACtor is NULL, look up the default ctor of the target
            // type. If that lookup fails and the target is not a value type, throw
            // MissingMethodException; if it is a value type, fall back to mdPublic.
            let attrCtorId : int64 option =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation attrCtorArg

            let attrCtorMethodOpt : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option =
                match attrCtorId with
                | Some _ ->
                    // The caller supplied a non-null RuntimeMethodHandleInternal;
                    // resolve it through the registry the same way the other arms do.
                    Some (resolveMethodInfoFromHandleArg operation state attrCtorArg)
                | None ->
                    // Look up the default (parameterless instance) ctor on the
                    // attribute type. CoreCLR's MethodTable::GetDefaultConstructor
                    // walks the type's vtable looking for an instance ctor with no
                    // parameters; we approximate that with the same "name = .ctor,
                    // not static, no parameters" predicate used elsewhere
                    // (IlMachineStateExecution.fs activator paths).
                    attrTypeInfo.Methods
                    |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0)

            let attrCtorAttrs : MethodAttributes =
                match attrCtorMethodOpt with
                | Some (MethodInfo.Synthesised _ as m) ->
                    // A custom attribute's constructor is always a declared method; a synthesised
                    // one has no attribute flags to report.
                    failwith $"%s{operation}: attribute constructor %O{m} is synthesised and has no MethodAttributes"
                | Some (MethodInfo.Metadata (_, facts)) -> facts.MethodAttributes
                | None ->
                    // No constructor was supplied or found.
                    if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies attrTypeInfo then
                        // CoreCLR: value types fall through with dwAttr = mdPublic, so
                        // canAccessMethod only checks class visibility.
                        MethodAttributes.Public
                    else
                        // CoreCLR throws MissingMethodException(COR_CTOR_METHOD_NAME_W).
                        // PawPrint doesn't yet have a host helper to raise that from a
                        // QCall, so surface the precise condition the same way the
                        // Activator paths do.
                        failwith
                            $"TODO: %s{operation}: attribute type %s{attrTypeInfo.Namespace}.%s{attrTypeInfo.Name} has no default constructor; CoreCLR throws MissingMethodException"

            let targetChain = buildAccessLevelChain operation attrAssembly attrTypeInfo

            // Source / accessor: the decorated type (which may be null, in which
            // case CoreCLR builds an AccessCheckContext with a NULL pDecoratedMT
            // and only the assembly is consulted) plus the assembly carried by the
            // QCallModule.
            let sourceTargetOpt =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTargetOption operation state sourceTypeArg

            let sourceModuleAssemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName operation state sourceModuleArg

            let sourceAssembly =
                state.LoadedAssembly sourceModuleAssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: source module's assembly %s{sourceModuleAssemblyFullName} is not loaded"
                )

            let sourceChain =
                match sourceTargetOpt with
                | None ->
                    // CoreCLR: AccessCheckContext(NULL, pDecoratedMT=NULL, sourceAsm).
                    // AccessCheck.canAccessClass only iterates target.TypeChain, so the
                    // accessor's chain is unused in this slice. An empty list reflects
                    // "no decorated type", and any future widening that does consume
                    // it will fail loudly rather than silently using a default.
                    []
                | Some target ->
                    let _, sourceTypeInfo =
                        resolveMethodTableType operation "decorated type" state target

                    buildAccessLevelChain operation sourceAssembly sourceTypeInfo

            let accessor : AccessParty =
                {
                    TypeChain = sourceChain
                    Assembly = sourceAssembly.Name
                    Friends = sourceAssembly.Friends
                }

            let target : AccessParty =
                {
                    TypeChain = targetChain
                    Assembly = attrAssembly.Name
                    Friends = attrAssembly.Friends
                }

            let sameAssembly = accessor.Assembly.FullName = target.Assembly.FullName

            let visible = AccessCheck.canAccessMethod sameAssembly accessor target attrCtorAttrs

            // Interop.BOOL is int-backed with FALSE=0, TRUE=1.
            let state =
                let ret = if visible then 1 else 0
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeMethodHandle_GetIsCollectible",
          "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetIsCollectible",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", handleGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
            handleGenerics.IsEmpty && boolGenerics.IsEmpty
            ->
            let operation = "RuntimeMethodHandle.GetIsCollectible"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            // CoreCLR is `pMethod->GetLoaderAllocator()->IsCollectible()`
            // (runtimehandles.cpp:1294), on a `MethodDesc*` it asserts non-null. Resolving the
            // handle keeps that precondition; the resolved method is not consulted, because with
            // one loader allocator the answer cannot depend on it. `FromDynamic` is admitted here
            // rather than refused: the resolution below accepts either kind, and a dynamic method
            // never reaches this QCall anyway -- `DynamicMethod` does not override
            // `MemberInfo.IsCollectible`, so it answers `true` from the managed default without
            // asking the runtime.
            resolveMethodHandleFromArg operation state instruction.Arguments.[0]
            |> ignore<MethodHandle>

            // Interop.BOOL is int-backed with FALSE = 0 and TRUE = 1.
            let state =
                let ret =
                    if LoaderAllocator.isCollectible LoaderAllocator.Global then
                        1
                    else
                        0

                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "RuntimeMethodHandle_GetMethodInstantiation",
          "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetMethodInstantiation",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", handleGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics)
            CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics) ],
          MethodReturnType.Void when handleGenerics.IsEmpty && objectHandleGenerics.IsEmpty && boolGenerics.IsEmpty ->
            // CoreCLR runtimehandles.cpp:1708:
            //   Instantiation inst = pMethod->LoadMethodInstantiation();
            //   retTypes.Set(CopyRuntimeTypeHandles(inst.GetRawArgs(), inst.GetNumArgs(),
            //                                       fAsRuntimeTypeArray ? CLASS__CLASS : CLASS__TYPE));
            // See `methodInstantiationTargets` above for the instantiation itself.
            let operation = "RuntimeMethodHandle.GetMethodInstantiation"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let identity =
                resolveMetadataIdentityFromArg operation state instruction.Arguments.[0]

            let methodInfo = methodInfoOfMetadataIdentity operation state identity

            let retTypes =
                NativeCall.objectHandleOnStackTarget operation state "retTypes" instruction.Arguments.[1]

            // Interop.BOOL is an int32-backed enum. TRUE selects RuntimeType[] (CLASS__CLASS);
            // FALSE selects Type[] (CLASS__TYPE).
            let asRuntimeTypeArray =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[2] with
                | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
                | other -> failwith $"%s{operation}: expected Interop.BOOL as Int32, got %O{other}"

            let targets =
                methodInstantiationTargets
                    operation
                    methodInfo.RequiredDeclaringType.Identity
                    (identity.GetMethodDefinitionHandle ())
                    methodInfo.Generics.Length
                    (identity.GetMethodGenerics ())

            // An empty instantiation leaves `retTypes` unwritten, so the caller's local stays
            // null. That is what CopyRuntimeTypeHandles does for 0 args (runtimehandles.cpp:573),
            // and the managed wrappers are written for it: `GetMethodInstantiationPublic` launders
            // the null through `?? Type.EmptyTypes` (RuntimeMethodInfo.CoreCLR.cs:461), while
            // `GetMethodInstantiationInternal` propagates it via a null-forgiving `types!`
            // (RuntimeHandles.cs:1217). The latter's nullable-oblivious signature is not a claim
            // that the null never arrives: `RuntimeType.GetMethodBase` calls it whenever the
            // handle is not a generic method *definition* -- which includes every non-generic
            // method -- and deliberately tolerates the result being null, passing it on to
            // `GetStubIfNeeded` under the comment "If methodInstantiation is not null,
            // GetStubIfNeeded will rebind the generic method arguments"
            // (RuntimeType.CoreCLR.cs:1905-1929). So writing a zero-length array here instead
            // would be observably wrong, not merely redundant.
            let state =
                NativeRuntimeTypeHelpers.copyRuntimeTypeHandles
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    asRuntimeTypeArray
                    retTypes
                    targets

            NativeHandlerResult.completed state |> Some
        | "RuntimeMethodHandle_GetStubIfNeededSlow",
          "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetStubIfNeededSlow",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", handleGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System",
                                                                     "RuntimeMethodHandleInternal",
                                                                     retGenerics)) when
            handleGenerics.IsEmpty
            && qCallGenerics.IsEmpty
            && objectHandleGenerics.IsEmpty
            && retGenerics.IsEmpty
            ->
            // CoreCLR runtimehandles.cpp:1914. The slow half of `RuntimeMethodHandle.GetStubIfNeeded`:
            // decode the optional `RuntimeType[]` instantiation and delegate to
            // `MethodDesc::FindOrCreateAssociatedMethodDescForReflection` (genmeth.cpp:1233). See
            // `stubOutcome` above for the decision itself.
            //
            // PawPrint's `MethodHandle` already records the declaring type and method instantiation
            // a CoreCLR instantiating stub exists to supply -- there is no shared canonical code
            // here -- so "create a stub" is just "register the handle denoting this method at this
            // instantiation".
            let operation = "RuntimeMethodHandle.GetStubIfNeededSlow"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let methodHandleId =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null RuntimeMethodHandleInternal")

            let identity =
                resolveMetadataIdentityFromArg operation state instruction.Arguments.[0]

            let methodInfo = methodInfoOfMetadataIdentity operation state identity

            let declaringTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (instruction.Arguments.[1] |> EvalStackValue.ofCliType)

            let instantiationSource =
                NativeCall.objectHandleOnStackTarget operation state "methodInstantiation" instruction.Arguments.[2]

            // CoreCLR treats a null array and an empty one alike here, so collapse them.
            let instantiationTargets : RuntimeTypeHandleTarget list =
                NativeRuntimeTypeHelpers.readRuntimeTypeHandleArray
                    ctx.BaseClassTypes
                    operation
                    "methodInstantiation"
                    state
                    instantiationSource
                |> Option.defaultValue []

            let declaringFacts =
                stubDeclaringTypeOfTarget operation ctx.BaseClassTypes state declaringTarget

            match
                stubOutcome
                    declaringFacts
                    methodInfo.IsStatic
                    methodInfo.Generics.Length
                    (List.length instantiationTargets)
            with
            | StubOutcome.ArityMismatch ->
                // genmeth.cpp:1261-1262. `RuntimeType.SanityCheckGenericArguments` already screens
                // this on the managed side, so reaching here means a BCL path we don't model got
                // through; raise the same exception CoreCLR would rather than trusting the screen.
                NativeHandlerResult.raiseException ctx.BaseClassTypes.ArgumentException state
                |> Some
            | StubOutcome.Original ->
                let state =
                    MethodHandleRegistry.internalHandleFromId ctx.BaseClassTypes state.ConcreteTypes methodHandleId
                    |> CliType.ValueType
                    |> fun handle -> IlMachineState.pushToEvalStack handle ctx.Thread state

                NativeHandlerResult.completed state |> Some
            | StubOutcome.Rebind ->

            // Only now that a stub is actually wanted do we require the instantiation's *elements*
            // to be closed. Narrowing earlier would reject inputs CoreCLR accepts: its TypeDesc arm
            // returns before the instantiation is inspected at all, so a method whose declaring type
            // is a byref/pointer/fnptr/type-variable ignores whatever was passed.
            let methodInstantiation : ConcreteTypeHandle list =
                instantiationTargets
                |> List.mapi (fun index target ->
                    match target with
                    | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                        RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                    | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                        failwith
                            $"TODO: open constructed types are not handled at Native/NativeRuntimeMethodHandle.fs:%s{__LINE__}; got %O{openConstructed}"
                    | RuntimeTypeHandleTarget.Closed handle -> handle
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                    | RuntimeTypeHandleTarget.GenericParameter _
                    | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                        // Reached by `MakeGenericMethod` with a type argument that still contains
                        // generic parameters -- `M.MakeGenericMethod(typeof(G<>))` or
                        // `M.MakeGenericMethod(someTypeParameter)`. Both are legal: real .NET
                        // returns a MethodInfo with `ContainsGenericParameters = true` (verified
                        // against the runtime), which you can inspect but not invoke.
                        //
                        // PawPrint cannot represent one yet: `MethodHandle.MethodGenerics` is a
                        // `ConcreteTypeHandle list`, and `ConcreteTypeHandle` is closed by
                        // construction (it indexes `AllConcreteTypes`, whose entries are identity
                        // plus *closed* generic arguments). Widening it is a change to the core
                        // registry representation that reaches concretization and every other
                        // MethodHandle consumer, so it is deliberately not attempted here; see
                        // `sourcesPure/MakeGenericMethodOpenArgument.cs`, which is parked in
                        // TestPureCases.unimplemented against this gap.
                        failwith
                            $"TODO: %s{operation}: methodInstantiation[%d{index}] is %O{target}, which is not a closed type; this is MakeGenericMethod with an open type argument, and PawPrint's MethodHandle can only bind closed method generic arguments"
                )

            match declaringTarget, methodInstantiation with
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition definition, [] ->
                // CoreCLR's `instType` is the definition's *typical* instantiation, and the stub it
                // asks `FindOrCreateAssociatedMethodDesc` for has that typical MethodTable as its
                // exact one (genmeth.cpp:1288-1297): an instantiating stub, plus an unboxing stub
                // when the method is virtual, since the typical `SBox<T>` MethodTable canonicalises
                // to `SBox<__Canon>` and its methods `RequiresInstArg`. PawPrint collapses the
                // definition's method, the instantiating stub and the unboxing stub onto one
                // registry id, which preserves every equality a guest can observe: a stub's
                // reflection-visible identity is exactly (typical MethodTable, MethodDef token,
                // empty method instantiation), and every managed route to a `MethodBase` normalises
                // through this QCall or through the `PopulateConstructors`/`PopulateMethods` that
                // call it, so an un-normalised walk `MethodDesc` never reaches a guest.
                //
                // Naming that identity needs no substitution context. `MetadataMethodIdentity`
                // records its declaring type as a `RuntimeTypeHandleTarget`, so a definition is an
                // ordinary declaring type rather than a stand-in for one, and this is the very
                // identity `RuntimeTypeHandle.GetFirstIntroducedMethod` mints -- the registry dedups
                // against that rather than issuing a second id. The closed path below concretizes
                // only to derive the equivalent tuple, and with no method generic arguments to bind
                // there is nothing for a substitution context to substitute.
                if not methodInfo.Generics.IsEmpty then
                    // `stubOutcome` answers `Rebind` on an empty instantiation only through
                    // `needsStub`, whose first conjunct is `methodGenericParamCount = 0`. That is
                    // also why no constraint validation runs here: `validateConstraintsOn` zips the
                    // method's declared generic parameters against the arguments being bound, and
                    // both are empty. Asserted rather than assumed, because the theorem lives in a
                    // different function; were it to change, this arm would quietly mint a generic
                    // method *definition*'s handle in place of a rebind.
                    failwith
                        $"%s{operation}: %s{MethodOwner.describe methodInfo.Owner}.%s{methodInfo.Name} declares %d{methodInfo.Generics.Length} generic parameters, but this rebind onto %O{declaringTarget} binds none"

                if definition <> methodInfo.RequiredDeclaringType.Identity then
                    // The MethodDef token is scoped to the assembly the handle names, and the
                    // declaring type minted here comes from the QCall's `instType` instead, so the
                    // two must agree about which type declares the method. CoreCLR carries the
                    // weakened form of this precondition into
                    // `FindOrCreateAssociatedMethodDesc` (genmeth.cpp:753-806), which walks to the
                    // exact declaring type rather than trusting the pair.
                    failwith
                        $"%s{operation}: rebinding %s{methodInfo.Name} onto %O{declaringTarget}, but its MethodDef row is declared by %s{MethodOwner.describe methodInfo.Owner} in %s{identity.GetAssemblyFullName ()}"

                let handleValue, registry =
                    MethodHandleRegistry.getOrAllocateInternalHandle
                        ctx.BaseClassTypes
                        state.ConcreteTypes
                        (identity.GetAssemblyFullName ())
                        declaringTarget
                        methodInfo
                        state.MethodHandles

                { state with
                    MethodHandles = registry
                }
                |> IlMachineState.pushToEvalStack (CliType.ValueType handleValue) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | _ ->

            // Rebinding needs the declaring type's own generic arguments as the substitution
            // context, which only a nominal closed type carries.
            let declaringTypeGenerics : ImmutableArray<ConcreteTypeHandle> =
                match declaringTarget with
                | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
                    AllConcreteTypes.lookup handle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: declaring type handle %O{handle} is not registered in ConcreteTypes"
                    )
                    |> fun concreteType -> concreteType.Generics
                | other ->
                    // `stubOutcome` only says `Rebind` for a MethodTable-backed declaring type, and
                    // the arm above has served the definition-level rebind that binds no method
                    // generic arguments, so what is left here is an array declaring type and an open
                    // generic definition that *is* binding some.
                    //
                    // The latter is `typeof(G<>).GetMethod("M").MakeGenericMethod(typeof(int))`, an
                    // ordinary reflection idiom (genmeth.cpp:1256-1270) whose *identity* PawPrint
                    // could already name. What it cannot do is check the constraints, and CoreCLR
                    // checks them against the definition's own formals: for `M<U>(U) where U : T` on
                    // `G<T>`, every closed argument is rejected, while for the contravariant
                    // `where U : IComparer<T>` the closed `IComparer<object>` is *accepted* (both
                    // measured against the real runtime). So this needs a variance-aware
                    // assignability check against a formal, with the formal's own constraints in
                    // play, and `validateConstraintsOn` wants each of those formals as a
                    // `ConcreteTypeHandle`, which is closed by construction. Serving the shape
                    // without the check would hand back a usable handle where real .NET throws.
                    //
                    // A guest reaches it by the plain idiom, now that `RuntimeTypeHandle.GetNumVirtuals`
                    // answers for an open definition and so `typeof(G<>).GetMethod` succeeds;
                    // `sourcesPure/MakeGenericMethodOnOpenDefinition.cs` is parked on it.
                    failwith
                        $"TODO: %s{operation}: rebinding onto %O{other} is not supported; a closed nominal declaring type carries the generic arguments needed as a substitution context, and constraint validation under an open declaring context is unimplemented"

            // CoreCLR validates the method's generic constraints while binding
            // (`FindOrCreateAssociatedMethodDesc` -> `SatisfiesClassConstraints`) and surfaces a
            // violation to the caller of `MakeGenericMethod` as `ArgumentException`: the binder
            // raises `VerificationException`, which `RuntimeMethodInfo.MakeGenericMethod` catches
            // and rewrites via `ValidateGenericArguments`
            // (RuntimeMethodInfo.CoreCLR.cs:446-450). The managed `SanityCheckGenericArguments`
            // that runs *before* the QCall only screens nulls, non-RuntimeType arguments and arity,
            // so without this check PawPrint would hand back a usable handle where real .NET
            // throws. We raise `ArgumentException` directly, which the managed `catch
            // (VerificationException)` does not intercept, so it propagates as the same exception
            // type the caller would have seen.
            //
            // A method's generic parameters live in the same substitution scope as its declaring
            // type's, so a constraint on one of them may mention either (`!0` or `!!0`). Both
            // contexts therefore have to go in: the declaring type's arguments as `typeGenerics`,
            // the instantiation being bound as `methodGenerics`.
            let state, constraintViolation =
                NativeRuntimeTypeHelpers.validateConstraintsOn
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    $"%s{MethodOwner.describe methodInfo.Owner}.%s{methodInfo.Name}"
                    methodInfo.DeclaringAssemblyFullName
                    declaringTypeGenerics
                    (ImmutableArray.CreateRange methodInstantiation)
                    methodInfo.Generics
                    methodInstantiation

            match constraintViolation with
            | Some _message ->
                NativeHandlerResult.raiseException ctx.BaseClassTypes.ArgumentException state
                |> Some
            | None ->

            let state, concretizedMethod, _ =
                ExecutionConcretization.concretizeMethodWithAllGenerics
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    declaringTypeGenerics
                    methodInfo
                    (ImmutableArray.CreateRange methodInstantiation)
                    state

            let handleValue, registry =
                MethodHandleRegistry.getOrAllocateConcreteInternalHandle
                    ctx.BaseClassTypes
                    state.ConcreteTypes
                    concretizedMethod
                    state.MethodHandles

            let state =
                { state with
                    MethodHandles = registry
                }

            let state =
                IlMachineState.pushToEvalStack (CliType.ValueType handleValue) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetUtf8NameInternal",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePointer (ConcreteVoid state.ConcreteTypes)) when generics.IsEmpty ->
            // CoreCLR's RuntimeMethodHandle.GetUtf8NameInternal returns a raw pointer into
            // metadata; the managed wrapper RuntimeMethodHandle.GetUtf8Name(...) wraps the
            // result in MdUtf8String, which calls string.strlen on the pointer to discover
            // the byte length. PawPrint materialises the method's metadata name as a
            // freshly-allocated null-terminated UTF-8 byte[] and returns a byref to it; the
            // managed strlen path then walks the array as expected.
            let operation = "RuntimeMethodHandle.GetUtf8NameInternal"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let namePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes methodInfo.Name state

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer namePtr) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetAttributes",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System.Reflection",
                                                                     "MethodAttributes",
                                                                     retGenerics)) when
            generics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR (runtimehandles.cpp): asserts non-null and returns
            // (INT32)pMethod->GetAttrs(). The managed wrapper exposes this as the
            // MethodAttributes flags backing MethodBase.Attributes / RuntimeMethodInfo's
            // candidate filter.
            let operation = "RuntimeMethodHandle.GetAttributes"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            // `RuntimeMethodHandle.GetAttributes` reports the raw flags to the guest. Only a
            // declared method has any: a synthesised one cannot be reached through a
            // `RuntimeMethodHandle` at all, because minting one for it already fails.
            let attributes =
                match methodInfo.TryMetadata with
                | Some facts -> facts.MethodAttributes
                | None ->
                    failwith
                        $"%s{operation}: %O{methodInfo} is synthesised by the runtime and has no MethodAttributes to report"

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (int32 attributes)))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetSlot",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when generics.IsEmpty ->
            // CoreCLR (runtimehandles.cpp:1352): asserts non-null and returns
            // (INT32)pMethod->GetSlot(), which is a bare read of the MethodDesc's slot number as
            // assigned once during method-table building. PawPrint has no persisted slot number, so
            // the layout is recomputed from the declaring type's chain; see
            // `VirtualSlotLayout.slotTableOfClosed` for the rule and for why MethodImpls are
            // not consulted.
            //
            // The number spans both halves of the method table, so this asks the slot table rather
            // than the vtable alone. `PopulateMethods` (RuntimeType.CoreCLR.cs:683) only ever asks
            // about a method carrying MethodAttributes.Virtual, which on a class or value type
            // always occupies an instance vtable slot -- CoreCLR rejects static+virtual outside
            // interfaces with a TypeLoadException, and `PopulateMethods` routes interfaces down a
            // branch that never reaches here. But `PopulateProperties` (RuntimeType.CoreCLR.cs:1358)
            // calls this on a property's accessor with *no* Virtual guard, testing
            // `slot < numVirtuals` afterwards, so an ordinary non-virtual getter reaches it and
            // CoreCLR answers with a slot in the region past the vtable.
            //
            // One shape still lands outside anything modelled here, and is recorded because it is
            // the answer to "what would CoreCLR have returned": for a value type the MethodTable
            // builder duplicates every virtual, leaving the unboxing stub in the vtable slot and
            // giving the unboxed copy a slot of its own beyond the rest (`AddUnboxedMethod`,
            // methodtablebuilder.cpp:7178). Those duplicates are MethodDesc-level artifacts living
            // in the MethodDescChunks, and they take their numbers *after* everything placed from
            // metadata, so they shift nothing. PawPrint enumerates metadata MethodDefs once and so
            // never sees them; nor can a guest name one, since reflection surfaces the original.
            let operation = "RuntimeMethodHandle.GetSlot"

            let identity =
                resolveMetadataIdentityFromArg operation state instruction.Arguments.[0]

            let methodInfo = methodInfoOfMetadataIdentity operation state identity

            let declaringType = identity.GetDeclaringType ()

            // Both spellings of a metadata declaring type carry a method table, and they carry the
            // *same* one: CoreCLR places virtuals once, on the definition, and every instantiation
            // inherits that layout. Which one the guest named is therefore a question about the
            // handle it holds, not about the answer.
            let state, slotTable =
                match declaringType with
                | RuntimeTypeHandleTarget.Closed handle ->
                    VirtualSlotLayout.slotTableOfClosed ctx.LoggerFactory ctx.BaseClassTypes operation state handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition definition ->
                    VirtualSlotLayout.slotTableOfDefinition
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        operation
                        state
                        definition
                | other ->
                    // `MethodHandleRegistry` admits only `Closed` and `OpenGenericTypeDefinition`
                    // when minting, so any other shape here means a handle was built outside that
                    // chokepoint.
                    failwith
                        $"%s{operation}: declaring type %O{other} cannot declare a metadata-backed method; MethodHandleRegistry refuses to mint such a handle, so this identity did not come from it"

            let slot =
                slotTable
                |> VirtualSlotLayout.slotIndexInTable (identity.GetAssemblyFullName (), methodInfo.IdentityKey)
                |> Option.defaultWith (fun () ->
                    // Every method a type declares in metadata is placed in one half or the other,
                    // so reaching here means the method is not the declaring type's to place: a
                    // synthesised method, which has no MethodDef row for `DeclaredMethodIterator` to
                    // find, or an identity naming a type that does not declare it.
                    // A `RuntimeTypeHandleTarget` renders as its metadata handles, which name no
                    // type; the definition walk can name one, so ask it.
                    let declaringDescription =
                        match declaringType with
                        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition definition ->
                            (VirtualSlotLayout.ownerOfDefinition operation state definition).Description
                        | other -> string other

                    failwith
                        $"%s{operation}: method %s{methodInfo.Name} occupies no slot in the method table of its declaring type %s{declaringDescription}; every metadata-declared method is placed either in the vtable or in the region beyond it, so this is a method the declaring type does not declare (a runtime-synthesised method has no MethodDef row and is never placed)"
                )

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 slot)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetMethodDef",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when generics.IsEmpty ->
            // CoreCLR (runtimehandles.cpp:1577): asserts non-null and returns
            // (INT32)pMethod->GetMemberDef(). See `methodDefToken` above for what that token is and
            // why neither the declaring assembly nor the handle's instantiations come into it.
            //
            // Resolves the handle rather than the `MethodInfo` behind it, unlike its neighbours
            // here: this native is legal on a dynamic method, which has no `MethodInfo` to find, so
            // routing it through `resolveMethodInfoFromHandleArg` for consistency with `GetSlot`
            // and friends would turn a valid call into a failure.
            let operation = "RuntimeMethodHandle.GetMethodDef"

            let methodHandle =
                resolveMethodHandleFromArg operation state instruction.Arguments.[0]

            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (methodDefToken methodHandle)))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "IsGenericMethodDefinition",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when generics.IsEmpty ->
            // CoreCLR (runtimehandles.cpp:1730): FC_RETURN_BOOL(pMethod->IsGenericMethodDefinition()).
            // See `isGenericMethodDefinition` above for the predicate and how it maps onto
            // PawPrint's representation.
            let operation = "RuntimeMethodHandle.IsGenericMethodDefinition"

            let identity =
                resolveMetadataIdentityFromArg operation state instruction.Arguments.[0]

            let methodInfo = methodInfoOfMetadataIdentity operation state identity

            let result =
                isGenericMethodDefinition methodInfo.Generics.Length (identity.GetMethodGenerics ()).Length

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "IsDynamicMethod",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when generics.IsEmpty ->
            // CoreCLR (runtimehandles.cpp:1746): FC_RETURN_BOOL(pMethod->IsNoMetadata()).
            // See `isDynamicMethod` above for the predicate.
            //
            // Deliberately resolves the handle rather than the `MethodInfo` behind it: this is the
            // one native here whose whole job is to say whether that metadata lookup is legitimate,
            // so performing the lookup first would beg the question.
            let operation = "RuntimeMethodHandle.IsDynamicMethod"

            let methodHandle =
                resolveMethodHandleFromArg operation state instruction.Arguments.[0]

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool (isDynamicMethod methodHandle)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetMethodTable",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                                                      "MethodTable",
                                                                                      retGenerics))) when
            generics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR (runtimehandles.cpp:1344): asserts non-null and returns
            // pMethod->GetMethodTable(). See `methodTableOfDeclaringType` above for what that
            // MethodTable is and why the instantiation is preserved as-is.
            //
            // The only managed caller is `RuntimeMethodHandle.GetDeclaringType`
            // (RuntimeHandles.cs:1094), whose body is `RuntimeTypeHandle.GetRuntimeType(pMT)` --
            // i.e. `pMT->AuxiliaryData->ExposedClassObject`, which
            // `MethodTableProjection.tryProjectAuxiliaryDataFieldAddress` serves by pre-allocating
            // the canonical `RuntimeType`. So the `?? GetRuntimeTypeFromHandleSlow(...)` fallback in
            // that accessor never fires, which is what lets this hand back a bare pointer identity.
            let operation = "RuntimeMethodHandle.GetMethodTable"

            // Legal on a dynamic method, and this is the FCall that makes `CreateDelegate` work on
            // one: `Delegate.CreateDelegate` reaches it through `RuntimeMethodHandle.GetDeclaringType`
            // (Delegate.CoreCLR.cs:381-391) before handing the result to `Delegate_BindToMethodInfo`.
            //
            // A dynamic method's answer is the synthetic per-module class CoreCLR allocates its
            // `DynamicMethodDesc` from, and PawPrint models it as such rather than standing a real
            // type in for it — see `RuntimeTypeHandleTarget.DynamicMethodsClass`. Note it is
            // produced straight from the scope assembly recorded at mint time: no assembly needs
            // loading and no type needs concretising, because there is no metadata behind it.
            let target =
                match resolveMethodHandleFromArg operation state instruction.Arguments.[0] with
                | MethodHandle.FromMetadata identity ->
                    match identity.GetDeclaringType () with
                    | RuntimeTypeHandleTarget.Closed handle ->
                        match methodTableOfDeclaringType handle with
                        | Ok target -> target
                        | Error reason -> failwith $"%s{operation}: %s{reason}"
                    // Already the answer: CoreCLR's typical instantiation of `G<>` is a
                    // MethodTable, and it is the one a method of the definition belongs to.
                    | (RuntimeTypeHandleTarget.OpenGenericTypeDefinition _) as target -> target
                    | other ->
                        failwith
                            $"%s{operation}: declaring type %O{other} cannot declare a metadata-backed method; MethodHandleRegistry refuses to mint such a handle, so this identity did not come from it"
                | MethodHandle.FromDynamic dynamicHandle ->
                    let definition =
                        MethodHandleRegistry.resolveDynamicMethod dynamicHandle state.MethodHandles
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: %O{dynamicHandle} is not registered in the method-handle registry"
                        )

                    RuntimeTypeHandleTarget.DynamicMethodsClass (definition.GetScopeAssemblyFullName ())

            let state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr target))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "HasMethodInstantiation",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when generics.IsEmpty ->
            // CoreCLR (runtimehandles.cpp:1722): FC_RETURN_BOOL(pMethod->HasMethodInstantiation()).
            // See `hasMethodInstantiation` above for the predicate, and in particular for why the
            // handle's own instantiation is not what it consults.
            let operation = "RuntimeMethodHandle.HasMethodInstantiation"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let result = hasMethodInstantiation methodInfo.Generics.Length

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "IsConstructor",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", generics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when generics.IsEmpty ->
            // CoreCLR (runtimehandles.cpp:2135): asserts non-null and returns
            // pMethod->IsClassConstructorOrCtor(). See `isConstructorOrClassConstructor` above for
            // the predicate; it reads the same two things CoreCLR does, the method's attributes and
            // its metadata name.
            let operation = "RuntimeMethodHandle.IsConstructor"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let result =
                let facts = MethodInfo.requireMetadata operation methodInfo
                isConstructorOrClassConstructor facts.MethodAttributes methodInfo.Name

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetStubIfNeededInternal",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", handleGenerics)
            CorelibType state.ConcreteTypes ("System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System",
                                                                     "RuntimeMethodHandleInternal",
                                                                     retGenerics)) when
            handleGenerics.IsEmpty && runtimeTypeGenerics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR runtimehandles.cpp:1886-1911. Fast path that returns the same MethodDesc*
            // when no instantiating/unboxing stub is needed. Returning NULL hands off to the slow
            // QCall RuntimeMethodHandle_GetStubIfNeededSlow, which materialises an
            // InstantiatedMethodDesc via FindOrCreateAssociatedMethodDescForReflection.
            //
            // The predicate lives in `fastPathReturnsOriginal` above, so that it and the slow path's
            // `stubOutcome` -- which CoreCLR documents as duplicates of each other -- can be
            // cross-checked against one another by property test.
            let operation = "RuntimeMethodHandle.GetStubIfNeededInternal"

            let methodInfo =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let methodHandleId =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation instruction.Arguments.[0]
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: null RuntimeMethodHandleInternal")

            // Same CoreCLR predicate the `HasMethodInstantiation` FCall returns, so it is spelled
            // once: `GetStubIfNeededInternal`'s condition opens with `pMethod->HasMethodInstantiation()`
            // (runtimehandles.cpp:1901).
            let methodHasInstantiation = hasMethodInstantiation methodInfo.Generics.Length

            let state = IlMachineState.loadArgument ctx.Thread 1 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let declaringType =
                stubDeclaringTypeOfTarget operation ctx.BaseClassTypes state target

            let returnsOriginalHandle =
                fastPathReturnsOriginal methodHasInstantiation declaringType

            let returnValue =
                if returnsOriginalHandle then
                    MethodHandleRegistry.internalHandleFromId ctx.BaseClassTypes state.ConcreteTypes methodHandleId
                else
                    MethodHandleRegistry.zeroInternalHandle ctx.BaseClassTypes state.ConcreteTypes

            let state =
                IlMachineState.pushToEvalStack (CliType.ValueType returnValue) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeMethodHandle",
          "GetLoaderAllocatorInternal",
          [ CorelibType state.ConcreteTypes ("System", "RuntimeMethodHandleInternal", handleGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System.Reflection", "LoaderAllocator", retGenerics)) when
            handleGenerics.IsEmpty && retGenerics.IsEmpty
            ->
            // CoreCLR runtimehandles.cpp:2148 returns
            //   pMethod->GetLoaderAllocator()->GetExposedObject()
            // and `GetExposedObject` (loaderallocator.inl:11) reads
            // `m_hLoaderAllocatorObjectHandle`, which is only populated by
            // `LoaderAllocator::SetupManagedTracking`. That function is only invoked
            // from `Assembly::Create` and `AssemblyNative::CreateAssemblyLoadContext`
            // for *collectible* loader allocators (assembly.cpp:468). Non-collectible
            // assemblies — i.e. everything PawPrint currently loads — leave the handle
            // null, so the FCall returns null and the BCL takes the static-cache path
            // (e.g. `RuntimeType.RuntimeTypeCache.GetGenericMethodInfo` switches to
            // `s_methodInstantiations`). Allocating a fresh `LoaderAllocator` here would
            // route those caches into a per-call object and silently break
            // canonicalization of reflected generic methods.
            //
            // When collectible AssemblyLoadContexts get modelled, this arm should look
            // up the method's LoaderAllocator identity and return the corresponding
            // exposed object.
            let operation = "RuntimeMethodHandle.GetLoaderAllocatorInternal"

            // CoreCLR asserts non-null on the FCall entry; surface the same precondition.
            let _ : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> =
                resolveMethodInfoFromHandleArg operation state instruction.Arguments.[0]

            let state = IlMachineState.pushToEvalStack (CliType.ObjectRef None) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None
