namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// The rule of a type parameter's constraints that an argument fails, as CoreCLR's
/// `TypeVarTypeDesc::SatisfiesConstraints` (typedesc.cpp) checks them.
[<RequireQualifiedAccess>]
type ConstraintRule =
    /// `struct`: the argument is not a value type, or is an instantiation of `System.Nullable`.
    | NonNullableValueType
    /// `class`: the argument is a value type.
    | ReferenceType
    /// `new()`: the argument is abstract, or has no public instance constructor taking nothing
    /// (a value type always has one).
    | ParameterlessConstructor
    /// The argument is byref-like, and the parameter does not carry `allows ref struct`.
    | ByRefLike
    /// The argument cannot be cast to this general constraint, instantiated as the declarer is.
    | Assignable of required : ConcreteTypeHandle

/// An argument of an instantiation that its type parameter's constraints reject.
type ConstraintViolation =
    {
        /// The index of the type parameter among its owner's.
        Parameter : int
        Argument : ConcreteTypeHandle
        /// The first rule it fails, in the order CoreCLR checks them.
        Rule : ConstraintRule
    }

/// Whether an instantiation's arguments satisfy its type parameters' constraints.
[<RequireQualifiedAccess>]
type ConstraintVerdict =
    | Satisfied
    /// The first argument, in order, that its parameter's constraints reject.
    | Violated of ConstraintViolation
    /// Up to this parameter every argument satisfies its constraints, and this one's argument, an
    /// abstract class or an interface, can be cast to a constraint that is an interface with static
    /// virtual methods. CoreCLR then also requires each of those methods to resolve on the argument
    /// (typedesc.cpp:1686), which this does not decide; should that pass, later arguments are checked
    /// in turn.
    | Undecided of parameter : int * required : ConcreteTypeHandle

/// An argument CoreCLR does not admit in any instantiation (`Generics::CheckInstantiation`,
/// generics.cpp).
[<RequireQualifiedAccess>]
type MalformedArgument =
    | Byref
    | TypedReference
    | Void
    | Pointer
    | FunctionPointer

/// Whether a closed instantiation of a generic type or method is one CoreCLR admits: well formed
/// (`malformedArgument`), and satisfying its type parameters' constraints (`satisfiesClosed`).
///
/// CoreCLR checks a type's constraints as it completes the instantiation's load to `CLASS_LOADED`
/// (`SatisfiesClassConstraints`, from `MethodTable::DoFullyLoad`), raising `TypeLoadException`; and a
/// method's as it first creates the instantiated method (`InstantiatedMethodDesc::NewInstantiatedMethodDesc`),
/// raising `VerificationException`. It exempts an instantiation that code shared between
/// instantiations sees, one with an argument in its canonical form; whether an instantiation is such
/// is the caller's to decide.
///
/// Whatever loads an assembly or registers a concrete type on the way returns the state it leaves
/// behind; `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module ConstraintSatisfaction =

    /// The first argument, by index, that CoreCLR admits in no instantiation, and why: a byref,
    /// `TypedReference`, `void`, a pointer or a function pointer. CoreCLR stops at the first type
    /// variable among the arguments and checks none after it; a closed argument is never one.
    let malformedArgument
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (arguments : ConcreteTypeHandle list)
        : (int * MalformedArgument) option
        =
        arguments
        |> List.indexed
        |> List.tryPick (fun (index, argument) ->
            match argument with
            | ConcreteTypeHandle.Byref _ -> Some (index, MalformedArgument.Byref)
            | ConcreteTypeHandle.Pointer _ -> Some (index, MalformedArgument.Pointer)
            | ConcreteTypeHandle.FunctionPointer _ -> Some (index, MalformedArgument.FunctionPointer)
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> None
            | ConcreteTypeHandle.Concrete _ ->
                match TypeSystemState.tryGetConcreteTypeInfo state argument with
                | Some (concrete, _) when concrete.Identity = baseClassTypes.TypedReference.Identity ->
                    Some (index, MalformedArgument.TypedReference)
                | Some (concrete, _) when concrete.Identity = baseClassTypes.Void.Identity ->
                    Some (index, MalformedArgument.Void)
                | _ -> None
        )

    /// Whether a nominal handle names a value type, and whether an instantiation of `Nullable`.
    let private valueTypeShape
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (argument : ConcreteTypeHandle)
        : bool * bool
        =
        match TypeSystemState.tryGetConcreteTypeInfo state argument with
        | None -> false, false
        | Some (concrete, typeInfo) ->
            LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies typeInfo,
            concrete.Identity = baseClassTypes.Nullable.Identity

    /// `MethodTable::HasExplicitOrImplicitPublicDefaultConstructor`, and not abstract: a value type,
    /// or a class that is not abstract and declares a public instance constructor whose signature is
    /// exactly `instance void ()` (`MethodTableBuilder` finds the default constructor so). An array is
    /// neither.
    let private satisfiesParameterlessConstructor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (argument : ConcreteTypeHandle)
        : bool
        =
        match TypeSystemState.tryGetConcreteTypeInfo state argument with
        | None -> false
        | Some (_, typeInfo) ->
            if LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                true
            elif typeInfo.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Abstract then
                false
            else
                typeInfo.Methods
                |> List.exists (fun method ->
                    method.Name = ".ctor"
                    && not method.IsStatic
                    && method.IsPublic
                    && method.Signature.ParameterTypes.IsEmpty
                    && method.Signature.GenericParameterCount = 0
                    && method.Signature.ReturnType = MethodReturnType.Void
                    && method.Signature.Header.Get.CallingConvention = SignatureCallingConvention.Default
                )

    /// Whether a nominal handle names a byref-like type; no structural handle is one.
    let private isByRefLike
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (argument : ConcreteTypeHandle)
        : bool
        =
        match TypeSystemState.tryGetConcreteTypeInfo state argument with
        | None -> false
        | Some (_, typeInfo) -> LoadedTypeInfo.isByRefLike baseClassTypes state._LoadedAssemblies typeInfo

    /// `MethodTable::HasVirtualStaticMethods` for an interface: it declares a static virtual method,
    /// or one of the interfaces it implements, transitively, does (`MethodTableBuilder` sets the flag
    /// from each interface it expands into the interface map).
    let private hasVirtualStaticMethods
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * Result<bool, TypeBindingFailure>
        =
        let rec walk
            (state : TypeSystemState)
            (visited : Set<ConcreteTypeHandle>)
            (pending : ConcreteTypeHandle list)
            : TypeSystemState * Result<bool, TypeBindingFailure>
            =
            match pending with
            | [] -> state, Ok false
            | handle :: rest when visited.Contains handle -> walk state visited rest
            | handle :: rest ->

            match TypeSystemState.tryGetConcreteTypeInfo state handle with
            | None -> walk state (visited.Add handle) rest
            | Some (concrete, typeInfo) ->
                if
                    typeInfo.Methods
                    |> List.exists (fun method -> method.IsStatic && method.IsVirtual)
                then
                    state, Ok true
                else

                let assembly =
                    state._LoadedAssemblies.ByDefinitionName concrete.Identity.AssemblyFullName

                let state, interfaces =
                    (state, [], typeInfo.ImplementedInterfaces)
                    |||> StateResult.fold (fun state found implementation ->
                        match
                            TypeSystemState.resolveTypeMetadataToken
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                assembly
                                implementation.InterfaceHandle
                        with
                        | state, Error failure -> state, Error failure
                        | state, Ok (spelling, spelledIn) ->
                            match
                                TypeSystemState.concretizeType
                                    loggerFactory
                                    dotnetRuntimeDirs
                                    baseClassTypes
                                    state
                                    spelledIn.DefinitionFullName
                                    concrete.Generics
                                    ImmutableArray.Empty
                                    spelling
                            with
                            | state, Error failure -> state, Error failure
                            | state, Ok implemented -> state, Ok (implemented :: found)
                    )

                match interfaces with
                | Error failure -> state, Error failure
                | Ok interfaces -> walk state (visited.Add handle) (List.rev interfaces @ rest)

        walk state Set.empty [ handle ]

    /// <summary>
    /// Whether <paramref name="arguments" />, closed types, satisfy the constraints of
    /// <paramref name="parameters" />, the type parameters of a generic type or method whose
    /// metadata is in <paramref name="declaringAssembly" />, as CoreCLR's
    /// <c>TypeVarTypeDesc::SatisfiesConstraints</c> checks a concrete argument. The parameters'
    /// general constraints are instantiated as the declarer is: by
    /// <paramref name="typeGenerics" />, the declaring type's arguments (a type's own), and
    /// <paramref name="methodGenerics" />, a method's own (none for a type).
    /// </summary>
    /// <remarks>
    /// For each argument in turn: <c>struct</c>, which rejects <c>Nullable</c>; <c>class</c>;
    /// <c>new()</c>; a byref-like argument without <c>allows ref struct</c>; then each general
    /// constraint in metadata order, which the argument must be castable to
    /// (<c>TypeAssignability</c>, with variance and the array rules, and <c>Nullable</c> not
    /// unwrapped), except <c>System.Object</c>, which every argument satisfies. A constraint that does
    /// not bind is the failure.
    /// </remarks>
    let satisfiesClosed
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (declaringAssembly : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (parameters : GenericParamFromMetadata ImmutableArray)
        (arguments : ConcreteTypeHandle list)
        : TypeSystemState * Result<ConstraintVerdict, TypeBindingFailure>
        =
        if parameters.Length <> arguments.Length then
            failwith
                $"BUG: ConstraintSatisfaction.satisfiesClosed was given %d{arguments.Length} arguments for %d{parameters.Length} type parameters in %s{declaringAssembly}"

        let special (index : int) (metadata : GenericParamMetadata) (argument : ConcreteTypeHandle) =
            let isValueType, isNullable = valueTypeShape baseClassTypes state argument

            let rule =
                if metadata.RequiresNonNullableValueType && (not isValueType || isNullable) then
                    Some ConstraintRule.NonNullableValueType
                elif metadata.RequiresReferenceType && isValueType then
                    Some ConstraintRule.ReferenceType
                elif
                    metadata.RequiresParameterlessConstructor
                    && not (satisfiesParameterlessConstructor baseClassTypes state argument)
                then
                    Some ConstraintRule.ParameterlessConstructor
                elif not metadata.AllowsByRefLike && isByRefLike baseClassTypes state argument then
                    Some ConstraintRule.ByRefLike
                else
                    None

            rule
            |> Option.map (fun rule ->
                ConstraintVerdict.Violated
                    {
                        Parameter = index
                        Argument = argument
                        Rule = rule
                    }
            )

        // Whether the argument is an abstract class or an interface, which the rule for an interface
        // constraint with static virtual methods asks.
        let isAbstract (argument : ConcreteTypeHandle) : bool =
            match TypeSystemState.tryGetConcreteTypeInfo state argument with
            | None -> false
            | Some (_, typeInfo) -> typeInfo.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Abstract

        let general
            (state : TypeSystemState)
            (index : int)
            (metadata : GenericParamMetadata)
            (argument : ConcreteTypeHandle)
            : TypeSystemState * Result<ConstraintVerdict option, TypeBindingFailure>
            =
            (state, None, metadata.Constraints)
            |||> StateResult.fold (fun state found spelling ->
                match found with
                | Some _ -> state, Ok found
                | None ->

                // `SigParser` reads past a requireds custom modifiers, which `unmanaged` spells.
                match
                    TypeSystemState.concretizeType
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        declaringAssembly
                        typeGenerics
                        methodGenerics
                        (TypeDefn.stripCustomModifiers spelling)
                with
                | state, Error failure -> state, Error failure
                | state, Ok required ->

                match required with
                | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, Ok None
                | _ ->

                match
                    TypeAssignability.isConcreteTypeAssignableTo
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        argument
                        required
                with
                | state, Error failure -> state, Error failure
                | state, Ok false ->
                    state,
                    Ok (
                        Some (
                            ConstraintVerdict.Violated
                                {
                                    Parameter = index
                                    Argument = argument
                                    Rule = ConstraintRule.Assignable required
                                }
                        )
                    )
                | state, Ok true ->
                    let isInterface =
                        match TypeSystemState.tryGetConcreteTypeInfo state required with
                        | Some (_, typeInfo) -> typeInfo.IsInterface
                        | None -> false

                    if not (isAbstract argument && isInterface) then
                        state, Ok None
                    else

                    match hasVirtualStaticMethods loggerFactory dotnetRuntimeDirs baseClassTypes state required with
                    | state, Error failure -> state, Error failure
                    | state, Ok false -> state, Ok None
                    | state, Ok true -> state, Ok (Some (ConstraintVerdict.Undecided (index, required)))
            )

        let state, verdict =
            (state, None, Seq.zip parameters arguments |> Seq.indexed)
            |||> StateResult.fold (fun state found (index, ((_, metadata), argument)) ->
                match found with
                | Some _ -> state, Ok found
                | None ->

                match special index metadata argument with
                | Some violated -> state, Ok (Some violated)
                | None -> general state index metadata argument
            )

        state, verdict |> Result.map (Option.defaultValue ConstraintVerdict.Satisfied)
