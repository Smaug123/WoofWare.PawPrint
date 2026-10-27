namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// Checked normalization of an enum's backing field, preserving its declared signature until
/// CoreCLR's field-type rules have been applied. This does not calculate struct layout facts.
[<RequireQualifiedAccess>]
module internal EnumBackingType =

    [<Literal>]
    let private MaxNesting = 64

    let private unmodelled (diagnostic : string) : TypeBindingFailure =
        TypeBindingFailure.Unmodelled
            {
                Diagnostic = diagnostic
            }

    let private isBackingPrimitive (primitive : PrimitiveType) : bool =
        match primitive with
        | PrimitiveType.Boolean
        | PrimitiveType.Char
        | PrimitiveType.SByte
        | PrimitiveType.Byte
        | PrimitiveType.Int16
        | PrimitiveType.UInt16
        | PrimitiveType.Int32
        | PrimitiveType.UInt32
        | PrimitiveType.Int64
        | PrimitiveType.UInt64
        | PrimitiveType.Single
        | PrimitiveType.Double
        | PrimitiveType.IntPtr
        | PrimitiveType.UIntPtr -> true
        | PrimitiveType.String
        | PrimitiveType.Object
        | PrimitiveType.TypedReference -> false

    let rec private isNominalValueType (signature : TypeDefn) : bool =
        match signature with
        | TypeDefn.FromDefinition (_, SignatureTypeKind.ValueType)
        | TypeDefn.FromReference (_, SignatureTypeKind.ValueType) -> true
        | TypeDefn.GenericInstantiation (generic, _) -> isNominalValueType generic
        | _ -> false

    let rec private parametersInScope (arity : int) (signature : TypeDefn) : bool =
        let inScope = parametersInScope arity

        match signature with
        | TypeDefn.GenericTypeParameter index -> index >= 0 && index < arity
        | TypeDefn.GenericMethodParameter _ -> false
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> inScope element
        | TypeDefn.Modified modifier -> inScope modifier.Unmodified
        | TypeDefn.GenericInstantiation (generic, arguments) -> inScope generic && Seq.forall inScope arguments
        | TypeDefn.FunctionPointer signature ->
            (match signature.ReturnType with
             | MethodReturnType.Void -> true
             | MethodReturnType.Returns ty -> inScope ty)
            && List.forall inScope signature.ParameterTypes
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _
        | TypeDefn.Void -> true

    let rec private hasLegalGenericArguments
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        let legal = hasLegalGenericArguments baseClassTypes state

        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match TypeSystemState.tryGetConcreteTypeInfo state handle with
            | None -> false
            | Some (concrete, _) ->
                ConstraintSatisfaction.malformedArgument baseClassTypes state (List.ofSeq concrete.Generics)
                |> Option.isNone
                && Seq.forall legal concrete.Generics
        | ConcreteTypeHandle.Array (element, _)
        | ConcreteTypeHandle.OneDimArrayZero element
        | ConcreteTypeHandle.Pointer element
        | ConcreteTypeHandle.Byref element -> legal element
        | ConcreteTypeHandle.FunctionPointer signature ->
            (match signature.ReturnType with
             | MethodReturnType.Void -> true
             | MethodReturnType.Returns ty -> legal ty)
            && List.forall legal signature.ParameterTypes

    let rec private resolveEnum
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (stripping : Set<ConcreteTypeHandle>)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * Result<PrimitiveType, TypeBindingFailure>
        =
        match TypeSystemState.tryGetConcreteTypeInfo state handle with
        | None -> state, Error (unmodelled $"the enum handle %O{handle} has no nominal type definition")
        | Some (concrete, typeInfo) ->

        if stripping.Contains handle then
            state,
            Error (unmodelled $"the enum %O{concrete} has a cycle in its backing fields, which CoreCLR refuses to load")
        elif stripping.Count >= MaxNesting then
            state,
            Error (
                unmodelled
                    $"the enum %O{concrete} is nested %d{MaxNesting} backing fields deep; PawPrint does not follow enum backing fields beyond this limit"
            )
        elif
            (typeInfo.TypeAttributes &&& TypeAttributes.LayoutMask)
            <> TypeAttributes.AutoLayout
        then
            state, Error (unmodelled $"the enum %O{concrete} declares layout, which CoreCLR refuses to load")
        else

        let declaring = state._LoadedAssemblies.ByDefinitionName concrete.AssemblyFullName
        // MethodTableBuilder checks the typical definition before any closed instantiation.
        // An enum may inherit its enclosing type's parameters, but may not introduce its own.
        let arityMatches =
            typeInfo.Generics.IsEmpty
            || (typeInfo.IsNested
                && declaring.TypeDefs.[typeInfo.DeclaringType].Generics.Length = typeInfo.Generics.Length)

        if not arityMatches then
            state,
            Error (
                unmodelled
                    $"the enum %O{concrete} introduces generic parameters beyond its enclosing type, which CoreCLR refuses to load"
            )
        else

        // EnumerateFields and InitializeFieldDescs omit literals, including non-static ones.
        let fields =
            typeInfo.Fields
            |> List.filter (fun field -> not field.IsStatic && not (field.Attributes.HasFlag FieldAttributes.Literal))

        match fields with
        | [ field ] ->
            let unsupported
                (state : TypeSystemState)
                (reason : string)
                : TypeSystemState * Result<PrimitiveType, TypeBindingFailure>
                =
                state,
                Error (
                    unmodelled
                        $"PawPrint cannot normalize the enum %O{concrete}: its backing-field signature %O{field.Signature} %s{reason}"
                )

            let refused (state : TypeSystemState) : TypeSystemState * Result<PrimitiveType, TypeBindingFailure> =
                state,
                Error (
                    unmodelled
                        $"the enum %O{concrete} has an illegal backing-field signature %O{field.Signature}, which CoreCLR refuses to load"
                )

            let rec fieldType (signature : TypeDefn) : TypeSystemState * Result<PrimitiveType, TypeBindingFailure> =
                match signature with
                | TypeDefn.Modified modifier -> fieldType modifier.Unmodified
                | TypeDefn.PrimitiveType primitive when isBackingPrimitive primitive -> state, Ok primitive
                | signature when isNominalValueType signature ->
                    match
                        TypeSystemState.concretizeType
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            concrete.AssemblyFullName
                            concrete.Generics
                            ImmutableArray.Empty
                            signature
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok underlying when not (hasLegalGenericArguments baseClassTypes state underlying) ->
                        unsupported state "contains a malformed generic instantiation"
                    | state, Ok underlying ->
                        match
                            TypeSystemState.isEnumValueType
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                underlying
                        with
                        | state, Error failure -> state, Error failure
                        | state, Ok false -> refused state
                        | state, Ok true ->
                            // #FieldDescTypeMorph strips enum-typed instance fields. It does not
                            // turn a VALUETYPE reference to System.Int32 into an I4 field.
                            resolveEnum
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                (stripping.Add handle)
                                underlying
                // A direct VAR field makes the typical enum definition invalid, even when a
                // closed instantiation would substitute a primitive for the parameter.
                | _ -> refused state

            if parametersInScope concrete.Generics.Length field.Signature then
                fieldType field.Signature
            else
                unsupported state "contains a generic parameter outside the declaring type's scope"
        | fields ->
            state,
            Error (
                unmodelled
                    $"the enum %O{concrete} has %d{fields.Length} non-literal instance fields, not one, which CoreCLR refuses to load"
            )

    /// The exact primitive backing type of a concrete enum, stripping enum-typed backing fields;
    /// `None` for a handle whose immediate base is not CoreLib's System.Enum.
    /// Checks backing-field shape, parameter scope, generic argument kinds, enum layout and
    /// generic arity, not unrelated type members or generic parameter constraints.
    /// Out-of-scope parameters and malformed instantiations inside reference-type arguments are
    /// unsupported even where CoreCLR can skip resolving them while normalizing the field.
    /// Binding failures and unsupported shapes retain the state reached before the failure.
    /// A cycle is refused; a chain beyond 64 enums reports a PawPrint limit rather than a CLR refusal.
    let tryResolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * Result<PrimitiveType option, TypeBindingFailure>
        =
        match TypeSystemState.isEnumValueType loggerFactory dotnetRuntimeDirs baseClassTypes state handle with
        | state, Error failure -> state, Error failure
        | state, Ok false -> state, Ok None
        | state, Ok true ->
            let state, result =
                resolveEnum loggerFactory dotnetRuntimeDirs baseClassTypes state Set.empty handle

            state, result |> Result.map Some
