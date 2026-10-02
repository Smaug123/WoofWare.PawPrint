namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// Why PawPrint cannot vouch that CoreCLR's load of a MemberRef's parent fails only at a type
/// reference that names nothing.
[<RequireQualifiedAccess>]
type UnvouchedLoad =
    /// A type CoreLib does not declare.
    | OutsideCoreLib of identity : ResolvedTypeIdentity * name : string
    /// A type reference that names nothing, nested in a type outside CoreLib.
    | MissNestedOutsideCoreLib of TypeResolutionMiss
    /// A type reference that cannot be followed without loading an assembly not yet loaded.
    | AssemblyNotLoaded of WoofWare.PawPrint.AssemblyReference
    /// An instantiation of a generic definition that constrains one of its parameters.
    | ConstrainedDefinition of identity : ResolvedTypeIdentity * name : string
    /// An instantiation with a different number of arguments from the definition's parameters.
    | WrongArity of identity : ResolvedTypeIdentity * name : string * spelled : int
    /// A generic definition named, inside a signature, without an instantiation.
    | UninstantiatedDefinition of identity : ResolvedTypeIdentity * name : string
    /// A value-type instantiation anywhere but as the parent itself.
    | NestedValueTypeInstantiation of identity : ResolvedTypeIdentity * name : string
    /// A byref-like type, or `System.Void`, as a generic argument or an array element.
    | InvalidArgument of identity : ResolvedTypeIdentity * name : string
    /// A type spelled as a value type that is not one, or as a class that is a value type.
    | KindMismatch of identity : ResolvedTypeIdentity * name : string
    /// A pointer, byref, function pointer, type variable, custom modifier, pinned type, `void`, or
    /// an array of rank above 32.
    | UnsupportedSpelling of TypeDefn
    /// A spelling nested more deeply than PawPrint follows.
    | TooDeep

    override this.ToString () : string =
        match this with
        | UnvouchedLoad.OutsideCoreLib (_, name) -> $"%s{name} is not declared by CoreLib"
        | UnvouchedLoad.MissNestedOutsideCoreLib miss -> $"%O{miss}, outside CoreLib"
        | UnvouchedLoad.AssemblyNotLoaded reference -> $"following a type reference needs %s{reference.FullName}"
        | UnvouchedLoad.ConstrainedDefinition (_, name) -> $"%s{name} constrains a generic parameter"
        | UnvouchedLoad.WrongArity (_, name, spelled) -> $"%s{name} is instantiated at %d{spelled} arguments"
        | UnvouchedLoad.UninstantiatedDefinition (_, name) -> $"%s{name} is named without an instantiation"
        | UnvouchedLoad.NestedValueTypeInstantiation (_, name) ->
            $"a value-type instantiation of %s{name} inside another type"
        | UnvouchedLoad.InvalidArgument (_, name) -> $"%s{name} as a generic argument or array element"
        | UnvouchedLoad.KindMismatch (_, name) -> $"%s{name} is spelled as the wrong kind of type"
        | UnvouchedLoad.UnsupportedSpelling spelling -> $"the spelling %O{spelling}"
        | UnvouchedLoad.TooDeep -> "a spelling nested too deeply"

/// Whether PawPrint vouches for CoreCLR's full load of a MemberRef's parent.
[<RequireQualifiedAccess>]
type ParentLoadVouch =
    /// The load fails at the first type reference that names nothing, if there is one, which
    /// `MemberReferenceParent.resolve` reports as <c>Unresolved</c>; otherwise it succeeds.
    | Vouched
    | Unvouched of UnvouchedLoad

/// <summary>
/// Whether every type CoreCLR's full load of a MemberRef's parent reaches is one PawPrint knows to
/// load, so that the only way the load can fail is a type reference that names nothing.
/// </summary>
/// <remarks>
/// <para>
/// CoreCLR's class loader fails a load for well over a hundred reasons besides a missing type
/// (constraint violations, layout limits, invalid overrides, malformed signatures, which throw
/// <c>BadImageFormatException</c> instead, and many more), and binding a member throws whichever comes
/// first. PawPrint models none of them, so it vouches only for loads in which none can arise.
/// </para>
/// <para>
/// The trust anchor is CoreLib: every type it declares loads, and so does each of its generic
/// definitions that constrains none of its parameters, at arguments drawn from its own non-generic
/// types and from reference types built from them. On top of that, the parent's own spelling must
/// keep to these rules, each of which keeps clear of a class of load failure:
/// <list type="bullet">
/// <item>every nominal type is CoreLib's, spelled as the kind of type it is, or a reference that
/// names nothing, at top level or nested in a CoreLib type;</item>
/// <item>an instantiation names a generic definition with no constraints, at exactly as many
/// arguments as it has parameters, and only the parent itself may be a value type, so that no
/// guest spelling compounds a layout towards CoreCLR's limits on a value type's size and on an
/// array element's;</item>
/// <item>no generic argument or array element is byref-like or <c>System.Void</c>, an array has rank
/// at most 32, and nothing is a pointer, byref, function pointer, type variable, custom modifier or
/// generic definition named without an instantiation.</item>
/// </list>
/// A type reference that names nothing may sit anywhere; every other type the spelling names keeps
/// to these rules, so the first failure CoreCLR meets is one of those references.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module ParentLoadVouching =

    /// Where a type sits in the parent's spelling.
    [<RequireQualifiedAccess>]
    type private Position =
        /// The parent itself, named by a TypeRef or TypeDef token, which may name a generic
        /// definition's typical instantiation.
        | Token
        /// The parent itself, spelled by a TypeSpec.
        | Spelled
        /// A generic argument.
        | Argument
        /// An array's element type.
        | Element

    /// What a nominal spelling names.
    [<RequireQualifiedAccess>]
    type private Named =
        | Found of ResolvedTypeIdentity * TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// A type reference that names nothing, where the miss is the failure CoreCLR throws.
        | Absent

    /// CoreCLR's limit on an array's rank, `MAX_RANK`.
    [<Literal>]
    let private MaxRank : int = 32

    /// Deeper than any signature a compiler writes, and shallow enough that nothing here exhausts
    /// the stack.
    [<Literal>]
    let private MaxDepth : int = 64

    /// <summary>
    /// Whether PawPrint vouches for CoreCLR's full load of the parent of
    /// <paramref name="referencingAssembly" />'s MemberRef <paramref name="reference" />. Loads
    /// nothing: call it after <c>MemberReferenceParent.resolve</c> or
    /// <c>FieldReferenceResolution.resolve</c>, which load what CoreCLR's own load of the parent
    /// reaches up to its first failure, so that every type before that failure can be read.
    /// </summary>
    let vouch
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (referencingAssembly : DumpedAssembly)
        (reference : MemberReferenceHandle)
        : ParentLoadVouch
        =
        let corelib = baseClassTypes.Corelib.DefinitionFullName

        let named (spelling : TypeDefn) : Result<Named, UnvouchedLoad> =
            match spelling with
            | TypeDefn.PrimitiveType primitive ->
                let definition = BaseClassTypes.ofPrimitive baseClassTypes primitive
                Ok (Named.Found (definition.Identity, definition))
            | TypeDefn.FromDefinition (identity, _) ->
                let definition =
                    assemblies.ByDefinitionName(identity.AssemblyFullName).TypeDefs.[identity.TypeDefinition.Get]

                Ok (Named.Found (identity, definition))
            | TypeDefn.FromReference (typeRef, _) ->
                match
                    LoadedTypeResolution.resolveTypeRef assemblies referencingAssembly ImmutableArray.Empty typeRef
                with
                | TypeResolutionResult.Resolved (definedIn, identity, resolved) ->
                    Ok (Named.Found (identity, definedIn.TypeDefs.[resolved.TypeDefHandle]))
                | TypeResolutionResult.NotFound (TypeResolutionMiss.TopLevelTypeAbsent _) -> Ok Named.Absent
                | TypeResolutionResult.NotFound (TypeResolutionMiss.NestedTypeAbsent (searchedIn, _, _)) when
                    searchedIn = corelib
                    ->
                    Ok Named.Absent
                | TypeResolutionResult.NotFound miss -> Error (UnvouchedLoad.MissNestedOutsideCoreLib miss)
                | TypeResolutionResult.FirstLoadAssy assembly -> Error (UnvouchedLoad.AssemblyNotLoaded assembly)
            | other -> failwith $"BUG: %O{other} is not a nominal spelling"

        let spelledKind (spelling : TypeDefn) : SignatureTypeKind =
            match spelling with
            | TypeDefn.FromDefinition (_, kind)
            | TypeDefn.FromReference (_, kind) -> kind
            | _ -> SignatureTypeKind.Unknown

        // The checks on one nominal type that hold wherever it sits; `instantiated` says whether the
        // spelling supplies its arguments.
        let nameOf (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
            if System.String.IsNullOrEmpty definition.Namespace then
                definition.Name
            else
                $"%s{definition.Namespace}.%s{definition.Name}"

        let checkNominal
            (position : Position)
            (kind : SignatureTypeKind)
            (instantiated : bool)
            (identity : ResolvedTypeIdentity, definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : Result<unit, UnvouchedLoad>
            =
            let name = nameOf definition

            // CoreLib first: what the type is can only be read from a base chain that loads.
            if identity.AssemblyFullName <> corelib then
                Error (UnvouchedLoad.OutsideCoreLib (identity, name))
            else

            let isValueType = LoadedTypeInfo.isValueType baseClassTypes assemblies definition
            let isVoid = definition.Namespace = "System" && definition.Name = "Void"

            if
                (kind = SignatureTypeKind.ValueType && not isValueType)
                || (kind = SignatureTypeKind.Class && isValueType)
            then
                Error (UnvouchedLoad.KindMismatch (identity, name))
            elif
                (position = Position.Argument || position = Position.Element)
                && (isVoid || LoadedTypeInfo.isByRefLike baseClassTypes assemblies definition)
            then
                Error (UnvouchedLoad.InvalidArgument (identity, name))
            elif
                not instantiated
                && not definition.Generics.IsEmpty
                && position <> Position.Token
            then
                Error (UnvouchedLoad.UninstantiatedDefinition (identity, name))
            else
                Ok ()

        let rec check (depth : int) (position : Position) (spelling : TypeDefn) : Result<unit, UnvouchedLoad> =
            if depth > MaxDepth then
                Error UnvouchedLoad.TooDeep
            else

            match spelling with
            | TypeDefn.PrimitiveType _
            | TypeDefn.FromDefinition _
            | TypeDefn.FromReference _ ->
                match named spelling with
                | Error reason -> Error reason
                | Ok Named.Absent -> Ok ()
                | Ok (Named.Found (identity, definition)) ->
                    checkNominal position (spelledKind spelling) false (identity, definition)
            | TypeDefn.GenericInstantiation (generic, arguments) ->
                let definitionChecked =
                    match generic with
                    | TypeDefn.FromDefinition _
                    | TypeDefn.FromReference _ ->
                        match named generic with
                        | Error reason -> Error reason
                        | Ok Named.Absent -> Ok ()
                        | Ok (Named.Found (identity, definition)) ->
                            let constrains ((_, metadata) : GenericParamFromMetadata) : bool =
                                metadata.Constraint.IsSome
                                || metadata.RequiresParameterlessConstructor
                                || not metadata.Constraints.IsEmpty

                            match checkNominal position (spelledKind generic) true (identity, definition) with
                            | Error reason -> Error reason
                            | Ok () ->
                                if definition.Generics.Length <> arguments.Length then
                                    Error (UnvouchedLoad.WrongArity (identity, nameOf definition, arguments.Length))
                                elif definition.Generics |> Seq.exists constrains then
                                    Error (UnvouchedLoad.ConstrainedDefinition (identity, nameOf definition))
                                elif
                                    position <> Position.Spelled
                                    && LoadedTypeInfo.isValueType baseClassTypes assemblies definition
                                then
                                    Error (UnvouchedLoad.NestedValueTypeInstantiation (identity, nameOf definition))
                                else
                                    Ok ()
                    | other -> Error (UnvouchedLoad.UnsupportedSpelling other)

                match definitionChecked with
                | Error reason -> Error reason
                | Ok () ->
                    arguments
                    |> Seq.map (check (depth + 1) Position.Argument)
                    |> Seq.tryPick (fun result ->
                        match result with
                        | Error reason -> Some reason
                        | Ok () -> None
                    )
                    |> function
                        | Some reason -> Error reason
                        | None -> Ok ()
            | TypeDefn.OneDimensionalArrayLowerBoundZero element -> check (depth + 1) Position.Element element
            | TypeDefn.Array (element, rank) ->
                if rank < 1 || rank > MaxRank then
                    Error (UnvouchedLoad.UnsupportedSpelling spelling)
                else
                    check (depth + 1) Position.Element element
            | TypeDefn.Modified _
            | TypeDefn.Pointer _
            | TypeDefn.Byref _
            | TypeDefn.Pinned _
            | TypeDefn.FunctionPointer _
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _
            | TypeDefn.Void -> Error (UnvouchedLoad.UnsupportedSpelling spelling)

        let result =
            match referencingAssembly.Members.[reference].Parent with
            | MetadataToken.TypeDefinition handle ->
                let identity = referencingAssembly.TypeDefs.[handle].Identity
                check 0 Position.Token (TypeDefn.FromDefinition (identity, SignatureTypeKind.Unknown))
            | MetadataToken.TypeReference handle ->
                check
                    0
                    Position.Token
                    (TypeDefn.FromReference (referencingAssembly.TypeRefs.[handle], SignatureTypeKind.Unknown))
            | MetadataToken.TypeSpecification handle ->
                check 0 Position.Spelled referencingAssembly.TypeSpecs.[handle].Signature
            | other ->
                failwith
                    $"BUG: MemberRef %s{referencingAssembly.Members.[reference].PrettyName} in %s{referencingAssembly.DefinitionFullName} has parent %O{other}, which names no type to load"

        match result with
        | Ok () -> ParentLoadVouch.Vouched
        | Error reason -> ParentLoadVouch.Unvouched reason
