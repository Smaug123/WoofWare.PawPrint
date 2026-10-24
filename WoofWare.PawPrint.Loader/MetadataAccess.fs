namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

/// The visibility of a method or a field, or of a nested type as `ClassLoader::CanAccessClass`
/// translates it into a member's (clsload.cpp).
[<RequireQualifiedAccess>]
type MemberVisibility =
    | Public
    | Assembly
    | FamilyOrAssembly
    | Family
    | FamilyAndAssembly
    | Private
    /// `privatescope`: compiler-controlled, accessible only from its own module.
    | PrivateScope

[<RequireQualifiedAccess>]
module MemberVisibility =

    let ofMethodAttributes (attributes : MethodAttributes) : MemberVisibility =
        match attributes &&& MethodAttributes.MemberAccessMask with
        | MethodAttributes.Public -> MemberVisibility.Public
        | MethodAttributes.Assembly -> MemberVisibility.Assembly
        | MethodAttributes.FamORAssem -> MemberVisibility.FamilyOrAssembly
        | MethodAttributes.Family -> MemberVisibility.Family
        | MethodAttributes.FamANDAssem -> MemberVisibility.FamilyAndAssembly
        | MethodAttributes.Private -> MemberVisibility.Private
        | _ -> MemberVisibility.PrivateScope

    let ofFieldAttributes (attributes : FieldAttributes) : MemberVisibility =
        match attributes &&& FieldAttributes.FieldAccessMask with
        | FieldAttributes.Public -> MemberVisibility.Public
        | FieldAttributes.Assembly -> MemberVisibility.Assembly
        | FieldAttributes.FamORAssem -> MemberVisibility.FamilyOrAssembly
        | FieldAttributes.Family -> MemberVisibility.Family
        | FieldAttributes.FamANDAssem -> MemberVisibility.FamilyAndAssembly
        | FieldAttributes.Private -> MemberVisibility.Private
        | _ -> MemberVisibility.PrivateScope

/// What CoreCLR checks a method may access where an instruction's token names it.
[<RequireQualifiedAccess>]
type AccessTarget =
    /// A type, as the token spells it.
    | Type of TypeDefn
    /// A method or field: the type the token names it a member of, as the token spells it, which
    /// may derive from the type that declares it; the type that declares it; its visibility; and,
    /// for a method, the method instantiation the token spells.
    | Member of
        parent : TypeDefn *
        declaredBy : ResolvedTypeIdentity *
        visibility : MemberVisibility *
        instantiation : TypeDefn list

/// Whether an access check passes, as far as metadata alone decides it.
[<RequireQualifiedAccess>]
type AccessDecision =
    /// The check passes, whatever else is true of the program.
    | Accessible
    /// Metadata alone does not decide it: the access may be refused, or allowed only by a friend
    /// assembly, by the accessing type deriving from another, or by a definition this does not find
    /// among the loaded assemblies.
    | Undecided

/// <summary>
/// CoreCLR's access check (<c>ClassLoader::CanAccess</c>, <c>CanAccessClass</c> and
/// <c>CheckAccessMember</c>, clsload.cpp), decided where the visibility of each type and member the
/// check reaches settles it, and otherwise left undecided.
/// </summary>
/// <remarks>
/// The check walks the type a token names, each type argument it spells, the element type of an
/// array, pointer or byref, and the type that encloses a nested type; for a member, it also walks
/// the member's own visibility and its method instantiation. A type variable is not checked: CoreCLR
/// checks a generic method's tokens in its open form (<c>CEEInfo::GetMethodForSecurity</c>). This
/// decides public types and members; those visible within their own assembly, accessed from it;
/// and any accessed from the type that declares them or one nested in it. It leaves undecided what
/// turns on friend assemblies, on <c>IgnoresAccessChecksTo</c>, or on the accessing type deriving
/// from another.
/// </remarks>
[<RequireQualifiedAccess>]
module MetadataAccess =

    let private andThen (next : unit -> AccessDecision) (decision : AccessDecision) : AccessDecision =
        match decision with
        | AccessDecision.Accessible -> next ()
        | AccessDecision.Undecided -> AccessDecision.Undecided

    let private all (decide : 'a -> AccessDecision) (items : 'a seq) : AccessDecision =
        (AccessDecision.Accessible, items)
        ||> Seq.fold (fun decision item -> decision |> andThen (fun () -> decide item))

    /// A type definition and the assembly that defines it.
    type private Definition = DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>

    let private enclosingOf ((definedIn, definition) : Definition) : Definition option =
        if definition.DeclaringType.IsNil then
            None
        else
            Some (definedIn, definedIn.TypeDefs.[definition.DeclaringType])

    /// The accessing type, then each type enclosing it, innermost first.
    let rec private chainOf (caller : Definition) : Definition list =
        caller :: (enclosingOf caller |> Option.map chainOf |> Option.defaultValue [])

    let private identityOf ((_, definition) : Definition) : ResolvedTypeIdentity = definition.Identity

    /// `CheckAccessMember` after its check of the target type itself: whether a method of `caller`,
    /// in the assembly whose definition name is `callerAssembly`, may access a member of `target`
    /// with this visibility.
    let private memberOf
        (callerAssembly : string)
        (caller : Definition)
        (target : Definition)
        (visibility : MemberVisibility)
        : AccessDecision
        =
        let ownAssembly = (fst target).DefinitionFullName = callerAssembly

        // CoreCLR walks the accessing type and the types enclosing it, granting access at the
        // first that is the target, or, for a family member, derives from it, which this does not
        // model; a private member stops the walk at the first type that is not nested.
        let rec walk (chain : Definition list) : AccessDecision =
            match chain with
            | [] -> AccessDecision.Undecided
            | current :: _ when identityOf current = identityOf target -> AccessDecision.Accessible
            | current :: _ when visibility = MemberVisibility.Private && (snd current).DeclaringType.IsNil ->
                AccessDecision.Undecided
            | _ :: enclosing -> walk enclosing

        match visibility with
        | MemberVisibility.Public -> AccessDecision.Accessible
        | MemberVisibility.PrivateScope -> AccessDecision.Undecided
        | MemberVisibility.Assembly ->
            if ownAssembly then
                AccessDecision.Accessible
            else
                AccessDecision.Undecided
        | MemberVisibility.FamilyOrAssembly ->
            if ownAssembly then
                AccessDecision.Accessible
            else
                walk (chainOf caller)
        | MemberVisibility.FamilyAndAssembly ->
            if ownAssembly then
                walk (chainOf caller)
            else
                AccessDecision.Undecided
        | MemberVisibility.Family
        | MemberVisibility.Private -> walk (chainOf caller)

    /// `CanAccessClass` of a type definition.
    let rec private definitionAccessible
        (callerAssembly : string)
        (caller : Definition)
        (target : Definition)
        : AccessDecision
        =
        let definedIn, definition = target
        let ownAssembly = definedIn.DefinitionFullName = callerAssembly

        // A nested type's visibility is a member's of the type enclosing it.
        let asMember (visibility : MemberVisibility) : AccessDecision =
            match enclosingOf target with
            | None -> AccessDecision.Undecided
            | Some enclosing ->
                definitionAccessible callerAssembly caller enclosing
                |> andThen (fun () -> memberOf callerAssembly caller enclosing visibility)

        match definition.TypeAttributes &&& TypeAttributes.VisibilityMask with
        | TypeAttributes.Public -> AccessDecision.Accessible
        | TypeAttributes.NotPublic when ownAssembly -> AccessDecision.Accessible
        | TypeAttributes.NestedPublic -> asMember MemberVisibility.Public
        | TypeAttributes.NestedPrivate -> asMember MemberVisibility.Private
        | TypeAttributes.NestedFamily -> asMember MemberVisibility.Family
        | TypeAttributes.NestedAssembly when ownAssembly -> asMember MemberVisibility.Public
        | TypeAttributes.NestedFamANDAssem when ownAssembly -> asMember MemberVisibility.Family
        | TypeAttributes.NestedFamORAssem when ownAssembly -> asMember MemberVisibility.Public
        | TypeAttributes.NestedFamORAssem -> asMember MemberVisibility.Family
        | _ -> AccessDecision.Undecided

    /// The definition a nominal spelling in `spellingAssembly` names, if this finds it.
    let private definitionOf
        (assemblies : LoadedAssemblies)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        : Definition option
        =
        match spelling with
        | TypeDefn.FromDefinition (identity, _) ->
            assemblies.TryByDefinitionName identity.AssemblyFullName
            |> Option.map (fun definedIn -> definedIn, definedIn.TypeDefs.[identity.TypeDefinition.Get])
        | TypeDefn.FromReference (reference, _) ->
            match LoadedTypeResolution.resolveTypeRef assemblies spellingAssembly ImmutableArray.Empty reference with
            | TypeResolutionResult.Resolved (definedIn, identity, _) ->
                Some (definedIn, definedIn.TypeDefs.[identity.TypeDefinition.Get])
            | TypeResolutionResult.FirstLoadAssy _
            | TypeResolutionResult.NotFound _
            | TypeResolutionResult.Unmodelled _ -> None
        | _ -> None

    let rec private typeAccessible
        (assemblies : LoadedAssemblies)
        (callerAssembly : DumpedAssembly)
        (caller : Definition)
        (spelling : TypeDefn)
        : AccessDecision
        =
        let recurse = typeAccessible assemblies callerAssembly caller

        match spelling with
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> AccessDecision.Accessible
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> recurse element
        | TypeDefn.Modified modified -> recurse modified.Unmodified
        | TypeDefn.GenericInstantiation (generic, arguments) -> all recurse (Seq.append [ generic ] arguments)
        // CoreCLR checks none of a function pointer's signature as a type argument
        // (`CanAccessClass` skips one), but this does not establish what it checks of one
        // the token names itself.
        | TypeDefn.FunctionPointer _ -> AccessDecision.Undecided
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ ->
            match definitionOf assemblies callerAssembly spelling with
            | None -> AccessDecision.Undecided
            | Some target -> definitionAccessible callerAssembly.DefinitionFullName caller target

    /// The generic definition or nominal type a spelling names.
    let rec private rootOf (spelling : TypeDefn) : TypeDefn =
        match spelling with
        | TypeDefn.GenericInstantiation (generic, _) -> rootOf generic
        | TypeDefn.Modified modified -> rootOf modified.Unmodified
        | other -> other

    /// Whether a method of `caller`, a type of `callerAssembly`, may access `target`, whose
    /// spellings are in `callerAssembly` too.
    let decide
        (assemblies : LoadedAssemblies)
        (callerAssembly : DumpedAssembly)
        (caller : ResolvedTypeIdentity)
        (target : AccessTarget)
        : AccessDecision
        =
        if caller.AssemblyFullName <> callerAssembly.DefinitionFullName then
            failwith
                $"logic error: asked about access from %O{caller} as a type of %s{callerAssembly.DefinitionFullName}, which does not define it"

        let caller : Definition =
            callerAssembly, callerAssembly.TypeDefs.[caller.TypeDefinition.Get]

        let typeAccessible = typeAccessible assemblies callerAssembly caller

        match target with
        | AccessTarget.Type spelling -> typeAccessible spelling
        | AccessTarget.Member (parent, declaredBy, visibility, instantiation) ->
            // CoreCLR checks the member's visibility against the parent's definition
            // (`ClassLoader::CheckAccessMember`), which is the member's declaring type unless the
            // parent inherits the member; this decides an inherited member only where it is public.
            let visible () : AccessDecision =
                match visibility, definitionOf assemblies callerAssembly (rootOf parent) with
                | MemberVisibility.Public, _ -> AccessDecision.Accessible
                | _, Some definition when identityOf definition = declaredBy ->
                    memberOf callerAssembly.DefinitionFullName caller definition visibility
                | _, Some _
                | _, None -> AccessDecision.Undecided

            typeAccessible parent
            |> andThen (fun () -> all typeAccessible instantiation)
            |> andThen visible
