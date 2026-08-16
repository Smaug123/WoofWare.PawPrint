namespace WoofWare.PawPrint

open System.Reflection

/// <summary>
/// Per-level visibility input for <c>AccessCheck.canAccessClass</c>'s walk
/// over a type's enclosing-type chain. We project this out of
/// <c>TypeInfo</c> so that AccessCheck has no dependency on the larger
/// type-info surface (which is generic over a name encoding).
/// </summary>
type AccessLevelInfo =
    {
        /// The type's <c>TypeAttributes</c>; AccessCheck looks at the
        /// <c>VisibilityMask</c> bits only.
        Visibility : TypeAttributes
        /// The type's name. Used solely for diagnostic messages when a
        /// not-yet-ported visibility flag triggers a loud failure.
        Name : string
    }

/// <summary>
/// One side of an access-check decision. CoreCLR's
/// <c>ClassLoader::CanAccess*</c> family threads an <c>AccessCheckContext</c>
/// describing the accessing side; we make both sides symmetric so the same
/// record describes either the accessor or the target.
/// </summary>
type AccessParty =
    {
        /// Type-nesting chain. <c>Head</c>: the immediate type whose
        /// visibility we are deciding about. <c>Tail</c>: the chain of
        /// enclosing types, up to and including the outermost (whose
        /// <c>Visibility</c> is one of <c>Public</c> / <c>NotPublic</c>).
        /// For a top-level type, this list has a single element.
        TypeChain : AccessLevelInfo list
        /// The assembly identity used by <c>FriendAssemblyName.matchesDef</c>
        /// to decide whether the other side's IVT / IgnoresAccessChecksTo
        /// declarations grant cross-assembly visibility.
        Assembly : AssemblyName
        /// The IVT / IgnoresAccessChecksTo declarations parsed from
        /// assembly-level custom attributes on this party's assembly.
        Friends : FriendAssemblies
    }

[<RequireQualifiedAccess>]
module AccessCheck =

    /// Run an IVT / IgnoresAccessChecksTo list against a candidate identity:
    /// the match succeeds if any entry in the list is a friend-ref that
    /// matches the candidate def (per CoreCLR's
    /// <c>BaseAssemblySpec::RefMatchesDef</c>, ported as
    /// <c>FriendAssemblyName.matchesDef</c>).
    let private friendDeclares (friends : FriendAssemblyName array) (candidate : AssemblyName) : bool =
        friends
        |> Array.exists (fun friendRef -> FriendAssemblyName.matchesDef friendRef candidate)

    /// Mirrors CoreCLR's <c>ClassLoader::AssemblyOrFriendAccessAllowed</c>:
    /// <list type="number">
    /// <item>If the two parties are the same assembly, access is granted.</item>
    /// <item>If the accessor's <c>IgnoresAccessChecksTo</c> list names the
    /// target's assembly, access is granted (the accessor has opted out of
    /// the target's visibility checks; this is the standard mechanism the
    /// "PrivateProxy" / "InternalsVisibleTo"-by-attacker tooling relies
    /// on).</item>
    /// <item>If the target's <c>InternalsVisibleTo</c> list names the
    /// accessor's assembly, access is granted (the target has granted
    /// friend status to the accessor).</item>
    /// <item>Otherwise access is denied.</item>
    /// </list>
    let private assemblyOrFriendAccessAllowed
        (sameAssembly : bool)
        (accessor : AccessParty)
        (target : AccessParty)
        : bool
        =
        sameAssembly
        || friendDeclares accessor.Friends.IgnoresAccessChecksTo target.Assembly
        || friendDeclares target.Friends.InternalsVisibleTo accessor.Assembly

    /// Decide visibility of a single type-chain level. <c>Public</c> /
    /// <c>NestedPublic</c> is unconditional; <c>NotPublic</c> /
    /// <c>NestedAssembly</c> routes through
    /// <c>assemblyOrFriendAccessAllowed</c>. The family-style and private
    /// nested flags raise <c>failwith</c>: porting them requires
    /// <c>CanAccessFamily</c>'s subclass-walk and chain-equality checks,
    /// and guessing would silently grant (or deny) access.
    let private levelIsVisible
        (sameAssembly : bool)
        (accessor : AccessParty)
        (target : AccessParty)
        (level : AccessLevelInfo)
        : bool
        =
        let vis = level.Visibility &&& TypeAttributes.VisibilityMask

        match vis with
        | TypeAttributes.Public
        | TypeAttributes.NestedPublic -> true
        | TypeAttributes.NotPublic
        | TypeAttributes.NestedAssembly -> assemblyOrFriendAccessAllowed sameAssembly accessor target
        | TypeAttributes.NestedPrivate ->
            failwithf
                "AccessCheck: NestedPrivate visibility on type '%s' is not implemented in this slice (would require CanAccessFamily-style chain-equality check)"
                level.Name
        | TypeAttributes.NestedFamily
        | TypeAttributes.NestedFamORAssem
        | TypeAttributes.NestedFamANDAssem ->
            failwithf
                "AccessCheck: family-style nested-type visibility (%A) on type '%s' is not implemented in this slice (would require CanAccessFamily subclass walk)"
                vis
                level.Name
        | _ -> failwithf "AccessCheck: unrecognised TypeAttributes visibility 0x%x on type '%s'" (int vis) level.Name

    /// Mirrors <c>ClassLoader::CanAccessClass</c> for the visibility flags
    /// supported here: a target type is visible to the accessor
    /// iff every level in the target's nesting chain (from innermost up to
    /// the outermost top-level type) is visible. Generic-instantiation
    /// argument visibility (<c>CanAccessClass</c>'s recursive walk over
    /// the target's generic arguments) is not modelled: the only caller
    /// asks about a non-generic custom attribute type.
    let canAccessClass (sameAssembly : bool) (accessor : AccessParty) (target : AccessParty) : bool =
        target.TypeChain |> List.forall (levelIsVisible sameAssembly accessor target)

    /// Mirrors the visibility portion of <c>ClassLoader::CanAccess</c> /
    /// <c>CheckAccessMember</c> for a method member: access requires both
    /// <c>canAccessClass</c> on the declaring type and the method's own
    /// visibility bit to admit the accessor.
    /// <c>MethodAttributes.Public</c> is unconditional;
    /// <c>MethodAttributes.Assembly</c> routes through
    /// <c>assemblyOrFriendAccessAllowed</c>. The family-style flags and
    /// <c>Private</c> / <c>PrivateScope</c> raise <c>failwith</c>: they
    /// would require <c>CanAccessFamily</c> and chain-equality logic that
    /// is not ported.
    let canAccessMethod
        (sameAssembly : bool)
        (accessor : AccessParty)
        (target : AccessParty)
        (targetMethodAttrs : MethodAttributes)
        : bool
        =
        if not (canAccessClass sameAssembly accessor target) then
            false
        else
            let memberAccess = targetMethodAttrs &&& MethodAttributes.MemberAccessMask

            match memberAccess with
            | MethodAttributes.Public -> true
            | MethodAttributes.Assembly -> assemblyOrFriendAccessAllowed sameAssembly accessor target
            | MethodAttributes.Private ->
                failwith
                    "AccessCheck: Private member access is not implemented in this slice (would require CanAccessFamily-style chain-equality check)"
            | MethodAttributes.Family
            | MethodAttributes.FamANDAssem
            | MethodAttributes.FamORAssem ->
                failwithf
                    "AccessCheck: family-style member visibility (%A) is not implemented in this slice (would require CanAccessFamily subclass walk)"
                    memberAccess
            | MethodAttributes.PrivateScope ->
                failwith
                    "AccessCheck: PrivateScope (compiler-controlled) member access is not implemented in this slice"
            | _ -> failwithf "AccessCheck: unrecognised MethodAttributes member access flag 0x%x" (int memberAccess)
