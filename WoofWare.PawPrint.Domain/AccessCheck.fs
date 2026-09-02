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
        /// assembly-level custom attributes on this party's assembly, or the
        /// reason they could not be parsed (<c>DumpedAssembly.Friends</c>).
        /// The <c>Error</c> is reported only by a check that actually
        /// consults this party's declarations, which is when CoreCLR would
        /// first parse them and throw.
        Friends : Result<FriendAssemblies, string>
    }

[<RequireQualifiedAccess>]
module AccessCheck =

    /// Run one of a party's friend lists against a candidate identity: the
    /// match succeeds if any entry in the list is a friend-ref that matches
    /// the candidate def (per CoreCLR's
    /// <c>BaseAssemblySpec::RefMatchesDef</c>, ported as
    /// <c>FriendAssemblyName.matchesDef</c>). This is the point at which
    /// CoreCLR first parses the owner's declarations
    /// (<c>Assembly::GetFriendAssemblyInfo</c>), so an owner whose
    /// declarations are invalid yields <c>Error</c> here and nowhere earlier.
    let private friendDeclares
        (owner : AccessParty)
        (list : FriendAssemblies -> FriendAssemblyName array)
        (candidate : AssemblyName)
        : Result<bool, string>
        =
        match owner.Friends with
        | Error e ->
            Error (
                sprintf
                    "friend-assembly declarations on %s are invalid, and this access check consults them: %s"
                    owner.Assembly.FullName
                    e
            )
        | Ok friends ->
            list friends
            |> Array.exists (fun friendRef -> FriendAssemblyName.matchesDef friendRef candidate)
            |> Ok

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
    /// Each step consults one party's declarations, in that order, and stops
    /// at the first that grants access; so <c>Error</c> names the accessor's
    /// assembly if its declarations are invalid, and the target's only if the
    /// accessor's were valid and did not grant access.
    let private assemblyOrFriendAccessAllowed
        (sameAssembly : bool)
        (accessor : AccessParty)
        (target : AccessParty)
        : Result<bool, string>
        =
        if sameAssembly then
            Ok true
        else
            match friendDeclares accessor (fun f -> f.IgnoresAccessChecksTo) target.Assembly with
            | Error e -> Error e
            | Ok true -> Ok true
            | Ok false -> friendDeclares target (fun f -> f.InternalsVisibleTo) accessor.Assembly

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
        : Result<bool, string>
        =
        let vis = level.Visibility &&& TypeAttributes.VisibilityMask

        match vis with
        | TypeAttributes.Public
        | TypeAttributes.NestedPublic -> Ok true
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
    /// <c>Error</c> means a level's decision needed a party's friend
    /// declarations and they are invalid; CoreCLR throws at that point
    /// rather than answering.
    let canAccessClass (sameAssembly : bool) (accessor : AccessParty) (target : AccessParty) : Result<bool, string> =
        let rec walk (levels : AccessLevelInfo list) : Result<bool, string> =
            match levels with
            | [] -> Ok true
            | level :: outer ->
                match levelIsVisible sameAssembly accessor target level with
                | Ok true -> walk outer
                | Ok false -> Ok false
                | Error e -> Error e

        walk target.TypeChain

    /// Mirrors the visibility portion of <c>ClassLoader::CanAccess</c> /
    /// <c>CheckAccessMember</c> for a method member: access requires both
    /// <c>canAccessClass</c> on the declaring type and the method's own
    /// visibility bit to admit the accessor.
    /// <c>MethodAttributes.Public</c> is unconditional;
    /// <c>MethodAttributes.Assembly</c> routes through
    /// <c>assemblyOrFriendAccessAllowed</c>. The family-style flags and
    /// <c>Private</c> / <c>PrivateScope</c> raise <c>failwith</c>: they
    /// would require <c>CanAccessFamily</c> and chain-equality logic that
    /// is not ported. <c>Error</c> is as for <c>canAccessClass</c>.
    let canAccessMethod
        (sameAssembly : bool)
        (accessor : AccessParty)
        (target : AccessParty)
        (targetMethodAttrs : MethodAttributes)
        : Result<bool, string>
        =
        match canAccessClass sameAssembly accessor target with
        | Error e -> Error e
        | Ok false -> Ok false
        | Ok true ->
            let memberAccess = targetMethodAttrs &&& MethodAttributes.MemberAccessMask

            match memberAccess with
            | MethodAttributes.Public -> Ok true
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
