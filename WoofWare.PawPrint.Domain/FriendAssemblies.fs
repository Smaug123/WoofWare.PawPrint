namespace WoofWare.PawPrint

// We read AssemblyName.ProcessorArchitecture to recover the presence of a
// `ProcessorArchitecture=` segment in a display name. The property is marked
// obsolete on net8+ because the runtime no longer consults it during binding,
// but it remains the supported route for *reading* the parser's view of the
// segment, and our use here is exactly that: detect whether the friend name
// carried the segment, so checkFriendRestrictions can reject it.
#nowarn "44"

open System
open System.Reflection

/// <summary>
/// Public-key state of a friend-assembly display name. Mirrors CoreCLR's
/// <c>BaseAssemblySpec</c> distinction between non-strong-named (no key),
/// full public key (<c>PublicKey=</c>), and public key token
/// (<c>PublicKeyToken=</c>). Per ECMA-335 II.6.3 the token is the low eight
/// bytes of <c>SHA-1(publicKey)</c>, reversed.
/// </summary>
[<RequireQualifiedAccess>]
type FriendPublicKey =
    /// The display name had no <c>PublicKey</c> or <c>PublicKeyToken</c> segment,
    /// or had <c>PublicKey=null</c>.
    | NotStrongNamed
    /// The display name had <c>PublicKey=&lt;hex&gt;</c>.
    | FullPublicKey of byte[]
    /// The display name had <c>PublicKeyToken=&lt;hex&gt;</c> (eight bytes).
    /// Valid for the general grammar but rejected by
    /// <c>checkFriendRestrictions</c>: CoreCLR requires IVT to carry the
    /// full public key, not just the token.
    | PublicKeyToken of byte[]

/// <summary>
/// Parsed form of the single string argument to
/// <c>InternalsVisibleToAttribute</c> or <c>IgnoresAccessChecksToAttribute</c>.
/// Mirrors CoreCLR's <c>BaseAssemblySpec</c> for the fields that matter to
/// friend-assembly matching.
/// </summary>
type FriendAssemblyName =
    {
        /// The simple assembly name (the leading token of the display string).
        Name : string
        /// Strong-naming state.
        PublicKey : FriendPublicKey
        /// Value of the <c>Version=</c> segment, if present. <c>None</c> means
        /// unspecified, in which case the def's version is unconstrained.
        Version : Version option
        /// Value of the <c>Culture=</c> segment, if present. <c>None</c> means
        /// no segment was supplied; <c>Some ""</c> is the wire form of
        /// <c>Culture=neutral</c> (the BCL parser normalizes "neutral" to the
        /// empty string, matching CoreCLR's internal locale representation);
        /// <c>Some "en-US"</c> and similar carry the specific culture name.
        /// CoreCLR compares cultures with <c>strcmp</c>, i.e. case-sensitively.
        Culture : string option
        /// Set when the display name contained a <c>ProcessorArchitecture=</c>
        /// segment. CoreCLR's <c>CheckFriendAssemblyName</c> rejects this; see
        /// <c>checkFriendRestrictions</c>.
        HasProcessorArchitecture : bool
        /// Set when the display name contained a <c>Retargetable=Yes</c>
        /// segment. CoreCLR's <c>CompareRefToDef</c> requires the retargetable
        /// flag to match strictly between ref and def, so a friend ref with
        /// this flag set only matches a def that also carries the flag (and
        /// vice versa). <c>CheckFriendAssemblyName</c> tolerates this flag on
        /// IVT, so we do not reject it at parse time.
        HasRetargetable : bool
        /// Value of the <c>ContentType=</c> segment, defaulting to
        /// <c>Default</c> when no segment is present. CoreCLR's
        /// <c>CompareRefToDef</c> treats content type as optional in the ref:
        /// when the ref's content type is non-default, the def's must equal
        /// it; otherwise content type is ignored. <c>CheckFriendAssemblyName</c>
        /// tolerates a content type on IVT.
        ContentType : AssemblyContentType
    }

[<RequireQualifiedAccess>]
module FriendAssemblyName =
    /// Map an <c>AssemblyName</c> (populated by the BCL's display-name parser)
    /// to a <c>FriendPublicKey</c>. The display-name grammar accepts:
    /// <list type="bullet">
    /// <item><c>PublicKey=&lt;hex&gt;</c>: the <c>PublicKey</c> flag bit is set
    /// and <c>GetPublicKey()</c> returns the bytes. We do not validate the
    /// blob's structural shape: CoreCLR's
    /// <c>AssemblySpec::InitNoThrow</c> and <c>CheckFriendAssemblyName</c>
    /// both accept arbitrary bytes; an invalid blob simply won't match any
    /// real assembly's bytes at <c>CompareRefToDef</c> time.</item>
    /// <item><c>PublicKey=null</c>: the flag bit is set but the byte array is
    /// empty. Treated as not strong-named.</item>
    /// <item><c>PublicKeyToken=&lt;hex&gt;</c>: the flag bit is not set; the
    /// BCL has already validated hex and 8-byte length.</item>
    /// <item><c>PublicKeyToken=null</c>: the flag bit is not set and
    /// <c>GetPublicKeyToken()</c> returns an empty array. Treated as not
    /// strong-named.</item>
    /// <item>No segment: <c>GetPublicKeyToken()</c> returns null. Not
    /// strong-named.</item>
    /// </list>
    let private friendPublicKeyOfAssemblyName (an : AssemblyName) : FriendPublicKey =
        let flagsHasPK =
            (an.Flags &&& AssemblyNameFlags.PublicKey) <> AssemblyNameFlags.None

        if flagsHasPK then
            let pk = an.GetPublicKey ()

            if isNull pk || pk.Length = 0 then
                FriendPublicKey.NotStrongNamed
            else
                FriendPublicKey.FullPublicKey pk
        else
            let pkt = an.GetPublicKeyToken ()

            if isNull pkt || pkt.Length = 0 then
                FriendPublicKey.NotStrongNamed
            else
                FriendPublicKey.PublicKeyToken pkt

    /// Parse a friend-assembly display name. Delegates to the host BCL's
    /// <c>AssemblyName(string)</c> constructor, which enforces the same
    /// display-name grammar that CoreCLR's <c>AssemblySpec::InitNoThrow</c>
    /// applies (escape conventions, balanced <c>"</c>/<c>'</c> quoting,
    /// non-empty keys/values, length-checked tokens, single key+token
    /// segment). Per-friend restrictions (no <c>Version</c>, no
    /// <c>Culture</c>, no <c>ProcessorArchitecture</c>, no bare
    /// <c>PublicKeyToken</c>) are enforced separately by
    /// <c>checkFriendRestrictions</c>.
    let parse (displayName : string) : Result<FriendAssemblyName, string> =
        if isNull displayName then
            Error "FriendAssemblyName: null display name"
        else
            let parsed =
                try
                    Ok (AssemblyName displayName)
                with e ->
                    Error (sprintf "FriendAssemblyName: %s" e.Message)

            match parsed with
            | Error e -> Error e
            | Ok an ->
                if isNull an.Name || an.Name.Length = 0 then
                    Error "FriendAssemblyName: missing leading name"
                else
                    let publicKey = friendPublicKeyOfAssemblyName an

                    // Culture distinction: null CultureName means no Culture
                    // segment was present; "" means Culture=neutral.
                    let culture = if isNull an.CultureName then None else Some an.CultureName

                    let version = if isNull an.Version then None else Some an.Version

                    let hasProcessorArchitecture =
                        an.ProcessorArchitecture <> ProcessorArchitecture.None

                    let hasRetargetable =
                        (an.Flags &&& AssemblyNameFlags.Retargetable) <> AssemblyNameFlags.None

                    Ok
                        {
                            Name = an.Name
                            PublicKey = publicKey
                            Version = version
                            Culture = culture
                            HasProcessorArchitecture = hasProcessorArchitecture
                            HasRetargetable = hasRetargetable
                            ContentType = an.ContentType
                        }

    /// Apply CoreCLR's <c>AssemblySpec::CheckFriendAssemblyName</c> restrictions:
    /// <c>Version</c>, <c>Culture</c>, processor architecture, and bare
    /// <c>PublicKeyToken</c> (without full key) are not permitted on a friend
    /// assembly name. Friend names that fail this check would cause CoreCLR
    /// to return <c>META_E_CA_BAD_FRIENDS_ARGS</c>
    /// (or <c>META_E_CA_FRIENDS_SN_REQUIRED</c> for the bare-token case).
    /// Note that <c>Retargetable</c> and <c>ContentType</c> are NOT rejected
    /// here: CoreCLR tolerates them on IVT and lets
    /// <c>CompareRefToDef</c> handle the matching.
    let checkFriendRestrictions (name : FriendAssemblyName) : Result<unit, string> =
        if Option.isSome name.Version then
            Error "FriendAssemblyName: Version is not permitted on a friend assembly name"
        elif Option.isSome name.Culture then
            Error "FriendAssemblyName: Culture is not permitted on a friend assembly name"
        elif name.HasProcessorArchitecture then
            Error "FriendAssemblyName: ProcessorArchitecture is not permitted on a friend assembly name"
        else
            match name.PublicKey with
            | FriendPublicKey.PublicKeyToken _ ->
                Error
                    "FriendAssemblyName: PublicKeyToken is not permitted on a friend assembly name (must use full PublicKey)"
            | FriendPublicKey.NotStrongNamed
            | FriendPublicKey.FullPublicKey _ -> Ok ()

    let private bytesEqual (a : byte[]) (b : byte[]) : bool =
        if isNull a || isNull b then
            isNull a && isNull b
        elif a.Length <> b.Length then
            false
        else
            let mutable eq = true
            let mutable i = 0

            while eq && i < a.Length do
                if a.[i] <> b.[i] then
                    eq <- false

                i <- i + 1

            eq

    /// Compute the eight-byte public key token from a full public key blob,
    /// delegating to the host BCL's <c>AssemblyName.GetPublicKeyToken</c>.
    /// Per ECMA-335 II.6.3 this is the low eight bytes of SHA-1 of the key,
    /// reversed. The computation is pure (input bytes -> output bytes) so
    /// the host's BCL agrees with what the guest's BCL would compute.
    let private computeTokenFromPublicKey (publicKey : byte[]) : byte[] =
        let an = AssemblyName ()
        an.SetPublicKey publicKey
        an.GetPublicKeyToken ()

    let private namesEqual (a : string) (b : string) : bool =
        if isNull a || isNull b then
            isNull a && isNull b
        else
            String.Equals (a, b, StringComparison.OrdinalIgnoreCase)

    /// Faithful port of CoreCLR's <c>BaseAssemblySpec::RefMatchesDef</c>
    /// composed with <c>CompareRefToDef</c>. Treats the
    /// <c>FriendAssemblyName</c> as the ref and the supplied
    /// <c>AssemblyName</c> as the def.
    let matchesDef (ref : FriendAssemblyName) (def : AssemblyName) : bool =
        // RefMatchesDef: if ref is not strong-named, only compare names.
        match ref.PublicKey with
        | FriendPublicKey.NotStrongNamed -> namesEqual ref.Name def.Name
        | _ ->

        // Strong-named ref: def must also be strong-named. Inspect the
        // AssemblyName fields directly rather than calling
        // GetPublicKeyToken(), which derives the token (and throws
        // SecurityException on a structurally malformed full-key blob)
        // when the def carries a key but no explicit token. We use
        // AssemblyNameFlags.PublicKey to distinguish: when the flag is set
        // the def has a full key (GetPublicKeyToken would derive on
        // demand); when clear, GetPublicKeyToken returns the literal
        // stored token bytes (no derivation, no validation).
        let defKey = def.GetPublicKey ()
        let defHasFullKey = not (isNull defKey) && defKey.Length > 0

        let defHasFullKeyFlag =
            (def.Flags &&& AssemblyNameFlags.PublicKey) <> AssemblyNameFlags.None

        let defStoredToken : byte[] =
            if defHasFullKeyFlag then
                // Stored-token field is unused when the full-key flag is
                // set; defer the derivation to the token-ref branch
                // below, which is the only branch that needs the token.
                Array.empty
            else
                let t = def.GetPublicKeyToken ()
                if isNull t then Array.empty else t

        let defIsStrongNamed = defHasFullKey || defStoredToken.Length > 0

        if not defIsStrongNamed then
            false
        else if

            // CompareRefToDef name comparison.
            not (namesEqual ref.Name def.Name)
        then
            false
        else

        // CompareRefToDef public key / token comparison.
        // CoreCLR stores a single m_pbPublicKeyOrToken blob per spec and a
        // flag that says whether it's a full key or a token; CompareRefToDef
        // is a length+memcmp on those bytes. RefMatchesDef branches on the
        // ref:
        //   - Full-key ref: CompareRefToDef runs directly. If the def
        //     carries only a token (smaller bytes), the length mismatch
        //     fails the comparison; checkFriendRestrictions has already
        //     guaranteed no Friend ref is token-only, but a def populated
        //     from a token-only display name is possible. Model this
        //     faithfully by requiring def to expose a full key. CoreCLR
        //     additionally compares the afPublicKey flag in its
        //     masked-flags strict-equality check, so we also require the
        //     def's PublicKey flag bit to be set; otherwise a manually
        //     constructed def with bytes set via SetPublicKey but the
        //     flag cleared (the AssemblyName.Flags setter is mutable)
        //     would spuriously match. The comparison is over raw bytes,
        //     so a structurally malformed full-key blob is fine —
        //     CoreCLR does not validate the blob during the comparison
        //     either.
        //   - Token-only ref: CoreCLR copies the def and calls
        //     ConvertPublicKeyToToken before CompareRefToDef, so a full-key
        //     def is reduced to its token first. We derive the token here
        //     when needed.
        let keysMatch =
            match ref.PublicKey with
            | FriendPublicKey.NotStrongNamed -> false
            | FriendPublicKey.FullPublicKey k ->
                if defHasFullKey && defHasFullKeyFlag then
                    bytesEqual k defKey
                else
                    false
            | FriendPublicKey.PublicKeyToken t ->
                let defTokenMaterialised =
                    if defStoredToken.Length > 0 then defStoredToken
                    elif defHasFullKey then computeTokenFromPublicKey defKey
                    else Array.empty

                bytesEqual t defTokenMaterialised

        if not keysMatch then
            false
        else

        // CompareRefToDef Retargetable comparison. CoreCLR includes
        // Retargetable in the masked-flags strict-equality check, so the
        // ref's flag must equal the def's. (ProcessorArchitecture is masked
        // out and ignored; ContentType is handled separately below.)
        let defIsRetargetable =
            (def.Flags &&& AssemblyNameFlags.Retargetable) <> AssemblyNameFlags.None

        if ref.HasRetargetable <> defIsRetargetable then
            false
        else

        // CompareRefToDef ContentType comparison. Optional in the ref: if
        // the ref's content type is Default, the def's is unconstrained;
        // otherwise it must equal the def's exactly.
        let contentTypeMatch =
            ref.ContentType = AssemblyContentType.Default
            || ref.ContentType = def.ContentType

        if not contentTypeMatch then
            false
        else

        // CompareRefToDef cascading version comparison; -1 in any component
        // means "unspecified, do not constrain lower components". The
        // AssemblyName surface uses Version objects so we approximate by
        // looking at the structured fields.
        let versionMatch =
            match ref.Version with
            | None -> true
            | Some refV ->
                let defV = def.Version

                if isNull defV then
                    false
                else
                    let cmp (refField : int) (defField : int) (lower : (unit -> bool) option) : bool =
                        if refField < 0 then
                            true
                        elif refField <> defField then
                            false
                        else
                            match lower with
                            | None -> true
                            | Some f -> f ()

                    cmp
                        refV.Major
                        defV.Major
                        (Some (fun () ->
                            cmp
                                refV.Minor
                                defV.Minor
                                (Some (fun () ->
                                    cmp refV.Build defV.Build (Some (fun () -> cmp refV.Revision defV.Revision None))
                                ))
                        ))

        if not versionMatch then
            false
        else

        // CompareRefToDef locale comparison. CoreCLR uses strcmp, i.e.
        // case-sensitive ordinal.
        match ref.Culture with
        | None -> true
        | Some refCulture ->
            let defCulture =
                let c = def.CultureName

                if isNull c then "" else c

            String.Equals (refCulture, defCulture, StringComparison.Ordinal)

/// <summary>
/// The set of friend assemblies declared on a given assembly. Friend access
/// is granted through <c>InternalsVisibleToAttribute</c> (callers may see
/// non-public members) or <c>IgnoresAccessChecksToAttribute</c> (this
/// assembly may see non-public members of the named target).
/// </summary>
type FriendAssemblies =
    {
        /// Names from <c>[assembly: InternalsVisibleTo(...)]</c> on this
        /// assembly: assemblies that this assembly grants friend access to.
        InternalsVisibleTo : FriendAssemblyName array
        /// Names from <c>[assembly: IgnoresAccessChecksTo(...)]</c> on this
        /// assembly: assemblies whose accessibility checks this assembly
        /// asks to ignore. (Compiler-emitted; rare outside of tooling.)
        IgnoresAccessChecksTo : FriendAssemblyName array
    }

[<RequireQualifiedAccess>]
module FriendAssemblies =
    let empty : FriendAssemblies =
        {
            InternalsVisibleTo = Array.empty
            IgnoresAccessChecksTo = Array.empty
        }

    /// Resolve a custom-attribute constructor token to the
    /// <c>(namespace, name)</c> of its declaring type, using only the
    /// already-parsed indexes on a <c>DumpedAssembly</c>. Returns
    /// <c>None</c> if the constructor lives in another assembly we
    /// haven't loaded, or the metadata is malformed.
    let private constructorTypeName (assembly : DumpedAssembly) (ctorToken : MetadataToken) : (string * string) option =
        match ctorToken with
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | false, _ -> None
            | true, memberRef ->
                match memberRef.Parent with
                | MetadataToken.TypeReference typeRefHandle ->
                    match assembly.TypeRefs.TryGetValue typeRefHandle with
                    | true, typeRef -> Some (typeRef.Namespace, typeRef.Name)
                    | false, _ -> None
                | MetadataToken.TypeDefinition typeDefHandle ->
                    match assembly.TypeDefs.TryGetValue typeDefHandle with
                    | true, typeDef -> Some (typeDef.Namespace, typeDef.Name)
                    | false, _ -> None
                | _ -> None
        | MetadataToken.MethodDef methodHandle ->
            match assembly.Methods.TryGetValue methodHandle with
            | false, _ -> None
            | true, methodInfo -> Some (methodInfo.DeclaringType.Namespace, methodInfo.DeclaringType.Name)
        | _ -> None

    /// The fully-qualified type name of <c>System.Runtime.CompilerServices.InternalsVisibleToAttribute</c>.
    [<Literal>]
    let private FriendAssemblyTypeNamespace = "System.Runtime.CompilerServices"

    [<Literal>]
    let private FriendAssemblyTypeName = "InternalsVisibleToAttribute"

    [<Literal>]
    let private SubjectAssemblyTypeName = "IgnoresAccessChecksToAttribute"

    /// The raw int32 metadata token for the single-row AssemblyDefinition
    /// table (<c>mdtAssembly | 1</c>).
    [<Literal>]
    let private AssemblyDefinitionToken = 0x20000001

    /// Scan an assembly for <c>InternalsVisibleTo</c> and
    /// <c>IgnoresAccessChecksTo</c> attributes applied to the assembly itself,
    /// parsing each into a <c>FriendAssemblyName</c> after enforcing
    /// CoreCLR's friend-assembly restrictions. Returns <c>Error</c> if any
    /// such attribute carries a malformed display name or fails the
    /// restrictions check; CoreCLR would throw in that case.
    let scan (assembly : DumpedAssembly) : Result<FriendAssemblies, string> =
        let ivts = ResizeArray<FriendAssemblyName> ()
        let subjects = ResizeArray<FriendAssemblyName> ()
        let mutable error : string option = None

        match assembly.CustomAttributesByParentToken.TryGetValue AssemblyDefinitionToken with
        | false, _ -> ()
        | true, attrTokens ->
            for tokenInt in attrTokens do
                if Option.isNone error then
                    let token = MetadataToken.ofInt tokenInt

                    match token with
                    | MetadataToken.CustomAttribute handle ->
                        match assembly.Attributes.TryGetValue handle with
                        | false, _ -> ()
                        | true, attr ->
                            match constructorTypeName assembly attr.Constructor with
                            | None -> ()
                            | Some (ns, name) when ns = FriendAssemblyTypeNamespace ->
                                let isIvt = name = FriendAssemblyTypeName
                                let isSubject = name = SubjectAssemblyTypeName

                                if isIvt || isSubject then
                                    match CustomAttribute.tryReadLeadingSerString attr.Value with
                                    | Error e ->
                                        error <- Some (sprintf "FriendAssemblies: malformed CA blob on %s: %s" name e)
                                    | Ok None ->
                                        error <- Some (sprintf "FriendAssemblies: null assembly name on %s" name)
                                    | Ok (Some displayName) ->
                                        match FriendAssemblyName.parse displayName with
                                        | Error e ->
                                            error <-
                                                Some (
                                                    sprintf
                                                        "FriendAssemblies: failed to parse '%s' on %s: %s"
                                                        displayName
                                                        name
                                                        e
                                                )
                                        | Ok parsed ->
                                            match FriendAssemblyName.checkFriendRestrictions parsed with
                                            | Error e ->
                                                error <-
                                                    Some (
                                                        sprintf
                                                            "FriendAssemblies: friend restrictions failed on '%s' on %s: %s"
                                                            displayName
                                                            name
                                                            e
                                                    )
                                            | Ok () -> if isIvt then ivts.Add parsed else subjects.Add parsed
                            | Some _ -> ()
                    | _ -> ()

        match error with
        | Some e -> Error e
        | None ->
            Ok
                {
                    InternalsVisibleTo = ivts.ToArray ()
                    IgnoresAccessChecksTo = subjects.ToArray ()
                }
