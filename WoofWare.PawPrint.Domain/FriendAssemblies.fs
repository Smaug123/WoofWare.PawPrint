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
