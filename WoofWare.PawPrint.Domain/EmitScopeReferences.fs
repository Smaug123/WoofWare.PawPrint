namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Security.Cryptography
open System.Text

/// <summary>
/// What CoreCLR's emitter is asked to reference when a dynamic module refers to a type of another
/// assembly: the arguments <c>AssemblySpec::EmitToken</c> passes to <c>DefineAssemblyRef</c>, read
/// from that assembly's own <c>Assembly</c> row.
/// </summary>
type AssemblyRefRequest =
    {
        Name : string
        /// As the referenced assembly's row holds it. A component of 65535 means "not given" to the
        /// emitter, which leaves that column of a new row 0.
        Version : MetadataVersion
        Culture : string
        /// The referenced assembly's public key token, or empty if it has no public key: the emitter
        /// never writes a full key into a reference it makes for a type.
        PublicKeyToken : ImmutableArray<byte>
        Flags : AssemblyFlags
    }

/// Reading the reference CoreCLR's emitter makes to an assembly from that assembly's metadata.
[<RequireQualifiedAccess>]
module AssemblyRefRequest =
    /// <summary>
    /// The token of a public key (ECMA-335 II.6.2.1.3): the last eight bytes of the key's SHA-1, in
    /// reverse order, as CoreCLR's <c>StrongNameTokenFromPublicKey</c> computes it.
    /// </summary>
    let publicKeyToken (publicKey : ImmutableArray<byte>) : ImmutableArray<byte> =
        if publicKey.IsDefaultOrEmpty then
            invalidArg (nameof publicKey) "an assembly with no public key has no public key token"

        let hash = SHA1.HashData (publicKey.AsSpan ())
        ImmutableArray.CreateRange (Seq.init 8 (fun i -> hash.[hash.Length - 1 - i]))

    /// <summary>
    /// The reference CoreCLR's emitter makes to the assembly whose metadata <paramref name="reader"/>
    /// reads: <c>AssemblySpec::InitializeSpec</c> over its <c>Assembly</c> row, with the key reduced to
    /// its token as <c>AssemblySpec::EmitToken</c> does.
    /// </summary>
    let ofDefinition (reader : MetadataReader) : AssemblyRefRequest =
        let definition = reader.GetAssemblyDefinition ()
        let key = reader.GetBlobContent definition.PublicKey

        {
            Name = reader.GetString definition.Name
            Version =
                {
                    Major = uint16 definition.Version.Major
                    Minor = uint16 definition.Version.Minor
                    Build = uint16 definition.Version.Build
                    Revision = uint16 definition.Version.Revision
                }
            Culture = reader.GetString definition.Culture
            PublicKeyToken =
                if key.IsEmpty then
                    ImmutableArray.Empty
                else
                    publicKeyToken key
            // `InitializeSpec` marks a row with a key as holding one, and `EmitToken` passes the
            // token instead, unmarked.
            Flags =
                if key.IsEmpty then
                    definition.Flags
                else
                    definition.Flags &&& ~~~AssemblyFlags.PublicKey
        }

/// <summary>
/// The references CoreCLR's emitter adds to a dynamic module's scope, each found among the rows the
/// scope already has before one is added, as the emitter checks for duplicates in a dynamic module.
/// </summary>
[<RequireQualifiedAccess>]
module EmitScopeReferences =
    /// <summary>
    /// A type name as <c>ModuleBuilder_GetTypeRef</c> stores it: each backslash that escapes one of the
    /// characters a type name reserves (<c>, [ ] &amp; * + \</c>) is dropped, and the character after
    /// it kept. Every other character, a backslash before any other character included, is kept.
    /// </summary>
    let unescapeTypeName (name : string) : string =
        let isReserved (c : char) : bool =
            match c with
            | ','
            | '['
            | ']'
            | '&'
            | '*'
            | '+'
            | '\\' -> true
            | _ -> false

        let result = StringBuilder name.Length
        let mutable i = 0

        while i < name.Length do
            if name.[i] = '\\' && i + 1 < name.Length && isReserved name.[i + 1] then
                result.Append name.[i + 1] |> ignore<StringBuilder>
                i <- i + 2
            else
                result.Append name.[i] |> ignore<StringBuilder>
                i <- i + 1

        result.ToString ()

    /// <summary>
    /// The namespace and name a <c>TypeRef</c> row holds for a full type name, as CoreCLR's
    /// <c>ns::SplitPath</c> divides it: at the last <c>.</c>, unless that is the name's first
    /// character, and at the <c>.</c> before it if there is one, so that <c>A..ctor</c> is the name
    /// <c>.ctor</c> in the namespace <c>A</c>. With no such <c>.</c>, the namespace is empty.
    /// </summary>
    let splitTypeName (fullName : string) : string * string =
        let last = fullName.LastIndexOf '.'

        if last <= 0 then
            "", fullName
        else
            let separator = if fullName.[last - 1] = '.' then last - 1 else last
            fullName.Substring (0, separator), fullName.Substring (separator + 1)

    /// <summary>
    /// The <c>AssemblyRef</c> row that CoreCLR's <c>ImportHelper::FindAssemblyRef</c> finds for
    /// <paramref name="request"/>: the first with the same name, culture, version and public key token.
    /// Build and revision are not compared for a row named <c>mscorlib</c> or
    /// <c>Microsoft.VisualC</c>, in any case, and flags are not compared at all.
    /// </summary>
    let tryFindAssemblyRef (request : AssemblyRefRequest) (scope : EmitScope) : AssemblyRefRid option =
        let matches (row : AssemblyRefRow) : bool =
            // Every row this module writes holds a token. A row holding a full key is compared by the
            // key's token, and nothing here writes one yet.
            if row.Flags.HasFlag AssemblyFlags.PublicKey then
                failwith
                    "TODO: EmitScopeReferences.tryFindAssemblyRef: an AssemblyRef row holds a full public key, which ImportHelper::FindAssemblyRef compares by its token"

            let name = EmitScope.stringAt row.Name scope

            let unified =
                String.Equals (name, "mscorlib", StringComparison.OrdinalIgnoreCase)
                || String.Equals (name, "microsoft.visualc", StringComparison.OrdinalIgnoreCase)

            String.Equals (name, request.Name, StringComparison.Ordinal)
            && String.Equals (EmitScope.stringAt row.Culture scope, request.Culture, StringComparison.Ordinal)
            && row.Version.Major = request.Version.Major
            && row.Version.Minor = request.Version.Minor
            && (unified
                || row.Version.Build = request.Version.Build
                   && row.Version.Revision = request.Version.Revision)
            && (EmitScope.blobAt row.PublicKeyOrToken scope).AsSpan().SequenceEqual (request.PublicKeyToken.AsSpan ())

        EmitScope.assemblyRefs scope
        |> Seq.tryFindIndex matches
        |> Option.map (fun index -> AssemblyRefRid (index + 1))

    /// <summary>
    /// The <c>AssemblyRef</c> row for <paramref name="request"/>: the one <c>tryFindAssemblyRef</c>
    /// finds, or a new one, as <c>RegMeta::DefineAssemblyRef</c> adds it. A new row's version
    /// components are the request's, each 65535 stored as 0; its flags are the request's, with the
    /// processor-architecture bits cleared unless the request marks them specified, and that mark
    /// cleared (<c>PrepareForSaving</c>); and its hash value is empty.
    /// </summary>
    let defineAssemblyRef (request : AssemblyRefRequest) (scope : EmitScope) : AssemblyRefRid * EmitScope =
        match tryFindAssemblyRef request scope with
        | Some row -> row, scope
        | None ->
            // `_SetAssemblyRefProps` puts the token, the name, the culture and the hash, in that order.
            let token, scope = EmitScope.internBlob request.PublicKeyToken scope
            let name, scope = EmitScope.internString request.Name scope
            let culture, scope = EmitScope.internString request.Culture scope
            let hash, scope = EmitScope.internBlob ImmutableArray.Empty scope

            let given (part : uint16) : uint16 =
                if part = UInt16.MaxValue then 0us else part

            // `afPA_Specified` and `afPA_FullMask`, which `PrepareForSaving` reads.
            let architectureSpecified = enum<AssemblyFlags> 0x0080
            let architecture = enum<AssemblyFlags> 0x00F0

            let flags =
                if request.Flags.HasFlag architectureSpecified then
                    request.Flags &&& ~~~architectureSpecified
                else
                    request.Flags &&& ~~~architecture

            EmitScope.addAssemblyRef
                {
                    Version =
                        {
                            Major = given request.Version.Major
                            Minor = given request.Version.Minor
                            Build = given request.Version.Build
                            Revision = given request.Version.Revision
                        }
                    Flags = flags
                    PublicKeyOrToken = token
                    Name = name
                    Culture = culture
                    HashValue = hash
                }
                scope

    /// <summary>
    /// The <c>TypeRef</c> row with this resolution scope, namespace and name, compared ordinally, as
    /// CoreCLR's <c>ImportHelper::FindTypeRefByName</c> finds it.
    /// </summary>
    let tryFindTypeRef
        (resolutionScope : ResolutionScope)
        (ns : string)
        (name : string)
        (scope : EmitScope)
        : TypeRefRid option
        =
        // Equal strings are one heap entry, so a row matches exactly when it holds these offsets.
        match EmitScope.tryStringOffset ns scope, EmitScope.tryStringOffset name scope with
        | Some ns, Some name ->
            EmitScope.typeRefs scope
            |> Seq.tryFindIndex (fun row ->
                row.ResolutionScope = resolutionScope && row.Namespace = ns && row.Name = name
            )
            |> Option.map (fun index -> TypeRefRid (index + 1))
        | _ -> None

    /// <summary>
    /// The <c>TypeRef</c> row for a full type name in <paramref name="resolutionScope"/>, divided by
    /// <c>splitTypeName</c>: the one <c>tryFindTypeRef</c> finds, or a new one, as
    /// <c>RegMeta::DefineTypeRefByName</c> adds it, interning the namespace before the name.
    /// </summary>
    let defineTypeRefByName
        (resolutionScope : ResolutionScope)
        (fullName : string)
        (scope : EmitScope)
        : TypeRefRid * EmitScope
        =
        let ns, name = splitTypeName fullName

        match tryFindTypeRef resolutionScope ns name scope with
        | Some row -> row, scope
        | None ->
            let ns, scope = EmitScope.internString ns scope
            let name, scope = EmitScope.internString name scope

            EmitScope.addTypeRef
                {
                    ResolutionScope = resolutionScope
                    Name = name
                    Namespace = ns
                }
                scope
