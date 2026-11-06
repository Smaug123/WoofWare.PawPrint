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

/// What CoreCLR's emitter writes into a dynamic module for a method signature read from another module
/// (<c>TranslateSigWithScope</c>).
[<RequireQualifiedAccess>]
type MethodSignatureImport =
    /// The signature names no type by token, and these are the bytes the emitter writes for it.
    | Copied of ImmutableArray<byte>
    /// The signature names a type by token, which the emitter rewrites to name a <c>TypeRef</c> of the
    /// dynamic module, adding <c>TypeRef</c> and <c>AssemblyRef</c> rows of its own.
    | NamesTypeByToken

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
            let name = EmitScope.stringAt row.Name scope
            let key = EmitScope.blobAt row.PublicKeyOrToken scope

            // `FindAssemblyRef` reads a row's `PublicKey` flag only when the row holds key bytes. A row
            // holding a full key is compared by the key's token, and nothing here writes one yet.
            if not key.IsEmpty && row.Flags.HasFlag AssemblyFlags.PublicKey then
                failwith
                    "TODO: EmitScopeReferences.tryFindAssemblyRef: an AssemblyRef row holds a full public key, which ImportHelper::FindAssemblyRef compares by its token"

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
            && key.AsSpan().SequenceEqual (request.PublicKeyToken.AsSpan ())

        EmitScope.assemblyRefs scope
        |> Seq.tryFindIndex matches
        |> Option.map (fun index -> AssemblyRefRid (index + 1))

    let private storedVersion (version : MetadataVersion) : MetadataVersion =
        let given (part : uint16) : uint16 =
            if part = UInt16.MaxValue then 0us else part

        {
            Major = given version.Major
            Minor = given version.Minor
            Build = given version.Build
            Revision = given version.Revision
        }

    let private storedFlags (flags : AssemblyFlags) : AssemblyFlags =
        // `afPA_Specified` and `afPA_FullMask`, which `PrepareForSaving` reads.
        let architectureSpecified = enum<AssemblyFlags> 0x0080
        let architecture = enum<AssemblyFlags> 0x00F0

        if flags.HasFlag architectureSpecified then
            flags &&& ~~~architectureSpecified
        else
            flags &&& ~~~architecture

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

            EmitScope.addAssemblyRef
                {
                    Version = storedVersion request.Version
                    Flags = storedFlags request.Flags
                    PublicKeyOrToken = token
                    Name = name
                    Culture = culture
                    HashValue = hash
                }
                scope

    /// <summary>
    /// Whether <paramref name="row"/> is a row that <c>defineAssemblyRef</c> could have returned for
    /// <paramref name="request"/>: the one <c>tryFindAssemblyRef</c> finds, or a row holding what it
    /// adds. These differ for a version component of 65535, which a new row stores as 0, so that the
    /// row it adds is never found again.
    /// </summary>
    let isAssemblyRefFor (request : AssemblyRefRequest) (row : AssemblyRefRid) (scope : EmitScope) : bool =
        if tryFindAssemblyRef request scope = Some row then
            true
        else
            match EmitScope.tryAssemblyRefRid row.Rid scope with
            | None -> false
            | Some _ ->
                let held = (EmitScope.assemblyRefs scope).[row.Rid - 1]

                String.Equals (EmitScope.stringAt held.Name scope, request.Name, StringComparison.Ordinal)
                && String.Equals (EmitScope.stringAt held.Culture scope, request.Culture, StringComparison.Ordinal)
                && held.Version = storedVersion request.Version
                && held.Flags = storedFlags request.Flags
                && (EmitScope.blobAt held.PublicKeyOrToken scope)
                    .AsSpan()
                    .SequenceEqual (request.PublicKeyToken.AsSpan ())
                && (EmitScope.blobAt held.HashValue scope).IsEmpty

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

    /// `CompareMemberRefs` skips the name or the signature when it is empty, and the linear search
    /// `FindMemberRef` falls back to differs from the hashed one there; a method's name and signature
    /// are never empty, so neither is admitted.
    let private requireMemberRefKey (operation : string) (name : string) (signature : ImmutableArray<byte>) : unit =
        if String.IsNullOrEmpty name then
            invalidArg (nameof name) $"%s{operation}: a member reference's name is never empty"

        if signature.IsDefaultOrEmpty then
            invalidArg (nameof signature) $"%s{operation}: a member reference's signature is never empty"

    /// <summary>
    /// The <c>MemberRef</c> row with this parent, name and signature, as CoreCLR's
    /// <c>CMiniMdRW::CompareMemberRefs</c> finds it: the first whose parent is the same, whose name is
    /// the same UTF-8, and whose signature is the same bytes.
    /// </summary>
    /// <exception cref="ArgumentException">The name or the signature is empty.</exception>
    let tryFindMemberRef
        (parent : MemberRefParent)
        (name : string)
        (signature : ImmutableArray<byte>)
        (scope : EmitScope)
        : MemberRefRid option
        =
        requireMemberRefKey "EmitScopeReferences.tryFindMemberRef" name signature

        // Equal strings, and equal blobs, are one heap entry each, so a row matches exactly when it
        // holds these offsets.
        match EmitScope.tryStringOffset name scope, EmitScope.tryBlobOffset signature scope with
        | Some name, Some signature ->
            EmitScope.memberRefs scope
            |> Seq.tryFindIndex (fun row -> row.Parent = parent && row.Name = name && row.Signature = signature)
            |> Option.map (fun index -> MemberRefRid (index + 1))
        | _ -> None

    /// <summary>
    /// The <c>MemberRef</c> row for a member of <paramref name="parent"/>: the one
    /// <c>tryFindMemberRef</c> finds, or a new one, as <c>RegMeta::DefineMemberRef</c> adds it in a
    /// dynamic module, whose emitter checks for duplicate member references. A new row interns the
    /// name before the signature.
    /// </summary>
    /// <exception cref="ArgumentException">The name or the signature is empty.</exception>
    let defineMemberRef
        (parent : MemberRefParent)
        (name : string)
        (signature : ImmutableArray<byte>)
        (scope : EmitScope)
        : MemberRefRid * EmitScope
        =
        match tryFindMemberRef parent name signature scope with
        | Some row -> row, scope
        | None ->
            let name, scope = EmitScope.internString name scope
            let signature, scope = EmitScope.internBlob signature scope

            EmitScope.addMemberRef
                {
                    Parent = parent
                    Name = name
                    Signature = signature
                }
                scope

    /// The bytes of a compressed unsigned integer (ECMA-335 II.23.2) in its shortest form.
    let private compressed (value : int) : byte list =
        if value < 0x80 then
            [ byte value ]
        elif value < 0x4000 then
            [ byte (0x80 ||| (value >>> 8)) ; byte value ]
        else
            [
                byte (0xC0 ||| (value >>> 24))
                byte (value >>> 16)
                byte (value >>> 8)
                byte value
            ]

    /// <summary>
    /// What <c>TranslateSigWithScope</c> writes into a dynamic module for a method signature (ECMA-335
    /// II.23.2.1) of another module, walking it as <c>ImportHelper::MergeUpdateTokenInSig</c> does. A
    /// class or value type, a generic type it instantiates, or a custom modifier names a type by token,
    /// which the emitter rewrites. Of the rest, the index of a type or method parameter is written in
    /// its shortest form, every other byte is copied, and bytes after the signature's last type are
    /// dropped.
    /// </summary>
    /// <exception cref="ArgumentException">
    /// The signature is empty, ends early, is not a method signature, or holds an element type the
    /// emitter refuses.
    /// </exception>
    let importMethodSignature (signature : ImmutableArray<byte>) : MethodSignatureImport =
        if signature.IsDefaultOrEmpty then
            invalidArg (nameof signature) "a method signature is never empty"

        let output = ResizeArray<byte> ()
        let position = ref 0

        let fail (reason : string) : 'a =
            invalidArg
                (nameof signature)
                $"%s{reason} at byte %d{position.Value} of the method signature %A{List.ofSeq signature}"

        let readCompressed () : int =
            let at = position.Value

            if at >= signature.Length then
                fail "the signature ends early"

            let first = int signature.[at]

            let length =
                if first &&& 0x80 = 0 then 1
                elif first &&& 0xC0 = 0x80 then 2
                elif first &&& 0xE0 = 0xC0 then 4
                else fail "a compressed integer has no valid length"

            if at + length > signature.Length then
                fail "the signature ends early"

            position.Value <- at + length

            match length with
            | 1 -> first
            | 2 -> ((first &&& 0x3F) <<< 8) ||| int signature.[at + 1]
            | _ ->
                ((first &&& 0x1F) <<< 24)
                ||| (int signature.[at + 1] <<< 16)
                ||| (int signature.[at + 2] <<< 8)
                ||| int signature.[at + 3]

        let copyFrom (start : int) : unit =
            for i in start .. position.Value - 1 do
                output.Add signature.[i]

        // `CorIsModifierElementType`: `PTR`, `BYREF`, and every element type with bit 0x40, such as
        // `SENTINEL` and `PINNED`.
        let isModifier (element : int) : bool =
            element = 0x0F || element = 0x10 || element &&& 0x40 <> 0

        // Each returns false, as soon as it meets one, for a type named by token.
        let rec methodSignature () : bool =
            let start = position.Value
            let header = readCompressed ()

            match header &&& 0x0F with
            // `FIELD`, `LOCAL_SIG`, `PROPERTY`, `GENERICINST`, and kinds past the last.
            | 0x06
            | 0x07
            | 0x08
            | 0x0A -> fail $"calling convention 0x%02x{header} is not a method's"
            | kind when kind > 0x0B -> fail $"calling convention 0x%02x{header} is not a method's"
            | _ -> ()

            if header &&& 0x10 <> 0 then
                readCompressed () |> ignore<int>

            let count = readCompressed ()
            // The calling convention and the counts are copied as they are encoded.
            copyFrom start

            let rec parameters (remaining : int) : bool =
                remaining = 0 || (fieldType () && parameters (remaining - 1))

            fieldType () && parameters count

        and fieldType () : bool =
            let start = position.Value
            let mutable element = readCompressed ()

            while isModifier element do
                element <- readCompressed ()

            copyFrom start

            match element with
            // `SZARRAY`
            | 0x1D -> fieldType ()
            // `GENERICINST`, whose argument count is written in its shortest form.
            | 0x15 ->
                fieldType ()
                && (let count = readCompressed ()
                    output.AddRange (compressed count)

                    let rec arguments (remaining : int) : bool =
                        remaining = 0 || (fieldType () && arguments (remaining - 1))

                    arguments count)
            // `VAR` and `MVAR`
            | 0x13
            | 0x1E ->
                output.AddRange (compressed (readCompressed ()))
                true
            // `ARRAY`, whose shape is copied as it is encoded. A rank of 0 is all of the shape the
            // emitter reads.
            | 0x14 ->
                fieldType ()
                && (let start = position.Value

                    if readCompressed () <> 0 then
                        for _ in 1 .. readCompressed () do
                            readCompressed () |> ignore<int>

                        // A compressed signed integer's length is read as an unsigned one's.
                        for _ in 1 .. readCompressed () do
                            readCompressed () |> ignore<int>

                    copyFrom start
                    true)
            // `FNPTR`
            | 0x1B -> methodSignature ()
            // `VALUETYPE`, `CLASS`, `CMOD_REQD` and `CMOD_OPT`
            | 0x11
            | 0x12
            | 0x1F
            | 0x20 -> false
            // `ELEMENT_TYPE_MAX` and past it, and `VALUEARRAY`, which the emitter refuses as a bad
            // signature.
            | 0x17 -> fail $"element type 0x%02x{element} is refused"
            | _ when element >= 0x22 -> fail $"element type 0x%02x{element} is refused"
            | _ -> true

        if methodSignature () then
            MethodSignatureImport.Copied (ImmutableArray.CreateRange output)
        else
            MethodSignatureImport.NamesTypeByToken
