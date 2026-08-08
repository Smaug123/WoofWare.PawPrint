namespace WoofWare.PawPrint

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

/// <summary>
/// Represents a .NET assembly definition.
/// This is a strongly-typed representation of AssemblyDefinition from System.Reflection.Metadata.
/// </summary>
type AssemblyDefinition =
    {
        /// <summary>
        /// The fully specified name of the assembly, including name, version, culture, and public key token.
        /// </summary>
        Name : AssemblyName
    }

[<RequireQualifiedAccess>]
module AssemblyDefinition =
    let make (assy : System.Reflection.Metadata.AssemblyDefinition) : AssemblyDefinition =
        {
            Name = assy.GetAssemblyName ()
        }

/// Metadata for a manifest resource whose payload is embedded in this assembly image.
type EmbeddedManifestResource =
    {
        AssemblyFullName : string
        Name : string
        PayloadRelativeVirtualAddress : int
        PayloadLength : int
    }

/// Metadata for a manifest resource whose payload is stored in a file named by this assembly's File table.
type ExternalManifestResource =
    {
        AssemblyFullName : string
        Name : string
        FileName : string
        /// Byte offset within the linked file at which the resource's 4-byte
        /// little-endian length prefix begins. The payload bytes follow that
        /// prefix. Not validated here, since the linked file may not be loaded.
        Offset : int64
    }

/// Result of looking up a manifest resource by exact metadata name.
[<RequireQualifiedAccess>]
type ManifestResourceLookupResult =
    | NotFound
    | Embedded of EmbeddedManifestResource
    | ExternalFile of ExternalManifestResource
    | ReferencedAssembly of resourceName : string * assemblyReference : WoofWare.PawPrint.AssemblyReference

/// <summary>
/// Represents a fully parsed .NET assembly with all its metadata components.
/// This serves as the main container for accessing assembly information in the PawPrint library.
/// </summary>
type DumpedAssembly =
    {
        OriginalPath : string option

        /// <summary>Logger for recording information about this assembly.</summary>
        Logger : ILogger

        /// <summary>
        /// Dictionary of all type definitions in this assembly, keyed by their handle.
        /// </summary>
        TypeDefs :
            IReadOnlyDictionary<TypeDefinitionHandle, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>

        /// <summary>
        /// Dictionary of all type references in this assembly, keyed by their handle.
        /// </summary>
        TypeRefs : IReadOnlyDictionary<TypeReferenceHandle, WoofWare.PawPrint.TypeRef>

        /// <summary>
        /// Dictionary of all type specifications in this assembly, keyed by their handle.
        /// Type specifications represent complex types like generic instantiations.
        /// </summary>
        TypeSpecs : IReadOnlyDictionary<TypeSpecificationHandle, WoofWare.PawPrint.TypeSpec>

        /// <summary>
        /// Dictionary of all method definitions in this assembly, keyed by their handle.
        /// </summary>
        Methods :
            IReadOnlyDictionary<
                MethodDefinitionHandle,
                WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
             >

        /// <summary>
        /// Dictionary of all member references in this assembly, keyed by their handle.
        /// </summary>
        Members : IReadOnlyDictionary<MemberReferenceHandle, WoofWare.PawPrint.MemberReference<MetadataToken>>

        /// <summary>
        /// Dictionary of all field definitions in this assembly, keyed by their handle.
        /// </summary>
        Fields :
            IReadOnlyDictionary<FieldDefinitionHandle, WoofWare.PawPrint.FieldInfo<GenericParamFromMetadata, TypeDefn>>

        /// <summary>
        /// The entry point method of the assembly, if one exists.
        /// </summary>
        MainMethod : MethodDefinitionHandle option

        /// <summary>
        /// Dictionary of all method specifications in this assembly, keyed by their handle.
        /// Method specifications typically represent generic method instantiations.
        /// </summary>
        MethodSpecs : ImmutableDictionary<MethodSpecificationHandle, MethodSpec>

        /// <summary>
        /// Function to resolve string tokens to their actual string values.
        /// </summary>
        Strings : StringToken -> string

        /// <summary>
        /// Dictionary of all assembly references in this assembly, keyed by their handle.
        /// </summary>
        AssemblyReferences : ImmutableDictionary<AssemblyReferenceHandle, WoofWare.PawPrint.AssemblyReference>

        /// <summary>
        /// Information about this assembly.
        /// </summary>
        ThisAssemblyDefinition : AssemblyDefinition

        /// <summary>
        /// The root namespace of this assembly.
        /// </summary>
        RootNamespace : Namespace

        /// <summary>
        /// Dictionary of all non-root namespaces in this assembly, keyed by their name components.
        /// </summary>
        NonRootNamespaces : ImmutableDictionary<string list, Namespace>

        /// <summary>
        /// The PE reader for the underlying assembly file.
        /// TODO: work out how to render all the strings up front, then drop this.
        /// </summary>
        PeReader : PEReader

        /// <summary>
        /// True when disposing this record should dispose <c>PeReader</c>.
        /// Cached file-backed assemblies share a PE reader, so cache hits do not own it.
        /// </summary>
        OwnsPeReader : bool

        /// <summary>
        /// SHA-256 of the entire PE image: the identity of this assembly's <em>content</em>, as
        /// opposed to the identity it declares for itself. See <c>HasSameContentAs</c>.
        /// </summary>
        /// <remarks>
        /// Computed while the backing stream is known to be open, because it cannot be computed
        /// later: <c>PEReader</c> reads lazily, and callers of <c>Assembly.read</c> own the stream
        /// they pass and may close it as soon as the parse returns.
        /// </remarks>
        ContentHash : ImmutableArray<byte>

        /// <summary>
        /// Dictionary of all custom attributes in this assembly, keyed by their handle.
        /// </summary>
        Attributes : ImmutableDictionary<CustomAttributeHandle, WoofWare.PawPrint.CustomAttribute>

        /// <summary>
        /// Index from parent metadata token (as raw int32) to the raw int32 tokens of all
        /// CustomAttribute rows applied to that parent.
        /// </summary>
        CustomAttributesByParentToken : ImmutableDictionary<int, ImmutableArray<int>>

        /// <summary>
        /// Dictionary of all exported types in this assembly, keyed by their handle.
        /// </summary>
        ExportedTypes : ImmutableDictionary<ExportedTypeHandle, WoofWare.PawPrint.ExportedType>

        /// <summary>
        /// Internal lookup for top-level type definitions by namespace and name.
        /// </summary>
        _TopLevelTypeDefsLookup :
            ImmutableDictionary<string * string, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>

        /// <summary>
        /// Internal lookup for nested type definitions by declaring type and simple name.
        /// </summary>
        _NestedTypeDefsLookup :
            ImmutableDictionary<
                ComparableTypeDefinitionHandle * string,
                WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>
             >

        /// <summary>
        /// Internal lookup for top-level exported types by namespace and name.
        /// </summary>
        _TopLevelExportedTypesLookup : ImmutableDictionary<string option * string, WoofWare.PawPrint.ExportedType>

        /// <summary>
        /// Internal lookup for nested exported types by parent export and simple name.
        /// </summary>
        _NestedExportedTypesLookup : ImmutableDictionary<ExportedTypeHandle * string, WoofWare.PawPrint.ExportedType>

        /// <summary>
        /// Friend-assembly declarations parsed from assembly-level
        /// <c>InternalsVisibleTo</c> and <c>IgnoresAccessChecksTo</c> custom
        /// attributes on this assembly. Cached eagerly so visibility checks
        /// (e.g. CA filtering via <c>IsCAVisibleFromDecoratedType</c>) do not
        /// re-walk the assembly-level attribute list per call.
        /// </summary>
        Friends : FriendAssemblies
    }

    static member internal BuildTopLevelTypeDefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeDefs : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> seq)
        : ImmutableDictionary<string * string, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeDefs do
            if not ty.IsNested then
                let key = ty.Namespace, ty.Name

                if keys.Add key then
                    result.Add (key, ty)
                else
                    logger.LogDebug (
                        "Duplicate top-level type defs from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                        name,
                        ty.Namespace,
                        ty.Name
                    )

        result.ToImmutable ()

    static member internal BuildNestedTypeDefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeDefs : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeDefs do
            if ty.IsNested then
                let key = (ComparableTypeDefinitionHandle.Make ty.DeclaringType, ty.Name)

                if keys.Add key then
                    result.Add (key, ty)
                else
                    logger.LogDebug (
                        "Duplicate nested type defs from assembly {ThisAssemblyName}: parent {DeclaringTypeHandle}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                        name,
                        ty.DeclaringType,
                        ty.Name
                    )

        result.ToImmutable ()

    static member internal BuildTopLevelExportedTypesLookup
        (logger : ILogger)
        (name : AssemblyName)
        (types : WoofWare.PawPrint.ExportedType seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in types do
            match ty.Data with
            | ExportedTypeData.ParentExportedType _ -> ()
            | _ ->
                let key = ty.Namespace, ty.Name

                if keys.Add key then
                    result.Add (key, ty)
                else
                    failwithf
                        "Duplicate top-level exported types from assembly %O: namespace %O, type %s"
                        name
                        ty.Namespace
                        ty.Name

        result.ToImmutable ()

    static member internal BuildNestedExportedTypesLookup
        (logger : ILogger)
        (name : AssemblyName)
        (types : WoofWare.PawPrint.ExportedType seq)
        : ImmutableDictionary<ExportedTypeHandle * string, WoofWare.PawPrint.ExportedType>
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in types do
            match ty.Data with
            | ExportedTypeData.ParentExportedType parent ->
                let key = (parent, ty.Name)

                if keys.Add key then
                    result.Add (key, ty)
                else
                    failwithf "Duplicate nested exported types from assembly %O: parent %O, type %s" name parent ty.Name
            | _ -> ()

        result.ToImmutable ()

    member this.Name = this.ThisAssemblyDefinition.Name

    /// <summary>
    /// The module version ID: a GUID the compiler stamps afresh on every build. Useful for
    /// reporting, but it is metadata like any other — see <c>HasSameContentAs</c>.
    /// </summary>
    member this.ModuleVersionId : Guid =
        let metadata = this.PeReader.GetMetadataReader ()
        metadata.GetGuid (metadata.GetModuleDefinition().Mvid)

    /// <summary>
    /// The raw <c>Culture</c> column of this assembly's manifest row: the empty string for a
    /// culture-neutral assembly, and otherwise a culture name such as <c>"en-GB"</c>.
    /// </summary>
    /// <remarks>
    /// Deliberately *not* <c>this.Name.CultureName</c>. That goes through
    /// <c>AssemblyName</c>, which normalises the string via <c>CultureInfo</c>: an assembly
    /// whose column reads <c>"EN-gb"</c> reports <c>"en-GB"</c> there. CoreCLR's
    /// <c>PEAssembly::GetLocale</c> returns <c>md.szLocale</c>, the column verbatim, and
    /// hands that to the guest, so anything reproducing that primitive needs the raw string.
    ///
    /// A nil <c>Culture</c> handle reads as the empty string, which is what CoreCLR sees too:
    /// its <c>getString</c> resolves the nil index to offset 0 of the <c>#Strings</c> heap,
    /// where the empty string lives. So "culture-neutral" and "culture explicitly empty" are
    /// indistinguishable here exactly as they are there.
    /// </remarks>
    member this.CultureName : string =
        let metadata = this.PeReader.GetMetadataReader ()
        metadata.GetString (metadata.GetAssemblyDefinition().Culture)

    /// <summary>
    /// The raw <c>PublicKey</c> blob column of this assembly's manifest row: the full public
    /// key of a strong-named assembly, and empty for one that is not strong-named. This is
    /// the key itself, not the eight-byte token that display names carry.
    /// </summary>
    /// <remarks>
    /// Deliberately *not* <c>this.Name.GetPublicKey()</c>, for the same reason
    /// <see cref="CultureName"/> is not <c>this.Name.CultureName</c>: <c>AssemblyName</c>
    /// reports <c>null</c> for an assembly with no key, where the column — and so CoreCLR's
    /// <c>PEAssembly::GetPublicKey</c>, and so the guest — sees a zero-length blob. The two
    /// agree whenever a key is actually present.
    ///
    /// A nil <c>PublicKey</c> handle reads as an empty (not default) array, matching the
    /// zero <c>cbPublicKey</c> that CoreCLR's <c>GetAssemblyProps</c> reports for that row.
    /// </remarks>
    member this.PublicKey : ImmutableArray<byte> =
        let metadata = this.PeReader.GetMetadataReader ()
        metadata.GetBlobContent (metadata.GetAssemblyDefinition().PublicKey)

    /// <summary>
    /// Whether this and <paramref name="other"/> are byte-identical PE images, i.e. the same
    /// assembly by every observation this interpreter can make of it.
    /// </summary>
    /// <remarks>
    /// This is how to ask "are these two <c>DumpedAssembly</c> values the same assembly?" when they
    /// are not reference-equal. Weaker proxies do not hold:
    ///
    /// <para>
    /// The declared identity (name, version, culture, public key) is a <em>claim</em>, not a
    /// fingerprint — two different builds can make the identical claim, most obviously for unsigned
    /// assemblies whose whole identity is
    /// "Foo, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null".
    /// </para>
    ///
    /// <para>
    /// The MVID is likewise only an assertion of sameness stamped into the image, not a digest of
    /// it: an IL rewriter that preserves the MVID, or hand-crafted metadata, yields differing
    /// images that claim to be one build.
    /// </para>
    ///
    /// <para>
    /// Even the metadata block is not enough. It excludes IL method bodies and manifest-resource
    /// payloads, which live elsewhere in the image and which we read through
    /// <c>PEReader.GetMethodBody</c> and <c>PEReader.GetSectionData</c> — so two images agreeing on
    /// metadata can still execute differently.
    /// </para>
    ///
    /// Hence the whole image. Comparing more than we strictly consume can only ever cost us a
    /// spurious crash on two images we would have treated alike, which is the safe direction to
    /// err: correctness over availability.
    /// </remarks>
    member this.HasSameContentAs (other : DumpedAssembly) : bool =
        Object.ReferenceEquals (this, other)
        || this.ContentHash.AsSpan().SequenceEqual (other.ContentHash.AsSpan ())

    member this.TryGetTopLevelTypeDef
        (``namespace`` : string)
        (name : string)
        : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        match this._TopLevelTypeDefsLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TryGetNestedTypeDef
        (declaringType : TypeDefinitionHandle)
        (name : string)
        : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        match this._NestedTypeDefsLookup.TryGetValue ((ComparableTypeDefinitionHandle.Make declaringType, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TryGetTopLevelExportedType
        (``namespace`` : string option)
        (name : string)
        : WoofWare.PawPrint.ExportedType option
        =
        // ExportedType.Namespace uses None for the global namespace,
        // but callers may pass Some "". Normalize to match the dictionary key.
        let ns =
            match ``namespace`` with
            | Some "" -> None
            | other -> other

        match this._TopLevelExportedTypesLookup.TryGetValue ((ns, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TryGetNestedExportedType
        (parent : ExportedTypeHandle)
        (name : string)
        : WoofWare.PawPrint.ExportedType option
        =
        match this._NestedExportedTypesLookup.TryGetValue ((parent, name)) with
        | false, _ -> None
        | true, v -> Some v

    interface IDisposable with
        member this.Dispose () =
            if this.OwnsPeReader then
                this.PeReader.Dispose ()

/// <summary>
/// The assemblies the interpreter has loaded, together with the record of which
/// AssemblyReferences have already been bound to which of them. This is the CLR's split
/// between a *binder* (which reference identity resolves to which assembly) and a *load
/// context* (which assembly has which identity).
/// </summary>
/// <remarks>
/// Two kinds of identity are in play, and they are NOT interchangeable:
///
/// <para>
/// A <em>definition</em> identity is an assembly's own <c>AssemblyDefinition</c> FullName. It is
/// what <c>DumpedAssembly.Name</c>, <c>ConcreteType.Assembly</c> and
/// <c>ResolvedTypeIdentity.AssemblyFullName</c> all carry, and it is the load context's key.
/// </para>
///
/// <para>
/// A <em>reference</em> identity is the FullName recorded in an <c>AssemblyReference</c> row of
/// some other assembly. It need not equal the definition identity of whatever it binds to:
/// in the .NET shared framework the .NET Framework compatibility facades (mscorlib, System,
/// System.Core, ...) reference their implementation assemblies with <c>Version=0.0.0.0</c>,
/// and any assembly built for an older target framework references the BCL by the version
/// that framework shipped rather than the version on disk.
/// </para>
///
/// Consequently the only way to ask about a reference identity is <c>TryResolveReference</c>,
/// which takes an <c>AssemblyReference</c> — you cannot reach the binder with a bare
/// <c>AssemblyName</c>, and every other member consumes a definition identity.
/// </remarks>
[<NoEquality ; NoComparison>]
type LoadedAssemblies =
    private
        {
            /// AssemblyDefinition FullName -> the assembly bearing that identity.
            ByDefinition : ImmutableDictionary<string, DumpedAssembly>
            /// AssemblyReference FullName -> the AssemblyDefinition FullName it bound to.
            Bindings : ImmutableDictionary<string, string>
        }

    /// Every definition identity currently loaded. For diagnostics only.
    member this.DefinitionNames : string seq = this.ByDefinition.Keys

    /// Look an assembly up by its definition identity.
    member this.TryByDefinitionName (fullName : string) : DumpedAssembly option =
        match this.ByDefinition.TryGetValue fullName with
        | false, _ -> None
        | true, v -> Some v

    /// Look an assembly up by its definition identity.
    member this.TryByDefinition (name : AssemblyName) : DumpedAssembly option = this.TryByDefinitionName name.FullName

    /// Look an assembly up by its definition identity, failing loudly if it is not loaded.
    member this.ByDefinitionName (fullName : string) : DumpedAssembly =
        match this.ByDefinition.TryGetValue fullName with
        | true, v -> v
        | false, _ ->
            failwithf
                "Assembly %s is not loaded. Loaded assemblies: %s"
                fullName
                (this.ByDefinition.Keys |> Seq.sort |> String.concat " ; ")

    /// Look an assembly up by its definition identity, failing loudly if it is not loaded.
    member this.Item
        with get (name : AssemblyName) : DumpedAssembly = this.ByDefinitionName name.FullName

    /// True if an assembly with this definition identity is loaded.
    member this.ContainsDefinition (name : AssemblyName) : bool =
        this.ByDefinition.ContainsKey name.FullName

    /// <summary>
    /// Resolve an AssemblyReference to the assembly it names, if we have already bound it.
    /// </summary>
    /// <remarks>
    /// A reference with no recorded binding may still name an assembly we hold, if its
    /// reference identity happens to equal that assembly's definition identity — the CLR's
    /// exact-identity match. That is how an assembly registered directly (the entry assembly,
    /// or a fixture that exists only in memory) is found the first time some other assembly
    /// references it; without this fallback we would go to disk for an assembly we already
    /// have, and fail outright for one that was never written to disk.
    /// </remarks>
    member this.TryResolveReference (reference : WoofWare.PawPrint.AssemblyReference) : DumpedAssembly option =
        let refFullName = reference.Name.FullName

        match this.Bindings.TryGetValue refFullName with
        | true, definitionName -> this.TryByDefinitionName definitionName
        | false, _ -> this.TryByDefinitionName refFullName

    /// <summary>
    /// The canonical instance for <paramref name="assy"/>'s definition identity: whatever we
    /// already hold under that identity, or <paramref name="assy"/> itself if we hold nothing.
    /// </summary>
    /// <remarks>
    /// Every route into the load context goes through here, so that none of them can register a
    /// second, conflicting build under an identity already spoken for. Two distinct assemblies
    /// claiming one definition identity means silently picking one of two different sets of
    /// metadata to resolve and execute against; there is no safe choice, so crash.
    ///
    /// Sameness is decided by comparing metadata content, not by trusting any identifier the image
    /// carries — see <c>DumpedAssembly.HasSameContentAs</c>. The comparison only runs when two
    /// non-reference-equal instances collide, which is rare.
    /// </remarks>
    member private this.Canonicalise (assy : DumpedAssembly) (describeRequester : unit -> string) : DumpedAssembly =
        match this.ByDefinition.TryGetValue assy.Name.FullName with
        | false, _ -> assy
        | true, existing ->
            if not (existing.HasSameContentAs assy) then
                failwithf
                    "Two different assemblies both claim definition identity %s (module version IDs %O and %O, and their metadata differs). Refusing to guess which one %s refers to."
                    assy.Name.FullName
                    existing.ModuleVersionId
                    assy.ModuleVersionId
                    (describeRequester ())

            existing

    /// <summary>
    /// Register an assembly under its own definition identity. Idempotent for the same build: if
    /// that identity is already loaded, the existing instance wins. Registering a *different*
    /// build under an identity we already hold is an error — see <c>Canonicalise</c>.
    /// </summary>
    member this.WithLoadedAssembly (assy : DumpedAssembly) : LoadedAssemblies =
        let canonical = this.Canonicalise assy (fun () -> "this direct registration")

        if Object.ReferenceEquals (canonical, assy) then
            { this with
                ByDefinition = this.ByDefinition.SetItem (assy.Name.FullName, assy)
            }
        else
            this

    /// <summary>
    /// Record that <paramref name="reference"/> binds to <paramref name="assy"/>, registering
    /// the assembly under its own definition identity if we do not already hold it. Returns the
    /// canonical instance for that definition identity, which is the previously-loaded one if
    /// there was one — so exactly one <c>DumpedAssembly</c> exists per definition identity.
    /// </summary>
    member this.WithBoundReference
        (reference : WoofWare.PawPrint.AssemblyReference)
        (assy : DumpedAssembly)
        : LoadedAssemblies * DumpedAssembly
        =
        let definitionName = assy.Name.FullName
        let canonical = this.Canonicalise assy (fun () -> reference.Name.FullName)

        let result =
            {
                ByDefinition = this.ByDefinition.SetItem (definitionName, canonical)
                Bindings = this.Bindings.SetItem (reference.Name.FullName, definitionName)
            }

        result, canonical

[<RequireQualifiedAccess>]
module LoadedAssemblies =
    let empty : LoadedAssemblies =
        {
            ByDefinition = ImmutableDictionary.Empty
            Bindings = ImmutableDictionary.Empty
        }

    /// Build a load context from assemblies indexed by their own definition identities, with no
    /// reference bindings recorded yet.
    let ofAssemblies (assemblies : DumpedAssembly seq) : LoadedAssemblies =
        assemblies
        |> Seq.fold (fun (acc : LoadedAssemblies) assy -> acc.WithLoadedAssembly assy) empty

    /// <summary>
    /// Assert that the load prompted by a <c>TypeResolutionResult.FirstLoadAssy</c> achieved what
    /// the caller is about to retry on: the reference now resolves.
    /// </summary>
    /// <remarks>
    /// Every resolution loop responds to <c>FirstLoadAssy</c> by loading and then re-running the
    /// identical resolution. If the load leaves the reference still unbound, that re-run takes the
    /// identical branch and the loop spins forever. A livelock is far worse to diagnose than a
    /// crash — and, since the only way to leave a reference unbound after loading it is to have
    /// mis-filed it, the crash names the real fault.
    /// </remarks>
    let assertReferenceBound
        (context : string)
        (reference : WoofWare.PawPrint.AssemblyReference)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies
        =
        match assemblies.TryResolveReference reference with
        | Some _ -> assemblies
        | None ->
            failwithf
                "While resolving %s: loaded %s (referenced by %s), but it is still not bound afterwards. Retrying would loop forever; the load context has probably filed it under the wrong identity."
                context
                reference.Name.FullName
                (snd reference.Handle).FullName

type TypeResolutionResult =
    | FirstLoadAssy of WoofWare.PawPrint.AssemblyReference
    | Resolved of DumpedAssembly * ResolvedTypeIdentity * TypeInfo<TypeDefn, TypeDefn>

    override this.ToString () : string =
        match this with
        | TypeResolutionResult.FirstLoadAssy a -> $"FirstLoadAssy(%s{a.Name.FullName})"
        | TypeResolutionResult.Resolved (assy, identity, ty) ->
            $"Resolved(%s{assy.Name.FullName}: %O{identity} {string<TypeInfo<TypeDefn, TypeDefn>> ty})"

[<RequireQualifiedAccess>]
module Assembly =
    type private AssemblyFileCacheKey =
        {
            FullPath : string
            Length : int64
            LastWriteTimeUtc : DateTime
        }

    /// Process-lifetime cache for explicit file-backed reads. The parsed metadata is
    /// immutable, and the key includes simple file metadata so rebuilt files get a fresh parse.
    let private fileCache : ConcurrentDictionary<AssemblyFileCacheKey, Lazy<DumpedAssembly>> =
        ConcurrentDictionary<AssemblyFileCacheKey, Lazy<DumpedAssembly>> ()

    let private readUncached
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (dllBytes : Stream)
        : DumpedAssembly
        =
        let peReader = new PEReader (dllBytes)
        let metadataReader = peReader.GetMetadataReader ()

        // Must happen here, not on demand: PEReader reads lazily, and callers of `read` own the
        // stream they handed us and are free to close it the moment this returns.
        let contentHash =
            let image = peReader.GetEntireImage ()

            System.Security.Cryptography.SHA256.HashData (image.GetContent().AsSpan ())
            |> ImmutableArray.Create<byte>

        let assy = metadataReader.GetAssemblyDefinition () |> AssemblyDefinition.make

        let entryPoint =
            peReader.PEHeaders.CorHeader.EntryPointTokenOrRelativeVirtualAddress
            |> fun x -> if x = 0 then None else Some x

        let entryPointMethod =
            entryPoint |> Option.map MetadataTokens.MethodDefinitionHandle

        let assemblyRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ref in metadataReader.AssemblyReferences do
                builder.Add (ref, AssemblyReference.make (ref, assy.Name) (metadataReader.GetAssemblyReference ref))

            builder.ToImmutable ()

        let typeRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeReferences do
                builder.Add (ty, TypeRef.make metadataReader ty)

            builder.ToImmutable ()

        let typeDefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeDefinitions do
                builder.Add (ty, TypeInfo.read peReader assy.Name metadataReader ty)

            builder.ToImmutable ()

        // TODO: this probably misses any methods out which aren't associated with a type definition?
        let methods =
            typeDefs
            |> Seq.collect (fun (KeyValue (_, ty)) -> ty.Methods |> List.map (fun mi -> KeyValuePair (mi.Handle, mi)))
            |> ImmutableDictionary.CreateRange

        let methodSpecs =
            Seq.init
                (metadataReader.GetTableRowCount TableIndex.MethodSpec)
                (fun i ->
                    let i = i + 1
                    let handle = MetadataTokens.MethodSpecificationHandle i
                    KeyValuePair (handle, MethodSpec.make assy.Name (metadataReader.GetMethodSpecification handle))
                )
            |> ImmutableDictionary.CreateRange

        let typeSpecs =
            let result = ImmutableDictionary.CreateBuilder ()

            for i = 1 to metadataReader.GetTableRowCount TableIndex.TypeSpec do
                let handle = MetadataTokens.TypeSpecificationHandle i
                result.Add (handle, metadataReader.GetTypeSpecification handle |> TypeSpec.make assy.Name handle)

            result.ToImmutable ()

        let memberReferences =
            let builder = ImmutableDictionary.CreateBuilder ()

            for c in metadataReader.MemberReferences do
                builder.Add (
                    c,
                    MemberReference.make<MetadataToken>
                        metadataReader.GetBlobReader
                        metadataReader.GetString
                        MetadataToken.ofEntityHandle
                        assy.Name
                        (metadataReader.GetMemberReference c)
                )

            builder.ToImmutable ()

        // TODO: render all this up front
        let strings (token : StringToken) =
            match token with
            | StringToken.String s -> metadataReader.GetString s
            | StringToken.UserString s -> metadataReader.GetUserString s

        let rootNamespace, nonRootNamespaces =
            metadataReader.GetNamespaceDefinitionRoot ()
            |> Namespace.make metadataReader.GetString metadataReader.GetNamespaceDefinition

        let fields =
            let result = ImmutableDictionary.CreateBuilder ()

            for field in metadataReader.FieldDefinitions do
                let fieldDefn =
                    metadataReader.GetFieldDefinition field
                    |> FieldInfo.make metadataReader assy.Name field

                result.Add (field, fieldDefn)

            result.ToImmutable ()

        let exportedTypes =
            let result = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.ExportedTypes do
                result.Add (ty, ExportedType.make metadataReader.GetString ty (metadataReader.GetExportedType ty))

            result.ToImmutable ()

        let attrs, (customAttributesByParentToken : ImmutableDictionary<int, ImmutableArray<int>>) =
            let attrsBuilder =
                ImmutableDictionary.CreateBuilder<CustomAttributeHandle, WoofWare.PawPrint.CustomAttribute> ()

            let groupedByParent = Dictionary<int, ImmutableArray<int>.Builder> ()

            for handle in metadataReader.CustomAttributes do
                let typed =
                    metadataReader.GetCustomAttribute handle
                    |> CustomAttribute.make metadataReader handle

                attrsBuilder.Add (handle, typed)

                let parentToken = MetadataToken.toInt typed.Parent

                let attrAsEntity : EntityHandle = CustomAttributeHandle.op_Implicit handle
                let attrToken = MetadataTokens.GetToken attrAsEntity

                let builder =
                    match groupedByParent.TryGetValue parentToken with
                    | true, b -> b
                    | false, _ ->
                        let b = ImmutableArray.CreateBuilder ()
                        groupedByParent.[parentToken] <- b
                        b

                builder.Add attrToken

            let parentTokenResult = ImmutableDictionary.CreateBuilder ()

            for kvp in groupedByParent do
                parentTokenResult.Add (kvp.Key, kvp.Value.ToImmutable ())

            attrsBuilder.ToImmutable (), parentTokenResult.ToImmutable ()

        let logger = loggerFactory.CreateLogger assy.Name.Name

        let friends =
            let input : FriendAssembliesScanInput =
                {
                    CustomAttributesByParentToken = customAttributesByParentToken
                    Attributes = attrs
                    Members = memberReferences
                    TypeRefs = typeRefs
                    TypeDefs = typeDefs
                    Methods = methods
                }

            match FriendAssemblies.scan input with
            | Ok f -> f
            | Error e -> failwithf "FriendAssemblies.scan failed on %s: %s" assy.Name.Name e

        {
            Logger = logger
            OriginalPath = originalPath
            TypeDefs = typeDefs
            TypeRefs = typeRefs
            TypeSpecs = typeSpecs
            MainMethod = entryPointMethod
            Methods = methods
            MethodSpecs = methodSpecs
            Members = memberReferences
            Strings = strings
            Fields = fields
            AssemblyReferences = assemblyRefs
            ThisAssemblyDefinition = assy
            RootNamespace = rootNamespace
            NonRootNamespaces = nonRootNamespaces
            PeReader = peReader
            OwnsPeReader = true
            ContentHash = contentHash
            Attributes = attrs
            CustomAttributesByParentToken = customAttributesByParentToken
            ExportedTypes = exportedTypes
            _TopLevelTypeDefsLookup = DumpedAssembly.BuildTopLevelTypeDefsLookup logger assy.Name typeDefs.Values
            _NestedTypeDefsLookup = DumpedAssembly.BuildNestedTypeDefsLookup logger assy.Name typeDefs.Values
            _TopLevelExportedTypesLookup =
                DumpedAssembly.BuildTopLevelExportedTypesLookup logger assy.Name exportedTypes.Values
            _NestedExportedTypesLookup =
                DumpedAssembly.BuildNestedExportedTypesLookup logger assy.Name exportedTypes.Values
            Friends = friends
        }

    let private fileCacheKey (path : string) : AssemblyFileCacheKey =
        let fileInfo = FileInfo path

        {
            FullPath = fileInfo.FullName
            Length = fileInfo.Length
            LastWriteTimeUtc = fileInfo.LastWriteTimeUtc
        }

    let private withLogger
        (loggerFactory : ILoggerFactory)
        (fullPath : string)
        (assembly : DumpedAssembly)
        : DumpedAssembly
        =
        { assembly with
            Logger = loggerFactory.CreateLogger assembly.Name.Name
            OriginalPath = Some fullPath
        }

    let private removeCachedFile (key : AssemblyFileCacheKey) (cached : Lazy<DumpedAssembly>) : unit =
        let pair = KeyValuePair<AssemblyFileCacheKey, Lazy<DumpedAssembly>> (key, cached)

        (fileCache :> ICollection<KeyValuePair<AssemblyFileCacheKey, Lazy<DumpedAssembly>>>).Remove pair
        |> ignore<bool>

    let private readCachedFile (loggerFactory : ILoggerFactory) (path : string) : DumpedAssembly =
        let key = fileCacheKey path

        let cached =
            fileCache.GetOrAdd (
                key,
                Func<AssemblyFileCacheKey, Lazy<DumpedAssembly>> (fun key ->
                    lazy
                        (let bytes = File.ReadAllBytes key.FullPath
                         let stream = new MemoryStream (bytes, false)
                         let assembly = readUncached loggerFactory (Some key.FullPath) stream

                         { assembly with
                             OwnsPeReader = false
                         })
                )
            )

        try
            cached.Value |> withLogger loggerFactory key.FullPath
        with _ ->
            removeCachedFile key cached
            reraise ()

    /// Read an assembly from a file path using the process-lifetime parse cache.
    let readFile (loggerFactory : ILoggerFactory) (path : string) : DumpedAssembly = readCachedFile loggerFactory path

    /// Read an assembly from the supplied stream without consulting the file cache.
    let read (loggerFactory : ILoggerFactory) (originalPath : string option) (dllBytes : Stream) : DumpedAssembly =
        readUncached loggerFactory originalPath dllBytes

    let private checkedManifestResourceOffset (resourceName : string) (offset : int64) : int =
        if offset < 0L || offset > int64 Int32.MaxValue then
            failwith $"Manifest resource %s{resourceName} has unsupported metadata offset %d{offset}"

        int offset

    let private checkedManifestResourceLength (resourceName : string) (length : uint32) : int =
        if length > uint32 Int32.MaxValue then
            failwith $"Manifest resource %s{resourceName} has unsupported payload length %d{length}"

        int length

    let private checkedManifestResourceRelativeVirtualAddress (resourceName : string) (rva : int64) : int =
        if rva < 0L || rva > int64 Int32.MaxValue then
            failwith $"Manifest resource %s{resourceName} has unsupported relative virtual address %d{rva}"

        int rva

    let private validateManifestResourceImplementationRow
        (metadataReader : MetadataReader)
        (resourceName : string)
        (tableIndex : TableIndex)
        (implementation : EntityHandle)
        : unit
        =
        let expectedKind =
            match tableIndex with
            | TableIndex.File -> HandleKind.AssemblyFile
            | TableIndex.AssemblyRef -> HandleKind.AssemblyReference
            | _ ->
                failwith
                    $"Manifest resource %s{resourceName} row validation does not support implementation table %O{tableIndex}"

        if implementation.Kind <> expectedKind then
            failwith
                $"Manifest resource %s{resourceName} validates implementation table %O{tableIndex}, but handle kind is %O{implementation.Kind}"

        let row = MetadataTokens.GetRowNumber implementation
        let rowCount = metadataReader.GetTableRowCount tableIndex

        if row < 1 || row > rowCount then
            failwith
                $"Manifest resource %s{resourceName} points at invalid %O{tableIndex} row %d{row}; table has %d{rowCount} rows"

    let private readEmbeddedManifestResource
        (assy : DumpedAssembly)
        (resourceName : string)
        (resource : System.Reflection.Metadata.ManifestResource)
        : EmbeddedManifestResource
        =
        let resourceDirectory = assy.PeReader.PEHeaders.CorHeader.ResourcesDirectory

        if resourceDirectory.RelativeVirtualAddress <= 0 then
            failwith
                $"Manifest resource %s{resourceName} is embedded, but the CLI resource directory RVA is non-positive"

        if resourceDirectory.Size < 0 then
            failwith $"Manifest resource %s{resourceName} is embedded, but the CLI resource directory size is negative"

        let resourceOffset = checkedManifestResourceOffset resourceName resource.Offset

        if int64 resourceOffset + 4L > int64 resourceDirectory.Size then
            failwith
                $"Manifest resource %s{resourceName} header offset %d{resourceOffset} is outside CLI resource directory size %d{resourceDirectory.Size}"

        let headerRva =
            int64 resourceDirectory.RelativeVirtualAddress + int64 resourceOffset
            |> checkedManifestResourceRelativeVirtualAddress resourceName

        let sectionData = assy.PeReader.GetSectionData headerRva
        let mutable reader = sectionData.GetReader ()

        // Corrupt directory RVAs can point outside every PE section; in that case
        // GetSectionData may have fewer bytes than the directory bounds imply.
        if reader.Length < 4 then
            failwith
                $"Manifest resource %s{resourceName} section data at header RVA %d{headerRva} is shorter than 4 bytes"

        let payloadLength =
            reader.ReadUInt32 () |> checkedManifestResourceLength resourceName

        let resourcePayloadEnd = int64 resourceOffset + 4L + int64 payloadLength

        if resourcePayloadEnd > int64 resourceDirectory.Size then
            failwith
                $"Manifest resource %s{resourceName} declares payload end offset %d{resourcePayloadEnd}, beyond CLI resource directory size %d{resourceDirectory.Size}"

        if payloadLength > reader.RemainingBytes then
            failwith
                $"Manifest resource %s{resourceName} declares payload length %d{payloadLength}, but only %d{reader.RemainingBytes} bytes remain in the section"

        let payloadRva =
            int64 headerRva + 4L
            |> checkedManifestResourceRelativeVirtualAddress resourceName

        {
            AssemblyFullName = assy.Name.FullName
            Name = resourceName
            PayloadRelativeVirtualAddress = payloadRva
            PayloadLength = payloadLength
        }

    let findManifestResource (assy : DumpedAssembly) (resourceName : string) : ManifestResourceLookupResult =
        let metadataReader = assy.PeReader.GetMetadataReader ()

        let resource =
            metadataReader.ManifestResources
            |> Seq.tryPick (fun handle ->
                let resource = metadataReader.GetManifestResource handle
                let name = metadataReader.GetString resource.Name

                if String.Equals (name, resourceName, StringComparison.Ordinal) then
                    Some (name, resource)
                else
                    None
            )

        match resource with
        | None -> ManifestResourceLookupResult.NotFound
        | Some (name, resource) ->
            if resource.Implementation.IsNil then
                resource
                |> readEmbeddedManifestResource assy name
                |> ManifestResourceLookupResult.Embedded
            else
                match resource.Implementation.Kind with
                | HandleKind.AssemblyFile ->
                    validateManifestResourceImplementationRow
                        metadataReader
                        name
                        TableIndex.File
                        resource.Implementation

                    let fileHandle = AssemblyFileHandle.op_Explicit resource.Implementation

                    let file = metadataReader.GetAssemblyFile fileHandle
                    let fileName = metadataReader.GetString file.Name

                    // CoreCLR rejects file-backed manifest resources when fetching them on .NET
                    // Core. This metadata layer still reports the ECMA shape so the eventual
                    // AssemblyNative_GetResource implementation can make that policy, including
                    // File row flag checks, explicit.
                    {
                        AssemblyFullName = assy.Name.FullName
                        Name = name
                        FileName = fileName
                        Offset = resource.Offset
                    }
                    |> ManifestResourceLookupResult.ExternalFile
                | HandleKind.AssemblyReference ->
                    validateManifestResourceImplementationRow
                        metadataReader
                        name
                        TableIndex.AssemblyRef
                        resource.Implementation

                    if resource.Offset <> 0L then
                        failwith
                            $"Manifest resource %s{name} is forwarded to an assembly reference, but declares non-zero offset %d{resource.Offset}"

                    let assemblyReferenceHandle =
                        AssemblyReferenceHandle.op_Explicit resource.Implementation

                    // Assembly.read populates this map from the AssemblyRef table rows. Keep the
                    // guard so manually constructed DumpedAssembly values fail with resource context.
                    match assy.AssemblyReferences.TryGetValue assemblyReferenceHandle with
                    | true, assemblyReference ->
                        ManifestResourceLookupResult.ReferencedAssembly (name, assemblyReference)
                    | false, _ ->
                        failwith
                            $"Manifest resource %s{name} points at missing assembly reference handle %O{assemblyReferenceHandle}"
                | other -> failwith $"Manifest resource %s{name} has unsupported implementation handle kind %O{other}"

    let print (main : MethodDefinitionHandle) (dumped : DumpedAssembly) : unit =
        for KeyValue (_, typ) in dumped.TypeDefs do
            Console.WriteLine $"\nType: %s{typ.Namespace}.%s{typ.Name}"

            for method in typ.Methods do
                if method.Handle = main then
                    Console.WriteLine "Entry point!"

                Console.WriteLine $"\nMethod: %s{method.Name}"

                match method.Body with
                | MethodBody.Il instructions ->
                    instructions.Instructions
                    |> List.map (fun (op, index) -> IlOp.Format op index)
                    |> List.iter Console.WriteLine
                | MethodBody.InternalCall -> Console.WriteLine "<InternalCall: no IL>"
                | MethodBody.PInvoke -> Console.WriteLine "<P/Invoke: no IL>"
                | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateCtor ->
                    Console.WriteLine "<runtime-provided: delegate .ctor>"
                | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke ->
                    Console.WriteLine "<runtime-provided: delegate Invoke>"
                | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, targetName)) ->
                    let nameStr =
                        match targetName with
                        | Some n -> $"\"%s{n}\""
                        | None -> "<inherits attributed name>"

                    Console.WriteLine $"<runtime-provided: UnsafeAccessor %O{kind}, target=%s{nameStr}>"
                | MethodBody.RuntimeProvided (RuntimeBehaviour.Unrecognised name) ->
                    Console.WriteLine $"<runtime-provided: unclassified ({name})>"
                | MethodBody.Abstract -> Console.WriteLine "<abstract: no IL>"

    let private applyGenericArgs
        (genericArgs : ImmutableArray<TypeDefn>)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeInfo<TypeDefn, TypeDefn>
        =
        ty
        |> TypeInfo.mapGeneric (fun (param, _) ->
            if param.SequenceNumber < genericArgs.Length then
                genericArgs.[param.SequenceNumber]
            else
                TypeDefn.GenericTypeParameter param.SequenceNumber
        )

    let private resolveDefinedType
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeResolutionResult
        =
        TypeResolutionResult.Resolved (
            assy,
            ResolvedTypeIdentity.ofTypeDefinition assy.Name ty.TypeDefHandle,
            applyGenericArgs genericArgs ty
        )

    let resolveTypeIdentityDefinition
        (assy : DumpedAssembly)
        (identity : ResolvedTypeIdentity)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        if assy.Name.FullName <> identity.AssemblyFullName then
            failwithf
                "ResolvedTypeIdentity points at assembly %s but attempted lookup used assembly %s"
                identity.AssemblyFullName
                assy.Name.FullName

        match assy.TypeDefs.TryGetValue identity.TypeDefinition.Get with
        | true, defn -> defn
        | false, _ ->
            failwithf
                "ResolvedTypeIdentity points at missing type definition handle %A in assembly %s"
                identity.TypeDefinition.Get
                assy.Name.FullName

    let rec fullName (assy : DumpedAssembly) (identity : ResolvedTypeIdentity) : string =
        resolveTypeIdentityDefinition assy identity
        |> TypeInfo.fullName (fun h -> assy.TypeDefs.[h])

    let rec private resolveTopLevelTypeInAssembly
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (ns : string option)
        (name : string)
        : TypeResolutionResult
        =
        let nsString = ns |> Option.defaultValue ""

        match assy.TryGetTopLevelTypeDef nsString name with
        | Some typeDef -> resolveDefinedType genericArgs assy typeDef
        | None ->
            match assy.TryGetTopLevelExportedType ns name with
            | Some export -> resolveTypeFromExport assy assemblies genericArgs export
            | None ->
                failwithf
                    "Top-level type resolution failed for %s %s in %s"
                    (ns |> Option.defaultValue "<global>")
                    name
                    assy.Name.FullName

    // No exported-type fallback is needed here (unlike resolveTopLevelTypeInAssembly).
    // This function is only reached after the parent TypeRef has been fully resolved through
    // any forwarding chains, so `assy` is the assembly that *defines* the declaring type.
    // ECMA-335 requires nested types to reside in the same assembly as their declaring type,
    // so the child must be a local TypeDef here.
    // Nested *forwarded* types (ExportedTypeData.ParentExportedType) are handled by a
    // separate code path: resolveTypeFromExport -> resolveExportedTypeByChain.
    and private resolveNestedTypeInAssembly
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (declaringType : ResolvedTypeIdentity)
        (childName : string)
        : TypeResolutionResult
        =
        match assy.TryGetNestedTypeDef declaringType.TypeDefinition.Get childName with
        | Some typeDef -> resolveDefinedType genericArgs assy typeDef
        | None ->
            failwithf
                "Failed to resolve nested type %s inside %s in assembly %s"
                childName
                (fullName assy declaringType)
                assy.Name.FullName

    and private resolveTypeRefInAssembly
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (referencedInAssembly : DumpedAssembly)
        (typeRefHandle : TypeReferenceHandle)
        : TypeResolutionResult
        =
        let target = referencedInAssembly.TypeRefs.[typeRefHandle]
        resolveTypeRef assemblies referencedInAssembly genericArgs target

    and private resolveExportedTypeByChain
        (targetAssembly : DumpedAssembly)
        (resolvedParent : ResolvedTypeIdentity option)
        (exportedType : WoofWare.PawPrint.ExportedType)
        : ResolvedTypeIdentity
        =
        match resolvedParent with
        | Some parent ->
            match targetAssembly.TryGetNestedTypeDef parent.TypeDefinition.Get exportedType.Name with
            | Some nested -> ResolvedTypeIdentity.ofTypeDefinition targetAssembly.Name nested.TypeDefHandle
            | None ->
                failwithf
                    "Failed to resolve nested exported type %s under %s in assembly %s"
                    exportedType.Name
                    (fullName targetAssembly parent)
                    targetAssembly.Name.FullName
        | None ->
            let nsString = exportedType.Namespace |> Option.defaultValue ""

            match targetAssembly.TryGetTopLevelTypeDef nsString exportedType.Name with
            | Some topLevel -> ResolvedTypeIdentity.ofTypeDefinition targetAssembly.Name topLevel.TypeDefHandle
            | None ->
                failwithf
                    "Failed to resolve top-level exported type %s.%s in assembly %s"
                    nsString
                    exportedType.Name
                    targetAssembly.Name.FullName

    and resolveTypeFromExport
        (fromAssembly : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (ty : WoofWare.PawPrint.ExportedType)
        : TypeResolutionResult
        =
        match ty.Data with
        | ExportedTypeData.ForwardsTo assyRef ->
            let assyRef = fromAssembly.AssemblyReferences.[assyRef]

            match assemblies.TryResolveReference assyRef with
            | None -> TypeResolutionResult.FirstLoadAssy assyRef
            | Some toAssy -> resolveTopLevelTypeInAssembly assemblies genericArgs toAssy ty.Namespace ty.Name
        | ExportedTypeData.ParentExportedType parentExport ->
            let parent = fromAssembly.ExportedTypes.[parentExport]

            match resolveTypeFromExport fromAssembly assemblies genericArgs parent with
            | TypeResolutionResult.FirstLoadAssy assyRef -> TypeResolutionResult.FirstLoadAssy assyRef
            | TypeResolutionResult.Resolved (targetAssembly, parentIdentity, _) ->
                let identity = resolveExportedTypeByChain targetAssembly (Some parentIdentity) ty
                let typeDef = resolveTypeIdentityDefinition targetAssembly identity
                TypeResolutionResult.Resolved (targetAssembly, identity, applyGenericArgs genericArgs typeDef)
        | ExportedTypeData.AssemblyFile _ ->
            failwithf
                "AssemblyFile exported types are not yet supported while resolving %A from %s"
                ty.Handle
                fromAssembly.Name.FullName

    and resolveTypeRef
        (assemblies : LoadedAssemblies)
        (referencedInAssembly : DumpedAssembly)
        (genericArgs : ImmutableArray<TypeDefn>)
        (target : TypeRef)
        : TypeResolutionResult
        =
        match target.ResolutionScope with
        | TypeRefResolutionScope.Assembly r ->
            match referencedInAssembly.AssemblyReferences.TryGetValue r with
            | false, _ ->
                failwithf
                    "AssemblyReferenceHandle %A not found in assembly %s. Available references: %A"
                    r
                    referencedInAssembly.Name.FullName
                    (referencedInAssembly.AssemblyReferences.Keys |> Seq.toList)
            | true, assemblyRef ->

            match assemblies.TryResolveReference assemblyRef with
            | None -> TypeResolutionResult.FirstLoadAssy assemblyRef
            | Some assy -> resolveTopLevelTypeInAssembly assemblies genericArgs assy (Some target.Namespace) target.Name
        | TypeRefResolutionScope.TypeRef parent ->
            match resolveTypeRefInAssembly assemblies genericArgs referencedInAssembly parent with
            | TypeResolutionResult.FirstLoadAssy assyRef -> TypeResolutionResult.FirstLoadAssy assyRef
            | TypeResolutionResult.Resolved (targetAssembly, parentIdentity, _) ->
                resolveNestedTypeInAssembly assemblies genericArgs targetAssembly parentIdentity target.Name
        | TypeRefResolutionScope.ModuleDef _ ->
            // The type is defined in the current module.
            resolveTopLevelTypeInAssembly
                assemblies
                genericArgs
                referencedInAssembly
                (Some target.Namespace)
                target.Name
        | TypeRefResolutionScope.ModuleRef moduleRef ->
            failwithf
                "ModuleRef type resolution is not yet supported for type %s.%s in assembly %s via module ref %A"
                target.Namespace
                target.Name
                referencedInAssembly.Name.FullName
                moduleRef

    and resolveTopLevelTypeFromName
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        resolveTopLevelTypeInAssembly assemblies genericArgs assy ns name

[<RequireQualifiedAccess>]
module DumpedAssembly =
    let private getName (a : DumpedAssembly) : AssemblyName = a.Name

    let private getTypeDef (a : DumpedAssembly) (h : TypeDefinitionHandle) : TypeInfo<TypeDefn, TypeDefn> =
        a.TypeDefs.[h]
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

    let private getTypeRef
        (loadedAssemblies : LoadedAssemblies)
        (a : DumpedAssembly)
        (h : TypeReferenceHandle)
        : DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeRef loadedAssemblies a ImmutableArray.Empty a.TypeRefs.[h] with
        | TypeResolutionResult.Resolved (resultAssy, _, typeInfo) -> resultAssy, typeInfo
        | TypeResolutionResult.FirstLoadAssy _ ->
            failwith "seems pretty unlikely that we could have constructed this object without loading its base type"

    let private getTypeSpec
        (loadedAssemblies : LoadedAssemblies)
        (a : DumpedAssembly)
        (h : TypeSpecificationHandle)
        : DumpedAssembly * TypeDefinitionHandle
        =
        let signature = a.TypeSpecs.[h].Signature

        let rec go (currentAssembly : DumpedAssembly) (ty : TypeDefn) =
            match ty with
            | TypeDefn.GenericInstantiation (generic, _) -> go currentAssembly generic
            // A custom modifier annotates the signature; the type definition being named is the
            // unmodified one. Stepping into `Modifier` would resolve `InAttribute`/`IsVolatile`/etc.
            | TypeDefn.Modified m -> go currentAssembly m.Unmodified
            | TypeDefn.FromDefinition (identity, _) ->
                let resolvedAssembly = loadedAssemblies.ByDefinitionName identity.AssemblyFullName
                let resolvedType = resolvedAssembly.TypeDefs.[identity.TypeDefinition.Get]
                resolvedAssembly, resolvedType.TypeDefHandle
            | TypeDefn.FromReference (typeRef, _) ->
                match Assembly.resolveTypeRef loadedAssemblies currentAssembly ImmutableArray.Empty typeRef with
                | TypeResolutionResult.FirstLoadAssy assyRef ->
                    failwithf
                        "Base type traversal unexpectedly needed to load assembly %s while resolving %O from %s"
                        assyRef.Name.FullName
                        signature
                        a.Name.FullName
                | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
                    resolvedAssembly, resolvedType.TypeDefHandle
            | unexpected ->
                failwithf
                    "Unexpected TypeSpec base type shape while resolving %O from %s: %O"
                    signature
                    a.Name.FullName
                    unexpected

        go a signature

    let private assemblies (loadedAssemblies : LoadedAssemblies) (n : AssemblyName) : DumpedAssembly =
        loadedAssemblies.[n]

    /// ECMA "value type": transitively inherits from System.ValueType (possibly via System.Enum),
    /// but is NOT exactly System.ValueType or System.Enum themselves.
    let isValueType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isValueType
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// Convenience: not a value type.
    let isReferenceType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isReferenceType
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// Metadata layout kind: ValueType for value types, Class otherwise. Note that System.Enum and
    /// System.ValueType themselves encode as Class, matching real CLR signature encoding.
    let signatureTypeKind
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : SignatureTypeKind
        =
        TypeInfo.signatureTypeKind
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    let typeInfoToTypeDefn
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ti : TypeInfo<TypeDefn, TypeDefn>)
        : TypeDefn
        =
        TypeInfo.toTypeDefn
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ti

    let typeInfoToTypeDefn'
        (bct : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        =
        ti
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)
        |> typeInfoToTypeDefn bct assemblies

[<RequireQualifiedAccess>]
module AssemblyApi =
    let read = Assembly.read
    let readFile = Assembly.readFile
    let findManifestResource = Assembly.findManifestResource
    let resolveTypeRef = Assembly.resolveTypeRef
    let resolveTopLevelTypeFromName = Assembly.resolveTopLevelTypeFromName

    let resolveTypeFromExport = Assembly.resolveTypeFromExport
    let resolveTypeIdentityDefinition = Assembly.resolveTypeIdentityDefinition
    let fullName = Assembly.fullName
