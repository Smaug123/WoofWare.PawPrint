namespace WoofWare.PawPrint

open System
open System.Diagnostics
open System.Reflection
open System.Reflection.Metadata

[<NoComparison>]
type MetadataTypeIdentifier =
    private
        {
            ReferencedInAssembly : AssemblyName
            Token : MetadataToken
        }

    member this.Assembly = this.ReferencedInAssembly
    member this.MetadataToken = this.Token

[<RequireQualifiedAccess>]
module MetadataTypeIdentifier =
    let private make (assemblyName : AssemblyName) (token : MetadataToken) : MetadataTypeIdentifier =
        match token with
        | MetadataToken.TypeDefinition _
        | MetadataToken.TypeReference _
        | MetadataToken.TypeSpecification _ ->
            {
                ReferencedInAssembly = assemblyName
                Token = token
            }
        | _ -> failwithf "MetadataTypeIdentifier can only be constructed from type metadata tokens, but got %A" token

    let ofTypeDef (assemblyName : AssemblyName) (handle : TypeDefinitionHandle) : MetadataTypeIdentifier =
        make assemblyName (MetadataToken.TypeDefinition handle)

    let ofTypeRef (assemblyName : AssemblyName) (handle : TypeReferenceHandle) : MetadataTypeIdentifier =
        make assemblyName (MetadataToken.TypeReference handle)

    let ofTypeSpec (assemblyName : AssemblyName) (handle : TypeSpecificationHandle) : MetadataTypeIdentifier =
        make assemblyName (MetadataToken.TypeSpecification handle)

[<CustomEquality>]
[<CustomComparison>]
type ResolvedTypeIdentity =
    private
        {
            DefiningAssemblyFullName : string
            Definition : ComparableTypeDefinitionHandle
        }

    member this.AssemblyFullName = this.DefiningAssemblyFullName
    member this.TypeDefinition = this.Definition

    override this.Equals (other : obj) : bool =
        match other with
        | :? ResolvedTypeIdentity as other ->
            this.Definition = other.Definition
            && this.DefiningAssemblyFullName = other.DefiningAssemblyFullName
        | _ -> false

    override this.GetHashCode () : int =
        hash (this.DefiningAssemblyFullName, this.Definition)

    interface System.IComparable<ResolvedTypeIdentity> with
        member this.CompareTo (other : ResolvedTypeIdentity) : int =
            compare (this.DefiningAssemblyFullName, this.Definition) (other.DefiningAssemblyFullName, other.Definition)

    interface System.IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ResolvedTypeIdentity as other -> (this :> System.IComparable<ResolvedTypeIdentity>).CompareTo other
            | _ -> failwith "invalid comparison"

/// Helpers over an assembly's <em>definition identity</em>: the display name its
/// <c>AssemblyDefinition</c> serialises to, which is what <c>LoadedAssemblies</c> keys on and what
/// <c>ResolvedTypeIdentity</c> carries.
[<RequireQualifiedAccess>]
module AssemblyDefinitionName =
    /// Does <paramref name="identity"/> name an assembly whose simple name is
    /// <paramref name="simpleName"/>?
    ///
    /// Allocation-free, and so the spelling to reach for on a hot path: a display name puts the simple
    /// name first, so this is a prefix test rather than a parse. Ordinal and case-sensitive, which is
    /// stricter than the CLR binder's own simple-name comparison.
    ///
    /// <paramref name="simpleName"/> must need no escaping when a display name is built from it;
    /// otherwise the prefix looked for here is not the one the display name carries.
    let isNamed (simpleName : string) (identity : string) : bool =
        Debug.Assert (
            simpleName.Length > 0
            && simpleName = simpleName.Trim ()
            && simpleName.IndexOfAny [| ',' ; '=' ; '"' ; '\'' ; '\\' |] < 0,
            $"assembly simple name %s{simpleName} would be escaped in a display name, so it cannot be matched as a prefix of one"
        )

        identity.StartsWith (simpleName, StringComparison.Ordinal)
        && (identity.Length = simpleName.Length || identity.[simpleName.Length] = ',')

    /// The simple name inside a definition identity: <c>System.Runtime</c> out of
    /// <c>System.Runtime, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</c>.
    ///
    /// Parses, and so costs an <c>AssemblyName</c> with its <c>CultureInfo</c> and <c>Version</c>: this is
    /// for rendering a name to a human, and never for looking an assembly up. What a lookup wants is the
    /// whole display name, which every caller here already holds.
    let simpleName (identity : string) : string =
        match AssemblyName(identity).Name with
        | null ->
            failwith
                $"assembly definition identity %s{identity} carries no simple name, so it cannot have been serialised from an AssemblyDefinition"
        | name -> name

/// Matches a definition identity naming CoreLib, so that a classifier keyed on "is this CoreLib's
/// X" can read the identity a `ConcreteType` or a `MethodInfo` already carries rather than
/// reconstituting an `AssemblyName` to ask for its simple name.
[<AutoOpen>]
module AssemblyDefinitionNamePatterns =
    // Must stay allocation-free in its result: a match applies a partial active pattern once per
    // arm, `Intrinsics.handle` keys 66 arms on this one, and it is applied 214k times on a
    // regex-construction guest. Returning an option would allocate on each of those.
    let (|CorelibAssembly|_|) (identity : string) : bool =
        AssemblyDefinitionName.isNamed "System.Private.CoreLib" identity

[<RequireQualifiedAccess>]
module ResolvedTypeIdentity =
    let internal make (assemblyFullName : string) (handle : ComparableTypeDefinitionHandle) : ResolvedTypeIdentity =
        {
            DefiningAssemblyFullName = assemblyFullName
            Definition = handle
        }

    let ofTypeDefinition (assemblyName : AssemblyName) (handle : TypeDefinitionHandle) : ResolvedTypeIdentity =
        make assemblyName.FullName (ComparableTypeDefinitionHandle.Make handle)

    /// The identity of a TypeDef row in the assembly with this definition identity.
    ///
    /// Takes the identity rather than an <c>AssemblyName</c>, for a caller that already holds one:
    /// <c>ofTypeDefinition</c> has to serialise the name it is given, and a metadata-derived one
    /// derives its public key token by SHA-1 on every such call.
    let ofDefinitionInAssembly (assemblyFullName : string) (handle : TypeDefinitionHandle) : ResolvedTypeIdentity =
        make assemblyFullName (ComparableTypeDefinitionHandle.Make handle)
