namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

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
        failwith "TODO: AssemblyRefRequest.publicKeyToken"

    /// <summary>
    /// The reference CoreCLR's emitter makes to the assembly whose metadata <paramref name="reader"/>
    /// reads: <c>AssemblySpec::InitializeSpec</c> over its <c>Assembly</c> row, with the key reduced to
    /// its token as <c>AssemblySpec::EmitToken</c> does.
    /// </summary>
    let ofDefinition (reader : MetadataReader) : AssemblyRefRequest =
        failwith "TODO: AssemblyRefRequest.ofDefinition"

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
        failwith "TODO: EmitScopeReferences.unescapeTypeName"

    /// <summary>
    /// The namespace and name a <c>TypeRef</c> row holds for a full type name, as CoreCLR's
    /// <c>ns::SplitPath</c> divides it: at the last <c>.</c>, unless that is the name's first
    /// character, and at the <c>.</c> before it if there is one, so that <c>A..ctor</c> is the name
    /// <c>.ctor</c> in the namespace <c>A</c>. With no such <c>.</c>, the namespace is empty.
    /// </summary>
    let splitTypeName (fullName : string) : string * string =
        failwith "TODO: EmitScopeReferences.splitTypeName"

    /// <summary>
    /// The <c>AssemblyRef</c> row that CoreCLR's <c>ImportHelper::FindAssemblyRef</c> finds for
    /// <paramref name="request"/>: the first with the same name, culture, version and public key token.
    /// Build and revision are not compared for a row named <c>mscorlib</c> or
    /// <c>Microsoft.VisualC</c>, in any case, and flags are not compared at all.
    /// </summary>
    let tryFindAssemblyRef (request : AssemblyRefRequest) (scope : EmitScope) : AssemblyRefRid option =
        failwith "TODO: EmitScopeReferences.tryFindAssemblyRef"

    /// <summary>
    /// The <c>AssemblyRef</c> row for <paramref name="request"/>: the one <c>tryFindAssemblyRef</c>
    /// finds, or a new one, as <c>RegMeta::DefineAssemblyRef</c> adds it. A new row's version
    /// components are the request's, each 65535 stored as 0; its flags are the request's, with the
    /// processor-architecture bits cleared unless the request marks them specified, and that mark
    /// cleared (<c>PrepareForSaving</c>); and its hash value is empty.
    /// </summary>
    let defineAssemblyRef (request : AssemblyRefRequest) (scope : EmitScope) : AssemblyRefRid * EmitScope =
        failwith "TODO: EmitScopeReferences.defineAssemblyRef"

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
        failwith "TODO: EmitScopeReferences.tryFindTypeRef"

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
        failwith "TODO: EmitScopeReferences.defineTypeRefByName"
