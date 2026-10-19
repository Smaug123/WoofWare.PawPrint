namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// What a MemberRef's parent token names, as <c>MemberLoader::GetDescFromMemberRef</c> reads it
/// before looking for the member.
/// </summary>
[<RequireQualifiedAccess>]
type MemberReferenceParent =
    /// A type definition, with any instantiation, custom modifier or primitive spelling read
    /// through to the definition it names.
    | Nominal of ResolvedTypeIdentity
    /// An array type, whose members the runtime supplies. Custom modifiers are stripped.
    | Array of TypeDefn
    /// A type variable of the context using the reference, which only an instantiation decides.
    | TypeVariable
    /// A MethodDef of this module: a vararg call site naming its definition.
    | VarArgDefinition of MethodDefinitionHandle
    /// CoreCLR's load of the parent reaches a type reference that names no type in the assembly it
    /// is scoped to, so binding the member throws <c>TypeLoadException</c>, whatever the member. The
    /// miss is the first such reference that load reaches.
    | Unresolved of TypeResolutionMiss
    /// CoreCLR's load of the parent reaches a type reference to an assembly no runtime directory
    /// supplies, so binding the member throws <c>FileNotFoundException</c>, whatever the member. The
    /// reference is the first such one that load reaches.
    | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference

[<RequireQualifiedAccess>]
module MemberReferenceParent =

    /// The definition a type spelling in `spellingAssembly` names, looking through custom modifiers
    /// and instantiations, and reading a primitive as its CoreLib type.
    let private identityOfSpelling
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<ResolvedTypeIdentity, TypeLoad.LoadMiss>
        =
        let ofReference (typeRef : TypeRef) =
            let assemblies, identity =
                TypeResolution.tryResolveTypeRefIdentity
                    loggerFactory
                    dotnetRuntimeDirs
                    spellingAssembly
                    typeRef
                    assemblies

            assemblies, TypeLoad.ofIdentity identity

        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, _) ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromDefinition (identity, _) -> assemblies, Ok identity
            | TypeDefn.FromReference (typeRef, _) -> ofReference typeRef
            | other ->
                failwith
                    $"An instantiation in %s{spellingAssembly.DefinitionFullName} applies arguments to %O{other}, which names no generic definition"
        | TypeDefn.FromDefinition (identity, _) -> assemblies, Ok identity
        | TypeDefn.FromReference (typeRef, _) -> ofReference typeRef
        | TypeDefn.PrimitiveType primitive ->
            assemblies, Ok (BaseClassTypes.ofPrimitive baseClassTypes primitive).Identity
        | other ->
            failwith $"Expected a type with members in %s{spellingAssembly.DefinitionFullName}, but got %O{other}"

    /// Read the parent token of one of `referencingAssembly`'s MemberRefs. Loads whatever assemblies
    /// naming it takes.
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (referencingAssembly : DumpedAssembly)
        (reference : MemberReferenceHandle)
        : LoadedAssemblies * MemberReferenceParent
        =
        let row = referencingAssembly.Members.[reference]

        let unloadable (miss : TypeLoad.LoadMiss) : MemberReferenceParent =
            match miss with
            | TypeLoad.LoadMiss.TypeAbsent miss -> MemberReferenceParent.Unresolved miss
            | TypeLoad.LoadMiss.AssemblyUnavailable reference -> MemberReferenceParent.AssemblyUnavailable reference

        let nominal (assemblies, result) =
            match result with
            | Ok identity -> assemblies, MemberReferenceParent.Nominal identity
            | Error miss -> assemblies, unloadable miss

        // CoreCLR loads the whole parent before it looks for the member, so a parent whose load
        // reaches a type reference that names nothing, or an assembly that is not there, binds no
        // member.
        let loaded (spelling : TypeDefn) (answer : LoadedAssemblies -> LoadedAssemblies * MemberReferenceParent) =
            match
                TypeLoad.loadMiss loggerFactory dotnetRuntimeDirs baseClassTypes referencingAssembly spelling assemblies
            with
            | assemblies, Some miss -> assemblies, unloadable miss
            | assemblies, None -> answer assemblies

        match row.Parent with
        | MetadataToken.MethodDef handle -> assemblies, MemberReferenceParent.VarArgDefinition handle
        | MetadataToken.TypeDefinition handle ->
            let identity = referencingAssembly.TypeDefs.[handle].Identity

            loaded
                (TypeDefn.FromDefinition (identity, SignatureTypeKind.Unknown))
                (fun assemblies -> assemblies, MemberReferenceParent.Nominal identity)
        | MetadataToken.TypeReference handle ->
            let typeRef = referencingAssembly.TypeRefs.[handle]

            loaded
                (TypeDefn.FromReference (typeRef, SignatureTypeKind.Unknown))
                (fun assemblies ->
                    let assemblies, identity =
                        TypeResolution.tryResolveTypeRefIdentity
                            loggerFactory
                            dotnetRuntimeDirs
                            referencingAssembly
                            typeRef
                            assemblies

                    nominal (assemblies, TypeLoad.ofIdentity identity)
                )
        | MetadataToken.TypeSpecification handle ->
            let spelling = referencingAssembly.TypeSpecs.[handle].Signature

            loaded spelling
            <| fun assemblies ->

                match TypeDefn.stripCustomModifiers spelling with
                | TypeDefn.OneDimensionalArrayLowerBoundZero _
                | TypeDefn.Array _ as arrayType -> assemblies, MemberReferenceParent.Array arrayType
                | TypeDefn.GenericTypeParameter _
                | TypeDefn.GenericMethodParameter _ -> assemblies, MemberReferenceParent.TypeVariable
                | _ ->
                    identityOfSpelling
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        referencingAssembly
                        spelling
                        assemblies
                    |> nominal
        | MetadataToken.ModuleReference _ ->
            failwith
                $"TODO: MemberRef %s{row.PrettyName} in %s{referencingAssembly.DefinitionFullName} names a global member of another module, which is not modelled"
        | other ->
            failwith
                $"MemberRef %s{row.PrettyName} in %s{referencingAssembly.DefinitionFullName} has parent %O{other}, which ECMA-335 does not permit"
