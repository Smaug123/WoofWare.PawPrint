namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// The field a MemberRef row names, as CoreCLR's <c>MemberLoader::GetDescFromMemberRef</c> binds it.
/// </summary>
[<RequireQualifiedAccess>]
type FieldReferenceTarget =
    /// A field with a definition: the assembly that declares it, and its row there. For a reference
    /// whose parent is an instantiation, this is the generic definition's field.
    | Defined of declaringAssembly : DumpedAssembly * field : FieldDefinitionHandle
    /// Nothing of this name and type where CoreCLR looks, so binding the reference throws
    /// <c>MissingFieldException</c>.
    | Missing
    /// The parent is a type variable of the context using the reference.
    | DependsOnInstantiation
    /// The parent names no type in the assembly it is scoped to, so binding the reference throws
    /// <c>TypeLoadException</c>.
    | ParentTypeMissing of TypeResolutionMiss

/// <summary>
/// Which field a MemberRef names, answered at the level of generic definitions.
/// </summary>
[<RequireQualifiedAccess>]
module FieldReferenceResolution =

    /// <summary>
    /// <c>MemberLoader::FindField</c> for a field-shaped MemberRef of `referencingAssembly`.
    /// </summary>
    /// <remarks>
    /// The search is of the parent's own fields and no others: fields are not inherited, an array
    /// has none, and a literal field has no <c>FieldDesc</c> to find. A field matches by name and by
    /// its type as <c>MetaSig::CompareFieldSigs</c> compares it: exactly, custom modifiers
    /// included, with type variables left standing.
    /// </remarks>
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (referencingAssembly : DumpedAssembly)
        (reference : MemberReferenceHandle)
        : LoadedAssemblies * FieldReferenceTarget
        =
        let row = referencingAssembly.Members.[reference]
        let name = referencingAssembly.Strings row.Name

        let fieldType =
            match row.Signature with
            | MemberSignature.Field ty -> ty
            | MemberSignature.Method _ ->
                failwith $"MemberRef %s{name} in %s{referencingAssembly.DefinitionFullName} names a method, not a field"

        let operation =
            $"resolving the reference to field %s{name} from %s{referencingAssembly.DefinitionFullName}"

        match
            MemberReferenceParent.resolve
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                assemblies
                referencingAssembly
                reference
        with
        | assemblies, MemberReferenceParent.Nominal identity ->
            let declaring = assemblies.ByDefinitionName identity.AssemblyFullName
            let ty = declaring.TypeDefs.[identity.TypeDefinition.Get]

            let rec search
                (assemblies : LoadedAssemblies)
                (fields : FieldInfo<GenericParamFromMetadata, TypeDefn> list)
                =
                match fields with
                | [] -> assemblies, FieldReferenceTarget.Missing
                | field :: rest when field.Name <> name || field.Attributes.HasFlag FieldAttributes.Literal ->
                    search assemblies rest
                | field :: rest ->
                    match
                        SignatureComparison.compareSignatureTypes
                            loggerFactory
                            dotnetRuntimeDirs
                            operation
                            assemblies
                            referencingAssembly
                            fieldType
                            declaring
                            field.Signature
                    with
                    | assemblies, true -> assemblies, FieldReferenceTarget.Defined (declaring, field.Handle)
                    | assemblies, false -> search assemblies rest

            search assemblies ty.Fields
        | assemblies, MemberReferenceParent.Array _ -> assemblies, FieldReferenceTarget.Missing
        | assemblies, MemberReferenceParent.TypeVariable -> assemblies, FieldReferenceTarget.DependsOnInstantiation
        | assemblies, MemberReferenceParent.Unresolved miss -> assemblies, FieldReferenceTarget.ParentTypeMissing miss
        | _, MemberReferenceParent.VarArgDefinition _ ->
            failwith $"%s{operation}: a field reference's parent is a MethodDef, which ECMA-335 does not permit"
