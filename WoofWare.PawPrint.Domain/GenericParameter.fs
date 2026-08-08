namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type GenericVariance =
    | Covariant
    | Contravariant

type GenericConstraint =
    | Reference
    | NonNullableValue

type GenericParamMetadata =
    {
        Variance : GenericVariance option
        Constraint : GenericConstraint option
        RequiresParameterlessConstructor : bool
        /// The <c>allows ref struct</c> anti-constraint (<c>gpAllowByRefLike</c>, corhdr.h:845).
        /// Unlike every other entry here it *widens* what the parameter accepts: without it, a
        /// byref-like type argument is rejected (CoreCLR `TypeVarTypeDesc::SatisfiesConstraints`,
        /// typedesc.cpp:1606). It is orthogonal to <see cref="Constraint"/> rather than another
        /// member of that mutually-exclusive pair, so it lives in its own field alongside
        /// <see cref="RequiresParameterlessConstructor"/>.
        AllowsByRefLike : bool
        /// The general (base-type and interface) constraints declared on this parameter,
        /// from the GenericParamConstraint table (ECMA-335 §II.22.21).
        /// Each entry is the constraint type as a TypeDefn, decoded relative to
        /// the assembly that owns the parameter.
        ///
        /// Note: this captures only the "must be assignable to" constraints. The
        /// "struct" / "class" / "new()" flag-style constraints continue to live in
        /// the <see cref="Constraint"/> and <see cref="RequiresParameterlessConstructor"/>
        /// fields above.
        Constraints : TypeDefn ImmutableArray
    }

/// <summary>
/// Represents a generic type or method parameter definition.
/// Corresponds to GenericParameter in System.Reflection.Metadata.
/// </summary>
type GenericParameter =
    {
        /// <summary>The name of the generic parameter (e.g., 'T', 'TKey', etc.).</summary>
        Name : string

        /// <summary>
        /// The zero-based index of the generic parameter in the generic parameter list.
        /// For example, in Dictionary&lt;TKey, TValue&rt;, TKey has index 0 and TValue has index 1.
        /// </summary>
        SequenceNumber : int

        /// <summary>
        /// The metadata handle identifying this parameter's row in the GenericParam table
        /// (ECMA-335 §II.22.20). Meaningful only relative to the assembly whose
        /// MetadataReader produced it.
        /// </summary>
        Handle : ComparableGenericParameterHandle
    }

type GenericParamFromMetadata = GenericParameter * GenericParamMetadata

[<RequireQualifiedAccess>]
module GenericParameter =
    /// <c>gpAllowByRefLike</c> (corhdr.h:845), the metadata flag behind <c>allows ref struct</c>.
    /// Spelled as a literal rather than <c>GenericParameterAttributes.AllowByRefLike</c> because
    /// that enum member was added in .NET 9 and this project targets net8.0.
    let private allowByRefLikeFlag : GenericParameterAttributes =
        LanguagePrimitives.EnumOfValue 0x0020

    /// Decode a single constraint target (a TypeDef, TypeRef, or TypeSpec entity
    /// handle from the GenericParamConstraint table) into a TypeDefn.
    ///
    /// TypeSpec targets are decoded through the normal signature decoder so that
    /// e.g. <c>where T : IEnumerable&lt;int&gt;</c> becomes a
    /// <see cref="TypeDefn.GenericInstantiation"/>. TypeDef and TypeRef targets
    /// are top-level entity handles that are not embedded in a signature blob, so
    /// we construct the TypeDefn directly. Constraint targets are restricted by
    /// the metadata model (ECMA-335 §II.10.1.7) to non-final classes and
    /// interfaces, so SignatureTypeKind.Class is always the correct kind.
    let private decodeConstraintType
        (assemblyName : AssemblyName)
        (metadata : MetadataReader)
        (handle : EntityHandle)
        : TypeDefn
        =
        match handle.Kind with
        | HandleKind.TypeDefinition ->
            let typeDefHandle = TypeDefinitionHandle.op_Explicit handle

            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition assemblyName typeDefHandle,
                SignatureTypeKind.Class
            )
        | HandleKind.TypeReference ->
            let typeRefHandle = TypeReferenceHandle.op_Explicit handle
            let typeRef = TypeRef.make metadata typeRefHandle
            TypeDefn.FromReference (typeRef, SignatureTypeKind.Class)
        | HandleKind.TypeSpecification ->
            let typeSpecHandle = TypeSpecificationHandle.op_Explicit handle
            let typeSpec = metadata.GetTypeSpecification typeSpecHandle
            typeSpec.DecodeSignature (TypeDefn.typeProvider assemblyName, ())
        | other -> failwith $"Unexpected GenericParamConstraint Type entity kind: %O{other}"

    /// True if this constraint row is the synthetic <c>System.ValueType</c>
    /// reference Roslyn (and other compilers) emit alongside the
    /// <c>NotNullableValueTypeConstraint</c> flag for <c>where T : struct</c>
    /// (and <c>where T : unmanaged</c>). Such a row is fully redundant with
    /// the flag, so we exclude it from <see cref="GenericParamMetadata.Constraints"/>
    /// to keep the contract that flag-style constraints live only in the
    /// flag-derived fields.
    let private isSyntheticValueTypeConstraint (metadata : MetadataReader) (handle : EntityHandle) : bool =
        match handle.Kind with
        | HandleKind.TypeReference ->
            let typeRef = metadata.GetTypeReference (TypeReferenceHandle.op_Explicit handle)

            metadata.GetString typeRef.Namespace = "System"
            && metadata.GetString typeRef.Name = "ValueType"
        | HandleKind.TypeDefinition ->
            // A direct TypeDef constraint to System.ValueType only happens
            // when reading the corelib itself; it is still synthetic next to
            // the value-type flag.
            let typeDef = metadata.GetTypeDefinition (TypeDefinitionHandle.op_Explicit handle)

            metadata.GetString typeDef.Namespace = "System"
            && metadata.GetString typeDef.Name = "ValueType"
        | _ -> false

    let readAll
        (assemblyName : AssemblyName)
        (metadata : MetadataReader)
        (param : GenericParameterHandleCollection)
        : GenericParamFromMetadata ImmutableArray
        =
        param
        |> Seq.map (fun paramHandle ->
            let param = metadata.GetGenericParameter paramHandle

            let requiresParamlessCons =
                param.Attributes.HasFlag GenericParameterAttributes.DefaultConstructorConstraint

            let constr =
                if param.Attributes.HasFlag GenericParameterAttributes.NotNullableValueTypeConstraint then
                    Some GenericConstraint.NonNullableValue
                elif param.Attributes.HasFlag GenericParameterAttributes.ReferenceTypeConstraint then
                    Some GenericConstraint.Reference
                else
                    None

            let variance =
                if param.Attributes.HasFlag GenericParameterAttributes.Contravariant then
                    Some GenericVariance.Contravariant
                elif param.Attributes.HasFlag GenericParameterAttributes.Covariant then
                    Some GenericVariance.Covariant
                else
                    None

            let isValueTypeFlagged =
                param.Attributes.HasFlag GenericParameterAttributes.NotNullableValueTypeConstraint

            let constraints =
                let builder = ImmutableArray.CreateBuilder ()

                for handle in param.GetConstraints () do
                    let constr = metadata.GetGenericParameterConstraint handle

                    if not (isValueTypeFlagged && isSyntheticValueTypeConstraint metadata constr.Type) then
                        builder.Add (decodeConstraintType assemblyName metadata constr.Type)

                builder.ToImmutable ()

            let md =
                {
                    Variance = variance
                    Constraint = constr
                    RequiresParameterlessConstructor = requiresParamlessCons
                    AllowsByRefLike = param.Attributes.HasFlag allowByRefLikeFlag
                    Constraints = constraints
                }

            let p =
                {
                    Name = metadata.GetString param.Name
                    SequenceNumber = param.Index
                    Handle = ComparableGenericParameterHandle.Make paramHandle
                }

            p, md
        )
        |> ImmutableArray.CreateRange
