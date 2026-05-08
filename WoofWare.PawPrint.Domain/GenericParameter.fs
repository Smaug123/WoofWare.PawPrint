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
    }

type GenericParamFromMetadata = GenericParameter * GenericParamMetadata

[<RequireQualifiedAccess>]
module GenericParameter =
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

    let readAll
        (assemblyName : AssemblyName)
        (metadata : MetadataReader)
        (param : GenericParameterHandleCollection)
        : GenericParamFromMetadata ImmutableArray
        =
        param
        |> Seq.map (fun param ->
            let param = metadata.GetGenericParameter param

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

            let constraints =
                let builder = ImmutableArray.CreateBuilder ()

                for handle in param.GetConstraints () do
                    let constr = metadata.GetGenericParameterConstraint handle
                    builder.Add (decodeConstraintType assemblyName metadata constr.Type)

                builder.ToImmutable ()

            let md =
                {
                    Variance = variance
                    Constraint = constr
                    RequiresParameterlessConstructor = requiresParamlessCons
                    Constraints = constraints
                }

            let p =
                {
                    Name = metadata.GetString param.Name
                    SequenceNumber = param.Index
                }

            p, md
        )
        |> ImmutableArray.CreateRange
