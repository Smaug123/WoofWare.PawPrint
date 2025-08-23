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
    let readAll
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

            let md =
                {
                    Variance = variance
                    Constraint = constr
                    RequiresParameterlessConstructor = requiresParamlessCons
                }

            let p =
                {
                    Name = metadata.GetString param.Name
                    SequenceNumber = param.Index
                }

            p, md
        )
        |> ImmutableArray.CreateRange
