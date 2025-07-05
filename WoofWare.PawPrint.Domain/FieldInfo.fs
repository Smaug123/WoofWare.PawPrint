namespace WoofWare.PawPrint

open System
open System.Reflection
open System.Reflection.Metadata

/// <summary>
/// Represents detailed information about a field in a .NET assembly.
/// This is a strongly-typed representation of FieldDefinition from System.Reflection.Metadata.
/// </summary>
type FieldInfo<'typeGeneric, 'fieldGeneric when 'typeGeneric : comparison and 'typeGeneric :> IComparable<'typeGeneric>>
    =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this field in the assembly.
        /// </summary>
        Handle : FieldDefinitionHandle

        /// <summary>The name of the field.</summary>
        Name : string

        /// <summary>
        /// The type that declares this field.
        /// </summary>
        DeclaringType : ConcreteType<'typeGeneric>

        /// <summary>
        /// The type of the field.
        /// </summary>
        Signature : 'fieldGeneric

        /// <summary>
        /// The attributes applied to this field, including visibility, static/instance,
        /// literal, and other characteristics.
        /// </summary>
        Attributes : FieldAttributes
    }

    member this.HasFieldRVA = this.Attributes.HasFlag FieldAttributes.HasFieldRVA

    override this.ToString () : string =
        $"%s{this.DeclaringType.Assembly.Name}.{this.DeclaringType.Name}.%s{this.Name}"

[<RequireQualifiedAccess>]
module FieldInfo =
    let make
        (mr : MetadataReader)
        (assembly : AssemblyName)
        (handle : FieldDefinitionHandle)
        (def : FieldDefinition)
        : FieldInfo<FakeUnit, TypeDefn>
        =
        let name = mr.GetString def.Name
        let fieldSig = def.DecodeSignature (TypeDefn.typeProvider assembly, ())
        let declaringType = def.GetDeclaringType ()
        let typeGenerics = mr.GetTypeDefinition(declaringType).GetGenericParameters().Count
        let decType = mr.GetTypeDefinition declaringType
        let declaringTypeNamespace = mr.GetString decType.Namespace
        let declaringTypeName = mr.GetString decType.Name

        let declaringType =
            ConcreteType.make' assembly declaringType declaringTypeNamespace declaringTypeName typeGenerics

        {
            Name = name
            Signature = fieldSig
            DeclaringType = declaringType
            Handle = handle
            Attributes = def.Attributes
        }

    let mapTypeGenerics<'a, 'b, 'field
        when 'a :> IComparable<'a> and 'a : comparison and 'b :> IComparable<'b> and 'b : comparison>
        (f : int -> 'a -> 'b)
        (input : FieldInfo<'a, 'field>)
        : FieldInfo<'b, 'field>
        =
        let declaringType = input.DeclaringType |> ConcreteType.mapGeneric f

        {
            Handle = input.Handle
            Name = input.Name
            DeclaringType = declaringType
            Signature = input.Signature
            Attributes = input.Attributes

        }
