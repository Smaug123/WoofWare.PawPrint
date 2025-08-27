namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

/// <summary>
/// Represents detailed information about a field in a .NET assembly.
/// This is a strongly-typed representation of FieldDefinition from System.Reflection.Metadata.
/// </summary>
type FieldInfo<'typeGeneric, 'fieldGeneric> =
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

        /// Static fields don't have an offset at all; also, instance fields which don't have an explicit offset (but
        /// which of course do have one implicitly, which is most fields) are None here.
        Offset : int option
    }

    member this.HasFieldRVA = this.Attributes.HasFlag FieldAttributes.HasFieldRVA
    member this.IsStatic = this.Attributes.HasFlag FieldAttributes.Static

    override this.ToString () : string =
        $"%s{this.DeclaringType.Assembly.Name}.{this.DeclaringType.Name}.%s{this.Name}"

[<RequireQualifiedAccess>]
module FieldInfo =
    let make
        (mr : MetadataReader)
        (assembly : AssemblyName)
        (handle : FieldDefinitionHandle)
        (def : FieldDefinition)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        let name = mr.GetString def.Name
        let fieldSig = def.DecodeSignature (TypeDefn.typeProvider assembly, ())
        let declaringType = def.GetDeclaringType ()

        let decType = mr.GetTypeDefinition declaringType

        let typeGenerics = decType.GetGenericParameters () |> GenericParameter.readAll mr

        let declaringTypeNamespace = mr.GetString decType.Namespace
        let declaringTypeName = mr.GetString decType.Name

        let declaringType =
            ConcreteType.make assembly declaringType declaringTypeNamespace declaringTypeName typeGenerics

        let offset =
            match def.GetOffset () with
            | -1 -> None
            | s -> Some s

        {
            Name = name
            Signature = fieldSig
            DeclaringType = declaringType
            Handle = handle
            Attributes = def.Attributes
            Offset = offset
        }

    let mapTypeGenerics<'a, 'b, 'field> (f : int -> 'a -> 'b) (input : FieldInfo<'a, 'field>) : FieldInfo<'b, 'field> =
        let declaringType = input.DeclaringType |> ConcreteType.mapGeneric f

        {
            Handle = input.Handle
            Name = input.Name
            DeclaringType = declaringType
            Signature = input.Signature
            Attributes = input.Attributes
            Offset = input.Offset
        }
