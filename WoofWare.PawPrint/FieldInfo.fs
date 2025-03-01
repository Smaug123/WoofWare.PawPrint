namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// <summary>
/// Represents detailed information about a field in a .NET assembly.
/// This is a strongly-typed representation of FieldDefinition from System.Reflection.Metadata.
/// </summary>
type FieldInfo =
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
        DeclaringType : TypeDefinitionHandle

        /// <summary>
        /// The type of the field.
        /// </summary>
        Signature : TypeDefn

        /// <summary>
        /// The attributes applied to this field, including visibility, static/instance,
        /// literal, and other characteristics.
        /// </summary>
        Attributes : System.Reflection.FieldAttributes
    }

[<RequireQualifiedAccess>]
module FieldInfo =
    let make (getString : StringHandle -> string) (handle : FieldDefinitionHandle) (def : FieldDefinition) : FieldInfo =
        let name = getString def.Name
        let fieldSig = def.DecodeSignature (TypeDefn.typeProvider, ())
        let declaringType = def.GetDeclaringType ()

        {
            Name = name
            Signature = fieldSig
            DeclaringType = declaringType
            Handle = handle
            Attributes = def.Attributes
        }
