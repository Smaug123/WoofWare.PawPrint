namespace WoofWare.PawPrint

open System.Reflection.Metadata

type FieldInfo =
    {
        Handle : FieldDefinitionHandle
        Name : string
        DeclaringType : TypeDefinitionHandle
        Signature : TypeDefn
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
