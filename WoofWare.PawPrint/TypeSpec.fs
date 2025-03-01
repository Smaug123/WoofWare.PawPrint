namespace WoofWare.PawPrint

open System.Reflection.Metadata

type TypeSpec =
    {
        Handle : TypeSpecificationHandle
        Signature : TypeDefn
    }

[<RequireQualifiedAccess>]
module TypeSpec =
    let make (handle : TypeSpecificationHandle) (r : TypeSpecification) : TypeSpec =
        let spec = r.DecodeSignature (TypeDefn.typeProvider, ())

        {
            Handle = handle
            Signature = spec
        }
