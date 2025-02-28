namespace WoofWare.PawPrint

open System.Reflection.Metadata

type CustomAttribute =
    {
        Handle : CustomAttributeHandle
        Constructor : MetadataToken
    }

[<RequireQualifiedAccess>]
module CustomAttribute =
    let make (handle : CustomAttributeHandle) (attr : System.Reflection.Metadata.CustomAttribute) : CustomAttribute =
        let ctor = attr.Constructor |> MetadataToken.ofEntityHandle

        {
            Handle = handle
            Constructor = ctor
        }
