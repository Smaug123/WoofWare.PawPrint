namespace WoofWare.PawPrint

open System
open System.Reflection
open System.Reflection.Metadata

type AssemblyReference =
    {
        Handle : AssemblyReferenceHandle
        Culture : StringToken
        Flags : AssemblyFlags
        Name : AssemblyName
        Version : Version
    }

[<RequireQualifiedAccess>]
module AssemblyReference =
    let make (handle : AssemblyReferenceHandle) (ref : System.Reflection.Metadata.AssemblyReference) : AssemblyReference =
        {
            Handle = handle
            Culture = StringToken.String ref.Culture
            Flags = ref.Flags
            Name = ref.GetAssemblyName ()
            Version = ref.Version
        }
