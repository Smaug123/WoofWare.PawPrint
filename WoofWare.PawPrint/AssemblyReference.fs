namespace WoofWare.PawPrint

open System
open System.Reflection

type AssemblyReference =
    {
        Culture : StringToken
        Flags : AssemblyFlags
        Name : AssemblyName
        Version : Version
    }

[<RequireQualifiedAccess>]
module AssemblyReference =
    let make (ref : System.Reflection.Metadata.AssemblyReference) : AssemblyReference =
        {
            Culture = StringToken.String ref.Culture
            Flags = ref.Flags
            Name = ref.GetAssemblyName ()
            Version = ref.Version
        }
