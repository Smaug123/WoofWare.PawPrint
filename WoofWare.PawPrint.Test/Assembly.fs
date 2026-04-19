namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Assembly =

    let getEmbeddedResource (name : string) (assy : Assembly) : Stream =
        let suffix = "." + name

        let resourceName =
            assy.GetManifestResourceNames ()
            |> Seq.filter (fun a -> a.EndsWith (suffix, StringComparison.Ordinal))
            |> Seq.exactlyOne

        assy.GetManifestResourceStream resourceName

    let getEmbeddedResourceAsString (name : string) (assy : Assembly) : string =
        use stream = getEmbeddedResource name assy
        use reader = new StreamReader (stream, leaveOpen = true)
        reader.ReadToEnd ()
