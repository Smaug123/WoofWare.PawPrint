namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

type EventDefn =
    {
        Name : string
        Attrs : EventAttributes
    }

[<RequireQualifiedAccess>]
module EventDefn =

    let make (mr : MetadataReader) (event : EventDefinition) : EventDefn =
        let name = mr.GetString event.Name

        {
            Name = name
            Attrs = event.Attributes
        }
