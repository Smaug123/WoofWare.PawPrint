namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

type EventDefn =
    {
        /// The metadata handle identifying this event's row in the Event table
        /// (ECMA-335 §II.22.13). Meaningful only relative to the assembly whose
        /// MetadataReader produced it.
        Handle : EventDefinitionHandle
        Name : string
        Attrs : EventAttributes
    }

[<RequireQualifiedAccess>]
module EventDefn =

    let make (mr : MetadataReader) (handle : EventDefinitionHandle) (event : EventDefinition) : EventDefn =
        let name = mr.GetString event.Name

        {
            Handle = handle
            Name = name
            Attrs = event.Attributes
        }
