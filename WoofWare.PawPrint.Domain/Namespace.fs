namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

type Namespace =
    {
        PrettyName : string
        Parent : NamespaceDefinitionHandle
        TypeDefinitions : ImmutableArray<TypeDefinitionHandle>
        ExportedTypes : ImmutableArray<ExportedTypeHandle>
    }

[<RequireQualifiedAccess>]
module Namespace =
    /// Returns also the children.
    let make
        (getString : StringHandle -> string)
        (getNamespace : NamespaceDefinitionHandle -> NamespaceDefinition)
        (ns : NamespaceDefinition)
        : Namespace * ImmutableDictionary<string list, Namespace>
        =
        let children = ImmutableDictionary.CreateBuilder ()

        let rec inner (path : string list) (ns : NamespaceDefinition) : Namespace =
            for child in ns.NamespaceDefinitions do
                let rendered = getNamespace child
                let location = getString rendered.Name :: path
                children.Add (List.rev location, inner location rendered)

            {
                PrettyName = getString ns.Name
                Parent = ns.Parent
                TypeDefinitions = ns.TypeDefinitions
                ExportedTypes = ns.ExportedTypes
            }

        let result = inner [] ns
        result, children.ToImmutable ()
