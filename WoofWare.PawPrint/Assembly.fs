namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.FSharp.Core

type DumpedAssembly =
    {
        Types : TypeInfo list
        Methods : IReadOnlyDictionary<MethodDefinitionHandle, MethodInfo>
        MainMethod : MethodDefinitionHandle
        /// Map of four-byte int token to metadata
        MethodDefinitions : Map<int, MethodDefinition>
        MethodSpecs : ImmutableDictionary<MethodSpecificationHandle, MethodSpecification>
    }

[<RequireQualifiedAccess>]
module Assembly =
    let read (dllBytes : Stream) : DumpedAssembly =
        use peReader = new PEReader (dllBytes)
        let metadataReader = peReader.GetMetadataReader ()

        let entryPoint =
            peReader.PEHeaders.CorHeader.EntryPointTokenOrRelativeVirtualAddress
            |> fun x -> if x = 0 then failwith "No entry point" else x

        let entryPointMethod = MetadataTokens.MethodDefinitionHandle entryPoint

        let result =
            metadataReader.TypeDefinitions
            |> Seq.map (TypeInfo.read peReader metadataReader)
            |> Seq.toList

        let methods =
            result
            |> List.collect (fun ty -> ty.Methods |> List.map (fun mi -> KeyValuePair (mi.Handle, mi)))
            |> ImmutableDictionary.CreateRange

        let methodDefnMetadata =
            metadataReader.MethodDefinitions
            |> Seq.map (fun mh ->
                let def = metadataReader.GetMethodDefinition mh
                let eh : EntityHandle = MethodDefinitionHandle.op_Implicit mh
                let token = MetadataTokens.GetToken eh
                token, def
            )
            |> Map.ofSeq

        let methodSpecs =
            Seq.init
                (metadataReader.GetTableRowCount TableIndex.MethodSpec)
                (fun i ->
                    let i = i + 1
                    let handle = MetadataTokens.MethodSpecificationHandle i
                    KeyValuePair (handle, metadataReader.GetMethodSpecification handle)
                )
            |> ImmutableDictionary.CreateRange

        {
            Types = result
            MainMethod = entryPointMethod
            Methods = methods
            MethodDefinitions = methodDefnMetadata
            MethodSpecs = methodSpecs
        }

    let print (main : MethodDefinitionHandle) (dumped : DumpedAssembly) : unit =
        for typ in dumped.Types do
            printfn "\nType: %s.%s" typ.Namespace typ.Name

            for method in typ.Methods do
                if method.Handle = main then
                    printfn "Entry point!"

                printfn "\nMethod: %s" method.Name

                method.Instructions
                |> List.map (fun (op, index) -> IlOp.Format op index)
                |> List.iter Console.WriteLine
