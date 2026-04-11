namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.CodeAnalysis
open WoofWare.PawPrint

module TypeIdentityTestHelpers =

    type NoAssemblyLoad () =
        interface IAssemblyLoad with
            member _.LoadAssembly _loadedAssemblies _referencedIn _handle =
                failwith "Test unexpectedly attempted to load an assembly"

    type RecordingAssemblyLoad (availableAssemblies : ImmutableDictionary<string, DumpedAssembly>) =
        let calls = ResizeArray<string * string> ()

        member _.Calls : (string * string) list = calls |> Seq.toList

        interface IAssemblyLoad with
            member _.LoadAssembly
                (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
                (referencedIn : System.Reflection.AssemblyName)
                (handle : AssemblyReferenceHandle)
                : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly
                =
                let referencedInAssembly =
                    match loadedAssemblies.TryGetValue referencedIn.FullName with
                    | false, _ -> failwithf "Missing loaded assembly %s" referencedIn.FullName
                    | true, assy -> assy

                let assemblyRef =
                    match referencedInAssembly.AssemblyReferences.TryGetValue handle with
                    | false, _ -> failwithf "Missing assembly reference handle %A in %s" handle referencedIn.FullName
                    | true, assyRef -> assyRef

                let targetAssembly =
                    match availableAssemblies.TryGetValue assemblyRef.Name.FullName with
                    | false, _ -> failwithf "Missing available assembly %s" assemblyRef.Name.FullName
                    | true, assy -> assy

                calls.Add (referencedIn.FullName, targetAssembly.Name.FullName)
                loadedAssemblies.SetItem (targetAssembly.Name.FullName, targetAssembly), targetAssembly

    let loggerFactory () = LoggerFactory.makeTest () |> snd

    let dumpedAssembly (path : string option) (bytes : byte[]) : DumpedAssembly =
        use stream = new MemoryStream (bytes)
        global.WoofWare.PawPrint.AssemblyApi.read (loggerFactory ()) path stream

    let compileLibrary
        (assemblyName : string)
        (references : MetadataReference list)
        (sources : string list)
        : byte[]
        =
        Roslyn.compileAssembly assemblyName Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary references sources

    let metadataReferenceFromImage (bytes : byte[]) : MetadataReference = MetadataReference.CreateFromImage bytes

    let loadedAssemblies (assemblies : DumpedAssembly list) : ImmutableDictionary<string, DumpedAssembly> =
        assemblies
        |> Seq.map (fun assy -> KeyValuePair (assy.Name.FullName, assy))
        |> ImmutableDictionary.CreateRange

    let emptyConcretizationContext
        (assemblies : DumpedAssembly list)
        : TypeConcretization.ConcretizationContext<DumpedAssembly>
        =
        {
            TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
            TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
            TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies assemblies
            TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
        }

    let getTopLevelTypeDef
        (assy : DumpedAssembly)
        (``namespace`` : string)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assy.TryGetTopLevelTypeDef ``namespace`` name
        |> Option.defaultWith (fun () -> failwithf "Missing type %s.%s" ``namespace`` name)

    let getNestedTypeDef
        (assy : DumpedAssembly)
        (parent : TypeInfo<_, _>)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assy.TryGetNestedTypeDef parent.TypeDefHandle name
        |> Option.defaultWith (fun () -> failwithf "Missing nested type %s inside %s" name parent.Name)

    let getResolvedIdentity
        (result : TypeResolutionResult)
        : DumpedAssembly * ResolvedTypeIdentity * TypeInfo<TypeDefn, TypeDefn>
        =
        match result with
        | TypeResolutionResult.FirstLoadAssy assyRef ->
            failwithf "Expected a resolved type, but the resolver requested assembly load for %s" assyRef.Name.FullName
        | TypeResolutionResult.Resolved (assy, identity, typeInfo) -> assy, identity, typeInfo

    let findTypeRef (predicate : TypeRef -> bool) (assy : DumpedAssembly) : TypeRef =
        assy.TypeRefs |> Seq.map _.Value |> Seq.filter predicate |> Seq.exactlyOne

    let findExportedType (predicate : ExportedType -> bool) (assy : DumpedAssembly) : ExportedType =
        assy.ExportedTypes |> Seq.map _.Value |> Seq.filter predicate |> Seq.exactlyOne

    let getOnlyNestedTypeDef
        (assy : DumpedAssembly)
        (parent : TypeInfo<_, _>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assy.TypeDefs.Values
        |> Seq.filter (fun ty -> ty.IsNested && ty.DeclaringType = parent.TypeDefHandle)
        |> Seq.exactlyOne

    let findAssemblyReferenceHandle
        (targetAssemblyFullName : string)
        (assy : DumpedAssembly)
        : AssemblyReferenceHandle
        =
        assy.AssemblyReferences
        |> Seq.choose (fun (KeyValue (handle, assemblyRef)) ->
            if assemblyRef.Name.FullName = targetAssemblyFullName then
                Some handle
            else
                None
        )
        |> Seq.exactlyOne

    let synthesizeTopLevelForwarderExport
        (``namespace`` : string)
        (name : string)
        (targetAssemblyFullName : string)
        (forwarder : DumpedAssembly)
        : ExportedType * DumpedAssembly
        =
        let exportHandle =
            System.Reflection.Metadata.Ecma335.MetadataTokens.ExportedTypeHandle (forwarder.ExportedTypes.Count + 1000)

        let export =
            {
                Handle = exportHandle
                Name = name
                Namespace = Some ``namespace``
                NamespaceDefn = Unchecked.defaultof<NamespaceDefinitionHandle>
                TypeAttrs = System.Reflection.TypeAttributes.Public
                Data = ExportedTypeData.ForwardsTo (findAssemblyReferenceHandle targetAssemblyFullName forwarder)
            }

        let updatedForwarder =
            { forwarder with
                ExportedTypes = forwarder.ExportedTypes.Add (exportHandle, export)
                _TopLevelExportedTypesLookup =
                    forwarder._TopLevelExportedTypesLookup.Add ((Some ``namespace``, name), export)
            }

        export, updatedForwarder

    let getOrSynthesizeNestedExportedType
        (parentExport : ExportedType)
        (nestedName : string)
        (forwarder : DumpedAssembly)
        : ExportedType * DumpedAssembly
        =
        match forwarder.TryGetNestedExportedType parentExport.Handle nestedName with
        | Some nestedExport -> nestedExport, forwarder
        | None ->
            let nestedHandle =
                System.Reflection.Metadata.Ecma335.MetadataTokens.ExportedTypeHandle (
                    forwarder.ExportedTypes.Count + 1000
                )

            let nestedExport =
                {
                    Handle = nestedHandle
                    Name = nestedName
                    Namespace = None
                    NamespaceDefn = Unchecked.defaultof<NamespaceDefinitionHandle>
                    TypeAttrs = System.Reflection.TypeAttributes.NestedPublic
                    Data = ExportedTypeData.ParentExportedType parentExport.Handle
                }

            let updatedForwarder =
                { forwarder with
                    ExportedTypes = forwarder.ExportedTypes.Add (nestedHandle, nestedExport)
                    _NestedExportedTypesLookup =
                        forwarder._NestedExportedTypesLookup.Add (
                            (parentExport.Handle, nestedExport.Name),
                            nestedExport
                        )
                }

            nestedExport, updatedForwarder
