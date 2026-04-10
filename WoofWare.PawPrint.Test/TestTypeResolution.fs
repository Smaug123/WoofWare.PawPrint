namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Collections.Generic
open System.IO
open System.Reflection.Metadata
open Microsoft.CodeAnalysis
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestTypeResolution =

    type private NoAssemblyLoad () =
        interface IAssemblyLoad with
            member _.LoadAssembly _loadedAssemblies _referencedIn _handle =
                failwith "Test unexpectedly attempted to load an assembly"

    type private RecordingAssemblyLoad (availableAssemblies : ImmutableDictionary<string, DumpedAssembly>) =
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
                    | false, _ ->
                        failwithf
                            "Missing assembly reference handle %A in %s"
                            handle
                            referencedIn.FullName
                    | true, assyRef -> assyRef

                let targetAssembly =
                    match availableAssemblies.TryGetValue assemblyRef.Name.FullName with
                    | false, _ -> failwithf "Missing available assembly %s" assemblyRef.Name.FullName
                    | true, assy -> assy

                calls.Add (referencedIn.FullName, targetAssembly.Name.FullName)
                loadedAssemblies.SetItem (targetAssembly.Name.FullName, targetAssembly), targetAssembly

    let private loggerFactory () = LoggerFactory.makeTest () |> snd

    let private dumpedAssembly (path : string option) (bytes : byte[]) : DumpedAssembly =
        use stream = new MemoryStream (bytes)
        global.WoofWare.PawPrint.AssemblyApi.read (loggerFactory ()) path stream

    let private compileLibrary
        (assemblyName : string)
        (references : MetadataReference list)
        (sources : string list)
        : byte[]
        =
        Roslyn.compileAssembly assemblyName OutputKind.DynamicallyLinkedLibrary references sources

    let private metadataReferenceFromImage (bytes : byte[]) : MetadataReference =
        MetadataReference.CreateFromImage bytes

    let private loadedAssemblies (assemblies : DumpedAssembly list) =
        assemblies
        |> Seq.map (fun assy -> KeyValuePair (assy.Name.FullName, assy))
        |> ImmutableDictionary.CreateRange

    let private emptyConcretizationContext
        (assemblies : DumpedAssembly list)
        : TypeConcretization.ConcretizationContext<DumpedAssembly>
        =
        {
            TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
            TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
            TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies assemblies
            TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
        }

    let private getTopLevelTypeDef (assy : DumpedAssembly) (ns : string) (name : string) =
        assy.TryGetTopLevelTypeDef ns name
        |> Option.defaultWith (fun () -> failwithf "Missing type %s.%s" ns name)

    let private getNestedTypeDef (assy : DumpedAssembly) (parent : TypeInfo<_, _>) (name : string) =
        assy.TryGetNestedTypeDef parent.TypeDefHandle name
        |> Option.defaultWith (fun () -> failwithf "Missing nested type %s inside %s" name parent.Name)

    let private getResolvedIdentity
        (result : TypeResolutionResult)
        : DumpedAssembly * ResolvedTypeIdentity * TypeInfo<TypeDefn, TypeDefn>
        =
        match result with
        | TypeResolutionResult.FirstLoadAssy assyRef ->
            failwithf "Expected a resolved type, but the resolver requested assembly load for %s" assyRef.Name.FullName
        | TypeResolutionResult.Resolved (assy, identity, typeInfo) -> assy, identity, typeInfo

    let private findTypeRef (predicate : TypeRef -> bool) (assy : DumpedAssembly) =
        assy.TypeRefs |> Seq.map _.Value |> Seq.filter predicate |> Seq.exactlyOne

    let private findExportedType (predicate : ExportedType -> bool) (assy : DumpedAssembly) =
        assy.ExportedTypes |> Seq.map _.Value |> Seq.filter predicate |> Seq.exactlyOne

    let private getOnlyNestedTypeDef (assy : DumpedAssembly) (parent : TypeInfo<_, _>) =
        assy.TypeDefs.Values
        |> Seq.filter (fun ty -> ty.IsNested && ty.DeclaringType = parent.TypeDefHandle)
        |> Seq.exactlyOne

    let private findAssemblyReferenceHandle
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

    let private synthesizeTopLevelForwarderExport
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

    let private getOrSynthesizeNestedExportedType
        (parentExport : ExportedType)
        (nestedName : string)
        (forwarder : DumpedAssembly)
        : ExportedType * DumpedAssembly
        =
        match forwarder.TryGetNestedExportedType parentExport.Handle nestedName with
        | Some nestedExport -> nestedExport, forwarder
        | None ->
            let nestedHandle =
                System.Reflection.Metadata.Ecma335.MetadataTokens.ExportedTypeHandle
                    (forwarder.ExportedTypes.Count + 1000)

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
                        forwarder._NestedExportedTypesLookup.Add ((parentExport.Handle, nestedExport.Name), nestedExport)
                }

            nestedExport, updatedForwarder

    [<Test>]
    let ``nested type refs across assemblies resolve through the TypeRef parent chain`` () =
        let definingBytes =
            compileLibrary
                "NestedIdentity.Defining"
                []
                [
                    """
namespace N;
public class Outer
{
    public class Inner { }
}
"""
                ]

        let consumerBytes =
            compileLibrary
                "NestedIdentity.Consumer"
                [ metadataReferenceFromImage definingBytes ]
                [
                    """
using N;
public class Consumer
{
    private Outer.Inner _field = new Outer.Inner();
}
"""
                ]

        let defining = dumpedAssembly (Some "NestedIdentity.Defining.dll") definingBytes
        let consumer = dumpedAssembly (Some "NestedIdentity.Consumer.dll") consumerBytes
        let assemblies = loadedAssemblies [ defining ; consumer ]

        let innerRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = "Inner"
                    && typeRef.Namespace = ""
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.TypeRef _ -> true
                       | _ -> false
                )
                consumer

        let resolvedAssembly, identity, resolvedType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual defining.Name.FullName
        resolvedType.Name |> shouldEqual "Inner"

        global.WoofWare.PawPrint.AssemblyApi.fullName resolvedAssembly identity
        |> shouldEqual "N.Outer.Inner"

        let outer = getTopLevelTypeDef defining "N" "Outer"
        let inner = getNestedTypeDef defining outer "Inner"

        identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition defining.Name inner.TypeDefHandle)

    [<Test>]
    let ``concretizing nested type refs can lazy-load when the outer resolution scope is TypeRef`` () =
        let definingBytes =
            compileLibrary
                "NestedIdentity.Concretize.Defining"
                []
                [
                    """
namespace N;
public class Outer
{
    public class Inner { }
}
"""
                ]

        let consumerBytes =
            compileLibrary
                "NestedIdentity.Concretize.Consumer"
                [ metadataReferenceFromImage definingBytes ]
                [
                    """
using N;
public class Consumer
{
    private Outer.Inner _field = new Outer.Inner();
}
"""
                ]

        let defining = dumpedAssembly (Some "NestedIdentity.Concretize.Defining.dll") definingBytes
        let consumer = dumpedAssembly (Some "NestedIdentity.Concretize.Consumer.dll") consumerBytes
        let loader = RecordingAssemblyLoad (loadedAssemblies [ consumer ; defining ])

        let innerRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = "Inner"
                    && typeRef.Namespace = ""
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.TypeRef _ -> true
                       | _ -> false
                )
                consumer

        let innerDefn = TypeDefn.FromReference (innerRef, SignatureTypeKind.Class)

        let handle, ctx =
            TypeConcretization.concretizeType
                (emptyConcretizationContext [ consumer ])
                (loader :> IAssemblyLoad)
                consumer.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                innerDefn

        loader.Calls |> shouldEqual [ (consumer.Name.FullName, defining.Name.FullName) ]

        let outer = getTopLevelTypeDef defining "N" "Outer"
        let inner = getNestedTypeDef defining outer "Inner"

        let concretizedType =
            AllConcreteTypes.lookup handle ctx.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "Expected concretized nested type to exist")

        concretizedType.Identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition defining.Name inner.TypeDefHandle)

    [<Test>]
    let ``top-level and nested types with the same simple name remain distinct`` () =
        let definingBytes =
            compileLibrary
                "TypeIdentity.Collision.Defining"
                []
                [
                    """
namespace N;
public class Inner { }
public class Outer
{
    public class Inner { }
}
"""
                ]

        let consumerBytes =
            compileLibrary
                "TypeIdentity.Collision.Consumer"
                [ metadataReferenceFromImage definingBytes ]
                [
                    """
using N;
public class Consumer
{
    private Inner _topLevel = new Inner();
    private Outer.Inner _nested = new Outer.Inner();
}
"""
                ]

        let defining =
            dumpedAssembly (Some "TypeIdentity.Collision.Defining.dll") definingBytes

        let consumer =
            dumpedAssembly (Some "TypeIdentity.Collision.Consumer.dll") consumerBytes

        let assemblies = loadedAssemblies [ defining ; consumer ]

        let topLevelRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = "Inner"
                    && typeRef.Namespace = "N"
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.Assembly _ -> true
                       | _ -> false
                )
                consumer

        let nestedRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = "Inner"
                    && typeRef.Namespace = ""
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.TypeRef _ -> true
                       | _ -> false
                )
                consumer

        let _, topLevelIdentity, _ =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty topLevelRef
            |> getResolvedIdentity

        let _, nestedIdentity, _ =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty nestedRef
            |> getResolvedIdentity

        topLevelIdentity |> shouldNotEqual nestedIdentity

    [<Test>]
    let ``same simple nested names under different parents resolve to distinct identities`` () =
        let definingBytes =
            compileLibrary
                "TypeIdentity.Parents.Defining"
                []
                [
                    """
namespace N;
public class X
{
    public class Inner { }
}
public class Y
{
    public class Inner { }
}
"""
                ]

        let consumerBytes =
            compileLibrary
                "TypeIdentity.Parents.Consumer"
                [ metadataReferenceFromImage definingBytes ]
                [
                    """
using N;
public class Consumer
{
    private X.Inner _x = new X.Inner();
    private Y.Inner _y = new Y.Inner();
}
"""
                ]

        let defining =
            dumpedAssembly (Some "TypeIdentity.Parents.Defining.dll") definingBytes

        let consumer =
            dumpedAssembly (Some "TypeIdentity.Parents.Consumer.dll") consumerBytes

        let assemblies = loadedAssemblies [ defining ; consumer ]

        let nestedRefs =
            consumer.TypeRefs
            |> Seq.map _.Value
            |> Seq.filter (fun typeRef ->
                typeRef.Name = "Inner"
                && typeRef.Namespace = ""
                && match typeRef.ResolutionScope with
                   | TypeRefResolutionScope.TypeRef _ -> true
                   | _ -> false
            )
            |> Seq.toList

        nestedRefs.Length |> shouldEqual 2

        let identities =
            nestedRefs
            |> List.map (fun typeRef ->
                global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty typeRef
                |> getResolvedIdentity
            )
            |> List.map (fun (_, identity, _) -> identity)
            |> Set.ofList

        identities.Count |> shouldEqual 2

    [<Test>]
    let ``ModuleRef resolution fails explicitly`` () =
        let consumer =
            {
                Name = "Inner"
                Namespace = "N"
                ResolutionScope =
                    TypeRefResolutionScope.ModuleRef (
                        System.Reflection.Metadata.Ecma335.MetadataTokens.ModuleReferenceHandle 1
                    )
            }

        let assemblyBytes =
            compileLibrary "ModuleRef.Test" [] [ "public class Placeholder { }" ]

        let dumped = dumpedAssembly (Some "ModuleRef.Test.dll") assemblyBytes
        let assemblies = loadedAssemblies [ dumped ]

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies dumped ImmutableArray.Empty consumer
                |> ignore
            )

        Assert.That (ex.Message, Does.Contain "ModuleRef type resolution is not yet supported for type N.Inner")

    [<Test>]
    let ``forwarded top-level exported types resolve to the target assembly`` () =
        let targetBytes =
            compileLibrary
                "TypeIdentity.Forwarded.Target"
                []
                [
                    """
namespace N;
public class Forwarded { }
"""
                ]

        let forwarderBytes =
            compileLibrary
                "TypeIdentity.Forwarded.Forwarder"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
using System.Runtime.CompilerServices;
using N;
[assembly: TypeForwardedTo(typeof(Forwarded))]
public class Placeholder { }
"""
                ]

        let target = dumpedAssembly (Some "TypeIdentity.Forwarded.Target.dll") targetBytes

        let forwarder =
            dumpedAssembly (Some "TypeIdentity.Forwarded.Forwarder.dll") forwarderBytes

        let assemblies = loadedAssemblies [ target ; forwarder ]

        let exportedType =
            findExportedType
                (fun export ->
                    export.Name = "Forwarded"
                    && export.Namespace = Some "N"
                    && match export.Data with
                       | ExportedTypeData.ForwardsTo _ -> true
                       | _ -> false
                )
                forwarder

        let resolvedAssembly, identity, resolvedType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeFromExport
                forwarder
                assemblies
                ImmutableArray.Empty
                exportedType
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "Forwarded"

        global.WoofWare.PawPrint.AssemblyApi.fullName resolvedAssembly identity
        |> shouldEqual "N.Forwarded"

        let forwarded = getTopLevelTypeDef target "N" "Forwarded"

        identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition target.Name forwarded.TypeDefHandle)

    [<Test>]
    let ``concretizing forwarded top-level type refs retries using the returned assembly reference`` () =
        let targetBytes =
            compileLibrary
                "TypeIdentity.ForwardedConcretize.Target"
                []
                [
                    """
namespace N;
public class Forwarded { }
"""
                ]

        let forwarderBytes =
            compileLibrary
                "TypeIdentity.ForwardedConcretize.Forwarder"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
using System.Runtime.CompilerServices;
using N;
[assembly: TypeForwardedTo(typeof(Forwarded))]
public class Placeholder { }
"""
                ]

        let consumerBytes =
            compileLibrary
                "TypeIdentity.ForwardedConcretize.Consumer"
                [ metadataReferenceFromImage forwarderBytes ]
                [
                    """
using N;
public class Consumer
{
    private Forwarded _field = new Forwarded();
}
"""
                ]

        let target = dumpedAssembly (Some "TypeIdentity.ForwardedConcretize.Target.dll") targetBytes
        let forwarder = dumpedAssembly (Some "TypeIdentity.ForwardedConcretize.Forwarder.dll") forwarderBytes
        let consumer = dumpedAssembly (Some "TypeIdentity.ForwardedConcretize.Consumer.dll") consumerBytes
        let loader = RecordingAssemblyLoad (loadedAssemblies [ consumer ; forwarder ; target ])

        let forwardedRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = "Forwarded"
                    && typeRef.Namespace = "N"
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.Assembly _ -> true
                       | _ -> false
                )
                consumer

        let forwardedDefn = TypeDefn.FromReference (forwardedRef, SignatureTypeKind.Class)

        let handle, ctx =
            TypeConcretization.concretizeType
                (emptyConcretizationContext [ consumer ])
                (loader :> IAssemblyLoad)
                consumer.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                forwardedDefn

        loader.Calls
        |> shouldEqual
            [
                (consumer.Name.FullName, forwarder.Name.FullName)
                (forwarder.Name.FullName, target.Name.FullName)
            ]

        let concretizedType =
            AllConcreteTypes.lookup handle ctx.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "Expected concretized forwarded type to exist")

        let forwarded = getTopLevelTypeDef target "N" "Forwarded"

        concretizedType.Identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition target.Name forwarded.TypeDefHandle)

    [<Test>]
    let ``forwarded top-level exported types resolve transitively through chained forwarders`` () =
        let targetBytes =
            compileLibrary
                "TypeIdentity.ForwardedChain.Target"
                []
                [
                    """
namespace N;
public class Forwarded { }
"""
                ]

        let middleBytes =
            compileLibrary
                "TypeIdentity.ForwardedChain.Middle"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
using System.Runtime.CompilerServices;
using N;
[assembly: TypeForwardedTo(typeof(Forwarded))]
namespace Middle;
public class Placeholder { }
"""
                ]

        let outerBytes =
            compileLibrary
                "TypeIdentity.ForwardedChain.Outer"
                [ metadataReferenceFromImage middleBytes ]
                [
                    """
using Middle;
public class Placeholder
{
    private Middle.Placeholder _field = new Middle.Placeholder();
}
"""
                ]

        let target = dumpedAssembly (Some "TypeIdentity.ForwardedChain.Target.dll") targetBytes
        let middle = dumpedAssembly (Some "TypeIdentity.ForwardedChain.Middle.dll") middleBytes
        let outer = dumpedAssembly (Some "TypeIdentity.ForwardedChain.Outer.dll") outerBytes
        let exportedType, outer = synthesizeTopLevelForwarderExport "N" "Forwarded" middle.Name.FullName outer
        let assemblies = loadedAssemblies [ target ; middle ; outer ]

        let resolvedAssembly, identity, resolvedType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeFromExport
                outer
                assemblies
                ImmutableArray.Empty
                exportedType
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "Forwarded"

        global.WoofWare.PawPrint.AssemblyApi.fullName resolvedAssembly identity
        |> shouldEqual "N.Forwarded"

        let forwarded = getTopLevelTypeDef target "N" "Forwarded"

        identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition target.Name forwarded.TypeDefHandle)

    [<Test>]
    let ``forwarded nested exported types resolve through the exported parent chain when metadata provides it`` () =
        let targetBytes =
            compileLibrary
                "TypeIdentity.ForwardedNested.Target"
                []
                [
                    """
namespace N;
public class Outer
{
    public class Inner { }
}
"""
                ]

        let forwarderBytes =
            compileLibrary
                "TypeIdentity.ForwardedNested.Forwarder"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
	using System.Runtime.CompilerServices;
	using N;
	[assembly: TypeForwardedTo(typeof(Outer))]
	public class Placeholder { }
	"""
                ]

        let target =
            dumpedAssembly (Some "TypeIdentity.ForwardedNested.Target.dll") targetBytes

        let forwarder =
            dumpedAssembly (Some "TypeIdentity.ForwardedNested.Forwarder.dll") forwarderBytes

        let parentExport =
            findExportedType
                (fun export ->
                    export.Name = "Outer"
                    && export.Namespace = Some "N"
                    && match export.Data with
                       | ExportedTypeData.ForwardsTo _ -> true
                       | _ -> false
                )
                forwarder

        let nestedExport, forwarder =
            getOrSynthesizeNestedExportedType parentExport "Inner" forwarder

        let assemblies = loadedAssemblies [ target ; forwarder ]

        let resolvedAssembly, identity, resolvedType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeFromExport
                forwarder
                assemblies
                ImmutableArray.Empty
                nestedExport
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "Inner"

        global.WoofWare.PawPrint.AssemblyApi.fullName resolvedAssembly identity
        |> shouldEqual "N.Outer.Inner"

        let outer = getTopLevelTypeDef target "N" "Outer"
        let inner = getNestedTypeDef target outer "Inner"

        identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition target.Name inner.TypeDefHandle)

    [<Test>]
    let ``IlMachineState exported-type retries preserve nested export context after loading`` () =
        let targetBytes =
            compileLibrary
                "TypeIdentity.ForwardedNested.Runtime.Target"
                []
                [
                    """
namespace N;
public class Outer
{
    public class Inner { }
}
"""
                ]

        let forwarderBytes =
            compileLibrary
                "TypeIdentity.ForwardedNested.Runtime.Forwarder"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
using System.Runtime.CompilerServices;
using N;
[assembly: TypeForwardedTo(typeof(Outer))]
public class Placeholder { }
"""
                ]

        let target =
            dumpedAssembly (Some "TypeIdentity.ForwardedNested.Runtime.Target.dll") targetBytes

        let forwarder =
            dumpedAssembly (Some "TypeIdentity.ForwardedNested.Runtime.Forwarder.dll") forwarderBytes

        let parentExport =
            findExportedType
                (fun export ->
                    export.Name = "Outer"
                    && export.Namespace = Some "N"
                    && match export.Data with
                       | ExportedTypeData.ForwardsTo _ -> true
                       | _ -> false
                )
                forwarder

        let nestedExport, forwarder =
            getOrSynthesizeNestedExportedType parentExport "Inner" forwarder

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore

        let targetPath = Path.Combine (tempDir, target.Name.Name + ".dll")
        File.WriteAllBytes (targetPath, targetBytes)

        let lf = loggerFactory ()

        try
            let state =
                global.WoofWare.PawPrint.IlMachineState.initial
                    lf
                    (ImmutableArray.Create tempDir)
                    forwarder

            let state, resolvedAssembly, resolvedType =
                global.WoofWare.PawPrint.IlMachineState.resolveTypeFromExport
                    lf
                    forwarder
                    nestedExport
                    ImmutableArray.Empty
                    state

            let outer = getTopLevelTypeDef target "N" "Outer"
            let inner = getNestedTypeDef target outer "Inner"

            resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
            resolvedType.TypeDefHandle |> shouldEqual inner.TypeDefHandle

            match state.LoadedAssembly target.Name with
            | Some loaded -> loaded.Name.FullName |> shouldEqual target.Name.FullName
            | None -> failwith "Expected target assembly to be loaded after exported-type retry"
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? System.UnauthorizedAccessException -> ()

    [<Test>]
    let ``concretizing the same nominal type twice is idempotent`` () =
        let definingBytes =
            compileLibrary
                "ConcreteType.Idempotent"
                []
                [
                    """
namespace N;
public class Outer
{
    public class Inner { }
}
"""
                ]

        let defining = dumpedAssembly (Some "ConcreteType.Idempotent.dll") definingBytes
        let outer = getTopLevelTypeDef defining "N" "Outer"
        let inner = getNestedTypeDef defining outer "Inner"

        let ctx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let first, ctx =
            TypeConcretization.concretizeTypeDefinition
                ctx
                defining.Name
                (ComparableTypeDefinitionHandle.Make inner.TypeDefHandle)

        let second, _ =
            TypeConcretization.concretizeTypeDefinition
                ctx
                defining.Name
                (ComparableTypeDefinitionHandle.Make inner.TypeDefHandle)

        first |> shouldEqual second

    [<Test>]
    let ``distinct nested identities produce distinct concrete type handles`` () =
        let definingBytes =
            compileLibrary
                "ConcreteType.Distinct"
                []
                [
                    """
namespace N;
public class X
{
    public class Inner { }
}
public class Y
{
    public class Inner { }
}
"""
                ]

        let defining = dumpedAssembly (Some "ConcreteType.Distinct.dll") definingBytes
        let x = getTopLevelTypeDef defining "N" "X"
        let y = getTopLevelTypeDef defining "N" "Y"
        let xInner = getNestedTypeDef defining x "Inner"
        let yInner = getNestedTypeDef defining y "Inner"

        let ctx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let xHandle, ctx =
            TypeConcretization.concretizeTypeDefinition
                ctx
                defining.Name
                (ComparableTypeDefinitionHandle.Make xInner.TypeDefHandle)

        let yHandle, _ =
            TypeConcretization.concretizeTypeDefinition
                ctx
                defining.Name
                (ComparableTypeDefinitionHandle.Make yInner.TypeDefHandle)

        xHandle |> shouldNotEqual yHandle

    [<Test>]
    let ``generic instantiation uses resolved nominal identity for uniqueness`` () =
        let definingBytes =
            compileLibrary
                "ConcreteType.Generic"
                []
                [
                    """
namespace N;
public class Argument { }
public class Outer
{
    public class Box<T> { }
}
"""
                ]

        let defining = dumpedAssembly (Some "ConcreteType.Generic.dll") definingBytes
        let argument = getTopLevelTypeDef defining "N" "Argument"
        let outer = getTopLevelTypeDef defining "N" "Outer"
        let box = getNestedTypeDef defining outer "Box`1"

        let boxDefn =
            TypeDefn.FromDefinition (
                ComparableTypeDefinitionHandle.Make box.TypeDefHandle,
                defining.Name.FullName,
                SignatureTypeKind.Class
            )

        let argumentDefn =
            TypeDefn.FromDefinition (
                ComparableTypeDefinitionHandle.Make argument.TypeDefHandle,
                defining.Name.FullName,
                SignatureTypeKind.Class
            )

        let genericType =
            TypeDefn.GenericInstantiation (boxDefn, ImmutableArray.Create argumentDefn)

        let ctx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let first, ctx =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                genericType

        let second, _ =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                genericType

        first |> shouldEqual second

    [<Test>]
    let ``enclosing generic arguments propagate into nested concretization`` () =
        let definingBytes =
            compileLibrary
                "ConcreteType.NestedGenericContext"
                []
                [
                    """
namespace N;
public class FirstArgument { }
public class SecondArgument { }
public class Outer<T>
{
    public class Inner
    {
        public T Value;
    }
}
"""
                ]

        let defining =
            dumpedAssembly (Some "ConcreteType.NestedGenericContext.dll") definingBytes

        let firstArgument = getTopLevelTypeDef defining "N" "FirstArgument"
        let secondArgument = getTopLevelTypeDef defining "N" "SecondArgument"
        let outer = getTopLevelTypeDef defining "N" "Outer`1"
        let inner = getOnlyNestedTypeDef defining outer

        inner.Generics.Length |> shouldEqual 1

        inner.Fields
        |> List.exactlyOne
        |> _.Signature
        |> shouldEqual (TypeDefn.GenericTypeParameter 0)

        let firstArgumentDefn =
            TypeDefn.FromDefinition (
                ComparableTypeDefinitionHandle.Make firstArgument.TypeDefHandle,
                defining.Name.FullName,
                SignatureTypeKind.Class
            )

        let secondArgumentDefn =
            TypeDefn.FromDefinition (
                ComparableTypeDefinitionHandle.Make secondArgument.TypeDefHandle,
                defining.Name.FullName,
                SignatureTypeKind.Class
            )

        let innerDefn =
            TypeDefn.FromDefinition (
                ComparableTypeDefinitionHandle.Make inner.TypeDefHandle,
                defining.Name.FullName,
                SignatureTypeKind.Class
            )

        let firstInstantiated =
            TypeDefn.GenericInstantiation (innerDefn, ImmutableArray.Create firstArgumentDefn)

        let secondInstantiated =
            TypeDefn.GenericInstantiation (innerDefn, ImmutableArray.Create secondArgumentDefn)

        let ctx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let firstArgumentHandle, ctx =
            TypeConcretization.concretizeTypeDefinition
                ctx
                defining.Name
                (ComparableTypeDefinitionHandle.Make firstArgument.TypeDefHandle)

        let firstHandle, ctx =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                firstInstantiated

        let repeatedFirstHandle, ctx =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                firstInstantiated

        let secondHandle, ctx =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                secondInstantiated

        firstHandle |> shouldEqual repeatedFirstHandle
        firstHandle |> shouldNotEqual secondHandle

        let concretizedInner =
            AllConcreteTypes.lookup firstHandle ctx.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "Expected concretized nested generic type to exist")

        concretizedInner.Generics.Length |> shouldEqual 1
        concretizedInner.Generics.[0] |> shouldEqual firstArgumentHandle
