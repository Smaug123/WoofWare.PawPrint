namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open Microsoft.FSharp.Reflection
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open TypeIdentityTestHelpers

[<TestFixture>]
module TestTypeResolution =

    [<Test>]
    let ``FromDefinition carries a structured resolved identity`` () : unit =
        let fromDefinitionCase =
            FSharpType.GetUnionCases typeof<TypeDefn>
            |> Array.find (fun unionCase -> unionCase.Name = "FromDefinition")

        let fieldTypes = fromDefinitionCase.GetFields () |> Array.map _.PropertyType

        fieldTypes
        |> shouldEqual [| typeof<ResolvedTypeIdentity> ; typeof<SignatureTypeKind> |]

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

        let defining =
            dumpedAssembly (Some "NestedIdentity.Concretize.Defining.dll") definingBytes

        let consumer =
            dumpedAssembly (Some "NestedIdentity.Concretize.Consumer.dll") consumerBytes

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
    let ``resolving the same type ref twice is idempotent`` () =
        let definingBytes =
            compileLibrary
                "TypeIdentity.Idempotent.Defining"
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
                "TypeIdentity.Idempotent.Consumer"
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

        let defining =
            dumpedAssembly (Some "TypeIdentity.Idempotent.Defining.dll") definingBytes

        let consumer =
            dumpedAssembly (Some "TypeIdentity.Idempotent.Consumer.dll") consumerBytes

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

        let firstAssembly, firstIdentity, firstType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        let secondAssembly, secondIdentity, secondType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        firstAssembly.Name.FullName |> shouldEqual secondAssembly.Name.FullName
        firstIdentity |> shouldEqual secondIdentity
        firstType |> shouldEqual secondType

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
public class Consumer
{
    private Placeholder _field = new Placeholder();
}
"""
                ]

        let target =
            dumpedAssembly (Some "TypeIdentity.ForwardedConcretize.Target.dll") targetBytes

        let forwarder =
            dumpedAssembly (Some "TypeIdentity.ForwardedConcretize.Forwarder.dll") forwarderBytes

        let consumer =
            dumpedAssembly (Some "TypeIdentity.ForwardedConcretize.Consumer.dll") consumerBytes

        let loader =
            RecordingAssemblyLoad (loadedAssemblies [ consumer ; forwarder ; target ])

        let forwardedRef =
            {
                Name = "Forwarded"
                Namespace = "N"
                ResolutionScope =
                    TypeRefResolutionScope.Assembly (findAssemblyReferenceHandle forwarder.Name.FullName consumer)
            }

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
    let ``forwarded top-level type in the global namespace resolves via resolveTypeRef`` () =
        let targetBytes =
            compileLibrary "TypeIdentity.ForwardedGlobalNs.Target" [] [ "public class GlobalType { }" ]

        let forwarderBytes =
            compileLibrary
                "TypeIdentity.ForwardedGlobalNs.Forwarder"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
using System.Runtime.CompilerServices;
[assembly: TypeForwardedTo(typeof(GlobalType))]
public class Placeholder { }
"""
                ]

        // Consumer references the forwarder (via Placeholder) so we get an assembly reference handle.
        let consumerBytes =
            compileLibrary
                "TypeIdentity.ForwardedGlobalNs.Consumer"
                [ metadataReferenceFromImage forwarderBytes ]
                [ "public class Consumer { private Placeholder _field = new Placeholder(); }" ]

        let target =
            dumpedAssembly (Some "TypeIdentity.ForwardedGlobalNs.Target.dll") targetBytes

        let forwarder =
            dumpedAssembly (Some "TypeIdentity.ForwardedGlobalNs.Forwarder.dll") forwarderBytes

        let consumer =
            dumpedAssembly (Some "TypeIdentity.ForwardedGlobalNs.Consumer.dll") consumerBytes

        let assemblies = loadedAssemblies [ target ; forwarder ; consumer ]

        // Manually construct a TypeRef as if the consumer had a reference to GlobalType
        // via the forwarder. This is the scenario where an assembly was compiled against
        // the original assembly, which later became a forwarder.
        let globalTypeRef : TypeRef =
            {
                Name = "GlobalType"
                Namespace = ""
                ResolutionScope =
                    TypeRefResolutionScope.Assembly (findAssemblyReferenceHandle forwarder.Name.FullName consumer)
            }

        let resolvedAssembly, identity, resolvedType =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty globalTypeRef
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "GlobalType"

        global.WoofWare.PawPrint.AssemblyApi.fullName resolvedAssembly identity
        |> shouldEqual "GlobalType"

        let globalTypeDef = getTopLevelTypeDef target "" "GlobalType"

        identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition target.Name globalTypeDef.TypeDefHandle)

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

        let target =
            dumpedAssembly (Some "TypeIdentity.ForwardedChain.Target.dll") targetBytes

        let middle =
            dumpedAssembly (Some "TypeIdentity.ForwardedChain.Middle.dll") middleBytes

        let outer = dumpedAssembly (Some "TypeIdentity.ForwardedChain.Outer.dll") outerBytes

        let exportedType, outer =
            synthesizeTopLevelForwarderExport "N" "Forwarded" middle.Name.FullName outer

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
                global.WoofWare.PawPrint.IlMachineState.initial lf (ImmutableArray.Create tempDir) forwarder

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
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let first, ctx =
            TypeConcretization.concretizeTypeDefinition
                ctx
                (ResolvedTypeIdentity.ofTypeDefinition defining.Name inner.TypeDefHandle)

        let second, _ =
            TypeConcretization.concretizeTypeDefinition
                ctx
                (ResolvedTypeIdentity.ofTypeDefinition defining.Name inner.TypeDefHandle)

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
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let xHandle, ctx =
            TypeConcretization.concretizeTypeDefinition
                ctx
                (ResolvedTypeIdentity.ofTypeDefinition defining.Name xInner.TypeDefHandle)

        let yHandle, _ =
            TypeConcretization.concretizeTypeDefinition
                ctx
                (ResolvedTypeIdentity.ofTypeDefinition defining.Name yInner.TypeDefHandle)

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
                ResolvedTypeIdentity.ofTypeDefinition defining.Name box.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let argumentDefn =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition defining.Name argument.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let genericType =
            TypeDefn.GenericInstantiation (boxDefn, ImmutableArray.Create argumentDefn)

        let ctx =
            {
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
    let ``runtime type token target preserves open generic type definitions`` () =
        let definingBytes =
            compileLibrary
                "RuntimeTypeHandle.OpenGenericDefinition"
                []
                [
                    """
namespace N;
public class OpenBox<T> { }
"""
                ]

        let defining =
            dumpedAssembly (Some "RuntimeTypeHandle.OpenGenericDefinition.dll") definingBytes

        let openBox = getTopLevelTypeDef defining "N" "OpenBox`1"

        let identity =
            ResolvedTypeIdentity.ofTypeDefinition defining.Name openBox.TypeDefHandle

        let openBoxDefn = TypeDefn.FromDefinition (identity, SignatureTypeKind.Class)

        let openGenericToken =
            TypeDefn.GenericInstantiation (openBoxDefn, ImmutableArray.Create (TypeDefn.GenericTypeParameter 0))

        let loggerFactory = loggerFactory ()

        let state =
            global.WoofWare.PawPrint.IlMachineState.initial loggerFactory ImmutableArray.Empty defining

        let _, target =
            IlMachineState.runtimeTypeHandleTargetForTypeToken
                loggerFactory
                Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
                defining
                ImmutableArray.Empty
                ImmutableArray.Empty
                openGenericToken
                state

        target
        |> shouldEqual (RuntimeTypeHandleTarget.OpenGenericTypeDefinition (identity, 1))

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
                ResolvedTypeIdentity.ofTypeDefinition defining.Name firstArgument.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let secondArgumentDefn =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition defining.Name secondArgument.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let innerDefn =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition defining.Name inner.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let firstInstantiated =
            TypeDefn.GenericInstantiation (innerDefn, ImmutableArray.Create firstArgumentDefn)

        let secondInstantiated =
            TypeDefn.GenericInstantiation (innerDefn, ImmutableArray.Create secondArgumentDefn)

        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies [ defining ]
                TypeConcretization.ConcretizationContext.BaseTypes = Unchecked.defaultof<BaseClassTypes<DumpedAssembly>>
            }

        let firstArgumentHandle, ctx =
            TypeConcretization.concretizeTypeDefinition
                ctx
                (ResolvedTypeIdentity.ofTypeDefinition defining.Name firstArgument.TypeDefHandle)

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

    [<Test>]
    let ``resolved type identity lookup fails fast on assembly mismatch`` () =
        let firstAssemblyBytes =
            compileLibrary "TypeIdentity.Lookup.First" [] [ "namespace N; public class First { }" ]

        let secondAssemblyBytes =
            compileLibrary "TypeIdentity.Lookup.Second" [] [ "namespace N; public class Second { }" ]

        let firstAssembly =
            dumpedAssembly (Some "TypeIdentity.Lookup.First.dll") firstAssemblyBytes

        let secondAssembly =
            dumpedAssembly (Some "TypeIdentity.Lookup.Second.dll") secondAssemblyBytes

        let firstType = getTopLevelTypeDef firstAssembly "N" "First"

        let identity =
            ResolvedTypeIdentity.ofTypeDefinition firstAssembly.Name firstType.TypeDefHandle

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.resolveTypeIdentityDefinition secondAssembly identity
                |> ignore
            )

        Assert.That (ex.Message, Does.Contain "ResolvedTypeIdentity points at assembly")

    [<Test>]
    let ``resolved type identity lookup fails fast on missing handle`` () =
        let assemblyBytes =
            compileLibrary "TypeIdentity.Lookup.MissingHandle" [] [ "namespace N; public class Present { }" ]

        let assy =
            dumpedAssembly (Some "TypeIdentity.Lookup.MissingHandle.dll") assemblyBytes

        let missingIdentity =
            ResolvedTypeIdentity.ofTypeDefinition
                assy.Name
                (System.Reflection.Metadata.Ecma335.MetadataTokens.TypeDefinitionHandle 999)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.resolveTypeIdentityDefinition assy missingIdentity
                |> ignore
            )

        Assert.That (ex.Message, Does.Contain "missing type definition handle")
