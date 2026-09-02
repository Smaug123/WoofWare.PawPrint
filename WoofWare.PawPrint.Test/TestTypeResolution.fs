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
    /// A row handle for `TypeRef`s these tests build by hand rather than parse. They correspond to
    /// no real TypeRef row, and resolution consumes name, namespace and resolution scope but never
    /// the handle, so any value does — but it must be *some* value, since a `TypeRef` records which
    /// row it came from.
    let private syntheticTypeRefHandle : ComparableTypeReferenceHandle =
        ComparableTypeReferenceHandle.Make (System.Reflection.Metadata.Ecma335.MetadataTokens.TypeReferenceHandle 1)

    let private baseClassTypes () : BaseClassTypes<DumpedAssembly> =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelibPath = typeof<obj>.Assembly.Location

        let corelib = AssemblyApi.readFile loggerFactory corelibPath

        Corelib.getBaseTypes corelib

    [<Test>]
    let ``FromDefinition carries a structured resolved identity`` () : unit =
        let fromDefinitionCase =
            FSharpType.GetUnionCases typeof<TypeDefn>
            |> Array.find (fun unionCase -> unionCase.Name = "FromDefinition")

        let fieldTypes = fromDefinitionCase.GetFields () |> Array.map _.PropertyType

        fieldTypes
        |> shouldEqual [| typeof<ResolvedTypeIdentity> ; typeof<SignatureTypeKind> |]

    [<Test>]
    let ``NominallyEqual compares assembly identities by value`` () : unit =
        // `NominallyEqual` compares the two types' assemblies, and must do so by what the identity
        // says rather than by which object holds it: two `TypeInfo`s denoting the same type need not
        // carry the same string instance.
        let corelib = baseClassTypes ()
        let stringType = corelib.String

        let rehomed =
            { stringType with
                AssemblyFullName = System.String (stringType.AssemblyFullName.ToCharArray ())
            }

        System.Object.ReferenceEquals (stringType.AssemblyFullName, rehomed.AssemblyFullName)
        |> shouldEqual false

        rehomed.AssemblyFullName |> shouldEqual stringType.AssemblyFullName

        TypeInfo.NominallyEqual stringType rehomed |> shouldEqual true
        TypeInfo.NominallyEqual rehomed stringType |> shouldEqual true

    [<Test>]
    let ``NominallyEqual still separates identical rows in different assemblies`` () : unit =
        // The mirror of the above: the same TypeDef row number in a different assembly must not
        // compare equal, or two assemblies' type graphs collapse into one.
        let corelib = baseClassTypes ()
        let stringType = corelib.String

        let elsewhere =
            { stringType with
                AssemblyFullName = "Some.Other.Assembly"
            }

        TypeInfo.NominallyEqual stringType elsewhere |> shouldEqual false
        TypeInfo.NominallyEqual elsewhere stringType |> shouldEqual false

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
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual defining.Name.FullName
        resolvedType.Name |> shouldEqual "Inner"

        AssemblyApi.fullName resolvedAssembly identity |> shouldEqual "N.Outer+Inner"

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
                consumer.DefinitionFullName
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
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty topLevelRef
            |> getResolvedIdentity

        let _, nestedIdentity, _ =
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty nestedRef
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
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        let secondAssembly, secondIdentity, secondType =
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        firstAssembly.Name.FullName |> shouldEqual secondAssembly.Name.FullName
        firstIdentity |> shouldEqual secondIdentity
        TypeInfo.NominallyEqual firstType secondType |> shouldEqual true

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
                AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty typeRef
                |> getResolvedIdentity
            )
            |> List.map (fun (_, identity, _) -> identity)
            |> Set.ofList

        identities.Count |> shouldEqual 2

    [<Test>]
    let ``ModuleRef resolution fails explicitly`` () =
        let consumer =
            {
                Handle = syntheticTypeRefHandle
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
                AssemblyApi.resolveTypeRef assemblies dumped ImmutableArray.Empty consumer
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
            AssemblyApi.resolveTypeFromExport forwarder assemblies ImmutableArray.Empty exportedType
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "Forwarded"

        AssemblyApi.fullName resolvedAssembly identity |> shouldEqual "N.Forwarded"

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
                Handle = syntheticTypeRefHandle
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
                consumer.DefinitionFullName
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
                Handle = syntheticTypeRefHandle
                Name = "GlobalType"
                Namespace = ""
                ResolutionScope =
                    TypeRefResolutionScope.Assembly (findAssemblyReferenceHandle forwarder.Name.FullName consumer)
            }

        let resolvedAssembly, identity, resolvedType =
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty globalTypeRef
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "GlobalType"

        AssemblyApi.fullName resolvedAssembly identity |> shouldEqual "GlobalType"

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
            AssemblyApi.resolveTypeFromExport outer assemblies ImmutableArray.Empty exportedType
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "Forwarded"

        AssemblyApi.fullName resolvedAssembly identity |> shouldEqual "N.Forwarded"

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
            AssemblyApi.resolveTypeFromExport forwarder assemblies ImmutableArray.Empty nestedExport
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        resolvedType.Name |> shouldEqual "Inner"

        AssemblyApi.fullName resolvedAssembly identity |> shouldEqual "N.Outer+Inner"

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

        try
            let _, lf = LoggerFactory.makeTest ()
            use _loggerFactoryResource = lf

            // The host's shared framework has to be on the search path as well as the temp dir
            // holding the forwarding target: resolving `N.Outer.Inner` means loading every
            // assembly on its base-type chain, and that chain runs to System.Object.
            let runtimeDirs =
                ImmutableArray.Create (tempDir, Path.GetDirectoryName typeof<obj>.Assembly.Location)

            let state = IlMachineState.initial lf runtimeDirs forwarder

            let state, resolvedAssembly, resolvedType =
                IlMachineState.resolveTypeFromExport lf forwarder nestedExport ImmutableArray.Empty state

            let outer = getTopLevelTypeDef target "N" "Outer"
            let inner = getNestedTypeDef target outer "Inner"

            resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
            resolvedType.TypeDefHandle |> shouldEqual inner.TypeDefHandle

            match state.LoadedAssembly target.DefinitionFullName with
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
                defining.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                genericType

        let second, _ =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.DefinitionFullName
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
public class Argument { }
public class OpenBox<T> { }
"""
                ]

        let defining =
            dumpedAssembly (Some "RuntimeTypeHandle.OpenGenericDefinition.dll") definingBytes

        let openBox = getTopLevelTypeDef defining "N" "OpenBox`1"
        let argument = getTopLevelTypeDef defining "N" "Argument"

        let identity =
            ResolvedTypeIdentity.ofTypeDefinition defining.Name openBox.TypeDefHandle

        let argumentIdentity =
            ResolvedTypeIdentity.ofTypeDefinition defining.Name argument.TypeDefHandle

        let openBoxDefn = TypeDefn.FromDefinition (identity, SignatureTypeKind.Class)

        let openGenericToken =
            TypeDefn.GenericInstantiation (openBoxDefn, ImmutableArray.Create (TypeDefn.GenericTypeParameter 0))

        let _, loggerFactory = LoggerFactory.makeTest ()
        let baseClassTypes = baseClassTypes ()

        let argumentHandle, ctx =
            TypeConcretization.concretizeTypeDefinition (emptyConcretizationContext [ defining ]) argumentIdentity

        let state =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty defining with
                ConcreteTypes = ctx.ConcreteTypes
            }

        let _, target =
            IlMachineState.runtimeTypeHandleTargetForTypeToken
                loggerFactory
                baseClassTypes
                defining
                true
                ImmutableArray.Empty
                ImmutableArray.Empty
                openGenericToken
                state

        target
        |> shouldEqual (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)

        let _, targetInGenericTypeContext =
            IlMachineState.runtimeTypeHandleTargetForTypeToken
                loggerFactory
                baseClassTypes
                defining
                true
                (ImmutableArray.Create argumentHandle)
                ImmutableArray.Empty
                openGenericToken
                state

        targetInGenericTypeContext
        |> shouldEqual (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)

        let state, constructedTarget =
            IlMachineState.runtimeTypeHandleTargetForTypeToken
                loggerFactory
                baseClassTypes
                defining
                false
                (ImmutableArray.Create argumentHandle)
                ImmutableArray.Empty
                openGenericToken
                state

        match constructedTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
            failwith "TypeSpec-like token was incorrectly classified as an open generic type definition"
        | RuntimeTypeHandleTarget.GenericParameter _ ->
            failwith "TypeSpec-like token was incorrectly classified as a generic parameter"
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            failwith "TypeSpec-like token was incorrectly classified as a method generic parameter"
        | RuntimeTypeHandleTarget.DynamicMethodsClass _ ->
            failwith "TypeSpec-like token was incorrectly classified as the dynamic-methods class"
        | RuntimeTypeHandleTarget.OpenConstructed _ ->
            failwith
                "TypeSpec-like token was incorrectly classified as an open constructed type; its argument is closed, so it must be a Closed handle"
        | RuntimeTypeHandleTarget.Closed handle ->
            let constructed =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"Expected constructed type handle %O{handle} to be registered"
                )

            constructed.Identity |> shouldEqual identity
            constructed.Generics |> shouldEqual (ImmutableArray.Create argumentHandle)

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
                defining.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                firstInstantiated

        let repeatedFirstHandle, ctx =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                firstInstantiated

        let secondHandle, ctx =
            TypeConcretization.concretizeType
                ctx
                (NoAssemblyLoad ())
                defining.DefinitionFullName
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
                AssemblyApi.resolveTypeIdentityDefinition secondAssembly identity |> ignore
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
                AssemblyApi.resolveTypeIdentityDefinition assy missingIdentity |> ignore
            )

        Assert.That (ex.Message, Does.Contain "missing type definition handle")

    /// `loadAssembly` binds by simple name against the runtime dirs "in turn", taking the
    /// first hit. Directories after that hit must not be read at all: a runtime dir list
    /// routinely holds more than one framework, and a later directory containing an
    /// unreadable `<name>.dll` (a native DLL, a truncated file, a foreign-RID pack) must
    /// not break a load the earlier directory already satisfied.
    [<Test>]
    let ``loadAssembly does not read runtime dirs past the first hit`` () : unit =
        let targetBytes =
            compileLibrary "Probe.Target" [] [ "namespace N; public class T { }" ]

        let referencingBytes =
            compileLibrary
                "Probe.Referencing"
                [ metadataReferenceFromImage targetBytes ]
                [ "namespace N; public class C { public static T M() => new T(); }" ]

        let referencing = dumpedAssembly (Some "Probe.Referencing.dll") referencingBytes

        let handle =
            referencing.AssemblyReferences
            |> Seq.choose (fun (KeyValue (handle, assemblyRef)) ->
                if assemblyRef.Name.Name = "Probe.Target" then
                    Some handle
                else
                    None
            )
            |> Seq.exactlyOne

        // A fresh directory per run: `Assembly.readFile` memoises by path for the lifetime of
        // the process, so a fixed path would let one run's parse answer another's.
        let root = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        let firstDir = Path.Combine (root, "first")
        let secondDir = Path.Combine (root, "second")
        Directory.CreateDirectory firstDir |> ignore<DirectoryInfo>
        Directory.CreateDirectory secondDir |> ignore<DirectoryInfo>

        try
            let firstPath = Path.Combine (firstDir, "Probe.Target.dll")
            File.WriteAllBytes (firstPath, targetBytes)

            // Not a PE image at all, so reading it throws something other than
            // FileNotFoundException: a loader that probes past the first hit fails the load.
            File.WriteAllBytes (Path.Combine (secondDir, "Probe.Target.dll"), "not a PE image"B)

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory

            let _, loaded, name =
                TypeResolution.loadAssembly
                    loggerFactory
                    [ firstDir ; secondDir ]
                    referencing
                    handle
                    (loadedAssemblies [])

            name.Name |> shouldEqual "Probe.Target"
            loaded.OriginalPath |> shouldEqual (Some firstPath)
        finally
            Directory.Delete (root, true)

    /// One argument per position, all distinct, so that a substitution which permutes or repeats
    /// its inputs is visible in the result.
    let private distinctPrimitiveArgs (count : int) : ImmutableArray<TypeDefn> =
        let pool =
            [
                PrimitiveType.Int32
                PrimitiveType.String
                PrimitiveType.Boolean
                PrimitiveType.Char
                PrimitiveType.Int64
                PrimitiveType.Double
                PrimitiveType.Byte
                PrimitiveType.Single
            ]

        if count > pool.Length then
            failwith $"distinctPrimitiveArgs: at most %d{pool.Length} distinct arguments are available"

        pool
        |> List.truncate count
        |> List.map TypeDefn.PrimitiveType
        |> ImmutableArray.CreateRange

    [<Test>]
    let ``applyGenericArgs accepts exactly the declared arity or no arguments at all`` () =
        // Every arity from 0 to 4, both top-level and nested (a nested type's arity includes the
        // parameters it inherits from its declaring types), so that the rule is checked at every
        // shape a TypeRef or TypeSpec can name.
        let assemblyBytes =
            compileLibrary
                "ApplyGenericArgs.Arity"
                []
                [
                    """
namespace N;
public class Plain { }
public struct PlainStruct { }
public class One<T>
{
    public class InnerZero { }
    public class InnerOne<U> { }
}
public class Two<T, U>
{
    public class InnerTwo<V, W> { }
}
"""
                ]

        let assy = dumpedAssembly (Some "ApplyGenericArgs.Arity.dll") assemblyBytes

        let typeDefs = assy.TypeDefs.Values |> Seq.toList
        // 8 definitions: <Module>, Plain, PlainStruct, One, InnerZero, InnerOne, Two, InnerTwo.
        typeDefs.Length |> shouldEqual 8

        typeDefs
        |> List.map (fun ty -> ty.Name, ty.Generics.Length)
        |> List.sort
        |> shouldEqual (
            [
                "<Module>", 0
                "Plain", 0
                "PlainStruct", 0
                "One`1", 1
                "InnerZero", 1
                "InnerOne`1", 2
                "Two`2", 2
                "InnerTwo`2", 4
            ]
            |> List.sort
        )

        for ty in typeDefs do
            let arity = ty.Generics.Length

            for count in 0 .. arity + 2 do
                let args = distinctPrimitiveArgs count

                if count = 0 then
                    let resolved = TypeInfo.applyGenericArgs args ty

                    resolved.Generics
                    |> Seq.toList
                    |> shouldEqual (List.init arity TypeDefn.GenericTypeParameter)
                elif count = arity then
                    let resolved = TypeInfo.applyGenericArgs args ty
                    resolved.Generics |> Seq.toList |> shouldEqual (Seq.toList args)
                else
                    let ex =
                        Assert.Throws<System.Exception> (fun () -> TypeInfo.applyGenericArgs args ty |> ignore)

                    Assert.That (ex.Message, Does.Contain ty.Name, $"%s{ty.Name} with %d{count} argument(s)")
                    Assert.That (ex.Message, Does.Contain $"%d{arity} generic parameter")
                    Assert.That (ex.Message, Does.Contain $"%d{count} argument")

    let private nestedGenericDefiningSource : string =
        """
namespace N;
public class Outer<T>
{
    public class Inner<U> { }
}
"""

    [<Test>]
    let ``resolving a nested generic TypeRef substitutes every parameter or refuses`` () =
        let definingBytes =
            compileLibrary "NestedGeneric.Defining" [] [ nestedGenericDefiningSource ]

        let consumerBytes =
            compileLibrary
                "NestedGeneric.Consumer"
                [ metadataReferenceFromImage definingBytes ]
                [
                    """
using N;
public class Consumer
{
    public static Outer<int>.Inner<string> Make() => new Outer<int>.Inner<string>();
}
"""
                ]

        let defining = dumpedAssembly (Some "NestedGeneric.Defining.dll") definingBytes
        let consumer = dumpedAssembly (Some "NestedGeneric.Consumer.dll") consumerBytes
        let assemblies = loadedAssemblies [ defining ; consumer ]

        let innerRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = "Inner`1"
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.TypeRef _ -> true
                       | _ -> false
                )
                consumer

        let outer = getTopLevelTypeDef defining "N" "Outer`1"
        let inner = getNestedTypeDef defining outer "Inner`1"

        let expectedIdentity =
            ResolvedTypeIdentity.ofTypeDefinition defining.Name inner.TypeDefHandle

        // Inner<U> has two generic parameters: T inherited from Outer, then its own U.
        inner.Generics.Length |> shouldEqual 2

        // The full instantiation, spelled the way a TypeSpec for Outer<int>.Inner<string> is.
        let fullArgs =
            ImmutableArray.CreateRange
                [
                    TypeDefn.PrimitiveType PrimitiveType.Int32
                    TypeDefn.PrimitiveType PrimitiveType.String
                ]

        let resolvedAssembly, identity, resolvedType =
            AssemblyApi.resolveTypeRef assemblies consumer fullArgs innerRef
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual defining.Name.FullName
        identity |> shouldEqual expectedIdentity
        resolvedType.Generics |> Seq.toList |> shouldEqual (Seq.toList fullArgs)

        // No arguments leaves the type open.
        let _, identity, openType =
            AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty innerRef
            |> getResolvedIdentity

        identity |> shouldEqual expectedIdentity

        openType.Generics
        |> Seq.toList
        |> shouldEqual [ TypeDefn.GenericTypeParameter 0 ; TypeDefn.GenericTypeParameter 1 ]

        // Only the outer instantiation's argument: neither open nor closed, and the resolver must
        // say so rather than hand back a type whose second parameter would be resolved against
        // whatever generic context happens to be in scope.
        let shortArgs = ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Int32)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                AssemblyApi.resolveTypeRef assemblies consumer shortArgs innerRef |> ignore
            )

        Assert.That (ex.Message, Does.Contain "Inner`1")
        Assert.That (ex.Message, Does.Contain "2 generic parameter")
        Assert.That (ex.Message, Does.Contain "1 argument")

        // One too many is refused the same way.
        let longArgs = distinctPrimitiveArgs 3

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                AssemblyApi.resolveTypeRef assemblies consumer longArgs innerRef |> ignore
            )

        Assert.That (ex.Message, Does.Contain "Inner`1")
        Assert.That (ex.Message, Does.Contain "3 argument")

    [<Test>]
    let ``resolving a nested generic exported type substitutes every parameter or refuses`` () =
        let targetBytes =
            compileLibrary "NestedGeneric.Forwarded.Target" [] [ nestedGenericDefiningSource ]

        let forwarderBytes =
            compileLibrary
                "NestedGeneric.Forwarded.Forwarder"
                [ metadataReferenceFromImage targetBytes ]
                [
                    """
using System.Runtime.CompilerServices;
using N;
[assembly: TypeForwardedTo(typeof(Outer<>))]
public class Placeholder { }
"""
                ]

        let target = dumpedAssembly (Some "NestedGeneric.Forwarded.Target.dll") targetBytes

        let forwarder =
            dumpedAssembly (Some "NestedGeneric.Forwarded.Forwarder.dll") forwarderBytes

        let parentExport =
            findExportedType
                (fun export ->
                    export.Name = "Outer`1"
                    && export.Namespace = Some "N"
                    && match export.Data with
                       | ExportedTypeData.ForwardsTo _ -> true
                       | _ -> false
                )
                forwarder

        let nestedExport, forwarder =
            getOrSynthesizeNestedExportedType parentExport "Inner`1" forwarder

        let assemblies = loadedAssemblies [ target ; forwarder ]

        let outer = getTopLevelTypeDef target "N" "Outer`1"
        let inner = getNestedTypeDef target outer "Inner`1"

        let expectedIdentity =
            ResolvedTypeIdentity.ofTypeDefinition target.Name inner.TypeDefHandle

        let fullArgs = distinctPrimitiveArgs 2

        let resolvedAssembly, identity, resolvedType =
            AssemblyApi.resolveTypeFromExport forwarder assemblies fullArgs nestedExport
            |> getResolvedIdentity

        resolvedAssembly.Name.FullName |> shouldEqual target.Name.FullName
        identity |> shouldEqual expectedIdentity
        resolvedType.Generics |> Seq.toList |> shouldEqual (Seq.toList fullArgs)

        let shortArgs = distinctPrimitiveArgs 1

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                AssemblyApi.resolveTypeFromExport forwarder assemblies shortArgs nestedExport
                |> ignore
            )

        Assert.That (ex.Message, Does.Contain "Inner`1")
        Assert.That (ex.Message, Does.Contain "2 generic parameter")
        Assert.That (ex.Message, Does.Contain "1 argument")
