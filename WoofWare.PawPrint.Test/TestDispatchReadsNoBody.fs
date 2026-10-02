namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Dispatch decides which method a call runs from the receiver's method table, without reading the
/// body of the method it picks: CoreCLR reads that body only when it compiles the method. A
/// client compiled against a provider and loaded beside a later version lacking a type that only
/// the chosen method's local names therefore dispatches as before.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDispatchReadsNoBody =

    let private provider1 : string =
        """
namespace Provider;
public class Kept { }
public class GoneType { }
"""

    let private provider2 : string =
        """
namespace Provider;
public class Kept { }
"""

    let private client : string =
        """
namespace Client;

public interface IRuns { int Run(); }

public struct UsesGone : IRuns
{
    public int Run()
    {
        Provider.GoneType x = null;
        return x == null ? 0 : 1;
    }
}

public interface IDefaultGone : IRuns
{
    int IRuns.Run()
    {
        Provider.GoneType x = null;
        return x == null ? 0 : 1;
    }
}

public struct UsesDefaultGone : IDefaultGone { }

public class BaseRuns { public virtual int Go() => 0; }

public sealed class DerivedGone : BaseRuns
{
    public override int Go()
    {
        Provider.GoneType x = null;
        return x == null ? 0 : 1;
    }
}
"""

    /// The receiver, the type declaring the method called, the method's name, and the type declaring
    /// the method dispatch must choose, with that method's name.
    let cases : TestCaseData list =
        [
            "UsesGone", "IRuns", "Run", "UsesGone", "Run"
            "UsesDefaultGone", "IRuns", "Run", "IDefaultGone", "Client.IRuns.Run"
            "DerivedGone", "BaseRuns", "Go", "DerivedGone", "Go"
        ]
        |> List.map (fun (receiver, declaring, methodName, chosen, chosenMethod) ->
            TestCaseData
                [|
                    box receiver
                    box declaring
                    box methodName
                    box chosen
                    box chosenMethod
                |]
        )

    [<TestCaseSource(nameof cases)>]
    let ``dispatch chooses a method whose local names a type the provider lacks``
        (receiverName : string)
        (declaringName : string)
        (methodName : string)
        (chosenName : string)
        (chosenMethodName : string)
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly =
            read "Client" (compile "Client" [ compile "Provider" [] provider1 ] client)

        let providerAssembly = read "Provider" (compile "Provider" [] provider2)

        let corelib =
            Assembly.readFile
                loggerFactory
                (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

        let bct = BaseClassTypes.ofCorelib corelib

        let providerReference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let loaded =
            LoadedAssemblies.ofAssemblies [ corelib ; clientAssembly ; providerAssembly ]
            |> fun loaded -> fst (loaded.WithBoundReference providerReference providerAssembly)

        let state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        let typeNamed (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            clientAssembly.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = name)

        let receiverType = typeNamed receiverName

        let state, receiver =
            TypeSystemState.concretizeType
                loggerFactory
                runtimeDirs
                bct
                state
                clientAssembly.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (
                    receiverType.Identity,
                    if receiverType.Name.StartsWith "Uses" then
                        SignatureTypeKind.ValueType
                    else
                        SignatureTypeKind.Class
                ))

        let method =
            (typeNamed declaringName).Methods |> List.find (fun m -> m.Name = methodName)

        let state, concretized, _ =
            MethodConcretisation.concretizeMethodWithAllGenerics
                loggerFactory
                runtimeDirs
                bct
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        let _, resolved =
            ConcreteVirtualDispatch.tryResolveVirtualImplementation
                loggerFactory
                runtimeDirs
                bct
                concretized.Generics
                concretized
                receiver
                true
                state

        match resolved with
        | VirtualImplementation.Found found ->
            found.Definition.RequiredDeclaringType.Name |> shouldEqual chosenName
            found.Definition.Name |> shouldEqual chosenMethodName
        | other -> failwith $"expected dispatch to choose %s{chosenName}'s method, got %A{other}"
