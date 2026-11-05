namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Concretising a method, as binding a call to it does, loads what its signature names but not what
/// its locals name. CoreCLR loads a method's locals when it compiles the method, so an assembly a
/// method names only in a local is loaded when the method is first entered, not when a method that
/// calls it is compiled.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMethodConcretisationLocals =

    let private holder =
        """
public static class Holder
{
    public static int Touch()
    {
        var stack = new System.Collections.Generic.Stack<int>();
        stack.Push(1);
        return stack.Count;
    }
}
"""

    let private namesCollections (state : TypeSystemState) : bool =
        state._LoadedAssemblies.DefinitionNames
        |> Seq.exists (fun name -> name.StartsWith "System.Collections,")

    [<Test>]
    let ``concretising a method does not load what only its locals name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()

        let image =
            Roslyn.compileAssembly "Holder" OutputKind.DynamicallyLinkedLibrary [] [ holder ]

        let holderAssembly =
            Assembly.read loggerFactory (Some "Holder.dll") (new MemoryStream (image))

        let corelib =
            Assembly.readFile
                loggerFactory
                (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

        let bct = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; holderAssembly ]

        let state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        let touch =
            holderAssembly.TypeDefs.Values
            |> Seq.find (fun ty -> ty.Name = "Holder")
            |> fun ty -> ty.Methods |> List.find (fun m -> m.Name = "Touch")

        let state, concretised =
            MethodConcretisation.concretizeMethodWithAllGenerics
                loggerFactory
                runtimeDirs
                bct
                ImmutableArray.Empty
                touch
                ImmutableArray.Empty
                state

        BindingResult.value concretised |> ignore

        namesCollections state |> shouldEqual false
