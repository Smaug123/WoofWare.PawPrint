namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestFieldHandleRegistry =

    [<Test>]
    let ``Ldtoken field handle stores RuntimeFieldInfoStub object`` () : unit =
        let source =
            """
public static class HasField
{
    public static int Data = 1;
}
"""

        let image =
            Roslyn.compileAssembly
                "FieldHandleTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        use corelibStream = File.OpenRead corelibPath

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory (Some corelibPath) corelibStream

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let field =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "HasField" && field.Name = "Data")

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib.Name corelib

        let state =
            (state,
             [
                 baseClassTypes.Object
                 baseClassTypes.Int32
                 baseClassTypes.IntPtr
                 baseClassTypes.RuntimeFieldHandle
                 baseClassTypes.RuntimeFieldHandleInternal
                 baseClassTypes.RuntimeFieldInfoStub
             ])
            ||> List.fold (fun state ty ->
                let typeDefn =
                    DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies ty

                let state, _ =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        baseClassTypes.Corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        typeDefn

                state
            )

        let fieldHandle, state =
            IlMachineState.getOrAllocateField loggerFactory baseClassTypes assembly.Name field.Handle state

        let runtimeFieldInfoStubAddr =
            match fieldHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_ptr" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"Expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
            | other -> failwith $"Expected RuntimeFieldHandle value type, got %O{other}"

        let allocated = ManagedHeap.get runtimeFieldInfoStubAddr state.ManagedHeap

        let runtimeFieldInfoStubType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.RuntimeFieldInfoStub

        allocated.ConcreteType |> shouldEqual runtimeFieldInfoStubType
