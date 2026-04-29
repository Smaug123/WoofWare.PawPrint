namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestMethodHandleRegistry =

    let private loadAssemblyFromSource
        (assemblyName : string)
        (source : string)
        : Microsoft.Extensions.Logging.ILoggerFactory * BaseClassTypes<DumpedAssembly> * DumpedAssembly * IlMachineState
        =
        let image =
            Roslyn.compileAssembly assemblyName Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            let state = initialState.WithLoadedAssembly corelib.Name corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        loggerFactory, baseClassTypes, assembly, state

    let private findMethod
        (declaringTypeName : string)
        (methodName : string)
        (assembly : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        assembly.Methods.Values
        |> Seq.find (fun method -> method.DeclaringType.Name = declaringTypeName && method.Name = methodName)

    let private installFrameForMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assembly : DumpedAssembly)
        (state : IlMachineState)
        (currentMethod : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ThreadId
        =
        let method =
            currentMethod
            |> MethodInfo.mapTypeGenerics (fun (param, _) -> TypeDefn.GenericTypeParameter param.SequenceNumber)

        let state, concretizedMethod, _declaringType =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    assembly
                    concretizedMethod
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating method-handle test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        state, thread

    let private loadFixture () =
        let source =
            """
public static class HasMethod
{
    public static int Target()
    {
        return 1;
    }
}
"""

        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "MethodHandleTestAssembly" source

        let targetMethod = assembly |> findMethod "HasMethod" "Target"

        let method =
            targetMethod
            |> MethodInfo.mapTypeGenerics (fun (param, _) -> TypeDefn.GenericTypeParameter param.SequenceNumber)

        let state, concretizedMethod, _declaringType =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        loggerFactory, baseClassTypes, assembly, targetMethod, concretizedMethod, state

    let private assertRuntimeMethodInfoStub
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : unit
        =
        let allocated = ManagedHeap.get addr state.ManagedHeap

        let runtimeMethodInfoStubType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.RuntimeMethodInfoStub

        allocated.ConcreteType |> shouldEqual runtimeMethodInfoStubType

        match allocated |> AllocatedNonArrayObject.DereferenceField "m_value" with
        | CliType.ValueType vt ->
            vt.PrimitiveLikeKind
            |> shouldEqual (Some PrimitiveLikeKind.FlattenToRuntimePointer)

            match CliValueType.DereferenceField "m_handle" vt with
            | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle 1L) -> ()
            | other ->
                failwith $"Expected RuntimeMethodHandleInternal.m_handle to be method registry handle 1, got %O{other}"
        | other -> failwith $"Expected RuntimeMethodInfoStub.m_value to be a value type, got %O{other}"

    [<Test>]
    let ``Method handle stores RuntimeMethodInfoStub object`` () : unit =
        let loggerFactory, baseClassTypes, _assembly, _targetMethod, concretizedMethod, state =
            loadFixture ()

        let methodHandle, state =
            IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretizedMethod state

        let runtimeMethodInfoStubAddr =
            match methodHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_value" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"Expected RuntimeMethodHandle.m_value to be an object ref, got %O{other}"
            | other -> failwith $"Expected RuntimeMethodHandle value type, got %O{other}"

        assertRuntimeMethodInfoStub baseClassTypes state runtimeMethodInfoStubAddr

    [<Test>]
    let ``Ldtoken MethodDef pushes RuntimeMethodInfoStub object`` () : unit =
        let loggerFactory, baseClassTypes, assembly, targetMethod, concretizedMethod, state =
            loadFixture ()

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    assembly
                    concretizedMethod
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating method-handle test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        let token =
            MetadataToken.MethodDef targetMethod.Handle
            |> SourcedMetadataToken.make assembly.Name

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Ldtoken token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        match IlMachineState.peekEvalStack thread state with
        | Some (EvalStackValue.ObjectRef addr) -> assertRuntimeMethodInfoStub baseClassTypes state addr
        | other -> failwith $"Expected ldtoken MethodDef to push a RuntimeMethodHandle object ref, got %O{other}"

    [<Test>]
    let ``Ldtoken MethodDef on generic declaring type fails explicitly`` () : unit =
        let source =
            """
public static class Caller
{
    public static int Current()
    {
        return 0;
    }
}

public class GenericHasMethod<T>
{
    public static int Target()
    {
        return 1;
    }
}
"""

        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "GenericMethodHandleTestAssembly" source

        let currentMethod = assembly |> findMethod "Caller" "Current"
        let targetMethod = assembly |> findMethod "GenericHasMethod`1" "Target"

        let state, thread =
            installFrameForMethod loggerFactory baseClassTypes assembly state currentMethod

        let token =
            MetadataToken.MethodDef targetMethod.Handle
            |> SourcedMetadataToken.make assembly.Name

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute
                    loggerFactory
                    baseClassTypes
                    UnaryMetadataTokenIlOp.Ldtoken
                    token
                    state
                    thread
                |> ignore
            )

        ex.Message
        |> shouldContainText "TODO: ldtoken MethodDef for methods on generic declaring types"

    [<Test>]
    let ``Ldtoken MethodDef on generic method fails explicitly`` () : unit =
        let source =
            """
public static class Caller
{
    public static int Current()
    {
        return 0;
    }
}

public static class GenericMethodHolder
{
    public static T Target<T>()
    {
        return default(T);
    }
}
"""

        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "GenericMethodDefHandleTestAssembly" source

        let currentMethod = assembly |> findMethod "Caller" "Current"
        let targetMethod = assembly |> findMethod "GenericMethodHolder" "Target"

        let state, thread =
            installFrameForMethod loggerFactory baseClassTypes assembly state currentMethod

        let token =
            MetadataToken.MethodDef targetMethod.Handle
            |> SourcedMetadataToken.make assembly.Name

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute
                    loggerFactory
                    baseClassTypes
                    UnaryMetadataTokenIlOp.Ldtoken
                    token
                    state
                    thread
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: ldtoken MethodDef for generic methods"
