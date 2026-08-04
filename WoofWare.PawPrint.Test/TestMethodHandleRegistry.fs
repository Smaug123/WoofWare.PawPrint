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

            let state = initialState.WithLoadedAssembly corelib

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

    /// Locate the closed `ConcreteType<ConcreteTypeHandle>` for the declaring type of the given
    /// open method. Used by the introduced-method iterator tests (which do not concretize the
    /// method itself, only its declaring type).
    let private findDeclaringConcreteType
        (state : IlMachineState)
        (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes method.DeclaringType.Identity
            |> Option.defaultWith (fun () ->
                failwith
                    $"Closed ConcreteType for declaring type '%s{method.DeclaringType.Name}' was not registered in state.ConcreteTypes"
            )

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"declaring-type handle %O{handle} not present in mapping")

    [<Test>]
    let ``zeroInternalHandle encodes m_handle as a verbatim IntPtr.Zero`` () : unit =
        let _, baseClassTypes, _, _, _, state = loadFixture ()

        let zero =
            MethodHandleRegistry.zeroInternalHandle baseClassTypes state.ConcreteTypes

        zero.PrimitiveLikeKind
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToRuntimePointer)

        // The BCL terminates `IntroducedMethodEnumerator` by comparing m_handle to IntPtr.Zero,
        // so the null sentinel must flatten to a Verbatim 0L native int (NOT a method-registry
        // pointer with id 0, which would be opaque to that comparison).
        match CliValueType.DereferenceField "m_handle" zero |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> ()
        | other -> failwith $"Expected null RuntimeMethodHandleInternal m_handle to be Verbatim 0L, got %O{other}"

    [<Test>]
    let ``getOrAllocateInternalHandle round-trips through resolveMethodFromId`` () : unit =
        let _, baseClassTypes, _, targetMethod, _, state = loadFixture ()

        let declaringType = findDeclaringConcreteType state targetMethod

        let internalHandle, registry =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringType
                targetMethod
                state.MethodHandles

        let registryId =
            match
                CliValueType.DereferenceField "m_handle" internalHandle
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id
            | other -> failwith $"Expected MethodRegistryHandle id in m_handle, got %O{other}"

        registryId |> shouldNotEqual 0L

        // Round-trip: the registered id resolves back to a MethodHandle whose method-def handle
        // matches the one we registered.
        let resolved =
            MethodHandleRegistry.resolveMethodFromId registryId registry
            |> Option.defaultWith (fun () -> failwith $"registry id %d{registryId} did not resolve")

        resolved.GetMethodDefinitionHandle ()
        |> shouldEqual (ComparableMethodDefinitionHandle.Make targetMethod.Handle)

        // Open-form registration intentionally records empty MethodGenerics: the iterator
        // surfaces method-table slots, i.e. method definitions, not specific instantiations.
        resolved.GetMethodGenerics () |> shouldEqual []

        // resolveMethodFromId returns None for the null-sentinel id (0).
        MethodHandleRegistry.resolveMethodFromId 0L registry |> shouldEqual None

    [<Test>]
    let ``getOrAllocateInternalHandle is idempotent for the same method`` () : unit =
        let _, baseClassTypes, _, targetMethod, _, state = loadFixture ()
        let declaringType = findDeclaringConcreteType state targetMethod

        let firstHandle, registry1 =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringType
                targetMethod
                state.MethodHandles

        let secondHandle, _ =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringType
                targetMethod
                registry1

        let extractId (vt : CliValueType) : int64 =
            match CliValueType.DereferenceField "m_handle" vt |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id
            | other -> failwith $"Expected MethodRegistryHandle id, got %O{other}"

        extractId firstHandle |> shouldEqual (extractId secondHandle)

    [<Test>]
    let ``getOrAllocateInternalHandle accepts methods with method-generic parameters`` () : unit =
        // Regression test: prior to commit fixing introduced-method iterator generics, the path
        // routed through `concretizeMethodWithAllGenerics` with empty methodArgs and crashed at
        // `methodArgs.[gp.SequenceNumber]` for any method with method-generic parameters.
        let source =
            """
public static class GenericMethodHolder
{
    public static T Identity<T>(T value)
    {
        return value;
    }
}
"""

        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "IteratorGenericMethodTestAssembly" source

        let targetMethod = assembly |> findMethod "GenericMethodHolder" "Identity"

        // Register the (non-generic) declaring type in state.ConcreteTypes WITHOUT concretizing
        // the generic method itself.
        let declaringTypeInfo =
            assembly.TypeDefs.[targetMethod.DeclaringType.Definition.Get]

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies declaringTypeInfo

        let state, declaringHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (targetMethod.DeclaringType.Identity, stk))

        let declaringConcrete, _ =
            IlMachineState.tryGetConcreteTypeInfo state declaringHandle
            |> Option.defaultWith (fun () -> failwith "declaring type not registered after concretizeType")

        // The bug reproducer: this should not throw IndexOutOfRangeException.
        let internalHandle, _ =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringConcrete
                targetMethod
                state.MethodHandles

        // The handle should carry a non-zero registry id.
        match
            CliValueType.DereferenceField "m_handle" internalHandle
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id |> shouldNotEqual 0L
        | other -> failwith $"Expected MethodRegistryHandle id for generic-method definition, got %O{other}"

    [<Test>]
    let ``methodHandleIdOfRuntimeMethodHandleInternal accepts both canonical and post-rewrap forms`` () : unit =
        // GetFirstIntroducedMethod returns a RuntimeMethodHandleInternal whose m_handle carries
        // a RuntimePointer (MethodRegistryHandle id). When the BCL stores that struct into its
        // managed `m_handle` field and the IntroducedMethodEnumerator passes it back to
        // GetNextIntroducedMethod through a byref, primitive-like rewrapping (EvalStack.fs:538)
        // promotes the runtime pointer to NativeInt (MethodHandlePtr id). The shared helper that
        // GetNextIntroducedMethod uses must accept both forms; otherwise the iterator stalls
        // after the first method.
        let op = "methodHandleIdOfRuntimeMethodHandleInternal-test"

        let canonical = CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle 42L)

        NativeCall.methodHandleIdOfRuntimeMethodHandleInternal op canonical
        |> shouldEqual (Some 42L)

        let postRewrap =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodHandlePtr 42L))

        NativeCall.methodHandleIdOfRuntimeMethodHandleInternal op postRewrap
        |> shouldEqual (Some 42L)

        // Both null-sentinel encodings (verbatim 0L on either tag) signal "iteration exhausted".
        NativeCall.methodHandleIdOfRuntimeMethodHandleInternal
            op
            (CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L))
        |> shouldEqual None

        NativeCall.methodHandleIdOfRuntimeMethodHandleInternal
            op
            (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
        |> shouldEqual None
