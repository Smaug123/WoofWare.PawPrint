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
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
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
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
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
            match MethodHandleRegistry.resolveMethodFromId registryId registry with
            | Some (MethodHandle.FromMetadata identity) -> identity
            | None -> failwith $"registry id %d{registryId} did not resolve"

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

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    /// Drive a one-argument `RuntimeMethodHandle` InternalCall directly, with the given
    /// `RuntimeMethodHandleInternal` as its sole argument, and return what it pushed.
    ///
    /// The natives driven this way cannot be reached from guest C#/F# in isolation:
    /// `RuntimeType.GetMethodBase` is the only BCL caller of `IsDynamicMethod`, and on the `false`
    /// branch it immediately calls `RuntimeMethodHandle.GetDeclaringType`, whose body is
    /// `GetRuntimeType(GetMethodTable(method))` -- so no guest source can reach either of those two
    /// without reaching the other. `GetMethodBase` then goes on to `IsConstructor`
    /// (RuntimeType.CoreCLR.cs:1934) and `HasMethodInstantiation`, neither of which is implemented,
    /// so the whole chain is exercised here rather than by a case in `sourcesPure/`.
    let private invokeRuntimeMethodHandleFCall
        (methodName : string)
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (methodHandleInternal : CliType)
        (state : IlMachineState)
        : EvalStackValue
        =
        let runtimeMethodHandleType =
            requiredTopLevelType baseClassTypes.Corelib "System" "RuntimeMethodHandle"

        let rawMethod =
            runtimeMethodHandleType.Methods
            |> List.filter (fun method -> method.Name = methodName && method.Parameters.Length = 1)
            |> function
                | [ method ] -> method
                | [] -> failwith $"RuntimeMethodHandle.%s{methodName} native method not found"
                | methods -> failwith $"RuntimeMethodHandle.%s{methodName} was ambiguous: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.Corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create methodHandleInternal)
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith
                    $"Unexpected missing assembly references creating RuntimeMethodHandle.%s{methodName} frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = baseClassTypes.Corelib
                TargetType = runtimeMethodHandleType
            }

        let state =
            match NativeRuntimeMethodHandle.tryExecute ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected RuntimeMethodHandle.%s{methodName} execution result: %O{result}"
            | None -> failwith $"RuntimeMethodHandle.%s{methodName} did not match"

        IlMachineState.popEvalStack thread state |> fst

    let private invokeIsDynamicMethod = invokeRuntimeMethodHandleFCall "IsDynamicMethod"

    let private invokeGetMethodTable = invokeRuntimeMethodHandleFCall "GetMethodTable"

    let private invokeIsConstructor = invokeRuntimeMethodHandleFCall "IsConstructor"

    /// Register `method`'s (non-generic) declaring type in `state.ConcreteTypes` and hand back the
    /// `ConcreteType` the method-handle registry needs. `findDeclaringConcreteType` above only
    /// finds a type some earlier concretization already registered; this puts one there.
    let private concretizeDeclaringType
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assembly : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * ConcreteType<ConcreteTypeHandle>
        =
        let declaringTypeInfo = assembly.TypeDefs.[method.DeclaringType.Definition.Get]

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies declaringTypeInfo

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (method.DeclaringType.Identity, stk))

        let concrete =
            AllConcreteTypes.lookup handle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"declaring-type handle %O{handle} not present in mapping")

        state, concrete

    /// Mint a `RuntimeMethodHandleInternal` for the named method of the given source, and return
    /// what the `IsConstructor` InternalCall pushes for it.
    let private isConstructorOf (assemblyName : string) (source : string) (typeName : string) (methodName : string) =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource assemblyName source

        let targetMethod = assembly |> findMethod typeName methodName

        let state, declaringType =
            concretizeDeclaringType loggerFactory baseClassTypes assembly targetMethod state

        let internalHandle, registry =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringType
                targetMethod
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        invokeIsConstructor loggerFactory baseClassTypes (CliType.ValueType internalHandle) state

    /// One type carrying all three method shapes the predicate must distinguish. The static
    /// constructor is written explicitly rather than left to a static field initialiser, because
    /// Roslyn folds a constant initialiser into the field's metadata and emits no `.cctor` at all.
    let private constructorFixtureSource : string =
        """
public class HasConstructors
{
    public static int Shared;

    static HasConstructors()
    {
        Shared = 5;
    }

    public HasConstructors()
    {
    }

    public int Plain()
    {
        return 1;
    }
}
"""

    [<Test>]
    let ``IsConstructor is true for an instance constructor`` () : unit =
        isConstructorOf "IsConstructorCtorAssembly" constructorFixtureSource "HasConstructors" ".ctor"
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

    [<Test>]
    let ``IsConstructor is true for a static constructor`` () : unit =
        // CoreCLR's `IsClassConstructorOrCtor` (method.hpp:491) covers `.cctor` as well as `.ctor`,
        // which is why the FCall's name understates it. `RuntimeType.GetMethodBase` relies on that:
        // a `.cctor` handle must produce a ConstructorInfo, not a MethodInfo.
        isConstructorOf "IsConstructorCctorAssembly" constructorFixtureSource "HasConstructors" ".cctor"
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

    [<Test>]
    let ``IsConstructor is false for an ordinary method`` () : unit =
        isConstructorOf "IsConstructorPlainAssembly" constructorFixtureSource "HasConstructors" "Plain"
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``IsDynamicMethod is false for a registry-minted handle`` () : unit =
        let loggerFactory, baseClassTypes, _, targetMethod, _, state = loadFixture ()
        let declaringType = findDeclaringConcreteType state targetMethod

        let internalHandle, registry =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringType
                targetMethod
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        // PawPrint has no Reflection.Emit, so nothing it can mint is a no-metadata method.
        // `CliType.ofBool false` is an Int32 0 on the eval stack.
        invokeIsDynamicMethod loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``IsDynamicMethod fails loudly for an unregistered handle id`` () : unit =
        // CoreCLR dereferences the MethodDesc* and asserts non-null, so an id the registry never
        // minted is a PawPrint contract violation rather than a guest-visible condition. Returning
        // `false` here would let the guest walk on into GetDeclaringType and fail further from the
        // cause.
        let loggerFactory, baseClassTypes, _, _, _, state = loadFixture ()

        let bogusHandle =
            MethodHandleRegistry.internalHandleFromId baseClassTypes state.ConcreteTypes 12345L

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeIsDynamicMethod loggerFactory baseClassTypes (CliType.ValueType bogusHandle) state
                |> ignore
            )

        ex.Message
        |> shouldContainText "RuntimeMethodHandle.IsDynamicMethod: registry id 12345 did not resolve"

    [<Test>]
    let ``GetMethodTable names the declaring type of a method on a non-generic type`` () : unit =
        let loggerFactory, baseClassTypes, _, targetMethod, _, state = loadFixture ()
        let declaringType = findDeclaringConcreteType state targetMethod

        let expected =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes targetMethod.DeclaringType.Identity
            |> Option.defaultWith (fun () -> failwith "declaring type was not registered in ConcreteTypes")

        let internalHandle, registry =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                declaringType
                targetMethod
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        invokeGetMethodTable loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed expected))
        )

    [<Test>]
    let ``GetMethodTable preserves the instantiation of a closed generic declaring type`` () : unit =
        // CoreCLR's `MethodDesc::GetMethodTable` returns the MethodTable of the chunk the MethodDesc
        // lives in, which for a *shared* (reference-type) instantiation is the canonical
        // `Holder<__Canon>`. PawPrint models no sharing at all, which is exactly CoreCLR's
        // value-type-instantiation regime: the MethodTable of `Holder<int>.Target` is `Holder<int>`.
        // So the instantiation must survive; collapsing it to the open generic definition (which is
        // a different MethodTable in CoreCLR too, with IsGenericTypeDefinition = true) would be
        // wrong, and this assertion is what fails if someone tries it.
        let source =
            """
public class GenericHolder<T>
{
    public int Target(T t)
    {
        return 1;
    }
}
"""

        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "GetMethodTableGenericTypeAssembly" source

        let targetMethod = assembly |> findMethod "GenericHolder`1" "Target"

        let declaringTypeInfo =
            assembly.TypeDefs.[targetMethod.DeclaringType.Definition.Get]

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies declaringTypeInfo

        let closedDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (targetMethod.DeclaringType.Identity, stk),
                ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Int32)
            )

        let state, closedHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                closedDefn

        let closedConcrete =
            AllConcreteTypes.lookup closedHandle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"closed handle %O{closedHandle} not present in mapping")

        // Guard the guard: if this were empty, the test could not distinguish an exact MethodTable
        // from a canonicalised one.
        closedConcrete.Generics.IsEmpty |> shouldEqual false

        let internalHandle, registry =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                closedConcrete
                targetMethod
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        invokeGetMethodTable loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed closedHandle))
        )

    [<Test>]
    let ``GetMethodTable ignores the method instantiation bound to the handle`` () : unit =
        // A method's MethodTable is its *declaring type*'s; the method's own generic arguments live
        // on the MethodDesc, not the MethodTable. `Identity<int>` and `Identity<string>` therefore
        // share a MethodTable, and this pins that the handle's `MethodGenerics` do not leak into it.
        let source =
            """
public static class GenericMethodHolder
{
    public static T Identity<T>(T t)
    {
        return t;
    }
}
"""

        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "GetMethodTableGenericMethodAssembly" source

        let targetMethod = assembly |> findMethod "GenericMethodHolder" "Identity"

        let concretizeWith (methodGeneric : TypeDefn) (state : IlMachineState) =
            let state, concretized, _ =
                ExecutionConcretization.concretizeMethodWithTypeGenerics
                    loggerFactory
                    baseClassTypes
                    ImmutableArray.Empty
                    targetMethod
                    (Some (ImmutableArray.Create methodGeneric))
                    assembly.Name
                    ImmutableArray.Empty
                    state

            let internalHandle, registry =
                MethodHandleRegistry.getOrAllocateConcreteInternalHandle
                    baseClassTypes
                    state.ConcreteTypes
                    concretized
                    state.MethodHandles

            internalHandle,
            { state with
                MethodHandles = registry
            }

        let intHandle, state =
            concretizeWith (TypeDefn.PrimitiveType PrimitiveType.Int32) state

        let stringHandle, state =
            concretizeWith (TypeDefn.PrimitiveType PrimitiveType.String) state

        let expected =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes targetMethod.DeclaringType.Identity
            |> Option.defaultWith (fun () -> failwith "declaring type was not registered in ConcreteTypes")

        let expectedValue =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed expected))

        invokeGetMethodTable loggerFactory baseClassTypes (CliType.ValueType intHandle) state
        |> shouldEqual expectedValue

        invokeGetMethodTable loggerFactory baseClassTypes (CliType.ValueType stringHandle) state
        |> shouldEqual expectedValue

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
