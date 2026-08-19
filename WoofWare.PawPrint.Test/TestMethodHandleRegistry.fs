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

    /// Mint a bare `RuntimeMethodHandleInternal` for a method on a *closed* declaring type, the way
    /// the runtime's closed introduced-method path does. Tests that need the open-definition target
    /// call `MethodHandleRegistry.getOrAllocateInternalHandle` directly.
    let private internalHandleForClosed
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (concreteTypes : AllConcreteTypes)
        (declaringConcrete : ConcreteType<ConcreteTypeHandle>)
        (method : WoofWare.PawPrint.MethodInfo<'tyGen, GenericParamFromMetadata, TypeDefn>)
        (reg : MethodHandleRegistry)
        : CliValueType * MethodHandleRegistry
        =
        let target =
            AllConcreteTypes.findExistingConcreteType
                concreteTypes
                declaringConcrete.Identity
                declaringConcrete.Generics
            |> Option.defaultWith (fun () ->
                failwith $"declaring type %O{declaringConcrete} was not registered in ConcreteTypes"
            )
            |> RuntimeTypeHandleTarget.Closed

        MethodHandleRegistry.getOrAllocateInternalHandle
            baseClassTypes
            concreteTypes
            declaringConcrete.Assembly.FullName
            target
            method
            reg

    let private findMethod
        (declaringTypeName : string)
        (methodName : string)
        (assembly : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        assembly.Methods.Values
        |> Seq.find (fun method ->
            method.RequiredDeclaringType.Name = declaringTypeName
            && method.Name = methodName
        )

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
    let ``getOrAllocateStub names the stub inside the RuntimeMethodHandle`` () : unit =
        // `Delegate_FindMethodHandle` writes back a bare `IRuntimeMethodInfo`, so it takes the
        // stub address directly rather than unwrapping the `RuntimeMethodHandle` struct that
        // `getOrAllocateMethod` builds around it. The two must name one object: a guest that
        // reaches the same method through `Delegate.Method` and through `ldtoken` must see one
        // identity, not two.
        let loggerFactory, baseClassTypes, _assembly, _targetMethod, concretizedMethod, state =
            loadFixture ()

        let runtimeMethodInfoStubType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.RuntimeMethodInfoStub

        let stubAddr, registry, state =
            MethodHandleRegistry.getOrAllocateStub
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> IlMachineState.allocateManagedObject runtimeMethodInfoStubType fields state)
                concretizedMethod
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        assertRuntimeMethodInfoStub baseClassTypes state stubAddr

        // `allocateFreshStub` is the other half of the contract: it must *not* reuse, because
        // CoreCLR's `AllocateStubMethodInfo` allocates at every call site and a guest can compare
        // two of the resulting objects by reference.
        let freshAddr, registry, state =
            MethodHandleRegistry.allocateFreshStub
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> IlMachineState.allocateManagedObject runtimeMethodInfoStubType fields state)
                concretizedMethod
                state.MethodHandles

        freshAddr |> shouldNotEqual stubAddr
        assertRuntimeMethodInfoStub baseClassTypes state freshAddr

        let state =
            { state with
                MethodHandles = registry
            }

        // Asking again does not mint a second stub, in either direction.
        let stubAddrAgain, registry, state =
            MethodHandleRegistry.getOrAllocateStub
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> IlMachineState.allocateManagedObject runtimeMethodInfoStubType fields state)
                concretizedMethod
                state.MethodHandles

        stubAddrAgain |> shouldEqual stubAddr

        let state =
            { state with
                MethodHandles = registry
            }

        let methodHandle, _state =
            IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretizedMethod state

        match methodHandle with
        | CliType.ValueType vt ->
            match CliValueType.DereferenceField "m_value" vt with
            | CliType.ObjectRef (Some addr) -> addr |> shouldEqual stubAddr
            | other -> failwith $"Expected RuntimeMethodHandle.m_value to be an object ref, got %O{other}"
        | other -> failwith $"Expected RuntimeMethodHandle value type, got %O{other}"

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
            MetadataToken.MethodDef (MethodInfo.requireMetadata "test" targetMethod).Handle
            |> SourcedMetadataToken.make assembly.Name

        let state, whatWeDid =
            UnaryMetadataIlOp.execute
                loggerFactory
                baseClassTypes
                UnaryMetadataTokenIlOp.Ldtoken
                (MetadataOperand.FromMetadata token)
                state
                thread

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
            MetadataToken.MethodDef (MethodInfo.requireMetadata "test" targetMethod).Handle
            |> SourcedMetadataToken.make assembly.Name

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute
                    loggerFactory
                    baseClassTypes
                    UnaryMetadataTokenIlOp.Ldtoken
                    (MetadataOperand.FromMetadata token)
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
            MetadataToken.MethodDef (MethodInfo.requireMetadata "test" targetMethod).Handle
            |> SourcedMetadataToken.make assembly.Name

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute
                    loggerFactory
                    baseClassTypes
                    UnaryMetadataTokenIlOp.Ldtoken
                    (MetadataOperand.FromMetadata token)
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
            AllConcreteTypes.findExistingNonGenericConcreteType
                state.ConcreteTypes
                method.RequiredDeclaringType.Identity
            |> Option.defaultWith (fun () ->
                failwith
                    $"Closed ConcreteType for declaring type '%s{method.RequiredDeclaringType.Name}' was not registered in state.ConcreteTypes"
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
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

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
            | Some (MethodHandle.FromDynamic handle) ->
                failwith $"registry id %d{registryId} resolved to %O{handle}, but a metadata method was registered"
            | None -> failwith $"registry id %d{registryId} did not resolve"

        resolved.GetMethodDefinitionHandle ()
        |> shouldEqual (ComparableMethodDefinitionHandle.Make (MethodInfo.requireMetadata "test" targetMethod).Handle)

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
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

        let secondHandle, _ =
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod registry1

        let extractId (vt : CliValueType) : int64 =
            match CliValueType.DereferenceField "m_handle" vt |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id
            | other -> failwith $"Expected MethodRegistryHandle id, got %O{other}"

        extractId firstHandle |> shouldEqual (extractId secondHandle)

    [<Test>]
    let ``getOrAllocateInternalHandle accepts methods with method-generic parameters`` () : unit =
        // Regression test: registering a method with method-generic parameters must not route
        // through `concretizeMethodWithAllGenerics` with empty methodArgs, which crashes at
        // `methodArgs.[gp.SequenceNumber]`.
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
            assembly.TypeDefs.[targetMethod.RequiredDeclaringType.Definition.Get]

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
                (TypeDefn.FromDefinition (targetMethod.RequiredDeclaringType.Identity, stk))

        let declaringConcrete, _ =
            IlMachineState.tryGetConcreteTypeInfo state declaringHandle
            |> Option.defaultWith (fun () -> failwith "declaring type not registered after concretizeType")

        // The bug reproducer: this should not throw IndexOutOfRangeException.
        let internalHandle, _ =
            internalHandleForClosed
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

        // Some of these natives share a name and arity with a managed wrapper that forwards to them:
        // `HasMethodInstantiation(IRuntimeMethodInfo)` calls
        // `HasMethodInstantiation(RuntimeMethodHandleInternal)` (RuntimeHandles.cs:1241-1245). It is
        // the InternalCall these tests mean to drive -- the wrapper is ordinary IL and would not
        // reach a native handler at all -- so select on that rather than on arity alone.
        let rawMethod =
            runtimeMethodHandleType.Methods
            |> List.filter (fun method ->
                let facts = MethodInfo.requireMetadata "test" method

                method.Name = methodName
                && facts.Parameters.Length = 1
                && facts.ImplAttributes.HasFlag System.Reflection.MethodImplAttributes.InternalCall
            )
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

    let private invokeHasMethodInstantiation =
        invokeRuntimeMethodHandleFCall "HasMethodInstantiation"

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
        let declaringTypeInfo =
            assembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

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
                (TypeDefn.FromDefinition (method.RequiredDeclaringType.Identity, stk))

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
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

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
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

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
            AllConcreteTypes.findExistingNonGenericConcreteType
                state.ConcreteTypes
                targetMethod.RequiredDeclaringType.Identity
            |> Option.defaultWith (fun () -> failwith "declaring type was not registered in ConcreteTypes")

        let internalHandle, registry =
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

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
        // The instantiation must survive: the open generic definition is a different MethodTable
        // in CoreCLR too (IsGenericTypeDefinition = true).
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
            assembly.TypeDefs.[targetMethod.RequiredDeclaringType.Definition.Get]

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies declaringTypeInfo

        let closedDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (targetMethod.RequiredDeclaringType.Identity, stk),
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
            internalHandleForClosed baseClassTypes state.ConcreteTypes closedConcrete targetMethod state.MethodHandles

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
            AllConcreteTypes.findExistingNonGenericConcreteType
                state.ConcreteTypes
                targetMethod.RequiredDeclaringType.Identity
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

    /// Source carrying a non-generic method and a generic one, for the `HasMethodInstantiation`
    /// cases below.
    let private methodInstantiationFixtureSource : string =
        """
public static class InstantiationHolder
{
    public static int Plain()
    {
        return 1;
    }

    public static T Identity<T>(T t)
    {
        return t;
    }
}
"""

    [<Test>]
    let ``HasMethodInstantiation is false for a non-generic method`` () : unit =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "HasMethodInstantiationPlainAssembly" methodInstantiationFixtureSource

        let targetMethod = assembly |> findMethod "InstantiationHolder" "Plain"

        let state, declaringType =
            concretizeDeclaringType loggerFactory baseClassTypes assembly targetMethod state

        let internalHandle, registry =
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        invokeHasMethodInstantiation loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``HasMethodInstantiation is true for a generic method definition handle`` () : unit =
        // The unbound/typical form: the method declares `T`, and this handle binds nothing.
        // CoreCLR's `IMD_HasMethodInstantiation` (method.hpp:3524) returns TRUE for exactly this
        // case, and `RuntimeMethodInfo.IsGenericMethod` is the FCall verbatim -- an open generic
        // method is `IsGenericMethod = true`. Reading the FCall as "this handle has type arguments
        // bound" would answer false here.
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "HasMethodInstantiationDefinitionAssembly" methodInstantiationFixtureSource

        let targetMethod = assembly |> findMethod "InstantiationHolder" "Identity"

        let state, declaringType =
            concretizeDeclaringType loggerFactory baseClassTypes assembly targetMethod state

        let internalHandle, registry =
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

        // `getOrAllocateInternalHandle` mints the definition: empty MethodGenerics.
        match MethodHandleRegistry.resolveMethodFromId 1L registry with
        | Some (MethodHandle.FromMetadata identity) -> identity.GetMethodGenerics () |> shouldEqual []
        | Some (MethodHandle.FromDynamic handle) ->
            failwith $"registry id 1 resolved to %O{handle}, but a metadata method was registered"
        | None -> failwith "expected the freshly minted handle to resolve"

        let state =
            { state with
                MethodHandles = registry
            }

        invokeHasMethodInstantiation loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

    [<Test>]
    let ``HasMethodInstantiation is true for a bound generic method handle`` () : unit =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "HasMethodInstantiationBoundAssembly" methodInstantiationFixtureSource

        let targetMethod = assembly |> findMethod "InstantiationHolder" "Identity"

        let state, concretized, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                targetMethod
                (Some (ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Int32)))
                assembly.Name
                ImmutableArray.Empty
                state

        let internalHandle, registry =
            MethodHandleRegistry.getOrAllocateConcreteInternalHandle
                baseClassTypes
                state.ConcreteTypes
                concretized
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        invokeHasMethodInstantiation loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

    let private invokeGetMethodDef = invokeRuntimeMethodHandleFCall "GetMethodDef"

    [<Test>]
    let ``mdMethodDefNil is the token of a nil MethodDefinitionHandle`` () : unit =
        // The literal in `NativeRuntimeMethodHandle.mdMethodDefNil` is CoreCLR's `mdMethodDefNil`
        // (corhdr.h:1525). Deriving it a second way keeps the literal honest: a nil MethodDef
        // handle tokenises to the bare table tag, since a token is `(table <<< 24) ||| rid` and a
        // nil handle has rid 0.
        //
        // The value is deliberately NOT zero, which is what CoreCLR *stores* for such a method
        // (`SetMemberDef(0)`); `MergeToken` (method.hpp:148) ORs `mdtMethodDef` back in on the way
        // out. `MdToken.IsNullToken` masks the table byte off, so no BCL caller can tell the two
        // apart -- this assertion, and the dynamic-method test below, are the only things that can.
        let nilHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit (
                Unchecked.defaultof<System.Reflection.Metadata.MethodDefinitionHandle>
            )

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken nilHandle
        |> shouldEqual NativeRuntimeMethodHandle.mdMethodDefNil

        NativeRuntimeMethodHandle.mdMethodDefNil |> shouldEqual 0x06000000

    /// Every method the host CLR says the type declares, paired with the MetadataToken the host
    /// reports for it. `GetMethods` alone is not that -- it never returns constructors -- so the
    /// instance constructors and the class constructor are fetched separately.
    let private hostDeclaredMethods (t : System.Type) : System.Reflection.MethodBase list =
        let flags =
            System.Reflection.BindingFlags.DeclaredOnly
            ||| System.Reflection.BindingFlags.Instance
            ||| System.Reflection.BindingFlags.Static
            ||| System.Reflection.BindingFlags.Public
            ||| System.Reflection.BindingFlags.NonPublic

        [
            yield! (t.GetMethods flags |> Seq.cast<System.Reflection.MethodBase>)
            yield! (t.GetConstructors flags |> Seq.cast<System.Reflection.MethodBase>)
        ]

    /// PawPrint's `methodDefToken` for every method the given corelib type declares, keyed by the
    /// method's metadata name.
    ///
    /// Deliberately routed through the real minting path (`getOrAllocateInternalHandle`) rather
    /// than fabricating a `MethodHandle`, so that what is measured is the token a guest could
    /// actually obtain, registry plumbing included.
    let private pawPrintTokensOfCorelibType
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (namespaceName : string)
        (typeName : string)
        : (WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * int) list
        =
        let typeInfo = requiredTopLevelType baseClassTypes.Corelib namespaceName typeName

        match typeInfo.Methods with
        | [] -> failwith $"corelib type %s{namespaceName}.%s{typeName} declares no methods"
        | firstMethod :: _ ->

        let state, declaringType =
            concretizeDeclaringType loggerFactory baseClassTypes baseClassTypes.Corelib firstMethod state

        let mutable registry = state.MethodHandles

        typeInfo.Methods
        |> List.map (fun method ->
            let internalHandle, reg =
                internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType method registry

            registry <- reg

            let registryId =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal "test" (CliType.ValueType internalHandle)
                |> Option.defaultWith (fun () -> failwith $"minting %s{method.Name} produced a null handle")

            let methodHandle =
                MethodHandleRegistry.resolveMethodFromId registryId registry
                |> Option.defaultWith (fun () -> failwith $"registry id %d{registryId} did not resolve")

            method, NativeRuntimeMethodHandle.methodDefToken methodHandle
        )

    /// Corelib types chosen so that between them they cover the MethodDesc flavours whose
    /// `GetMemberDef` answers could plausibly differ: instance and static methods, instance
    /// constructors, a class constructor, property and operator accessors, explicit interface
    /// implementations, generic methods, and methods on both a reference type and a value type.
    ///
    /// A hand-picked list of individual *methods* would be weaker: it can only cover flavours
    /// somebody thought of. Sweeping every MethodDef row these types declare cannot miss one.
    let private tokenSweepCorpus : obj array list =
        [
            // A reference type with many overloads, operators, explicit interface
            // implementations, generic methods, a class constructor and several instance
            // constructors.
            [| box "System" ; box "String" |]
            // A value type, whose methods CoreCLR duplicates into unboxing stubs.
            [| box "System" ; box "Guid" |]
            // Properties, operators, and `IComparable`/`IEquatable` implementations.
            [| box "System" ; box "Version" |]
            // Static-only, so every row is a static method with no `this`.
            [| box "System" ; box "Math" |]
        ]

    [<TestCaseSource(nameof tokenSweepCorpus)>]
    let ``GetMethodDef agrees with the host CLR for every method a corelib type declares``
        (namespaceName : string, typeName : string)
        : unit
        =
        // PawPrint reads the host's own corelib (`typeof<obj>.Assembly.Location`), so the host CLR
        // is an independent implementation reading the *same* metadata -- an outside oracle rather
        // than a second derivation of the thing under test.
        //
        // The comparison is deliberately made in the token->row direction as well as by set
        // equality. Set equality alone would accept a permutation of the type's tokens among
        // themselves; `Module.ResolveMethod`, which is the host's independent token->row map,
        // pins each token to a row with the right name.
        let loggerFactory, baseClassTypes, _, state =
            loadAssemblyFromSource "TokenSweepAssembly" "public class Unused { }"

        let hostType =
            typeof<obj>.Assembly.GetType ($"%s{namespaceName}.%s{typeName}", true)

        let measured =
            pawPrintTokensOfCorelibType loggerFactory baseClassTypes state namespaceName typeName

        // A sweep that swept nothing would pass every assertion below.
        measured.Length |> shouldBeGreaterThan 10

        let hostTokens =
            hostDeclaredMethods hostType |> List.map _.MetadataToken |> Set.ofList

        let pawPrintTokens = measured |> List.map snd

        // Distinct rows get distinct tokens: a token is a row identity, so a duplicate would mean
        // two methods had collapsed onto one row.
        pawPrintTokens
        |> List.length
        |> shouldEqual (pawPrintTokens |> Set.ofList |> Set.count)

        // Exactly the rows the host says this type declares -- neither a token from some other
        // type nor a missing one.
        Set.ofList pawPrintTokens |> shouldEqual hostTokens

        // ... and each token names the row it came from, not merely *a* row of this type.
        for method, token in measured do
            let resolved = hostType.Module.ResolveMethod token

            if isNull resolved then
                failwith $"host CLR could not resolve token 0x%08x{token}, produced for %s{method.Name}"

            resolved.Name |> shouldEqual method.Name

            if resolved.DeclaringType <> hostType then
                failwith
                    $"token 0x%08x{token} for %s{method.Name} resolved to a method on %O{resolved.DeclaringType}, not %O{hostType}"

    [<Test>]
    let ``GetMethodDef pushes the MethodDef token through the FCall`` () : unit =
        // The sweep above exercises `methodDefToken`; this pins that the FCall arm calls it and
        // pushes the answer as an Int32, which is the half a pure-function test cannot see.
        let loggerFactory, baseClassTypes, _, targetMethod, _, state = loadFixture ()
        let declaringType = findDeclaringConcreteType state targetMethod

        let internalHandle, registry =
            internalHandleForClosed baseClassTypes state.ConcreteTypes declaringType targetMethod state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        let expected =
            let handle : System.Reflection.Metadata.EntityHandle =
                System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit (
                    MethodInfo.requireMetadata "test" targetMethod |> fun facts -> facts.Handle
                )

            System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

        // Sanity: the fixture's method really is a MethodDef row with a non-nil rid, so an
        // implementation returning the nil token would not accidentally satisfy this.
        expected |> shouldNotEqual NativeRuntimeMethodHandle.mdMethodDefNil

        invokeGetMethodDef loggerFactory baseClassTypes (CliType.ValueType internalHandle) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim expected))

    [<Test>]
    let ``GetMethodDef fails loudly for an unregistered handle id`` () : unit =
        // CoreCLR dereferences the MethodDesc* and asserts non-null. An id the registry never
        // minted is a PawPrint contract violation, and answering some plausible token for it would
        // send the guest on to enumerate the parameters of a method that does not exist.
        let loggerFactory, baseClassTypes, _, _, _, state = loadFixture ()

        let bogusHandle =
            MethodHandleRegistry.internalHandleFromId baseClassTypes state.ConcreteTypes 999L

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetMethodDef loggerFactory baseClassTypes (CliType.ValueType bogusHandle) state
                |> ignore
            )

        ex.Message
        |> shouldContainText "RuntimeMethodHandle.GetMethodDef: registry id 999 did not resolve"

    [<Test>]
    let ``the host CLR reports one MethodDef token for every instantiation of a method`` () : unit =
        // The CoreCLR fact `methodDefToken` relies on, asserted where it can be *observed*: on the
        // host runtime, whose behaviour PawPrint is copying. `MethodDesc::GetMemberDef` reads a
        // stored token, and `InstantiatedMethodDesc::CreateMethodDesc` (genmeth.cpp:85,134) copies
        // the generic definition's token onto every instantiation it builds.
        //
        // The PawPrint-side counterpart of this is *structurally* true rather than testable: two
        // handles for the same method differing only in instantiation carry one and the same
        // `ComparableMethodDefinitionHandle`, and `methodDefToken` is a pure function of it, so
        // no implementation of it could make them disagree. Asserting it here instead is the
        // honest placement -- it pins the claim against something that could refute it.
        // `typedefof<List<int>>` is `List<>`, the generic type definition.
        let openAdd : System.Reflection.MethodInfo =
            typedefof<System.Collections.Generic.List<int>>.GetMethod "Add"

        let closedAdd : System.Reflection.MethodInfo =
            typeof<System.Collections.Generic.List<int>>.GetMethod "Add"

        let otherClosedAdd : System.Reflection.MethodInfo =
            typeof<System.Collections.Generic.List<string>>.GetMethod "Add"

        closedAdd.MetadataToken |> shouldEqual openAdd.MetadataToken
        otherClosedAdd.MetadataToken |> shouldEqual openAdd.MetadataToken

    /// A generic type with two constructors and a plain method, for the open-definition tests.
    let private genericBoxSource =
        """
public sealed class Box<T>
{
    public T Item;
    public Box() { }
    public Box(T t) { Item = t; }
    public int Plain() { return 1; }
}
"""

    /// The declaring type of `Box<T>`'s methods, as both an open generic definition and the closed
    /// `Box<int>` instantiation, so a test can mint against either.
    let private boxTargets () =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "OpenGenericBoxAssembly" genericBoxSource

        let ctors =
            assembly.Methods.Values
            |> Seq.filter (fun method -> method.RequiredDeclaringType.Name = "Box`1" && method.Name = ".ctor")
            |> Seq.sortBy (fun method -> method.Signature.ParameterTypes.Length)
            |> List.ofSeq

        ctors.Length |> shouldEqual 2

        let declaringIdentity = ctors.Head.RequiredDeclaringType.Identity

        let openTarget = RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringIdentity

        // `Box<int>`: the same TypeDef, instantiated. Concretized through the ordinary path so the
        // handle is the one the runtime would hold.
        let state, intHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Int32.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.PrimitiveType PrimitiveType.Int32)

        // `Box<int>`, spelled as an instantiation whose single argument is the type-generic
        // parameter, substituted from the supplied vector -- the way a signature mentioning
        // `Box<T>` inside a `T = int` frame concretizes.
        let state, closedHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assembly.Name
                (ImmutableArray.Create intHandle)
                ImmutableArray.Empty
                (TypeDefn.GenericInstantiation (
                    TypeDefn.FromDefinition (declaringIdentity, System.Reflection.Metadata.SignatureTypeKind.Class),
                    ImmutableArray.Create (TypeDefn.GenericTypeParameter 0)
                ))

        baseClassTypes, assembly, ctors, openTarget, RuntimeTypeHandleTarget.Closed closedHandle, state

    /// The registry id carried by a bare `RuntimeMethodHandleInternal`, as the runtime reads it.
    let private registryIdOf (internalHandle : CliValueType) : int64 =
        match
            CliValueType.DereferenceField "m_handle" internalHandle
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id
        | other -> failwith $"Expected MethodRegistryHandle id in m_handle, got %O{other}"

    [<Test>]
    let ``an open generic definition and its instantiation mint distinct handles`` () : unit =
        // The MethodDef row is shared between `Box<>`'s ctor and `Box<int>`'s -- CoreCLR copies the
        // definition's token onto every instantiation -- so the declaring type is the only thing
        // that separates these two identities. Standing a closed instantiation in for the open
        // definition would collapse them onto one registry id, and the guest can see that
        // (sourcesPure/ReflectionOpenGenericConstructors.cs check 7); this pins it at the registry,
        // where the dedup map actually lives.
        let baseClassTypes, assembly, ctors, openTarget, closedTarget, state = boxTargets ()

        let ctor = ctors.Head

        let openHandle, reg =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                assembly.Name.FullName
                openTarget
                ctor
                state.MethodHandles

        let closedHandle, reg =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                assembly.Name.FullName
                closedTarget
                ctor
                reg

        let openId = registryIdOf openHandle
        let closedId = registryIdOf closedHandle

        closedId |> shouldNotEqual openId

        // Each id resolves back to the declaring target it was minted with, rather than to
        // whichever was minted last.
        match MethodHandleRegistry.resolveMethodFromId openId reg with
        | Some (MethodHandle.FromMetadata identity) ->
            match identity.GetDeclaringType () with
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ()
            | other -> failwithf "expected OpenGenericTypeDefinition, got %A" other
        | other -> failwithf "expected a metadata handle, got %A" other

        match MethodHandleRegistry.resolveMethodFromId closedId reg with
        | Some (MethodHandle.FromMetadata identity) ->
            match identity.GetDeclaringType () with
            | RuntimeTypeHandleTarget.Closed _ -> ()
            | other -> failwithf "expected Closed, got %A" other
        | other -> failwithf "expected a metadata handle, got %A" other

    [<Test>]
    let ``minting twice against an open generic definition dedups to one id`` () : unit =
        let baseClassTypes, assembly, ctors, openTarget, _closedTarget, state =
            boxTargets ()

        let ctor = ctors.Head

        let first, reg =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                assembly.Name.FullName
                openTarget
                ctor
                state.MethodHandles

        let second, _ =
            MethodHandleRegistry.getOrAllocateInternalHandle
                baseClassTypes
                state.ConcreteTypes
                assembly.Name.FullName
                openTarget
                ctor
                reg

        registryIdOf second |> shouldEqual (registryIdOf first)

    [<Test>]
    let ``distinct methods on one open generic definition never share an id`` () : unit =
        // Injectivity over (declaringTarget, methodDef): the definition's two constructors and its
        // ordinary method are three identities, so three ids.
        let baseClassTypes, assembly, ctors, openTarget, _closedTarget, state =
            boxTargets ()

        let plain =
            assembly.Methods.Values
            |> Seq.find (fun method -> method.RequiredDeclaringType.Name = "Box`1" && method.Name = "Plain")

        let methods = ctors @ [ plain ]

        let ids, _ =
            (([], state.MethodHandles), methods)
            ||> List.fold (fun (ids, reg) method ->
                let handle, reg =
                    MethodHandleRegistry.getOrAllocateInternalHandle
                        baseClassTypes
                        state.ConcreteTypes
                        assembly.Name.FullName
                        openTarget
                        method
                        reg

                registryIdOf handle :: ids, reg
            )

        ids |> List.distinct |> List.length |> shouldEqual methods.Length

    [<Test>]
    [<TestCase "Box`1">]
    [<TestCase "HasNestedGeneric`2">]
    let ``the open-definition method list matches the MethodDef rows System.Reflection.Metadata reports``
        (typeName : string)
        : unit
        =
        // Outside oracle: the expectation comes from `System.Reflection.Metadata` reading the same
        // image, not from PawPrint's own graph, so a walk that is wrong-but-self-consistent (say,
        // one that dropped constructors, or included inherited methods) fails here.
        let source =
            """
public sealed class Box<T>
{
    public T Item;
    public Box() { }
    public Box(T t) { Item = t; }
    public int Plain() { return 1; }
}

public class HasNestedGeneric<TKey, TValue>
{
    public TKey K;
    public TValue V;
    public HasNestedGeneric() { }
    public virtual TValue Get(TKey k) { return V; }
    public static int Count() { return 0; }
}
"""

        let image =
            Roslyn.compileAssembly
                "OpenGenericOracleAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let typeInfo = assembly.TypeDefs.Values |> Seq.find (fun t -> t.Name = typeName)

        // Read the list through the dispatcher the enumerator actually calls, not off `TypeInfo`
        // directly: a dispatcher that re-ordered or filtered what it was given would otherwise pass.
        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory typeof<obj>.Assembly.Location

        let baseClassTypes = Corelib.getBaseTypes corelib

        let state =
            let initial = IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            let state = initial.WithLoadedAssembly corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        let _, _, walked =
            NativeRuntimeTypeHelpers.introducedMethodsOf
                "test"
                state
                (RuntimeTypeHandleTarget.OpenGenericTypeDefinition typeInfo.Identity)
            |> Option.defaultWith (fun () ->
                failwith $"open generic definition %s{typeName} reported no introduced methods"
            )

        let pawPrintTokens =
            walked
            |> List.map (fun method ->
                match method.TryMetadata with
                | Some facts ->
                    System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken (
                        MethodDefinitionHandle.op_Implicit facts.Handle : EntityHandle
                    )
                | None -> failwith $"method %s{method.Name} has no MethodDef row"
            )

        use oracleStream = new MemoryStream (image)
        use peReader = new System.Reflection.PortableExecutable.PEReader (oracleStream)
        let reader = peReader.GetMetadataReader ()

        let oracleTokens =
            reader.TypeDefinitions
            |> Seq.map reader.GetTypeDefinition
            |> Seq.filter (fun t -> reader.GetString t.Name = typeName)
            |> Seq.exactlyOne
            |> fun t -> t.GetMethods ()
            |> Seq.map (fun (h : MethodDefinitionHandle) ->
                System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken (
                    MethodDefinitionHandle.op_Implicit h : EntityHandle
                )
            )
            |> List.ofSeq

        // Order matters: the enumerator surfaces method-table slots in metadata order, and
        // sourcesPure/ReflectionOpenGenericConstructors.cs asserts ascending tokens for that reason.
        pawPrintTokens |> shouldEqual oracleTokens
        oracleTokens |> shouldNotEqual []

    [<Test>]
    let ``minting refuses a declaring type that cannot own a metadata method`` () : unit =
        // The mint-time chokepoint is what lets consumers of `GetDeclaringType ()` treat the other
        // `RuntimeTypeHandleTarget` arms as contract violations rather than as cases to serve. A
        // generic parameter is a TypeVarTypeDesc: methods live on the type that mentions it.
        let baseClassTypes, assembly, ctors, _openTarget, _closedTarget, state =
            boxTargets ()

        let declaringIdentity = ctors.Head.RequiredDeclaringType.Identity

        let exn =
            Assert.Throws<exn> (fun () ->
                MethodHandleRegistry.getOrAllocateInternalHandle
                    baseClassTypes
                    state.ConcreteTypes
                    assembly.Name.FullName
                    (RuntimeTypeHandleTarget.GenericParameter (declaringIdentity, 0))
                    ctors.Head
                    state.MethodHandles
                |> ignore
            )

        exn.Message
        |> shouldContainText "declaring type must be Closed or OpenGenericTypeDefinition"
