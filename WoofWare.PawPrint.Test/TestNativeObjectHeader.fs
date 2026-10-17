namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Direct calls of .NET 11's fat-lock natives, `ObjectHeader_GetOrCreateLockObject` and
/// `ObjectHeader.GetLockHandleIfExists`, for the states a guest reaches too seldom to test through
/// one: a second thread arriving while another is still inflating the same object, which CoreCLR's
/// sync-block spin lock makes it wait out, and the header words PawPrint never writes. The objects
/// stand in for a locked object and its `Lock`s; inflating them needs no more than their
/// addresses until `Lock.InitializeForMonitor` runs, which these states never call.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeObjectHeader =

    let private trivialSource : string =
        """
public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private prepareProgram (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : Program.PreparedProgram =
        let image =
            Roslyn.compileAssemblyWithResources "ObjectHeaderTest" OutputKind.ConsoleApplication [] [] [ trivialSource ]

        use peImage = new MemoryStream (image)

        match
            Program.prepare
                loggerFactory
                (Some "ObjectHeaderTest.cs")
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected program to be ready before Main, got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    /// The `System.Threading.ObjectHeader` method `choose` picks, concretised.
    let private objectHeaderMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (choose : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> -> bool)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectHeader =
            requiredTopLevelType baseClassTypes.Corelib "System.Threading" "ObjectHeader"

        let raw =
            match objectHeader.Methods |> List.filter choose with
            | [ method ] -> method
            | methods -> failwith $"expected one matching ObjectHeader method, found %d{methods.Length}"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                raw
                None
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        state, objectHeader, method

    /// A fresh `object[1]`, standing in for an object of the guest's.
    let private allocateObject (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.TypeSystem.ConcreteTypes baseClassTypes.Object

        IlMachineState.allocateArray
            (ConcreteTypeHandle.OneDimArrayZero objectHandle)
            (fun () -> CliType.ObjectRef None)
            1
            state

    /// `ObjectHandleOnStack.Create(ref local)` over a local holding `contents`, and that local.
    let private objectHandleOnStack
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : ManagedHeapAddress)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * IlMachineState
        =
        let local, state = HostStartupCall.allocateReferenceLocal contents state

        let handleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (handleType.Identity, SignatureTypeKind.ValueType))

        match IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle with
        | CliType.ValueType vt, state ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed local)) vt
            |> CliType.ValueType,
            local,
            state
        | other, _ -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

    /// What a native call of `method` on the entry thread with `arguments` does, through the
    /// handlers .NET 11 dispatches to.
    let private call
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (objectHeader : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (arguments : CliType list)
        (state : IlMachineState)
        : NativeHandlerResult
        =
        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = prepared.BaseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction =
                    { state.ThreadState.[prepared.EntryThread].MethodState with
                        ExecutingMethod = method
                        Arguments = ImmutableArray.CreateRange arguments
                        EvaluationStack = EvalStack.Empty
                    }
                TargetAssembly = prepared.BaseClassTypes.Corelib
                TargetType = objectHeader
            }

        NativeDispatch.tryExecute ctx
        |> Option.defaultWith (fun () -> failwith $"no handler matched %s{method.Name}")

    let private isGetOrCreateLockObject (m : MethodInfo<_, _, _>) : bool =
        match m.TryNativeImport with
        | Some import ->
            import.ModuleName = "QCall"
            && import.EntryPointName = "ObjectHeader_GetOrCreateLockObject"
        | None -> false

    let private isGetLockHandleIfExists (m : MethodInfo<_, _, _>) : bool = m.Name = "GetLockHandleIfExists"

    let private requireNet11 () : unit =
        if FrameworkUnderTest.runtime () <> EmulatedRuntime.Net11 then
            Assert.Ignore "Only .NET 11's CoreLib declares the fat lock's natives"

    let private read (prepared : Program.PreparedProgram) (local : ManagedPointerSource) (state : IlMachineState) =
        IlMachineState.readManagedByref prepared.BaseClassTypes state (ManagedPointerSource.requireAddressed local)

    [<Test>]
    let ``a thread arriving while another inflates the object yields, then takes the Lock that one installs``
        ()
        : unit
        =
        requireNet11 ()
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory
        let bct = prepared.BaseClassTypes

        let state, objectHeader, getOrCreate =
            objectHeaderMethod loggerFactory bct isGetOrCreateLockObject prepared.State

        let state, _, getHandle =
            objectHeaderMethod loggerFactory bct isGetLockHandleIfExists state

        let obj, state = allocateObject bct state
        let installed, state = allocateObject bct state
        let offered, state = allocateObject bct state

        // Another thread is inflating `obj`, with `installed` as its Lock.
        let inflater =
            let (ThreadId entry) = prepared.EntryThread
            ThreadId (entry + 1)

        let handle, gcHandles =
            state.GcHandles
            |> GcHandleRegistry.allocate GcHandleKind.Normal (GcHandleOwner.MonitorLock obj) (Some installed)

        let _, heap = ManagedHeap.beginInflation obj inflater handle state.ManagedHeap

        let state =
            { state with
                GcHandles = gcHandles
                ManagedHeap = heap
            }

        let objArg, _, state = objectHandleOnStack loggerFactory bct obj state
        let lockArg, lockLocal, state = objectHandleOnStack loggerFactory bct offered state

        // The handle is not the sync block's until the inflation finishes.
        match call loggerFactory prepared objectHeader getHandle [ CliType.ObjectRef (Some obj) ] state with
        | NativeHandlerResult.Completed (after, _) ->
            IlMachineState.peekEvalStack prepared.EntryThread after
            |> shouldEqual (Some (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)))
        | other -> failwith $"GetLockHandleIfExists: %O{other}"

        let yielded =
            match call loggerFactory prepared objectHeader getOrCreate [ objArg ; lockArg ] state with
            | NativeHandlerResult.YieldedRetainingFrame (after, _) -> after
            | other -> failwith $"expected the arriving thread to yield and re-issue the call, got %O{other}"

        ManagedHeap.tryGetMonitorLock obj yielded.ManagedHeap
        |> shouldEqual (Some (MonitorLock.Inflating (inflater, handle)))

        read prepared lockLocal yielded
        |> shouldEqual (CliType.ObjectRef (Some offered))

        let finished =
            { yielded with
                ManagedHeap = ManagedHeap.completeInflation obj inflater yielded.ManagedHeap
            }

        match call loggerFactory prepared objectHeader getOrCreate [ objArg ; lockArg ] finished with
        | NativeHandlerResult.Completed (after, _) ->
            read prepared lockLocal after
            |> shouldEqual (CliType.ObjectRef (Some installed))

            ManagedHeap.getHeaderWord obj after.ManagedHeap
            |> shouldEqual (ManagedHeap.getHeaderWord obj finished.ManagedHeap)
        | other -> failwith $"expected the re-issued call to complete, got %O{other}"

        match call loggerFactory prepared objectHeader getHandle [ CliType.ObjectRef (Some obj) ] finished with
        | NativeHandlerResult.Completed (after, _) ->
            IlMachineState.peekEvalStack prepared.EntryThread after
            |> shouldEqual (Some (EvalStackValue.NativeInt (NativeIntSource.gcHandlePtr handle)))
        | other -> failwith $"GetLockHandleIfExists: %O{other}"

    [<Test>]
    let ``inflating an unlocked object installs the offered Lock and a sync block index`` () : unit =
        requireNet11 ()
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory
        let bct = prepared.BaseClassTypes

        let state, objectHeader, getOrCreate =
            objectHeaderMethod loggerFactory bct isGetOrCreateLockObject prepared.State

        let obj, state = allocateObject bct state
        let offered, state = allocateObject bct state

        // A GC bit the VM leaves alone, and no thin lock.
        let state =
            { state with
                ManagedHeap = ManagedHeap.setHeaderWord obj 0x20000000 state.ManagedHeap
            }

        let objArg, _, state = objectHandleOnStack loggerFactory bct obj state
        let lockArg, lockLocal, state = objectHandleOnStack loggerFactory bct offered state

        match call loggerFactory prepared objectHeader getOrCreate [ objArg ; lockArg ] state with
        | NativeHandlerResult.Completed (after, _) ->
            read prepared lockLocal after |> shouldEqual (CliType.ObjectRef (Some offered))

            match ManagedHeap.tryGetMonitorLock obj after.ManagedHeap with
            | Some (MonitorLock.Inflated handle) ->
                GcHandleRegistry.target handle after.GcHandles |> shouldEqual (Some offered)
            | other -> failwith $"expected an inflated sync block, got %O{other}"

            ManagedHeap.getHeaderWord obj after.ManagedHeap
            |> shouldEqual (0x20000000 ||| 0x08000000 ||| 1)
        | other -> failwith $"expected inflating an unlocked object to complete, got %O{other}"

    [<TestCase(0x08000000)>]
    [<TestCase(0x0C000123)>]
    [<TestCase(0x10000000)>]
    let ``a header word holding what PawPrint never writes is refused`` (word : int) : unit =
        requireNet11 ()
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory
        let bct = prepared.BaseClassTypes

        let state, objectHeader, getOrCreate =
            objectHeaderMethod loggerFactory bct isGetOrCreateLockObject prepared.State

        let obj, state = allocateObject bct state
        let offered, state = allocateObject bct state

        let state =
            { state with
                ManagedHeap = ManagedHeap.setHeaderWord obj word state.ManagedHeap
            }

        let objArg, _, state = objectHandleOnStack loggerFactory bct obj state
        let lockArg, _, state = objectHandleOnStack loggerFactory bct offered state

        let e =
            Assert.Throws<exn> (fun () ->
                call loggerFactory prepared objectHeader getOrCreate [ objArg ; lockArg ] state
                |> ignore
            )

        e.Message |> shouldContainText "PawPrint never writes those"
