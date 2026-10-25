namespace WoofWare.PawPrint

open System.Collections.Immutable

/// .NET 11's fat lock: the natives `System.Threading.ObjectHeader.GetLockObject` calls when an
/// object's thin lock gives way to a `System.Threading.Lock`, on contention, on `Monitor.Wait`
/// and `Pulse`, and on a recursion too deep for the header word. .NET 10's CoreLib declares
/// neither; its monitor is `NativeMonitor`'s.
[<RequireQualifiedAccess>]
module NativeObjectHeader =

    let private nativeNull : CliType =
        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

    /// The object an `ObjectHandleOnStack` argument holds, and where it holds it.
    let private objectOfHandleOnStack
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (argName : string)
        (arg : CliType)
        : ManagedHeapAddress * ManagedPointerSource
        =
        let ptr = NativeCall.objectHandleOnStackTarget operation state argName arg

        match IlMachineState.readManagedByref baseClassTypes state (ManagedPointerSource.requireAddressed ptr) with
        | CliType.ObjectRef (Some addr) -> addr, ptr
        | CliType.ObjectRef None -> failwith $"%s{operation}: %s{argName} holds a null reference"
        | other -> failwith $"%s{operation}: expected an object reference in %s{argName}, got %O{other}"

    /// `ObjectHeader.GetLockHandleIfExists(object)`, the FCall `ObjectHeader_GetLockHandleIfExists`.
    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Threading",
          "ObjectHeader",
          "GetLockHandleIfExists",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "ObjectHeader.GetLockHandleIfExists"

            let obj =
                match instruction.Arguments.[0] with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"%s{operation}: expected a non-null object, got %O{other}"

            // CoreCLR answers the sync block's `m_Lock`, which is null until the inflation that
            // creates it has finished, as it is while `Inflating`.
            let state =
                match ManagedHeap.tryGetMonitorLock obj state.ManagedHeap with
                | Some (MonitorLock.Inflated handle) -> NativeCall.pushGcHandleAddress handle ctx.Thread state
                | Some (MonitorLock.Inflating _)
                | None -> IlMachineState.pushToEvalStack nativeNull ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None

    /// `ObjectHeader.GetOrCreateLockObject(ObjectHandleOnStack obj, ObjectHandleOnStack lockObj)`,
    /// the QCall `ObjectHeader_GetOrCreateLockObject`.
    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "ObjectHeader_GetOrCreateLockObject",
          "System.Private.CoreLib",
          "System.Threading",
          "ObjectHeader",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "ObjectHandleOnStack",
                                                        objGenerics)
            CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "ObjectHandleOnStack",
                                                        lockGenerics) ],
          MethodReturnType.Void when objGenerics.IsEmpty && lockGenerics.IsEmpty ->
            let operation = "ObjectHeader_GetOrCreateLockObject"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let obj, _ =
                objectOfHandleOnStack operation ctx.BaseClassTypes state "obj" instruction.Arguments.[0]

            let offered, lockCell =
                objectOfHandleOnStack operation ctx.BaseClassTypes state "lockObj" instruction.Arguments.[1]

            // CoreCLR writes the object's `Lock` into `lockObj`: the one it was offered if that
            // became the sync block's, and otherwise the one already there.
            let answer (handle : GcHandleAddress) (state : IlMachineState) : NativeHandlerResult option =
                let target =
                    GcHandleRegistry.target handle state.GcHandles
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: the sync block's handle %O{handle} holds no Lock"
                    )

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        (ManagedPointerSource.requireAddressed lockCell)
                        (CliType.ObjectRef (Some target))

                NativeHandlerResult.completed state |> Some

            // The handler runs in two phases when the thin lock is held, joined by the call of
            // CoreLib's `Lock.InitializeForMonitor`, as `SyncBlock::TryUpgradeThinLockToFullLock`
            // calls it: first entry (an empty eval stack) begins the inflation and makes the
            // call, leaving the handle and the call's `Exception*` slot as markers beneath its
            // arguments; re-entry finds them and finishes.
            match instruction.EvaluationStack.Values with
            | [ EvalStackValue.ManagedPointer slot ; EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, 0L)) ] ->
                let _, state = IlMachineState.popEvalStack ctx.Thread state
                let _, state = IlMachineState.popEvalStack ctx.Thread state

                match
                    IlMachineState.readManagedByref
                        ctx.BaseClassTypes
                        state
                        (ManagedPointerSource.requireAddressed slot)
                with
                | CliType.ObjectRef None -> ()
                | caught ->
                    // `InitializeForMonitor` only stores three fields, so nothing short of a
                    // runtime failure can raise there; CoreCLR would rethrow it from this QCall.
                    failwith
                        $"TODO: %s{operation}: Lock.InitializeForMonitor stored %O{caught} in its exception slot; rethrowing it from the QCall is not modelled"

                let state =
                    { state with
                        ManagedHeap = ManagedHeap.completeInflation obj ctx.Thread state.ManagedHeap
                    }

                answer handle state
            | [] ->
                match ManagedHeap.tryGetMonitorLock obj state.ManagedHeap with
                | Some (MonitorLock.Inflated handle) -> answer handle state
                | Some (MonitorLock.Inflating (inflater, _)) when inflater = ctx.Thread ->
                    failwith $"%s{operation}: thread %O{ctx.Thread} reached its own inflation of %O{obj} from inside it"
                | Some (MonitorLock.Inflating _) ->
                    // Another thread holds the sync block's spin lock while its
                    // `InitializeForMonitor` runs; CoreCLR spins, giving the processor away,
                    // until it is released.
                    NativeHandlerResult.yieldedRetainingFrame state |> Some
                | None ->

                let handle, gcHandles =
                    state.GcHandles
                    |> GcHandleRegistry.allocate GcHandleKind.Normal (GcHandleOwner.MonitorLock obj) (Some offered)

                let index, heap = ManagedHeap.beginInflation obj ctx.Thread handle state.ManagedHeap

                let owner, heap =
                    match ObjectHeaderInflation.inflate index (ManagedHeap.getHeaderWord obj heap) with
                    | HeaderWordInflation.Unmodelled word ->
                        failwith
                            $"TODO: %s{operation}: the header word of %O{obj} is 0x%08x{word}, which holds a hash code, a sync block index or the VM's spin lock; PawPrint never writes those, so it does not model inflating from them"
                    | HeaderWordInflation.FromThinLock (owner, word) -> owner, ManagedHeap.setHeaderWord obj word heap

                let state =
                    { state with
                        GcHandles = gcHandles
                        ManagedHeap = heap
                    }

                match owner with
                | None ->
                    let state =
                        { state with
                            ManagedHeap = ManagedHeap.completeInflation obj ctx.Thread state.ManagedHeap
                        }

                    answer handle state
                | Some owner ->

                let purpose = "hand an inflating thin lock's owner to the object's new Lock"

                let initializeForMonitor =
                    CorelibEntryPoint.findCorelibStaticMethod
                        ctx.BaseClassTypes
                        "System.Threading"
                        "Lock"
                        "InitializeForMonitor"
                        4
                        purpose

                let state, concretizedResult =
                    ExecutionConcretization.concretizeMethodWithTypeGenerics
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ImmutableArray.Empty
                        initializeForMonitor
                        None
                        ctx.BaseClassTypes.Corelib.DefinitionFullName
                        ImmutableArray.Empty
                        state

                let concretized, _declaringType = BindingStop.bound concretizedResult

                let pLock, state = HostStartupCall.allocateReferenceLocal offered state
                let slot, state = HostStartupCall.allocateExceptionSlot state

                let state =
                    state
                    |> NativeCall.pushGcHandleAddress handle ctx.Thread
                    |> IlMachineState.pushToEvalStack
                        (CliType.RuntimePointer (CliRuntimePointer.Managed slot.Pointer))
                        ctx.Thread
                    |> IlMachineState.pushToEvalStack
                        (CliType.RuntimePointer (CliRuntimePointer.Managed pLock))
                        ctx.Thread
                    |> IlMachineState.pushToEvalStack
                        (CliType.Numeric (CliNumericType.Int32 owner.ManagedThreadId))
                        ctx.Thread
                    |> IlMachineState.pushToEvalStack
                        (CliType.Numeric (CliNumericType.Int32 owner.RecursionLevel))
                        ctx.Thread
                    |> IlMachineState.pushToEvalStack
                        (CliType.RuntimePointer (CliRuntimePointer.Managed slot.Pointer))
                        ctx.Thread

                // The VM calls an `[UnmanagedCallersOnly]` method from native code
                // (`UnmanagedCallersOnlyCaller`), which is the transition such a method admits.
                // The native frame has no IL, so its program counter stays where it is.
                let state, commitment =
                    IlMachineStateExecution.callMethodWithCommitment
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        None
                        ConstructionState.NotConstructing
                        IlMachineStateExecution.CallDispatch.Direct
                        false
                        false
                        IlMachineStateExecution.CallSiteTransition.EntersPreemptive
                        concretized.Generics
                        concretized
                        ctx.Thread
                        state.ThreadState.[ctx.Thread]
                        None
                        ReturnValueDisposition.PushToCaller
                        (ExceptionEscape.UnhandledAtNativeCaller "SyncBlock::TryUpgradeThinLockToFullLock")
                        state

                match commitment with
                | IlMachineStateExecution.CallCommitment.Committed ->
                    NativeHandlerResult.pushedManagedCallee state |> Some
                | IlMachineStateExecution.CallCommitment.Raised ->
                    failwith
                        $"TODO: %s{operation}: calling Lock.InitializeForMonitor raised before it ran; CoreCLR would rethrow that from the QCall, which is not modelled"
                | IlMachineStateExecution.CallCommitment.Aborted fatal ->
                    NativeHandlerResult.aborted ctx.Thread fatal state |> Some
            | other ->
                failwith
                    $"%s{operation}: expected no values on the native frame's eval stack, or the two re-entry markers, got %A{other}"
        | _ -> None
