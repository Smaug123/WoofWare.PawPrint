namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineThreadState =
    // --- Cross-thread frame resolution primitives ---

    let getFrame (thread : ThreadId) (frameId : FrameId) (state : IlMachineState) : MethodState =
        ThreadState.getFrame frameId state.ThreadState.[thread]

    let setFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (frame : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.setFrame frameId frame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let mapFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (f : MethodState -> MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.mapFrame frameId f threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    /// Replace the `WasConstructingObj` of the active frame's `ReturnState` with `Some newAddr`.
    /// Used by InternalCall constructors that allocate the constructed object themselves
    /// (e.g. `String..ctor(char*)`): the placeholder allocated by `executeNewobj` is discarded
    /// and the next `returnStackFrame` pushes `newAddr` onto the caller's eval stack instead.
    /// Fails loudly if invoked outside a constructor frame.
    let withReplacedConstructedObject
        (newAddr : ManagedHeapAddress)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let activeFrameId = threadState.ActiveMethodState

        let updateFrame (frame : MethodState) : MethodState =
            match frame.ReturnState with
            | None ->
                failwith
                    $"withReplacedConstructedObject: active frame %s{frame.ExecutingMethod.Name} has no ReturnState; cannot redirect a non-existent constructor return"
            | Some returnState ->
                match returnState.WasConstructingObj with
                | None ->
                    failwith
                        $"withReplacedConstructedObject: active frame %s{frame.ExecutingMethod.Name} is not a constructor frame (WasConstructingObj is None)"
                | Some _ ->
                    { frame with
                        ReturnState =
                            Some
                                { returnState with
                                    WasConstructingObj = Some newAddr
                                }
                    }

        let threadState = ThreadState.mapFrame activeFrameId updateFrame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let pushToEvalStack' (o : EvalStackValue) (thread : ThreadId) (state : IlMachineState) =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack' o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let pushToEvalStack (o : CliType) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let peekEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue option =
        ThreadState.peekEvalStack state.ThreadState.[thread]

    let popEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue * IlMachineState =
        let ret, popped = ThreadState.popFromEvalStack state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread popped
            }

        ret, state

    let advanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.advanceProgramCounter |> Some
                    )
        }

    let setArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.setArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let getArrayValue (arrayAllocation : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.getArrayValue arrayAllocation index state.ManagedHeap

    /// Pops a synthetic frame that is only a dispatch trampoline, not a real method return.
    /// The concrete callee it dispatches to is responsible for producing any return value.
    let returnFromSyntheticStackFrame (currentThread : ThreadId) (state : IlMachineState) : ReturnFrameResult =
        let threadStateWithSyntheticFrame = state.ThreadState.[currentThread]
        let syntheticFrameId = threadStateWithSyntheticFrame.ActiveMethodState

        match threadStateWithSyntheticFrame.MethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->
            match returnState.WasConstructingObj with
            | Some _ ->
                failwith
                    $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly represented object construction"
            | None ->
                if returnState.DispatchAsExceptionOnReturn then
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly requested exception dispatch on return"

                match returnState.WasInitialisingType with
                | None -> ()
                | Some _ ->
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly represented type initialisation"

                match threadStateWithSyntheticFrame.MethodState.EvaluationStack.Values with
                | [] -> ()
                | _ ->
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly had evaluation stack values"

                let callerFrame =
                    ThreadState.getFrame returnState.JumpTo threadStateWithSyntheticFrame

                let threadState =
                    threadStateWithSyntheticFrame
                    |> ThreadState.setActiveFrame returnState.JumpTo
                    |> ThreadState.removeFrame syntheticFrameId

                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread threadState
                }
                |> ReturnFrameResult.NormalReturn

    /// There might be no stack frame to return to, so you might get NoFrameToReturn.
    let returnStackFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ReturnFrameResult
        =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]
        let returningFrameId = threadStateAtEndOfMethod.ActiveMethodState
        let returningMethodState = threadStateAtEndOfMethod.MethodState

        match returningMethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let callerFrame = ThreadState.getFrame returnState.JumpTo threadStateAtEndOfMethod

        let threadState =
            threadStateAtEndOfMethod
            |> ThreadState.setActiveFrame returnState.JumpTo
            |> ThreadState.removeFrame returningFrameId

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            if returnState.DispatchAsExceptionOnReturn then
                // This ctor was constructing a runtime-synthesised exception object.
                // Don't push it onto the eval stack; signal to the caller that exception
                // dispatch should occur.
                let constructed = state.ManagedHeap.NonArrayObjects.[constructing]
                ReturnFrameResult.DispatchException (state, constructing, constructed.ConcreteType)
            else

            // Assumption: a constructor can't also return a value.
            // If we were constructing a reference type, we push a reference to it.
            // Otherwise, extract the now-complete object from the heap and push it to the stack directly.
            let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

            let ty =
                AllConcreteTypes.lookup constructed.ConcreteType state.ConcreteTypes
                |> Option.get

            let ty' =
                state.LoadedAssembly (ty.Assembly)
                |> Option.get
                |> fun a -> a.TypeDefs.[ty.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty' then
                state
                // TODO: ordering of fields probably important
                |> pushToEvalStack (CliType.ValueType constructed.Contents) currentThread
            else
                state |> pushToEvalStack (CliType.ofManagedObject constructing) currentThread
            |> ReturnFrameResult.NormalReturn
        | None ->
            let retType = returningMethodState.ExecutingMethod.Signature.ReturnType

            match retType, returningMethodState.EvaluationStack.Values with
            | MethodReturnType.Void, [] -> state
            | MethodReturnType.Void, _ ->
                failwith
                    $"Invalid CIL: void method %s{returningMethodState.ExecutingMethod.Name} returned with a non-empty evaluation stack"
            | MethodReturnType.Returns _, [] ->
                failwith
                    $"Invalid CIL: non-void method %s{returningMethodState.ExecutingMethod.Name} returned with an empty evaluation stack"
            | MethodReturnType.Returns retType, [ retVal ] ->
                let zero, state =
                    IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes retType

                let toPush = EvalStackValue.toCliTypeCoerced zero retVal

                state |> pushToEvalStack toPush currentThread
            | MethodReturnType.Returns _, _ ->
                failwith
                    $"Invalid CIL: method %s{returningMethodState.ExecutingMethod.Name} returned with more than one evaluation stack value"

            |> ReturnFrameResult.NormalReturn

    let initial
        (lf : ILoggerFactory)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (entryAssembly : DumpedAssembly)
        : IlMachineState
        =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name
        let logger = lf.CreateLogger "IlMachineState"

        let state =
            {
                ConcreteTypes = AllConcreteTypes.Empty
                Logger = logger
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                _Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
                TypeHandles = TypeHandleRegistry.empty ()
                GcHandles = GcHandleRegistry.empty ()
                FieldHandles = FieldHandleRegistry.empty ()
                MethodHandles = MethodHandleRegistry.empty ()
                HardwareIntrinsics = HardwareIntrinsicsProfile.ScalarOnly
                Debugger = DebuggerState.Detached
                RuntimeAssemblyObjects = ImmutableDictionary.Empty
                RuntimeModuleObjects = ImmutableDictionary.Empty
                ManagedThreadObjects = Map.empty
                NextManagedThreadId = 2
                LastPInvokeError = 0
                LastSystemError = 0
                NextEventPipeId = 1L
            }

        state.WithLoadedAssembly assyName entryAssembly

    let addThread (newThreadState : MethodState) (state : IlMachineState) : IlMachineState * ThreadId =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState = state.ThreadState |> Map.add thread (ThreadState.New newThreadState)
            }

        newState, thread

    let allocateArray
        (arrayType : ConcreteTypeHandle)
        (zeroOfType : unit -> CliType)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                ConcreteType = arrayType
                Length = len
                Lengths = ImmutableArray.Create len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    /// Allocate a multi-dimensional array of `arrayType` (which should be a
    /// `ConcreteTypeHandle.Array (elementHandle, rank)`), zero-initialised in row-major
    /// layout. Each entry of `dimensionLengths` must be non-negative and the array
    /// must have rank >= 1; multi-dim arrays with non-zero lower bounds are not
    /// representable here (C# never emits them, and ECMA-335 II.14.2 calls them out
    /// as a separate constructor form).
    let allocateMultiDimArray
        (arrayType : ConcreteTypeHandle)
        (zeroOfType : unit -> CliType)
        (dimensionLengths : ImmutableArray<int>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        if dimensionLengths.Length = 0 then
            failwith
                "TODO: cannot allocate multi-dim array with rank 0; this should have been ruled out at the dispatch site"

        // Match CoreCLR's product-overflow rule (vm/gchelpers.cpp `AllocateArrayEx`):
        // accumulate the running product in unsigned 32-bit, throwing OutOfMemoryException
        // only if the multiply itself would overflow UInt32 — *not* on a transient prefix
        // that exceeds Int32.MaxValue but later gets zeroed by a 0 dimension. So
        // `new int[50000, 50000, 0]` allocates an empty array (the prefix 2.5e9 fits in
        // UInt32 and the trailing zero brings the product back to 0), while
        // `new int[65536, 65536, ...]` throws because 65536 * 65536 overflows UInt32 at
        // the multiply step regardless of any later zero. After the loop, the final
        // product must also fit in Int32, since our backing-store length is Int32.
        let mutable totalLength : uint32 = 1u

        for i = 0 to dimensionLengths.Length - 1 do
            let d = dimensionLengths.[i]

            if d < 0 then
                failwith
                    $"TODO: multi-dim array constructor was given a negative length %d{d} at dimension %d{i}; should raise OverflowException"

            let dU = uint32 d
            // Multiplying by zero is always safe; it just zeroes the running product.
            if dU <> 0u && totalLength > System.UInt32.MaxValue / dU then
                failwith
                    $"TODO: multi-dim array running product overflows UInt32 at dimension %d{i}; should raise OutOfMemoryException"

            totalLength <- totalLength * dU

        if totalLength > uint32 System.Int32.MaxValue then
            failwith "TODO: multi-dim array total length exceeds Int32.MaxValue; should raise OutOfMemoryException"

        let totalLength = int totalLength

        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init totalLength |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                ConcreteType = arrayType
                Length = totalLength
                Lengths = dimensionLengths
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.setStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject
        (ty : ConcreteTypeHandle)
        (fields : CliValueType)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Contents = fields
                ConcreteType = ty
                SyncBlock = SyncBlock.Free
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let popFromStackToLocalVariable
        (thread : ThreadId)
        (localVariableIndex : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToVariable localVariableIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let popFromStackToArgument (thread : ThreadId) (argumentIndex : int) (state : IlMachineState) : IlMachineState =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToArg argumentIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let jumpProgramCounter (thread : ThreadId) (bytes : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.jumpProgramCounter bytes |> Some
                    )
        }

    let loadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some state -> state |> ThreadState.loadArgument index |> Some
                    )
        }

    let getLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (state : IlMachineState)
        : CliType
        =
        (getFrame thread frameId state).LocalVariables.[int<uint16> varIndex]

    let setLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set variable in nonactive thread"
                        | Some existing -> existing |> ThreadState.setLocalVariable frameId varIndex value |> Some
                    )
        }

    let setArgument
        (thread : ThreadId)
        (frameId : FrameId)
        (argIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set argument in nonactive thread"
                        | Some existing -> existing |> ThreadState.setArgument frameId argIndex value |> Some
                    )
        }

    let allocateLocalMemory
        (thread : ThreadId)
        (initialization : LocalMemoryInitialization)
        (byteCount : int)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let frameId = threadState.ActiveMethodState
        let frame = ThreadState.getFrame frameId threadState

        let blockId, pool =
            LocalMemoryPool.allocate initialization byteCount frame.LocalMemoryPool

        let frame =
            { frame with
                LocalMemoryPool = pool
            }

        let state = setFrame thread frameId frame state

        ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frameId, blockId, 0), []), state

    let getLocalMemoryPool (thread : ThreadId) (frameId : FrameId) (state : IlMachineState) : LocalMemoryPool =
        (getFrame thread frameId state).LocalMemoryPool

    let setLocalMemoryPool
        (thread : ThreadId)
        (frameId : FrameId)
        (pool : LocalMemoryPool)
        (state : IlMachineState)
        : IlMachineState
        =
        let frame = getFrame thread frameId state

        let frame =
            { frame with
                LocalMemoryPool = pool
            }

        setFrame thread frameId frame state

    let setSyncBlock
        (addr : ManagedHeapAddress)
        (syncBlockValue : SyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = state.ManagedHeap |> ManagedHeap.setSyncBlock addr syncBlockValue
        }

    let getSyncBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        state.ManagedHeap |> ManagedHeap.getSyncBlock addr
