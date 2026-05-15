namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint

[<RequireQualifiedAccess>]
module System_Threading_Monitor =
    /// EnterHelperResult enum (nested in System.Threading.Monitor): Contention=0, Entered=1, UseSlowPath=2.
    /// LeaveHelperAction enum (nested in System.Threading.Monitor): None=0, Signal=1, Yield=2, Contention=3, Error=4.

    let private popOneObject (currentThread : ThreadId) (argIndex : int) (state : IlMachineState) =
        state
        |> IlMachineState.loadArgument currentThread argIndex
        |> IlMachineState.popEvalStack currentThread

    let private popInt32 (currentThread : ThreadId) (argIndex : int) (state : IlMachineState) =
        state
        |> IlMachineState.loadArgument currentThread argIndex
        |> IlMachineState.popEvalStack currentThread

    /// .NET 10 InternalCall: Monitor.TryEnter_FastPath(obj) -> bool.
    /// Returns true if the lock can be acquired without contention; false routes the IL to
    /// Monitor.Enter_Slowpath, which calls back into the QCall path. PawPrint runs each
    /// thread to completion between scheduler points, so a "Locked by another thread"
    /// SyncBlock represents real contention with no way to make progress in a fast path.
    let TryEnter_FastPath
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state

        let acquired, state =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None -> failwith "TODO: Monitor.TryEnter_FastPath should throw ArgumentNullException for null obj"
            | Some addr ->
                match IlMachineState.getSyncBlock addr state with
                | SyncBlock.Free ->
                    let state =
                        IlMachineState.setSyncBlock addr (SyncBlock.Locked (currentThread, 1)) state

                    true, state
                | SyncBlock.Locked (holder, count) ->
                    if holder = currentThread then
                        let state =
                            IlMachineState.setSyncBlock addr (SyncBlock.Locked (holder, count + 1)) state

                        true, state
                    else
                        false, state

        let state =
            IlMachineState.pushToEvalStack (CliType.ofBool acquired) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.TryEnter_FastPath_WithTimeout(obj, int32) -> EnterHelperResult.
    /// Caller treats the result as: 0 (Contention) → return false; 1 (Entered) → return true;
    /// 2 (UseSlowPath) → call Monitor.TryEnter_Slowpath. We never need the slowpath because
    /// PawPrint can answer Free / SelfHeld / OtherHeld directly from the SyncBlock.
    let TryEnter_FastPath_WithTimeout
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state
        let timeoutVal, state = popInt32 currentThread 1 state

        let timeout =
            match timeoutVal with
            | EvalStackValue.Int32 i -> i
            | other -> failwith $"Monitor.TryEnter_FastPath_WithTimeout: expected int32 timeout, got %O{other}"

        let result, state =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None ->
                failwith "TODO: Monitor.TryEnter_FastPath_WithTimeout should throw ArgumentNullException for null obj"
            | Some addr ->
                match IlMachineState.getSyncBlock addr state with
                | SyncBlock.Free ->
                    let state =
                        IlMachineState.setSyncBlock addr (SyncBlock.Locked (currentThread, 1)) state

                    1, state
                | SyncBlock.Locked (holder, count) ->
                    if holder = currentThread then
                        let state =
                            IlMachineState.setSyncBlock addr (SyncBlock.Locked (holder, count + 1)) state

                        1, state
                    elif timeout = 0 then
                        // Non-blocking poll: report contention without waiting.
                        0, state
                    else
                        // The deterministic scheduler runs the holding thread to a yield point
                        // before the waiter resumes, so blocking on a foreign-held monitor would
                        // require modelling cross-thread Monitor wait queues. Fail loud rather
                        // than silently returning Contention, which would corrupt guest control
                        // flow. Same envelope as the existing ReliableEnter handler.
                        failwith
                            "TODO: Monitor.TryEnter_FastPath_WithTimeout cross-thread blocking is not yet implemented"

        let state =
            IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 result)) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.IsEnteredNative(obj) -> bool.
    /// Returns true if the SyncBlock for `obj` is held by the current thread.
    let IsEnteredNative
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state

        let result =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None -> failwith "TODO: Monitor.IsEnteredNative should throw ArgumentNullException for null obj"
            | Some addr ->
                match IlMachineState.getSyncBlock addr state with
                | SyncBlock.Free -> false
                | SyncBlock.Locked (holder, _) -> holder = currentThread

        let state =
            IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.Exit_FastPath(obj) -> LeaveHelperAction.
    /// LeaveHelperAction.None (0) means the unlock fully succeeded and IL skips the slowpath;
    /// any non-zero value (Signal/Yield/Contention/Error) routes the IL through Exit_Slowpath.
    /// PawPrint can decrement the SyncBlock directly, so we always return None on success and
    /// fail loud if the unlock would have surfaced as Error in the real runtime.
    let Exit_FastPath
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state

        let state =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None -> failwith "TODO: Monitor.Exit_FastPath should throw ArgumentNullException for null obj"
            | Some addr ->
                match IlMachineState.getSyncBlock addr state with
                | SyncBlock.Free ->
                    failwith "TODO: Monitor.Exit_FastPath on a Free SyncBlock should throw SynchronizationLockException"
                | SyncBlock.Locked (holder, count) ->
                    if holder <> currentThread then
                        failwith
                            "TODO: Monitor.Exit_FastPath by a non-owning thread should throw SynchronizationLockException"
                    elif count = 1 then
                        IlMachineState.setSyncBlock addr SyncBlock.Free state
                    else
                        IlMachineState.setSyncBlock addr (SyncBlock.Locked (holder, count - 1)) state

        // LeaveHelperAction.None = 0 — caller's IL takes the early Ret branch.
        let state =
            IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped
