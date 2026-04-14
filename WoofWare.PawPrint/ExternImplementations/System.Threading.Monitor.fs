namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint

type ISystem_Threading_Monitor =
    /// Signature: (PrimitiveType Object, Byref (PrimitiveType Boolean)) -> Void
    ///
    /// That is, the object on which to wait, and the result of the attempt to acquire the lock.
    /// <param name="lockTaken">The result of the attempt to acquire the lock, passed by reference. The input must be <see langword="false" />. The output is <see langword="true" /> if the lock is acquired; otherwise, the output is <see langword="false" />. The output is set even if an exception occurs during the attempt to acquire the lock.
    ///
    /// Note   If no exception occurs, the output of this method is always <see langword="true" />.</param>
    /// <exception cref="T:System.ArgumentException">The input to <paramref name="lockTaken" /> is <see langword="true" />.</exception>
    /// <exception cref="T:System.ArgumentNullException">The <paramref name="obj" /> parameter is <see langword="null" />.</exception>
    abstract ReliableEnter : ThreadId -> IlMachineState -> ExecutionResult

    /// Signature: (PrimitiveType Object) -> Void
    /// That is, the object whose lock is to be released.
    ///
    /// <summary>Releases an exclusive lock on the specified object.</summary>
    /// <param name="obj">The object on which to release the lock.</param>
    /// <exception cref="T:System.ArgumentNullException">The <paramref name="obj" /> parameter is <see langword="null" />.</exception>
    /// <exception cref="T:System.Threading.SynchronizationLockException">The current thread does not own the lock for the specified object.</exception>
    abstract Exit : ThreadId -> IlMachineState -> ExecutionResult

[<RequireQualifiedAccess>]
module System_Threading_Monitor =
    let private evalStackValueToObjectRef
        (state : IlMachineState)
        (value : EvalStackValue)
        : ManagedHeapAddress option
        =
        match value with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | EvalStackValue.ManagedPointer src ->
            match IlMachineState.readManagedByref state src with
            | CliType.ObjectRef addr -> addr
            | other -> failwith $"expected object reference, got {other}"
        | other -> failwith $"expected object reference, got {other}"

    let passThru : ISystem_Threading_Monitor =
        { new ISystem_Threading_Monitor with
            member _.ReliableEnter currentThread state =
                let lockObj, state =
                    state
                    |> IlMachineState.loadArgument currentThread 0
                    |> IlMachineState.popEvalStack currentThread

                let outVar, state =
                    state
                    |> IlMachineState.loadArgument currentThread 1
                    |> IlMachineState.popEvalStack currentThread

                let outVar =
                    match outVar with
                    | EvalStackValue.ManagedPointer src ->
                        match IlMachineState.readManagedByref state src with
                        | CliType.Bool 0uy -> src
                        | CliType.Bool _ -> failwith "TODO: raise ArgumentException"
                        | c -> failwith $"Bad IL: in ReliableEnter, expected bool, got {c}"
                    | _ -> failwith $"expected out var of ReliableEnter to be byref<bool>, got {outVar}"

                let state =
                    match evalStackValueToObjectRef state lockObj with
                    | None -> failwith "TODO: throw ArgumentNullException"
                    | Some addr ->
                        match IlMachineState.getSyncBlock addr state with
                        | SyncBlock.Free ->
                            state |> IlMachineState.setSyncBlock addr (SyncBlock.Locked (currentThread, 1))
                        | SyncBlock.Locked (thread, counter) ->
                            if thread = currentThread then
                                state
                                |> IlMachineState.setSyncBlock addr (SyncBlock.Locked (thread, counter + 1))
                            else
                                failwith "TODO: somehow need to block on the monitor"

                // Set result to True
                let state = IlMachineState.writeManagedByref state outVar (CliType.ofBool true)

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped

            member this.Exit thread state =
                let lockObj, state =
                    state
                    |> IlMachineState.loadArgument thread 0
                    |> IlMachineState.popEvalStack thread

                let state =
                    match evalStackValueToObjectRef state lockObj with
                    | None -> failwith "TODO: throw ArgumentNullException"
                    | Some addr ->
                        match IlMachineState.getSyncBlock addr state with
                        | SyncBlock.Free -> failwith "TODO: throw SynchronizationLockException"
                        | SyncBlock.Locked (holdingThread, count) ->
                            if thread <> holdingThread then
                                failwith "TODO: throw SynchronizationLockException"
                            else if count = 1 then
                                IlMachineState.setSyncBlock addr SyncBlock.Free state
                            else
                                IlMachineState.setSyncBlock addr (SyncBlock.Locked (holdingThread, count - 1)) state

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        }

type ISystem_Threading_Monitor_Env =
    abstract System_Threading_Monitor : ISystem_Threading_Monitor

[<RequireQualifiedAccess>]
module ISystem_Threading_Monitor_Env =
    let inline get (env : ISystem_Threading_Monitor_Env) : ISystem_Threading_Monitor = env.System_Threading_Monitor
