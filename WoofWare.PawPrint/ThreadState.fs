namespace WoofWare.PawPrint

open System.Reflection

/// Scheduling status of a thread. The scheduler only picks Runnable threads; a thread in any
/// other state is paused until something external (another thread terminating, for instance)
/// flips it back to Runnable.
type ThreadStatus =
    | Runnable
    /// This thread is blocked inside Thread.Join, waiting for the named thread to terminate.
    | BlockedOnJoin of target : ThreadId
    /// This thread tried to access a type whose .cctor is currently being run by another
    /// thread. Per ECMA-335 II.10.5.3.3 it must wait for that thread to finish initialising
    /// the type before it can proceed.
    | BlockedOnClassInit of blocker : ThreadId
    /// This thread has executed its final `ret`; it will never run again. Its state is kept
    /// only so other threads can observe termination (e.g. to satisfy Join).
    | Terminated

type ThreadState =
    {
        // TODO: thread-local storage, synchronisation state, exception handling context
        MethodStates : Map<FrameId, MethodState>
        NextFrameId : int
        ActiveMethodState : FrameId
        Status : ThreadStatus
    }

    // --- Frame resolution primitives ---

    static member getFrame (frameId : FrameId) (s : ThreadState) : MethodState =
        match s.MethodStates |> Map.tryFind frameId with
        | Some frame -> frame
        | None -> failwith $"Frame %O{frameId} is not live in this thread"

    static member setFrame (frameId : FrameId) (frame : MethodState) (s : ThreadState) : ThreadState =
        if not (s.MethodStates |> Map.containsKey frameId) then
            failwith $"Cannot update frame %O{frameId} because it is not live in this thread"

        { s with
            MethodStates = s.MethodStates |> Map.add frameId frame
        }

    static member mapFrame (frameId : FrameId) (f : MethodState -> MethodState) (s : ThreadState) : ThreadState =
        ThreadState.setFrame frameId (f (ThreadState.getFrame frameId s)) s

    static member appendFrame (frame : MethodState) (s : ThreadState) : FrameId * ThreadState =
        let newId = FrameId s.NextFrameId

        let s =
            { s with
                NextFrameId = s.NextFrameId + 1
                MethodStates = s.MethodStates |> Map.add newId frame
            }

        newId, s

    static member removeFrame (frameId : FrameId) (s : ThreadState) : ThreadState =
        if frameId = s.ActiveMethodState then
            failwith $"Cannot remove active frame %O{frameId}; switch active frames first"

        if not (s.MethodStates |> Map.containsKey frameId) then
            failwith $"Cannot remove frame %O{frameId} because it is not live in this thread"

        { s with
            MethodStates = s.MethodStates |> Map.remove frameId
        }

    static member setActiveFrame (frameId : FrameId) (s : ThreadState) : ThreadState =
        if not (s.MethodStates |> Map.containsKey frameId) then
            failwith $"Cannot make frame %O{frameId} active because it is not live in this thread"

        { s with
            ActiveMethodState = frameId
        }

    static member replaceFrames (methodState : MethodState) (s : ThreadState) : ThreadState =
        let newId = FrameId s.NextFrameId

        { s with
            ActiveMethodState = newId
            MethodStates = Map.empty |> Map.add newId methodState
            NextFrameId = s.NextFrameId + 1
        }

    // --- Derived operations (implemented via the primitives above) ---

    member this.MethodState : MethodState =
        ThreadState.getFrame this.ActiveMethodState this

    member this.ActiveAssembly : AssemblyName =
        this.MethodState.ExecutingMethod.DeclaringType.Assembly

    member this.LiveFrameCount : int = this.MethodStates.Count

    static member New (methodState : MethodState) =
        {
            ActiveMethodState = FrameId 0
            MethodStates = Map.empty |> Map.add (FrameId 0) methodState
            NextFrameId = 1
            Status = ThreadStatus.Runnable
        }

    static member peekEvalStack (state : ThreadState) : EvalStackValue option =
        MethodState.peekEvalStack (ThreadState.getFrame state.ActiveMethodState state)

    static member popFromEvalStack (state : ThreadState) : EvalStackValue * ThreadState =
        let activeFrame = ThreadState.getFrame state.ActiveMethodState state
        let ret, popped = activeFrame |> MethodState.popFromStack
        let state = ThreadState.setFrame state.ActiveMethodState popped state
        ret, state

    static member pushToEvalStack (o : CliType) (frameId : FrameId) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame frameId (MethodState.pushToEvalStack o) state

    static member pushToEvalStack' (e : EvalStackValue) (frameId : FrameId) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame frameId (MethodState.pushToEvalStack' e) state

    static member jumpProgramCounter (bytes : int) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame state.ActiveMethodState (MethodState.jumpProgramCounter bytes) state

    static member advanceProgramCounter (state : ThreadState) : ThreadState =
        ThreadState.mapFrame state.ActiveMethodState MethodState.advanceProgramCounter state

    static member loadArgument (i : int) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame state.ActiveMethodState (MethodState.loadArgument i) state

    static member setLocalVariable
        (frameId : FrameId)
        (localVariable : uint16)
        (value : CliType)
        (s : ThreadState)
        : ThreadState
        =
        ThreadState.mapFrame
            frameId
            (fun frame ->
                { frame with
                    LocalVariables = frame.LocalVariables.SetItem (int<uint16> localVariable, value)
                }
            )
            s

    static member setArgument
        (frameId : FrameId)
        (argument : uint16)
        (value : CliType)
        (s : ThreadState)
        : ThreadState
        =
        ThreadState.mapFrame
            frameId
            (fun frame ->
                { frame with
                    Arguments = frame.Arguments.SetItem (int<uint16> argument, value)
                }
            )
            s
