namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

type ThreadState =
    {
        // TODO: thread-local storage, synchronisation state, exception handling context
        MethodStates : MethodState ImmutableArray
        ActiveMethodState : FrameId
        ActiveAssembly : AssemblyName
    }

    // --- Frame resolution primitives (all FrameId -> int extraction lives here) ---

    static member getFrame (frameId : FrameId) (s : ThreadState) : MethodState =
        let (FrameId idx) = frameId
        s.MethodStates.[idx]

    static member setFrame (frameId : FrameId) (frame : MethodState) (s : ThreadState) : ThreadState =
        let (FrameId idx) = frameId

        { s with
            MethodStates = s.MethodStates.SetItem (idx, frame)
        }

    static member mapFrame (frameId : FrameId) (f : MethodState -> MethodState) (s : ThreadState) : ThreadState =
        ThreadState.setFrame frameId (f (ThreadState.getFrame frameId s)) s

    static member appendFrame (frame : MethodState) (s : ThreadState) : FrameId * ThreadState =
        let newId = FrameId s.MethodStates.Length

        let s =
            { s with
                MethodStates = s.MethodStates.Add frame
            }

        newId, s

    static member setActiveFrame (frameId : FrameId) (s : ThreadState) : ThreadState =
        { s with
            ActiveMethodState = frameId
        }

    // --- Derived operations (implemented via the primitives above) ---

    member this.MethodState : MethodState =
        ThreadState.getFrame this.ActiveMethodState this

    static member New (activeAssy : AssemblyName) (methodState : MethodState) =
        {
            ActiveMethodState = FrameId 0
            MethodStates = ImmutableArray.Create methodState
            ActiveAssembly = activeAssy
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
