namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

type ThreadState =
    {
        // TODO: thread-local storage, synchronisation state, exception handling context
        MethodStates : MethodState ImmutableArray
        ActiveMethodState : int
        ActiveAssembly : AssemblyName
    }

    member this.MethodState = this.MethodStates.[this.ActiveMethodState]

    static member New (activeAssy : AssemblyName) (methodState : MethodState) =
        {
            ActiveMethodState = 0
            MethodStates = ImmutableArray.Create methodState
            ActiveAssembly = activeAssy
        }

    static member peekEvalStack (state : ThreadState) : EvalStackValue option =
        MethodState.peekEvalStack state.MethodStates.[state.ActiveMethodState]

    static member popFromEvalStack (state : ThreadState) : EvalStackValue * ThreadState =
        let activeMethodState = state.MethodStates.[state.ActiveMethodState]
        let ret, popped = activeMethodState |> MethodState.popFromStack

        let state =
            { state with
                MethodStates = state.MethodStates.SetItem (state.ActiveMethodState, popped)
            }

        ret, state

    static member pushToEvalStack (o : CliType) (methodStateIndex : int) (state : ThreadState) =
        let newMethodStates =
            state.MethodStates.SetItem (
                methodStateIndex,
                MethodState.pushToEvalStack o state.MethodStates.[methodStateIndex]
            )

        { state with
            MethodStates = newMethodStates
        }

    static member pushToEvalStack' (e : EvalStackValue) (methodStateIndex : int) (state : ThreadState) =
        let newMethodStates =
            state.MethodStates.SetItem (
                methodStateIndex,
                MethodState.pushToEvalStack' e state.MethodStates.[methodStateIndex]
            )

        { state with
            MethodStates = newMethodStates
        }

    static member jumpProgramCounter (bytes : int) (state : ThreadState) =
        let methodState =
            state.MethodStates.SetItem (
                state.ActiveMethodState,
                state.MethodStates.[state.ActiveMethodState]
                |> MethodState.jumpProgramCounter bytes
            )

        { state with
            MethodStates = methodState
        }

    static member advanceProgramCounter (state : ThreadState) =
        let methodState =
            state.MethodStates.SetItem (
                state.ActiveMethodState,
                state.MethodStates.[state.ActiveMethodState]
                |> MethodState.advanceProgramCounter
            )

        { state with
            MethodStates = methodState
        }

    static member loadArgument (i : int) (state : ThreadState) =
        let methodState =
            state.MethodStates.SetItem (
                state.ActiveMethodState,
                state.MethodStates.[state.ActiveMethodState] |> MethodState.loadArgument i
            )

        { state with
            MethodStates = methodState
        }
