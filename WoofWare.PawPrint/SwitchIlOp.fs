namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal SwitchIlOp =
    let private switchIndex (value : EvalStackValue) : int32 =
        match value with
        | EvalStackValue.Int32 index -> index
        | other -> failwith $"switch expected an Int32 index on the eval stack, got %O{other}"

    let execute
        (state : IlMachineState)
        (currentThread : ThreadId)
        (targets : ImmutableArray<int32>)
        : IlMachineState * WhatWeDid
        =
        let indexValue, state = IlMachineThreadState.popEvalStack currentThread state
        let index = switchIndex indexValue

        let state = IlMachineThreadState.advanceProgramCounter currentThread state

        let state =
            if index < 0 || int index >= targets.Length then
                state
            else
                IlMachineThreadState.jumpProgramCounter currentThread (int targets.[int index]) state

        state, WhatWeDid.Executed
