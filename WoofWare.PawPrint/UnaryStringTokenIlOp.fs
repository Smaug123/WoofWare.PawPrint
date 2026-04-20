namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryStringTokenIlOp =
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryStringTokenIlOp)
        (sh : StringToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            let addressToLoad, state =
                match state.InternedStrings.TryGetValue sh with
                | false, _ ->
                    let stringToAllocate = state.ActiveAssembly(thread).Strings sh

                    let addr, state =
                        IlMachineState.allocateManagedString loggerFactory baseClassTypes stringToAllocate state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (sh, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addressToLoad)) thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
