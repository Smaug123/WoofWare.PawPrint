namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryStringTokenIlOp =
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryStringTokenIlOp)
        (sh : SourcedStringToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            let stringToAllocate =
                match state.LoadedAssembly sh.SourceAssembly with
                | Some assy -> assy.Strings sh.Token
                | None ->
                    let available = state._LoadedAssemblies.Keys |> String.concat " ; "

                    failwith
                        $"Tried to resolve ldstr token %O{sh.Token} from assembly {sh.SourceAssembly.FullName}, but only had the following available: {available}"

            let addressToLoad, state =
                match state.InternedStrings.TryGetValue stringToAllocate with
                | false, _ ->
                    let addr, state =
                        IlMachineState.allocateManagedString loggerFactory baseClassTypes stringToAllocate state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (stringToAllocate, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addressToLoad)) thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
