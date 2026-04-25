namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeQCall =
    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        match NativeCall.tryQCallEntryPoint ctx with
        | None -> None
        | Some entryPoint ->
            match NativeRuntimeHelpers.tryExecuteQCall entryPoint ctx with
            | Some result -> Some result
            | None ->
                match NativeRuntimeFieldHandle.tryExecuteQCall entryPoint ctx with
                | Some result -> Some result
                | None ->
                    match NativeGcHandle.tryExecuteQCall entryPoint ctx with
                    | Some result -> Some result
                    | None -> NativeMarshal.tryExecuteQCall entryPoint ctx
