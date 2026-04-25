namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeDispatch =
    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        match NativeEnvironment.tryExecute ctx with
        | Some result -> Some result
        | None ->
            match NativeMonitor.tryExecute ctx with
            | Some result -> Some result
            | None ->
                match NativeRuntimeFieldHandle.tryExecute ctx with
                | Some result -> Some result
                | None ->
                    match NativeRuntimeHelpers.tryExecute ctx with
                    | Some result -> Some result
                    | None ->
                        match NativeGcHandle.tryExecute ctx with
                        | Some result -> Some result
                        | None ->
                            match NativeRuntimeType.tryExecute ctx with
                            | Some result -> Some result
                            | None ->
                                match NativeThreading.tryExecute ctx with
                                | Some result -> Some result
                                | None -> NativeType.tryExecute ctx

    let failUnimplemented (ctx : NativeCallContext) : ExecutionResult = NativeCall.failUnimplemented ctx
