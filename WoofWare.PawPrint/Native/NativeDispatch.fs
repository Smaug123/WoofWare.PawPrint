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
                match NativeMarshal.tryExecute ctx with
                | Some result -> Some result
                | None ->
                    match NativeQCall.tryExecute ctx with
                    | Some result -> Some result
                    | None ->
                        // QCall migration note: some name-based native handlers below still model
                        // CoreCLR QCalls on newer runtimes. Move each to NativeQCall as its import
                        // metadata is needed, then delete the corresponding name-based fallback.
                        match NativeMetadataImport.tryExecute ctx with
                        | Some result -> Some result
                        | None ->
                            match NativeGcHandle.tryExecute ctx with
                            | Some result -> Some result
                            | None ->
                                match NativeRuntimeType.tryExecute ctx with
                                | Some result -> Some result
                                | None ->
                                    match NativeRuntimeAssembly.tryExecute ctx with
                                    | Some result -> Some result
                                    | None ->
                                        match NativeThreading.tryExecute ctx with
                                        | Some result -> Some result
                                        | None ->
                                            match NativeType.tryExecute ctx with
                                            | Some result -> Some result
                                            | None ->
                                                match NativeString.tryExecute ctx with
                                                | Some result -> Some result
                                                | None ->
                                                    match NativeSystemNative.tryExecute ctx with
                                                    | Some result -> Some result
                                                    | None -> NativeDebugger.tryExecute ctx

    let failUnimplemented (ctx : NativeCallContext) : ExecutionResult = NativeCall.failUnimplemented ctx
