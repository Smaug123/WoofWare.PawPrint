namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataIlOp =
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryMetadataTokenIlOp)
        (sourcedMetadataToken : SourcedMetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        let logger = loggerFactory.CreateLogger (op.ToString ())

        let activeAssy =
            state.LoadedAssembly sourcedMetadataToken.SourceAssembly
            |> Option.defaultWith (fun () ->
                let available = state._LoadedAssemblies.Keys |> String.concat " ; "

                failwith
                    $"Metadata token source assembly %O{sourcedMetadataToken.SourceAssembly} is not loaded; available assemblies: {available}"
            )

        let ctx : UnaryMetadataIlOpContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Op = op
                ActiveAssembly = activeAssy
                MetadataToken = sourcedMetadataToken.Token
                CurrentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod
                Thread = thread
                Logger = logger
            }

        match op with
        | UnaryMetadataTokenIlOp.Call -> UnaryMetadataCallOps.executeCall ctx state
        | UnaryMetadataTokenIlOp.Callvirt -> UnaryMetadataCallOps.executeCallvirt ctx state
        | UnaryMetadataTokenIlOp.Castclass -> UnaryMetadataObjectOps.executeCastclass ctx state
        | UnaryMetadataTokenIlOp.Newobj -> UnaryMetadataObjectOps.executeNewobj ctx state
        | UnaryMetadataTokenIlOp.Newarr -> UnaryMetadataArrayOps.executeNewarr ctx state
        | UnaryMetadataTokenIlOp.Box -> UnaryMetadataObjectOps.executeBox ctx state
        | UnaryMetadataTokenIlOp.Ldelema -> UnaryMetadataArrayOps.executeLdelema ctx state
        | UnaryMetadataTokenIlOp.Isinst -> UnaryMetadataObjectOps.executeIsinst ctx state
        | UnaryMetadataTokenIlOp.Stfld -> UnaryMetadataFieldOps.executeStfld ctx state
        | UnaryMetadataTokenIlOp.Stsfld -> UnaryMetadataFieldOps.executeStsfld ctx state
        | UnaryMetadataTokenIlOp.Ldfld -> UnaryMetadataFieldOps.executeLdfld ctx state
        | UnaryMetadataTokenIlOp.Ldflda -> UnaryMetadataFieldOps.executeLdflda ctx state
        | UnaryMetadataTokenIlOp.Ldsfld -> UnaryMetadataFieldOps.executeLdsfld ctx state
        | UnaryMetadataTokenIlOp.Ldsflda -> UnaryMetadataFieldOps.executeLdsflda ctx state
        | UnaryMetadataTokenIlOp.Unbox_Any -> UnaryMetadataObjectOps.executeUnboxAny ctx state
        | UnaryMetadataTokenIlOp.Stelem -> UnaryMetadataArrayOps.executeStelem ctx state
        | UnaryMetadataTokenIlOp.Ldelem -> UnaryMetadataArrayOps.executeLdelem ctx state
        | UnaryMetadataTokenIlOp.Initobj -> UnaryMetadataMemoryOps.executeInitobj ctx state
        | UnaryMetadataTokenIlOp.Ldftn -> UnaryMetadataTokenOps.executeLdftn ctx state
        | UnaryMetadataTokenIlOp.Stobj -> UnaryMetadataMemoryOps.executeStobj ctx state
        | UnaryMetadataTokenIlOp.Constrained -> UnaryMetadataCallOps.executeConstrained ctx state
        | UnaryMetadataTokenIlOp.Ldtoken -> UnaryMetadataTokenOps.executeLdtoken ctx state
        | UnaryMetadataTokenIlOp.Cpobj -> failwith "TODO: Cpobj unimplemented"
        | UnaryMetadataTokenIlOp.Ldobj -> UnaryMetadataMemoryOps.executeLdobj ctx state
        | UnaryMetadataTokenIlOp.Sizeof -> UnaryMetadataMemoryOps.executeSizeof ctx state
        | UnaryMetadataTokenIlOp.Calli -> failwith "TODO: Calli unimplemented"
        | UnaryMetadataTokenIlOp.Unbox -> failwith "TODO: Unbox unimplemented"
        | UnaryMetadataTokenIlOp.Ldvirtftn -> failwith "TODO: Ldvirtftn unimplemented"
        | UnaryMetadataTokenIlOp.Mkrefany -> failwith "TODO: Mkrefany unimplemented"
        | UnaryMetadataTokenIlOp.Refanyval -> failwith "TODO: Refanyval unimplemented"
        | UnaryMetadataTokenIlOp.Jmp -> failwith "TODO: Jmp unimplemented"
