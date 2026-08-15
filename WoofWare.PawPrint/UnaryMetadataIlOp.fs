namespace WoofWare.PawPrint

open System.Collections.Concurrent
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataIlOp =
    /// One logger per (factory, opcode), rather than one per executed instruction: an
    /// `ILoggerFactory` is under no obligation to return a cached instance, and this runs on
    /// every `call`/`ldfld`/`newobj`/... the guest executes. Keyed weakly on the factory so a
    /// disposed factory is not kept alive by this cache.
    let private loggerCache =
        ConditionalWeakTable<ILoggerFactory, ConcurrentDictionary<UnaryMetadataTokenIlOp, ILogger>> ()

    let private logger (loggerFactory : ILoggerFactory) (op : UnaryMetadataTokenIlOp) : ILogger =
        let forFactory =
            loggerCache.GetValue (loggerFactory, fun _ -> ConcurrentDictionary ())

        forFactory.GetOrAdd (op, (fun op -> loggerFactory.CreateLogger (op.ToString ())))

    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryMetadataTokenIlOp)
        (operand : MetadataOperand)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        let logger = logger loggerFactory op

        // Resolving a `DynamicScope` operand here rather than in each op is what keeps the
        // "not a closed type" refusal in one place. Which *kind* of entry to read it as comes from
        // `IlDecoding.scopeOperandKind`, the same function the decoder consulted when it accepted
        // the body — so this cannot disagree with what the body was admitted on, and there is no
        // second copy of the classification to drift.
        //
        // Doing it before the op runs also matches CoreCLR, which resolves every token when it JITs
        // the method — i.e. before any of its instructions execute. Nothing observable turns on the
        // ordering here in any case: a scope type is already resolved, so reading it loads no type
        // and runs no `.cctor`, unlike the metadata path each op still performs for itself.
        let resolved =
            match operand with
            | MetadataOperand.FromMetadata sourced ->
                let activeAssy =
                    state.LoadedAssembly sourced.SourceAssembly
                    |> Option.defaultWith (fun () ->
                        let available = state._LoadedAssemblies.DefinitionNames |> String.concat " ; "

                        failwith
                            $"Metadata token source assembly %O{sourced.SourceAssembly} is not loaded; available assemblies: {available}"
                    )

                Ok (ResolvedMetadataOperand.FromMetadata (activeAssy, sourced.Token))
            | MetadataOperand.FromDynamicScope scopeIndex ->
                let operation = string<UnaryMetadataTokenIlOp> op

                match IlDecoding.scopeOperandKind op with
                | IlDecoding.ScopeOperandKind.Type ->
                    DynamicScopeOperand.closedType baseClassTypes operation scopeIndex state thread
                    |> Result.map ResolvedMetadataOperand.ScopeType
                | IlDecoding.ScopeOperandKind.Method ->
                    // No `Error` arm: every way a method-position entry can be wrong is unreachable
                    // from a guest today, so `dynamicMethod` crashes rather than fabricating the
                    // exception real .NET would raise. See its docs for the measurements.
                    DynamicScopeOperand.dynamicMethod baseClassTypes operation scopeIndex state thread
                    |> ResolvedMetadataOperand.ScopeMethod
                    |> Ok
                | IlDecoding.ScopeOperandKind.NotYetSupported missing ->
                    // Unreachable: the decoder refuses such a body when the method is minted, so no
                    // `IlOp` carrying a scope operand for this opcode exists to be executed.
                    failwith
                        $"BUG: %O{op} is executing a DynamicScope operand naming entry %d{scopeIndex}, but IlDecoding.scopeOperandKind says %s{missing}, so the decoder should have refused this body at mint"

        match resolved with
        | Error (exceptionType, why) ->
            // Measured on real .NET: an open generic definition, a bare generic parameter and an
            // open constructed type all make the method throw InvalidProgramException when it is
            // compiled, against a closed control that runs. `Emit` accepts all of them, because
            // each is a perfectly good `RuntimeType`.
            //
            // The residual divergence is *when*: real .NET compiles the whole body before running
            // any of it, so nothing the method would have done first happens; PawPrint throws when
            // this instruction is reached. That is inherent to interpreting rather than JITting and
            // is already true of every invalid metadata operand.
            //
            // Don't advance the PC: exception dispatch needs the faulting instruction's offset.
            // `why` is a PawPrint diagnostic and goes to the log; the exception the guest catches
            // carries the message CoreCLR gives it, which for these types is either their own
            // default or a fixed string (see `clrMessageFor`).
            logger.LogWarning ("{Op} refused a DynamicScope operand: {Reason}", op, why)

            IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                loggerFactory
                baseClassTypes
                exceptionType
                (DynamicScopeOperand.clrMessageFor baseClassTypes exceptionType)
                thread
                state
        | Ok operand ->

        let ctx : UnaryMetadataIlOpContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Op = op
                Operand = operand
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
        | UnaryMetadataTokenIlOp.Calli -> UnaryMetadataCallOps.executeCalli ctx state
        | UnaryMetadataTokenIlOp.Unbox -> UnaryMetadataObjectOps.executeUnbox ctx state
        | UnaryMetadataTokenIlOp.Ldvirtftn -> UnaryMetadataTokenOps.executeLdvirtftn ctx state
        | UnaryMetadataTokenIlOp.Mkrefany -> failwith "TODO: Mkrefany unimplemented"
        | UnaryMetadataTokenIlOp.Refanyval -> failwith "TODO: Refanyval unimplemented"
        | UnaryMetadataTokenIlOp.Jmp -> failwith "TODO: Jmp unimplemented"
