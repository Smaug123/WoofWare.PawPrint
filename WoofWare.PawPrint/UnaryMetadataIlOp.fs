namespace WoofWare.PawPrint

open System.Collections.Concurrent
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

/// <summary>
/// How far <c>UnaryMetadataIlOp.execute</c> got in turning an operand into something it can
/// dispatch on.
/// </summary>
/// <remarks>
/// Three outcomes rather than a <c>Result</c>, because resolving a <c>DynamicScope</c> operand can
/// come back needing *guest code to run first* — which is neither a resolved operand nor an invalid
/// one, and which the instruction answers by suspending rather than by dispatching or throwing.
/// </remarks>
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type private OperandResolution =
    /// The operand resolved; dispatch on it.
    | Ready of ResolvedMetadataOperand
    /// The operand names a <c>DynamicMethod</c> at this address that has not been minted. Mint it
    /// and re-execute this instruction.
    | NeedsMinting of ManagedHeapAddress
    /// The operand is one real .NET refuses when it compiles the body; raise this exception type
    /// into the guest. <c>why</c> is a PawPrint diagnostic for the log, not the guest's message.
    | Invalid of exceptionType : TypeInfo<GenericParamFromMetadata, TypeDefn> * why : string

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

                ResolvedMetadataOperand.FromMetadata (activeAssy, sourced.Token)
                |> OperandResolution.Ready
            | MetadataOperand.FromDynamicScope scopeIndex ->
                let operation = string<UnaryMetadataTokenIlOp> op

                // The operand belongs to the method whose instruction this is, which is the one
                // executing: an operand is only ever read as its own instruction runs.
                let scope = DynamicScopeOperand.executingScope operation state thread

                match IlDecoding.scopeOperandKind op with
                | IlDecoding.ScopeOperandKind.Type ->
                    match DynamicScopeOperand.closedType baseClassTypes operation scopeIndex state scope with
                    | Ok handle -> ResolvedMetadataOperand.ScopeType handle |> OperandResolution.Ready
                    | Error (exceptionType, why) -> OperandResolution.Invalid (exceptionType, why)
                | IlDecoding.ScopeOperandKind.Method ->
                    // No `Invalid` arm: every way a method-position entry can be wrong is
                    // unreachable from a guest today, so `dynamicMethod` crashes rather than
                    // fabricating the exception real .NET would raise. See its docs for the
                    // measurements.
                    match DynamicScopeOperand.dynamicMethod baseClassTypes operation scopeIndex state scope with
                    | DynamicMethodResolution.Resolved handle ->
                        ResolvedMetadataOperand.ScopeMethod handle |> OperandResolution.Ready
                    | DynamicMethodResolution.NeedsMinting callee -> OperandResolution.NeedsMinting callee
                | IlDecoding.ScopeOperandKind.AnyType ->
                    // No narrowing: `ldtoken` hands the handle to the guest rather than consuming
                    // it, so every shape a target can take is a legal operand. See `closedType` for
                    // the three refusals that apply to the opcodes which do consume a type.
                    match DynamicScopeOperand.typeHandleTarget baseClassTypes operation scopeIndex state scope with
                    | Ok target -> ResolvedMetadataOperand.ScopeTypeTarget target |> OperandResolution.Ready
                    | Error (exceptionType, why) -> OperandResolution.Invalid (exceptionType, why)
                | IlDecoding.ScopeOperandKind.Field ->
                    match DynamicScopeOperand.field baseClassTypes operation scopeIndex state scope with
                    | Ok handle -> ResolvedMetadataOperand.ScopeField handle |> OperandResolution.Ready
                    | Error (exceptionType, why) -> OperandResolution.Invalid (exceptionType, why)
                | IlDecoding.ScopeOperandKind.NotYetSupported missing ->
                    // Unreachable: the decoder refuses such a body when the method is minted, so no
                    // `IlOp` carrying a scope operand for this opcode exists to be executed.
                    failwith
                        $"BUG: %O{op} is executing a DynamicScope operand naming entry %d{scopeIndex}, but IlDecoding.scopeOperandKind says %s{missing}, so the decoder should have refused this body at mint"

        match resolved with
        | OperandResolution.NeedsMinting callee ->
            // CoreCLR mints the callee here too, from inside `ResolveToken`, by calling the guest's
            // `GetMethodDescriptor` (`DynamicILGenerator.cs:800`). Push that call and stop: the
            // caller's PC has not moved, so this instruction runs again when the mint returns, and
            // resolves against the `_methodHandle` the mint wrote.
            //
            // Nothing has been popped or otherwise disturbed yet — this is before `ctx` is even
            // built, let alone before the op pops its arguments — so re-execution starts from
            // exactly the state that reached here.
            let state =
                DynamicScopeOperand.mintDynamicMethod
                    loggerFactory
                    baseClassTypes
                    (string<UnaryMetadataTokenIlOp> op)
                    callee
                    thread
                    state

            state, WhatWeDid.SuspendedForManagedCall
        | OperandResolution.Invalid (exceptionType, why) ->
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
        | OperandResolution.Ready operand ->

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
