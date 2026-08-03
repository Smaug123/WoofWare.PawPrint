namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEnvironment =
    /// Read a UTF-16 char* argument, returning None for a null pointer and Some <string> otherwise.
    let private tryReadOptionalUtf16
        (operation : string)
        (argName : string)
        (ctx : NativeCallContext)
        (arg : CliType)
        : string option
        =
        let ptr = NativeCall.managedPointerOfPointerArgument operation argName arg

        match ptr with
        | ManagedPointerSource.Null -> None
        | _ ->
            NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes ctx.State ptr
            |> Some

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "GetProcessorCount",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Answered from kernel state, never from the host: see the
            // `EmulatedKernel.ProcessorCount` doc comment for why a host read
            // here would be a replayability bug rather than a mere impurity.
            let processorCount = state.Kernel.ProcessorCount

            if processorCount < 1 then
                // `Environment.ProcessorCount` is documented as always
                // positive, and CoreLib callers (ThreadPool sizing,
                // `Parallel` partitioning) divide by it. A kernel built by
                // record-copy can bypass `EmulatedKernel.withProcessorCount`,
                // so re-assert here: the guest must never observe a value
                // that the real property could not produce.
                failwith
                    $"Environment.GetProcessorCount: kernel ProcessorCount is %d{processorCount}, which is not a legal value for Environment.ProcessorCount (must be at least 1)"

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 processorCount) ctx.Thread

            NativeHandlerResult.Completed (state, StepEffect.NoEffect) |> Some
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "get_CurrentManagedThreadId",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (IlMachineState.getCurrentManagedThreadId ctx.Thread state))
                    ctx.Thread

            NativeHandlerResult.Completed (state, StepEffect.NoEffect) |> Some
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "_Exit",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // `Environment.Exit(int)` is `=> _Exit(exitCode)` and nothing else
            // (see the pinned runtime's
            // src/coreclr/System.Private.CoreLib/src/System/Environment.CoreCLR.cs:
            // `_Exit` is a QCall to `Environment_Exit`). So reaching here means
            // the process is going away right now: no managed shutdown work
            // remains, no further guest code runs, and the exit applies to the
            // whole process regardless of which thread called it.
            //
            // Hence `ProcessExit`, not `Terminated`. `Terminated` is
            // thread-scoped — `Program.stepPrepared` only ends the run when the
            // terminating thread is the entry thread — so using it here would
            // silently reduce `Environment.Exit` on a worker to "that worker
            // finished" and let the process keep running.
            //
            // Push the exit code (arg 0) onto the eval stack first: both
            // `RunOutcome.ProcessExit` and `RunOutcome.NormalExit` read the exit
            // code off the reporting thread's eval stack.
            let state = state |> IlMachineState.loadArgument ctx.Thread 0

            ExecutionResult.ProcessExit (state, ctx.Thread)
            |> NativeHandlerResult.Terminating
            |> Some
        | "System.Private.CoreLib", "System", "Environment", _, _, _ when
            NativeCall.tryQCallEntryPoint ctx = Some "Environment_FailFast"
            ->
            // QCall lowering of Environment.FailFast(string?, Exception?, string?). The
            // C# source uses LibraryImport with non-blittable string args, so Roslyn
            // emits a marshalling stub whose synthesized name (e.g.
            // `<FailFast>g____PInvoke|N_M`) carries source-generator counters and is
            // not stable across runtime/source-generator versions. Match on the QCall
            // entry-point name (`Environment_FailFast`) instead, then verify the
            // signature shape before reading args.
            //
            // The StackCrawlMarkHandle and ObjectHandleOnStack args are diagnostic-only
            // on the native side (used by CoreCLR to walk the managed stack and capture
            // the exception object); PawPrint surfaces FailFast as an abort outcome and
            // does not yet inspect either, so they're ignored here. `message` and
            // `errorSource` are UTF-16 char* pointers (possibly null).
            let operation = "Environment_FailFast"

            match
                instruction.ExecutingMethod.Signature.ParameterTypes, instruction.ExecutingMethod.Signature.ReturnType
            with
            | [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                  "System.Runtime.CompilerServices",
                                                  "StackCrawlMarkHandle",
                                                  stackMarkGenerics)
                ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
                ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                  "System.Runtime.CompilerServices",
                                                  "ObjectHandleOnStack",
                                                  objHandleGenerics)
                ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16) ],
              MethodReturnType.Void when stackMarkGenerics.IsEmpty && objHandleGenerics.IsEmpty ->
                if instruction.Arguments.Length <> 4 then
                    failwith
                        $"%s{operation}: expected four native arguments after matching signature, got %d{instruction.Arguments.Length}"

                let message = tryReadOptionalUtf16 operation "message" ctx instruction.Arguments.[1]

                // Read (and thereby validate the shape of) `errorSource` even
                // though the abort outcome does not carry it: a malformed
                // pointer here is a guest/marshalling bug we want to surface at
                // the boundary rather than silently ignore.
                let _errorSource =
                    tryReadOptionalUtf16 operation "errorSource" ctx instruction.Arguments.[3]

                // FailFast aborts the process. We deliberately do not load the
                // StackCrawlMark / exception / errorSource arguments onto the
                // eval stack because the caller never returns — the run loop
                // converts `ExecutionResult.FailFast` directly into
                // `RunOutcome.FailFast` for the host to surface.
                ExecutionResult.FailFast (state, ctx.Thread, message)
                |> NativeHandlerResult.Terminating
                |> Some
            | paramTypes, returnType ->
                failwith
                    $"%s{operation}: matched QCall entry point but signature unexpected: params=%A{paramTypes}, return=%A{returnType}"
        | _ -> None
