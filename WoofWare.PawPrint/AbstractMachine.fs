namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core
open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        impls
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Instructions with
        | None ->
            let targetAssy =
                state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly
                |> Option.get

            let targetType =
                targetAssy.TypeDefs.[instruction.ExecutingMethod.DeclaringType.Definition.Get]

            match DumpedAssembly.isDelegate baseClassTypes state._LoadedAssemblies targetType with
            | true ->
                match instruction.ReturnState with
                | None -> failwith "How come we don't have a return point from a delegate?!"
                | Some {
                           WasConstructingObj = Some _
                       } ->
                    IlMachineState.executeDelegateConstructor baseClassTypes instruction state
                    // can't advance the program counter here - there's no IL instructions executing!
                    |> IlMachineState.returnStackFrame loggerFactory baseClassTypes thread
                    |> function
                        | ReturnFrameResult.NormalReturn state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                        | result -> failwith $"unexpected ReturnFrameResult from delegate constructor: %A{result}"
                | Some {
                           WasConstructingObj = None
                       } ->
                    // We've been instructed to run a delegate.
                    let delegateToRunAddr =
                        match instruction.Arguments.[0] with
                        | CliType.ObjectRef (Some addr) -> addr
                        | _ -> failwith "expected a managed object ref to delegate"

                    let delegateToRun = state.ManagedHeap.NonArrayObjects.[delegateToRunAddr]

                    let target =
                        match delegateToRun |> AllocatedNonArrayObject.DereferenceField "_target" with
                        | CliType.ObjectRef addr -> addr
                        | x -> failwith $"TODO: delegate target wasn't an object ref: %O{x}"

                    let methodPtr =
                        // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                        match
                            delegateToRun
                            |> AllocatedNonArrayObject.DereferenceField "_methodPtr"
                            |> CliType.unwrapPrimitiveLike
                        with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                        | d -> failwith $"unexpectedly not a method pointer in delegate invocation: {d}"

                    let methodGenerics = instruction.ExecutingMethod.Generics

                    // Preserve the original call-site offset from the callvirt Invoke that
                    // created this delegate frame.  After returnStackFrame the caller's
                    // IlOpIndex has already been advanced, so we must carry the original
                    // call-site through to the delegate target's MethodReturnState.
                    let originalCallSitePC =
                        instruction.ReturnState |> Option.map (fun rs -> rs.CallSiteIlOpIndex)

                    // When we return, we need to go back up the stack
                    match state |> IlMachineState.returnFromSyntheticStackFrame thread with
                    | ReturnFrameResult.NoFrameToReturn -> failwith "unexpectedly nowhere to return from delegate"
                    | ReturnFrameResult.DispatchException _ ->
                        failwith "unexpected exception dispatch from delegate frame pop"
                    | ReturnFrameResult.NormalReturn state ->

                    // Rebuild the stack in normal instance-call shape: `this` below the real arguments.
                    // Push `target` first (if instance method) so it ends up at the bottom.
                    let state =
                        match target with
                        | None -> state
                        | Some target -> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some target)) thread state

                    // Push the real invoke parameters, skipping instruction.Arguments.[0] which is the
                    // delegate object itself (not needed by the target method).
                    let state =
                        let mutable s = state

                        for i = 1 to instruction.Arguments.Length - 1 do
                            s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

                        s

                    let state, _ =
                        state.WithThreadSwitchedToAssembly methodPtr.DeclaringType.Assembly thread

                    // Don't advance the program counter again on return; that was already done by the Callvirt that
                    // caused this delegate to be invoked.
                    let currentThreadState = state.ThreadState.[thread]

                    let state =
                        IlMachineStateExecution.callMethod
                            loggerFactory
                            baseClassTypes
                            None
                            None
                            false
                            false
                            false
                            methodGenerics
                            methodPtr
                            thread
                            currentThreadState
                            originalCallSitePC
                            false
                            state

                    ExecutionResult.Stepped (state, WhatWeDid.Executed)
            | false ->

            if not instruction.ExecutingMethod.IsNativeMethod then
                failwith
                    $"BUG: reached extern dispatch for {targetAssy.Name.Name} {targetType.Namespace}.{targetType.Name}::{instruction.ExecutingMethod.Name} which has no IL body but is not marked as a native method (ImplAttributes=%O{instruction.ExecutingMethod.ImplAttributes}, MethodAttributes=%O{instruction.ExecutingMethod.MethodAttributes})"

            let nativeContext =
                {
                    LoggerFactory = loggerFactory
                    Implementations = impls
                    BaseClassTypes = baseClassTypes
                    Thread = thread
                    State = state
                    Instruction = instruction
                    TargetAssembly = targetAssy
                    TargetType = targetType
                }

            let outcome =
                match NativeDispatch.tryExecute nativeContext with
                | Some result -> result
                | None -> NativeDispatch.failUnimplemented nativeContext


            match outcome with
            | ExecutionResult.Terminated (state, terminating) -> ExecutionResult.Terminated (state, terminating)
            | ExecutionResult.ProcessExit _ -> outcome
            | ExecutionResult.UnhandledException _ -> outcome
            | ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit) ->
                // A cctor was pushed; the native frame must stay on the stack so the dispatch loop
                // runs the cctor first, then re-enters this native method on the next step.
                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit)
            | ExecutionResult.Stepped (state, WhatWeDid.ThrowingTypeInitializationException) ->
                // Exception dispatch has already unwound past this native frame to the matching
                // handler, so returnStackFrame would pop the wrong frame.
                ExecutionResult.Stepped (state, WhatWeDid.ThrowingTypeInitializationException)
            | ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy) ->
                // Another thread owns this type's .cctor lock; the native frame must persist
                // until that thread finishes, then we re-enter.
                ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
            | ExecutionResult.Stepped (state, whatWeDid) ->
                match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
                | ReturnFrameResult.NormalReturn state -> ExecutionResult.Stepped (state, whatWeDid)
                | result -> failwith $"unexpected ReturnFrameResult from extern method return: %A{result}"

        | Some instructions ->

        match instructions.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ ->
            failwith
                $"Wanted to execute a nonexistent instruction in {instruction.ExecutingMethod.DeclaringType.Name}.{instruction.ExecutingMethod.Name}"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue instruction.ExecutingMethod.DeclaringType.Definition.Get with
                | true, v -> v.Name
                | false, _ -> "<unrecognised type>"

        logger.LogTrace (
            "Executing one step (index {ExecutingIlOpIndex}, max {MaxIlOpIndex}, in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            (Map.maxKeyValue instruction.ExecutingMethod.Instructions.Value.Locations |> fst),
            executingInType,
            instruction.ExecutingMethod.Name,
            executingInstruction
        )

        match instruction.ExecutingMethod.Instructions.Value.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> NullaryIlOp.execute loggerFactory baseClassTypes state thread op
        | IlOp.UnaryConst unaryConstIlOp ->
            UnaryConstIlOp.execute state thread unaryConstIlOp |> ExecutionResult.Stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.Stepped
        | IlOp.Switch immutableArray -> failwith "TODO: Switch unimplemented"
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            UnaryStringTokenIlOp.execute loggerFactory baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
