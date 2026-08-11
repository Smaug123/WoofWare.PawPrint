namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeException =
    /// CoreCLR's `ExceptionNative_GetMessageFromNativeResources` looks the
    /// message up in the runtime's localised resource table, falling back to
    /// the literal English strings below when the lookup fails. PawPrint has
    /// no resource pipeline, so we always return the fallback string. These
    /// values are byte-for-byte the runtime's own English fallbacks
    /// (`comutilnative.cpp`), so they remain a faithful default.
    let private messageForKind (operation : string) (kind : int) : string =
        match kind with
        | 1 -> "Thread was being aborted."
        | 2 -> "Thread was interrupted from a waiting state."
        | 3 -> "Insufficient memory to continue the execution of the program."
        | other ->
            failwith
                $"%s{operation}: unknown ExceptionMessageKind value %d{other} (expected 1=ThreadAbort, 2=ThreadInterrupted, 3=OutOfMemory)"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "ExceptionNative_GetMessageFromNativeResources",
          "System.Private.CoreLib",
          "System",
          "Exception",
          "GetMessageFromNativeResources",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "ExceptionMessageKind", kindGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "StringHandleOnStack",
                                              stringHandleGenerics) ],
          MethodReturnType.Void when kindGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "ExceptionNative_GetMessageFromNativeResources"

            if instruction.Arguments.Length <> 2 then
                failwith
                    $"%s{operation}: expected two native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let kind = NativeCall.int32Argument operation instruction.Arguments.[0]

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retMesg" instruction.Arguments.[1]

            let message = messageForKind operation kind

            let messageAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes message state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some messageAddr))

            NativeHandlerResult.completed state |> Some
        | "ExceptionNative_GetFrozenStackTrace",
          "System.Private.CoreLib",
          "System",
          "Exception",
          "GetFrozenStackTrace",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              exceptionGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              stackTraceGenerics) ],
          MethodReturnType.Void when exceptionGenerics.IsEmpty && stackTraceGenerics.IsEmpty ->
            // Reached from `ExceptionDispatchInfo.Capture` via `Exception.CaptureDispatchState`
            // (Exception.CoreCLR.cs:229-237). CoreCLR fetches the exception's `StackTraceArray`,
            // marks it frozen, and hands it back (comutilnative.cpp:81-118). "Frozen" means
            // copy-on-write: a later append clones rather than mutating, so the captured trace
            // cannot be rewritten by continued propagation of the same exception object.
            //
            // PawPrint gets that for free. `_stackTrace` holds a token minted afresh by each
            // dispatch (`IlMachineRuntimeMetadata.recordThrownStackTrace`), standing for an
            // immutable frame list in `IlMachineState.FrozenStackTraces`, so there is nothing
            // to freeze and nothing that could later mutate. This handler is therefore exactly
            // CoreCLR's remaining behaviour: hand back whatever `_stackTrace` holds.
            //
            // Null is a legitimate answer, not a failure: an exception that has never been
            // thrown has no trace, and CoreCLR returns null for it too (`ret.Set` of a NULL
            // array). `ExceptionDispatchInfo.Capture(new Exception())` is legal and depends on
            // that round-tripping as null, so this must not fail on a null trace. It *does*
            // fail on a null exception, which CoreCLR likewise asserts against
            // (comutilnative.cpp:88).
            let operation = "ExceptionNative_GetFrozenStackTrace"

            if instruction.Arguments.Length <> 2 then
                failwith
                    $"%s{operation}: expected two native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let exceptionPtr =
                NativeCall.objectHandleOnStackTarget operation state "exception" instruction.Arguments.[0]

            let exceptionAddr =
                match IlMachineState.readManagedByref ctx.BaseClassTypes state exceptionPtr with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    failwith $"%s{operation}: ObjectHandleOnStack pointed to a null Exception reference"
                | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

            let retStackTrace =
                NativeCall.objectHandleOnStackTarget operation state "stackTrace" instruction.Arguments.[1]

            let frozenTrace =
                IlMachineState.frozenStackTraceToken ctx.BaseClassTypes exceptionAddr state
                |> CliType.ObjectRef

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state retStackTrace frozenTrace

            NativeHandlerResult.completed state |> Some
        | _ -> None

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
          "Exception",
          "IsImmutableAgileException",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "Exception", exceptionGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            exceptionGenerics.IsEmpty
            ->
            // "Is this one of the runtime's *preallocated* exception objects?" — a reference-identity
            // test against exactly three startup singletons: `OutOfMemoryException`,
            // `StackOverflowException` and `ExecutionEngineException`
            // (`CLRException::IsPreallocatedExceptionObject`, clrex.cpp:433). They must stay
            // immutable because the runtime hands the same instance to every thread that needs one,
            // which is why CoreLib gives them a read-only `Data` and skips restoring dispatch state
            // onto them.
            //
            // PawPrint has no such singletons: every exception object it raises is freshly allocated
            // by `ExceptionDispatching.allocateRuntimeException`, and it does not construct these
            // three at all — the sites that would raise them are still `failwith` TODOs (e.g.
            // `NativeString.fs:15`, `NullaryIlOp.fs:344`). So the answer is unconditionally false,
            // and that is a fact about PawPrint's design rather than a convenient default. If a
            // preallocated-singleton pool is ever introduced, this handler is where it must be
            // consulted.
            //
            // Three near-misses, each a plausible way to get this wrong:
            //
            //  * PawPrint *does* cache a `TypeInitializationException` per failed type and rethrow
            //    that same instance. Cached is not preallocated — the predicate is identity against
            //    those three specific objects, and a TIE is not one of them.
            //  * A guest-constructed `new OutOfMemoryException()` is an ordinary object: being of a
            //    preallocated *type* is not being the preallocated *object*.
            //  * Most usefully for whoever implements the OOM TODOs above: **an ordinary
            //    allocation-too-large `OutOfMemoryException` is not the singleton either**, so those
            //    TODOs will not invalidate this handler. `GetOutOfMemoryException` allocates a fresh
            //    one and falls back to the preallocated instance only in its `EX_CATCH`
            //    (clrex.cpp:525-540) — i.e. only when allocating the exception object *itself*
            //    fails, which is genuine heap exhaustion rather than an oversized request. Measured
            //    on .NET 10: two `new long[int.MaxValue]` attempts yield two *distinct* OOM objects,
            //    and `IsImmutableAgileException` answers false for both.
            //
            // So the guest-observable reach of a `true` answer is narrower than it looks:
            // `StackOverflowException` is not catchable at all in CoreCLR (the process dies, so no
            // guest ever holds that instance), `ExecutionEngineException` is obsolete and no longer
            // thrown, and the OOM singleton appears only under true exhaustion — a state PawPrint's
            // unbounded in-memory heap has no notion of.
            let operation = "System.Exception.IsImmutableAgileException"

            if instruction.Arguments.Length <> 1 then
                failwith
                    $"%s{operation}: expected one argument after matching signature, got %d{instruction.Arguments.Length}"

            // A null argument answers false rather than failing. The `ASSERT(pExceptionUNSAFE !=
            // NULL)` above the comparison (comutilnative.cpp:53) is debug-only and compiled out of
            // the shipping runtime, so it is not a precondition a guest can violate — and in any
            // case null is not one of the three preallocated objects, which is all the identity
            // comparison asks. No CoreLib caller can pass null (all three pass `this`), but this is
            // a private static, so reflection can, and real .NET answers False when it does;
            // `ImmutableAgileExceptionNullArgument.cs` pins that against both runtimes.
            //
            // A non-ObjectRef argument is a different matter: that means the interpreter built the
            // call wrong, and there is no honest answer to give.
            match instruction.Arguments.[0] |> CliType.unwrapPrimitiveLike with
            | CliType.ObjectRef _ -> ()
            | other -> failwith $"%s{operation}: expected an ObjectRef Exception argument, got %O{other}"

            let state = IlMachineState.pushToEvalStack (CliType.ofBool false) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib", "System", "Exception", "PrepareForForeignExceptionRaise", [], MethodReturnType.Void ->
            // The last primitive `Exception.RestoreDispatchState` needs, and so the last gate on
            // `ExceptionDispatchInfo.Throw()`. CoreCLR sets a one-shot per-thread flag
            // (`SetRaisingForeignException`, comutilnative.cpp:75) which the *next* throw on that
            // thread reads twice:
            //
            //  * `IL_Throw` (jithelpers.cpp:814) nulls only `_stackTraceString`, leaving
            //    `_stackTrace` in place — where an ordinary `throw ex` would call
            //    `ClearStackTracePreservingRemoteStackTrace` and drop the frames entirely. This is
            //    what lets the restored trace survive the rethrow and be appended to.
            //  * the first `StackTraceInfo::AppendElement` afterwards (excep.cpp:3016-3017) reads
            //    and *resets* the flag, and marks the last already-present frame
            //    `STEF_LAST_FRAME_FROM_FOREIGN_STACK_TRACE`, which is what renders as
            //    "--- End of stack trace from previous location ---".
            //
            // PawPrint collapses both reads into one place, because it has no clear-at-throw step
            // to suppress: `ExceptionDispatching.throwExceptionObject` consumes the flag when it
            // seeds the new `CliException`, splicing the frames behind the exception's own
            // `_stackTrace` token in front of the throw-site frame and marking the last of them.
            // So all this handler does is set it.
            let operation = "System.Exception.PrepareForForeignExceptionRaise"

            if not instruction.Arguments.IsEmpty then
                failwith
                    $"%s{operation}: expected no arguments after matching signature, got %d{instruction.Arguments.Length}"

            let threadState =
                match state.ThreadState |> Map.tryFind ctx.Thread with
                | Some threadState -> threadState
                | None -> failwith $"%s{operation}: thread %O{ctx.Thread} has no ThreadState"

            // Set unconditionally rather than checked-then-set. CoreCLR's `SetRaisingForeignException`
            // is likewise an idempotent bit-set: a flag left over from an earlier `RestoreDispatchState`
            // whose throw never happened is simply overwritten, not diagnosed.
            let state =
                { state with
                    ThreadState =
                        state.ThreadState
                        |> Map.add
                            ctx.Thread
                            { threadState with
                                IsRaisingForeignException = true
                            }
                }

            NativeHandlerResult.completed state |> Some
        | _ -> None
