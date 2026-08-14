namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Accumulates IL prefix opcodes (ECMA-335 III.2) that have been executed but not yet
/// consumed by their target instruction. Multiple prefixes can stack on a single
/// target instruction (e.g. `volatile. unaligned. ldfld`), so this is a record of
/// independent slots rather than a single-slot DU.
///
/// Prefixes apply to the immediately-following instruction; after the target instruction
/// consumes the state, callers must reset it back to `PrefixState.empty`.
type PrefixState =
    {
        /// `constrained. T` (III.2.1) — applies to the next `callvirt`.
        Constrained : ConcreteTypeHandle option
        /// `volatile.` (III.2.5) — applies to the next ldind/stind/ldfld/stfld/ldobj/stobj/initblk/cpblk.
        Volatile : bool
        /// `tail.` (III.2.4) — applies to the next call/callvirt/calli.
        /// PawPrint never sets this: `tail.` executes as a no-op (see `NullaryIlOp.execute`),
        /// so there is nothing for the following call to consume. It exists for a future
        /// implementation that actually releases the caller's frame.
        Tail : bool
        /// `unaligned. alignment` (III.2.3) — applies to the next ldind/stind/ldfld/stfld/ldobj/stobj/initblk/cpblk.
        Unaligned : uint8 option
        /// `readonly.` (III.2.2) — applies to the next ldelema.
        Readonly : bool
    }

[<RequireQualifiedAccess>]
module PrefixState =
    let empty : PrefixState =
        {
            Constrained = None
            Volatile = false
            Tail = false
            Unaligned = None
            Readonly = false
        }


/// Whether a frame was entered by a `newobj` in its caller, and if so under which of
/// the CLI's two object-construction calling conventions. On return, this decides what
/// (if anything) gets pushed onto the caller's evaluation stack.
type ConstructionState =
    /// The frame was entered by an ordinary `call`/`callvirt`, not a `newobj`. Whatever
    /// the method's signature says it returns is what gets pushed.
    | NotConstructing
    /// Fixed-size object: `newobj` allocated the object *before* the constructor ran and
    /// passed its address as `this`, so the address is known up front. On return we push
    /// that address (or, for value types, the object's now-complete contents).
    | Constructing of ManagedHeapAddress

/// What `returnStackFrame` should do with the object a constructor frame was constructing,
/// once that constructor returns.
[<RequireQualifiedAccess>]
type ConstructedObjectDisposition =
    /// The ordinary `newobj` convention: push the constructed object (or, for value types,
    /// its now-complete contents) onto the caller's evaluation stack.
    | PushToCaller
    /// The runtime synthesised this exception and pushed its ctor frame itself (see
    /// `IlMachineStateExecution.raiseRuntimeException`). Dispatch the constructed object as
    /// a managed exception instead of pushing it.
    ///
    /// `message`, when present, overwrites `_message` *after* the ctor has run — it must be
    /// applied post-ctor, because the parameterless ctor sets `_message` to the type's
    /// default resource string and would otherwise clobber it. Use it where the CLR would
    /// have called a message-taking ctor overload that PawPrint cannot yet invoke (e.g.
    /// `IndexOutOfRangeException(SR.IndexOutOfRange_ArrayRankIndex)`); leave it `None` to
    /// accept the parameterless ctor's default, which is what the CLR produces when it
    /// throws the exception with no argument.
    | DispatchAsException of message : string option

type MethodReturnState =
    {
        /// Handle to the caller's frame
        JumpTo : FrameId
        WasInitialisingType : ConcreteTypeHandle option
        /// Whether a Newobj instruction in the caller is awaiting an object reference to be
        /// pushed immediately after Ret, and under which construction calling convention.
        Constructing : ConstructionState
        /// The IL offset of the call/callvirt/newobj instruction in the caller that created
        /// this frame. Exception dispatch must use this (not the caller's resumed IlOpIndex)
        /// so that handler lookup sees the call site inside the protected region, even when
        /// the advanced resume PC falls outside it.
        CallSiteIlOpIndex : int
        /// What to do with the constructed object (see `Constructing`) when this frame
        /// returns. Anything other than `PushToCaller` is set by `raiseRuntimeException`,
        /// which runs exception ctors via the dispatch loop.
        ConstructedObjectDisposition : ConstructedObjectDisposition
        /// When true, an exception escaping this frame is wrapped in a fresh
        /// `System.Reflection.TargetInvocationException` whose `_innerException` points at the
        /// original exception object. Used by the `Activator.CreateInstance<T>()` intrinsic to
        /// reproduce CoreCLR's `RuntimeType.CreateInstanceOfT` `try { ctor } catch (Exception e)
        /// { throw new TargetInvocationException(e); }` wrap without synthesising a trampoline
        /// frame. The wrap fires only on unwind across this frame's boundary, so a `try`/`catch`
        /// *inside* the ctor that handles the exception is unaffected.
        WrapExceptionInTargetInvocation : bool
    }

and MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliType ImmutableArray
        /// Index into the stream of IL bytes.
        _IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliType ImmutableArray
        ExecutingMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        StackMemoryPool : StackMemoryPool
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodReturnState option
        Generics : ImmutableArray<ConcreteTypeHandle>
        /// Track which exception regions are currently active (innermost first)
        ActiveExceptionRegions : ExceptionRegion list
        /// When executing finally/fault/filter bodies, we need to know how to resume.
        /// Nested EH inside those bodies pushes a new continuation over the outer one.
        ExceptionContinuations :
            ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        /// Active catch/filter handler body -> caught exception.
        /// TODO: replace with a push/pop active-catch stack so escaped handlers cannot leave stale entries.
        CatchExceptions : Map<ExceptionOffset, CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>>
        /// Prefix opcodes (constrained./volatile./tail./unaligned./readonly.) executed but
        /// not yet consumed by the following instruction. Reset to `PrefixState.empty` after
        /// consumption.
        PendingPrefix : PrefixState
        /// This frame's declaring type still needs its initialiser run, and no instruction of
        /// the frame may execute until it has. Cleared once the check reports nothing to do.
        ///
        /// This is a method prologue, which is where the CLR puts the check: the frame is
        /// established first, so a `.cctor` that throws produces a `TypeInitializationException`
        /// whose trace names *this* method, and a stack walk taken during the `.cctor` sees this
        /// frame. Running it at the call site instead — as PawPrint used to — loses both, and
        /// answers for the type named at the call site rather than the one whose method actually
        /// runs, which differ for an interface call.
        ///
        /// Deliberately survives the suspension paths. When the check pushes a `.cctor` frame, or
        /// parks the thread on another thread's initialisation, this frame is re-entered with the
        /// field still set and simply asks again.
        PendingTypeInit : ConcreteTypeHandle option
    }

    member this.IlOpIndex = this._IlOpIndex

    member this.ExceptionContinuation =
        this.ExceptionContinuations |> List.tryHead |> Option.map _.Continuation

    /// Set the program counter to an absolute byte offset from the start of the method.
    static member setProgramCounter (absoluteOffset : int) (state : MethodState) =
        let jumped =
            { state with
                _IlOpIndex = absoluteOffset
            }

        let newActiveRegions =
            ExceptionHandling.getActiveRegionsAtOffset jumped.IlOpIndex state.ExecutingMethod

        { jumped with
            ActiveExceptionRegions = newActiveRegions
        }


    static member jumpProgramCounter (bytes : int) (state : MethodState) =
        MethodState.setProgramCounter (state._IlOpIndex + bytes) state

    static member advanceProgramCounter (state : MethodState) =
        let instruction =
            match state.ExecutingMethod.Body with
            | MethodBody.Il instr -> instr.Locations.[state.IlOpIndex]
            | other ->
                failwith
                    $"advanceProgramCounter: executing method %O{state.ExecutingMethod} has no IL body (Body=%A{other})"

        MethodState.jumpProgramCounter (IlOp.NumberOfBytes instruction) state

    static member peekEvalStack (state : MethodState) : EvalStackValue option = EvalStack.Peek state.EvaluationStack

    static member clearEvalStack (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Empty
        }

    static member pushExceptionContinuation
        (scope : ExceptionContinuationScope)
        (cont : ExceptionContinuation<_, _, _>)
        (state : MethodState)
        : MethodState
        =
        match scope, cont with
        | ExceptionContinuationScope.FilterHandler _, ExceptionContinuation.ResumeAfterFilter _
        | ExceptionContinuationScope.FinallyHandler _, ExceptionContinuation.ResumeAfterFinally _
        | ExceptionContinuationScope.FinallyHandler _, ExceptionContinuation.PropagatingException _
        | ExceptionContinuationScope.FaultHandler _, ExceptionContinuation.PropagatingException _ -> ()
        | _ -> failwith $"Exception continuation scope %O{scope} does not match continuation %O{cont}"

        { state with
            ExceptionContinuations =
                {
                    Scope = scope
                    Continuation = cont
                }
                :: state.ExceptionContinuations
        }

    static member popExceptionContinuation
        (state : MethodState)
        : ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option * MethodState
        =
        match state.ExceptionContinuations with
        | [] -> None, state
        | head :: tail ->
            Some head,
            { state with
                ExceptionContinuations = tail
            }

    /// Store the full caught exception for `rethrow`, which must preserve the original
    /// stack trace rather than creating a fresh throw record from the eval-stack object.
    static member setCatchException
        (offset : ExceptionOffset)
        (exn : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : MethodState)
        : MethodState
        =
        { state with
            CatchExceptions = state.CatchExceptions |> Map.add offset exn
        }

    static member clearCatchException (offset : ExceptionOffset) (state : MethodState) : MethodState =
        { state with
            CatchExceptions = state.CatchExceptions |> Map.remove offset
        }

    /// Record that this frame's declaring type must be initialised before any of its
    /// instructions run. See `PendingTypeInit`.
    static member withPendingTypeInit (ty : ConcreteTypeHandle) (state : MethodState) : MethodState =
        { state with
            PendingTypeInit = Some ty
        }

    /// The prologue's check has reported nothing left to do, so the frame may execute.
    static member clearPendingTypeInit (state : MethodState) : MethodState =
        { state with
            PendingTypeInit = None
        }

    /// Clear any pending prefix opcodes. Must be called whenever the PC is set to a
    /// non-sequential target (exception handler entry, finally entry) so that a prefix
    /// set before the transfer cannot be consumed by an unrelated instruction in the handler.
    static member clearPendingPrefix (state : MethodState) : MethodState =
        { state with
            PendingPrefix = PrefixState.empty
        }

    static member pushToEvalStack' (e : EvalStackValue) (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Push' e state.EvaluationStack
        }

    static member pushToEvalStack (o : CliType) (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Push o state.EvaluationStack
        }

    /// Pop the eval stack into the given argument slot.
    static member popFromStackToArg (index : int) (state : MethodState) : MethodState =
        let popped, state = MethodState.popFromStack state

        let arg =
            if index < state.Arguments.Length then
                state.Arguments.[index]
            else
                failwith
                    $"Tried to get element {index} of the args list for method {state.ExecutingMethod.Name}, which has only {state.Arguments.Length} elements"

        let popped = EvalStackValue.toCliTypeCoerced arg popped

        { state with
            Arguments = state.Arguments.SetItem (index, popped)
        }

    static member loadArgument (index : int) (state : MethodState) : MethodState =
        // Correct CIL guarantees that we are loading an argument from an index that exists.
        MethodState.pushToEvalStack state.Arguments.[index] state

    static member popFromStack (state : MethodState) : EvalStackValue * MethodState =
        let popped, newStack = EvalStack.Pop state.EvaluationStack

        let state =
            { state with
                EvaluationStack = newStack
            }

        popped, state

    static member popFromStackToVariable (localVariableIndex : int) (state : MethodState) : MethodState =
        if localVariableIndex >= state.LocalVariables.Length then
            failwith
                $"Tried to access zero-indexed local variable %i{localVariableIndex} but only %i{state.LocalVariables.Length} exist"

        if localVariableIndex < 0 || localVariableIndex >= 65535 then
            failwith $"Incorrect CIL encountered: local variable index has value %i{localVariableIndex}"

        let popped, state = MethodState.popFromStack state

        let desiredValue =
            EvalStackValue.toCliTypeCoerced state.LocalVariables.[localVariableIndex] popped

        { state with
            LocalVariables = state.LocalVariables.SetItem (localVariableIndex, desiredValue)
        }

    /// `args` must be populated with entries of the right type.
    /// If `method` is an instance method, `args` must be of length 1+numParams.
    /// If `method` is static, `args` must be of length numParams.
    static member Empty
        (concreteTypes : AllConcreteTypes)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (containingAssembly : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (args : ImmutableArray<CliType>)
        (returnState : MethodReturnState option)
        : Result<MethodState, WoofWare.PawPrint.AssemblyReference list>
        =
        do
            let expectsThis = not method.IsStatic

            let expected = MethodInfo.arity method + (if expectsThis then 1 else 0)

            if args.Length <> expected then
                let shape =
                    if method.IsStatic then
                        "Static method"
                    else
                        "Non-static method"

                failwith
                    $"%s{shape} {method.Name} should have had %i{expected} parameters, but was given %i{args.Length}"

        let localVariableSig =
            match MethodInfo.tryIlBody method with
            | None -> ImmutableArray.Empty
            | Some instr ->
                match instr.LocalVars with
                | None -> ImmutableArray.Empty
                | Some vars -> vars
        // I think valid code should remain valid if we unconditionally localsInit - it should be undefined
        // to use an uninitialised value? Not checked this; TODO.

        let localVars =
            let result = ImmutableArray.CreateBuilder ()

            for var in localVariableSig do
                // Note: This assumes all types have already been concretized
                // If this fails with "ConcreteTypeHandle not found", it means
                // we need to ensure types are concretized before creating the MethodState
                //
                // Deliberately the non-loading walk. This function returns only a `MethodState`,
                // so it has nowhere to put an updated registry or load context; handing it a real
                // loader would let it mint `ConcreteTypeHandle`s into a registry that is then
                // discarded, leaving the locals' `FieldId`s pointing at handles the machine state
                // does not know. `Concretization.concretizeMethod` primes every local's type
                // before we get here, so a miss really is a bug — see
                // `IAssemblyLoad.alreadyLoadedOnly`.
                let zero, _, _ =
                    CliType.zeroOf IAssemblyLoad.alreadyLoadedOnly concreteTypes loadedAssemblies baseClassTypes var

                result.Add zero

            result.ToImmutable ()

        let activeRegions = ExceptionHandling.getActiveRegionsAtOffset 0 method

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
            _IlOpIndex = 0
            Arguments = args
            ExecutingMethod = method
            StackMemoryPool = StackMemoryPool.empty
            ReturnState = returnState
            Generics = methodGenerics
            ActiveExceptionRegions = activeRegions
            ExceptionContinuations = []
            CatchExceptions = Map.empty
            PendingPrefix = PrefixState.empty
            PendingTypeInit = None
        }
        |> Ok
