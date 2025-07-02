namespace WoofWare.PawPrint

open System.Collections.Immutable

type MethodReturnState =
    {
        /// Index in the MethodStates array of a ThreadState
        JumpTo : int
        WasInitialisingType : ConcreteTypeHandle option
        /// The Newobj instruction means we need to push a reference immediately after Ret.
        WasConstructingObj : ManagedHeapAddress option
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
        /// We don't implement the local memory pool right now
        LocalMemoryPool : unit
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodReturnState option
        Generics : ImmutableArray<ConcreteTypeHandle>
        /// Track which exception regions are currently active (innermost first)
        ActiveExceptionRegions : ExceptionRegion list
        /// When executing a finally/fault/filter, we need to know where to return
        ExceptionContinuation : ExceptionContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
    }

    member this.IlOpIndex = this._IlOpIndex

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
            state.ExecutingMethod.Instructions.Value.Locations.[state.IlOpIndex]

        MethodState.jumpProgramCounter (IlOp.NumberOfBytes instruction) state

    static member peekEvalStack (state : MethodState) : EvalStackValue option = EvalStack.Peek state.EvaluationStack

    static member clearEvalStack (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Empty
        }

    static member setExceptionContinuation (cont : ExceptionContinuation<_, _, _>) (state : MethodState) : MethodState =
        { state with
            ExceptionContinuation = Some cont
        }

    static member clearExceptionContinuation (state : MethodState) : MethodState =
        { state with
            ExceptionContinuation = None
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
        (corelib : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
        (containingAssembly : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (args : ImmutableArray<CliType>)
        (returnState : MethodReturnState option)
        : Result<MethodState, WoofWare.PawPrint.AssemblyReference list>
        =
        do
            if method.IsStatic then
                if args.Length <> method.Parameters.Length then
                    failwith
                        $"Static method {method.Name} should have had %i{method.Parameters.Length} parameters, but was given %i{args.Length}"
            else if args.Length <> method.Parameters.Length + 1 then
                failwith
                    $"Non-static method {method.Name} should have had %i{method.Parameters.Length + 1} parameters, but was given %i{args.Length}"

        let localVariableSig =
            match method.Instructions with
            | None -> ImmutableArray.Empty
            | Some method ->
                match method.LocalVars with
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
                let zero, _ = CliType.zeroOf concreteTypes loadedAssemblies corelib var
                result.Add zero

            result.ToImmutable ()

        let activeRegions = ExceptionHandling.getActiveRegionsAtOffset 0 method

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
            _IlOpIndex = 0
            Arguments = args
            ExecutingMethod = method
            LocalMemoryPool = ()
            ReturnState = returnState
            Generics = methodGenerics
            ActiveExceptionRegions = activeRegions
            ExceptionContinuation = None
        }
        |> Ok
