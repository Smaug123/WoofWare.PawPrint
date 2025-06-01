namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open WoofWare.PawPrint

// Exception types moved to ExceptionHandling.fs

type MethodReturnState =
    {
        /// Index in the MethodStates array of a ThreadState
        JumpTo : int
        WasInitialisingType : (TypeDefinitionHandle * AssemblyName) option
        /// The Newobj instruction means we need to push a reference immediately after Ret.
        WasConstructingObj : ManagedHeapAddress option
    }

and MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliType ImmutableArray
        /// Index into the stream of IL bytes.
        IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliType ImmutableArray
        ExecutingMethod : WoofWare.PawPrint.MethodInfo
        /// We don't implement the local memory pool right now
        LocalMemoryPool : unit
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodReturnState option
        /// Track which exception regions are currently active (innermost first)
        ActiveExceptionRegions : WoofWare.PawPrint.ExceptionRegion list
        /// When executing a finally/fault/filter, we need to know where to return
        ExceptionContinuation : ExceptionContinuation option
    }

    static member jumpProgramCounter (bytes : int) (state : MethodState) =
        { state with
            IlOpIndex = state.IlOpIndex + bytes
        }

    static member advanceProgramCounter (state : MethodState) =
        MethodState.jumpProgramCounter
            (IlOp.NumberOfBytes state.ExecutingMethod.Instructions.Value.Locations.[state.IlOpIndex])
            state

    static member peekEvalStack (state : MethodState) : EvalStackValue option = EvalStack.Peek state.EvaluationStack

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
        (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
        (containingAssembly : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo)
        (args : ImmutableArray<CliType>)
        (returnState : MethodReturnState option)
        : Result<MethodState, AssemblyReference list>
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

        let requiredAssemblies = ResizeArray ()

        let localVars =
            // TODO: generics?
            let result = ImmutableArray.CreateBuilder ()

            for var in localVariableSig do
                match CliType.zeroOf loadedAssemblies containingAssembly ImmutableArray.Empty var with
                | CliTypeResolutionResult.Resolved t -> result.Add t
                | CliTypeResolutionResult.FirstLoad assy -> requiredAssemblies.Add assy

            result.ToImmutable ()

        if requiredAssemblies.Count > 0 then
            Error (List.ofSeq requiredAssemblies)
        else

        let activeRegions = ExceptionHandling.getActiveRegionsAtOffset 0 method

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
            IlOpIndex = 0
            Arguments = args
            ExecutingMethod = method
            LocalMemoryPool = ()
            ReturnState = returnState
            ActiveExceptionRegions = activeRegions
            ExceptionContinuation = None
        }
        |> Ok

    static member updateActiveRegions (newOffset : int) (state : MethodState) : MethodState =
        let newActiveRegions =
            ExceptionHandling.getActiveRegionsAtOffset newOffset state.ExecutingMethod

        { state with
            ActiveExceptionRegions = newActiveRegions
        }
