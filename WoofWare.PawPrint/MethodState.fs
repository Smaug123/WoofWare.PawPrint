namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type StackFrame =
    {
        Method : WoofWare.PawPrint.MethodInfo
        IlOffset : int
    }

type CliException =
    {
        /// The actual exception object on the heap
        ExceptionObject : ManagedHeapAddress
        /// The type of the exception (for catch matching)
        ExceptionType : TypeDefn
        StackTrace : StackFrame list
    }

type ExceptionContinuation =
    | ResumeAfterFinally of targetPC : int
    | PropagatingException of exn : CliException
    | ResumeAfterFilter of handlerPC : int * exn : CliException

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
        (method : WoofWare.PawPrint.MethodInfo)
        (args : ImmutableArray<CliType>)
        (returnState : MethodReturnState option)
        : MethodState
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
            // TODO: generics?
            localVariableSig
            |> Seq.map (CliType.zeroOf ImmutableArray.Empty)
            |> ImmutableArray.CreateRange

        let activeRegions =
            match method.Instructions with
            | None -> []
            | Some instructions ->
                instructions.ExceptionRegions
                |> Seq.filter (fun region ->
                    match region with
                    | ExceptionRegion.Catch (_, offset)
                    | ExceptionRegion.Finally offset
                    | ExceptionRegion.Fault offset
                    | ExceptionRegion.Filter (_, offset) ->
                        // We start at IL offset 0, check if we're inside any try blocks
                        0 >= offset.TryOffset && 0 < offset.TryOffset + offset.TryLength
                )
                |> Seq.toList

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

    static member private getActiveRegionsAtOffset
        (offset : int)
        (method : WoofWare.PawPrint.MethodInfo)
        : WoofWare.PawPrint.ExceptionRegion list
        =
        match method.Instructions with
        | None -> []
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.filter (fun region ->
                match region with
                | ExceptionRegion.Catch (_, exOffset)
                | ExceptionRegion.Finally exOffset
                | ExceptionRegion.Fault exOffset
                | ExceptionRegion.Filter (_, exOffset) ->
                    offset >= exOffset.TryOffset && offset < exOffset.TryOffset + exOffset.TryLength
            )
            |> Seq.toList

    static member updateActiveRegions (newOffset : int) (state : MethodState) : MethodState =
        let newActiveRegions =
            MethodState.getActiveRegionsAtOffset newOffset state.ExecutingMethod

        { state with
            ActiveExceptionRegions = newActiveRegions
        }

    static member findFinallyBlocksToRun
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo)
        : WoofWare.PawPrint.ExceptionRegion list
        =
        match method.Instructions with
        | None -> []
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.choose (fun region ->
                match region with
                | ExceptionRegion.Finally offset ->
                    // We're leaving if we're in the try block and target is outside
                    if
                        currentPC >= offset.TryOffset
                        && currentPC < offset.TryOffset + offset.TryLength
                        && (targetPC < offset.TryOffset || targetPC >= offset.TryOffset + offset.TryLength)
                    then
                        Some region
                    else
                        None
                | ExceptionRegion.Catch (_, _)
                | ExceptionRegion.Filter (_, _)
                | ExceptionRegion.Fault _ -> None
            )
            |> Seq.sortBy (fun region ->
                match region with
                | ExceptionRegion.Finally offset -> -offset.TryOffset
                | _ -> 0
            ) // Inner to outer
            |> Seq.toList

    /// Find the first matching exception handler for the given exception at the given PC
    static member findExceptionHandler
        (currentPC : int)
        (exceptionType : TypeDefn)
        (method : WoofWare.PawPrint.MethodInfo)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : (WoofWare.PawPrint.ExceptionRegion * bool) option // handler, isFinally
        =
        match method.Instructions with
        | None -> None
        | Some instructions ->
            // Find all handlers that cover the current PC
            instructions.ExceptionRegions
            |> Seq.tryPick (fun region ->
                match region with
                | ExceptionRegion.Catch (typeToken, offset) when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    // Check if exception type matches
                    // TODO: Resolve typeToken and check type compatibility
                    Some (region, false)
                | ExceptionRegion.Filter (filterOffset, offset) when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    // Filter needs to be evaluated
                    Some (region, false)
                | ExceptionRegion.Finally offset when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    Some (region, true)
                | ExceptionRegion.Fault offset when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    Some (region, true)
                | _ -> None
            )
