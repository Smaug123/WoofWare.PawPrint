namespace WoofWare.PawPrint

#nowarn "42"

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type FieldSlot =
    {
        FieldName : string
        FieldSize : int
    }

type ManagedObject =
    {
        Fields : (string * CliType) list
        SyncBlock : unit
    }

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
    }

    static member jumpProgramCounter (bytes : int) (state : MethodState) =
        { state with
            IlOpIndex = state.IlOpIndex + bytes
        }

    static member advanceProgramCounter (state : MethodState) =
        MethodState.jumpProgramCounter (IlOp.NumberOfBytes state.ExecutingMethod.Locations.[state.IlOpIndex]) state

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
            match method.LocalVars with
            | None -> ImmutableArray.Empty
            | Some vars -> vars
        // I think valid code should remain valid if we unconditionally localsInit - it should be undefined
        // to use an uninitialised value? Not checked this; TODO.
        let localVars =
            localVariableSig |> Seq.map CliType.zeroOf |> ImmutableArray.CreateRange

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
            IlOpIndex = 0
            Arguments = args
            ExecutingMethod = method
            LocalMemoryPool = ()
            ReturnState = returnState
        }

type ThreadState =
    {
        // TODO: thread-local storage, synchronisation state, exception handling context
        MethodStates : MethodState ImmutableArray
        ActiveMethodState : int
        ActiveAssembly : AssemblyName
    }

    member this.MethodState = this.MethodStates.[this.ActiveMethodState]

    static member New (activeAssy : AssemblyName) (methodState : MethodState) =
        {
            ActiveMethodState = 0
            MethodStates = ImmutableArray.Create methodState
            ActiveAssembly = activeAssy
        }

    static member peekEvalStack (state : ThreadState) : EvalStackValue option =
        MethodState.peekEvalStack state.MethodStates.[state.ActiveMethodState]

    static member popFromEvalStack (state : ThreadState) : EvalStackValue * ThreadState =
        let activeMethodState = state.MethodStates.[state.ActiveMethodState]
        let ret, popped = activeMethodState |> MethodState.popFromStack

        let state =
            { state with
                MethodStates = state.MethodStates.SetItem (state.ActiveMethodState, popped)
            }

        ret, state

    static member pushToEvalStack (o : CliType) (methodStateIndex : int) (state : ThreadState) =
        let newMethodStates =
            state.MethodStates.SetItem (
                methodStateIndex,
                MethodState.pushToEvalStack o state.MethodStates.[methodStateIndex]
            )

        { state with
            MethodStates = newMethodStates
        }

    static member pushToEvalStack' (e : EvalStackValue) (methodStateIndex : int) (state : ThreadState) =
        let newMethodStates =
            state.MethodStates.SetItem (
                methodStateIndex,
                MethodState.pushToEvalStack' e state.MethodStates.[methodStateIndex]
            )

        { state with
            MethodStates = newMethodStates
        }

    static member jumpProgramCounter (bytes : int) (state : ThreadState) =
        let methodState =
            state.MethodStates.SetItem (
                state.ActiveMethodState,
                state.MethodStates.[state.ActiveMethodState]
                |> MethodState.jumpProgramCounter bytes
            )

        { state with
            MethodStates = methodState
        }

    static member advanceProgramCounter (state : ThreadState) =
        let methodState =
            state.MethodStates.SetItem (
                state.ActiveMethodState,
                state.MethodStates.[state.ActiveMethodState]
                |> MethodState.advanceProgramCounter
            )

        { state with
            MethodStates = methodState
        }

    static member loadArgument (i : int) (state : ThreadState) =
        let methodState =
            state.MethodStates.SetItem (
                state.ActiveMethodState,
                state.MethodStates.[state.ActiveMethodState] |> MethodState.loadArgument i
            )

        { state with
            MethodStates = methodState
        }

type WhatWeDid =
    | Executed
    /// We didn't run what you wanted, because we have to do class initialisation first.
    | SuspendedForClassInit
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId

type IlMachineState =
    {
        NextThreadId : int
        // CallStack : StackFrame list
        /// Multiple managed heaps are allowed, but we hopefully only need one.
        ManagedHeap : ManagedHeap
        ThreadState : Map<ThreadId, ThreadState>
        InternedStrings : ImmutableDictionary<StringToken, ManagedHeapAddress>
        /// Keyed by FullName. (Sometimes an assembly has a PublicKey when we read it from the disk, but we
        /// only have a reference to it by an AssemblyName without a PublicKey.)
        _LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
        /// Tracks initialization state of types across assemblies
        TypeInitTable : TypeInitTable
        Statics : ImmutableDictionary<TypeDefinitionHandle * AssemblyName, CliType>
        DotnetRuntimeDirs : string ImmutableArray
    }

    member this.WithLoadedAssembly (name : AssemblyName) (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.Add (name.FullName, value)
        }

    member this.LoadedAssembly (name : AssemblyName) : DumpedAssembly option =
        match this._LoadedAssemblies.TryGetValue name.FullName with
        | false, _ -> None
        | true, v -> Some v

    /// Returns also the original assembly name.
    member this.WithThreadSwitchedToAssembly (assy : AssemblyName) (thread : ThreadId) : IlMachineState * AssemblyName =
        let mutable existing = Unchecked.defaultof<AssemblyName>

        let newState =
            this.ThreadState
            |> Map.change
                thread
                (fun s ->
                    match s with
                    | None -> failwith $"expected thread {thread} to be in a state already; internal logic error"
                    | Some s ->
                        existing <- s.ActiveAssembly

                        { s with
                            ActiveAssembly = assy
                        }
                        |> Some
                )

        { this with
            ThreadState = newState
        },
        existing

    member this.ActiveAssembly (thread : ThreadId) =
        let active = this.ThreadState.[thread].ActiveAssembly

        match this.LoadedAssembly active with
        | Some v -> v
        | None ->
            let available = this._LoadedAssemblies.Keys |> String.concat " ; "

            failwith
                $"Somehow we believe the active assembly is {active}, but only had the following available: {available}"

type StateLoadResult =
    /// The type is loaded; you can proceed.
    | NothingToDo of IlMachineState
    /// We didn't manage to load the requested type, because that type itself requires first loading something.
    /// The state we give you is ready to load that something.
    | FirstLoadThis of IlMachineState

[<RequireQualifiedAccess>]
module IlMachineState =
    type private Dummy = class end

    let loadAssembly
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * AssemblyName
        =
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]
        let assemblyName = assemblyRef.Name

        match state.LoadedAssembly assemblyName with
        | Some v -> state, v, assemblyName
        | None ->
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            let assy =
                state.DotnetRuntimeDirs
                |> Seq.choose (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        use f = File.OpenRead file
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.read loggerFactory f |> Some
                    with :? FileNotFoundException ->
                        None
                )
                |> Seq.toList

            match assy with
            | [] -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | _ :: _ :: _ -> failwith $"Found multiple DLLs in runtime dirs with name %s{assemblyName.Name}.dll"
            | [ assy ] ->

            state.WithLoadedAssembly assemblyName assy, assy, assemblyName

    let rec internal resolveTypeFromName
        (loggerFactory : ILoggerFactory)
        (ns : string option)
        (name : string)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match ns with
        | None -> failwith "what are the semantics here"
        | Some ns ->

        match assy.TypeDef ns name with
        | Some typeDef ->
            // If resolved from TypeDef, it won't have generic parameters, I hope?
            let typeDef =
                typeDef
                |> TypeInfo.mapGeneric (fun _ -> failwith<TypeDefn> "no generic parameters")

            state, assy, typeDef
        | None ->

        match assy.TypeRef ns name with
        | Some typeRef -> resolveTypeFromRef loggerFactory assy typeRef state
        | None ->

        match assy.ExportedType (Some ns) name with
        | Some export -> resolveTypeFromExport loggerFactory assy export state
        | None -> failwith $"TODO: type resolution unimplemented for {ns} {name}"

    and resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match ty.Data with
        | NonForwarded _ -> failwith "Somehow didn't find type definition but it is exported"
        | ForwardsTo assy ->
            let state, targetAssy, _ = loadAssembly loggerFactory fromAssembly assy state
            resolveTypeFromName loggerFactory ty.Namespace ty.Name targetAssy state

    and resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match target.ResolutionScope with
        | AssemblyReference r ->
            let state, assy, newAssyName =
                loadAssembly loggerFactory referencedInAssembly r state

            let nsPath = target.Namespace.Split '.' |> Array.toList

            let targetNs = assy.NonRootNamespaces.[nsPath]

            let targetType =
                targetNs.TypeDefinitions
                |> Seq.choose (fun td ->
                    let ty = assy.TypeDefs.[td]

                    if ty.Name = target.Name && ty.Namespace = target.Namespace then
                        Some ty
                    else
                        None
                )
                |> Seq.toList

            match targetType with
            | [ t ] ->
                // If resolved from TypeDef (above), it won't have generic parameters, I hope?
                let t =
                    t |> TypeInfo.mapGeneric (fun _ -> failwith<TypeDefn> "no generic parameters")

                state, assy, t
            | _ :: _ :: _ -> failwith $"Multiple matching type definitions! {nsPath} {target.Name}"
            | [] ->
                match assy.ExportedType (Some target.Namespace) target.Name with
                | None -> failwith $"Failed to find type {nsPath} {target.Name} in {assy.Name.FullName}!"
                | Some ty -> resolveTypeFromExport loggerFactory assy ty state
        | k -> failwith $"Unexpected: {k}"

    and resolveType
        (loggerFactory : ILoggerFactory)
        (ty : TypeReferenceHandle)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        let target = assy.TypeRefs.[ty]

        resolveTypeFromRef loggerFactory assy target state

    let rec resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (ty : TypeDefn)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState *
          DumpedAssembly *
          WoofWare.PawPrint.TypeInfo<WoofWare.PawPrint.GenericParameter> *
          TypeDefn ImmutableArray option
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let state, _, generic, subArgs =
                resolveTypeFromDefn loggerFactory generic assy state

            match subArgs with
            | Some _ -> failwith "unexpectedly had multiple generic instantiations for the same type"
            | None ->

            state, assy, generic, Some args
        | TypeDefn.FromDefinition (defn, _typeKind) -> state, assy, assy.TypeDefs.[defn], None
        | s -> failwith $"TODO: resolveTypeFromDefn unimplemented for {s}"

    let rec resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        let state, assy, generic, args =
            resolveTypeFromDefn loggerFactory assy.TypeSpecs.[ty].Signature assy state

        match args with
        | None ->
            let generic =
                generic
                |> TypeInfo.mapGeneric (fun _ -> failwith<TypeDefn> "no generic parameters")

            state, assy, generic
        | Some args ->
            let generic = TypeInfo.withGenerics args generic
            state, assy, generic

    let callMethod
        (wasInitialising : (TypeDefinitionHandle * AssemblyName) option)
        (wasConstructing : ManagedHeapAddress option)
        (methodToCall : WoofWare.PawPrint.MethodInfo)
        (thread : ThreadId)
        (threadState : ThreadState)
        (state : IlMachineState)
        : IlMachineState
        =
        let activeMethodState = threadState.MethodStates.[threadState.ActiveMethodState]

        let newFrame, oldFrame =
            if methodToCall.IsStatic then
                let args = ImmutableArray.CreateBuilder methodToCall.Parameters.Length
                let mutable afterPop = activeMethodState

                for i = 0 to methodToCall.Parameters.Length - 1 do
                    let poppedArg, afterPop' = afterPop |> MethodState.popFromStack
                    let zeroArg = CliType.zeroOf methodToCall.Signature.ParameterTypes.[i]
                    let poppedArg = EvalStackValue.toCliTypeCoerced zeroArg poppedArg
                    afterPop <- afterPop'
                    args.Add poppedArg

                let newFrame =
                    MethodState.Empty
                        methodToCall
                        (args.ToImmutable ())
                        (Some
                            {
                                JumpTo = threadState.ActiveMethodState
                                WasInitialisingType = None
                                WasConstructingObj = wasConstructing
                            })

                let oldFrame = afterPop |> MethodState.advanceProgramCounter
                newFrame, oldFrame
            else
                let args = ImmutableArray.CreateBuilder (methodToCall.Parameters.Length + 1)
                let poppedArg, afterPop = activeMethodState |> MethodState.popFromStack
                // it only matters that the RuntimePointer is a RuntimePointer, so that the coercion has a target of the
                // right shape
                args.Add (
                    EvalStackValue.toCliTypeCoerced (CliType.RuntimePointer (CliRuntimePointer.Unmanaged ())) poppedArg
                )

                let mutable afterPop = afterPop

                for i = 1 to methodToCall.Parameters.Length do
                    let poppedArg, afterPop' = afterPop |> MethodState.popFromStack
                    let zeroArg = CliType.zeroOf methodToCall.Signature.ParameterTypes.[i - 1]
                    let poppedArg = EvalStackValue.toCliTypeCoerced zeroArg poppedArg
                    afterPop <- afterPop'
                    args.Add poppedArg

                let newFrame =
                    MethodState.Empty
                        methodToCall
                        (args.ToImmutable ())
                        (Some
                            {
                                JumpTo = threadState.ActiveMethodState
                                WasInitialisingType = wasInitialising
                                WasConstructingObj = wasConstructing
                            })

                let oldFrame = afterPop |> MethodState.advanceProgramCounter
                newFrame, oldFrame

        let newThreadState =
            { threadState with
                MethodStates = threadState.MethodStates.Add(newFrame).SetItem (threadState.ActiveMethodState, oldFrame)
                ActiveMethodState = threadState.MethodStates.Length
            }

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let rec loadClass
        (loggerFactory : ILoggerFactory)
        (typeDefHandle : TypeDefinitionHandle)
        (assemblyName : AssemblyName)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : StateLoadResult
        =
        if typeDefHandle.IsNil then
            failwith "Called `loadClass` with a nil typedef"

        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

        match state.TypeInitTable.TryGetValue ((typeDefHandle, assemblyName)) with
        | true, TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | true, TypeInitState.InProgress tid when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | true, TypeInitState.InProgress _ ->
            // This is usually signalled by WhatWeDid.Blocked
            failwith
                "TODO: cross-thread class init synchronization unimplemented - this thread has to wait for the other thread to finish initialisation"
        | false, _ ->
            // We have work to do!

            let state, origAssyName =
                state.WithThreadSwitchedToAssembly assemblyName currentThread

            let sourceAssembly = state.LoadedAssembly assemblyName |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue typeDefHandle with
                | false, _ -> failwith $"Failed to find type definition {typeDefHandle} in {assemblyName.Name}"
                | true, v -> v

            logger.LogDebug ("Resolving type {TypeDefNamespace}.{TypeDefName}", typeDef.Namespace, typeDef.Name)

            // First mark as in-progress to detect cycles
            let state =
                { state with
                    TypeInitTable =
                        state.TypeInitTable
                        |> TypeInitTable.beginInitialising currentThread (typeDefHandle, assemblyName)
                }

            // Check if the type has a base type that needs initialization
            let firstDoBaseClass =
                match typeDef.BaseType with
                | Some baseTypeInfo ->
                    // Determine if base type is in the same or different assembly
                    match baseTypeInfo with
                    | ForeignAssemblyType (baseAssemblyName, baseTypeHandle) ->
                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to foreign assembly {ForeignAssemblyName}",
                            typeDef.Namespace,
                            typeDef.Name,
                            baseAssemblyName.Name
                        )

                        match loadClass loggerFactory baseTypeHandle baseAssemblyName currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | TypeDef typeDefinitionHandle ->
                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to this assembly, typedef",
                            typeDef.Namespace,
                            typeDef.Name
                        )

                        match loadClass loggerFactory typeDefinitionHandle assemblyName currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | TypeRef typeReferenceHandle ->
                        let state, assy, targetType =
                            resolveType loggerFactory typeReferenceHandle (state.ActiveAssembly currentThread) state

                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to a typeref in assembly {ResolvedAssemblyName}, {BaseTypeNamespace}.{BaseTypeName}",
                            typeDef.Namespace,
                            typeDef.Name,
                            assy.Name.Name,
                            targetType.Namespace,
                            targetType.Name
                        )

                        match loadClass loggerFactory targetType.TypeDefHandle assy.Name currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | TypeSpec typeSpecificationHandle -> failwith "TODO: TypeSpec base type loading unimplemented"
                | None -> Ok state // No base type (or it's System.Object)

            match firstDoBaseClass with
            | Error state -> FirstLoadThis state
            | Ok state ->

            // Find the class constructor (.cctor) if it exists
            let cctor =
                typeDef.Methods
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && method.Parameters.IsEmpty)

            match cctor with
            | Some ctorMethod ->
                // Call the class constructor! Note that we *don't* use `callMethodInActiveAssembly`, because that
                // performs class loading, but we're already in the middle of loading this class.
                // TODO: factor out the common bit.
                let currentThreadState = state.ThreadState.[currentThread]

                callMethod (Some (typeDefHandle, assemblyName)) None ctorMethod currentThread currentThreadState state
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state =
                    { state with
                        TypeInitTable =
                            state.TypeInitTable
                            |> TypeInitTable.markInitialised currentThread (typeDefHandle, assemblyName)
                    }

                // Restore original assembly context if needed
                state.WithThreadSwitchedToAssembly origAssyName currentThread
                |> fst
                |> NothingToDo

    let callMethodInActiveAssembly
        (loggerFactory : ILoggerFactory)
        (thread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo)
        (weAreConstructingObj : ManagedHeapAddress option)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let threadState = state.ThreadState.[thread]

        match state.TypeInitTable.TryGetValue methodToCall.DeclaringType with
        | false, _ ->
            match
                loadClass loggerFactory (fst methodToCall.DeclaringType) (snd methodToCall.DeclaringType) thread state
            with
            | NothingToDo state ->
                callMethod None weAreConstructingObj methodToCall thread threadState state, WhatWeDid.Executed
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | true, TypeInitState.Initialized ->
            callMethod None weAreConstructingObj methodToCall thread threadState state, WhatWeDid.Executed
        | true, InProgress threadId -> state, WhatWeDid.BlockedOnClassInit threadId

    let initial (dotnetRuntimeDirs : ImmutableArray<string>) (entryAssembly : DumpedAssembly) : IlMachineState =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name

        let state =
            {
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.Empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
            }

        state.WithLoadedAssembly assyName entryAssembly

    let addThread
        (newThreadState : MethodState)
        (newThreadAssy : AssemblyName)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState =
                    state.ThreadState
                    |> Map.add thread (ThreadState.New newThreadAssy newThreadState)
            }

        newState, thread

    // let allocate (o : CliObject) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
    //     let alloc, heap = ManagedHeap.Allocate o state.ManagedHeap

    //     let state =
    //         { state with
    //             ManagedHeap = heap
    //         }

    //     alloc, state

    let allocateArray
        (zeroOfType : unit -> CliType)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                Length = len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.AllocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.AllocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.SetStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject<'generic>
        (typeInfo : WoofWare.PawPrint.TypeInfo<'generic>)
        (fields : (string * CliType) list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Fields = Map.ofList fields
                Type = TypeInfoCrate.make typeInfo
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.AllocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let pushToEvalStack' (o : EvalStackValue) (thread : ThreadId) (state : IlMachineState) =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack' o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let pushToEvalStack (o : CliType) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let popFromStackToLocalVariable
        (thread : ThreadId)
        (localVariableIndex : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToVariable
                localVariableIndex
                threadState.MethodStates.[threadState.ActiveMethodState]

        { state with
            ThreadState =
                state.ThreadState
                |> Map.add
                    thread
                    { threadState with
                        MethodStates = threadState.MethodStates.SetItem (threadState.ActiveMethodState, methodState)
                    }
        }

    let peekEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue option =
        ThreadState.peekEvalStack state.ThreadState.[thread]

    let popEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue * IlMachineState =
        let ret, popped = ThreadState.popFromEvalStack state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread popped
            }

        ret, state

    let setArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.SetArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let advanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.advanceProgramCounter |> Some
                    )
        }

    let jumpProgramCounter (thread : ThreadId) (bytes : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.jumpProgramCounter bytes |> Some
                    )
        }

    let loadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some state -> state |> ThreadState.loadArgument index |> Some
                    )
        }

type ExecutionResult =
    | Terminated of IlMachineState * terminatingThread : ThreadId
    | Stepped of IlMachineState * WhatWeDid

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let internal executeNullary
        (state : IlMachineState)
        (currentThread : ThreadId)
        (op : NullaryIlOp)
        : ExecutionResult
        =
        match op with
        | Nop ->
            (IlMachineState.advanceProgramCounter currentThread state, WhatWeDid.Executed)
            |> ExecutionResult.Stepped
        | LdArg0 ->
            state
            |> IlMachineState.loadArgument currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg1 ->
            state
            |> IlMachineState.loadArgument currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg2 ->
            state
            |> IlMachineState.loadArgument currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg3 ->
            state
            |> IlMachineState.loadArgument currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_0 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[0]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_1 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[1]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_2 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[2]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_3 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[3]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Pop -> failwith "TODO: Pop unimplemented"
        | Dup ->
            let topValue =
                match IlMachineState.peekEvalStack currentThread state with
                | None -> failwith "tried to Dup when nothing on top of stack"
                | Some v -> v

            state
            |> IlMachineState.pushToEvalStack' topValue currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ret ->
            let threadStateAtEndOfMethod = state.ThreadState.[currentThread]

            match threadStateAtEndOfMethod.MethodState.ReturnState with
            | None -> ExecutionResult.Terminated (state, currentThread)
            | Some returnState ->

            let state =
                match returnState.WasInitialisingType with
                | None -> state
                | Some finishedInitialising ->
                    { state with
                        TypeInitTable =
                            state.TypeInitTable
                            |> TypeInitTable.markInitialised currentThread finishedInitialising
                    }

            // Return to previous stack frame
            let state =
                { state with
                    ThreadState =
                        state.ThreadState
                        |> Map.add
                            currentThread
                            { threadStateAtEndOfMethod with
                                ActiveMethodState = returnState.JumpTo
                            }
                }

            let state =
                match returnState.WasConstructingObj with
                | Some constructing ->
                    // Assumption: a constructor can't also return a value.
                    state
                    |> IlMachineState.pushToEvalStack (CliType.OfManagedObject constructing) currentThread
                | None ->
                    match threadStateAtEndOfMethod.MethodState.EvaluationStack.Values with
                    | [] ->
                        // no return value
                        state
                    | [ retVal ] ->
                        let retType =
                            threadStateAtEndOfMethod.MethodState.ExecutingMethod.Signature.ReturnType

                        let toPush = EvalStackValue.toCliTypeCoerced (CliType.zeroOf retType) retVal

                        state |> IlMachineState.pushToEvalStack toPush currentThread
                    | _ ->
                        failwith
                            "Unexpected interpretation result has a local evaluation stack with more than one element on RET"

            state |> Tuple.withRight WhatWeDid.Executed |> ExecutionResult.Stepped

        | LdcI4_0 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_1 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_2 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 2)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_3 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 3)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_4 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 4)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_5 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 5)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_6 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 6)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_7 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 7)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_8 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 8)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_m1 -> failwith "TODO: LdcI4_m1 unimplemented"
        | LdNull -> failwith "TODO: LdNull unimplemented"
        | Ceq -> failwith "TODO: Ceq unimplemented"
        | Cgt -> failwith "TODO: Cgt unimplemented"
        | Cgt_un -> failwith "TODO: Cgt_un unimplemented"
        | Clt ->
            let var2, state = state |> IlMachineState.popEvalStack currentThread
            let var1, state = state |> IlMachineState.popEvalStack currentThread

            let comparisonResult =
                match var1, var2 with
                | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> if var1 < var2 then 1 else 0
                | EvalStackValue.Float var1, EvalStackValue.Float var2 ->
                    failwith "TODO: Clt float comparison unimplemented"
                | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
                    failwith $"Clt instruction invalid for comparing object refs, {var1} vs {var2}"
                | EvalStackValue.ObjectRef var1, other -> failwith $"invalid comparison, ref %O{var1} vs %O{other}"
                | other, EvalStackValue.ObjectRef var2 -> failwith $"invalid comparison, %O{other} vs ref %O{var2}"
                | EvalStackValue.Float i, other -> failwith $"invalid comparison, float %f{i} vs %O{other}"
                | other, EvalStackValue.Float i -> failwith $"invalid comparison, %O{other} vs float %f{i}"
                | EvalStackValue.Int64 i, other -> failwith $"invalid comparison, int64 %i{i} vs %O{other}"
                | other, EvalStackValue.Int64 i -> failwith $"invalid comparison, %O{other} vs int64 %i{i}"
                | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> if var1 < var2 then 1 else 0
                | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
                    failwith "TODO: Clt Int32 vs NativeInt comparison unimplemented"
                | EvalStackValue.Int32 i, other -> failwith $"invalid comparison, int32 %i{i} vs %O{other}"
                | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
                    failwith "TODO: Clt NativeInt vs Int32 comparison unimplemented"
                | other, EvalStackValue.Int32 var2 -> failwith $"invalid comparison, {other} vs int32 {var2}"
                | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> if var1 < var2 then 1 else 0
                | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
                | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
                    failwith "TODO: Clt ManagedPointer vs NativeInt comparison unimplemented"
                | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
                    failwith "TODO: Clt ManagedPointer vs ManagedPointer comparison unimplemented"
                | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType ->
                    failwith "TODO: Clt ManagedPointer vs UserDefinedValueType comparison unimplemented"
                | EvalStackValue.UserDefinedValueType, NativeInt int64 ->
                    failwith "TODO: Clt UserDefinedValueType vs NativeInt comparison unimplemented"
                | EvalStackValue.UserDefinedValueType, ManagedPointer managedPointerSource ->
                    failwith "TODO: Clt UserDefinedValueType vs ManagedPointer comparison unimplemented"
                | EvalStackValue.UserDefinedValueType, UserDefinedValueType ->
                    failwith "TODO: Clt UserDefinedValueType vs UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 comparisonResult) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Clt_un -> failwith "TODO: Clt_un unimplemented"
        | Stloc_0 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_1 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_2 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_3 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Sub -> failwith "TODO: Sub unimplemented"
        | Sub_ovf -> failwith "TODO: Sub_ovf unimplemented"
        | Sub_ovf_un -> failwith "TODO: Sub_ovf_un unimplemented"
        | Add ->
            let val1, state = IlMachineState.popEvalStack currentThread state
            let val2, state = IlMachineState.popEvalStack currentThread state
            // see table at https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes.add?view=net-9.0
            let result =
                match val1, val2 with
                | EvalStackValue.Int32 val1, EvalStackValue.Int32 val2 ->
                    (# "add" val1 val2 : int32 #) |> EvalStackValue.Int32
                | EvalStackValue.Int32 val1, EvalStackValue.NativeInt val2 -> failwith "" |> EvalStackValue.NativeInt
                | EvalStackValue.Int32 val1, EvalStackValue.ManagedPointer val2 ->
                    failwith "" |> EvalStackValue.ManagedPointer
                | EvalStackValue.Int32 val1, EvalStackValue.ObjectRef val2 -> failwith "" |> EvalStackValue.ObjectRef
                | EvalStackValue.Int64 val1, EvalStackValue.Int64 val2 ->
                    (# "add" val1 val2 : int64 #) |> EvalStackValue.Int64
                | EvalStackValue.NativeInt val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt val1, EvalStackValue.NativeInt val2 ->
                    failwith "" |> EvalStackValue.NativeInt
                | EvalStackValue.NativeInt val1, EvalStackValue.ManagedPointer val2 ->
                    failwith "" |> EvalStackValue.ManagedPointer
                | EvalStackValue.NativeInt val1, EvalStackValue.ObjectRef val2 ->
                    failwith "" |> EvalStackValue.ObjectRef
                | EvalStackValue.Float val1, EvalStackValue.Float val2 ->
                    (# "add" val1 val2 : float #) |> EvalStackValue.Float
                | EvalStackValue.ManagedPointer val1, EvalStackValue.NativeInt val2 ->
                    failwith "" |> EvalStackValue.ManagedPointer
                | EvalStackValue.ObjectRef val1, EvalStackValue.NativeInt val2 ->
                    failwith "" |> EvalStackValue.ObjectRef
                | EvalStackValue.ManagedPointer val1, EvalStackValue.Int32 val2 ->
                    failwith "" |> EvalStackValue.ManagedPointer
                | EvalStackValue.ObjectRef val1, EvalStackValue.Int32 val2 -> failwith "" |> EvalStackValue.ObjectRef
                | val1, val2 -> failwith $"invalid add operation: {val1} and {val2}"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Add_ovf -> failwith "TODO: Add_ovf unimplemented"
        | Add_ovf_un -> failwith "TODO: Add_ovf_un unimplemented"
        | Mul -> failwith "TODO: Mul unimplemented"
        | Mul_ovf -> failwith "TODO: Mul_ovf unimplemented"
        | Mul_ovf_un -> failwith "TODO: Mul_ovf_un unimplemented"
        | Div -> failwith "TODO: Div unimplemented"
        | Div_un -> failwith "TODO: Div_un unimplemented"
        | Shr -> failwith "TODO: Shr unimplemented"
        | Shr_un -> failwith "TODO: Shr_un unimplemented"
        | Shl -> failwith "TODO: Shl unimplemented"
        | And -> failwith "TODO: And unimplemented"
        | Or -> failwith "TODO: Or unimplemented"
        | Xor -> failwith "TODO: Xor unimplemented"
        | Conv_I -> failwith "TODO: Conv_I unimplemented"
        | Conv_I1 -> failwith "TODO: Conv_I1 unimplemented"
        | Conv_I2 -> failwith "TODO: Conv_I2 unimplemented"
        | Conv_I4 -> failwith "TODO: Conv_I4 unimplemented"
        | Conv_I8 -> failwith "TODO: Conv_I8 unimplemented"
        | Conv_R4 -> failwith "TODO: Conv_R4 unimplemented"
        | Conv_R8 -> failwith "TODO: Conv_R8 unimplemented"
        | Conv_U ->
            let popped, state = IlMachineState.popEvalStack currentThread state
            let converted = EvalStackValue.toUnsignedNativeInt popped

            let state =
                match converted with
                | None -> failwith "TODO: Conv_U conversion failure unimplemented"
                | Some conv ->
                    // > If overflow occurs when converting one integer type to another, the high-order bits are silently truncated.
                    let conv =
                        match conv with
                        | UnsignedNativeIntSource.Verbatim conv ->
                            if conv > uint64 System.Int64.MaxValue then
                                (conv % uint64 System.Int64.MaxValue) |> int64 |> NativeIntSource.Verbatim
                            else
                                int64 conv |> NativeIntSource.Verbatim

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt conv) currentThread

            let state = state |> IlMachineState.advanceProgramCounter currentThread

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
        | Conv_U1 -> failwith "TODO: Conv_U1 unimplemented"
        | Conv_U2 -> failwith "TODO: Conv_U2 unimplemented"
        | Conv_U4 -> failwith "TODO: Conv_U4 unimplemented"
        | Conv_U8 -> failwith "TODO: Conv_U8 unimplemented"
        | LdLen -> failwith "TODO: LdLen unimplemented"
        | Endfilter -> failwith "TODO: Endfilter unimplemented"
        | Endfinally -> failwith "TODO: Endfinally unimplemented"
        | Rethrow -> failwith "TODO: Rethrow unimplemented"
        | Throw -> failwith "TODO: Throw unimplemented"
        | Localloc -> failwith "TODO: Localloc unimplemented"
        | Stind_I -> failwith "TODO: Stind_I unimplemented"
        | Stind_I1 -> failwith "TODO: Stind_I1 unimplemented"
        | Stind_I2 -> failwith "TODO: Stind_I2 unimplemented"
        | Stind_I4 -> failwith "TODO: Stind_I4 unimplemented"
        | Stind_I8 -> failwith "TODO: Stind_I8 unimplemented"
        | Stind_R4 -> failwith "TODO: Stind_R4 unimplemented"
        | Stind_R8 -> failwith "TODO: Stind_R8 unimplemented"
        | Ldind_i -> failwith "TODO: Ldind_i unimplemented"
        | Ldind_i1 -> failwith "TODO: Ldind_i1 unimplemented"
        | Ldind_i2 -> failwith "TODO: Ldind_i2 unimplemented"
        | Ldind_i4 -> failwith "TODO: Ldind_i4 unimplemented"
        | Ldind_i8 -> failwith "TODO: Ldind_i8 unimplemented"
        | Ldind_u1 -> failwith "TODO: Ldind_u1 unimplemented"
        | Ldind_u2 -> failwith "TODO: Ldind_u2 unimplemented"
        | Ldind_u4 -> failwith "TODO: Ldind_u4 unimplemented"
        | Ldind_u8 -> failwith "TODO: Ldind_u8 unimplemented"
        | Ldind_r4 -> failwith "TODO: Ldind_r4 unimplemented"
        | Ldind_r8 -> failwith "TODO: Ldind_r8 unimplemented"
        | Rem -> failwith "TODO: Rem unimplemented"
        | Rem_un -> failwith "TODO: Rem_un unimplemented"
        | Volatile -> failwith "TODO: Volatile unimplemented"
        | Tail -> failwith "TODO: Tail unimplemented"
        | Conv_ovf_i_un -> failwith "TODO: Conv_ovf_i_un unimplemented"
        | Conv_ovf_u_un -> failwith "TODO: Conv_ovf_u_un unimplemented"
        | Conv_ovf_i1_un -> failwith "TODO: Conv_ovf_i1_un unimplemented"
        | Conv_ovf_u1_un -> failwith "TODO: Conv_ovf_u1_un unimplemented"
        | Conv_ovf_i2_un -> failwith "TODO: Conv_ovf_i2_un unimplemented"
        | Conv_ovf_u2_un -> failwith "TODO: Conv_ovf_u2_un unimplemented"
        | Conv_ovf_i4_un -> failwith "TODO: Conv_ovf_i4_un unimplemented"
        | Conv_ovf_u4_un -> failwith "TODO: Conv_ovf_u4_un unimplemented"
        | Conv_ovf_i8_un -> failwith "TODO: Conv_ovf_i8_un unimplemented"
        | Conv_ovf_u8_un -> failwith "TODO: Conv_ovf_u8_un unimplemented"
        | Conv_ovf_i -> failwith "TODO: Conv_ovf_i unimplemented"
        | Conv_ovf_u -> failwith "TODO: Conv_ovf_u unimplemented"
        | Neg -> failwith "TODO: Neg unimplemented"
        | Not -> failwith "TODO: Not unimplemented"
        | Ldind_ref -> failwith "TODO: Ldind_ref unimplemented"
        | Stind_ref -> failwith "TODO: Stind_ref unimplemented"
        | Ldelem_i -> failwith "TODO: Ldelem_i unimplemented"
        | Ldelem_i1 -> failwith "TODO: Ldelem_i1 unimplemented"
        | Ldelem_u1 -> failwith "TODO: Ldelem_u1 unimplemented"
        | Ldelem_i2 -> failwith "TODO: Ldelem_i2 unimplemented"
        | Ldelem_u2 -> failwith "TODO: Ldelem_u2 unimplemented"
        | Ldelem_i4 -> failwith "TODO: Ldelem_i4 unimplemented"
        | Ldelem_u4 -> failwith "TODO: Ldelem_u4 unimplemented"
        | Ldelem_i8 -> failwith "TODO: Ldelem_i8 unimplemented"
        | Ldelem_u8 -> failwith "TODO: Ldelem_u8 unimplemented"
        | Ldelem_r4 -> failwith "TODO: Ldelem_r4 unimplemented"
        | Ldelem_r8 -> failwith "TODO: Ldelem_r8 unimplemented"
        | Ldelem_ref -> failwith "TODO: Ldelem_ref unimplemented"
        | Stelem_i -> failwith "TODO: Stelem_i unimplemented"
        | Stelem_i1 -> failwith "TODO: Stelem_i1 unimplemented"
        | Stelem_u1 -> failwith "TODO: Stelem_u1 unimplemented"
        | Stelem_i2 -> failwith "TODO: Stelem_i2 unimplemented"
        | Stelem_u2 -> failwith "TODO: Stelem_u2 unimplemented"
        | Stelem_i4 -> failwith "TODO: Stelem_i4 unimplemented"
        | Stelem_u4 -> failwith "TODO: Stelem_u4 unimplemented"
        | Stelem_i8 -> failwith "TODO: Stelem_i8 unimplemented"
        | Stelem_u8 -> failwith "TODO: Stelem_u8 unimplemented"
        | Stelem_r4 -> failwith "TODO: Stelem_r4 unimplemented"
        | Stelem_r8 -> failwith "TODO: Stelem_r8 unimplemented"
        | Stelem_ref -> failwith "TODO: Stelem_ref unimplemented"
        | Cpblk -> failwith "TODO: Cpblk unimplemented"
        | Initblk -> failwith "TODO: Initblk unimplemented"
        | Conv_ovf_u1 -> failwith "TODO: Conv_ovf_u1 unimplemented"
        | Conv_ovf_u2 -> failwith "TODO: Conv_ovf_u2 unimplemented"
        | Conv_ovf_u4 -> failwith "TODO: Conv_ovf_u4 unimplemented"
        | Conv_ovf_u8 -> failwith "TODO: Conv_ovf_u8 unimplemented"
        | Conv_ovf_i1 -> failwith "TODO: Conv_ovf_i1 unimplemented"
        | Conv_ovf_i2 -> failwith "TODO: Conv_ovf_i2 unimplemented"
        | Conv_ovf_i4 -> failwith "TODO: Conv_ovf_i4 unimplemented"
        | Conv_ovf_i8 -> failwith "TODO: Conv_ovf_i8 unimplemented"
        | Break -> failwith "TODO: Break unimplemented"
        | Conv_r_un -> failwith "TODO: Conv_r_un unimplemented"
        | Arglist -> failwith "TODO: Arglist unimplemented"
        | Ckfinite -> failwith "TODO: Ckfinite unimplemented"
        | Readonly -> failwith "TODO: Readonly unimplemented"
        | Refanytype -> failwith "TODO: Refanytype unimplemented"

    let private resolveMember
        (loggerFactory : ILoggerFactory)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * AssemblyName * Choice<WoofWare.PawPrint.MethodInfo, WoofWare.PawPrint.FieldInfo>
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]

        let memberName : string = assy.Strings mem.Name

        let state, assy, targetType =
            match mem.Parent with
            | MetadataToken.TypeReference parent -> IlMachineState.resolveType loggerFactory parent assy state
            | MetadataToken.TypeSpecification parent ->
                IlMachineState.resolveTypeFromSpec loggerFactory parent assy state
            | parent -> failwith $"Unexpected: {parent}"

        match mem.Signature with
        | MemberSignature.Field fieldSig ->
            let availableFields =
                targetType.Fields
                |> List.filter (fun fi -> fi.Name = memberName)
                |> List.filter (fun fi -> fi.Signature = fieldSig)

            let field =
                match availableFields with
                | [] ->
                    failwith
                        $"Could not find field member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] -> x
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for {targetType.Namespace}.{targetType.Name}'s field {memberName}!"

            state, assy.Name, Choice2Of2 field

        | MemberSignature.Method memberSig ->
            let availableMethods =
                targetType.Methods
                |> List.filter (fun mi -> mi.Name = memberName)
                |> List.filter (fun mi -> mi.Signature = memberSig)

            let method =
                match availableMethods with
                | [] ->
                    failwith
                        $"Could not find member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] -> x
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

            state, assy.Name, Choice1Of2 method

    let private executeUnaryMetadata
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryMetadataTokenIlOp)
        (metadataToken : MetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | Call ->
            // TODO: make an abstraction for "call this method" that wraps up all the `loadClass` stuff too
            let state, methodToCall =
                match metadataToken with
                | MetadataToken.MethodSpecification h ->
                    // TODO: do we need to initialise the parent class here?
                    let spec = (state.ActiveAssembly thread).MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token -> state, (state.ActiveAssembly thread).Methods.[token]
                    | k -> failwith $"Unrecognised kind: %O{k}"
                | MetadataToken.MemberReference h ->
                    let state, _, method =
                        resolveMember loggerFactory (state.ActiveAssembly thread) h state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Call a field"
                    | Choice1Of2 method -> state, method

                | MetadataToken.MethodDef defn -> state, (state.ActiveAssembly thread).Methods.[defn]
                | k -> failwith $"Unrecognised kind: %O{k}"

            state.WithThreadSwitchedToAssembly (snd methodToCall.DeclaringType) thread
            |> fst
            |> IlMachineState.callMethodInActiveAssembly loggerFactory thread methodToCall None
        // TODO: push the instance pointer if necessary
        // TODO: push args?

        | Callvirt -> failwith "TODO: Callvirt unimplemented"
        | Castclass -> failwith "TODO: Castclass unimplemented"
        | Newobj ->
            let state, assy, ctor =
                match metadataToken with
                | MethodDef md ->
                    let activeAssy = state.ActiveAssembly thread
                    let method = activeAssy.Methods.[md]
                    state, activeAssy.Name, method
                | MemberReference mr ->
                    let state, name, method =
                        resolveMember loggerFactory (state.ActiveAssembly thread) mr state

                    match method with
                    | Choice1Of2 mr -> state, name, mr
                    | Choice2Of2 _field -> failwith "unexpectedly NewObj found a constructor which is a field"
                | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

            let ctorType, ctorAssembly = ctor.DeclaringType
            let ctorAssembly = state.LoadedAssembly ctorAssembly |> Option.get
            let ctorType = ctorAssembly.TypeDefs.[ctorType]

            let fields =
                ctorType.Fields
                |> List.map (fun field ->
                    let zeroedAllocation = CliType.zeroOf field.Signature
                    field.Name, zeroedAllocation
                )

            let allocatedAddr, state =
                IlMachineState.allocateManagedObject ctorType fields state

            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer (ManagedPointerSource.Heap allocatedAddr))
                    thread

            let state, whatWeDid =
                state.WithThreadSwitchedToAssembly assy thread
                |> fst
                |> IlMachineState.callMethodInActiveAssembly loggerFactory thread ctor (Some allocatedAddr)

            match whatWeDid with
            | SuspendedForClassInit -> failwith "unexpectedly suspended while initialising constructor"
            | BlockedOnClassInit threadBlockingUs ->
                failwith "TODO: Newobj blocked on class init synchronization unimplemented"
            | Executed -> ()

            state, WhatWeDid.Executed
        | Newarr ->
            let currentState = state.ThreadState.[thread]
            let popped, newMethodState = MethodState.popFromStack currentState.MethodState

            let currentState =
                { currentState with
                    MethodStates = currentState.MethodStates.SetItem (currentState.ActiveMethodState, newMethodState)
                }

            let len =
                match popped with
                | EvalStackValue.Int32 v -> v
                | popped -> failwith $"unexpectedly popped value %O{popped} to serve as array len"

            let elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state.LoadedAssembly currentState.ActiveAssembly
                    |> Option.get
                    |> fun assy -> assy.TypeDefs.[defn]
                | x -> failwith $"TODO: Newarr element type resolution unimplemented for {x}"

            let baseType =
                elementType.BaseType
                |> TypeInfo.resolveBaseType
                    (fun (x : DumpedAssembly) -> x.Name)
                    (fun x y -> x.TypeDefs.[y])
                    baseClassTypes
                    elementType.Assembly

            let zeroOfType =
                match baseType with
                | ResolvedBaseType.Object ->
                    // initialise with null references
                    fun () -> CliType.ObjectRef None
                | ResolvedBaseType.Enum -> failwith "TODO: Newarr Enum array initialization unimplemented"
                | ResolvedBaseType.ValueType -> failwith "TODO: Newarr ValueType array initialization unimplemented"
                | ResolvedBaseType.Delegate -> failwith "TODO: Newarr Delegate array initialization unimplemented"

            let alloc, state = IlMachineState.allocateArray zeroOfType len state

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add thread currentState
                }
                |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Box -> failwith "TODO: Box unimplemented"
        | Ldelema -> failwith "TODO: Ldelema unimplemented"
        | Isinst -> failwith "TODO: Isinst unimplemented"
        | Stfld -> failwith "TODO: Stfld unimplemented"
        | Stsfld ->
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: Stsfld - throw MissingFieldException"
            | true, field ->

            match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            let popped, state = IlMachineState.popEvalStack thread state

            let toStore =
                match popped with
                | EvalStackValue.ManagedPointer source ->
                    match source with
                    | ManagedPointerSource.LocalVariable _ ->
                        failwith "TODO: Stsfld LocalVariable storage unimplemented"
                    | ManagedPointerSource.Heap addr -> CliType.ObjectRef (Some addr)
                    | ManagedPointerSource.Null -> CliType.ObjectRef None
                | _ -> failwith "TODO: Stsfld non-managed pointer storage unimplemented"

            let state =
                { state with
                    Statics = state.Statics.SetItem ((field.DeclaringType, activeAssy.Name), toStore)
                }
            // TODO: do we need to advance the program counter here?

            state, WhatWeDid.Executed

        | Ldfld ->
            let state, assyName, field =
                match metadataToken with
                | MetadataToken.FieldDefinition f ->
                    state, (state.ActiveAssembly thread).Name, state.ActiveAssembly(thread).Fields.[f]
                | MetadataToken.MemberReference mr ->
                    let state, assyName, field =
                        resolveMember loggerFactory (state.ActiveAssembly thread) mr state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, assyName, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            let currentObj, state = IlMachineState.popEvalStack thread state

            if field.Attributes.HasFlag FieldAttributes.Static then
                let staticField = state.Statics.[field.DeclaringType, assyName]
                let state = state |> IlMachineState.pushToEvalStack staticField thread
                state, WhatWeDid.Executed
            else

            let currentObj : unit =
                match currentObj with
                | EvalStackValue.Int32 i -> failwith "todo: int32"
                | EvalStackValue.Int64 int64 -> failwith "todo: int64"
                | EvalStackValue.NativeInt nativeIntSource -> failwith $"todo: nativeint {nativeIntSource}"
                | EvalStackValue.Float f -> failwith "todo: float"
                | EvalStackValue.ManagedPointer managedPointerSource ->
                    match managedPointerSource with
                    | ManagedPointerSource.LocalVariable (source, whichVar) ->
                        failwith $"todo: local variable {whichVar}"
                    | ManagedPointerSource.Heap managedHeapAddress -> failwith $"todo: heap addr {managedHeapAddress}"
                    | ManagedPointerSource.Null -> failwith "TODO: raise NullReferenceException"
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith $"todo: {managedHeapAddress}"
                | EvalStackValue.UserDefinedValueType -> failwith "todo"

            failwith "TODO: Ldfld unimplemented"
        | Ldflda -> failwith "TODO: Ldflda unimplemented"
        | Ldsfld -> failwith "TODO: Ldsfld unimplemented"
        | Unbox_Any -> failwith "TODO: Unbox_Any unimplemented"
        | Stelem -> failwith "TODO: Stelem unimplemented"
        | Ldelem -> failwith "TODO: Ldelem unimplemented"
        | Initobj -> failwith "TODO: Initobj unimplemented"
        | Ldsflda ->
            // TODO: check whether we should throw FieldAccessException
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to load a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: Ldsflda - throw MissingFieldException"
            | true, field ->
                match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
                | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
                | NothingToDo state ->

                if TypeDefn.isManaged field.Signature then
                    match state.Statics.TryGetValue ((field.DeclaringType, activeAssy.Name)) with
                    | true, v ->
                        IlMachineState.pushToEvalStack v thread state
                        |> IlMachineState.advanceProgramCounter thread
                        |> Tuple.withRight WhatWeDid.Executed
                    | false, _ ->
                        let allocation, state =
                            state |> (failwith "TODO: Ldsflda static field allocation unimplemented")

                        state
                        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocation)) thread
                        |> Tuple.withRight WhatWeDid.Executed
                else
                    failwith "TODO: Ldsflda - push unmanaged pointer"
        | Ldftn -> failwith "TODO: Ldftn unimplemented"
        | Stobj -> failwith "TODO: Stobj unimplemented"
        | Constrained -> failwith "TODO: Constrained unimplemented"
        | Ldtoken -> failwith "TODO: Ldtoken unimplemented"
        | Cpobj -> failwith "TODO: Cpobj unimplemented"
        | Ldobj -> failwith "TODO: Ldobj unimplemented"
        | Sizeof -> failwith "TODO: Sizeof unimplemented"
        | Calli -> failwith "TODO: Calli unimplemented"
        | Unbox -> failwith "TODO: Unbox unimplemented"
        | Ldvirtftn -> failwith "TODO: Ldvirtftn unimplemented"
        | Mkrefany -> failwith "TODO: Mkrefany unimplemented"
        | Refanyval -> failwith "TODO: Refanyval unimplemented"
        | Jmp -> failwith "TODO: Jmp unimplemented"

    let private executeUnaryStringToken
        (baseClassTypes : BaseClassTypes<'a>)
        (op : UnaryStringTokenIlOp)
        (sh : StringToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            let addressToLoad, state =
                match state.InternedStrings.TryGetValue sh with
                | false, _ ->
                    let stringToAllocate = state.ActiveAssembly(thread).Strings sh

                    let dataAddr, state =
                        IlMachineState.allocateStringData stringToAllocate.Length state

                    let state = state |> IlMachineState.setStringData dataAddr stringToAllocate

                    let stringInstanceFields =
                        baseClassTypes.String.Fields
                        |> List.choose (fun field ->
                            if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                                Some (field.Name, field.Signature)
                            else
                                None
                        )
                        |> List.sortBy fst
                    // Assert that the string type has the fields we expect
                    if
                        stringInstanceFields
                        <> [
                            ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                            ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
                        ]
                    then
                        failwith
                            $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

                    let fields =
                        [
                            "_firstChar", CliType.OfChar state.ManagedHeap.StringArrayData.[dataAddr]
                            "_stringLength", CliType.Numeric (CliNumericType.Int32 stringToAllocate.Length)
                        ]

                    let addr, state =
                        IlMachineState.allocateManagedObject
                            (baseClassTypes.String
                             |> TypeInfo.mapGeneric (fun _ -> failwith<unit> "string is not generic"))
                            fields
                            state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (sh, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addressToLoad)) thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

    let private executeUnaryConst
        (state : IlMachineState)
        (currentThread : ThreadId)
        (op : UnaryConstIlOp)
        : IlMachineState * WhatWeDid
        =
        match op with
        | Stloc s ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread (int s)
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Stloc_s b ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread (int b)
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I8 i ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int64 i)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I4 i ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 i)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_R4 f ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float32 f)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_R8 f ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 f)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I4_s b ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int8 b)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Br i -> failwith "TODO: Br unimplemented"
        | Br_s b ->
            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IlMachineState.jumpProgramCounter currentThread (int b)
            |> Tuple.withRight WhatWeDid.Executed
        | Brfalse_s b -> failwith "TODO: Brfalse_s unimplemented"
        | Brtrue_s b ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isTrue =
                match popped with
                | EvalStackValue.Int32 i -> i <> 0
                | EvalStackValue.Int64 i -> i <> 0L
                | EvalStackValue.NativeInt i -> not (NativeIntSource.isZero i)
                | EvalStackValue.Float f -> failwith "TODO: Brtrue_s float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> false
                | EvalStackValue.ManagedPointer _ -> true
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brtrue_s ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType ->
                    failwith "TODO: Brtrue_s UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isTrue then
                   IlMachineState.jumpProgramCounter currentThread (int b)
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Brfalse i ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isFalse =
                match popped with
                | EvalStackValue.Int32 i -> i = 0
                | EvalStackValue.Int64 i -> i = 0L
                | EvalStackValue.NativeInt i -> NativeIntSource.isZero i
                | EvalStackValue.Float f -> failwith "TODO: Brfalse float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> true
                | EvalStackValue.ManagedPointer _ -> false
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brfalse ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType ->
                    failwith "TODO: Brfalse UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isFalse then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Brtrue i ->
            let popped, state = IlMachineState.popEvalStack currentThread state

            let isTrue =
                match popped with
                | EvalStackValue.Int32 i -> i <> 0
                | EvalStackValue.Int64 i -> i <> 0L
                | EvalStackValue.NativeInt i -> not (NativeIntSource.isZero i)
                | EvalStackValue.Float f -> failwith "TODO: Brtrue float semantics undocumented"
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> false
                | EvalStackValue.ManagedPointer _ -> true
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Brtrue ObjectRef comparison unimplemented"
                | EvalStackValue.UserDefinedValueType ->
                    failwith "TODO: Brtrue UserDefinedValueType comparison unimplemented"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> if isTrue then
                   IlMachineState.jumpProgramCounter currentThread i
               else
                   id
            |> Tuple.withRight WhatWeDid.Executed
        | Beq_s b -> failwith "TODO: Beq_s unimplemented"
        | Blt_s b -> failwith "TODO: Blt_s unimplemented"
        | Ble_s b -> failwith "TODO: Ble_s unimplemented"
        | Bgt_s b -> failwith "TODO: Bgt_s unimplemented"
        | Bge_s b -> failwith "TODO: Bge_s unimplemented"
        | Beq i -> failwith "TODO: Beq unimplemented"
        | Blt i -> failwith "TODO: Blt unimplemented"
        | Ble i -> failwith "TODO: Ble unimplemented"
        | Bgt i -> failwith "TODO: Bgt unimplemented"
        | Bge i -> failwith "TODO: Bge unimplemented"
        | Bne_un_s b -> failwith "TODO: Bne_un_s unimplemented"
        | Bge_un_s b -> failwith "TODO: Bge_un_s unimplemented"
        | Bgt_un_s b -> failwith "TODO: Bgt_un_s unimplemented"
        | Ble_un_s b -> failwith "TODO: Ble_un_s unimplemented"
        | Blt_un_s b -> failwith "TODO: Blt_un_s unimplemented"
        | Bne_un i -> failwith "TODO: Bne_un unimplemented"
        | Bge_un i -> failwith "TODO: Bge_un unimplemented"
        | Bgt_un i -> failwith "TODO: Bgt_un unimplemented"
        | Ble_un i -> failwith "TODO: Ble_un unimplemented"
        | Blt_un i -> failwith "TODO: Blt_un unimplemented"
        | Ldloc_s b -> failwith "TODO: Ldloc_s unimplemented"
        | Ldloca_s b ->
            let threadState = state.ThreadState.[currentThread]
            let methodState = threadState.MethodStates.[threadState.ActiveMethodState]

            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer (ManagedPointerSource.LocalVariable ((), uint16<uint8> b)))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            state, WhatWeDid.Executed
        | Ldarga s -> failwith "TODO: Ldarga unimplemented"
        | Ldarg_s b -> failwith "TODO: Ldarg_s unimplemented"
        | Ldarga_s b -> failwith "TODO: Ldarga_s unimplemented"
        | Leave i -> failwith "TODO: Leave unimplemented"
        | Leave_s b -> failwith "TODO: Leave_s unimplemented"
        | Starg_s b -> failwith "TODO: Starg_s unimplemented"
        | Starg s -> failwith "TODO: Starg unimplemented"
        | Unaligned b -> failwith "TODO: Unaligned unimplemented"
        | Ldloc s -> failwith "TODO: Ldloc unimplemented"
        | Ldloca s -> failwith "TODO: Ldloca unimplemented"
        | Ldarg s -> failwith "TODO: Ldarg unimplemented"

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ -> failwith "Wanted to execute a nonexistent instruction"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly (snd instruction.ExecutingMethod.DeclaringType) with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue (fst instruction.ExecutingMethod.DeclaringType) with
                | true, v -> v.Name
                | false, _ -> "<unrecognised type>"

        logger.LogInformation (
            "Executing one step (index {ExecutingIlOpIndex} in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            executingInType,
            instruction.ExecutingMethod.Name,
            executingInstruction
        )

        match instruction.ExecutingMethod.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> executeNullary state thread op
        | IlOp.UnaryConst unaryConstIlOp -> executeUnaryConst state thread unaryConstIlOp |> ExecutionResult.Stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            executeUnaryMetadata loggerFactory baseClassTypes unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.Stepped
        | IlOp.Switch immutableArray -> failwith "TODO: Switch unimplemented"
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            executeUnaryStringToken baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
