namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type IlMachineState =
    {
        Logger : ILogger
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

    member this.WithTypeBeginInit (thread : ThreadId) (handle : TypeDefinitionHandle, assy : AssemblyName) =
        this.Logger.LogDebug (
            "Beginning initialisation of type {TypeName}, handle {TypeDefinitionHandle} from assy {AssemblyHash}",
            this.LoadedAssembly(assy).Value.TypeDefs.[handle].Name,
            handle.GetHashCode (),
            assy.GetHashCode ()
        )

        let typeInitTable =
            this.TypeInitTable |> TypeInitTable.beginInitialising thread (handle, assy)

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithTypeEndInit (thread : ThreadId) (handle : TypeDefinitionHandle, assy : AssemblyName) =
        this.Logger.LogDebug (
            "Marking complete initialisation of type {TypeName}, handle {TypeDefinitionHandle} from assy {AssemblyHash}",
            this.LoadedAssembly(assy).Value.TypeDefs.[handle].Name,
            handle.GetHashCode (),
            assy.GetHashCode ()
        )

        let typeInitTable =
            this.TypeInitTable |> TypeInitTable.markInitialised thread (handle, assy)

        { this with
            TypeInitTable = typeInitTable
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

(*
Type load algorithm, from II.10.5.3.3
1. At class load-time (hence prior to initialization time) store zero or null into all static fields of the
type.
2. If the type is initialized, you are done.
2.1. If the type is not yet initialized, try to take an initialization lock.
2.2. If successful, record this thread as responsible for initializing the type and proceed to step 2.3.
2.2.1. If not successful, see whether this thread or any thread waiting for this thread to complete already
holds the lock.
2.2.2. If so, return since blocking would create a deadlock. This thread will now see an incompletely
initialized state for the type, but no deadlock will arise.
2.2.3 If not, block until the type is initialized then return.
2.3 Initialize the base class type and then all interfaces implemented by this type.
2.4 Execute the type initialization code for this type.
2.5 Mark the type as initialized, release the initialization lock, awaken any threads waiting for this type
to be initialized, and return.
*)
type WhatWeDid =
    | Executed
    /// We didn't run what you wanted, because we have to do class initialisation first.
    | SuspendedForClassInit
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId

type ExecutionResult =
    | Terminated of IlMachineState * terminatingThread : ThreadId
    | Stepped of IlMachineState * WhatWeDid

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
                        Assembly.read loggerFactory (Some file) f |> Some
                    with :? FileNotFoundException ->
                        None
                )
                |> Seq.toList

            match assy |> List.tryHead with
            | None -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | Some assy ->

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
        (wasClassConstructor : bool)
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
                    // TODO: generics
                    let zeroArg =
                        CliType.zeroOf ImmutableArray.Empty methodToCall.Signature.ParameterTypes.[i]

                    let poppedArg = EvalStackValue.toCliTypeCoerced zeroArg poppedArg
                    afterPop <- afterPop'
                    args.Add poppedArg

                args.Reverse ()

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

                let oldFrame =
                    if wasClassConstructor then
                        afterPop
                    else
                        afterPop |> MethodState.advanceProgramCounter

                newFrame, oldFrame
            else
                let args = ImmutableArray.CreateBuilder (methodToCall.Parameters.Length + 1)
                let poppedArg, afterPop = activeMethodState |> MethodState.popFromStack
                let mutable afterPop = afterPop

                for i = 1 to methodToCall.Parameters.Length do
                    let poppedArg, afterPop' = afterPop |> MethodState.popFromStack
                    // TODO: generics
                    let zeroArg =
                        CliType.zeroOf ImmutableArray.Empty methodToCall.Signature.ParameterTypes.[i - 1]

                    let poppedArg = EvalStackValue.toCliTypeCoerced zeroArg poppedArg
                    afterPop <- afterPop'
                    args.Add poppedArg

                // it only matters that the RuntimePointer is a RuntimePointer, so that the coercion has a target of the
                // right shape
                args.Add (
                    EvalStackValue.toCliTypeCoerced (CliType.RuntimePointer (CliRuntimePointer.Unmanaged ())) poppedArg
                )

                args.Reverse ()

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

        let logger = loggerFactory.CreateLogger "LoadClass"

        match TypeInitTable.tryGet (typeDefHandle, assemblyName) state.TypeInitTable with
        | Some TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress tid) when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress _) ->
            // This is usually signalled by WhatWeDid.Blocked
            failwith
                "TODO: cross-thread class init synchronization unimplemented - this thread has to wait for the other thread to finish initialisation"
        | None ->
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
            let state = state.WithTypeBeginInit currentThread (typeDefHandle, assemblyName)

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

            // TODO: also need to initialise all interfaces implemented by the type

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

                callMethod
                    (Some (typeDefHandle, assemblyName))
                    None
                    true
                    ctorMethod
                    currentThread
                    currentThreadState
                    state
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state = state.WithTypeEndInit currentThread (typeDefHandle, assemblyName)

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

        match TypeInitTable.tryGet methodToCall.DeclaringType state.TypeInitTable with
        | None ->
            match
                loadClass loggerFactory (fst methodToCall.DeclaringType) (snd methodToCall.DeclaringType) thread state
            with
            | NothingToDo state ->
                callMethod None weAreConstructingObj false methodToCall thread threadState state, WhatWeDid.Executed
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | Some TypeInitState.Initialized ->
            callMethod None weAreConstructingObj false methodToCall thread threadState state, WhatWeDid.Executed
        | Some (InProgress threadId) ->
            if threadId = thread then
                // II.10.5.3.2: avoid the deadlock by simply proceeding.
                callMethod None weAreConstructingObj false methodToCall thread threadState state, WhatWeDid.Executed
            else
                state, WhatWeDid.BlockedOnClassInit threadId

    let initial
        (lf : ILoggerFactory)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (entryAssembly : DumpedAssembly)
        : IlMachineState
        =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name
        let logger = lf.CreateLogger "IlMachineState"

        let state =
            {
                Logger = logger
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

    let resolveMember
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
            | MetadataToken.TypeReference parent -> resolveType loggerFactory parent assy state
            | MetadataToken.TypeSpecification parent -> resolveTypeFromSpec loggerFactory parent assy state
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

    /// There might be no stack frame to return to, so you might get None.
    let returnStackFrame (currentThread : ThreadId) (state : IlMachineState) : IlMachineState option =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]

        match threadStateAtEndOfMethod.MethodState.ReturnState with
        | None -> None
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add
                        currentThread
                        { threadStateAtEndOfMethod with
                            ActiveMethodState = returnState.JumpTo
                            ActiveAssembly =
                                snd
                                    threadStateAtEndOfMethod.MethodStates.[returnState.JumpTo].ExecutingMethod
                                        .DeclaringType
                        }
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            // Assumption: a constructor can't also return a value.
            state |> pushToEvalStack (CliType.OfManagedObject constructing) currentThread
        | None ->
            match threadStateAtEndOfMethod.MethodState.EvaluationStack.Values with
            | [] ->
                // no return value
                state
            | [ retVal ] ->
                let retType =
                    threadStateAtEndOfMethod.MethodState.ExecutingMethod.Signature.ReturnType

                match retType with
                | TypeDefn.Void -> state
                | retType ->
                    // TODO: generics
                    let toPush =
                        EvalStackValue.toCliTypeCoerced (CliType.zeroOf ImmutableArray.Empty retType) retVal

                    state |> pushToEvalStack toPush currentThread
            | _ ->
                failwith
                    "Unexpected interpretation result has a local evaluation stack with more than one element on RET"

        |> Some
