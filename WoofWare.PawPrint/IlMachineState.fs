namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type IlMachineState =
    {
        ConcreteTypes : AllConcreteTypes
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
        /// For each concrete type, a map of field definition handle to static value.
        /// The FieldDefinitionHandle is scoped to the assembly that defines the outer ConcreteTypeHandle's type;
        /// do not mix handles from different assemblies under the same key.
        _Statics : ImmutableDictionary<ConcreteTypeHandle, Map<ComparableFieldDefinitionHandle, CliType>>
        DotnetRuntimeDirs : string ImmutableArray
        TypeHandles : TypeHandleRegistry
        FieldHandles : FieldHandleRegistry
        /// Cache of RuntimeAssembly heap objects keyed by assembly full name, so that
        /// two types from the same assembly return the same Assembly object (reference identity).
        RuntimeAssemblyObjects : ImmutableDictionary<string, ManagedHeapAddress>
        /// Cache of managed `System.Threading.Thread` heap objects, one per ThreadId, so that
        /// `Thread.CurrentThread` returns a reference-identical object on repeated access from
        /// the same guest thread.
        ManagedThreadObjects : Map<ThreadId, ManagedHeapAddress>
        /// Next managed thread ID to assign. Consumed by `Thread.Initialize()` (user-created
        /// threads) and by `getOrAllocateManagedThreadObject` for non-main scheduler-created
        /// threads.  Starts at 2 because ID 0 is the CLR's "no managed thread" sentinel and
        /// ID 1 is reserved for the main thread (ThreadId 0).
        NextManagedThreadId : int
    }

    member this.WithTypeBeginInit (thread : ThreadId) (ty : ConcreteTypeHandle) =
        let concreteType = AllConcreteTypes.lookup ty this.ConcreteTypes |> Option.get

        this.Logger.LogDebug (
            "Beginning initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            concreteType.Assembly.FullName,
            this.LoadedAssembly(concreteType.Assembly).Value.TypeDefs.[concreteType.Definition.Get].Name,
            concreteType.Definition.Get.GetHashCode ()
        )

        let typeInitTable = this.TypeInitTable |> TypeInitTable.beginInitialising thread ty

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithTypeEndInit (thread : ThreadId) (ty : ConcreteTypeHandle) =
        let concreteType = AllConcreteTypes.lookup ty this.ConcreteTypes |> Option.get

        this.Logger.LogDebug (
            "Marking complete initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            concreteType.Assembly.FullName,
            this.LoadedAssembly(concreteType.Assembly).Value.TypeDefs.[concreteType.Definition.Get].Name,
            concreteType.Definition.Get.GetHashCode ()
        )

        let typeInitTable = this.TypeInitTable |> TypeInitTable.markInitialised thread ty

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithTypeFailedInit
        (thread : ThreadId)
        (ty : ConcreteTypeHandle)
        (tieAddress : ManagedHeapAddress)
        (tieType : ConcreteTypeHandle)
        =
        let concreteType = AllConcreteTypes.lookup ty this.ConcreteTypes |> Option.get

        this.Logger.LogDebug (
            "Marking failed initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            concreteType.Assembly.FullName,
            this.LoadedAssembly(concreteType.Assembly).Value.TypeDefs.[concreteType.Definition.Get].Name,
            concreteType.Definition.Get.GetHashCode ()
        )

        let typeInitTable =
            this.TypeInitTable |> TypeInitTable.markFailed thread ty tieAddress tieType

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithLoadedAssembly (name : AssemblyName) (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.Add (name.FullName, value)
        }

    member this.LoadedAssembly' (fullName : string) : DumpedAssembly option =
        match this._LoadedAssemblies.TryGetValue fullName with
        | false, _ -> None
        | true, v -> Some v

    member this.LoadedAssembly (name : AssemblyName) : DumpedAssembly option = this.LoadedAssembly' name.FullName

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
    NOTE: The real CLR does not eagerly run base type initializers here. Base types get
    initialized lazily when their own constructors or static members are touched. We follow
    the CLR's actual behaviour, not the spec text.
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
    /// A TypeInitializationException was thrown into the guest because a .cctor previously failed.
    /// The state has already been updated with exception dispatch (handler search and frame unwinding).
    | ThrowingTypeInitializationException

type ExecutionResult =
    /// A single thread finished (its top frame hit `ret`). For the entry thread this means
    /// the whole program exits; for a worker it just means that thread is done.
    | Terminated of IlMachineState * terminatingThread : ThreadId
    /// Environment.Exit was called on `exitingThread`. The process terminates immediately
    /// regardless of which thread made the call, carrying whatever state / eval-stack the
    /// caller had at the moment of exit.
    ///
    /// Note: the exiting thread's frame stack is not cleaned up — the Environment.Exit
    /// native frame is still on top, and its Status is still Runnable. That's fine because
    /// the process is being torn down, but if anyone ever makes ProcessExit allow further
    /// guest execution (e.g. finalizers, AppDomain-unload hooks), this constructor will
    /// need to return the thread to a consistent state first.
    | ProcessExit of IlMachineState * exitingThread : ThreadId
    | Stepped of IlMachineState * WhatWeDid
    | UnhandledException of
        IlMachineState *
        terminatingThread : ThreadId *
        CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

/// Result of returning from a method frame via `Ret`.
type ReturnFrameResult =
    /// No caller frame to return to (entry-point method hit Ret).
    | NoFrameToReturn
    /// Normal return; state is positioned at the caller frame.
    | NormalReturn of IlMachineState
    /// The ctor that just returned was constructing a runtime-synthesised exception.
    /// The caller should dispatch this object as a managed exception instead of pushing it
    /// onto the eval stack.  Before dispatching, the caller MUST call
    /// ExceptionDispatching.overwriteHResultPostCtor to apply the CLR's post-ctor
    /// SetHResult(GetHR()) step.
    | DispatchException of IlMachineState * exceptionAddr : ManagedHeapAddress * exceptionType : ConcreteTypeHandle

/// Result of a complete program run (the pump loop having finished).
type RunOutcome =
    /// Every thread ran to `ret`. `terminatingThread` is the entry thread, whose
    /// eval stack carries the exit code.
    | NormalExit of IlMachineState * terminatingThread : ThreadId
    /// A thread called `Environment.Exit`. The process tore itself down regardless
    /// of other threads still running; `exitingThread`'s eval stack carries the exit
    /// code. Distinct from `NormalExit` so the pre-main cctor pump can bail rather
    /// than silently continuing into `Main` after the guest already asked to die.
    | ProcessExit of IlMachineState * exitingThread : ThreadId
    | GuestUnhandledException of
        IlMachineState *
        terminatingThread : ThreadId *
        CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

type StateLoadResult =
    /// The type is loaded; you can proceed.
    | NothingToDo of IlMachineState
    /// We didn't manage to load the requested type, because that type itself requires first loading something.
    /// The state we give you is ready to load that something.
    | FirstLoadThis of IlMachineState
    /// The type's .cctor previously failed. A TypeInitializationException has been dispatched into the guest.
    | ThrowingTypeInitializationException of IlMachineState

[<RequireQualifiedAccess>]
module IlMachineState =
    /// <summary>
    /// Create a new IlMachineState which has loaded the given assembly.
    /// This involves reading assemblies from the disk and doing a complete parse of them, so it might be quite slow!
    ///
    /// This function doesn't do anything if the referenced assembly has already been loaded.
    /// </summary>
    /// <param name="loggerFactory">LoggerFactory into which to emit logs.</param>
    /// <param name="referencedInAssembly">The assembly which contains an AssemblyReference which causes us to want to load a new assembly.</param>
    /// <param name="r">The AssemblyReferenceHandle pointing at an assembly we want to load. *Important*: this is an AssemblyReferenceHandle from <c>referencedInAssembly</c>; in general, AssemblyReferenceHandles are only well-defined if you know what assembly they were defined in.</param>
    /// <param name="state">The immutable state to augment with the new assembly.</param>
    let loadAssembly
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * AssemblyName
        =
        let assemblies, dumped, assyName =
            TypeResolution.loadAssembly
                loggerFactory
                state.DotnetRuntimeDirs
                referencedInAssembly
                r
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        dumped,
        assyName

    let internal loader (loggerFactory : ILoggerFactory) (state : IlMachineState) : IAssemblyLoad =
        { new IAssemblyLoad with
            member _.LoadAssembly loaded assyName ref =
                let assemblies, targetAssy, _name =
                    TypeResolution.loadAssembly
                        loggerFactory
                        state.DotnetRuntimeDirs
                        loaded.[assyName.FullName]
                        ref
                        loaded

                assemblies, targetAssy
        }

    let concretizeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (declaringAssembly : AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (ty : TypeDefn)
        : IlMachineState * ConcreteTypeHandle
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let handle, ctx =
            TypeConcretization.concretizeType
                ctx
                (loader loggerFactory state)
                declaringAssembly
                typeGenerics
                methodGenerics
                ty

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, handle

    let internal resolveTopLevelTypeFromName
        (loggerFactory : ILoggerFactory)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTopLevelTypeFromName
                loggerFactory
                state.DotnetRuntimeDirs
                ns
                name
                genericArgs
                assy
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromExport
                loggerFactory
                state.DotnetRuntimeDirs
                fromAssembly
                ty
                genericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromRef
                loggerFactory
                state.DotnetRuntimeDirs
                referencedInAssembly
                target
                typeGenericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveType
        (loggerFactory : ILoggerFactory)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveType loggerFactory state.DotnetRuntimeDirs ty genericArgs assy state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromDefn
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                ty
                typeGenericArgs
                methodGenericArgs
                assy
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : TypeDefn ImmutableArray)
        (methodGenericArgs : TypeDefn ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromSpec
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                ty
                assy
                typeGenericArgs
                methodGenericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    /// Resolve a TypeSpecification using concrete type handles from execution context
    let resolveTypeFromSpecConcrete
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : ConcreteTypeHandle ImmutableArray)
        (methodGenericArgs : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let sign = assy.TypeSpecs.[ty].Signature

        // Convert ConcreteTypeHandle to TypeDefn
        let typeGenericArgsAsDefn =
            typeGenericArgs
            |> ImmutableArray.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )

        let methodGenericArgsAsDefn =
            methodGenericArgs
            |> ImmutableArray.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )

        resolveTypeFromDefn loggerFactory baseClassTypes sign typeGenericArgsAsDefn methodGenericArgsAsDefn assy state

    /// Resolve a TypeDefinition using concrete type handles from execution context
    let resolveTypeFromDefnConcrete
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefinitionHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : ConcreteTypeHandle ImmutableArray)
        (methodGenericArgs : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let typeDef = assy.TypeDefs.[ty]

        // Convert ConcreteTypeHandle to TypeDefn for the generics
        let typeGenericArgsAsDefn =
            typeGenericArgs
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let methodGenericArgsAsDefn =
            methodGenericArgs
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        // Map the type definition's generics using the provided type generic arguments
        let resolvedTypeDef =
            typeDef
            |> TypeInfo.mapGeneric (fun (param, _) ->
                if param.SequenceNumber < typeGenericArgsAsDefn.Length then
                    typeGenericArgsAsDefn.[param.SequenceNumber]
                else
                    failwithf "Generic type parameter %d out of range" param.SequenceNumber
            )

        state, assy, resolvedTypeDef

    /// Get zero value for a type that's already been concretized
    let cliTypeZeroOfHandle
        (state : IlMachineState)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : CliType * IlMachineState
        =
        let zero, updatedConcreteTypes =
            CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes handle

        let newState =
            { state with
                ConcreteTypes = updatedConcreteTypes
            }

        zero, newState

    /// Concretize a ConcreteType<TypeDefn> to get a ConcreteTypeHandle for static field access
    let concretizeFieldDeclaringType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringType : ConcreteType<TypeDefn>)
        (state : IlMachineState)
        : ConcreteTypeHandle * IlMachineState
        =
        // Create a concretization context from the current state
        let ctx : TypeConcretization.ConcretizationContext<_> =
            {
                ConcreteTypes = state.ConcreteTypes
                LoadedAssemblies = state._LoadedAssemblies
                BaseTypes = baseClassTypes
            }

        // Concretize each generic argument first
        let mutable currentCtx = ctx
        let genericHandles = ImmutableArray.CreateBuilder declaringType.Generics.Length

        for genericArg in declaringType.Generics do
            let handle, newCtx =
                TypeConcretization.concretizeType
                    currentCtx
                    (loader loggerFactory state)
                    declaringType.Assembly
                    ImmutableArray.Empty // No type generics in this context
                    ImmutableArray.Empty // No method generics in this context
                    genericArg

            currentCtx <- newCtx
            genericHandles.Add handle

        // Now we need to concretize the type definition itself
        // If it's a non-generic type, we can use concretizeTypeDefinition directly
        if declaringType.Generics.IsEmpty then
            let handle, currentCtx =
                TypeConcretization.concretizeTypeDefinition currentCtx declaringType.Identity

            let newState =
                { state with
                    ConcreteTypes = currentCtx.ConcreteTypes
                    _LoadedAssemblies = currentCtx.LoadedAssemblies
                }

            handle, newState
        else
            // For generic types, we need to check if this concrete instantiation already exists
            let genericHandles = genericHandles.ToImmutable ()

            match
                AllConcreteTypes.findExistingConcreteType currentCtx.ConcreteTypes declaringType.Identity genericHandles
            with
            | Some handle ->
                // Type already exists, just return it
                handle,
                { state with
                    ConcreteTypes = currentCtx.ConcreteTypes
                    _LoadedAssemblies = currentCtx.LoadedAssemblies
                }
            | None ->
                // Create the concrete type using mapGeneric to transform from TypeDefn to ConcreteTypeHandle
                let concreteTypeWithHandles =
                    declaringType |> ConcreteType.mapGeneric (fun i _ -> genericHandles.[i])

                // Add to the concrete types
                let handle, newConcreteTypes =
                    AllConcreteTypes.add concreteTypeWithHandles currentCtx.ConcreteTypes

                // Update the state with the new concrete types
                let newState =
                    { state with
                        ConcreteTypes = newConcreteTypes
                        _LoadedAssemblies = currentCtx.LoadedAssemblies
                    }

                handle, newState

    /// Get zero value for a TypeDefn, concretizing it first
    let cliTypeZeroOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (ty : TypeDefn)
        (typeGenerics : ConcreteTypeHandle ImmutableArray)
        (methodGenerics : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * CliType * ConcreteTypeHandle
        =

        // First concretize the type
        // Make sure the current assembly is included in the state for concretization
        let state =
            if state.LoadedAssembly assy.Name |> Option.isSome then
                state
            else
                state.WithLoadedAssembly assy.Name assy

        let state, handle =
            concretizeType loggerFactory baseClassTypes state assy.Name typeGenerics methodGenerics ty

        // Now get the zero value
        let zero, state = cliTypeZeroOfHandle state baseClassTypes handle
        state, zero, handle

    // --- Cross-thread frame resolution primitives ---

    let getFrame (thread : ThreadId) (frameId : FrameId) (state : IlMachineState) : MethodState =
        ThreadState.getFrame frameId state.ThreadState.[thread]

    let setFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (frame : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.setFrame frameId frame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let mapFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (f : MethodState -> MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.mapFrame frameId f threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

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

    let peekEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue option =
        ThreadState.peekEvalStack state.ThreadState.[thread]

    let popEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue * IlMachineState =
        let ret, popped = ThreadState.popFromEvalStack state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread popped
            }

        ret, state

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

    let setArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.setArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let getArrayValue (arrayAllocation : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.getArrayValue arrayAllocation index state.ManagedHeap

    /// There might be no stack frame to return to, so you might get NoFrameToReturn.
    let returnStackFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ReturnFrameResult
        =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]

        match threadStateAtEndOfMethod.MethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let callerFrame = ThreadState.getFrame returnState.JumpTo threadStateAtEndOfMethod

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add
                        currentThread
                        { threadStateAtEndOfMethod with
                            ActiveMethodState = returnState.JumpTo
                            ActiveAssembly = callerFrame.ExecutingMethod.DeclaringType.Assembly
                        }
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            if returnState.DispatchAsExceptionOnReturn then
                // This ctor was constructing a runtime-synthesised exception object.
                // Don't push it onto the eval stack; signal to the caller that exception
                // dispatch should occur.
                let constructed = state.ManagedHeap.NonArrayObjects.[constructing]
                ReturnFrameResult.DispatchException (state, constructing, constructed.ConcreteType)
            else

            // Assumption: a constructor can't also return a value.
            // If we were constructing a reference type, we push a reference to it.
            // Otherwise, extract the now-complete object from the heap and push it to the stack directly.
            let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

            let ty =
                AllConcreteTypes.lookup constructed.ConcreteType state.ConcreteTypes
                |> Option.get

            let ty' =
                state.LoadedAssembly (ty.Assembly)
                |> Option.get
                |> fun a -> a.TypeDefs.[ty.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty' then
                state
                // TODO: ordering of fields probably important
                |> pushToEvalStack (CliType.ValueType constructed.Contents) currentThread
            else
                state |> pushToEvalStack (CliType.ofManagedObject constructing) currentThread
            |> ReturnFrameResult.NormalReturn
        | None ->
            match threadStateAtEndOfMethod.MethodState.EvaluationStack.Values with
            | [] ->
                // no return value
                state
            | [ retVal ] ->
                let retType =
                    threadStateAtEndOfMethod.MethodState.ExecutingMethod.Signature.ReturnType

                match retType with
                // TODO: Claude, don't worry about this one for now, I need to think harder about what's going on here.
                // | TypeDefn.Void -> state
                | retType ->
                    // TODO: generics
                    let zero, state = cliTypeZeroOfHandle state baseClassTypes retType

                    let toPush = EvalStackValue.toCliTypeCoerced zero retVal

                    state |> pushToEvalStack toPush currentThread
            | _ ->
                failwith
                    "Unexpected interpretation result has a local evaluation stack with more than one element on RET"

            |> ReturnFrameResult.NormalReturn

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
                ConcreteTypes = AllConcreteTypes.Empty
                Logger = logger
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                _Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
                TypeHandles = TypeHandleRegistry.empty ()
                FieldHandles = FieldHandleRegistry.empty ()
                RuntimeAssemblyObjects = ImmutableDictionary.Empty
                ManagedThreadObjects = Map.empty
                NextManagedThreadId = 2
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
                ElementZero = zeroOfType ()
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.setStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject
        (ty : ConcreteTypeHandle)
        (fields : CliValueType)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Contents = fields
                ConcreteType = ty
                SyncBlock = SyncBlock.Free
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

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
            MethodState.popFromStackToVariable localVariableIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
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
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (genericMethodTypeArgs : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]

        let memberName : string = assy.Strings mem.Name

        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod
        // Create synthetic TypeDefn generics based on the arity of the concrete generics
        let typeGenerics =
            executing.DeclaringType.Generics
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, assy, targetType, extractedTypeArgs =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                // TODO: generics here?
                let state, assy, targetType =
                    resolveType loggerFactory parent ImmutableArray.Empty assy state

                state, assy, targetType, ImmutableArray.Empty // No type args from TypeReference
            | MetadataToken.TypeSpecification parent ->
                let methodGenerics =
                    executing.Generics
                    |> Seq.map (fun handle ->
                        Concretization.concreteHandleToTypeDefn
                            baseClassTypes
                            handle
                            state.ConcreteTypes
                            state._LoadedAssemblies
                    )
                    |> ImmutableArray.CreateRange

                let state, assy, targetType =
                    resolveTypeFromSpec loggerFactory baseClassTypes parent assy typeGenerics methodGenerics state

                // Extract type arguments from the resolved type
                let extractedTypeArgs = targetType.Generics

                state, assy, targetType, extractedTypeArgs
            | parent -> failwith $"Unexpected: {parent}"

        let state, concreteExtractedTypeArgs =
            ((state, ImmutableArray.CreateBuilder ()), extractedTypeArgs)
            ||> Seq.fold (fun (state, acc) ty ->
                // TODO: generics?
                let state, t =
                    concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        targetType.Assembly
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty

                acc.Add t
                state, acc
            )
            |> Tuple.rmap (fun x -> x.ToImmutable ())

        match mem.Signature with
        | MemberSignature.Field fieldSig ->
            // Concretize the field signature from the member reference
            let state, concreteFieldSig =
                concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    (state.ActiveAssembly(currentThread).Name)
                    concreteExtractedTypeArgs
                    ImmutableArray.Empty
                    fieldSig

            // Find matching fields by comparing concretized signatures
            let state, availableFields =
                ((state, []), targetType.Fields)
                ||> List.fold (fun (state, acc) fi ->
                    if fi.Name <> memberName then
                        state, acc
                    else
                        // Concretize the field's signature for comparison
                        let state, fieldSigConcrete =
                            concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                assy.Name
                                concreteExtractedTypeArgs
                                ImmutableArray.Empty
                                fi.Signature

                        if fieldSigConcrete = concreteFieldSig then
                            state, fi :: acc
                        else
                            state, acc
                )

            let field =
                match availableFields with
                | [] ->
                    failwith
                        $"Could not find field member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> FieldInfo.mapTypeGenerics (fun _ (par, md) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for {targetType.Namespace}.{targetType.Name}'s field {memberName}!"

            state, assy.Name, Choice2Of2 field, extractedTypeArgs

        | MemberSignature.Method memberSig ->
            let availableMethods =
                targetType.Methods |> List.filter (fun mi -> mi.Name = memberName)

            let state, memberSig =
                memberSig
                |> TypeMethodSignature.map
                    state
                    (fun state ty ->
                        concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            (state.ActiveAssembly(currentThread).Name)
                            concreteExtractedTypeArgs
                            genericMethodTypeArgs
                            ty
                    )

            let state, availableMethods =
                ((state, []), availableMethods)
                ||> List.fold (fun (state, acc) meth ->
                    // A candidate overload whose generic arity doesn't match the call site
                    // cannot be the target. Reject it up front: concretising its signature
                    // would otherwise index past the end of `genericMethodTypeArgs` (which
                    // was sized for `memberSig`) whenever the candidate signature mentions
                    // a `GenericMethodParameter`. See e.g. Interlocked.CompareExchange,
                    // where the generic `<T>` overload sits alongside type-specific ones.
                    if meth.Signature.GenericParameterCount <> memberSig.GenericParameterCount then
                        state, acc
                    else
                        let state, methSig =
                            meth.Signature
                            |> TypeMethodSignature.map
                                state
                                (fun state ty ->
                                    concretizeType
                                        loggerFactory
                                        baseClassTypes
                                        state
                                        assy.Name
                                        concreteExtractedTypeArgs
                                        genericMethodTypeArgs
                                        ty
                                )

                        if methSig = memberSig then
                            state, meth :: acc
                        else
                            state, acc
                )

            let method =
                match availableMethods with
                | [] ->
                    failwith
                        $"Could not find member {memberName} with the right signature {memberSig} on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

            state, assy.Name, Choice1Of2 method, extractedTypeArgs

    let getLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (state : IlMachineState)
        : CliType
        =
        (getFrame thread frameId state).LocalVariables.[int<uint16> varIndex]

    let setLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set variable in nonactive thread"
                        | Some existing -> existing |> ThreadState.setLocalVariable frameId varIndex value |> Some
                    )
        }

    let setArgument
        (thread : ThreadId)
        (frameId : FrameId)
        (argIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set argument in nonactive thread"
                        | Some existing -> existing |> ThreadState.setArgument frameId argIndex value |> Some
                    )
        }

    let setSyncBlock
        (addr : ManagedHeapAddress)
        (syncBlockValue : SyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = state.ManagedHeap |> ManagedHeap.setSyncBlock addr syncBlockValue
        }

    let getSyncBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        state.ManagedHeap |> ManagedHeap.getSyncBlock addr

    /// `true` when a `ReinterpretAs ty` projection against a value of the given
    /// shape can be treated as a no-op. Matches same-width primitive reinterprets
    /// within the integer family (including signed<->unsigned and char<->ushort
    /// pairs, which share bit patterns and round-trip through the Int32 stack
    /// slot with modular narrowing) and within the float family (same width
    /// only). Rejects float<->int bit reinterprets, overlay structs, enum
    /// underlying coercions, and any size change; those still need a proper
    /// bytewise implementation.
    let private classifyValueForReinterpret (value : CliType) : (string * int) voption =
        match value with
        | CliType.Bool _ -> ValueSome ("int", 1)
        | CliType.Char _ -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.UInt8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.Int16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.UInt16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int32 _) -> ValueSome ("int", 4)
        | CliType.Numeric (CliNumericType.Int64 _) -> ValueSome ("int", 8)
        | CliType.Numeric (CliNumericType.Float32 _) -> ValueSome ("float", 4)
        | CliType.Numeric (CliNumericType.Float64 _) -> ValueSome ("float", 8)
        | _ -> ValueNone

    let private classifyTypeForReinterpret (ty : ConcreteType<ConcreteTypeHandle>) : (string * int) voption =
        if ty.Namespace <> "System" then
            ValueNone
        else
            match ty.Name with
            | "Boolean"
            | "SByte"
            | "Byte" -> ValueSome ("int", 1)
            | "Int16"
            | "UInt16"
            | "Char" -> ValueSome ("int", 2)
            | "Int32"
            | "UInt32" -> ValueSome ("int", 4)
            | "Int64"
            | "UInt64" -> ValueSome ("int", 8)
            | "Single" -> ValueSome ("float", 4)
            | "Double" -> ValueSome ("float", 8)
            | _ -> ValueNone

    let private isSafeReinterpretPassthrough (value : CliType) (ty : ConcreteType<ConcreteTypeHandle>) : bool =
        match classifyValueForReinterpret value, classifyTypeForReinterpret ty with
        | ValueSome v, ValueSome t -> v = t
        | _ -> false

    /// A zero-valued `CliType` for one of the primitive `System.*` types that
    /// `classifyTypeForReinterpret` recognises. Used by the narrowing-reinterpret
    /// path in `readManagedByref` to seed `CliType.ofBytesLike` without having
    /// to thread `BaseClassTypes` through every caller.
    let private primitiveZero (ty : ConcreteType<ConcreteTypeHandle>) : CliType option =
        if ty.Namespace <> "System" then
            None
        else
            match ty.Name with
            | "Boolean" -> Some (CliType.Bool 0uy)
            | "SByte" -> Some (CliType.Numeric (CliNumericType.Int8 0y))
            | "Byte" -> Some (CliType.Numeric (CliNumericType.UInt8 0uy))
            | "Int16" -> Some (CliType.Numeric (CliNumericType.Int16 0s))
            | "UInt16" -> Some (CliType.Numeric (CliNumericType.UInt16 0us))
            | "Char" -> Some (CliType.Char (0uy, 0uy))
            | "Int32" -> Some (CliType.Numeric (CliNumericType.Int32 0))
            | "UInt32" -> Some (CliType.Numeric (CliNumericType.Int32 0))
            | "Int64" -> Some (CliType.Numeric (CliNumericType.Int64 0L))
            | "UInt64" -> Some (CliType.Numeric (CliNumericType.Int64 0L))
            | "Single" -> Some (CliType.Numeric (CliNumericType.Float32 0.0f))
            | "Double" -> Some (CliType.Numeric (CliNumericType.Float64 0.0))
            | _ -> None

    /// Truncating reinterpret: a wider primitive value read through a narrower
    /// primitive view yields the low-order `sizeof(target)` bytes of the source.
    /// Returns `None` when the reinterpret is not a same-family narrowing
    /// (different families, widening, or non-primitive target). Widening is
    /// intentionally excluded: reading a wider type through a narrower byref
    /// would need a bytewise gather across cells (what `Unsafe.ReadUnaligned`
    /// provides), which is out of scope here.
    let private narrowingReinterpret (value : CliType) (ty : ConcreteType<ConcreteTypeHandle>) : CliType option =
        match classifyValueForReinterpret value, classifyTypeForReinterpret ty, primitiveZero ty with
        | ValueSome (vFam, vSize), ValueSome (tFam, tSize), Some targetZero when vFam = tFam && vSize > tSize ->
            let bytes = CliType.ToBytes value
            let truncated = Array.sub bytes 0 tSize
            Some (CliType.ofBytesLike targetZero truncated)
        | _ -> None

    /// Gather `count` bytes from the cell stream of an `ArrayElement` or
    /// `StringCharAt` root, starting at `startCell` + `startInCellOffset`
    /// bytes. Walks cell-by-cell, honouring each cell's own serialised size;
    /// whole-cell byte offsets are folded into the starting cell first so the
    /// per-iteration arithmetic stays in `[0, cellSize)`. Used by the
    /// byte-view read path of `readManagedByref` when a byref ends in a
    /// non-zero `ByteOffset` under a trailing reinterpret — the natural
    /// projection fold can't express the mid-cell start, because `ReinterpretAs`
    /// as a per-value transform only sees whole cells.
    let private gatherBytesFromByteCursor
        (state : IlMachineState)
        (root : ByrefRoot)
        (startCell : int)
        (startInCellOffset : int)
        (count : int)
        : byte[]
        =
        let buf = Array.zeroCreate<byte> count

        match root with
        | ByrefRoot.ArrayElement (arr, _) ->
            let arrObj = state.ManagedHeap.Arrays.[arr]

            let cellSize =
                if arrObj.Length = 0 then
                    CliType.sizeOf arrObj.ElementZero
                else
                    CliType.sizeOf arrObj.Elements.[0]

            let mutable filled = 0
            let mutable cell = startCell
            let mutable inCellOffset = startInCellOffset

            if cellSize > 0 && inCellOffset >= cellSize then
                cell <- cell + inCellOffset / cellSize
                inCellOffset <- inCellOffset % cellSize

            while filled < count do
                if cell >= arrObj.Length then
                    failwith
                        $"readManagedByref: byte-view gather past end of array at cell %d{cell} of length %d{arrObj.Length} while gathering %d{count} bytes"

                let cellBytes = CliType.ToBytes arrObj.Elements.[cell]
                let canTake = cellBytes.Length - inCellOffset
                let take = min canTake (count - filled)
                Array.blit cellBytes inCellOffset buf filled take
                filled <- filled + take
                cell <- cell + 1
                inCellOffset <- 0

            buf
        | ByrefRoot.StringCharAt (strAddr, _) ->
            let charSize = 2

            let stringLength =
                match state.ManagedHeap.StringContents.TryGetValue strAddr with
                | true, s -> s.Length
                | false, _ -> failwith $"readManagedByref: unknown string length for %O{strAddr}"

            let mutable filled = 0
            let mutable cell = startCell
            let mutable inCellOffset = startInCellOffset

            if inCellOffset >= charSize then
                cell <- cell + inCellOffset / charSize
                inCellOffset <- inCellOffset % charSize

            while filled < count do
                if cell > stringLength then
                    failwith
                        $"readManagedByref: byte-view gather past end of string at cell %d{cell} of length %d{stringLength} while gathering %d{count} bytes"

                let absIndex = ManagedHeap.resolveStringCharAt strAddr cell state.ManagedHeap
                let ch = state.ManagedHeap.StringArrayData.[absIndex]
                let cellBytes = CliType.ToBytes (CliType.ofChar ch)
                let canTake = cellBytes.Length - inCellOffset
                let take = min canTake (count - filled)
                Array.blit cellBytes inCellOffset buf filled take
                filled <- filled + take
                cell <- cell + 1
                inCellOffset <- 0

            buf
        | _ -> failwith $"gatherBytesFromByteCursor: root %O{root} is not a bytewise-addressable cell stream"

    /// Symmetric scatter for the byte-view write path. Splices `bytes` into the
    /// cell stream of an `ArrayElement` or `StringCharAt` root starting at
    /// `startCell` + `startInCellOffset`, preserving the bytes of each cell
    /// that lie outside the splice window. Returns the updated machine state.
    let private scatterBytesToByteCursor
        (state : IlMachineState)
        (root : ByrefRoot)
        (startCell : int)
        (startInCellOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let count = bytes.Length

        match root with
        | ByrefRoot.ArrayElement (arr, _) ->
            let arrObj = state.ManagedHeap.Arrays.[arr]

            let cellSize =
                if arrObj.Length = 0 then
                    CliType.sizeOf arrObj.ElementZero
                else
                    CliType.sizeOf arrObj.Elements.[0]

            let mutable state = state
            let mutable filled = 0
            let mutable cell = startCell
            let mutable inCellOffset = startInCellOffset

            if cellSize > 0 && inCellOffset >= cellSize then
                cell <- cell + inCellOffset / cellSize
                inCellOffset <- inCellOffset % cellSize

            while filled < count do
                if cell >= arrObj.Length then
                    failwith
                        $"writeManagedByref: byte-view scatter past end of array at cell %d{cell} of length %d{arrObj.Length}"

                let existing = arrObj.Elements.[cell]
                let existingBytes = CliType.ToBytes existing
                let canTake = existingBytes.Length - inCellOffset
                let take = min canTake (count - filled)
                let newCellBytes = Array.copy existingBytes
                Array.blit bytes filled newCellBytes inCellOffset take
                let newCell = CliType.ofBytesLike existing newCellBytes
                state <- setArrayValue arr newCell cell state
                filled <- filled + take
                cell <- cell + 1
                inCellOffset <- 0

            state
        | ByrefRoot.StringCharAt (strAddr, _) ->
            let charSize = 2

            let stringLength =
                match state.ManagedHeap.StringContents.TryGetValue strAddr with
                | true, s -> s.Length
                | false, _ -> failwith $"writeManagedByref: unknown string length for %O{strAddr}"

            let mutable state = state
            let mutable filled = 0
            let mutable cell = startCell
            let mutable inCellOffset = startInCellOffset

            if inCellOffset >= charSize then
                cell <- cell + inCellOffset / charSize
                inCellOffset <- inCellOffset % charSize

            while filled < count do
                if cell > stringLength then
                    failwith
                        $"writeManagedByref: byte-view scatter past end of string at cell %d{cell} of length %d{stringLength}"

                let absIndex = ManagedHeap.resolveStringCharAt strAddr cell state.ManagedHeap
                let existingChar = state.ManagedHeap.StringArrayData.[absIndex]
                let existingBytes = CliType.ToBytes (CliType.ofChar existingChar)
                let canTake = existingBytes.Length - inCellOffset
                let take = min canTake (count - filled)
                let newCellBytes = Array.copy existingBytes
                Array.blit bytes filled newCellBytes inCellOffset take
                let newChar = CliType.ofBytesLike (CliType.ofChar existingChar) newCellBytes

                let newCharValue =
                    match newChar with
                    | CliType.Char (hi, lo) -> char (int hi * 256 + int lo)
                    | other -> failwith $"writeManagedByref: expected Char, got %O{other}"

                let heap, updateFirstChar =
                    ManagedHeap.writeStringChar strAddr cell newCharValue state.ManagedHeap

                state <-
                    { state with
                        ManagedHeap = heap
                    }

                if updateFirstChar then
                    let updated =
                        ManagedHeap.get strAddr state.ManagedHeap
                        |> AllocatedNonArrayObject.SetField "_firstChar" (CliType.ofChar newCharValue)

                    state <-
                        { state with
                            ManagedHeap = ManagedHeap.set strAddr updated state.ManagedHeap
                        }

                filled <- filled + take
                cell <- cell + 1
                inCellOffset <- 0

            state
        | _ -> failwith $"scatterBytesToByteCursor: root %O{root} is not a bytewise-addressable cell stream"

    /// Read the full CliType value of a non-cell-stream byref root
    /// (LocalVariable, Argument, HeapValue, HeapObjectField). Used by the
    /// single-value byte-view gather/scatter path, which serialises the value
    /// to a byte image, slices or splices, and writes back.
    let private readSingleValueRoot (state : IlMachineState) (root : ByrefRoot) : CliType =
        match root with
        | ByrefRoot.LocalVariable (t, f, v) -> (getFrame t f state).LocalVariables.[int<uint16> v]
        | ByrefRoot.Argument (t, f, v) -> (getFrame t f state).Arguments.[int<uint16> v]
        | ByrefRoot.HeapValue addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | ByrefRoot.HeapObjectField (addr, fieldName) ->
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceField fieldName
        | _ -> failwith $"readSingleValueRoot: root %O{root} is not a single-value root"

    /// Symmetric writeback for `readSingleValueRoot`. Updates the storage at
    /// `root` to hold `newValue`.
    let private writeSingleValueRoot (state : IlMachineState) (root : ByrefRoot) (newValue : CliType) : IlMachineState =
        match root with
        | ByrefRoot.LocalVariable (t, f, v) -> state |> setLocalVariable t f v newValue
        | ByrefRoot.Argument (t, f, v) -> state |> setArgument t f v newValue
        | ByrefRoot.HeapValue addr ->
            match newValue with
            | CliType.ValueType contents ->
                let existing = ManagedHeap.get addr state.ManagedHeap

                { state with
                    ManagedHeap =
                        ManagedHeap.set
                            addr
                            { existing with
                                Contents = contents
                            }
                            state.ManagedHeap
                }
            | other -> failwith $"writeSingleValueRoot: cannot write non-value-type {other} through heap value byref"
        | ByrefRoot.HeapObjectField (addr, fieldName) ->
            let updated =
                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetField fieldName newValue

            { state with
                ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
            }
        | _ -> failwith $"writeSingleValueRoot: root %O{root} is not a single-value root"

    /// Slice `count` bytes starting at `byteOffset` out of the serialised
    /// byte image of a single-value root. Fails if the slice would read past
    /// the root's size — a CLR runtime would permit this as
    /// implementation-defined behaviour, but for the deterministic interpreter
    /// it's surfaced as an explicit interpreter bug rather than returning
    /// stale bytes.
    let private gatherBytesFromSingleValueRoot
        (state : IlMachineState)
        (root : ByrefRoot)
        (byteOffset : int)
        (count : int)
        : byte[]
        =
        let rootValue = readSingleValueRoot state root
        let rootBytes = CliType.ToBytes rootValue

        if byteOffset < 0 || byteOffset + count > rootBytes.Length then
            failwith
                $"readManagedByref: byte-view gather [%d{byteOffset}, %d{byteOffset + count}) past end of single-value root %O{root} of size %d{rootBytes.Length}"

        Array.sub rootBytes byteOffset count

    /// Splice `bytes` into the serialised byte image of a single-value root at
    /// `byteOffset` and write the updated value back to the root's storage.
    let private scatterBytesToSingleValueRoot
        (state : IlMachineState)
        (root : ByrefRoot)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let rootValue = readSingleValueRoot state root
        let rootBytes = CliType.ToBytes rootValue

        if byteOffset < 0 || byteOffset + bytes.Length > rootBytes.Length then
            failwith
                $"writeManagedByref: byte-view scatter [%d{byteOffset}, %d{byteOffset + bytes.Length}) past end of single-value root %O{root} of size %d{rootBytes.Length}"

        let newBytes = Array.copy rootBytes
        Array.blit bytes 0 newBytes byteOffset bytes.Length
        let updated = CliType.ofBytesLike rootValue newBytes
        writeSingleValueRoot state root updated

    /// Recognise a byref whose trailing projections describe a byte cursor
    /// under an optional reinterpret view: `[ByteOffset n]`,
    /// `[ReinterpretAs ty]`, or `[ReinterpretAs ty; ByteOffset n]`. Returns
    /// the view type (if any) and the byte offset (0 when no `ByteOffset`
    /// projection is present). The prefix must be empty: `ArrayElement` and
    /// `StringCharAt` are the only byte-addressable roots, and they don't
    /// compose with `Field` prefixes here.
    ///
    /// Routing bare `[ReinterpretAs ty]` through the bytewise path
    /// generalises narrowing/widening reinterprets (e.g. `ref int` viewed as
    /// `ref byte` or vice versa) to the same gather/scatter that handles
    /// mid-cell offsets, so a plain `stind` / `ldind` at offset zero goes
    /// through the same machinery as one at a non-zero offset.
    let private tryByteViewTail
        (projs : ByrefProjection list)
        : (ConcreteType<ConcreteTypeHandle> option * int) option
        =
        match List.rev projs with
        | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs ty :: [] -> Some (Some ty, n)
        | ByrefProjection.ReinterpretAs ty :: [] -> Some (Some ty, 0)
        | ByrefProjection.ByteOffset n :: [] -> Some (None, n)
        | _ -> None

    let readManagedByref (state : IlMachineState) (src : ManagedPointerSource) : CliType =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i) as root, projs) when
            (tryByteViewTail projs).IsSome
            ->
            // Byte-cursor read over an array. The view type (if present on the
            // reinterpret) tells us how many bytes to gather and how to
            // rebuild a `CliType`; without a reinterpret we read the cell's
            // own type starting at the unaligned byte position.
            let viewTy, byteOffset = (tryByteViewTail projs).Value

            let template, count =
                match viewTy with
                | Some ty ->
                    match primitiveZero ty with
                    | Some zero -> zero, CliType.sizeOf zero
                    | None ->
                        failwith
                            $"TODO: readManagedByref: byte-view over %O{root} with non-primitive reinterpret type %s{ty.Namespace}.%s{ty.Name}"
                | None ->
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    let template =
                        if arrObj.Length = 0 then
                            arrObj.ElementZero
                        else
                            arrObj.Elements.[0]

                    template, CliType.sizeOf template

            let bytes = gatherBytesFromByteCursor state root i byteOffset count
            CliType.ofBytesLike template bytes
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (_, i) as root, projs) when (tryByteViewTail projs).IsSome ->
            // Byte-cursor read over a string. Cell type is `char` (2 bytes);
            // without a reinterpret view we read a char starting at the
            // possibly-unaligned byte position.
            let viewTy, byteOffset = (tryByteViewTail projs).Value

            let template, count =
                match viewTy with
                | Some ty ->
                    match primitiveZero ty with
                    | Some zero -> zero, CliType.sizeOf zero
                    | None ->
                        failwith
                            $"TODO: readManagedByref: byte-view over %O{root} with non-primitive reinterpret type %s{ty.Namespace}.%s{ty.Name}"
                | None -> CliType.Char (0uy, 0uy), 2

            let bytes = gatherBytesFromByteCursor state root i byteOffset count
            CliType.ofBytesLike template bytes
        | ManagedPointerSource.Byref (root, projs) when
            (match root with
             | ByrefRoot.LocalVariable _
             | ByrefRoot.Argument _
             | ByrefRoot.HeapValue _
             | ByrefRoot.HeapObjectField _ -> (tryByteViewTail projs).IsSome
             | _ -> false)
            ->
            // Byte-cursor read over a single-value root. The root's own
            // `CliType` is serialised to its byte image, the requested slice
            // is extracted, and (if a reinterpret view is present) the bytes
            // are rebuilt as that view's primitive zero; otherwise we read
            // back into the root's natural representation at the offset.
            let viewTy, byteOffset = (tryByteViewTail projs).Value

            let template, count =
                match viewTy with
                | Some ty ->
                    match primitiveZero ty with
                    | Some zero -> zero, CliType.sizeOf zero
                    | None ->
                        failwith
                            $"TODO: readManagedByref: byte-view over single-value root %O{root} with non-primitive reinterpret type %s{ty.Namespace}.%s{ty.Name}"
                | None ->
                    let rootValue = readSingleValueRoot state root
                    rootValue, CliType.sizeOf rootValue

            let bytes = gatherBytesFromSingleValueRoot state root byteOffset count
            CliType.ofBytesLike template bytes
        | ManagedPointerSource.Byref (root, projs) ->
            let rootValue =
                match root with
                | ByrefRoot.LocalVariable (t, f, v) -> (getFrame t f state).LocalVariables.[int<uint16> v]
                | ByrefRoot.Argument (t, f, v) -> (getFrame t f state).Arguments.[int<uint16> v]
                | ByrefRoot.HeapValue addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
                | ByrefRoot.HeapObjectField (addr, fieldName) ->
                    ManagedHeap.get addr state.ManagedHeap
                    |> AllocatedNonArrayObject.DereferenceField fieldName
                | ByrefRoot.ArrayElement (arr, index) -> getArrayValue arr index state
                | ByrefRoot.StringCharAt (strAddr, charIndex) ->
                    // Strings are null-terminated in our heap (matching the CLR layout),
                    // so addresses within `[0, _stringLength]` are legal reads. Reading
                    // past the terminator is an interpreter bug — surface it loudly.
                    // Bounds-check against the owning string's own length so unsafe
                    // pointer arithmetic cannot silently step into a neighbouring
                    // string's allocation in the shared char pool.
                    let absIndex = ManagedHeap.resolveStringCharAt strAddr charIndex state.ManagedHeap

                    CliType.ofChar state.ManagedHeap.StringArrayData.[absIndex]

            projs
            |> List.fold
                (fun value proj ->
                    match proj with
                    | ByrefProjection.Field name ->
                        match value with
                        | CliType.ValueType vt -> CliValueType.DereferenceField name vt
                        | v -> failwith $"could not find field {name} on non-ValueType {v}"
                    | ByrefProjection.ReinterpretAs ty ->
                        // `ReinterpretAs` is address-preserving, but the bits we
                        // hand back must still make sense to the caller: they
                        // will be coerced to the caller's static target type
                        // (e.g. via `Ldind_*`) and a size- or family-changing
                        // reinterpret would silently corrupt the result. Pass
                        // through for same-representation primitive reinterprets;
                        // narrow within the same family (e.g. `ref char` read
                        // as `ref byte` produces the low-order byte of the
                        // char); reject everything else until a proper bytewise
                        // gather exists.
                        if isSafeReinterpretPassthrough value ty then
                            value
                        else
                            match narrowingReinterpret value ty with
                            | Some v -> v
                            | None ->
                                failwith
                                    $"TODO: read through `ReinterpretAs` from value %O{value} as type %s{ty.Namespace}.%s{ty.Name}; needs a bytewise implementation"
                    | ByrefProjection.ByteOffset n ->
                        // ByteOffset only makes sense as a byte cursor under a
                        // trailing ReinterpretAs. Reading a ByteOffset-terminated
                        // byref via the natural projection fold produces an
                        // already-reinterpreted CliType, not the raw byte stream
                        // we would need to slice. Callers that need a byte-offset
                        // read go through Unsafe.ReadUnaligned (which gathers
                        // bytes from the cell stream directly); dropping into
                        // the generic fold here means the shape is something we
                        // don't yet model.
                        failwith
                            $"TODO: readManagedByref via ByteOffset %d{n} requires the bytewise gather implemented by Unsafe.ReadUnaligned; generic Ldind at a non-zero byte offset is out of scope for this PR (byref: %O{src})"
                )
                rootValue

    let private applyProjectionsForWrite
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType
        =
        let rec go (rootValue : CliType) (projs : ByrefProjection list) (newValue : CliType) : CliType =
            match projs with
            | [] -> newValue
            | [ ByrefProjection.Field name ] -> CliType.withFieldSet name newValue rootValue
            | ByrefProjection.Field name :: rest ->
                let fieldValue = CliType.getField name rootValue
                let updatedField = go fieldValue rest newValue
                CliType.withFieldSet name updatedField rootValue
            | [ ByrefProjection.ReinterpretAs ty ] ->
                // Same safety gate as `readManagedByref`: size-preserving
                // primitive reinterprets share storage with the underlying
                // value. Require both the stored value and the newValue to
                // match the reinterpret target's natural representation; if
                // either differs, the caller is doing a bit-reinterpret we
                // don't model and the write stays an explicit TODO.
                if
                    isSafeReinterpretPassthrough rootValue ty
                    && isSafeReinterpretPassthrough newValue ty
                then
                    // Normalise the stored value back to the rootValue's
                    // CliType so the slot keeps its original view: writing a
                    // `short` through a `ref short` obtained via
                    // `Unsafe.As<ushort, short>` must leave the backing slot
                    // as a ushort with bit-preserving narrowing, not replace
                    // the slot's type with Int16. The stack round-trip matches
                    // ECMA III.1.1.1 narrowing semantics for same-width ints;
                    // it's the identity for matching-float widths.
                    EvalStackValue.toCliTypeCoerced rootValue (EvalStackValue.ofCliType newValue)
                else
                    failwith
                        $"TODO: write through `ReinterpretAs` as type %s{ty.Namespace}.%s{ty.Name}; rootValue=%O{rootValue}, newValue=%O{newValue}"
            | ByrefProjection.ReinterpretAs ty :: _ ->
                failwith
                    $"TODO: write through `ReinterpretAs` as %s{ty.Namespace}.%s{ty.Name} followed by further projections; needs a bytewise implementation"
            | ByrefProjection.ByteOffset n :: _ ->
                // Symmetric to the readManagedByref ByteOffset case: byte-offset
                // writes go through Unsafe.WriteUnaligned (which scatters bytes
                // into the cell stream directly), not through the generic write
                // fold. Reaching here means a generic Stind at a non-zero byte
                // offset, which we don't yet model.
                failwith
                    $"TODO: writeManagedByref via ByteOffset %d{n} requires the bytewise scatter implemented by Unsafe.WriteUnaligned; generic Stind at a non-zero byte offset is out of scope for this PR"

        go rootValue projs newValue

    let writeManagedByref (state : IlMachineState) (src : ManagedPointerSource) (newValue : CliType) : IlMachineState =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i) as root, projs) when
            (tryByteViewTail projs).IsSome
            ->
            // Byte-cursor write over an array: coerce `newValue` to the view
            // type's canonical representation, serialise it, and scatter the
            // bytes into the cell stream.
            let viewTy, byteOffset = (tryByteViewTail projs).Value

            let template =
                match viewTy with
                | Some ty ->
                    match primitiveZero ty with
                    | Some zero -> zero
                    | None ->
                        failwith
                            $"TODO: writeManagedByref: byte-view over %O{root} with non-primitive reinterpret type %s{ty.Namespace}.%s{ty.Name}"
                | None ->
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    if arrObj.Length = 0 then
                        arrObj.ElementZero
                    else
                        arrObj.Elements.[0]

            let coerced =
                EvalStackValue.toCliTypeCoerced template (EvalStackValue.ofCliType newValue)

            let bytes = CliType.ToBytes coerced
            scatterBytesToByteCursor state root i byteOffset bytes
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (_, i) as root, projs) when (tryByteViewTail projs).IsSome ->
            let viewTy, byteOffset = (tryByteViewTail projs).Value

            let template =
                match viewTy with
                | Some ty ->
                    match primitiveZero ty with
                    | Some zero -> zero
                    | None ->
                        failwith
                            $"TODO: writeManagedByref: byte-view over %O{root} with non-primitive reinterpret type %s{ty.Namespace}.%s{ty.Name}"
                | None -> CliType.Char (0uy, 0uy)

            let coerced =
                EvalStackValue.toCliTypeCoerced template (EvalStackValue.ofCliType newValue)

            let bytes = CliType.ToBytes coerced
            scatterBytesToByteCursor state root i byteOffset bytes
        | ManagedPointerSource.Byref (root, projs) when
            (match root with
             | ByrefRoot.LocalVariable _
             | ByrefRoot.Argument _
             | ByrefRoot.HeapValue _
             | ByrefRoot.HeapObjectField _ -> (tryByteViewTail projs).IsSome
             | _ -> false)
            ->
            // Byte-cursor write over a single-value root: coerce `newValue` to
            // the view type (when present), serialise it, and splice the
            // bytes into the root's stored byte image before writing back.
            let viewTy, byteOffset = (tryByteViewTail projs).Value

            let template =
                match viewTy with
                | Some ty ->
                    match primitiveZero ty with
                    | Some zero -> zero
                    | None ->
                        failwith
                            $"TODO: writeManagedByref: byte-view over single-value root %O{root} with non-primitive reinterpret type %s{ty.Namespace}.%s{ty.Name}"
                | None -> readSingleValueRoot state root

            let coerced =
                EvalStackValue.toCliTypeCoerced template (EvalStackValue.ofCliType newValue)

            let bytes = CliType.ToBytes coerced
            scatterBytesToSingleValueRoot state root byteOffset bytes
        | ManagedPointerSource.Byref (root, []) ->
            // Direct write to the root location, no projections to navigate.
            match root with
            | ByrefRoot.LocalVariable (t, f, v) -> state |> setLocalVariable t f v newValue
            | ByrefRoot.Argument (t, f, v) -> state |> setArgument t f v newValue
            | ByrefRoot.HeapValue addr ->
                let contents =
                    match newValue with
                    | CliType.ValueType contents -> contents
                    | other -> failwith $"cannot write non-value-type {other} through heap value byref"

                let updated =
                    let existing = ManagedHeap.get addr state.ManagedHeap

                    { existing with
                        Contents = contents
                    }

                { state with
                    ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
                }
            | ByrefRoot.HeapObjectField (addr, fieldName) ->
                let updated =
                    ManagedHeap.get addr state.ManagedHeap
                    |> AllocatedNonArrayObject.SetField fieldName newValue

                { state with
                    ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
                }
            | ByrefRoot.ArrayElement (arr, index) -> state |> setArrayValue arr newValue index
            | ByrefRoot.StringCharAt (strAddr, charIndex) ->
                let newChar =
                    match newValue with
                    | CliType.Char (hi, lo) -> char (int hi * 256 + int lo)
                    | other -> failwith $"cannot write non-Char value %O{other} through StringCharAt byref"

                let heap, updateFirstChar =
                    ManagedHeap.writeStringChar strAddr charIndex newChar state.ManagedHeap

                let state =
                    { state with
                        ManagedHeap = heap
                    }

                if updateFirstChar then
                    let updated =
                        ManagedHeap.get strAddr state.ManagedHeap
                        |> AllocatedNonArrayObject.SetField "_firstChar" (CliType.ofChar newChar)

                    { state with
                        ManagedHeap = ManagedHeap.set strAddr updated state.ManagedHeap
                    }
                else
                    state
        | ManagedPointerSource.Byref (root, projs) ->
            // Projected write: read root, navigate projections, write new value, reconstruct backward.
            let rootValue, writeBack =
                match root with
                | ByrefRoot.LocalVariable (t, f, v) ->
                    (getFrame t f state).LocalVariables.[int<uint16> v],
                    (fun updated -> state |> setLocalVariable t f v updated)
                | ByrefRoot.Argument (t, f, v) ->
                    (getFrame t f state).Arguments.[int<uint16> v], (fun updated -> state |> setArgument t f v updated)
                | ByrefRoot.HeapValue addr ->
                    CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents,
                    (fun updated ->
                        match updated with
                        | CliType.ValueType contents ->
                            let existing = ManagedHeap.get addr state.ManagedHeap

                            { state with
                                ManagedHeap =
                                    ManagedHeap.set
                                        addr
                                        { existing with
                                            Contents = contents
                                        }
                                        state.ManagedHeap
                            }
                        | other -> failwith $"cannot write non-value-type {other} through heap value byref"
                    )
                | ByrefRoot.HeapObjectField (addr, fieldName) ->
                    (ManagedHeap.get addr state.ManagedHeap
                     |> AllocatedNonArrayObject.DereferenceField fieldName),
                    (fun updated ->
                        let obj =
                            ManagedHeap.get addr state.ManagedHeap
                            |> AllocatedNonArrayObject.SetField fieldName updated

                        { state with
                            ManagedHeap = ManagedHeap.set addr obj state.ManagedHeap
                        }
                    )
                | ByrefRoot.ArrayElement (arr, index) ->
                    getArrayValue arr index state, (fun updated -> state |> setArrayValue arr updated index)
                | ByrefRoot.StringCharAt (strAddr, charIndex) ->
                    let absIndex = ManagedHeap.resolveStringCharAt strAddr charIndex state.ManagedHeap

                    CliType.ofChar state.ManagedHeap.StringArrayData.[absIndex],
                    (fun updated ->
                        let newChar =
                            match updated with
                            | CliType.Char (hi, lo) -> char (int hi * 256 + int lo)
                            | other -> failwith $"cannot write non-Char value %O{other} through StringCharAt byref"

                        let heap, updateFirstChar =
                            ManagedHeap.writeStringChar strAddr charIndex newChar state.ManagedHeap

                        let state =
                            { state with
                                ManagedHeap = heap
                            }

                        if updateFirstChar then
                            let obj =
                                ManagedHeap.get strAddr state.ManagedHeap
                                |> AllocatedNonArrayObject.SetField "_firstChar" (CliType.ofChar newChar)

                            { state with
                                ManagedHeap = ManagedHeap.set strAddr obj state.ManagedHeap
                            }
                        else
                            state
                    )

            let updatedRoot = applyProjectionsForWrite rootValue projs newValue
            writeBack updatedRoot

    let executeDelegateConstructor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (instruction : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        // We've been called with arguments already popped from the stack into local arguments.
        let constructing = instruction.Arguments.[0]
        let targetObj = instruction.Arguments.[1]
        let methodPtr = instruction.Arguments.[2]

        let targetObj =
            match targetObj with
            | CliType.ObjectRef (Some target) -> Some target
            | CliType.ObjectRef None -> None
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
            | _ -> failwith $"Unexpected target type for delegate: {targetObj}"

        let constructing =
            match constructing with
            | CliType.ObjectRef None -> failwith "unexpectedly constructing the null delegate"
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) ->
                failwith "unexpectedly constructing the null delegate"
            | CliType.ObjectRef (Some target) -> target
            | _ -> failwith $"Unexpectedly not constructing a managed object: {constructing}"

        let heapObj =
            match state.ManagedHeap.NonArrayObjects.TryGetValue constructing with
            | true, obj -> obj
            | false, _ -> failwith $"Delegate object {constructing} not found on heap"

        // Standard delegate fields in .NET are _target and _methodPtr
        // Update the fields with the target object and method pointer
        let allConcreteTypes = state.ConcreteTypes

        let objectHandle =
            AllConcreteTypes.findExistingNonGenericConcreteType allConcreteTypes baseClassTypes.Object.Identity
            |> Option.get

        let updatedObj =
            let newContents =
                heapObj.Contents
                |> CliValueType.AddField
                    {
                        Name = "_target"
                        Contents = CliType.ObjectRef targetObj
                        Offset = None
                        Type = objectHandle
                    }
                |> CliValueType.AddField
                    {
                        Name = "_methodPtr"
                        Contents = methodPtr
                        Offset = None
                        Type = objectHandle
                    }

            { heapObj with
                Contents = newContents
            }

        let updatedHeap =
            { state.ManagedHeap with
                NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add constructing updatedObj
            }

        { state with
            ManagedHeap = updatedHeap
        }

    /// Returns the type handle and an allocated System.RuntimeType.
    let getOrAllocateType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (defn : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, runtimeType =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeType.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            TypeHandleRegistry.getOrAllocate
                state.ConcreteTypes
                baseClassTypes
                state
                (fun fields state -> allocateManagedObject runtimeType fields state)
                defn
                state.TypeHandles

        let state =
            { state with
                TypeHandles = reg
            }

        result, state

    /// Returns a System.RuntimeFieldHandle.
    let getOrAllocateField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssy : AssemblyName)
        (fieldHandle : FieldDefinitionHandle)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let field = state.LoadedAssembly(declaringAssy).Value.Fields.[fieldHandle]

        // For LdToken, we need to convert GenericParamFromMetadata to TypeDefn
        // When we don't have generic context, we use the generic type parameters directly
        let declaringTypeWithGenerics =
            field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringType, state =
            concretizeFieldDeclaringType loggerFactory baseClassTypes declaringTypeWithGenerics state

        let state, runtimeType =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeType.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            FieldHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> allocateManagedObject runtimeType fields state)
                declaringAssy
                declaringType
                fieldHandle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = reg
            }

        result, state

    let setStatic
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (value : CliType)
        (this : IlMachineState)
        : IlMachineState
        =
        let statics =
            match this._Statics.TryGetValue ty with
            | false, _ -> this._Statics.Add (ty, Map.ofList [ field, value ])
            | true, v -> this._Statics.SetItem (ty, Map.add field value v)

        { this with
            _Statics = statics
        }

    let getStatic
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (this : IlMachineState)
        : CliType option
        =
        match this._Statics.TryGetValue ty with
        | false, _ -> None
        | true, v -> Map.tryFind field v

    let evalStackValueToObjectRef (state : IlMachineState) (value : EvalStackValue) : ManagedHeapAddress option =
        match value with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | EvalStackValue.ManagedPointer src ->
            match readManagedByref state src with
            | CliType.ObjectRef addr -> addr
            | other -> failwith $"expected object reference, got {other}"
        | other -> failwith $"expected object reference, got {other}"

    let lookupTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeDef : TypeDefinitionHandle)
        : IlMachineState * TypeDefn
        =
        let defn = activeAssy.TypeDefs.[typeDef]
        state, DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies defn

    let lookupTypeRef
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        typeGenerics
        (ref : TypeReferenceHandle)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        let ref = activeAssy.TypeRefs.[ref]

        // Convert ConcreteTypeHandles back to TypeDefn for metadata operations
        let typeGenerics =
            typeGenerics
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, assy, resolved =
            resolveTypeFromRef loggerFactory activeAssy ref typeGenerics state

        state, DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved, assy

    let private ensureAssemblyLoadedByName
        (loggerFactory : ILoggerFactory)
        (state : IlMachineState)
        (referencedInAssembly : DumpedAssembly)
        (assemblyName : AssemblyName)
        : IlMachineState * DumpedAssembly
        =
        match state.LoadedAssembly assemblyName with
        | Some loadedAssembly -> state, loadedAssembly
        | None ->
            let handle =
                referencedInAssembly.AssemblyReferences
                |> Seq.tryPick (fun (KeyValue (assemblyRefHandle, assemblyRef)) ->
                    if assemblyRef.Name.FullName = assemblyName.FullName then
                        Some assemblyRefHandle
                    else
                        None
                )
                |> Option.defaultWith (fun () ->
                    failwithf
                        "Assembly %s needs base assembly %s, but no AssemblyReferenceHandle was found"
                        referencedInAssembly.Name.FullName
                        assemblyName.FullName
                )

            let state, loadedAssembly, _ =
                loadAssembly loggerFactory referencedInAssembly handle state

            state, loadedAssembly

    /// Resolve a BaseTypeInfo to the assembly and TypeDefn of the base type.
    let resolveBaseTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo)
        : IlMachineState * DumpedAssembly * TypeDefn
        =
        match baseTypeInfo with
        | BaseTypeInfo.TypeDef handle ->
            let typeInfo = currentAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, currentAssembly, typeDefn
        | BaseTypeInfo.TypeRef handle ->
            let state, assy, resolved =
                resolveTypeFromRef
                    loggerFactory
                    currentAssembly
                    (currentAssembly.TypeRefs.[handle])
                    ImmutableArray.Empty
                    state

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved

            state, assy, typeDefn
        | BaseTypeInfo.ForeignAssemblyType (assemblyName, handle) ->
            let state, foreignAssembly =
                ensureAssemblyLoadedByName loggerFactory state currentAssembly assemblyName

            let typeInfo = foreignAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, foreignAssembly, typeDefn
        | BaseTypeInfo.TypeSpec handle ->
            let signature = currentAssembly.TypeSpecs.[handle].Signature
            state, currentAssembly, signature

    /// Given a ConcreteTypeHandle, resolve and return its base type as a ConcreteTypeHandle.
    /// Returns None for types without a base type (System.Object).
    let resolveBaseConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * ConcreteTypeHandle option
        =
        match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
        | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
        | Some ct ->
            let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
            let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

            match typeInfo.BaseType with
            | None -> state, None
            | Some baseTypeInfo ->
                let state, baseAssy, baseTypeDefn =
                    resolveBaseTypeInfo loggerFactory baseClassTypes state assy baseTypeInfo

                let state, baseHandle =
                    concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        baseAssy.Name
                        ct.Generics
                        ImmutableArray.Empty
                        baseTypeDefn

                state, Some baseHandle

    /// Collect ALL instance fields from the entire type hierarchy for a given ConcreteTypeHandle,
    /// walking from base to derived (base class fields appear first in the returned list).
    let rec collectAllInstanceFields
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * CliField list
        =
        let ct =
            AllConcreteTypes.lookup concreteType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"collectAllInstanceFields: ConcreteTypeHandle %O{concreteType} not found in AllConcreteTypes"
            )

        let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
        let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

        // Get this type's own instance fields
        let state, ownFields =
            let instanceFields =
                typeInfo.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

            ((state, []), instanceFields)
            ||> List.fold (fun (state, fields) field ->
                let state, zero, fieldTypeHandle =
                    cliTypeZeroOf
                        loggerFactory
                        baseClassTypes
                        assy
                        field.Signature
                        ct.Generics
                        ImmutableArray.Empty
                        state

                let cliField : CliField =
                    {
                        Name = field.Name
                        Contents = zero
                        Offset = field.Offset
                        Type = fieldTypeHandle
                    }

                state, cliField :: fields
            )

        let ownFields = List.rev ownFields

        // Recurse into base type
        let state, baseHandle =
            resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

        match baseHandle with
        | None -> state, ownFields
        | Some parentHandle ->
            let state, baseFields =
                collectAllInstanceFields loggerFactory baseClassTypes state parentHandle

            state, baseFields @ ownFields

    /// Allocate a new System.String managed object on the heap with the given contents.
    /// Does NOT intern the string: every call returns a fresh heap object.  The Ldstr opcode
    /// wraps this with its own interning cache (see UnaryStringTokenIlOp); runtime-generated
    /// strings (stack traces, type names, etc.) call this directly.
    let allocateManagedString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : string)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        // String type is:
        // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/libraries/System.Private.CoreLib/src/System/String.cs#L26
        let stringInstanceFields =
            baseClassTypes.String.Fields
            |> List.choose (fun field ->
                if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                    Some (field.Name, field.Signature)
                else
                    None
            )
            |> List.sortBy fst

        if
            stringInstanceFields
            <> [
                ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
            ]
        then
            failwith $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

        let dataAddr, state = allocateStringData contents.Length state
        let state = setStringData dataAddr contents state

        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.String
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let fields =
            [
                {
                    Name = "_firstChar"
                    Contents = CliType.ofChar state.ManagedHeap.StringArrayData.[dataAddr]
                    Offset = None
                    Type = AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Char
                }
                {
                    Name = "_stringLength"
                    Contents = CliType.Numeric (CliNumericType.Int32 contents.Length)
                    Offset = None
                    Type = AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32
                }
            ]
            |> CliValueType.OfFields baseClassTypes state.ConcreteTypes stringType Layout.Default

        let addr, state = allocateManagedObject stringType fields state

        let state =
            { state with
                ManagedHeap =
                    state.ManagedHeap
                    |> ManagedHeap.recordStringContents addr contents
                    |> ManagedHeap.recordStringDataOffset addr dataAddr
            }

        addr, state

    /// Return the managed `System.Threading.Thread` heap object corresponding to the given guest
    /// thread, allocating it on first request and caching the address thereafter so that repeated
    /// calls yield reference-identical objects. Populates only the fields whose zero-initialised
    /// defaults would observably diverge from the CLR: `_managedThreadId` (ThreadId 0 is
    /// hardcoded to managed ID 1; others consume `NextManagedThreadId`), `_priority` (CLR
    /// exposes `ThreadPriority.Normal = 2`, not zero-valued `Lowest`), and
    /// `_DONT_USE_InternalThread` (non-zero sentinel so `GetNativeHandle()` doesn't throw).
    /// The Thread constructor is NOT run; other fields remain zero-initialised.
    let getOrAllocateManagedThreadObject
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (threadId : ThreadId)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr -> addr, state
        | None ->

        let threadTypeInfo =
            baseClassTypes.Corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) ->
                if v.Namespace = "System.Threading" && v.Name = "Thread" then
                    Some v
                else
                    None
            )
            |> Seq.exactlyOne

        let state, threadTypeHandle =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies threadTypeInfo
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state threadTypeHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes threadTypeHandle threadTypeInfo.Layout allFields

        let addr, state = allocateManagedObject threadTypeHandle fields state

        // The main thread (ThreadId 0) always gets managed ID 1 — the CLR assigns it at
        // startup, before user code runs.  Other scheduler-created threads consume the shared
        // counter so IDs remain globally unique.
        let managedThreadId, state =
            let (ThreadId idx) = threadId

            if idx = 0 then
                1, state
            else
                let id = state.NextManagedThreadId

                id,
                { state with
                    NextManagedThreadId = id + 1
                }

        let threadPriorityNormal = 2
        let (ManagedHeapAddress addrInt) = addr

        let updatedObj =
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.SetField
                "_managedThreadId"
                (CliType.Numeric (CliNumericType.Int32 managedThreadId))
            |> AllocatedNonArrayObject.SetField
                "_priority"
                (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
            |> AllocatedNonArrayObject.SetField
                "_DONT_USE_InternalThread"
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                ManagedThreadObjects = state.ManagedThreadObjects |> Map.add threadId addr
            }

        addr, state

    /// Synthesize a TypeInitializationException wrapping the given inner exception object.
    /// Allocates the exception on the heap with zero-initialized fields (constructor is NOT run).
    /// Sets the _innerException, _typeName, and _HResult fields on the TIE to match what the
    /// TypeInitializationException(string, Exception) ctor would have done.
    /// Returns the heap address, the ConcreteTypeHandle, and the updated state.
    let synthesizeTypeInitializationException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeFullName : string)
        (innerExceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let tieTypeInfo = baseClassTypes.TypeInitializationException

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies tieTypeInfo

        let state, tieHandle =
            concretizeType
                loggerFactory
                baseClassTypes
                state
                tieTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (tieTypeInfo.Identity, stk))

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state tieHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes tieHandle tieTypeInfo.Layout allFields

        let addr, state = allocateManagedObject tieHandle fields state

        let typeNameAddr, state =
            allocateManagedString loggerFactory baseClassTypes typeFullName state

        // Set _innerException, _typeName and _HResult on the allocated TIE, matching what the
        // TypeInitializationException(string, Exception) ctor would have done.
        // See CLR's EEException::CreateThrowable:
        // https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let heapObj =
            heapObj
            |> AllocatedNonArrayObject.SetField "_innerException" (CliType.ObjectRef (Some innerExceptionAddr))
            |> AllocatedNonArrayObject.SetField "_typeName" (CliType.ObjectRef (Some typeNameAddr))
            |> AllocatedNonArrayObject.SetField
                "_HResult"
                (CliType.Numeric (CliNumericType.Int32 (ExceptionHResults.lookup "System.TypeInitializationException")))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
            }

        addr, tieHandle, state

    /// Resolve a MetadataToken (TypeDefinition, TypeReference, or TypeSpecification) to a TypeDefn,
    /// together with the assembly the type was resolved in.
    let resolveTypeMetadataToken
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (token : MetadataToken)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        match token with
        | MetadataToken.TypeDefinition h ->
            let state, ty = lookupTypeDefn baseClassTypes state activeAssy h
            state, ty, activeAssy
        | MetadataToken.TypeReference ref ->
            lookupTypeRef loggerFactory baseClassTypes state activeAssy typeGenerics ref
        | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
        | m -> failwith $"unexpected type metadata token {m}"

    /// Check whether the concrete type `objType` is assignable to `targetType`.
    /// Walks the base type chain and checks implemented interfaces at each level.
    /// Returns true if objType = targetType, or targetType is a base class of objType,
    /// or targetType is an interface implemented by objType or any of its base classes.
    let isConcreteTypeAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if objType = targetType then
            state, true
        else

        let rec checkInterfaces (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match AllConcreteTypes.lookup current state.ConcreteTypes with
            | None ->
                // Byref/pointer handles are not in AllConcreteTypes; they have no interfaces.
                state, false
            | Some ct ->

            let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
            let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

            ((state, false), typeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun (state, found) impl ->
                if found then
                    state, true
                else
                    let implAssy =
                        match state.LoadedAssembly impl.RelativeToAssembly with
                        | Some a -> a
                        | None ->
                            // Assembly not yet loaded; use the assembly we already have since
                            // RelativeToAssembly is set to the assembly containing the type definition.
                            assy

                    let state, implTypeDefn, implResolvedAssy =
                        resolveTypeMetadataToken
                            loggerFactory
                            baseClassTypes
                            state
                            implAssy
                            ct.Generics
                            impl.InterfaceHandle

                    let state, implHandle =
                        concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            implResolvedAssy.Name
                            ct.Generics
                            ImmutableArray.Empty
                            implTypeDefn

                    // Check exact match, then recurse into the interface's own parent interfaces
                    walk state implHandle
            )

        and walk (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            if current = targetType then
                state, true
            else

            match AllConcreteTypes.lookup current state.ConcreteTypes with
            | None ->
                // Byref/pointer handles are not in AllConcreteTypes; no inheritance or interfaces.
                state, false
            | Some currentCt ->

            // If two types share the same definition but differ in generics, check whether
            // variance could apply. Classes are invariant so the answer is definitively false.
            // Interfaces and delegates can have variance, so we must crash rather than guess.
            let sameDefnDifferentGenerics =
                match AllConcreteTypes.lookup targetType state.ConcreteTypes with
                | Some targetCt when
                    currentCt.Identity = targetCt.Identity
                    && currentCt.Generics <> targetCt.Generics
                    ->
                    Some targetCt
                | _ -> None

            match sameDefnDifferentGenerics with
            | Some targetCt ->
                let targetAssy = state._LoadedAssemblies.[targetCt.Identity.AssemblyFullName]
                let targetTypeInfo = targetAssy.TypeDefs.[targetCt.Identity.TypeDefinition.Get]

                let hasVariantGenericParams =
                    targetTypeInfo.Generics
                    |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

                if hasVariantGenericParams then
                    failwith $"TODO: generic variance check needed: is %O{currentCt} assignable to %O{targetCt}?"
                else
                    // All generic parameters are invariant; same definition + different generics = not assignable.
                    state, false
            | None ->

            let state, interfaceMatch = checkInterfaces state current

            if interfaceMatch then
                state, true
            else
                let state, baseType =
                    resolveBaseConcreteType loggerFactory baseClassTypes state current

                match baseType with
                | None ->
                    // Every reference type (including interfaces) is assignable to System.Object
                    match targetType with
                    | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, true
                    | _ -> state, false
                | Some parent -> walk state parent

        walk state objType
