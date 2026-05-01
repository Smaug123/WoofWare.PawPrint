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
        InternedStrings : ImmutableDictionary<string, ManagedHeapAddress>
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
        GcHandles : GcHandleRegistry
        FieldHandles : FieldHandleRegistry
        MethodHandles : MethodHandleRegistry
        /// Deterministic virtual hardware capability profile. This is deliberately
        /// not derived from the host CPU running PawPrint.
        HardwareIntrinsics : HardwareIntrinsicsProfile
        /// Deterministic guest debugger attachment state. This is deliberately not
        /// derived from whether a debugger is attached to the PawPrint host process.
        Debugger : DebuggerState
        /// Cache of RuntimeAssembly heap objects keyed by assembly full name, so that
        /// two types from the same assembly return the same Assembly object (reference identity).
        RuntimeAssemblyObjects : ImmutableDictionary<string, ManagedHeapAddress>
        /// Cache of RuntimeModule heap objects keyed by assembly full name. PawPrint currently
        /// models one loaded PE module per DumpedAssembly.
        RuntimeModuleObjects : ImmutableDictionary<string, ManagedHeapAddress>
        /// Cache of managed `System.Threading.Thread` heap objects, one per ThreadId, so that
        /// `Thread.CurrentThread` returns a reference-identical object on repeated access from
        /// the same guest thread.
        ManagedThreadObjects : Map<ThreadId, ManagedHeapAddress>
        /// Next managed thread ID to assign. Consumed by `Thread.Initialize()` (user-created
        /// threads) and by `getOrAllocateManagedThreadObject` for non-main scheduler-created
        /// threads.  Starts at 2 because ID 0 is the CLR's "no managed thread" sentinel and
        /// ID 1 is reserved for the main thread (ThreadId 0).
        NextManagedThreadId : int
        /// Last error reported by a modelled P/Invoke with SetLastError=true.
        /// This is currently process-wide; model it per-thread when a guest
        /// depends on thread-local last-error state.
        LastPInvokeError : int
        /// Last system error tracked separately from LastPInvokeError because
        /// CoreLib wrappers can read this and then write LastPInvokeError.
        /// This is currently process-wide; model it per-thread when a guest
        /// depends on thread-local GetLastError or errno state.
        LastSystemError : int
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
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IlMachineState =
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
                GcHandles = GcHandleRegistry.empty ()
                FieldHandles = FieldHandleRegistry.empty ()
                MethodHandles = MethodHandleRegistry.empty ()
                HardwareIntrinsics = HardwareIntrinsicsProfile.ScalarOnly
                Debugger = DebuggerState.Detached
                RuntimeAssemblyObjects = ImmutableDictionary.Empty
                RuntimeModuleObjects = ImmutableDictionary.Empty
                ManagedThreadObjects = Map.empty
                NextManagedThreadId = 2
                LastPInvokeError = 0
                LastSystemError = 0
            }

        state.WithLoadedAssembly assyName entryAssembly
