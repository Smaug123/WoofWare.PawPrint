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
        /// Round-robin cursor for `EmulatedKernel.cpuForRotation`: the number
        /// of *guest-visible* threads created so far, and hence the rotation
        /// index the next one will be placed by.
        ///
        /// Deliberately separate from `NextThreadId`, which is also consumed by
        /// PawPrint-internal auxiliary threads that never run guest IL
        /// (`allocateParkedThread`). Keying placement off `NextThreadId` would
        /// let an interpreter-internal thread allocation shift which core every
        /// subsequently created guest thread lands on, leaking an interpreter
        /// detail into guest-observable state. `allocateParkedThread` therefore
        /// leaves this cursor alone.
        ///
        /// Advanced when a thread is *created*, not when it is started: a guest
        /// that constructs a `Thread` and never calls `Start` still consumes a
        /// rotation slot, mirroring real .NET's eager `ManagedThreadId`
        /// assignment in the constructor.
        NextCpuRotation : int
        // CallStack : StackFrame list
        /// Multiple managed heaps are allowed, but we hopefully only need one.
        ManagedHeap : ManagedHeap
        ThreadState : Map<ThreadId, ThreadState>
        InternedStrings : ImmutableDictionary<string, ManagedHeapAddress>
        /// The assemblies we have loaded, keyed by their own AssemblyDefinition identity, plus the
        /// record of which AssemblyReferences have been bound to which of them. An assembly's
        /// reference identity routinely differs from its definition identity (the .NET Framework
        /// compatibility facades reference implementation assemblies as `Version=0.0.0.0`), so the
        /// two must not be conflated; see `LoadedAssemblies`.
        _LoadedAssemblies : LoadedAssemblies
        /// Tracks initialization state of types across assemblies
        TypeInitTable : TypeInitTable
        /// For each static-storage owner, then for each concrete type, a map of field definition
        /// handle to static value.
        /// The FieldDefinitionHandle is scoped to the assembly that defines the outer ConcreteTypeHandle's type;
        /// do not mix handles from different assemblies under the same key.
        /// An ordinary static lives under `StaticOwner.Shared`; a `[ThreadStatic]` field lives
        /// under `StaticOwner.OwnedBy` of each thread that has touched it, and a thread that has
        /// not touched it simply misses, which is how zero-initialisation stays lazy.
        _Statics :
            ImmutableDictionary<
                StaticOwner,
                ImmutableDictionary<ConcreteTypeHandle, Map<ComparableFieldDefinitionHandle, CliType>>
             >
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
        /// Deterministic counter-assignment state for synthesised pointer
        /// hash bits. Each canonical pointer key gets a stable bit pattern
        /// derived from its registration order; distinct keys produce
        /// distinct bits with no collisions. See `PointerHashSynthesis`.
        PointerHashCounters : PointerHashCounters
        /// Host-kernel / syscall-emulation state: last-error registers, native
        /// heap pool, file-descriptor table, `LowLevelMonitor` registry, and
        /// monotonic ID counters for opaque kernel handles. Bundled into a
        /// sub-record because the rest of `IlMachineState` models the CIL
        /// execution layer, not the kernel surface PawPrint refuses to use.
        Kernel : EmulatedKernel
        /// Scheduling policy state. `RoundRobin` reproduces the legacy
        /// deterministic ordering and is the default for runs that don't
        /// request fuzzing; `Pct _` carries the live priority assignment
        /// and splitmix64 RNG state for Probabilistic Concurrency Testing.
        /// Lives here (rather than on `EmulatedKernel`) because the scheduler
        /// is an interpreter-level concern — it isn't a syscall the guest
        /// can observe or perturb.
        Scheduling : SchedulerState
    }

    member this.WithKernel (kernel : EmulatedKernel) : IlMachineState =
        { this with
            Kernel = kernel
        }

    member this.MapKernel (f : EmulatedKernel -> EmulatedKernel) : IlMachineState =
        { this with
            Kernel = f this.Kernel
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

    /// Register an assembly under its own definition identity. Idempotent: if an assembly with
    /// that identity is already loaded, the existing instance is kept.
    member this.WithLoadedAssembly (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.WithLoadedAssembly value
        }

    member this.LoadedAssembly' (fullName : string) : DumpedAssembly option =
        this._LoadedAssemblies.TryByDefinitionName fullName

    member this.LoadedAssembly (name : AssemblyName) : DumpedAssembly option =
        this._LoadedAssemblies.TryByDefinition name

    member this.ActiveAssembly (thread : ThreadId) =
        let active = this.ThreadState.[thread].ActiveAssembly

        match this.LoadedAssembly active with
        | Some v -> v
        | None ->
            let available = this._LoadedAssemblies.DefinitionNames |> String.concat " ; "

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
    /// A native handler has set up a managed call as a continuation: it pushed a managed callee
    /// frame on top of itself and now needs the dispatch loop to run that callee before returning
    /// to the handler. The active frame is the new managed callee; the native handler frame
    /// remains on the stack and will become active again when the callee returns. The native
    /// handler will then be re-entered by the dispatch loop and is responsible for distinguishing
    /// first entry from re-entry. This is the same shape as `SuspendedForClassInit` but
    /// generalised for arbitrary managed continuations (e.g. invoking a default ctor on a
    /// freshly-allocated object inside a QCall).
    | SuspendedForManagedCall
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId
    /// A TypeInitializationException was thrown into the guest because a .cctor previously failed.
    /// The state has already been updated with exception dispatch (handler search and frame unwinding).
    | ThrowingTypeInitializationException
    /// The thread completed a step (no frame change, no suspension) and the guest explicitly
    /// requested that the scheduler consider running another thread before resuming this one
    /// (e.g. `Thread.Yield()`). Under today's scheduling policy this has the same observable
    /// effect as `Executed`: `Scheduler.chooseNext`'s signature is `(lastRan, state)` and does
    /// not consume the previous outcome, so the round-robin policy is hint-insensitive. The
    /// variant exists so a future fuzz/pruning policy at the driver/scheduler boundary can
    /// distinguish voluntary yields from forced context switches (e.g. for coverage weighting,
    /// or to constrain the next-thread choice). Surfacing the hint to `chooseNext` itself is
    /// a separate later change — either widen its signature, or carry the last outcome on
    /// the state — neither of which this variant alone prescribes.
    | VoluntaryYield

/// An externally-observable side-effect that a single interpreter step requests
/// from the driver (the imperative shell around the functional core). The
/// interpreter never performs host I/O directly; it emits a data description
/// of the effect alongside the new machine state, and the shell decides what
/// (if anything) to do with it.
///
/// `StepEffect` is orthogonal to `WhatWeDid`: `WhatWeDid` answers "did this
/// step make progress, suspend, or block?" — i.e. it's input to the
/// scheduler. `StepEffect` answers "did this step want to talk to the outside
/// world?" — i.e. it's input to the driver. A single step can do both (e.g.
/// emit a `WroteToFd` effect *and* report `WhatWeDid.Executed`).
///
/// Today the variants are `NoEffect` and `WroteToFd`; widening the contract
/// up front lets us add `ReadFromFd` etc. in subsequent PRs without retouching
/// the construction sites that don't emit effects.
type StepEffect =
    /// The step had no externally-observable I/O effect. The overwhelming
    /// majority of IL steps land here; the variant exists so that effect-
    /// emitting handlers (`SystemNative_Write`, etc.) are distinguishable at
    /// the type level.
    | NoEffect
    /// The step accepted `bytes` for the file descriptor backing `role`, and
    /// the bytes have already been appended as a single entry to the
    /// canonical `EmulatedKernel.OutputLog` (so the state alone is
    /// sufficient to reconstruct the full output, in cross-stream order).
    /// Drivers that want to stream output as it is produced — instead of
    /// waiting until the end of the run and reading the log — should
    /// consume this variant. The `role` is one of the standard-stream
    /// roles; PawPrint does not currently model any other writable fd.
    /// `bytes` carries exactly the bytes appended in this step (it is
    /// not the cumulative log); a driver that streams therefore does not
    /// need to track an offset.
    | WroteToFd of role : FileDescriptorRole * bytes : ImmutableArray<byte>

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
    /// Environment.FailFast was called on `abortingThread`. Like ProcessExit, the process
    /// terminates immediately; unlike ProcessExit, this represents an abort (no exit code
    /// on the stack, no clean-shutdown semantics). `message` is the guest-supplied diagnostic
    /// string (if any). Distinct from ProcessExit so test harnesses can assert the difference,
    /// and so callers can surface the abort to the host (logs, non-zero exit) rather than
    /// reporting a clean exit.
    | FailFast of IlMachineState * abortingThread : ThreadId * message : string option
    /// A non-cancelled signal handler reached the kernel-default
    /// `Terminate` disposition, so the simulated process exits with the
    /// signal's identity. Mirrors `pal_signal.c`'s
    /// `SystemNative_HandleNonCanceledPosixSignal` Terminate branch,
    /// where the native code restores the original `sigaction` and calls
    /// `kill(g_pid, signalCode)` to let the kernel terminate the process
    /// with the signal-default exit status (POSIX convention: exit code
    /// `128 + signo`). Carries no `ThreadId` because POSIX
    /// `kill(pid, sig)` is process-global; there is no single owning
    /// thread for this termination. The App layer derives the exit code
    /// from `Signal.toLinuxSigno`.
    | SignalTerminated of IlMachineState * signal : Signal
    | Stepped of IlMachineState * WhatWeDid * StepEffect
    | UnhandledException of
        IlMachineState *
        terminatingThread : ThreadId *
        CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

/// Outcome of invoking a hand-written JIT intrinsic (`Intrinsics.call`). This is the
/// intrinsic analogue of `NativeHandlerResult` below, and exists for the same reason:
/// an intrinsic must be able to say "raise a guest exception" without being able to
/// perform the raise itself.
type IntrinsicResult =
    /// There is no hand-written implementation for this intrinsic key. The caller reports
    /// this as an unimplemented intrinsic; it is NOT an instruction to interpret the
    /// method's own IL (that decision is `Intrinsics.isSafeIntrinsic`, taken earlier).
    | Unrecognised
    /// The intrinsic ran to completion, and has already advanced the caller's program
    /// counter itself.
    | Completed of IlMachineState
    /// The intrinsic wants `exnType` raised at the current instruction. The intrinsic must
    /// NOT have advanced the program counter: exception dispatch keys both the handler
    /// search and the stack trace on the faulting instruction's offset. `exnType` must be a
    /// non-generic BCL exception type with a parameterless constructor — see
    /// `IlMachineStateExecution.raiseRuntimeException`, which the caller invokes on the
    /// intrinsic's behalf.
    ///
    /// `message` names the string the CLR would have passed to a message-taking ctor
    /// overload; `None` accepts the parameterless ctor's default resource string, which is
    /// correct wherever the CLR itself throws with no argument.
    | RaiseException of
        IlMachineState *
        exnType : TypeInfo<GenericParamFromMetadata, TypeDefn> *
        message : string option

/// Outcome of invoking a native handler (QCall, P/Invoke shim, or other host-provided
/// primitive registered under `WoofWare.PawPrint.Native`). Each variant names a single
/// dispatcher decision, so the dispatcher's pattern match is total and the convention
/// "remember to set the right `WhatWeDid`" that the legacy `ExecutionResult` shape relied
/// on is eliminated.
///
/// The native dispatcher (`AbstractMachine.executeOneStep.dispatchNative`) translates each
/// variant into an `ExecutionResult`, performing frame-management on the handler's behalf
/// (popping the native frame when it has finished, or leaving it on the stack so exception
/// dispatch / re-entry / cctor unwinding can walk through it).
type NativeHandlerResult =
    /// Native handler ran to completion. Dispatcher pops the native frame and reports
    /// `WhatWeDid.Executed` to the Scheduler.
    | Completed of IlMachineState * StepEffect
    /// Native handler ran to completion AND explicitly requested a scheduler yield (the
    /// canonical caller is `Thread.Yield()` / `ThreadNative_YieldThread`). Dispatcher pops
    /// the native frame and reports `WhatWeDid.VoluntaryYield` to the Scheduler. Semantically
    /// equivalent to `Completed` for frame-management purposes; distinct so the guest's
    /// yield intent reaches the scheduler boundary unmangled — see `WhatWeDid.VoluntaryYield`
    /// for the longer-term motivation.
    | Yielded of IlMachineState * StepEffect
    /// Native handler synchronously pushed a managed callee on top of itself for re-entry:
    /// the handler will be invoked again after the callee returns, typically via re-entry
    /// markers placed on the eval stack so the handler can distinguish first entry from
    /// resumption. Dispatcher leaves the native frame on the stack and reports
    /// `WhatWeDid.SuspendedForManagedCall` to the Scheduler.
    | PushedManagedCallee of IlMachineState * StepEffect
    /// Native handler is raising a runtime exception. The dispatcher invokes
    /// `IlMachineStateExecution.raiseRuntimeException` for the supplied exception type
    /// (which must be a non-generic BCL exception with a parameterless ctor), allocating
    /// the object, calling its ctor, and arming dispatch-on-return. The native frame stays
    /// on the stack so exception dispatch can unwind it on the ctor's `Ret`; the handler
    /// is never re-entered. Reports `WhatWeDid.SuspendedForManagedCall` to the Scheduler.
    | RaiseException of IlMachineState * exnType : TypeInfo<GenericParamFromMetadata, TypeDefn> * StepEffect
    /// A type's `.cctor` has been pushed on top of the native frame (typically because a
    /// sub-call into managed code needed to initialise an uninitialised type). Dispatcher
    /// leaves the native frame on the stack until the `.cctor` completes, then re-enters
    /// the handler. Reports `WhatWeDid.SuspendedForClassInit` to the Scheduler.
    | SuspendedForClassInit of IlMachineState * StepEffect
    /// Another thread owns the `.cctor` lock for a type the handler needs initialised;
    /// this thread yields. Dispatcher leaves the native frame on the stack so the handler
    /// can be re-entered when the lock is released. Reports `WhatWeDid.BlockedOnClassInit`
    /// to the Scheduler.
    | BlockedOnClassInit of IlMachineState * blockedBy : ThreadId * StepEffect
    /// A sub-call's exception (typically a `TypeInitializationException` raised by a
    /// previously-failed `.cctor`) has already been dispatched into the guest and unwound
    /// past this native frame to a matching handler. The state already reflects the
    /// unwind; dispatcher leaves it alone. Reports
    /// `WhatWeDid.ThrowingTypeInitializationException` to the Scheduler.
    | ThrowingTypeInitializationException of IlMachineState * StepEffect
    /// The handler produced a terminating `ExecutionResult` (one of `Terminated`,
    /// `ProcessExit`, `FailFast`, or `UnhandledException`) that the dispatcher should
    /// surface to the run loop verbatim, bypassing native-frame management. This variant
    /// arises from handlers whose method tears the simulated process down rather than
    /// returning to the guest — `Environment._Exit` and `Environment.FailFast` are the
    /// canonical cases. The embedded `ExecutionResult` must never be a `Stepped` value;
    /// `NativeHandlerResult.ofExecutionResult` enforces that when constructing one from
    /// an arbitrary `ExecutionResult`, routing `Stepped(Executed)` to `Completed` and
    /// rejecting other `Stepped` shapes as logic errors.
    | Terminating of ExecutionResult

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
    /// SetHResult(GetHR()) step, and then, when `message` is `Some`, overwrite `_message`
    /// with it (see `ConstructedObjectDisposition.DispatchAsException` for why that has to
    /// happen after the ctor rather than before it).
    | DispatchException of
        IlMachineState *
        exceptionAddr : ManagedHeapAddress *
        exceptionType : ConcreteTypeHandle *
        message : string option

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
    /// A thread called `Environment.FailFast`. The process aborts immediately; the
    /// abort message (if any) is surfaced for diagnostics. Distinct from ProcessExit
    /// because FailFast is semantically an abort, not a clean exit — finalizers do not
    /// run on real CoreCLR, and the host typically reports a non-zero/abort exit.
    | FailFast of IlMachineState * abortingThread : ThreadId * message : string option
    /// The simulation was terminated by a POSIX signal whose
    /// registered handler(s) did not cancel the default disposition,
    /// and whose kernel default is `Terminate`. Carries the originating
    /// `Signal` so the host can compute the POSIX-conventional exit
    /// code `128 + Signal.toLinuxSigno signal` and surface the cause
    /// for diagnostics. No `ThreadId` because process-level signal
    /// termination is not attributable to a single thread (the real
    /// native code calls `kill(g_pid, signalCode)`, which tears down
    /// the whole process).
    | SignalTerminated of IlMachineState * signal : Signal
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
    /// Another thread is currently running the type's .cctor. The current thread must park on
    /// `BlockedOnClassInit blockedBy` until that thread completes initialisation (or its cctor
    /// fails, at which point the parked thread is woken to observe the cached
    /// TypeInitializationException). The state is unchanged from the caller's perspective: in
    /// particular the caller's program counter must not advance, so the opcode is retried on
    /// wake-up.
    | Blocked of IlMachineState * blockedBy : ThreadId

[<RequireQualifiedAccess>]
module ExecutionResult =
    /// Construct a `Stepped` result that emits no externally-observable effect.
    /// Use this for the overwhelming majority of step outcomes; reserve
    /// `steppedWith` for handlers that genuinely want the driver to do
    /// something (e.g. `SystemNative_Write` emitting `StepEffect.WroteToFd`
    /// once that variant exists).
    let stepped ((state, whatWeDid) : IlMachineState * WhatWeDid) : ExecutionResult =
        ExecutionResult.Stepped (state, whatWeDid, StepEffect.NoEffect)

    /// Construct a `Stepped` result that emits the supplied effect. Argument
    /// order matches `stepped` so existing pipelines can swap one for the
    /// other by inserting `|> steppedWith effect` in place of `|> stepped`.
    let steppedWith (effect : StepEffect) ((state, whatWeDid) : IlMachineState * WhatWeDid) : ExecutionResult =
        ExecutionResult.Stepped (state, whatWeDid, effect)

[<RequireQualifiedAccess>]
module NativeHandlerResult =
    /// Native handler completed normally with no externally-observable effect.
    /// The default for the overwhelming majority of native handlers.
    let completed (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.Completed (state, StepEffect.NoEffect)

    /// Native handler completed normally and emitted `effect`. Use this for handlers
    /// that performed an externally-observable side effect (e.g. `SystemNative_Write`).
    let completedWith (effect : StepEffect) (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.Completed (state, effect)

    /// Native handler completed normally AND requested a scheduler yield. Use this only
    /// for genuine yield primitives (today: `ThreadNative_YieldThread`). The dispatcher
    /// pops the native frame and reports `WhatWeDid.VoluntaryYield`. Handlers that simply
    /// finish — even ones that touched shared state — should use `completed`; the yield
    /// signal is reserved for the guest-requested hint, not derived from the handler's
    /// side-effects.
    let yielded (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.Yielded (state, StepEffect.NoEffect)

    /// Native handler pushed a managed callee on top of itself for re-entry. The
    /// handler will be re-invoked on a future step (typically distinguishing first
    /// entry from re-entry via markers placed on the eval stack).
    let pushedManagedCallee (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.PushedManagedCallee (state, StepEffect.NoEffect)

    /// Native handler is raising the given exception type. The dispatcher allocates
    /// the exception, calls its parameterless ctor, arms dispatch-on-return, and
    /// leaves the native frame on the stack so exception dispatch can unwind it.
    /// The exception type must be a non-generic BCL exception (typically a field on
    /// `BaseClassTypes`); use this for runtime-synthesised exceptions only, not for
    /// guest-thrown exceptions (which go through `newobj` + `throw`).
    let raiseException
        (exnType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : NativeHandlerResult
        =
        NativeHandlerResult.RaiseException (state, exnType, StepEffect.NoEffect)

    /// Forward a `WhatWeDid.SuspendedForClassInit` outcome from a sub-call. Use this
    /// at the leaf of a passthrough branch when the dispatcher should keep the native
    /// frame on the stack while a `.cctor` runs.
    let suspendedForClassInit (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.SuspendedForClassInit (state, StepEffect.NoEffect)

    /// Forward a `WhatWeDid.BlockedOnClassInit` outcome from a sub-call. Use this
    /// at the leaf of a passthrough branch when another thread owns the cctor lock.
    let blockedOnClassInit (blockedBy : ThreadId) (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.BlockedOnClassInit (state, blockedBy, StepEffect.NoEffect)

    /// Forward a `WhatWeDid.ThrowingTypeInitializationException` outcome from a sub-call.
    /// Use this at the leaf of a passthrough branch when an exception has already been
    /// dispatched past this native frame.
    let throwingTypeInitializationException (state : IlMachineState) : NativeHandlerResult =
        NativeHandlerResult.ThrowingTypeInitializationException (state, StepEffect.NoEffect)

    /// Translate the outcome of a managed sub-call (e.g. `ensureTypeInitialised`,
    /// `callMethod`) into a `NativeHandlerResult` the native handler can return early
    /// with. Returns `Some` when the sub-call's outcome means the native handler must
    /// stop work and propagate (cctor pending, blocked on another thread, exception
    /// already dispatched). Returns `None` when the sub-call ran to completion
    /// (`WhatWeDid.Executed`) so the handler should continue.
    ///
    /// `WhatWeDid.SuspendedForManagedCall` is rejected as a logic error: that variant
    /// is produced only by native handlers themselves pushing a managed callee, never
    /// by managed sub-calls returning to a native handler.
    let tryEarlyReturn ((state, whatWeDid) : IlMachineState * WhatWeDid) : NativeHandlerResult option =
        match whatWeDid with
        | WhatWeDid.Executed -> None
        // A sub-call that voluntarily yielded did make forward progress, so the
        // calling native handler should continue exactly as for Executed. The yield
        // hint is meaningful only at the dispatcher/scheduler boundary; it does not
        // bubble up as a sub-call control-flow signal because the handler's own
        // outcome is what the dispatcher records, and the handler can decide for
        // itself whether to yield (via `NativeHandlerResult.yielded`) when it returns.
        | WhatWeDid.VoluntaryYield -> None
        | WhatWeDid.SuspendedForClassInit -> Some (suspendedForClassInit state)
        | WhatWeDid.BlockedOnClassInit blockedBy -> Some (blockedOnClassInit blockedBy state)
        | WhatWeDid.ThrowingTypeInitializationException -> Some (throwingTypeInitializationException state)
        | WhatWeDid.SuspendedForManagedCall ->
            failwith
                "logic error: managed sub-call produced SuspendedForManagedCall; that variant is only valid as a native handler's own outcome"

    /// Translate an `ExecutionResult` produced by an ExternImpl (`ISystem_Environment`,
    /// `System_Threading_Monitor`, etc.) into a `NativeHandlerResult` suitable for return
    /// from a native handler. ExternImpls produce a constrained subset of `ExecutionResult`
    /// values:
    ///
    /// * `Stepped(state, WhatWeDid.Executed, effect)` — the handler ran a normal step;
    ///   routed to `Completed(state, effect)` so the dispatcher pops the native frame.
    /// * `Terminated`, `ProcessExit`, `FailFast`, `SignalTerminated`,
    ///   `UnhandledException` — terminating outcomes wrapped in
    ///   `NativeHandlerResult.Terminating` so the dispatcher surfaces
    ///   them verbatim to the run loop, bypassing frame management.
    ///
    /// Any `Stepped` value with a `WhatWeDid` other than `Executed` is rejected as a logic
    /// error: ExternImpls are not authorised to drive cctor / managed-call / exception
    /// re-entry directly, because those decisions require structural support (re-entry
    /// markers, `ConstructedObjectDisposition.DispatchAsException` arming) that ExternImpls don't
    /// have access to.
    /// `VoluntaryYield` is rejected here for the same structural-boundary reason: the yield
    /// signal is produced at the native-handler return shape via `NativeHandlerResult.Yielded`,
    /// not threaded back through `ExecutionResult`. If an ExternImpl ever needs to yield, the
    /// right answer is a dedicated `NativeHandlerResult.yielded` at the native handler that
    /// called it. Such cases should instead use the dedicated `NativeHandlerResult`
    /// constructors from the native handler that called the ExternImpl.
    let ofExecutionResult (executionResult : ExecutionResult) : NativeHandlerResult =
        match executionResult with
        | ExecutionResult.Stepped (state, WhatWeDid.Executed, effect) -> NativeHandlerResult.Completed (state, effect)
        | ExecutionResult.Stepped (_, other, _) ->
            failwith
                $"logic error: ExternImpl produced Stepped with WhatWeDid=%A{other}; ExternImpls may only produce Executed steps or terminating outcomes. Use a dedicated NativeHandlerResult constructor from the native handler instead."
        | ExecutionResult.Terminated _
        | ExecutionResult.ProcessExit _
        | ExecutionResult.FailFast _
        | ExecutionResult.SignalTerminated _
        | ExecutionResult.UnhandledException _ -> NativeHandlerResult.Terminating executionResult
