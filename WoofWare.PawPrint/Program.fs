namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open WoofWare.PosixKernel

[<RequireQualifiedAccess>]
module Program =
    /// A program ready to run `Main`, or running it: an opaque handle on the driver
    /// (`MultiProgram`) of the one program, which owns the machine the program's process runs
    /// on. `stepPrepared` steps it.
    [<Struct>]
    type PreparedProgram =
        internal
            {
                Driver : MultiProgram
            }

        /// The program's interpreter state as it currently stands.
        member this.State : IlMachineState = this.Driver.Current.State

        /// The base class types of the CoreLib the program runs against.
        member this.BaseClassTypes : BaseClassTypes<DumpedAssembly> =
            this.Driver.Current.BaseClassTypes

        /// The thread `Main` runs on.
        member this.EntryThread : ThreadId = this.Driver.Current.EntryThread

        /// The thread that retired the program's most recent step, which the scheduler's
        /// round-robin choice starts from.
        member this.LastRan : ThreadId = this.Driver.Current.LastRan

        /// The program with `state` in place of its interpreter state. `state`'s view of the
        /// machine must descend from `this.State`'s, as any state a step or a syscall made from
        /// it does: the driver writes it back into the machine the program runs on, and fails
        /// loudly at the program's end if it cannot.
        member this.WithState (state : IlMachineState) : PreparedProgram =
            {
                Driver =
                    MultiProgram.withCurrent
                        { this.Driver.Current with
                            State = state
                        }
                        this.Driver
            }

    type ProgramStartResult =
        | Ready of PreparedProgram
        | CompletedBeforeMain of RunEnd

    type ProgramStepOutcome =
        /// `effect` is the step's `StepEffect`, forwarded verbatim from
        /// `ExecutionResult.Stepped`. It is what makes a *streaming* driver
        /// possible: `StepEffect.WroteToFd` carries exactly the bytes this step
        /// appended to `EmulatedKernel.OutputLog`, so a driver can write them to
        /// a real stream as they are produced instead of waiting for a
        /// `RunOutcome` and draining the log. A run that never produces a
        /// `RunOutcome` (a livelocked guest, a guest killed from outside,
        /// `Deadlocked`) has no end-of-run drain to reach, so without streaming
        /// its output is lost entirely.
        ///
        /// Steps that terminate the run do not carry an effect: those outcomes
        /// are `Completed`, and their `RunOutcome` carries the final state whose
        /// `OutputLog` is authoritative. A driver that streams should still drain
        /// any log entries beyond what it has written when the run ends, because
        /// writes performed *before* the driver's own loop starts (a `.cctor`
        /// that prints, pumped inside `prepare`) never pass through here.
        | InstructionStepped of PreparedProgram * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
        | WorkerTerminated of PreparedProgram * terminatingThread : ThreadId
        | Completed of RunOutcome
        | Deadlocked of PreparedProgram * stuckThreads : string

    /// Where a `Startup` has got to, together with whatever that phase needs to hand on.
    ///
    /// Startup runs guest code up to three times before `Main`, and the runs are not
    /// interchangeable. They are in the order CoreCLR runs them: the AppContext seed is
    /// `CorHost2::CreateAppDomainWithManager`, and the command line is
    /// `CorHost2::ExecuteAssembly` calling `SetCommandLineArgs` immediately before
    /// `ExecuteMainMethod` — which is what triggers the entry type's `.cctor`. Both deadlines
    /// bite: BCL feature switches latch into `static readonly` fields on first read, and a
    /// `.cctor` may call `Environment.GetCommandLineArgs` itself.
    ///
    /// Modelled as a DU carrying each phase's own data so the phases cannot drift apart —
    /// there is no way to be initialising classes without `Main`'s arguments in hand, nor to
    /// be pumping a call without knowing what to do when it returns. Each pumped phase names
    /// its successor rather than always yielding to class initialisation, so inserting or
    /// skipping one is a local change.
    type private StartupPhase =
        /// Pumping `AppContext.Setup`.
        | SeedingAppContext of onReturn : (IlMachineState -> IlMachineState * StartupPhase)
        /// Pumping `Environment.InitializeCommandLineArgs`, whose return value is the array
        /// `Main` must receive.
        | InitialisingCommandLine of onReturn : (IlMachineState -> IlMachineState * StartupPhase)
        /// Pumping class initialisers, the entry type's included, with `Main`'s arguments
        /// already in hand.
        | InitialisingClasses of mainArgs : ImmutableArray<CliType>

    /// Startup in progress. Holds the driver of the program, as a `PreparedProgram` does, so
    /// the same scheduler tick drives startup as drives `Main`, plus what remains to be done at
    /// each phase boundary.
    ///
    /// This exists so a driver can *step* startup rather than having it run to completion
    /// behind a single call. Guest code runs here — a static initialiser may print, block, or
    /// wedge — and a driver that cannot see those steps cannot stream their output or report
    /// where startup got stuck.
    ///
    /// The phase transitions are closures. They capture concretization results (a concretized
    /// `Main`, the entry type's handle) whose inspectable form would be no more use to a caller
    /// than the functions that consume them, and hoisting them to module scope would mean
    /// threading ten parameters through for no gain in reasoning. What a caller *can* see —
    /// the machine state, and which outcome a step produced — is data.
    type Startup =
        private
            {
                Driver : MultiProgram
                Phase : StartupPhase
                /// Installs the `Main` frame once class initialisation has returned.
                InstallMain : MultiProgram -> ImmutableArray<CliType> -> ProgramStartResult
            }

        /// The machine state as it currently stands. A driver streaming guest output reads
        /// `Kernel.OutputLog` from here when startup ends without a `ProgramStartResult`.
        member this.State : IlMachineState = this.Driver.Current.State

    /// The result of stepping startup once. Mirrors `ProgramStepOutcome`, and for the same
    /// reason carries the step's `StepEffect`: a driver consumes it to stream guest writes as
    /// they happen, which is the whole point of startup being steppable.
    [<RequireQualifiedAccess>]
    type StartupStepOutcome =
        | Stepped of Startup * ranThread : ThreadId * whatWeDid : WhatWeDid * effect : StepEffect
        | WorkerTerminated of Startup * terminatingThread : ThreadId
        /// The entry thread's frame returned and startup moved to its next phase. No guest
        /// instruction retired, so there is no effect to report.
        | PhaseAdvanced of Startup
        | Completed of ProgramStartResult
        | Deadlocked of Startup * stuckThreads : string

    /// Advance the machine by one scheduler tick (`MultiProgram.step`).
    ///
    /// What the entry thread's bottom frame returning means depends on what it is running
    /// (`EntryFrameKind`):
    ///   * `StartupCall`: the pumped call is done, which ends a phase of startup rather than the
    ///     run, whatever the other threads are doing. `stepStartup` steps a program still
    ///     starting up, and moves it to its next phase then; this fails loudly. The entry thread
    ///     is deliberately not marked Terminated — startup is about to give it its next frame,
    ///     ultimately `Main` — because a worker that joined it during a `.cctor` must not observe
    ///     a false end-of-thread and proceed past its Join before `Main` has started.
    ///   * `Main`: an `int Main`'s return value is latched as the exit code, the entry thread
    ///     goes to `WaitingForForegroundThreads` and becomes a background thread, as
    ///     `WaitForOtherThreads` makes it, and the run goes on until shutdown has been
    ///     signalled — the first tick, `Main`'s own included, at which no foreground thread was
    ///     alive — at which point it reports `NormalExit`; the exit code is whatever
    ///     `IlMachineState.LatchedExitCode` holds by then, which a worker may have rewritten
    ///     through `Environment.ExitCode`. A worker's `Environment.Exit` in the meantime is a
    ///     `ProcessExit` like any other. If the foreground threads that remain can make no
    ///     progress, the tick reports `Deadlocked`: real .NET would hang there.
    let stepPrepared
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : ProgramStepOutcome
        =
        match MultiProgram.step loggerFactory logger prepared.Driver with
        | DriverTick.InstructionStepped (driver, ranThread, whatWeDid, effect) ->
            ProgramStepOutcome.InstructionStepped (
                {
                    Driver = driver
                },
                ranThread,
                whatWeDid,
                effect
            )
        | DriverTick.WorkerTerminated (driver, terminatingThread) ->
            ProgramStepOutcome.WorkerTerminated (
                {
                    Driver = driver
                },
                terminatingThread
            )
        // The machine the process's end leaves holds no process, and a run of one program has
        // nothing more to do with it.
        | DriverTick.Ended (outcome, _) -> ProgramStepOutcome.Completed outcome
        | DriverTick.Deadlocked (driver, stuck) ->
            ProgramStepOutcome.Deadlocked (
                {
                    Driver = driver
                },
                stuck
            )
        | DriverTick.StartupCallReturned _ ->
            failwith
                "Program.stepPrepared: the entry thread's startup call returned, which ends a phase of startup; a program still starting up is stepped by stepStartup"

    let rec pumpPrepared (loggerFactory : ILoggerFactory) (logger : ILogger) (prepared : PreparedProgram) : RunEnd =
        match stepPrepared loggerFactory logger prepared with
        | ProgramStepOutcome.Completed outcome -> RunEnd.Ended outcome
        | ProgramStepOutcome.Deadlocked (_, stuck) ->
            failwith $"Deadlock: no runnable threads and the process has not exited. Stuck: {stuck}"
        | ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
        | ProgramStepOutcome.WorkerTerminated (prepared, _) -> pumpPrepared loggerFactory logger prepared

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to schedule.
    ///
    /// `hostConfig.Guest.Kernel` carries the host's choices for the simulated process's kernel and
    /// is applied here rather than by the caller afterwards, because this function pumps the entry
    /// type's `.cctor` and CoreLib latches some of these values during static initialisation
    /// (notably `Environment.ProcessorCount`). `KernelConfig.Default` is the no-preference
    /// choice. Its `Environment` follows whichever `EmulatedKernel.defaultEnvironment` entries it
    /// does not name (see `EmulatedKernel.withEnvironment`), so callers that supply no environment
    /// still get the seeded `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` default, and a caller that
    /// names that variable replaces it — that's how the CLI lets the host process override the
    /// seed if it really needs to.
    ///
    /// `hostConfig.PctSeed = Some s` selects the PCT scheduling policy seeded with `s`; `None` keeps the
    /// default round-robin policy. Applied before any cctor frame is pushed so the very first
    /// `chooseNext` decision is policy-correct — `IlMachineState.initial` defaults the field
    /// to `RoundRobin`, and `withPctSeed` simply overwrites it.
    ///
    /// Raises `UnsupportedRuntimeException`, before any guest code runs, if the CoreLib the guest
    /// resolves along `hostConfig.Guest.DotnetRuntimeDirs` has an `AssemblyVersion` major that is
    /// not in `EmulatedRuntime.supported`. The run then serves the runtime `EmulatedRuntime.ofCoreLib`
    /// reads from that CoreLib.
    let beginStartup
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : Startup
        =
        let logger = loggerFactory.CreateLogger "Program"
        let dotnetRuntimeDirs = hostConfig.Guest.DotnetRuntimeDirs
        let kernelConfig = hostConfig.Guest.Kernel
        let pctSeed = hostConfig.PctSeed
        let argv = hostConfig.Guest.Argv

        let dumped = Assembly.read loggerFactory originalPath fileStream

        // How the guest sees itself named on its own command line. `originalPath` is
        // deliberately not consulted: that is where the *host* read the image from, used to
        // find a sidecar PDB, and it is not part of the replay contract — the test harness
        // passes a `.cs` source name there. `ScopeName` is the file name the compiler stamped
        // into the image, so a host that expresses no preference still gets a name that came
        // from the image rather than from the machine it is running on.
        let exePath : string =
            hostConfig.Guest.AssemblyPath |> Option.defaultValue dumped.ScopeName

        let entryPoint =
            match dumped.MainMethod with
            | None -> failwith "No entry point in input DLL"
            | Some d -> d

        let mainMethodFromMetadata = dumped.Methods.[entryPoint]

        if mainMethodFromMetadata.Signature.GenericParameterCount > 0 then
            failwith "Refusing to execute generic main method"

        let mainTakesStringArrayArg =
            match mainMethodFromMetadata.Signature.ParameterTypes |> Seq.toList with
            | [] -> false
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] -> true
            | _ ->
                failwith
                    "Main method must take no parameters or a single string[]; other signatures not yet implemented"

        // CoreCLR's `ValidateMainMethod` reads the return column through
        // `MetaSig::GetReturnType`, which skips custom modifiers, so `void modreq(X) Main()` is a
        // `void Main` there. The decoded signature mirrors the blob and spells that as
        // `Returns (Modified ...)`, so the modifiers are looked through here before classifying.
        let mainReturn =
            match mainMethodFromMetadata.Signature.ReturnType with
            | MethodReturnType.Void -> MainReturn.Void
            | MethodReturnType.Returns returns ->
                match TypeDefn.stripCustomModifiers returns with
                | TypeDefn.Void -> MainReturn.Void
                | TypeDefn.PrimitiveType PrimitiveType.Int32 -> MainReturn.Int32
                | _ ->
                    // CoreCLR's `ValidateMainMethod` also admits a `uint32` return, which no C# or
                    // F# source can declare.
                    failwith $"Main method returns %O{returns}; only a void or int32 Main is supported"

        // `KernelConfig.toKernel`, keeping the machine's part of the configuration for the
        // driver, whose clock it describes. The clock is checked first, so that a host that
        // misconfigured it finds out before anything boots.
        let machineConfig, processConfig = KernelConfig.split kernelConfig
        let clock = MachineConfig.clock "KernelConfig" machineConfig

        let kernel =
            MachineConfig.boot "KernelConfig" "KernelConfig" machineConfig processConfig

        let machine, view = MultiProgram.ofBooted kernel.System

        let state =
            IlMachineState.initial loggerFactory dotnetRuntimeDirs dumped
            |> fun s -> s.MapKernel (fun _ -> EmulatedKernel.withUnix view kernel)
            |> fun s ->
                match pctSeed with
                | None -> s
                | Some seed -> IlMachineState.withPctSeed seed s

        // Find the core library by traversing the type hierarchy of the main method's declaring type
        // until we reach System.Object
        let rec handleBaseTypeInfo
            (state : IlMachineState)
            (baseTypeInfo : BaseTypeInfo)
            (currentAssembly : DumpedAssembly)
            (continueWithGeneric :
                IlMachineState
                    -> TypeInfo<GenericParamFromMetadata, TypeDefn>
                    -> DumpedAssembly
                    -> IlMachineState * BaseClassTypes<DumpedAssembly> option)
            (continueWithResolved :
                IlMachineState
                    -> TypeInfo<TypeDefn, TypeDefn>
                    -> DumpedAssembly
                    -> IlMachineState * BaseClassTypes<DumpedAssembly> option)
            : IlMachineState * BaseClassTypes<DumpedAssembly> option
            =
            match baseTypeInfo with
            | BaseTypeInfo.TypeRef typeRefHandle ->
                // Look up the TypeRef from the handle
                let typeRef = currentAssembly.TypeRefs.[typeRefHandle]

                let rec go state =
                    // Resolve the type reference to find which assembly it's in
                    match
                        LoadedTypeResolution.resolveTypeRef
                            state.TypeSystem._LoadedAssemblies
                            currentAssembly
                            ImmutableArray.Empty
                            typeRef
                    with
                    | TypeResolutionResult.FirstLoadAssy assyRef ->
                        // Need to load this assembly first
                        let handle, definedIn = assyRef.Handle

                        let state, _, _ =
                            IlMachineState.loadAssembly
                                loggerFactory
                                state.TypeSystem._LoadedAssemblies.[definedIn]
                                handle
                                state

                        go state
                    | TypeResolutionResult.NotFound miss ->
                        failwithf
                            "Base type reference %s from %s does not resolve: %O"
                            typeRef.Name
                            currentAssembly.DefinitionFullName
                            miss
                    | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
                        continueWithResolved state resolvedType resolvedAssembly

                go state
            | BaseTypeInfo.TypeDef typeDefHandle ->
                // Base type is in the same assembly
                let baseType = currentAssembly.TypeDefs.[typeDefHandle]
                continueWithGeneric state baseType currentAssembly
            | BaseTypeInfo.TypeSpec _ -> failwith "Type specs not yet supported in base type traversal"

        // Admission precedes `BaseClassTypes.ofCorelib`: a CoreLib from an unsupported major may lack a type
        // that `BaseClassTypes.ofCorelib` demands, and that failure would hide the reason the CoreLib is
        // unusable.
        let coreLibBaseTypes (corelib : DumpedAssembly) : BaseClassTypes<DumpedAssembly> =
            match EmulatedRuntime.classify corelib with
            | Error unsupported -> raise (UnsupportedRuntimeException (unsupported, dotnetRuntimeDirs))
            | Ok _ -> BaseClassTypes.ofCorelib corelib

        let rec findCoreLibraryAssemblyFromGeneric
            (state : IlMachineState)
            (currentType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                state, Some (coreLibBaseTypes currentAssembly)
            | Some baseTypeInfo ->
                handleBaseTypeInfo
                    state
                    baseTypeInfo
                    currentAssembly
                    findCoreLibraryAssemblyFromGeneric
                    findCoreLibraryAssemblyFromResolved

        and findCoreLibraryAssemblyFromResolved
            (state : IlMachineState)
            (currentType : TypeInfo<TypeDefn, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                state, Some (coreLibBaseTypes currentAssembly)
            | Some baseTypeInfo ->
                handleBaseTypeInfo
                    state
                    baseTypeInfo
                    currentAssembly
                    findCoreLibraryAssemblyFromGeneric
                    findCoreLibraryAssemblyFromResolved

        /// The frame the entry thread runs during startup: the entry point's *signature* with
        /// its body replaced by a bare `ret`. Pushing cctors underneath it and pumping until
        /// it returns is how `prepare` drives static initialisation without entering Main.
        ///
        /// Rebuildable rather than built once, because seeding AppContext also has to pump
        /// the entry thread to completion, which consumes this frame; the seed then puts a
        /// fresh one back for the cctor pump that follows.
        let buildStartupFrame
            (baseTypes : BaseClassTypes<DumpedAssembly>)
            (state : IlMachineState)
            : IlMachineState * MethodState
            =
            // Use the original method from metadata, but convert FakeUnit to TypeDefn
            let rawMainMethod =
                mainMethodFromMetadata
                |> MethodInfo.mapTypeGenerics (fun (i, _) -> TypeDefn.GenericTypeParameter i.SequenceNumber)

            let state, concretizedMainMethod, _ =
                ExecutionConcretization.concretizeMethodWithTypeGenerics
                    loggerFactory
                    baseTypes
                    ImmutableArray.Empty // No type generics for main method's declaring type
                    // Synthesised, not the entry point with its body swapped: the substituted
                    // body is not what `Main`'s MethodDef row describes, so carrying that row's
                    // identity would let anything keyed by it — debug information above all —
                    // describe this frame as though `Main` were running. It is not; `Main` has
                    // not been installed yet.
                    (MethodInfo.Synthesised (
                        { rawMainMethod.Core with
                            Body = MethodBody.Il (MethodInstructions.onlyRet ())
                            // The body returns nothing, so the signature must say so: every
                            // frame's stack is checked against its signature at `ret`, the
                            // bottom frame's included, and `Main`'s own return type would
                            // have an `int Main`'s placeholder refused as invalid CIL.
                            Signature =
                                { rawMainMethod.Core.Signature with
                                    ReturnType = MethodReturnType.Void
                                }
                        },
                        SynthesisedMethod.EntryPointPlaceholder
                    ))
                    None
                    dumped.DefinitionFullName
                    ImmutableArray.Empty
                    state

            // Create the method state with the concretized method.
            // The body has been replaced with onlyRet, so these are placeholders whose
            // length must match the method's parameter count.
            let placeholderArgs =
                if mainTakesStringArrayArg then
                    ImmutableArray.CreateRange [ CliType.ObjectRef None ]
                else
                    ImmutableArray.Empty

            match
                MethodState.Empty
                    state.TypeSystem.ConcreteTypes
                    baseTypes
                    state.TypeSystem._LoadedAssemblies
                    dumped
                    concretizedMainMethod
                    ImmutableArray.Empty
                    placeholderArgs
                    None
            with
            | Ok concretizedMeth -> state, concretizedMeth
            | Error _ -> failwith "Unexpected failure creating method state with concretized method"

        let rec computeState (baseClassTypes : BaseClassTypes<DumpedAssembly> option) (state : IlMachineState) =
            match baseClassTypes with
            | Some baseTypes ->
                // We already have base class types, can directly create the concretized method
                let state, concretizedMeth = buildStartupFrame baseTypes state
                IlMachineState.addThread concretizedMeth state, Some baseTypes
            | None ->
                // We need to discover the core library by traversing the type hierarchy
                let mainMethodType =
                    dumped.TypeDefs.[mainMethodFromMetadata.RequiredDeclaringType.Definition.Get]

                let state, baseTypes =
                    findCoreLibraryAssemblyFromGeneric state mainMethodType dumped

                computeState baseTypes state

        let (state, mainThread), baseClassTypes = state |> computeState None

        let baseClassTypes =
            match baseClassTypes with
            | Some c -> c
            | None -> failwith "Expected base class types to be available at this point"

        // Now that we have base class types, concretize the main method for use in the rest of the function
        let state, concretizedMainMethod, mainTypeHandle =
            let rawMainMethod =
                mainMethodFromMetadata
                |> MethodInfo.mapTypeGenerics (fun (i, _) -> TypeDefn.GenericTypeParameter i.SequenceNumber)

            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty // No type generics for main method's declaring type
                rawMainMethod
                None
                dumped.DefinitionFullName
                ImmutableArray.Empty
                state

        let state =
            { state with
                TypeSystem =
                    { state.TypeSystem with
                        ConcreteTypes =
                            Corelib.concretizeAll
                                state.TypeSystem._LoadedAssemblies
                                baseClassTypes
                                state.TypeSystem.ConcreteTypes
                    }
            }

        // Seed AppContext before anything else runs. On CoreCLR this happens in
        // `CorHost2::CreateAppDomainWithManager`, before any managed code at all; the deadline
        // that actually bites is that BCL feature switches latch on first read into a
        // `static readonly` (`EventSource.IsSupported` is the motivating one), so seeding has
        // to precede the entry type's cctor pump below, not merely precede Main.
        //
        // This runs the entry thread to completion, which consumes its startup frame; a fresh
        // one goes back afterwards so the cctor pump that follows is unaffected.
        // The host's properties sit on top of PawPrint's own runtime baseline, which is how
        // "this runtime does not support dynamic code" reaches every guest without each host
        // having to remember to say so. Applied here rather than in `HostConfig.Default` so
        // that a host which builds its `HostConfig` some other way — the App, which replaces
        // `AppContext` wholesale with the guest's `runtimeconfig.json` — cannot drop it.
        let propertiesToSeed =
            AppContextProperties.withRuntimeBaseline hostConfig.Guest.AppContext

        let rec loadInitialState (state : IlMachineState) =
            match
                state
                |> IlMachineStateExecution.loadClass loggerFactory baseClassTypes mainTypeHandle mainThread
            with
            | StateLoadResult.NothingToDo ilMachineState -> ilMachineState
            | StateLoadResult.FirstLoadThis ilMachineState -> loadInitialState ilMachineState
            | StateLoadResult.ThrowingTypeInitializationException _
            | StateLoadResult.UnhandledTypeInitializationException _ ->
                // Unreachable at startup: `loadClass` only pushes cctor frames, and no cctor has
                // run yet, so the entry type cannot already have failed.
                failwith
                    "logic error: initial loadClass for entry point type observed an already-failed class initialiser"
            | StateLoadResult.Blocked _ ->
                // Unreachable at startup: only the entry thread exists, so no other thread can
                // be mid-cctor on the entry type.
                failwith
                    "logic error: initial loadClass for entry point cannot block on another thread (no other threads exist yet)"

        /// Load the entry class, so that the class-initialisation phase has its `.cctor` to
        /// pump. Runs no guest instructions of its own — `loadClass` only pushes cctor frames.
        let enterClassInit
            (mainArgs : ImmutableArray<CliType>)
            (state : IlMachineState)
            : IlMachineState * StartupPhase
            =
            loadInitialState state, StartupPhase.InitialisingClasses mainArgs

        let installMain (driver : MultiProgram) (mainArgs : ImmutableArray<CliType>) : ProgramStartResult =
            logger.LogInformation "Main method class now initialised"
            let state = driver.Current.State

            // Now that BCL initialisation has taken place and the user-code classes are constructed,
            // overwrite the main thread completely using the already-concretized method. The entry
            // thread Terminated during the cctor pump (its onlyRet body hit `ret`); we're resurrecting
            // it to run Main, so restore Status to Runnable before the scheduler is asked to pick again.
            let methodState =
                match
                    MethodState.Empty
                        state.TypeSystem.ConcreteTypes
                        baseClassTypes
                        state.TypeSystem._LoadedAssemblies
                        dumped
                        concretizedMainMethod
                        ImmutableArray.Empty
                        mainArgs
                        None
                with
                | Ok s -> s
                | Error _ -> failwith "TODO: I'd be surprised if this could ever happen in a valid program"

            let threadState =
                state.ThreadState.[mainThread]
                |> ThreadState.replaceFrames methodState
                |> fun threadState ->
                    { threadState with
                        Status = ThreadStatus.Runnable
                    }

            let state, init =
                { state with
                    ThreadState = state.ThreadState |> Map.add mainThread threadState
                }
                |> IlMachineStateExecution.ensureTypeInitialised loggerFactory baseClassTypes mainThread mainTypeHandle

            match init with
            | WhatWeDid.Aborted fatal ->
                // Triggered when initialising the entry point's declaring type tears the process
                // down. Startup has no `RunOutcome` to hand back at this point -- it is still
                // assembling the machine -- so the abort cannot be reported as one; surface it
                // rather than installing Main on a state whose process has already died.
                let message = fatal.Message |> Option.defaultValue "<no message>"

                failwith
                    $"TODO: initialising the entry point's declaring type aborted the process (%O{fatal.Code}): %s{message}"
            | WhatWeDid.SuspendedForClassInit -> failwith "TODO: suspended for class init"
            | WhatWeDid.SuspendedForManagedCall ->
                failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
            | WhatWeDid.BlockedOnClassInit _ ->
                failwith "logic error: surely this thread can't be blocked on class init"
            | WhatWeDid.ThrowingTypeInitializationException
            | WhatWeDid.UnhandledException _ ->
                // A failing entry-type cctor ends the run during the class-initialisation phase,
                // as `RunOutcome.GuestUnhandledException`, before Main is ever installed.
                failwith "logic error: entry point type's class initialiser had already failed when Main was installed"
            | WhatWeDid.VoluntaryYield _ ->
                // ensureTypeInitialised drives cctor execution, which has no path to a
                // yield primitive: voluntary yields are produced by native handlers like
                // `ThreadNative_YieldThread`, never by a synthetic cctor step. If this
                // arm ever fires, the cctor pipeline has acquired a producer we didn't
                // anticipate, and the entry-point sequencer needs to decide explicitly
                // whether to honour the yield before running Main.
                failwith "logic error: ensureTypeInitialised cannot produce a VoluntaryYield"
            | WhatWeDid.Executed -> ()

            ProgramStartResult.Ready
                {
                    Driver =
                        MultiProgram.withCurrent
                            {
                                State = state
                                BaseClassTypes = baseClassTypes
                                EntryThread = mainThread
                                // Nothing can have signalled shutdown yet: the entry thread is about
                                // to run `Main` as a foreground thread, and only `Main` arms the latch.
                                EntryFrame = EntryFrameKind.Main (mainReturn, false)
                                LastRan = mainThread
                            }
                            driver
                }

        let atPhase (state : IlMachineState) (phase : StartupPhase) : Startup =
            {
                Driver =
                    MultiProgram.create
                        clock
                        machine
                        {
                            State = state
                            BaseClassTypes = baseClassTypes
                            EntryThread = mainThread
                            EntryFrame = EntryFrameKind.StartupCall
                            LastRan = mainThread
                        }
                Phase = phase
                InstallMain = installMain
            }

        /// Run `methodState` on the entry thread in place of whatever frame it currently holds.
        /// Every phase that pumps a call installs it this way, and every such call consumes the
        /// frame by terminating the thread, so `reinstateStartupFrame` is its counterpart.
        let installCall (methodState : MethodState) (state : IlMachineState) : IlMachineState =
            let threadState =
                state.ThreadState.[mainThread]
                |> ThreadState.replaceFrames methodState
                |> fun threadState ->
                    { threadState with
                        Status = ThreadStatus.Runnable
                    }

            { state with
                ThreadState = state.ThreadState |> Map.add mainThread threadState
            }

        /// Put a fresh startup frame on the entry thread, which a pumped call consumed by
        /// running the thread to completion.
        ///
        /// The invariant every phase-entry function below relies on: on entry, the entry thread
        /// holds a startup frame. `computeState` establishes it, and each `onReturn` restores it.
        let reinstateStartupFrame (state : IlMachineState) : IlMachineState =
            let state, startupFrame = buildStartupFrame baseClassTypes state
            installCall startupFrame state

        /// Install the guest's command line, the way `CorHost2::ExecuteAssembly` does
        /// immediately before it runs `Main`. The array CoreLib returns is the one `Main`
        /// receives, so there is no second construction of it to disagree.
        let enterCommandLineInit (state : IlMachineState) : IlMachineState * StartupPhase =
            let state, initFrame =
                CommandLineArgsInit.prepareCall loggerFactory baseClassTypes exePath argv state

            logger.LogInformation "Installing the guest's command line"

            let onInitialised (state : IlMachineState) : IlMachineState * StartupPhase =
                // `InitializeCommandLineArgs` returns the arguments `Main` is to be given,
                // having just built `s_commandLineArgs` from the same input in the same
                // pass. The entry thread has terminated, so its eval stack holds the return
                // value the way it holds `Main`'s.
                let returned =
                    match state.ThreadState.[mainThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.ObjectRef addr :: _ -> addr
                    | [] ->
                        failwith
                            "System.Environment::InitializeCommandLineArgs returned without leaving its string[] on the eval stack."
                    | other :: _ ->
                        failwith
                            $"System.Environment::InitializeCommandLineArgs left %O{other} on the eval stack; expected the string[] of Main's arguments."

                let mainArgs =
                    if mainTakesStringArrayArg then
                        ImmutableArray.Create (CliType.ofManagedObject returned)
                    else
                        // The call is made regardless — `ExecuteAssembly` makes it before
                        // it knows the entry point's signature, and `GetCommandLineArgs`
                        // must work for a `Main` that takes nothing — so the array is
                        // simply not passed on.
                        ImmutableArray.Empty

                state |> reinstateStartupFrame |> enterClassInit mainArgs

            installCall initFrame state, StartupPhase.InitialisingCommandLine onInitialised

        match AppContextSeed.prepareCall loggerFactory baseClassTypes propertiesToSeed state with
        | None ->
            // Nothing to seed, so there is no first phase to pump. The startup frame
            // `computeState` installed is still in place, never having been consumed.
            let state, phase = enterCommandLineInit state
            atPhase state phase
        | Some (state, setupFrame) ->
            logger.LogInformation "Seeding AppContext from the host's configuration properties"

            let onSeeded (state : IlMachineState) : IlMachineState * StartupPhase =
                state |> reinstateStartupFrame |> enterCommandLineInit

            atPhase (installCall setupFrame state) (StartupPhase.SeedingAppContext onSeeded)

    /// How a startup call that was pumped to completion ended, as the tail of a sentence
    /// naming what was being run: "Seeding AppContext <this>."
    ///
    /// By case rather than with `%O`: every `RunOutcome` carries an `IlMachineState`, so
    /// structural formatting would render the entire heap into the exception message.
    let private describeStartupOutcome (outcome : RunOutcome) : string =
        match outcome with
        | RunOutcome.NormalExit _ -> "exited normally"
        | RunOutcome.ProcessExit (_, thread, _) -> $"called Environment.Exit on %O{thread}"
        | RunOutcome.Aborted (_, thread, fatal, _) ->
            let message = fatal.Message |> Option.defaultValue "<no message>"
            $"aborted on %O{thread} with %O{fatal.Code}: %s{message}"
        | RunOutcome.SignalTerminated (_, signal, coreDumped) ->
            if coreDumped then
                $"was terminated by signal %O{signal} (core dumped)"
            else
                $"was terminated by signal %O{signal}"
        | RunOutcome.GuestUnhandledException (finalState, thread, exn, _) ->
            $"threw an unhandled exception on %O{thread}:\n%s{UnhandledExceptionReport.describe finalState exn}"

    /// Advance startup by one guest instruction, crossing a phase boundary when the entry
    /// thread's current frame returns.
    let stepStartup (loggerFactory : ILoggerFactory) (logger : ILogger) (startup : Startup) : StartupStepOutcome =
        match MultiProgram.step loggerFactory logger startup.Driver with
        | DriverTick.StartupCallReturned driver ->
            match startup.Phase with
            | StartupPhase.SeedingAppContext onReturn
            | StartupPhase.InitialisingCommandLine onReturn ->
                let state, phase = onReturn driver.Current.State

                StartupStepOutcome.PhaseAdvanced
                    { startup with
                        Driver =
                            MultiProgram.withCurrent
                                { driver.Current with
                                    State = state
                                }
                                driver
                        Phase = phase
                    }
            | StartupPhase.InitialisingClasses mainArgs ->
                StartupStepOutcome.Completed (startup.InstallMain driver mainArgs)
        | DriverTick.InstructionStepped (driver, ran, whatWeDid, effect) ->
            StartupStepOutcome.Stepped (
                { startup with
                    Driver = driver
                },
                ran,
                whatWeDid,
                effect
            )
        | DriverTick.WorkerTerminated (driver, terminated) ->
            StartupStepOutcome.WorkerTerminated (
                { startup with
                    Driver = driver
                },
                terminated
            )
        | DriverTick.Deadlocked (driver, stuck) ->
            StartupStepOutcome.Deadlocked (
                { startup with
                    Driver = driver
                },
                stuck
            )
        | DriverTick.Ended (outcome, _) ->

        // The process ended before `Main` was installed.
        match startup.Phase with
        | StartupPhase.InitialisingCommandLine _ ->
            // `InitializeCommandLineArgs` news up two arrays and copies strings out of buffers
            // we ourselves just wrote; it has no other way to end. Anything else means a cctor
            // dragged in by that work misbehaved, and pressing on would run Main with an
            // unpopulated command line.
            failwith $"Installing the guest's command line %s{describeStartupOutcome outcome}."
        | StartupPhase.SeedingAppContext _ ->
            // Nothing in `AppContext.Setup` can legitimately exit, fail fast or throw: it
            // allocates a Dictionary and copies strings out of buffers we ourselves just
            // wrote. Anything else means a cctor dragged in by that work misbehaved, and
            // pressing on would run Main against a half-seeded AppContext.
            failwith $"Seeding AppContext %s{describeStartupOutcome outcome}."
        | StartupPhase.InitialisingClasses _ ->
            // The entry thread's `.cctor` raised, or a worker spawned during cctor pumping
            // exited, failed fast, or took a terminating signal. In every case the CLR would
            // tear the process down; propagate rather than collapsing to a host `failwith`
            // that would mask the guest-level diagnostic, and rather than pressing on into
            // Main.
            StartupStepOutcome.Completed (ProgramStartResult.CompletedBeforeMain (RunEnd.Ended outcome))

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to
    /// schedule, running startup to completion.
    ///
    /// This is `beginStartup` driven by `stepStartup` in a loop. A driver that wants to observe
    /// startup — to stream a static initialiser's output, or to report where startup wedged
    /// rather than throwing out of it — should drive those two directly instead; guest code
    /// runs during startup, and this function gives back nothing until all of it has finished.
    ///
    /// See `beginStartup` for the kernel-config and PCT-seed timing contracts.
    let prepare
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : ProgramStartResult
        =
        let logger = loggerFactory.CreateLogger "Program"

        let rec go (startup : Startup) : ProgramStartResult =
            match stepStartup loggerFactory logger startup with
            | StartupStepOutcome.Completed result -> result
            | StartupStepOutcome.Stepped (startup, _, _, _)
            | StartupStepOutcome.WorkerTerminated (startup, _)
            | StartupStepOutcome.PhaseAdvanced startup -> go startup
            | StartupStepOutcome.Deadlocked (_, stuck) ->
                failwith $"Deadlock during startup: no runnable threads and startup has not finished. Stuck: {stuck}"

        go (beginStartup loggerFactory originalPath fileStream hostConfig)

    /// Returns the outcome of the program run: normal exit or unhandled guest exception.
    ///
    /// `hostConfig.PctSeed` flows through to `prepare`: `Some s` selects PCT with seed `s`,
    /// `None` keeps the default round-robin scheduler. See `prepare` for the
    /// timing contract (applied before the first cctor frame is pushed).
    let run
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (hostConfig : HostConfig)
        : RunEnd
        =
        let logger = loggerFactory.CreateLogger "Program"

        match prepare loggerFactory originalPath fileStream hostConfig with
        | ProgramStartResult.CompletedBeforeMain runEnd -> runEnd
        | ProgramStartResult.Ready prepared -> pumpPrepared loggerFactory logger prepared

    /// A machine state sitting at a scheduler tick *boundary* whose next decision is contended:
    /// once this tick's preamble has run, more than one thread is Runnable, so which of them runs
    /// is a genuine choice — and it is the first such choice since this snapshot's run began.
    ///
    /// Contention is a property of the state the *policy*
    /// sees, which is not the state held here: a deadline expiring or the signal dispatcher waking
    /// can make a second thread Runnable inside the tick. So `State` may well show only one
    /// Runnable thread, and `Contenders` may name a thread that is blocked in it. Guests reaching
    /// their first fork organically do not show this — there the second thread arrives via the
    /// guest's own `Thread.Start`, which is a retired instruction — but `runToNextFork` from
    /// mid-run does, and a caller inspecting `State` should not expect otherwise.
    ///
    /// Why this is worth having: everything before a fork point is forced, so every scheduling
    /// policy makes the same choices there and — since `Scheduler` only ever mutates policy state
    /// at a contended decision — the policy state is still exactly what it was seeded with. A
    /// harness sweeping many PCT seeds over one guest can therefore compute this prefix *once*,
    /// under `RoundRobin`, and hand each seed a run bit-identical to what it would have produced
    /// from scratch. Measured on the `sourcesConcurrencyBugs` guests, that prefix is 74-94% of a
    /// run's instructions and ~90% of its wall clock.
    ///
    /// The state held is the one from *before* the tick's preamble, not from between the preamble
    /// and the choice: a mid-tick value would be a new kind of resumable
    /// thing, and handing it to the ordinary driver would run the preamble twice — advancing
    /// `StepCounter` twice and shifting the spurious-wakeup schedule. Resuming therefore re-runs
    /// the contended tick's preamble, which is policy-independent (see `MultiProgram.advance`) and
    /// so reproduces it exactly.
    ///
    /// Construct one only through `runToFirstFork` / `runToNextFork`: the representation is
    /// private because the type's whole value is the claim that the prefix behind it was forced,
    /// and a hand-built one would carry that claim without having earned it.
    type ForkSnapshot =
        private
            {
                Prepared : PreparedProgram
                Contending : ThreadId list
            }

        /// The machine as it stands at the fork point.
        member this.State : IlMachineState = this.Prepared.State

        /// The threads whose contention makes this a fork point: at least two, ascending by
        /// `ThreadId`. Runnable *at the decision point* — i.e. after this tick's preamble — which
        /// is not necessarily the same as Runnable in `State`. Ascending order is the order
        /// `PctState.ensurePriorityFor` samples in, so it is part of what makes a seeded
        /// schedule reproducible.
        member this.Contenders : ThreadId list = this.Contending

    /// How far a run got before it first had a scheduling choice to make.
    [<RequireQualifiedAccess>]
    type PrefixOutcome =
        /// Reached a contended decision. Resume with `resumeFork`, once per seed.
        | ForkedAt of ForkSnapshot
        /// The program ran to completion without ever reaching a contended decision. No policy
        /// had a choice anywhere, so this is the outcome under *every* seed, and a sweep is
        /// answered by this one run. (Its state's `Scheduling` is the `RoundRobin` the prefix ran
        /// under, where a from-scratch `Pct s` run would carry `Pct (ofSeed s)`; nothing
        /// guest-visible depends on the difference, but do not compare that field.)
        | NeverForked of RunEnd
        /// Every thread blocked before any choice arose. Like `NeverForked`, seed-independent.
        | DeadlockedBeforeFork of stuckThreads : string
        /// A class initialiser started a thread, so the first contended decision happens during
        /// startup rather than in `Main`.
        ///
        /// Detected and refused rather than snapshotted. Snapshotting it is possible — the
        /// detector finds the exact point — but resuming it means handing the caller a
        /// half-finished `Startup` rather than a `PreparedProgram`, so `resumeFork` would have to
        /// return a two-shape value and every caller would have to drive both phases. No guest in
        /// this repository does it, so refuse loudly rather than build the surface. To lift the
        /// restriction, give `ForkSnapshot` a startup arm — nothing else here has to change.
        ///
        /// Carries the contenders rather than a rendered message, so a caller can decide what to
        /// do about the refusal (report it, fall back to per-seed runs).
        | ForkedDuringStartup of contenders : ThreadId list

    /// Guard against a yield retiring at a tick we classified as forced whose *post*-step state is
    /// contended.
    ///
    /// This is the one way a prefix could be seed-dependent despite every decision being forced.
    /// `Scheduler.onStepOutcome` wakes class-init waiters *before* charging the yield debt, so
    /// `chargeYieldDebt` reads contention against a Runnable set that may have grown since the
    /// choice was made. At such a tick a `Pct` policy would toss its honour coin — and could
    /// decline the yield where `RoundRobin` always honours it, which the guest sees directly in
    /// `Thread.Yield()`'s return value. A prefix containing one is not shareable.
    ///
    /// Unreachable today: a thread parked `BlockedOnClassInit` must have executed a step to get
    /// there, and a `.cctor` can only be `InProgress` on another thread, so two threads have
    /// already run and contention has already occurred. But that is a chain of facts about wake
    /// paths rather than a structural property, so check the conclusion and crash rather than
    /// silently emit a snapshot that does not commute.
    let private checkYieldDidNotStraddle (ran : ThreadId) (whatWeDid : WhatWeDid) (after : IlMachineState) : unit =
        match whatWeDid with
        | WhatWeDid.VoluntaryYield _ ->
            match Scheduler.tryContenders after with
            | None -> ()
            | Some contenders ->
                failwith
                    $"Program: thread %O{ran} yielded at a tick whose scheduling decision was forced, but the state after the step is contended (Runnable: %A{contenders}). Scheduler.chargeYieldDebt reads contention after class-init waiters are woken, so a Pct policy would have drawn here — and could have declined the yield where RoundRobin honours it — which means the prefix up to this point is not seed-independent and must not be shared. See Scheduler.onStepOutcome."
        | WhatWeDid.Executed
        | WhatWeDid.Aborted _
        | WhatWeDid.UnhandledException _
        | WhatWeDid.SuspendedForClassInit
        | WhatWeDid.SuspendedForManagedCall
        | WhatWeDid.BlockedOnClassInit _
        | WhatWeDid.ThrowingTypeInitializationException -> ()

    /// Advance `prepared` until the next contended scheduling decision, returning the machine as
    /// it stood at the start of that tick.
    ///
    /// This is the general primitive: from a fresh `Main` it finds the *first* fork point, and
    /// from a mid-run state it finds the next one, which is what a future schedule-space tree
    /// search descends with. What "resume" means differs between those two — see
    /// `IlMachineState.withPctSeed` — but finding the point does not.
    ///
    /// Each *retired* tick's preamble runs exactly once: the probe consumes it and hands the
    /// advanced state straight to the decision half. The fork tick itself is the exception:
    /// its preamble runs here to answer the probe, and again on every resume.
    let rec runToNextFork
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (prepared : PreparedProgram)
        : PrefixOutcome
        =
        match MultiProgram.annotating prepared.State (fun () -> MultiProgram.advance prepared.Driver) with
        | Advanced.Ended (outcome, _) -> PrefixOutcome.NeverForked (RunEnd.Ended outcome)
        | Advanced.Deadlocked (_, stuck) -> PrefixOutcome.DeadlockedBeforeFork stuck
        | Advanced.Decide advanced ->

        match Scheduler.tryContenders advanced.Current.State with
        | Some contenders ->
            PrefixOutcome.ForkedAt
                {
                    Prepared = prepared
                    Contending = contenders
                }
        | None ->

        match
            MultiProgram.annotating advanced.Current.State (fun () -> MultiProgram.decide loggerFactory logger advanced)
        with
        | DriverTick.StartupCallReturned _ ->
            failwith
                "Program.runToNextFork: the entry thread's startup call returned, but a fork snapshot is taken only once Main is installed"
        | DriverTick.Deadlocked _ ->
            failwith
                "Program.runToNextFork: the program deadlocked in the decision half of a tick, which only the preamble reports (this is an interpreter bug)."
        | DriverTick.Ended (outcome, _) -> PrefixOutcome.NeverForked (RunEnd.Ended outcome)
        | DriverTick.WorkerTerminated (next, _) ->
            runToNextFork
                loggerFactory
                logger
                {
                    Driver = next
                }
        | DriverTick.InstructionStepped (next, ran, whatWeDid, _) ->
            checkYieldDidNotStraddle ran whatWeDid next.Current.State

            runToNextFork
                loggerFactory
                logger
                {
                    Driver = next
                }

    /// Read the guest assembly and run it — startup and all — up to its first contended
    /// scheduling decision.
    ///
    /// Takes a `GuestConfig` rather than a `HostConfig` precisely so that no seed can be passed:
    /// the prefix is the part of the run every seed shares, and it is computed under the
    /// randomness-free `RoundRobin` policy. `resumeFork` supplies the seed afterwards.
    let runToFirstFork
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (guestConfig : GuestConfig)
        : PrefixOutcome
        =
        let logger = loggerFactory.CreateLogger "Program"

        let hostConfig =
            {
                Guest = guestConfig
                PctSeed = None
            }

        let rec goStartup (startup : Startup) : PrefixOutcome =
            // Probe startup with the same predicate `runToNextFork` uses, so a `.cctor` that
            // starts a thread is reported rather than silently mistaken for a forced prefix. The
            // preamble runs twice per startup tick here, once for the probe and once inside
            // `stepStartup`; that is a handful of map operations against `executeOneStep`, and it
            // is paid once for a whole sweep rather than once per seed.
            let contenders =
                match MultiProgram.annotating startup.State (fun () -> MultiProgram.advance startup.Driver) with
                | Advanced.Decide probed -> Scheduler.tryContenders probed.Current.State
                // `stepStartup` runs the same preamble, and ends or deadlocks the same way.
                | Advanced.Ended _
                | Advanced.Deadlocked _ -> None

            match contenders with
            | Some contenders -> PrefixOutcome.ForkedDuringStartup contenders
            | None ->

            match stepStartup loggerFactory logger startup with
            | StartupStepOutcome.Completed (ProgramStartResult.Ready prepared) ->
                runToNextFork loggerFactory logger prepared
            | StartupStepOutcome.Completed (ProgramStartResult.CompletedBeforeMain outcome) ->
                PrefixOutcome.NeverForked outcome
            | StartupStepOutcome.Deadlocked (_, stuck) -> PrefixOutcome.DeadlockedBeforeFork stuck
            | StartupStepOutcome.Stepped (startup, ran, whatWeDid, _) ->
                checkYieldDidNotStraddle ran whatWeDid startup.State
                goStartup startup
            | StartupStepOutcome.WorkerTerminated (startup, _)
            | StartupStepOutcome.PhaseAdvanced startup -> goStartup startup

        goStartup (beginStartup loggerFactory originalPath fileStream hostConfig)

    /// Install a scheduling policy on a fork snapshot and hand back an ordinary `PreparedProgram`,
    /// to be driven with `stepPrepared` / `pumpPrepared` like any other.
    ///
    /// For a snapshot from `runToFirstFork`, `pctSeed = Some s` gives a run bit-identical to
    /// `Program.run` with `PctSeed = Some s` over the same image and `GuestConfig`: the prefix was
    /// forced, so the policy state a from-scratch run would hold here is exactly
    /// `PctState.ofSeed s`. See `IlMachineState.withPctSeed`, which spells out why that stops
    /// being true for a mid-run snapshot from `runToNextFork`.
    ///
    /// `None` installs no policy at all — it keeps whatever the snapshot carries. For a
    /// `runToFirstFork` snapshot that is the `RoundRobin` the prefix ran under, so it reproduces
    /// the default run; for a mid-run snapshot it is whatever policy got you there, mid-flight.
    ///
    /// `loggerFactory` rebinds the state's logging sink, which would otherwise still be the
    /// prefix's: every seed resumed from one snapshot would log through the factory the *prefix*
    /// was built with, losing whatever per-run properties the caller attaches. The prefix's own
    /// factory must outlive every resume regardless, because `BaseClassTypes` and the loaded
    /// assemblies were built against it.
    ///
    /// One thing a resumed run does *not* reproduce: `StepEffect`s retired during the prefix. A
    /// driver streaming guest output per step sees only post-fork effects. The final state's
    /// `Kernel.OutputLog` is still complete, because it came through the snapshot.
    let resumeFork
        (loggerFactory : ILoggerFactory)
        (pctSeed : uint64 option)
        (snapshot : ForkSnapshot)
        : PreparedProgram
        =
        let state =
            snapshot.Prepared.State |> IlMachineState.withLoggerFactory loggerFactory

        let state =
            match pctSeed with
            | None -> state
            | Some seed -> IlMachineState.withPctSeed seed state

        snapshot.Prepared.WithState state
