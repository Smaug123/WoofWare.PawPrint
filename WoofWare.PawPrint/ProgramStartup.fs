namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// One program a driver (`MultiProgram.start`) launches as a process of its own on a
/// simulated machine: the image, and everything the host supplies that describes the
/// process. Each field means what the field of its name on `GuestConfig` or `HostConfig`
/// means, and every field but `OriginalPath` is part of the run's replay contract.
type ProgramLaunch =
    {
        /// The guest assembly's image, read once when the driver starts.
        Image : Stream
        /// Where the host read `Image` from, used only to find a sidecar PDB: see
        /// `Program.prepare`.
        OriginalPath : string option
        /// See `GuestConfig.DotnetRuntimeDirs`.
        DotnetRuntimeDirs : ImmutableArray<string>
        /// How the program's process starts on the machine.
        Process : ProcessConfig
        /// See `GuestConfig.Argv`.
        Argv : string list
        /// See `GuestConfig.AssemblyPath`.
        AssemblyPath : string option
        /// See `GuestConfig.AppContext`.
        AppContext : AppContextProperties
        /// See `HostConfig.PctSeed`: how this program's own scheduler chooses among its
        /// threads.
        PctSeed : uint64 option
    }

/// A guest image read, with its entry point checked: what a program starts from.
type internal EntryImage =
    {
        Dumped : DumpedAssembly
        EntryPoint : MethodDefinitionHandle
        /// Whether `Main` takes a `string[]`; otherwise it takes nothing.
        TakesArguments : bool
        Returns : MainReturn
    }

/// Starting one program: reading its image, and the phases of startup before `Main`, which a
/// driver (`MultiProgram`) then runs tick by tick as it runs `Main`.
[<RequireQualifiedAccess>]
module internal ProgramStartup =
    /// Read the guest assembly from `image`, and check that its entry point is one PawPrint
    /// can run: a non-generic `Main` taking nothing or a `string[]`, returning `void` or
    /// `int`. Fails loudly otherwise.
    let read (loggerFactory : ILoggerFactory) (originalPath : string option) (image : Stream) : EntryImage =
        let dumped = Assembly.read loggerFactory originalPath image

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

        {
            Dumped = dumped
            EntryPoint = entryPoint
            TakesArguments = mainTakesStringArrayArg
            Returns = mainReturn
        }

    /// The program `entry` starts, as `launch` describes it, in a process whose kernel is
    /// `kernel`: its entry thread holds the first call of startup, and its interpreter state
    /// the setup that needs no guest code.
    ///
    /// Raises `UnsupportedRuntimeException`, before any guest code runs, if the CoreLib the guest
    /// resolves along `launch.DotnetRuntimeDirs` has an `AssemblyVersion` major that is not in
    /// `EmulatedRuntime.supported`. The run then serves the runtime `EmulatedRuntime.ofCoreLib`
    /// reads from that CoreLib.
    ///
    /// Fails loudly, before any guest code runs, if `launch.Argv` or `launch.AssemblyPath`
    /// contains a NUL (`CommandLineArgsInit.validate`).
    ///
    /// `launch.PctSeed = Some s` selects the PCT scheduling policy seeded with `s`; `None` keeps
    /// the default round-robin policy. Applied before any cctor frame is pushed so the very first
    /// `chooseNext` decision is policy-correct.
    let start
        (loggerFactory : ILoggerFactory)
        (launch : ProgramLaunch)
        (entry : EntryImage)
        (kernel : EmulatedKernel)
        : RunningProgram
        =
        let logger = loggerFactory.CreateLogger "Program"
        let dotnetRuntimeDirs = launch.DotnetRuntimeDirs
        let argv = launch.Argv
        let dumped = entry.Dumped
        let mainMethodFromMetadata = dumped.Methods.[entry.EntryPoint]
        let mainTakesStringArrayArg = entry.TakesArguments

        // How the guest sees itself named on its own command line. `OriginalPath` is
        // deliberately not consulted: that is where the *host* read the image from, used to
        // find a sidecar PDB, and it is not part of the replay contract — the test harness
        // passes a `.cs` source name there. `ScopeName` is the file name the compiler stamped
        // into the image, so a host that expresses no preference still gets a name that came
        // from the image rather than from the machine it is running on.
        let exePath : string = launch.AssemblyPath |> Option.defaultValue dumped.ScopeName

        // Refused before any guest code runs, rather than when the command line is installed:
        // the phase that installs it runs inside a tick, and this is the host's mistake, not
        // something the guest did.
        CommandLineArgsInit.validate exePath argv

        let state =
            IlMachineState.initial loggerFactory dotnetRuntimeDirs dumped
            |> fun s -> s.MapKernel (fun _ -> kernel)
            |> fun s ->
                match launch.PctSeed with
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
        let propertiesToSeed = AppContextProperties.withRuntimeBaseline launch.AppContext

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

        /// Put `Main` on the entry thread, to be given `mainArgs`, once class initialisation
        /// has returned.
        let installMain (mainArgs : ImmutableArray<CliType>) (state : IlMachineState) : IlMachineState =
            logger.LogInformation "Main method class now initialised"

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

            state

        /// Load the entry class, so that the class-initialisation phase has its `.cctor` to
        /// pump. Runs no guest instructions of its own — `loadClass` only pushes cctor frames.
        let enterClassInit
            (mainArgs : ImmutableArray<CliType>)
            (state : IlMachineState)
            : IlMachineState * StartupPhase
            =
            loadInitialState state, StartupPhase.InitialisingClasses (installMain mainArgs, entry.Returns)

        let atPhase (state : IlMachineState) (phase : StartupPhase) : RunningProgram =
            {
                State = state
                BaseClassTypes = baseClassTypes
                EntryThread = mainThread
                EntryFrame = EntryFrameKind.StartupCall phase
                LastRan = mainThread
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
