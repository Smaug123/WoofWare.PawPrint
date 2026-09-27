namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineStateExecution =
    let isAssignableFrom
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (objToCast : ConcreteTypeHandle)
        (possibleTargetType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state objToCast possibleTargetType

    /// `ConcreteVirtualDispatch.interfaceMapHandles` against the machine's type system.
    let interfaceMapHandles
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (receiverType : ConcreteTypeHandle)
        : IlMachineState * ConcreteTypeHandle list
        =
        let typeSystem, result =
            ConcreteVirtualDispatch.interfaceMapHandles
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                state.TypeSystem
                receiverType

        state.WithTypeSystem typeSystem, result

    /// `ConcreteVirtualDispatch.tryResolveVirtualImplementation` against the machine's type system,
    /// with the method it finds instantiated: `None` when nothing overrides the method named. Refuses where more than one default
    /// interface body is most specific, where the guest would see `AmbiguousImplementationException`,
    /// and where the type system does not model the dispatch.
    let tryResolveVirtualImplementation
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        let typeSystem, result =
            ConcreteVirtualDispatch.tryResolveVirtualImplementation
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                methodGenerics
                methodToCall
                dispatchTypeHandle
                walkBaseTypes
                state.TypeSystem

        match result with
        | VirtualImplementation.Found implementation ->
            let typeSystem, implementation, _ =
                MethodConcretisation.concretizeMethodWithAllGenerics
                    loggerFactory
                    state.DotnetRuntimeDirs
                    baseClassTypes
                    implementation.TypeGenerics
                    implementation.Definition
                    implementation.MethodGenerics
                    typeSystem

            state.WithTypeSystem typeSystem, Some implementation
        | VirtualImplementation.NotOverridden -> state.WithTypeSystem typeSystem, None
        | VirtualImplementation.Ambiguous candidates ->
            candidates
            |> List.map (fun m -> $"%s{MethodOwner.describe m.Owner}::%s{m.Name}")
            |> String.concat ", "
            // TODO: throw guest System.Runtime.AmbiguousImplementationException here.
            |> failwithf "multiple most-specific default interface implementations matched this virtual slot: %s"
        | VirtualImplementation.Unmodelled reason -> failwith reason

    /// How a call chooses the method it runs.
    [<RequireQualifiedAccess>]
    type CallDispatch =
        /// Run the named method.
        | Direct
        /// Run the implementation of the named virtual method that this receiver's runtime type
        /// selects. The receiver is the object the call passes as `this`. It is never null: a call
        /// site raises NullReferenceException for a null receiver before it dispatches.
        | Virtual of receiver : ManagedHeapAddress

    /// The dispatch for a call site that dispatches on its receiver `this`: `Virtual` when
    /// `methodToCall` dispatches virtually, and `Direct` otherwise. The caller must already have
    /// raised NullReferenceException for a null receiver; `site` names the call site in the failure
    /// when it has not, or when `this` is not an object reference at all.
    let dispatchOnReceiver
        (site : string)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (this : EvalStackValue)
        : CallDispatch
        =
        if not methodToCall.DispatchesVirtually then
            CallDispatch.Direct
        else

        match this with
        | EvalStackValue.ObjectRef receiver -> CallDispatch.Virtual receiver
        | EvalStackValue.NullObjectRef ->
            failwith
                $"BUG: %s{site}: virtual dispatch of %O{methodToCall} reached a null receiver; the call site must raise NullReferenceException before dispatching"
        | other ->
            failwith
                $"%s{site}: virtual dispatch of %O{methodToCall} needs an object reference as its receiver, but `this` is %O{other}"

    /// What `callMethodWithCommitment` actually did, for callers that must distinguish the cases.
    ///
    /// Initialising the callee's declaring type is the callee's own prologue, which runs after
    /// this function has pushed its frame — so every call commits, and the only question is
    /// whether it committed by running or by raising.
    [<RequireQualifiedAccess>]
    type CallCommitment =
        /// The call happened: a frame was pushed for the callee, or an intrinsic serviced it
        /// inline.
        ///
        /// This says the *call* took effect, not that the calling instruction is finished.
        /// Whether that instruction re-executes is the caller's own choice, made through
        /// `advanceProgramCounterOfCaller`.
        | Committed
        /// The callee raised instead of running: an exception constructor is now the active frame
        /// and dispatch follows. The calling instruction will not re-execute, and its arguments
        /// have already been consumed.
        | Raised
        /// The call was refused in a way the guest cannot catch, and the process is going down.
        /// No frame was pushed and nothing else will run on any thread.
        ///
        /// Distinct from `Raised` because there is no handler search to follow and no state the
        /// caller could usefully continue from: every caller must propagate rather than carry on.
        | Aborted of FatalError
        /// The callee is an intrinsic PawPrint performs itself, which reads its arguments, and one
        /// of them is undefined. Nothing was performed and no frame was pushed; as for `Aborted`,
        /// every caller must propagate, and the run ends.
        | UndefinedValueObserved of UndefinedValueObservation

    /// What a call site does to the thread on the way into its callee: whether it is still in
    /// cooperative mode when the callee's prologue runs.
    ///
    /// Read from the call site's signature; PawPrint does not model GC mode. This is the property
    /// CoreCLR's reverse-P/Invoke prologue actually tests, and so the thing that decides whether
    /// entering a `[UnmanagedCallersOnly]` method is the legal native transition or a fatal one.
    ///
    /// Keyed on the call site rather than the callee because the same method admits both:
    /// `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs` and
    /// `sourcesImpure/UnmanagedCallersOnlyManagedCalli.cs` are exactly that pair.
    [<RequireQualifiedAccess>]
    type CallSiteTransition =
        /// The thread leaves cooperative mode before the callee runs. Only a `calli` through a
        /// `delegate* unmanaged&lt;...&gt;` does this, and only when it does not suppress the
        /// transition. This is the entry a `[UnmanagedCallersOnly]` method admits.
        | EntersPreemptive
        /// The thread is still cooperative when the callee's prologue runs. Every managed call site
        /// — `call`, `callvirt`, a delegate's `Invoke`, reflection — and *also* an unmanaged one
        /// carrying `CallConvSuppressGCTransition`, which is an unmanaged calling convention that
        /// nevertheless skips the transition.
        ///
        /// That last case is why this is not simply "is the calling convention managed": measured,
        /// real .NET refuses a `delegate* unmanaged[SuppressGCTransition]&lt;int, int&gt;` entry into
        /// such a method with the same fatal error it gives a managed one, because the caller never
        /// left cooperative mode for the callee's prologue to find it in.
        | StaysCooperative

    [<RequireQualifiedAccess>]
    module CallSiteTransition =
        /// The namespace every calling-convention modifier lives in; CoreCLR's
        /// `CMOD_CALLCONV_NAMESPACE`, and the first thing it compares (callconvbuilder.cpp).
        let private callConvNamespace = "System.Runtime.CompilerServices"

        /// Is this custom modifier `CallConvSuppressGCTransition`?
        ///
        /// `resolveTypeDefName` names a modifier the signature gives as a TypeDef, which needs the
        /// owning module's tables. CoreCLR resolves both forms —
        /// `GetNameOfTypeRefOrDef(pModule, tk, ...)` — and *ignores* a modifier it cannot name
        /// rather than failing, so `None` lands on the same "not this one" as an unrelated
        /// modifier. Erroring here instead would crash on a legal call that merely carries a
        /// modifier we do not recognise.
        ///
        /// Same accepted risk as the rest of PawPrint's well-known-type matching: this compares
        /// namespace and name without checking that the type resolves to corelib's.
        let private isSuppressGcTransition
            (resolveTypeDefName : ResolvedTypeIdentity -> (string * string) option)
            (modifier : TypeDefn)
            : bool
            =
            let named =
                match modifier with
                | TypeDefn.FromReference (typeRef, _) -> Some (typeRef.Namespace, typeRef.Name)
                | TypeDefn.FromDefinition (identity, _) -> resolveTypeDefName identity
                | _ -> None

            match named with
            | Some (ns, name) -> ns = callConvNamespace && name = "CallConvSuppressGCTransition"
            | None -> false

        /// The whole signature, not just its header: `delegate* unmanaged[SuppressGCTransition]<...>`
        /// carries the *same* `Unmanaged` header as a plain `delegate* unmanaged<...>` and differs
        /// only by a `modopt` on the return type (measured: `09 01 08 08` against
        /// `09 01 20 49 08 08`), so a classifier reading the header alone would call the two the
        /// same thing.
        ///
        /// This follows CoreCLR's own algorithm rather than an approximation of it, because each
        /// place the two could differ is a call PawPrint would refuse and .NET would run:
        ///
        ///  * only the `Unmanaged` (0x09) header consults modifiers at all. A legacy header names
        ///    its convention outright, and `getUnmanagedCallConv` (jitinterface.cpp) returns it
        ///    without ever calling `TryGetUnmanagedCallingConventionFromModOpt`;
        ///  * only *optional* modifiers count. The parser skips required ones outright
        ///    (`if (!fIsOptional) continue;`, callconvbuilder.cpp), so a
        ///    `modreq(CallConvSuppressGCTransition)` suppresses nothing.
        let ofCallSiteSignature
            (resolveTypeDefName : ResolvedTypeIdentity -> (string * string) option)
            (signature : TypeMethodSignature<TypeDefn>)
            : CallSiteTransition
            =
            match signature.Header.Get.CallingConvention with
            | SignatureCallingConvention.Default
            | SignatureCallingConvention.VarArgs -> CallSiteTransition.StaysCooperative
            | SignatureCallingConvention.CDecl
            | SignatureCallingConvention.StdCall
            | SignatureCallingConvention.ThisCall
            | SignatureCallingConvention.FastCall -> CallSiteTransition.EntersPreemptive
            | SignatureCallingConvention.Unmanaged ->
                // Only the outermost run of modifiers describes the call site; one nested inside
                // the return type (`int32 modopt(X)[]`) is about that type, not the transition.
                let rec suppresses (ty : TypeDefn) : bool =
                    match ty with
                    | TypeDefn.Modified modified ->
                        (not modified.IsRequired
                         && isSuppressGcTransition resolveTypeDefName modified.Modifier)
                        || suppresses modified.Unmodified
                    | _ -> false

                match signature.ReturnType with
                // An unmodified void return carries no modifiers to inspect. A *modified* one
                // lands in `Returns` even when what it modifies is void, which is where the walk
                // above finds it.
                | MethodReturnType.Void -> CallSiteTransition.EntersPreemptive
                | MethodReturnType.Returns returnType ->
                    if suppresses returnType then
                        CallSiteTransition.StaysCooperative
                    else
                        CallSiteTransition.EntersPreemptive
            | other ->
                failwith
                    $"call site declares calling convention %O{other}, which is not one ECMA-335 II.23.2.3 admits for a method signature; refusing to guess whether the thread leaves cooperative mode"

    /// The fatal error that entering <paramref name="method"/> raises, or `None` when the entry is
    /// legal.
    ///
    /// A `[UnmanagedCallersOnly]` method may be entered only from native code. CoreCLR compiles one
    /// with `CORJIT_FLAG_REVERSE_PINVOKE`, whose prologue performs a reverse-P/Invoke transition
    /// asserting *preemptive* GC mode; a thread that is still cooperative trips
    /// `ReversePInvokeBadTransition` (dllimportcallback.cpp) and the process goes down uncatchably.
    ///
    /// One-directional deliberately: a *transitioning* entry into a method that is not
    /// `[UnmanagedCallersOnly]` is undefined behaviour in real .NET rather than a diagnosed error,
    /// so there is no answer to be faithful to and none is invented.
    ///
    /// This is a rule about *entering a method*, not about calling one, which is why it lives here
    /// rather than inside `callMethodWithCommitment`: a frame the VM installs directly is entered
    /// without any call instruction. Every caller must apply it before anything the callee could
    /// observe — real .NET refuses the entry *without* running the declaring type's static
    /// constructor, which `sourcesImpure/UnmanagedCallersOnlyCctorNotRun.cs` pins.
    ///
    /// The places a method gets entered, and what each does with this:
    /// <list type="bullet">
    /// <item><c>callMethodWithCommitment</c>, which every call instruction, delegate dispatch and
    /// reflective invoke passes through — applies it. A thread's delegate arrives this way too: the
    /// worker's bottom frame is CoreLib's <c>Thread.StartCallback</c>, whose
    /// <c>StartHelper.RunWorker</c> invokes the delegate with an ordinary <c>callvirt</c>, and
    /// <c>sourcesImpure/UnmanagedCallersOnlyThreadStart.cs</c> is that route;</item>
    /// <item>the guest's entry point, installed by <c>Program</c> — does not. Roslyn refuses to
    /// attribute one (CS8899), so no guest compiled from C# can present the shape; an image handed
    /// to PawPrint directly could, and what CoreCLR does with it is unmeasured. See
    /// docs/divergences.md;</item>
    /// <item><c>AppContextSeed</c>, <c>Thread.StartInternal</c> and <c>SignalDispatch</c>, which
    /// build frames directly — do not, because none lets the guest choose the method: the first two
    /// name BCL methods, and a signal handler takes a <c>PosixSignalContext</c>, whose
    /// non-blittability makes the attribute illegal on it (CS8894).</item>
    /// </list>
    let unmanagedCallersOnlyRefusal
        (transition : CallSiteTransition)
        (method : WoofWare.PawPrint.MethodInfo<'a, 'b, 'c>)
        : FatalError option
        =
        match transition with
        | CallSiteTransition.EntersPreemptive -> None
        | CallSiteTransition.StaysCooperative ->
            if MethodInfo.isUnmanagedCallersOnly method then
                {
                    Code = FatalErrorCode.ExecutionEngine
                    // CoreCLR's own wording, extended with the method it refused: the guest cannot
                    // observe either way, and a run that ends this way should say what ended it.
                    Message =
                        Some
                            $"Invalid Program: attempted to call a UnmanagedCallersOnly method from managed code. (%s{MethodOwner.describe method.Owner}::%s{method.Name})"
                }
                |> Some
            else
                None

    /// The first undefined value, deepest first, among the `count` evaluation-stack entries that
    /// lie `skip` entries below the top of `thread`'s stack: the operands a runtime-synthesised
    /// member uses, which its instruction's `OperandUse` entry cannot pick out.
    let undefinedAmongStackEntries
        (thread : ThreadId)
        (skip : int)
        (count : int)
        (state : IlMachineState)
        : UndefinedValue option
        =
        state.ThreadState.[thread].MethodState.EvaluationStack.Values
        |> List.skip skip
        |> List.truncate count
        |> List.rev
        |> List.tryPick EvalStackValue.tryFindUndefined

    /// End the step because the instruction this thread is positioned at would use `value`, an
    /// undefined value, in a way the instruction's `OperandUse` entry does not show and only its
    /// implementation discovers. `description` says what that use is.
    ///
    /// The state returned is irrelevant: `AbstractMachine` reports the run as ending at the state
    /// from before the instruction began.
    let observeUndefinedInInstruction
        (description : string)
        (value : UndefinedValue)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let methodState = state.ThreadState.[currentThread].MethodState

        let instruction =
            match methodState.ExecutingMethod.Body with
            | MethodBody.Il instructions ->
                match instructions.Locations.TryGetValue methodState.IlOpIndex with
                | true, op -> op
                | false, _ ->
                    failwith
                        $"logic error: %s{MethodOwner.describe methodState.ExecutingMethod.Owner}::%s{methodState.ExecutingMethod.Name} is positioned at IL_%04X{methodState.IlOpIndex}, which is not an instruction offset in that body"
            | body ->
                failwith
                    $"logic error: %s{description} of the undefined %O{value} was reported by an instruction, but %s{MethodOwner.describe methodState.ExecutingMethod.Owner}::%s{methodState.ExecutingMethod.Name}'s body is %O{body} rather than IL"

        let observation =
            {
                Value = value
                Use =
                    UndefinedValueUse.InstructionDetail (
                        methodState.ExecutingMethod,
                        methodState.IlOpIndex,
                        instruction,
                        description
                    )
            }

        state, WhatWeDid.UndefinedValueObserved observation

    /// What `OpcodeFaults` says the instruction this thread is positioned at may raise.
    ///
    /// Positioned at, not "has retired": the program counter advances only once an instruction has
    /// completed, so before that point this reads the instruction currently executing — which is
    /// the one whose faults are in question. A caller that has already advanced is asking about
    /// the wrong instruction.
    ///
    /// Fails if the thread is not executing IL at all. A native frame or a runtime-provided body
    /// has no `OpcodeFaults` entry, and silently answering `Unmodelled` for one would let a
    /// caller's check pass by accident rather than by being right.
    let faultsOfCurrentInstruction (currentThread : ThreadId) (state : IlMachineState) : OpcodeFaults =
        let methodState = state.ThreadState.[currentThread].MethodState

        match methodState.ExecutingMethod.Body with
        | MethodBody.Il instructions ->
            match instructions.Locations.TryGetValue methodState.IlOpIndex with
            | true, op -> OpcodeFaults.ofIlOp op
            | false, _ ->
                failwith
                    $"logic error: %s{MethodOwner.describe methodState.ExecutingMethod.Owner}::%s{methodState.ExecutingMethod.Name} is positioned at IL_%04X{methodState.IlOpIndex}, which is not an instruction offset in that body"
        | body ->
            failwith
                $"logic error: asked for the permitted faults of %s{MethodOwner.describe methodState.ExecutingMethod.Owner}::%s{methodState.ExecutingMethod.Name}, whose body is %O{body} rather than IL; only an instruction has an OpcodeFaults entry"

    /// What a call does once `callMethod` has offered it to PawPrint's own intrinsic
    /// implementations.
    [<RequireQualifiedAccess>]
    type private IntrinsicOutcome =
        /// An implementation handled the call.
        | Handled of IlMachineState * CallCommitment
        /// The call runs this method's IL: the callee's own, or the IL `IntrinsicBody.lower` puts
        /// in place of its placeholder.
        | RunIl of WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

    let rec callMethodWithCommitment
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (wasInitialising : ConcreteTypeHandle option)
        (wasConstructing : ConstructionState)
        (dispatch : CallDispatch)
        (wasClassConstructor : bool)
        (advanceProgramCounterOfCaller : bool)
        (callSiteTransition : CallSiteTransition)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (thread : ThreadId)
        (threadState : ThreadState)
        (callSiteIlOpIndexOverride : int option)
        (constructedObjectDisposition : ReturnValueDisposition)
        (exceptionEscape : ExceptionEscape)
        (state : IlMachineState)
        : IlMachineState * CallCommitment
        =
        let logger = loggerFactory.CreateLogger "CallMethod"

        let activeMethodState = threadState.MethodState

        // Virtual/interface resolution runs before the `[Intrinsic]` classification below, so
        // that `intrinsic` describes the method we are actually about to execute.
        let state, methodToCall =
            match dispatch with
            | CallDispatch.Direct -> state, methodToCall
            | CallDispatch.Virtual receiver ->
                if not methodToCall.DispatchesVirtually then
                    failwith
                        $"BUG: virtual dispatch requested for %O{methodToCall}, which does not dispatch virtually; build the dispatch with `dispatchOnReceiver`"

                // The receiver the caller dispatches on must be the `this` it passes.
                match
                    activeMethodState.EvaluationStack
                    |> EvalStack.PeekNthFromTop (MethodInfo.arity methodToCall)
                with
                | Some (EvalStackValue.ObjectRef this) when this = receiver -> ()
                | this ->
                    failwith
                        $"BUG: virtual dispatch of %O{methodToCall} on receiver %O{receiver}, but `this`, %d{MethodInfo.arity methodToCall} slot(s) down the evaluation stack, is %O{this}"

                let state, resolved =
                    tryResolveVirtualImplementation
                        loggerFactory
                        baseClassTypes
                        thread
                        methodGenerics
                        methodToCall
                        (ManagedHeap.getObjectConcreteType receiver state.ManagedHeap)
                        true
                        state

                state, resolved |> Option.defaultValue methodToCall

        // Keyed on the call site, not on the target alone -- the target is perfectly legal to
        // enter. `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs` calls this very method
        // through a `delegate* unmanaged<int, int>` and must keep working, and
        // `sourcesImpure/UnmanagedCallersOnlyManagedCalli.cs` is the same method reached by a
        // *managed* `calli`, which must not. When `Marshal.GetDelegateForFunctionPointer` lands, a
        // delegate wrapping such a method's native pointer is likewise a host-initiated entry and
        // must be given a transitioning call site rather than arriving here as cooperative.
        //
        // Before the callee's class initialiser is armed, and before anything else the callee could
        // observe; see `unmanagedCallersOnlyRefusal`.
        match unmanagedCallersOnlyRefusal callSiteTransition methodToCall with
        | Some fatal -> state, CallCommitment.Aborted fatal
        | None ->

        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringAssemblyFullName with
            | Some assy -> assy
            | None ->
                failwith
                    $"CallMethod: declaring assembly for %O{methodToCall} is not loaded: %O{methodToCall.DeclaringAssemblyFullName}"

        // Whether the method about to run is a JIT intrinsic, as `IntrinsicBody.isIntrinsic` reads
        // it off that method and its declaring type: after virtual resolution, so that
        // `callvirt ICloneable::Clone()` is recognised as `Array::Clone`.
        //
        // A method the runtime synthesised is never an intrinsic, and there is no TypeDef row to
        // ask about one: CoreCLR never intrinsic-classifies synthesised code, and this also covers
        // the struct-marshal stub, whose owner is the type being *marshalled*.
        //
        // `[Intrinsic]` on an abstract method is a JIT inlining hint for the call site only: there
        // is no body to implement or to run. Virtual resolution has already run above, so this
        // matters only where it was skipped or found no implementation.
        let intrinsic : (MethodDefinitionHandle * IntrinsicMethodKeys.IntrinsicMethodKey) option =
            match methodToCall with
            | MethodInfo.Synthesised _ -> None
            | MethodInfo.Metadata (_, facts) ->
                match methodToCall.Body with
                | MethodBody.Abstract -> None
                | MethodBody.Il _
                | MethodBody.InternalCall
                | MethodBody.PInvoke
                | MethodBody.RuntimeProvided _ ->
                    if IntrinsicBody.isIntrinsic declaringAssy facts.Handle then
                        Some (facts.Handle, Intrinsics.methodKey state methodToCall)
                    else
                        None

        let intrinsicKey : IntrinsicMethodKeys.IntrinsicMethodKey option =
            intrinsic |> Option.map snd

        // `static T Activator.CreateInstance<T>()` is marked `[Intrinsic]` because the JIT inlines it
        // to an allocate+ctor sequence. The managed IL bottoms out in InternalCalls
        // (`RuntimeType.CreateInstanceOfT`, `CallDefaultStructConstructor`) we don't model, so we
        // implement the high-level intrinsic semantics directly: for a value type T, push `default(T)`
        // (skipping any explicit parameterless struct ctor for now — see TODO); for a reference type T,
        // allocate the object and run its parameterless ctor by recursing through `callMethod`.
        // See https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Activator.RuntimeType.cs#L137-L160
        // (`CreateInstanceOfT` and `CallDefaultStructConstructor` are RuntimeType.CoreCLR.cs#L4028 and #L4056.)
        //
        // Exception wrapping:
        //  - CoreCLR's `CreateInstanceOfT` wraps any exception thrown by the recursed ctor in a
        //    `TargetInvocationException`. We can't observe that in a separate Activator frame
        //    because we inline the intrinsic, so the recursive `callMethod` for the ctor sets
        //    `ExceptionEscape.WrapInTargetInvocation` on the ctor frame's `ReturnState`. Exception
        //    dispatch treats that as a boundary its first pass cannot see past — the wrap
        //    changes the exception's *type*, so outer frames must be searched against the wrapper
        //    — and its second pass, on reaching the ctor frame, pops it, synthesises a fresh
        //    `TargetInvocationException` with the original exception as `_innerException`, and
        //    starts a new search from the caller. A try/catch *inside* the ctor that handles the
        //    exception is unaffected, matching CoreCLR.
        //
        // Intentional divergence (see docs/divergences.md):
        //  - For `BeforeFieldInit` reference types, CoreCLR defers the type initializer past the
        //    Activator allocation/ctor pair. PawPrint's `newobj` (UnaryMetadataObjectOps.fs:240)
        //    runs cctor eagerly on every instance creation regardless of the flag, so this
        //    intrinsic follows the same convention. ECMA-335 II.10.5.3.2 permits eager schedules.
        let tryHandleActivatorCreateInstance () : (IlMachineState * CallCommitment) option =
            // A synthesised method has no key, and is not `Activator.CreateInstance` whatever else
            // it is.
            match intrinsicKey with
            | None -> None
            | Some intrinsicKey ->

            if
                AssemblyDefinitionName.isNamed "System.Private.CoreLib" intrinsicKey.DeclaringAssemblyFullName
                && intrinsicKey.DeclaringTypeFullName = "System.Activator"
                && intrinsicKey.MethodName = "CreateInstance"
                && List.isEmpty intrinsicKey.ParameterShapes
                && methodToCall.Generics.Length = 1
            then
                let tHandle = methodToCall.Generics.[0]

                // Determine whether T is a value type BEFORE running its cctor: CoreCLR's
                // `Activator.CreateInstance<T>()` for a value type without an explicit parameterless
                // ctor returns `default(T)` and does NOT trigger T's static constructor. We must not
                // observe cctor side effects on that path. The ref-type path picks up cctor naturally
                // via the recursive `callMethod` for the .ctor.
                let isValueType, typeDefOpt =
                    match tHandle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ ->
                        failwith
                            $"Activator.CreateInstance<T>() requires T to satisfy `new()`, but T has handle %O{tHandle}"
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Arrays are reference types but their construction is special; defer.
                        false, None
                    | ConcreteTypeHandle.Concrete _ ->
                        match IlMachineState.tryGetConcreteTypeInfo state tHandle with
                        | Some (_, typeInfo) ->
                            LoadedTypeInfo.isValueType baseClassTypes state.TypeSystem._LoadedAssemblies typeInfo,
                            Some typeInfo
                        | None ->
                            failwith
                                $"Activator.CreateInstance<T>(): concrete type handle %O{tHandle} has no TypeDef row"

                if isValueType then
                    match typeDefOpt with
                    | Some typeDef ->
                        let hasExplicitParameterlessCtor =
                            typeDef.Methods
                            |> List.exists (fun m -> m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0)

                        if hasExplicitParameterlessCtor then
                            failwith
                                $"TODO: Activator.CreateInstance<T>() for value type %s{typeDef.Namespace}.%s{typeDef.Name} with an explicit parameterless ctor is not yet implemented (CoreCLR runs it via CallDefaultStructConstructor, including running the cctor)"
                    | None -> failwith "Activator.CreateInstance<T>(): value-type branch without typeDef"

                    let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes tHandle

                    let state = state |> IlMachineState.pushToEvalStack zero thread

                    let state =
                        if advanceProgramCounterOfCaller then
                            IlMachineState.advanceProgramCounter thread state
                        else
                            state

                    // Serviced inline: the zero value is already on the caller's stack.
                    Some (state, CallCommitment.Committed)
                else

                match tHandle with
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    failwith $"TODO: Activator.CreateInstance<T>() for array type %O{tHandle} is not yet implemented"
                | _ -> ()

                let typeDef =
                    match typeDefOpt with
                    | Some typeDef -> typeDef
                    | None -> failwith "Activator.CreateInstance<T>(): reference-type branch without typeDef"

                // Validate T BEFORE running its cctor. CoreCLR rejects abstract types and types
                // without a public parameterless ctor in `RuntimeType.CreateInstanceOfT` /
                // ActivatorCache construction, before any class-init side effects are observable.
                // Running `ensureTypeInitialised` first would let a throwing `.cctor` mask the
                // `MissingMethodException` users actually expect — empirically verified against
                // .NET 10.
                if typeDef.TypeAttributes.HasFlag TypeAttributes.Abstract then
                    // CoreCLR's MissingMethodException carries the message
                    // "Cannot dynamically create an instance of type 'X'. Reason: Cannot create
                    // an abstract class." (verified against .NET 10).
                    failwith
                        $"TODO: Activator.CreateInstance<T>() should throw MissingMethodException because T = %s{typeDef.Namespace}.%s{typeDef.Name} is abstract"

                // CoreCLR's `CreateInstanceOfT` consults `ActivatorCache.CtorIsPublic` and throws
                // `MissingMethodException` if the parameterless ctor is non-public — see
                // RuntimeType.CoreCLR.cs:4034. Filter accordingly so an internal/private ctor is
                // not silently invoked.
                let isPublic (m : MethodInfo<_, _, _>) : bool = m.IsPublic

                let ctor =
                    typeDef.Methods
                    |> List.tryFind (fun m ->
                        m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0 && isPublic m
                    )

                match ctor with
                | None ->
                    // CoreCLR throws MissingMethodException here. We don't yet have a host helper
                    // to raise that, so fail loudly with the precise condition.
                    let hasNonPublicParameterless =
                        typeDef.Methods
                        |> List.exists (fun m ->
                            m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0 && not (isPublic m)
                        )

                    let reason =
                        if hasNonPublicParameterless then
                            "its parameterless instance constructor is non-public"
                        else
                            "it has no parameterless instance constructor"

                    failwith
                        $"TODO: Activator.CreateInstance<T>() should throw MissingMethodException because T = %s{typeDef.Namespace}.%s{typeDef.Name} %s{reason}"
                | Some ctor ->

                let ct =
                    AllConcreteTypes.lookup tHandle state.TypeSystem.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"Activator.CreateInstance<T>(): concrete type handle %O{tHandle} not found in AllConcreteTypes"
                    )

                // CoreCLR's `CreateInstanceOfT` catches *every* exception escaping the
                // cache.CallRefConstructor path — including a `TypeInitializationException`
                // raised by T's `.cctor` — and rethrows it wrapped in `TargetInvocationException`.
                // `ExceptionEscape.WrapInTargetInvocation` on T's ctor frame below is the whole
                // of that: T's initialisation happens in that frame's prologue, so a `.cctor`
                // failure — running for the first time or cached from an earlier one — unwinds
                // through the ctor frame and meets the wrap on its way out, and so does anything
                // the ctor body itself throws.
                let state, concretizedCtor, declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        loggerFactory
                        baseClassTypes
                        ct.Generics
                        ctor
                        ImmutableArray.Empty
                        state

                let state, fields =
                    IlMachineState.buildInstanceStorage loggerFactory baseClassTypes state declaringTypeHandle

                let allocatedAddr, state =
                    IlMachineState.allocateManagedObject declaringTypeHandle fields state

                let state =
                    state
                    |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocatedAddr)) thread

                let threadState = state.ThreadState.[thread]

                callMethod
                    loggerFactory
                    baseClassTypes
                    None
                    (ConstructionState.Constructing allocatedAddr)
                    CallDispatch.Direct
                    false
                    advanceProgramCounterOfCaller
                    concretizedCtor.Generics
                    concretizedCtor
                    thread
                    threadState
                    None
                    ReturnValueDisposition.PushToCaller
                    ExceptionEscape.WrapInTargetInvocation // mirror CreateInstanceOfT
                    state
                // T's ctor frame is pushed; the activator call itself is done.
                |> fun state -> Some (state, CallCommitment.Committed)
            else
                None

        // An intrinsic's result as the call's outcome, or `None` where the intrinsic declined.
        let handled (result : IntrinsicResult) : IntrinsicOutcome option =
            match result with
            | IntrinsicResult.Completed result -> Some (IntrinsicOutcome.Handled (result, CallCommitment.Committed))
            | IntrinsicResult.RaiseException (state, exnType, message) ->
                // The intrinsic described an exception rather than raising it, because it
                // cannot see `raiseRuntimeException` (compile order) and because raising it
                // here is what makes an *unhandled* one expressible: `raiseRuntimeException`
                // defers dispatch to the ctor's `Ret`, which can report
                // `ExecutionResult.UnhandledException`. The intrinsic has deliberately not
                // advanced the PC, so dispatch sees the faulting instruction's offset.
                // `WhatWeDid` is always `Executed` here — the ctor frame is now the active
                // frame, exactly as for an opcode-manufactured exception.
                raiseRuntimeExceptionWithMessage loggerFactory baseClassTypes exnType message thread state
                |> fst
                |> fun state -> Some (IntrinsicOutcome.Handled (state, CallCommitment.Raised))
            // Whatever the intrinsic did before it found the value is discarded: the state from
            // before the call is the one to report.
            | IntrinsicResult.UndefinedValueObserved observation ->
                Some (IntrinsicOutcome.Handled (state, CallCommitment.UndefinedValueObserved observation))
            | IntrinsicResult.Unrecognised -> None

        // An intrinsic PawPrint performs itself reads its arguments, which are still on the
        // caller's stack. Refused before any is tried, so that none of them can see an undefined
        // value; one that declines runs IL that would only have moved it, which makes this
        // stricter than it needs to be for those.
        let undefinedIntrinsicArgument : (int * UndefinedValue) option =
            match intrinsic with
            | None -> None
            | Some _ ->
                let argumentCount =
                    MethodInfo.arity methodToCall + (if methodToCall.IsStatic then 0 else 1)

                activeMethodState.EvaluationStack.Values
                |> List.truncate argumentCount
                |> List.rev
                |> List.indexed
                |> List.tryPick (fun (index, value) ->
                    EvalStackValue.tryFindUndefined value |> Option.map (fun u -> index, u)
                )

        match undefinedIntrinsicArgument with
        | Some (index, value) ->
            let observation =
                {
                    Value = value
                    Use = UndefinedValueUse.RuntimeArgument (methodToCall, index)
                }

            state, CallCommitment.UndefinedValueObserved observation
        | None ->

        let outcome =
            match intrinsic with
            | None -> IntrinsicOutcome.RunIl methodToCall
            | Some (handle, key) ->
                match tryHandleActivatorCreateInstance () with
                | Some result -> IntrinsicOutcome.Handled result
                | None ->

                let performPrimitive (primitive : IntrinsicPrimitive) : IntrinsicOutcome =
                    match
                        Intrinsics.performPrimitive
                            loggerFactory
                            baseClassTypes
                            primitive
                            methodToCall
                            thread
                            advanceProgramCounterOfCaller
                            state
                        |> handled
                    with
                    | Some outcome -> outcome
                    | None -> failwith $"BUG: Intrinsics.performPrimitive did not perform %A{primitive}"

                match
                    Intrinsics.call
                        loggerFactory
                        baseClassTypes
                        wasConstructing
                        methodToCall
                        thread
                        advanceProgramCounterOfCaller
                        state
                    |> handled
                with
                | Some outcome -> outcome
                | None ->
                    // PawPrint has no implementation of its own, so the call gets what CoreCLR runs
                    // when its JIT does not expand the call: the method's IL, except that a
                    // placeholder's call to itself is what the JIT's expansion does on this run's
                    // CPU, and a VM-substituted body is the IL CoreCLR's VM runs instead. A body
                    // with no IL is already PawPrint's implementation of the method --
                    // `NativeDispatch` for an InternalCall or P/Invoke, delegate or accessor
                    // dispatch for a runtime-provided body -- so the call proceeds to it as though
                    // unmarked.
                    //
                    // `StackShapeOfMethod` finds a substituted body through
                    // `IntrinsicBody.substitutedBody`, which decides as this does.
                    match IntrinsicBody.classify declaringAssy handle with
                    | IntrinsicBody.OwnIl
                    | IntrinsicBody.NoIl -> IntrinsicOutcome.RunIl methodToCall
                    | IntrinsicBody.JitExpansion expansion ->
                        // `gtIsRecursiveCall`: only the method's call to itself is must-expand. A
                        // call from anywhere else, a delegate's included, runs the method's IL,
                        // which reaches that call.
                        if not (MethodInfo.NominallyEqual activeMethodState.ExecutingMethod methodToCall) then
                            IntrinsicOutcome.RunIl methodToCall
                        else

                        match IntrinsicBody.expandSelfCall state.HardwareIntrinsics expansion with
                        | SelfCallExpansion.Constant value ->
                            if MethodInfo.arity methodToCall <> 0 || not methodToCall.IsStatic then
                                failwith
                                    $"%s{Intrinsics.formatMethodKey key}: a capability query must be a static method with no parameters"

                            let state = IlMachineState.pushToEvalStack (CliType.ofBool value) thread state

                            let state =
                                if advanceProgramCounterOfCaller then
                                    IlMachineState.advanceProgramCounter thread state
                                else
                                    state

                            IntrinsicOutcome.Handled (state, CallCommitment.Committed)
                        | SelfCallExpansion.ThrowPlatformNotSupported ->
                            if not methodToCall.IsStatic then
                                failwith
                                    $"%s{Intrinsics.formatMethodKey key}: a hardware instruction must be a static method"

                            let state =
                                (state, [ 1 .. MethodInfo.arity methodToCall ])
                                ||> List.fold (fun state _ -> IlMachineState.popEvalStack thread state |> snd)

                            let helper =
                                declaringAssy.Methods.[IntrinsicBody.platformNotSupportedHelper declaringAssy]

                            let state, helper, _ =
                                ExecutionConcretization.concretizeMethodWithAllGenerics
                                    loggerFactory
                                    baseClassTypes
                                    ImmutableArray.Empty
                                    helper
                                    ImmutableArray.Empty
                                    state

                            // The helper throws, so its frame never returns to advance the caller.
                            callMethod
                                loggerFactory
                                baseClassTypes
                                None
                                ConstructionState.NotConstructing
                                CallDispatch.Direct
                                false
                                advanceProgramCounterOfCaller
                                helper.Generics
                                helper
                                thread
                                state.ThreadState.[thread]
                                None
                                ReturnValueDisposition.PushToCaller
                                ExceptionEscape.Propagate
                                state
                            |> fun state -> IntrinsicOutcome.Handled (state, CallCommitment.Committed)
                        | SelfCallExpansion.Primitive primitive -> performPrimitive primitive
                        | SelfCallExpansion.HardwareInstruction _
                        | SelfCallExpansion.Unrecognised as expanded ->
                            failwith
                                $"TODO: implement JIT intrinsic %s{Intrinsics.formatMethodKey key} in Intrinsics.call: its IL calls itself, which is CoreCLR's placeholder for a body its JIT must expand, and on this CPU that call is %A{expanded}"
                    | IntrinsicBody.VmSubstitution ->
                        match VmSubstitution.unsafeStub declaringAssy handle with
                        | Some stub ->
                            let stub = MethodInstructions.setLocalVars<TypeDefn, ConcreteTypeHandle> None stub

                            methodToCall
                            |> MethodInfo.setMethodVars (MethodBody.Il stub) methodToCall.Signature
                            |> IntrinsicOutcome.RunIl
                        | None ->

                        // A substitution that depends on the method's instantiation, which the VM
                        // makes for the whole body, so it is performed at any call.
                        match Intrinsics.primitiveOf state methodToCall with
                        | Some primitive -> performPrimitive primitive
                        | None ->
                            failwith
                                $"TODO: implement JIT intrinsic %s{Intrinsics.formatMethodKey key} in Intrinsics.call: CoreCLR's VM substitutes its body, and the IL CoreLib ships in its place cannot return"

        match outcome with
        | IntrinsicOutcome.Handled (state, commitment) -> state, commitment
        | IntrinsicOutcome.RunIl methodToCall ->

        // Get zero values for all parameters.
        //
        // These are the coercion targets for the popped arguments below, and they are
        // deliberately derived from the `methodToCall` post-resolution — i.e. the body we are
        // about to execute, not the declaration named at the call site. The two
        // differ under `in`-variance: dispatching `IContravariant<string>::Set(string)` selects
        // a body declaring `Set(object)`, and the argument must be coerced to the body's
        // parameter type. `thisArgCoercionTarget` and `createNewFrame` below share that basis.
        let state, argZeroObjects =
            ((state, []), methodToCall.Signature.ParameterTypes)
            ||> List.fold (fun (state, zeros) tyHandle ->
                let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes tyHandle
                state, zero :: zeros
            )

        let argZeroObjects = List.rev argZeroObjects

        // Helper to pop and coerce a single argument
        let popAndCoerceArg zeroType methodState =
            let value, newState = MethodState.popFromStack methodState
            EvalStackValue.toArgumentCoerced zeroType value, newState

        let thisArgCoercionTarget
            (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            : CliType
            =
            let declaringAssembly =
                state.LoadedAssembly (methodToCall.DeclaringAssemblyFullName) |> Option.get

            let declaringType =
                declaringAssembly.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

            if LoadedTypeInfo.isValueType baseClassTypes state.TypeSystem._LoadedAssemblies declaringType then
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
            else
                CliType.ObjectRef None

        // Pop exactly the method's declared parameters, leaving no `this` slot in the
        // resulting `Arguments` array.
        let popDeclaredParametersOnly () =
            let args = ImmutableArray.CreateBuilder (MethodInfo.arity methodToCall)
            let mutable currentState = activeMethodState

            for i = MethodInfo.arity methodToCall - 1 downto 0 do
                let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                args.Add arg
                currentState <- newState

            args.Reverse ()
            args.ToImmutable (), currentState

        // Collect arguments based on calling convention
        let args, afterPop =
            if methodToCall.IsStatic then
                popDeclaredParametersOnly ()
            else

            match wasConstructing with
            | ConstructionState.Constructing _ ->
                // Instance method: handle `this` pointer
                let argCount = MethodInfo.arity methodToCall
                let args = ImmutableArray.CreateBuilder (argCount + 1)
                let mutable currentState = activeMethodState
                let thisArgTarget = thisArgCoercionTarget methodToCall

                // Constructor: `this` is on top of stack, by our own odd little calling convention
                // where Newobj puts the object pointer on top
                let thisArg, newState = popAndCoerceArg thisArgTarget currentState

                currentState <- newState

                // Pop remaining args in reverse
                for i = argCount - 1 downto 0 do
                    let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                    args.Add arg
                    currentState <- newState

                args.Add thisArg
                args.Reverse ()
                args.ToImmutable (), currentState
            | ConstructionState.NotConstructing ->
                // Instance method: handle `this` pointer
                let argCount = MethodInfo.arity methodToCall
                let args = ImmutableArray.CreateBuilder (argCount + 1)
                let mutable currentState = activeMethodState
                let thisArgTarget = thisArgCoercionTarget methodToCall

                // Regular instance method: args then `this`
                for i = argCount - 1 downto 0 do
                    let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                    args.Add arg
                    currentState <- newState

                let thisArg, newState =
                    let rawThis, newState = MethodState.popFromStack currentState

                    let coerced =
                        match thisArgTarget, rawThis with
                        | CliType.RuntimePointer _, EvalStackValue.ObjectRef addr ->
                            // Boxed value type receiver: implicit unbox to managed pointer
                            // into the heap object's value data.
                            CliType.RuntimePointer (
                                CliRuntimePointer.Managed (
                                    ManagedPointerSource.Byref
                                        {
                                            Root = ByrefRoot.HeapValue addr
                                            Projections = []
                                        }
                                )
                            )
                        | _ -> EvalStackValue.toCliTypeCoerced thisArgTarget rawThis

                    coerced, newState

                args.Add thisArg
                currentState <- newState

                args.Reverse ()
                args.ToImmutable (), currentState

        // Helper to create new frame with assembly loading
        let rec createNewFrame state =
            let returnInfo =
                Some
                    {
                        JumpTo = threadState.ActiveMethodState
                        WasInitialisingType = wasInitialising
                        Constructing = wasConstructing
                        CallSiteIlOpIndex = callSiteIlOpIndexOverride |> Option.defaultValue afterPop.IlOpIndex
                        ReturnValueDisposition = constructedObjectDisposition
                        ExceptionEscape = exceptionEscape
                    }

            match
                MethodState.Empty
                    state.TypeSystem.ConcreteTypes
                    baseClassTypes
                    state.TypeSystem._LoadedAssemblies
                    declaringAssy
                    methodToCall
                    methodGenerics
                    args
                    returnInfo
            with
            | Ok frame -> state, frame
            | Error toLoad ->
                let state' =
                    (state, toLoad)
                    ||> List.fold (fun s (asmRef : WoofWare.PawPrint.AssemblyReference) ->
                        let s, _, _ =
                            IlMachineState.loadAssembly
                                loggerFactory
                                (state.LoadedAssembly methodToCall.DeclaringAssemblyFullName |> Option.get)
                                (fst asmRef.Handle)
                                s

                        s
                    )

                createNewFrame state'

        let state, newFrame = createNewFrame state

        // The callee's prologue. Recorded on the frame rather than run here, and asked *after*
        // virtual resolution, so it names the type whose method actually runs: measured on
        // .NET 10, `callvirt IFace::M` resolving to `Impl.M` never runs `IFace`'s own
        // initialiser. A `.cctor` reached from here therefore unwinds through this frame and its
        // `TypeInitializationException` names this method, which is what the CLR reports.
        //
        // Only the calls ECMA-335 II.10.5.3.1 names as triggers arm one: a static method, an
        // instance constructor, or any instance method of a value type. An instance method call on
        // a reference-type object that already exists is not a trigger, and the difference is
        // observable — measured on .NET 10, an instance published by a `.cctor` that then threw
        // still answers a virtual call, while constructing another of the same type throws
        // `TypeInitializationException`. Arming every metadata method fails the first;
        // arming only statics and constructors fails the value-type clause.
        //
        // A `.cctor` frame is exempt too: it *is* the initialisation, and asking again would see
        // its own type in progress. `loadClass` answers `NothingToDo` for that, so this is an
        // optimisation rather than a correctness guard — but it keeps the invariant "a frame with
        // a pending init has not started" true of every frame that has one.
        let newFrame =
            if wasClassConstructor then
                newFrame
            else

            // The synthesised arm comes first, and asks its question *without* looking the
            // declaring type up: a method minted by `Reflection.Emit` is owned by a class with
            // no TypeDef row, so running the lookup below first would crash on every
            // dynamic-method call.
            match methodToCall with
            | MethodInfo.Synthesised (_, kind) ->
                if SynthesisedMethod.initialisesDeclaringType kind then
                    // No synthesised kind answers `true` today. When one does, it will need a
                    // declaring type to initialise, and this is where to look it up — separately
                    // from the metadata arm, because "which type does this synthesised method
                    // initialise" is a question about its semantics rather than about its owner.
                    failwith
                        $"TODO: %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} is a synthesised method whose kind claims to initialise its declaring type, but no path yet resolves which type that is"
                else
                    newFrame
            | MethodInfo.Metadata _ ->

            let handle =
                match
                    AllConcreteTypes.findExistingConcreteType
                        state.TypeSystem.ConcreteTypes
                        methodToCall.RequiredDeclaringType.Identity
                        methodToCall.DeclaringTypeGenerics
                with
                | Some handle -> handle
                | None ->
                    failwith
                        $"calling %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name}: the resolved method's declaring type is not registered in AllConcreteTypes, so its initialiser cannot be scheduled"

            let initialises =
                match methodToCall.Body with
                | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor _)
                | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessorInvalidKind _) ->
                    // CoreCLR binds an accessor's target while compiling its stub, which is before
                    // the method's prologue, so a declaration that fails to bind raises without
                    // its declaring type having been initialised. `UnsafeAccessorDispatch.execute`
                    // therefore runs this frame's initialisation itself, after binding.
                    false
                | _ ->

                if methodToCall.IsStatic then
                    true
                elif methodToCall.Name = ".ctor" then
                    // Identified by name, as elsewhere in the codebase. `wasConstructing` would be
                    // the wrong question: a derived constructor chaining to `base..ctor()` is not
                    // constructing a fresh object and yet does trigger the base type's
                    // initialiser — measured on .NET 10, `new Derived()` runs `Derived..cctor` and
                    // then `Base..cctor`, the latter from that chained call's own prologue.
                    true
                else
                    // An instance method of a *value type* is a trigger in its own right, and the
                    // only instance-method shape where that is observable: a class instance
                    // implies its constructor chain ran, and construction is itself a trigger,
                    // whereas `default(S)` runs nothing.
                    // `DelegateToValueTypeInstanceMethodRunsCctor.cs` is the case.
                    AllConcreteTypes.tryIsValueType
                        baseClassTypes
                        state.TypeSystem._LoadedAssemblies
                        state.TypeSystem.ConcreteTypes
                        handle
                    |> Option.defaultValue false

            if initialises then
                newFrame |> MethodState.withPendingTypeInit handle
            else
                newFrame

        let oldFrame =
            if wasClassConstructor || not advanceProgramCounterOfCaller then
                afterPop
            else
                afterPop |> MethodState.advanceProgramCounter

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState oldFrame threadState

        let calleeFrameId, threadState = ThreadState.appendFrame newFrame threadState
        let newThreadState = ThreadState.setActiveFrame calleeFrameId threadState

        // The callee's frame is now active and the caller's PC has been advanced (unless the
        // caller asked otherwise): the call has happened.
        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        },
        CallCommitment.Committed

    /// `callMethodWithCommitment` for the callers that do not need to distinguish whether the call
    /// committed by running or by raising: in both cases the returned state already reflects what
    /// happened, so there is nothing left to decide.
    ///
    /// Only for a call site whose target cannot be *refused*. An abort has nowhere to go in this
    /// return type, so it is a loud failure rather than a silently dropped outcome; a call site
    /// that can name a refusable target must use `callMethodWithCommitment` and propagate
    /// `CallCommitment.Aborted`, as `call`, `callvirt` and `calli` do.
    and callMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (wasInitialising : ConcreteTypeHandle option)
        (wasConstructing : ConstructionState)
        (dispatch : CallDispatch)
        (wasClassConstructor : bool)
        (advanceProgramCounterOfCaller : bool)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (thread : ThreadId)
        (threadState : ThreadState)
        (callSiteIlOpIndexOverride : int option)
        (constructedObjectDisposition : ReturnValueDisposition)
        (exceptionEscape : ExceptionEscape)
        (state : IlMachineState)
        : IlMachineState
        =
        callMethodWithCommitment
            loggerFactory
            baseClassTypes
            wasInitialising
            wasConstructing
            dispatch
            wasClassConstructor
            advanceProgramCounterOfCaller
            // Every caller of this wrapper is interpreter-internal machinery entering a method the
            // ordinary managed way: a class constructor, a delegate's constructor, a helper the
            // interpreter itself decided to run. None of them leaves cooperative mode, so a target
            // carrying `[UnmanagedCallersOnly]` is refused here just as it would be at a `call` --
            // and the wrapper's own guard below then fails loudly, because such a caller has no way
            // to propagate the abort.
            CallSiteTransition.StaysCooperative
            methodGenerics
            methodToCall
            thread
            threadState
            callSiteIlOpIndexOverride
            constructedObjectDisposition
            exceptionEscape
            state
        |> function
            | state, CallCommitment.Committed
            | state, CallCommitment.Raised -> state
            | _, CallCommitment.UndefinedValueObserved observation ->
                // As for an abort below: this wrapper serves the interpreter's own entries, and has
                // nowhere to put the end of the run.
                failwith
                    $"TODO: a call made through `callMethod` would have performed an intrinsic on an undefined argument (%O{observation}), and this wrapper has no way to end the run"
            | _, CallCommitment.Aborted fatal ->
                // This wrapper's return type has nowhere to put an abort, and dropping one would
                // let the caller carry on against a state whose process has already died. Its
                // callers all name a constructor, a class initialiser, or a specific BCL method,
                // and no guest that compiles can point any of those at a `[UnmanagedCallersOnly]`
                // method: C# admits the attribute only on ordinary method declarations (CS0592
                // rejects it on a static constructor), and the BCL targets are ours to choose.
                //
                // So reaching here means metadata PawPrint has no answer for, not a wrong call-site
                // choice — and CoreCLR's behaviour when its *own* machinery enters such a method is
                // not something we have been able to measure. Refuse rather than guess; see
                // docs/divergences.md, "`[UnmanagedCallersOnly]` declarations and unmanaged call
                // sites are not validated".
                let message = fatal.Message |> Option.defaultValue "<no message>"

                failwith
                    $"a call made through `callMethod` aborted the process (%O{fatal.Code}: %s{message}). PawPrint cannot say what should happen here: this wrapper serves the interpreter's own entries — class initialisers, constructors, chosen BCL helpers — and it is unmeasured whether CoreCLR applies the reverse-P/Invoke transition to those at all. A guest cannot produce this; hand-authored metadata can"

    and loadClass
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : ConcreteTypeHandle)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : StateLoadResult
        =
        let logger = loggerFactory.CreateLogger "LoadClass"

        match TypeInitTable.tryGet ty state.TypeInitTable with
        | Some TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.Failed (tieAddr, tieType)) ->
            // The .cctor previously threw. Per ECMA-335, subsequent access should throw
            // TypeInitializationException. We rethrow the *same* cached instance to match
            // CLR identity semantics (ReferenceEquals across repeated accesses), clearing
            // whatever trace its previous raise left as CoreCLR's `DoRunClassInitThrowing` does.
            let state =
                ExceptionDispatching.clearStackTraceForThrow baseClassTypes tieAddr state

            match
                ExceptionDispatching.throwExceptionObject
                    loggerFactory
                    baseClassTypes
                    state
                    currentThread
                    tieAddr
                    tieType
            with
            | ExceptionDispatchResult.Dispatched state -> StateLoadResult.ThrowingTypeInitializationException state
            | ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
                StateLoadResult.UnhandledTypeInitializationException (state, exn)
        | Some (TypeInitState.InProgress tid) when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress blocker) ->
            if ThreadState.classInitWaitChainReaches currentThread blocker state.ThreadState then
                // `blocker` is (transitively) waiting for a type whose initialisation this thread
                // owns, so parking here would deadlock. ECMA-335 II.10.5.3.3 step 2.2.2: proceed
                // and see the type as `blocker` has left it so far, exactly as for the
                // same-thread re-entry above.
                StateLoadResult.NothingToDo state
            else
                // Another thread owns this type's .cctor lock. Surface the blocker so the caller
                // can translate to `WhatWeDid.BlockedOnClassInit blocker`; the scheduler then
                // parks this thread until `blocker` makes progress or its cctor fails. We
                // deliberately do not touch `state` (no WithTypeBeginInit, no PC advance): on
                // wake-up the caller retries the same opcode and re-enters loadClass to observe
                // the new TypeInitTable entry.
                StateLoadResult.Blocked (state, blocker)
        | None ->
            // We have work to do!

            // Look up the concrete type from the handle
            let concreteType =
                match AllConcreteTypes.lookup ty state.TypeSystem.ConcreteTypes with
                | Some ct -> ct
                | None -> failwith $"ConcreteTypeHandle {ty} not found in ConcreteTypes mapping"

            let sourceAssembly =
                state.LoadedAssembly concreteType.AssemblyFullName |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue concreteType.Definition.Get with
                | false, _ ->
                    failwith
                        $"Failed to find type definition {concreteType.Definition.Get} in {concreteType.AssemblyFullName}"
                | true, v -> v

            logger.LogDebug ("Resolving type {TypeDefNamespace}.{TypeDefName}", typeDef.Namespace, typeDef.Name)

            // The CLR does not eagerly run base type initializers before the current type's .cctor.
            // Base types get initialized later when their own constructors or static members are touched.
            // TODO: also need to initialise any prerequisites that the CLI genuinely requires here;
            // if so, do them *before* WithTypeBeginInit, otherwise a suspended prerequisite causes
            // retries to see "in-progress" and skip this type's own .cctor.
            let state = state.WithTypeBeginInit currentThread ty

            // Find the class constructor (.cctor) if it exists
            let cctor =
                typeDef.Methods
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && MethodInfo.arity method = 0)

            match cctor with
            | Some cctorMethod ->
                // Call the class constructor! We concretize manually and call `callMethod` directly,
                // because we're already in the middle of loading this class.
                let currentThreadState = state.ThreadState.[currentThread]

                // Convert the method's type generics from TypeDefn to ConcreteTypeHandle
                let cctorMethodWithTypeGenerics =
                    cctorMethod
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> concreteType.Generics.[par.SequenceNumber])

                // Convert method generics (should be empty for cctor)
                let cctorMethodWithMethodGenerics =
                    cctorMethodWithTypeGenerics
                    |> MethodInfo.mapMethodGenerics (fun _ -> failwith "cctor cannot be generic")

                // Convert method signature from TypeDefn to ConcreteTypeHandle using concretization
                let state, convertedSignature =
                    cctorMethodWithMethodGenerics.Signature
                    |> IlMachineState.concretizeMethodSignature
                        loggerFactory
                        baseClassTypes
                        state
                        concreteType.AssemblyFullName
                        concreteType.Generics
                        // no method generics for cctor
                        ImmutableArray.Empty

                // Convert method instructions (local variables)
                let state, convertedBody =
                    match cctorMethodWithMethodGenerics.Body with
                    | MethodBody.Il methodInstr ->
                        let state, convertedLocalVars =
                            match methodInstr.LocalVars with
                            | None -> state, None
                            | Some localVars ->
                                // Concretize each local variable type. The result is indexed by
                                // local-variable slot, so it must preserve the declaration order
                                // of `localVars`.
                                let state, convertedVars =
                                    ((state, ImmutableArray.CreateBuilder<ConcreteTypeHandle> ()), localVars)
                                    ||> Seq.fold (fun (state, acc) typeDefn ->
                                        let state, handle =
                                            IlMachineState.concretizeType
                                                loggerFactory
                                                baseClassTypes
                                                state
                                                concreteType.AssemblyFullName
                                                concreteType.Generics
                                                ImmutableArray.Empty // no method generics for cctor
                                                typeDefn

                                        acc.Add handle
                                        state, acc
                                    )
                                    |> Tuple.rmap (fun builder -> builder.ToImmutable ())

                                state, Some convertedVars

                        state, MethodBody.Il (MethodInstructions.setLocalVars convertedLocalVars methodInstr)
                    | MethodBody.InternalCall -> state, MethodBody.InternalCall
                    | MethodBody.PInvoke -> state, MethodBody.PInvoke
                    | MethodBody.RuntimeProvided rb -> state, MethodBody.RuntimeProvided rb
                    | MethodBody.Abstract -> state, MethodBody.Abstract

                let fullyConvertedMethod =
                    MethodInfo.setMethodVars convertedBody convertedSignature cctorMethodWithMethodGenerics

                callMethod
                    loggerFactory
                    baseClassTypes
                    (Some ty)
                    ConstructionState.NotConstructing
                    CallDispatch.Direct
                    true
                    false
                    // constructor is surely not generic
                    ImmutableArray.Empty
                    fullyConvertedMethod
                    currentThread
                    currentThreadState
                    None
                    ReturnValueDisposition.PushToCaller
                    ExceptionEscape.Propagate
                    state
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state = state.WithTypeEndInit currentThread ty

                NothingToDo state

    and ensureTypeInitialised
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (ty : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        match loadClass loggerFactory baseClassTypes ty thread state with
        | StateLoadResult.NothingToDo state -> state, WhatWeDid.Executed
        | StateLoadResult.FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | StateLoadResult.ThrowingTypeInitializationException state ->
            state, WhatWeDid.ThrowingTypeInitializationException
        | StateLoadResult.UnhandledTypeInitializationException (state, exn) -> state, WhatWeDid.UnhandledException exn
        | StateLoadResult.Blocked (state, blockedBy) -> state, WhatWeDid.BlockedOnClassInit blockedBy

    /// Synthesise an exception from inside the runtime itself (the host emulating the CLR),
    /// as opposed to a `throw` opcode executed by guest IL. Allocates the exception without
    /// running the exception type's .cctor, pushes its default instance constructor frame,
    /// and returns to the dispatch loop. When the ctor completes (Ret), returnStackFrame
    /// will signal DispatchException so the Ret handler can dispatch the exception.
    ///
    /// Use this for opcode-manufactured exceptions like `NullReferenceException` from a null
    /// dereference or `InvalidCastException` from a failed `castclass`. Do NOT use it for
    /// dispatching exceptions that the guest itself constructs and throws via `newobj` + `throw`
    /// — those go through `ExceptionDispatching.throwExceptionObject` and the cctor will already
    /// have run during the guest's `newobj`.
    ///
    /// All current call sites pass a non-generic BCL exception type from `BaseClassTypes`. The
    /// cctor-skip is safe for those (their cctors are trivial or empty); it would not be safe
    /// for an arbitrary guest-defined exception type, which is why this entry point is
    /// reserved for runtime use.
    ///
    /// This is a runtime boundary, not guest `newobj` semantics. It mirrors the CLR's
    /// EEException::CreateThrowable path: allocate the object directly, call the default
    /// instance ctor, then overwrite HResult.
    /// See: https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
    ///
    /// `fields` are written once the ctor has run, for the cases where the CLR would have used
    /// a constructor overload taking them: a message, or a `TypeLoadException`'s type and
    /// assembly names. Most callers want none — the CLR throws the great majority of these
    /// with no argument — and should use `raiseRuntimeException` below, which is this
    /// function with an empty list supplied; `raiseRuntimeExceptionWithMessage` is the
    /// message-only spelling.
    and raiseRuntimeExceptionWithFields
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (fields : RuntimeExceptionField list)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        // This is part of the `callMethod` recursion group only because it needs to call
        // `callMethod` to run the ctor, and `callMethod` needs to call it to service
        // `IntrinsicResult.RaiseException`.
        //
        // 1. Allocate the zero-initialised exception with _HResult pre-set.  This deliberately
        //    bypasses ensureTypeInitialised: opcode-manufactured exceptions are produced by the
        //    runtime rather than by guest `newobj` class-initialisation semantics.
        let addr, _exnHandle, state =
            ExceptionDispatching.allocateRuntimeException loggerFactory baseClassTypes exceptionTypeInfo state

        // 2. Find the parameterless .ctor on the exception type.
        let assy =
            state.TypeSystem._LoadedAssemblies.ByDefinitionName exceptionTypeInfo.AssemblyFullName

        let typeDef = assy.TypeDefs.[exceptionTypeInfo.Identity.TypeDefinition.Get]

        if not typeDef.Generics.IsEmpty then
            failwith
                $"raiseRuntimeException: expected non-generic exception type, but %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} has %i{typeDef.Generics.Length} generic parameter(s)"

        let ctor =
            typeDef.Methods
            |> List.tryFind (fun method -> method.Name = ".ctor" && not method.IsStatic && MethodInfo.arity method = 0)
            |> Option.defaultWith (fun () ->
                failwith
                    $"raiseRuntimeException: no parameterless .ctor found on %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name}"
            )
            // The type has no generic parameters (guarded above), so any GenericParamFromMetadata
            // in the ctor's type-generic positions is unreachable. Map them to TypeDefn to satisfy
            // concretizeMethodForExecution's signature.
            |> MethodInfo.mapTypeGenerics (fun _ ->
                failwith<TypeDefn> "raiseRuntimeException: exception type was unexpectedly generic"
            )

        // 3. Push the allocated object ref as `this` for the ctor.
        let state =
            IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) currentThread state

        // 4. Call the ctor, marking the return state so that returnStackFrame dispatches
        //    the exception instead of pushing the object onto the caller's eval stack.
        //    Do NOT advance the caller's PC: when the ctor returns and exception dispatch
        //    begins, handler lookup and the stack-trace frame must see the faulting
        //    instruction's PC, not the next instruction.  (Same class of bug as call-site
        //    vs resumed-PC for cross-frame unwinding, which CallSiteIlOpIndex solves.)
        let state, concretizedCtor, ctorDeclaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                currentThread
                ctor
                None
                None
                state

        let threadState = state.ThreadState.[currentThread]

        let state =
            callMethod
                loggerFactory
                baseClassTypes
                None
                (ConstructionState.Constructing addr) // weAreConstructingObj
                CallDispatch.Direct
                false // wasClassConstructor
                false // do NOT advance caller PC — dispatch needs the faulting instruction's offset
                concretizedCtor.Generics
                concretizedCtor
                currentThread
                threadState
                None
                (ReturnValueDisposition.DispatchAsException fields)
                ExceptionEscape.Propagate
                state

        // 5. Discharge the ctor frame's prologue without running it, holding step 1's bypass.
        //    `callMethod` arms a type-initialisation check on every metadata callee it pushes,
        //    which for this one would run the exception type's own `.cctor` — guest code, in the
        //    middle of manufacturing a runtime exception, able to replace it with a
        //    `TypeInitializationException`.
        //
        //    Latent as it stands: no exception type this path manufactures has a `.cctor` in the
        //    CoreLib we resolve. CoreCLR reaches these through `EEException::CreateThrowable`
        //    rather than through a JIT'd prologue.
        //
        //    Checked rather than assumed, because clearing the flag off the wrong frame would
        //    silently let a `.cctor` run somewhere else instead: the frame `callMethod` just
        //    pushed must be active, and must be awaiting this very exception type.
        let threadState = state.ThreadState.[currentThread]
        let ctorFrameId = threadState.ActiveMethodState

        match threadState.MethodState.PendingTypeInit with
        | Some pending when pending = ctorDeclaringTypeHandle ->
            state
            |> IlMachineState.mapFrame currentThread ctorFrameId MethodState.clearPendingTypeInit,
            WhatWeDid.Executed
        | other ->
            failwith
                $"logic error: manufacturing %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} pushed a constructor frame whose pending type initialisation is %O{other}, not the exception's own type %O{ctorDeclaringTypeHandle}; the class-initialisation bypass cannot be applied to it"

    /// `raiseRuntimeExceptionWithFields` writing only `_message`, or nothing when `message` is
    /// `None`.
    and raiseRuntimeExceptionWithMessage
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (message : string option)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        raiseRuntimeExceptionWithFields
            loggerFactory
            baseClassTypes
            exceptionTypeInfo
            (message |> Option.toList |> List.map RuntimeExceptionField.Message)
            currentThread
            state

    /// `raiseRuntimeExceptionWithMessage` with no message override, i.e. the exception is
    /// constructed exactly as `new SomeException()` would construct it. This is the right
    /// entry point wherever the CLR throws the exception with no argument, which is almost
    /// everywhere the runtime manufactures one.
    and raiseRuntimeException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        raiseRuntimeExceptionWithMessage loggerFactory baseClassTypes exceptionTypeInfo None currentThread state

    /// Raise the fault an *instruction* faulted with, naming the fault rather than the corelib
    /// type. `OpcodeFault.resolve` supplies the type, so no call site here decides that
    /// correspondence for itself.
    ///
    /// Prefer this to `raiseRuntimeException` at every opcode site. Besides removing the type
    /// choice from the call site, it checks the raise against `OpcodeFaults`: an instruction that
    /// faults with something its table entry does not list means one of the two is wrong, and
    /// which one is not something this can decide, so it says so and stops. That check is what
    /// makes the table something an analyser may believe rather than a second, unpoliced
    /// description of the same behaviour.
    ///
    /// The instruction is read from the thread's own frame rather than passed in, so a site cannot
    /// name one opcode while executing another. The program counter is not advanced at a raise
    /// site — exception dispatch keys the handler search and the stack trace on the faulting
    /// instruction's offset — so what it reads is the instruction that faulted.
    ///
    /// Not for the runtime's non-instruction raises: a native handler, an intrinsic, or a frame
    /// prologue is not executing an opcode, and `OpcodeFaults` has nothing to say about it. Those
    /// keep `raiseRuntimeException`.
    and raiseOpcodeFault
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (fault : OpcodeFault)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        raiseOpcodeFaultWithMessage loggerFactory baseClassTypes fault None currentThread state

    /// `raiseOpcodeFault` with the message the CLR would have passed to a message-taking ctor
    /// overload. Most instruction faults want `None` — the CLR raises them with no argument — so
    /// `raiseOpcodeFault` is the usual entry point.
    and raiseOpcodeFaultWithMessage
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (fault : OpcodeFault)
        (message : string option)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let permitted = faultsOfCurrentInstruction currentThread state

        if not (OpcodeFaults.mayRaise fault permitted) then
            let methodState = state.ThreadState.[currentThread].MethodState

            failwith
                $"logic error: %s{MethodOwner.describe methodState.ExecutingMethod.Owner}::%s{methodState.ExecutingMethod.Name} at IL_%04X{methodState.IlOpIndex} raised %O{fault}, but OpcodeFaults says that instruction raises %O{permitted}. Either the interpreter is raising the wrong exception here, or the table is missing an entry; both are bugs and only a human can say which."

        raiseRuntimeExceptionWithMessage
            loggerFactory
            baseClassTypes
            (OpcodeFault.resolve baseClassTypes fault)
            message
            currentThread
            state

    /// Result of the ECMA-335 III.4.x runtime array-store variance gate.
    [<RequireQualifiedAccess>]
    type ArrayStoreVarianceCheck =
        /// The store may proceed. The state may have been updated as a side effect of
        /// the assignability walk (which may concretize additional metadata), so the
        /// caller must use the state carried here, not its pre-check state.
        | Allowed of state : IlMachineState
        /// The store was rejected as covariance-incompatible. The caller must raise
        /// `ArrayTypeMismatchException` and return without advancing PC: exception dispatch needs
        /// the faulting instruction's offset.
        ///
        /// The raise is the *caller's* to make, not this check's, because the callers are not
        /// alike. `stelem` and `stelem.ref` are instructions, so their fault goes through
        /// `raiseOpcodeFault` and is checked against `OpcodeFaults`; the runtime-synthesized
        /// `T[<rank>]::Set` is a callee reached from a plain `call`, about which the table says
        /// nothing, so its fault must not be. A helper raising on both their behalves would have to
        /// choose one route for both.
        | Refused of state : IlMachineState
        /// The value stored into a reference-typed element is undefined, and the check would have
        /// to read its type. The caller ends the run, naming its own kind of use: an instruction
        /// or a runtime-synthesised method.
        | ValueUndefined of UndefinedValue

    /// ECMA-335 III.4.x runtime-assignment-compatibility gate for `stelem` /
    /// runtime-synthesized `T[<rank>]::Set`. For reference-typed array elements, the
    /// value's runtime type must be assignment-compatible with the array's stored
    /// element type; otherwise raise `ArrayTypeMismatchException`. Null is always
    /// storable. Value-typed-element arrays bypass the gate: there is no covariance
    /// for value types, the verifier rejects mismatching value-store opcodes at
    /// load time, and primitive coercion is handled by `EvalStackValue.toCliTypeCoerced`.
    let checkArrayStoreVariance
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (arrayAddress : ManagedHeapAddress)
        (value : EvalStackValue)
        (state : IlMachineState)
        : ArrayStoreVarianceCheck
        =
        let arrayObj =
            match ManagedHeap.tryGetArrayShape arrayAddress state.ManagedHeap with
            | Some v -> v
            | None ->
                failwith
                    $"checkArrayStoreVariance: no array allocation at %O{arrayAddress}; helper called with a non-array heap address"

        let storedElement =
            match arrayObj.ConcreteType with
            | ConcreteTypeHandle.OneDimArrayZero elt -> elt
            | ConcreteTypeHandle.Array (elt, _) -> elt
            | other ->
                failwith
                    $"checkArrayStoreVariance: array allocation at %O{arrayAddress} has non-array ConcreteType %O{other}"

        let storedElementIsReference =
            IlMachineState.isReferenceTypeHandle baseClassTypes "checkArrayStoreVariance" state storedElement

        if not storedElementIsReference then
            // Value-type element store: variance does not apply. Numeric coercion
            // happens in toCliTypeCoerced; the verifier guards value-type identity.
            ArrayStoreVarianceCheck.Allowed state
        else

        match value with
        | EvalStackValue.Undefined u -> ArrayStoreVarianceCheck.ValueUndefined u
        | EvalStackValue.NullObjectRef ->
            // Null is always storable into a reference-typed array slot.
            ArrayStoreVarianceCheck.Allowed state
        | EvalStackValue.ObjectRef addr ->
            let valueRuntimeType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            let state, isAssignable =
                IlMachineState.isConcreteTypeAssignableTo
                    loggerFactory
                    baseClassTypes
                    state
                    valueRuntimeType
                    storedElement

            if isAssignable then
                ArrayStoreVarianceCheck.Allowed state
            else
                ArrayStoreVarianceCheck.Refused state
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.NativeInt _
        | EvalStackValue.Float _
        | EvalStackValue.UserDefinedValueType _ ->
            // Reference-typed-element arrays only accept ObjectRef / NullObjectRef stack
            // values. The verifier rejects other shapes at load time, so reaching this
            // arm means either the verifier was skipped or the interpreter produced a
            // value of the wrong shape. Surface the gap explicitly rather than letting
            // the store silently mis-coerce.
            failwith
                $"TODO: array-store variance check for reference-typed-element array with stack value form %O{value}; expected ObjectRef or NullObjectRef"
