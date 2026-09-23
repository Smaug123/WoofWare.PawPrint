namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// The runtime-synthesised stub a multicast delegate's `_methodPtr` names.
///
/// `MulticastDelegate.NewMulticastDelegate` builds a multicast delegate with `_target` pointing
/// at the delegate itself and `_methodPtr` holding `Delegate.GetMulticastInvoke()`, so invoking it
/// is an ordinary closed-instance delegate call whose target is this stub. CoreCLR emits the stub
/// as IL (`Delegate_GetMulticastInvokeSlow`, comdelegate.cpp:2177): a loop that calls `Invoke` on
/// each of `_invocationList[0 .. _invocationCount - 1]` with the stub's own arguments, keeps the
/// last result, and returns it. PawPrint has no IL synthesis, so the stub is a
/// `MethodInfo.Synthesised` carrying `RuntimeBehaviour.MulticastDelegateInvoke`, held by an
/// ordinary `FunctionPointerTarget.Managed`, and `execute` is its interpreter.
///
/// Each element is invoked by pushing its `Invoke` as an ordinary callee, one per scheduler step,
/// rather than by a host-side loop: the stub's frame stays on the stack between elements, so every
/// element's body is interpreted, and can be interleaved, exactly as a direct call would be.
[<RequireQualifiedAccess>]
module MulticastDelegateStub =

    /// The `Invoke` method of a delegate type, fully concretised. CoreCLR reaches it through
    /// `COMDelegate::FindDelegateInvokeMethod` (comdelegate.cpp:2516), which reads the slot the
    /// `DelegateEEClass` caches; PawPrint has no such cache and looks the method up by name, which
    /// is the same thing given that the runtime is what synthesises `Invoke` in the first place
    /// and gives every delegate type exactly one.
    let invokeMethodOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (delegateType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let concreteType, typeInfo =
            IlMachineState.tryGetConcreteTypeInfo state delegateType
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: the delegate's type %O{delegateType} has no TypeDef row"
            )

        let invoke =
            typeInfo.Methods
            |> List.filter (fun method -> method.Name = "Invoke" && not method.IsStatic)
            |> function
                | [ single ] -> single
                | [] ->
                    // CoreCLR raises `MissingMethodException("Invoke")` here
                    // (comdelegate.cpp:2530). Unreachable from every caller. From
                    // `Delegate_BindToMethodInfo`, because `CreateDelegate`'s callers all check
                    // `rtType.IsDelegate()` first; from `Delegate.GetInvokeMethod` and
                    // `Delegate.GetMulticastInvoke`, because the MethodTable each is handed is
                    // that of a live delegate instance; from the multicast stub, because it is
                    // only ever minted by the latter. Either way, a type whose base is
                    // `MulticastDelegate` got its `Invoke` from the runtime.
                    failwith
                        $"%s{operation}: delegate type %s{typeInfo.Namespace}.%s{typeInfo.Name} declares no instance method named Invoke"
                | several ->
                    failwith
                        $"%s{operation}: delegate type %s{typeInfo.Namespace}.%s{typeInfo.Name} declares %d{several.Length} instance methods named Invoke; a delegate type has exactly one"

        let state, concretised, _declaringHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                concreteType.Generics
                invoke
                ImmutableArray.Empty
                state

        state, concretised

    /// The synthesised method that *is* a delegate type's multicast invoke stub.
    ///
    /// The declaring type is the delegate type, and the signature is its `Invoke`'s, `this`
    /// included: the stub is entered as the closed-instance target of the multicast delegate's own
    /// `Invoke`, with the multicast delegate as its receiver. That makes its identity
    /// per-delegate-type — `MethodInfo.NominallyEqual` compares declaring type plus synthesised
    /// kind — which is the per-`DelegateEEClass` identity of CoreCLR's `m_pMultiCastInvokeStub`
    /// cache, modulo the canonical sharing `Delegate.GetInvokeMethod` also does not model.
    let synthesise
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (delegateType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let state, invoke =
            invokeMethodOf loggerFactory baseClassTypes operation delegateType state

        let declaringType =
            AllConcreteTypes.lookup delegateType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: delegate type %O{delegateType} is not registered in AllConcreteTypes"
            )

        let stub =
            MethodInfo.Synthesised (
                {
                    Owner = MethodOwner.DeclaredOn declaringType
                    Name = "<MulticastDelegateInvokeStub>"
                    Body = MethodBody.RuntimeProvided RuntimeBehaviour.MulticastDelegateInvoke
                    Generics = ImmutableArray.Empty
                    Signature = invoke.Signature
                    IsStatic = false
                },
                SynthesisedMethod.MulticastDelegateInvokeStub
            )

        state, stub

    /// Run one step of a multicast invoke stub's frame: invoke the next element of the invocation
    /// list, or, once every element has been invoked, return the last one's result.
    ///
    /// Dispatched from `AbstractMachine.executeOneStep` exactly as the delegate constructor and
    /// `Invoke` are, so the stub has an ordinary frame: the multicast delegate in `Arguments.[0]`,
    /// `Invoke`'s own arguments after it, and an evaluation stack of its own. That stack is where
    /// the stub keeps its progress, because it is re-entered once per element. On each call it
    /// pushes the index of the element being invoked, then the call's arguments; the callee pops
    /// the arguments, and returning puts its result (if `Invoke` has one) on top of the index. So
    /// on entry the stack is empty the first time, and `index` or `result; index` on each
    /// re-entry. Neither shape can be mistaken for the other, since which one to expect is fixed
    /// by the signature.
    ///
    /// An element that throws unwinds through this frame like any other: the stub has no
    /// handlers, so the remaining elements are not invoked, which is what CoreCLR's stub does.
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (instruction : MethodState)
        (state : IlMachineState)
        : ExecutionResult
        =
        let operation = "multicast delegate invoke stub"

        let multicastAddr =
            match instruction.Arguments.[0] with
            | CliType.ObjectRef (Some addr) -> addr
            | other ->
                // The stub is reached only as the target of a multicast delegate's `Invoke`, which
                // passes that delegate's `_target` — the delegate itself — as the receiver.
                failwith
                    $"%s{operation}: expected the multicast delegate as the receiver, got %O{other}; NewMulticastDelegate points _target at the delegate itself"

        let multicast = ManagedHeap.get multicastAddr state.ManagedHeap

        let read (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) : CliType =
            AllocatedNonArrayObject.DereferenceFieldById (DelegateLayout.fieldId state.ConcreteTypes field) multicast

        // Read on every step, as CoreCLR's stub reloads both fields on every iteration. Neither
        // changes after `NewMulticastDelegate` publishes the delegate, but the backing array is
        // shared with the delegate it was combined from and may have been *extended* since, which
        // is why the count and not the array's length bounds the walk: `CombineImpl` grows the
        // array by doubling and appends into its spare slots in place (`TrySetSlot`). `elementAt`
        // is the delegate the list holds at an index, read from the list as the layout stores it.
        let invocationCount, elementAt =
            match DelegateLayout.require baseClassTypes with
            | DelegateLayout.InvocationListAndCount (_, invocations) ->
                let invocationCount =
                    match read invocations.InvocationCount |> CliType.unwrapPrimitiveLikeDeep with
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim count)) -> count
                    | other ->
                        failwith $"%s{operation}: expected _invocationCount to be a verbatim native int, got %O{other}"

                let invocationList =
                    match read invocations.InvocationList with
                    | CliType.ObjectRef (Some addr) -> addr
                    | other ->
                        failwith
                            $"%s{operation}: expected _invocationList to reference the invocation-list array, got %O{other}"

                // An `object[]` whose elements are the delegates themselves.
                let elementAt (index : int) (state : IlMachineState) : CliType =
                    match ManagedHeap.getArrayValue invocationList index state.ManagedHeap with
                    | CliType.ObjectRef (Some _) as element -> element
                    | other ->
                        failwith
                            $"%s{operation}: expected element %d{index} of _invocationList to reference a delegate, got %O{other}"

                invocationCount, elementAt

        // CoreCLR's loop tests the bound only after its first call, so a count below one would
        // still invoke element 0. `NewMulticastDelegate`'s callers never build one: combining
        // produces at least two elements, and removal collapses a one-element result to that
        // element itself.
        if invocationCount < 1L then
            failwith
                $"%s{operation}: the multicast delegate's _invocationCount is %d{invocationCount}; every multicast delegate CoreLib builds has at least two elements"

        let returnsValue =
            match instruction.ExecutingMethod.Signature.ReturnType with
            | MethodReturnType.Void -> false
            | MethodReturnType.Returns _ -> true

        let frameId = state.ThreadState.[thread].ActiveMethodState

        let stackOnEntry =
            (IlMachineState.getFrame thread frameId state).EvaluationStack.Values

        let indexOf (value : EvalStackValue) : int =
            match value with
            | EvalStackValue.Int32 (Int32Source.Verbatim index) -> index
            | other -> failwith $"%s{operation}: expected the index of the element last invoked, got %O{other}"

        let nextIndex, lastResult =
            match stackOnEntry, returnsValue with
            | [], _ -> 0, None
            | [ index ], false -> indexOf index + 1, None
            | [ result ; index ], true -> indexOf index + 1, Some result
            | other, _ ->
                failwith
                    $"%s{operation}: unexpected evaluation stack on re-entry (returns a value: %b{returnsValue}): %A{other}"

        let state =
            stackOnEntry
            |> List.fold (fun state _ -> IlMachineState.popEvalStack thread state |> snd) state

        if int64 nextIndex < invocationCount then
            let element = elementAt nextIndex state

            let delegateType =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    instruction.ExecutingMethod.RequiredDeclaringType.Identity
                    instruction.ExecutingMethod.DeclaringTypeGenerics
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: declaring type %s{MethodOwner.describe instruction.ExecutingMethod.Owner} is not registered in AllConcreteTypes"
                )

            let state, invoke =
                invokeMethodOf loggerFactory baseClassTypes operation delegateType state

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 nextIndex)) thread state

            let state = IlMachineState.pushToEvalStack element thread state

            let state =
                let mutable s = state

                for i = 1 to instruction.Arguments.Length - 1 do
                    s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

                s

            let threadState = state.ThreadState.[thread]

            let state, commitment =
                IlMachineStateExecution.callMethodWithCommitment
                    loggerFactory
                    baseClassTypes
                    None
                    ConstructionState.NotConstructing
                    false // CoreCLR's stub `call`s `Invoke`; it is not virtual
                    false
                    false // this frame has no program counter to advance
                    IlMachineStateExecution.CallSiteTransition.StaysCooperative
                    invoke.Generics
                    invoke
                    thread
                    threadState
                    None
                    ReturnValueDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            match commitment with
            | IlMachineStateExecution.CallCommitment.Aborted fatal ->
                ExecutionResult.stepped (state, WhatWeDid.Aborted fatal)
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised ->
                // Either the element's `Invoke` frame or an exception constructor is now on top of
                // us; in both cases our frame stays put and the dispatch loop takes it from here.
                ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)
        else

        let state =
            match lastResult with
            | Some result -> IlMachineState.pushToEvalStack' result thread state
            | None -> state

        match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
        | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
        | result -> failwith $"%s{operation}: unexpected ReturnFrameResult returning from stub frame: %A{result}"
