namespace WoofWare.PawPrint

/// <summary>
/// The live frames of one guest thread, in the form a captured stack trace reports them.
/// </summary>
/// <remarks>
/// The current-thread half of what <c>StackTrace_GetStackFramesInternal</c> can be asked for; the
/// other half, a trace frozen onto an exception at dispatch, is
/// <c>IlMachineRuntimeMetadata.frozenStackTraceFrames</c>. Both answer in
/// <c>ExceptionStackFrame</c>, so the two sources cannot drift in what a frame is.
///
/// Guest-observable, unlike <c>GuestLocation</c>'s superficially similar walk: that one is
/// PawPrint's own diagnostic and is allowed to lose precision on an unexpected chain shape, which
/// is why this does not reuse it.
/// </remarks>
[<RequireQualifiedAccess>]
module StackFrameCapture =

    /// <summary>
    /// Whether <paramref name="frame" /> is a delegate's <c>Invoke</c>, which a captured trace
    /// must not report.
    /// </summary>
    /// <remarks>
    /// A delegate's `Invoke` is a stub, not a managed method: real .NET has no frame for it, and
    /// an exception crossing a delegate call reports the target and then whoever called `Invoke`.
    /// PawPrint's ordinary delegate path gets that for free, because `dispatchDelegateInvoke`
    /// pops its synthetic frame before calling the target — so the frame is already gone by the
    /// time anything can throw. The exception is class initialisation, which runs
    /// *while* that frame is still active so the instruction can be retried after the `.cctor`
    /// returns; without this, a `.cctor` that throws would report a `System.Action.Invoke` frame
    /// that no real trace contains. A live-stack capture taken from inside such a call sees the
    /// same still-active frame, and must suppress it for the same reason.
    ///
    /// Only `DelegateInvoke` is suppressed, not runtime-provided frames at large: an InternalCall
    /// or QCall *is* a managed method by name and real traces do show it.
    /// </remarks>
    let isDelegateInvokeStub (frame : MethodState) : bool =
        match frame.ExecutingMethod.Body with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke -> true
        | _ -> false

    /// <summary>
    /// The frames of <paramref name="thread" />'s live stack, innermost first — the order
    /// <c>StackTrace</c> reports and <c>ExceptionDispatching</c> accumulates.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The innermost frame is attributed to its own program counter; every enclosing frame is
    /// attributed to the <c>CallSiteIlOpIndex</c> recorded by the frame it called, not to the
    /// offset it will resume at. Those differ — the resume point is the instruction *after* the
    /// call, so for a call ending a statement it belongs to the next statement — and dispatch
    /// picks the call site for exactly this reason.
    /// </para>
    /// <para>
    /// Requires <paramref name="thread" /> to have a live frame; guard with
    /// <c>ThreadStatus.hasNoActiveFrame</c>. The active frame's program counter is used as-is
    /// rather than stepped back onto a blocking call, because this walk answers a QCall the thread
    /// is itself executing, so it is never parked when asked.
    /// </para>
    /// <para>
    /// Refuses while a first-pass handler search is suspended in a filter. The active frame is then
    /// the one hosting the filter, and the frames inner to it are still live but are not on its
    /// return chain, so walking outward from it would silently omit them.
    /// </para>
    /// </remarks>
    let ofThread
        (thread : ThreadState)
        : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        =
        let describe
            (frame : MethodState)
            (ilOffset : int)
            : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            =
            {
                Method = frame.ExecutingMethod
                IlOffset = ilOffset
                // A live stack has no foreign-raise history: that flag marks the last frame of a
                // trace spliced back on by `ExceptionDispatchInfo.Throw`, and only an
                // exception-sourced capture can carry one. CoreCLR agrees — it sets
                // `fDoWeHaveAnyFramesFromForeignStackTrace` only in `GetStackFramesFromException`.
                IsLastFrameFromForeignExceptionStackTrace = false
            }

        // A valid return chain is strictly decreasing in frame id and so cannot cycle; the bound
        // is here so that a chain which *is* malformed says so rather than looping forever. Unlike
        // `GuestLocation`, this is a normal execution path, so a malformed chain is an interpreter
        // bug to report rather than a diagnostic to degrade.
        let rec walk
            (remaining : int)
            (acc : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
            (frame : MethodState)
            (ilOffset : int)
            : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
            =
            if remaining < 0 then
                failwith
                    $"StackFrameCapture.ofThread: walked more than %d{thread.MethodStates.Count} frames, the number live, without reaching one that has no caller; the return chain from frame %O{thread.ActiveMethodState} does not terminate. This is an interpreter bug."

            // A filter body runs during the *search* pass, so the frames between the throw and this
            // one have not been unwound: `ExceptionDispatching.firstPass` moves the active frame
            // outward to the filter's host and leaves them live. They are not on this frame's
            // return chain, though, so continuing the walk would report the filter, its callers,
            // and nothing of the throw — measured on .NET 10, a capture from inside a filter
            // reports the filter, its host, the runtime's two dispatch frames, and then the whole
            // still-live throwing stack (`P.Thrower` / `P.Middle` / `P.Main`).
            //
            // The inputs a fix would need are right here: the suspended `ExceptionSearchState`
            // carries `StartFrame` and `StartPC`, the frame and offset the raise began at, so the
            // throwing chain can be walked from there. What it does not settle is the shape of the
            // answer — real .NET reports the filter's host frame *twice*, once as the filter's host
            // and again as part of the throwing stack, and interposes two `System.Runtime.EH`
            // frames PawPrint has no analogue of. That is a dispatch-shaped decision rather than a
            // capture-shaped one.
            //
            // Until it is made, this refuses: a trace missing the frames that explain the exception
            // being filtered is a wrong answer presented as a right one. The refusal is now
            // reachable — a non-empty capture from a non-generic frame runs end to end
            // (`sourcesPure/StackTraceCurrentThreadFrames.cs`), so a guest that captures from
            // inside a filter body gets this failure rather than a wrong trace.
            frame.ExceptionContinuations
            |> List.iter (fun continuation ->
                match continuation.Continuation with
                | ExceptionContinuation.ResumeAfterFilter _ ->
                    failwith
                        $"StackFrameCapture.ofThread: cannot capture a stack trace while a first-pass handler search is suspended in a filter of %s{frame.ExecutingMethod.Name}. The frames inner to it are still live but are not on its return chain, so this walk would report a trace missing the throw that is being filtered. Splicing them in means walking from the suspended search's StartFrame/StartPC, and deciding how to present the filter host, which appears twice in real .NET's answer."
                | ExceptionContinuation.ResumeAfterFinally _
                | ExceptionContinuation.PropagatingException _ -> ()
            )

            let acc =
                if isDelegateInvokeStub frame then
                    acc
                else
                    describe frame ilOffset :: acc

            match frame.ReturnState with
            | None -> List.rev acc
            | Some returnState ->

            match ThreadState.tryGetFrame returnState.JumpTo thread with
            | None ->
                failwith
                    $"StackFrameCapture.ofThread: frame %O{returnState.JumpTo} is named as the caller of a live frame but is not itself live. This is an interpreter bug."
            | Some caller -> walk (remaining - 1) acc caller returnState.CallSiteIlOpIndex

        let active = thread.MethodState
        walk thread.MethodStates.Count [] active active.IlOpIndex
