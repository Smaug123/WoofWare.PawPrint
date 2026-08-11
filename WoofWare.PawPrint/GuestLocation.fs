namespace WoofWare.PawPrint

/// <summary>
/// One frame of a guest thread, described for a developer reading a PawPrint diagnostic.
/// </summary>
type GuestFrame =
    {
        /// <summary>
        /// The executing method, as <c>MethodInfo.ToString</c> renders it: <c>Assembly.Type.Method</c>.
        /// </summary>
        Method : string

        /// <summary>
        /// Absolute byte offset into the method's IL, which is the unit a portable PDB's
        /// sequence points are keyed by.
        /// </summary>
        IlOffset : int
    }

/// <summary>
/// Where one guest thread is, in the terms a developer asking "why is this stuck?" needs.
/// </summary>
/// <remarks>
/// The three framed cases are distinguished rather than folded into one record with an optional
/// location because they are answers to different questions, and a caller that conflates them
/// reports a framework method as though it were the guest's own. Only the shared framework
/// routinely ships without a PDB, so which case a thread falls into is largely a statement about
/// whose code the thread is currently inside.
/// </remarks>
[<RequireQualifiedAccess>]
type GuestThreadPosition =
    /// <summary>
    /// The thread has no live frame: it is <c>NotStarted</c> or <c>Parked</c>, so no IL has run
    /// on it and there is nothing to locate.
    /// </summary>
    | NoFrame

    /// <summary>
    /// The active frame, and the source span the compiler attributed to the IL offset it is at.
    /// </summary>
    | AtSource of frame : GuestFrame * source : SourceLocation

    /// <summary>
    /// The active frame has no source attribution — its assembly carries no debug information,
    /// or the offset falls in IL the compiler marked hidden — but a frame enclosing it does.
    /// </summary>
    /// <remarks>
    /// <paramref name="ancestor" /> is the innermost enclosing frame that has attribution, which
    /// need not be the immediate caller: <paramref name="framesOut" /> says how far out the walk
    /// went, and is 1 exactly when it is. The ancestor's offset is its *call site*, not the
    /// offset it will resume at, so it names the call that led here rather than the instruction
    /// after it.
    /// </remarks>
    | CalledFrom of frame : GuestFrame * framesOut : int * ancestor : GuestFrame * ancestorSource : SourceLocation

    /// <summary>
    /// Neither the active frame nor any frame enclosing it has source attribution. The ordinary
    /// outcome for a guest built without a PDB, and for a stack lying entirely within the shared
    /// framework.
    /// </summary>
    | Unattributed of frame : GuestFrame

/// <summary>
/// Where one guest thread is, together with what it is doing there.
/// </summary>
type GuestThreadLocation =
    {
        Thread : ThreadId
        Status : ThreadStatus
        Position : GuestThreadPosition
    }

/// <summary>
/// Locating a guest in its own source, for PawPrint's developer-facing diagnostics.
/// </summary>
/// <remarks>
/// <para>
/// Strictly a read over existing state, and deliberately not reachable by guest code: this is
/// what PawPrint tells the person debugging PawPrint, and it may say things — absolute paths from
/// the machine that built the guest, frame counts — that the real runtime would never expose. The
/// guest-observable rendering of a stack lives in
/// <c>IlMachineRuntimeMetadata.renderExceptionStackFrame</c> and is a separate, CoreCLR-shaped
/// thing; the two must not be unified, because fidelity constrains one and legibility the other.
/// </para>
/// <para>
/// Nothing here may affect execution. It is called from failure paths, so a bug in it would
/// replace the diagnostic that explains a wedged guest with one about itself; hence the total
/// lookups and the bounded walk below.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module GuestLocation =

    let private describeFrame (method : MethodState) (ilOffset : int) : GuestFrame =
        {
            Method =
                string<MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>> method.ExecutingMethod
            IlOffset = ilOffset
        }

    /// <summary>
    /// The source span the compiler attributed to <paramref name="ilOffset" /> in
    /// <paramref name="method" />, if that method's assembly came with debug information.
    /// </summary>
    /// <remarks>
    /// The <c>MethodDefinitionHandle</c> is a row index scoped to the assembly it came from, so it
    /// must be resolved against the *declaring type's* assembly and no other; pairing it with any
    /// other loaded assembly would silently name a different method's lines. A synthesised method
    /// (a marshalling or delegate stub) has no row at all and so is legitimately unattributable.
    /// </remarks>
    let private trySourceOf
        (state : IlMachineState)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (ilOffset : int)
        : SourceLocation option
        =
        match method.TryMetadata with
        | None -> None
        | Some facts ->
            state.LoadedAssembly method.DeclaringType.Assembly
            |> Option.bind (fun assy -> assy.TryResolveSourceLocation facts.Handle ilOffset)

    /// <summary>
    /// The offset of the instruction immediately before <paramref name="ilOffset" /> in
    /// <paramref name="method" />, when that instruction is a call. <c>None</c> otherwise.
    /// </summary>
    /// <remarks>
    /// The cross-check that keeps <c>ThreadStatus.parksPastTheBlockingCall</c> honest. That
    /// classifier knows how a status is reached but not what the frame actually contains, so
    /// stepping back on its word alone would move the report onto whatever happened to precede
    /// the PC. Requiring a call there means a misclassified status costs the step-back — which
    /// is where we started — instead of naming an unrelated instruction.
    /// </remarks>
    let private precedingCallOffset
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (ilOffset : int)
        : int option
        =
        match MethodInfo.tryIlBody method with
        | None -> None
        | Some instructions ->

        let candidate =
            instructions.Locations
            |> Map.toSeq
            |> Seq.filter (fun (offset, _) -> offset < ilOffset)
            |> Seq.tryLast

        match candidate with
        | Some (offset, IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, _))
        | Some (offset, IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _))
        | Some (offset, IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _)) -> Some offset
        | Some _
        | None -> None

    /// <summary>
    /// The offset in <paramref name="frame" /> that a diagnostic should name: normally the
    /// program counter, but the blocking call site for a thread parked past one.
    /// </summary>
    let private reportableOffset (status : ThreadStatus) (frame : MethodState) : int =
        if ThreadStatus.parksPastTheBlockingCall status then
            precedingCallOffset frame.ExecutingMethod frame.IlOpIndex
            |> Option.defaultValue frame.IlOpIndex
        else
            frame.IlOpIndex

    /// <summary>
    /// Where <paramref name="thread" /> is: its active frame, and — when that frame has no source
    /// attribution — the innermost frame enclosing it that has.
    /// </summary>
    let positionOfThread (state : IlMachineState) (thread : ThreadState) : GuestThreadPosition =
        // `thread.MethodState` resolves the active frame and throws when there isn't one, which
        // `NotStarted` and `Parked` threads genuinely have. A guest holding a constructed but
        // unstarted `Thread` while another thread wedges is entirely ordinary, so reaching for
        // the frame unconditionally would replace the diagnostic about the guest with one about
        // this function.
        if ThreadStatus.hasNoActiveFrame thread.Status then
            GuestThreadPosition.NoFrame
        else

        let active = thread.MethodState

        // Not `active.IlOpIndex`: a thread parked inside a blocking QCall has already advanced
        // past the call that blocked it, so the raw PC names the statement *after* the one the
        // guest is stuck on. A frame further out is attributed at its `CallSiteIlOpIndex` for
        // exactly the same reason; this is that rule applied to the frame whose callee has
        // already been popped.
        let activeOffset = reportableOffset thread.Status active
        let activeFrame = describeFrame active activeOffset

        match trySourceOf state active.ExecutingMethod activeOffset with
        | Some source -> GuestThreadPosition.AtSource (activeFrame, source)
        | None ->

        // Walk outwards for the innermost frame that *does* have attribution. Without this the
        // answer for a guest blocked in the BCL is a framework method name, which is the case
        // this whole module exists to improve on: the shared framework ships without PDBs, so
        // the innermost frame of a stuck guest usually has nothing to say.
        //
        // Bounded by the number of live frames. A valid return chain is strictly decreasing in
        // frame id and so cannot cycle, but this runs on a failure path, where looping forever
        // would destroy the diagnostic that brought us here rather than merely degrade it.
        let rec walk (framesOut : int) (frame : MethodState) : GuestThreadPosition =
            if framesOut > thread.MethodStates.Count then
                GuestThreadPosition.Unattributed activeFrame
            else

            match frame.ReturnState with
            | None -> GuestThreadPosition.Unattributed activeFrame
            | Some returnState ->

            match ThreadState.tryGetFrame returnState.JumpTo thread with
            | None -> GuestThreadPosition.Unattributed activeFrame
            | Some caller ->

            // `CallSiteIlOpIndex`, not the caller's own `IlOpIndex`. The caller's PC really has
            // advanced past the call while the callee is live (measured: a blocked guest's
            // `Monitor.Wait` frame sat at 36 with its callee's call site at 31), so the resume PC
            // is the instruction *after* the call and attributing to it would report the next
            // statement. `ExceptionDispatching` picks the call site for the same reason.
            //
            // No test distinguishes the two, and cannot with the guests we have: the harness
            // compiles unoptimized, where Roslyn emits a `nop` immediately after each call
            // statement, so the resume PC stays inside the call's own sequence point and both
            // offsets resolve to the same line. Substituting `caller.IlOpIndex` here is therefore
            // a surviving mutant. Pinning it needs an optimized guest or hand-written IL.
            let callSite = returnState.CallSiteIlOpIndex

            match trySourceOf state caller.ExecutingMethod callSite with
            | Some source ->
                GuestThreadPosition.CalledFrom (activeFrame, framesOut, describeFrame caller callSite, source)
            | None -> walk (framesOut + 1) caller

        walk 1 active

    /// <summary>
    /// Where every thread that has not terminated is. Terminated threads are omitted: they are
    /// not why the guest is stuck, and a long-running guest accumulates many of them.
    /// </summary>
    let ofState (state : IlMachineState) : GuestThreadLocation list =
        state.ThreadState
        |> Map.toList
        |> List.filter (fun (_, ts) -> ts.Status <> ThreadStatus.Terminated)
        |> List.map (fun (threadId, ts) ->
            {
                Thread = threadId
                Status = ts.Status
                Position = positionOfThread state ts
            }
        )

    let private renderSource (source : SourceLocation) : string =
        // The document path exactly as the PDB records it, unshortened: it names the machine
        // that built the guest, which is information, and two `Program.cs` in different
        // directories are otherwise indistinguishable. Only the start line is rendered — the
        // span's end is rarely what a reader wants and doubles the length of every frame.
        $"%s{source.DocumentPath}:%d{source.StartLine}"

    let private renderFrame (frame : GuestFrame) : string =
        $"%s{frame.Method} at IL offset %d{frame.IlOffset}"

    let renderThread (location : GuestThreadLocation) : string =
        let (ThreadId i) = location.Thread
        let prefix = $"thread %d{i} (%O{location.Status})"

        match location.Position with
        | GuestThreadPosition.NoFrame -> prefix
        | GuestThreadPosition.Unattributed frame -> $"%s{prefix} in %s{renderFrame frame}"
        | GuestThreadPosition.AtSource (frame, source) ->
            $"%s{prefix} in %s{renderFrame frame} (%s{renderSource source})"
        | GuestThreadPosition.CalledFrom (frame, framesOut, ancestor, ancestorSource) ->
            let from =
                if framesOut = 1 then
                    "called from"
                else
                    // Say how far out, rather than letting "called from" imply the immediate
                    // caller: the distance is the difference between "the guest called straight
                    // into this" and "this is buried deep in framework code".
                    $"called %d{framesOut} frames out from"

            $"%s{prefix} in %s{renderFrame frame}, %s{from} %s{renderFrame ancestor} (%s{renderSource ancestorSource})"

    /// <summary>
    /// One line per non-terminated thread, joined by <c>"; "</c>.
    /// </summary>
    let renderThreads (locations : GuestThreadLocation list) : string =
        locations |> List.map renderThread |> String.concat "; "

    /// <summary>
    /// One line per non-terminated thread, joined by <c>"; "</c>: the summary PawPrint prints
    /// when it gives up on a guest.
    /// </summary>
    let describe (state : IlMachineState) : string = ofState state |> renderThreads

    /// <summary>
    /// <paramref name="message" /> with the guest's position appended.
    /// </summary>
    let annotate (message : string) (locations : GuestThreadLocation list) : string =
        // On its own line, and after the original message rather than before it: the first line
        // of an exception message is what most log lines and test runners show, and "what went
        // wrong" outranks "where the guest was" for that slot.
        $"%s{message}%s{System.Environment.NewLine}  Guest was: %s{renderThreads locations}"

/// <summary>
/// A host-side failure raised while interpreting guest code, annotated with where the guest was
/// when it happened.
/// </summary>
/// <remarks>
/// <para>
/// PawPrint fails by <c>failwith</c> in some 2,400 places, and almost none of them can name the
/// guest: the most context-free messages of all come from pure helpers deep in the opcode
/// implementations, which have no <c>IlMachineState</c> to consult and are much better off
/// without one. So the annotation is applied once, at the boundary in
/// <c>AbstractMachine.executeOneStep</c>, rather than at the sites.
/// </para>
/// <para>
/// The original exception is kept as <c>InnerException</c>, not flattened into text: it carries
/// the *host* stack trace, which says where in PawPrint's own source the failure was raised.
/// A reader debugging PawPrint needs that as much as the guest location, and the two answer
/// different questions.
/// </para>
/// <para>
/// <see cref="Guest" /> is the structured position rather than only the rendered string, because
/// the App, the debugger server and the test harness each want to present it differently.
/// </para>
/// </remarks>
type GuestFailureException (inner : exn, guest : GuestThreadLocation list) =
    inherit System.Exception (GuestLocation.annotate inner.Message guest, inner)

    /// <summary>
    /// Where each live guest thread was when the failure was raised.
    /// </summary>
    member _.Guest : GuestThreadLocation list = guest
