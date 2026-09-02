namespace WoofWare.PawPrint

open System
open System.Collections.Immutable

/// Represents a location in the code where an exception occurred
type ExceptionStackFrame<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        Method : WoofWare.PawPrint.MethodInfo<'typeGen, 'methodGen, 'methodVar>
        /// The number of bytes into the IL of the method we were in
        IlOffset : int
        /// This is the last frame carried over from an *earlier* throw of the same exception —
        /// i.e. the trace continues past it with frames from a later re-raise. Set only when
        /// `ExceptionDispatchInfo.Throw()` splices a captured trace back on; renders as
        /// "--- End of stack trace from previous location ---" after this frame's line.
        ///
        /// A property of the frame rather than a separator between frames because that is how the
        /// CLR exposes it: `STEF_LAST_FRAME_FROM_FOREIGN_STACK_TRACE` (clrex.h:26) is a bit on the
        /// `StackTraceElement`, and `debugdebugger.cpp:475-477` materialises those bits into a
        /// `bool[]` parallel to the frame array, which managed code reads as
        /// `System.Diagnostics.StackFrame.IsLastFrameFromForeignExceptionStackTrace`. Any
        /// representation PawPrint chose would have to answer that question frame by frame.
        ///
        /// More than one frame in a trace can carry this: each capture/rethrow hop adds another.
        IsLastFrameFromForeignExceptionStackTrace : bool
    }

/// Represents a CLI exception being propagated
type CliException<'typeGen, 'methodGen, 'methodVar when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        /// The exception object allocated on the heap
        ExceptionObject : ManagedHeapAddress
        /// Stack trace built during unwinding
        StackTrace : ExceptionStackFrame<'typeGen, 'methodGen, 'methodVar> list
    }

type ExceptionFilterRegion =
    {
        FilterOffset : int
        HandlerOffset : ExceptionOffset
    }

/// How a first-pass handler search concluded. Every case but `NoHandler` names the frame the
/// second pass unwinds *to*; the frame itself stays live until the second pass reaches it.
///
/// The payload is no more than a `FrameId` plus, where a frame has several clauses,
/// which one won. Everything else the second pass needs — the wrap flags on the frame's
/// `ReturnState`, the parked filter continuation, the caller to advance to — is still readable
/// from the live frame when the second pass arrives, so copying it here could only introduce a
/// second version of the truth.
type ExceptionSearchOutcome =
    /// A `catch` or `filter` clause of this frame accepted the exception. For a `Filter`, its
    /// body has *already* run — in the first pass, with the inner frames still live — so the
    /// second pass enters `HandlerOffset` directly rather than evaluating anything.
    | CaughtAt of frame : FrameId * handler : ExceptionRegion
    /// This frame is evaluating an exception filter which this exception escaped. The CLR
    /// catches such an exception at the filter boundary and reports the filter as false, so the
    /// second pass runs the cleanup between the throw point and the boundary, discards the
    /// exception, and resumes the search parked on that filter's continuation.
    | AbandonedAtFilter of frame : FrameId
    /// Leaving this frame interposes a synthesised wrapper — a `TypeInitializationException`
    /// around a throwing `.cctor`, a `TargetInvocationException` around an
    /// `Activator.CreateInstance<T>()` ctor, or both. The search cannot continue past it,
    /// because the wrap changes the exception's *type* and every outer frame must be searched
    /// against the wrapper instead. The second pass unwinds to here, wraps, and starts a fresh
    /// first pass at the caller.
    | WrappedAt of frame : FrameId
    /// No frame on the thread has a handler. The second pass still runs — measured on .NET 10,
    /// an unhandled exception unwinds and runs every `finally` after the runtime has reported
    /// it — and terminates the thread when it reaches the outermost frame.
    | NoHandler

/// The state of a first-pass handler search: a walk outward along the `ReturnState.JumpTo`
/// chain that pops nothing, appending a stack-trace frame per frame boundary crossed and
/// running any filter it meets in place.
type ExceptionSearchState<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        /// The exception being dispatched, carrying the trace accumulated by the walk so far.
        Exception : CliException<'typeGen, 'methodGen, 'methodVar>
        /// Carried rather than re-read from the heap so that the walk works on a state whose
        /// exception object is not a real heap object, as low-level dispatch tests use.
        ExceptionType : 'typeGen
        /// The frame the raise began in, and the IL offset within it from which the second pass
        /// must start looking for cleanup. Not recoverable afterwards: the first pass runs
        /// filters in place, so it may leave the thread's active frame at some outer frame.
        StartFrame : FrameId
        StartPC : int
        /// Where the walk currently is.
        Frame : FrameId
        SearchPC : int
        /// Filters of `Frame` that have already run and rejected this exception. Cleared
        /// whenever the walk advances to a caller.
        SkippedFilters : ExceptionFilterRegion list
    }

/// The state of a second-pass unwind: a walk from the throw point to the frame that `Target`
/// names, running each `finally`/`fault` clause in between.
type ExceptionUnwindState<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        /// The exception, carrying the trace the first pass completed. By the time any clause
        /// this unwind runs can observe it, it has already been projected onto the exception
        /// object, which is the whole point of separating the passes.
        Exception : CliException<'typeGen, 'methodGen, 'methodVar>
        ExceptionType : 'typeGen
        /// Where the unwind currently is. `PC` is only ever the *entry* PC of a frame: once a
        /// cleanup clause has been entered, the frame's own live `IlOpIndex` — by then inside
        /// the handler body — is what the resume re-searches from, which is what stops a
        /// tower of nested clauses from re-entering the one that just ran.
        Frame : FrameId
        PC : int
        Target : ExceptionSearchOutcome
    }

/// A first-pass search suspended in the middle of evaluating one `filter` clause.
type ExceptionFilterContinuation<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        CurrentFilter : ExceptionFilterRegion
        /// The first-pass search that suspended to evaluate `CurrentFilter`. Its
        /// `SkippedFilters` does *not* yet include `CurrentFilter`; rejection adds it.
        Search : ExceptionSearchState<'typeGen, 'methodGen, 'methodVar>
    }

/// Represents what to do after executing a finally/filter block
type ExceptionContinuation<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    | ResumeAfterFinally of targetPC : int
    | PropagatingException of unwind : ExceptionUnwindState<'typeGen, 'methodGen, 'methodVar>
    | ResumeAfterFilter of continuation : ExceptionFilterContinuation<'typeGen, 'methodGen, 'methodVar>

type ExceptionContinuationScope =
    | FinallyHandler of offset : ExceptionOffset
    | FaultHandler of offset : ExceptionOffset
    | FilterHandler of filter : ExceptionFilterRegion

type ExceptionContinuationFrame<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        Scope : ExceptionContinuationScope
        Continuation : ExceptionContinuation<'typeGen, 'methodGen, 'methodVar>
    }

/// Maps CLR exception type full names to the HResult the real CLR would set for a
/// runtime-synthesised exception of that type.  Entries here correspond to
/// EEException::GetHR() in the CLR source.
/// This module is internal so that tests can validate the values against the real CLR.
[<RequireQualifiedAccess>]
module internal ExceptionHResults =

    /// Maps CLR exception type full names to their HResult values.
    let table : (string * int) list =
        [
            "System.NullReferenceException", 0x80004003 // E_POINTER
            "System.IndexOutOfRangeException", int 0x80131508u // COR_E_INDEXOUTOFRANGE
            "System.DivideByZeroException", 0x80020012 // COR_E_DIVIDEBYZERO
            "System.OverflowException", int 0x80131516u // COR_E_OVERFLOW
            "System.InvalidCastException", 0x80004002 // COR_E_INVALIDCAST
            "System.ArithmeticException", 0x80070216 // COR_E_ARITHMETIC
            "System.StackOverflowException", int 0x800703E9u // COR_E_STACKOVERFLOW
            "System.OutOfMemoryException", 0x8007000E // COR_E_OUTOFMEMORY
            "System.TypeInitializationException", int 0x80131534u // COR_E_TYPEINITIALIZATION
            "System.TypeLoadException", int 0x80131522u // COR_E_TYPELOAD
            "System.MissingFieldException", int 0x80131511u // COR_E_MISSINGFIELD
            "System.FieldAccessException", int 0x80131507u // COR_E_FIELDACCESS
            "System.MissingMethodException", int 0x80131513u // COR_E_MISSINGMETHOD
            "System.ArgumentException", int 0x80070057u // COR_E_ARGUMENT
            "System.ArgumentNullException", 0x80004003 // E_POINTER (ArgumentNullException maps to E_POINTER in the CLR)
            "System.NotSupportedException", int 0x80131515u // COR_E_NOTSUPPORTED
            "System.DuplicateWaitObjectException", int 0x80131529u // COR_E_DUPLICATEWAITOBJECT
            "System.InvalidProgramException", int 0x8013153Au // COR_E_INVALIDPROGRAM
            "System.BadImageFormatException", int 0x8007000Bu // COR_E_BADIMAGEFORMAT
            "System.ArgumentOutOfRangeException", int 0x80131502u // COR_E_ARGUMENTOUTOFRANGE
            "System.Reflection.TargetInvocationException", int 0x80131604u // COR_E_TARGETINVOCATION
        ]

    /// The fallback HResult for exception types not in the table.
    let corEException : int = int 0x80131500u

    /// Look up the HResult for a given fully-qualified type name, falling back to COR_E_EXCEPTION.
    let lookup (fullName : string) : int =
        match table |> List.tryFind (fun (name, _) -> name = fullName) with
        | Some (_, hresult) -> hresult
        | None -> corEException

/// Helper functions for exception handling
[<RequireQualifiedAccess>]
module ExceptionHandling =

    let isInHandlerBody (pc : int) (offset : ExceptionOffset) : bool =
        pc >= offset.HandlerOffset && pc < offset.HandlerOffset + offset.HandlerLength

    let findCatchHandlersToLeave
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo<'typeGeneric, 'methodGeneric, 'methodVar>)
        : ExceptionOffset list
        =
        match MethodInfo.tryIlBody method with
        | None -> []
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.choose (fun region ->
                match region with
                | ExceptionRegion.Catch (_, offset)
                | ExceptionRegion.Filter (_, offset) ->
                    if isInHandlerBody currentPC offset && not (isInHandlerBody targetPC offset) then
                        Some offset
                    else
                        None
                | ExceptionRegion.Finally _
                | ExceptionRegion.Fault _ -> None
            )
            |> Seq.toList

    /// The `ExceptionOffset` of any exception region, whatever its clause kind.
    let regionOffset (region : ExceptionRegion) : ExceptionOffset =
        match region with
        | ExceptionRegion.Catch (_, offset)
        | ExceptionRegion.Filter (_, offset)
        | ExceptionRegion.Finally offset
        | ExceptionRegion.Fault offset -> offset

    /// True iff `offset`'s *protected* region — its `try`, not its handler — contains `pc`.
    let private tryRegionCovers (pc : int) (offset : ExceptionOffset) : bool =
        pc >= offset.TryOffset && pc < offset.TryOffset + offset.TryLength

    /// The regions accepted by `isWanted` whose `try` covers `currentPC` but not `boundary`,
    /// ordered innermost first. `None` for `boundary` means nothing in this method is the
    /// destination, so every covering region qualifies.
    let private regionsBetween
        (regions : ExceptionRegion seq)
        (isWanted : ExceptionRegion -> bool)
        (currentPC : int)
        (boundary : int option)
        : ExceptionRegion list
        =
        regions
        |> Seq.filter (fun region ->
            if not (isWanted region) then
                false
            else

            let offset = regionOffset region

            tryRegionCovers currentPC offset
            && (
                match boundary with
                | None -> true
                | Some boundary -> not (tryRegionCovers boundary offset)
            )
        )
        |> Seq.sortBy (fun region ->
            // Inner to outer: later-starting first, and among regions that start together,
            // the shorter one is the nested one. Sorting on `-TryOffset` alone is not total:
            // a `try` may begin at the same IL offset as the `try` enclosing it, and then only
            // the extent distinguishes them. ECMA-335 II.25.4.6 does require the table itself
            // to list more deeply nested clauses first, so a stable sort on the offset alone
            // happens to work today — but that would leave correctness resting on both the
            // producer's clause order and `Seq.sortBy`'s stability.
            let offset = regionOffset region
            -offset.TryOffset, offset.TryLength
        )
        |> Seq.toList

    /// Every `finally` that must run when control leaves `currentPC` for `targetPC`,
    /// ordered innermost first — the order ECMA-335 III.3.55 requires `leave` to run them in.
    ///
    /// Takes a bare region table rather than a method: this is the whole of the decision —
    /// which handlers, in which order — while `findFinallyBlocksToRun` below only fetches the
    /// table. Tests can then exercise the rule against hand-built towers of regions without
    /// standing up a method.
    ///
    /// `fault` clauses are absent: `leave` is a *non-exceptional* transfer of
    /// control, and ECMA-335 III.3.55 runs only `finally` handlers for it. The exceptional
    /// counterpart, which does run both kinds, is `cleanupRegionsBetween` below.
    let finallyBlocksBetween (regions : ExceptionRegion seq) (currentPC : int) (targetPC : int) : ExceptionOffset list =
        regionsBetween
            regions
            (fun region ->
                match region with
                | ExceptionRegion.Finally _ -> true
                | ExceptionRegion.Catch _
                | ExceptionRegion.Filter _
                | ExceptionRegion.Fault _ -> false
            )
            currentPC
            (Some targetPC)
        |> List.map regionOffset

    /// Every exceptional-cleanup clause — `finally` *and* `fault` — that an exception leaving
    /// `currentPC` must run in this method, ordered innermost first. This is the second pass of
    /// CoreCLR's two-pass dispatch, restricted to one frame.
    ///
    /// `boundary` is the IL offset at which unwinding stops *inside this method*: the entry
    /// offset of the clause that is going to receive the exception — a `catch`/`filter`
    /// handler's `HandlerOffset`, or a filter's `FilterOffset` when the exception dies at the
    /// filter boundary. `None` means the exception is leaving the method altogether, so every
    /// covering clause runs.
    ///
    /// A cleanup clause whose `try` also covers
    /// the destination *encloses* the handler rather than lying between the throw point and it,
    /// so it must not run now; it runs later, when control eventually leaves it by `leave`.
    /// Without the exclusion this would run the `finally` of a plain C# `try/catch/finally`
    /// before its own `catch`, and then again on the way out: Roslyn lowers that construct to
    /// `try { try { … } catch { … } } finally { … }`, so the outer `finally`'s `try` covers the
    /// throw point *and* the catch handler. `sourcesPure/ComplexTryCatch.cs` is that shape.
    let cleanupRegionsBetween
        (regions : ExceptionRegion seq)
        (currentPC : int)
        (boundary : int option)
        : ExceptionRegion list
        =
        regionsBetween
            regions
            (fun region ->
                match region with
                | ExceptionRegion.Finally _
                | ExceptionRegion.Fault _ -> true
                | ExceptionRegion.Catch _
                | ExceptionRegion.Filter _ -> false
            )
            currentPC
            boundary

    let findFinallyBlocksToRun
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo<'typeGeneric, 'methodGeneric, 'methodVar>)
        : ExceptionOffset list
        =
        match MethodInfo.tryIlBody method with
        | None -> []
        | Some instructions -> finallyBlocksBetween instructions.ExceptionRegions currentPC targetPC

    /// The handlers a `leave` bound for `targetPC` has still to run once `justRan` completes,
    /// innermost first. `None` means `justRan` is not in that chain at all, which is a caller
    /// contract violation rather than an empty tail — the two are distinguished so the
    /// `MethodInfo` wrapper can report the former loudly.
    let finallyBlocksAfter
        (regions : ExceptionRegion seq)
        (justRan : ExceptionOffset)
        (targetPC : int)
        : ExceptionOffset list option
        =
        let rec afterJustRan (remaining : ExceptionOffset list) : ExceptionOffset list option =
            match remaining with
            | [] -> None
            | candidate :: rest -> if candidate = justRan then Some rest else afterJustRan rest

        // `justRan.TryOffset` stands in for the original leave site, which the continuation does
        // not carry, and it selects the same remaining regions. Any region still to run properly
        // encloses `justRan`: it contained the leave site, and two protected regions sharing a
        // point must nest (ECMA-335 II.12.4.2.7 forbids partial overlap), so one that began after
        // `justRan.TryOffset` would be nested *inside* `justRan` and would therefore already have
        // run before it. Enclosing regions contain the whole of `justRan`, hence its first byte.
        finallyBlocksBetween regions justRan.TryOffset targetPC |> afterJustRan

    /// The next `finally` a `leave` bound for `targetPC` must run, given that `justRan` — the
    /// innermost one not yet accounted for — has just completed. `None` once the chain is
    /// exhausted, at which point control belongs at `targetPC`.
    ///
    /// A single `leave` may exit several nested protected regions at once, and ECMA-335
    /// III.3.55 requires every one of their handlers to run, innermost first. Rather than
    /// carrying the remaining list in the continuation, each `endfinally` asks this for its
    /// successor. That keeps `MethodState`'s continuation shape unchanged, and means there is
    /// no second copy of the chain that could disagree with the method's own handler table if
    /// an exception unwinds partway through it.
    let nextFinallyToRun
        (justRan : ExceptionOffset)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo<'typeGeneric, 'methodGeneric, 'methodVar>)
        : ExceptionOffset option
        =
        let regions =
            match MethodInfo.tryIlBody method with
            | None -> Seq.empty
            | Some instructions -> instructions.ExceptionRegions :> seq<_>

        match finallyBlocksAfter regions justRan targetPC with
        | Some remaining -> List.tryHead remaining
        | None ->
            // `justRan` is absent from a list built from a point inside its own try and a
            // target outside it, which is the exact condition that put it there. Reaching
            // here means the handler table changed under us, or the continuation named a
            // region belonging to a different method.
            failwith
                $"endfinally: finally handler at IL offset %d{justRan.HandlerOffset} (try %d{justRan.TryOffset}..%d{justRan.TryOffset + justRan.TryLength}) is not among the finally regions a leave to IL offset %d{targetPC} would run in %s{method.Name}"
