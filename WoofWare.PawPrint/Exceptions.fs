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

type ExceptionFilterContinuation<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        CurrentFilter : ExceptionFilterRegion
        SkippedFilters : ExceptionFilterRegion list
        SearchPC : int
        CliException : CliException<'typeGen, 'methodGen, 'methodVar>
    }

/// Represents what to do after executing a finally/filter block
type ExceptionContinuation<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    | ResumeAfterFinally of targetPC : int
    | PropagatingException of exn : CliException<'typeGen, 'methodGen, 'methodVar>
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
            "System.MissingMethodException", int 0x80131513u // COR_E_MISSINGMETHOD
            "System.ArgumentException", int 0x80070057u // COR_E_ARGUMENT
            "System.ArgumentNullException", 0x80004003 // E_POINTER (ArgumentNullException maps to E_POINTER in the CLR)
            "System.NotSupportedException", int 0x80131515u // COR_E_NOTSUPPORTED
            "System.DuplicateWaitObjectException", int 0x80131529u // COR_E_DUPLICATEWAITOBJECT
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

    /// Every `finally` that must run when control leaves `currentPC` for `targetPC`,
    /// ordered innermost first — the order ECMA-335 III.3.55 requires `leave` to run them in.
    ///
    /// Takes a bare region table rather than a method: this is the whole of the decision —
    /// which handlers, in which order — while `findFinallyBlocksToRun` below only fetches the
    /// table. Tests can then exercise the rule against hand-built towers of regions without
    /// standing up a method.
    ///
    /// The ordering key is `(-TryOffset, TryLength)`. Sorting on `-TryOffset` alone is not
    /// total: a `try` may begin at the same IL offset as the `try` enclosing it, and then only
    /// the extent distinguishes them, the shorter being the inner. ECMA-335 II.25.4.6 does
    /// require the table itself to list more deeply nested clauses first, so a stable sort on
    /// the offset alone happens to work today — but that leaves correctness resting on both
    /// the producer's clause order and `Seq.sortBy`'s stability, neither of which is stated
    /// where a reader of this function would look.
    let finallyBlocksBetween (regions : ExceptionRegion seq) (currentPC : int) (targetPC : int) : ExceptionOffset list =
        regions
        |> Seq.choose (fun region ->
            match region with
            | ExceptionRegion.Finally offset ->
                // We're leaving if we're in the try block and target is outside
                if
                    currentPC >= offset.TryOffset
                    && currentPC < offset.TryOffset + offset.TryLength
                    && (targetPC < offset.TryOffset || targetPC >= offset.TryOffset + offset.TryLength)
                then
                    Some offset
                else
                    None
            | ExceptionRegion.Filter _
            | ExceptionRegion.Catch _
            | ExceptionRegion.Fault _ -> None
        )
        |> Seq.sortBy (fun offset ->
            // Inner to outer: later-starting first, and among regions that start together,
            // the shorter one is the nested one.
            -offset.TryOffset, offset.TryLength
        )
        |> Seq.toList

    let findFinallyBlocksToRun
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo<'typeGeneric, 'methodGeneric, 'methodVar>)
        : ExceptionOffset list
        =
        match MethodInfo.tryIlBody method with
        | None -> []
        | Some instructions -> finallyBlocksBetween instructions.ExceptionRegions currentPC targetPC

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
    ///
    /// `justRan.TryOffset` stands in for the original leave site, which the continuation does
    /// not carry, and it selects the same remaining regions. Any region still to run properly
    /// encloses `justRan`: it contained the leave site, and two protected regions sharing a
    /// point must nest (ECMA-335 II.12.4.2.7 forbids partial overlap), so one that began after
    /// `justRan.TryOffset` would be nested *inside* `justRan` and would therefore already have
    /// run before it. Enclosing regions contain the whole of `justRan`, hence its first byte.
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

        finallyBlocksBetween regions justRan.TryOffset targetPC |> afterJustRan

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

    /// Get the active exception regions at a given offset
    let getActiveRegionsAtOffset
        (offset : int)
        (method : WoofWare.PawPrint.MethodInfo<'a, 'b, 'c>)
        : WoofWare.PawPrint.ExceptionRegion list
        =
        match MethodInfo.tryIlBody method with
        | None -> []
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.filter (fun region ->
                match region with
                | ExceptionRegion.Catch (_, exOffset)
                | ExceptionRegion.Finally exOffset
                | ExceptionRegion.Fault exOffset
                | ExceptionRegion.Filter (_, exOffset) ->
                    offset >= exOffset.TryOffset && offset < exOffset.TryOffset + exOffset.TryLength
            )
            |> Seq.toList
