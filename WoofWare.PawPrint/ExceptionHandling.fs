namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// Represents a location in the code where an exception occurred
type StackFrame =
    {
        Method : WoofWare.PawPrint.MethodInfo
        IlOffset : int
    }

/// Represents a CLI exception being propagated
type CliException =
    {
        /// The actual exception object on the heap
        ExceptionObject : ManagedHeapAddress
        /// Stack trace built during unwinding
        StackTrace : StackFrame list
    }

/// Represents what to do after executing a finally/filter block
type ExceptionContinuation =
    | ResumeAfterFinally of targetPC : int
    | PropagatingException of exn : CliException
    | ResumeAfterFilter of handlerPC : int * exn : CliException

/// Helper functions for exception handling
[<RequireQualifiedAccess>]
module ExceptionHandling =

    /// Check if an exception type matches a catch handler type
    let private isExceptionAssignableTo
        (exceptionTypeCrate : TypeInfoCrate)
        (catchTypeToken : MetadataToken)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : bool
        =
        // TODO: Implement proper type assignability checking
        // For now, we'll accept all exceptions to get basic functionality working
        true

    /// Find the first matching exception handler for the given exception at the given PC
    let findExceptionHandler
        (currentPC : int)
        (exceptionTypeCrate : TypeInfoCrate)
        (method : WoofWare.PawPrint.MethodInfo)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : (WoofWare.PawPrint.ExceptionRegion * bool) option // handler, isFinally
        =
        match method.Instructions with
        | None -> None
        | Some instructions ->
            // Find all handlers that cover the current PC
            instructions.ExceptionRegions
            |> Seq.tryPick (fun region ->
                match region with
                | ExceptionRegion.Catch (typeToken, offset) when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    // Check if exception type matches
                    if isExceptionAssignableTo exceptionTypeCrate typeToken assemblies then
                        Some (region, false)
                    else
                        None
                | ExceptionRegion.Filter (filterOffset, offset) when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    // Filter needs to be evaluated
                    Some (region, false)
                | ExceptionRegion.Finally offset when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    Some (region, true)
                | ExceptionRegion.Fault offset when
                    currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength
                    ->
                    Some (region, true)
                | _ -> None
            )

    /// Find finally blocks that need to run when leaving a try region
    let findFinallyBlocksToRun
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo)
        : WoofWare.PawPrint.ExceptionRegion list
        =
        match method.Instructions with
        | None -> []
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.choose (fun region ->
                match region with
                | ExceptionRegion.Finally offset ->
                    // We're leaving if we're in the try block and target is outside
                    if
                        currentPC >= offset.TryOffset
                        && currentPC < offset.TryOffset + offset.TryLength
                        && (targetPC < offset.TryOffset || targetPC >= offset.TryOffset + offset.TryLength)
                    then
                        Some region
                    else
                        None
                | _ -> None
            )
            |> Seq.sortBy (fun region ->
                match region with
                | ExceptionRegion.Finally offset -> -offset.TryOffset
                | _ -> 0
            ) // Inner to outer
            |> Seq.toList

    /// Get the active exception regions at a given offset
    let getActiveRegionsAtOffset
        (offset : int)
        (method : WoofWare.PawPrint.MethodInfo)
        : WoofWare.PawPrint.ExceptionRegion list
        =
        match method.Instructions with
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
