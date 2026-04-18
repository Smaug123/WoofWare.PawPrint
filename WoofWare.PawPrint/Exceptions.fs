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
    }

/// Represents a CLI exception being propagated
type CliException<'typeGen, 'methodGen, 'methodVar when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    {
        /// The exception object allocated on the heap
        ExceptionObject : ManagedHeapAddress
        /// Stack trace built during unwinding
        StackTrace : ExceptionStackFrame<'typeGen, 'methodGen, 'methodVar> list
    }

/// Represents what to do after executing a finally/filter block
type ExceptionContinuation<'typeGen, 'methodGen, 'methodVar
    when 'typeGen : comparison and 'typeGen :> IComparable<'typeGen>> =
    | ResumeAfterFinally of targetPC : int
    | PropagatingException of exn : CliException<'typeGen, 'methodGen, 'methodVar>
    | ResumeAfterFilter of handlerPC : int * exn : CliException<'typeGen, 'methodGen, 'methodVar>

/// Helper functions for exception handling
[<RequireQualifiedAccess>]
module ExceptionHandling =

    /// Find finally blocks that need to run when leaving a try region
    let findFinallyBlocksToRun
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo<'typeGeneric, 'methodGeneric, 'methodVar>)
        : ExceptionOffset list
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
                        Some offset
                    else
                        None
                | _ -> None
            )
            |> Seq.sortBy (fun offset ->
                // Inner to outer
                -offset.TryOffset
            )
            |> Seq.toList

    /// Get the active exception regions at a given offset
    let getActiveRegionsAtOffset
        (offset : int)
        (method : WoofWare.PawPrint.MethodInfo<'a, 'b, 'c>)
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
