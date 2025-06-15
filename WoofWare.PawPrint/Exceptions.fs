namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Represents a location in the code where an exception occurred
type ExceptionStackFrame =
    {
        Method : WoofWare.PawPrint.MethodInfo<TypeDefn, TypeDefn>
        /// The number of bytes into the IL of the method we were in
        IlOffset : int
    }

/// Represents a CLI exception being propagated
type CliException =
    {
        /// The exception object allocated on the heap
        ExceptionObject : ManagedHeapAddress
        /// Stack trace built during unwinding
        StackTrace : ExceptionStackFrame list
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
        true

    /// Find the first matching exception handler for the given exception at the given PC.
    /// Also returns `isFinally : bool`: whether this is a `finally` block (as opposed to e.g. a `catch`).
    let findExceptionHandler
        (currentPC : int)
        (exceptionTypeCrate : TypeInfoCrate)
        (method : WoofWare.PawPrint.MethodInfo<TypeDefn, 'methodGeneric>)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : (WoofWare.PawPrint.ExceptionRegion * bool) option // handler, isFinally
        =
        match method.Instructions with
        | None -> None
        | Some instructions ->

        // Find all handlers that cover the current PC
        instructions.ExceptionRegions
        |> Seq.choose (fun region ->
            match region with
            | ExceptionRegion.Catch (typeToken, offset) ->
                if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                    // Check if exception type matches
                    if isExceptionAssignableTo exceptionTypeCrate typeToken assemblies then
                        Some (region, false)
                    else
                        None
                else
                    None
            | ExceptionRegion.Filter (filterOffset, offset) ->
                if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                    failwith "TODO: filter needs to be evaluated"
                else
                    None
            | ExceptionRegion.Finally offset ->
                if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                    Some (region, true)
                else
                    None
            | ExceptionRegion.Fault offset ->
                if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                    Some (region, true)
                else
                    None
        )
        |> Seq.toList
        |> fun x ->
            match x with
            | [] -> None
            | [ x ] -> Some x
            | _ -> failwith "multiple exception regions"

    /// Find finally blocks that need to run when leaving a try region
    let findFinallyBlocksToRun
        (currentPC : int)
        (targetPC : int)
        (method : WoofWare.PawPrint.MethodInfo<TypeDefn, 'methodGeneric>)
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
        (method : WoofWare.PawPrint.MethodInfo<TypeDefn, 'methodGeneric>)
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
