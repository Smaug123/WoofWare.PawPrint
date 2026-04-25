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
