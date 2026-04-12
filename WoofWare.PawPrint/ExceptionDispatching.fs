namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Exception handler dispatch that requires IlMachineState for type resolution.
[<RequireQualifiedAccess>]
module ExceptionDispatching =

    /// Check if an exception type matches a catch handler type.
    let private isExceptionAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (exceptionType : ConcreteTypeHandle)
        (catchTypeToken : MetadataToken)
        : IlMachineState * bool
        =
        let state, catchTypeDefn, catchAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                ImmutableArray.Empty
                catchTypeToken

        let state, catchTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                catchAssy.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                catchTypeDefn

        let rec walk (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            if current = catchTypeHandle then
                state, true
            else
                let state, baseType =
                    IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state current

                match baseType with
                | None -> state, false
                | Some parent -> walk state parent

        walk state exceptionType

    /// Find the first matching exception handler for the given exception at the given PC.
    /// Also returns `isFinally : bool`: whether this is a `finally` block (as opposed to e.g. a `catch`).
    let findExceptionHandler
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (currentPC : int)
        (exceptionType : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<'typeGen, 'methodGeneric, 'methodVar>)
        : IlMachineState * (WoofWare.PawPrint.ExceptionRegion * bool) option
        =
        match method.Instructions with
        | None -> state, None
        | Some instructions ->

        let state, matches =
            ((state, []), instructions.ExceptionRegions)
            ||> Seq.fold (fun (state, acc) region ->
                match region with
                | ExceptionRegion.Catch (typeToken, offset) ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        let state, matches =
                            isExceptionAssignableTo
                                loggerFactory
                                baseClassTypes
                                state
                                activeAssy
                                exceptionType
                                typeToken

                        if matches then
                            state, (region, false) :: acc
                        else
                            state, acc
                    else
                        state, acc
                | ExceptionRegion.Filter (_filterOffset, offset) ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        failwith "TODO: filter needs to be evaluated"
                    else
                        state, acc
                | ExceptionRegion.Finally offset ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        state, (region, true) :: acc
                    else
                        state, acc
                | ExceptionRegion.Fault offset ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        state, (region, true) :: acc
                    else
                        state, acc
            )

        let result =
            match matches |> List.rev with
            | [] -> None
            | [ x ] -> Some x
            | _ -> failwith "multiple exception regions"

        state, result
