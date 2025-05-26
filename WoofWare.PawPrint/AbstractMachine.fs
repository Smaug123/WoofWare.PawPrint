namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type ManagedObject =
    {
        Fields : (string * CliType) list
        SyncBlock : unit
    }

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Instructions with
        | None ->
            failwith
                $"TODO: tried to IL-interpret a method in {snd(instruction.ExecutingMethod.DeclaringType).Name} named {instruction.ExecutingMethod.Name} with no implementation"

        | Some instructions ->

        match instructions.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ -> failwith "Wanted to execute a nonexistent instruction"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly (snd instruction.ExecutingMethod.DeclaringType) with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue (fst instruction.ExecutingMethod.DeclaringType) with
                | true, v -> v.Name
                | false, _ -> "<unrecognised type>"

        logger.LogInformation (
            "Executing one step (index {ExecutingIlOpIndex} in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            executingInType,
            instruction.ExecutingMethod.Name,
            executingInstruction
        )

        match instruction.ExecutingMethod.Instructions.Value.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> NullaryIlOp.execute state thread op
        | IlOp.UnaryConst unaryConstIlOp ->
            UnaryConstIlOp.execute state thread unaryConstIlOp |> ExecutionResult.Stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.Stepped
        | IlOp.Switch immutableArray -> failwith "TODO: Switch unimplemented"
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            UnaryStringTokenIlOp.execute baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
