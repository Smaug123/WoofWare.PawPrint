namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core
open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        impls
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Instructions with
        | None ->
            let targetAssy =
                state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly
                |> Option.get

            let targetType =
                targetAssy.TypeDefs.[instruction.ExecutingMethod.DeclaringType.Definition.Get]

            let baseType =
                DumpedAssembly.resolveBaseType
                    baseClassTypes
                    state._LoadedAssemblies
                    targetAssy.Name
                    targetType.BaseType

            match baseType with
            | ResolvedBaseType.Delegate ->
                match instruction.ReturnState with
                | None -> failwith "How come we don't have a return point from a delegate?!"
                | Some {
                           WasConstructingObj = Some _
                       } ->
                    IlMachineState.executeDelegateConstructor instruction state
                    // can't advance the program counter here - there's no IL instructions executing!
                    |> IlMachineState.returnStackFrame loggerFactory baseClassTypes thread
                    |> Option.get
                    |> Tuple.withRight WhatWeDid.Executed
                    |> ExecutionResult.Stepped
                | Some {
                           WasConstructingObj = None
                       } ->
                    // We've been instructed to run a delegate.
                    let delegateToRunAddr =
                        match instruction.Arguments.[0] with
                        | CliType.ObjectRef (Some addr) -> addr
                        | _ -> failwith "expected a managed object ref to delegate"

                    let delegateToRun = state.ManagedHeap.NonArrayObjects.[delegateToRunAddr]

                    if delegateToRun.Fields.["_target"] <> CliType.ObjectRef None then
                        failwith "TODO: delegate target wasn't None"

                    let methodPtr =
                        match delegateToRun.Fields.["_methodPtr"] with
                        | CliType.Numeric (CliNumericType.ProvenanceTrackedNativeInt64 mi) -> mi
                        | _ -> failwith "unexpectedly not a method pointer in delegate invocation"

                    failwith "TODO"
            | _ ->

            let outcome =
                match
                    targetAssy.Name.Name,
                    targetType.Namespace,
                    targetType.Name,
                    instruction.ExecutingMethod.Name,
                    instruction.ExecutingMethod.Signature.ParameterTypes,
                    instruction.ExecutingMethod.Signature.ReturnType
                with
                | "System.Private.CoreLib",
                  "System",
                  "Environment",
                  "GetProcessorCount",
                  [],
                  TypeDefn.PrimitiveType PrimitiveType.Int32 ->
                    let env = ISystem_Environment_Env.get impls
                    env.GetProcessorCount thread state
                | "System.Private.CoreLib",
                  "System",
                  "Environment",
                  "_Exit",
                  [ TypeDefn.PrimitiveType PrimitiveType.Int32 ],
                  TypeDefn.Void ->
                    let env = ISystem_Environment_Env.get impls
                    env._Exit thread state
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Monitor",
                  "ReliableEnter",
                  [ TypeDefn.PrimitiveType PrimitiveType.Object
                    TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.Boolean) ],
                  TypeDefn.Void ->
                    let env = ISystem_Threading_Monitor_Env.get impls
                    env.ReliableEnter thread state
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Monitor",
                  "Exit",
                  [ TypeDefn.PrimitiveType PrimitiveType.Object ],
                  TypeDefn.Void ->
                    let env = ISystem_Threading_Monitor_Env.get impls
                    env.Exit thread state
                | assy, ns, typeName, methName, param, retType ->
                    failwith
                        $"TODO: tried to IL-interpret a method in {assy} {ns}.{typeName} named {methName} with no implementation; {param} -> {retType}"

            match outcome with
            | ExecutionResult.Terminated (state, terminating) -> ExecutionResult.Terminated (state, terminating)
            | ExecutionResult.Stepped (state, whatWeDid) ->
                ExecutionResult.Stepped (
                    IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state
                    |> Option.get,
                    whatWeDid
                )

        | Some instructions ->

        match instructions.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ -> failwith "Wanted to execute a nonexistent instruction"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue instruction.ExecutingMethod.DeclaringType.Definition.Get with
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
        | IlOp.Nullary op -> NullaryIlOp.execute loggerFactory baseClassTypes state thread op
        | IlOp.UnaryConst unaryConstIlOp ->
            UnaryConstIlOp.execute state thread unaryConstIlOp |> ExecutionResult.Stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.Stepped
        | IlOp.Switch immutableArray -> failwith "TODO: Switch unimplemented"
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            UnaryStringTokenIlOp.execute baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
