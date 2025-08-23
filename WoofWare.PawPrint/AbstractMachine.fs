namespace WoofWare.PawPrint

open System.Collections.Immutable
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

                    let target =
                        match delegateToRun.Fields.["_target"] with
                        | CliType.ObjectRef addr -> addr
                        | x -> failwith $"TODO: delegate target wasn't an object ref: %O{x}"

                    let methodPtr =
                        match delegateToRun.Fields.["_methodPtr"] with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                        | d -> failwith $"unexpectedly not a method pointer in delegate invocation: {d}"

                    let methodGenerics = instruction.ExecutingMethod.Generics

                    // When we return, we need to go back up the stack
                    match state |> IlMachineState.returnStackFrame loggerFactory baseClassTypes thread with
                    | None -> failwith "unexpectedly nowhere to return from delegate"
                    | Some state ->

                    // Push args
                    let state =
                        (state, instruction.Arguments)
                        ||> Seq.fold (fun state arg -> IlMachineState.pushToEvalStack arg thread state)

                    // The odd little calling convention strikes again: we push the `target` parameter on top of the
                    // stack, although that doesn't actually happen in the CLR.
                    // We'll pretend we're constructing an object, so that the calling convention gets respected in
                    // `callMethod`.
                    let state, constructing =
                        match target with
                        | None -> state, None
                        | Some target ->
                            let state =
                                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some target)) thread state

                            state, Some target

                    let state, _ =
                        state.WithThreadSwitchedToAssembly methodPtr.DeclaringType.Assembly thread

                    // Don't advance the program counter again on return; that was already done by the Callvirt that
                    // caused this delegate to be invoked.
                    let currentThreadState = state.ThreadState.[thread]

                    let state =
                        IlMachineStateExecution.callMethod
                            loggerFactory
                            baseClassTypes
                            None
                            constructing
                            false
                            false
                            false
                            methodGenerics
                            methodPtr
                            thread
                            currentThreadState
                            state

                    ExecutionResult.Stepped (state, WhatWeDid.Executed)
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
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ->
                    let env = ISystem_Environment_Env.get impls
                    env.GetProcessorCount thread state
                | "System.Private.CoreLib",
                  "System",
                  "Environment",
                  "_Exit",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
                  ConcreteVoid state.ConcreteTypes ->
                    let env = ISystem_Environment_Env.get impls
                    env._Exit thread state
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Monitor",
                  "ReliableEnter",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
                    ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ],
                  ConcreteVoid state.ConcreteTypes ->
                    let env = ISystem_Threading_Monitor_Env.get impls
                    env.ReliableEnter thread state
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Monitor",
                  "Exit",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
                  ConcreteVoid state.ConcreteTypes ->
                    let env = ISystem_Threading_Monitor_Env.get impls
                    env.Exit thread state
                | "System.Private.CoreLib",
                  "System",
                  "Type",
                  "GetField",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.String ; ty ],
                  ret ->
                    let ty = AllConcreteTypes.lookup ty state.ConcreteTypes |> Option.get
                    let ret = AllConcreteTypes.lookup ret state.ConcreteTypes |> Option.get

                    match ty.Namespace, ty.Name, ty.Generics.IsEmpty, ret.Namespace, ret.Name, ret.Generics.IsEmpty with
                    | "System.Reflection", "BindingFlags", true, "System.Reflection", "FieldInfo", true ->
                        failwith "TODO: GetField"
                    | _ -> failwith "unexpected signature for Type.GetField"
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
        | false, _ ->
            failwith
                $"Wanted to execute a nonexistent instruction in {instruction.ExecutingMethod.DeclaringType.Name}.{instruction.ExecutingMethod.Name}"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue instruction.ExecutingMethod.DeclaringType.Definition.Get with
                | true, v -> v.Name
                | false, _ -> "<unrecognised type>"

        logger.LogInformation (
            "Executing one step (index {ExecutingIlOpIndex}, max {MaxIlOpIndex}, in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            (Map.maxKeyValue instruction.ExecutingMethod.Instructions.Value.Locations |> fst),
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
