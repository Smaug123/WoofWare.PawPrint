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

            match DumpedAssembly.isDelegate baseClassTypes state._LoadedAssemblies targetType with
            | true ->
                match instruction.ReturnState with
                | None -> failwith "How come we don't have a return point from a delegate?!"
                | Some {
                           WasConstructingObj = Some _
                       } ->
                    IlMachineState.executeDelegateConstructor baseClassTypes instruction state
                    // can't advance the program counter here - there's no IL instructions executing!
                    |> IlMachineState.returnStackFrame loggerFactory baseClassTypes thread
                    |> function
                        | ReturnFrameResult.NormalReturn state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                        | result -> failwith $"unexpected ReturnFrameResult from delegate constructor: %A{result}"
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
                        match delegateToRun |> AllocatedNonArrayObject.DereferenceField "_target" with
                        | CliType.ObjectRef addr -> addr
                        | x -> failwith $"TODO: delegate target wasn't an object ref: %O{x}"

                    let methodPtr =
                        match delegateToRun |> AllocatedNonArrayObject.DereferenceField "_methodPtr" with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                        | d -> failwith $"unexpectedly not a method pointer in delegate invocation: {d}"

                    let methodGenerics = instruction.ExecutingMethod.Generics

                    // Preserve the original call-site offset from the callvirt Invoke that
                    // created this delegate frame.  After returnStackFrame the caller's
                    // IlOpIndex has already been advanced, so we must carry the original
                    // call-site through to the delegate target's MethodReturnState.
                    let originalCallSitePC =
                        instruction.ReturnState |> Option.map (fun rs -> rs.CallSiteIlOpIndex)

                    // When we return, we need to go back up the stack
                    match state |> IlMachineState.returnStackFrame loggerFactory baseClassTypes thread with
                    | ReturnFrameResult.NoFrameToReturn -> failwith "unexpectedly nowhere to return from delegate"
                    | ReturnFrameResult.DispatchException _ ->
                        failwith "unexpected exception dispatch from delegate frame pop"
                    | ReturnFrameResult.NormalReturn state ->

                    // Rebuild the stack in normal instance-call shape: `this` below the real arguments.
                    // Push `target` first (if instance method) so it ends up at the bottom.
                    let state =
                        match target with
                        | None -> state
                        | Some target -> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some target)) thread state

                    // Push the real invoke parameters, skipping instruction.Arguments.[0] which is the
                    // delegate object itself (not needed by the target method).
                    let state =
                        let mutable s = state

                        for i = 1 to instruction.Arguments.Length - 1 do
                            s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

                        s

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
                            None
                            false
                            false
                            false
                            methodGenerics
                            methodPtr
                            thread
                            currentThreadState
                            originalCallSitePC
                            false
                            state

                    ExecutionResult.Stepped (state, WhatWeDid.Executed)
            | false ->

            if not instruction.ExecutingMethod.IsNativeMethod then
                failwith
                    $"BUG: reached extern dispatch for {targetAssy.Name.Name} {targetType.Namespace}.{targetType.Name}::{instruction.ExecutingMethod.Name} which has no IL body but is not marked as a native method (ImplAttributes=%O{instruction.ExecutingMethod.ImplAttributes}, MethodAttributes=%O{instruction.ExecutingMethod.MethodAttributes})"

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
                  "System.Runtime.CompilerServices",
                  "RuntimeHelpers",
                  "RunClassConstructor",
                  [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                      "System.Runtime.CompilerServices",
                                                      "QCallTypeHandle",
                                                      generics) ],
                  ConcreteVoid state.ConcreteTypes when generics.IsEmpty ->
                    // QCall: triggers the .cctor for the type identified by the QCallTypeHandle argument.
                    // Extract the ConcreteTypeHandle from the QCallTypeHandle's _handle field, then
                    // ensure the type is initialised.
                    let state = IlMachineState.loadArgument thread 0 state
                    let arg, state = IlMachineState.popEvalStack thread state

                    let concreteTypeHandle =
                        match arg with
                        | EvalStackValue.UserDefinedValueType vt ->
                            match CliValueType.DereferenceField "_handle" vt with
                            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr cth)) -> cth
                            | other ->
                                failwith $"RunClassConstructor: expected TypeHandlePtr in _handle field, got %O{other}"
                        | other -> failwith $"RunClassConstructor: expected QCallTypeHandle value type, got %O{other}"

                    match concreteTypeHandle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Pointer, byref, and array type descriptors have no .cctor; CoreCLR treats this
                        // as a no-op. Return immediately.
                        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                    | ConcreteTypeHandle.Concrete _ ->

                    let state, typeInit =
                        IlMachineStateExecution.ensureTypeInitialised
                            loggerFactory
                            baseClassTypes
                            thread
                            concreteTypeHandle
                            state

                    match typeInit with
                    | WhatWeDid.Executed -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                    | WhatWeDid.SuspendedForClassInit ->
                        // The cctor was pushed as a new frame. We must NOT go through the normal
                        // returnStackFrame path (which would pop the cctor frame we just pushed).
                        // Instead, return Stepped directly so the dispatch loop runs the cctor.
                        // When the cctor finishes, returnStackFrame pops it, bringing us back to
                        // this native method frame. executeOneStep re-enters here and
                        // ensureTypeInitialised will return Executed.
                        ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit)
                    | WhatWeDid.ThrowingTypeInitializationException ->
                        (state, WhatWeDid.ThrowingTypeInitializationException)
                        |> ExecutionResult.Stepped
                    | WhatWeDid.BlockedOnClassInit blockedBy ->
                        // Another thread owns this type's .cctor lock. Yield so the scheduler
                        // can run that thread to completion before re-entering.
                        ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
                | "System.Private.CoreLib",
                  "System",
                  "RuntimeTypeHandle",
                  "GetAssembly",
                  [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                      "System",
                                                      "RuntimeType",
                                                      runtimeTypeGenerics) ],
                  ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                    "System.Reflection",
                                                    "RuntimeAssembly",
                                                    runtimeAssemblyGenerics) when
                    runtimeTypeGenerics.IsEmpty && runtimeAssemblyGenerics.IsEmpty
                    ->
                    // Load arg0 (the RuntimeType object)
                    let state = IlMachineState.loadArgument thread 0 state
                    let runtimeTypeRef, state = IlMachineState.popEvalStack thread state

                    // Get the heap object and read m_handle to find the ConcreteTypeHandle
                    let runtimeTypeAddr =
                        match runtimeTypeRef with
                        | EvalStackValue.ObjectRef addr -> addr
                        | other -> failwith $"GetAssembly: expected ObjectRef for RuntimeType argument, got %O{other}"

                    let heapObj = ManagedHeap.get runtimeTypeAddr state.ManagedHeap

                    let concreteTypeHandle =
                        match AllocatedNonArrayObject.DereferenceField "m_handle" heapObj with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr cth)) -> cth
                        | other -> failwith $"GetAssembly: expected TypeHandlePtr in m_handle field, got %O{other}"

                    // Unwrap Byref/Pointer/Array to reach the element type's Concrete handle.
                    // In .NET, typeof(T[]).Assembly == typeof(T).Assembly, so arrays follow the
                    // same rule: return the element type's assembly.
                    let rec unwrapToConcreteHandle (h : ConcreteTypeHandle) : ConcreteTypeHandle =
                        match h with
                        | ConcreteTypeHandle.Concrete _ -> h
                        | ConcreteTypeHandle.Byref inner -> unwrapToConcreteHandle inner
                        | ConcreteTypeHandle.Pointer inner -> unwrapToConcreteHandle inner
                        | ConcreteTypeHandle.OneDimArrayZero inner -> unwrapToConcreteHandle inner
                        | ConcreteTypeHandle.Array (inner, _) -> unwrapToConcreteHandle inner

                    let concreteHandle = unwrapToConcreteHandle concreteTypeHandle

                    // Look up the assembly for this type
                    let concreteType =
                        AllConcreteTypes.lookup concreteHandle state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"GetAssembly: could not find concrete type for handle %O{concreteTypeHandle} (unwrapped to %O{concreteHandle})"
                        )

                    let assemblyName = concreteType.Assembly

                    // Return a cached RuntimeAssembly object if we already created one for this assembly,
                    // so that two types from the same assembly return reference-identical Assembly objects.
                    match state.RuntimeAssemblyObjects.TryGetValue assemblyName.FullName with
                    | true, cachedAddr ->
                        let state =
                            IlMachineState.pushToEvalStack (CliType.ObjectRef (Some cachedAddr)) thread state

                        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                    | false, _ ->

                    // Concretize RuntimeAssembly type
                    let runtimeAssemblyTypeInfo =
                        baseClassTypes.Corelib.TypeDefs
                        |> Seq.choose (fun (KeyValue (_, v)) ->
                            if v.Namespace = "System.Reflection" && v.Name = "RuntimeAssembly" then
                                Some v
                            else
                                None
                        )
                        |> Seq.exactlyOne

                    let stk =
                        DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies runtimeAssemblyTypeInfo

                    let state, runtimeAssemblyTypeHandle =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseClassTypes.Corelib.Name
                            System.Collections.Immutable.ImmutableArray.Empty
                            System.Collections.Immutable.ImmutableArray.Empty
                            (TypeDefn.FromDefinition (runtimeAssemblyTypeInfo.Identity, stk))

                    // Collect all fields and allocate the RuntimeAssembly object
                    let state, allFields =
                        IlMachineState.collectAllInstanceFields
                            loggerFactory
                            baseClassTypes
                            state
                            runtimeAssemblyTypeHandle

                    let fields = CliValueType.OfFields runtimeAssemblyTypeInfo.Layout allFields

                    let addr, state =
                        IlMachineState.allocateManagedObject runtimeAssemblyTypeHandle fields state

                    // Set the m_assembly field to a tagged native pointer so downstream native
                    // calls can map back to the PawPrint DumpedAssembly.
                    let updatedObj =
                        ManagedHeap.get addr state.ManagedHeap
                        |> AllocatedNonArrayObject.SetField
                            "m_assembly"
                            (CliType.Numeric (
                                CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyName.FullName)
                            ))

                    let state =
                        { state with
                            ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                            RuntimeAssemblyObjects = state.RuntimeAssemblyObjects.Add (assemblyName.FullName, addr)
                        }

                    // Push the RuntimeAssembly object ref onto the eval stack
                    let state =
                        IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) thread state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Thread",
                  "GetCurrentThreadNative",
                  [],
                  ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                    "System.Threading",
                                                    "Thread",
                                                    threadGenerics) when threadGenerics.IsEmpty ->
                    let addr, state =
                        IlMachineState.getOrAllocateManagedThreadObject loggerFactory baseClassTypes thread state

                    let state =
                        IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) thread state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
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
                    let implKind =
                        if instruction.ExecutingMethod.IsCliInternal then
                            "InternalCall"
                        elif instruction.ExecutingMethod.IsPinvokeImpl then
                            "PInvokeImpl"
                        elif
                            instruction.ExecutingMethod.ImplAttributes.HasFlag
                                System.Reflection.MethodImplAttributes.Runtime
                        then
                            "Runtime"
                        else
                            $"Unknown (ImplAttributes=%O{instruction.ExecutingMethod.ImplAttributes})"

                    let rec formatTypeHandle (cth : ConcreteTypeHandle) : string =
                        match AllConcreteTypes.lookup cth state.ConcreteTypes with
                        | Some ct -> $"{ct.Namespace}.{ct.Name}"
                        | None ->
                            match cth with
                            | ConcreteTypeHandle.Byref inner -> $"&({formatTypeHandle inner})"
                            | ConcreteTypeHandle.Pointer inner -> $"*({formatTypeHandle inner})"
                            | ConcreteTypeHandle.OneDimArrayZero inner -> $"{formatTypeHandle inner}[]"
                            | ConcreteTypeHandle.Array (inner, rank) ->
                                let dims = if rank <= 1 then "*" else String.replicate (rank - 1) ","
                                $"{formatTypeHandle inner}[{dims}]"
                            | ConcreteTypeHandle.Concrete i -> string i

                    let paramStr = param |> List.map formatTypeHandle |> String.concat ", "
                    let retStr = formatTypeHandle retType

                    failwith
                        $"Unimplemented native method ({implKind}): {assy} {ns}.{typeName}::{methName}({paramStr}) -> {retStr}. Add a mock implementation in ExternImplementations."

            match outcome with
            | ExecutionResult.Terminated (state, terminating) -> ExecutionResult.Terminated (state, terminating)
            | ExecutionResult.UnhandledException _ -> outcome
            | ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit) ->
                // A cctor was pushed; the native frame must stay on the stack so the dispatch loop
                // runs the cctor first, then re-enters this native method on the next step.
                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit)
            | ExecutionResult.Stepped (state, WhatWeDid.ThrowingTypeInitializationException) ->
                // Exception dispatch has already unwound past this native frame to the matching
                // handler, so returnStackFrame would pop the wrong frame.
                ExecutionResult.Stepped (state, WhatWeDid.ThrowingTypeInitializationException)
            | ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy) ->
                // Another thread owns this type's .cctor lock; the native frame must persist
                // until that thread finishes, then we re-enter.
                ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
            | ExecutionResult.Stepped (state, whatWeDid) ->
                match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
                | ReturnFrameResult.NormalReturn state -> ExecutionResult.Stepped (state, whatWeDid)
                | result -> failwith $"unexpected ReturnFrameResult from extern method return: %A{result}"

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
            UnaryStringTokenIlOp.execute loggerFactory baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
