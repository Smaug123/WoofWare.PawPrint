namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core
open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let private qCallTypeHandleToConcreteTypeHandle (operation : string) (arg : EvalStackValue) : ConcreteTypeHandle =
        match arg with
        | EvalStackValue.UserDefinedValueType vt ->
            match CliValueType.DereferenceField "_handle" vt |> CliType.unwrapPrimitiveLike with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr cth)) ->
                match cth with
                | RuntimeTypeHandleTarget.Closed cth -> cth
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition resolved ->
                    failwith
                        $"%s{operation}: expected closed RuntimeTypeHandleTarget in QCallTypeHandle._handle, but got open generic"
            | other -> failwith $"%s{operation}: expected TypeHandlePtr in QCallTypeHandle._handle, got %O{other}"
        | other -> failwith $"%s{operation}: expected QCallTypeHandle value type, got %O{other}"

    let private gcHandleKindOfEvalStackValue (operation : string) (arg : EvalStackValue) : GcHandleKind =
        let value =
            match arg with
            | EvalStackValue.Int32 i -> i
            | other -> failwith $"%s{operation}: expected GCHandleType enum as int32, got %O{other}"

        match value with
        | 0 -> GcHandleKind.Weak
        | 1 -> GcHandleKind.WeakTrackResurrection
        | 2 -> GcHandleKind.Normal
        | 3 -> GcHandleKind.Pinned
        | 6 -> GcHandleKind.Dependent
        | other -> failwith $"%s{operation}: unsupported GC handle kind %i{other}"

    let private objectTargetOfEvalStackValue (operation : string) (arg : EvalStackValue) : ManagedHeapAddress option =
        match arg with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | other -> failwith $"%s{operation}: expected object reference, got %O{other}"

    let private gcHandleAddressOfEvalStackValue (operation : string) (arg : EvalStackValue) : GcHandleAddress =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr handle) -> handle
        | other -> failwith $"%s{operation}: expected GC handle pointer, got %O{other}"

    let private pushGcHandleAddress
        (handle : GcHandleAddress)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr handle)) thread state

    let private pushObjectTarget
        (target : ManagedHeapAddress option)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        IlMachineState.pushToEvalStack (CliType.ObjectRef target) thread state

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
                        // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                        match
                            delegateToRun
                            |> AllocatedNonArrayObject.DereferenceField "_methodPtr"
                            |> CliType.unwrapPrimitiveLike
                        with
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
                  "get_CurrentManagedThreadId",
                  [],
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ->
                    let env = ISystem_Environment_Env.get impls
                    env.GetCurrentManagedThreadId thread state
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

                    let typeHandleTarget =
                        match arg with
                        | EvalStackValue.UserDefinedValueType vt ->
                            // QCallTypeHandle._handle is typed as IntPtr (a primitive-like wrapper),
                            // so the dereferenced field contents are wrapped; unwrap to the inner NativeInt.
                            match CliValueType.DereferenceField "_handle" vt |> CliType.unwrapPrimitiveLike with
                            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) ->
                                target
                            | other ->
                                failwith $"RunClassConstructor: expected TypeHandlePtr in _handle field, got %O{other}"
                        | other -> failwith $"RunClassConstructor: expected QCallTypeHandle value type, got %O{other}"

                    match typeHandleTarget with
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                        failwith
                            $"TODO: RuntimeHelpers.RunClassConstructor for open generic type definition %O{typeHandleTarget}"
                    | RuntimeTypeHandleTarget.Closed concreteTypeHandle ->
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
                  "GetGCHandle",
                  [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                      "System.Runtime.CompilerServices",
                                                      "QCallTypeHandle",
                                                      qCallGenerics)
                    ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                      "System.Runtime.InteropServices",
                                                      "GCHandleType",
                                                      gcHandleTypeGenerics) ],
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr when
                    qCallGenerics.IsEmpty && gcHandleTypeGenerics.IsEmpty
                    ->
                    let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType
                    let gcHandleType = instruction.Arguments.[1] |> EvalStackValue.ofCliType

                    let typeHandle =
                        qCallTypeHandleToConcreteTypeHandle "RuntimeTypeHandle.GetGCHandle" qCallHandle

                    let kind = gcHandleKindOfEvalStackValue "RuntimeTypeHandle.GetGCHandle" gcHandleType

                    let handle, gcHandles =
                        state.GcHandles
                        |> GcHandleRegistry.allocate kind (GcHandleOwner.TypeAssociated typeHandle) None

                    let state =
                        { state with
                            GcHandles = gcHandles
                        }

                    let state = pushGcHandleAddress handle thread state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System",
                  "RuntimeTypeHandle",
                  "FreeGCHandle",
                  [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                      "System.Runtime.CompilerServices",
                                                      "QCallTypeHandle",
                                                      qCallGenerics)
                    ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
                  returnType when qCallGenerics.IsEmpty ->
                    let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType
                    let objHandle = instruction.Arguments.[1] |> EvalStackValue.ofCliType

                    // Extract this for validation. CoreCLR uses the type's loader allocator to
                    // unregister the handle before destroying it; PawPrint has one process-wide
                    // handle registry, but keeping the type association visible makes a future
                    // collector/loader model easier to add.
                    qCallTypeHandleToConcreteTypeHandle "RuntimeTypeHandle.FreeGCHandle" qCallHandle
                    |> ignore

                    let handle =
                        gcHandleAddressOfEvalStackValue "RuntimeTypeHandle.FreeGCHandle" objHandle

                    let state =
                        { state with
                            GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                        }

                    match returnType with
                    | ConcreteVoid state.ConcreteTypes -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                    | ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ->
                        state
                        |> IlMachineState.pushToEvalStack'
                            (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L))
                            thread
                        |> Tuple.withRight WhatWeDid.Executed
                        |> ExecutionResult.Stepped
                    | other -> failwith $"RuntimeTypeHandle.FreeGCHandle: unexpected return type %O{other}"
                | "System.Private.CoreLib",
                  "System.Runtime.InteropServices",
                  "GCHandle",
                  "_InternalAlloc",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
                    ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                      "System.Runtime.InteropServices",
                                                      "GCHandleType",
                                                      gcHandleTypeGenerics) ],
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr when gcHandleTypeGenerics.IsEmpty ->
                    let target =
                        instruction.Arguments.[0]
                        |> EvalStackValue.ofCliType
                        |> objectTargetOfEvalStackValue "GCHandle._InternalAlloc"

                    let kind =
                        instruction.Arguments.[1]
                        |> EvalStackValue.ofCliType
                        |> gcHandleKindOfEvalStackValue "GCHandle._InternalAlloc"

                    let handle, gcHandles =
                        state.GcHandles
                        |> GcHandleRegistry.allocate kind GcHandleOwner.GuestAllocated target

                    let state =
                        { state with
                            GcHandles = gcHandles
                        }

                    let state = pushGcHandleAddress handle thread state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Runtime.InteropServices",
                  "GCHandle",
                  "_InternalFree",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean ->
                    let handle =
                        instruction.Arguments.[0]
                        |> EvalStackValue.ofCliType
                        |> gcHandleAddressOfEvalStackValue "GCHandle._InternalFree"

                    let state =
                        { state with
                            GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                        }

                    let state = IlMachineState.pushToEvalStack (CliType.ofBool true) thread state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Runtime.InteropServices",
                  "GCHandle",
                  "_InternalFreeWithGCTransition",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
                  ConcreteVoid state.ConcreteTypes ->
                    let handle =
                        instruction.Arguments.[0]
                        |> EvalStackValue.ofCliType
                        |> gcHandleAddressOfEvalStackValue "GCHandle._InternalFreeWithGCTransition"

                    let state =
                        { state with
                            GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                        }

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Runtime.InteropServices",
                  "GCHandle",
                  "InternalSet",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
                    ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
                  ConcreteVoid state.ConcreteTypes ->
                    let handle =
                        instruction.Arguments.[0]
                        |> EvalStackValue.ofCliType
                        |> gcHandleAddressOfEvalStackValue "GCHandle.InternalSet"

                    let target =
                        instruction.Arguments.[1]
                        |> EvalStackValue.ofCliType
                        |> objectTargetOfEvalStackValue "GCHandle.InternalSet"

                    let state =
                        { state with
                            GcHandles = state.GcHandles |> GcHandleRegistry.setTarget handle target
                        }

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Runtime.InteropServices",
                  "GCHandle",
                  "InternalCompareExchange",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
                    ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
                    ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ->
                    let handle =
                        instruction.Arguments.[0]
                        |> EvalStackValue.ofCliType
                        |> gcHandleAddressOfEvalStackValue "GCHandle.InternalCompareExchange"

                    let value =
                        instruction.Arguments.[1]
                        |> EvalStackValue.ofCliType
                        |> objectTargetOfEvalStackValue "GCHandle.InternalCompareExchange"

                    let comparand =
                        instruction.Arguments.[2]
                        |> EvalStackValue.ofCliType
                        |> objectTargetOfEvalStackValue "GCHandle.InternalCompareExchange"

                    let oldTarget, gcHandles =
                        state.GcHandles |> GcHandleRegistry.compareExchangeTarget handle value comparand

                    let state =
                        { state with
                            GcHandles = gcHandles
                        }

                    let state = pushObjectTarget oldTarget thread state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
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

                    let typeHandleTarget =
                        // RuntimeType.m_handle is typed as IntPtr (primitive-like); unwrap to reach the inner NativeInt.
                        match
                            AllocatedNonArrayObject.DereferenceField "m_handle" heapObj
                            |> CliType.unwrapPrimitiveLike
                        with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
                        | other -> failwith $"GetAssembly: expected TypeHandlePtr in m_handle field, got %O{other}"

                    let assemblyName =
                        match typeHandleTarget with
                        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> identity.Assembly
                        | RuntimeTypeHandleTarget.Closed concreteTypeHandle ->
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

                            concreteType.Assembly

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

                    let fields =
                        CliValueType.OfFields
                            baseClassTypes
                            state.ConcreteTypes
                            runtimeAssemblyTypeHandle
                            runtimeAssemblyTypeInfo.Layout
                            allFields

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
                  "System.Threading",
                  "Thread",
                  "Initialize",
                  [],
                  ConcreteVoid state.ConcreteTypes ->
                    // InternalCall backing `new Thread(...)` constructor. Sets up the managed
                    // thread ID, priority, and native handle sentinel on the Thread object.
                    let state = IlMachineState.loadArgument thread 0 state
                    let thisRef, state = IlMachineState.popEvalStack thread state

                    let threadAddr =
                        match thisRef with
                        | EvalStackValue.ObjectRef addr -> addr
                        | other -> failwith $"Thread.Initialize: expected ObjectRef for 'this', got %O{other}"

                    let managedThreadId = state.NextManagedThreadId
                    let threadPriorityNormal = 2
                    let (ManagedHeapAddress addrInt) = threadAddr

                    let updatedObj =
                        ManagedHeap.get threadAddr state.ManagedHeap
                        |> AllocatedNonArrayObject.SetField
                            "_managedThreadId"
                            (CliType.Numeric (CliNumericType.Int32 managedThreadId))
                        |> AllocatedNonArrayObject.SetField
                            "_priority"
                            (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
                        |> AllocatedNonArrayObject.SetField
                            "_DONT_USE_InternalThread"
                            (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

                    let state =
                        { state with
                            ManagedHeap = ManagedHeap.set threadAddr updatedObj state.ManagedHeap
                            NextManagedThreadId = state.NextManagedThreadId + 1
                        }

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Thread",
                  "StartInternal",
                  _,
                  ConcreteVoid state.ConcreteTypes ->
                    // StartInternal (ThreadHandle t, int stackSize, int priority, Interop.BOOL isThreadPool, char* pThreadName) -> void
                    // We don't yet model stack size / priority / thread-pool / native name; we recover the
                    // Thread heap object from the handle and spawn a new interpreter thread that begins
                    // executing the user-supplied delegate directly, bypassing the BCL StartCallback
                    // path (which otherwise pulls in ExecutionContext/culture/autorelease machinery).
                    let threadHandleArg = instruction.Arguments.[0]

                    let threadAddr =
                        match threadHandleArg |> CliType.unwrapPrimitiveLike with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim addrInt)) ->
                            ManagedHeapAddress (int addrInt)
                        | CliType.ValueType vt ->
                            match CliValueType.DereferenceField "_ptr" vt |> CliType.unwrapPrimitiveLike with
                            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim addrInt)) ->
                                ManagedHeapAddress (int addrInt)
                            | other ->
                                failwith
                                    $"Thread.StartInternal: expected Verbatim nativeint inside ThreadHandle._ptr, got %O{other}"
                        | other ->
                            failwith $"Thread.StartInternal: unexpected shape for ThreadHandle argument: %O{other}"

                    // Double-Start detection: if this Thread heap object is already bound to
                    // an interpreter thread, the guest has called Start() twice. The real
                    // runtime nulls out _startHelper on a successful Start so the second
                    // call throws ThreadStateException; we can't synthesise that exception
                    // yet, so fail the interpreter loudly instead of silently spawning a
                    // second worker. When exception synthesis lands, replace this failwith
                    // with the ThreadStateException raise and the _startHelper nulling.
                    if state.ManagedThreadObjects |> Map.exists (fun _ addr -> addr = threadAddr) then
                        failwith
                            $"Thread.StartInternal: Thread object at {threadAddr} has already been started; the guest would observe ThreadStateException, which is not yet synthesised. Double-Start is a guest bug."

                    let threadObj = ManagedHeap.get threadAddr state.ManagedHeap

                    let startHelperAddr =
                        match AllocatedNonArrayObject.DereferenceField "_startHelper" threadObj with
                        | CliType.ObjectRef (Some a) -> a
                        | other ->
                            failwith
                                $"Thread.StartInternal: expected non-null _startHelper on Thread object, got %O{other}"

                    let startHelperObj = ManagedHeap.get startHelperAddr state.ManagedHeap

                    let delegateAddr =
                        match AllocatedNonArrayObject.DereferenceField "_start" startHelperObj with
                        | CliType.ObjectRef (Some a) -> a
                        | other ->
                            failwith
                                $"Thread.StartInternal: expected non-null StartHelper._start delegate, got %O{other}"

                    let delegateObj = ManagedHeap.get delegateAddr state.ManagedHeap

                    let target =
                        match AllocatedNonArrayObject.DereferenceField "_target" delegateObj with
                        | CliType.ObjectRef addr -> addr
                        | other ->
                            failwith $"Thread.StartInternal: expected ObjectRef for delegate _target, got %O{other}"

                    let targetMethod =
                        // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                        match
                            AllocatedNonArrayObject.DereferenceField "_methodPtr" delegateObj
                            |> CliType.unwrapPrimitiveLike
                        with
                        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                        | other ->
                            failwith
                                $"Thread.StartInternal: expected FunctionPointer in delegate _methodPtr, got %O{other}"

                    let containingAssembly =
                        state.LoadedAssembly targetMethod.DeclaringType.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"Thread.StartInternal: assembly {targetMethod.DeclaringType.Assembly.Name} not loaded"
                        )

                    let thisArgs =
                        if targetMethod.IsStatic then
                            System.Collections.Immutable.ImmutableArray.Empty
                        else
                            match target with
                            | Some t ->
                                // For delegates bound to value-type instance methods, the receiver
                                // must be a managed pointer into the boxed heap object's value
                                // data, matching `callMethod`'s coercion in IlMachineStateExecution.
                                let declaringTypeDef =
                                    containingAssembly.TypeDefs.[targetMethod.DeclaringType.Definition.Get]

                                let receiver =
                                    if
                                        DumpedAssembly.isValueType
                                            baseClassTypes
                                            state._LoadedAssemblies
                                            declaringTypeDef
                                    then
                                        CliType.RuntimePointer (
                                            CliRuntimePointer.Managed (
                                                ManagedPointerSource.Byref (ByrefRoot.HeapValue t, [])
                                            )
                                        )
                                    else
                                        CliType.ObjectRef (Some t)

                                System.Collections.Immutable.ImmutableArray.Create receiver
                            | None -> failwith "Thread.StartInternal: instance-method delegate has null _target"

                    // ParameterizedThreadStart passes StartHelper._startArg as the single
                    // declared parameter; plain ThreadStart takes none. `this` is not counted
                    // in Signature.ParameterTypes.
                    let args =
                        match targetMethod.Signature.ParameterTypes.Length with
                        | 0 -> thisArgs
                        | 1 ->
                            let startArg = AllocatedNonArrayObject.DereferenceField "_startArg" startHelperObj
                            thisArgs.Add startArg
                        | other ->
                            failwith
                                $"Thread.StartInternal: target method %s{targetMethod.Name} declares %d{other} parameters; only ThreadStart/ParameterizedThreadStart are supported"

                    let newMethodState =
                        match
                            MethodState.Empty
                                state.ConcreteTypes
                                baseClassTypes
                                state._LoadedAssemblies
                                containingAssembly
                                targetMethod
                                targetMethod.Generics
                                args
                                None
                        with
                        | Ok ms -> ms
                        | Error _ ->
                            failwith "Thread.StartInternal: failed to build MethodState for thread delegate target"

                    let state, newThreadId =
                        IlMachineState.addThread newMethodState targetMethod.DeclaringType.Assembly state

                    // Link the fresh ThreadId to the pre-existing Thread heap object so that
                    // Thread.CurrentThread on the new thread returns the original Thread reference.
                    let state =
                        { state with
                            ManagedThreadObjects = state.ManagedThreadObjects |> Map.add newThreadId threadAddr
                        }

                    // ECMA-335: a type's .cctor must run before any of its static methods
                    // or before the first instance is touched. For delegates bound to a
                    // method on a not-yet-initialised type, the normal call path would
                    // trigger initialisation, but we bypass that by building the worker's
                    // initial frame directly. Route the worker through
                    // ensureTypeInitialised so all four cctor states are handled: already
                    // initialised (no-op), fresh load (cctor frame pushed on the worker,
                    // runs before the target method), another thread is mid-init (worker
                    // marked BlockedOnClassInit so the scheduler stalls it), or the cctor
                    // already failed (cached TypeInitializationException dispatched onto
                    // the worker's frames).
                    let declaringTypeHandle =
                        AllConcreteTypes.findExistingConcreteType
                            state.ConcreteTypes
                            targetMethod.DeclaringType.Identity
                            targetMethod.DeclaringType.Generics
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"Thread.StartInternal: declaring type %s{targetMethod.DeclaringType.Name} of delegate target is not registered in ConcreteTypes"
                        )

                    let state, workerInitOutcome =
                        IlMachineStateExecution.ensureTypeInitialised
                            loggerFactory
                            baseClassTypes
                            newThreadId
                            declaringTypeHandle
                            state

                    // The worker's bottom frame is the target method itself, not a
                    // `call` of the target. That matters for BlockedOnClassInit: the
                    // speculative wake in Scheduler.onStepOutcome would flip the worker
                    // back to Runnable on the blocker's next step, but unlike every
                    // other call site we can't re-run ensureTypeInitialised when the
                    // worker resumes — it would just start executing the target's
                    // first IL op before the cctor has actually finished. Fail loud
                    // for now; every other cross-thread-InProgress path in the
                    // interpreter also fails loud (see loadClass and UnaryMetadataIlOp
                    // Call/Newobj). Fixing this properly requires either a synthetic
                    // caller frame that issues the call or first-class class-init
                    // re-entry, both of which are out of scope for this change.
                    match workerInitOutcome with
                    | WhatWeDid.BlockedOnClassInit _ ->
                        failwith
                            $"Thread.StartInternal: target type %s{targetMethod.DeclaringType.Name} is being initialised on another thread. Cross-thread class-init synchronisation for workers is not yet implemented."
                    | _ -> ()

                    let state = Scheduler.onWorkerSpawned newThreadId workerInitOutcome state

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                | "System.Private.CoreLib",
                  "System.Threading",
                  "Thread",
                  "Join",
                  [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
                  ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean ->
                    // public bool Thread.Join(int millisecondsTimeout) — shipped as an InternalCall in
                    // the deployed CoreLib (the managed body we see in source exists only in the
                    // reference assembly). `this` is arg 0, the timeout is arg 1.
                    let thisArg = instruction.Arguments.[0]

                    let timeout =
                        match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                        | CliType.Numeric (CliNumericType.Int32 i) -> i
                        | other -> failwith $"Thread.Join: expected int32 timeout, got %O{other}"

                    let threadAddr =
                        match thisArg with
                        | CliType.ObjectRef (Some a) -> a
                        | other -> failwith $"Thread.Join: expected non-null Thread `this`, got %O{other}"

                    // `timeout` follows Thread.Join semantics: -1 (Timeout.Infinite) blocks
                    // until the target terminates, 0 is a non-blocking poll. Any other value
                    // is a finite wait, which PawPrint cannot honour because the scheduler
                    // doesn't model wall-clock time — a guest that relies on a Join(100) to
                    // fall through after a timeout would instead block forever here. Fail
                    // loud rather than silently diverging from guest semantics; once a
                    // virtual-clock story lands, replace this with the real finite-wait
                    // implementation. The CLR also rejects timeout < -1 with
                    // ArgumentOutOfRangeException; we can't synthesise that yet, so the
                    // same failwith covers it.
                    match timeout with
                    | -1
                    | 0 -> ()
                    | other ->
                        failwith
                            $"Thread.Join: millisecondsTimeout=%d{other} is not supported. Only -1 (Timeout.Infinite) and 0 (non-blocking poll) are implemented; finite timeouts require a virtual clock PawPrint does not yet model. Negative values other than -1 would raise ArgumentOutOfRangeException in the real CLR, which PawPrint doesn't synthesise yet."

                    let targetThreadId =
                        state.ManagedThreadObjects
                        |> Map.toSeq
                        |> Seq.tryPick (fun (tid, addr) -> if addr = threadAddr then Some tid else None)
                        |> Option.defaultWith (fun () ->
                            // Distinguish "guest called Join on a Thread it never Start()ed"
                            // (real CLR would raise ThreadStateException) from "the interpreter's
                            // ManagedThreadObjects bookkeeping is out of sync with a live thread".
                            // Presence of a heap object at `threadAddr` means the guest legitimately
                            // allocated a Thread; absence means we've been handed a wild pointer
                            // and the bug is inside PawPrint.
                            match state.ManagedHeap.NonArrayObjects |> Map.tryFind threadAddr with
                            | Some _ ->
                                failwith
                                    $"Thread.Join: Thread object at {threadAddr} was never Start()ed. The real CLR raises ThreadStateException here; PawPrint doesn't synthesise that yet, so this is a guest bug we can't currently report structurally."
                            | None ->
                                failwith
                                    $"Thread.Join: no heap object at {threadAddr} (interpreter bug: stale or invalid Thread reference handed to Join)."
                        )

                    // Self-join is an immediate deadlock: blocking ourselves on ourselves means
                    // no thread will ever wake us. The real CLR also hangs, but in PawPrint this
                    // would surface much later as a generic "no runnable threads" failure far
                    // from the actual Join call; report it at the cause site.
                    if targetThreadId = thread then
                        failwith
                            $"Thread.Join: thread {thread} is attempting to join itself, which would deadlock. The real CLR also hangs on self-join; PawPrint reports this at the call site rather than as a downstream deadlock."

                    let targetState =
                        state.ThreadState
                        |> Map.tryFind targetThreadId
                        |> Option.defaultWith (fun () ->
                            failwith $"Thread.Join: target ThreadId {targetThreadId} has no ThreadState"
                        )

                    let targetTerminated = targetState.Status = ThreadStatus.Terminated

                    match timeout with
                    | 0 ->
                        let state =
                            IlMachineState.pushToEvalStack (CliType.ofBool targetTerminated) thread state

                        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
                    | _ ->
                        // Push `true` onto the caller's eval stack before (possibly) blocking.
                        // This push persists across the block: the IP has already advanced past
                        // the Join call by the time we return Stepped, so when the scheduler
                        // eventually flips us back to Runnable the `true` is already sitting as
                        // Join's return value and control flows straight past the call site.
                        let state = IlMachineState.pushToEvalStack (CliType.ofBool true) thread state

                        let state =
                            if targetTerminated then
                                state
                            else
                                Scheduler.blockOnJoin thread targetThreadId state

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
            | ExecutionResult.ProcessExit _ -> outcome
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
