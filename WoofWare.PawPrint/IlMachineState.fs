namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type IlMachineState =
    {
        Logger : ILogger
        NextThreadId : int
        // CallStack : StackFrame list
        /// Multiple managed heaps are allowed, but we hopefully only need one.
        ManagedHeap : ManagedHeap
        ThreadState : Map<ThreadId, ThreadState>
        InternedStrings : ImmutableDictionary<StringToken, ManagedHeapAddress>
        /// Keyed by FullName. (Sometimes an assembly has a PublicKey when we read it from the disk, but we
        /// only have a reference to it by an AssemblyName without a PublicKey.)
        _LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
        /// Tracks initialization state of types across assemblies
        TypeInitTable : TypeInitTable
        /// For each type, specialised to each set of generic args, a map of string field name to static value contained therein.
        _Statics : ImmutableDictionary<ConcreteType<FakeUnit>, ImmutableDictionary<string, CliType>>
        DotnetRuntimeDirs : string ImmutableArray
        TypeHandles : TypeHandleRegistry
    }

    member this.WithTypeBeginInit (thread : ThreadId) (ty : RuntimeConcreteType) =
        this.Logger.LogDebug (
            "Beginning initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            ty.Assembly.FullName,
            this.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get].Name,
            ty.Definition.Get.GetHashCode ()
        )

        let typeInitTable = this.TypeInitTable |> TypeInitTable.beginInitialising thread ty

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithTypeEndInit (thread : ThreadId) (ty : RuntimeConcreteType) =
        this.Logger.LogDebug (
            "Marking complete initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            ty.Assembly.FullName,
            this.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get].Name,
            ty.Definition.Get.GetHashCode ()
        )

        let typeInitTable = this.TypeInitTable |> TypeInitTable.markInitialised thread ty

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithLoadedAssembly (name : AssemblyName) (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.Add (name.FullName, value)
        }

    member this.LoadedAssembly' (fullName : string) : DumpedAssembly option =
        match this._LoadedAssemblies.TryGetValue fullName with
        | false, _ -> None
        | true, v -> Some v

    member this.LoadedAssembly (name : AssemblyName) : DumpedAssembly option = this.LoadedAssembly' name.FullName

    /// Returns also the original assembly name.
    member this.WithThreadSwitchedToAssembly (assy : AssemblyName) (thread : ThreadId) : IlMachineState * AssemblyName =
        let mutable existing = Unchecked.defaultof<AssemblyName>

        let newState =
            this.ThreadState
            |> Map.change
                thread
                (fun s ->
                    match s with
                    | None -> failwith $"expected thread {thread} to be in a state already; internal logic error"
                    | Some s ->
                        existing <- s.ActiveAssembly

                        { s with
                            ActiveAssembly = assy
                        }
                        |> Some
                )

        { this with
            ThreadState = newState
        },
        existing

    member this.ActiveAssembly (thread : ThreadId) =
        let active = this.ThreadState.[thread].ActiveAssembly

        match this.LoadedAssembly active with
        | Some v -> v
        | None ->
            let available = this._LoadedAssemblies.Keys |> String.concat " ; "

            failwith
                $"Somehow we believe the active assembly is {active}, but only had the following available: {available}"

(*
Type load algorithm, from II.10.5.3.3
1. At class load-time (hence prior to initialization time) store zero or null into all static fields of the
type.
2. If the type is initialized, you are done.
2.1. If the type is not yet initialized, try to take an initialization lock.
2.2. If successful, record this thread as responsible for initializing the type and proceed to step 2.3.
2.2.1. If not successful, see whether this thread or any thread waiting for this thread to complete already
holds the lock.
2.2.2. If so, return since blocking would create a deadlock. This thread will now see an incompletely
initialized state for the type, but no deadlock will arise.
2.2.3 If not, block until the type is initialized then return.
2.3 Initialize the base class type and then all interfaces implemented by this type.
2.4 Execute the type initialization code for this type.
2.5 Mark the type as initialized, release the initialization lock, awaken any threads waiting for this type
to be initialized, and return.
*)
type WhatWeDid =
    | Executed
    /// We didn't run what you wanted, because we have to do class initialisation first.
    | SuspendedForClassInit
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId

type ExecutionResult =
    | Terminated of IlMachineState * terminatingThread : ThreadId
    | Stepped of IlMachineState * WhatWeDid

type StateLoadResult =
    /// The type is loaded; you can proceed.
    | NothingToDo of IlMachineState
    /// We didn't manage to load the requested type, because that type itself requires first loading something.
    /// The state we give you is ready to load that something.
    | FirstLoadThis of IlMachineState

[<RequireQualifiedAccess>]
module IlMachineState =
    type private Dummy = class end

    let loadAssembly
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * AssemblyName
        =
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]
        let assemblyName = assemblyRef.Name

        match state.LoadedAssembly assemblyName with
        | Some v -> state, v, assemblyName
        | None ->
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            let assy =
                state.DotnetRuntimeDirs
                |> Seq.choose (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        use f = File.OpenRead file
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.read loggerFactory (Some file) f |> Some
                    with :? FileNotFoundException ->
                        None
                )
                |> Seq.toList

            match assy |> List.tryHead with
            | None -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | Some assy ->

            state.WithLoadedAssembly assemblyName assy, assy, assemblyName

    let rec internal resolveTypeFromName
        (loggerFactory : ILoggerFactory)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn> option)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match Assembly.resolveTypeFromName assy state._LoadedAssemblies ns name genericArgs with
        | TypeResolutionResult.Resolved (assy, typeDef) -> state, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let state, _, _ =
                loadAssembly
                    loggerFactory
                    state._LoadedAssemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    state

            resolveTypeFromName loggerFactory ns name genericArgs assy state

    and resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn> option)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match Assembly.resolveTypeFromExport fromAssembly state._LoadedAssemblies ty genericArgs with
        | TypeResolutionResult.Resolved (assy, typeDef) -> state, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let state, targetAssy, _ =
                loadAssembly
                    loggerFactory
                    state._LoadedAssemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    state

            resolveTypeFromName loggerFactory ty.Namespace ty.Name genericArgs targetAssy state

    and resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn> option)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match Assembly.resolveTypeRef state._LoadedAssemblies referencedInAssembly target typeGenericArgs with
        | TypeResolutionResult.Resolved (assy, typeDef) -> state, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let state, _, _ =
                loadAssembly
                    loggerFactory
                    state._LoadedAssemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    state

            resolveTypeFromRef loggerFactory referencedInAssembly target typeGenericArgs state

    and resolveType
        (loggerFactory : ILoggerFactory)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn> option)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        let target = assy.TypeRefs.[ty]

        resolveTypeFromRef loggerFactory assy target genericArgs state

    let rec resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn> option)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let args' = ImmutableArray.CreateBuilder ()

            let state =
                (state, args)
                ||> Seq.fold (fun state arg ->
                    let state, assy, arg =
                        resolveTypeFromDefn loggerFactory corelib arg typeGenericArgs methodGenericArgs assy state

                    let baseType =
                        arg.BaseType
                        |> DumpedAssembly.resolveBaseType corelib state._LoadedAssemblies assy.Name

                    let signatureTypeKind =
                        match baseType with
                        | ResolvedBaseType.Enum
                        | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                        | ResolvedBaseType.Object -> SignatureTypeKind.Class
                        | ResolvedBaseType.Delegate -> failwith "TODO: delegate"

                    args'.Add (
                        TypeDefn.FromDefinition (
                            ComparableTypeDefinitionHandle.Make arg.TypeDefHandle,
                            assy.Name.FullName,
                            signatureTypeKind
                        )
                    )

                    state
                )

            let args' = args'.ToImmutable ()
            resolveTypeFromDefn loggerFactory corelib generic (Some args') methodGenericArgs assy state
        | TypeDefn.FromDefinition (defn, assy, _typeKind) ->
            let assy = state._LoadedAssemblies.[assy]

            let defn =
                assy.TypeDefs.[defn.Get]
                |> TypeInfo.mapGeneric (fun _ param ->
                    match typeGenericArgs with
                    | None -> failwith "somehow got a generic TypeDefn.FromDefinition without any type generic args"
                    | Some genericArgs -> genericArgs.[param.SequenceNumber]
                )

            state, assy, defn
        | TypeDefn.FromReference (ref, _typeKind) ->
            let state, assy, ty =
                resolveTypeFromRef loggerFactory assy ref typeGenericArgs state

            state, assy, ty
        | TypeDefn.PrimitiveType prim ->
            let ty =
                match prim with
                | PrimitiveType.Boolean -> corelib.Boolean
                | PrimitiveType.Char -> corelib.Char
                | PrimitiveType.SByte -> corelib.SByte
                | PrimitiveType.Byte -> corelib.Byte
                | PrimitiveType.Int16 -> corelib.Int16
                | PrimitiveType.UInt16 -> corelib.UInt16
                | PrimitiveType.Int32 -> corelib.Int32
                | PrimitiveType.UInt32 -> corelib.UInt32
                | PrimitiveType.Int64 -> corelib.Int64
                | PrimitiveType.UInt64 -> corelib.UInt64
                | PrimitiveType.Single -> corelib.Single
                | PrimitiveType.Double -> corelib.Double
                | PrimitiveType.String -> corelib.String
                | PrimitiveType.TypedReference -> failwith "todo"
                | PrimitiveType.IntPtr -> failwith "todo"
                | PrimitiveType.UIntPtr -> failwith "todo"
                | PrimitiveType.Object -> failwith "todo"
                |> TypeInfo.mapGeneric (fun _ -> failwith "none of these types are generic")

            state, corelib.Corelib, ty
        | TypeDefn.GenericTypeParameter param ->
            match typeGenericArgs with
            | None -> failwith "tried to resolve generic parameter without generic args"
            | Some genericArgs ->
                let arg = genericArgs.[param]
                // TODO: this assembly is probably wrong?
                resolveTypeFromDefn loggerFactory corelib arg (Some genericArgs) methodGenericArgs assy state
        | TypeDefn.GenericMethodParameter param ->
            let arg = methodGenericArgs.[param]
            // TODO: this assembly is probably wrong?
            resolveTypeFromDefn loggerFactory corelib arg typeGenericArgs methodGenericArgs assy state
        | s -> failwith $"TODO: resolveTypeFromDefn unimplemented for {s}"

    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : TypeDefn ImmutableArray option)
        (methodGenericArgs : TypeDefn ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn>
        =
        let sign = assy.TypeSpecs.[ty].Signature
        resolveTypeFromDefn loggerFactory corelib sign typeGenericArgs methodGenericArgs assy state

    let rec cliTypeZeroOf
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (ty : TypeDefn)
        (typeGenerics : TypeDefn ImmutableArray option)
        (methodGenerics : TypeDefn ImmutableArray option)
        (state : IlMachineState)
        : IlMachineState * CliType
        =
        match CliType.zeroOf state._LoadedAssemblies corelib assy typeGenerics methodGenerics ty with
        | CliTypeResolutionResult.Resolved result -> state, result
        | CliTypeResolutionResult.FirstLoad ref ->
            let state, _, _ =
                loadAssembly loggerFactory state._LoadedAssemblies.[snd(ref.Handle).FullName] (fst ref.Handle) state

            cliTypeZeroOf loggerFactory corelib assy ty typeGenerics methodGenerics state

    let pushToEvalStack' (o : EvalStackValue) (thread : ThreadId) (state : IlMachineState) =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack' o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let pushToEvalStack (o : CliType) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let peekEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue option =
        ThreadState.peekEvalStack state.ThreadState.[thread]

    let popEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue * IlMachineState =
        let ret, popped = ThreadState.popFromEvalStack state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread popped
            }

        ret, state

    let advanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.advanceProgramCounter |> Some
                    )
        }

    let setArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.SetArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let getArrayValue (arrayAllocation : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.GetArrayValue arrayAllocation index state.ManagedHeap

    /// There might be no stack frame to return to, so you might get None.
    let returnStackFrame
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState option
        =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]

        match threadStateAtEndOfMethod.MethodState.ReturnState with
        | None -> None
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add
                        currentThread
                        { threadStateAtEndOfMethod with
                            ActiveMethodState = returnState.JumpTo
                            ActiveAssembly =
                                threadStateAtEndOfMethod.MethodStates.[returnState.JumpTo].ExecutingMethod.DeclaringType
                                    .Assembly
                        }
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            // Assumption: a constructor can't also return a value.
            // If we were constructing a reference type, we push a reference to it.
            // Otherwise, extract the now-complete object from the heap and push it to the stack directly.
            let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

            let resolvedBaseType =
                DumpedAssembly.resolveBaseType
                    corelib
                    state._LoadedAssemblies
                    constructed.Type.Assembly
                    constructed.Type.BaseType

            match resolvedBaseType with
            | ResolvedBaseType.Delegate
            | ResolvedBaseType.Object -> state |> pushToEvalStack (CliType.OfManagedObject constructing) currentThread
            | ResolvedBaseType.ValueType ->
                state
                |> pushToEvalStack (CliType.ValueType (Seq.toList constructed.Fields.Values)) currentThread
            | ResolvedBaseType.Enum -> failwith "TODO"
        | None ->
            match threadStateAtEndOfMethod.MethodState.EvaluationStack.Values with
            | [] ->
                // no return value
                state
            | [ retVal ] ->
                let retType =
                    threadStateAtEndOfMethod.MethodState.ExecutingMethod.Signature.ReturnType

                match retType with
                | TypeDefn.Void -> state
                | retType ->
                    // TODO: generics
                    let state, zero =
                        cliTypeZeroOf loggerFactory corelib (state.ActiveAssembly currentThread) retType None None state

                    let toPush = EvalStackValue.toCliTypeCoerced zero retVal

                    state |> pushToEvalStack toPush currentThread
            | _ ->
                failwith
                    "Unexpected interpretation result has a local evaluation stack with more than one element on RET"

        |> Some

    let private safeIntrinsics =
        [
            // The IL implementation is fine: https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L677
            "System.Private.CoreLib", "Unsafe", "AsRef"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L739-L750
            "System.Private.CoreLib", "String", "get_Length"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/ArgumentNullException.cs#L54
            "System.Private.CoreLib", "ArgumentNullException", "ThrowIfNull"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/System.Private.CoreLib/src/System/Type.CoreCLR.cs#L82
            "System.Private.CoreLib", "Type", "GetTypeFromHandle"
        ]
        |> Set.ofList

    let callIntrinsic
        (baseClassTypes : BaseClassTypes<_>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, WoofWare.PawPrint.GenericParameter>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState option
        =
        let callerAssy =
            state.ThreadState.[currentThread].MethodState.ExecutingMethod.DeclaringType.Assembly

        if
            methodToCall.DeclaringType.Assembly.Name = "System.Private.CoreLib"
            && methodToCall.DeclaringType.Name = "Volatile"
        then
            // These are all safely implemented in IL, just inefficient.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Volatile.cs#L13
            None
        elif
            Set.contains
                (methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name)
                safeIntrinsics
        then
            None
        else

        match methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name with
        | "System.Private.CoreLib", "Type", "get_TypeHandle" ->
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L470
            // no args, returns RuntimeTypeHandle, a struct with a single field (a RuntimeType class)

            // The thing on top of the stack will be a RuntimeType.
            let arg, state = popEvalStack currentThread state

            let arg =
                let rec go (arg : EvalStackValue) =
                    match arg with
                    | EvalStackValue.UserDefinedValueType [ s ] -> go s
                    | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                    | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr) -> Some addr
                    | s -> failwith $"TODO: called with unrecognised arg %O{s}"

                go arg

            let state =
                pushToEvalStack (CliType.ValueType [ CliType.ObjectRef arg ]) currentThread state
                |> advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "Unsafe", "AsPointer" ->
            // Method signature: 1 generic parameter, we take a Byref of that parameter, and return a TypeDefn.Pointer(Void)
            let arg, state = popEvalStack currentThread state

            let toPush =
                match arg with
                | EvalStackValue.ManagedPointer ptr ->
                    match ptr with
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        CliRuntimePointer.Managed (
                            CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, whichVar)
                        )
                    | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
                        CliRuntimePointer.Managed (
                            CliRuntimePointerSource.Argument (sourceThread, methodFrame, whichVar)
                        )
                    | ManagedPointerSource.Heap managedHeapAddress ->
                        CliRuntimePointer.Managed (CliRuntimePointerSource.Heap managedHeapAddress)
                    | ManagedPointerSource.Null -> failwith "todo"
                    | ManagedPointerSource.ArrayIndex _ -> failwith "TODO"
                | x -> failwith $"TODO: Unsafe.AsPointer(%O{x})"

            pushToEvalStack (CliType.RuntimePointer toPush) currentThread state
            |> advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "SingleToInt32Bits" ->
            let arg, state = popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f -> BitConverter.SingleToInt32Bits (float32<float> f) |> EvalStackValue.Int32
                | _ -> failwith "TODO"

            state
            |> pushToEvalStack' result currentThread
            |> advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int32BitsToSingle" ->
            let arg, state = popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int32BitsToSingle arg |> CliNumericType.Float32 |> CliType.Numeric

            state
            |> pushToEvalStack result currentThread
            |> advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int64BitsToDouble" ->
            let arg, state = popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> pushToEvalStack result currentThread
            |> advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "DoubleToInt64Bits" ->
            let arg, state = popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f -> BitConverter.DoubleToInt64Bits f |> EvalStackValue.Int64
                | _ -> failwith "TODO"

            state
            |> pushToEvalStack' result currentThread
            |> advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "String", "Equals" ->
            let arg1, state = popEvalStack currentThread state

            let arg1 =
                match arg1 with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap h) -> h
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg1}"
                | _ -> failwith $"TODO: %O{arg1}"

            let arg2, state = popEvalStack currentThread state

            let arg2 =
                match arg2 with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap h) -> h
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg2}"
                | _ -> failwith $"TODO: %O{arg2}"

            if arg1 = arg2 then
                state
                |> pushToEvalStack (CliType.OfBool true) currentThread
                |> advanceProgramCounter currentThread
                |> Some
            else
                failwith "TODO"
        | "System.Private.CoreLib", "Unsafe", "ReadUnaligned" ->
            let ptr, state = popEvalStack currentThread state

            let v : CliType =
                let rec go ptr =
                    match ptr with
                    | EvalStackValue.ManagedPointer src ->
                        match src with
                        | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) -> failwith "todo"
                        | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) -> failwith "todo"
                        | ManagedPointerSource.Heap managedHeapAddress -> failwith "todo"
                        | ManagedPointerSource.ArrayIndex (arr, index) -> state |> getArrayValue arr index
                        | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                    | EvalStackValue.NativeInt src -> failwith "TODO"
                    | EvalStackValue.ObjectRef ptr -> failwith "TODO"
                    | EvalStackValue.UserDefinedValueType [ field ] -> go field
                    | EvalStackValue.UserDefinedValueType []
                    | EvalStackValue.UserDefinedValueType (_ :: _ :: _)
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a pointer! {ptr}"

                go ptr

            let state =
                state |> pushToEvalStack v currentThread |> advanceProgramCounter currentThread

            Some state
        | a, b, c -> failwith $"TODO: implement JIT intrinsic {a}.{b}.{c}"
        |> Option.map (fun s -> s.WithThreadSwitchedToAssembly callerAssy currentThread |> fst)

    let callMethod
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (wasInitialising : RuntimeConcreteType option)
        (wasConstructing : ManagedHeapAddress option)
        (wasClassConstructor : bool)
        (advanceProgramCounterOfCaller : bool)
        (methodGenerics : ImmutableArray<TypeDefn> option)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, WoofWare.PawPrint.GenericParameter>)
        (thread : ThreadId)
        (threadState : ThreadState)
        (state : IlMachineState)
        : IlMachineState
        =
        let activeAssy = state.ActiveAssembly thread

        // Check for intrinsics first
        let isIntrinsic =
            methodToCall.IsJITIntrinsic
                (fun handle ->
                    match activeAssy.Members.[handle].Parent with
                    | MetadataToken.TypeReference r -> activeAssy.TypeRefs.[r]
                    | x -> failwith $"{x}"
                )
                activeAssy.Methods

        match
            if isIntrinsic then
                callIntrinsic corelib methodToCall thread state
            else
                None
        with
        | Some result -> result
        | None ->

        let typeGenerics =
            match methodToCall.DeclaringType.Generics with
            | [] -> None
            | x -> Some (ImmutableArray.CreateRange x)

        // Get zero values for all parameters
        let state, argZeroObjects =
            ((state, []), methodToCall.Signature.ParameterTypes)
            ||> List.fold (fun (state, zeros) ty ->
                let state, zero =
                    cliTypeZeroOf
                        loggerFactory
                        corelib
                        (state.ActiveAssembly thread)
                        ty
                        typeGenerics
                        methodGenerics
                        state

                state, zero :: zeros
            )

        let argZeroObjects = List.rev argZeroObjects

        let activeMethodState = threadState.MethodStates.[threadState.ActiveMethodState]

        let methodToCall =
            methodToCall
            |> MethodInfo.mapMethodGenerics (fun _ param -> methodGenerics.Value.[param.SequenceNumber])

        // Helper to pop and coerce a single argument
        let popAndCoerceArg zeroType methodState =
            let value, newState = MethodState.popFromStack methodState
            EvalStackValue.toCliTypeCoerced zeroType value, newState

        // Collect arguments based on calling convention
        let args, afterPop =
            if methodToCall.IsStatic then
                // Static method: pop args in reverse order
                let args = ImmutableArray.CreateBuilder methodToCall.Parameters.Length
                let mutable currentState = activeMethodState

                for i = methodToCall.Parameters.Length - 1 downto 0 do
                    let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                    args.Add arg
                    currentState <- newState

                args.Reverse ()
                args.ToImmutable (), currentState
            else
                // Instance method: handle `this` pointer
                let argCount = methodToCall.Parameters.Length
                let args = ImmutableArray.CreateBuilder (argCount + 1)
                let mutable currentState = activeMethodState

                match wasConstructing with
                | Some _ ->
                    // Constructor: `this` is on top of stack, by our own odd little calling convention
                    // where Newobj puts the object pointer on top
                    let thisArg, newState =
                        popAndCoerceArg
                            (CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null))
                            currentState

                    args.Add thisArg
                    currentState <- newState

                    // Pop remaining args in reverse
                    for i = argCount - 1 downto 0 do
                        let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                        args.Add (arg)
                        currentState <- newState

                    args.ToImmutable (), currentState
                | None ->
                    // Regular instance method: args then `this`
                    for i = argCount - 1 downto 0 do
                        let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                        args.Add (arg)
                        currentState <- newState

                    let thisArg, newState =
                        popAndCoerceArg
                            (CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null))
                            currentState

                    args.Add thisArg
                    currentState <- newState

                    args.Reverse ()
                    args.ToImmutable (), currentState

        // Helper to create new frame with assembly loading
        let rec createNewFrame state =
            let returnInfo =
                Some
                    {
                        JumpTo = threadState.ActiveMethodState
                        WasInitialisingType = wasInitialising
                        WasConstructingObj = wasConstructing
                    }

            match
                MethodState.Empty
                    corelib
                    state._LoadedAssemblies
                    (state.ActiveAssembly thread)
                    methodToCall
                    methodGenerics
                    args
                    returnInfo
            with
            | Ok frame -> state, frame
            | Error toLoad ->
                let state' =
                    (state, toLoad)
                    ||> List.fold (fun s (asmRef : WoofWare.PawPrint.AssemblyReference) ->
                        let s, _, _ =
                            loadAssembly
                                loggerFactory
                                (state.LoadedAssembly methodToCall.DeclaringType.Assembly |> Option.get)
                                (fst asmRef.Handle)
                                s

                        s
                    )

                createNewFrame state'

        let state, newFrame = createNewFrame state

        let oldFrame =
            if wasClassConstructor || not advanceProgramCounterOfCaller then
                afterPop
            else
                afterPop |> MethodState.advanceProgramCounter

        let newThreadState =
            { threadState with
                MethodStates = threadState.MethodStates.Add(newFrame).SetItem (threadState.ActiveMethodState, oldFrame)
                ActiveMethodState = threadState.MethodStates.Length
            }

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let rec loadClass
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (ty : RuntimeConcreteType)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : StateLoadResult
        =
        let logger = loggerFactory.CreateLogger "LoadClass"

        match TypeInitTable.tryGet ty state.TypeInitTable with
        | Some TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress tid) when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress _) ->
            // This is usually signalled by WhatWeDid.Blocked
            failwith
                "TODO: cross-thread class init synchronization unimplemented - this thread has to wait for the other thread to finish initialisation"
        | None ->
            // We have work to do!

            let state, origAssyName =
                state.WithThreadSwitchedToAssembly ty.Assembly currentThread

            let sourceAssembly = state.LoadedAssembly ty.Assembly |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue ty.Definition.Get with
                | false, _ -> failwith $"Failed to find type definition {ty.Definition.Get} in {ty.Assembly.FullName}"
                | true, v -> v

            logger.LogDebug ("Resolving type {TypeDefNamespace}.{TypeDefName}", typeDef.Namespace, typeDef.Name)

            // First mark as in-progress to detect cycles
            let state = state.WithTypeBeginInit currentThread ty

            // Check if the type has a base type that needs initialization
            let firstDoBaseClass =
                match typeDef.BaseType with
                | Some baseTypeInfo ->
                    // Determine if base type is in the same or different assembly
                    match baseTypeInfo with
                    | BaseTypeInfo.ForeignAssemblyType _ -> failwith "TODO"
                    //logger.LogDebug (
                    //    "Resolved base type of {TypeDefNamespace}.{TypeDefName} to foreign assembly {ForeignAssemblyName}",
                    //    typeDef.Namespace,
                    //    typeDef.Name,
                    //    baseAssemblyName.Name
                    //)

                    //match loadClass loggerFactory baseTypeHandle baseAssemblyName currentThread state with
                    //| FirstLoadThis state -> Error state
                    //| NothingToDo state -> Ok state
                    | BaseTypeInfo.TypeDef typeDefinitionHandle ->
                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to this assembly, typedef",
                            typeDef.Namespace,
                            typeDef.Name
                        )

                        // TypeDef won't have any generics; it would be a TypeSpec if it did
                        let ty = ConcreteType.make ty.Assembly ty.Namespace ty.Name typeDefinitionHandle []

                        match loadClass loggerFactory corelib ty currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | BaseTypeInfo.TypeRef typeReferenceHandle ->
                        let state, assy, targetType =
                            // TypeRef won't have any generics; it would be a TypeSpec if it did
                            resolveType
                                loggerFactory
                                typeReferenceHandle
                                None
                                (state.ActiveAssembly currentThread)
                                state

                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to a typeref in assembly {ResolvedAssemblyName}, {BaseTypeNamespace}.{BaseTypeName}",
                            typeDef.Namespace,
                            typeDef.Name,
                            assy.Name.Name,
                            targetType.Namespace,
                            targetType.Name
                        )

                        let ty =
                            ConcreteType.make assy.Name targetType.Namespace targetType.Name targetType.TypeDefHandle []

                        match loadClass loggerFactory corelib ty currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | BaseTypeInfo.TypeSpec typeSpecificationHandle ->
                        failwith "TODO: TypeSpec base type loading unimplemented"
                | None -> Ok state // No base type (or it's System.Object)

            match firstDoBaseClass with
            | Error state -> FirstLoadThis state
            | Ok state ->

            // TODO: also need to initialise all interfaces implemented by the type

            // Find the class constructor (.cctor) if it exists
            let cctor =
                typeDef.Methods
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && method.Parameters.IsEmpty)

            match cctor with
            | Some cctorMethod ->
                // Call the class constructor! Note that we *don't* use `callMethodInActiveAssembly`, because that
                // performs class loading, but we're already in the middle of loading this class.
                // TODO: factor out the common bit.
                let currentThreadState = state.ThreadState.[currentThread]

                let cctorMethod =
                    cctorMethod
                    |> MethodInfo.mapTypeGenerics (fun i _ -> ty.Generics.[i])
                    |> MethodInfo.mapMethodGenerics (fun _ -> failwith "cctor cannot be generic")

                callMethod
                    loggerFactory
                    corelib
                    (Some ty)
                    None
                    true
                    true
                    // constructor is surely not generic
                    None
                    cctorMethod
                    currentThread
                    currentThreadState
                    state
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state = state.WithTypeEndInit currentThread ty

                // Restore original assembly context if needed
                state.WithThreadSwitchedToAssembly origAssyName currentThread
                |> fst
                |> NothingToDo

    let ensureTypeInitialised
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (ty : RuntimeConcreteType)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        match TypeInitTable.tryGet ty state.TypeInitTable with
        | None ->
            match loadClass loggerFactory corelib ty thread state with
            | NothingToDo state -> state, WhatWeDid.Executed
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | Some TypeInitState.Initialized -> state, WhatWeDid.Executed
        | Some (InProgress threadId) ->
            if threadId = thread then
                // II.10.5.3.2: avoid the deadlock by simply proceeding.
                state, WhatWeDid.Executed
            else
                state, WhatWeDid.BlockedOnClassInit threadId

    /// It may be useful to *not* advance the program counter of the caller, e.g. if you're using `callMethodInActiveAssembly`
    /// as a convenient way to move to a different method body rather than to genuinely perform a call.
    /// (Delegates do this, for example: we get a call to invoke the delegate, and then we implement the delegate as
    /// another call to its function pointer.)
    let callMethodInActiveAssembly
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (advanceProgramCounterOfCaller : bool)
        (methodGenerics : TypeDefn ImmutableArray option)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, WoofWare.PawPrint.GenericParameter>)
        (weAreConstructingObj : ManagedHeapAddress option)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let threadState = state.ThreadState.[thread]

        let state, typeInit =
            ensureTypeInitialised loggerFactory corelib thread methodToCall.DeclaringType state

        match typeInit with
        | WhatWeDid.Executed ->
            callMethod
                loggerFactory
                corelib
                None
                weAreConstructingObj
                false
                advanceProgramCounterOfCaller
                methodGenerics
                methodToCall
                thread
                threadState
                state,
            WhatWeDid.Executed
        | _ -> state, typeInit

    let initial
        (lf : ILoggerFactory)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (entryAssembly : DumpedAssembly)
        : IlMachineState
        =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name
        let logger = lf.CreateLogger "IlMachineState"

        let state =
            {
                Logger = logger
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.Empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                _Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
                TypeHandles = TypeHandleRegistry.empty ()
            }

        state.WithLoadedAssembly assyName entryAssembly

    let addThread
        (newThreadState : MethodState)
        (newThreadAssy : AssemblyName)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState =
                    state.ThreadState
                    |> Map.add thread (ThreadState.New newThreadAssy newThreadState)
            }

        newState, thread

    let allocateArray
        (zeroOfType : unit -> CliType)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                Length = len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.AllocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.AllocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.SetStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject<'generic>
        (typeInfo : WoofWare.PawPrint.TypeInfo<'generic>)
        (fields : (string * CliType) list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Fields = Map.ofList fields
                Type = TypeInfoCrate.make typeInfo
                SyncBlock = SyncBlock.Free
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.AllocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let popFromStackToLocalVariable
        (thread : ThreadId)
        (localVariableIndex : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToVariable
                localVariableIndex
                threadState.MethodStates.[threadState.ActiveMethodState]

        { state with
            ThreadState =
                state.ThreadState
                |> Map.add
                    thread
                    { threadState with
                        MethodStates = threadState.MethodStates.SetItem (threadState.ActiveMethodState, methodState)
                    }
        }

    let jumpProgramCounter (thread : ThreadId) (bytes : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.jumpProgramCounter bytes |> Some
                    )
        }

    let loadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some state -> state |> ThreadState.loadArgument index |> Some
                    )
        }

    let resolveMember
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, WoofWare.PawPrint.GenericParameter>,
              WoofWare.PawPrint.FieldInfo<TypeDefn>
           >
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]

        let memberName : string = assy.Strings mem.Name

        let state, assy, targetType =
            match mem.Parent with
            | MetadataToken.TypeReference parent -> resolveType loggerFactory parent None assy state
            | MetadataToken.TypeSpecification parent ->
                let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod

                let typeGenerics =
                    match executing.DeclaringType.Generics with
                    | [] -> None
                    | l -> Some (ImmutableArray.CreateRange l)

                let methodGenerics = executing.Generics

                resolveTypeFromSpec loggerFactory corelib parent assy typeGenerics methodGenerics state
            | parent -> failwith $"Unexpected: {parent}"

        match mem.Signature with
        | MemberSignature.Field fieldSig ->
            let availableFields =
                targetType.Fields
                |> List.filter (fun fi -> fi.Name = memberName)
                |> List.filter (fun fi -> fi.Signature = fieldSig)

            let field =
                match availableFields with
                | [] ->
                    failwith
                        $"Could not find field member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] -> x |> FieldInfo.mapTypeGenerics (fun index _ -> targetType.Generics.[index])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for {targetType.Namespace}.{targetType.Name}'s field {memberName}!"

            state, assy.Name, Choice2Of2 field

        | MemberSignature.Method memberSig ->
            let availableMethods =
                targetType.Methods
                |> List.filter (fun mi -> mi.Name = memberName)
                // TODO: this needs to resolve the TypeMethodSignature to e.g. remove references to generic parameters
                |> List.filter (fun mi -> mi.Signature = memberSig)

            let method =
                match availableMethods with
                | [] ->
                    failwith
                        $"Could not find member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] -> x |> MethodInfo.mapTypeGenerics (fun i _ -> targetType.Generics.[i])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

            state, assy.Name, Choice1Of2 method

    let setLocalVariable
        (thread : ThreadId)
        (stackFrame : int)
        (varIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set variable in nonactive thread"
                        | Some existing -> existing |> ThreadState.setLocalVariable stackFrame varIndex value |> Some
                    )
        }

    let setSyncBlock
        (addr : ManagedHeapAddress)
        (syncBlockValue : SyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = state.ManagedHeap |> ManagedHeap.SetSyncBlock addr syncBlockValue
        }

    let getSyncBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        state.ManagedHeap |> ManagedHeap.GetSyncBlock addr

    let executeDelegateConstructor (instruction : MethodState) (state : IlMachineState) : IlMachineState =
        // We've been called with arguments already popped from the stack into local arguments.
        let constructing = instruction.Arguments.[0]
        let methodPtr = instruction.Arguments.[1]
        let targetObj = instruction.Arguments.[2]

        let targetObj =
            match targetObj with
            | CliType.ObjectRef target -> target
            | _ -> failwith $"Unexpected target type for delegate: {targetObj}"

        let constructing =
            match constructing with
            | CliType.ObjectRef None -> failwith "unexpectedly constructing the null delegate"
            | CliType.ObjectRef (Some target) -> target
            | _ -> failwith $"Unexpectedly not constructing a managed object: {constructing}"

        let heapObj =
            match state.ManagedHeap.NonArrayObjects.TryGetValue constructing with
            | true, obj -> obj
            | false, _ -> failwith $"Delegate object {constructing} not found on heap"

        // Standard delegate fields in .NET are _target and _methodPtr
        // Update the fields with the target object and method pointer
        let updatedFields =
            heapObj.Fields
            |> Map.add "_target" (CliType.ObjectRef targetObj)
            |> Map.add "_methodPtr" methodPtr

        let updatedObj =
            { heapObj with
                Fields = updatedFields
            }

        let updatedHeap =
            { state.ManagedHeap with
                NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add constructing updatedObj
            }

        { state with
            ManagedHeap = updatedHeap
        }

    /// Returns the type handle and an allocated System.RuntimeType.
    let getOrAllocateType<'corelib>
        (baseClassTypes : BaseClassTypes<'corelib>)
        (defn : CanonicalTypeIdentity)
        (state : IlMachineState)
        : (int64<typeHandle> * ManagedHeapAddress) * IlMachineState
        =
        let result, reg, state =
            TypeHandleRegistry.getOrAllocate
                state
                (fun fields state -> allocateManagedObject baseClassTypes.RuntimeType fields state)
                defn
                state.TypeHandles

        let state =
            { state with
                TypeHandles = reg
            }

        result, state

    let setStatic
        (ty : RuntimeConcreteType)
        (field : string)
        (value : CliType)
        (this : IlMachineState)
        : IlMachineState
        =
        // Static variables are shared among all instantiations of a generic type.
        let ty = ty |> ConcreteType.mapGeneric (fun _ _ -> FakeUnit.ofUnit ())

        let statics =
            match this._Statics.TryGetValue ty with
            | false, _ -> this._Statics.Add (ty, ImmutableDictionary.Create().Add (field, value))
            | true, v -> this._Statics.SetItem (ty, v.SetItem (field, value))

        { this with
            _Statics = statics
        }

    let getStatic (ty : RuntimeConcreteType) (field : string) (this : IlMachineState) : CliType option =
        // Static variables are shared among all instantiations of a generic type.
        let ty = ty |> ConcreteType.mapGeneric (fun _ _ -> FakeUnit.ofUnit ())

        match this._Statics.TryGetValue ty with
        | false, _ -> None
        | true, v ->
            match v.TryGetValue field with
            | false, _ -> None
            | true, v -> Some v

    let rec canonicaliseTypeReference
        (assy : AssemblyName)
        (ty : TypeReferenceHandle)
        (state : IlMachineState)
        : Result<CanonicalTypeIdentity, AssemblyName>
        =
        match state.LoadedAssembly assy with
        | None -> Error assy
        | Some assy ->

        match assy.TypeRefs.TryGetValue ty with
        | false, _ -> failwith $"could not find type reference in assembly %s{assy.Name.FullName}"
        | true, v ->

        match v.ResolutionScope with
        | TypeRefResolutionScope.Assembly newAssy ->
            let newAssy = assy.AssemblyReferences.[newAssy].Name

            match state.LoadedAssembly newAssy with
            | None -> Error newAssy
            | Some newAssy ->
                {
                    AssemblyFullName = newAssy.Name.FullName
                    FullyQualifiedTypeName = $"%s{v.Namespace}.%s{v.Name}"
                    // TODO: I think TypeRef can't have generics?
                    Generics = []
                }
                |> Ok
        | TypeRefResolutionScope.ModuleRef _ -> failwith "todo"
        | TypeRefResolutionScope.TypeRef r ->
            if (r.GetHashCode ()) <> (ty.GetHashCode ()) then
                failwith "apparently this doesn't do what I thought"

            {

                AssemblyFullName = assy.Name.FullName
                FullyQualifiedTypeName = $"%s{v.Namespace}.%s{v.Name}"
                Generics = []
            }
            |> Ok

    let canonicaliseTypeDef
        (assy : AssemblyName)
        (ty : TypeDefinitionHandle)
        (typeGenerics : CanonicalTypeIdentity list)
        (methodGenerics : CanonicalTypeIdentity list)
        (state : IlMachineState)
        : Result<CanonicalTypeIdentity, AssemblyName>
        =
        match state.LoadedAssembly assy with
        | None -> Error assy
        | Some assy ->

        match assy.TypeDefs.TryGetValue ty with
        | false, _ -> failwith $"could not find type def in assembly %s{assy.Name.FullName}"
        | true, v ->

        if not (typeGenerics.IsEmpty && methodGenerics.IsEmpty) then
            failwith "TODO: generics"

        {
            AssemblyFullName = assy.Name.FullName
            FullyQualifiedTypeName = $"%s{v.Namespace}.%s{v.Name}"
            Generics = []
        }
        |> Ok
