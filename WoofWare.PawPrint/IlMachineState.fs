namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IlMachineState =
    let loadAssembly = IlMachineTypeResolution.loadAssembly

    let internal loader = IlMachineTypeResolution.loader

    let concretizeType = IlMachineTypeResolution.concretizeType

    let internal resolveTopLevelTypeFromName =
        IlMachineTypeResolution.resolveTopLevelTypeFromName

    let resolveTypeFromExport = IlMachineTypeResolution.resolveTypeFromExport

    let resolveTypeFromRef = IlMachineTypeResolution.resolveTypeFromRef

    let resolveType = IlMachineTypeResolution.resolveType

    let resolveTypeFromDefn = IlMachineTypeResolution.resolveTypeFromDefn

    let resolveTypeFromSpec = IlMachineTypeResolution.resolveTypeFromSpec

    let resolveTypeFromSpecConcrete =
        IlMachineTypeResolution.resolveTypeFromSpecConcrete

    let resolveTypeFromDefnConcrete =
        IlMachineTypeResolution.resolveTypeFromDefnConcrete

    let runtimeTypeHandleTargetForTypeToken =
        IlMachineTypeResolution.runtimeTypeHandleTargetForTypeToken

    let cliTypeZeroOfHandle = IlMachineTypeResolution.cliTypeZeroOfHandle

    let concretizeFieldDeclaringType =
        IlMachineTypeResolution.concretizeFieldDeclaringType

    let cliTypeZeroOf = IlMachineTypeResolution.cliTypeZeroOf

    let ensureByteConcreteType = IlMachineTypeResolution.ensureByteConcreteType

    let peByteRangeForFieldRva = IlMachineTypeResolution.peByteRangeForFieldRva

    let peByteRangePointer = IlMachineTypeResolution.peByteRangePointer

    // --- Cross-thread frame resolution primitives ---

    let getFrame (thread : ThreadId) (frameId : FrameId) (state : IlMachineState) : MethodState =
        ThreadState.getFrame frameId state.ThreadState.[thread]

    let setFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (frame : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.setFrame frameId frame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let mapFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (f : MethodState -> MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.mapFrame frameId f threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

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
        let heap = ManagedHeap.setArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let getArrayValue (arrayAllocation : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.getArrayValue arrayAllocation index state.ManagedHeap

    /// Pops a synthetic frame that is only a dispatch trampoline, not a real method return.
    /// The concrete callee it dispatches to is responsible for producing any return value.
    let returnFromSyntheticStackFrame (currentThread : ThreadId) (state : IlMachineState) : ReturnFrameResult =
        let threadStateWithSyntheticFrame = state.ThreadState.[currentThread]
        let syntheticFrameId = threadStateWithSyntheticFrame.ActiveMethodState

        match threadStateWithSyntheticFrame.MethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->
            match returnState.WasConstructingObj with
            | Some _ ->
                failwith
                    $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly represented object construction"
            | None ->
                if returnState.DispatchAsExceptionOnReturn then
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly requested exception dispatch on return"

                match returnState.WasInitialisingType with
                | None -> ()
                | Some _ ->
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly represented type initialisation"

                match threadStateWithSyntheticFrame.MethodState.EvaluationStack.Values with
                | [] -> ()
                | _ ->
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly had evaluation stack values"

                let callerFrame =
                    ThreadState.getFrame returnState.JumpTo threadStateWithSyntheticFrame

                let threadState =
                    threadStateWithSyntheticFrame
                    |> ThreadState.setActiveFrame returnState.JumpTo
                    |> ThreadState.removeFrame syntheticFrameId

                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread threadState
                }
                |> ReturnFrameResult.NormalReturn

    /// There might be no stack frame to return to, so you might get NoFrameToReturn.
    let returnStackFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ReturnFrameResult
        =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]
        let returningFrameId = threadStateAtEndOfMethod.ActiveMethodState
        let returningMethodState = threadStateAtEndOfMethod.MethodState

        match returningMethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let callerFrame = ThreadState.getFrame returnState.JumpTo threadStateAtEndOfMethod

        let threadState =
            threadStateAtEndOfMethod
            |> ThreadState.setActiveFrame returnState.JumpTo
            |> ThreadState.removeFrame returningFrameId

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            if returnState.DispatchAsExceptionOnReturn then
                // This ctor was constructing a runtime-synthesised exception object.
                // Don't push it onto the eval stack; signal to the caller that exception
                // dispatch should occur.
                let constructed = state.ManagedHeap.NonArrayObjects.[constructing]
                ReturnFrameResult.DispatchException (state, constructing, constructed.ConcreteType)
            else

            // Assumption: a constructor can't also return a value.
            // If we were constructing a reference type, we push a reference to it.
            // Otherwise, extract the now-complete object from the heap and push it to the stack directly.
            let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

            let ty =
                AllConcreteTypes.lookup constructed.ConcreteType state.ConcreteTypes
                |> Option.get

            let ty' =
                state.LoadedAssembly (ty.Assembly)
                |> Option.get
                |> fun a -> a.TypeDefs.[ty.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty' then
                state
                // TODO: ordering of fields probably important
                |> pushToEvalStack (CliType.ValueType constructed.Contents) currentThread
            else
                state |> pushToEvalStack (CliType.ofManagedObject constructing) currentThread
            |> ReturnFrameResult.NormalReturn
        | None ->
            let retType = returningMethodState.ExecutingMethod.Signature.ReturnType

            match retType, returningMethodState.EvaluationStack.Values with
            | MethodReturnType.Void, [] -> state
            | MethodReturnType.Void, _ ->
                failwith
                    $"Invalid CIL: void method %s{returningMethodState.ExecutingMethod.Name} returned with a non-empty evaluation stack"
            | MethodReturnType.Returns _, [] ->
                failwith
                    $"Invalid CIL: non-void method %s{returningMethodState.ExecutingMethod.Name} returned with an empty evaluation stack"
            | MethodReturnType.Returns retType, [ retVal ] ->
                let zero, state = cliTypeZeroOfHandle state baseClassTypes retType

                let toPush = EvalStackValue.toCliTypeCoerced zero retVal

                state |> pushToEvalStack toPush currentThread
            | MethodReturnType.Returns _, _ ->
                failwith
                    $"Invalid CIL: method %s{returningMethodState.ExecutingMethod.Name} returned with more than one evaluation stack value"

            |> ReturnFrameResult.NormalReturn

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
                ConcreteTypes = AllConcreteTypes.Empty
                Logger = logger
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                _Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
                TypeHandles = TypeHandleRegistry.empty ()
                GcHandles = GcHandleRegistry.empty ()
                FieldHandles = FieldHandleRegistry.empty ()
                MethodHandles = MethodHandleRegistry.empty ()
                HardwareIntrinsics = HardwareIntrinsicsProfile.ScalarOnly
                Debugger = DebuggerState.Detached
                RuntimeAssemblyObjects = ImmutableDictionary.Empty
                RuntimeModuleObjects = ImmutableDictionary.Empty
                ManagedThreadObjects = Map.empty
                NextManagedThreadId = 2
                LastPInvokeError = 0
                LastSystemError = 0
            }

        state.WithLoadedAssembly assyName entryAssembly

    let addThread (newThreadState : MethodState) (state : IlMachineState) : IlMachineState * ThreadId =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState = state.ThreadState |> Map.add thread (ThreadState.New newThreadState)
            }

        newState, thread

    let allocateArray
        (arrayType : ConcreteTypeHandle)
        (zeroOfType : unit -> CliType)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                ConcreteType = arrayType
                Length = len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.setStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject
        (ty : ConcreteTypeHandle)
        (fields : CliValueType)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Contents = fields
                ConcreteType = ty
                SyncBlock = SyncBlock.Free
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray o

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
            MethodState.popFromStackToVariable localVariableIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let popFromStackToArgument (thread : ThreadId) (argumentIndex : int) (state : IlMachineState) : IlMachineState =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToArg argumentIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
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

    let resolveMemberWithGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (typeGenerics : ImmutableArray<TypeDefn>)
        (methodGenerics : ImmutableArray<TypeDefn>)
        (genericMethodTypeArgs : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]
        let sourceAssembly = assy

        let memberName : string = assy.Strings mem.Name

        let state, assy, targetType, extractedTypeArgs =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                // TODO: generics here?
                let state, assy, targetType =
                    resolveType loggerFactory parent ImmutableArray.Empty assy state

                state, assy, targetType, ImmutableArray.Empty // No type args from TypeReference
            | MetadataToken.TypeSpecification parent ->
                let state, assy, targetType =
                    resolveTypeFromSpec loggerFactory baseClassTypes parent assy typeGenerics methodGenerics state

                // Extract type arguments from the resolved type
                let extractedTypeArgs = targetType.Generics

                state, assy, targetType, extractedTypeArgs
            | parent -> failwith $"Unexpected: {parent}"

        let state, concreteExtractedTypeArgs =
            ((state, ImmutableArray.CreateBuilder ()), extractedTypeArgs)
            ||> Seq.fold (fun (state, acc) ty ->
                // TODO: generics?
                let state, t =
                    concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        targetType.Assembly
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty

                acc.Add t
                state, acc
            )
            |> Tuple.rmap (fun x -> x.ToImmutable ())

        match mem.Signature with
        | MemberSignature.Field fieldSig ->
            // Concretize the field signature from the member reference
            let state, concreteFieldSig =
                concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    sourceAssembly.Name
                    concreteExtractedTypeArgs
                    ImmutableArray.Empty
                    fieldSig

            // Find matching fields by comparing concretized signatures
            let state, availableFields =
                ((state, []), targetType.Fields)
                ||> List.fold (fun (state, acc) fi ->
                    if fi.Name <> memberName then
                        state, acc
                    else
                        // Concretize the field's signature for comparison
                        let state, fieldSigConcrete =
                            concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                assy.Name
                                concreteExtractedTypeArgs
                                ImmutableArray.Empty
                                fi.Signature

                        if fieldSigConcrete = concreteFieldSig then
                            state, fi :: acc
                        else
                            state, acc
                )

            let field =
                match availableFields with
                | [] ->
                    failwith
                        $"Could not find field member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> FieldInfo.mapTypeGenerics (fun _ (par, md) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for {targetType.Namespace}.{targetType.Name}'s field {memberName}!"

            state, assy.Name, Choice2Of2 field, extractedTypeArgs

        | MemberSignature.Method memberSig ->
            let availableMethods =
                targetType.Methods |> List.filter (fun mi -> mi.Name = memberName)

            let state, memberSig =
                memberSig
                |> TypeMethodSignature.map
                    state
                    (fun state ty ->
                        concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            sourceAssembly.Name
                            concreteExtractedTypeArgs
                            genericMethodTypeArgs
                            ty
                    )

            let state, availableMethods =
                ((state, []), availableMethods)
                ||> List.fold (fun (state, acc) meth ->
                    // A candidate overload whose generic arity doesn't match the call site
                    // cannot be the target. Reject it up front: concretising its signature
                    // would otherwise index past the end of `genericMethodTypeArgs` (which
                    // was sized for `memberSig`) whenever the candidate signature mentions
                    // a `GenericMethodParameter`. See e.g. Interlocked.CompareExchange,
                    // where the generic `<T>` overload sits alongside type-specific ones.
                    if meth.Signature.GenericParameterCount <> memberSig.GenericParameterCount then
                        state, acc
                    else
                        let state, methSig =
                            meth.Signature
                            |> TypeMethodSignature.map
                                state
                                (fun state ty ->
                                    concretizeType
                                        loggerFactory
                                        baseClassTypes
                                        state
                                        assy.Name
                                        concreteExtractedTypeArgs
                                        genericMethodTypeArgs
                                        ty
                                )

                        if methSig = memberSig then
                            state, meth :: acc
                        else
                            state, acc
                )

            let method =
                match availableMethods with
                | [] ->
                    failwith
                        $"Could not find member {memberName} with the right signature {memberSig} on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

            state, assy.Name, Choice1Of2 method, extractedTypeArgs

    let resolveMember
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (genericMethodTypeArgs : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod

        let toTypeDefn (handle : ConcreteTypeHandle) : TypeDefn =
            Concretization.concreteHandleToTypeDefn baseClassTypes handle state.ConcreteTypes state._LoadedAssemblies

        let typeGenerics =
            executing.DeclaringType.Generics
            |> Seq.map toTypeDefn
            |> ImmutableArray.CreateRange

        let methodGenerics =
            executing.Generics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        resolveMemberWithGenerics
            loggerFactory
            baseClassTypes
            currentThread
            assy
            typeGenerics
            methodGenerics
            genericMethodTypeArgs
            m
            state

    let getLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (state : IlMachineState)
        : CliType
        =
        (getFrame thread frameId state).LocalVariables.[int<uint16> varIndex]

    let setLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
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
                        | Some existing -> existing |> ThreadState.setLocalVariable frameId varIndex value |> Some
                    )
        }

    let setArgument
        (thread : ThreadId)
        (frameId : FrameId)
        (argIndex : uint16)
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
                        | None -> failwith "tried to set argument in nonactive thread"
                        | Some existing -> existing |> ThreadState.setArgument frameId argIndex value |> Some
                    )
        }

    let allocateLocalMemory
        (thread : ThreadId)
        (initialization : LocalMemoryInitialization)
        (byteCount : int)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let frameId = threadState.ActiveMethodState
        let frame = ThreadState.getFrame frameId threadState

        let blockId, pool =
            LocalMemoryPool.allocate initialization byteCount frame.LocalMemoryPool

        let frame =
            { frame with
                LocalMemoryPool = pool
            }

        let state = setFrame thread frameId frame state

        ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frameId, blockId, 0), []), state

    let readLocalMemoryBytes
        (thread : ThreadId)
        (frameId : FrameId)
        (blockId : LocallocBlockId)
        (byteOffset : int)
        (byteCount : int)
        (state : IlMachineState)
        : byte[]
        =
        let frame = getFrame thread frameId state
        LocalMemoryPool.readBytes blockId byteOffset byteCount frame.LocalMemoryPool

    let writeLocalMemoryBytes
        (thread : ThreadId)
        (frameId : FrameId)
        (blockId : LocallocBlockId)
        (byteOffset : int)
        (bytes : byte[])
        (state : IlMachineState)
        : IlMachineState
        =
        let frame = getFrame thread frameId state

        let frame =
            { frame with
                LocalMemoryPool = LocalMemoryPool.writeBytes blockId byteOffset bytes frame.LocalMemoryPool
            }

        setFrame thread frameId frame state

    let setSyncBlock
        (addr : ManagedHeapAddress)
        (syncBlockValue : SyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = state.ManagedHeap |> ManagedHeap.setSyncBlock addr syncBlockValue
        }

    let getSyncBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        state.ManagedHeap |> ManagedHeap.getSyncBlock addr

    /// `true` when a `ReinterpretAs ty` projection against a value of the given
    /// shape can be treated as a no-op. Matches same-width primitive reinterprets
    /// within the integer family (including signed<->unsigned and char<->ushort
    /// pairs, which share bit patterns and round-trip through the Int32 stack
    /// slot with modular narrowing) and within the float family (same width
    /// only). Rejects float<->int bit reinterprets, overlay structs, enum
    /// underlying coercions, and any size change; those still need a proper
    /// bytewise implementation.
    let private classifyValueForReinterpret (value : CliType) : (string * int) voption =
        match value with
        | CliType.Bool _ -> ValueSome ("int", 1)
        | CliType.Char _ -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.UInt8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.Int16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.UInt16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int32 _) -> ValueSome ("int", 4)
        | CliType.Numeric (CliNumericType.Int64 _) -> ValueSome ("int", 8)
        | CliType.Numeric (CliNumericType.Float32 _) -> ValueSome ("float", 4)
        | CliType.Numeric (CliNumericType.Float64 _) -> ValueSome ("float", 8)
        | _ -> ValueNone

    let private classifyTypeForReinterpret (ty : ConcreteType<ConcreteTypeHandle>) : (string * int) voption =
        if ty.Namespace <> "System" then
            ValueNone
        else
            match ty.Name with
            | "Boolean"
            | "SByte"
            | "Byte" -> ValueSome ("int", 1)
            | "Int16"
            | "UInt16"
            | "Char" -> ValueSome ("int", 2)
            | "Int32"
            | "UInt32" -> ValueSome ("int", 4)
            | "Int64"
            | "UInt64" -> ValueSome ("int", 8)
            | "Single" -> ValueSome ("float", 4)
            | "Double" -> ValueSome ("float", 8)
            | _ -> ValueNone

    let private isSafeReinterpretPassthrough (value : CliType) (ty : ConcreteType<ConcreteTypeHandle>) : bool =
        match classifyValueForReinterpret value, classifyTypeForReinterpret ty with
        | ValueSome v, ValueSome t -> v = t
        | _ -> false

    let private zeroForPrimitiveReinterpret (ty : ConcreteType<ConcreteTypeHandle>) : CliType voption =
        if ty.Namespace <> "System" || not ty.Generics.IsEmpty then
            ValueNone
        else
            match ty.Name with
            | "Boolean" -> ValueSome (CliType.Bool 0uy)
            | "SByte" -> ValueSome (CliType.Numeric (CliNumericType.Int8 0y))
            | "Byte" -> ValueSome (CliType.Numeric (CliNumericType.UInt8 0uy))
            | "Int16" -> ValueSome (CliType.Numeric (CliNumericType.Int16 0s))
            | "UInt16" -> ValueSome (CliType.Numeric (CliNumericType.UInt16 0us))
            | "Char" -> ValueSome (CliType.Char (0uy, 0uy))
            | "Int32"
            | "UInt32" ->
                // ECMA III.1.1.1 has no separate unsigned 32-bit stack type;
                // PawPrint stores UInt32-shaped values in the same CliType as Int32.
                ValueSome (CliType.Numeric (CliNumericType.Int32 0))
            | "Int64"
            | "UInt64" -> ValueSome (CliType.Numeric (CliNumericType.Int64 0L))
            | "IntPtr"
            | "UIntPtr" -> ValueSome (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
            | "Single" -> ValueSome (CliType.Numeric (CliNumericType.Float32 0.0f))
            | "Double" -> ValueSome (CliType.Numeric (CliNumericType.Float64 0.0))
            | _ -> ValueNone

    let setStatic
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (value : CliType)
        (this : IlMachineState)
        : IlMachineState
        =
        let statics =
            match this._Statics.TryGetValue ty with
            | false, _ -> this._Statics.Add (ty, Map.ofList [ field, value ])
            | true, v -> this._Statics.SetItem (ty, Map.add field value v)

        { this with
            _Statics = statics
        }

    let getStatic
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (this : IlMachineState)
        : CliType option
        =
        match this._Statics.TryGetValue ty with
        | false, _ -> None
        | true, v -> Map.tryFind field v

    let private readRootValue (state : IlMachineState) (root : ByrefRoot) : CliType =
        match root with
        | ByrefRoot.LocalVariable (t, f, v) -> (getFrame t f state).LocalVariables.[int<uint16> v]
        | ByrefRoot.Argument (t, f, v) -> (getFrame t f state).Arguments.[int<uint16> v]
        | ByrefRoot.LocalMemoryByte (t, f, block, byteOffset) ->
            readLocalMemoryBytes t f block byteOffset 1 state
            |> Array.exactlyOne
            |> CliNumericType.UInt8
            |> CliType.Numeric
        | ByrefRoot.HeapValue addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | ByrefRoot.HeapObjectField (addr, field) ->
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById field
        | ByrefRoot.ArrayElement (arr, index) -> getArrayValue arr index state
        | ByrefRoot.PeByteRange peByteRange ->
            failwith
                $"TODO: reading PE byte-range root %O{peByteRange} requires a primitive byte-view projection; plain typed PE byte-range root reads are not modelled"
        | ByrefRoot.StaticField (ty, field) ->
            match getStatic ty field state with
            | Some value -> value
            | None ->
                failwith
                    $"Static field byref %O{field.Get} on concrete type %O{ty} was read before the static slot was initialised"
        | ByrefRoot.StringCharAt (str, charIndex) ->
            ManagedHeap.getStringChar str charIndex state.ManagedHeap |> CliType.ofChar

    let private writeRootValue (state : IlMachineState) (root : ByrefRoot) (updated : CliType) : IlMachineState =
        match root with
        | ByrefRoot.LocalVariable (t, f, v) -> state |> setLocalVariable t f v updated
        | ByrefRoot.Argument (t, f, v) -> state |> setArgument t f v updated
        | ByrefRoot.LocalMemoryByte (t, f, block, byteOffset) ->
            // A bare LocalMemoryByte root is a single-byte cell. Wider local-memory
            // writes go through splitTrailingByteView/writeManagedByrefBytes instead.
            let byteValue =
                match updated with
                | CliType.Numeric (CliNumericType.UInt8 b) -> b
                | other -> failwith $"cannot write non-byte value %O{other} through local-memory byte root %O{block}"

            writeLocalMemoryBytes t f block byteOffset [| byteValue |] state
        | ByrefRoot.HeapValue addr ->
            let contents =
                match updated with
                | CliType.ValueType contents -> contents
                | other -> failwith $"cannot write non-value-type {other} through heap value byref"

            let existing = ManagedHeap.get addr state.ManagedHeap

            { state with
                ManagedHeap =
                    ManagedHeap.set
                        addr
                        { existing with
                            Contents = contents
                        }
                        state.ManagedHeap
            }
        | ByrefRoot.HeapObjectField (addr, field) ->
            let updated =
                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetFieldById field updated

            { state with
                ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
            }
        | ByrefRoot.ArrayElement (arr, index) -> state |> setArrayValue arr updated index
        | ByrefRoot.PeByteRange peByteRange ->
            failwith $"PE byte range is read-only; refusing to write %O{updated} through %O{peByteRange}"
        | ByrefRoot.StaticField (ty, field) -> state |> setStatic ty field updated
        | ByrefRoot.StringCharAt (str, charIndex) ->
            let updated =
                match updated with
                | CliType.Char (high, low) -> char (int high * 256 + int low)
                | other ->
                    // Direct same-width primitive writes, for example Stind.I2
                    // storing a UInt16 through a ref char byref, preserve the
                    // raw UTF-16 bits while normalising the stored cell to char.
                    let charTemplate = CliType.ofChar (char 0)
                    let updatedBytes = CliType.ToBytes other

                    if updatedBytes.Length <> CliType.sizeOf charTemplate then
                        failwith
                            $"string character write expected a 2-byte char-compatible value, got %d{updatedBytes.Length} bytes from %O{other}"

                    match CliType.ofBytesLike charTemplate updatedBytes with
                    | CliType.Char (high, low) -> char (int high * 256 + int low)
                    | reconstructed -> failwith $"string character write reconstructed non-char value %O{reconstructed}"

            { state with
                ManagedHeap = ManagedHeap.setStringChar str charIndex updated state.ManagedHeap
            }

    let private readProjectedValue (rootValue : CliType) (projs : ByrefProjection list) : CliType =
        projs
        |> List.fold
            (fun value proj ->
                match proj with
                | ByrefProjection.Field field ->
                    match value with
                    | CliType.ValueType vt -> CliValueType.DereferenceFieldById field vt
                    | v -> failwith $"could not find field {field.Name} on non-ValueType {v}"
                | ByrefProjection.ReinterpretAs ty ->
                    if isSafeReinterpretPassthrough value ty then
                        value
                    else
                        failwith
                            $"TODO: read through `ReinterpretAs` from value %O{value} as type %s{ty.Namespace}.%s{ty.Name}; needs a bytewise implementation"
                | ByrefProjection.ByteOffset n ->
                    failwith
                        $"TODO: readManagedByref via ByteOffset %d{n} requires a trailing byte-view byref shape; generic Ldind at a non-normalised byte offset is not modelled (value: %O{value})"
            )
            rootValue

    let private primitiveCellBytes (context : string) (value : CliType) : byte[] =
        match value with
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _ -> CliType.ToBytes value
        | other ->
            failwith
                $"TODO: byte-view over non-primitive cell in %s{context}: %O{other} (struct/object byte streams are not modelled in PR B)"

    let private splitTrailingByteView (src : ManagedPointerSource) : (ByrefRoot * ByrefProjection list * int) voption =
        match src with
        | ManagedPointerSource.Null -> ValueNone
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revPrefix ->
                ValueSome (root, List.rev revPrefix, n)
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | ByrefProjection.ReinterpretAs _ :: revPrefix -> ValueSome (root, List.rev revPrefix, 0)
            | _ -> ValueNone

    let private floorDivRem (value : int) (divisor : int) : int * int =
        if divisor <= 0 then
            failwith $"floorDivRem requires a positive divisor, got %d{divisor}"

        let q = value / divisor
        let r = value - q * divisor

        if r < 0 then q - 1, r + divisor else q, r

    let private readArrayBytesAs
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let arrObj = state.ManagedHeap.Arrays.[arr]

        if arrObj.Length = 0 then
            failwith $"TODO: byte-view read from empty array %O{arr} at index %d{index} offset %d{byteOffset}"

        let firstCellBytes =
            primitiveCellBytes $"array %O{arr} element 0" arrObj.Elements.[0]

        let cellAdvance, inCellStart = floorDivRem byteOffset firstCellBytes.Length
        let buf = Array.zeroCreate<byte> targetSize
        let mutable filled = 0
        let mutable cell = index + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < targetSize do
            if cell < 0 || cell >= arrObj.Length then
                failwith
                    $"TODO: byte-view read past array bounds at cell %d{cell} of length %d{arrObj.Length} while gathering %d{targetSize} bytes"

            let cellBytes =
                primitiveCellBytes $"array %O{arr} element %d{cell}" arrObj.Elements.[cell]

            let canTake = cellBytes.Length - inCellOffset
            let take = min canTake (targetSize - filled)
            Array.blit cellBytes inCellOffset buf filled take
            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        CliType.ofBytesLike targetTemplate buf

    let private readPeByteRangeBytesAs
        (state : IlMachineState)
        (peByteRange : PeByteRangePointer)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 || byteOffset + targetSize > peByteRange.Size then
            failwith
                $"PE byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside byte range size %d{peByteRange.Size}: %O{peByteRange}"

        let assembly =
            state.LoadedAssembly' peByteRange.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"PE byte-view read needs loaded assembly %s{peByteRange.AssemblyFullName}"
            )

        let sectionData =
            assembly.PeReader.GetSectionData peByteRange.RelativeVirtualAddress

        let mutable reader = sectionData.GetReader ()
        reader.Offset <- byteOffset
        let bytes = reader.ReadBytes targetSize

        CliType.ofBytesLike targetTemplate bytes

    let private readStringBytesAs
        (state : IlMachineState)
        (str : ManagedHeapAddress)
        (charIndex : int)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let cellAdvance, inCellStart = floorDivRem byteOffset 2
        let buf = Array.zeroCreate<byte> targetSize
        let mutable filled = 0
        let mutable cell = charIndex + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < targetSize do
            let charBytes =
                ManagedHeap.getStringChar str cell state.ManagedHeap
                |> CliType.ofChar
                |> CliType.ToBytes

            let canTake = charBytes.Length - inCellOffset
            let take = min canTake (targetSize - filled)
            Array.blit charBytes inCellOffset buf filled take
            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        CliType.ofBytesLike targetTemplate buf

    let private heapValueBytes (operation : string) (state : IlMachineState) (addr : ManagedHeapAddress) : byte[] =
        let obj = ManagedHeap.get addr state.ManagedHeap

        if CliValueType.ContainsObjectReferences obj.Contents then
            failwith $"%s{operation}: refusing byte view over boxed value type containing object references at %O{addr}"

        // Writes reconstruct the boxed value with CliValueType.OfBytesLike; padded
        // layouts have bytes that are not represented by fields, so they cannot
        // be preserved through that reconstruction today. RawBytes storage is
        // also rejected by this predicate until boxed byte-view writes have
        // dedicated coverage for fieldless raw-backed value types.
        if not (CliValueType.IsTightlyPacked obj.Contents) then
            failwith $"%s{operation}: refusing byte view over non-tightly-packed boxed value type at %O{addr}"

        CliValueType.ToBytes obj.Contents

    let private readHeapValueBytesAs
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let bytes = heapValueBytes "boxed value byte-view read" state addr
        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 || byteOffset + targetSize > bytes.Length then
            failwith
                $"boxed value byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside %d{bytes.Length}-byte boxed payload at %O{addr}"

        CliType.ofBytesLike targetTemplate bytes.[byteOffset .. byteOffset + targetSize - 1]

    let private readLocalMemoryBytesAs
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : LocallocBlockId)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let bytes = readLocalMemoryBytes thread frame block byteOffset targetSize state
        CliType.ofBytesLike targetTemplate bytes

    let readManagedByrefBytesAs
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (targetTemplate : CliType)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) -> readHeapValueBytesAs state addr 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset), []) ->
            readLocalMemoryBytesAs state thread frame block byteOffset targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            readArrayBytesAs state arr index 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, []) ->
            readPeByteRangeBytesAs state peByteRange 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            readStringBytesAs state str charIndex 0 targetTemplate
        | ManagedPointerSource.Byref (outerRoot, outerProjs) ->
            match splitTrailingByteView src with
            | ValueSome (ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset), [], byteOffset) ->
                readLocalMemoryBytesAs state thread frame block (rootByteOffset + byteOffset) targetTemplate
            | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                readArrayBytesAs state arr index byteOffset targetTemplate
            | ValueSome (ByrefRoot.PeByteRange peByteRange, [], byteOffset) ->
                readPeByteRangeBytesAs state peByteRange byteOffset targetTemplate
            | ValueSome (ByrefRoot.PeByteRange peByteRange, prefixProjs, _) ->
                failwith $"TODO: PE byte-view read with non-empty prefix projections %O{prefixProjs}: %O{peByteRange}"
            | ValueSome (ByrefRoot.StringCharAt (str, charIndex), [], byteOffset) ->
                readStringBytesAs state str charIndex byteOffset targetTemplate
            | ValueSome (ByrefRoot.HeapValue addr, [], byteOffset) ->
                readHeapValueBytesAs state addr byteOffset targetTemplate
            | ValueSome (byteViewRoot, prefixProjs, byteOffset) ->
                let rootValue = readRootValue state byteViewRoot
                let cell = readProjectedValue rootValue prefixProjs
                let cellBytes = primitiveCellBytes $"single-cell byref %O{src}" cell
                let targetSize = CliType.sizeOf targetTemplate

                if byteOffset < 0 || byteOffset + targetSize > cellBytes.Length then
                    failwith
                        $"TODO: byte-view read at offset %d{byteOffset} for %d{targetSize} bytes does not fit in single primitive cell of size %d{cellBytes.Length}: %O{src}"

                let bytes = cellBytes.[byteOffset .. byteOffset + targetSize - 1]
                CliType.ofBytesLike targetTemplate bytes
            | ValueNone ->
                let raw = readProjectedValue (readRootValue state outerRoot) outerProjs
                let rawBytes = primitiveCellBytes $"plain byref %O{src}" raw
                let targetSize = CliType.sizeOf targetTemplate

                if targetSize > rawBytes.Length then
                    failwith
                        $"TODO: byte-view read of %d{targetSize} bytes does not fit in plain primitive cell of size %d{rawBytes.Length}: %O{src}"

                CliType.ofBytesLike targetTemplate rawBytes.[0 .. targetSize - 1]

    let readManagedByref (state : IlMachineState) (src : ManagedPointerSource) : CliType =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs ty :: _
            | ByrefProjection.ReinterpretAs ty :: _ ->
                match zeroForPrimitiveReinterpret ty with
                | ValueSome targetTemplate -> readManagedByrefBytesAs state src targetTemplate
                | ValueNone ->
                    failwith
                        $"TODO: read through `ReinterpretAs` as non-primitive type %s{ty.Namespace}.%s{ty.Name}; struct/object byte views are not modelled in PR B"
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ -> readProjectedValue (readRootValue state root) projs

    let private zeroForConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ty : ConcreteType<ConcreteTypeHandle>)
        : CliType
        =
        let handle =
            AllConcreteTypes.findExistingConcreteType state.ConcreteTypes ty.Identity ty.Generics
            |> Option.defaultWith (fun () ->
                failwith $"ReinterpretAs target %O{ty} is not present in the concrete-type registry"
            )

        CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes handle
        |> fst

    let private readReinterpretedByrefField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (field : FieldId)
        : CliType
        =
        let targetTemplate = zeroForConcreteType baseClassTypes state reinterpretTy
        let fieldTemplate = CliType.getFieldById field targetTemplate
        let fieldOffset, fieldSize = CliType.getFieldLayoutById field targetTemplate

        match fieldTemplate with
        | CliType.ObjectRef _ ->
            match splitTrailingByteView src with
            | ValueSome (root, prefixProjs, byteOffset) ->
                let totalByteOffset = byteOffset + fieldOffset

                if totalByteOffset <> 0 then
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} starts at byte offset %d{totalByteOffset}; object-reference interior byte views are not modelled"

                if fieldSize <> CliType.sizeOf fieldTemplate then
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} has storage size %d{fieldSize}, expected %d{CliType.sizeOf fieldTemplate}"

                match readProjectedValue (readRootValue state root) prefixProjs with
                | CliType.ObjectRef _ as value -> value
                | other ->
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} over non-object storage %O{other}"
            | ValueNone ->
                failwith
                    $"TODO: object-reference field %O{field} through %O{reinterpretTy} without a trailing ReinterpretAs byte-view shape: %O{src}"
        | CliType.RuntimePointer _ ->
            failwith
                $"TODO: runtime-pointer field %O{field} through %O{reinterpretTy}; pointer byte views are not modelled"
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _
        | CliType.ValueType _ ->
            let fieldPtr =
                if fieldOffset = 0 then
                    src
                else
                    ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset fieldOffset) src

            readManagedByrefBytesAs state fieldPtr fieldTemplate

    let readManagedByrefField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (field : FieldId)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs ty :: _
            | ByrefProjection.ReinterpretAs ty :: _ -> readReinterpretedByrefField baseClassTypes state src ty field
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ ->
                readProjectedValue (readRootValue state root) projs
                |> CliType.getFieldById field

    let private applyProjectionsForWrite
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType
        =
        let rec go (rootValue : CliType) (projs : ByrefProjection list) (newValue : CliType) : CliType =
            match projs with
            | [] -> newValue
            | [ ByrefProjection.Field field ] -> CliType.withFieldSetById field newValue rootValue
            | ByrefProjection.Field field :: rest ->
                let fieldValue = CliType.getFieldById field rootValue
                let updatedField = go fieldValue rest newValue
                CliType.withFieldSetById field updatedField rootValue
            | [ ByrefProjection.ReinterpretAs ty ] ->
                // Same safety gate as `readManagedByref`: size-preserving
                // primitive reinterprets share storage with the underlying
                // value. Require both the stored value and the newValue to
                // match the reinterpret target's natural representation; if
                // either differs, the caller is doing a bit-reinterpret we
                // don't model and the write stays an explicit TODO.
                if
                    isSafeReinterpretPassthrough rootValue ty
                    && isSafeReinterpretPassthrough newValue ty
                then
                    // Normalise the stored value back to the rootValue's
                    // CliType so the slot keeps its original view: writing a
                    // `short` through a `ref short` obtained via
                    // `Unsafe.As<ushort, short>` must leave the backing slot
                    // as a ushort with bit-preserving narrowing, not replace
                    // the slot's type with Int16. The stack round-trip matches
                    // ECMA III.1.1.1 narrowing semantics for same-width ints;
                    // it's the identity for matching-float widths.
                    EvalStackValue.toCliTypeCoerced rootValue (EvalStackValue.ofCliType newValue)
                else
                    failwith
                        $"TODO: write through `ReinterpretAs` as type %s{ty.Namespace}.%s{ty.Name}; rootValue=%O{rootValue}, newValue=%O{newValue}"
            | ByrefProjection.ReinterpretAs ty :: _ ->
                failwith
                    $"TODO: write through `ReinterpretAs` as %s{ty.Namespace}.%s{ty.Name} followed by further projections; needs a bytewise implementation"
            | ByrefProjection.ByteOffset n :: _ ->
                // Symmetric to the readManagedByref ByteOffset case: byte-offset
                // writes go through Unsafe.WriteUnaligned (which scatters bytes
                // into the cell stream directly), not through the generic write
                // fold. Reaching here means a generic Stind at a non-zero byte
                // offset, which we don't yet model.
                failwith
                    $"TODO: writeManagedByref via ByteOffset %d{n} requires the bytewise scatter implemented by Unsafe.WriteUnaligned; generic Stind at a non-zero byte offset is out of scope for this PR"

        go rootValue projs newValue

    let private writeArrayBytes
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let arrObj = state.ManagedHeap.Arrays.[arr]

        if arrObj.Length = 0 then
            failwith $"TODO: byte-view write to empty array %O{arr} at index %d{index} offset %d{byteOffset}"

        let firstCellBytes =
            primitiveCellBytes $"array %O{arr} element 0" arrObj.Elements.[0]

        let cellAdvance, inCellStart = floorDivRem byteOffset firstCellBytes.Length
        let mutable state = state
        let mutable filled = 0
        let mutable cell = index + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < bytes.Length do
            if cell < 0 || cell >= arrObj.Length then
                failwith $"TODO: byte-view write past array bounds at cell %d{cell} of length %d{arrObj.Length}"

            let existing = state.ManagedHeap.Arrays.[arr].Elements.[cell]
            let existingBytes = primitiveCellBytes $"array %O{arr} element %d{cell}" existing
            let canTake = existingBytes.Length - inCellOffset
            let take = min canTake (bytes.Length - filled)
            let newCellBytes = Array.copy existingBytes
            Array.blit bytes filled newCellBytes inCellOffset take
            let newCell = CliType.ofBytesLike existing newCellBytes
            state <- setArrayValue arr newCell cell state
            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        state

    let private writeLocalMemoryBytesAt
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : LocallocBlockId)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        writeLocalMemoryBytes thread frame block byteOffset bytes state

    let private writeStringBytes
        (state : IlMachineState)
        (str : ManagedHeapAddress)
        (charIndex : int)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let cellAdvance, inCellStart = floorDivRem byteOffset 2
        let mutable state = state
        let mutable filled = 0
        let mutable cell = charIndex + cellAdvance
        let mutable inCellOffset = inCellStart
        let charTemplate = CliType.ofChar (char 0)
        let cellSize = CliType.sizeOf charTemplate

        while filled < bytes.Length do
            let canTake = cellSize - inCellOffset
            let take = min canTake (bytes.Length - filled)

            let newCellBytes =
                if inCellOffset = 0 && take = cellSize then
                    bytes.[filled .. filled + cellSize - 1]
                else
                    let existingBytes =
                        ManagedHeap.getStringChar str cell state.ManagedHeap
                        |> CliType.ofChar
                        |> CliType.ToBytes

                    let newCellBytes = Array.copy existingBytes
                    Array.blit bytes filled newCellBytes inCellOffset take
                    newCellBytes

            let newChar =
                match CliType.ofBytesLike charTemplate newCellBytes with
                | CliType.Char (high, low) -> char (int high * 256 + int low)
                | other -> failwith $"string byte-view write reconstructed non-char value %O{other}"

            state <-
                { state with
                    ManagedHeap = ManagedHeap.setStringChar str cell newChar state.ManagedHeap
                }

            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        state

    let private writeHeapValueBytes
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let existing = ManagedHeap.get addr state.ManagedHeap
        let existingBytes = heapValueBytes "boxed value byte-view write" state addr

        if byteOffset < 0 || byteOffset + bytes.Length > existingBytes.Length then
            failwith
                $"boxed value byte-view write at offset %d{byteOffset} for %d{bytes.Length} bytes is outside %d{existingBytes.Length}-byte boxed payload at %O{addr}"

        let updatedBytes = Array.copy existingBytes
        Array.blit bytes 0 updatedBytes byteOffset bytes.Length

        let updated =
            { existing with
                Contents = CliValueType.OfBytesLike existing.Contents updatedBytes
            }

        { state with
            ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
        }

    let writeManagedByrefBytes
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        let bytes = CliType.ToBytes newValue

        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) -> writeHeapValueBytes state addr 0 bytes
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset), []) ->
            writeLocalMemoryBytesAt state thread frame block byteOffset bytes
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, _) ->
            failwith
                $"PE byte range is read-only; refusing byte-view write of %d{bytes.Length} bytes through %O{peByteRange}"
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            writeStringBytes state str charIndex 0 bytes
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            writeArrayBytes state arr index 0 bytes
        | ManagedPointerSource.Byref (outerRoot, outerProjs) ->
            match splitTrailingByteView src with
            | ValueSome (ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset), [], byteOffset) ->
                writeLocalMemoryBytesAt state thread frame block (rootByteOffset + byteOffset) bytes
            | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                writeArrayBytes state arr index byteOffset bytes
            | ValueSome (ByrefRoot.StringCharAt (str, charIndex), [], byteOffset) ->
                writeStringBytes state str charIndex byteOffset bytes
            | ValueSome (ByrefRoot.HeapValue addr, [], byteOffset) -> writeHeapValueBytes state addr byteOffset bytes
            | ValueSome (byteViewRoot, prefixProjs, byteOffset) ->
                let rootValue = readRootValue state byteViewRoot
                let cell = readProjectedValue rootValue prefixProjs
                let cellBytes = primitiveCellBytes $"single-cell byref %O{src}" cell

                if byteOffset < 0 || byteOffset + bytes.Length > cellBytes.Length then
                    failwith
                        $"TODO: byte-view write at offset %d{byteOffset} for %d{bytes.Length} bytes does not fit in single primitive cell of size %d{cellBytes.Length}: %O{src}"

                let updatedCellBytes = Array.copy cellBytes
                Array.blit bytes 0 updatedCellBytes byteOffset bytes.Length
                let updatedCell = CliType.ofBytesLike cell updatedCellBytes
                let updatedRoot = applyProjectionsForWrite rootValue prefixProjs updatedCell
                writeRootValue state byteViewRoot updatedRoot
            | ValueNone ->
                let rootValue = readRootValue state outerRoot
                let cell = readProjectedValue rootValue outerProjs
                let cellBytes = primitiveCellBytes $"plain byref %O{src}" cell

                if bytes.Length > cellBytes.Length then
                    failwith
                        $"TODO: byte-view write of %d{bytes.Length} bytes does not fit in plain primitive cell of size %d{cellBytes.Length}: %O{src}"

                let updatedCellBytes = Array.copy cellBytes
                Array.blit bytes 0 updatedCellBytes 0 bytes.Length
                let updatedCell = CliType.ofBytesLike cell updatedCellBytes
                let updatedRoot = applyProjectionsForWrite rootValue outerProjs updatedCell
                writeRootValue state outerRoot updatedRoot

    let private splitFirstReinterpret
        (projs : ByrefProjection list)
        : (ByrefProjection list * ConcreteType<ConcreteTypeHandle> * ByrefProjection list) option
        =
        let rec loop (revPrefix : ByrefProjection list) (remaining : ByrefProjection list) =
            match remaining with
            | [] -> None
            | ByrefProjection.ReinterpretAs ty :: rest -> Some (List.rev revPrefix, ty, rest)
            | proj :: rest -> loop (proj :: revPrefix) rest

        loop [] projs

    let private describeCliStorage (state : IlMachineState) (value : CliType) : string =
        match value with
        | CliType.ValueType vt ->
            match AllConcreteTypes.lookup vt.Declared state.ConcreteTypes with
            | Some ty -> $"%O{ty}"
            | None -> $"value type handle %O{vt.Declared}"
        | CliType.ObjectRef _ -> "object reference"
        | CliType.RuntimePointer _ -> "runtime pointer"
        | CliType.Numeric numeric -> $"numeric %O{numeric}"
        | CliType.Bool _ -> "bool"
        | CliType.Char _ -> "char"

    let private reinterpretStorageBytes
        (state : IlMachineState)
        (operation : string)
        (storageValue : CliType)
        : byte[]
        =
        match storageValue with
        | CliType.ObjectRef _ ->
            failwith
                $"TODO: %s{operation}: write through `ReinterpretAs` over object-reference storage is not modelled; storage type was %s{describeCliStorage state storageValue}"
        | CliType.RuntimePointer _ ->
            failwith
                $"TODO: %s{operation}: write through `ReinterpretAs` over runtime-pointer storage is not modelled; storage type was %s{describeCliStorage state storageValue}"
        | CliType.ValueType vt when CliValueType.ContainsObjectReferences vt ->
            failwith
                $"TODO: %s{operation}: write through `ReinterpretAs` over value-type storage containing object references is not modelled; storage type was %s{describeCliStorage state storageValue}"
        | _ -> CliType.ToBytes storageValue

    let private ofBytesLikeForReinterpret
        (state : IlMachineState)
        (operation : string)
        (storageValue : CliType)
        (bytes : byte[])
        : CliType
        =
        try
            CliType.ofBytesLike storageValue bytes
        with ex ->
            failwith
                $"%s{operation}: failed to reconstruct storage type %s{describeCliStorage state storageValue} from reinterpreted bytes. Reinterpret writes into unrepresented padding are not modelled. Inner error: %s{ex.Message}"

    let private splitTrailingPrefixByteOffset (projs : ByrefProjection list) : ByrefProjection list * int =
        match List.rev projs with
        | ByrefProjection.ByteOffset n :: revPrefix -> List.rev revPrefix, n
        | _ -> projs, 0

    let rec private writeProjectedValue
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType
        =
        match baseClassTypes, splitFirstReinterpret projs with
        | Some baseClassTypes, Some (prefixProjs, reinterpretTy, reinterpretProjs) ->
            let storageProjs, byteOffset = splitTrailingPrefixByteOffset prefixProjs
            let storageValue = readProjectedValue rootValue storageProjs

            let updatedStorage =
                writeReinterpretedStorage
                    baseClassTypes
                    state
                    storageValue
                    byteOffset
                    reinterpretTy
                    reinterpretProjs
                    newValue

            applyProjectionsForWrite rootValue storageProjs updatedStorage
        | _ -> applyProjectionsForWrite rootValue projs newValue

    and private writeReinterpretedStorage
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (storageValue : CliType)
        (byteOffset : int)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (reinterpretProjs : ByrefProjection list)
        (newValue : CliType)
        : CliType
        =
        // Reinterpret writes are byte updates to the original storage shape. This covers patterns
        // such as `Unsafe.As<bool, VolatileBoolean>(ref location).Value = value`, and recurses for
        // nested `Unsafe.As` chains before rebuilding the original cell.
        let operation =
            $"write through `ReinterpretAs` as %s{reinterpretTy.Namespace}.%s{reinterpretTy.Name}"

        let storageBytes = reinterpretStorageBytes state operation storageValue
        let reinterpretZero = zeroForConcreteType baseClassTypes state reinterpretTy
        let reinterpretSize = CliType.sizeOf reinterpretZero

        if byteOffset < 0 || byteOffset + reinterpretSize > storageBytes.Length then
            failwith
                $"TODO: %s{operation} requires %d{reinterpretSize} bytes at offset %d{byteOffset}, but storage type %s{describeCliStorage state storageValue} has %d{storageBytes.Length} bytes"

        let reinterpretBytes = storageBytes.[byteOffset .. byteOffset + reinterpretSize - 1]

        let reinterpretTemplate =
            ofBytesLikeForReinterpret state operation reinterpretZero reinterpretBytes

        let updatedReinterpret =
            writeProjectedValue (Some baseClassTypes) state reinterpretTemplate reinterpretProjs newValue

        let updatedBytes = CliType.ToBytes updatedReinterpret

        if updatedBytes.Length <> reinterpretSize then
            failwith
                $"TODO: %s{operation} produced %d{updatedBytes.Length} bytes for reinterpret type %O{reinterpretTy}, expected %d{reinterpretSize}; storage type was %s{describeCliStorage state storageValue}"

        let updatedStorageBytes = Array.copy storageBytes
        Array.blit updatedBytes 0 updatedStorageBytes byteOffset updatedBytes.Length
        ofBytesLikeForReinterpret state operation storageValue updatedStorageBytes

    let private writeManagedByrefCore
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (root, []) -> writeRootValue state root newValue
        | ManagedPointerSource.Byref (root, projs) ->
            match splitTrailingByteView src with
            | ValueSome _ -> writeManagedByrefBytes state src newValue
            | ValueNone ->
                let rootValue = readRootValue state root
                let updatedRoot = writeProjectedValue baseClassTypes state rootValue projs newValue
                writeRootValue state root updatedRoot

    let writeManagedByref (state : IlMachineState) (src : ManagedPointerSource) (newValue : CliType) : IlMachineState =
        // Call sites that can supply BaseClassTypes should use writeManagedByrefWithBase so
        // non-trailing ReinterpretAs projections can be applied bytewise. This legacy entry point
        // remains for primitive/external boundaries that do not currently carry type metadata.
        writeManagedByrefCore None state src newValue

    let writeManagedByrefWithBase
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        writeManagedByrefCore (Some baseClassTypes) state src newValue

    let executeDelegateConstructor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (instruction : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        // We've been called with arguments already popped from the stack into local arguments.
        let constructing = instruction.Arguments.[0]
        let targetObj = instruction.Arguments.[1]
        let methodPtr = instruction.Arguments.[2]

        let targetObj =
            match targetObj with
            | CliType.ObjectRef (Some target) -> Some target
            | CliType.ObjectRef None -> None
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
            | _ -> failwith $"Unexpected target type for delegate: {targetObj}"

        let constructing =
            match constructing with
            | CliType.ObjectRef None -> failwith "unexpectedly constructing the null delegate"
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) ->
                failwith "unexpectedly constructing the null delegate"
            | CliType.ObjectRef (Some target) -> target
            | _ -> failwith $"Unexpectedly not constructing a managed object: {constructing}"

        let heapObj =
            match state.ManagedHeap.NonArrayObjects.TryGetValue constructing with
            | true, obj -> obj
            | false, _ -> failwith $"Delegate object {constructing} not found on heap"

        let delegateTypeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.DelegateType

        let targetField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.DelegateType "_target"
            |> FieldIdentity.fieldId delegateTypeHandle

        let methodPtrField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.DelegateType "_methodPtr"
            |> FieldIdentity.fieldId delegateTypeHandle

        let updatedObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById targetField (CliType.ObjectRef targetObj)
            |> AllocatedNonArrayObject.SetFieldById methodPtrField methodPtr

        let updatedHeap =
            { state.ManagedHeap with
                NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add constructing updatedObj
            }

        { state with
            ManagedHeap = updatedHeap
        }

    /// Returns the type handle and an allocated System.RuntimeType.
    let getOrAllocateType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (defn : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, runtimeType =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeType.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            TypeHandleRegistry.getOrAllocate
                state.ConcreteTypes
                baseClassTypes
                state
                (fun fields state -> allocateManagedObject runtimeType fields state)
                defn
                state.TypeHandles

        let state =
            { state with
                TypeHandles = reg
            }

        result, state

    /// Returns a System.RuntimeFieldHandle.
    let getOrAllocateField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssy : AssemblyName)
        (fieldHandle : FieldDefinitionHandle)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let field = state.LoadedAssembly(declaringAssy).Value.Fields.[fieldHandle]

        // For LdToken, we need to convert GenericParamFromMetadata to TypeDefn
        // When we don't have generic context, we use the generic type parameters directly
        let declaringTypeWithGenerics =
            field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringType, state =
            concretizeFieldDeclaringType loggerFactory baseClassTypes declaringTypeWithGenerics state

        let state, runtimeFieldInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeFieldInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            FieldHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> allocateManagedObject runtimeFieldInfoStub fields state)
                declaringAssy
                declaringType
                fieldHandle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = reg
            }

        result, state

    /// Returns a System.RuntimeMethodHandle.
    let getOrAllocateMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, runtimeMethodInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeMethodInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            MethodHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> allocateManagedObject runtimeMethodInfoStub fields state)
                method
                state.MethodHandles

        let state =
            { state with
                MethodHandles = reg
            }

        result, state

    let evalStackValueToObjectRef (state : IlMachineState) (value : EvalStackValue) : ManagedHeapAddress option =
        match value with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | EvalStackValue.ManagedPointer src ->
            match readManagedByref state src with
            | CliType.ObjectRef addr -> addr
            | other -> failwith $"expected object reference, got {other}"
        | other -> failwith $"expected object reference, got {other}"

    let lookupTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeDef : TypeDefinitionHandle)
        : IlMachineState * TypeDefn
        =
        let defn = activeAssy.TypeDefs.[typeDef]
        state, DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies defn

    let lookupTypeRef
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        typeGenerics
        (ref : TypeReferenceHandle)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        let ref = activeAssy.TypeRefs.[ref]

        // Convert ConcreteTypeHandles back to TypeDefn for metadata operations
        let typeGenerics =
            typeGenerics
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, assy, resolved =
            resolveTypeFromRef loggerFactory activeAssy ref typeGenerics state

        state, DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved, assy

    let private ensureAssemblyLoadedByName
        (loggerFactory : ILoggerFactory)
        (state : IlMachineState)
        (referencedInAssembly : DumpedAssembly)
        (assemblyName : AssemblyName)
        : IlMachineState * DumpedAssembly
        =
        match state.LoadedAssembly assemblyName with
        | Some loadedAssembly -> state, loadedAssembly
        | None ->
            let handle =
                referencedInAssembly.AssemblyReferences
                |> Seq.tryPick (fun (KeyValue (assemblyRefHandle, assemblyRef)) ->
                    if assemblyRef.Name.FullName = assemblyName.FullName then
                        Some assemblyRefHandle
                    else
                        None
                )
                |> Option.defaultWith (fun () ->
                    failwithf
                        "Assembly %s needs base assembly %s, but no AssemblyReferenceHandle was found"
                        referencedInAssembly.Name.FullName
                        assemblyName.FullName
                )

            let state, loadedAssembly, _ =
                loadAssembly loggerFactory referencedInAssembly handle state

            state, loadedAssembly

    /// Resolve a BaseTypeInfo to the assembly and TypeDefn of the base type.
    let resolveBaseTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo)
        : IlMachineState * DumpedAssembly * TypeDefn
        =
        match baseTypeInfo with
        | BaseTypeInfo.TypeDef handle ->
            let typeInfo = currentAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, currentAssembly, typeDefn
        | BaseTypeInfo.TypeRef handle ->
            let state, assy, resolved =
                resolveTypeFromRef
                    loggerFactory
                    currentAssembly
                    (currentAssembly.TypeRefs.[handle])
                    ImmutableArray.Empty
                    state

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved

            state, assy, typeDefn
        | BaseTypeInfo.ForeignAssemblyType (assemblyName, handle) ->
            let state, foreignAssembly =
                ensureAssemblyLoadedByName loggerFactory state currentAssembly assemblyName

            let typeInfo = foreignAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, foreignAssembly, typeDefn
        | BaseTypeInfo.TypeSpec handle ->
            let signature = currentAssembly.TypeSpecs.[handle].Signature
            state, currentAssembly, signature

    /// Given a ConcreteTypeHandle, resolve and return its base type as a ConcreteTypeHandle.
    /// Returns None for types without a base type (System.Object).
    let resolveBaseConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * ConcreteTypeHandle option
        =
        match concreteType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Structural array handles keep their own runtime identity; their base type is System.Array.
            let state, arrayHandle =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Array
                |> concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            state, Some arrayHandle
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ ->

            match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | Some ct ->
                let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
                let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

                match typeInfo.BaseType with
                | None -> state, None
                | Some baseTypeInfo ->
                    let state, baseAssy, baseTypeDefn =
                        resolveBaseTypeInfo loggerFactory baseClassTypes state assy baseTypeInfo

                    let state, baseHandle =
                        concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssy.Name
                            ct.Generics
                            ImmutableArray.Empty
                            baseTypeDefn

                    state, Some baseHandle

    /// Collect ALL instance fields from the entire type hierarchy for a given ConcreteTypeHandle,
    /// walking from base to derived (base class fields appear first in the returned list).
    let rec collectAllInstanceFields
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * CliField list
        =
        let ct =
            AllConcreteTypes.lookup concreteType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"collectAllInstanceFields: ConcreteTypeHandle %O{concreteType} not found in AllConcreteTypes"
            )

        let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
        let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

        // Get this type's own instance fields
        let state, ownFields =
            let instanceFields =
                typeInfo.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

            ((state, []), instanceFields)
            ||> List.fold (fun (state, fields) field ->
                let state, zero, fieldTypeHandle =
                    cliTypeZeroOf
                        loggerFactory
                        baseClassTypes
                        assy
                        field.Signature
                        ct.Generics
                        ImmutableArray.Empty
                        state

                let cliField : CliField =
                    {
                        Id = FieldId.metadata concreteType field.Handle field.Name
                        Name = field.Name
                        Contents = zero
                        Offset = field.Offset
                        Type = fieldTypeHandle
                    }

                state, cliField :: fields
            )

        let ownFields = List.rev ownFields

        // Recurse into base type
        let state, baseHandle =
            resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

        match baseHandle with
        | None -> state, ownFields
        | Some parentHandle ->
            let state, baseFields =
                collectAllInstanceFields loggerFactory baseClassTypes state parentHandle

            state, baseFields @ ownFields

    /// Allocate a new System.String managed object on the heap with the given contents.
    /// Does NOT intern the string: every call returns a fresh heap object.  The Ldstr opcode
    /// wraps this with its own interning cache (see UnaryStringTokenIlOp); runtime-generated
    /// strings (stack traces, type names, etc.) call this directly.
    let allocateManagedString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : string)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        // String type is:
        // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/libraries/System.Private.CoreLib/src/System/String.cs#L26
        let stringInstanceFields =
            baseClassTypes.String.Fields
            |> List.choose (fun field ->
                if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                    Some (field.Name, field.Signature)
                else
                    None
            )
            |> List.sortBy fst

        if
            stringInstanceFields
            <> [
                ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
            ]
        then
            failwith $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

        let dataAddr, state = allocateStringData contents.Length state
        let state = setStringData dataAddr contents state

        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.String
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let fields =
            let firstCharField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.String "_firstChar"

            let stringLengthField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.String "_stringLength"

            [
                FieldIdentity.cliField
                    stringType
                    firstCharField
                    (CliType.ofChar state.ManagedHeap.StringArrayData.[dataAddr])
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Char)
                FieldIdentity.cliField
                    stringType
                    stringLengthField
                    (CliType.Numeric (CliNumericType.Int32 contents.Length))
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32)
            ]
            |> CliValueType.OfFields baseClassTypes state.ConcreteTypes stringType Layout.Default

        let addr, state = allocateManagedObject stringType fields state

        let state =
            { state with
                ManagedHeap =
                    state.ManagedHeap
                    |> ManagedHeap.recordStringContents addr contents
                    |> ManagedHeap.recordStringDataOffset addr dataAddr
            }

        addr, state

    let private concreteTypeFullName (state : IlMachineState) (ty : ConcreteType<ConcreteTypeHandle>) : string =
        match state.LoadedAssembly ty.Assembly with
        | Some assy -> Assembly.fullName assy ty.Identity
        | None when String.IsNullOrEmpty ty.Namespace -> ty.Name
        | None -> $"{ty.Namespace}.{ty.Name}"

    let private renderExceptionStackFrame
        (state : IlMachineState)
        (frame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : string
        =
        let typeName = concreteTypeFullName state frame.Method.DeclaringType
        $"   at %s{typeName}.%s{frame.Method.Name}()"

    let private renderExceptionStackTrace
        (state : IlMachineState)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        : string
        =
        stackTrace
        |> List.map (renderExceptionStackFrame state)
        |> String.concat Environment.NewLine

    /// Project PawPrint's structured exception trace into the managed `System.Exception`
    /// object so guest code observing `Exception.StackTrace` sees a non-null trace string.
    let setExceptionStackTraceString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        (state : IlMachineState)
        : IlMachineState
        =
        match stackTrace with
        | [] -> state
        | _ :: _ ->
            // Low-level dispatch tests sometimes use synthetic exception addresses in skeletal states.
            // Full guest execution has both pieces, so only then can we project into the managed object.
            match
                state.ManagedHeap.NonArrayObjects |> Map.tryFind exceptionAddr,
                AllConcreteTypes.findExistingNonGenericConcreteType
                    state.ConcreteTypes
                    baseClassTypes.Exception.Identity
            with
            | Some heapObj, Some exceptionHandle ->
                let trace = renderExceptionStackTrace state stackTrace

                let traceAddr, state =
                    allocateManagedString loggerFactory baseClassTypes trace state

                let stackTraceStringField =
                    FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_stackTraceString"
                    |> FieldIdentity.fieldId exceptionHandle

                let heapObj =
                    heapObj
                    |> AllocatedNonArrayObject.SetFieldById stackTraceStringField (CliType.ObjectRef (Some traceAddr))

                { state with
                    ManagedHeap = ManagedHeap.set exceptionAddr heapObj state.ManagedHeap
                }
            | None, _
            | _, None -> state

    /// Return the managed `System.Threading.Thread` heap object corresponding to the given guest
    /// thread, allocating it on first request and caching the address thereafter so that repeated
    /// calls yield reference-identical objects. Populates only the fields whose zero-initialised
    /// defaults would observably diverge from the CLR: `_managedThreadId` (ThreadId 0 is
    /// hardcoded to managed ID 1; others consume `NextManagedThreadId`), `_priority` (CLR
    /// exposes `ThreadPriority.Normal = 2`, not zero-valued `Lowest`), and
    /// `_DONT_USE_InternalThread` (non-zero sentinel so `GetNativeHandle()` doesn't throw).
    /// The Thread constructor is NOT run; other fields remain zero-initialised.
    let getOrAllocateManagedThreadObject
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (threadId : ThreadId)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr -> addr, state
        | None ->

        let threadTypeInfo =
            baseClassTypes.Corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) ->
                if v.Namespace = "System.Threading" && v.Name = "Thread" then
                    Some v
                else
                    None
            )
            |> Seq.exactlyOne

        let state, threadTypeHandle =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies threadTypeInfo
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state threadTypeHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes threadTypeHandle threadTypeInfo.Layout allFields

        let addr, state = allocateManagedObject threadTypeHandle fields state

        // The main thread (ThreadId 0) always gets managed ID 1 — the CLR assigns it at
        // startup, before user code runs.  Other scheduler-created threads consume the shared
        // counter so IDs remain globally unique.
        let managedThreadId, state =
            let (ThreadId idx) = threadId

            if idx = 0 then
                1, state
            else
                let id = state.NextManagedThreadId

                id,
                { state with
                    NextManagedThreadId = id + 1
                }

        let threadPriorityNormal = 2
        let (ManagedHeapAddress addrInt) = addr

        let managedThreadIdField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_managedThreadId"
            |> FieldIdentity.fieldId threadTypeHandle

        let priorityField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_priority"
            |> FieldIdentity.fieldId threadTypeHandle

        let internalThreadField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_DONT_USE_InternalThread"
            |> FieldIdentity.fieldId threadTypeHandle

        let updatedObj =
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.SetFieldById
                managedThreadIdField
                (CliType.Numeric (CliNumericType.Int32 managedThreadId))
            |> AllocatedNonArrayObject.SetFieldById
                priorityField
                (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
            |> AllocatedNonArrayObject.SetFieldById
                internalThreadField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                ManagedThreadObjects = state.ManagedThreadObjects |> Map.add threadId addr
            }

        addr, state

    /// Return the CLR-visible managed thread ID for the current guest thread.
    /// This is distinct from PawPrint's scheduler ThreadId.
    let getCurrentManagedThreadId (threadId : ThreadId) (state : IlMachineState) : int =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr ->
            let threadObj = ManagedHeap.get addr state.ManagedHeap

            let threadConcreteType =
                AllConcreteTypes.lookup threadObj.ConcreteType state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Environment.CurrentManagedThreadId: Thread object has unknown concrete type %O{threadObj.ConcreteType}"
                )

            let threadAssembly =
                state._LoadedAssemblies.[threadConcreteType.Identity.AssemblyFullName]

            let threadTypeInfo =
                threadAssembly.TypeDefs.[threadConcreteType.Identity.TypeDefinition.Get]

            let managedThreadIdField =
                FieldIdentity.requiredOwnInstanceField threadTypeInfo "_managedThreadId"
                |> FieldIdentity.fieldId threadObj.ConcreteType

            match AllocatedNonArrayObject.DereferenceFieldById managedThreadIdField threadObj with
            | CliType.Numeric (CliNumericType.Int32 id) -> id
            | other ->
                failwith
                    $"Environment.CurrentManagedThreadId: Thread object for ThreadId %O{threadId} has non-int32 _managedThreadId field %O{other}"
        | None ->
            match threadId with
            | ThreadId.ThreadId 0 -> 1
            | ThreadId.ThreadId _ ->
                failwith
                    $"Environment.CurrentManagedThreadId: non-main ThreadId %O{threadId} has no managed Thread object"

    /// Synthesize a TypeInitializationException wrapping the given inner exception object.
    /// Allocates the exception on the heap with zero-initialized fields (constructor is NOT run).
    /// Sets the _innerException, _typeName, and _HResult fields on the TIE to match what the
    /// TypeInitializationException(string, Exception) ctor would have done.
    /// Returns the heap address, the ConcreteTypeHandle, and the updated state.
    let synthesizeTypeInitializationException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeFullName : string)
        (innerExceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let tieTypeInfo = baseClassTypes.TypeInitializationException

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies tieTypeInfo

        let state, tieHandle =
            concretizeType
                loggerFactory
                baseClassTypes
                state
                tieTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (tieTypeInfo.Identity, stk))

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state tieHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes tieHandle tieTypeInfo.Layout allFields

        let addr, state = allocateManagedObject tieHandle fields state

        let typeNameAddr, state =
            allocateManagedString loggerFactory baseClassTypes typeFullName state

        // Set _innerException, _typeName and _HResult on the allocated TIE, matching what the
        // TypeInitializationException(string, Exception) ctor would have done.
        // See CLR's EEException::CreateThrowable:
        // https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let exceptionHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Exception

        let innerExceptionField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_innerException"
            |> FieldIdentity.fieldId exceptionHandle

        let typeNameField =
            FieldIdentity.requiredOwnInstanceField tieTypeInfo "_typeName"
            |> FieldIdentity.fieldId tieHandle

        let hresultField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_HResult"
            |> FieldIdentity.fieldId exceptionHandle

        let heapObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById innerExceptionField (CliType.ObjectRef (Some innerExceptionAddr))
            |> AllocatedNonArrayObject.SetFieldById typeNameField (CliType.ObjectRef (Some typeNameAddr))
            |> AllocatedNonArrayObject.SetFieldById
                hresultField
                (CliType.Numeric (CliNumericType.Int32 (ExceptionHResults.lookup "System.TypeInitializationException")))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
            }

        addr, tieHandle, state

    /// Resolve a MetadataToken (TypeDefinition, TypeReference, or TypeSpecification) to a TypeDefn,
    /// together with the assembly the type was resolved in.
    let resolveTypeMetadataToken
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (token : MetadataToken)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        match token with
        | MetadataToken.TypeDefinition h ->
            let state, ty = lookupTypeDefn baseClassTypes state activeAssy h
            state, ty, activeAssy
        | MetadataToken.TypeReference ref ->
            lookupTypeRef loggerFactory baseClassTypes state activeAssy typeGenerics ref
        | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
        | m -> failwith $"unexpected type metadata token {m}"

    /// Get the metadata row directly represented by this concrete handle.
    /// Structural arrays, byrefs, and pointers have no direct TypeDef row; callers that are walking
    /// inheritance should ask for their base type explicitly.
    let tryGetConcreteTypeInfo
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>) option
        =
        match concreteType with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | Some concreteType ->
                let assembly = state._LoadedAssemblies.[concreteType.Identity.AssemblyFullName]

                Some (concreteType, assembly.TypeDefs.[concreteType.Identity.TypeDefinition.Get])
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> None

    let requiredOwnInstanceFieldId
        (state : IlMachineState)
        (declaringType : ConcreteTypeHandle)
        (fieldName : string)
        : FieldId
        =
        match tryGetConcreteTypeInfo state declaringType with
        | Some (_, typeInfo) ->
            FieldIdentity.requiredOwnInstanceField typeInfo fieldName
            |> FieldIdentity.fieldId declaringType
        | None ->
            failwith
                $"requiredOwnInstanceFieldId: %O{declaringType} has no TypeDef row; cannot resolve field '%s{fieldName}'"

    /// Check whether the concrete type `objType` is assignable to `targetType`.
    /// Walks the base type chain and checks implemented interfaces at each level.
    /// Returns true if objType = targetType, or targetType is a base class of objType,
    /// or targetType is an interface implemented by objType or any of its base classes.
    let rec isConcreteTypeAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if objType = targetType then
            state, true
        else

        let isReferenceTypeHandle (state : IlMachineState) (handle : ConcreteTypeHandle) : bool =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> true
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _ -> false
            | ConcreteTypeHandle.Concrete _ ->
                match tryGetConcreteTypeInfo state handle with
                | Some (_, typeInfo) -> DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
                | None -> failwith $"isReferenceTypeHandle: concrete type handle %O{handle} has no TypeDef row"

        let arrayShape (handle : ConcreteTypeHandle) : (ConcreteTypeHandle * int option) option =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero element -> Some (element, None)
            | ConcreteTypeHandle.Array (element, rank) -> Some (element, Some rank)
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _ -> None

        let rec checkInterfaces (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match tryGetConcreteTypeInfo state current with
            | None ->
                // This node has no metadata-declared interfaces. The caller decides whether to walk its base.
                state, false
            | Some (ct, typeInfo) ->
                let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]

                ((state, false), typeInfo.ImplementedInterfaces)
                ||> Seq.fold (fun (state, found) impl ->
                    if found then
                        state, true
                    else
                        let implAssy =
                            match state.LoadedAssembly impl.RelativeToAssembly with
                            | Some a -> a
                            | None ->
                                // Assembly not yet loaded; use the assembly we already have since
                                // RelativeToAssembly is set to the assembly containing the type definition.
                                assy

                        let state, implTypeDefn, implResolvedAssy =
                            resolveTypeMetadataToken
                                loggerFactory
                                baseClassTypes
                                state
                                implAssy
                                ct.Generics
                                impl.InterfaceHandle

                        let state, implHandle =
                            concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                implResolvedAssy.Name
                                ct.Generics
                                ImmutableArray.Empty
                                implTypeDefn

                        // Check exact match, then recurse into the interface's own parent interfaces.
                        walk state implHandle
                )

        and walkBase (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match current with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _ -> state, false
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                let state, baseType =
                    resolveBaseConcreteType loggerFactory baseClassTypes state current

                match baseType with
                | None ->
                    // Every reference type (including interfaces) is assignable to System.Object.
                    match targetType with
                    | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, true
                    | _ -> state, false
                | Some parent -> walk state parent

        and walk (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            if current = targetType then
                state, true
            else

            match tryGetConcreteTypeInfo state current with
            | None -> walkBase state current
            | Some (currentCt, _) ->
                // If two types share the same definition but differ in generics, check whether
                // variance could apply. Classes are invariant so the answer is definitively false.
                // Interfaces and delegates can have variance, so we must crash rather than guess.
                let sameDefnDifferentGenerics =
                    match AllConcreteTypes.lookup targetType state.ConcreteTypes with
                    | Some targetCt when
                        currentCt.Identity = targetCt.Identity
                        && currentCt.Generics <> targetCt.Generics
                        ->
                        Some targetCt
                    | _ -> None

                match sameDefnDifferentGenerics with
                | Some targetCt ->
                    let targetAssy = state._LoadedAssemblies.[targetCt.Identity.AssemblyFullName]
                    let targetTypeInfo = targetAssy.TypeDefs.[targetCt.Identity.TypeDefinition.Get]

                    let hasVariantGenericParams =
                        targetTypeInfo.Generics
                        |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

                    if hasVariantGenericParams then
                        failwith $"TODO: generic variance check needed: is %O{currentCt} assignable to %O{targetCt}?"
                    else
                        // All generic parameters are invariant; same definition + different generics = not assignable.
                        state, false
                | None ->
                    let state, interfaceMatch = checkInterfaces state current

                    if interfaceMatch then
                        state, true
                    else
                        walkBase state current

        let checkArraySpecificRules
            (state : IlMachineState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : IlMachineState * bool option
            =
            match arrayShape objType, arrayShape targetType with
            | Some (objElement, objShape), Some (targetElement, targetShape) ->
                if objShape <> targetShape then
                    state, Some false
                elif objElement = targetElement then
                    state, Some true
                elif
                    isReferenceTypeHandle state objElement
                    && isReferenceTypeHandle state targetElement
                then
                    let state, elementAssignable =
                        isConcreteTypeAssignableTo loggerFactory baseClassTypes state objElement targetElement

                    state, Some elementAssignable
                else
                    // TODO: ECMA-335 permits some value-type array assignments when
                    // the element types have equivalent underlying primitive types
                    // (for example int[] <-> uint[]). Model that rule explicitly
                    // before broadening this branch.
                    state, Some false
            | Some _, None -> state, None
            | None, _ -> failwith $"checkArraySpecificRules called with non-array source %O{objType}"

        match objType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            let state, assignable = walk state objType

            if assignable then
                state, assignable
            else
                match checkArraySpecificRules state objType targetType with
                | state, Some assignable -> state, assignable
                | state, None ->
                    let targetTypeInfo = tryGetConcreteTypeInfo state targetType

                    let targetNeedsArraySpecificRules =
                        match targetType with
                        | ConcreteTypeHandle.OneDimArrayZero _
                        | ConcreteTypeHandle.Array _ -> true
                        | ConcreteTypeHandle.Concrete _
                        | ConcreteTypeHandle.Byref _
                        | ConcreteTypeHandle.Pointer _ ->
                            match targetTypeInfo with
                            | Some (targetCt, targetTypeInfo) ->
                                targetTypeInfo.IsInterface && not targetCt.Generics.IsEmpty
                            | None -> false

                    if targetNeedsArraySpecificRules then
                        failwith $"TODO: array assignability check from %O{objType} to %O{targetType}"
                    else
                        state, false
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> walk state objType
