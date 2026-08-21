namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// <summary>
/// The <c>StackTrace_GetStackFramesInternal</c> QCall's writes into the <c>StackFrameHelper</c> it
/// is handed.
/// </summary>
/// <remarks>
/// These are the only tests that can see those writes. A guest cannot: `CaptureStackTrace` builds a
/// `StackFrame` per captured frame and that constructor calls `GetMethodBase` unconditionally, which
/// needs the unimplemented `IsTypicalMethodDefinition` — so every guest-visible route to a non-empty
/// capture is blocked (`sourcesPure/StackTraceCurrentThreadFrames.cs` is parked on exactly that).
/// The one guest-reachable branch is the empty capture, covered end to end by
/// `sourcesPure/StackTraceFromUnthrownException.cs`; but `StackFrameHelper`'s constructor already
/// sets `iFrameCount` to 0, so that guest would pass against a handler that wrote nothing at all.
/// Hence the empty case is asserted here too, as a claim about what the handler did rather than
/// about what the guest saw.
/// </remarks>
[<TestFixture>]
module TestNativeStackTrace =

    let private guestSource =
        """
public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private prepareGuest (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : Program.PreparedProgram =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        let image = Roslyn.compile [ guestSource ]

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "StackTraceHandlerGuest.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected guest to be ready before Main, but got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () ->
            failwith $"type %s{namespaceName}.%s{typeName} not found in %s{assembly.Name.Name}"
        )

    /// This QCall's own method, concretized, so a frame or a `NativeCallContext` sees the same
    /// `ExecutingMethod` signature the interpreter would have handed the handler.
    let private qCallMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            requiredTopLevelType baseClassTypes.Corelib "System.Diagnostics" "StackTrace"

        let rawMethod =
            declaringType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import ->
                    import.ModuleName = "QCall"
                    && import.EntryPointName = "StackTrace_GetStackFramesInternal"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point StackTrace_GetStackFramesInternal not found on StackTrace"
                | methods -> failwith $"StackTrace_GetStackFramesInternal was ambiguous: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        state, method

    /// Concretize a class of corelib by name. The prepared guest has only concretized what its own
    /// startup touched, so nothing here may assume a handle already exists.
    let private concretizeCorelibClass
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (namespaceName : string)
        (typeName : string)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeInfo = requiredTopLevelType baseClassTypes.Corelib namespaceName typeName

        IlMachineState.concretizeType
            loggerFactory
            baseClassTypes
            state
            baseClassTypes.Corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.Class))

    /// Allocate an instance of a corelib class with every field zeroed, as `newobj` does before
    /// running a constructor. No constructor is run: these tests are about what the QCall writes,
    /// and `StackFrameHelper`'s constructor only nulls fields that are already null.
    let private allocateZeroed
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (namespaceName : string)
        (typeName : string)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, handle =
            concretizeCorelibClass loggerFactory baseClassTypes namespaceName typeName state

        let state, fields =
            IlMachineState.buildInstanceStorage loggerFactory baseClassTypes state handle

        IlMachineState.allocateManagedObject handle fields state

    /// A freshly-allocated `StackFrameHelper` with `iFrameCount` set to `framesRequested`, which is
    /// the field's meaning on entry (`NumFramesRequested`; 0 asks for every frame). Every managed
    /// caller passes a helper straight from its constructor, so 0 is the only value a guest
    /// produces — the non-zero case is reachable only from here.
    let private allocateHelper
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (framesRequested : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let addr, state =
            allocateZeroed loggerFactory baseClassTypes "System.Diagnostics" "StackFrameHelper" state

        addr,
        IlMachineState.setOwnInstanceField
            addr
            "iFrameCount"
            (CliType.Numeric (CliNumericType.Int32 framesRequested))
            state

    /// Push `count` further frames onto `thread`'s stack, each the callee of the one before it.
    ///
    /// A freshly-prepared guest has a single frame, which cannot distinguish honouring
    /// `NumFramesRequested` from ignoring it, nor a walk that reports callers from one that reports
    /// only the active frame. Each pushed frame records a distinct call site so the offsets stay
    /// tellable apart.
    let private deepenStack (thread : ThreadId) (count : int) (state : IlMachineState) : IlMachineState =
        (state, [ 1..count ])
        ||> List.fold (fun state i ->
            let threadState = state.ThreadState.[thread]

            let returnState : MethodReturnState =
                {
                    JumpTo = threadState.ActiveMethodState
                    WasInitialisingType = None
                    Constructing = ConstructionState.NotConstructing
                    CallSiteIlOpIndex = i
                    ReturnValueDisposition = ReturnValueDisposition.PushToCaller
                    WrapExceptionInTargetInvocation = false
                }

            let callee =
                { threadState.MethodState with
                    ReturnState = Some returnState
                }

            let calleeFrameId, threadState = ThreadState.appendFrame callee threadState
            let threadState = ThreadState.setActiveFrame calleeFrameId threadState

            { state with
                ThreadState = state.ThreadState |> Map.add thread threadState
            }
        )

    /// Mint a real `Reflection.Emit` method and push a frame running it.
    ///
    /// A dynamic method has no MethodDef row, so it is `SynthesisedMethod.DynamicMethod` — but it
    /// is nameable, its `DynamicMethodHandle` carrying the registry id minted here. Returns that
    /// id, which is what `rgMethodHandle` must report for the frame.
    let private pushDynamicMethodFrame
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * int64
        =
        // `static void ()`: default calling convention, no parameters, void return.
        let signature = ImmutableArray.Create (0x00uy, 0x00uy, 0x01uy)

        let body =
            MintedDynamicMethodBody.make [ IlOp.Nullary NullaryIlOp.Ret, 0 ] None ImmutableArray.Empty

        // `mintDynamicMethod` allocates the `RuntimeMethodInfoStub` through the caller's allocator,
        // so the machine state threads through as the allocation state.
        let stubAddr, registry, state =
            MethodHandleRegistry.mintDynamicMethod
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state ->
                    IlMachineState.allocateManagedObject
                        (AllConcreteTypes.getRequiredNonGenericHandle
                            state.ConcreteTypes
                            baseClassTypes.RuntimeMethodInfoStub)
                        fields
                        state
                )
                "Thrower"
                signature
                baseClassTypes.Corelib.DefinitionFullName
                None
                body
                state.MethodHandles

        let state =
            { state with
                MethodHandles = registry
            }

        // The minted id is only reported through the `RuntimeMethodInfoStub` the registry
        // allocated, so read it back from there rather than guessing at the registry's counter.
        let handle =
            let stub = ManagedHeap.get stubAddr state.ManagedHeap

            AllocatedNonArrayObject.DereferenceField "m_value" stub
            |> NativeCall.methodHandleIdOfRuntimeMethodHandleInternal "pushDynamicMethodFrame"
            |> Option.defaultWith (fun () -> failwith "the minted stub carried no method-registry id")
            |> DynamicMethodHandle.ofRegistryId

        let state, method =
            ExecutionConcretization.concretizeDynamicMethod
                loggerFactory
                baseClassTypes
                "pushDynamicMethodFrame"
                handle
                false
                state

        // The point of the frame.
        match method.SynthesisedKind with
        | Some (SynthesisedMethod.DynamicMethod _) -> ()
        | other -> failwith $"expected a dynamic method, got %O{other}"

        let threadState = state.ThreadState.[thread]

        let returnState : MethodReturnState =
            {
                JumpTo = threadState.ActiveMethodState
                WasInitialisingType = None
                Constructing = ConstructionState.NotConstructing
                CallSiteIlOpIndex = 0
                ReturnValueDisposition = ReturnValueDisposition.PushToCaller
                WrapExceptionInTargetInvocation = false
            }

        let frame =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.Corelib
                    method
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (Some returnState)
            with
            | Ok frame -> frame
            | Error missing ->
                failwith $"unexpected missing assembly references building a dynamic-method frame: %O{missing}"

        let frameId, threadState = ThreadState.appendFrame frame threadState
        let threadState = ThreadState.setActiveFrame frameId threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        },
        handle.GetRegistryId ()

    /// Push a frame whose method has no IL body — this very QCall, which is `MethodBody.PInvoke`.
    ///
    /// Not a contrived shape: the innermost frame of every real current-thread capture is exactly
    /// this, the P/Invoke stub the guest is standing in when the QCall runs. A `MethodState` for such
    /// a method carries the synthetic program counter 0, which is what must not be reported as a
    /// genuine IL offset.
    let private pushNonIlFrame
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let state, method = qCallMethod loggerFactory baseClassTypes state

        // The point of the frame.
        match method.Body with
        | MethodBody.Il _ -> failwith "expected the QCall's own method to have no IL body"
        | _ -> ()

        let threadState = state.ThreadState.[thread]

        let returnState : MethodReturnState =
            {
                JumpTo = threadState.ActiveMethodState
                WasInitialisingType = None
                Constructing = ConstructionState.NotConstructing
                CallSiteIlOpIndex = 0
                ReturnValueDisposition = ReturnValueDisposition.PushToCaller
                WrapExceptionInTargetInvocation = false
            }

        let frame =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.Corelib
                    method
                    ImmutableArray.Empty
                    // Placeholders: `MethodState.Empty` checks only the count, and this frame is
                    // never executed — it exists to be walked.
                    (ImmutableArray.Create (CliType.ObjectRef None, CliType.ObjectRef None, CliType.ObjectRef None))
                    (Some returnState)
            with
            | Ok frame -> frame
            | Error missing -> failwith $"unexpected missing assembly references building a non-IL frame: %O{missing}"

        // A runtime-provided method's frame really does sit at the synthetic 0.
        frame.IlOpIndex |> shouldEqual 0

        let frameId, threadState = ThreadState.appendFrame frame threadState
        let threadState = ThreadState.setActiveFrame frameId threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    /// Push a frame running a *generic* method instantiated at `int` — `Array.Empty<int>()`.
    ///
    /// Without one, nothing on a startup stack has method-generic arguments, and the claim that a
    /// frame's handle names the typical definition rather than the instantiation is untestable:
    /// stripping and binding give the same answer for every non-generic method.
    let private pushGenericFrame
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let arrayEmpty =
            baseClassTypes.Array.Methods
            |> List.filter (fun method -> method.Name = "Empty" && method.Generics.Length = 1)
            |> function
                | [ method ] -> method
                | [] -> failwith "System.Array::Empty<T> not found"
                | methods -> failwith $"System.Array::Empty<T> was ambiguous: %d{methods.Length} matches"

        let int32Defn =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int32

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                arrayEmpty
                (Some (ImmutableArray.Create int32Defn))
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        // The point of the frame: it really is an instantiation, not a definition.
        method.Generics.Length |> shouldEqual 1

        let threadState = state.ThreadState.[thread]

        let returnState : MethodReturnState =
            {
                JumpTo = threadState.ActiveMethodState
                WasInitialisingType = None
                Constructing = ConstructionState.NotConstructing
                CallSiteIlOpIndex = 0
                ReturnValueDisposition = ReturnValueDisposition.PushToCaller
                WrapExceptionInTargetInvocation = false
            }

        let frame =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.Corelib
                    method
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (Some returnState)
            with
            | Ok frame -> frame
            | Error missing -> failwith $"unexpected missing assembly references building a generic frame: %O{missing}"

        let frameId, threadState = ThreadState.appendFrame frame threadState
        let threadState = ThreadState.setActiveFrame frameId threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    /// An `ObjectHandleOnStack` whose `_ptr` addresses a one-element `object[]` seeded with
    /// `contents`, standing in for the caller's local.
    let private objectHandleOnStack
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : ManagedHeapAddress option)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, objectHandle =
            concretizeCorelibClass loggerFactory baseClassTypes "System" "Object" state

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                1
                state

        let state =
            IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef contents) 0 state

        let target = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let handleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (handleType.Identity, SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
            |> CliType.ValueType,
            state
        | other -> failwith $"ObjectHandleOnStack zero was not a value type: %O{other}"

    /// Runs the QCall against the entry thread with the given helper and exception arguments.
    ///
    /// Deliberately through `NativeQCall.tryExecute` rather than straight at
    /// `NativeStackTrace.tryExecuteQCall`: that is the interpreter's own path, and it derives the
    /// entry point from the method's import metadata, so this also fails if the handler exists but
    /// was never registered in the dispatch table.
    let private invoke
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (needFileInfo : bool)
        (helperAddr : ManagedHeapAddress)
        (exceptionAddr : ManagedHeapAddress option)
        (state : IlMachineState)
        : IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let declaringType =
            requiredTopLevelType baseClassTypes.Corelib "System.Diagnostics" "StackTrace"

        let state, method = qCallMethod loggerFactory baseClassTypes state

        let helperArg, state =
            objectHandleOnStack loggerFactory baseClassTypes (Some helperAddr) state

        let exceptionArg, state =
            objectHandleOnStack loggerFactory baseClassTypes exceptionAddr state

        let arguments =
            [
                helperArg
                CliType.Numeric (CliNumericType.Int32 (if needFileInfo then 1 else 0))
                exceptionArg
            ]

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = method
                Arguments = ImmutableArray.CreateRange arguments
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = declaringType
            }

        match NativeQCall.tryExecute ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> state
        | Some result -> failwith $"unexpected StackTrace_GetStackFramesInternal result: %O{result}"
        | None -> failwith "StackTrace_GetStackFramesInternal QCall did not match, or is not registered in NativeQCall"

    let private readField (state : IlMachineState) (addr : ManagedHeapAddress) (name : string) : CliType =
        let obj = ManagedHeap.get addr state.ManagedHeap
        let field = IlMachineState.requiredOwnInstanceFieldId state obj.ConcreteType name
        AllocatedNonArrayObject.DereferenceFieldById field obj

    let private readInt32Field (state : IlMachineState) (addr : ManagedHeapAddress) (name : string) : int =
        match readField state addr name |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.Numeric (CliNumericType.Int32 value) -> value
        | other -> failwith $"expected Int32 in %s{name}, got %O{other}"

    /// The array a reference-shaped field points at, or `None` if the field is still null.
    let private readArrayField
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (name : string)
        : ManagedHeapAddress option
        =
        match readField state addr name with
        | CliType.ObjectRef addr -> addr
        | other -> failwith $"expected an array reference in %s{name}, got %O{other}"

    let private int32Elements (state : IlMachineState) (arrayAddr : ManagedHeapAddress) : int list =
        let shape = ManagedHeap.getArrayShape arrayAddr state.ManagedHeap

        [ 0 .. shape.Length - 1 ]
        |> List.map (fun i ->
            match
                IlMachineState.getArrayValue arrayAddr i state
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.Numeric (CliNumericType.Int32 value) -> value
            | other -> failwith $"expected Int32 array element, got %O{other}"
        )

    let private methodHandleElements (state : IlMachineState) (arrayAddr : ManagedHeapAddress) : int64 list =
        let shape = ManagedHeap.getArrayShape arrayAddr state.ManagedHeap

        [ 0 .. shape.Length - 1 ]
        |> List.map (fun i ->
            IlMachineState.getArrayValue arrayAddr i state
            |> NativeCall.methodHandleIdOfRuntimeMethodHandleInternal "test"
            |> Option.defaultWith (fun () -> failwith $"rgMethodHandle[%d{i}] did not carry a method-registry id")
        )

    /// The four fields the handler fills, plus the conditional foreign-frame one.
    let private filledArrayFields =
        [ "rgMethodHandle" ; "rgiOffset" ; "rgiILOffset" ; "rgiMethodToken" ]

    /// The fields the handler deliberately leaves null, because reporting every method token as 0
    /// means CoreLib never reads them.
    let private untouchedArrayFields =
        [
            "rgAssembly"
            "rgAssemblyPath"
            "rgLoadedPeAddress"
            "rgiLoadedPeSize"
            "rgiIsFileLayout"
            "rgInMemoryPdbAddress"
            "rgiInMemoryPdbSize"
            "rgFilename"
            "rgiLineNumber"
            "rgiColumnNumber"
        ]

    [<Test>]
    let ``an unthrown exception yields no frames and no arrays at all`` () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        // Allocated but never thrown, so its `_stackTrace` is null and it has no frames to report.
        let exceptionAddr, state =
            allocateZeroed loggerFactory baseClassTypes "System" "Exception" state

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 0 state

        let state =
            invoke loggerFactory prepared false helperAddr (Some exceptionAddr) state

        readInt32Field state helperAddr "iFrameCount" |> shouldEqual 0

        // CoreCLR allocates nothing in this branch, so a handler that allocated empty arrays would
        // be diverging even though no guest could currently tell.
        for field in filledArrayFields @ untouchedArrayFields do
            readArrayField state helperAddr field |> shouldEqual None

        readArrayField state helperAddr "rgiLastFrameFromForeignExceptionStackTrace"
        |> shouldEqual None

    [<Test>]
    let ``a current-thread capture reports the thread's frames and fills exactly four arrays`` () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        let state = deepenStack prepared.EntryThread 2 state

        let expected = StackFrameCapture.ofThread state.ThreadState.[prepared.EntryThread]

        // A single-frame stack would leave the per-frame arrays indistinguishable from a handler
        // that wrote only the active frame's values, so require several.
        expected |> List.length |> shouldBeGreaterThan 1

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 0 state
        let state = invoke loggerFactory prepared false helperAddr None state

        readInt32Field state helperAddr "iFrameCount" |> shouldEqual expected.Length

        for field in filledArrayFields do
            match readArrayField state helperAddr field with
            | None -> failwith $"%s{field} was left null by a non-empty capture"
            | Some arrayAddr ->
                (ManagedHeap.getArrayShape arrayAddr state.ManagedHeap).Length
                |> shouldEqual expected.Length

        for field in untouchedArrayFields do
            readArrayField state helperAddr field |> shouldEqual None

        // No frame of a live capture ends an earlier trace, so CoreCLR's conditional array is not
        // allocated and `IsLastFrameFromForeignExceptionStackTrace` answers false for every frame.
        readArrayField state helperAddr "rgiLastFrameFromForeignExceptionStackTrace"
        |> shouldEqual None

        let ilOffsets =
            readArrayField state helperAddr "rgiILOffset"
            |> Option.get
            |> int32Elements state

        // Normalised the same way the handler does: a frame with no IL body reports
        // `OFFSET_UNKNOWN`. Every frame in this fixture has IL, so this reduces to the raw offsets
        // here; `a frame with no IL body reports OFFSET_UNKNOWN` below is what pins the other arm.
        ilOffsets
        |> shouldEqual (
            expected
            |> List.map (fun frame ->
                match frame.Method.Body with
                | MethodBody.Il _ -> frame.IlOffset
                | _ -> -1
            )
        )

        // The fixture would not detect a handler that answered -1 for everything.
        ilOffsets |> List.forall (fun offset -> offset >= 0) |> shouldEqual true

        // PawPrint runs no native code, so every native offset is `StackFrame.OFFSET_UNKNOWN`.
        readArrayField state helperAddr "rgiOffset"
        |> Option.get
        |> int32Elements state
        |> shouldEqual (expected |> List.map (fun _ -> -1))

        // Zero everywhere: a non-zero token is CoreLib's signal to consult a PDB reader, and there
        // is none.
        readArrayField state helperAddr "rgiMethodToken"
        |> Option.get
        |> int32Elements state
        |> shouldEqual (expected |> List.map (fun _ -> 0))

    [<Test>]
    let ``each reported frame's method handle names that frame's method, with method generics stripped`` () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        let state = deepenStack prepared.EntryThread 2 state
        let state = pushGenericFrame loggerFactory baseClassTypes prepared.EntryThread state
        let expected = StackFrameCapture.ofThread state.ThreadState.[prepared.EntryThread]

        // The innermost frame is the generic instantiation pushed above; without it, stripping and
        // binding the method generics would be indistinguishable here.
        expected.Head.Method.Generics.Length |> shouldEqual 1

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 0 state
        let state = invoke loggerFactory prepared false helperAddr None state

        let ids =
            readArrayField state helperAddr "rgMethodHandle"
            |> Option.get
            |> methodHandleElements state

        ids |> List.length |> shouldEqual expected.Length

        let resolved =
            ids
            |> List.map (fun id ->
                MethodHandleRegistry.resolveMethodFromId id state.MethodHandles
                |> Option.defaultWith (fun () -> failwith $"method handle id %d{id} is not registered")
            )

        // A stack frame names the *typical* method definition: CoreCLR strips the method
        // instantiation and leaves the class instantiation alone (debugdebugger.cpp:449-452).
        for handle in resolved do
            match handle with
            | MethodHandle.FromMetadata identity -> identity.GetMethodGenerics () |> List.isEmpty |> shouldEqual true
            | MethodHandle.FromDynamic _ ->
                failwith "a frame of this guest's startup stack should not name a dynamic method"

        // The handle must name the frame's own method, not the innermost one repeated.
        let expectedTokens =
            expected
            |> List.map (fun frame ->
                match frame.Method.TryMetadata with
                | Some facts -> ComparableMethodDefinitionHandle.Make facts.Handle
                | None -> failwith "expected every startup frame to be metadata-backed"
            )

        let actualTokens =
            resolved
            |> List.map (fun handle ->
                match handle with
                | MethodHandle.FromMetadata identity -> identity.GetMethodDefinitionHandle ()
                | MethodHandle.FromDynamic _ -> failwith "unexpected dynamic method"
            )

        actualTokens |> shouldEqual expectedTokens

    [<Test>]
    let ``a non-zero NumFramesRequested truncates a current-thread capture`` () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        // A freshly-prepared guest has one frame, which cannot distinguish honouring the field from
        // ignoring it, so the stack is deepened first.
        let state = deepenStack prepared.EntryThread 2 state

        let available =
            StackFrameCapture.ofThread state.ThreadState.[prepared.EntryThread]
            |> List.length

        available |> shouldBeGreaterThan 1

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 1 state
        let state = invoke loggerFactory prepared false helperAddr None state

        readInt32Field state helperAddr "iFrameCount" |> shouldEqual 1

        readArrayField state helperAddr "rgiILOffset"
        |> Option.get
        |> int32Elements state
        |> List.length
        |> shouldEqual 1

    [<Test>]
    let ``NumFramesRequested does not truncate an exception-sourced capture`` () =
        // CoreCLR consults `NumFramesRequested` only in `GetStackFrames`' walk callback
        // (debugdebugger.cpp:242); `GetStackFramesFromException` ignores it, because "for
        // StackTraces from an Exception, the EE always captures all frames"
        // (StackFrameHelper.cs:78-80). An unthrown exception has no frames either way, so the
        // claim that survives here is the narrower one: asking for one frame of an exception with
        // none does not invent one.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        // Allocated but never thrown, so its `_stackTrace` is null and it has no frames to report.
        let exceptionAddr, state =
            allocateZeroed loggerFactory baseClassTypes "System" "Exception" state

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 1 state

        let state =
            invoke loggerFactory prepared false helperAddr (Some exceptionAddr) state

        readInt32Field state helperAddr "iFrameCount" |> shouldEqual 0

    [<Test>]
    let ``a frame with no IL body reports OFFSET_UNKNOWN rather than offset zero`` () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        // Two IL frames underneath a non-IL one, so the test distinguishes "normalises the non-IL
        // frame" from "answers -1 for everything".
        let state = deepenStack prepared.EntryThread 2 state
        let state = pushNonIlFrame loggerFactory baseClassTypes prepared.EntryThread state

        let expected = StackFrameCapture.ofThread state.ThreadState.[prepared.EntryThread]

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 0 state
        let state = invoke loggerFactory prepared false helperAddr None state

        let ilOffsets =
            readArrayField state helperAddr "rgiILOffset"
            |> Option.get
            |> int32Elements state

        ilOffsets |> List.length |> shouldEqual expected.Length

        // Innermost first, so the non-IL frame is the head.
        ilOffsets.Head |> shouldEqual -1

        // The rest have IL and keep their real offsets, none of which is -1.
        ilOffsets.Tail |> List.forall (fun offset -> offset >= 0) |> shouldEqual true

        // The native-offset array is -1 throughout regardless, so a handler that filled
        // `rgiILOffset` from it would pass the head check above; this pins that it did not.
        readArrayField state helperAddr "rgiOffset"
        |> Option.get
        |> int32Elements state
        |> shouldEqual (expected |> List.map (fun _ -> -1))

    [<Test>]
    let ``a dynamic method's frame reports the registry id it already carries`` () =
        // A `Reflection.Emit` method is synthesised — no MethodDef row — but it is not nameless: its
        // `DynamicMethodHandle` carries a registry id, and the handler must report that rather than
        // try to mint a metadata identity it has none of. Such frames do reach captures: an
        // exception thrown out of a `DynamicMethod` carries one in its frozen trace.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let prepared = prepareGuest loggerFactory
        let baseClassTypes = prepared.BaseClassTypes
        let state = prepared.State

        let state, expectedId =
            pushDynamicMethodFrame loggerFactory baseClassTypes prepared.EntryThread state

        let expected = StackFrameCapture.ofThread state.ThreadState.[prepared.EntryThread]

        let helperAddr, state = allocateHelper loggerFactory baseClassTypes 0 state
        let state = invoke loggerFactory prepared false helperAddr None state

        let ids =
            readArrayField state helperAddr "rgMethodHandle"
            |> Option.get
            |> methodHandleElements state

        ids |> List.length |> shouldEqual expected.Length

        // Innermost first, so the dynamic method is the head.
        ids.Head |> shouldEqual expectedId

        // And it resolves back to a dynamic method, not to a fabricated metadata one.
        match MethodHandleRegistry.resolveMethodFromId ids.Head state.MethodHandles with
        | Some (MethodHandle.FromDynamic handle) -> handle.GetRegistryId () |> shouldEqual expectedId
        | other -> failwith $"expected the frame's handle to resolve to a dynamic method, got %O{other}"
