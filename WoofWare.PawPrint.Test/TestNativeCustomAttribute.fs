namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestNativeCustomAttribute =

    let private attributeSource =
        """
public sealed class MyAttribute : System.Attribute
{
    public MyAttribute(int x, string s) { }
}
"""

    /// Variant attribute whose declaring type has a non-trivial static initialiser, so that
    /// the QCall's `ensureTypeInitialised` call returns `SuspendedForClassInit` on first
    /// entry. Pins down the contract that the cursor / named-arg out-slots are *not* written
    /// when class init suspends — otherwise the re-entered QCall would re-parse from the
    /// wrong offset.
    let private cctorAttributeSource =
        """
public sealed class CctorAttribute : System.Attribute
{
    private static readonly int Sentinel = ComputeSentinel();
    public CctorAttribute(int x, string s) { }
    private static int ComputeSentinel() => 7;
}
"""

    type private Fixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Corelib : DumpedAssembly
            GuestAssembly : DumpedAssembly
            CustomAttributeType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            QCallMethod : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            /// The `CustomAttribute_CreatePropertyOrFieldData` stub, concretized alongside the
            /// instance-creating one so that both QCalls' parameter types are registered.
            NamedArgQCallMethod : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            AttributeType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            AttributeTypeHandle : ConcreteTypeHandle
            AttributeRuntimeTypeAddr : ManagedHeapAddress
            CtorStubAddr : ManagedHeapAddress
            State : IlMachineState
        }

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

    let private concretizeTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeDefn =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

        IlMachineState.concretizeType
            loggerFactory
            baseClassTypes
            state
            typeInfo.AssemblyFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            typeDefn

    /// Constructs an `ObjectHandleOnStack` value that wraps a `void*` pointing at the
    /// given managed pointer source. Mirrors the inline `new ObjectHandleOnStack(ref x)`
    /// pattern that the C# wrapper IL produces before calling the PInvoke stub.
    let private objectHandleOnStackValue
        (fixture : Fixture)
        (target : ManagedPointerSource)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let objectHandleOnStackType =
            requiredTopLevelType fixture.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let state, objectHandleOnStackHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state objectHandleOnStackType

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes objectHandleOnStackHandle

        let value =
            match zero with
            | CliType.ValueType vt ->
                let ptrField =
                    IlMachineState.requiredOwnInstanceFieldId state objectHandleOnStackHandle "_ptr"

                CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
                |> CliType.ValueType
            | other -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

        value, state

    /// Constructs a `QCallModule` value with the `_module` field set to the guest assembly's
    /// `ModuleHandle` tag and `_ptr` left at its zero value. Mirrors the CoreCLR layout
    /// `struct QCallModule { void* _ptr; IntPtr _module; }`.
    let private qCallModuleValue (fixture : Fixture) (state : IlMachineState) : CliType * IlMachineState =
        let qCallModuleType =
            requiredTopLevelType fixture.Corelib "System.Runtime.CompilerServices" "QCallModule"

        let state, qCallModuleHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state qCallModuleType

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes qCallModuleHandle

        let value =
            match zero with
            | CliType.ValueType vt ->
                let moduleField =
                    IlMachineState.requiredOwnInstanceFieldId state qCallModuleHandle "_module"

                CliValueType.WithFieldSetById
                    moduleField
                    (CliType.Numeric (
                        CliNumericType.NativeInt (NativeIntSource.ModuleHandle fixture.GuestAssembly.Name.FullName)
                    ))
                    vt
                |> CliType.ValueType
            | other -> failwith $"QCallModule zero value was not a value type: %O{other}"

        value, state

    /// Allocates a single-element object[] holding `value` and returns a managed pointer
    /// source targeting cell 0. Suitable for backing an `ObjectHandleOnStack._ptr`.
    let private allocateObjectRefSlot
        (fixture : Fixture)
        (value : CliType)
        (state : IlMachineState)
        : ManagedHeapAddress * ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero objectHandle) (fun () -> value) 1 state

        arrayAddr, ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    /// Build the CustomAttrib blob for `MyAttribute(42, "hello")` plus a zero named-arg count.
    /// Layout (14 bytes total) follows ECMA-335 II.23.3:
    ///   prolog              0x01 0x00                            (2 bytes)
    ///   I4 value 42         0x2A 0x00 0x00 0x00                  (4 bytes)
    ///   SerString "hello"   0x05 0x68 0x65 0x6C 0x6C 0x6F        (6 bytes)
    ///   named-arg count     0x00 0x00                            (2 bytes)
    let private blobBytes : byte array =
        [|
            0x01uy
            0x00uy
            0x2Auy
            0x00uy
            0x00uy
            0x00uy
            0x05uy
            0x68uy
            0x65uy
            0x6Cuy
            0x6Cuy
            0x6Fuy
            0x00uy
            0x00uy
        |]

    /// Allocates a byte[] holding `bytes`, plus a one-cell IntPtr[] holding the current cursor
    /// (a byref into the blob's cell `cursorIdx`). Returns the blob array address and the IntPtr[]
    /// address so the test can later read back the updated cursor cell.
    ///
    /// `cursorIdx` is a parameter because the named-arg QCall is normally entered with the cursor
    /// already partway through the blob — the fixed-args section and the NumNamed count sit in
    /// front of it — and feeding one call's written cursor into the next is how the back-to-back
    /// test works.
    let private allocateBlobArraysFrom
        (fixture : Fixture)
        (bytes : byte array)
        (cursorIdx : int)
        (state : IlMachineState)
        : ManagedHeapAddress * ManagedHeapAddress * IlMachineState
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Byte

        let blobArr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero byteHandle)
                (fun () -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                bytes.Length
                state

        let state =
            (state, [ 0 .. bytes.Length - 1 ])
            ||> List.fold (fun state i ->
                IlMachineState.setArrayValue
                    blobArr
                    (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim bytes.[i])))
                    i
                    state
            )

        let intPtrHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.IntPtr

        let intPtrArr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero intPtrHandle)
                (fun () -> CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L))
                1
                state

        let cursorStart =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (blobArr, cursorIdx), []))
            )

        let state = IlMachineState.setArrayValue intPtrArr cursorStart 0 state

        blobArr, intPtrArr, state

    let private allocateBlobArrays
        (fixture : Fixture)
        (state : IlMachineState)
        : ManagedHeapAddress * ManagedHeapAddress * IlMachineState
        =
        allocateBlobArraysFrom fixture blobBytes 0 state

    /// Allocates a single-cell Int32[] zero-initialised. The QCall writes `pcNamedArgs`
    /// into cell 0.
    let private allocateInt32OutSlot
        (fixture : Fixture)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Int32

        IlMachineState.allocateArray
            (ConcreteTypeHandle.OneDimArrayZero int32Handle)
            (fun () -> CliType.Numeric (CliNumericType.Int32 0))
            1
            state

    /// Locates the static partial PInvoke stub for the named QCall entry point on the corelib's
    /// `System.Reflection.CustomAttribute` type. The wrapper method has IL; the QCall target is the
    /// static stub whose `NativeImport` points at `QCall::<entryPoint>`. Roslyn mangles the stub's
    /// own name (`<CreateCustomAttributeInstance>g____PInvoke|30_0`), so the entry point is the
    /// only stable way to find it.
    let private findQCallStub
        (entryPoint : string)
        (customAttributeType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        customAttributeType.Methods
        |> List.filter (fun method ->
            match method.TryNativeImport with
            | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
            | None -> false
        )
        |> function
            | [ method ] -> method
            | [] -> failwith $"QCall entry point %s{entryPoint} not found on System.Reflection.CustomAttribute"
            | methods ->
                failwith
                    $"QCall entry point %s{entryPoint} was ambiguous on System.Reflection.CustomAttribute: %d{methods.Length} matches"

    /// Compile the given attribute source, load corelib, find and concretize the QCall method
    /// and the named attribute type, allocate the RuntimeType and RuntimeMethodInfoStub for
    /// the attribute + its ctor. Mirrors the state the BCL would have prepared by the time it
    /// reaches the CreateCustomAttributeInstance QCall.
    let private makeFixtureWith (attributeSource : string) (attributeTypeName : string) : Fixture =
        let image =
            Roslyn.compileAssembly
                "CustomAttributeQCallTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ attributeSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let guestAssembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty guestAssembly

            let state = initialState.WithLoadedAssembly corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        let customAttributeType =
            requiredTopLevelType corelib "System.Reflection" "CustomAttribute"

        let rawQCallStub =
            findQCallStub "CustomAttribute_CreateCustomAttributeInstance" customAttributeType

        // Concretize the QCall method; this also concretizes its parameter types so the
        // active-pattern match (QCallModule / ObjectHandleOnStack / pointer-to-Int32 etc.)
        // in the handler succeeds at dispatch time.
        let state, qCallMethod, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawQCallStub
                None
                corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        let state, namedArgQCallMethod, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                (findQCallStub "CustomAttribute_CreatePropertyOrFieldData" customAttributeType)
                None
                corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        let attributeType = requiredTopLevelType guestAssembly "" attributeTypeName

        let state, attributeTypeHandle =
            concretizeTypeInfo loggerFactory baseClassTypes state attributeType

        let attributeRuntimeTypeAddr, state =
            IlMachineState.getOrAllocateType
                loggerFactory
                baseClassTypes
                (RuntimeTypeHandleTarget.Closed attributeTypeHandle)
                state

        // Locate the attribute ctor (we wrote exactly one, taking int + string).
        let rawCtor = attributeType.Methods |> List.find (fun m -> m.Name = ".ctor")

        let state, concretizedCtor, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawCtor
                ImmutableArray.Empty
                state

        let ctorMethodHandle, state =
            IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretizedCtor state

        let ctorStubAddr =
            match ctorMethodHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_value" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"Expected RuntimeMethodHandle.m_value to be an object ref, got %O{other}"
            | other -> failwith $"Expected RuntimeMethodHandle value type, got %O{other}"

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Corelib = corelib
            GuestAssembly = guestAssembly
            CustomAttributeType = customAttributeType
            QCallMethod = qCallMethod
            NamedArgQCallMethod = namedArgQCallMethod
            AttributeType = attributeType
            AttributeTypeHandle = attributeTypeHandle
            AttributeRuntimeTypeAddr = attributeRuntimeTypeAddr
            CtorStubAddr = ctorStubAddr
            State = state
        }

    let private makeFixture () : Fixture =
        makeFixtureWith attributeSource "MyAttribute"

    /// Build the seven-argument frame the QCall expects, install it on a fresh thread,
    /// and return the slot addresses the test will inspect after invocation.
    let private prepareInvocation
        (fixture : Fixture)
        : {|
              BlobArr : ManagedHeapAddress
              IntPtrArr : ManagedHeapAddress
              NamedArgsArr : ManagedHeapAddress
              InstanceArr : ManagedHeapAddress
              Thread : ThreadId
              State : IlMachineState
          |}
        =
        let state = fixture.State

        let qCallModule, state = qCallModuleValue fixture state

        // pCaType points at a slot holding the RuntimeType for MyAttribute.
        let _typeArr, typeSlot, state =
            allocateObjectRefSlot fixture (CliType.ObjectRef (Some fixture.AttributeRuntimeTypeAddr)) state

        let pCaType, state = objectHandleOnStackValue fixture typeSlot state

        // pCtor points at a slot holding the RuntimeMethodInfoStub for the ctor.
        let _ctorArr, ctorSlot, state =
            allocateObjectRefSlot fixture (CliType.ObjectRef (Some fixture.CtorStubAddr)) state

        let pCtor, state = objectHandleOnStackValue fixture ctorSlot state

        let blobArr, intPtrArr, state = allocateBlobArrays fixture state

        let ppBlob =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (intPtrArr, 0), []))
            )

        let pEndBlob =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (
                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (blobArr, blobBytes.Length), [])
                )
            )

        let namedArgsArr, state = allocateInt32OutSlot fixture state

        let pcNamedArgs =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (namedArgsArr, 0), []))
            )

        // The `instance` ObjectHandleOnStack receives the allocated attribute. Start it null.
        let instanceArr, instanceSlot, state =
            allocateObjectRefSlot fixture (CliType.ObjectRef None) state

        let pInstance, state = objectHandleOnStackValue fixture instanceSlot state

        let methodArgs =
            ImmutableArray.CreateRange [ qCallModule ; pCaType ; pCtor ; ppBlob ; pEndBlob ; pcNamedArgs ; pInstance ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.Corelib
                    fixture.QCallMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing -> failwith $"Unexpected missing assembly references creating QCall frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        {|
            BlobArr = blobArr
            IntPtrArr = intPtrArr
            NamedArgsArr = namedArgsArr
            InstanceArr = instanceArr
            Thread = thread
            State = state
        |}

    let private invokeHandler (fixture : Fixture) (thread : ThreadId) (state : IlMachineState) : NativeHandlerResult =
        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.Corelib
                TargetType = fixture.CustomAttributeType
            }

        match NativeCustomAttribute.tryExecuteQCall "CustomAttribute_CreateCustomAttributeInstance" ctx with
        | Some result -> result
        | None -> failwith "NativeCustomAttribute handler did not match"

    [<Test>]
    let ``Phase 1 suspends with ctor frame and advances blob cursor / pcNamedArgs`` () : unit =
        let fixture = makeFixture ()
        let prep = prepareInvocation fixture

        let result = invokeHandler fixture prep.Thread prep.State

        let state =
            match result with
            | NativeHandlerResult.PushedManagedCallee (state, _) -> state
            | other -> failwithf "Phase 1 expected PushedManagedCallee, got %A" other

        // The blob cursor cell now points one-past-end of the blob: fixed-args consumed
        // 12 bytes, then the named-arg count uint16 added 2 more.
        match IlMachineState.getArrayValue prep.IntPtrArr 0 state with
        | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr,
                                                                                                                 idx),
                                                                                         []))) ->
            arr |> shouldEqual prep.BlobArr
            idx |> shouldEqual blobBytes.Length
        | other -> failwithf "Expected ppBlob cell to hold an advanced ArrayElement byref, got %A" other

        // pcNamedArgs out-cell holds the parsed named-arg count (zero in our blob).
        match IlMachineState.getArrayValue prep.NamedArgsArr 0 state with
        | CliType.Numeric (CliNumericType.Int32 n) -> n |> shouldEqual 0
        | other -> failwithf "Expected pcNamedArgs cell to hold an Int32, got %A" other

        // The instance slot must still be null at this point: phase 2 writes it on re-entry.
        match IlMachineState.getArrayValue prep.InstanceArr 0 state with
        | CliType.ObjectRef None -> ()
        | other -> failwithf "Expected instance slot to remain null after phase 1, got %A" other

        // The QCall frame's eval stack carries the allocated-instance marker; the ctor
        // frame is now the active one with `this` + 2 args.
        let threadState = state.ThreadState.[prep.Thread]

        let qCallFrameId =
            threadState.MethodStates
            |> Map.toList
            |> List.find (fun (_, ms) ->
                ms.ExecutingMethod.RequiredDeclaringType.Identity = fixture.CustomAttributeType.Identity
                && ms.ExecutingMethod.Name = fixture.QCallMethod.Name
            )
            |> fst

        let qCallFrame = threadState.MethodStates.[qCallFrameId]

        let markerAddr =
            match qCallFrame.EvaluationStack.Values with
            | [ EvalStackValue.ObjectRef addr ] -> addr
            | other -> failwithf "Expected QCall frame eval stack to be [ObjectRef marker], got %A" other

        // The marker references an allocated MyAttribute heap object.
        let allocated = ManagedHeap.get markerAddr state.ManagedHeap
        allocated.ConcreteType |> shouldEqual fixture.AttributeTypeHandle

        // The active frame is the ctor; check its arguments match the blob payload.
        let activeFrame = threadState.MethodState
        activeFrame.ExecutingMethod.Name |> shouldEqual ".ctor"

        activeFrame.ExecutingMethod.RequiredDeclaringType.Identity
        |> shouldEqual fixture.AttributeType.Identity

        activeFrame.Arguments.Length |> shouldEqual 3

        match activeFrame.Arguments.[0] with
        | CliType.ObjectRef (Some a) -> a |> shouldEqual markerAddr
        | other -> failwithf "Expected ctor arg 0 to be `this` = marker, got %A" other

        match activeFrame.Arguments.[1] with
        | CliType.Numeric (CliNumericType.Int32 n) -> n |> shouldEqual 42
        | other -> failwithf "Expected ctor arg 1 to be Int32 42, got %A" other

        match activeFrame.Arguments.[2] with
        | CliType.ObjectRef (Some stringAddr) ->
            ManagedHeap.getStringContents stringAddr state.ManagedHeap
            |> shouldEqual (Some "hello")
        | other -> failwithf "Expected ctor arg 2 to be a managed string \"hello\", got %A" other

    [<Test>]
    let ``Phase 2 pops the marker and writes the instance ObjectHandleOnStack`` () : unit =
        let fixture = makeFixture ()
        let prep = prepareInvocation fixture

        // Synthesise a post-suspend state: we don't need to actually run the ctor; the
        // contract is "QCall frame has a single ObjectRef marker on its eval stack, and the
        // handler is re-entered with that marker visible". Build that directly by pushing
        // the marker onto the only frame's eval stack — its address can be any
        // ManagedHeapAddress, and using the AttributeRuntimeTypeAddr happens to also be a
        // real allocated heap object, which makes failure diagnostics readable.
        let markerAddr = fixture.AttributeRuntimeTypeAddr

        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef markerAddr) prep.Thread prep.State

        let result = invokeHandler fixture prep.Thread state

        let state =
            match result with
            | NativeHandlerResult.Completed (state, _) -> state
            | other -> failwithf "Phase 2 expected Completed, got %A" other

        // The instance ObjectHandleOnStack slot now points at the marker.
        match IlMachineState.getArrayValue prep.InstanceArr 0 state with
        | CliType.ObjectRef (Some addr) -> addr |> shouldEqual markerAddr
        | other -> failwithf "Expected instance slot to hold the marker addr, got %A" other

        // And the eval stack has been emptied.
        let threadState = state.ThreadState.[prep.Thread]
        threadState.MethodState.EvaluationStack.Values |> shouldEqual []

    [<Test>]
    let ``Class-init suspension leaves blob cursor and pcNamedArgs untouched`` () : unit =
        // CctorAttribute has a static field initialiser, so the attribute type's cctor must
        // run before the QCall can allocate the instance. The handler must therefore return
        // `SuspendedForClassInit` *without* having written the blob cursor or named-arg count
        // out-cells — when the cctor finishes and the QCall is re-entered, the BCL re-parses
        // the blob from `*ppBlob`, and a prematurely-advanced cursor would feed it bytes that
        // belong past the end of the blob.
        let fixture = makeFixtureWith cctorAttributeSource "CctorAttribute"
        let prep = prepareInvocation fixture

        let result = invokeHandler fixture prep.Thread prep.State

        let state =
            match result with
            | NativeHandlerResult.SuspendedForClassInit (state, _) -> state
            | other -> failwithf "Class-init suspension test expected SuspendedForClassInit, got %A" other

        // The blob cursor cell must still be the starting byref into cell 0 of the blob.
        match IlMachineState.getArrayValue prep.IntPtrArr 0 state with
        | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr,
                                                                                                                 idx),
                                                                                         []))) ->
            arr |> shouldEqual prep.BlobArr
            idx |> shouldEqual 0
        | other -> failwithf "Expected ppBlob cell to still point at blob[0] after suspension, got %A" other

        // pcNamedArgs must still hold its zero-initialised default.
        match IlMachineState.getArrayValue prep.NamedArgsArr 0 state with
        | CliType.Numeric (CliNumericType.Int32 n) -> n |> shouldEqual 0
        | other -> failwithf "Expected pcNamedArgs cell to still hold its initial zero, got %A" other

        // The instance slot is still null too — phase 2 hasn't run.
        match IlMachineState.getArrayValue prep.InstanceArr 0 state with
        | CliType.ObjectRef None -> ()
        | other -> failwithf "Expected instance slot to remain null after suspension, got %A" other

    // ------------------------------------------------------------------
    // CustomAttribute_CreatePropertyOrFieldData
    // ------------------------------------------------------------------

    /// Builds a `StringHandleOnStack` whose `_ptr` targets `target`. Structurally identical to
    /// `objectHandleOnStackValue` — both are a one-field struct wrapping a `_ptr` — but they are
    /// distinct corelib types and the QCall's signature match distinguishes them.
    let private stringHandleOnStackValue
        (fixture : Fixture)
        (target : ManagedPointerSource)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let stringHandleOnStackType =
            requiredTopLevelType fixture.Corelib "System.Runtime.CompilerServices" "StringHandleOnStack"

        let state, handle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state stringHandleOnStackType

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes handle

        let value =
            match zero with
            | CliType.ValueType vt ->
                let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

                CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
                |> CliType.ValueType
            | other -> failwith $"StringHandleOnStack zero value was not a value type: %O{other}"

        value, state

    /// Every out-slot starts holding this, never null.
    ///
    /// The managed caller pre-nulls its own locals, and so does the obvious fixture, so a handler
    /// that simply never wrote the type slot would pass a `type slot is null` assertion by reading
    /// back the fixture's own initialisation. Seeding a non-null sentinel is what makes
    /// "the handler wrote null" distinguishable from "the handler did nothing", which is the whole
    /// content of the explicit-null contract.
    let private outSlotSentinel (fixture : Fixture) : CliType =
        CliType.ObjectRef (Some fixture.AttributeRuntimeTypeAddr)

    let private prepareNamedArgInvocation
        (fixture : Fixture)
        (blob : byte array)
        (cursorIdx : int)
        : {|
              BlobArr : ManagedHeapAddress
              IntPtrArr : ManagedHeapAddress
              IsPropertyArr : ManagedHeapAddress
              NameArr : ManagedHeapAddress
              TypeArr : ManagedHeapAddress
              ValueArr : ManagedHeapAddress
              Thread : ThreadId
              State : IlMachineState
          |}
        =
        let state = fixture.State

        let qCallModule, state = qCallModuleValue fixture state

        let blobArr, intPtrArr, state = allocateBlobArraysFrom fixture blob cursorIdx state

        let ppBlobStart =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (intPtrArr, 0), []))
            )

        let pBlobEnd =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (
                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (blobArr, blob.Length), [])
                )
            )

        let nameArr, nameSlot, state =
            allocateObjectRefSlot fixture (outSlotSentinel fixture) state

        let pName, state = stringHandleOnStackValue fixture nameSlot state

        let isPropertyArr, state = allocateInt32OutSlot fixture state

        let pbIsProperty =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (isPropertyArr, 0), []))
            )

        let typeArr, typeSlot, state =
            allocateObjectRefSlot fixture (outSlotSentinel fixture) state

        let pType, state = objectHandleOnStackValue fixture typeSlot state

        let valueArr, valueSlot, state =
            allocateObjectRefSlot fixture (outSlotSentinel fixture) state

        let pValue, state = objectHandleOnStackValue fixture valueSlot state

        let methodArgs =
            ImmutableArray.CreateRange [ qCallModule ; ppBlobStart ; pBlobEnd ; pName ; pbIsProperty ; pType ; pValue ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.Corelib
                    fixture.NamedArgQCallMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing -> failwith $"Unexpected missing assembly references creating QCall frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        {|
            BlobArr = blobArr
            IntPtrArr = intPtrArr
            IsPropertyArr = isPropertyArr
            NameArr = nameArr
            TypeArr = typeArr
            ValueArr = valueArr
            Thread = thread
            State = state
        |}

    let private invokeNamedArgHandler
        (fixture : Fixture)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.Corelib
                TargetType = fixture.CustomAttributeType
            }

        match NativeCustomAttribute.tryExecuteQCall "CustomAttribute_CreatePropertyOrFieldData" ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> state
        | Some other -> failwithf "expected Completed, got %A" other
        | None -> failwith "NativeCustomAttribute handler did not match CreatePropertyOrFieldData"

    /// Encode one NamedArg: kind tag, one-byte FieldOrPropType, SerString name, then value bytes.
    let private namedArgBlob (kindTag : byte) (typeTag : byte) (name : string) (value : byte array) : byte array =
        let nameUtf8 = System.Text.Encoding.UTF8.GetBytes name

        if nameUtf8.Length >= 0x80 then
            failwith "test helper only encodes short (single-byte PackedLen) names"

        Array.concat [ [| kindTag ; typeTag ; byte nameUtf8.Length |] ; nameUtf8 ; value ]

    let private readCursorIndex (arr : ManagedHeapAddress) (state : IlMachineState) : ManagedHeapAddress * int =
        match IlMachineState.getArrayValue arr 0 state with
        | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (a, idx),
                                                                                         []))) -> a, idx
        | other -> failwithf "expected the cursor cell to hold an ArrayElement byref, got %A" other

    let private readObjectSlot (arr : ManagedHeapAddress) (state : IlMachineState) : ManagedHeapAddress option =
        match IlMachineState.getArrayValue arr 0 state with
        | CliType.ObjectRef addr -> addr
        | other -> failwithf "expected an object ref in the out-slot, got %A" other

    let private readInt32Slot (arr : ManagedHeapAddress) (state : IlMachineState) : int =
        match IlMachineState.getArrayValue arr 0 state with
        | CliType.Numeric (CliNumericType.Int32 v) -> v
        | other -> failwithf "expected an Int32 in the out-slot, got %A" other

    /// The corelib type an allocated object was stamped with, as "Namespace.Name".
    let private objectTypeName (fixture : Fixture) (addr : ManagedHeapAddress) (state : IlMachineState) : string =
        let obj = ManagedHeap.get addr state.ManagedHeap

        let concrete =
            AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwithf "object at %O had an unregistered concrete type" addr)

        let assembly = state._LoadedAssemblies.ByDefinitionName concrete.AssemblyFullName
        let defn = assembly.TypeDefs.[concrete.Definition.Get]
        $"%s{defn.Namespace}.%s{defn.Name}"

    let private readManagedString (addr : ManagedHeapAddress) (state : IlMachineState) : string =
        match ManagedHeap.getStringContents addr state.ManagedHeap with
        | Some s -> s
        | None -> failwithf "object at %O was not a managed string" addr

    [<Test>]
    let ``property named arg with a string value`` () : unit =
        let fixture = makeFixture ()

        // 0x54 PROPERTY, 0x0E STRING, name "Label", SerString "hi"
        let blob = namedArgBlob 0x54uy 0x0Euy "Label" [| 0x02uy ; byte 'h' ; byte 'i' |]

        let prep = prepareNamedArgInvocation fixture blob 0
        let state = invokeNamedArgHandler fixture prep.Thread prep.State

        readInt32Slot prep.IsPropertyArr state |> shouldEqual 1

        readObjectSlot prep.NameArr state
        |> Option.map (fun a -> readManagedString a state)
        |> shouldEqual (Some "Label")

        readObjectSlot prep.ValueArr state
        |> Option.map (fun a -> readManagedString a state)
        |> shouldEqual (Some "hi")

        // A non-null string leaves pType alone in CoreCLR; we write the null explicitly, and the
        // slot was seeded non-null, so this asserts the write happened.
        readObjectSlot prep.TypeArr state |> shouldEqual None

        readCursorIndex prep.IntPtrArr state |> shouldEqual (prep.BlobArr, blob.Length)

    [<Test>]
    let ``field named arg boxes an Int32`` () : unit =
        let fixture = makeFixture ()

        // 0x53 FIELD, 0x08 I4, name "Count", 42
        let blob =
            namedArgBlob 0x53uy 0x08uy "Count" [| 0x2Auy ; 0x00uy ; 0x00uy ; 0x00uy |]

        let prep = prepareNamedArgInvocation fixture blob 0
        let state = invokeNamedArgHandler fixture prep.Thread prep.State

        readInt32Slot prep.IsPropertyArr state |> shouldEqual 0

        readObjectSlot prep.NameArr state
        |> Option.map (fun a -> readManagedString a state)
        |> shouldEqual (Some "Count")

        match readObjectSlot prep.ValueArr state with
        | None -> failwith "expected a boxed value"
        | Some addr ->
            objectTypeName fixture addr state |> shouldEqual "System.Int32"

            match ManagedHeap.get addr state.ManagedHeap with
            | box ->
                match AllocatedNonArrayObject.DereferenceField "m_value" box with
                | CliType.Numeric (CliNumericType.Int32 v) -> v |> shouldEqual 42
                | other -> failwithf "expected Int32 payload in the box, got %A" other

        readObjectSlot prep.TypeArr state |> shouldEqual None
        readCursorIndex prep.IntPtrArr state |> shouldEqual (prep.BlobArr, blob.Length)

    /// The box's corelib type comes from the *blob's* type byte, not from the decoded value's F#
    /// shape. Each row here is a distinct way for that to go wrong and still pass an Int32-only
    /// test: U4/U8 are the widths CLI eval-stack rules normalise to Int32/Int64, Boolean and Char
    /// are sub-word narrowings, and R4 is the float width — the eval stack has a single float type,
    /// so narrowing to float32 inside a System.Single box is a mode no integer row exercises.
    [<Test>]
    let ``box type comes from the blob type byte, not the decoded value`` () : unit =
        let cases : (byte * byte array * string * (CliType -> unit)) list =
            [
                0x09uy,
                [| 0xFFuy ; 0xFFuy ; 0xFFuy ; 0xFFuy |],
                "System.UInt32",
                fun v ->
                    match v with
                    | CliType.Numeric (CliNumericType.Int32 x) -> x |> shouldEqual -1
                    | other -> failwithf "UInt32 box payload: %A" other
                0x02uy,
                [| 0x01uy |],
                "System.Boolean",
                fun v ->
                    match v with
                    | CliType.Bool b -> b |> shouldEqual 1uy
                    | other -> failwithf "Boolean box payload: %A" other
                0x03uy,
                [| 0x5Auy ; 0x00uy |],
                "System.Char",
                fun v ->
                    match v with
                    | CliType.Char (hi, lo) -> (hi, lo) |> shouldEqual (0uy, byte 'Z')
                    | other -> failwithf "Char box payload: %A" other
                0x0Cuy,
                System.BitConverter.GetBytes 1.5f,
                "System.Single",
                fun v ->
                    match v with
                    | CliType.Numeric (CliNumericType.Float32 f) -> f |> shouldEqual 1.5f
                    | other -> failwithf "Single box payload: %A" other
                0x0Buy,
                System.BitConverter.GetBytes System.UInt64.MaxValue,
                "System.UInt64",
                fun v ->
                    match v with
                    | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim v)) -> v |> shouldEqual -1L
                    | other -> failwithf "UInt64 box payload: %A" other
            ]

        for typeTag, valueBytes, expectedTypeName, checkPayload in cases do
            let fixture = makeFixture ()
            let blob = namedArgBlob 0x53uy typeTag "F" valueBytes
            let prep = prepareNamedArgInvocation fixture blob 0
            let state = invokeNamedArgHandler fixture prep.Thread prep.State

            match readObjectSlot prep.ValueArr state with
            | None -> failwithf "type tag 0x%02X: expected a boxed value" typeTag
            | Some addr ->
                objectTypeName fixture addr state |> shouldEqual expectedTypeName

                let box = ManagedHeap.get addr state.ManagedHeap
                checkPayload (AllocatedNonArrayObject.DereferenceField "m_value" box)

            readCursorIndex prep.IntPtrArr state |> shouldEqual (prep.BlobArr, blob.Length)

    [<Test>]
    let ``null string value yields a null value and typeof string`` () : unit =
        let fixture = makeFixture ()

        // 0x54 PROPERTY, 0x0E STRING, name "Label", 0xFF null SerString sentinel
        let blob = namedArgBlob 0x54uy 0x0Euy "Label" [| 0xFFuy |]

        let prep = prepareNamedArgInvocation fixture blob 0
        let state = invokeNamedArgHandler fixture prep.Thread prep.State

        // The value slot was seeded non-null, so this asserts the handler nulled it.
        readObjectSlot prep.ValueArr state |> shouldEqual None

        // ... and the one in-scope case where CoreCLR writes a real type, so that the managed
        // caller can pick the string-typed GetProperty overload.
        match readObjectSlot prep.TypeArr state with
        | None -> failwith "expected typeof(string) in the type slot"
        | Some addr -> objectTypeName fixture addr state |> shouldEqual "System.RuntimeType"

        readCursorIndex prep.IntPtrArr state |> shouldEqual (prep.BlobArr, blob.Length)

    [<Test>]
    let ``empty string value routes through the canonical empty string`` () : unit =
        let fixture = makeFixture ()
        let blob = namedArgBlob 0x54uy 0x0Euy "Label" [| 0x00uy |]
        let prep = prepareNamedArgInvocation fixture blob 0
        let state = invokeNamedArgHandler fixture prep.Thread prep.State

        match readObjectSlot prep.ValueArr state with
        | None -> failwith "expected an empty string, got null"
        | Some addr ->
            readManagedString addr state |> shouldEqual ""

            let canonical, _ =
                IlMachineState.internCanonicalEmptyString fixture.LoggerFactory fixture.BaseClassTypes state

            addr |> shouldEqual canonical

    [<Test>]
    let ``null member name is passed through`` () : unit =
        let fixture = makeFixture ()

        // 0x53 FIELD, 0x08 I4, 0xFF null name sentinel, then the value.
        let blob =
            Array.concat [ [| 0x53uy ; 0x08uy ; 0xFFuy |] ; [| 0x07uy ; 0x00uy ; 0x00uy ; 0x00uy |] ]

        let prep = prepareNamedArgInvocation fixture blob 0
        let state = invokeNamedArgHandler fixture prep.Thread prep.State

        // Seeded non-null, so this is the handler's write, not the fixture's initialisation.
        readObjectSlot prep.NameArr state |> shouldEqual None
        readCursorIndex prep.IntPtrArr state |> shouldEqual (prep.BlobArr, blob.Length)

    /// Feeds the first call's written cursor into a second call, so that the cursor arithmetic is
    /// checked against a following argument rather than only against blob end. A real guest gets
    /// the equivalent check from the managed caller's `blobStart != blobEnd`.
    [<Test>]
    let ``two named args back to back`` () : unit =
        let fixture = makeFixture ()

        let first = namedArgBlob 0x54uy 0x0Euy "Label" [| 0x02uy ; byte 'h' ; byte 'i' |]

        let second =
            namedArgBlob 0x53uy 0x08uy "Count" [| 0x2Auy ; 0x00uy ; 0x00uy ; 0x00uy |]

        let blob = Array.append first second

        let prep = prepareNamedArgInvocation fixture blob 0
        let state = invokeNamedArgHandler fixture prep.Thread prep.State

        let _, afterFirst = readCursorIndex prep.IntPtrArr state
        afterFirst |> shouldEqual first.Length

        // Second call starts exactly where the first left off.
        let prep2 = prepareNamedArgInvocation fixture blob afterFirst
        let state2 = invokeNamedArgHandler fixture prep2.Thread prep2.State

        readInt32Slot prep2.IsPropertyArr state2 |> shouldEqual 0

        readObjectSlot prep2.NameArr state2
        |> Option.map (fun a -> readManagedString a state2)
        |> shouldEqual (Some "Count")

        readCursorIndex prep2.IntPtrArr state2
        |> shouldEqual (prep2.BlobArr, blob.Length)

    /// Each out-of-scope serialization type fails with its own message, naming the construct and
    /// the capability it needs, so a diagnostic identifies which feature the blob required. An
    /// array names its element type too, which takes one row per kind an element can be: that is
    /// the only position those kinds are described from, a scalar TYPE or TAGGED_OBJECT being
    /// refused by a message of its own before any description is asked for.
    [<Test>]
    let ``out-of-scope serialization types each fail with their own message`` () : unit =
        let cases =
            [
                // SZARRAY of I4: element count then elements.
                [|
                    0x53uy
                    0x1Duy
                    0x08uy
                    0x01uy
                    byte 'F'
                    0x00uy
                    0x00uy
                    0x00uy
                    0x00uy
                |],
                "SZARRAY type (element int32)"
                // SZARRAY of each remaining element kind. The refusal precedes the value, so these
                // need no bytes past the member name.
                [| 0x53uy ; 0x1Duy ; 0x50uy ; 0x01uy ; byte 'F' |], "(element TYPE (0x50))"
                [| 0x53uy ; 0x1Duy ; 0x51uy ; 0x01uy ; byte 'F' |], "(element TAGGED_OBJECT (0x51))"
                Array.concat
                    [
                        [| 0x53uy ; 0x1Duy ; 0x55uy ; 0x04uy |]
                        System.Text.Encoding.UTF8.GetBytes "MyEn"
                        [| 0x01uy ; byte 'F' |]
                    ],
                "(element ENUM (0x55) named \"MyEn\")"
                // ENUM naming its type by a SerString.
                Array.concat
                    [
                        [| 0x53uy ; 0x55uy ; 0x04uy |]
                        System.Text.Encoding.UTF8.GetBytes "MyEn"
                        [| 0x01uy ; byte 'F' ; 0x00uy ; 0x00uy ; 0x00uy ; 0x00uy |]
                    ],
                "ENUM"
                [| 0x53uy ; 0x50uy ; 0x01uy ; byte 'F' ; 0xFFuy |], "TYPE (0x50)"
                [| 0x53uy ; 0x51uy ; 0x01uy ; byte 'F' ; 0x0Euy ; 0xFFuy |], "TAGGED_OBJECT (0x51)"
                // SZARRAY whose element tag is itself SZARRAY. The reader takes that second byte
                // as the element tag and stops, so this reaches the SZARRAY refusal rather than
                // spending a host stack frame per 0x1D; the refusal names the tag it kept.
                [|
                    0x53uy
                    0x1Duy
                    0x1Duy
                    0x01uy
                    byte 'F'
                    0xFFuy
                    0xFFuy
                    0xFFuy
                    0xFFuy
                |],
                "0x1D, which names no serialization type"
            ]

        for blob, expectedFragment in cases do
            let fixture = makeFixture ()
            let prep = prepareNamedArgInvocation fixture blob 0

            let exn =
                Assert.Throws (fun () -> invokeNamedArgHandler fixture prep.Thread prep.State |> ignore)

            exn.Message |> shouldContainText expectedFragment

    [<Test>]
    let ``a malformed kind tag fails loudly`` () : unit =
        let fixture = makeFixture ()

        let blob =
            [| 0x52uy ; 0x08uy ; 0x01uy ; byte 'F' ; 0x00uy ; 0x00uy ; 0x00uy ; 0x00uy |]

        let prep = prepareNamedArgInvocation fixture blob 0

        let exn =
            Assert.Throws (fun () -> invokeNamedArgHandler fixture prep.Thread prep.State |> ignore)

        exn.Message |> shouldContainText "neither FIELD (0x53) nor PROPERTY (0x54)"
