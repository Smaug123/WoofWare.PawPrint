namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestNativeSignature =

    /// Mint a bare `RuntimeMethodHandleInternal` for a method on a *closed* declaring type, the way
    /// the runtime's closed introduced-method path does. Tests that need the open-definition target
    /// call `MethodHandleRegistry.getOrAllocateInternalHandle` directly.
    let private internalHandleForClosed
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (concreteTypes : AllConcreteTypes)
        (declaringConcrete : ConcreteType<ConcreteTypeHandle>)
        (method : WoofWare.PawPrint.MethodInfo<'tyGen, GenericParamFromMetadata, TypeDefn>)
        (reg : MethodHandleRegistry)
        : CliValueType * MethodHandleRegistry
        =
        let target =
            AllConcreteTypes.findExistingConcreteType
                concreteTypes
                declaringConcrete.Identity
                declaringConcrete.Generics
            |> Option.defaultWith (fun () ->
                failwith $"declaring type %O{declaringConcrete} was not registered in ConcreteTypes"
            )
            |> RuntimeTypeHandleTarget.Closed

        MethodHandleRegistry.getOrAllocateInternalHandle
            baseClassTypes
            concreteTypes
            declaringConcrete.AssemblyFullName
            target
            method
            reg


    let private signatureSource =
        """
public sealed class DistinctiveFieldType
{
}

public sealed class GenericFieldHost<T>
{
    public T Payload;
}

public sealed class MethodSignatureHost
{
    public static int Twice (int x, string s) => x * 2;

    public void Instance (double d)
    {
    }

    public static void Nothing ()
    {
    }

    public static T Generic<T> (T t) => t;
}
"""

    type private SignatureFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assembly : DumpedAssembly
            HostType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            DistinctiveType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            Field : FieldInfo<GenericParamFromMetadata, TypeDefn>
            State : IlMachineState
        }

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

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

    let private makeSignatureFixture () : SignatureFixture =
        let image =
            Roslyn.compileAssembly
                "SignatureFieldHandleTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ signatureSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let hostType = requiredTopLevelType assembly "" "GenericFieldHost`1"
        let distinctiveType = requiredTopLevelType assembly "" "DistinctiveFieldType"

        let field = hostType.Fields |> List.find (fun field -> field.Name = "Payload")

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib

        let state : IlMachineState =
            (state,
             [
                 baseClassTypes.Object
                 baseClassTypes.Byte
                 baseClassTypes.Int32
                 baseClassTypes.IntPtr
                 baseClassTypes.RuntimeType
                 baseClassTypes.RuntimeFieldHandle
                 baseClassTypes.RuntimeFieldHandleInternal
                 baseClassTypes.RuntimeFieldInfoStub
                 baseClassTypes.RuntimeMethodHandleInternal
             ])
            ||> List.fold (fun state typeInfo -> concretizeTypeInfo loggerFactory baseClassTypes state typeInfo |> fst)

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Assembly = assembly
            HostType = hostType
            DistinctiveType = distinctiveType
            Field = field
            State = state
        }

    let private runtimeFieldInfoStubAddress (fieldHandle : CliType) : ManagedHeapAddress =
        match fieldHandle with
        | CliType.ValueType vt ->
            match CliValueType.DereferenceField "m_ptr" vt with
            | CliType.ObjectRef (Some addr) -> addr
            | other -> failwith $"Expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
        | other -> failwith $"Expected RuntimeFieldHandle value type, got %O{other}"

    let private runtimeFieldHandleInternalInRuntimeFieldInfoStub (allocated : AllocatedNonArrayObject) : CliType =
        match CliValueType.DereferenceField "m_fieldHandle" allocated.Contents with
        | CliType.ValueType _ as runtimeFieldHandleInternal -> runtimeFieldHandleInternal
        | other ->
            failwith $"Expected RuntimeFieldInfoStub.m_fieldHandle to be a RuntimeFieldHandleInternal, got %O{other}"

    let private closedGenericFieldHandle
        (fixture : SignatureFixture)
        (state : IlMachineState)
        : CliType * ConcreteTypeHandle * IlMachineState
        =
        let distinctiveDefn =
            TypeDefn.FromDefinition (fixture.DistinctiveType.Identity, SignatureTypeKind.Class)

        let state, distinctiveHandle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.Assembly.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                distinctiveDefn

        let genericHostDefn =
            TypeDefn.FromDefinition (fixture.HostType.Identity, SignatureTypeKind.Class)

        let closedHostDefn =
            TypeDefn.GenericInstantiation (genericHostDefn, ImmutableArray.Create distinctiveDefn)

        let state, closedHostHandle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.Assembly.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                closedHostDefn

        let runtimeFieldInfoStubHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.RuntimeFieldInfoStub

        let runtimeFieldHandle, fieldHandles, state =
            FieldHandleRegistry.getOrAllocate
                fixture.BaseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> IlMachineState.allocateManagedObject runtimeFieldInfoStubHandle fields state)
                (RuntimeTypeHandleTarget.Closed closedHostHandle)
                fixture.Field.Handle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = fieldHandles
            }

        let stubAddress = runtimeFieldInfoStubAddress runtimeFieldHandle
        let stub = ManagedHeap.get stubAddress state.ManagedHeap

        runtimeFieldHandleInternalInRuntimeFieldInfoStub stub, distinctiveHandle, state

    let private signatureInitMethod
        (fixture : SignatureFixture)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          ConcreteTypeHandle *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let signatureType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System" "Signature"

        // .NET 10 routes the field-signature population through the Signature_Init QCall stub.
        // The wrapper Init method has IL; the QCall target is the same-named static stub with
        // 5 parameters and a NativeImport pointing at "Signature_Init".
        let rawMethod =
            signatureType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = "Signature_Init"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point Signature_Init not found on System.Signature"
                | methods ->
                    failwith
                        $"QCall entry point Signature_Init was ambiguous on System.Signature: %d{methods.Length} matches"

        let state, signatureTypeHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state signatureType

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                fixture.LoggerFactory
                fixture.BaseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                fixture.BaseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        state, signatureType, signatureTypeHandle, method

    let private allocateZeroInitializedObject
        (fixture : SignatureFixture)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (typeHandle : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, fields =
            ((state, []), typeInfo.Fields |> List.filter (fun field -> not field.IsStatic))
            ||> List.fold (fun (state, fields) field ->
                let state, zero, fieldTypeHandle =
                    IlMachineState.cliTypeZeroOf
                        fixture.LoggerFactory
                        fixture.BaseClassTypes
                        assembly
                        field.Signature
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        state

                state, FieldIdentity.cliField typeHandle field zero fieldTypeHandle :: fields
            )

        let contents =
            fields
            |> List.rev
            |> CliValueType.OfFields
                fixture.BaseClassTypes
                state.ConcreteTypes
                typeHandle
                (DeclaredTypeFacts.ofTypeInfo fixture.BaseClassTypes state._LoadedAssemblies typeInfo)

        IlMachineState.allocateManagedObject typeHandle contents state

    let private signatureField
        (state : IlMachineState)
        (signatureAddr : ManagedHeapAddress)
        (fieldName : string)
        : CliType
        =
        let signatureObj = ManagedHeap.get signatureAddr state.ManagedHeap

        let field =
            IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType fieldName

        AllocatedNonArrayObject.DereferenceFieldById field signatureObj

    /// Allocates a one-element object array, stores `value` at index 0, and returns a managed
    /// pointer source that targets that slot — suitable for use as the byref backing an
    /// `ObjectHandleOnStack`.
    let private allocateObjectRefSlot
        (fixture : SignatureFixture)
        (value : CliType)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero objectHandle) (fun () -> value) 1 state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private objectHandleOnStackValue
        (fixture : SignatureFixture)
        (target : ManagedPointerSource)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let objectHandleOnStackType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let state, objectHandleOnStackHandle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.BaseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (objectHandleOnStackType.Identity, SignatureTypeKind.ValueType))

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

    let private invokeSignatureInit
        (fixture : SignatureFixture)
        (pCorSig : CliType)
        (cCorSig : CliType)
        (fieldHandleOverride : CliType option)
        (methodHandle : IlMachineState -> CliType * IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let fieldHandleInternal, expectedFieldTypeHandle, state =
            closedGenericFieldHandle fixture fixture.State

        let fieldHandleInternal =
            fieldHandleOverride |> Option.defaultValue fieldHandleInternal

        let methodHandle, state = methodHandle state

        let state, signatureType, signatureTypeHandle, signatureInitMethod =
            signatureInitMethod fixture state

        let signatureAddr, state =
            allocateZeroInitializedObject fixture fixture.BaseClassTypes.Corelib signatureType signatureTypeHandle state

        // Build an ObjectHandleOnStack pointing at a one-element object array slot containing
        // the Signature reference. The QCall reads through the byref to find the Signature.
        let signatureRefSlot, state =
            allocateObjectRefSlot fixture (CliType.ObjectRef (Some signatureAddr)) state

        let objectHandleOnStack, state =
            objectHandleOnStackValue fixture signatureRefSlot state

        let methodArgs =
            ImmutableArray.CreateRange [ objectHandleOnStack ; pCorSig ; cCorSig ; fieldHandleInternal ; methodHandle ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    signatureInitMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating Signature_Init frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.BaseClassTypes.Corelib
                TargetType = signatureType
            }

        let state =
            match NativeSignature.tryExecuteQCall "Signature_Init" ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected Signature_Init execution result: %O{result}"
            | None -> failwith "Signature_Init did not match"

        signatureAddr, expectedFieldTypeHandle, state

    let private nullPCorSig : CliType =
        CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)

    /// The `RuntimeMethodHandleInternal` null sentinel. `Signature_Init`'s fourth parameter is a
    /// struct (`default(RuntimeMethodHandleInternal)`, i.e. `m_handle == IntPtr.Zero`), unlike the
    /// pre-.NET-10 `Signature.GetSignature` InternalCall whose corresponding argument is an
    /// `IRuntimeMethodInfo` reference and so is null as an object reference.
    let private nullMethodHandle (state : IlMachineState) : CliType * IlMachineState =
        CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L), state

    let private methodSignatureHost (fixture : SignatureFixture) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        requiredTopLevelType fixture.Assembly "" "MethodSignatureHost"

    let private requiredHostMethod
        (fixture : SignatureFixture)
        (methodName : string)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        (methodSignatureHost fixture).Methods
        |> List.filter (fun method -> method.Name = methodName)
        |> function
            | [ method ] -> method
            | [] -> failwith $"method %s{methodName} not found on MethodSignatureHost"
            | methods ->
                failwith $"method %s{methodName} was ambiguous on MethodSignatureHost: %d{methods.Length} matches"

    /// A `RuntimeMethodHandleInternal` naming a fully-closed method on `MethodSignatureHost`, as
    /// `RuntimeMethodInfo`'s handle would be by the time it reaches `Signature_Init`.
    let private closedMethodHandle
        (fixture : SignatureFixture)
        (methodName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, _ =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state (methodSignatureHost fixture)

        let state, concretised, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                fixture.LoggerFactory
                fixture.BaseClassTypes
                ImmutableArray.Empty
                (requiredHostMethod fixture methodName)
                None
                fixture.Assembly.DefinitionFullName
                ImmutableArray.Empty
                state

        let handle, methodHandles =
            MethodHandleRegistry.getOrAllocateConcreteInternalHandle
                fixture.BaseClassTypes
                state.ConcreteTypes
                concretised
                state.MethodHandles

        CliType.ValueType handle,
        { state with
            MethodHandles = methodHandles
        }

    /// A `RuntimeMethodHandleInternal` naming a generic method *definition* -- the shape
    /// `RuntimeTypeHandle.GetFirstIntroducedMethod` mints, whose `MethodGenerics` is empty even
    /// though the method declares type parameters.
    let private openMethodHandle
        (fixture : SignatureFixture)
        (methodName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, hostHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state (methodSignatureHost fixture)

        let declaringType =
            AllConcreteTypes.lookup hostHandle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "MethodSignatureHost was not concretized")

        let handle, methodHandles =
            internalHandleForClosed
                fixture.BaseClassTypes
                state.ConcreteTypes
                declaringType
                (requiredHostMethod fixture methodName)
                state.MethodHandles

        CliType.ValueType handle,
        { state with
            MethodHandles = methodHandles
        }

    /// Every spelling `default(RuntimeFieldHandleInternal)` can reach a QCall as. A real guest
    /// calling `new Signature(IRuntimeMethodInfo, RuntimeType)` produces the last of these, so a
    /// classifier that recognises only the verbatim zeros throws on the shape that actually
    /// arrives.
    let nullFieldHandleSpellings : CliType list =
        [
            CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
            CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        ]

    /// The `RuntimeFieldHandleInternal` null sentinel in the shape a real guest produces, so that
    /// `Signature_Init` dispatches on the method handle rather than refusing both-handles-non-null.
    let private nullFieldHandle : CliType =
        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))

    [<Test>]
    let ``Signature_Init stores field RuntimeType into _returnTypeORfieldType`` () : unit =
        // .NET 10 split the field-signature population: the managed Signature constructor sets
        // `_declaringType` itself before calling the QCall, which is responsible only for
        // `_returnTypeORfieldType`, `_sig`, and the calling-convention flags.
        let fixture = makeSignatureFixture ()

        let signatureAddr, expectedFieldTypeHandle, state =
            invokeSignatureInit fixture nullPCorSig (CliType.Numeric (CliNumericType.Int32 0)) None nullMethodHandle

        let fieldTypeAddr =
            match signatureField state signatureAddr "_returnTypeORfieldType" with
            | CliType.ObjectRef (Some addr) -> addr
            | other -> failwith $"Expected _returnTypeORfieldType to be a RuntimeType object ref, got %O{other}"

        let fieldTypeTarget =
            NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                "Signature_Init test"
                state
                (EvalStackValue.ObjectRef fieldTypeAddr)

        fieldTypeTarget
        |> shouldEqual (RuntimeTypeHandleTarget.Closed expectedFieldTypeHandle)

    [<Test>]
    let ``Signature_Init rejects mixed field handle and pCorSig blob inputs`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInit
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Verbatim 1L))
                    (CliType.Numeric (CliNumericType.Int32 0))
                    None
                    nullMethodHandle
                |> ignore
            )

        // 1 is not a shape any COR signature pointer arrives in — the classifier accepts a null
        // pointer or a byref over a PE byte range, and nothing else. It is rejected there rather
        // than reaching the both-inputs check, which the sibling test below covers with a *real*
        // blob pointer.
        ex.Message
        |> shouldContainText "expected a null COR signature pointer or a managed pointer over a PE byte range"

    [<Test>]
    let ``Signature_Init rejects a field handle and a method handle together`` () : unit =
        // CoreCLR's Signature_Init would silently prefer the method (`if (pMethodDesc != NULL) ...
        // else if (pFieldDesc != NULL)`), but no managed Signature constructor passes both, so a
        // value arriving here would be a PawPrint bug. Refusing is deliberately stricter than
        // upstream: preferring one input would hide it.
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInit
                    fixture
                    nullPCorSig
                    (CliType.Numeric (CliNumericType.Int32 0))
                    None
                    (closedMethodHandle fixture "Twice")
                |> ignore
            )

        ex.Message
        |> shouldContainText "Signature_Init: got both a field handle and a method handle"

    [<Test>]
    let ``Signature_Init rejects mixed field handle and non-zero cCorSig`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInit
                    fixture
                    nullPCorSig
                    (CliType.Numeric (CliNumericType.Int32 1))
                    None
                    nullMethodHandle
                |> ignore
            )

        // A handle-backed call must not carry a blob length either: CoreCLR overwrites both pCorSig
        // and cCorSig from the handle, so a non-zero length is a caller that thinks it is supplying
        // a blob.
        ex.Message
        |> shouldContainText "a handle-backed signature was given cCorSig 1, expected 0"

    [<Test>]
    let ``Signature_Init rejects a call with no handle and no blob`` () : unit =
        // CoreCLR asserts `pCorSig != NULL && cCorSig > 0` once both handles are null, so with
        // neither a handle nor a blob there is simply no input to derive a signature from. This is
        // the one remaining unreachable corner of the four-way dispatch.
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInit
                    fixture
                    nullPCorSig
                    (CliType.Numeric (CliNumericType.Int32 0))
                    (Some (CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)))
                    nullMethodHandle
                |> ignore
            )

        ex.Message |> shouldContainText "there is nothing to build a signature from"

    /// Concretize a `TypeDefn` in `state` so it can be compared against a `RuntimeType` the QCall
    /// allocated. Concretization is idempotent, so this recovers the handle the QCall used rather
    /// than minting a second one.
    let private expectedTarget
        (fixture : SignatureFixture)
        (state : IlMachineState)
        (defn : TypeDefn)
        : RuntimeTypeHandleTarget
        =
        let _, handle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.Assembly.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                defn

        RuntimeTypeHandleTarget.Closed handle

    let private runtimeTypeTargetOfField
        (state : IlMachineState)
        (signatureAddr : ManagedHeapAddress)
        (fieldName : string)
        : RuntimeTypeHandleTarget
        =
        match signatureField state signatureAddr fieldName with
        | CliType.ObjectRef (Some addr) ->
            NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                $"Signature_Init test (%s{fieldName})"
                state
                (EvalStackValue.ObjectRef addr)
        | other -> failwith $"Expected %s{fieldName} to be a RuntimeType object ref, got %O{other}"

    let private argumentTargets
        (state : IlMachineState)
        (signatureAddr : ManagedHeapAddress)
        : RuntimeTypeHandleTarget list
        =
        let arrayAddr =
            match signatureField state signatureAddr "_arguments" with
            | CliType.ObjectRef (Some addr) -> addr
            | CliType.ObjectRef None ->
                failwith
                    "Expected _arguments to be an allocated RuntimeType[]; CoreCLR allocates it even for a nullary method, and the managed Signature.Arguments getter asserts it is non-null"
            | other -> failwith $"Expected _arguments to be a RuntimeType[] object ref, got %O{other}"

        let array =
            match HeapObserver.tryGetArray arrayAddr state.ManagedHeap with
            | Some array -> array
            | None -> failwith $"_arguments pointed at %O{arrayAddr}, which is not an array"

        [
            for index in 0 .. array.Shape.Length - 1 do
                match IlMachineState.getArrayValue arrayAddr index state with
                | CliType.ObjectRef (Some addr) ->
                    NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                        "Signature_Init test (_arguments)"
                        state
                        (EvalStackValue.ObjectRef addr)
                | other -> failwith $"Expected _arguments[%d{index}] to be a RuntimeType object ref, got %O{other}"
        ]

    let private invokeMethodArm
        (fixture : SignatureFixture)
        (methodName : string)
        : ManagedHeapAddress * IlMachineState
        =
        let signatureAddr, _, state =
            invokeSignatureInit
                fixture
                nullPCorSig
                (CliType.Numeric (CliNumericType.Int32 0))
                (Some nullFieldHandle)
                (closedMethodHandle fixture methodName)

        signatureAddr, state

    [<Test>]
    let ``Signature_Init fills the method arm's return type and arguments`` () : unit =
        let fixture = makeSignatureFixture ()
        let signatureAddr, state = invokeMethodArm fixture "Twice"

        runtimeTypeTargetOfField state signatureAddr "_returnTypeORfieldType"
        |> shouldEqual (expectedTarget fixture state (TypeDefn.PrimitiveType PrimitiveType.Int32))

        // Order matters: `SetArgument(i, ...)` fills index i with the i'th fixed argument, so a
        // reversed fill would still produce a two-element array of the right types.
        argumentTargets state signatureAddr
        |> shouldEqual
            [
                expectedTarget fixture state (TypeDefn.PrimitiveType PrimitiveType.Int32)
                expectedTarget fixture state (TypeDefn.PrimitiveType PrimitiveType.String)
            ]

    [<Test>]
    let ``Signature_Init gives a void nullary method System.Void and an empty argument array`` () : unit =
        let fixture = makeSignatureFixture ()
        let signatureAddr, state = invokeMethodArm fixture "Nothing"

        let voidTarget =
            let _, _, handle =
                NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                    fixture.LoggerFactory
                    fixture.BaseClassTypes
                    state
                    "System"
                    "Void"

            RuntimeTypeHandleTarget.Closed handle

        // CoreCLR's `msig.GetRetTypeHandleThrowing()` yields System.Void's TypeHandle for a void
        // return rather than a null one, and the QCall asserts `_returnTypeORfieldType != NULL` on
        // the way out.
        runtimeTypeTargetOfField state signatureAddr "_returnTypeORfieldType"
        |> shouldEqual voidTarget

        argumentTargets state signatureAddr |> shouldEqual []

    [<TestCase("Twice", 0x1)>]
    [<TestCase("Instance", 0x21)>]
    let ``Signature_Init translates the calling convention rather than storing the raw byte``
        (methodName : string)
        (expected : int)
        : unit
        =
        // SignatureNative::SetCallingConvention (runtimehandles.h:455) maps the ECMA
        // calling-convention byte onto the managed CallingConventions bits: everything that is not
        // IMAGE_CEE_CS_CALLCONV_VARARG becomes CALLCONV_Standard (0x1), plus 0x20 for HASTHIS.
        // Storing the raw byte would give 0x0 / 0x20 here, since IMAGE_CEE_CS_CALLCONV_DEFAULT is 0.
        let fixture = makeSignatureFixture ()
        let signatureAddr, state = invokeMethodArm fixture methodName

        match signatureField state signatureAddr "_managedCallingConventionAndArgIteratorFlags" with
        | CliType.Numeric (CliNumericType.Int32 actual) -> actual |> shouldEqual expected
        | other -> failwith $"Expected _managedCallingConventionAndArgIteratorFlags to be Int32, got %O{other}"

    [<Test>]
    let ``Signature_Init points _sig at the MethodDef signature blob`` () : unit =
        let fixture = makeSignatureFixture ()
        let signatureAddr, state = invokeMethodArm fixture "Twice"

        let expectedHandle =
            ComparableMethodDefinitionHandle.Make
                (MethodInfo.requireMetadata "test" (requiredHostMethod fixture "Twice")).Handle

        let expectedSize =
            let mdReader = fixture.Assembly.PeReader.GetMetadataReader ()
            let methodDef = mdReader.GetMethodDefinition expectedHandle.Get
            mdReader.GetBlobReader(methodDef.Signature).Length

        let peByteRange =
            match signatureField state signatureAddr "_sig" |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange,
                                                                                             _))) -> peByteRange
            | other -> failwith $"Expected _sig to be a byref over a PE byte range, got %O{other}"

        peByteRange.Source
        |> shouldEqual (PeByteRangePointerSource.MethodSignatureBlob expectedHandle)

        peByteRange.Size |> shouldEqual expectedSize

        // CoreCLR asserts `cCorSig > 0` on the way out, and every downstream reader
        // (GetParameterOffsetInternal, GetCustomModifiersAtOffset) cross-checks `_csig` against the
        // real blob length before walking it.
        match signatureField state signatureAddr "_csig" with
        | CliType.Numeric (CliNumericType.Int32 csig) -> csig |> shouldEqual expectedSize
        | other -> failwith $"Expected _csig to be Int32, got %O{other}"

    [<Test>]
    let ``Signature_Init stores the method handle into _pMethod`` () : unit =
        // `_pMethod` is what SignatureNative::GetTypeContext branches on to decide whether a
        // signature's type context carries a method instantiation, so it has to survive the QCall.
        let fixture = makeSignatureFixture ()
        let expectedHandle, _ = closedMethodHandle fixture "Twice" fixture.State
        let signatureAddr, state = invokeMethodArm fixture "Twice"

        signatureField state signatureAddr "_pMethod"
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual (CliType.unwrapPrimitiveLikeDeep expectedHandle)

    [<Test>]
    let ``Signature_Init resolves a generic method definition's own variables`` () : unit =
        // The handle the introduced-method iterator mints for `Generic<T>` has empty MethodGenerics
        // even though the method declares one type parameter -- which is how a handle spells the
        // typical instantiation. CoreCLR resolves the signature against it, so `!!0` has to come
        // back as the RuntimeType for the method's own `T` rather than being substituted away.
        let fixture = makeSignatureFixture ()

        let signatureAddr, _, state =
            invokeSignatureInit
                fixture
                nullPCorSig
                (CliType.Numeric (CliNumericType.Int32 0))
                (Some nullFieldHandle)
                (openMethodHandle fixture "Generic")

        let methodInfo = requiredHostMethod fixture "Generic"

        let expected =
            RuntimeTypeHandleTarget.MethodGenericParameter (
                (methodInfo.TryDeclaringType |> Option.get).Identity,
                ComparableMethodDefinitionHandle.Make (MethodInfo.requireMetadata "test" methodInfo).Handle,
                0
            )

        // Parameter and return alike: `T Generic<T>(T t)` spells `!!0` in both positions, and one
        // walk answers both.
        argumentTargets state signatureAddr |> shouldEqual [ expected ]

        runtimeTypeTargetOfField state signatureAddr "_returnTypeORfieldType"
        |> shouldEqual expected

    [<TestCaseSource(nameof nullFieldHandleSpellings)>]
    let ``Signature_Init reaches the method arm for every spelling of a null field handle``
        (nullFieldHandle : CliType)
        : unit
        =
        // The four-way dispatch classifies *both* handles before choosing an arm, so the field
        // classifier has to answer "no field handle" for whichever zero shape the guest's
        // `default(RuntimeFieldHandleInternal)` happens to be, rather than throwing on the ones it
        // does not list.
        let fixture = makeSignatureFixture ()

        let signatureAddr, _, state =
            invokeSignatureInit
                fixture
                nullPCorSig
                (CliType.Numeric (CliNumericType.Int32 0))
                (Some nullFieldHandle)
                (closedMethodHandle fixture "Twice")

        runtimeTypeTargetOfField state signatureAddr "_returnTypeORfieldType"
        |> shouldEqual (expectedTarget fixture state (TypeDefn.PrimitiveType PrimitiveType.Int32))

    /// Build the closed `GenericFieldHost<DistinctiveFieldType>` handle plus a `RuntimeType` for it,
    /// which is what a real `MdFieldInfo` would carry in `Signature._declaringType`.
    let private closedHostDeclaringType
        (fixture : SignatureFixture)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let distinctiveDefn =
            TypeDefn.FromDefinition (fixture.DistinctiveType.Identity, SignatureTypeKind.Class)

        let state, distinctiveHandle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.Assembly.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                distinctiveDefn

        let closedHostDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (fixture.HostType.Identity, SignatureTypeKind.Class),
                ImmutableArray.Create distinctiveDefn
            )

        let state, closedHostHandle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.Assembly.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                closedHostDefn

        let declaringTypeAddr, state =
            IlMachineState.getOrAllocateType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed closedHostHandle)
                state

        declaringTypeAddr, distinctiveHandle, state

    /// The PE byte range over `GenericFieldHost&lt;T&gt;.Payload`'s COR signature blob, and a pointer
    /// over it in the shape `MetadataImport.GetSigOfFieldDef` hands back.
    let private payloadSignatureBlobPointer
        (fixture : SignatureFixture)
        (state : IlMachineState)
        : PeByteRangePointer * ManagedPointerSource * IlMachineState
        =
        let peByteRange =
            IlMachineState.peByteRangeForFieldSignatureBlob fixture.Assembly fixture.Field.Handle

        let state, pointer =
            IlMachineState.peByteRangePointer fixture.LoggerFactory fixture.BaseClassTypes peByteRange state

        peByteRange, pointer, state

    /// Drive `Signature_Init` with no field or method handle and a raw blob pointer — the shape
    /// `new Signature(void*, int, RuntimeType)` produces. `declaringType` is preset on the freshly
    /// allocated `Signature` because the managed constructor sets it before calling in, and CoreCLR
    /// asserts it is non-null.
    let private invokeSignatureInitRawBlob
        (fixture : SignatureFixture)
        (declaringType : CliType)
        (pCorSig : CliType)
        (cCorSig : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, signatureType, signatureTypeHandle, signatureInitMethod =
            signatureInitMethod fixture state

        let signatureAddr, state =
            allocateZeroInitializedObject fixture fixture.BaseClassTypes.Corelib signatureType signatureTypeHandle state

        let state =
            let signatureObj = ManagedHeap.get signatureAddr state.ManagedHeap

            let declaringTypeFieldId =
                IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_declaringType"

            let signatureObj =
                AllocatedNonArrayObject.SetFieldById declaringTypeFieldId declaringType signatureObj

            { state with
                ManagedHeap = ManagedHeap.set signatureAddr signatureObj state.ManagedHeap
            }

        let signatureRefSlot, state =
            allocateObjectRefSlot fixture (CliType.ObjectRef (Some signatureAddr)) state

        let objectHandleOnStack, state =
            objectHandleOnStackValue fixture signatureRefSlot state

        let methodArgs =
            ImmutableArray.CreateRange
                [
                    objectHandleOnStack
                    pCorSig
                    CliType.Numeric (CliNumericType.Int32 cCorSig)
                    nullFieldHandle
                    CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
                ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    signatureInitMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating Signature_Init frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.BaseClassTypes.Corelib
                TargetType = signatureType
            }

        let state =
            match NativeSignature.tryExecuteQCall "Signature_Init" ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected Signature_Init execution result: %O{result}"
            | None -> failwith "Signature_Init did not match"

        signatureAddr, state

    [<Test>]
    let ``Signature_Init raw blob resolves the field type against _declaringType`` () : unit =
        // `GenericFieldHost<T>.Payload` is declared `VAR 0`, so the blob alone cannot say what the
        // field's type is. CoreCLR builds its SigTypeContext from `_declaringType`
        // (SigTypeContext::InitTypeContext(declType)), *not* from the definition that owns the blob
        // -- so with `_declaringType = GenericFieldHost<DistinctiveFieldType>` the answer must be
        // DistinctiveFieldType. An implementation that took generics from the FieldDef's own
        // declaring TypeDef would see the open generic, get an empty instantiation, and fault.
        let fixture = makeSignatureFixture ()

        let declaringTypeAddr, distinctiveHandle, state =
            closedHostDeclaringType fixture fixture.State

        let peByteRange, sigPointer, state = payloadSignatureBlobPointer fixture state

        let signatureAddr, state =
            invokeSignatureInitRawBlob
                fixture
                (CliType.ObjectRef (Some declaringTypeAddr))
                (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))
                peByteRange.Size
                state

        runtimeTypeTargetOfField state signatureAddr "_returnTypeORfieldType"
        |> shouldEqual (RuntimeTypeHandleTarget.Closed distinctiveHandle)

        // `_sig` is the caller's own pointer, verbatim, as CoreCLR assigns it -- so the blob's
        // provenance survives for the later byte-level readers.
        match signatureField state signatureAddr "_sig" |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.RuntimePointer (CliRuntimePointer.Managed actual) -> actual |> shouldEqual sigPointer
        | other -> failwith $"Expected _sig to be a managed pointer, got %O{other}"

        match signatureField state signatureAddr "_csig" |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.Numeric (CliNumericType.Int32 actual) -> actual |> shouldEqual peByteRange.Size
        | other -> failwith $"Expected _csig to be Int32, got %O{other}"

    /// Both encodings of the same non-null blob pointer. Only the first is produced by the live
    /// route today (`ConstArray.m_constArray` through `MdFieldInfo.FieldType`, confirmed by
    /// mutation); the second is here for the same reason `nullFieldHandleSpellings` exists --
    /// `unwrapPrimitiveLikeDeep` does not canonicalise between them, so a classifier that lists one
    /// throws on the other.
    let corSigPointerSpellings : (ManagedPointerSource -> CliType) list =
        [
            (fun ptr -> CliType.RuntimePointer (CliRuntimePointer.Managed ptr))
            (fun ptr -> CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)))
        ]

    [<TestCaseSource(nameof corSigPointerSpellings)>]
    let ``Signature_Init raw blob accepts every spelling of the blob pointer``
        (spell : ManagedPointerSource -> CliType)
        : unit
        =
        let fixture = makeSignatureFixture ()

        let declaringTypeAddr, distinctiveHandle, state =
            closedHostDeclaringType fixture fixture.State

        let peByteRange, sigPointer, state = payloadSignatureBlobPointer fixture state

        let signatureAddr, state =
            invokeSignatureInitRawBlob
                fixture
                (CliType.ObjectRef (Some declaringTypeAddr))
                (spell sigPointer)
                peByteRange.Size
                state

        runtimeTypeTargetOfField state signatureAddr "_returnTypeORfieldType"
        |> shouldEqual (RuntimeTypeHandleTarget.Closed distinctiveHandle)

    [<Test>]
    let ``Signature_Init leaves the calling convention alone for a raw field blob`` () : unit =
        // CoreCLR's FIELD arm is `msig.NextArgNormalized(); SetReturnType(...)`; only the
        // method-shaped `else` branch calls SetCallingConvention. A field-backed Signature therefore
        // keeps the zero it was allocated with, and 0x6 -- the raw ECMA FIELD byte -- is not even a
        // legal CallingConventions value.
        let fixture = makeSignatureFixture ()

        let declaringTypeAddr, _, state = closedHostDeclaringType fixture fixture.State
        let peByteRange, sigPointer, state = payloadSignatureBlobPointer fixture state

        let signatureAddr, state =
            invokeSignatureInitRawBlob
                fixture
                (CliType.ObjectRef (Some declaringTypeAddr))
                (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))
                peByteRange.Size
                state

        match signatureField state signatureAddr "_managedCallingConventionAndArgIteratorFlags" with
        | CliType.Numeric (CliNumericType.Int32 actual) -> actual |> shouldEqual 0
        | other -> failwith $"Expected _managedCallingConventionAndArgIteratorFlags to be Int32, got %O{other}"

    [<Test>]
    let ``Signature_Init leaves the calling convention alone for a field handle`` () : unit =
        // The handle-backed field path shares CoreCLR's common tail with the raw-blob one, so it
        // must agree: two Signatures describing the same field cannot report different calling
        // conventions depending on which constructor built them.
        let fixture = makeSignatureFixture ()

        let signatureAddr, _, state =
            invokeSignatureInit fixture nullPCorSig (CliType.Numeric (CliNumericType.Int32 0)) None nullMethodHandle

        match signatureField state signatureAddr "_managedCallingConventionAndArgIteratorFlags" with
        | CliType.Numeric (CliNumericType.Int32 actual) -> actual |> shouldEqual 0
        | other -> failwith $"Expected _managedCallingConventionAndArgIteratorFlags to be Int32, got %O{other}"

    [<Test>]
    let ``Signature_Init raw blob refuses a null declaring type`` () : unit =
        // CoreCLR asserts `!declType.IsNull()`: every managed constructor sets `_declaringType`
        // before calling in, and there is no fallback to derive the type context from.
        let fixture = makeSignatureFixture ()

        let peByteRange, sigPointer, state =
            payloadSignatureBlobPointer fixture fixture.State

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInitRawBlob
                    fixture
                    (CliType.ObjectRef None)
                    (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))
                    peByteRange.Size
                    state
                |> ignore
            )

        ex.Message |> shouldContainText "Signature._declaringType was null"

    [<Test>]
    let ``Signature_Init raw blob refuses a cCorSig that disagrees with the blob`` () : unit =
        let fixture = makeSignatureFixture ()
        let declaringTypeAddr, _, state = closedHostDeclaringType fixture fixture.State
        let peByteRange, sigPointer, state = payloadSignatureBlobPointer fixture state

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInitRawBlob
                    fixture
                    (CliType.ObjectRef (Some declaringTypeAddr))
                    (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))
                    (peByteRange.Size + 1)
                    state
                |> ignore
            )

        ex.Message |> shouldContainText "does not match the"

    [<Test>]
    let ``Signature_Init raw blob refuses a pointer with no signature-blob provenance`` () : unit =
        // The arm does not parse the blob; it recovers the FieldDef the pointer names. A pointer
        // that is merely "some bytes" cannot answer that, and guessing would be worse than failing.
        let fixture = makeSignatureFixture ()
        let declaringTypeAddr, _, state = closedHostDeclaringType fixture fixture.State

        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Byte

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero byteHandle)
                (fun () -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                2
                state

        let anonymousPointer =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInitRawBlob
                    fixture
                    (CliType.ObjectRef (Some declaringTypeAddr))
                    (CliType.RuntimePointer (CliRuntimePointer.Managed anonymousPointer))
                    2
                    state
                |> ignore
            )

        ex.Message |> shouldContainText "managed pointer over a PE byte range"

    [<Test>]
    let ``Signature_Init refuses a blob alongside a field handle`` () : unit =
        // CoreCLR overwrites the caller's blob from the handle, so no managed constructor passes
        // both; silently preferring one would hide the bug that produced it.
        let fixture = makeSignatureFixture ()

        let peByteRange, sigPointer, state =
            payloadSignatureBlobPointer fixture fixture.State

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInit
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))
                    (CliType.Numeric (CliNumericType.Int32 peByteRange.Size))
                    None
                    nullMethodHandle
                |> ignore
            )

        ex.Message |> shouldContainText "no managed Signature constructor passes both"
