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

    let private signatureSource =
        """
public sealed class DistinctiveFieldType
{
}

public sealed class GenericFieldHost<T>
{
    public T Payload;
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
            typeInfo.Assembly
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
                fixture.Assembly.Name
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
                fixture.Assembly.Name
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
                fixture.Assembly.Name
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
                match method.NativeImport with
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
                fixture.BaseClassTypes.Corelib.Name
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
                typeInfo.Layout
                (CharSetMetadata.ofTypeAttributes typeInfo.TypeAttributes)

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
                fixture.BaseClassTypes.Corelib.Name
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
        (methodHandle : CliType)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let fieldHandleInternal, expectedFieldTypeHandle, state =
            closedGenericFieldHandle fixture fixture.State

        let fieldHandleInternal =
            fieldHandleOverride |> Option.defaultValue fieldHandleInternal

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
                ThreadState = Map.empty |> Map.add thread (ThreadState.New (CpuId 0) methodState)
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

    let private nullMethodHandle : CliType = CliType.ObjectRef None

    [<Test>]
    let ``Signature_Init stores field RuntimeType into _returnTypeORfieldType`` () : unit =
        // .NET 10 split the field-signature population: the managed Signature constructor sets
        // `_declaringType` itself before calling the QCall, so we no longer assert on it here.
        // The QCall is now responsible only for `_returnTypeORfieldType`, `_sig`, and the
        // calling-convention flags.
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

        ex.Message
        |> shouldContainText "TODO: Signature_Init pCorSig blob parsing is not implemented"

    [<Test>]
    let ``Signature_Init rejects mixed field handle and method handle inputs`` () : unit =
        let fixture = makeSignatureFixture ()

        // requireNullMethodHandle accepts ObjectRef None / null pointers / NativeInt 0; any
        // non-null primitive triggers the TODO.
        let nonNullMethodHandle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1L))

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeSignatureInit
                    fixture
                    nullPCorSig
                    (CliType.Numeric (CliNumericType.Int32 0))
                    None
                    nonNullMethodHandle
                |> ignore
            )

        ex.Message
        |> shouldContainText "TODO: Signature_Init method signature parsing is not implemented"

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

        ex.Message
        |> shouldContainText "TODO: Signature_Init pCorSig blob parsing is not implemented; got cCorSig 1"

    [<Test>]
    let ``Signature_Init rejects null field handle as non-field signature parsing`` () : unit =
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

        ex.Message
        |> shouldContainText "TODO: Signature_Init non-field signature parsing is not implemented; fieldHandle was null"
