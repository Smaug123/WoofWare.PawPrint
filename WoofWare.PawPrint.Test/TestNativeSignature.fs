namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

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

            initialState.WithLoadedAssembly corelib.Name corelib

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
                closedHostHandle
                fixture.Field.Handle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = fieldHandles
            }

        let stubAddress = runtimeFieldInfoStubAddress runtimeFieldHandle
        let stub = ManagedHeap.get stubAddress state.ManagedHeap

        runtimeFieldHandleInternalInRuntimeFieldInfoStub stub, distinctiveHandle, state

    let private signatureGetSignatureMethod
        (fixture : SignatureFixture)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          ConcreteTypeHandle *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let signatureType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System" "Signature"

        let rawMethod =
            signatureType.Methods
            |> List.filter (fun method ->
                method.Name = "GetSignature"
                && not method.IsStatic
                && method.Parameters.Length = 5
                && method.Signature.ReturnType = MethodReturnType.Void
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "Signature.GetSignature native method not found"
                | methods -> failwith $"Signature.GetSignature native method was ambiguous: %d{methods.Length} matches"

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
            |> CliValueType.OfFields fixture.BaseClassTypes state.ConcreteTypes typeHandle typeInfo.Layout

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

    let private invokeGetSignature
        (fixture : SignatureFixture)
        (pCorSig : CliType)
        (cCorSig : CliType)
        (fieldHandle : CliType option)
        (methodHandle : ManagedHeapAddress -> CliType)
        (declaringTypeOverride : CliType option)
        : ManagedHeapAddress * ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let fieldHandleInternal, expectedFieldTypeHandle, state =
            closedGenericFieldHandle fixture fixture.State

        let fieldHandleInternal = fieldHandle |> Option.defaultValue fieldHandleInternal

        let state, signatureType, signatureTypeHandle, getSignatureMethod =
            signatureGetSignatureMethod fixture state

        let signatureAddr, state =
            allocateZeroInitializedObject fixture fixture.BaseClassTypes.Corelib signatureType signatureTypeHandle state

        let declaringTypeAddr, state =
            IlMachineState.getOrAllocateType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed signatureTypeHandle)
                state

        let declaringTypeArg =
            declaringTypeOverride
            |> Option.defaultValue (CliType.ObjectRef (Some declaringTypeAddr))

        let methodArgs =
            ImmutableArray.CreateRange
                [
                    CliType.ObjectRef (Some signatureAddr)
                    pCorSig
                    cCorSig
                    fieldHandleInternal
                    methodHandle declaringTypeAddr
                    declaringTypeArg
                ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    getSignatureMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating Signature.GetSignature frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                Implementations = MockEnv.make ()
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.BaseClassTypes.Corelib
                TargetType = signatureType
            }

        let state =
            match NativeDispatch.tryExecute ctx with
            | Some (ExecutionResult.Stepped (state, WhatWeDid.Executed)) -> state
            | Some result -> failwith $"unexpected Signature.GetSignature execution result: %O{result}"
            | None -> failwith "Signature.GetSignature did not match"

        signatureAddr, declaringTypeAddr, expectedFieldTypeHandle, state

    [<Test>]
    let ``GetSignature stores field RuntimeType and preserves caller declaring type`` () : unit =
        let fixture = makeSignatureFixture ()

        let signatureAddr, declaringTypeAddr, expectedFieldTypeHandle, state =
            invokeGetSignature
                fixture
                (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                (CliType.Numeric (CliNumericType.Int32 0))
                None
                (fun _declaringTypeAddr -> CliType.ObjectRef None)
                None

        signatureField state signatureAddr "m_declaringType"
        |> shouldEqual (CliType.ObjectRef (Some declaringTypeAddr))

        let fieldTypeAddr =
            match signatureField state signatureAddr "m_returnTypeORfieldType" with
            | CliType.ObjectRef (Some addr) -> addr
            | other -> failwith $"Expected m_returnTypeORfieldType to be a RuntimeType object ref, got %O{other}"

        let fieldTypeTarget =
            NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                "Signature.GetSignature test"
                state
                (EvalStackValue.ObjectRef fieldTypeAddr)

        fieldTypeTarget
        |> shouldEqual (RuntimeTypeHandleTarget.Closed expectedFieldTypeHandle)

    [<Test>]
    let ``GetSignature rejects mixed field handle and pCorSig blob inputs`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSignature
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Verbatim 1L))
                    (CliType.Numeric (CliNumericType.Int32 0))
                    None
                    (fun _declaringTypeAddr -> CliType.ObjectRef None)
                    None
                |> ignore
            )

        ex.Message
        |> shouldContainText "TODO: Signature.GetSignature pCorSig blob parsing is not implemented"

    [<Test>]
    let ``GetSignature rejects mixed field handle and method handle inputs`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSignature
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                    (CliType.Numeric (CliNumericType.Int32 0))
                    None
                    (fun declaringTypeAddr -> CliType.ObjectRef (Some declaringTypeAddr))
                    None
                |> ignore
            )

        ex.Message
        |> shouldContainText "TODO: Signature.GetSignature method signature parsing is not implemented"

    [<Test>]
    let ``GetSignature rejects mixed field handle and non-zero cCorSig`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSignature
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                    (CliType.Numeric (CliNumericType.Int32 1))
                    None
                    (fun _declaringTypeAddr -> CliType.ObjectRef None)
                    None
                |> ignore
            )

        ex.Message
        |> shouldContainText "TODO: Signature.GetSignature pCorSig blob parsing is not implemented; got cCorSig 1"

    [<Test>]
    let ``GetSignature rejects null field handle as non-field signature parsing`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSignature
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                    (CliType.Numeric (CliNumericType.Int32 0))
                    (Some (CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)))
                    (fun _declaringTypeAddr -> CliType.ObjectRef None)
                    None
                |> ignore
            )

        ex.Message
        |> shouldContainText
            "TODO: Signature.GetSignature non-field signature parsing is not implemented; fieldHandle was null"

    [<Test>]
    let ``GetSignature rejects null declaring type`` () : unit =
        let fixture = makeSignatureFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSignature
                    fixture
                    (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                    (CliType.Numeric (CliNumericType.Int32 0))
                    None
                    (fun _declaringTypeAddr -> CliType.ObjectRef None)
                    (Some (CliType.ObjectRef None))
                |> ignore
            )

        ex.Message
        |> shouldContainText
            "Signature.GetSignature: declaringType was null; the field-backed slice has no fallback for null declaring types"
