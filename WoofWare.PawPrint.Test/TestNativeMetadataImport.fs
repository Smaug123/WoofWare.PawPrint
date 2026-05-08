namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

[<TestFixture>]
module TestNativeMetadataImport =
    let private metadataImportSource =
        """
public class MetadataFields
{
    public int InstanceField;
    public static string StaticField;
    public const int LiteralField = 7;
}

public class EmptyMetadataFields
{
}

public class ManyMetadataFields
{
    public int Field00;
    public int Field01;
    public int Field02;
    public int Field03;
    public int Field04;
    public int Field05;
    public int Field06;
    public int Field07;
    public int Field08;
    public int Field09;
    public int Field10;
    public int Field11;
    public int Field12;
    public int Field13;
    public int Field14;
    public int Field15;
    public int Field16;
}

public class GenericMetadataFields<T>
{
    public T GenericField;
    public int Count;
}

[System.Obsolete]
public class HasParameterlessAttribute
{
}

[System.Obsolete("deprecated")]
public class HasArgumentAttribute
{
}
"""

    type private MetadataImportFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assembly : DumpedAssembly
            TargetType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            EmptyType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ManyFieldsType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            GenericType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ParameterlessAttrType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ArgumentAttrType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            InstanceField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            StaticField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            LiteralField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            ConstArrayType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ConstArrayHandle : ConcreteTypeHandle
            ByteHandle : ConcreteTypeHandle
            State : IlMachineState
        }

    [<RequireQualifiedAccess>]
    type private EnumResultStorage =
        | ShortResult
        | LongResult

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    let private concretizeCorelibType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState
        =
        let typeDefn =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

        let state, _ =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                typeDefn

        state

    let private makeFixture () : MetadataImportFixture =
        let image =
            Roslyn.compileAssembly
                "MetadataImportTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ metadataImportSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let targetType = requiredTopLevelType assembly "" "MetadataFields"
        let emptyType = requiredTopLevelType assembly "" "EmptyMetadataFields"
        let manyFieldsType = requiredTopLevelType assembly "" "ManyMetadataFields"
        let genericType = requiredTopLevelType assembly "" "GenericMetadataFields`1"

        let parameterlessAttrType =
            requiredTopLevelType assembly "" "HasParameterlessAttribute"

        let argumentAttrType = requiredTopLevelType assembly "" "HasArgumentAttribute"
        let constArrayType = requiredTopLevelType corelib "System.Reflection" "ConstArray"

        let fieldByName (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
            targetType.Fields |> List.find (fun field -> field.Name = name)

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib.Name corelib

        let state =
            (state,
             [
                 baseClassTypes.Object
                 baseClassTypes.Int32
                 baseClassTypes.IntPtr
                 baseClassTypes.Byte
             ])
            ||> List.fold (concretizeCorelibType loggerFactory baseClassTypes)

        let state, constArrayHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (constArrayType.Identity, SignatureTypeKind.ValueType))

        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Assembly = assembly
            TargetType = targetType
            EmptyType = emptyType
            ManyFieldsType = manyFieldsType
            GenericType = genericType
            ParameterlessAttrType = parameterlessAttrType
            ArgumentAttrType = argumentAttrType
            InstanceField = fieldByName "InstanceField"
            StaticField = fieldByName "StaticField"
            LiteralField = fieldByName "LiteralField"
            ConstArrayType = constArrayType
            ConstArrayHandle = constArrayHandle
            ByteHandle = byteHandle
            State = state
        }

    let private metadataImportMethod
        (fixture : MetadataImportFixture)
        (state : IlMachineState)
        (methodName : string)
        (parameterCount : int)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let metadataImportType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System.Reflection" "MetadataImport"

        let rawMethod =
            metadataImportType.Methods
            |> List.filter (fun method -> method.Name = methodName && method.Parameters.Length = parameterCount)
            |> function
                | [ method ] -> method
                | [] ->
                    failwith $"MetadataImport method %s{methodName} with %d{parameterCount} parameters was not found"
                | methods ->
                    failwith
                        $"MetadataImport method %s{methodName} with %d{parameterCount} parameters was ambiguous: %d{methods.Length} matches"

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

        state, metadataImportType, method

    let private allocateInt32Buffer
        (fixture : MetadataImportFixture)
        (length : int)
        (value : int32)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Int32

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 value))
                length
                state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private allocateInt32Out
        (fixture : MetadataImportFixture)
        (value : int32)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        allocateInt32Buffer fixture 1 value state

    let private readInt32Out (state : IlMachineState) (ptr : ManagedPointerSource) : int32 =
        match IlMachineState.readManagedByref state ptr |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.Numeric (CliNumericType.Int32 value) -> value
        | other -> failwith $"expected Int32 out value, got %O{other}"

    let private readInt32BufferElement (state : IlMachineState) (ptr : ManagedPointerSource) (index : int) : int32 =
        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, baseIndex), []) ->
            match
                ManagedHeap.getArrayValue arrayAddr (baseIndex + index) state.ManagedHeap
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.Numeric (CliNumericType.Int32 value) -> value
            | other -> failwith $"expected Int32 buffer element, got %O{other}"
        | other -> failwith $"expected ArrayElement Int32 buffer, got %O{other}"

    let private typeDefToken (handle : TypeDefinitionHandle) : int32 =
        let handle : EntityHandle = TypeDefinitionHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private fieldDefToken (handle : FieldDefinitionHandle) : int32 =
        let handle : EntityHandle = FieldDefinitionHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private fieldDefTokens (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : int32 list =
        typeInfo.Fields |> List.map (fun field -> fieldDefToken field.Handle)

    let private allocateObjectOut
        (fixture : MetadataImportFixture)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                1
                state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private objectHandleOnStack
        (fixture : MetadataImportFixture)
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

    let private metadataImportHandle (fixture : MetadataImportFixture) : CliType =
        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MetadataImportHandle fixture.Assembly.Name.FullName))

    let private invokeMetadataImportNative
        (fixture : MetadataImportFixture)
        (metadataImportType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (arguments : CliType list)
        (state : IlMachineState)
        : IlMachineState
        =
        let thread = ThreadId 0

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.CreateRange arguments)
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating MetadataImport frame: %O{missing}"

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
                TargetType = metadataImportType
            }

        match NativeMetadataImport.tryExecute ctx with
        | Some (ExecutionResult.Stepped (state, WhatWeDid.Executed)) -> state
        | Some result -> failwith $"unexpected MetadataImport execution result: %O{result}"
        | None -> failwith "MetadataImport native method did not match"

    let private invokeEnumFields
        (fixture : MetadataImportFixture)
        (targetType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : int32 * int32 list * EnumResultStorage * IlMachineState
        =
        let state, metadataImportType, enumMethod =
            metadataImportMethod fixture state "<Enum>g____PInvoke|8_0" 6

        let lengthOut, state = allocateInt32Out fixture -1 state
        let shortResult, state = allocateInt32Buffer fixture 16 0 state
        let longResult, state = allocateObjectOut fixture state
        let longResultHandle, state = objectHandleOnStack fixture longResult state

        let fieldDefTokenType = 0x04000000
        let parent = typeDefToken targetType.TypeDefHandle

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                enumMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 fieldDefTokenType)
                    CliType.Numeric (CliNumericType.Int32 parent)
                    CliType.RuntimePointer (CliRuntimePointer.Managed lengthOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed shortResult)
                    longResultHandle
                ]
                state

        let length = readInt32Out state lengthOut

        let tokens, storage =
            match IlMachineState.readManagedByref state longResult with
            | CliType.ObjectRef (Some arrayAddr) ->
                let tokens =
                    [ 0 .. int length - 1 ]
                    |> List.map (fun index ->
                        match
                            ManagedHeap.getArrayValue arrayAddr index state.ManagedHeap
                            |> CliType.unwrapPrimitiveLikeDeep
                        with
                        | CliType.Numeric (CliNumericType.Int32 token) -> token
                        | other -> failwith $"expected Int32 token in long result, got %O{other}"
                    )

                tokens, EnumResultStorage.LongResult
            | CliType.ObjectRef None ->
                [ 0 .. int length - 1 ] |> List.map (readInt32BufferElement state shortResult),
                EnumResultStorage.ShortResult
            | other -> failwith $"expected object reference in long result slot, got %O{other}"

        length, tokens, storage, state

    let private invokeGetFieldDefProps
        (fixture : MetadataImportFixture)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : EvalStackValue * int32 * IlMachineState
        =
        let state, metadataImportType, getFieldDefPropsMethod =
            metadataImportMethod fixture state "GetFieldDefProps" 3

        let attributesOut, state = allocateInt32Out fixture 0 state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getFieldDefPropsMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 (fieldDefToken field.Handle))
                    CliType.RuntimePointer (CliRuntimePointer.Managed attributesOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state
        returnValue, readInt32Out state attributesOut, state

    let private allocateConstArrayOut
        (fixture : MetadataImportFixture)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes fixture.ConstArrayHandle

        let arrayAddr, state =
            IlMachineState.allocateArray fixture.ConstArrayHandle (fun () -> zero) 1 state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private readConstArrayOut
        (fixture : MetadataImportFixture)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : int32 * byte array
        =
        let cli = IlMachineState.readManagedByref state ptr

        let valueType =
            match cli with
            | CliType.ValueType vt -> vt
            | other -> failwith $"expected ConstArray ValueType, got %O{other}"

        let lengthFieldId =
            IlMachineState.requiredOwnInstanceFieldId state fixture.ConstArrayHandle "m_length"

        let pointerFieldId =
            IlMachineState.requiredOwnInstanceFieldId state fixture.ConstArrayHandle "m_constArray"

        let length =
            match
                CliValueType.DereferenceFieldById lengthFieldId valueType
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.Numeric (CliNumericType.Int32 n) -> n
            | other -> failwith $"expected Int32 ConstArray.m_length, got %O{other}"

        let bytes =
            match
                CliValueType.DereferenceFieldById pointerFieldId valueType
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) ->
                if length = 0 then
                    [||]
                else
                    failwith $"ConstArray with length %d{length} but null pointer"
            | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr,
                                                                                                                     baseIndex),
                                                                                             []))) ->
                Array.init
                    length
                    (fun i ->
                        match
                            ManagedHeap.getArrayValue arrayAddr (baseIndex + i) state.ManagedHeap
                            |> CliType.unwrapPrimitiveLikeDeep
                        with
                        | CliType.Numeric (CliNumericType.UInt8 b) -> b
                        | other -> failwith $"expected UInt8 in ConstArray storage, got %O{other}"
                    )
            | other -> failwith $"expected managed byref for ConstArray.m_constArray, got %O{other}"

        length, bytes

    let private invokeGetCustomAttributeProps
        (fixture : MetadataImportFixture)
        (attrToken : int32)
        (state : IlMachineState)
        : EvalStackValue * int32 * (int32 * byte array) * IlMachineState
        =
        let state, metadataImportType, getCustomAttributePropsMethod =
            metadataImportMethod fixture state "GetCustomAttributeProps" 4

        let ctorOut, state = allocateInt32Out fixture 0 state
        let signatureOut, state = allocateConstArrayOut fixture state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getCustomAttributePropsMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 attrToken)
                    CliType.RuntimePointer (CliRuntimePointer.Managed ctorOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed signatureOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state
        let ctorToken = readInt32Out state ctorOut
        let constArray = readConstArrayOut fixture state signatureOut
        returnValue, ctorToken, constArray, state

    let private singleCustomAttributeForType
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : int32 * WoofWare.PawPrint.CustomAttribute
        =
        let parentToken =
            let handle : EntityHandle = TypeDefinitionHandle.op_Implicit typeInfo.TypeDefHandle
            MetadataTokens.GetToken handle

        let tokens =
            match assembly.CustomAttributesByParentToken.TryGetValue parentToken with
            | true, t -> t
            | false, _ -> failwith $"no CustomAttributes for parent token 0x%08x{parentToken}"

        match tokens.Length with
        | 1 -> ()
        | n -> failwith $"expected exactly one CustomAttribute for parent token 0x%08x{parentToken}, got %d{n}"

        let attrToken = tokens.[0]

        let attrHandle =
            match MetadataToken.ofInt attrToken with
            | MetadataToken.CustomAttribute h -> h
            | other -> failwith $"expected CustomAttribute token, got %O{other}"

        attrToken, assembly.Attributes.[attrHandle]

    [<Test>]
    let ``MetadataImport Enum returns FieldDef tokens for TypeDef`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumFields fixture fixture.TargetType fixture.State

        length |> shouldEqual 3
        storage |> shouldEqual EnumResultStorage.ShortResult

        tokens
        |> shouldEqual
            [
                fieldDefToken fixture.InstanceField.Handle
                fieldDefToken fixture.StaticField.Handle
                fieldDefToken fixture.LiteralField.Handle
            ]

    [<Test>]
    let ``MetadataImport Enum returns empty FieldDef list for type without fields`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumFields fixture fixture.EmptyType fixture.State

        length |> shouldEqual 0
        tokens |> shouldEqual []
        storage |> shouldEqual EnumResultStorage.ShortResult

    [<Test>]
    let ``MetadataImport Enum uses large result for more than sixteen FieldDef tokens`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumFields fixture fixture.ManyFieldsType fixture.State

        length |> shouldEqual 17
        storage |> shouldEqual EnumResultStorage.LongResult
        tokens |> shouldEqual (fieldDefTokens fixture.ManyFieldsType)

    [<Test>]
    let ``MetadataImport Enum returns generic TypeDef field tokens`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumFields fixture fixture.GenericType fixture.State

        length |> shouldEqual 2
        storage |> shouldEqual EnumResultStorage.ShortResult
        tokens |> shouldEqual (fieldDefTokens fixture.GenericType)

    [<Test>]
    let ``MetadataImport GetFieldDefProps writes metadata field attributes`` () : unit =
        let fixture = makeFixture ()

        let returnValue, instanceAttributes, state =
            invokeGetFieldDefProps fixture fixture.InstanceField fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 0)

        instanceAttributes
        |> shouldEqual (int32 System.Reflection.FieldAttributes.Public)

        let returnValue, staticAttributes, state =
            invokeGetFieldDefProps fixture fixture.StaticField state

        returnValue |> shouldEqual (EvalStackValue.Int32 0)

        staticAttributes
        |> shouldEqual (
            int32 (
                System.Reflection.FieldAttributes.Public
                ||| System.Reflection.FieldAttributes.Static
            )
        )

        let returnValue, literalAttributes, _ =
            invokeGetFieldDefProps fixture fixture.LiteralField state

        returnValue |> shouldEqual (EvalStackValue.Int32 0)

        literalAttributes
        |> shouldEqual (
            int32 (
                System.Reflection.FieldAttributes.Public
                ||| System.Reflection.FieldAttributes.Static
                ||| System.Reflection.FieldAttributes.Literal
                ||| System.Reflection.FieldAttributes.HasDefault
            )
        )

    [<Test>]
    let ``MetadataImport GetCustomAttributeProps returns ctor token and signature blob`` () : unit =
        let fixture = makeFixture ()

        let attrToken, expected =
            singleCustomAttributeForType fixture.Assembly fixture.ParameterlessAttrType

        let returnValue, ctorToken, (length, bytes), _ =
            invokeGetCustomAttributeProps fixture attrToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 0)
        ctorToken |> shouldEqual (MetadataToken.toInt expected.Constructor)
        length |> shouldEqual expected.Value.Length

        bytes
        |> shouldEqual (Array.init expected.Value.Length (fun i -> expected.Value.[i]))

    [<Test>]
    let ``MetadataImport GetCustomAttributeProps returns blob for attribute with arguments`` () : unit =
        let fixture = makeFixture ()

        let attrToken, expected =
            singleCustomAttributeForType fixture.Assembly fixture.ArgumentAttrType

        let returnValue, ctorToken, (length, bytes), _ =
            invokeGetCustomAttributeProps fixture attrToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 0)
        ctorToken |> shouldEqual (MetadataToken.toInt expected.Constructor)
        length |> shouldEqual expected.Value.Length

        bytes
        |> shouldEqual (Array.init expected.Value.Length (fun i -> expected.Value.[i]))
        // Sanity check: the [Obsolete("deprecated")] blob must contain the literal "deprecated".
        let blobAsString = System.Text.Encoding.UTF8.GetString bytes
        blobAsString.Contains "deprecated" |> shouldEqual true
