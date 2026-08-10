namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

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

public class OuterMetadataField
{
    public class InnerMetadataField
    {
    }
}

public class OuterWithSeveralNested
{
    public class NestedPublicFirst
    {
    }

    // Private on purpose: the QCall enumerates the NestedClass table and does no visibility
    // filtering — `RuntimeType.PopulateNestedClasses` applies binding flags afterwards. A
    // handler that filtered here would silently drop this one.
    private class NestedPrivateSecond
    {
    }

    public class NestedThirdWithOwnNested
    {
        // Nesting is not transitive: this belongs to NestedThirdWithOwnNested's list, never to
        // OuterWithSeveralNested's.
        public class DeeplyNested
        {
        }
    }
}

// One field per interesting FIELD-signature shape (ECMA-335 II.23.2.4). `VolatileField` is here
// because `volatile` prefixes the field's type with `modreq(IsVolatile)`, whose coded token is
// image-dependent: it is the shape that a hand-written expectation cannot cover but the host CLR
// can.
public class FieldSignatureShapes
{
    public int Int32Field;
    public string StringField;
    public int[] Int32ArrayField;
    public volatile int VolatileField;
}

public class ReferencesOtherAssemblyMembers
{
    // `string.Empty` is a static field on a corelib type, so the reference is a MemberRef row
    // carrying a FIELD signature blob rather than a METHOD one.
    public static string FieldMemberRef() => string.Empty;

    // A call to a corelib instance method is a MemberRef row with a METHOD signature blob.
    public static string MethodMemberRef(int x) => x.ToString();
}

public class TypesWithMembers
{
    public int InstanceMethod(int x, string y) => x + y.Length;

    public T GenericMethod<T>(T value) => value;

    public event System.EventHandler MyEvent;

    public int MyProperty { get; set; }
}
"""

    type private MetadataImportFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assembly : DumpedAssembly
            /// The raw bytes Roslyn produced, so a test can also hand the same image to the host
            /// CLR and use it as an outside oracle.
            Image : byte array
            TargetType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            EmptyType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ManyFieldsType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            GenericType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ParameterlessAttrType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            ArgumentAttrType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            OuterType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            InnerType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            SeveralNestedType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            SignatureShapesType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            MembersType : TypeInfo<GenericParamFromMetadata, TypeDefn>
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
        let outerType = requiredTopLevelType assembly "" "OuterMetadataField"

        let innerType =
            assembly.TryGetNestedTypeDef outerType.TypeDefHandle "InnerMetadataField"
            |> Option.defaultWith (fun () -> failwith "nested type InnerMetadataField not found")

        let severalNestedType = requiredTopLevelType assembly "" "OuterWithSeveralNested"

        let signatureShapesType = requiredTopLevelType assembly "" "FieldSignatureShapes"

        let membersType = requiredTopLevelType assembly "" "TypesWithMembers"

        let constArrayType = requiredTopLevelType corelib "System.Reflection" "ConstArray"

        let fieldByName (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
            targetType.Fields |> List.find (fun field -> field.Name = name)

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib

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
            Image = image
            TargetType = targetType
            EmptyType = emptyType
            ManyFieldsType = manyFieldsType
            GenericType = genericType
            ParameterlessAttrType = parameterlessAttrType
            ArgumentAttrType = argumentAttrType
            OuterType = outerType
            InnerType = innerType
            SeveralNestedType = severalNestedType
            SignatureShapesType = signatureShapesType
            MembersType = membersType
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
            |> List.filter (fun method -> method.Name = methodName && (MethodInfo.arity method) = parameterCount)
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

    /// Look up a `MetadataImport` QCall stub by its `[LibraryImport]` entry-point name
    /// rather than by its IL method name. The Roslyn LibraryImport source generator emits
    /// a marshalling stub whose synthesised IL name (e.g. `<Enum>g____PInvoke|N_M`) carries
    /// source-generator counters that drift across runtime builds; the entry-point name on
    /// the `NativeImport` attribute is the stable identifier.
    let private metadataImportQCallMethod
        (fixture : MetadataImportFixture)
        (state : IlMachineState)
        (entryPointName : string)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let metadataImportType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System.Reflection" "MetadataImport"

        let rawMethod =
            metadataImportType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPointName
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith $"MetadataImport QCall stub with entry point %s{entryPointName} was not found"
                | methods ->
                    failwith
                        $"MetadataImport QCall stub with entry point %s{entryPointName} was ambiguous: %d{methods.Length} matches"

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

    let private readInt32Out
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : int32
        =
        match
            IlMachineState.readManagedByref baseClassTypes state ptr
            |> CliType.unwrapPrimitiveLikeDeep
        with
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
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.BaseClassTypes.Corelib
                TargetType = metadataImportType
            }

        match NativeDispatch.tryExecute ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> state
        | Some result -> failwith $"unexpected MetadataImport execution result: %O{result}"
        | None -> failwith "MetadataImport native method did not match"

    let private invokeEnum
        (fixture : MetadataImportFixture)
        (tokenType : int32)
        (parent : int32)
        (state : IlMachineState)
        : int32 * int32 list * EnumResultStorage * IlMachineState
        =
        let state, metadataImportType, enumMethod =
            metadataImportQCallMethod fixture state "MetadataImport_Enum"

        let lengthOut, state = allocateInt32Out fixture -1 state
        let shortResult, state = allocateInt32Buffer fixture 16 0 state
        let longResult, state = allocateObjectOut fixture state
        let longResultHandle, state = objectHandleOnStack fixture longResult state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                enumMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 tokenType)
                    CliType.Numeric (CliNumericType.Int32 parent)
                    CliType.RuntimePointer (CliRuntimePointer.Managed lengthOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed shortResult)
                    longResultHandle
                ]
                state

        let length = readInt32Out fixture.BaseClassTypes state lengthOut

        let tokens, storage =
            match IlMachineState.readManagedByref fixture.BaseClassTypes state longResult with
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

    /// `MetadataTokenType.FieldDef`, as `MetadataImport.EnumFields` passes it.
    let private invokeEnumFields
        (fixture : MetadataImportFixture)
        (targetType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : int32 * int32 list * EnumResultStorage * IlMachineState
        =
        invokeEnum fixture 0x04000000 (typeDefToken targetType.TypeDefHandle) state

    /// `MetadataTokenType.TypeDef`, as `MetadataImport.EnumNestedTypes` passes it — which asks for
    /// the parent's *nested classes*, not for TypeDefs at large.
    let private invokeEnumNestedTypes
        (fixture : MetadataImportFixture)
        (targetType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : int32 * int32 list * EnumResultStorage * IlMachineState
        =
        invokeEnum fixture 0x02000000 (typeDefToken targetType.TypeDefHandle) state

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
        returnValue, readInt32Out fixture.BaseClassTypes state attributesOut, state

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

    /// Read back a `ConstArray` written through `ptr`: its `m_length`, the bytes its `m_constArray`
    /// addresses, and the raw `m_constArray` pointer itself. The pointer is returned because it is
    /// part of the contract and not merely a route to the bytes: a handler that hands back the right
    /// bytes over the wrong pointer shape breaks the guest's `ConstArray[i]` while satisfying every
    /// content assertion.
    let private readConstArrayOut
        (fixture : MetadataImportFixture)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : int32 * byte array * ManagedPointerSource
        =
        let cli = IlMachineState.readManagedByref fixture.BaseClassTypes state ptr

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

        let pointer =
            match
                CliValueType.DereferenceFieldById pointerFieldId valueType
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> ptr
            | other -> failwith $"expected managed pointer for ConstArray.m_constArray, got %O{other}"

        let bytes =
            match pointer with
            | ManagedPointerSource.Null ->
                if length = 0 then
                    [||]
                else
                    failwith $"ConstArray with length %d{length} but null pointer"
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, baseIndex), []) ->
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
            | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, _) ->
                // Read one byte at a time through the machine's own byte-view reader — the reader a
                // guest's `ConstArray[i]` ultimately lands in, though the guest reaches it via
                // pointer arithmetic rather than by supplying the offset directly as we do here.
                let byteTemplate, _ =
                    IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes fixture.ByteHandle

                Array.init
                    length
                    (fun i ->
                        match
                            IlMachineState.readPeByteRangeBytesAs state peByteRange i byteTemplate
                            |> CliType.unwrapPrimitiveLikeDeep
                        with
                        | CliType.Numeric (CliNumericType.UInt8 b) -> b
                        | other -> failwith $"expected UInt8 in PE byte-range ConstArray storage, got %O{other}"
                    )
            | other -> failwith $"unexpected ConstArray.m_constArray pointer %O{other}"

        length, bytes, pointer

    let private invokeGetCustomAttributeProps
        (fixture : MetadataImportFixture)
        (attrToken : int32)
        (state : IlMachineState)
        : EvalStackValue * int32 * (int32 * byte array * ManagedPointerSource) * IlMachineState
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
        let ctorToken = readInt32Out fixture.BaseClassTypes state ctorOut
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

    /// Resolve a nested type by name, for use as the *expected* answer. Deliberately via
    /// `TryGetNestedTypeDef`, which is a different index from the one under test
    /// (`NestedTypeDefsByEnclosing`): reading the expectation out of the index being tested would
    /// pass for any self-consistent wrong answer.
    let private requiredNestedType
        (fixture : MetadataImportFixture)
        (enclosing : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        : int32
        =
        fixture.Assembly.TryGetNestedTypeDef enclosing.TypeDefHandle name
        |> Option.defaultWith (fun () -> failwith $"nested type %s{name} not found in %s{enclosing.Name}")
        |> fun ty -> typeDefToken ty.TypeDefHandle

    [<Test>]
    let ``MetadataImport Enum returns nested TypeDef tokens in declaration order`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumNestedTypes fixture fixture.SeveralNestedType fixture.State

        length |> shouldEqual 3
        storage |> shouldEqual EnumResultStorage.ShortResult

        // Order is guest-observable — `RuntimeType.PopulateNestedClasses` passes it straight
        // through to `Type.GetNestedTypes()` — so pin the sequence, not just the set.
        tokens
        |> shouldEqual
            [
                requiredNestedType fixture fixture.SeveralNestedType "NestedPublicFirst"
                requiredNestedType fixture fixture.SeveralNestedType "NestedPrivateSecond"
                requiredNestedType fixture fixture.SeveralNestedType "NestedThirdWithOwnNested"
            ]

    [<Test>]
    let ``MetadataImport Enum returns only immediately nested TypeDefs`` () : unit =
        let fixture = makeFixture ()

        let thirdType =
            fixture.Assembly.TryGetNestedTypeDef fixture.SeveralNestedType.TypeDefHandle "NestedThirdWithOwnNested"
            |> Option.defaultWith (fun () -> failwith "nested type NestedThirdWithOwnNested not found")

        // `DeeplyNested` belongs to this list and, per the previous test, not to its grandparent's.
        // A transitive walk would put it in both.
        let length, tokens, _, _ = invokeEnumNestedTypes fixture thirdType fixture.State

        length |> shouldEqual 1
        tokens |> shouldEqual [ requiredNestedType fixture thirdType "DeeplyNested" ]

    [<Test>]
    let ``MetadataImport Enum returns the single nested TypeDef of a simple outer type`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumNestedTypes fixture fixture.OuterType fixture.State

        length |> shouldEqual 1
        storage |> shouldEqual EnumResultStorage.ShortResult
        tokens |> shouldEqual [ typeDefToken fixture.InnerType.TypeDefHandle ]

    [<Test>]
    let ``MetadataImport Enum returns empty nested TypeDef list for a type with none`` () : unit =
        let fixture = makeFixture ()

        // The index stores no entry at all for such a type, so this pins that "absent" is reported
        // as an empty enumeration rather than as an error.
        let length, tokens, storage, _ =
            invokeEnumNestedTypes fixture fixture.EmptyType fixture.State

        length |> shouldEqual 0
        tokens |> shouldEqual []
        storage |> shouldEqual EnumResultStorage.ShortResult

    [<Test>]
    let ``MetadataImport Enum rejects a nil TypeDef parent for nested-type enumeration`` () : unit =
        let fixture = makeFixture ()

        // `RuntimeType.PopulateNestedClasses` screens nil tokens out before calling, and CoreCLR
        // asserts on one. Answering "no nested types" would look identical to a real empty result.
        let exn =
            Assert.Throws (fun () -> invokeEnum fixture 0x02000000 0x02000000 fixture.State |> ignore)

        exn.ToString () |> shouldContainText "nil TypeDef parent token"

    [<Test>]
    let ``MetadataImport GetFieldDefProps writes metadata field attributes`` () : unit =
        let fixture = makeFixture ()

        let returnValue, instanceAttributes, state =
            invokeGetFieldDefProps fixture fixture.InstanceField fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        instanceAttributes
        |> shouldEqual (int32 System.Reflection.FieldAttributes.Public)

        let returnValue, staticAttributes, state =
            invokeGetFieldDefProps fixture fixture.StaticField state

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        staticAttributes
        |> shouldEqual (
            int32 (
                System.Reflection.FieldAttributes.Public
                ||| System.Reflection.FieldAttributes.Static
            )
        )

        let returnValue, literalAttributes, _ =
            invokeGetFieldDefProps fixture fixture.LiteralField state

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        literalAttributes
        |> shouldEqual (
            int32 (
                System.Reflection.FieldAttributes.Public
                ||| System.Reflection.FieldAttributes.Static
                ||| System.Reflection.FieldAttributes.Literal
                ||| System.Reflection.FieldAttributes.HasDefault
            )
        )

    let private invokeGetParentToken
        (fixture : MetadataImportFixture)
        (mdToken : int32)
        (state : IlMachineState)
        : EvalStackValue * int32 * IlMachineState
        =
        let state, metadataImportType, getParentTokenMethod =
            metadataImportMethod fixture state "GetParentToken" 3

        let parentOut, state = allocateInt32Out fixture 0 state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getParentTokenMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 mdToken)
                    CliType.RuntimePointer (CliRuntimePointer.Managed parentOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state
        returnValue, readInt32Out fixture.BaseClassTypes state parentOut, state

    let private methodDefToken (handle : MethodDefinitionHandle) : int32 =
        let handle : EntityHandle = MethodDefinitionHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private methodHandleByName
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        : MethodDefinitionHandle
        =
        typeInfo.Methods
        |> List.tryFind (fun method -> method.Name = name)
        |> Option.map (fun method -> (MethodInfo.requireMetadata "test" method).Handle)
        |> Option.defaultWith (fun () -> failwith $"method %s{name} not found on %s{typeInfo.Name}")

    let private firstGenericParameterOfType
        (assembly : DumpedAssembly)
        (typeHandle : TypeDefinitionHandle)
        : GenericParameterHandle
        =
        let mr = assembly.PeReader.GetMetadataReader ()
        let typeDef = mr.GetTypeDefinition typeHandle
        let genericParams = typeDef.GetGenericParameters ()

        if genericParams.Count = 0 then
            failwith "expected at least one generic parameter on type"

        genericParams.[0]

    let private firstGenericParameterOfMethod
        (assembly : DumpedAssembly)
        (methodHandle : MethodDefinitionHandle)
        : GenericParameterHandle
        =
        let mr = assembly.PeReader.GetMetadataReader ()
        let methodDef = mr.GetMethodDefinition methodHandle
        let genericParams = methodDef.GetGenericParameters ()

        if genericParams.Count = 0 then
            failwith "expected at least one generic parameter on method"

        genericParams.[0]

    let private firstParameterOfMethod
        (assembly : DumpedAssembly)
        (methodHandle : MethodDefinitionHandle)
        : ParameterHandle
        =
        let mr = assembly.PeReader.GetMetadataReader ()
        let methodDef = mr.GetMethodDefinition methodHandle
        let parameters = methodDef.GetParameters ()
        let mutable enumerator = parameters.GetEnumerator ()

        if enumerator.MoveNext () then
            enumerator.Current
        else
            failwith "expected at least one parameter on method"

    let private firstEventOfType
        (assembly : DumpedAssembly)
        (typeHandle : TypeDefinitionHandle)
        : EventDefinitionHandle
        =
        let mr = assembly.PeReader.GetMetadataReader ()
        let typeDef = mr.GetTypeDefinition typeHandle
        let events = typeDef.GetEvents ()
        let mutable enumerator = events.GetEnumerator ()

        if enumerator.MoveNext () then
            enumerator.Current
        else
            failwith "expected at least one event on type"

    let private firstPropertyOfType
        (assembly : DumpedAssembly)
        (typeHandle : TypeDefinitionHandle)
        : PropertyDefinitionHandle
        =
        let mr = assembly.PeReader.GetMetadataReader ()
        let typeDef = mr.GetTypeDefinition typeHandle
        let properties = typeDef.GetProperties ()
        let mutable enumerator = properties.GetEnumerator ()

        if enumerator.MoveNext () then
            enumerator.Current
        else
            failwith "expected at least one property on type"

    let private genericParamToken (handle : GenericParameterHandle) : int32 =
        let handle : EntityHandle = GenericParameterHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private parameterToken (handle : ParameterHandle) : int32 =
        let handle : EntityHandle = ParameterHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private eventToken (handle : EventDefinitionHandle) : int32 =
        let handle : EntityHandle = EventDefinitionHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private propertyToken (handle : PropertyDefinitionHandle) : int32 =
        let handle : EntityHandle = PropertyDefinitionHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    [<Test>]
    let ``MetadataImport GetParentToken returns nil for top-level TypeDef`` () : unit =
        let fixture = makeFixture ()

        let returnValue, parent, _ =
            invokeGetParentToken fixture (typeDefToken fixture.TargetType.TypeDefHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        // mdTypeDefNil = TypeDef table | row 0 = 0x02000000
        parent |> shouldEqual 0x02000000

    [<Test>]
    let ``MetadataImport GetParentToken returns enclosing TypeDef for nested type`` () : unit =
        let fixture = makeFixture ()

        let returnValue, parent, _ =
            invokeGetParentToken fixture (typeDefToken fixture.InnerType.TypeDefHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.OuterType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns declaring TypeDef for MethodDef`` () : unit =
        let fixture = makeFixture ()

        let methodHandle =
            match fixture.TargetType.Methods with
            | method :: _ -> (MethodInfo.requireMetadata "test" method).Handle
            | [] -> failwith "expected at least one method on MetadataFields (implicit .ctor)"

        let returnValue, parent, _ =
            invokeGetParentToken fixture (methodDefToken methodHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.TargetType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns declaring TypeDef for FieldDef`` () : unit =
        let fixture = makeFixture ()

        let returnValue, parent, _ =
            invokeGetParentToken fixture (fieldDefToken fixture.InstanceField.Handle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.TargetType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns decorated entity for CustomAttribute`` () : unit =
        let fixture = makeFixture ()

        let attrToken, _ =
            singleCustomAttributeForType fixture.Assembly fixture.ParameterlessAttrType

        let returnValue, parent, _ = invokeGetParentToken fixture attrToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.ParameterlessAttrType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns owning TypeDef for type GenericParam`` () : unit =
        let fixture = makeFixture ()

        let genericParamHandle =
            firstGenericParameterOfType fixture.Assembly fixture.GenericType.TypeDefHandle

        let returnValue, parent, _ =
            invokeGetParentToken fixture (genericParamToken genericParamHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.GenericType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns owning MethodDef for method GenericParam`` () : unit =
        let fixture = makeFixture ()

        let methodHandle = methodHandleByName fixture.MembersType "GenericMethod"

        let genericParamHandle = firstGenericParameterOfMethod fixture.Assembly methodHandle

        let returnValue, parent, _ =
            invokeGetParentToken fixture (genericParamToken genericParamHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (methodDefToken methodHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns owning MethodDef for ParamDef`` () : unit =
        let fixture = makeFixture ()

        let methodHandle = methodHandleByName fixture.MembersType "InstanceMethod"
        let paramHandle = firstParameterOfMethod fixture.Assembly methodHandle

        let returnValue, parent, _ =
            invokeGetParentToken fixture (parameterToken paramHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (methodDefToken methodHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns owning TypeDef for Event`` () : unit =
        let fixture = makeFixture ()

        let eventHandle =
            firstEventOfType fixture.Assembly fixture.MembersType.TypeDefHandle

        let returnValue, parent, _ =
            invokeGetParentToken fixture (eventToken eventHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.MembersType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetParentToken returns owning TypeDef for Property`` () : unit =
        let fixture = makeFixture ()

        let propertyHandle =
            firstPropertyOfType fixture.Assembly fixture.MembersType.TypeDefHandle

        let returnValue, parent, _ =
            invokeGetParentToken fixture (propertyToken propertyHandle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        parent |> shouldEqual (typeDefToken fixture.MembersType.TypeDefHandle)

    [<Test>]
    let ``MetadataImport GetCustomAttributeProps returns ctor token and signature blob`` () : unit =
        let fixture = makeFixture ()

        let attrToken, expected =
            singleCustomAttributeForType fixture.Assembly fixture.ParameterlessAttrType

        let returnValue, ctorToken, (length, bytes, _), _ =
            invokeGetCustomAttributeProps fixture attrToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        ctorToken |> shouldEqual (MetadataToken.toInt expected.Constructor)
        length |> shouldEqual expected.Value.Length

        bytes
        |> shouldEqual (Array.init expected.Value.Length (fun i -> expected.Value.[i]))

    [<Test>]
    let ``MetadataImport GetCustomAttributeProps returns blob for attribute with arguments`` () : unit =
        let fixture = makeFixture ()

        let attrToken, expected =
            singleCustomAttributeForType fixture.Assembly fixture.ArgumentAttrType

        let returnValue, ctorToken, (length, bytes, _), _ =
            invokeGetCustomAttributeProps fixture attrToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        ctorToken |> shouldEqual (MetadataToken.toInt expected.Constructor)
        length |> shouldEqual expected.Value.Length

        bytes
        |> shouldEqual (Array.init expected.Value.Length (fun i -> expected.Value.[i]))
        // Sanity check: the [Obsolete("deprecated")] blob must contain the literal "deprecated".
        let blobAsString = System.Text.Encoding.UTF8.GetString bytes
        blobAsString.Contains "deprecated" |> shouldEqual true

    let private memberRefToken (handle : MemberReferenceHandle) : int32 =
        let handle : EntityHandle = MemberReferenceHandle.op_Implicit handle
        MetadataTokens.GetToken handle

    let private invokeGetMemberRefProps
        (fixture : MetadataImportFixture)
        (memberTokenRef : int32)
        (state : IlMachineState)
        : EvalStackValue * (int32 * byte array * ManagedPointerSource) * IlMachineState
        =
        let state, metadataImportType, getMemberRefPropsMethod =
            metadataImportMethod fixture state "GetMemberRefProps" 3

        let signatureOut, state = allocateConstArrayOut fixture state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getMemberRefPropsMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 memberTokenRef)
                    CliType.RuntimePointer (CliRuntimePointer.Managed signatureOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state
        returnValue, readConstArrayOut fixture state signatureOut, state

    /// The MemberRef row referring to <paramref name="memberName"/>; there must be exactly one.
    let private singleMemberRefNamed
        (assembly : DumpedAssembly)
        (memberName : string)
        : MemberReferenceHandle * MemberReference<MetadataToken>
        =
        assembly.Members
        |> Seq.filter (fun kvp -> kvp.Value.PrettyName = memberName)
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Seq.toList
        |> function
            | [ result ] -> result
            | [] -> failwith $"no MemberRef named %s{memberName}"
            | many -> failwith $"expected exactly one MemberRef named %s{memberName}, got %d{many.Length}"

    /// ECMA-335 II.23.2 compressed unsigned integer; returns the value and the offset just past it.
    let private readCompressedUInt (bytes : byte array) (offset : int) : uint32 * int =
        let b0 = bytes.[offset]

        if b0 &&& 0x80uy = 0uy then
            uint32 b0, offset + 1
        elif b0 &&& 0xC0uy = 0x80uy then
            ((uint32 b0 &&& 0x3Fu) <<< 8) ||| uint32 bytes.[offset + 1], offset + 2
        else
            ((uint32 b0 &&& 0x1Fu) <<< 24)
            ||| (uint32 bytes.[offset + 1] <<< 16)
            ||| (uint32 bytes.[offset + 2] <<< 8)
            ||| uint32 bytes.[offset + 3],
            offset + 4

    [<Test>]
    let ``MetadataImport GetMemberRefProps returns the signature of a corelib attribute ctor`` () : unit =
        let fixture = makeFixture ()

        let _, expected =
            singleCustomAttributeForType fixture.Assembly fixture.ParameterlessAttrType

        // [System.Obsolete] is declared in corelib, so the test assembly refers to its ctor
        // through a MemberRef rather than a MethodDef. This is exactly the split that makes
        // CoreLib's MetadataImport.GetMethodSignature dispatch here instead of to
        // GetSigOfMethodDef (which the sibling MetadataImportGetSigOfMethodDef.cs covers).
        let ctorToken = MetadataToken.toInt expected.Constructor

        match MetadataToken.ofInt ctorToken with
        | MetadataToken.MemberReference _ -> ()
        | other -> failwith $"expected [Obsolete] ctor to be a MemberRef, got %O{other}"

        let returnValue, (length, bytes, _), _ =
            invokeGetMemberRefProps fixture ctorToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        // HASTHIS | DEFAULT (0x20), zero parameters, ELEMENT_TYPE_VOID (0x01) return.
        bytes |> shouldEqual [| 0x20uy ; 0x00uy ; 0x01uy |]
        length |> shouldEqual bytes.Length

    [<Test>]
    let ``MetadataImport GetMemberRefProps returns the signature of a ctor with parameters`` () : unit =
        let fixture = makeFixture ()

        let _, expected =
            singleCustomAttributeForType fixture.Assembly fixture.ArgumentAttrType

        let ctorToken = MetadataToken.toInt expected.Constructor

        let returnValue, (length, bytes, _), _ =
            invokeGetMemberRefProps fixture ctorToken fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        // HASTHIS | DEFAULT (0x20), one parameter, ELEMENT_TYPE_VOID (0x01) return,
        // ELEMENT_TYPE_STRING (0x0e) parameter.
        bytes |> shouldEqual [| 0x20uy ; 0x01uy ; 0x01uy ; 0x0Euy |]
        length |> shouldEqual bytes.Length

    [<Test>]
    let ``MetadataImport GetMemberRefProps returns a FIELD signature for a field MemberRef`` () : unit =
        let fixture = makeFixture ()

        // `string.Empty` — a static field on a corelib type, so its MemberRef carries a FIELD
        // signature. CoreCLR's callers rely on the leading calling-convention byte to tell
        // field references apart from method references (RuntimeModule.ResolveMethod rejects
        // MdSigCallingConvention.Field), so the distinction has to survive this call.
        let handle, memberRef = singleMemberRefNamed fixture.Assembly "Empty"

        match memberRef.Signature with
        | MemberSignature.Field _ -> ()
        | MemberSignature.Method _ -> failwith "expected String.Empty MemberRef to have a field signature"

        let returnValue, (length, bytes, _), _ =
            invokeGetMemberRefProps fixture (memberRefToken handle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        // FIELD (0x06), ELEMENT_TYPE_STRING (0x0e).
        bytes |> shouldEqual [| 0x06uy ; 0x0Euy |]
        length |> shouldEqual bytes.Length

    [<Test>]
    let ``MetadataImport GetMemberRefProps blob decodes to the eagerly-parsed signature`` () : unit =
        let fixture = makeFixture ()

        let members =
            fixture.Assembly.Members
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
            |> Seq.toList

        members |> List.isEmpty |> shouldEqual false

        let mutable state = fixture.State

        for handle, memberRef in members do
            let returnValue, (length, bytes, _), nextState =
                invokeGetMemberRefProps fixture (memberRefToken handle) state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
            length |> shouldEqual bytes.Length
            bytes.Length > 0 |> shouldEqual true

            // The blob we hand back must agree with the signature PawPrint decoded independently
            // (via MemberReference.make) when the assembly was read.
            let header = SignatureHeader bytes.[0]

            match memberRef.Signature with
            | MemberSignature.Field _ -> header.Kind |> shouldEqual SignatureKind.Field
            | MemberSignature.Method methodSig ->
                header.Kind |> shouldEqual SignatureKind.Method
                bytes.[0] |> shouldEqual methodSig.Header.Get.RawValue

                let offset =
                    if header.IsGeneric then
                        let genericParameterCount, offset = readCompressedUInt bytes 1
                        int genericParameterCount |> shouldEqual methodSig.GenericParameterCount
                        offset
                    else
                        1

                let parameterCount, _ = readCompressedUInt bytes offset
                int parameterCount |> shouldEqual methodSig.ParameterTypes.Length

    [<Test>]
    let ``MetadataImport GetMemberRefProps rejects a non-MemberRef token`` () : unit =
        let fixture = makeFixture ()

        let ex =
            Assert.Throws (fun () ->
                invokeGetMemberRefProps fixture (typeDefToken fixture.TargetType.TypeDefHandle) fixture.State
                |> ignore
            )

        ex.Message |> shouldContainText "expected MemberRef token"

    let private fieldNamed
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        typeInfo.Fields
        |> List.tryFind (fun field -> field.Name = name)
        |> Option.defaultWith (fun () -> failwith $"field %s{name} not found on %s{typeInfo.Name}")

    let private invokeGetSigOfFieldDef
        (fixture : MetadataImportFixture)
        (fieldToken : int32)
        (state : IlMachineState)
        : EvalStackValue * (int32 * byte array * ManagedPointerSource) * IlMachineState
        =
        let state, metadataImportType, getSigOfFieldDefMethod =
            metadataImportMethod fixture state "GetSigOfFieldDef" 3

        let signatureOut, state = allocateConstArrayOut fixture state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getSigOfFieldDefMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 fieldToken)
                    CliType.RuntimePointer (CliRuntimePointer.Managed signatureOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state
        returnValue, readConstArrayOut fixture state signatureOut, state

    [<Test>]
    let ``MetadataImport GetSigOfFieldDef returns the ECMA FIELD signature blob`` () : unit =
        let fixture = makeFixture ()

        // ECMA-335 II.23.2.4: a FIELD signature is the FIELD calling-convention byte (0x06)
        // followed by a Type (II.23.2.12). These expectations are derived from the standard
        // rather than read back out of the image, so they pin the blob's width as well as its
        // content: ELEMENT_TYPE_I4 = 0x08, _STRING = 0x0e, _SZARRAY = 0x1d, _VAR = 0x13 followed
        // by the compressed generic-parameter index.
        let cases =
            [
                fixture.SignatureShapesType, "Int32Field", [| 0x06uy ; 0x08uy |]
                fixture.SignatureShapesType, "StringField", [| 0x06uy ; 0x0Euy |]
                fixture.SignatureShapesType, "Int32ArrayField", [| 0x06uy ; 0x1Duy ; 0x08uy |]
                fixture.GenericType, "GenericField", [| 0x06uy ; 0x13uy ; 0x00uy |]
            ]

        let mutable state = fixture.State

        for typeInfo, fieldName, expected in cases do
            let field = fieldNamed typeInfo fieldName

            let returnValue, (length, bytes, _), nextState =
                invokeGetSigOfFieldDef fixture (fieldDefToken field.Handle) state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
            bytes |> shouldEqual expected
            length |> shouldEqual expected.Length

    [<Test>]
    let ``MetadataImport GetSigOfFieldDef returns the signature of a literal field`` () : unit =
        let fixture = makeFixture ()

        // The literal shape is the one that matters in practice: a literal has no FieldDesc, so
        // CoreCLR reflects over it with `MdFieldInfo` (RuntimeType.CoreCLR.cs, PopulateLiteralFields),
        // and `MdFieldInfo.FieldType` is the only managed caller of GetSigOfFieldDef.
        let _, (length, bytes, _), _ =
            invokeGetSigOfFieldDef fixture (fieldDefToken fixture.LiteralField.Handle) fixture.State

        // FIELD (0x06), ELEMENT_TYPE_I4 (0x08). A literal's signature describes its type only; the
        // constant 7 lives in the Constant table, not here.
        bytes |> shouldEqual [| 0x06uy ; 0x08uy |]
        length |> shouldEqual bytes.Length

    [<Test>]
    let ``MetadataImport GetSigOfFieldDef agrees with the host runtime for every field`` () : unit =
        let fixture = makeFixture ()

        // Outside oracle: hand the same image to the host CLR and ask its own metadata engine the
        // same question. `Module.ResolveSignature` does not go through the managed
        // `MetadataImport.GetSigOfFieldDef`; for a FieldDef it calls `GetSignatureFromToken`, and
        // `MDInternalRO::GetSigFromToken` (md/runtime/mdinternalro.cpp) dispatches mdtFieldDef to
        // the same `MDInternalRO::GetSigOfFieldDef` and copies the blob byte for byte. This is the
        // only assertion that covers `VolatileField`, whose modreq carries an image-dependent
        // coded token that cannot be written down ahead of time.
        let hostModule = (System.Reflection.Assembly.Load fixture.Image).ManifestModule

        let fields = fixture.Assembly.Fields |> Seq.map (fun kvp -> kvp.Key) |> Seq.toList

        fields |> List.isEmpty |> shouldEqual false

        let mutable state = fixture.State

        for fieldHandle in fields do
            let token = fieldDefToken fieldHandle

            let returnValue, (length, bytes, _), nextState =
                invokeGetSigOfFieldDef fixture token state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
            bytes |> shouldEqual (hostModule.ResolveSignature token)
            length |> shouldEqual bytes.Length

    [<Test>]
    let ``MetadataImport GetSigOfFieldDef points at the field's own signature blob`` () : unit =
        let fixture = makeFixture ()
        let field = fieldNamed fixture.SignatureShapesType "Int32Field"

        let _, (length, _, pointer), state =
            invokeGetSigOfFieldDef fixture (fieldDefToken field.Handle) fixture.State

        // The pointer's shape is part of the contract, not just a route to the bytes. CoreCLR hands
        // back a PCCOR_SIGNATURE straight into the mapped metadata, and PawPrint models that with a
        // PeByteRange root whose Source names *this* FieldDef; `NativeSignature.resolveSignatureBlobHandle`
        // reads that provenance back when the blob later arrives as a `Signature`'s `_sig`. The
        // `ReinterpretAs byte` projection is equally load-bearing: `BinaryArithmetic` refuses
        // arithmetic on a bare PeByteRange root, so without it a guest's `ConstArray[i]` — which is
        // `((byte*)m_constArray)[index]` — would fail while every content assertion above still passed.
        let byteType =
            AllConcreteTypes.lookup fixture.ByteHandle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Byte was not concretized")

        let expected =
            ManagedPointerSource.Byref (
                ByrefRoot.PeByteRange
                    {
                        AssemblyFullName = fixture.Assembly.Name.FullName
                        Source =
                            PeByteRangePointerSource.FieldSignatureBlob (
                                ComparableFieldDefinitionHandle.Make field.Handle
                            )
                        RelativeVirtualAddress = 0
                        // `int` is `06 08`, so the range covers exactly two bytes. Spelled out
                        // rather than taken from `length`, so that a handler which derived both the
                        // struct's length and the range's size from the same wrong place would
                        // still fail here.
                        Size = 2
                    },
                [ ByrefProjection.ReinterpretAs byteType ]
            )

        length |> shouldEqual 2
        pointer |> shouldEqual expected

    [<Test>]
    let ``MetadataImport GetSigOfFieldDef rejects a non-FieldDef token`` () : unit =
        let fixture = makeFixture ()

        let ex =
            Assert.Throws (fun () ->
                invokeGetSigOfFieldDef fixture (typeDefToken fixture.TargetType.TypeDefHandle) fixture.State
                |> ignore
            )

        ex.Message |> shouldContainText "expected FieldDef token"

    [<Test>]
    let ``MetadataImport GetSigOfFieldDef rejects a FieldDef absent from the assembly`` () : unit =
        let fixture = makeFixture ()

        // Row 0xFFFFFF of the Field table; the fixture assembly has nothing like that many fields.
        let ex =
            Assert.Throws (fun () -> invokeGetSigOfFieldDef fixture 0x04FFFFFF fixture.State |> ignore)

        ex.Message |> shouldContainText "was not present in"
