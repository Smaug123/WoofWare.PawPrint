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

// One literal per Constant-table type code the C# compiler can emit, plus the two encodings that
// are easy to confuse: `NullString` is ELEMENT_TYPE_CLASS (four zero bytes), and `EmptyString` has
// a zero-length blob that the runtime reports as a null pointer. `NotAConstant` has no Constant row
// at all, which is a third thing again (ELEMENT_TYPE_VOID).
public class ConstantShapes
{
    public const bool BoolConst = true;
    public const char CharConst = 'q';
    public const sbyte SByteConst = -1;
    public const byte ByteConst = 200;
    public const short Int16Const = -300;
    public const ushort UInt16Const = 40000;
    public const int Int32Const = 42;
    public const uint UInt32Const = 3000000000;
    public const long Int64Const = -1234567890123L;
    public const ulong UInt64Const = 18446744073709551615UL;
    public const float SingleConst = 0.25f;
    public const double DoubleConst = 0.5;
    public const string StringConst = "hello";
    public const string EmptyStringConst = "";
    public const string NullStringConst = null;
    public static int NotAConstant = 7;
}

// `GetName` hands back the `#Strings` entry verbatim, so the interesting axis is the encoding, not
// the field's kind: `Größe` is five characters but seven UTF-8 bytes, which is what distinguishes a
// real UTF-8 encode from an ASCII or UTF-16 one. Both a literal and a non-literal field are here
// because the handler cannot tell them apart (a FieldDef is a FieldDef), even though only the
// literal ones are reachable from a guest — a plain field's name comes from `RtFieldInfo`, which
// uses the `RuntimeFieldHandle.GetName` QCall instead.
public class FieldNames
{
    public int PlainNamedField;
    public const int LiteralNamedField = 3;
    public const int Größe = 5;
}

public class ReferencesOtherAssemblyMembers
{
    // `string.Empty` is a static field on a corelib type, so the reference is a MemberRef row
    // carrying a FIELD signature blob rather than a METHOD one.
    public static string FieldMemberRef() => string.Empty;

    // A call to a corelib instance method is a MemberRef row with a METHOD signature blob.
    public static string MethodMemberRef(int x) => x.ToString();
}

public class PropertyBase
{
    public int Inherited { get; set; }
}

// Every shape the QCall must *not* filter out. `MetadataImport_Enum` walks the PropertyMap run
// verbatim: `RuntimeType.PopulateProperties` is what applies binding flags and drops inherited or
// private members afterwards, so a handler that filtered here would silently lose all but `Alpha`.
// `Größe` is five characters and seven UTF-8 bytes, which is what separates a real UTF-8 name from
// an ASCII or UTF-16 one; the indexer is the shape whose metadata name (`Item`) is not its C#
// spelling. Deriving from `PropertyBase` makes "declared-only" observable: enumeration is per-type,
// and the managed side walks the base chain itself.
public class PropertyShapes : PropertyBase
{
    public int Alpha { get; set; }
    public string Beta { get { return "b"; } }
    private int Hidden { get; set; }
    public static int Stat { get; set; }
    public int this[int i] { get { return i; } }
    public int Größe { get; set; }

    // Every property C# normally emits has `Property.Flags = 0`, so without this one a flags
    // assertion over this type would be vacuous and a handler that always answered 0 would pass.
    // `[SpecialName]` on a property does set the row's `SpecialName` bit (0x0200) — checked against
    // both the raw Property row and the host CLR's `PropertyInfo.Attributes`. It goes last so that
    // it takes the highest token and the declaration-order expectations above only grow at the tail.
    [System.Runtime.CompilerServices.SpecialName]
    public int Special { get; set; }
}

// A PROPERTY signature whose Type is ELEMENT_TYPE_VAR (`28 00 13 00`), which is the shape a handler
// that reconstructed the blob from PawPrint's parsed type model — rather than handing back the
// metadata bytes — would get wrong. Its own type rather than a property on `GenericMetadataFields`,
// because a property brings a `k__BackingField` with it and would churn that type's FieldDef
// enumeration expectations for no benefit.
public class GenericPropertyHolder<T>
{
    public T GenericProperty { get; set; }
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
            ConstantShapesType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            FieldNamesType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            PropertyShapesType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            GenericPropertyType : TypeInfo<GenericParamFromMetadata, TypeDefn>
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

        let constantShapesType = requiredTopLevelType assembly "" "ConstantShapes"

        let fieldNamesType = requiredTopLevelType assembly "" "FieldNames"

        let propertyShapesType = requiredTopLevelType assembly "" "PropertyShapes"

        let genericPropertyType = requiredTopLevelType assembly "" "GenericPropertyHolder`1"

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
                 baseClassTypes.Int64
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
            ConstantShapesType = constantShapesType
            FieldNamesType = fieldNamesType
            PropertyShapesType = propertyShapesType
            GenericPropertyType = genericPropertyType
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

    /// `MetadataTokenType.Property`, as `MetadataImport.EnumProperties` passes it.
    let private invokeEnumProperties
        (fixture : MetadataImportFixture)
        (targetType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : int32 * int32 list * EnumResultStorage * IlMachineState
        =
        invokeEnum fixture 0x17000000 (typeDefToken targetType.TypeDefHandle) state

    /// The property tokens the *host* CLR reports for a type in the fixture's image, in the order it
    /// reports them. `PropertyInfo.MetadataToken` is the raw `mdtProperty` token, so this is an
    /// oracle from outside PawPrint's own parse rather than a restatement of it.
    ///
    /// `DeclaredOnly` is what makes the comparison exact: `RuntimeType.PopulateProperties` walks the
    /// base chain and can drop inherited privates and vtable-slot duplicates, and none of that
    /// filtering is the QCall's job. Restricted to one type's own rows, the managed result is the
    /// PropertyMap run itself.
    let private hostDeclaredProperties (hostType : System.Type) : System.Reflection.PropertyInfo array =
        hostType.GetProperties (
            System.Reflection.BindingFlags.DeclaredOnly
            ||| System.Reflection.BindingFlags.Public
            ||| System.Reflection.BindingFlags.NonPublic
            ||| System.Reflection.BindingFlags.Instance
            ||| System.Reflection.BindingFlags.Static
        )

    let private hostPropertyTokens (image : byte array) (typeName : string) : int32 list =
        (System.Reflection.Assembly.Load image).GetType (typeName, true)
        |> hostDeclaredProperties
        |> Array.map (fun property -> property.MetadataToken)
        |> List.ofArray

    /// Every property row in the image, as the host CLR sees it. Used where the assertion should
    /// range over the whole image rather than over the handful of shapes anyone thought to write
    /// down, so a property added to the fixture is covered without anyone remembering to extend a
    /// list.
    let private hostPropertiesOfImage (image : byte array) : System.Reflection.PropertyInfo array =
        (System.Reflection.Assembly.Load image).GetTypes ()
        |> Array.collect hostDeclaredProperties

    /// The host CLR's `PropertyInfo` for one named property. Used as the source of the *token* to
    /// poke PawPrint with, so that a handler bug cannot pick its own input.
    let private hostPropertyNamed
        (image : byte array)
        (typeName : string)
        (propertyName : string)
        : System.Reflection.PropertyInfo
        =
        (System.Reflection.Assembly.Load image).GetType (typeName, true)
        |> hostDeclaredProperties
        |> Array.find (fun property -> property.Name = propertyName)

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
    let ``MetadataImport Enum returns a type's PropertyDef tokens in declaration order`` () : unit =
        let fixture = makeFixture ()

        let length, tokens, storage, _ =
            invokeEnumProperties fixture fixture.PropertyShapesType fixture.State

        // Seven declared properties; nothing is filtered by visibility, staticness, being an
        // indexer, or carrying `SpecialName`, and order is guest-observable because
        // `PopulateProperties` appends in the order it receives.
        length |> shouldEqual 7
        storage |> shouldEqual EnumResultStorage.ShortResult
        tokens |> shouldEqual (hostPropertyTokens fixture.Image "PropertyShapes")

    [<Test>]
    let ``MetadataImport Enum returns only a type's own PropertyDef tokens`` () : unit =
        let fixture = makeFixture ()

        let _, derivedTokens, _, _ =
            invokeEnumProperties fixture fixture.PropertyShapesType fixture.State

        let baseType = requiredTopLevelType fixture.Assembly "" "PropertyBase"
        let _, baseTokens, _, _ = invokeEnumProperties fixture baseType fixture.State

        // `PopulateProperties` walks the base chain itself, calling `Enum` once per type, so an
        // implementation that helpfully included inherited properties would report each base
        // property twice to the guest. Asserted directly as well as via the host oracle, because
        // this is the specific way the arm goes wrong.
        baseTokens |> shouldEqual (hostPropertyTokens fixture.Image "PropertyBase")
        baseTokens.Length |> shouldEqual 1

        derivedTokens
        |> List.filter (fun token -> List.contains token baseTokens)
        |> shouldEqual []

    [<Test>]
    let ``MetadataImport Enum returns no PropertyDef tokens for a type with only fields`` () : unit =
        let fixture = makeFixture ()

        // `MetadataFields` has three fields and no properties, so this separately rules out an arm
        // that enumerated the wrong table: an empty type could not tell the two apart.
        let length, tokens, storage, _ =
            invokeEnumProperties fixture fixture.TargetType fixture.State

        length |> shouldEqual 0
        tokens |> shouldEqual []
        storage |> shouldEqual EnumResultStorage.ShortResult

    [<Test>]
    let ``MetadataImport Enum returns no PropertyDef tokens for an empty type`` () : unit =
        let fixture = makeFixture ()

        // A type with no PropertyMap row at all, as opposed to one whose run is empty.
        let length, tokens, _, _ =
            invokeEnumProperties fixture fixture.EmptyType fixture.State

        length |> shouldEqual 0
        tokens |> shouldEqual []

    [<Test>]
    let ``MetadataImport Enum rejects a non-TypeDef parent for property enumeration`` () : unit =
        let fixture = makeFixture ()

        let exn =
            Assert.Throws (fun () ->
                invokeEnum fixture 0x17000000 (fieldDefToken fixture.InstanceField.Handle) fixture.State
                |> ignore
            )

        exn.ToString ()
        |> shouldContainText "expected TypeDef parent token for property enumeration"

    [<Test>]
    let ``MetadataImport Enum rejects a TypeDef parent absent from the assembly`` () : unit =
        let fixture = makeFixture ()

        // Unguarded, an out-of-range TypeDef handle reaches the metadata reader and surfaces as
        // `BadImageFormatException: Read out of bounds` — a PawPrint gap disguised as a corrupt
        // image.
        let exn =
            Assert.Throws (fun () -> invokeEnum fixture 0x17000000 0x02FFFFFF fixture.State |> ignore)

        exn.ToString () |> shouldContainText "was not present in"

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

    let private allocateSlotOut
        (fixture : MetadataImportFixture)
        (elementType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (zero : CliType)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes elementType

        let arrayAddr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero handle) (fun () -> zero) 1 state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    /// The four out-params of `GetDefaultValue`, read back: the 64-bit buffer, the `char*`, the
    /// length, and the ELEMENT_TYPE code.
    type private DefaultValueOut =
        {
            Value : int64
            StringPointer : ManagedPointerSource
            Length : int32
            CorElementType : int32
        }

    let private invokeGetDefaultValue
        (fixture : MetadataImportFixture)
        (fieldToken : int32)
        (state : IlMachineState)
        : EvalStackValue * DefaultValueOut * IlMachineState
        =
        let state, metadataImportType, getDefaultValueMethod =
            metadataImportMethod fixture state "GetDefaultValue" 6

        let valueOut, state =
            allocateSlotOut
                fixture
                fixture.BaseClassTypes.Int64
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                state

        let stringValueOut, state =
            allocateSlotOut
                fixture
                fixture.BaseClassTypes.IntPtr
                (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                state

        let lengthOut, state = allocateInt32Out fixture 0 state
        let corElementTypeOut, state = allocateInt32Out fixture 0 state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getDefaultValueMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 fieldToken)
                    CliType.RuntimePointer (CliRuntimePointer.Managed valueOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed stringValueOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed lengthOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed corElementTypeOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state

        let value =
            match
                IlMachineState.readManagedByref fixture.BaseClassTypes state valueOut
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim v)) -> v
            | other -> failwith $"expected Int64 value out, got %O{other}"

        let stringPointer =
            match
                IlMachineState.readManagedByref fixture.BaseClassTypes state stringValueOut
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> ptr
            | other -> failwith $"expected managed pointer string out, got %O{other}"

        let out =
            {
                Value = value
                StringPointer = stringPointer
                Length = readInt32Out fixture.BaseClassTypes state lengthOut
                CorElementType = readInt32Out fixture.BaseClassTypes state corElementTypeOut
            }

        returnValue, out, state

    let private constantField
        (fixture : MetadataImportFixture)
        (name : string)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        fixture.ConstantShapesType.Fields
        |> List.tryFind (fun field -> field.Name = name)
        |> Option.defaultWith (fun () -> failwith $"constant field %s{name} not found")

    [<Test>]
    let ``MetadataImport GetDefaultValue packs each constant into the low bytes of the buffer`` () : unit =
        let fixture = makeFixture ()

        // ECMA-335 II.23.1.16 element types, and the value as CoreCLR's `m_ullValue` carries it:
        // the blob's bytes little-endian in the low bytes, with the high bytes zero. `MdConstant`
        // reinterprets only the low member-width bytes (`*(sbyte*)&buffer` and friends), so the
        // *upper* bytes are unobservable to a guest and zero is PawPrint's determinism choice
        // rather than a fidelity claim — CoreCLR leaves them as stack garbage.
        let cases =
            [
                "BoolConst", 0x02, 1L, 1
                "CharConst", 0x03, int64 'q', 2
                // -1 as I1 is the single byte 0xFF: zero-extended here, and the managed side
                // recovers the sign by reinterpreting the low byte.
                "SByteConst", 0x04, 0xFFL, 1
                "ByteConst", 0x05, 200L, 1
                "Int16Const", 0x06, 0xFED4L, 2
                "UInt16Const", 0x07, 40000L, 2
                "Int32Const", 0x08, 42L, 4
                "UInt32Const", 0x09, 3000000000L, 4
                "Int64Const", 0x0A, -1234567890123L, 8
                // All eight bytes set: the one case where zero-extension has nothing left to do.
                "UInt64Const", 0x0B, -1L, 8
                // Floating point is a bit pattern, not a conversion.
                "SingleConst", 0x0C, int64 (System.BitConverter.SingleToInt32Bits 0.25f), 4
                "DoubleConst", 0x0D, System.BitConverter.DoubleToInt64Bits 0.5, 8
            ]

        let mutable state = fixture.State

        for name, expectedElementType, expectedValue, expectedLength in cases do
            let field = constantField fixture name

            let returnValue, out, nextState =
                invokeGetDefaultValue fixture (fieldDefToken field.Handle) state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
            out.CorElementType |> shouldEqual expectedElementType
            out.Value |> shouldEqual expectedValue
            out.Length |> shouldEqual expectedLength
            out.StringPointer |> shouldEqual ManagedPointerSource.Null

    [<Test>]
    let ``MetadataImport GetDefaultValue points at the constant blob for a string`` () : unit =
        let fixture = makeFixture ()
        let field = constantField fixture "StringConst"

        let _, out, state =
            invokeGetDefaultValue fixture (fieldDefToken field.Handle) fixture.State

        out.CorElementType |> shouldEqual 0x0E
        // Length is in *characters* here and in bytes for every other code: the FCall divides by
        // sizeof(WCHAR) only on this branch. "hello" is 5 chars in a 10-byte blob.
        out.Length |> shouldEqual 5
        out.Value |> shouldEqual 0L

        // The pointer must carry a `ReinterpretAs` projection, not just name the range:
        // `String.Ctor(char*, int, int)` offsets it before reading, and pointer arithmetic on a
        // bare PE-byte-range root is refused outright — the guest case fails without one. The
        // projection's *type* is not what makes the guest work (the offset is zero and the copy is
        // byte-wise, so `byte` would also do); `char` is pinned here because it is the type the API
        // declares, and pinning it is what stops it drifting silently.
        match out.StringPointer with
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange range, [ ByrefProjection.ReinterpretAs charType ]) ->
            range.AssemblyFullName |> shouldEqual fixture.Assembly.Name.FullName
            range.Size |> shouldEqual 10

            range.Source
            |> shouldEqual (PeByteRangePointerSource.ConstantBlob (ComparableFieldDefinitionHandle.Make field.Handle))

            // Look up Char in the *post*-invocation state: the handler is what concretizes it.
            let expectedCharType =
                AllConcreteTypes.lookup
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Char)
                    state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith "System.Char was not concretized")

            charType |> shouldEqual expectedCharType
        | other -> failwith $"expected a char pointer over the constant blob, got %O{other}"

    [<Test>]
    let ``MetadataImport GetDefaultValue reports an empty string as a null pointer`` () : unit =
        // `_FillMDDefaultValue` nulls the pointer when the blob is zero-length
        // (mdinternalro.cpp:3214); the managed wrapper's `stringVal ?? string.Empty` is what turns
        // that back into "". Handing back a zero-length range instead would make the wrapper build
        // a string from a pointer addressing nothing.
        let fixture = makeFixture ()
        let field = constantField fixture "EmptyStringConst"

        let _, out, _ =
            invokeGetDefaultValue fixture (fieldDefToken field.Handle) fixture.State

        out.CorElementType |> shouldEqual 0x0E
        out.Length |> shouldEqual 0
        out.StringPointer |> shouldEqual ManagedPointerSource.Null

    [<Test>]
    let ``MetadataImport GetDefaultValue reports a null constant as ELEMENT_TYPE_CLASS`` () : unit =
        // A null reference constant is its own element type, distinct from "no Constant row at
        // all", and ECMA-335 II.22.9 fixes its blob at four zero bytes.
        let fixture = makeFixture ()
        let field = constantField fixture "NullStringConst"

        let _, out, _ =
            invokeGetDefaultValue fixture (fieldDefToken field.Handle) fixture.State

        out.CorElementType |> shouldEqual 0x12
        out.Length |> shouldEqual 4
        out.Value |> shouldEqual 0L
        out.StringPointer |> shouldEqual ManagedPointerSource.Null

    [<Test>]
    let ``MetadataImport GetDefaultValue reports ELEMENT_TYPE_VOID for a field with no constant`` () : unit =
        // `MdConstant` turns VOID into DBNull.Value. CoreCLR leaves the buffer and length as stack
        // garbage on this path; PawPrint writes zeros, because a replay must not depend on the
        // host's stack.
        let fixture = makeFixture ()
        let field = constantField fixture "NotAConstant"

        let returnValue, out, _ =
            invokeGetDefaultValue fixture (fieldDefToken field.Handle) fixture.State

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        out.CorElementType |> shouldEqual 0x01
        out.Value |> shouldEqual 0L
        out.Length |> shouldEqual 0
        out.StringPointer |> shouldEqual ManagedPointerSource.Null

    [<Test>]
    let ``MetadataImport GetDefaultValue agrees with the host runtime for every literal`` () : unit =
        // Outside oracle: the same image in the host CLR, asked the same question through its own
        // metadata engine. This compares the *decoded* value rather than the raw out-params, so it
        // checks the packing and the element type together, in the way a guest would see them.
        let fixture = makeFixture ()

        let hostType =
            (System.Reflection.Assembly.Load fixture.Image).GetType "ConstantShapes"

        let literals =
            fixture.ConstantShapesType.Fields
            |> List.filter (fun field -> field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal)

        literals.Length |> shouldEqual 15

        let mutable state = fixture.State

        for field in literals do
            let _, out, nextState =
                invokeGetDefaultValue fixture (fieldDefToken field.Handle) state

            state <- nextState

            let expected = hostType.GetField(field.Name).GetRawConstantValue ()

            // Recover the same value the managed `MdConstant` would build from our out-params.
            let actual : obj =
                match out.CorElementType with
                | 0x02 -> box (out.Value <> 0L)
                | 0x03 -> box (char (uint16 out.Value))
                | 0x04 -> box (sbyte (byte out.Value))
                | 0x05 -> box (byte out.Value)
                | 0x06 -> box (int16 (uint16 out.Value))
                | 0x07 -> box (uint16 out.Value)
                | 0x08 -> box (int32 (uint32 out.Value))
                | 0x09 -> box (uint32 out.Value)
                | 0x0A -> box out.Value
                | 0x0B -> box (uint64 out.Value)
                | 0x0C -> box (System.BitConverter.Int32BitsToSingle (int32 (uint32 out.Value)))
                | 0x0D -> box (System.BitConverter.Int64BitsToDouble out.Value)
                | 0x0E ->
                    match out.StringPointer with
                    | ManagedPointerSource.Null -> box ""
                    | ManagedPointerSource.Byref (ByrefRoot.PeByteRange range, _) ->
                        let charTemplate, _ =
                            IlMachineState.cliTypeZeroOfHandle
                                state
                                fixture.BaseClassTypes
                                (AllConcreteTypes.getRequiredNonGenericHandle
                                    state.ConcreteTypes
                                    fixture.BaseClassTypes.Char)

                        System.String (
                            Array.init
                                out.Length
                                (fun i ->
                                    match
                                        IlMachineState.readPeByteRangeBytesAs state range (i * 2) charTemplate
                                        |> CliType.unwrapPrimitiveLikeDeep
                                    with
                                    | CliType.Char (hi, lo) -> char ((int hi <<< 8) ||| int lo)
                                    | other -> failwith $"expected Char in constant blob, got %O{other}"
                                )
                        )
                        |> box
                    | other -> failwith $"unexpected string pointer %O{other}"
                | 0x12 -> null
                | other -> failwith $"unexpected element type 0x%x{other} for %s{field.Name}"

            actual |> shouldEqual expected

    [<Test>]
    let ``MetadataImport GetDefaultValue rejects a non-FieldDef token`` () : unit =
        let fixture = makeFixture ()

        let ex =
            Assert.Throws (fun () ->
                invokeGetDefaultValue fixture (typeDefToken fixture.TargetType.TypeDefHandle) fixture.State
                |> ignore
            )

        ex.Message |> shouldContainText "expected FieldDef token"

    /// The bytes behind the `byte*` that `GetName` wrote, up to and including the NUL terminator,
    /// read straight out of the backing array rather than through `NativeCall.readNullTerminatedUtf8`.
    /// Decoding with the same assumption the handler encoded with would not distinguish UTF-8 from
    /// any other encoding, which is the single most likely way to get this wrong.
    ///
    /// A missing terminator surfaces as the array read running off the end, which `ManagedHeap`
    /// refuses; there is deliberately no scan limit here to soften that into a nicer message.
    let private readNameBufferIncludingTerminator (state : IlMachineState) (ptr : ManagedPointerSource) : byte array =
        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []) ->
            let rec loop (index : int) (acc : byte list) : byte list =
                match
                    IlMachineState.getArrayValue arrayAddr index state
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.UInt8 0uy) -> List.rev (0uy :: acc)
                | CliType.Numeric (CliNumericType.UInt8 b) -> loop (index + 1) (b :: acc)
                | other -> failwith $"expected a byte in the name buffer at index %d{index}, got %O{other}"

            loop 0 [] |> Array.ofList
        | other -> failwith $"expected a byref to the first element of a byte array, got %O{other}"

    let private invokeGetName
        (fixture : MetadataImportFixture)
        (mdToken : int32)
        (state : IlMachineState)
        : EvalStackValue * byte array * IlMachineState
        =
        let state, metadataImportType, getNameMethod =
            metadataImportMethod fixture state "GetName" 3

        let nameOut, state =
            allocateSlotOut
                fixture
                fixture.BaseClassTypes.IntPtr
                (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getNameMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 mdToken)
                    CliType.RuntimePointer (CliRuntimePointer.Managed nameOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state

        let namePtr =
            match
                IlMachineState.readManagedByref fixture.BaseClassTypes state nameOut
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> ptr
            | other -> failwith $"expected a managed pointer written to the name out param, got %O{other}"

        returnValue, readNameBufferIncludingTerminator state namePtr, state

    [<Test>]
    let ``MetadataImport GetName returns each field's name as null-terminated UTF-8`` () : unit =
        let fixture = makeFixture ()

        // Expectations are derived from PawPrint's own parse of the name, so this test pins the
        // encoding and the terminator but *not* the name itself; the host-runtime test below is
        // what stops both sides from being wrong together.
        let fields = fixture.FieldNamesType.Fields

        fields
        |> List.map (fun field -> field.Name)
        |> shouldEqual [ "PlainNamedField" ; "LiteralNamedField" ; "Größe" ]

        let mutable state = fixture.State

        let actual =
            fields
            |> List.map (fun field ->
                let returnValue, bytes, nextState =
                    invokeGetName fixture (fieldDefToken field.Handle) state

                state <- nextState
                returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
                field.Name, bytes
            )

        let expected =
            fields
            |> List.map (fun field ->
                field.Name, Array.append (System.Text.Encoding.UTF8.GetBytes field.Name) [| 0uy |]
            )

        actual |> shouldEqual expected

    [<Test>]
    let ``MetadataImport GetName encodes a non-ASCII name as UTF-8, not one byte per character`` () : unit =
        let fixture = makeFixture ()
        let field = fieldNamed fixture.FieldNamesType "Größe"

        let _, bytes, _ = invokeGetName fixture (fieldDefToken field.Handle) fixture.State

        // Five characters, seven UTF-8 bytes: `ö` and `ß` are two bytes each. An ASCII or Latin-1
        // encode would produce six bytes and a UTF-16 one eleven, so the length alone separates
        // all three, and the byte sequence pins which UTF-8 it is.
        field.Name.Length |> shouldEqual 5

        bytes
        |> shouldEqual [| 0x47uy ; 0x72uy ; 0xC3uy ; 0xB6uy ; 0xC3uy ; 0x9Fuy ; 0x65uy ; 0x00uy |]

    [<Test>]
    let ``MetadataImport GetName agrees with the host runtime for every field`` () : unit =
        let fixture = makeFixture ()

        // Outside oracle: the same image, the host CLR's own metadata engine. `Module.ResolveField`
        // reaches literal fields too — it catches `MissingFieldException` and falls back to
        // `ResolveLiteralField`, which is the `MdFieldInfo` path this handler exists to serve.
        let hostModule = (System.Reflection.Assembly.Load fixture.Image).ManifestModule

        let fields = fixture.Assembly.Fields |> Seq.map (fun kvp -> kvp.Key) |> Seq.toList

        fields |> List.isEmpty |> shouldEqual false

        let mutable state = fixture.State

        for fieldHandle in fields do
            let token = fieldDefToken fieldHandle

            let returnValue, bytes, nextState = invokeGetName fixture token state
            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

            let expected =
                Array.append (System.Text.Encoding.UTF8.GetBytes (hostModule.ResolveField token).Name) [| 0uy |]

            bytes |> shouldEqual expected

    [<Test>]
    let ``MetadataImport GetName agrees with the host runtime for every property`` () : unit =
        let fixture = makeFixture ()

        // Same outside oracle as the field case, but `Module` has no `ResolveProperty`, so the
        // property rows are reached through the reflected type instead. `Item` is here because the
        // indexer's metadata name is not its C# spelling, which a name reconstructed from anything
        // other than the `#Strings` entry would get wrong.
        let hostType =
            (System.Reflection.Assembly.Load fixture.Image).GetType ("PropertyShapes", true)

        let hostProperties =
            hostType.GetProperties (
                System.Reflection.BindingFlags.DeclaredOnly
                ||| System.Reflection.BindingFlags.Public
                ||| System.Reflection.BindingFlags.NonPublic
                ||| System.Reflection.BindingFlags.Instance
                ||| System.Reflection.BindingFlags.Static
            )

        hostProperties
        |> Array.map (fun property -> property.Name)
        |> List.ofArray
        |> shouldEqual [ "Alpha" ; "Beta" ; "Hidden" ; "Stat" ; "Item" ; "Größe" ; "Special" ]

        let mutable state = fixture.State

        for property in hostProperties do
            let returnValue, bytes, nextState =
                invokeGetName fixture property.MetadataToken state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

            bytes
            |> shouldEqual (Array.append (System.Text.Encoding.UTF8.GetBytes property.Name) [| 0uy |])

    [<Test>]
    let ``MetadataImport GetName rejects a token that is neither a FieldDef nor a PropertyDef`` () : unit =
        let fixture = makeFixture ()

        // CoreCLR would answer a TypeDef token here (with the type's name), but no managed caller
        // ever passes one: `RuntimeType.Name` goes through `Cache.GetName()`/`ConstructName`. A
        // TypeDef arriving means a PawPrint gap, so it must be loud rather than answered.
        let ex =
            Assert.Throws (fun () ->
                invokeGetName fixture (typeDefToken fixture.FieldNamesType.TypeDefHandle) fixture.State
                |> ignore
            )

        ex.Message |> shouldContainText "expected FieldDef or PropertyDef token"

    [<Test>]
    let ``MetadataImport GetName rejects a FieldDef token that is absent from the assembly`` () : unit =
        let fixture = makeFixture ()

        let ex =
            Assert.Throws (fun () -> invokeGetName fixture 0x04FFFFFF fixture.State |> ignore)

        ex.Message |> shouldContainText "was not present in"

    [<Test>]
    let ``MetadataImport GetName rejects a PropertyDef token that is absent from the assembly`` () : unit =
        let fixture = makeFixture ()

        // Unguarded, this reaches `MetadataReader.GetPropertyDefinition` and surfaces as
        // `BadImageFormatException: Read out of bounds` — which reads as "your assembly is corrupt"
        // when the truth is that PawPrint was handed a token it should never have seen.
        let ex =
            Assert.Throws (fun () -> invokeGetName fixture 0x17FFFFFF fixture.State |> ignore)

        ex.Message |> shouldContainText "was not present in"

    /// `GetPropertyProps` is the one `MetadataImport` call with three out parameters, so the helper
    /// hands back all three: the null-terminated name bytes, the raw `Property.Flags` column, and
    /// the `ConstArray` triple (length, bytes, and the `m_constArray` pointer itself).
    let private invokeGetPropertyProps
        (fixture : MetadataImportFixture)
        (propertyToken : int32)
        (state : IlMachineState)
        : EvalStackValue * byte array * int32 * (int32 * byte array * ManagedPointerSource) * IlMachineState
        =
        let state, metadataImportType, getPropertyPropsMethod =
            metadataImportMethod fixture state "GetPropertyProps" 5

        let nameOut, state =
            allocateSlotOut
                fixture
                fixture.BaseClassTypes.IntPtr
                (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                state

        let attributesOut, state = allocateInt32Out fixture 0 state
        let signatureOut, state = allocateConstArrayOut fixture state

        let state =
            invokeMetadataImportNative
                fixture
                metadataImportType
                getPropertyPropsMethod
                [
                    metadataImportHandle fixture
                    CliType.Numeric (CliNumericType.Int32 propertyToken)
                    CliType.RuntimePointer (CliRuntimePointer.Managed nameOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed attributesOut)
                    CliType.RuntimePointer (CliRuntimePointer.Managed signatureOut)
                ]
                state

        let returnValue, state = IlMachineState.popEvalStack (ThreadId 0) state

        let namePtr =
            match
                IlMachineState.readManagedByref fixture.BaseClassTypes state nameOut
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> ptr
            | other -> failwith $"expected a managed pointer written to the name out param, got %O{other}"

        returnValue,
        readNameBufferIncludingTerminator state namePtr,
        readInt32Out fixture.BaseClassTypes state attributesOut,
        readConstArrayOut fixture state signatureOut,
        state

    /// The number of rows in the image's Property table, so a test can name the first row that is
    /// genuinely absent. This computes an *input*, not an expectation, so reading it from the same
    /// metadata library the handler uses is not circular.
    let private propertyTableRowCount (fixture : MetadataImportFixture) : int =
        System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableRowCount (
            fixture.Assembly.PeReader.GetMetadataReader (),
            System.Reflection.Metadata.Ecma335.TableIndex.Property
        )

    [<Test>]
    let ``MetadataImport GetPropertyProps returns the name, flags and signature of each property shape`` () : unit =
        let fixture = makeFixture ()

        // ECMA-335 II.23.2.5: a PropertySig is `PROPERTY (0x08) [| HASTHIS (0x20)]`, then a
        // compressed ParamCount, then the Type, then one Type per index parameter. These
        // expectations come from the standard rather than being read back out of the image, so they
        // pin the blob's width as well as its content: ELEMENT_TYPE_I4 = 0x08, _STRING = 0x0e,
        // _VAR = 0x13 followed by the compressed generic-parameter index.
        let cases =
            [
                // Instance, no index parameters: HASTHIS set, ParamCount 0.
                "PropertyShapes", "Alpha", 0x0000, [| 0x28uy ; 0x00uy ; 0x08uy |]
                "PropertyShapes", "Beta", 0x0000, [| 0x28uy ; 0x00uy ; 0x0Euy |]
                "PropertyShapes", "Hidden", 0x0000, [| 0x28uy ; 0x00uy ; 0x08uy |]
                // Static: HASTHIS *clear*. This is the byte a handler that hardcoded 0x28 gets wrong.
                "PropertyShapes", "Stat", 0x0000, [| 0x08uy ; 0x00uy ; 0x08uy |]
                // The indexer: ParamCount 1, so the blob carries the index type after the property
                // type. Its metadata name is `Item`, not its C# spelling.
                "PropertyShapes", "Item", 0x0000, [| 0x28uy ; 0x01uy ; 0x08uy ; 0x08uy |]
                "PropertyShapes", "Größe", 0x0000, [| 0x28uy ; 0x00uy ; 0x08uy |]
                // The only property in the image whose Property.Flags column is not zero.
                "PropertyShapes", "Special", 0x0200, [| 0x28uy ; 0x00uy ; 0x08uy |]
                // ELEMENT_TYPE_VAR, generic parameter 0.
                "GenericPropertyHolder`1", "GenericProperty", 0x0000, [| 0x28uy ; 0x00uy ; 0x13uy ; 0x00uy |]
            ]

        let mutable state = fixture.State

        for typeName, propertyName, expectedFlags, expectedSignature in cases do
            let hostProperty = hostPropertyNamed fixture.Image typeName propertyName

            let returnValue, nameBytes, flags, (length, signatureBytes, _), nextState =
                invokeGetPropertyProps fixture hostProperty.MetadataToken state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

            nameBytes
            |> shouldEqual (Array.append (System.Text.Encoding.UTF8.GetBytes propertyName) [| 0uy |])

            flags |> shouldEqual expectedFlags
            signatureBytes |> shouldEqual expectedSignature
            length |> shouldEqual expectedSignature.Length

    [<Test>]
    let ``MetadataImport GetPropertyProps agrees with the host runtime for every property`` () : unit =
        let fixture = makeFixture ()

        // Outside oracle: the same image handed to the host CLR, whose answers come from CoreCLR's
        // own C++ metadata engine rather than from PawPrint's parse. This is what covers `Special`
        // without writing 0x0200 down a second time, and it ranges over every property row in the
        // image rather than the shapes listed above.
        //
        // The raw signature blob has no such oracle: `Module.ResolveSignature` refuses a property
        // token outright (`ArgumentException: Token 0x17...... is not valid in the scope of module`),
        // because the managed screen in `RuntimeModule.ResolveSignature` admits only
        // MemberRef/MethodDef/TypeSpec/StandAloneSig/FieldDef. See the next test for the parts of
        // the signature the host *can* answer.
        let hostProperties = hostPropertiesOfImage fixture.Image

        hostProperties.Length |> shouldEqual (propertyTableRowCount fixture)

        let mutable state = fixture.State

        for hostProperty in hostProperties do
            let returnValue, nameBytes, flags, _, nextState =
                invokeGetPropertyProps fixture hostProperty.MetadataToken state

            state <- nextState

            returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

            nameBytes
            |> shouldEqual (Array.append (System.Text.Encoding.UTF8.GetBytes hostProperty.Name) [| 0uy |])

            flags |> shouldEqual (int32 hostProperty.Attributes)

    [<Test>]
    let ``MetadataImport GetPropertyProps signature agrees with the host runtime where it can`` () : unit =
        let fixture = makeFixture ()

        // The host cannot hand back the blob, but it can independently answer the two fields the
        // blob leads with, from the MethodSemantics rows rather than from the Property row's Type
        // blob. That covers every property in the image, including any added later that nobody
        // wrote a byte-for-byte expectation for.
        let hostProperties = hostPropertiesOfImage fixture.Image

        let mutable state = fixture.State

        for hostProperty in hostProperties do
            let _, _, _, (length, signatureBytes, _), nextState =
                invokeGetPropertyProps fixture hostProperty.MetadataToken state

            state <- nextState

            // `PropertyInfo` has no `IsStatic` of its own; staticness is the accessors'. A property
            // always has at least one accessor.
            let accessor =
                match hostProperty.GetMethod, hostProperty.SetMethod with
                | null, null -> failwith $"property %s{hostProperty.Name} has no accessor"
                | null, setter -> setter
                | getter, _ -> getter

            length |> shouldEqual signatureBytes.Length
            // ECMA-335 II.23.2.5: the low nibble is the PROPERTY calling convention (0x08) and
            // 0x20 is HASTHIS.
            signatureBytes.[0] &&& 0x0Fuy |> shouldEqual 0x08uy
            signatureBytes.[0] &&& 0x20uy <> 0uy |> shouldEqual (not accessor.IsStatic)

            // ParamCount is a *compressed* unsigned integer, so it occupies one byte only below
            // 0x80. Nothing in this fixture comes close, and an indexer with 128 parameters is not
            // a shape C# can express.
            hostProperty.GetIndexParameters().Length < 0x80 |> shouldEqual true

            signatureBytes.[1]
            |> shouldEqual (byte (hostProperty.GetIndexParameters().Length))

    [<Test>]
    let ``MetadataImport GetPropertyProps points at the property's own signature blob`` () : unit =
        let fixture = makeFixture ()
        let hostProperty = hostPropertyNamed fixture.Image "PropertyShapes" "Alpha"

        let _, _, _, (length, _, pointer), state =
            invokeGetPropertyProps fixture hostProperty.MetadataToken fixture.State

        // The pointer's shape is part of the contract, not merely a route to the bytes, and this is
        // the assertion that separates the two ways of building a `ConstArray`. CoreCLR hands back a
        // PCCOR_SIGNATURE straight into the mapped metadata; PawPrint models that with a PeByteRange
        // root naming *this* PropertyDef. Copying the blob into a managed `byte[]` instead — as the
        // `GetMemberRefProps` sibling does — would satisfy every content assertion above and still
        // be wrong: `NativeSignature.corSigPeByteRange` accepts only null or a PeByteRange, so the
        // blob would be unresolvable by the one thing that ever consumes it
        // (`RuntimePropertyInfo.Signature`, via the handle-less `Signature` constructor).
        //
        // The `ReinterpretAs byte` projection is equally load-bearing: `BinaryArithmetic` refuses
        // arithmetic on a bare PeByteRange root, so without it a guest's `ConstArray[i]` — which is
        // `((byte*)m_constArray)[index]` — would fail while every content assertion still passed.
        let byteType =
            AllConcreteTypes.lookup fixture.ByteHandle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Byte was not concretized")

        let propertyHandle =
            System.Reflection.Metadata.Ecma335.MetadataTokens.PropertyDefinitionHandle hostProperty.MetadataToken

        let expected =
            ManagedPointerSource.Byref (
                ByrefRoot.PeByteRange
                    {
                        AssemblyFullName = fixture.Assembly.Name.FullName
                        Source =
                            PeByteRangePointerSource.PropertySignatureBlob (
                                ComparablePropertyDefinitionHandle.Make propertyHandle
                            )
                        RelativeVirtualAddress = 0
                        // `int Alpha { get; set; }` is `28 00 08`. Spelled out rather than taken
                        // from `length`, so that a handler which derived both the struct's length
                        // and the range's size from the same wrong place would still fail here.
                        Size = 3
                    },
                [ ByrefProjection.ReinterpretAs byteType ]
            )

        length |> shouldEqual 3
        pointer |> shouldEqual expected

    [<Test>]
    let ``MetadataImport GetPropertyProps rejects a non-PropertyDef token`` () : unit =
        let fixture = makeFixture ()

        let ex =
            Assert.Throws (fun () ->
                invokeGetPropertyProps fixture (typeDefToken fixture.PropertyShapesType.TypeDefHandle) fixture.State
                |> ignore
            )

        ex.Message |> shouldContainText "expected PropertyDef token"

    [<Test>]
    let ``MetadataImport GetPropertyProps rejects a PropertyDef absent from the assembly`` () : unit =
        let fixture = makeFixture ()

        // Two absent rows, because they catch different mistakes. Row 0xFFFFFF is far outside the
        // table and dies under almost any guard at all; the first row *past the end* is the one an
        // off-by-one guard (`> rowCount + 1`) would wave through, and it would then surface as
        // `BadImageFormatException: Read out of bounds` from inside the metadata reader — a PawPrint
        // gap wearing a corrupt image's clothes, which is exactly what the guard exists to prevent.
        let firstAbsentRow = propertyTableRowCount fixture + 1

        for token in [ 0x17FFFFFF ; 0x17000000 ||| firstAbsentRow ] do
            let ex =
                Assert.Throws (fun () -> invokeGetPropertyProps fixture token fixture.State |> ignore)

            ex.Message |> shouldContainText "was not present in"
